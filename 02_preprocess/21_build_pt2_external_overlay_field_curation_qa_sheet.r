# ==== 21_build_pt2_external_overlay_field_curation_qa_sheet.r =================
##
## PURPOSE:
##   Build a field-curation QA sheet for the PortaTreasure2 Tools / External
##   Overlays catalog.
##
## WHY:
##   A service that loads is not automatically useful.  For each external
##   FeatureServer/MapServer/GeoJSON row, we also need to know:
##
##     * Which fields are useful in popups?
##     * Which fields are numeric and useful for future styling?
##     * Does the service expose the actual value we care about, or only station
##       metadata?
##     * Should the row be approved, deferred, rejected, or replaced by a better
##       service?
##
## OUTPUTS:
##   04_processed_data/qa/pt2_external_overlay_field_curation_test_sheet_latest.csv
##   04_processed_data/qa/pt2_external_overlay_field_curation_test_sheet_YYYYMMDD_HHMMSS.csv
##
## NOTE:
##   The current PT2 HTML helper may not yet use popup_fields/style fields.  This
##   sheet is the curation foundation for a later map-helper update that will
##   request/display only curated fields and offer attribute styling choices.
##

# ==== 1. User switches ========================================================

RUN_HTTP_METADATA_CHECKS <- TRUE

USE_CANDIDATE_FILE_IF_PRESENT <- TRUE

MAX_ROWS_TO_CHECK <- Inf

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

approved_catalog_path <- file.path("00_config", "external_service_catalog.csv")

candidate_catalog_path <- file.path(
  "04_processed_data",
  "qa",
  "pt2_external_overlay_catalog_candidates.csv"
)

field_overrides_path <- file.path(
  "00_config",
  "external_service_field_overrides.csv"
)

qa_dir <- file.path("04_processed_data", "qa")
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

qa_latest_path <- file.path(
  qa_dir,
  "pt2_external_overlay_field_curation_test_sheet_latest.csv"
)

qa_timestamp_path <- file.path(
  qa_dir,
  paste0("pt2_external_overlay_field_curation_test_sheet_", timestamp, ".csv")
)

# ==== 4. Column definitions ===================================================

catalog_cols <- c(
  "agency",
  "program",
  "theme",
  "display_name",
  "service_type",
  "service_url",
  "supports_popups",
  "default_clickable",
  "default_opacity",
  "notes",
  "source_page",
  "priority",
  "default_load_mode",
  "where_clause",
  "large_layer_warning",
  "min_zoom_live",
  "min_zoom_current_view",
  "legend_url",
  "legend_note"
)

field_curation_cols <- c(
  "best_use",
  "useful_for_visualization",
  "popup_fields",
  "popup_aliases",
  "hover_fields",
  "default_label_field",
  "out_fields",
  "style_field_candidates",
  "default_style_field",
  "default_style_method",
  "style_units",
  "style_legend_title",
  "style_direction",
  "field_curation_notes"
)

qa_cols <- c(
  "qa_id",
  "test_status",
  "test_date",
  "tester",
  "manual_test_notes",
  "service_opens_in_browser",
  "metadata_read_ok",
  "legend_works",
  "loads_in_pt2",
  "loads_fast_enough",
  "popup_works",
  "approved_for_catalog",
  "defer_reason",
  "detected_service_type",
  "metadata_name",
  "metadata_geometry_type",
  "metadata_max_record_count",
  "metadata_capabilities",
  "metadata_field_count",
  "metadata_all_fields",
  "metadata_field_alias_pairs",
  "metadata_numeric_fields",
  "metadata_date_fields",
  "metadata_string_fields",
  "metadata_extent_summary",
  "metadata_error",
  "suggested_popup_fields",
  "suggested_hover_fields",
  "suggested_label_field",
  "suggested_style_fields",
  "suggested_best_use",
  "suggested_useful_for_visualization",
  "field_review_status",
  "field_review_notes",
  "recommended_service_type",
  "recommended_load_mode",
  "recommended_min_zoom_live",
  "recommended_min_zoom_current_view",
  "recommended_where_clause",
  "recommended_opacity",
  "recommended_supports_popups",
  "recommended_default_clickable",
  "recommended_legend_url",
  "recommended_legend_note"
)

# ==== 5. Utility functions ====================================================

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
  ## Vectorized on purpose.
  ##
  ## This function is used both:
  ##   1. inside dplyr::mutate() on full columns, and
  ##   2. inside scalar helper functions.
  ##
  ## A prior version used as.character(url)[1], which accidentally copied the
  ## first service_url to every row in the QA sheet.  Do not de-vectorize here.
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

detect_service_type <- function(url, service_type = "") {
  url_l <- stringr::str_to_lower(normalize_url(url)[1])
  service_type_l <- stringr::str_to_lower(stringr::str_trim(as.character(service_type)[1]))

  if (service_type_l %in% c("feature", "map", "geojson")) {
    return(service_type_l)
  }

  if (stringr::str_detect(url_l, "/featureserver(/\\d+)?($|\\?)")) {
    return("feature")
  }

  if (stringr::str_detect(url_l, "/mapserver(/\\d+)?($|\\?)")) {
    return("map")
  }

  if (stringr::str_detect(url_l, "\\.geojson($|\\?)|f=geojson|format=geojson")) {
    return("geojson")
  }

  if (stringr::str_detect(url_l, "/datasets/|hub\\.arcgis\\.com|arcgis\\.com/home/item")) {
    return("hub_page")
  }

  "unknown"
}

derive_legend_url <- function(url) {
  url <- normalize_url(url)[1]

  if (!isTRUE(stringr::str_detect(url, regex("/MapServer", ignore_case = TRUE)))) {
    return("")
  }

  base <- stringr::str_replace(
    url,
    regex("(/MapServer)(/\\d+)?$", ignore_case = TRUE),
    "\\1"
  )

  paste0(base, "/legend")
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

suggest_popup_fields <- function(ft, display_name = "", max_n = 12) {
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

  ## If the heuristic finds nothing, use a compact first-fields fallback.
  if (length(picked) == 0) {
    picked <- ft$name
  }

  collapse_fields(picked, max_n = max_n)
}

suggest_style_fields <- function(ft, max_n = 12) {
  if (nrow(ft) == 0) return("")

  ft_num <- ft |>
    dplyr::filter(is_numeric_type(type))

  if (nrow(ft_num) == 0) return("")

  names_l <- stringr::str_to_lower(ft_num$name)
  aliases_l <- stringr::str_to_lower(ft_num$alias)

  ## Avoid ID-only fields where possible.
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

suggest_best_use <- function(row, ft) {
  txt <- paste(
    row$display_name %||% "",
    row$theme %||% "",
    row$program %||% "",
    row$notes %||% "",
    collapse = " "
  ) |>
    stringr::str_to_lower()

  style_fields <- row$suggested_style_fields %||% ""

  dplyr::case_when(
    stringr::str_detect(txt, "station|well|monitoring") &
      !stringr::str_detect(stringr::str_to_lower(style_fields), "depth|elev|level|measure") ~
      "station inventory / monitoring locations",
    stringr::str_detect(txt, "groundwater") &
      stringr::str_detect(stringr::str_to_lower(style_fields), "depth|elev|level|measure") ~
      "groundwater value visualization / inspection",
    stringr::str_detect(txt, "critical habitat|habitat|species|biodiversity|ace") ~
      "biology / habitat context and attribute inspection",
    stringr::str_detect(txt, "flood|nfhl|levee") ~
      "flood-hazard visual context",
    stringr::str_detect(txt, "boundary|boundaries|region|district|area") ~
      "reference boundary overlay",
    TRUE ~ "external overlay / screening context"
  )
}

suggest_useful_for_visualization <- function(service_type, style_fields) {
  if (service_type == "map") {
    return("service-rendered")
  }

  if (!is.na(style_fields) && style_fields != "") {
    return("yes")
  }

  "limited"
}

# ==== 6. Read catalog/candidates =============================================

input_path <- approved_catalog_path

if (
  isTRUE(USE_CANDIDATE_FILE_IF_PRESENT) &&
    file.exists(candidate_catalog_path)
) {
  input_path <- candidate_catalog_path
}

if (!file.exists(input_path)) {
  stop("Catalog/candidate file not found: ", input_path)
}

message("Reading external overlay catalog/candidates:")
message("  ", input_path)

catalog <- readr::read_csv(
  input_path,
  show_col_types = FALSE,
  progress = FALSE
)

needed_cols <- unique(c(catalog_cols, field_curation_cols, qa_cols))
missing_cols <- setdiff(needed_cols, names(catalog))

if (length(missing_cols) > 0) {
  for (nm in missing_cols) {
    catalog[[nm]] <- ""
  }
}

catalog <- catalog |>
  dplyr::mutate(
    dplyr::across(dplyr::everything(), ~ as.character(.x)),
    service_url = normalize_url(service_url),
    service_type = stringr::str_to_lower(stringr::str_trim(service_type)),
    qa_id = dplyr::if_else(
      qa_id == "" | is.na(qa_id),
      paste0("PT2EXT-", stringr::str_pad(dplyr::row_number(), 4, pad = "0")),
      qa_id
    ),
    test_status = dplyr::if_else(
      test_status == "" | is.na(test_status),
      "untested",
      test_status
    ),
    field_review_status = dplyr::if_else(
      field_review_status == "" | is.na(field_review_status),
      "needs_field_review",
      field_review_status
    ),
    detected_service_type = purrr::map2_chr(
      service_url,
      service_type,
      detect_service_type
    )
  ) |>
  dplyr::filter(!is.na(service_url), service_url != "")

if (is.finite(MAX_ROWS_TO_CHECK)) {
  catalog <- catalog |>
    dplyr::slice_head(n = MAX_ROWS_TO_CHECK)
}

message("Rows after reading/filtering: ", nrow(catalog))
message("Unique service_url values after normalization: ", dplyr::n_distinct(catalog$service_url))

if (nrow(catalog) > 1 && dplyr::n_distinct(catalog$service_url) == 1) {
  warning(
    "All rows have the same service_url after normalization. ",
    "This usually indicates a catalog/input problem and should be reviewed before field curation."
  )
}

# ==== 7. Optional metadata/field checks ======================================

if (isTRUE(RUN_HTTP_METADATA_CHECKS)) {

  message("Running metadata/field checks for ", nrow(catalog), " row(s)...")

  checked <- purrr::pmap_dfr(
    list(
      catalog$service_url,
      catalog$detected_service_type,
      catalog$legend_url,
      catalog$display_name,
      catalog$theme,
      catalog$program,
      catalog$notes
    ),
    function(service_url,
             detected_service_type,
             legend_url,
             display_name,
             theme,
             program,
             notes) {

      Sys.sleep(REQUEST_SLEEP_SECONDS)

      service_url <- normalize_url(service_url)
      detected_service_type <- stringr::str_to_lower(detected_service_type)

      out <- tibble::tibble(
        metadata_read_ok = "FALSE",
        metadata_name = "",
        metadata_geometry_type = "",
        metadata_max_record_count = "",
        metadata_capabilities = "",
        metadata_field_count = "",
        metadata_all_fields = "",
        metadata_field_alias_pairs = "",
        metadata_numeric_fields = "",
        metadata_date_fields = "",
        metadata_string_fields = "",
        metadata_extent_summary = "",
        metadata_error = "",
        legend_works = "",
        suggested_popup_fields = "",
        suggested_hover_fields = "",
        suggested_label_field = "",
        suggested_style_fields = "",
        suggested_best_use = "",
        suggested_useful_for_visualization = ""
      )

      if (detected_service_type %in% c("feature", "map")) {

        meta <- safe_from_json_url(service_url)

        if (!is.null(meta$.error)) {

          out$metadata_error <- meta$.error

        } else {

          ft <- field_table(meta$fields)

          numeric_fields <- ft |>
            dplyr::filter(is_numeric_type(type)) |>
            dplyr::pull(name)

          date_fields <- ft |>
            dplyr::filter(is_date_type(type)) |>
            dplyr::pull(name)

          string_fields <- ft |>
            dplyr::filter(is_string_type(type)) |>
            dplyr::pull(name)

          out$metadata_read_ok <- "TRUE"
          out$metadata_name <- as_chr(meta$name %||% meta$mapName %||% meta$serviceDescription)
          out$metadata_geometry_type <- as_chr(meta$geometryType)
          out$metadata_max_record_count <- as_chr(meta$maxRecordCount)
          out$metadata_capabilities <- as_chr(meta$capabilities)
          out$metadata_field_count <- as_chr(nrow(ft))
          out$metadata_all_fields <- collapse_fields(ft$name)
          out$metadata_field_alias_pairs <- collapse_fields(
            paste0(ft$name, "=", ft$alias)
          )
          out$metadata_numeric_fields <- collapse_fields(numeric_fields)
          out$metadata_date_fields <- collapse_fields(date_fields)
          out$metadata_string_fields <- collapse_fields(string_fields)
          out$metadata_extent_summary <- extent_summary(meta$extent %||% meta$fullExtent)

          out$suggested_popup_fields <- suggest_popup_fields(ft, display_name)
          out$suggested_hover_fields <- suggest_label_field(ft)
          out$suggested_label_field <- suggest_label_field(ft)
          out$suggested_style_fields <- suggest_style_fields(ft)
        }

        legend_try_url <- normalize_url(legend_url)

        if (legend_try_url == "" && detected_service_type == "map") {
          legend_try_url <- derive_legend_url(service_url)
        }

        legend_try_url <- normalize_url(legend_try_url)

        if (legend_try_url != "") {
          legend_meta <- safe_from_json_url(legend_try_url)
          out$legend_works <- ifelse(is.null(legend_meta$.error), "TRUE", "FALSE")
        }
      }

      if (detected_service_type == "geojson") {
        out$metadata_error <- "GeoJSON field metadata check skipped. Manual test needed."
      }

      out$suggested_useful_for_visualization <- suggest_useful_for_visualization(
        detected_service_type,
        out$suggested_style_fields
      )

      row_stub <- list(
        display_name = display_name,
        theme = theme,
        program = program,
        notes = notes,
        suggested_style_fields = out$suggested_style_fields
      )

      out$suggested_best_use <- suggest_best_use(row_stub, tibble::tibble())

      out
    }
  )

  catalog <- catalog |>
    dplyr::select(-dplyr::any_of(names(checked))) |>
    dplyr::bind_cols(checked)
}

# ==== 8. Fill recommended/curation fields ====================================

catalog <- catalog |>
  dplyr::mutate(
    popup_fields = dplyr::if_else(
      popup_fields == "" | is.na(popup_fields),
      suggested_popup_fields,
      popup_fields
    ),
    hover_fields = dplyr::if_else(
      hover_fields == "" | is.na(hover_fields),
      suggested_hover_fields,
      hover_fields
    ),
    default_label_field = dplyr::if_else(
      default_label_field == "" | is.na(default_label_field),
      suggested_label_field,
      default_label_field
    ),
    style_field_candidates = dplyr::if_else(
      style_field_candidates == "" | is.na(style_field_candidates),
      suggested_style_fields,
      style_field_candidates
    ),
    best_use = dplyr::if_else(
      best_use == "" | is.na(best_use),
      suggested_best_use,
      best_use
    ),
    useful_for_visualization = dplyr::if_else(
      useful_for_visualization == "" | is.na(useful_for_visualization),
      suggested_useful_for_visualization,
      useful_for_visualization
    ),
    out_fields = purrr::pmap_chr(
      list(out_fields, popup_fields, style_field_candidates, default_label_field),
      function(existing_out_fields, popup_fields_i, style_fields_i, label_field_i) {

        existing_out_fields <- as.character(existing_out_fields %||% "")

        if (!is.na(existing_out_fields) && existing_out_fields != "") {
          return(existing_out_fields)
        }

        vals <- paste(
          popup_fields_i %||% "",
          style_fields_i %||% "",
          label_field_i %||% "",
          sep = "; "
        )

        vals <- unlist(strsplit(vals, ";\\s*"))
        vals <- unique(vals[!is.na(vals) & vals != ""])

        paste(vals, collapse = "; ")
      }
    ),
    default_style_method = dplyr::if_else(
      default_style_method == "" | is.na(default_style_method),
      dplyr::if_else(style_field_candidates != "", "quantile", ""),
      default_style_method
    ),
    recommended_service_type = dplyr::if_else(
      recommended_service_type == "" | is.na(recommended_service_type),
      detected_service_type,
      recommended_service_type
    ),
    recommended_load_mode = dplyr::if_else(
      recommended_load_mode == "" | is.na(recommended_load_mode),
      default_load_mode,
      recommended_load_mode
    ),
    recommended_min_zoom_live = dplyr::if_else(
      recommended_min_zoom_live == "" | is.na(recommended_min_zoom_live),
      min_zoom_live,
      recommended_min_zoom_live
    ),
    recommended_min_zoom_current_view = dplyr::if_else(
      recommended_min_zoom_current_view == "" | is.na(recommended_min_zoom_current_view),
      min_zoom_current_view,
      recommended_min_zoom_current_view
    ),
    recommended_where_clause = dplyr::if_else(
      recommended_where_clause == "" | is.na(recommended_where_clause),
      where_clause,
      recommended_where_clause
    ),
    recommended_opacity = dplyr::if_else(
      recommended_opacity == "" | is.na(recommended_opacity),
      default_opacity,
      recommended_opacity
    ),
    recommended_supports_popups = dplyr::if_else(
      recommended_supports_popups == "" | is.na(recommended_supports_popups),
      supports_popups,
      recommended_supports_popups
    ),
    recommended_default_clickable = dplyr::if_else(
      recommended_default_clickable == "" | is.na(recommended_default_clickable),
      default_clickable,
      recommended_default_clickable
    ),
    recommended_legend_url = dplyr::if_else(
      recommended_legend_url == "" | is.na(recommended_legend_url),
      legend_url,
      recommended_legend_url
    ),
    recommended_legend_note = dplyr::if_else(
      recommended_legend_note == "" | is.na(recommended_legend_note),
      legend_note,
      recommended_legend_note
    ),
    test_status = dplyr::case_when(
      test_status != "untested" ~ test_status,
      metadata_read_ok == "TRUE" ~ "metadata_ok_needs_manual_pt2_test",
      metadata_read_ok == "FALSE" & metadata_error != "" ~ "metadata_failed",
      TRUE ~ test_status
    )
  )

# ==== 9. Apply optional field-curation overrides ==============================
##
## Human-reviewed decisions should live in:
##
##   00_config/external_service_field_overrides.csv
##
## instead of being hand-edited into generated QA outputs. This keeps config and
## scripts as the source of truth, while processed QA sheets remain reproducible
## outputs.
##
## Matching order:
##   1. service_url, if provided
##   2. display_name, if service_url is blank
##
## Only nonblank override values are applied. Blank override cells leave the
## generated values unchanged.

apply_field_overrides <- function(catalog, overrides_path) {

  if (!file.exists(overrides_path)) {
    message("No field override file found; using generated suggestions only:")
    message("  ", overrides_path)
    return(catalog)
  }

  overrides <- readr::read_csv(
    overrides_path,
    show_col_types = FALSE,
    progress = FALSE
  ) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  if (nrow(overrides) == 0) {
    message("Field override file is empty; using generated suggestions only:")
    message("  ", overrides_path)
    return(catalog)
  }

  override_cols <- c(
    "best_use",
    "useful_for_visualization",
    "popup_fields",
    "popup_aliases",
    "hover_fields",
    "default_label_field",
    "out_fields",
    "style_field_candidates",
    "default_style_field",
    "default_style_method",
    "style_units",
    "style_legend_title",
    "style_direction",
    "field_curation_notes",
    "field_review_status",
    "test_status",
    "manual_test_notes",
    "approved_for_catalog",
    "recommended_service_type",
    "recommended_load_mode",
    "recommended_min_zoom_live",
    "recommended_min_zoom_current_view",
    "recommended_where_clause",
    "recommended_opacity",
    "recommended_supports_popups",
    "recommended_default_clickable",
    "recommended_legend_url",
    "recommended_legend_note"
  )

  for (nm in override_cols) {
    if (!nm %in% names(catalog)) {
      catalog[[nm]] <- ""
    }
    if (!nm %in% names(overrides)) {
      overrides[[nm]] <- ""
    }
  }

  if (!"service_url" %in% names(overrides)) {
    overrides$service_url <- ""
  }

  if (!"display_name" %in% names(overrides)) {
    overrides$display_name <- ""
  }

  n_applied_rows <- 0
  n_applied_values <- 0

  for (i in seq_len(nrow(overrides))) {

    service_url_i <- normalize_url(overrides$service_url[i])[1]
    display_name_i <- as.character(overrides$display_name[i] %||% "")

    idx <- integer()

    if (!is.na(service_url_i) && service_url_i != "") {
      idx <- which(normalize_url(catalog$service_url) == service_url_i)
    }

    if (length(idx) == 0 && !is.na(display_name_i) && display_name_i != "") {
      idx <- which(catalog$display_name == display_name_i)
    }

    if (length(idx) == 0) {
      warning(
        "No catalog row matched field override row ",
        i,
        ": ",
        display_name_i
      )
      next
    }

    applied_this_row <- FALSE

    for (nm in override_cols) {
      val <- overrides[[nm]][i]

      if (!is.na(val) && val != "") {
        catalog[[nm]][idx] <- val
        n_applied_values <- n_applied_values + length(idx)
        applied_this_row <- TRUE
      }
    }

    if (isTRUE(applied_this_row)) {
      n_applied_rows <- n_applied_rows + 1
    }
  }

  message("Applied field-curation override rows: ", n_applied_rows)
  message("Applied field-curation override values: ", n_applied_values)
  message("Override source:")
  message("  ", overrides_path)

  catalog
}

catalog <- apply_field_overrides(catalog, field_overrides_path)

# ==== 10. Write outputs =======================================================

out_cols <- unique(c(catalog_cols, field_curation_cols, qa_cols, names(catalog)))

catalog <- catalog |>
  dplyr::select(dplyr::any_of(out_cols))

readr::write_csv(catalog, qa_latest_path)
readr::write_csv(catalog, qa_timestamp_path)

message("Wrote external overlay field-curation QA sheets:")
message("  ", qa_latest_path)
message("  ", qa_timestamp_path)

message("Rows: ", nrow(catalog))
message("Field review status counts:")
print(table(catalog$field_review_status, useNA = "ifany"))

message("Test status counts:")
print(table(catalog$test_status, useNA = "ifany"))
