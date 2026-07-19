# ==== 26_review_pt2_external_overlay_fields_helpers.r =========================
##
## PURPOSE:
##   Convenience functions for reviewing external-overlay fields one row at a
##   time during ChatGPT/user curation sessions.
##
## USAGE:
##   source("02_preprocess/26_review_pt2_external_overlay_fields_helpers.r")
##
##   pt2_review_queue()
##   pt2_show_overlay_fields(pattern = "groundwater")
##   pt2_show_overlay_fields(qa_id = "PT2EXT-0012")
##
## FIX IN THIS VERSION:
##   The earlier helper had a data-mask scoping bug.  Inside dplyr::filter(),
##   `qa_id` on the right-hand side could be interpreted as the column instead
##   of the function argument, so `.data$qa_id == qa_id` effectively compared
##   the column to itself and returned every row.
##
##   This version uses `.env$qa_id_value` to force dplyr to use the function
##   argument.
##

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tidyr)
})

pt2_field_qa_path <- file.path(
  "04_processed_data",
  "qa",
  "pt2_external_overlay_field_curation_test_sheet_latest.csv"
)

pt2_load_field_qa <- function(path = pt2_field_qa_path) {
  if (!file.exists(path)) {
    stop("Field-curation QA sheet not found: ", path)
  }

  readr::read_csv(path, show_col_types = FALSE, progress = FALSE) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
}

pt2_split_fields <- function(x) {
  x <- as.character(x)

  if (length(x) == 0 || is.na(x) || x == "") {
    return(character())
  }

  unlist(strsplit(x, ";\\s*")) |>
    unique()
}

pt2_review_queue <- function(path = pt2_field_qa_path, n = 30) {
  qa <- pt2_load_field_qa(path)

  qa |>
    dplyr::filter(
      is.na(field_review_status) |
        field_review_status == "" |
        field_review_status == "needs_field_review"
    ) |>
    dplyr::select(
      qa_id,
      agency,
      display_name,
      service_type,
      metadata_geometry_type,
      useful_for_visualization,
      best_use,
      metadata_field_count
    ) |>
    dplyr::slice_head(n = n) |>
    print(n = n)
}

pt2_show_overlay_fields <- function(qa_id = NULL,
                                    pattern = NULL,
                                    path = pt2_field_qa_path,
                                    max_fields = 200) {
  qa <- pt2_load_field_qa(path)

  if (!is.null(qa_id)) {

    qa_id_value <- as.character(qa_id)[1]

    row <- qa |>
      dplyr::filter(.data$qa_id == .env$qa_id_value)

  } else if (!is.null(pattern)) {

    pattern_value <- as.character(pattern)[1]

    row <- qa |>
      dplyr::filter(stringr::str_detect(
        .data$display_name,
        stringr::regex(.env$pattern_value, ignore_case = TRUE)
      ))

  } else {
    stop("Provide qa_id or pattern.")
  }

  if (nrow(row) == 0) {
    stop("No matching rows found.")
  }

  if (nrow(row) > 1) {
    message("Multiple rows matched. Showing compact match list first:")
    print(
      row |>
        dplyr::select(qa_id, agency, display_name, service_type, metadata_geometry_type) |>
        dplyr::slice_head(n = 30),
      n = 30
    )
    message("Rerun with qa_id = '<one of the IDs above>' for full detail.")
    return(invisible(row))
  }

  r <- row[1, ]

  cat("\n")
  cat("QA ID:       ", r$qa_id, "\n", sep = "")
  cat("Agency:      ", r$agency, "\n", sep = "")
  cat("Layer:       ", r$display_name, "\n", sep = "")
  cat("Type:        ", r$service_type, "\n", sep = "")
  cat("Geometry:    ", r$metadata_geometry_type, "\n", sep = "")
  cat("Best use:    ", r$best_use, "\n", sep = "")
  cat("Service URL: ", r$service_url, "\n", sep = "")
  cat("\n")

  fields <- tibble::tibble(
    field = pt2_split_fields(r$metadata_all_fields),
    group = "all_fields"
  )

  numeric_fields <- pt2_split_fields(r$metadata_numeric_fields)
  date_fields <- pt2_split_fields(r$metadata_date_fields)
  string_fields <- pt2_split_fields(r$metadata_string_fields)

  field_table <- fields |>
    dplyr::mutate(
      is_numeric = field %in% numeric_fields,
      is_date = field %in% date_fields,
      is_string = field %in% string_fields,
      suggested_popup = field %in% pt2_split_fields(r$suggested_popup_fields),
      suggested_style = field %in% pt2_split_fields(r$suggested_style_fields),
      curated_popup = field %in% pt2_split_fields(r$popup_fields),
      curated_hover = field %in% pt2_split_fields(r$hover_fields),
      curated_style_candidate = field %in% pt2_split_fields(r$style_field_candidates)
    ) |>
    dplyr::slice_head(n = max_fields)

  cat("Suggested popup fields:\n")
  cat(r$suggested_popup_fields, "\n\n")

  cat("Curated popup fields:\n")
  cat(r$popup_fields, "\n\n")

  cat("Curated hover fields:\n")
  cat(r$hover_fields, "\n\n")

  cat("Suggested style fields:\n")
  cat(r$suggested_style_fields, "\n\n")

  cat("Curated style fields:\n")
  cat(r$style_field_candidates, "\n\n")

  cat("Default style field:\n")
  cat(r$default_style_field, "\n\n")

  cat("Field table:\n")
  print(field_table, n = max_fields)

  invisible(row)
}

pt2_show_curated_overlays <- function(path = pt2_field_qa_path) {
  qa <- pt2_load_field_qa(path)

  qa |>
    dplyr::filter(
      !is.na(field_review_status),
      field_review_status != "",
      field_review_status != "needs_field_review"
    ) |>
    dplyr::select(
      qa_id,
      agency,
      display_name,
      field_review_status,
      hover_fields,
      popup_fields,
      style_field_candidates,
      default_style_field
    ) |>
    print(n = 100)
}
