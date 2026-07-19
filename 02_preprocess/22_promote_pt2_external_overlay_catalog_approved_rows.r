# ==== 22_promote_pt2_external_overlay_catalog_approved_rows.r =================
##
## PURPOSE:
##   Promote manually reviewed/approved external-overlay catalog rows into the
##   PortaTreasure2 production catalog:
##
##     00_config/external_service_catalog.csv
##
## EXPECTED INPUT:
##   04_processed_data/qa/pt2_external_overlay_field_curation_test_sheet_latest.csv
##
## IMPORTANT:
##   This script preserves field-curation columns such as popup_fields and
##   style_field_candidates.  The current HTML helper may ignore some of these
##   fields until a later implementation stage, but preserving them now avoids
##   redoing the field-review work.
##

# ==== 1. User switches ========================================================

DRY_RUN <- TRUE

APPROVED_VALUES <- c("true", "yes", "y", "1", "approved")

# ==== 2. Packages =============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

# ==== 3. Paths ================================================================

qa_latest_path <- file.path(
  "04_processed_data",
  "qa",
  "pt2_external_overlay_field_curation_test_sheet_latest.csv"
)

approved_catalog_path <- file.path("00_config", "external_service_catalog.csv")

backup_path <- file.path(
  "00_config",
  paste0(
    "external_service_catalog_backup_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    ".csv"
  )
)

# ==== 4. Production catalog fields ===========================================

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
  "legend_note",
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

# ==== 5. Utility =============================================================

truthy <- function(x) {
  x <- stringr::str_to_lower(stringr::str_trim(as.character(x)))
  x %in% APPROVED_VALUES
}

coalesce_text <- function(preferred, fallback) {
  preferred <- as.character(preferred)
  fallback <- as.character(fallback)

  dplyr::if_else(
    !is.na(preferred) & preferred != "",
    preferred,
    fallback
  )
}

# ==== 6. Read QA sheet ========================================================

if (!file.exists(qa_latest_path)) {
  stop("QA sheet not found: ", qa_latest_path)
}

qa <- readr::read_csv(
  qa_latest_path,
  show_col_types = FALSE,
  progress = FALSE
) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

missing_cols <- setdiff(catalog_cols, names(qa))

if (length(missing_cols) > 0) {
  for (nm in missing_cols) {
    qa[[nm]] <- ""
  }
}

approved <- qa |>
  dplyr::filter(truthy(approved_for_catalog)) |>
  dplyr::mutate(
    service_type = coalesce_text(recommended_service_type, service_type),
    default_load_mode = coalesce_text(recommended_load_mode, default_load_mode),
    min_zoom_live = coalesce_text(recommended_min_zoom_live, min_zoom_live),
    min_zoom_current_view = coalesce_text(
      recommended_min_zoom_current_view,
      min_zoom_current_view
    ),
    where_clause = coalesce_text(recommended_where_clause, where_clause),
    default_opacity = coalesce_text(recommended_opacity, default_opacity),
    supports_popups = coalesce_text(recommended_supports_popups, supports_popups),
    default_clickable = coalesce_text(
      recommended_default_clickable,
      default_clickable
    ),
    legend_url = coalesce_text(recommended_legend_url, legend_url),
    legend_note = coalesce_text(recommended_legend_note, legend_note)
  ) |>
  dplyr::select(dplyr::all_of(catalog_cols)) |>
  dplyr::distinct(display_name, service_url, .keep_all = TRUE) |>
  dplyr::arrange(
    suppressWarnings(as.numeric(priority)),
    agency,
    theme,
    display_name
  )

message("Rows in field-curation QA sheet: ", nrow(qa))
message("Rows approved for production catalog: ", nrow(approved))

if (nrow(approved) == 0) {
  stop("No rows are marked approved_for_catalog = TRUE.")
}

message("Approved rows by agency:")
print(table(approved$agency, useNA = "ifany"))

if (isTRUE(DRY_RUN)) {
  message("")
  message("DRY_RUN = TRUE. No files written.")
  message("Set DRY_RUN <- FALSE to write:")
  message("  ", approved_catalog_path)
} else {
  if (file.exists(approved_catalog_path)) {
    file.copy(
      from = approved_catalog_path,
      to = backup_path,
      overwrite = TRUE
    )
    message("Backup written:")
    message("  ", backup_path)
  }

  readr::write_csv(approved, approved_catalog_path)

  message("Approved production catalog written:")
  message("  ", approved_catalog_path)
  message("Rows: ", nrow(approved))
}
