# ==== 25_normalize_pt2_external_overlay_catalog_agencies.r ====================
##
## PURPOSE:
##   Normalize agency/source names in the external-overlay catalog so the
##   Tools / External Overlays panel does not show near-duplicate groups such as:
##
##     CNRA
##     DWR / CNRA
##
## DEFAULT NORMALIZATION:
##   CNRA -> DWR / CNRA
##
## INPUT:
##   00_config/external_service_catalog.csv
##
## OUTPUTS:
##   00_config/external_service_catalog.csv
##   00_config/external_service_catalog_backup_YYYYMMDD_HHMMSS.csv
##   04_processed_data/qa/pt2_external_overlay_catalog_agency_normalization_audit_YYYYMMDD_HHMMSS.csv
##

# ==== 1. User switches ========================================================

DRY_RUN <- FALSE

NORMALIZE_FIELD_CURATION_QA_TOO <- FALSE

# ==== 2. Packages =============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
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

catalog_backup_path <- file.path(
  "00_config",
  paste0("external_service_catalog_backup_", timestamp, ".csv")
)

field_qa_backup_path <- file.path(
  qa_dir,
  paste0("pt2_external_overlay_field_curation_test_sheet_backup_", timestamp, ".csv")
)

audit_path <- file.path(
  qa_dir,
  paste0("pt2_external_overlay_catalog_agency_normalization_audit_", timestamp, ".csv")
)

# ==== 4. Normalization rule ===================================================

normalize_agency <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    stringr::str_trim(x) == "CNRA" ~ "DWR / CNRA",
    TRUE ~ x
  )
}

normalize_file <- function(path, backup_path = NULL) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  df0 <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

  if (!"agency" %in% names(df0)) {
    stop("File does not contain an agency column: ", path)
  }

  df1 <- df0 |>
    dplyr::mutate(
      agency_original = agency,
      agency = normalize_agency(agency),
      agency_changed = agency != agency_original
    )

  audit <- df1 |>
    dplyr::filter(agency_changed) |>
    dplyr::transmute(
      file = path,
      display_name = dplyr::if_else("display_name" %in% names(df1), display_name, ""),
      service_url = dplyr::if_else("service_url" %in% names(df1), service_url, ""),
      agency_original,
      agency_new = agency
    )

  out <- df1 |>
    dplyr::select(-agency_original, -agency_changed)

  list(data = out, audit = audit, backup_path = backup_path)
}

# ==== 5. Run normalization ====================================================

targets <- list(
  catalog = normalize_file(catalog_path, catalog_backup_path)
)

if (isTRUE(NORMALIZE_FIELD_CURATION_QA_TOO) && file.exists(field_qa_path)) {
  targets$field_qa <- normalize_file(field_qa_path, field_qa_backup_path)
}

audit <- dplyr::bind_rows(purrr::map(targets, "audit"))

message("Agency normalization changes found: ", nrow(audit))

if (nrow(audit) > 0) {
  print(audit |> dplyr::select(file, display_name, agency_original, agency_new), n = 100)
}

if (isTRUE(DRY_RUN)) {
  message("")
  message("DRY_RUN = TRUE. No files written.")
  message("Set DRY_RUN <- FALSE after reviewing the printed normalization list.")
} else {

  readr::write_csv(audit, audit_path)

  for (nm in names(targets)) {
    item <- targets[[nm]]

    if (!is.null(item$backup_path) && item$backup_path != "") {
      file.copy(
        from = if (nm == "catalog") catalog_path else field_qa_path,
        to = item$backup_path,
        overwrite = TRUE
      )
    }

    write_path <- if (nm == "catalog") catalog_path else field_qa_path
    readr::write_csv(item$data, write_path)

    message("Normalized file written:")
    message("  ", write_path)
  }

  message("Audit written:")
  message("  ", audit_path)
}
