# ==== 23_repair_pt2_external_overlay_catalog_bad_urls.r =======================
##
## PURPOSE:
##   Repair a specific external-overlay catalog problem found during the
##   field-curation QA pass: many DWR rows can accidentally point to the same
##   DWR groundwater-depth-contours proxy URL even though their source_page and
##   legend_url point to the intended service.
##
## WHY:
##   If service_url is wrong, the metadata/field-curation sheet will falsely show
##   the same contour fields for unrelated DWR rows, such as GSA/GSP areas,
##   water districts, hydrologic regions, C2VSim, etc.
##
## INPUT:
##   00_config/external_service_catalog.csv
##
## OUTPUTS:
##   00_config/external_service_catalog.csv
##   00_config/external_service_catalog_backup_YYYYMMDD_HHMMSS.csv
##   04_processed_data/qa/pt2_external_overlay_catalog_url_repair_audit_YYYYMMDD_HHMMSS.csv
##

# ==== 1. User switches ========================================================

DRY_RUN <- TRUE

# ==== 2. Packages =============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

# ==== 3. Paths ================================================================

catalog_path <- file.path("00_config", "external_service_catalog.csv")

qa_dir <- file.path("04_processed_data", "qa")
dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

backup_path <- file.path(
  "00_config",
  paste0("external_service_catalog_backup_", timestamp, ".csv")
)

audit_path <- file.path(
  qa_dir,
  paste0("pt2_external_overlay_catalog_url_repair_audit_", timestamp, ".csv")
)

# ==== 4. Known bad URL pattern ===============================================

bad_contour_url <- paste0(
  "https://utility.arcgis.com/usrsvcs/servers/",
  "15e2bef61bdb4cabb2719c3147211049",
  "/rest/services/Geoscientific/i08_GroundwaterDepthSeasonal_Contours/MapServer/0"
)

looks_direct_service_url <- function(x) {
  x <- stringr::str_trim(as.character(x))
  xl <- stringr::str_to_lower(x)

  (
    stringr::str_detect(xl, "/arcgis/rest/services/") |
      stringr::str_detect(xl, "/portalserver/rest/services/")
  ) &
    (
      stringr::str_detect(xl, "/mapserver") |
        stringr::str_detect(xl, "/featureserver")
    )
}

# ==== 5. Read catalog =========================================================

if (!file.exists(catalog_path)) {
  stop("Catalog not found: ", catalog_path)
}

cat0 <- readr::read_csv(
  catalog_path,
  show_col_types = FALSE,
  progress = FALSE
) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

required <- c("display_name", "agency", "service_url", "source_page")

missing_required <- setdiff(required, names(cat0))

if (length(missing_required) > 0) {
  stop(
    "Catalog is missing required columns: ",
    paste(missing_required, collapse = ", ")
  )
}

# ==== 6. Detect and repair ====================================================

cat1 <- cat0 |>
  dplyr::mutate(
    service_url_original = service_url,
    repair_action = dplyr::case_when(
      stringr::str_to_lower(stringr::str_trim(service_url)) ==
        stringr::str_to_lower(bad_contour_url) &
        looks_direct_service_url(source_page) &
        stringr::str_to_lower(stringr::str_trim(source_page)) !=
          stringr::str_to_lower(stringr::str_trim(service_url)) ~
        "replace_service_url_with_source_page",
      TRUE ~ "none"
    ),
    repair_reason = dplyr::case_when(
      repair_action == "replace_service_url_with_source_page" ~
        "service_url matched known bad DWR groundwater-depth-contours proxy URL while source_page was a direct REST service URL",
      TRUE ~ ""
    ),
    service_url = dplyr::if_else(
      repair_action == "replace_service_url_with_source_page",
      stringr::str_trim(source_page),
      service_url
    )
  )

audit <- cat1 |>
  dplyr::filter(repair_action != "none") |>
  dplyr::transmute(
    agency,
    display_name,
    old_service_url = service_url_original,
    new_service_url = service_url,
    repair_action,
    repair_reason
  )

message("Rows scanned: ", nrow(cat1))
message("Rows to repair: ", nrow(audit))

if (nrow(audit) > 0) {
  print(audit |> dplyr::select(agency, display_name, repair_action), n = 100)
}

# ==== 7. Write outputs ========================================================

if (isTRUE(DRY_RUN)) {
  message("")
  message("DRY_RUN = TRUE. No files written.")
  message("Set DRY_RUN <- FALSE after reviewing the printed repair list.")
} else {

  file.copy(catalog_path, backup_path, overwrite = TRUE)

  readr::write_csv(audit, audit_path)

  cat_out <- cat1 |>
    dplyr::select(-service_url_original, -repair_action, -repair_reason)

  readr::write_csv(cat_out, catalog_path)

  message("Backup written:")
  message("  ", backup_path)
  message("Audit written:")
  message("  ", audit_path)
  message("Repaired catalog written:")
  message("  ", catalog_path)
}
