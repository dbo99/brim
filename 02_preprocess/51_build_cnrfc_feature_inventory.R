# ==== 51_build_cnrfc_feature_inventory.R ====================================
##
## PURPOSE:
##   Promote the tested CNRFC product-intelligence inventory outputs into
##   stable preprocessed BRIM products.
##
## DESIGN NOTE:
##   This is the first durable preprocessor version of the CNRFC feature
##   inventory work.  For now it intentionally reuses the QA inventory outputs
##   produced by qa_cnrfc_product_intelligence_audit(), because that broad audit
##   is already tested and has useful diagnostics.  Later, if desired, the broad
##   inventory build logic can be moved fully out of qa/ and into this file.
##
## OUTPUTS:
##   04_processed_data/rds/cnrfc_feature_inventory.rds
##   04_processed_data/rds/cnrfc_feature_master.rds
##   04_processed_data/rds/cnrfc_id_collision_check.rds
##   04_processed_data/rds/cnrfc_raw_source_summary.rds
##   04_processed_data/rds/cnrfc_field_inventory.rds
##   04_processed_data/rds/cnrfc_feature_inventory_manifest.rds
##
## OPTIONAL:
##   Set option BRIM_CNRFC_FEATURE_REBUILD_QA = TRUE to rerun the broad QA
##   inventory first.  Default is FALSE so this preprocessor is fast and reuses
##   the newest reviewed QA CSVs.

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
})

RUN_TS <- make_timestamp()

dir.create(DIR$rds, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR$qa, showWarnings = FALSE, recursive = TRUE)

pt_log <- function(...) {
  message(format(Sys.time(), "%H:%M:%S"), " | ", ...)
}

pt_latest_file <- function(pattern, dir = DIR$qa) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  files[order(file.info(files)$mtime, decreasing = TRUE)][[1]]
}

pt_read_or_empty <- function(path) {
  if (is.na(path) || !file.exists(path)) return(tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

pt_require_file <- function(path, label) {
  if (is.na(path) || !file.exists(path)) {
    stop(
      "Missing ", label, " input. Run qa_cnrfc_product_intelligence_audit() ",
      "or rerun this preprocessor with rebuild_qa = TRUE."
    )
  }
  invisible(path)
}

# ==== 2. Optional broad QA rebuild ==========================================

REBUILD_QA <- isTRUE(getOption("BRIM_CNRFC_FEATURE_REBUILD_QA", FALSE))

if (REBUILD_QA) {
  qa_script <- "qa/qa_cnrfc_product_intelligence_audit.r"
  if (!file.exists(qa_script)) {
    stop("Cannot rebuild CNRFC feature inventory; missing script: ", qa_script)
  }
  pt_log("Rebuilding broad CNRFC feature inventory via QA script...")
  source(qa_script, local = FALSE)
} else {
  pt_log("Reusing latest broad CNRFC feature-inventory QA outputs.")
}

# ==== 3. Locate latest QA inventory outputs =================================

feature_inventory_path <- pt_latest_file("^cnrfc_feature_inventory_[0-9_]+\\.csv$")
feature_master_path <- pt_latest_file("^cnrfc_feature_master_[0-9_]+\\.csv$")
id_collision_path <- pt_latest_file("^cnrfc_id_collision_check_[0-9_]+\\.csv$")
raw_source_summary_path <- pt_latest_file("^cnrfc_raw_source_summary_[0-9_]+\\.csv$")
field_inventory_path <- pt_latest_file("^cnrfc_field_inventory_[0-9_]+\\.csv$")
product_group_bins_path <- pt_latest_file("^cnrfc_product_group_bins_by_feature_[0-9_]+\\.csv$")

pt_require_file(feature_inventory_path, "feature inventory")
pt_require_file(feature_master_path, "feature master")
pt_require_file(id_collision_path, "ID collision check")

# ==== 4. Read tables =========================================================

pt_log("Reading latest CNRFC inventory tables...")

feature_inventory <- pt_read_or_empty(feature_inventory_path)
feature_master <- pt_read_or_empty(feature_master_path)
id_collision_check <- pt_read_or_empty(id_collision_path)
raw_source_summary <- pt_read_or_empty(raw_source_summary_path)
field_inventory <- pt_read_or_empty(field_inventory_path)
product_group_bins_by_feature <- pt_read_or_empty(product_group_bins_path)

manifest <- tibble(
  run_timestamp = RUN_TS,
  product = c(
    "feature_inventory",
    "feature_master",
    "id_collision_check",
    "raw_source_summary",
    "field_inventory",
    "product_group_bins_by_feature"
  ),
  source_csv = c(
    feature_inventory_path,
    feature_master_path,
    id_collision_path,
    raw_source_summary_path,
    field_inventory_path,
    product_group_bins_path
  ),
  rows = c(
    nrow(feature_inventory),
    nrow(feature_master),
    nrow(id_collision_check),
    nrow(raw_source_summary),
    nrow(field_inventory),
    nrow(product_group_bins_by_feature)
  )
)

# ==== 5. Write stable preprocessed products =================================

out_feature_inventory <- file.path(DIR$rds, "cnrfc_feature_inventory.rds")
out_feature_master <- file.path(DIR$rds, "cnrfc_feature_master.rds")
out_id_collision <- file.path(DIR$rds, "cnrfc_id_collision_check.rds")
out_raw_summary <- file.path(DIR$rds, "cnrfc_raw_source_summary.rds")
out_field_inventory <- file.path(DIR$rds, "cnrfc_field_inventory.rds")
out_product_bins <- file.path(DIR$rds, "cnrfc_product_group_bins_by_feature.rds")
out_manifest <- file.path(DIR$rds, "cnrfc_feature_inventory_manifest.rds")

saveRDS(feature_inventory, out_feature_inventory)
saveRDS(feature_master, out_feature_master)
saveRDS(id_collision_check, out_id_collision)
saveRDS(raw_source_summary, out_raw_summary)
saveRDS(field_inventory, out_field_inventory)
saveRDS(product_group_bins_by_feature, out_product_bins)
saveRDS(manifest, out_manifest)

# Keep a small human-readable manifest in QA.
out_manifest_csv <- file.path(DIR$qa, paste0("cnrfc_feature_inventory_preprocess_manifest_", RUN_TS, ".csv"))
readr::write_csv(manifest, out_manifest_csv)

# ==== 6. Console summary =====================================================

message("CNRFC feature inventory preprocess complete.")
message("  Feature inventory RDS: ", out_feature_inventory, " (", nrow(feature_inventory), " rows)")
message("  Feature master RDS:    ", out_feature_master, " (", nrow(feature_master), " rows)")
message("  ID collision RDS:      ", out_id_collision, " (", nrow(id_collision_check), " rows)")
message("  Manifest CSV:          ", out_manifest_csv)
message("")
message("Feature master counts by type:")
if ("feature_type" %in% names(feature_master)) {
  print(feature_master |> count(.data$feature_type, name = "features", sort = TRUE), n = 40)
} else {
  message("  feature_type column not found in feature_master.")
}
