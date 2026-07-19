# ==== 11_reference_layers_batch.r ============================================
##
## PURPOSE:
##   Manifest-driven preprocessor for a batch of simple reference/admin/
##   conservation layers that share the same general workflow.
##
## DESIGN:
##   This script preserves the project's modular structure by treating these
##   layers as one coherent family rather than as 11 unrelated one-off scripts.
##
## IMPORTANT:
##   Geometry simplification is NOT done here. The manifest's simplify_keep
##   field is carried forward and applied later in the core map-cache script.
##
## INPUT:
##   00_config/reference_layers_manifest.csv
##
## OUTPUT:
##   04_processed_data/rds/reference_<nickname>_wgs84.rds
##   04_processed_data/qa/reference_layers_batch_qa_<timestamp>.csv
##

# ==== 1. Load configuration and helper functions =============================

source("00_config/config_paths.r")
source("00_config/config_source_files.r")
source("03_functions/cache_helpers.r")
source("03_functions/spatial_helpers.r")

# ==== 2. Load packages =======================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(purrr)
  library(tibble)
})

# ==== 3. Run settings ========================================================

WRITE_QA <- TRUE
RUN_TS <- make_timestamp()

# ==== 4. Helper functions ====================================================

pt_manifest_none <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "na", "none", "None", "nonw")] <- NA_character_
  x
}

pt_parse_popup_fields <- function(spec) {
  
  spec <- pt_manifest_none(spec)
  
  if (length(spec) == 0 || is.na(spec)) {
    return(character(0))
  }
  
  out <- unlist(strsplit(spec, "\\\\n"))
  out <- trimws(out)
  out <- out[out != ""]
  out
}

pt_wsr_state_is_california <- function(x) {
  v <- toupper(trimws(as.character(x)))
  !is.na(v) & (
    v %in% c("CA", "CALIFORNIA") |
      grepl("(^|[,; /])CA($|[,; /])", v) |
      grepl("CALIFORNIA", v)
  )
}

pt_wsr_text_is_california <- function(...) {
  pieces <- list(...)
  if (!length(pieces)) return(logical(0))
  txt <- vapply(seq_along(pieces[[1]]), function(i) {
    paste(vapply(pieces, function(x) as.character(x[[i]]), character(1)), collapse = " | ")
  }, character(1))
  grepl("California|, CA\b|\bCA\b", txt, ignore.case = TRUE)
}

pt_reference_display_name <- function(nickname) {
  
  dplyr::case_when(
    nickname == "trails"              ~ "National Scenic/Historic Trails *",
    nickname == "monuments"           ~ "National Monuments *",
    nickname == "cadesert_ncl"        ~ "CA Desert National Conservation Lands",
    nickname == "wildernessstudyarea" ~ "Wilderness Study Areas *",
    nickname == "fedwilderness"       ~ "Federal Wilderness *",
    nickname == "drecp"               ~ "DRECP",
    nickname == "acec"                ~ "ACECs",
    nickname == "allotments"          ~ "Grazing Allotments",
    nickname == "wsr_blm_lines"       ~ "Wild & Scenic Rivers | BLM-CA lines",
    nickname == "wsr_segments"        ~ "Wild & Scenic Rivers | USFS/interagency segments",
    nickname == "wsr_corridor_blm"    ~ "Wild & Scenic River corridors | BLM-CA",
    nickname == "wsr_corridor_lsrs_area"   ~ "Wild & Scenic River corridors | USFS/LSRS areas",
    nickname == "wsr_corridor_lsrs_status" ~ "Wild & Scenic River legal-status corridors | USFS/LSRS",
    nickname == "gsps"                ~ "Groundwater Sustainability Plan Areas",
    nickname == "gwbasins_adjd"       ~ "Adjudicated Groundwater Basins",
    TRUE                              ~ nickname
  )
}

# ==== 5. Read and normalize manifest =========================================

if (!file.exists(SRC$reference_layers_manifest)) {
  stop("Missing manifest: ", SRC$reference_layers_manifest)
}

manifest <- readr::read_csv(
  SRC$reference_layers_manifest,
  show_col_types = FALSE
) |>
  dplyr::mutate(
    nickname      = trimws(.data$nickname),
    geom_type     = tolower(trimws(.data$geom_type)),
    filename      = trimws(.data$filename),
    folder        = trimws(.data$folder),
    namecolumn    = pt_manifest_none(.data$namecolumn),
    colorbycolumn = pt_manifest_none(.data$colorbycolumn),
    popup         = pt_manifest_none(.data$popup),
    label_field   = pt_manifest_none(.data$label_field),
    simplify_keep = as.numeric(.data$simplify_keep),
    display_name  = pt_reference_display_name(.data$nickname)
  )

if (any(is.na(manifest$nickname) | manifest$nickname == "")) {
  stop("Manifest contains blank nickname values.")
}

if (any(!manifest$geom_type %in% c("polygon", "polyline"))) {
  stop("Manifest geom_type must be either 'polygon' or 'polyline'.")
}

if (any(is.na(manifest$simplify_keep))) {
  stop("Manifest simplify_keep contains NA values.")
}

message("Reference-layer manifest rows: ", nrow(manifest))
print(manifest |> dplyr::select(nickname, geom_type, folder, filename, simplify_keep))

# ==== 6. Process each layer ==================================================

qa_rows <- vector("list", length = nrow(manifest))

for (i in seq_len(nrow(manifest))) {
  
  row <- manifest[i, , drop = FALSE]
  
  raw_path <- file.path(DIR$raw, row$folder, row$filename)
  
  if (!file.exists(raw_path)) {
    stop("Missing shapefile for reference layer ", row$nickname, ": ", raw_path)
  }
  
  message("\n------------------------------------------------------------")
  message("Processing reference layer: ", row$nickname)
  message("  file: ", raw_path)
  
  raw <- sf::st_read(raw_path, quiet = TRUE)
  
  popup_fields <- pt_parse_popup_fields(row$popup)
  
  required_fields <- unique(na.omit(c(
    row$namecolumn,
    row$colorbycolumn,
    row$label_field,
    popup_fields
  )))
  
  missing_fields <- setdiff(required_fields, names(raw))
  
  if (length(missing_fields) > 0) {
    stop(
      "Layer ", row$nickname, " is missing required field(s): ",
      paste(missing_fields, collapse = ", "),
      "\nAvailable fields: ",
      paste(names(raw), collapse = ", ")
    )
  }

  # Keep WSR local layers California-focused and lightweight.  The raw
  # USFS/interagency sources are national, but BRIM Local layers should remain
  # snappy and California-centered.  Hosted External/API layers can later carry
  # national/live source access if needed.
  if (row$nickname == "wsr_segments" && "STATE" %in% names(raw)) {
    raw_before <- nrow(raw)
    raw <- raw |>
      dplyr::filter(pt_wsr_state_is_california(.data$STATE))
    message("  filtered USFS/interagency WSR segments to California: ", nrow(raw), " of ", raw_before, " rows")
  }

  if (row$nickname == "wsr_corridor_lsrs_area") {
    raw_before <- nrow(raw)
    raw <- raw |>
      dplyr::filter(pt_wsr_text_is_california(.data$DESIGNATED, .data$RIVER))
    message("  filtered USFS/LSRS WSR areas to California text matches: ", nrow(raw), " of ", raw_before, " rows")
  }

  if (row$nickname == "wsr_corridor_lsrs_status") {
    raw_before <- nrow(raw)
    raw <- raw |>
      dplyr::filter(pt_wsr_text_is_california(.data$CASENAME, .data$AREANAME))
    message("  filtered USFS/LSRS WSR legal-status polygons to California text matches: ", nrow(raw), " of ", raw_before, " rows")
  }
  
  keep_fields <- unique(c(required_fields, "geometry"))
  
  layer <- raw |>
    dplyr::select(dplyr::any_of(keep_fields)) |>
    to_wgs84() |>
    clean_sf_for_leaflet() |>
    dplyr::mutate(
      pt_nickname      = row$nickname,
      pt_display_name  = row$display_name,
      pt_geom_type     = row$geom_type,
      pt_namecolumn    = ifelse(is.na(row$namecolumn), "", row$namecolumn),
      pt_colorbycolumn = ifelse(is.na(row$colorbycolumn), "", row$colorbycolumn),
      pt_popup_spec    = ifelse(is.na(row$popup), "", row$popup),
      pt_label_field   = ifelse(is.na(row$label_field), "", row$label_field),
      pt_simplify_keep = row$simplify_keep,
      source           = "Reference layer"
    )
  
  base_name <- paste0("reference_", row$nickname, "_wgs84")
  
  save_rds_cached(
    x = layer,
    timestamped_path = file.path(
      DIR$rds,
      timestamped_name(base_name, "rds", RUN_TS)
    ),
    latest_path = file.path(
      DIR$rds,
      paste0(base_name, ".rds")
    )
  )
  
  qa_rows[[i]] <- tibble::tibble(
    nickname = row$nickname,
    display_name = row$display_name,
    geom_type = row$geom_type,
    source_file = row$filename,
    rows = nrow(layer),
    retained_fields = paste(names(sf::st_drop_geometry(layer)), collapse = ", "),
    simplify_keep = row$simplify_keep,
    run_timestamp = RUN_TS
  )
}

# ==== 7. Save QA =============================================================

if (WRITE_QA) {
  
  qa <- dplyr::bind_rows(qa_rows)
  
  out_qa <- file.path(
    DIR$qa,
    paste0("reference_layers_batch_qa_", RUN_TS, ".csv")
  )
  
  readr::write_csv(qa, out_qa)
  
  message("\nSaved QA CSV: ", out_qa)
  print(qa)
}

# ==== 8. Final summary =======================================================

message("\nDone: reference layer batch preprocessing complete.")
message("Latest RDS files written to:")
message("  ", DIR$rds)