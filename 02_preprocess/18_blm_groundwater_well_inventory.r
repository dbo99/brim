# ==== 18_blm_groundwater_well_inventory.r ===================================
##
## PURPOSE:
##   Normalize two small BLM groundwater-well source datasets into durable,
##   map-ready WGS84 RDS files for future BRIM Local layers:
##
##     1. BLM NOC well inventory
##        01_raw_data/blm/NOC_BLMdrilled.csv
##
##     2. Mojave limited field inventory (2025)
##        00_config/blm_well_inventory/ (corrected master, measurements, corrections)
##
##   A legacy auxiliary field list is retained in QA (not a dynamic popup selector):
##        01_raw_data/blm/NOC_Albion_fields.csv
##
## WHY THIS IS A SEPARATE PREPROCESSOR:
##   These sources are small, but they have messy field names, long comments,
##   source-specific status codes, and some known QA issues.  Normalizing them
##   once here keeps the final Leaflet build boring and consistent with BRIM's
##   other Local Monitoring Sites / Records layers.
##
## IMPORTANT USER-FACING NAMING:
##   - Do not expose the internal contractor shorthand "Albion" in the map UI.
##   - Local panel: "GW sites | 2025 Mojave limited field inventory" (record count added by the UI).
##     Legend/source: "Mojave limited field inventory (2025)".
##
## CURRENT DESIGN DECISIONS:
##   - This script does NOT add visible Leaflet layers.  It only writes clean,
##     map-ready data and QA files.  Browser-managed layers, legends, BLM
##     distance filters, and inline lbl checkboxes are added in later patches.
##   - BLM-distance fields are NOT calculated here.  A later numbered distance
##     preprocessor will calculate current on/off BLM and distance-to-BLM from
##     BRIM's canonical current BLM managed-lands RDS.
##   - The reviewed Mojave master owns retained sites; correction actions retain
##     historical identity and excluded baseline duplicates for QA.
##   - Hover line 1 is the best available Well Name.  For NOC records, hover
##     line 2 is the well-completion date when available; for the 2025 Mojave
##     records, there is no installation/completion date field, so hover line 2
##     remains depth to water when available.
##
## INPUTS:
##   NOC and the legacy fields list use ordinary CSV ingestion in 01_raw_data/blm/.
##   Corrected Mojave inputs are required exact files in 00_config/blm_well_inventory/.
##   No historical Albion, processed-output, or retained-NOC input fallback exists.
##
## OUTPUTS:
##   Map-ready RDS:
##     04_processed_data/rds/blm_noc_drilled_wells_wgs84.rds
##     04_processed_data/rds/mojave_2025_gw_well_inventory_wgs84.rds
##     04_processed_data/rds/blm_gw_well_inventory_combined_wgs84.rds
##
##   QA / audit CSVs:
##     04_processed_data/qa/blm_gw_well_inventory_source_summary_latest.csv
##     04_processed_data/qa/blm_gw_well_inventory_coordinate_qa_latest.csv
##     04_processed_data/qa/blm_gw_well_inventory_status_counts_latest.csv
##     04_processed_data/qa/blm_gw_well_inventory_field_summary_latest.csv
##     04_processed_data/qa/blm_gw_well_inventory_duplicate_exclusions_latest.csv
##     04_processed_data/qa/blm_gw_well_inventory_popup_field_guide_latest.csv
##
## HOW TO RUN FROM PORTATREASURE2 ROOT:
##   source("02_preprocess/18_blm_groundwater_well_inventory.r")
## ============================================================================


# ==== 1. Load configuration and packages ====================================

source("00_config/config_paths.r")
source("03_functions/spatial_helpers.r")

required_pkgs <- c("dplyr", "readr", "stringr", "tibble", "sf", "janitor")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Missing required package(s): ", paste(missing_pkgs, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(sf)
  library(janitor)
})


# ==== 2. User-facing switches ===============================================

WRITE_RDS <- TRUE
WRITE_QA <- TRUE

# Broad coordinate QA bounds.  These are intentionally broad enough to keep
# California-adjacent BLM administrative records near the CA boundary, but they
# flag/exclude records such as the known NOC Sagebrush Well in Kansas/Nebraska/
# New-Mexico-ish longitudes that clearly do not belong in BRIM-California.
BROAD_CA_LON_MIN <- -125
BROAD_CA_LON_MAX <- -113
BROAD_CA_LAT_MIN <- 30
BROAD_CA_LAT_MAX <- 43


# ==== 3. Input/output paths ==================================================

RAW_BLM_DIR <- file.path(DIR$raw, "blm")

OUT_NOC_RDS <- file.path(DIR$rds, "blm_noc_drilled_wells_wgs84.rds")
OUT_MOJAVE_RDS <- file.path(DIR$rds, "mojave_2025_gw_well_inventory_wgs84.rds")
OUT_COMBINED_RDS <- file.path(DIR$rds, "blm_gw_well_inventory_combined_wgs84.rds")

OUT_SOURCE_SUMMARY_CSV <- file.path(DIR$qa, "blm_gw_well_inventory_source_summary_latest.csv")
OUT_COORD_QA_CSV <- file.path(DIR$qa, "blm_gw_well_inventory_coordinate_qa_latest.csv")
OUT_STATUS_COUNTS_CSV <- file.path(DIR$qa, "blm_gw_well_inventory_status_counts_latest.csv")
OUT_FIELD_SUMMARY_CSV <- file.path(DIR$qa, "blm_gw_well_inventory_field_summary_latest.csv")
OUT_DUPLICATE_EXCLUSIONS_CSV <- file.path(DIR$qa, "blm_gw_well_inventory_duplicate_exclusions_latest.csv")
OUT_POPUP_FIELD_GUIDE_CSV <- file.path(DIR$qa, "blm_gw_well_inventory_popup_field_guide_latest.csv")


# ==== 4. Helper functions ====================================================

pt_msg_048a <- function(...) {
  message(format(Sys.time(), "%H:%M:%S"), " | ", paste0(..., collapse = ""))
}

pt_find_raw_csv_048a <- function(patterns, label) {
  if (!dir.exists(RAW_BLM_DIR)) {
    stop("Missing raw BLM data folder: ", RAW_BLM_DIR, call. = FALSE)
  }
  all_csv <- list.files(RAW_BLM_DIR, pattern = "\\.csv$", full.names = TRUE)
  if (length(all_csv) == 0) {
    stop("No CSV files found in raw BLM data folder: ", RAW_BLM_DIR, call. = FALSE)
  }
  bn <- basename(all_csv)
  for (pat in patterns) {
    hit <- all_csv[grepl(pat, bn, ignore.case = TRUE)]
    if (length(hit) > 0) return(hit[[1]])
  }
  stop(
    "Could not find ", label, " CSV in ", RAW_BLM_DIR,
    ". Looked for pattern(s): ", paste(patterns, collapse = ", "),
    call. = FALSE
  )
}

pt_strip_bom_names_048a <- function(x) {
  nm <- names(x)
  # Files moved between Excel/Windows/R sometimes carry a UTF-8 BOM that can
  # appear either as the actual BOM or the Latin-1 mojibake sequence "ï»¿".
  nm <- sub("^\\ufeff", "", nm)
  nm <- sub("^ï»¿", "", nm)
  names(x) <- nm
  x
}

pt_read_csv_latin1_048a <- function(path) {
  # Historical name retained to avoid a noisy patch.  The function now tries
  # UTF-8 first because the NOC export is UTF-8 with a BOM.  Reading that file
  # as Latin1 turns punctuation such as en dashes into mojibake (for example,
  # "Not cased â€“ Open hole completion").  The popup field-guide CSV may still
  # need Latin1, so fall back only when UTF-8 parsing fails.
  common_args <- list(
    file = path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("", "NA", "N/A", "<Null>", "NULL", "null"),
    show_col_types = FALSE,
    progress = FALSE
  )

  out <- tryCatch(
    do.call(readr::read_csv, c(common_args, list(locale = readr::locale(encoding = "UTF-8")))),
    error = function(e) NULL
  )

  if (is.null(out)) {
    out <- do.call(readr::read_csv, c(common_args, list(locale = readr::locale(encoding = "Latin1"))))
  }

  pt_strip_bom_names_048a(out)
}

pt_clean_chr_048a <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\u00a0", " ")
  # Clean common UTF-8-as-Latin1/Windows mojibake sequences defensively.  With
  # the auto-encoding reader above these should be rare, but this prevents
  # ugly strings such as "â€“" from reaching hovers/popups if a future export is
  # already partially mojibaked upstream.
  x <- stringr::str_replace_all(x, "\u00e2\u0080\u0093|\u00e2\u20ac\u201c", " - ")
  x <- stringr::str_replace_all(x, "\u00e2\u0080\u0094|\u00e2\u20ac\u201d", " - ")
  x <- stringr::str_replace_all(x, "\u00e2\u0080\u0098|\u00e2\u20ac\u02dc", "'")
  x <- stringr::str_replace_all(x, "\u00e2\u0080\u0099|\u00e2\u20ac\u2122", "'")
  x <- stringr::str_replace_all(x, "\u00e2\u0080\u009c|\u00e2\u20ac\u0153", '"')
  x <- stringr::str_replace_all(x, "\u00e2\u0080\u009d|\u00e2\u20ac\u009d", '"')
  x <- stringr::str_squish(x)
  x[x %in% c("", "NA", "N/A", "<Null>", "NULL", "null", "NaN")] <- NA_character_
  x
}

pt_num_048a <- function(x) {
  suppressWarnings(as.numeric(stringr::str_replace_all(as.character(x), ",", "")))
}

pt_first_nonblank_048a <- function(...) {
  vals <- list(...)
  if (length(vals) == 0) return(NA_character_)
  out <- pt_clean_chr_048a(vals[[1]])
  if (length(vals) > 1) {
    for (i in 2:length(vals)) {
      cand <- pt_clean_chr_048a(vals[[i]])
      out <- dplyr::coalesce(out, cand)
    }
  }
  out
}

pt_fmt_number_048a <- function(x, digits = 1) {
  v <- pt_num_048a(x)
  ifelse(
    is.na(v),
    NA_character_,
    formatC(v, format = "f", digits = digits, big.mark = ",")
  )
}

pt_fmt_ft_048a <- function(x, digits = 1) {
  raw <- pt_clean_chr_048a(x)
  v <- pt_num_048a(raw)
  dplyr::case_when(
    is.na(raw) ~ NA_character_,
    !is.na(v) ~ paste0(formatC(v, format = "f", digits = digits, big.mark = ","), " ft"),
    TRUE ~ raw
  )
}

pt_fmt_gpm_048a <- function(x, digits = 1) {
  v <- pt_num_048a(x)
  ifelse(
    is.na(v),
    NA_character_,
    paste0(formatC(v, format = "f", digits = digits, big.mark = ","), " gpm")
  )
}

pt_fmt_coord_048a <- function(x, digits = 6) {
  # Keep popup coordinates compact and copyable.  This helper is intentionally
  # vectorized so it can be used inside mapply() when building one popup per row.
  v <- pt_num_048a(x)
  ifelse(
    is.na(v),
    NA_character_,
    formatC(v, format = "f", digits = digits)
  )
}

pt_fmt_date_only_048a <- function(x) {
  # NOC exports date/time strings for some date fields.  For BRIM popups, the
  # time portion is noise, so keep only the date when a recognizable date is
  # present.  Some Excel exports also contain a long run of # characters when a
  # date cell is too narrow; those are not real dates and should display blank.
  raw <- pt_clean_chr_048a(x)
  raw[!is.na(raw) & grepl("^#+$", raw)] <- NA_character_
  ymd <- stringr::str_extract(raw, "\\d{4}[-/]\\d{1,2}[-/]\\d{1,2}")
  mdy <- stringr::str_extract(raw, "\\d{1,2}/\\d{1,2}/\\d{2,4}")
  out <- dplyr::coalesce(ymd, mdy)
  out <- stringr::str_replace_all(out, "^([0-9]{4})/([0-9]{1,2})/([0-9]{1,2})$", "\\1-\\2-\\3")
  out[out %in% c("", "NA", "N/A", "<Null>", "NULL", "null", "NaN")] <- NA_character_
  out
}

pt_clean_noc_depth_to_water_048a <- function(x) {
  # NOC currently has a single reported 0-ft static water-level value that does
  # not look reliable for BRIM screening.  Per user direction, treat numeric
  # zero values as missing for display/filter/hover purposes while leaving the
  # original raw CSV untouched.
  raw <- pt_clean_chr_048a(x)
  v <- pt_num_048a(raw)
  raw[!is.na(v) & abs(v) < 1e-9] <- NA_character_
  raw
}

pt_normalize_noc_elevation_source_048a <- function(x) {
  # Keep the popup wording compact and less awkward than the raw NOC value
  # "Interpolated from DEM".  The source remains NOC-provided; this is only a
  # display label cleanup.
  raw <- pt_clean_chr_048a(x)
  out <- raw
  out[!is.na(raw) & grepl("dem", raw, ignore.case = TRUE)] <- "DEM"
  out
}

pt_qa_value_048a <- function(x) {
  # Compact QA-line display: preserve values like raw Well ID = 0, but mark
  # source blanks explicitly instead of silently omitting them.
  out <- pt_clean_chr_048a(x)
  ifelse(is.na(out), "missing", out)
}

pt_popup_qa_line_048a <- function(label, values) {
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0) return("")
  paste0(
    "<div class='pt2-popup-qa' style='font-size:11px;color:#666;margin-top:5px;line-height:1.25;'><b>",
    pt_html_escape_048a(label),
    ":</b> ",
    pt_html_escape_048a(paste(values, collapse = " · ")),
    "</div>"
  )
}

pt_html_escape_048a <- function(x) {
  x <- pt_clean_chr_048a(x)
  x <- ifelse(is.na(x), "", x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

pt_popup_rows_048a <- function(labels, values, long_labels = character(0)) {
  keep <- !is.na(values) & nzchar(values)
  labels <- labels[keep]
  values <- values[keep]
  if (length(values) == 0) return(NA_character_)
  rows <- vapply(
    seq_along(values),
    function(i) {
      label <- labels[[i]]
      value <- values[[i]]
      if (label %in% long_labels) {
        paste0(
          "<div class='pt2-popup-row pt2-popup-row-long'><b>",
          pt_html_escape_048a(label),
          ":</b><br>",
          pt_html_escape_048a(value),
          "</div>"
        )
      } else {
        paste0(
          "<div class='pt2-popup-row'><b>",
          pt_html_escape_048a(label),
          ":</b> ",
          pt_html_escape_048a(value),
          "</div>"
        )
      }
    },
    character(1)
  )
  paste(rows, collapse = "")
}

pt_coord_status_048a <- function(lon, lat) {
  dplyr::case_when(
    is.na(lon) | is.na(lat) ~ "missing/non-numeric coordinate",
    lon < BROAD_CA_LON_MIN | lon > BROAD_CA_LON_MAX |
      lat < BROAD_CA_LAT_MIN | lat > BROAD_CA_LAT_MAX ~ "outside broad BRIM-CA lon/lat range",
    TRUE ~ "ok"
  )
}

pt_safe_col_048a <- function(df, col, default = NA_character_) {
  if (col %in% names(df)) df[[col]] else rep(default, nrow(df))
}


pt_write_csv_048a <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, path, na = "")
  pt_msg_048a("Wrote QA: ", path)
}


# Pure attribute functions: QA extracts these definitions without sourcing this script.
pt_normalize_noc_attributes_048a <- function(noc_raw, noc_csv) {
  required_noc_cols <- c("objectid", "well_name", "longitude", "latitude")
  missing_noc_cols <- setdiff(required_noc_cols, names(noc_raw))
  if (length(missing_noc_cols) > 0) {
    stop(
      "BLM NOC well inventory CSV is missing expected cleaned column(s): ",
      paste(missing_noc_cols, collapse = ", "),
      ". Available cleaned columns: ", paste(names(noc_raw), collapse = ", "),
      call. = FALSE
    )
  }

  noc_attr_all <- noc_raw |>
    dplyr::mutate(
      raw_source_file = basename(noc_csv),
      source_key = "noc_blm_drilled",
      source_display = "BLM National Operations Center",
      source_short = "NOC",
      layer_name = "GW wells | BLM NOC inventory",
      record_uid = paste0("noc_", dplyr::row_number()),
      source_record_id = pt_first_nonblank_048a(.data$objectid, .data$global_id, .data$well_uuid),
      # Preserve the raw NOC Well ID for the lower QA line, but do not use it
      # as the public-facing title/hover fallback.  Many records have Well ID = 0,
      # which is too GIS-y and too ambiguous for users.
      well_id_raw = pt_clean_chr_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "well_id")),
      well_id = ifelse(.data$well_id_raw %in% c("0", "0.0", "0.00"), NA_character_, .data$well_id_raw),
      well_name_raw = pt_clean_chr_048a(.data$well_name),
      facility_name = pt_clean_chr_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "facility_name")),
      well_name_display = pt_first_nonblank_048a(
        .data$well_name_raw,
        .data$facility_name
      ),
      well_name_display = ifelse(is.na(.data$well_name_display), "Unnamed well", .data$well_name_display),
      hover_line1 = .data$well_name_display,
      longitude = pt_num_048a(.data$longitude),
      latitude = pt_num_048a(.data$latitude),
      coord_status = pt_coord_status_048a(.data$longitude, .data$latitude),
      well_owner = pt_clean_chr_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "well_owner")),
      site_location_reliability = pt_clean_chr_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "site_location_reliability")),
      elevation_ft = pt_num_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "elevation_at_well_feet")),
      elevation_display = pt_fmt_ft_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "elevation_at_well_feet"), digits = 0),
      elevation_source = pt_normalize_noc_elevation_source_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "elevation_source")),
      well_completion_date = pt_fmt_date_only_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "well_completion_date")),
      initial_depth_ft = pt_num_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "initial_depth")),
      initial_depth_display = pt_fmt_ft_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "initial_depth"), digits = 1),
      screened_interval = pt_clean_chr_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "screened_interval")),
      depth_to_water_raw = pt_clean_noc_depth_to_water_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "initial_static_water_level")),
      depth_to_water_display = pt_fmt_ft_048a(.data$depth_to_water_raw, digits = 1),
      hover_line2 = .data$well_completion_date,
      initial_yield_gpm = pt_num_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "initial_yield_gpm")),
      initial_yield_display = pt_fmt_gpm_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "initial_yield_gpm"), digits = 1),
      casing_diameter_inches = pt_num_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "casing_diameter_inches")),
      casing_diameter_display = ifelse(
        is.na(.data$casing_diameter_inches),
        NA_character_,
        paste0(formatC(.data$casing_diameter_inches, format = "f", digits = 1), " in")
      ),
      well_casing_type = pt_clean_chr_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "well_casing_type")),
      use_of_well = pt_clean_chr_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "use_of_well")),
      primary_use_of_water = pt_clean_chr_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "primary_use_of_water")),
      current_historical_grazing_allotment = pt_clean_chr_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "blm_grazing_allotment")),
      producing_aquifer_description = pt_clean_chr_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "producing_aquifer_description")),
      driller_name_and_city = pt_clean_chr_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "driller_name_and_city")),
      well_comments = pt_clean_chr_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "well_comments")),
      well_present_key = "not_applicable",
      well_present_display = NA_character_,
      well_monitored_key = "unknown",
      well_monitored_display = NA_character_,
      depth_to_water_sort_ft = pt_num_048a(.data$depth_to_water_raw),
      data_note = "Generally NOC-provided data; BRIM made limited display/QC edits.",
      popup_html = mapply(
        FUN = function(
          well_name_display, source_display, data_note, well_owner, site_location_reliability,
          elevation_display, elevation_source, well_completion_date, initial_depth_display,
          screened_interval, depth_to_water_display, initial_yield_display,
          casing_diameter_display, well_casing_type, use_of_well, primary_use_of_water,
          current_historical_grazing_allotment, producing_aquifer_description,
          driller_name_and_city, well_comments, latitude, longitude, source_record_id,
          well_id_raw, well_name_raw
        ) {
          # IMPORTANT:
          # Build popup HTML one record at a time.  Do not pass whole columns into
          # pt_popup_rows_048a(); doing that creates one enormous all-record popup
          # and then repeats it for every feature, which bloats the standalone HTML
          # and makes popups unusably tall.
          paste0(
            "<div class='pt2-popup-title'>", pt_html_escape_048a(well_name_display), "</div>",
            pt_popup_rows_048a(
              labels = c(
                "Source", "Data note", "Well owner", "Site location reliability", "Elevation", "Elevation source",
                "Well completion date", "Initial depth", "Screened interval", "Depth to water",
                "Initial yield", "Casing diameter", "Well casing type", "Use of well",
                "Primary use of water", "Current/historical grazing allotment",
                "Producing aquifer description", "Driller", "Well comments", "Latitude", "Longitude"
              ),
              values = c(
                source_display, data_note, well_owner, site_location_reliability,
                elevation_display, elevation_source, well_completion_date,
                initial_depth_display, screened_interval, depth_to_water_display,
                initial_yield_display, casing_diameter_display, well_casing_type,
                use_of_well, primary_use_of_water,
                current_historical_grazing_allotment,
                producing_aquifer_description, driller_name_and_city,
                well_comments, pt_fmt_coord_048a(latitude), pt_fmt_coord_048a(longitude)
              ),
              long_labels = c("Producing aquifer description", "Well comments")
            ),
            pt_popup_qa_line_048a(
              "NOC QA IDs",
              c(
                paste0("GIS OBJECTID ", pt_qa_value_048a(source_record_id)),
                paste0("Well ID ", pt_qa_value_048a(well_id_raw)),
                paste0("Well name ", pt_qa_value_048a(well_name_raw))
              )
            )
          )
        },
        .data$well_name_display, .data$source_display, .data$data_note,
        .data$well_owner, .data$site_location_reliability, .data$elevation_display,
        .data$elevation_source, .data$well_completion_date,
        .data$initial_depth_display, .data$screened_interval,
        .data$depth_to_water_display, .data$initial_yield_display,
        .data$casing_diameter_display, .data$well_casing_type,
        .data$use_of_well, .data$primary_use_of_water,
        .data$current_historical_grazing_allotment,
        .data$producing_aquifer_description, .data$driller_name_and_city,
        .data$well_comments, .data$latitude, .data$longitude, .data$source_record_id,
        .data$well_id_raw, .data$well_name_raw,
        USE.NAMES = FALSE
      )
    )
  noc_attr_all
}

pt_validate_mojave_authority_048a <- function(master, measurements, corrections) {
  require_cols <- function(x, cols, label) {
    if (!is.data.frame(x) || anyDuplicated(names(x)) || !all(cols %in% names(x))) {
      stop("Invalid ", label, " columns.", call. = FALSE)
    }
  }
  key <- function(x, label, allow_blank = FALSE) {
    good <- !is.na(x) & nzchar(trimws(as.character(x)))
    if ((!allow_blank && !all(good)) || anyDuplicated(x[good])) {
      stop("Missing or duplicate ", label, ".", call. = FALSE)
    }
  }
  require_cols(master, c(
    "final_site_uid", "report_record_number", "field_maps_number", "final_site_name",
    "final_latitude", "final_longitude", "final_status", "final_feature_type",
    "table2_name_original", "table2_field_notes", "table2_report_basin_original",
    "reported_basin", "spatial_bulletin118_id", "spatial_bulletin118_name",
    "monitoring_performed", "laboratory_sample_collected", "review_status", "source_authority", "identity_notes"
  ), "Mojave master")
  require_cols(measurements, c(
    "report_record_number", "final_site_name", "final_latitude", "final_longitude",
    "spatial_bulletin118_name", "depth_to_water_ft", "total_well_depth_ft",
    "sample_date", "monitoring_basin_location_notes"
  ), "Mojave measurements")
  require_cols(corrections, c(
    "final_site_uid", "report_record_number", "current_brim_record_identifier",
    "action", "old_name", "reason", "source_file", "source_sheet", "source_record_reference"
  ), "Mojave corrections")
  if (!is.character(master$laboratory_sample_collected) ||
      any(!master$laboratory_sample_collected %in% c("True", "False"))) {
    stop("Invalid laboratory_sample_collected: expected documented True/False values.", call. = FALSE)
  }
  if (!nrow(master)) stop("Mojave master is empty.", call. = FALSE)
  key(master$final_site_uid, "final_site_uid")
  key(master$report_record_number, "master report key")
  key(measurements$report_record_number, "measurement report key")
  key(corrections$final_site_uid, "correction site key")
  key(corrections$report_record_number, "correction report key")
  key(corrections$current_brim_record_identifier, "historical record key", TRUE)
  if (any(!corrections$action %in% c("KEEP", "UPDATE", "ADD", "REMOVE_DUPLICATE"))) {
    stop("Invalid correction action.", call. = FALSE)
  }
  retained <- corrections[corrections$action != "REMOVE_DUPLICATE", , drop = FALSE]
  removed <- corrections[corrections$action == "REMOVE_DUPLICATE", , drop = FALSE]
  if (!setequal(master$final_site_uid, retained$final_site_uid) ||
      any(removed$final_site_uid %in% master$final_site_uid)) {
    stop("Correction retained-site coverage differs from master.", call. = FALSE)
  }
  ci <- match(master$final_site_uid, retained$final_site_uid)
  if (!identical(as.character(master$report_record_number), as.character(retained$report_record_number[ci]))) {
    stop("Correction site/report keys disagree.", call. = FALSE)
  }
  mi <- match(measurements$report_record_number, master$report_record_number)
  if (anyNA(mi) || any(master$final_feature_type[mi] != "groundwater_well_site") ||
      any(master$final_status[mi] != "present")) {
    stop("Measurement key is not a retained present ordinary well.", call. = FALSE)
  }
  lon <- suppressWarnings(as.numeric(master$final_longitude))
  lat <- suppressWarnings(as.numeric(master$final_latitude))
  if (any(!is.finite(lon) | !is.finite(lat) | abs(lon) > 180 | abs(lat) > 90)) {
    stop("Invalid corrected Mojave coordinates.", call. = FALSE)
  }
  if (any(!master$final_status %in% c("present", "not_found", "unknown", "spring_present")) ||
      any(!master$final_feature_type %in% c("groundwater_well_site", "spring_or_spring_box")) ||
      any((master$final_status == "spring_present") != (master$final_feature_type == "spring_or_spring_box")) ||
      any(!master$monitoring_performed %in% c("True", "False"))) {
    stop("Invalid corrected status, feature type, or monitoring flag.", call. = FALSE)
  }
  invisible(TRUE)
}

pt_read_mojave_authority_048a <- function(directory) {
  filenames <- c(
    master = "brim_mojave_fieldcheck_corrected_master.csv",
    measurements = "report_table_a2_confirmed_monitored_well_results.csv",
    corrections = "brim_mojave_fieldcheck_corrections.csv"
  )
  paths <- file.path(directory, filenames)
  if (!all(file.exists(paths))) {
    stop("Missing corrected Mojave input(s): ", paste(filenames[!file.exists(paths)], collapse = ", "), call. = FALSE)
  }
  tables <- lapply(paths, function(path) {
    readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()),
      na = character(), trim_ws = FALSE, name_repair = "check_unique",
      locale = readr::locale(encoding = "UTF-8"), show_col_types = FALSE, progress = FALSE)
  })
  names(tables) <- names(filenames)
  do.call(pt_validate_mojave_authority_048a, tables)
  tables
}

pt_normalize_mojave_attributes_048a <- function(master, measurements, corrections) {
  pt_validate_mojave_authority_048a(master, measurements, corrections)
  # Preserve all supplied master fields. Joins add namespaced measurement and
  # correction lineage; neither source names nor final blank names are replaced.
  ci <- match(master$final_site_uid, corrections$final_site_uid)
  mi <- match(master$report_record_number, measurements$report_record_number)
  out <- master
  for (nm in names(corrections)) out[[paste0("correction_", nm)]] <- corrections[[nm]][ci]
  for (nm in names(measurements)) out[[paste0("measurement_", nm)]] <- measurements[[nm]][mi]
  out$legacy_record_uid <- corrections$current_brim_record_identifier[ci]
  out$measurement_available <- !is.na(mi)
  status_labels <- c(present = "Well present", not_found = "Well not found",
    unknown = "Unknown / not verified", spring_present = "Spring / spring box present")
  out <- out |>
    dplyr::mutate(
      raw_source_file = "brim_mojave_fieldcheck_corrected_master.csv",
      source_key = "mojave_2025_blm_field_check",
      source_display = "Mojave limited field inventory (2025)",
      source_full = "Mojave limited field inventory (2025)",
      source_short = "2025 field check",
      layer_name = "GW sites | 2025 Mojave limited field inventory",
      record_uid = .data$final_site_uid,
      source_record_id = .data$report_record_number,
      well_name_raw = .data$table2_name_original,
      well_name_display = pt_clean_chr_048a(.data$final_site_name),
      longitude = as.numeric(.data$final_longitude), latitude = as.numeric(.data$final_latitude),
      coord_status = "ok",
      groundwater_basin = .data$spatial_bulletin118_name,
      field_recon_results = .data$table2_field_notes,
      well_present_key = .data$final_status,
      well_present_display = unname(status_labels[.data$final_status]),
      well_monitored_key = ifelse(.data$monitoring_performed == "True", "monitoring_reported", "not_reported"),
      well_monitored_display = ifelse(.data$monitoring_performed == "True", "Yes", "No"),
      depth_to_water_raw = .data$measurement_depth_to_water_ft,
      depth_to_water_display = pt_fmt_ft_048a(.data$depth_to_water_raw, digits = 2),
      depth_to_water_sort_ft = pt_num_048a(.data$depth_to_water_raw),
      water_level_recorded = .data$measurement_available & is.finite(.data$depth_to_water_sort_ft),
      lab_sample_documented = .data$laboratory_sample_collected == "True",
      total_depth_raw = .data$measurement_total_well_depth_ft,
      total_depth_display = pt_fmt_ft_048a(.data$total_depth_raw, digits = 1),
      well_completion_date = NA_character_, elevation_ft = NA_real_,
      elevation_display = NA_character_, elevation_source = NA_character_,
      hover_line1 = ifelse(is.na(.data$well_name_display), paste0("Site ", .data$report_record_number), .data$well_name_display),
      hover_line2 = ifelse(is.na(.data$depth_to_water_display), NA_character_, paste0("Depth to water: ", .data$depth_to_water_display))
    )
  out$popup_html <- vapply(seq_len(nrow(out)), function(i) {
    r <- out[i, , drop = FALSE]
    paste0("<div class='pt2-popup-title'>", pt_html_escape_048a(r$hover_line1), "</div>",
      pt_popup_rows_048a(c(
        "Source", "Record #", "Field Maps #", "Feature type", "Field result",
        "Monitoring reported", "Measurement table entry", "Measurement date",
        "Depth to water", "Total depth", "Field-reported basin", "Spatial Bulletin 118 basin",
        "Spatial Bulletin 118 ID", "Field reconnaissance notes", "Measurement notes",
        "Lab sample documented", "Unresolved attributes", "Source authority",
        "Latitude", "Longitude"
      ), c(r$source_full, r$report_record_number, r$field_maps_number,
        ifelse(r$final_feature_type == "spring_or_spring_box", "Spring / spring box", "Groundwater well site"),
        r$well_present_display, r$well_monitored_display,
        ifelse(r$measurement_available, "Dedicated monitoring table", NA_character_),
        r$measurement_sample_date, r$depth_to_water_display, r$total_depth_display,
        r$reported_basin, r$spatial_bulletin118_name, r$spatial_bulletin118_id,
        r$field_recon_results, r$measurement_monitoring_basin_location_notes,
        ifelse(r$lab_sample_documented, "Yes", "Not documented"),
        ifelse(r$review_status == "HUMAN REVIEW", r$identity_notes, NA_character_), r$source_authority,
        pt_fmt_coord_048a(r$latitude), pt_fmt_coord_048a(r$longitude)),
        long_labels = c("Field reconnaissance notes", "Measurement notes", "Unresolved attributes", "Source authority")))
  }, character(1))
  out
}

# ==== 5. Locate and read inputs =============================================

noc_csv <- pt_find_raw_csv_048a(
  patterns = c("^NOC_BLMdrilled.*\\.csv$", "BLMdrilled.*\\.csv$"),
  label = "BLM NOC well inventory"
)

mojave_authority <- pt_read_mojave_authority_048a(file.path(DIR$config, "blm_well_inventory"))
mojave_raw_original <- mojave_authority$master
mojave_raw <- mojave_raw_original

field_guide_csv <- pt_find_raw_csv_048a(
  patterns = c("^NOC_Albion_fields.*\\.csv$", "fields.*albion.*\\.csv$", "popup.*fields.*\\.csv$"),
  label = "NOC/2025 Mojave popup field guide"
)

pt_msg_048a("Reading BLM NOC well inventory: ", noc_csv)
noc_raw_original <- pt_read_csv_latin1_048a(noc_csv)
noc_raw <- noc_raw_original |>
  janitor::clean_names()

pt_msg_048a("Reading popup field guide: ", field_guide_csv)
field_guide_raw <- pt_read_csv_latin1_048a(field_guide_csv)


# ==== 6. Normalize BLM NOC well inventory ====================================

noc_attr_all <- pt_normalize_noc_attributes_048a(noc_raw, noc_csv)

noc_coord_qa <- noc_attr_all |>
  sf::st_drop_geometry() |>
  dplyr::select(
    source_key, source_display, record_uid, source_record_id, well_name_display,
    longitude, latitude, coord_status
  )

noc_attr <- noc_attr_all |>
  dplyr::filter(.data$coord_status == "ok")

noc_sf <- noc_attr |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  clean_sf_for_leaflet()


# ==== 7. Normalize 2025 Mojave limited field inventory ==========================

mojave_attr_all <- do.call(pt_normalize_mojave_attributes_048a, mojave_authority)
mojave_attr <- mojave_attr_all
mojave_duplicate_exclusions <- mojave_authority$corrections |>
  dplyr::filter(.data$action == "REMOVE_DUPLICATE")
mojave_coord_qa <- mojave_attr_all |>
  dplyr::select(source_key, source_display, record_uid, source_record_id,
    well_name_display, longitude, latitude, coord_status, field_recon_results)

mojave_sf <- mojave_attr |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) |>
  clean_sf_for_leaflet()


# ==== 8. Harmonize output columns and combine ================================

common_cols <- c(
  "record_uid", "source_key", "source_display", "source_short", "layer_name",
  "raw_source_file", "source_record_id", "well_name_raw", "well_name_display",
  "hover_line1", "hover_line2", "well_completion_date",
  "longitude", "latitude", "coord_status",
  "well_present_key", "well_present_display", "well_monitored_key",
  "well_monitored_display", "depth_to_water_raw", "depth_to_water_display",
  "depth_to_water_sort_ft", "elevation_ft", "elevation_display", "elevation_source",
  "popup_html", "geometry"
)

# Preserve the established NOC projection. Mojave retains its accepted source
# and correction/measurement lineage in the processed product; the browser
# helper projects only the compact display fields needed by the map.
noc_out <- noc_sf |>
  dplyr::mutate(source_sort = 1L) |>
  dplyr::select(dplyr::all_of(common_cols), source_sort)

mojave_out <- mojave_sf |>
  dplyr::mutate(source_sort = 2L) |>
  dplyr::select(dplyr::all_of(common_cols), source_sort, dplyr::everything())

## Carry the two typed Albion observation fields through the shared inventory.
## NOC rows receive missing union fields; the cache owner excludes them from NOC.
## Full corrected lineage remains in the dedicated Mojave processed product.
combined_out <- dplyr::bind_rows(
  noc_out, dplyr::select(mojave_out, dplyr::all_of(names(noc_out)),
    water_level_recorded, lab_sample_documented)
) |>
  dplyr::arrange(.data$source_sort, .data$well_name_display, .data$record_uid)


# ==== 9. QA tables ===========================================================

coord_qa <- dplyr::bind_rows(noc_coord_qa, mojave_coord_qa) |>
  dplyr::arrange(.data$source_key, .data$coord_status, .data$well_name_display)

source_summary <- dplyr::bind_rows(
  noc_attr_all |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      source_key = "noc_blm_drilled",
      source_display = "BLM National Operations Center",
      raw_rows = dplyr::n(),
      blank_rows_dropped = 0L,
      duplicate_rows_excluded = 0L,
      coordinate_rows_excluded = sum(.data$coord_status != "ok"),
      map_ready_rows = nrow(noc_out),
      named_rows = sum(!is.na(.data$well_name_display)),
      depth_to_water_rows = sum(!is.na(.data$depth_to_water_display)),
      elevation_rows = sum(!is.na(.data$elevation_ft)),
      .groups = "drop"
    ),
  mojave_attr_all |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      source_key = "mojave_2025_blm_field_check",
      source_display = "Mojave limited field inventory (2025)",
      raw_rows = dplyr::n(),
      blank_rows_dropped = 0L,
      # Current master is already deduplicated. The separate audit retains
      # historical baseline REMOVE_DUPLICATE actions, not current-run removals.
      duplicate_rows_excluded = 0L,
      coordinate_rows_excluded = 0L,
      map_ready_rows = nrow(mojave_out),
      named_rows = sum(!is.na(.data$well_name_display)),
      depth_to_water_rows = sum(!is.na(.data$depth_to_water_display), na.rm = TRUE),
      elevation_rows = sum(!is.na(.data$elevation_ft), na.rm = TRUE),
      .groups = "drop"
    )
)

status_counts <- combined_out |>
  sf::st_drop_geometry() |>
  dplyr::count(
    .data$source_key,
    .data$source_display,
    .data$well_present_key,
    .data$well_present_display,
    .data$well_monitored_key,
    .data$well_monitored_display,
    name = "n"
  ) |>
  dplyr::arrange(.data$source_key, dplyr::desc(.data$n))

field_summary <- dplyr::bind_rows(
  tibble::tibble(
    source_key = "noc_blm_drilled",
    source_display = "BLM National Operations Center",
    field_name = names(noc_raw_original),
    cleaned_field_name = names(noc_raw),
    nonmissing_rows = vapply(noc_raw, function(x) sum(!is.na(pt_clean_chr_048a(x))), integer(1)),
    total_rows = nrow(noc_raw)
  ),
  tibble::tibble(
    source_key = "mojave_2025_blm_field_check",
    source_display = "Mojave limited field inventory (2025)",
    field_name = names(mojave_raw_original),
    cleaned_field_name = names(mojave_raw),
    nonmissing_rows = vapply(mojave_raw, function(x) sum(!is.na(pt_clean_chr_048a(x))), integer(1)),
    total_rows = nrow(mojave_raw)
  )
) |>
  dplyr::mutate(nonmissing_pct = round(100 * .data$nonmissing_rows / pmax(.data$total_rows, 1), 1)) |>
  dplyr::arrange(.data$source_key, .data$cleaned_field_name)

popup_field_guide <- field_guide_raw |>
  janitor::clean_names() |>
  dplyr::mutate(
    notes = "Source popup-field guide provided with NOC/2025 Mojave well inputs; labels are normalized in 18_blm_groundwater_well_inventory.r."
  )


# ==== 10. Write outputs ======================================================

if (WRITE_RDS) {
  dir.create(DIR$rds, recursive = TRUE, showWarnings = FALSE)
  saveRDS(noc_out, OUT_NOC_RDS)
  saveRDS(mojave_out, OUT_MOJAVE_RDS)
  saveRDS(combined_out, OUT_COMBINED_RDS)
  pt_msg_048a("Wrote NOC map-ready RDS: ", OUT_NOC_RDS)
  pt_msg_048a("Wrote 2025 Mojave limited field inventory map-ready RDS: ", OUT_MOJAVE_RDS)
  pt_msg_048a("Wrote combined BLM GW well inventory RDS: ", OUT_COMBINED_RDS)
}

if (WRITE_QA) {
  pt_write_csv_048a(source_summary, OUT_SOURCE_SUMMARY_CSV)
  pt_write_csv_048a(coord_qa, OUT_COORD_QA_CSV)
  pt_write_csv_048a(status_counts, OUT_STATUS_COUNTS_CSV)
  pt_write_csv_048a(field_summary, OUT_FIELD_SUMMARY_CSV)
  pt_write_csv_048a(mojave_duplicate_exclusions, OUT_DUPLICATE_EXCLUSIONS_CSV)
  pt_write_csv_048a(popup_field_guide, OUT_POPUP_FIELD_GUIDE_CSV)
}

pt_msg_048a("Done: BLM groundwater well inventory source normalization complete.")
pt_msg_048a("  NOC map-ready rows: ", nrow(noc_out), " / raw rows: ", nrow(noc_attr_all))
pt_msg_048a("  2025 Mojave limited field inventory map-ready rows: ", nrow(mojave_out), " / raw rows: ", nrow(mojave_attr_all))
pt_msg_048a("  Combined map-ready rows: ", nrow(combined_out))
