# ==== 18_blm_groundwater_well_inventory.r ===================================
##
## PURPOSE:
##   Normalize two small BLM groundwater-well source datasets into durable,
##   map-ready WGS84 RDS files for future BRIM Local layers:
##
##     1. NOC internal database of BLM-drilled wells
##        01_raw_data/blm/NOC_BLMdrilled.csv
##
##     2. 2025 Mojave-BLM limited groundwater-well field investigation
##        01_raw_data/blm/albion_rev1.csv
##
##   A third CSV is used as a field-inclusion guide for popup design:
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
##   - The public/user-facing name for that source is:
##       "2025 Mojave-BLM limited field check"
##     or the fuller source description:
##       "2025 Mojave-BLM limited groundwater-well field investigation"
##
## CURRENT DESIGN DECISIONS:
##   - This script does NOT add visible Leaflet layers.  It only writes clean,
##     map-ready data and QA files.  Browser-managed layers, legends, BLM
##     distance filters, and inline lbl checkboxes are added in later patches.
##   - BLM-distance fields are NOT calculated here.  A later numbered distance
##     preprocessor will calculate current on/off BLM and distance-to-BLM from
##     BRIM's canonical current BLM managed-lands RDS.
##   - For the 2025 Mojave-BLM source, rows whose Field Recon Results mention
##     "duplicate" are excluded from the map-ready output and written to QA.
##   - Hover line 1 is the best available Well Name.  For NOC records, hover
##     line 2 is the well-completion date when available; for the 2025 Mojave-BLM
##     records, there is no installation/completion date field, so hover line 2
##     remains depth to water when available.
##
## INPUTS:
##   Required source CSVs in 01_raw_data/blm/.  The file finder is tolerant of
##   copied filenames such as "NOC_BLMdrilled(2).csv" or "albion_rev1(1).csv".
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


pt_name_key_048a <- function(x) {
  # Collapse a field name to a punctuation-free comparison key.  This keeps
  # 048a tolerant of small Excel/janitor differences such as:
  #   WellPresent_1Yes_2No_3Unknwn
  # becoming either:
  #   well_present_1_yes_2_no_3_unknwn
  # or:
  #   well_present_1_yes_2_no_3_un_knwn
  # depending on the exact source spelling / cleaning rules.
  stringr::str_replace_all(stringr::str_to_lower(as.character(x)), "[^a-z0-9]", "")
}

pt_standardize_col_048a <- function(df, standard_name, aliases) {
  # Rename the first matching alias to a standard BRIM column name, but only
  # when that standard name is not already present.  This avoids brittle stops
  # when a source CSV has the right field but janitor::clean_names() split it a
  # little differently than expected.
  if (standard_name %in% names(df)) return(df)
  keys <- pt_name_key_048a(names(df))
  alias_keys <- unique(pt_name_key_048a(c(standard_name, aliases)))
  hit <- which(keys %in% alias_keys)
  if (length(hit) > 0) {
    names(df)[hit[[1]]] <- standard_name
  }
  df
}

pt_standardize_mojave_cols_048a <- function(df) {
  # Normalize only the few 2025 Mojave-BLM fields that are likely to drift in
  # punctuation/camel-case.  Keep raw source names available in the field-summary
  # QA via mojave_raw_original; this function is only for downstream code.
  df |>
    pt_standardize_col_048a(
      "well_present_1_yes_2_no_3_unknwn",
      aliases = c(
        "well_present_1_yes_2_no_3_un_knwn",
        "well_present_1_yes_2_no_3_unknown",
        "wellpresent_1yes_2no_3unknwn",
        "wellpresent_1yes_2no_3unknown",
        "well_present_1yes_2no_3unknwn",
        "well_present_1yes_2no_3_unknown"
      )
    ) |>
    pt_standardize_col_048a(
      "well_monitored_1yes_2no",
      aliases = c(
        "well_monitored_1_yes_2_no",
        "wellmonitored_1yes_2no",
        "wellmonitored_1_yes_2_no"
      )
    )
}

pt_write_csv_048a <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, path, na = "")
  pt_msg_048a("Wrote QA: ", path)
}


# ==== 5. Locate and read inputs =============================================

noc_csv <- pt_find_raw_csv_048a(
  patterns = c("^NOC_BLMdrilled.*\\.csv$", "BLMdrilled.*\\.csv$"),
  label = "NOC BLM-drilled wells"
)

mojave_csv <- pt_find_raw_csv_048a(
  patterns = c("^albion_rev1.*\\.csv$", "mojave.*well.*\\.csv$", "field.*inventory.*\\.csv$"),
  label = "2025 Mojave-BLM well field inventory"
)

field_guide_csv <- pt_find_raw_csv_048a(
  patterns = c("^NOC_Albion_fields.*\\.csv$", "fields.*albion.*\\.csv$", "popup.*fields.*\\.csv$"),
  label = "NOC/2025 Mojave popup field guide"
)

pt_msg_048a("Reading NOC BLM-drilled wells: ", noc_csv)
noc_raw_original <- pt_read_csv_latin1_048a(noc_csv)
noc_raw <- noc_raw_original |>
  janitor::clean_names()

pt_msg_048a("Reading 2025 Mojave-BLM field inventory: ", mojave_csv)
mojave_raw_original <- pt_read_csv_latin1_048a(mojave_csv)
mojave_raw <- mojave_raw_original |>
  janitor::clean_names() |>
  pt_standardize_mojave_cols_048a()

pt_msg_048a("Reading popup field guide: ", field_guide_csv)
field_guide_raw <- pt_read_csv_latin1_048a(field_guide_csv)


# ==== 6. Normalize NOC BLM-drilled wells ====================================

required_noc_cols <- c("objectid", "well_name", "longitude", "latitude")
missing_noc_cols <- setdiff(required_noc_cols, names(noc_raw))
if (length(missing_noc_cols) > 0) {
  stop(
    "NOC BLM-drilled wells CSV is missing expected cleaned column(s): ",
    paste(missing_noc_cols, collapse = ", "),
    ". Available cleaned columns: ", paste(names(noc_raw), collapse = ", "),
    call. = FALSE
  )
}

noc_attr_all <- noc_raw |>
  dplyr::mutate(
    raw_source_file = basename(noc_csv),
    source_key = "noc_blm_drilled",
    source_display = "NOC BLM-drilled wells",
    source_short = "NOC",
    layer_name = "BLM-drilled wells | NOC",
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


# ==== 7. Normalize 2025 Mojave-BLM field inventory ==========================

required_mojave_cols <- c(
  "record_number", "field_maps_number", "well_name", "well_name_gama_usgs",
  "longitude", "latitude", "field_recon_results",
  "well_present_1_yes_2_no_3_unknwn", "well_monitored_1yes_2no",
  "depth_to_water", "total_depth"
)
missing_mojave_cols <- setdiff(required_mojave_cols, names(mojave_raw))
if (length(missing_mojave_cols) > 0) {
  stop(
    "2025 Mojave-BLM inventory CSV is missing expected cleaned column(s): ",
    paste(missing_mojave_cols, collapse = ", "),
    ". Available cleaned columns: ", paste(names(mojave_raw), collapse = ", "),
    call. = FALSE
  )
}

mojave_attr_all <- mojave_raw |>
  dplyr::mutate(
    raw_source_file = basename(mojave_csv),
    source_key = "mojave_2025_blm_field_check",
    source_display = "2025 Mojave-BLM limited field check",
    source_full = "2025 Mojave-BLM limited groundwater-well field investigation",
    source_short = "2025 field check",
    layer_name = "GW wells | 2025 Mojave-BLM limited field check",
    record_uid = paste0("mojave2025_", dplyr::row_number()),
    source_record_id = pt_first_nonblank_048a(.data$record_number, .data$field_maps_number),
    record_number = pt_clean_chr_048a(.data$record_number),
    field_maps_number = pt_clean_chr_048a(.data$field_maps_number),
    well_name_raw = pt_clean_chr_048a(.data$well_name),
    well_name_gama_usgs = pt_clean_chr_048a(.data$well_name_gama_usgs),
    field_maps_label = ifelse(!is.na(.data$field_maps_number), paste0("Field Maps #", .data$field_maps_number), NA_character_),
    record_number_label = ifelse(!is.na(.data$record_number), paste0("Record #", .data$record_number), NA_character_),
    well_name_display = pt_first_nonblank_048a(
      .data$well_name_raw,
      .data$well_name_gama_usgs,
      .data$field_maps_label,
      .data$record_number_label
    ),
    groundwater_basin = pt_clean_chr_048a(pt_safe_col_048a(dplyr::pick(dplyr::everything()), "groundwater_basin")),
    field_recon_results = pt_clean_chr_048a(.data$field_recon_results),
    duplicate_flag = !is.na(.data$field_recon_results) &
      stringr::str_detect(stringr::str_to_lower(.data$field_recon_results), "duplicat"),
    longitude = pt_num_048a(.data$longitude),
    latitude = pt_num_048a(.data$latitude),
    coord_status = pt_coord_status_048a(.data$longitude, .data$latitude),
    is_blank_row = is.na(.data$source_record_id) & is.na(.data$well_name_display) &
      is.na(.data$longitude) & is.na(.data$latitude),
    well_present_code = pt_clean_chr_048a(.data$well_present_1_yes_2_no_3_unknwn),
    well_present_key = dplyr::case_when(
      .data$well_present_code == "1" ~ "present",
      .data$well_present_code == "2" ~ "not_found",
      .data$well_present_code == "3" ~ "unknown",
      TRUE ~ "unknown"
    ),
    well_present_display = dplyr::case_when(
      .data$well_present_key == "present" ~ "Well present",
      .data$well_present_key == "not_found" ~ "Well not found",
      .data$well_present_key == "unknown" ~ "Unknown / not verified",
      TRUE ~ NA_character_
    ),
    well_monitored_code = pt_clean_chr_048a(.data$well_monitored_1yes_2no),
    well_monitored_key = dplyr::case_when(
      .data$well_monitored_code == "1" ~ "water_level_data",
      .data$well_monitored_code == "2" ~ "no_water_level_data",
      TRUE ~ "unknown"
    ),
    well_monitored_display = dplyr::case_when(
      .data$well_monitored_key == "water_level_data" ~ "Water-level data available",
      .data$well_monitored_key == "no_water_level_data" ~ "No water-level data",
      .data$well_monitored_key == "unknown" ~ "Unknown",
      TRUE ~ NA_character_
    ),
    depth_to_water_raw = pt_clean_chr_048a(.data$depth_to_water),
    depth_to_water_display = pt_fmt_ft_048a(.data$depth_to_water_raw, digits = 2),
    depth_to_water_sort_ft = pt_num_048a(.data$depth_to_water_raw),
    total_depth_raw = pt_clean_chr_048a(.data$total_depth),
    total_depth_display = pt_fmt_ft_048a(.data$total_depth_raw, digits = 1),
    # The 2025 Mojave-BLM field-check CSV does not currently provide an
    # installation/completion date.  Keep an explicit schema-compatible blank
    # so small well inventory cache records can share the same browser logic.
    well_completion_date = NA_character_,
    elevation_ft = NA_real_,
    elevation_display = NA_character_,
    elevation_source = NA_character_,
    hover_line1 = .data$well_name_display,
    hover_line2 = ifelse(
      !is.na(.data$depth_to_water_display),
      paste0("Depth to water: ", .data$depth_to_water_display),
      NA_character_
    ),
    popup_html = mapply(
      FUN = function(
        well_name_display, source_full, well_name_gama_usgs, field_maps_number,
        record_number, groundwater_basin, well_present_display,
        well_monitored_display, depth_to_water_display, total_depth_display,
        field_recon_results, longitude, latitude
      ) {
        # Keep this popup row-scoped for the same reason described above for NOC:
        # every row gets only its own attributes, never the concatenated full table.
        paste0(
          "<div class='pt2-popup-title'>", pt_html_escape_048a(well_name_display), "</div>",
          pt_popup_rows_048a(
            labels = c(
              "Source", "GAMA/USGS well name", "Field Maps #", "Record #",
              "Groundwater basin", "Well present?", "Water-level data available?",
              "Depth to water", "Total depth", "Field reconnaissance notes", "Latitude", "Longitude"
            ),
            values = c(
              source_full, well_name_gama_usgs, field_maps_number,
              record_number, groundwater_basin, well_present_display,
              # If the popup has an actual depth-to-water value, that row carries
              # the useful information; showing the separate "water-level data
              # available" flag above it is redundant.  Keep the flag only for
              # wells without a depth-to-water value, where "No water-level data"
              # or "Unknown" is useful context.
              ifelse(!is.na(depth_to_water_display) & nzchar(depth_to_water_display), NA_character_, well_monitored_display),
              depth_to_water_display, total_depth_display, field_recon_results,
              pt_fmt_coord_048a(latitude), pt_fmt_coord_048a(longitude)
            ),
            long_labels = c("Field reconnaissance notes")
          )
        )
      },
      .data$well_name_display, .data$source_full, .data$well_name_gama_usgs,
      .data$field_maps_number, .data$record_number, .data$groundwater_basin,
      .data$well_present_display, .data$well_monitored_display,
      .data$depth_to_water_display, .data$total_depth_display,
      .data$field_recon_results, .data$longitude, .data$latitude,
      USE.NAMES = FALSE
    )
  )

mojave_duplicate_exclusions <- mojave_attr_all |>
  dplyr::filter(.data$duplicate_flag) |>
  dplyr::select(
    source_key, source_display, record_uid, source_record_id, well_name_display,
    longitude, latitude, field_recon_results
  )

mojave_coord_qa <- mojave_attr_all |>
  dplyr::filter(!.data$is_blank_row) |>
  dplyr::mutate(
    coord_status = dplyr::case_when(
      .data$duplicate_flag ~ "excluded duplicate flagged in Field Recon Results",
      TRUE ~ .data$coord_status
    )
  ) |>
  dplyr::select(
    source_key, source_display, record_uid, source_record_id, well_name_display,
    longitude, latitude, coord_status, field_recon_results
  )

mojave_attr <- mojave_attr_all |>
  dplyr::filter(!.data$is_blank_row) |>
  dplyr::filter(!.data$duplicate_flag) |>
  dplyr::filter(.data$coord_status == "ok")

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

# Keep only compact, map-ready fields in the RDS outputs.  The original CSVs
# remain the source of truth and the QA tables still summarize raw fields, but
# the Leaflet cache does not need every attachment/link/admin column.  This keeps
# the self-contained HTML from carrying fields that are not shown in hover,
# popup, filters, or labels.
noc_out <- noc_sf |>
  dplyr::mutate(source_sort = 1L) |>
  dplyr::select(dplyr::all_of(common_cols), source_sort)

mojave_out <- mojave_sf |>
  dplyr::mutate(source_sort = 2L) |>
  dplyr::select(dplyr::all_of(common_cols), source_sort)

combined_out <- dplyr::bind_rows(noc_out, mojave_out) |>
  dplyr::arrange(.data$source_sort, .data$well_name_display, .data$record_uid)


# ==== 9. QA tables ===========================================================

coord_qa <- dplyr::bind_rows(noc_coord_qa, mojave_coord_qa) |>
  dplyr::arrange(.data$source_key, .data$coord_status, .data$well_name_display)

source_summary <- dplyr::bind_rows(
  noc_attr_all |>
    sf::st_drop_geometry() |>
    dplyr::summarise(
      source_key = "noc_blm_drilled",
      source_display = "NOC BLM-drilled wells",
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
      source_display = "2025 Mojave-BLM limited field check",
      raw_rows = dplyr::n(),
      blank_rows_dropped = sum(.data$is_blank_row),
      duplicate_rows_excluded = sum(.data$duplicate_flag, na.rm = TRUE),
      coordinate_rows_excluded = sum(!.data$is_blank_row & !.data$duplicate_flag & .data$coord_status != "ok"),
      map_ready_rows = nrow(mojave_out),
      named_rows = sum(!is.na(.data$well_name_display) & !.data$is_blank_row),
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
    source_display = "NOC BLM-drilled wells",
    field_name = names(noc_raw_original),
    cleaned_field_name = names(noc_raw),
    nonmissing_rows = vapply(noc_raw, function(x) sum(!is.na(pt_clean_chr_048a(x))), integer(1)),
    total_rows = nrow(noc_raw)
  ),
  tibble::tibble(
    source_key = "mojave_2025_blm_field_check",
    source_display = "2025 Mojave-BLM limited field check",
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
  pt_msg_048a("Wrote 2025 Mojave-BLM map-ready RDS: ", OUT_MOJAVE_RDS)
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
pt_msg_048a("  2025 Mojave-BLM map-ready rows: ", nrow(mojave_out), " / raw rows: ", nrow(mojave_attr_all))
pt_msg_048a("  Combined map-ready rows: ", nrow(combined_out))

