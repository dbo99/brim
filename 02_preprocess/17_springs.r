# ==== 17_springs.r ===========================================================
##
## PURPOSE:
##   Build the combined Local "Springs" source object used by BRIM from the
##   currently available spring inventories.
##
## CURRENT INPUT SOURCES:
##   1. NHD spring points
##      01_raw_data/springs/NHDPoint_Ftype458_springs.shp
##
##   2. 2015-16 Mojave Desert Spring Survey points
##      01_raw_data/springs/zdonsprings2020sob.csv
##
##      User-facing BRIM text intentionally does NOT use a personal-author
##      shorthand for this source.  The neutral display name is:
##        "2015-16 Mojave survey"
##
## WHY THIS SCRIPT IS A NORMALIZATION POINT:
##   Springs are likely to gain additional source inventories over time
##   (for example, springs referenced in the 2026 Amargosa State of the Basin
##   report).  Those future sources should be normalized here into the same
##   shared schema instead of becoming one-off map layers or one-off browser
##   special cases.
##
## DESIGN PRINCIPLES:
##   - Do NOT deduplicate.  Different source records can legitimately occupy
##     the same or nearly the same coordinate.  Map hover/popup logic can later
##     aggregate co-located records while preserving source provenance.
##   - Preserve both stable source keys and user-facing display names.
##   - Keep elevation fields even though NHD currently has no usable elevation
##     attribute in the local input.  Future source inventories can fill these
##     fields without redesigning the map object.
##   - Keep field names boring and explicit so browser-side filtering/labels can
##     mirror the existing CNRFC / USGS / SWRCB Local point-layer scaffolds.
##
## OUTPUTS:
##   Main processed source object:
##     04_processed_data/rds/springs_combined_wgs84.rds
##
##   Optional GeoPackage:
##     04_processed_data/gpkg/springs_combined_wgs84.gpkg
##
##   QA / audit outputs:
##     04_processed_data/qa/springs_source_summary.csv
##     04_processed_data/qa/springs_coordinate_qa.csv
##     04_processed_data/qa/springs_elevation_by_source_latest.csv
##     04_processed_data/qa/springs_source_detail_latest.csv
## ============================================================================


# ==== 1. Load configuration and packages =====================================

source("00_config/config_paths.r")
source("00_config/config_source_files.r")
source("03_functions/spatial_helpers.r")

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(janitor)
})


# ==== 2. User-facing switches ================================================

WRITE_RDS <- TRUE
WRITE_GPKG <- FALSE
WRITE_QA <- TRUE


# ==== 3. Output paths =========================================================

OUT_SPRINGS_RDS <- file.path(
  DIR$rds,
  "springs_combined_wgs84.rds"
)

OUT_SPRINGS_GPKG <- file.path(
  DIR$gpkg,
  "springs_combined_wgs84.gpkg"
)

OUT_SOURCE_SUMMARY_CSV <- file.path(
  DIR$qa,
  "springs_source_summary.csv"
)

OUT_COORD_QA_CSV <- file.path(
  DIR$qa,
  "springs_coordinate_qa.csv"
)

OUT_ELEVATION_QA_CSV <- file.path(
  DIR$qa,
  "springs_elevation_by_source_latest.csv"
)

OUT_SOURCE_DETAIL_CSV <- file.path(
  DIR$qa,
  "springs_source_detail_latest.csv"
)


# ==== 4. Shared source metadata ==============================================
##
## These fields are intentionally explicit and stable.  Later map code should
## filter on spring_source_key and display spring_source_display / short labels
## to the user.  That keeps UI text decoupled from raw file names, author names,
## or future source-specific quirks.

SOURCE_NHD <- list(
  key = "nhd",
  display = "NHD",
  short = "NHD",
  sort = 10L,
  report_url = NA_character_,
  report_label = NA_character_
)

SOURCE_MOJAVE_2015_16 <- list(
  key = "survey_2015_16",
  display = "2015–16 Mojave survey",
  short = "2015-16 survey",
  sort = 20L,
  report_url = "https://www.scienceforconservation.org/products/mojave-desert-spring-survey",
  report_label = "Survey report"
)

## FUTURE SOURCE ASSIMILATION NOTE:
##   When the 2026 Amargosa State of the Basin spring inventory is available,
##   add a SOURCE_AMARGOSA_SOB_2026 block here and normalize that input into the
##   same columns used below.  Do not create a separate visible layer unless
##   there is a clear user need; the combined Springs layer can expose source
##   filters while preserving provenance.


# ==== 5. Helper functions =====================================================

clean_spring_name <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x == ""] <- NA_character_
  x
}

spring_label_text_for_map <- function(name) {
  ## Return the text BRIM is allowed to use for the inline Local-layer `lbl`
  ## overlay.  The NHD spring inventory is dominated by features with no GNIS
  ## name, which BRIM represents as "Unnamed NHD spring".  Drawing thousands of
  ## those labels adds major browser work without adding map value, so labels are
  ## intentionally limited to records with a real name.  Do not treat strings
  ## such as "Noname Spring" as unnamed; only the explicit "Unnamed ..." family
  ## generated by BRIM/source data is suppressed.
  out <- clean_spring_name(name)
  out[grepl("^unnamed\\b", out, ignore.case = TRUE)] <- NA_character_
  out
}

pt_num_spr <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}

fmt_elev_ft <- function(x) {
  v <- pt_num_spr(x)
  ifelse(
    is.na(v),
    NA_character_,
    paste0(formatC(v, format = "f", digits = 0, big.mark = ","), " ft")
  )
}

check_required_file <- function(path, label) {
  if (!file.exists(path)) {
    stop("Missing ", label, ": ", path, call. = FALSE)
  }
  invisible(path)
}

first_existing_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

extract_elevation_ft <- function(df, source_label) {
  ## The current NHD springs shapefile used in BRIM has no usable elevation
  ## attribute, but this helper keeps the schema ready for future inventories.
  ## We only auto-convert fields whose names strongly indicate feet or meters;
  ## ambiguous fields are left for deliberate source-specific handling.
  n <- nrow(df)

  feet_candidates <- c(
    "elevation_ft", "elev_ft", "elev_feet", "elevation_feet",
    "altitude_ft", "alt_ft", "height_ft", "z_ft"
  )

  meter_candidates <- c(
    "elevation_m", "elev_m", "elev_meter", "elev_meters",
    "elevation_meter", "elevation_meters", "altitude_m", "alt_m",
    "height_m", "z_m"
  )

  feet_col <- first_existing_col(df, feet_candidates)
  meter_col <- first_existing_col(df, meter_candidates)

  if (!is.na(feet_col)) {
    val <- pt_num_spr(df[[feet_col]])
    return(tibble(
      elevation_ft = val,
      elevation_display = fmt_elev_ft(val),
      elevation_source_field = feet_col,
      elevation_source_unit = "ft",
      elevation_note = paste0("Parsed feet field from ", source_label, ": ", feet_col)
    ))
  }

  if (!is.na(meter_col)) {
    val <- pt_num_spr(df[[meter_col]]) * 3.280839895
    return(tibble(
      elevation_ft = val,
      elevation_display = fmt_elev_ft(val),
      elevation_source_field = meter_col,
      elevation_source_unit = "m_to_ft",
      elevation_note = paste0("Converted meter field from ", source_label, ": ", meter_col)
    ))
  }

  tibble(
    elevation_ft = rep(NA_real_, n),
    elevation_display = rep(NA_character_, n),
    elevation_source_field = rep(NA_character_, n),
    elevation_source_unit = rep(NA_character_, n),
    elevation_note = rep(paste0("No explicit elevation field detected in ", source_label), n)
  )
}

add_source_fields <- function(x, source_meta) {
  x |>
    dplyr::mutate(
      spring_source_key = source_meta$key,
      spring_source_display = source_meta$display,
      spring_source_short = source_meta$short,
      spring_source_sort = source_meta$sort,
      source_report_url = source_meta$report_url,
      source_report_label = source_meta$report_label,

      ## spring_source is retained as a backward-compatible display field for
      ## older cache/map code.  New code should prefer spring_source_key for
      ## filtering and spring_source_display for user-facing text.
      spring_source = source_meta$display
    )
}


# ==== 6. Read and normalize NHD spring points =================================

check_required_file(SRC$springs_nhd_ftype458, "NHD spring shapefile")

message("Reading NHD spring points:")
message("  ", SRC$springs_nhd_ftype458)

nhd_raw <- sf::st_read(
  SRC$springs_nhd_ftype458,
  quiet = TRUE
) |>
  janitor::clean_names()

if (!inherits(nhd_raw, "sf")) {
  stop("NHD spring input did not read as an sf object.", call. = FALSE)
}

if (is.na(sf::st_crs(nhd_raw))) {
  stop(
    "NHD spring shapefile has missing CRS. ",
    "Check the .prj file or manually assign the CRS before preprocessing.",
    call. = FALSE
  )
}

message("NHD spring CRS:")
print(sf::st_crs(nhd_raw))

needed_nhd_cols <- c("gnis_id", "gnis_name")
missing_nhd_cols <- setdiff(needed_nhd_cols, names(nhd_raw))

if (length(missing_nhd_cols) > 0) {
  stop(
    "NHD spring layer is missing expected column(s): ",
    paste(missing_nhd_cols, collapse = ", "),
    call. = FALSE
  )
}

nhd_elev <- extract_elevation_ft(
  sf::st_drop_geometry(nhd_raw),
  source_label = "NHD springs"
)

nhd_springs <- nhd_raw |>
  sf::st_zm(drop = TRUE, what = "ZM") |>
  sf::st_transform(4326) |>
  dplyr::bind_cols(nhd_elev) |>
  dplyr::mutate(
    spring_name = clean_spring_name(.data$gnis_name),
    spring_name_display = dplyr::coalesce(
      .data$spring_name,
      "Unnamed NHD spring"
    ),
    gnis_id = as.character(.data$gnis_id)
  ) |>
  add_source_fields(SOURCE_NHD) |>
  dplyr::select(
    spring_name,
    spring_name_display,
    spring_source,
    spring_source_key,
    spring_source_display,
    spring_source_short,
    spring_source_sort,
    gnis_id,
    elevation_ft,
    elevation_display,
    elevation_source_field,
    elevation_source_unit,
    elevation_note,
    source_report_url,
    source_report_label,
    geometry
  ) |>
  clean_sf_for_leaflet()


# ==== 7. Read and normalize 2015-16 Mojave survey spring points ===============

check_required_file(SRC$springs_zdon_2020, "2015-16 Mojave survey springs CSV")

message("Reading 2015-16 Mojave survey spring points:")
message("  ", SRC$springs_zdon_2020)

survey_raw <- readr::read_csv(
  SRC$springs_zdon_2020,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
) |>
  janitor::clean_names()

needed_survey_cols <- c("name", "latitude", "longitude", "elevation")
missing_survey_cols <- setdiff(needed_survey_cols, names(survey_raw))

if (length(missing_survey_cols) > 0) {
  stop(
    "2015-16 Mojave survey CSV is missing expected column(s): ",
    paste(missing_survey_cols, collapse = ", "),
    call. = FALSE
  )
}

survey_attr <- survey_raw |>
  dplyr::mutate(
    latitude_num = pt_num_spr(.data$latitude),
    longitude_num = pt_num_spr(.data$longitude),

    ## The local survey CSV column is named simply "elevation".  For this
    ## specific source, existing BRIM handling treated the value as feet, so we
    ## keep that convention explicitly here rather than relying on generic field
    ## detection.
    elevation_ft = pt_num_spr(.data$elevation),
    elevation_display = fmt_elev_ft(.data$elevation),
    elevation_source_field = "elevation",
    elevation_source_unit = "ft",
    elevation_note = "Parsed survey CSV elevation field as feet",

    spring_name = clean_spring_name(.data$name),
    spring_name_display = dplyr::coalesce(
      .data$spring_name,
      "Unnamed 2015-16 Mojave survey spring"
    ),
    gnis_id = NA_character_,
    coord_flag = dplyr::case_when(
      is.na(.data$latitude_num) | is.na(.data$longitude_num) ~ "missing_or_non_numeric_coordinate",
      .data$latitude_num < 32 | .data$latitude_num > 43 ~ "latitude_outside_expected_ca_range",
      .data$longitude_num < -125 | .data$longitude_num > -113 ~ "longitude_outside_expected_ca_range",
      TRUE ~ "ok"
    )
  ) |>
  add_source_fields(SOURCE_MOJAVE_2015_16)

coord_qa <- survey_attr |>
  dplyr::count(coord_flag, name = "n") |>
  dplyr::arrange(coord_flag)

if (WRITE_QA) {
  readr::write_csv(coord_qa, OUT_COORD_QA_CSV)
  message("Saved springs coordinate QA: ", OUT_COORD_QA_CSV)
}

if (any(survey_attr$coord_flag != "ok", na.rm = TRUE)) {
  warning(
    "Some 2015-16 Mojave survey spring coordinates are missing/non-numeric or outside ",
    "a broad California lon/lat range. See: ",
    OUT_COORD_QA_CSV
  )
}

survey_springs <- survey_attr |>
  dplyr::filter(.data$coord_flag == "ok") |>
  sf::st_as_sf(
    coords = c("longitude_num", "latitude_num"),
    crs = 4326,
    remove = FALSE
  ) |>
  dplyr::select(
    spring_name,
    spring_name_display,
    spring_source,
    spring_source_key,
    spring_source_display,
    spring_source_short,
    spring_source_sort,
    gnis_id,
    elevation_ft,
    elevation_display,
    elevation_source_field,
    elevation_source_unit,
    elevation_note,
    source_report_url,
    source_report_label,
    geometry
  ) |>
  clean_sf_for_leaflet()


# ==== 8. Combine spring sources ==============================================

springs_combined <- dplyr::bind_rows(
  nhd_springs,
  survey_springs
) |>
  sf::st_as_sf(crs = 4326) |>
  dplyr::arrange(.data$spring_source_sort, .data$spring_name_display) |>
  dplyr::mutate(
    spring_id = paste0(
      "spring_",
      sprintf("%06d", dplyr::row_number())
    ),
    elevation_known = !is.na(.data$elevation_ft),

    ## Browser-side labels should use this pre-screened text rather than trying
    ## to label every record.  This keeps future source assimilation explicit:
    ## if a source has real names, they can label; if not, the map stays quiet.
    spring_label_text = spring_label_text_for_map(.data$spring_name_display),
    spring_labelable = !is.na(.data$spring_label_text)
  ) |>
  dplyr::select(
    spring_id,
    spring_name,
    spring_name_display,
    spring_label_text,
    spring_labelable,
    spring_source,
    spring_source_key,
    spring_source_display,
    spring_source_short,
    spring_source_sort,
    gnis_id,
    elevation_ft,
    elevation_display,
    elevation_known,
    elevation_source_field,
    elevation_source_unit,
    elevation_note,
    source_report_url,
    source_report_label,
    geometry
  )

source_summary <- springs_combined |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  dplyr::count(
    .data$spring_source_key,
    .data$spring_source_display,
    name = "n"
  ) |>
  dplyr::arrange(.data$spring_source_key)

source_detail <- springs_combined |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  dplyr::group_by(
    .data$spring_source_key,
    .data$spring_source_display,
    .data$spring_source_short,
    .data$spring_source_sort,
    .data$source_report_label,
    .data$source_report_url
  ) |>
  dplyr::summarise(
    records = dplyr::n(),
    named_records = sum(!is.na(.data$spring_name)),
    labelable_records = sum(.data$spring_labelable, na.rm = TRUE),
    gnis_id_records = sum(!is.na(.data$gnis_id)),
    elevation_records = sum(.data$elevation_known, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$spring_source_sort)

elevation_qa <- springs_combined |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  dplyr::group_by(
    .data$spring_source_key,
    .data$spring_source_display,
    .data$elevation_source_field,
    .data$elevation_source_unit
  ) |>
  dplyr::summarise(
    records = dplyr::n(),
    elevation_nonmissing = sum(!is.na(.data$elevation_ft)),
    elevation_min_ft = suppressWarnings(ifelse(
      all(is.na(.data$elevation_ft)),
      NA_real_,
      min(.data$elevation_ft, na.rm = TRUE)
    )),
    elevation_max_ft = suppressWarnings(ifelse(
      all(is.na(.data$elevation_ft)),
      NA_real_,
      max(.data$elevation_ft, na.rm = TRUE)
    )),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$spring_source_key, .data$elevation_source_field)

message("\nCombined springs summary:")
print(source_summary, n = Inf)

message("\nSprings elevation availability by source:")
print(elevation_qa, n = Inf)


# ==== 9. Save outputs =========================================================

if (WRITE_QA) {
  readr::write_csv(source_summary, OUT_SOURCE_SUMMARY_CSV)
  readr::write_csv(source_detail, OUT_SOURCE_DETAIL_CSV)
  readr::write_csv(elevation_qa, OUT_ELEVATION_QA_CSV)

  message("Saved springs source summary: ", OUT_SOURCE_SUMMARY_CSV)
  message("Saved springs source detail QA: ", OUT_SOURCE_DETAIL_CSV)
  message("Saved springs elevation QA: ", OUT_ELEVATION_QA_CSV)
}

if (WRITE_RDS) {
  saveRDS(springs_combined, OUT_SPRINGS_RDS)
  message("Saved springs combined RDS: ", OUT_SPRINGS_RDS)
}

if (WRITE_GPKG) {
  sf::st_write(
    springs_combined,
    OUT_SPRINGS_GPKG,
    layer = "springs_combined_wgs84",
    delete_dsn = TRUE,
    quiet = TRUE
  )

  message("Saved springs combined GeoPackage: ", OUT_SPRINGS_GPKG)
}


# ==== 10. Final summary =======================================================

message("\nDone: springs preprocessing complete.")
message("Processed RDS:")
message("  ", OUT_SPRINGS_RDS)
message("QA outputs:")
message("  ", OUT_SOURCE_SUMMARY_CSV)
message("  ", OUT_COORD_QA_CSV)
message("  ", OUT_SOURCE_DETAIL_CSV)
message("  ", OUT_ELEVATION_QA_CSV)
