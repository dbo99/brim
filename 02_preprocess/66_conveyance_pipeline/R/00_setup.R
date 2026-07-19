# ==== 02_build_conveyance_sandbox_pilot.R ====================================
#
# BRIM conveyance canonical sandbox pilot — phase 1
#
# VERSION
#   CONVEYANCE_SANDBOX_PILOT_P1_20260715_02
#
# INPUTS IN PROJECT ROOT
#   majorconveyance.shp
#   WW_Canals.shp
#   00_conveyance_decision_ledger.csv
#   01_conveyance_schema_dictionary.csv
#
# OUTPUT
#   _conveyance_sandbox_pilot/
#     brim_conveyance_pilot.gpkg
#     csv/*.csv
#     html/brim_conveyance_sandbox_pilot.html
#
# DESIGN
#   - logical facilities are separate from physical line segments;
#   - aliases and parent systems are retained;
#   - meaningful component/type transitions are not dissolved away;
#   - one label anchor is generated per facility;
#   - BLM fields are reserved but not calculated in phase 1;
#   - source shapefiles are never modified.
# ==============================================================================

SCRIPT_VERSION <- "CONVEYANCE_PIPELINE_66_QUERY_PANEL_RANKS_20260716_01"

if (
  !exists("BRIM_PROJECT_ROOT", inherits = TRUE) ||
  !exists("CONVEYANCE_PIPELINE_ROOT", inherits = TRUE)
) {
  stop(
    "This module must be run through:\n  ",
    "02_preprocess/66_build_conveyance_pipeline.R"
  )
}

PROJECT_ROOT <- normalizePath(
  BRIM_PROJECT_ROOT,
  winslash = "/",
  mustWork = TRUE
)

PIPELINE_ROOT <- normalizePath(
  CONVEYANCE_PIPELINE_ROOT,
  winslash = "/",
  mustWork = TRUE
)

RAW_DIR <- file.path(PIPELINE_ROOT, "input", "raw")
CONFIG_DIR <- file.path(PIPELINE_ROOT, "config")

MAJOR_SHP <- file.path(RAW_DIR, "majorconveyance.shp")
DELTA_SHP <- file.path(RAW_DIR, "WW_Canals.shp")

DECISION_CSV <- file.path(
  CONFIG_DIR,
  "00_conveyance_decision_ledger.csv"
)

SCHEMA_CSV <- file.path(
  CONFIG_DIR,
  "01_conveyance_schema_dictionary.csv"
)

PROJECT_CROSSWALK_CSV <- file.path(
  CONFIG_DIR,
  "02_project_membership_crosswalk.csv"
)

EXPECTED_CVP_CSV <- file.path(
  CONFIG_DIR,
  "03_expected_cvp_facilities.csv"
)

SUPPLEMENT_REGISTRY_CSV <- file.path(
  CONFIG_DIR,
  "04_supplemental_geometry_registry.csv"
)

IDENTITY_OVERRIDES_CSV <- file.path(
  CONFIG_DIR,
  "05_facility_identity_overrides.csv"
)

LABEL_OVERRIDES_CSV <- file.path(
  CONFIG_DIR,
  "06_label_overrides.csv"
)

DISPLAY_RANK_OVERRIDES_CSV <- file.path(
  CONFIG_DIR,
  "07_display_rank_overrides.csv"
)

# Accepted BRIM source for on/off/distance calculations. This is the same
# dissolved California Albers polygon represented in BRIM as "BLM-CA Managed".
BLM_MANAGED_RDS <- file.path(
  PROJECT_ROOT,
  "04_processed_data",
  "rds",
  "blm_managed_core_3310.rds"
)

OUTPUT_ROOT <- file.path(PIPELINE_ROOT, "output")
CSV_DIR <- file.path(OUTPUT_ROOT, "csv")
HTML_DIR <- file.path(OUTPUT_ROOT, "html")
CANONICAL_DIR <- file.path(OUTPUT_ROOT, "canonical")
BRIM_EXPORT_DIR <- file.path(OUTPUT_ROOT, "brim_exports")

GPKG_PATH <- file.path(
  CANONICAL_DIR,
  "brim_conveyance_master.gpkg"
)

HTML_PATH <- file.path(
  HTML_DIR,
  "brim_conveyance_pilot.html"
)

AUTO_OPEN_HTML <- TRUE

# Geometry tolerances are intentionally conservative.
NEAR_BUFFER_M <- 100
EXTENSION_TRIM_BUFFER_M <- 40
COMPONENT_SPLIT_BUFFER_M <- 25
MIN_EXTENSION_M <- 100
MIN_UNNAMED_LENGTH_MI <- 2

USGS_HYDRO_TILES <- paste0(
  "https://basemap.nationalmap.gov/arcgis/rest/services/",
  "USGSHydroCached/MapServer/tile/{z}/{y}/{x}"
)

required_packages <- c(
  "sf",
  "dplyr",
  "readr",
  "stringr",
  "tidyr",
  "purrr",
  "leaflet",
  "htmlwidgets",
  "htmltools",
  "jsonlite",
  "DBI",
  "RSQLite"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  stop(
    "Install required package(s) in RStudio, then rerun:\n\n",
    "install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(leaflet)
  library(htmlwidgets)
  library(htmltools)
  library(jsonlite)
  library(DBI)
  library(RSQLite)
})

for (path in c(
  MAJOR_SHP,
  DELTA_SHP,
  DECISION_CSV,
  SCHEMA_CSV,
  PROJECT_CROSSWALK_CSV,
  EXPECTED_CVP_CSV,
  SUPPLEMENT_REGISTRY_CSV,
  IDENTITY_OVERRIDES_CSV,
  LABEL_OVERRIDES_CSV,
  DISPLAY_RANK_OVERRIDES_CSV,
  BLM_MANAGED_RDS
)) {
  if (!file.exists(path)) {
    stop("Missing required input:\n  ", path)
  }
}

if (dir.exists(OUTPUT_ROOT)) {
  unlink(OUTPUT_ROOT, recursive = TRUE, force = TRUE)
}

dir.create(CSV_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(HTML_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(CANONICAL_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(BRIM_EXPORT_DIR, recursive = TRUE, showWarnings = FALSE)

message("BRIM conveyance sandbox version: ", SCRIPT_VERSION)
message("Project root: ", PROJECT_ROOT)

# ---- Text and ID helpers -----------------------------------------------------

clean_text <- function(x) {
  y <- trimws(as.character(x))
  y[
    is.na(y) |
      y == "" |
      y == "0" |
      tolower(y) == "na"
  ] <- NA_character_
  y
}

first_nonmissing <- function(x, fields) {
  fields <- fields[fields %in% names(x)]

  if (length(fields) == 0L) {
    return(rep(NA_character_, nrow(x)))
  }

  out <- rep(NA_character_, nrow(x))

  for (field in fields) {
    vals <- clean_text(x[[field]])
    take <- is.na(out) & !is.na(vals)
    out[take] <- vals[take]
  }

  out
}

parse_rows <- function(x) {
  x <- clean_text(x)

  if (length(x) == 0L || is.na(x)) {
    return(integer())
  }

  values <- unlist(strsplit(x, "\\|"))
  values <- trimws(values)
  values <- values[values != ""]

  as.integer(values)
}

split_values <- function(x) {
  x <- clean_text(x)

  if (length(x) == 0L || is.na(x)) {
    return(character())
  }

  values <- unlist(strsplit(x, ";"))
  values <- trimws(values)
  unique(values[values != ""])
}

collapse_unique <- function(x, separator = "; ") {
  x <- clean_text(x)
  x <- unique(x[!is.na(x)])
  paste(x, collapse = separator)
}

normalize_name <- function(x) {
  y <- tolower(dplyr::coalesce(clean_text(x), ""))
  y <- stringr::str_replace_all(y, "&", " and ")
  y <- stringr::str_replace_all(y, "[’']", "")
  y <- stringr::str_replace_all(y, "[^a-z0-9]+", " ")
  y <- stringr::str_squish(y)
  y
}

smart_title <- function(x) {
  y <- clean_text(x)

  ifelse(
    is.na(y),
    NA_character_,
    stringr::str_to_title(tolower(y))
  )
}


choose_display_name <- function(x) {
  values <- unique(clean_text(x))
  values <- values[!is.na(values)]

  if (length(values) == 0L) {
    return(NA_character_)
  }

  all_upper <- values == toupper(values) &
    grepl("[A-Z]", values)

  all_lower <- values == tolower(values) &
    grepl("[a-z]", values)

  mixed_case <- !all_upper & !all_lower

  score <- 100L * mixed_case +
    10L * !all_upper +
    pmin(nchar(values), 80L)

  values[which.max(score)]
}

stable_hash8 <- function(x) {
  one_hash <- function(value) {
    ints <- utf8ToInt(enc2utf8(dplyr::coalesce(value, "")))
    h <- 2166136261 %% 2147483647

    for (i in ints) {
      h <- (h * 16777619 + i) %% 2147483647
    }

    sprintf("%08X", as.integer(h))
  }

  vapply(x, one_hash, character(1))
}

make_lbl <- function(canonical_name, aliases = NA_character_) {
  name <- clean_text(canonical_name)
  alias_values <- split_values(aliases)

  if (length(alias_values) > 0L) {
    usable_aliases <- alias_values[
      nchar(alias_values) >= 4 &
        !grepl(
          "source|unresolved|parent|system",
          alias_values,
          ignore.case = TRUE
        )
    ]

    if (
      length(usable_aliases) > 0L &&
      min(nchar(usable_aliases)) + 8 < nchar(name)
    ) {
      name <- usable_aliases[which.min(nchar(usable_aliases))]
    }
  }

  name <- stringr::str_replace_all(name, "\\s*\\([^)]*\\)", "")
  name <- stringr::str_squish(name)

  replacements <- c(
    "Glenn-Colusa Irrigation District" = "GCID",
    "Glenn Colusa Irrigation District" = "GCID",
    "Westlands Water District" = "WWD",
    "Briggs-West Gridley Irrigation District" = "BWGID",
    "South San Joaquin Irrigation District" = "SSJID",
    "Turlock Irrigation District" = "TID",
    "Madera Irrigation District" = "MID",
    "Oakdale Irrigation District" = "OID",
    "Richvale Irrigation District" = "RID",
    "San Francisco Public Utilities Commission" = "SFPUC",
    "San Diego County Water Authority" = "SDCWA"
  )

  for (pattern in names(replacements)) {
    name <- stringr::str_replace_all(
      name,
      stringr::fixed(pattern),
      replacements[[pattern]]
    )
  }

  name
}

