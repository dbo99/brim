# ==== 66_build_conveyance_pipeline.R =========================================
#
# Single BRIM entry point for the reproducible California conveyance pipeline.
#
# Preferred location:
#   <BRIM project>/02_preprocess/66_build_conveyance_pipeline.R
#
# Pipeline contents:
#   <BRIM project>/02_preprocess/66_conveyance_pipeline/
#
# The raw source shapefiles are copied into the self-contained pipeline folder
# on the first run when legacy copies exist in the project root.
# ==============================================================================

PIPELINE_ENTRY_VERSION <- "66_CONVEYANCE_ENTRY_20260715_02"

resolve_current_script <- function() {
  frames <- sys.frames()

  for (index in rev(seq_along(frames))) {
    candidate <- frames[[index]]$ofile

    if (!is.null(candidate) && nzchar(candidate)) {
      return(
        normalizePath(
          candidate,
          winslash = "/",
          mustWork = TRUE
        )
      )
    }
  }

  stop(
    "Could not determine the location of ",
    "66_build_conveyance_pipeline.R. ",
    "Run it with source()."
  )
}

copy_shapefile_bundle <- function(
  source_directory,
  destination_directory,
  basename_without_extension
) {
  pattern <- paste0(
    "^",
    gsub(
      "([.|()\\^{}+$*?]|\\[|\\])",
      "\\\\\\1",
      basename_without_extension
    ),
    "\\."
  )

  source_files <- list.files(
    source_directory,
    pattern = pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(source_files) == 0L) {
    return(FALSE)
  }

  dir.create(
    destination_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )

  copied <- file.copy(
    from = source_files,
    to = file.path(
      destination_directory,
      basename(source_files)
    ),
    overwrite = TRUE,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  all(copied)
}

ENTRY_SCRIPT <- resolve_current_script()
PREPROCESSORS_DIR <- dirname(ENTRY_SCRIPT)

BRIM_PROJECT_ROOT <- normalizePath(
  dirname(PREPROCESSORS_DIR),
  winslash = "/",
  mustWork = TRUE
)

CONVEYANCE_PIPELINE_ROOT <- file.path(
  PREPROCESSORS_DIR,
  "66_conveyance_pipeline"
)

RAW_DIR <- file.path(
  CONVEYANCE_PIPELINE_ROOT,
  "input",
  "raw"
)

R_DIR <- file.path(
  CONVEYANCE_PIPELINE_ROOT,
  "R"
)

dir.create(
  RAW_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

required_raw <- c(
  "majorconveyance",
  "WW_Canals"
)

for (source_basename in required_raw) {
  preferred_shp <- file.path(
    RAW_DIR,
    paste0(source_basename, ".shp")
  )

  if (file.exists(preferred_shp)) {
    next
  }

  message(
    "Bootstrapping raw source bundle into pipeline: ",
    source_basename
  )

  copied <- copy_shapefile_bundle(
    source_directory = BRIM_PROJECT_ROOT,
    destination_directory = RAW_DIR,
    basename_without_extension = source_basename
  )

  if (!isTRUE(copied) || !file.exists(preferred_shp)) {
    stop(
      "Could not find the raw shapefile bundle for ",
      source_basename,
      ".\nExpected either:\n  ",
      preferred_shp,
      "\nor a legacy copy in:\n  ",
      BRIM_PROJECT_ROOT
    )
  }
}

stage_files <- c(
  "00_setup.R",
  "01_ingest_and_build_geometry.R",
  "02_build_facilities_and_projects.R",
  "02b_calculate_blm_spatial_attribution.R",
  "03_build_labels_and_qa.R",
  "04_write_canonical_outputs.R",
  "05_build_public_pilot_html.R",
  "06_write_build_summary.R"
)

missing_stage_files <- stage_files[
  !file.exists(
    file.path(
      R_DIR,
      stage_files
    )
  )
]

if (length(missing_stage_files) > 0L) {
  stop(
    "Missing conveyance pipeline stage file(s):\n  ",
    paste(
      missing_stage_files,
      collapse = "\n  "
    )
  )
}

message(
  "BRIM conveyance entry version: ",
  PIPELINE_ENTRY_VERSION
)

message(
  "BRIM project root: ",
  BRIM_PROJECT_ROOT
)

message(
  "Conveyance pipeline root: ",
  normalizePath(
    CONVEYANCE_PIPELINE_ROOT,
    winslash = "/",
    mustWork = TRUE
  )
)

PIPELINE_ENV <- new.env(
  parent = globalenv()
)

PIPELINE_ENV$BRIM_PROJECT_ROOT <- BRIM_PROJECT_ROOT
PIPELINE_ENV$CONVEYANCE_PIPELINE_ROOT <- CONVEYANCE_PIPELINE_ROOT

for (stage_index in seq_along(stage_files)) {
  stage_file <- stage_files[[stage_index]]

  message(
    "\n[",
    stage_index,
    "/",
    length(stage_files),
    "] ",
    stage_file
  )

  source(
    file.path(
      R_DIR,
      stage_file
    ),
    local = PIPELINE_ENV,
    echo = FALSE,
    chdir = FALSE
  )
}

message(
  "\nConveyance pipeline completed successfully."
)
