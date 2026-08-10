#!/usr/bin/env Rscript

# Acquire the focused USFWS National Monument special-designation geometry
# needed to repair Tule Lake National Monument. This wrapper deliberately
# reuses the canonical BRIM R ArcGIS acquisition core and does not reacquire
# the four already-complete National Monuments sources.

TULE_LAKE_ACQUISITION_VERSION <-
  "BRIM_TULE_LAKE_SOURCE_REPAIR_ACQUISITION_20260809_01"

command_arguments <- commandArgs(trailingOnly = FALSE)
file_argument <- command_arguments[grepl("^--file=", command_arguments)]
script_path <- if (length(file_argument)) {
  normalizePath(sub("^--file=", "", file_argument[[1]]), mustWork = TRUE)
} else {
  normalizePath(
    "02_preprocess/70_national_monuments_pipeline/acquire_tule_lake_authoritative_source.R",
    mustWork = TRUE
  )
}
pipeline_dir <- dirname(script_path)
source(file.path(pipeline_dir, "acquire_authoritative_sources.R"))

if (sys.nframe() == 0L) {
  tryCatch(
    nm_run_configured_acquisition(
      arguments = commandArgs(trailingOnly = TRUE),
      default_config = file.path(pipeline_dir, "tule_lake_source_config.json"),
      required_source_keys = "fws_tule_lake",
      implementation_version = TULE_LAKE_ACQUISITION_VERSION
    ),
    error = function(error) {
      message("TULE LAKE ACQUISITION FAIL: ", conditionMessage(error))
      quit(status = 1L, save = "no")
    }
  )
}
