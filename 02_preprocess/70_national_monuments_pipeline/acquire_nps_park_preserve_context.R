#!/usr/bin/env Rscript

# Acquire immutable, complete NPS Land Resources Division snapshots for the
# focused National Park / National Preserve context owned by the BRIM National
# Monuments card. This wrapper deliberately reuses the canonical R ArcGIS
# acquisition core and does not introduce a Python production dependency.

NPS_CONTEXT_ACQUISITION_VERSION <-
  "BRIM_NPS_PARK_PRESERVE_CONTEXT_ACQUISITION_20260809_01"

command_arguments <- commandArgs(trailingOnly = FALSE)
file_argument <- command_arguments[grepl("^--file=", command_arguments)]
script_path <- if (length(file_argument)) {
  normalizePath(sub("^--file=", "", file_argument[[1]]), mustWork = TRUE)
} else {
  normalizePath(
    "02_preprocess/70_national_monuments_pipeline/acquire_nps_park_preserve_context.R",
    mustWork = TRUE
  )
}
pipeline_dir <- dirname(script_path)
source(file.path(pipeline_dir, "acquire_authoritative_sources.R"))

if (sys.nframe() == 0L) {
  tryCatch(
    nm_run_configured_acquisition(
      arguments = commandArgs(trailingOnly = TRUE),
      default_config = file.path(pipeline_dir, "nps_context_source_config.json"),
      required_source_keys = c(
        "nps_park_preserve_boundaries", "nps_park_preserve_tracts"
      ),
      implementation_version = NPS_CONTEXT_ACQUISITION_VERSION
    ),
    error = function(error) {
      message("NPS CONTEXT ACQUISITION FAIL: ", conditionMessage(error))
      quit(status = 1L, save = "no")
    }
  )
}
