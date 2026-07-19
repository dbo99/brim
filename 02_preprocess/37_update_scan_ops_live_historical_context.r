# ==== 37_update_scan_ops_live_historical_context.r ==========================
## RF063b: include compact usable prior-WY fallback trace export for
##         short/sparse SCAN station/depth Plot B context.
## RF058b: filename-fallback fix for the SCAN historical-context wrapper.
##
## PURPOSE:
##   One obvious annual/occasional maintenance runner for BRIM Ops Live SCAN
##   historical context files.  This wrapper rebuilds the historical/statistical
##   inputs used by the SCAN popup plots, then copies the browser-facing copies
##   into the GitHub Pages docs/data folder.
##
## WHEN TO RUN:
##   - After the water year ends, usually mid-October to early November, so the
##     just-completed water year can be incorporated into the historical context.
##   - After adding/removing SCAN stations.
##   - After changing the duplicate-sensor rule, percentile thresholds, or plot
##     context logic.
##   - After deliberately refreshing the full historical SCAN cache.
##
## WHEN NOT TO RUN:
##   - This is not the daily/current-value refresh.  Current/latest values are
##     handled by the SCAN live-feed script / GitHub workflow.
##
## OUTPUTS TO CHECK:
##   data/input/scan_sms_waterday_percentiles.csv     (source/input copy)
##   data/input/scan_sms_monthly_context.csv          (source/input copy)
##   docs/data/scan_sms_waterday_percentiles.csv      (browser/GitHub Pages copy)
##   docs/data/scan_sms_monthly_context.csv           (browser/GitHub Pages copy)
##   data/input/scan_sms_prior_wy_fallback_traces.csv  (source/input copy)
##   docs/data/scan_sms_prior_wy_fallback_traces.csv   (browser/GitHub Pages copy)
##   04_processed_data/qa/scan_ops_live_context_update_manifest.csv
##
## IMPORTANT:
##   If you are manually uploading to GitHub, only upload/replace the four CSVs
##   above when you actually want to publish the updated historical context.
## ============================================================================

# ==== 1. Project sanity check ===============================================

if (!dir.exists("00_config") || !dir.exists("02_preprocess") || !dir.exists("brim-live-data-feeds")) {
  stop(
    "Run this script from the PortaTreasure2 / BRIM project root.\n",
    "Expected folders 00_config/, 02_preprocess/, and brim-live-data-feeds/ were not found."
  )
}

# ==== 2. Small helpers ======================================================

scan_ctx_now_local <- function() {
  format(Sys.time(), tz = "America/Los_Angeles", usetz = TRUE, "%Y-%m-%d %I:%M %p %Z")
}

scan_ctx_current_wy <- function(date = Sys.Date()) {
  date <- as.Date(date)
  yr <- as.integer(format(date, "%Y"))
  mo <- as.integer(format(date, "%m"))
  ifelse(mo >= 10L, yr + 1L, yr)
}

scan_ctx_first_existing <- function(paths, label) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) {
    stop(
      "Missing required SCAN maintenance script for ", label, ":\n  ",
      paste(paths, collapse = "\n  ")
    )
  }
  hit[[1]]
}

scan_ctx_run_step <- function(path, label) {
  if (!file.exists(path)) {
    stop("Missing required SCAN maintenance script: ", path)
  }

  message("\n============================================================")
  message("[RUN ] ", label)
  message("       ", path)
  message("============================================================")

  t0 <- Sys.time()
  source(path, local = FALSE)
  elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

  message("[DONE] ", label, " (", elapsed, " sec)")
  invisible(elapsed)
}

scan_ctx_file_row <- function(path, role) {
  info <- file.info(path)
  data.frame(
    role = role,
    path = path,
    exists = file.exists(path),
    size_bytes = ifelse(is.na(info$size), NA_real_, as.numeric(info$size)),
    modified_time = ifelse(is.na(info$mtime), NA_character_, format(info$mtime, "%Y-%m-%d %H:%M:%S %Z")),
    stringsAsFactors = FALSE
  )
}

# ==== 3. Resolve script names robustly ======================================
##
## Earlier SCAN development used the filename:
##   34_build_scan_history_summary.r
##
## A clearer future name may be:
##   34_update_scan_soil_moisture_climatology.r
##
## This wrapper accepts either name.  Prefer the clearer name if it exists, but
## fall back to the current file seen in the project folder.

SCRIPT_UPDATE_CLIMATOLOGY <- scan_ctx_first_existing(
  c(
    "02_preprocess/34_update_scan_soil_moisture_climatology.r",
    "02_preprocess/34_build_scan_history_summary.r"
  ),
  label = "daily history / water-day percentile context"
)

SCRIPT_EXPORT_MONTHLY <- scan_ctx_first_existing(
  c("02_preprocess/35_export_scan_monthly_context_for_ops_live.r"),
  label = "monthly plot context export"
)

SCRIPT_EXPORT_FALLBACK <- scan_ctx_first_existing(
  c("02_preprocess/38_export_scan_prior_wy_fallback_for_ops_live.r"),
  label = "prior-WY fallback trace export"
)

SCRIPT_PUBLISH_CONTEXT <- scan_ctx_first_existing(
  c("02_preprocess/36_publish_scan_context_to_docs_data.r"),
  label = "publish context copies to docs/data"
)

# ==== 4. Run maintenance sequence ===========================================

local_date <- as.Date(format(Sys.time(), tz = "America/Los_Angeles"))
current_wy <- scan_ctx_current_wy(local_date)
completed_wy <- current_wy - 1L

message("\nSCAN Ops Live historical-context maintenance")
message("  Local date: ", local_date)
message("  Current water year excluded from historical context: WY", current_wy)
message("  Completed water years included through, if data are available: WY", completed_wy)
message("  Run started: ", scan_ctx_now_local())
message("  SCAN_REFRESH_SITE_CACHE = ", Sys.getenv("SCAN_REFRESH_SITE_CACHE", unset = "<unset>"))
message("  Daily percentile script: ", SCRIPT_UPDATE_CLIMATOLOGY)
message("  Monthly context script:  ", SCRIPT_EXPORT_MONTHLY)
message("  Fallback trace script:   ", SCRIPT_EXPORT_FALLBACK)
message("  Publish script:          ", SCRIPT_PUBLISH_CONTEXT)
message("\nNote: manual GitHub upload is needed only if you intend to publish the regenerated context files.")

elapsed_daily <- scan_ctx_run_step(
  SCRIPT_UPDATE_CLIMATOLOGY,
  "Update SCAN daily history and water-day percentile context"
)

elapsed_monthly <- scan_ctx_run_step(
  SCRIPT_EXPORT_MONTHLY,
  "Export SCAN monthly context for Ops Live"
)

elapsed_fallback <- scan_ctx_run_step(
  SCRIPT_EXPORT_FALLBACK,
  "Export SCAN usable prior-WY fallback traces for Ops Live"
)

elapsed_publish <- scan_ctx_run_step(
  SCRIPT_PUBLISH_CONTEXT,
  "Publish SCAN context CSVs to docs/data"
)

# ==== 5. Write maintenance manifest =========================================

manifest_paths <- rbind(
  scan_ctx_file_row(
    "brim-live-data-feeds/data/input/scan_sms_waterday_percentiles.csv",
    "source/input daily percentile context"
  ),
  scan_ctx_file_row(
    "brim-live-data-feeds/data/input/scan_sms_monthly_context.csv",
    "source/input monthly context"
  ),
  scan_ctx_file_row(
    "brim-live-data-feeds/data/input/scan_sms_prior_wy_fallback_traces.csv",
    "source/input usable prior-WY fallback traces"
  ),
  scan_ctx_file_row(
    "brim-live-data-feeds/docs/data/scan_sms_waterday_percentiles.csv",
    "published docs/data daily percentile context"
  ),
  scan_ctx_file_row(
    "brim-live-data-feeds/docs/data/scan_sms_monthly_context.csv",
    "published docs/data monthly context"
  ),
  scan_ctx_file_row(
    "brim-live-data-feeds/docs/data/scan_sms_prior_wy_fallback_traces.csv",
    "published docs/data usable prior-WY fallback traces"
  )
)

manifest <- data.frame(
  run_time_local = scan_ctx_now_local(),
  local_date = as.character(local_date),
  current_wy_excluded = current_wy,
  completed_wy_included_through = completed_wy,
  script_daily_percentiles = SCRIPT_UPDATE_CLIMATOLOGY,
  script_monthly_context = SCRIPT_EXPORT_MONTHLY,
  script_fallback_traces = SCRIPT_EXPORT_FALLBACK,
  script_publish_context = SCRIPT_PUBLISH_CONTEXT,
  elapsed_daily_sec = elapsed_daily,
  elapsed_monthly_sec = elapsed_monthly,
  elapsed_fallback_sec = elapsed_fallback,
  elapsed_publish_sec = elapsed_publish,
  stringsAsFactors = FALSE
)

qa_dir <- file.path("04_processed_data", "qa")
if (!dir.exists(qa_dir)) dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

manifest_path <- file.path(qa_dir, "scan_ops_live_context_update_manifest.csv")
manifest_files_path <- file.path(qa_dir, "scan_ops_live_context_update_files.csv")

utils::write.csv(manifest, manifest_path, row.names = FALSE)
utils::write.csv(manifest_paths, manifest_files_path, row.names = FALSE)

message("\nSCAN Ops Live historical-context maintenance complete.")
message("Wrote run manifest:  ", manifest_path)
message("Wrote file manifest: ", manifest_files_path)
message("\nGenerated/published local files:")
print(manifest_paths)

message("\nManual GitHub upload checklist, only when publishing context updates:")
message("  brim-live-data-feeds/data/input/scan_sms_waterday_percentiles.csv")
message("  brim-live-data-feeds/data/input/scan_sms_monthly_context.csv")
message("  brim-live-data-feeds/data/input/scan_sms_prior_wy_fallback_traces.csv")
message("  brim-live-data-feeds/docs/data/scan_sms_waterday_percentiles.csv")
message("  brim-live-data-feeds/docs/data/scan_sms_monthly_context.csv")
message("  brim-live-data-feeds/docs/data/scan_sms_prior_wy_fallback_traces.csv")

invisible(list(
  manifest = manifest,
  files = manifest_paths
))
