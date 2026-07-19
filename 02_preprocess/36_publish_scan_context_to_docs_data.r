# ==== 36_publish_scan_context_to_docs_data.r =================================
##
## PURPOSE:
##   Copy compact SCAN plot-context CSVs from the live-feed repo's local input
##   folder to its GitHub Pages publication folder.
##
## WHY:
##   BRIM's browser-side Ops Live helpers fetch files from the GitHub Pages URL:
##
##     https://dbo99.github.io/brim-live-data-feeds/data/<file>
##
##   In the repository, that URL corresponds to:
##
##     brim-live-data-feeds/docs/data/<file>
##
##   The local preprocessor / maintenance workflow writes source context files to:
##
##     brim-live-data-feeds/data/input/<file>
##
##   Run this script after regenerating SCAN historical context so the files used
##   by the browser are refreshed before you manually upload/replace them on
##   GitHub.
##
## INPUTS:
##   brim-live-data-feeds/data/input/scan_sms_waterday_percentiles.csv
##   brim-live-data-feeds/data/input/scan_sms_monthly_context.csv
##   brim-live-data-feeds/data/input/scan_sms_prior_wy_fallback_traces.csv
##
## OUTPUTS:
##   brim-live-data-feeds/docs/data/scan_sms_waterday_percentiles.csv
##   brim-live-data-feeds/docs/data/scan_sms_monthly_context.csv
##   brim-live-data-feeds/docs/data/scan_sms_prior_wy_fallback_traces.csv
##
## MANUAL GITHUB STEP:
##   Upload/replace the three OUTPUT files above in the GitHub web interface under:
##     docs/data/
## ============================================================================

if (!dir.exists("brim-live-data-feeds") || !dir.exists("00_config")) {
  stop(
    "Run this script from the PortaTreasure2 / BRIM project root.\n",
    "Expected folders like brim-live-data-feeds/ and 00_config/ were not found."
  )
}

src_dir <- file.path("brim-live-data-feeds", "data", "input")
dst_dir <- file.path("brim-live-data-feeds", "docs", "data")

files <- c(
  "scan_sms_waterday_percentiles.csv",
  "scan_sms_monthly_context.csv",
  "scan_sms_prior_wy_fallback_traces.csv"
)

if (!dir.exists(dst_dir)) {
  dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)
}

for (f in files) {
  src <- file.path(src_dir, f)
  dst <- file.path(dst_dir, f)

  if (!file.exists(src)) {
    stop("Missing SCAN context input file: ", src)
  }

  ok <- file.copy(src, dst, overwrite = TRUE)
  if (!isTRUE(ok)) {
    stop("Could not copy ", src, " to ", dst)
  }

  message("Published SCAN context file: ", dst)
}

message("\nSCAN context files ready for GitHub Pages upload:")
for (f in files) {
  path <- file.path(dst_dir, f)
  info <- file.info(path)
  message("  ", path, "  (", format(info$size, big.mark = ","), " bytes; ", info$mtime, ")")
}

message("\nManual GitHub upload target:")
message("  brim-live-data-feeds/docs/data/")
message("\nThese files will be fetched by BRIM as:")
for (f in files) {
  message("  https://dbo99.github.io/brim-live-data-feeds/data/", f)
}
