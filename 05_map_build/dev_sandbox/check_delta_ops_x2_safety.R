# 05_map_build/dev_sandbox/check_delta_ops_x2_safety.R
# Checks that:
# 1. Local X2 latest cache still has enriched fields needed by Local Layers.
# 2. Delta Ops X2 lookup script does not write local X2 RDS/cache files.

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

latest_path <- file.path(root, "04_processed_data", "cache", "latest", "x2_km_map.rds")
raw_rds_path <- file.path(root, "04_processed_data", "rds", "x2_km_wgs84.rds")
script_50 <- file.path(root, "02_preprocess", "50_build_delta_ops_x2_lookup.R")

cat("Checking Local Layers X2 cache...\n")
if (!file.exists(latest_path)) stop("Missing: ", latest_path)
x_latest <- readRDS(latest_path)
required <- c("hover_text", "popup_html", "x2_km_display")
cat("  File: ", latest_path, "\n", sep = "")
cat("  Rows: ", if (is.data.frame(x_latest)) nrow(x_latest) else NA_integer_, "\n", sep = "")
cat("  Fields: ", paste(names(x_latest), collapse = ", "), "\n", sep = "")
cat("  Required enriched fields present:\n")
print(setNames(required %in% names(x_latest), required))
stopifnot(all(required %in% names(x_latest)))

cat("\nChecking raw/static X2 WGS84 RDS exists...\n")
if (!file.exists(raw_rds_path)) stop("Missing: ", raw_rds_path)
x_raw <- readRDS(raw_rds_path)
cat("  File: ", raw_rds_path, "\n", sep = "")
cat("  Rows: ", if (is.data.frame(x_raw)) nrow(x_raw) else NA_integer_, "\n", sep = "")
cat("  Fields: ", paste(names(x_raw), collapse = ", "), "\n", sep = "")

cat("\nChecking Delta Ops 50_ script safety...\n")
if (!file.exists(script_50)) stop("Missing: ", script_50)
txt <- readLines(script_50, warn = FALSE)

bad_patterns <- c(
  "x2_km_wgs84\\.rds",
  "x2_km_map\\.rds",
  "saveRDS\\(",
  "out_rds",
  "out_latest",
  "cache/latest"
)

bad_hits <- unlist(lapply(bad_patterns, function(p) grep(p, txt, value = TRUE)))
bad_hits <- unique(bad_hits)

if (length(bad_hits) > 0) {
  cat("Suspicious lines found in 50_ script:\n")
  cat(paste0("  ", bad_hits, collapse = "\n"), "\n")
  stop("Delta Ops 50_ script is not safe yet; replace it before running.")
}

cat("  OK: no local X2 RDS/cache write references found.\n")
cat("\nAll X2 safety checks passed.\n")
