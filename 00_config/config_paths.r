# ==== config_paths.r =========================================================
##
## PURPOSE:
##   Define all BRIM project folders from the active project root.
##
## WHY THIS MATTERS:
##   The old project relied heavily on setwd(), here::here(), and active-script
##   locations. That can become fragile when a project folder is copied or
##   renamed, because here::here() may continue to anchor to an older RStudio
##   project root. This file derives the root from the current working directory
##   by walking upward until it finds the BRIM project markers.
##
##   Expected use: setwd() to the BRIM project folder before sourcing
##   run_build_map.r, or run from any subfolder inside the project.
##

# ---- 1. Project-root finder -------------------------------------------------

pt_find_project_root <- function(start = getwd(), max_depth = 10) {
  cur <- normalizePath(start, winslash = "/", mustWork = TRUE)

  for (i in seq_len(max_depth + 1)) {
    has_runner <- file.exists(file.path(cur, "run_build_map.r"))
    has_paths  <- file.exists(file.path(cur, "00_config", "config_paths.r"))
    has_build  <- dir.exists(file.path(cur, "05_map_build"))

    if (has_runner && has_paths && has_build) {
      return(cur)
    }

    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }

  stop(
    "Could not find BRIM project root from start directory: ", start, "\n",
    "Please setwd() to the BRIM project folder, then source run_build_map.r."
  )
}

PT_PROJECT_ROOT <- pt_find_project_root()

pt_path <- function(...) {
  file.path(PT_PROJECT_ROOT, ...)
}

# ---- 2. Main project directories -------------------------------------------

DIR <- list(
  root       = PT_PROJECT_ROOT,

  config     = pt_path("00_config"),
  raw        = pt_path("01_raw_data"),
  preprocess = pt_path("02_preprocess"),
  functions  = pt_path("03_functions"),

  processed  = pt_path("04_processed_data"),
  rds        = pt_path("04_processed_data", "rds"),
  gpkg       = pt_path("04_processed_data", "gpkg"),
  cache      = pt_path("04_processed_data", "cache"),
  cache_geom = pt_path("04_processed_data", "cache", "geom"),
  cache_enr  = pt_path("04_processed_data", "cache", "enriched"),
  cache_last = pt_path("04_processed_data", "cache", "latest"),
  qa         = pt_path("04_processed_data", "qa"),

  map_build  = pt_path("05_map_build"),
  output     = pt_path("06_output"),
  html       = pt_path("06_output", "html"),

  legacy     = pt_path("07_legacy_scripts"),
  docs       = pt_path("08_docs")
)

# ---- 3. Create any missing folders -----------------------------------------
## This makes the project self-healing: if a subfolder is missing, it is created.

invisible(lapply(DIR, function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}))

# ---- 4. Print confirmation --------------------------------------------------

message("BRIM paths loaded from project root:")
message("  root: ", DIR$root)
message("  raw:  ", DIR$raw)
message("  rds:  ", DIR$rds)
message("  html: ", DIR$html)
