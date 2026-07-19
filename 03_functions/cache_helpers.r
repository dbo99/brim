# ==== cache_helpers.r ========================================================
##
## PURPOSE:
##   Small helper functions for timestamped cache/output files.
##
## WHY:
##   PortaTreasure2 will use timestamped files for traceability, plus simple
##   "latest" cache files for easy map-building.
##

# ---- Timestamp helper -------------------------------------------------------
make_timestamp <- function(tz = "America/Los_Angeles") {
  format(Sys.time(), tz = tz, usetz = FALSE, format = "%Y%m%d_%H%M%S")
}

# ---- Timestamped filename helper -------------------------------------------
timestamped_name <- function(base_name, ext = "rds", timestamp = make_timestamp()) {
  paste0(base_name, "_", timestamp, ".", ext)
}

# ---- Save RDS with optional latest copy ------------------------------------
save_rds_cached <- function(x, timestamped_path, latest_path = NULL) {
  
  saveRDS(x, timestamped_path)
  message("Saved timestamped cache: ", timestamped_path)
  
  if (!is.null(latest_path)) {
    saveRDS(x, latest_path)
    message("Updated latest cache:    ", latest_path)
  }
  
  invisible(timestamped_path)
}

# ---- Safe RDS reader --------------------------------------------------------
read_rds_checked <- function(path, label = NULL) {
  
  if (!file.exists(path)) {
    nm <- if (!is.null(label)) paste0(" for ", label) else ""
    stop("Missing required RDS", nm, ": ", path)
  }
  
  readRDS(path)
}