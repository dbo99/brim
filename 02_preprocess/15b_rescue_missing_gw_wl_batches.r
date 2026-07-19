# ==== 15b_rescue_missing_gw_wl_batches.r =====================================
##
## PURPOSE:
##   Rescue a small number of missing USGS groundwater-level batch chunks.
##
## DESIGN:
##   - Tries whole-batch request first.
##   - If that fails, tries one site at a time.
##   - Saves whatever successful rows are returned.
##   - If some sites still fail, they are written to QA CSV and skipped.
##   - A batch chunk is still saved so the main script can combine all chunks.
##
## USE CASE:
##   Use when only a few batch_####.rds files are missing and the main full
##   fetch has otherwise completed.

source("00_config/config_paths.r")
source("03_functions/cache_helpers.r")

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(tibble)
  library(readr)
  library(dataRetrieval)
})

# ---- 1. Settings ------------------------------------------------------------

chunk_dir <- "C:/Users/doconnor/OneDrive - DOI/Documents/PortaTreasure2/04_processed_data/rds/usgs_gw_latest_water_levels_raw_chunks_20260428_134153"

missing_batches <- c(1333, 1459)

SITE_BATCH_SIZE <- 20
GWL_PARAMETER_CODE <- "72019"
GWL_CANDIDATE_REGEX <- "72019|72020|62610|62611|GW_ft_bgs|water.?level"

RUN_TS <- make_timestamp()

# Try to suppress CLI progress display, since the failure appears to be coming
# from cli progress formatting rather than the actual API response.
options(cli.progress_show_after = Inf)

# ---- 2. Empty table helper --------------------------------------------------

empty_gwl_raw <- function() {
  tibble::tibble(
    field_measurements_series_id = character(),
    field_visit_id = character(),
    parameter_code = character(),
    monitoring_location_id = character(),
    observing_procedure_code = character(),
    observing_procedure = character(),
    value = numeric(),
    unit_of_measure = character(),
    time = as.POSIXct(character()),
    vertical_datum = character(),
    approval_status = character(),
    measuring_agency = character(),
    last_modified = as.POSIXct(character()),
    control_condition = character(),
    measurement_rated = character(),
    qualifier = character()
  )
}

normalize_gwl <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "sf")) x <- sf::st_drop_geometry(x)
  x <- tibble::as_tibble(x)
  if (nrow(x) == 0) return(empty_gwl_raw())
  x
}

safe_fetch <- function(ids, label = "") {
  
  message("Fetching ", label, " | sites: ", length(ids))
  
  out <- tryCatch(
    {
      dataRetrieval::read_waterdata_field_measurements(
        monitoring_location_id = ids,
        parameter_code = GWL_PARAMETER_CODE
      )
    },
    error = function(e) {
      message("  FAILED: ", conditionMessage(e))
      NULL
    }
  )
  
  normalize_gwl(out)
}

# ---- 3. Recreate the same site_batches used by script 15 --------------------

wells <- readRDS(file.path(DIR$rds, "USGS_GW_final.rds"))

wells_tbl <- if (inherits(wells, "sf")) {
  sf::st_drop_geometry(wells)
} else {
  wells
}

candidate_tbl <- wells_tbl |>
  dplyr::filter(
    !is.na(.data$site_no),
    .data$site_no != ""
  )

text_fields <- intersect(
  names(candidate_tbl),
  c("param_list", "measurements", "measurement_list", "parameters")
)

candidate_text <- rep("", nrow(candidate_tbl))

for (ff in text_fields) {
  candidate_text <- paste(
    candidate_text,
    as.character(candidate_tbl[[ff]]),
    sep = "; "
  )
}

candidate_tbl <- candidate_tbl |>
  dplyr::filter(
    grepl(GWL_CANDIDATE_REGEX, candidate_text, ignore.case = TRUE)
  )

site_numbers <- unique(as.character(candidate_tbl$site_no))
monitoring_location_ids <- paste0("USGS-", site_numbers)

site_batches <- split(
  monitoring_location_ids,
  ceiling(seq_along(monitoring_location_ids) / SITE_BATCH_SIZE)
)

message("Total recreated batches: ", length(site_batches))

# ---- 4. Rescue missing batches ---------------------------------------------

failed_log <- tibble::tibble(
  batch = integer(),
  monitoring_location_id = character(),
  reason = character()
)

for (bb in missing_batches) {
  
  out_chunk <- file.path(chunk_dir, sprintf("batch_%04d.rds", bb))
  
  if (file.exists(out_chunk)) {
    message("Already exists, skipping: ", basename(out_chunk))
    next
  }
  
  ids <- site_batches[[bb]]
  
  message("\n------------------------------------------------------------")
  message("Rescuing batch ", bb, " | sites: ", length(ids))
  
  # Try whole-batch first.
  batch_tbl <- safe_fetch(
    ids,
    label = paste0("batch ", bb, " whole-batch")
  )
  
  # If whole batch fails, try one site at a time.
  if (is.null(batch_tbl)) {
    
    message("Whole batch failed. Trying individual sites for batch ", bb, "...")
    
    good_parts <- list()
    failed_ids <- character(0)
    
    for (j in seq_along(ids)) {
      
      site_tbl <- safe_fetch(
        ids[j],
        label = paste0("batch ", bb, " site ", j, "/", length(ids))
      )
      
      if (is.null(site_tbl)) {
        
        failed_ids <- c(failed_ids, ids[j])
        
        failed_log <- dplyr::bind_rows(
          failed_log,
          tibble::tibble(
            batch = bb,
            monitoring_location_id = ids[j],
            reason = "site fetch failed; skipped in rescue chunk"
          )
        )
        
      } else {
        
        good_parts[[length(good_parts) + 1]] <- site_tbl
      }
      
      Sys.sleep(2)
    }
    
    if (length(good_parts) == 0) {
      batch_tbl <- empty_gwl_raw()
    } else {
      batch_tbl <- dplyr::bind_rows(good_parts)
    }
    
    if (length(failed_ids) > 0) {
      message("Skipped failed site IDs for batch ", bb, ":")
      print(failed_ids)
    }
  }
  
  saveRDS(batch_tbl, out_chunk)
  
  message(
    "Saved rescued chunk: ",
    out_chunk,
    " | rows: ",
    nrow(batch_tbl)
  )
}

# ---- 5. Save rescue QA ------------------------------------------------------

qa_path <- file.path(
  DIR$qa,
  paste0("usgs_gw_latest_water_levels_rescue_failed_sites_", RUN_TS, ".csv")
)

readr::write_csv(failed_log, qa_path)

message("\nRescue QA written to:")
message("  ", qa_path)

message("\nFailed/skipped site count:")
print(nrow(failed_log))

# ---- 6. Check remaining missing chunks --------------------------------------

chunks <- list.files(
  chunk_dir,
  pattern = "^batch_[0-9]{4}\\.rds$",
  full.names = FALSE
)

batch_nums <- as.integer(gsub("\\D", "", chunks))

missing <- setdiff(seq_len(length(site_batches)), batch_nums)

message("\nRemaining missing batches:")
print(missing)
message("Missing batch count: ", length(missing))