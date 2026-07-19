# ==== 15_usgs_gw_latest_water_levels.r =======================================
##
## PURPOSE:
##   Fetch latest manually recorded USGS groundwater-level field measurements
##   for USGS well sites, without modifying USGS_GW_final.rds.
##
## INPUT:
##   04_processed_data/rds/USGS_GW_final.rds
##
## OUTPUTS:
##   Pilot mode:
##     04_processed_data/rds/usgs_gw_latest_water_levels_pilot.rds
##
##   Full mode:
##     04_processed_data/rds/usgs_gw_latest_water_levels.rds
##
## METHOD:
##   Uses dataRetrieval::read_waterdata_field_measurements().
##
## FUTURE-PROOF CHUNK WORKFLOW:
##   CHUNK_MODE controls whether this run resumes an existing fetch or starts a
##   new seasonal refresh.
##
##   Recommended for current interrupted run:
##     CHUNK_MODE <- "resume_latest"
##
##   Recommended for a future seasonal update:
##     CHUNK_MODE   <- "named"
##     FETCH_RUN_ID <- "2026_fall"
##
##   In named mode, rerunning with the same FETCH_RUN_ID resumes that seasonal
##   fetch. Changing FETCH_RUN_ID starts a new fetch and does not reuse old
##   chunks.
##
## NOTES:
##   - parameter_code 72019 is groundwater-level depth in feet.
##   - Map display label uses "ft bgs".
##   - This script creates a separate lookup table and does not edit the main
##     USGS well RDS.
##

# ==== 1. Load configuration ==================================================

source("00_config/config_paths.r")
source("03_functions/cache_helpers.r")

# ==== 2. Load packages =======================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(purrr)
  library(readr)
  library(tibble)
  library(dataRetrieval)
})

# ==== 3. User controls =======================================================

## Use "pilot" for a small test, "full" for the statewide candidate set.
RUN_MODE <- "full"   # "pilot" or "full"

PILOT_N <- 200

## API pacing. Keep conservative, even with API key.
SITE_BATCH_SIZE <- 20
SLEEP_SECONDS_BETWEEN_BATCHES <- 2

## Retry settings for temporary API / network issues.
MAX_RETRIES <- 5
RETRY_BASE_SLEEP_SECONDS <- 10

## Candidate filtering:
## TRUE = query only wells whose cached metadata suggests groundwater-level data.
## FALSE = query all wells in USGS_GW_final.rds.
ONLY_GWL_CANDIDATES <- TRUE

GWL_PARAMETER_CODE <- "72019"

GWL_CANDIDATE_REGEX <- "72019|72020|62610|62611|GW_ft_bgs|water.?level"

## Chunk behavior:
##   "resume_latest" = automatically use the existing chunk folder with the
##                     greatest saved batch number for this RUN_MODE.
##   "named"         = use a stable named folder based on FETCH_RUN_ID.
##                     Rerun with same FETCH_RUN_ID to resume; change it for a
##                     new seasonal update.
##   "manual"        = use MANUAL_RAW_CHUNK_DIR exactly.
##   "new_timestamp" = always create a fresh timestamped folder.
CHUNK_MODE <- "resume_latest"

## For future seasonal updates, use CHUNK_MODE <- "named" and set this clearly.
## Examples:
##   FETCH_RUN_ID <- "2026_summer"
##   FETCH_RUN_ID <- "2026_fall"
FETCH_RUN_ID <- "current"

## Only used when CHUNK_MODE <- "manual".
MANUAL_RAW_CHUNK_DIR <- NA_character_

## Recommended TRUE:
##   Prevents an incomplete partial run from overwriting the final full table.
STOP_ON_MISSING_BATCHES <- TRUE

RUN_TS <- make_timestamp()

# ==== 4. API-key diagnostic ==================================================
##
## USGS Water Data API keys are commonly exposed to dataRetrieval through:
##   API_USGS_PAT
##
## This script does not print the key. It only reports whether R sees one.

api_key_chars <- nchar(Sys.getenv("API_USGS_PAT"))

if (api_key_chars > 0) {
  message("USGS API key detected in API_USGS_PAT. Character count: ", api_key_chars)
} else {
  warning(
    "No USGS API key detected in API_USGS_PAT. ",
    "The run may hit HTTP 429 rate limits."
  )
}

# ==== 5. Input/output paths ==================================================

in_wells <- file.path(
  DIR$rds,
  "USGS_GW_final.rds"
)

if (!file.exists(in_wells)) {
  stop("Missing input well RDS: ", in_wells)
}

out_base <- dplyr::case_when(
  RUN_MODE == "pilot" ~ "usgs_gw_latest_water_levels_pilot",
  RUN_MODE == "full"  ~ "usgs_gw_latest_water_levels",
  TRUE                ~ NA_character_
)

if (is.na(out_base)) {
  stop("RUN_MODE must be either 'pilot' or 'full'.")
}

out_latest <- file.path(
  DIR$rds,
  paste0(out_base, ".rds")
)

out_timestamped <- file.path(
  DIR$rds,
  timestamped_name(out_base, "rds", RUN_TS)
)

# ==== 6. Chunk-folder helpers ================================================

pt_chunk_file <- function(chunk_dir, batch_i) {
  file.path(chunk_dir, sprintf("batch_%04d.rds", batch_i))
}

pt_count_chunk_files <- function(d) {

  if (!dir.exists(d)) {
    return(tibble::tibble(
      folder = d,
      n_chunks = 0L,
      min_batch = NA_integer_,
      max_batch = NA_integer_,
      modified = as.POSIXct(NA)
    ))
  }

  chunks <- list.files(
    d,
    pattern = "^batch_[0-9]{4}\\.rds$",
    full.names = TRUE
  )

  if (length(chunks) == 0) {
    return(tibble::tibble(
      folder = d,
      n_chunks = 0L,
      min_batch = NA_integer_,
      max_batch = NA_integer_,
      modified = file.info(d)$mtime
    ))
  }

  batch_nums <- as.integer(gsub("\\D", "", basename(chunks)))

  tibble::tibble(
    folder = d,
    n_chunks = length(chunks),
    min_batch = min(batch_nums, na.rm = TRUE),
    max_batch = max(batch_nums, na.rm = TRUE),
    modified = file.info(d)$mtime
  )
}

pt_find_best_existing_chunk_dir <- function(out_base) {

  dirs <- list.dirs(
    DIR$rds,
    recursive = FALSE,
    full.names = TRUE
  )

  target_pattern <- paste0("^", out_base, "_raw_chunks_")

  dirs <- dirs[
    grepl(target_pattern, basename(dirs))
  ]

  if (length(dirs) == 0) {
    return(NA_character_)
  }

  info <- dplyr::bind_rows(
    lapply(dirs, pt_count_chunk_files)
  ) |>
    dplyr::filter(.data$n_chunks > 0) |>
    dplyr::arrange(
      dplyr::desc(.data$max_batch),
      dplyr::desc(.data$n_chunks),
      dplyr::desc(.data$modified)
    )

  if (nrow(info) == 0) {
    return(NA_character_)
  }

  info$folder[1]
}

pt_choose_chunk_dir <- function() {

  if (CHUNK_MODE == "manual") {

    if (
      is.na(MANUAL_RAW_CHUNK_DIR) ||
      !nzchar(MANUAL_RAW_CHUNK_DIR)
    ) {
      stop("CHUNK_MODE is 'manual', but MANUAL_RAW_CHUNK_DIR is blank.")
    }

    return(MANUAL_RAW_CHUNK_DIR)
  }

  if (CHUNK_MODE == "named") {

    if (is.na(FETCH_RUN_ID) || !nzchar(FETCH_RUN_ID)) {
      stop("CHUNK_MODE is 'named', but FETCH_RUN_ID is blank.")
    }

    return(file.path(
      DIR$rds,
      paste0(out_base, "_raw_chunks_", FETCH_RUN_ID)
    ))
  }

  if (CHUNK_MODE == "resume_latest") {

    best_dir <- pt_find_best_existing_chunk_dir(out_base)

    if (!is.na(best_dir) && nzchar(best_dir)) {
      return(best_dir)
    }

    return(file.path(
      DIR$rds,
      paste0(out_base, "_raw_chunks_", RUN_TS)
    ))
  }

  if (CHUNK_MODE == "new_timestamp") {

    return(file.path(
      DIR$rds,
      paste0(out_base, "_raw_chunks_", RUN_TS)
    ))
  }

  stop(
    "Invalid CHUNK_MODE: ", CHUNK_MODE,
    ". Use 'resume_latest', 'named', 'manual', or 'new_timestamp'."
  )
}

out_raw_chunk_dir <- pt_choose_chunk_dir()

dir.create(out_raw_chunk_dir, recursive = TRUE, showWarnings = FALSE)

message("Chunk mode: ", CHUNK_MODE)
message("Raw chunk folder:")
message("  ", out_raw_chunk_dir)

chunk_status_start <- pt_count_chunk_files(out_raw_chunk_dir)

message(
  "Existing chunks at start: ",
  chunk_status_start$n_chunks,
  " | max batch: ",
  chunk_status_start$max_batch
)

# ==== 7. Read well metadata ==================================================

message("Reading USGS wells:")
message("  ", in_wells)

wells <- readRDS(in_wells)

wells_tbl <- if (inherits(wells, "sf")) {
  sf::st_drop_geometry(wells)
} else {
  wells
}

if (!"site_no" %in% names(wells_tbl)) {
  stop("USGS_GW_final.rds is missing site_no.")
}

# ==== 8. Select candidate sites ==============================================

candidate_tbl <- wells_tbl |>
  dplyr::filter(
    !is.na(.data$site_no),
    .data$site_no != ""
  )

if (ONLY_GWL_CANDIDATES) {

  text_fields <- intersect(
    names(candidate_tbl),
    c("param_list", "measurements", "measurement_list", "parameters")
  )

  if (length(text_fields) == 0) {

    warning(
      "ONLY_GWL_CANDIDATES is TRUE, but no candidate text fields were found. ",
      "Using all wells instead."
    )

  } else {

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
  }
}

site_numbers <- unique(as.character(candidate_tbl$site_no))

if (RUN_MODE == "pilot") {
  site_numbers <- head(site_numbers, PILOT_N)
}

message("Candidate wells to query: ", length(site_numbers))

if (length(site_numbers) == 0) {
  stop("No candidate wells found.")
}

monitoring_location_ids <- paste0("USGS-", site_numbers)

site_batches <- split(
  monitoring_location_ids,
  ceiling(seq_along(monitoring_location_ids) / SITE_BATCH_SIZE)
)

message("Total batches expected: ", length(site_batches))

# ==== 9. Empty raw-table helper ==============================================

pt_empty_gwl_raw <- function() {

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

pt_normalize_gwl_raw <- function(out) {

  if (is.null(out)) {
    return(NULL)
  }

  if (inherits(out, "sf")) {
    out <- sf::st_drop_geometry(out)
  }

  out <- tibble::as_tibble(out)

  if (nrow(out) == 0) {
    return(pt_empty_gwl_raw())
  }

  out
}

# ==== 10. Fetch one batch safely =============================================

fetch_gwl_batch <- function(location_batch, batch_id, n_batches) {

  message(
    "Fetching batch ", batch_id, "/", n_batches,
    " | sites: ", length(location_batch)
  )

  for (attempt in seq_len(MAX_RETRIES)) {

    out <- tryCatch(
      {
        dataRetrieval::read_waterdata_field_measurements(
          monitoring_location_id = location_batch,
          parameter_code = GWL_PARAMETER_CODE
        )
      },
      error = function(e) {
        e
      }
    )

    if (!inherits(out, "error")) {
      out <- pt_normalize_gwl_raw(out)
      return(out)
    }

    msg <- conditionMessage(out)

    warning(
      "Batch ", batch_id,
      " attempt ", attempt, "/", MAX_RETRIES,
      " failed: ", msg
    )

    if (attempt < MAX_RETRIES) {

      sleep_s <- RETRY_BASE_SLEEP_SECONDS * attempt

      if (grepl("429|Too Many Requests|rate limit", msg, ignore.case = TRUE)) {
        sleep_s <- max(sleep_s, 60 * attempt)
      }

      message("Sleeping ", sleep_s, " seconds before retry...")
      Sys.sleep(sleep_s)
    }
  }

  ## NULL means failed after all retries.
  NULL
}

# ==== 11. Fetch selected batches =============================================

failed_batches <- integer(0)

for (i in seq_along(site_batches)) {

  out_chunk <- pt_chunk_file(out_raw_chunk_dir, i)

  if (file.exists(out_chunk)) {
    message("Skipping existing chunk: ", basename(out_chunk))
    next
  }

  batch_tbl <- fetch_gwl_batch(
    location_batch = site_batches[[i]],
    batch_id = i,
    n_batches = length(site_batches)
  )

  if (is.null(batch_tbl)) {
    failed_batches <- c(failed_batches, i)
    next
  }

  ## Save every successful batch, even if it returned zero rows. This is what
  ## makes resume safe; empty-success batches are not re-queried.
  saveRDS(batch_tbl, out_chunk)

  Sys.sleep(SLEEP_SECONDS_BETWEEN_BATCHES)
}

expected_chunks <- file.path(
  out_raw_chunk_dir,
  sprintf("batch_%04d.rds", seq_along(site_batches))
)

missing_batches <- which(!file.exists(expected_chunks))

if (length(failed_batches) > 0) {
  warning(
    "Failed batches in this run: ",
    paste(failed_batches, collapse = ", ")
  )
}

if (length(missing_batches) > 0) {

  msg <- paste0(
    "Missing batch chunk files: ",
    paste(head(missing_batches, 50), collapse = ", "),
    if (length(missing_batches) > 50) " ..." else "",
    "\nMissing batch count: ", length(missing_batches),
    "\nRaw chunk folder: ", out_raw_chunk_dir
  )

  if (isTRUE(STOP_ON_MISSING_BATCHES)) {
    stop(
      msg,
      "\n\nFinal latest water-level table was NOT overwritten. ",
      "Rerun this script later; it will skip existing chunks and continue."
    )
  } else {
    warning(msg)
  }
}

# ==== 12. Read all available chunk files =====================================

chunk_files <- list.files(
  out_raw_chunk_dir,
  pattern = "^batch_[0-9]{4}\\.rds$",
  full.names = TRUE
)

message("Chunk files available for combine: ", length(chunk_files))

read_gwl_chunk <- function(f) {

  x <- readRDS(f)

  if (inherits(x, "sf")) {
    x <- sf::st_drop_geometry(x)
  }

  tibble::as_tibble(x)
}

gwl_raw <- purrr::map_dfr(chunk_files, read_gwl_chunk)

message("Raw groundwater-level rows returned/combined: ", nrow(gwl_raw))

# ==== 13. Build latest-value lookup table ====================================

if (nrow(gwl_raw) == 0) {

  latest_tbl <- tibble::tibble(
    site_no = character(),
    monitoring_location_id = character(),
    latest_wl_ft_bgs = numeric(),
    latest_wl_datetime = as.POSIXct(character()),
    latest_wl_date = as.Date(character()),
    latest_wl_status = character(),
    latest_wl_procedure = character(),
    latest_wl_qualifier = character(),
    latest_wl_units = character(),
    latest_wl_source = character(),
    latest_wl_run_ts = character()
  )

} else {

  needed_cols <- c(
    "monitoring_location_id",
    "parameter_code",
    "value",
    "unit_of_measure",
    "time"
  )

  missing_cols <- setdiff(needed_cols, names(gwl_raw))

  if (length(missing_cols) > 0) {
    stop(
      "Groundwater-level result is missing expected column(s): ",
      paste(missing_cols, collapse = ", "),
      "\nReturned columns: ",
      paste(names(gwl_raw), collapse = ", ")
    )
  }

  optional_cols <- c(
    "approval_status",
    "observing_procedure",
    "qualifier"
  )

  for (cc in optional_cols) {
    if (!cc %in% names(gwl_raw)) {
      gwl_raw[[cc]] <- NA_character_
    }
  }

  latest_tbl <- gwl_raw |>
    dplyr::mutate(
      monitoring_location_id = as.character(.data$monitoring_location_id),
      site_no = sub("^USGS-", "", .data$monitoring_location_id),
      parameter_code = as.character(.data$parameter_code),
      latest_wl_ft_bgs = as.numeric(.data$value),
      latest_wl_datetime = as.POSIXct(.data$time, tz = "UTC"),
      latest_wl_date = as.Date(.data$latest_wl_datetime),
      latest_wl_units = "ft bgs",
      latest_wl_status = as.character(.data$approval_status),
      latest_wl_procedure = as.character(.data$observing_procedure),
      latest_wl_qualifier = as.character(.data$qualifier)
    ) |>
    dplyr::filter(
      !is.na(.data$site_no),
      !is.na(.data$latest_wl_datetime),
      !is.na(.data$latest_wl_ft_bgs)
    ) |>
    dplyr::arrange(
      .data$site_no,
      dplyr::desc(.data$latest_wl_datetime)
    ) |>
    dplyr::group_by(.data$site_no) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      site_no,
      monitoring_location_id,
      latest_wl_ft_bgs,
      latest_wl_datetime,
      latest_wl_date,
      latest_wl_status,
      latest_wl_procedure,
      latest_wl_qualifier,
      latest_wl_units,
      latest_wl_source = "USGS Water Data API field measurements, parameter 72019",
      latest_wl_run_ts = RUN_TS
    )
}

# ==== 14. Save outputs =======================================================

save_rds_cached(
  x = latest_tbl,
  timestamped_path = out_timestamped,
  latest_path = out_latest
)

message("\nDone: latest groundwater-level lookup table created.")
message("Rows with latest groundwater level: ", nrow(latest_tbl))
message("Latest output:")
message("  ", out_latest)
message("Timestamped output:")
message("  ", out_timestamped)
message("Raw chunk folder:")
message("  ", out_raw_chunk_dir)

# ==== 15. Suggested QA =======================================================

message("\nSuggested QA:")
message('  wl <- readRDS("', out_latest, '")')
message("  nrow(wl)")
message("  summary(wl$latest_wl_ft_bgs)")
message("  head(wl, 20)")
