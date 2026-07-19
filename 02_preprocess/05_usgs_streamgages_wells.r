# ==== 05_usgs_streamgages_wells.r ===========================================
##
## PURPOSE:
##   Download or load cached USGS/NWIS streamgage and groundwater-well metadata,
##   enrich each site with parameter lists and useful site metadata, and save
##   clean sf point layers for PortaTreasure2.
##
## KEY FIX:
##   Groundwater wells are retained even if they have few records.
##   The old workflow filtered wells by record count, which could drop many
##   useful well locations. Here, all downloaded groundwater sites are retained
##   by default, while record-count information is preserved for popups/QA.
##
## INPUTS:
##   USGS/NWIS web services, or cached CSVs in:
##
##     01_raw_data/usgs/
##
## OUTPUTS:
##   04_processed_data/rds/USGS_SW_final.rds
##   04_processed_data/rds/USGS_SW_final_<timestamp>.rds
##   04_processed_data/rds/USGS_GW_final.rds
##   04_processed_data/rds/USGS_GW_final_<timestamp>.rds
##   04_processed_data/gpkg/usgs_streamgages_wells.gpkg
##   04_processed_data/qa/usgs_streamgages_wells_qa_<timestamp>.csv
##
## NOTES:
##   - Site coordinates from NWIS are treated as NAD83 lon/lat, EPSG:4269.
##   - Outputs are transformed to WGS84, EPSG:4326, for Leaflet.
##   - Raw downloaded CSVs are cached so repeated runs do not hit NWIS unless
##     explicitly requested.
##

# ==== 1. Load configuration and helper functions =============================

source("00_config/config_paths.r")
source("00_config/config_run_flags.r")
source("03_functions/cache_helpers.r")
source("03_functions/spatial_helpers.r")

# ==== 2. Load packages =======================================================

suppressPackageStartupMessages({
  library(dataRetrieval)
  library(dplyr)
  library(purrr)
  library(readr)
  library(sf)
  library(tibble)
})

# ==== 3. User-facing switches ================================================
##
## FORCE_DOWNLOAD_* = TRUE will ignore cached CSVs and redownload from NWIS.
## FORCE_DOWNLOAD_* = FALSE will use cached CSVs if present; if missing, the
## script will download them automatically.

FORCE_DOWNLOAD_SW_SITES  <- FALSE
FORCE_DOWNLOAD_SW_PARAMS <- FALSE

FORCE_DOWNLOAD_GW_SITES  <- FALSE
FORCE_DOWNLOAD_GW_PARAMS <- FALSE
FORCE_DOWNLOAD_GW_META   <- FALSE

## Streamgages can still be filtered to sites with a meaningful daily-discharge
## record. Groundwater wells are kept by default.
KEEP_ALL_SW_SITES <- FALSE
KEEP_ALL_GW_SITES <- TRUE

SW_MIN_RECORDS <- 365
GW_MIN_RECORDS <- 10   # only used if KEEP_ALL_GW_SITES = FALSE

## Chunk sizes for robust NWIS calls.
PARAM_CHUNK_SIZE <- 30
META_CHUNK_SIZE  <- 500

WRITE_GPKG <- TRUE
WRITE_QA   <- TRUE

RUN_TS <- make_timestamp()

# ==== 4. Define cache and output paths =======================================

RAW_USGS_DIR <- file.path(DIR$raw, "usgs")
dir.create(RAW_USGS_DIR, recursive = TRUE, showWarnings = FALSE)

raw_sw_sites <- file.path(RAW_USGS_DIR, "usgs_streamgages_raw.csv")
raw_sw_params <- file.path(RAW_USGS_DIR, "usgs_streamgages_param_list.csv")

raw_gw_sites <- file.path(RAW_USGS_DIR, "usgs_groundwater_raw.csv")
raw_gw_params <- file.path(RAW_USGS_DIR, "usgs_groundwater_param_list.csv")
raw_gw_meta <- file.path(RAW_USGS_DIR, "usgs_groundwater_site_meta.csv")

out_sw_latest <- file.path(DIR$rds, "USGS_SW_final.rds")
out_sw_timestamped <- file.path(DIR$rds, timestamped_name("USGS_SW_final", "rds", RUN_TS))

out_gw_latest <- file.path(DIR$rds, "USGS_GW_final.rds")
out_gw_timestamped <- file.path(DIR$rds, timestamped_name("USGS_GW_final", "rds", RUN_TS))

out_gpkg <- file.path(DIR$gpkg, "usgs_streamgages_wells.gpkg")
out_qa <- file.path(DIR$qa, paste0("usgs_streamgages_wells_qa_", RUN_TS, ".csv"))

# ==== 5. Parameter translation helpers =======================================
##
## These short labels are used later in popups so users do not have to interpret
## raw NWIS parameter codes.

param_lookup <- c(
  "00060" = "Q_cfs",         # Discharge
  "00065" = "STG_ft",        # Gage height / stage
  "00010" = "TEMP_c",        # Water temperature
  "00045" = "PRECIP_in",     # Precipitation
  "00095" = "COND_uScm",     # Specific conductance
  "00400" = "pH",            # pH
  "72019" = "GW_ft_bgs",     # Groundwater level, feet below land surface
  "72020" = "GW_ft_amsl",    # Groundwater level, feet above mean sea level
  "99133" = "BATT_V"         # Battery voltage
)

translate_codes <- function(code_str) {
  
  if (is.na(code_str) || !nzchar(code_str)) return("")
  
  codes <- strsplit(code_str, ",")[[1]]
  codes <- codes[nzchar(codes)]
  
  translated <- sapply(codes, function(code) {
    if (code %in% names(param_lookup)) {
      param_lookup[[code]]
    } else {
      paste0("param_", code)
    }
  })
  
  paste(sort(unique(translated)), collapse = "; ")
}

# ==== 6. General helper functions ============================================

read_or_download_csv <- function(path, force_download, label, download_fun) {
  
  if (file.exists(path) && !force_download) {
    message("Reading cached ", label, ": ", path)
    return(readr::read_csv(path, show_col_types = FALSE))
  }
  
  message("Downloading ", label, "...")
  out <- download_fun()
  
  if (!is.data.frame(out) || nrow(out) == 0) {
    stop("Download returned zero rows for: ", label)
  }
  
  readr::write_csv(out, path)
  message("Cached ", label, ": ", path)
  
  out
}

fetch_nwis_by_status <- function(service, parameterCd = NULL,
                                 statuses = c("active", "inactive"),
                                 label = "NWIS sites") {
  
  purrr::map_dfr(statuses, function(st) {
    
    message("  Fetching ", label, " | service=", service, " | status=", st)
    
    args <- list(
      stateCd = "CA",
      service = service,
      siteStatus = st
    )
    
    if (!is.null(parameterCd)) {
      args$parameterCd <- parameterCd
    }
    
    dat <- try(
      do.call(dataRetrieval::whatNWISdata, args),
      silent = TRUE
    )
    
    if (inherits(dat, "try-error") || is.null(dat) || nrow(dat) == 0) {
      warning("No rows or failed call for ", label, " status=", st)
      return(tibble())
    }
    
    dat$siteStatus_download <- st
    dat
  })
}

fetch_params_chunked <- function(site_ids, chunk_size = PARAM_CHUNK_SIZE,
                                 label = "parameter list") {
  
  site_ids <- unique(as.character(site_ids))
  site_ids <- site_ids[nzchar(site_ids)]
  
  if (length(site_ids) == 0) {
    return(tibble())
  }
  
  chunks <- split(
    site_ids,
    ceiling(seq_along(site_ids) / chunk_size)
  )
  
  out_list <- vector("list", length(chunks))
  
  message("Fetching ", label, " in ", length(chunks), " chunk(s).")
  
  pb <- txtProgressBar(min = 0, max = length(chunks), style = 3)
  
  for (i in seq_along(chunks)) {
    
    out_list[[i]] <- tryCatch(
      {
        dat <- dataRetrieval::whatNWISdata(siteNumber = chunks[[i]])
        Sys.sleep(0.2)
        dat
      },
      error = function(e) {
        warning("Parameter chunk ", i, " failed: ", conditionMessage(e))
        tibble()
      }
    )
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  bind_rows(out_list)
}

fetch_site_meta_chunked <- function(site_ids, chunk_size = META_CHUNK_SIZE) {
  
  site_ids <- unique(as.character(site_ids))
  site_ids <- site_ids[nzchar(site_ids)]
  
  if (length(site_ids) == 0) {
    return(tibble())
  }
  
  chunks <- split(
    site_ids,
    ceiling(seq_along(site_ids) / chunk_size)
  )
  
  out_list <- vector("list", length(chunks))
  
  message("Fetching groundwater site metadata in ", length(chunks), " chunk(s).")
  
  pb <- txtProgressBar(min = 0, max = length(chunks), style = 3)
  
  for (i in seq_along(chunks)) {
    
    out_list[[i]] <- tryCatch(
      {
        dat <- dataRetrieval::readNWISsite(chunks[[i]])
        Sys.sleep(0.2)
        dat
      },
      error = function(e) {
        warning("Site metadata chunk ", i, " failed: ", conditionMessage(e))
        tibble()
      }
    )
    
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  bind_rows(out_list)
}

ensure_columns <- function(df, cols) {
  
  for (col in cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  
  df
}

collapse_param_list <- function(params) {
  
  if (!is.data.frame(params) || nrow(params) == 0) {
    return(tibble(site_no = character(), param_list = character()))
  }
  
  params <- params |> mutate(site_no = as.character(site_no))
  
  param_col <- dplyr::case_when(
    "parm_cd" %in% names(params) ~ "parm_cd",
    "parameter_cd" %in% names(params) ~ "parameter_cd",
    TRUE ~ NA_character_
  )
  
  if (is.na(param_col)) {
    warning("No parameter-code column found in parameter table.")
    return(tibble(site_no = character(), param_list = character()))
  }
  
  params |>
    transmute(
      site_no = as.character(site_no),
      parameter_cd = as.character(.data[[param_col]])
    ) |>
    filter(!is.na(site_no), !is.na(parameter_cd)) |>
    group_by(site_no) |>
    summarise(
      param_list = paste(sort(unique(parameter_cd)), collapse = ","),
      .groups = "drop"
    )
}

to_usgs_sf <- function(df, label) {
  
  df <- df |>
    filter(!is.na(latitude), !is.na(longitude))
  
  message(label, " rows with valid coordinates: ", nrow(df))
  
  df |>
    sf::st_as_sf(
      coords = c("longitude", "latitude"),
      crs = 4269,
      remove = FALSE
    ) |>
    sf::st_transform(4326) |>
    clean_sf_for_leaflet()
}

# ==== 7. Download or load raw site tables ====================================

sw_raw <- read_or_download_csv(
  path = raw_sw_sites,
  force_download = FORCE_DOWNLOAD_SW_SITES,
  label = "USGS streamgage sites",
  download_fun = function() {
    ## Daily discharge sites. This is the main streamgage layer.
    fetch_nwis_by_status(
      service = "dv",
      parameterCd = "00060",
      label = "USGS streamgage sites"
    )
  }
)

gw_raw <- read_or_download_csv(
  path = raw_gw_sites,
  force_download = FORCE_DOWNLOAD_GW_SITES,
  label = "USGS groundwater sites",
  download_fun = function() {
    ## Groundwater-level sites. KEEP_ALL_GW_SITES controls filtering later.
    fetch_nwis_by_status(
      service = "gwlevels",
      parameterCd = NULL,
      label = "USGS groundwater sites"
    )
  }
)

# ==== 8. Download or load parameter lists ====================================

sw_params <- read_or_download_csv(
  path = raw_sw_params,
  force_download = FORCE_DOWNLOAD_SW_PARAMS,
  label = "USGS streamgage parameter list",
  download_fun = function() {
    fetch_params_chunked(sw_raw$site_no, label = "streamgage parameter list")
  }
)

gw_params <- read_or_download_csv(
  path = raw_gw_params,
  force_download = FORCE_DOWNLOAD_GW_PARAMS,
  label = "USGS groundwater parameter list",
  download_fun = function() {
    fetch_params_chunked(gw_raw$site_no, label = "groundwater parameter list")
  }
)

# ==== 9. Download or load groundwater site metadata ==========================
##
## This table adds useful well-depth and aquifer fields where available.

gw_meta <- read_or_download_csv(
  path = raw_gw_meta,
  force_download = FORCE_DOWNLOAD_GW_META,
  label = "USGS groundwater site metadata",
  download_fun = function() {
    fetch_site_meta_chunked(gw_raw$site_no)
  }
)

# ==== 10. Standardize raw tables =============================================

sw_raw <- sw_raw |> mutate(site_no = as.character(site_no))
gw_raw <- gw_raw |> mutate(site_no = as.character(site_no))
sw_params <- sw_params |> mutate(site_no = as.character(site_no))
gw_params <- gw_params |> mutate(site_no = as.character(site_no))
gw_meta <- gw_meta |> mutate(site_no = as.character(site_no))

sw_param_summary <- collapse_param_list(sw_params)
gw_param_summary <- collapse_param_list(gw_params)

# ==== 11. Build streamgage output ============================================

sw_raw <- ensure_columns(
  sw_raw,
  c(
    "site_no", "station_nm", "dec_lat_va", "dec_long_va", "alt_va",
    "site_tp_cd", "begin_date", "end_date", "count_nu",
    "siteStatus_download"
  )
)

sw_base <- sw_raw |>
  mutate(
    count_nu = suppressWarnings(as.numeric(count_nu))
  )

if (!KEEP_ALL_SW_SITES) {
  sw_base <- sw_base |>
    filter(is.na(count_nu) | count_nu >= SW_MIN_RECORDS)
}

sw_final <- sw_base |>
  arrange(site_no, desc(coalesce(count_nu, -Inf))) |>
  distinct(site_no, .keep_all = TRUE) |>
  left_join(sw_param_summary, by = "site_no") |>
  transmute(
    site_no = as.character(site_no),
    name = station_nm,
    latitude = suppressWarnings(as.numeric(dec_lat_va)),
    longitude = suppressWarnings(as.numeric(dec_long_va)),
    elev_ft = suppressWarnings(as.numeric(alt_va)),
    site_type = site_tp_cd,
    start_date = as.character(begin_date),
    end_date = as.character(end_date),
    count_nu = count_nu,
    status = siteStatus_download,
    param_list = coalesce(param_list, ""),
    source = "USGS streamgage",
    measurements = purrr::map_chr(param_list, translate_codes)
  )

sw_sf <- to_usgs_sf(sw_final, "USGS streamgage")

# ==== 12. Build groundwater-well output ======================================

gw_raw <- ensure_columns(
  gw_raw,
  c(
    "site_no", "station_nm", "dec_lat_va", "dec_long_va", "alt_va",
    "site_tp_cd", "begin_date", "end_date", "count_nu",
    "siteStatus_download"
  )
)

gw_meta <- ensure_columns(
  gw_meta,
  c(
    "site_no", "well_depth_va", "hole_depth_va",
    "aqfr_cd", "aqfr_type_cd", "nat_aqfr_cd"
  )
)

gw_base <- gw_raw |>
  mutate(
    count_nu = suppressWarnings(as.numeric(count_nu))
  )

if (!KEEP_ALL_GW_SITES) {
  gw_base <- gw_base |>
    filter(is.na(count_nu) | count_nu >= GW_MIN_RECORDS)
}

gw_final <- gw_base |>
  arrange(site_no, desc(coalesce(count_nu, -Inf))) |>
  distinct(site_no, .keep_all = TRUE) |>
  left_join(gw_param_summary, by = "site_no") |>
  left_join(
    gw_meta |>
      transmute(
        site_no = as.character(site_no),
        well_depth_ft = suppressWarnings(as.numeric(well_depth_va)),
        hole_depth_ft = suppressWarnings(as.numeric(hole_depth_va)),
        aqfr_cd = as.character(aqfr_cd),
        aqfr_type_cd = as.character(aqfr_type_cd),
        nat_aqfr_cd = as.character(nat_aqfr_cd)
      ) |>
      distinct(site_no, .keep_all = TRUE),
    by = "site_no"
  ) |>
  transmute(
    site_no = as.character(site_no),
    name = station_nm,
    latitude = suppressWarnings(as.numeric(dec_lat_va)),
    longitude = suppressWarnings(as.numeric(dec_long_va)),
    elev_ft = suppressWarnings(as.numeric(alt_va)),
    site_type = site_tp_cd,
    start_date = as.character(begin_date),
    end_date = as.character(end_date),
    count_nu = count_nu,
    status = siteStatus_download,
    well_depth_ft = well_depth_ft,
    hole_depth_ft = hole_depth_ft,
    aqfr_cd = aqfr_cd,
    aqfr_type_cd = aqfr_type_cd,
    nat_aqfr_cd = nat_aqfr_cd,
    param_list = coalesce(param_list, ""),
    source = "USGS groundwater well",
    measurements = purrr::map_chr(param_list, translate_codes)
  )

gw_sf <- to_usgs_sf(gw_final, "USGS groundwater well")

# ==== 13. Save RDS outputs ===================================================

save_rds_cached(
  x = sw_sf,
  timestamped_path = out_sw_timestamped,
  latest_path = out_sw_latest
)

save_rds_cached(
  x = gw_sf,
  timestamped_path = out_gw_timestamped,
  latest_path = out_gw_latest
)

# ==== 14. Save GPKG output ===================================================

if (WRITE_GPKG) {
  
  if (file.exists(out_gpkg)) {
    file.remove(out_gpkg)
  }
  
  sf::st_write(
    sw_sf,
    dsn = out_gpkg,
    layer = "usgs_streamgages",
    quiet = TRUE
  )
  
  sf::st_write(
    gw_sf,
    dsn = out_gpkg,
    layer = "usgs_groundwater_wells",
    quiet = TRUE
  )
  
  message("Saved GPKG: ", out_gpkg)
}

# ==== 15. Save QA output =====================================================

if (WRITE_QA) {
  
  qa <- tibble::tibble(
    layer_id = c("usgs_streamgages", "usgs_groundwater_wells"),
    raw_rows = c(nrow(sw_raw), nrow(gw_raw)),
    final_rows = c(nrow(sw_sf), nrow(gw_sf)),
    keep_all_sites = c(KEEP_ALL_SW_SITES, KEEP_ALL_GW_SITES),
    min_records_filter = c(SW_MIN_RECORDS, GW_MIN_RECORDS),
    missing_coordinates_after_final = c(
      sum(is.na(sw_final$latitude) | is.na(sw_final$longitude)),
      sum(is.na(gw_final$latitude) | is.na(gw_final$longitude))
    ),
    active_count = c(
      sum(tolower(sw_sf$status) == "active", na.rm = TRUE),
      sum(tolower(gw_sf$status) == "active", na.rm = TRUE)
    ),
    inactive_count = c(
      sum(tolower(sw_sf$status) == "inactive", na.rm = TRUE),
      sum(tolower(gw_sf$status) == "inactive", na.rm = TRUE)
    ),
    run_timestamp = RUN_TS
  )
  
  readr::write_csv(qa, out_qa)
  
  message("Saved QA CSV: ", out_qa)
  print(qa)
}

# ==== 16. Final summary ======================================================

message("\nDone: USGS streamgage/well preprocessing complete.")

message("\nUSGS streamgages:")
message("  rows: ", nrow(sw_sf))
message("  latest RDS: ", out_sw_latest)

message("\nUSGS groundwater wells:")
message("  rows: ", nrow(gw_sf))
message("  latest RDS: ", out_gw_latest)

message("\nGroundwater well record-count summary:")
print(summary(gw_sf$count_nu))

message("\nGroundwater well status table:")
print(table(gw_sf$status, useNA = "always"))