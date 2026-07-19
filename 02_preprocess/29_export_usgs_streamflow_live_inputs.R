# ==== 29_export_usgs_streamflow_live_inputs.R ================================
##
## PURPOSE:
##   Export a compact, committed station-index CSV for the BRIM live USGS
##   streamflow feed repository.
##
## WHY THIS EXISTS:
##   The scheduled GitHub Action in `brim-live-data-feeds` cannot read the local
##   BRIM RDS cache.  This script converts the existing map-ready USGS
##   streamgage cache into a small CSV backbone that the live-feed workflow can
##   use every 4 hours without rediscovering sites from NWIS.
##
## INPUTS, from PortaTreasure2 root:
##   04_processed_data/cache/latest/usgs_streamgages_map.rds
##   01_raw_data/cdec/cdec_usgs_cnrfc.csv                         optional
##   04_processed_data/cache/latest/cnrfc_stream_map.rds           optional
##
## OUTPUTS, into the feed repo:
##   brim-live-data-feeds/data/input/usgs_streamgages_index_ca.csv
##   brim-live-data-feeds/data/input/usgs_cnrfc_nwsli_crosswalk.csv
##
## HOW TO RUN FROM PORTATREASURE2 ROOT:
##   source("02_preprocess/29_export_usgs_streamflow_live_inputs.R")
## ============================================================================

# ---- 1. Packages ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sf)
  library(tibble)
})

# ---- 2. Paths ---------------------------------------------------------------

feed_repo_dir <- Sys.getenv(
  "BRIM_LIVE_DATA_FEEDS_DIR",
  unset = "brim-live-data-feeds"
)

input_dir <- file.path(feed_repo_dir, "data", "input")
dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)

usgs_cache_path <- Sys.getenv(
  "BRIM_USGS_STREAMGAGE_CACHE_RDS",
  unset = file.path("04_processed_data", "cache", "latest", "usgs_streamgages_map.rds")
)

cnrfc_stream_cache_path <- Sys.getenv(
  "BRIM_CNRFC_STREAM_CACHE_RDS",
  unset = file.path("04_processed_data", "cache", "latest", "cnrfc_stream_map.rds")
)

cdec_usgs_cnrfc_csv <- Sys.getenv(
  "BRIM_CDEC_USGS_CNRFC_CSV",
  unset = file.path("01_raw_data", "cdec", "cdec_usgs_cnrfc.csv")
)

out_station_index <- file.path(input_dir, "usgs_streamgages_index_ca.csv")
out_xwalk <- file.path(input_dir, "usgs_cnrfc_nwsli_crosswalk.csv")

# ---- 3. Small helpers -------------------------------------------------------

pt_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | is.na(x) | toupper(x) %in% c("NA", "NULL", "NAN")] <- NA_character_
  x
}

pt_site_no <- function(x) {
  x <- pt_chr(x)
  x <- gsub("\\.0$", "", x)
  x <- gsub("[^0-9]", "", x)
  x[nchar(x) == 0] <- NA_character_
  x
}

pt_num <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

pt_drop_geometry <- function(x) {
  if (inherits(x, "sf")) {
    return(sf::st_drop_geometry(x))
  }
  as.data.frame(x)
}

pt_ensure_cols <- function(df, cols) {
  for (nm in cols) {
    if (!nm %in% names(df)) {
      df[[nm]] <- NA
    }
  }
  df
}

# ---- 4. Export USGS streamgage station index --------------------------------

if (!file.exists(usgs_cache_path)) {
  stop("USGS streamgage cache not found: ", usgs_cache_path)
}

usgs_raw <- readRDS(usgs_cache_path)
usgs_tbl <- pt_drop_geometry(usgs_raw)

usgs_tbl <- pt_ensure_cols(
  usgs_tbl,
  c(
    "site_no", "name", "latitude", "longitude", "elev_ft", "site_type",
    "start_date", "end_date", "count_nu", "status", "param_list",
    "source", "measurements"
  )
)

usgs_index <- usgs_tbl |>
  dplyr::transmute(
    site_no = pt_site_no(.data$site_no),
    station_nm = pt_chr(.data$name),
    latitude = pt_num(.data$latitude),
    longitude = pt_num(.data$longitude),
    elev_ft = pt_num(.data$elev_ft),
    site_type = pt_chr(.data$site_type),
    start_date = pt_chr(.data$start_date),
    end_date = pt_chr(.data$end_date),
    count_nu = suppressWarnings(as.integer(.data$count_nu)),
    status = pt_chr(.data$status),
    param_list = pt_chr(.data$param_list),
    measurements = pt_chr(.data$measurements),
    source = dplyr::coalesce(pt_chr(.data$source), "USGS streamgage")
  ) |>
  dplyr::filter(!is.na(.data$site_no), !is.na(.data$latitude), !is.na(.data$longitude)) |>
  dplyr::arrange(.data$site_no) |>
  dplyr::distinct(.data$site_no, .keep_all = TRUE)

readr::write_csv(usgs_index, out_station_index)
message("Saved USGS streamflow live station index: ", out_station_index)
message("  rows: ", nrow(usgs_index))
message("  active rows: ", sum(tolower(usgs_index$status) == "active", na.rm = TRUE))

# ---- 5. Export optional USGS -> CNRFC/NWSLI crosswalk -----------------------
##
## This is link enrichment only.  The live USGS feed must never depend on this
## file for values.  Current flow and stage values come from USGS NWIS.

xwalk_out <- tibble::tibble(
  site_no = character(),
  nwsli = character(),
  cdec_id = character(),
  cdec_station_name = character(),
  usgs_station_name_xwalk = character(),
  cdec_group = character(),
  cdec_basin = character(),
  county = character(),
  nws_flood_stage_ft = numeric(),
  cnrfc_channel = character(),
  cnrfc_location = character(),
  cnrfc_nickname = character(),
  cnrfc_gage_class1 = character(),
  cnrfc_gage_class2 = character(),
  crosswalk_source = character(),
  crosswalk_note = character()
)

if (file.exists(cdec_usgs_cnrfc_csv)) {
  cdec_x <- readr::read_csv(
    cdec_usgs_cnrfc_csv,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  cdec_x <- pt_ensure_cols(
    cdec_x,
    c(
      "id_usgs", "id_nws", "id_cdec", "name_cdec", "name_usgs",
      "group_cdec", "basin_cdec", "county_cdec", "nwsfloodstg_ft_usgs"
    )
  )

  xwalk_out <- cdec_x |>
    dplyr::transmute(
      site_no = pt_site_no(.data$id_usgs),
      nwsli = toupper(pt_chr(.data$id_nws)),
      cdec_id = toupper(pt_chr(.data$id_cdec)),
      cdec_station_name = pt_chr(.data$name_cdec),
      usgs_station_name_xwalk = pt_chr(.data$name_usgs),
      cdec_group = pt_chr(.data$group_cdec),
      cdec_basin = pt_chr(.data$basin_cdec),
      county = pt_chr(.data$county_cdec),
      nws_flood_stage_ft = suppressWarnings(as.numeric(.data$nwsfloodstg_ft_usgs)),
      crosswalk_source = "01_raw_data/cdec/cdec_usgs_cnrfc.csv",
      crosswalk_note = "Legacy CDEC/USGS/CNRFC crosswalk used for link enrichment only."
    ) |>
    dplyr::filter(!is.na(.data$site_no), !is.na(.data$nwsli), .data$nwsli != "") |>
    dplyr::arrange(.data$site_no, .data$nwsli) |>
    dplyr::distinct(.data$site_no, .keep_all = TRUE)
}

if (file.exists(cnrfc_stream_cache_path) && nrow(xwalk_out) > 0) {
  cnrfc_raw <- readRDS(cnrfc_stream_cache_path)
  cnrfc_tbl <- pt_drop_geometry(cnrfc_raw)

  cnrfc_tbl <- pt_ensure_cols(
    cnrfc_tbl,
    c("nwsid", "channel", "loc", "nickname", "gage_class1", "gage_class2")
  )

  cnrfc_lookup <- cnrfc_tbl |>
    dplyr::transmute(
      nwsli = toupper(pt_chr(.data$nwsid)),
      cnrfc_channel = pt_chr(.data$channel),
      cnrfc_location = pt_chr(.data$loc),
      cnrfc_nickname = pt_chr(.data$nickname),
      cnrfc_gage_class1 = pt_chr(.data$gage_class1),
      cnrfc_gage_class2 = pt_chr(.data$gage_class2)
    ) |>
    dplyr::filter(!is.na(.data$nwsli), .data$nwsli != "") |>
    dplyr::distinct(.data$nwsli, .keep_all = TRUE)

  xwalk_out <- xwalk_out |>
    dplyr::left_join(cnrfc_lookup, by = "nwsli") |>
    dplyr::mutate(
      crosswalk_note = dplyr::if_else(
        !is.na(.data$cnrfc_nickname) & .data$cnrfc_nickname != "",
        paste0(.data$crosswalk_note, " CNRFC cache metadata joined by NWSLI."),
        .data$crosswalk_note
      )
    )
}

readr::write_csv(xwalk_out, out_xwalk)
message("Saved optional USGS-CNRFC/NWSLI crosswalk: ", out_xwalk)
message("  rows: ", nrow(xwalk_out))
message("  NWSLI rows: ", sum(!is.na(xwalk_out$nwsli) & xwalk_out$nwsli != ""))

message("\nDone: exported USGS streamflow live-feed inputs.")
