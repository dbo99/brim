# ==== 04_build_portatreasure2_core_map.r ====================================
##
## PURPOSE:
##   Build the current core PortaTreasure2 Leaflet HTML map from map-ready
##   cached layers.
##
## THIS SCRIPT SHOULD:
##   - read cached map-ready RDS files
##   - initialize Leaflet
##   - add basemaps
##   - add panes
##   - add cached polygon and point layers
##   - add controls
##   - export timestamped HTML
##
## THIS SCRIPT SHOULD NOT:
##   - download USGS data
##   - parse raw CNRFC text files
##   - dissolve BLM polygons
##   - compute %BLM
##   - simplify geometry
##   - build popup_html
##

# ==== 1. Load configuration ==================================================

source("00_config/config_paths.r")
source("00_config/config_map_display.r")
source("00_config/config_labels.r")
source("00_config/config_local_layer_registry.r")

# ==== 2. Load helper functions ==============================================

source("03_functions/cache_helpers.r")
source("03_functions/leaflet_core_helpers.r")
source("03_functions/leaflet_loading_helpers.r")
source("03_functions/leaflet_ops_live_helpers.r")
source("03_functions/leaflet_layer_helpers.r")
source("03_functions/leaflet_label_helpers.r")
source("03_functions/leaflet_huc_theme_helpers.r")
source("03_functions/leaflet_tools_adddata_helpers.r")
source("03_functions/leaflet_local_upload_helpers.r")

# ==== 3. Load packages =======================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(purrr)
  library(leaflet)
  library(htmlwidgets)
})

# ==== 4. Define output path ==================================================

RUN_TS <- make_timestamp()

OUT_HTML <- file.path(
  DIR$html,
  paste0("PortaTreasure2_core_", RUN_TS, ".html")
)

# ==== 5. Read cached map layers =============================================

pt_read_first_existing_rds <- function(paths, label = "optional layer") {
  paths <- as.character(paths)
  paths <- paths[!is.na(paths) & paths != ""]
  hit <- paths[file.exists(paths)]

  if (length(hit) == 0) {
    message("Optional ", label, " RDS not found; skipping. Checked: ", paste(paths, collapse = "; "))
    return(NULL)
  }

  readRDS(hit[[1]])
}

CONVEYANCE_BRIM_EXPORT_DIR <- file.path(
  "02_preprocess",
  "66_conveyance_pipeline",
  "output",
  "brim_exports"
)

layers <- list(
  blm_core            = readRDS(file.path(DIR$cache_last, "blm_core_map.rds")),
  blm_diffs           = readRDS(file.path(DIR$cache_last, "blm_diffs_map.rds")),
  huc_all             = readRDS(file.path(DIR$cache_last, "huc_all_map.rds")),
  gw                  = readRDS(file.path(DIR$cache_last, "gw_bull118_map.rds")),
  county              = readRDS(file.path(DIR$cache_last, "county_map.rds")),
  cnrfc_basins        = readRDS(file.path(DIR$cache_last, "cnrfc_basins_map.rds")),
  field_office_outer  = readRDS(file.path(DIR$cache_last, "field_office_outer_map.rds")),
  cnrfc_stream        = readRDS(file.path(DIR$cache_last, "cnrfc_stream_map.rds")),
  cnrfc_precip        = readRDS(file.path(DIR$cache_last, "cnrfc_precip_map.rds")),

  ## Local CDEC reservoir stations are intentionally not read unless enabled.
  ## This keeps the layer out of both the Local panel and the generated HTML
  ## payload when Ops Live reservoirs are serving as the main reservoir layer.
  cdec_reservoir_stations = if (isTRUE(MAP_DISPLAY$add_cdec_reservoir_stations)) {
    readRDS(file.path(DIR$cache_last, "cdec_reservoir_stations_map.rds"))
  } else {
    NULL
  },

  usgs_sw             = readRDS(file.path(DIR$cache_last, "usgs_streamgages_map.rds")),
  usgs_gw             = readRDS(file.path(DIR$cache_last, "usgs_wells_map.rds")),
  blm_noc_drilled_wells = pt_read_first_existing_rds(
    file.path(DIR$cache_last, "blm_noc_drilled_wells_map.rds"),
    label = "BLM-drilled wells | NOC"
  ),
  mojave_2025_gw_well_inventory = pt_read_first_existing_rds(
    file.path(DIR$cache_last, "mojave_2025_gw_well_inventory_map.rds"),
    label = "GW wells | 2025 Mojave-BLM limited field check"
  ),
  swrcb_pod_wr_blm    = readRDS(file.path(DIR$cache_last, "swrcb_pod_wr_blm_map.rds")),
  springs             = readRDS(file.path(DIR$cache_last, "springs_map.rds")),
  ## Local SCAN / Snow Pillow station layers are intentionally not read unless
  ## enabled.  The richer Ops Live layers carry these networks now, so this
  ## avoids duplicating stable station networks in Local and keeps them out of
  ## the generated HTML payload when disabled.
  scan_stations = if (isTRUE(MAP_DISPLAY$add_scan_stations)) {
    readRDS(file.path(DIR$cache_last, "scan_stations_map.rds"))
  } else {
    NULL
  },
  snow_pillows = if (isTRUE(MAP_DISPLAY$add_snow_pillows)) {
    readRDS(file.path(DIR$cache_last, "snow_pillows_map.rds"))
  } else {
    NULL
  },
  blm_offices         = readRDS(file.path(DIR$cache_last, "blm_offices_map.rds")),
  project_areas       = readRDS(file.path(DIR$cache_last, "project_areas_map.rds")),
  labels_all          = readRDS(file.path(DIR$cache_last, "labels_all_map.rds")),
  calsim3_arcs        = readRDS(file.path(DIR$cache_last, "calsim3_arcs_map.rds")),
  calsim3_nodes       = readRDS(file.path(DIR$cache_last, "calsim3_nodes_map.rds")),

  ## Consolidated pipeline output. Read only when enabled so disabling the layer
  ## also keeps its geometry and popup payload out of the standalone HTML.
  brim_mapped_conveyance = if (isTRUE(MAP_DISPLAY$add_brim_mapped_conveyance)) {
    pt_read_first_existing_rds(
      file.path(
        CONVEYANCE_BRIM_EXPORT_DIR,
        "conveyance_segments_brim_map.rds"
      ),
      label = "Water conveyance | BRIM mapped segments"
    )
  } else {
    NULL
  },

  brim_mapped_conveyance_labels = if (
    isTRUE(MAP_DISPLAY$add_brim_mapped_conveyance) &&
    isTRUE(MAP_DISPLAY$add_labels)
  ) {
    pt_read_first_existing_rds(
      file.path(
        CONVEYANCE_BRIM_EXPORT_DIR,
        "conveyance_labels_brim_map.rds"
      ),
      label = "Water conveyance | BRIM mapped labels"
    )
  } else {
    NULL
  },

  brim_mapped_conveyance_facilities = if (
    isTRUE(MAP_DISPLAY$add_brim_mapped_conveyance)
  ) {
    pt_read_first_existing_rds(
      file.path(
        CONVEYANCE_BRIM_EXPORT_DIR,
        "conveyance_facilities_brim_map.rds"
      ),
      label = "Water conveyance | BRIM mapped facility lookup"
    )
  } else {
    NULL
  },

  ## Legacy source-specific layers remain available for QA and rollback, but
  ## their RDS files are not read or embedded while their switches are FALSE.
  major_conveyance = if (isTRUE(MAP_DISPLAY$add_major_conveyance)) {
    readRDS(file.path(DIR$cache_last, "major_conveyance_map.rds"))
  } else {
    NULL
  },

  cnrfc_fnf_delta     = readRDS(file.path(DIR$cache_last, "cnrfc_fnf_delta_map.rds")),
  cnrfc_basin_availability = pt_read_first_existing_rds(
    c(
      file.path(DIR$cache_last, "cnrfc_basin_product_availability_map.rds"),
      file.path(DIR$rds, "cnrfc_basin_product_availability_map.rds")
    ),
    label = "CNRFC basin product availability"
  ),
  cnrfc_active_river_reservoir_forecast_points = pt_read_first_existing_rds(
    c(
      file.path(DIR$cache_last, "cnrfc_active_river_reservoir_forecast_points_map.rds"),
      file.path(DIR$rds, "cnrfc_active_river_reservoir_forecast_points_map.rds")
    ),
    label = "CNRFC active river/reservoir forecast points"
  ),
  cnrfc_precip_weather_stations = pt_read_first_existing_rds(
    c(
      file.path(DIR$cache_last, "cnrfc_precip_weather_stations_map.rds"),
      file.path(DIR$rds, "cnrfc_precip_weather_stations_map.rds")
    ),
    label = "CNRFC precip/weather stations"
  ),
  cnrfc_precip_weather_station_catalog_local = pt_read_first_existing_rds(
    c(
      file.path(DIR$cache_last, "cnrfc_precip_weather_station_catalog_local_map.rds"),
      file.path(DIR$rds, "cnrfc_precip_weather_station_catalog_local_map.rds")
    ),
    label = "CNRFC precip/weather station catalog local layer"
  ),
  x2_km               = readRDS(file.path(DIR$cache_last, "x2_km_map.rds")),

  deltamapr_canals = if (isTRUE(MAP_DISPLAY$add_deltamapr_canals)) {
    readRDS(file.path(DIR$cache_last, "deltamapr_canals_map.rds"))
  } else {
    NULL
  },

  water_districts     = readRDS(file.path(DIR$cache_last, "water_districts_map.rds")),
  rwqcb_regions       = readRDS(file.path(DIR$cache_last, "rwqcb_regions_map.rds")),
  reference_layers    = readRDS(file.path(DIR$cache_last, "reference_layers_all_map.rds"))
)

message("Cached layers loaded.")

# ---- 5.1 Enrich Local USGS streamgages with current Ops data flags ---------
##
## The Ops Live USGS streamflow GeoJSON intentionally uses the same station
## backbone as the Local streamgage catalog.  Therefore, generic "in Ops" file
## membership is not a useful visual distinction.  The useful Local catalog cue
## is whether the current Ops feed joined a recent/latest discharge OR stage
## value to that station.
##
## This join is updateable: rerun 58_audit_usgs_streamgage_catalog_vs_ops.R
## after refreshing the feed, then rebuild the map.

pt_clean_site_no_local <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("\\.0$", "", x)
  x <- gsub("^USGS[-_: /]*", "", x, ignore.case = TRUE)
  x <- gsub("^NWIS[-_: /]*", "", x, ignore.case = TRUE)
  x <- gsub("\\s+", "", x)
  x[x %in% c("", "NA", "NaN", "NULL", "null", "undefined")] <- NA_character_
  x
}

pt_boolish_local <- function(x) {
  raw <- tolower(trimws(as.character(x)))
  raw %in% c("true", "t", "1", "yes", "y")
}

pt_boolish_nullable_local <- function(x) {
  raw <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(raw))
  out[raw %in% c("true", "t", "1", "yes", "y")] <- TRUE
  out[raw %in% c("false", "f", "0", "no", "n")] <- FALSE
  out
}

pt_first_existing_path_local <- function(paths) {
  paths <- as.character(paths)
  paths <- paths[!is.na(paths) & nzchar(paths)]
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

pt_enrich_usgs_streamgages_with_current_ops_data <- function(usgs_sw) {
  if (!inherits(usgs_sw, "sf") && !is.data.frame(usgs_sw)) return(usgs_sw)
  if (!"site_no" %in% names(usgs_sw)) return(usgs_sw)

  sync_path <- file.path(DIR$qa, "usgs_streamgage_catalog_vs_ops_sync_matrix_latest.csv")
  if (!file.exists(sync_path)) {
    message("USGS streamgage current-data sync matrix not found; Local streamgage current-data halos disabled. Run 58_audit_usgs_streamgage_catalog_vs_ops.R to enable.")
    usgs_sw$current_ops_data_value_available <- FALSE
    usgs_sw$current_ops_data_class <- NA_character_
    usgs_sw$current_ops_data_age_hours <- NA_real_
    usgs_sw$current_ops_discharge_value_available <- FALSE
    return(usgs_sw)
  }

  sync <- tryCatch(
    utils::read.csv(sync_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      warning("Could not read USGS streamgage current-data sync matrix: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(sync) || !"site_no" %in% names(sync)) {
    usgs_sw$current_ops_data_value_available <- FALSE
    usgs_sw$current_ops_data_class <- NA_character_
    usgs_sw$current_ops_data_age_hours <- NA_real_
    usgs_sw$current_ops_discharge_value_available <- FALSE
    return(usgs_sw)
  }

  wanted <- intersect(
    c(
      "site_no",
      "ops_current_data_class",
      "ops_current_data_age_hours",
      "ops_current_discharge_class",
      "ops_has_latest_iv_q",
      "ops_has_latest_iv_stage",
      "ops_q_obs_age_hours",
      "ops_stage_obs_age_hours",
      "ops_q_cfs",
      "current_ops_snapshot_audit_timestamp",
      "ops_streamflow_source_audit_timestamp"
    ),
    names(sync)
  )

  sync2 <- sync[, wanted, drop = FALSE]
  sync2$site_no <- pt_clean_site_no_local(sync2$site_no)
  sync2 <- sync2[!is.na(sync2$site_no) & sync2$site_no != "", , drop = FALSE]
  sync2 <- sync2[!duplicated(sync2$site_no), , drop = FALSE]

  for (nm in c(
    "ops_current_data_class", "ops_current_discharge_class",
    "current_ops_snapshot_audit_timestamp", "ops_streamflow_source_audit_timestamp"
  )) {
    if (!nm %in% names(sync2)) sync2[[nm]] <- NA_character_
  }
  for (nm in c("ops_current_data_age_hours", "ops_q_obs_age_hours", "ops_stage_obs_age_hours", "ops_q_cfs")) {
    if (!nm %in% names(sync2)) sync2[[nm]] <- NA_real_
  }
  for (nm in c("ops_has_latest_iv_q", "ops_has_latest_iv_stage")) {
    if (!nm %in% names(sync2)) sync2[[nm]] <- NA
  }

  sync2 <- sync2 |>
    dplyr::transmute(
      site_no = .data$site_no,
      current_ops_data_class = dplyr::coalesce(
        as.character(.data$ops_current_data_class),
        as.character(.data$ops_current_discharge_class)
      ),
      current_ops_discharge_class = as.character(.data$ops_current_discharge_class),
      current_ops_has_latest_iv_q = pt_boolish_local(.data$ops_has_latest_iv_q),
      current_ops_has_latest_iv_stage = pt_boolish_local(.data$ops_has_latest_iv_stage),
      current_ops_data_age_hours = dplyr::coalesce(
        suppressWarnings(as.numeric(.data$ops_current_data_age_hours)),
        pmin(
          suppressWarnings(as.numeric(.data$ops_q_obs_age_hours)),
          suppressWarnings(as.numeric(.data$ops_stage_obs_age_hours)),
          na.rm = TRUE
        )
      ),
      current_ops_q_cfs = suppressWarnings(as.numeric(.data$ops_q_cfs)),
      current_ops_data_audit_timestamp = dplyr::coalesce(
        as.character(.data$ops_streamflow_source_audit_timestamp),
        as.character(.data$current_ops_snapshot_audit_timestamp)
      )
    ) |>
    dplyr::mutate(
      current_ops_data_age_hours = ifelse(is.infinite(.data$current_ops_data_age_hours), NA_real_, .data$current_ops_data_age_hours),
      current_ops_data_value_available = .data$current_ops_data_class %in% c(
        "latest_data_<=6h",
        "latest_data_6_24h",
        "latest_data_24_72h",
        "latest_discharge_<=6h",
        "latest_discharge_6_24h",
        "latest_discharge_24_72h"
      ),
      ## Backward-compatible field name for older helper code.
      current_ops_discharge_value_available = .data$current_ops_data_value_available
    )

  usgs_sw$site_no <- pt_clean_site_no_local(usgs_sw$site_no)
  out <- dplyr::left_join(usgs_sw, sync2, by = "site_no")
  out$current_ops_data_value_available[is.na(out$current_ops_data_value_available)] <- FALSE
  out$current_ops_discharge_value_available[is.na(out$current_ops_discharge_value_available)] <- FALSE

  message(
    "USGS streamgage current-data sync joined: ",
    sum(out$current_ops_data_value_available %in% TRUE, na.rm = TRUE),
    " of ", nrow(out), " Local streamgages have recent/current Ops discharge or stage values."
  )

  out
}

pt_enrich_usgs_streamgages_with_blm_distance <- function(usgs_sw) {
  if (!inherits(usgs_sw, "sf") && !is.data.frame(usgs_sw)) return(usgs_sw)
  if (!"site_no" %in% names(usgs_sw)) return(usgs_sw)

  index_path <- pt_first_existing_path_local(c(
    Sys.getenv("USGS_STREAMFLOW_LIVE_INDEX_CSV", unset = ""),
    file.path("brim-live-data-feeds", "data", "input", "usgs_streamgages_index_ca.csv"),
    if (exists("DIR") && !is.null(DIR$root)) file.path(DIR$root, "brim-live-data-feeds", "data", "input", "usgs_streamgages_index_ca.csv") else NA_character_
  ))

  if (is.na(index_path)) {
    message("USGS streamgage BLM-distance index not found; Local streamgage BLM filters will be unavailable. Run 49_update_usgs_streamflow_blm_distance_fields.R after refreshing BLM lands.")
    if (!"on_blm_ca" %in% names(usgs_sw)) usgs_sw$on_blm_ca <- NA
    if (!"dist_to_blm_mi" %in% names(usgs_sw)) usgs_sw$dist_to_blm_mi <- NA_real_
    if (!"dist_to_blm_ft" %in% names(usgs_sw)) usgs_sw$dist_to_blm_ft <- NA_real_
    return(usgs_sw)
  }

  idx <- tryCatch(
    utils::read.csv(index_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      warning("Could not read USGS streamgage BLM-distance index: ", conditionMessage(e))
      NULL
    }
  )

  needed <- c("site_no", "on_blm_ca", "dist_to_blm_mi", "dist_to_blm_ft")
  if (is.null(idx) || !"site_no" %in% names(idx) || !any(c("on_blm_ca", "dist_to_blm_mi", "dist_to_blm_ft") %in% names(idx))) {
    message("USGS streamgage BLM-distance index lacks distance fields; Local streamgage BLM filters will be unavailable. Run 49_update_usgs_streamflow_blm_distance_fields.R after refreshing BLM lands.")
    if (!"on_blm_ca" %in% names(usgs_sw)) usgs_sw$on_blm_ca <- NA
    if (!"dist_to_blm_mi" %in% names(usgs_sw)) usgs_sw$dist_to_blm_mi <- NA_real_
    if (!"dist_to_blm_ft" %in% names(usgs_sw)) usgs_sw$dist_to_blm_ft <- NA_real_
    return(usgs_sw)
  }

  for (nm in setdiff(needed, names(idx))) idx[[nm]] <- NA

  idx2 <- idx[, needed, drop = FALSE]
  idx2$site_no <- pt_clean_site_no_local(idx2$site_no)
  idx2 <- idx2[!is.na(idx2$site_no) & idx2$site_no != "", , drop = FALSE]
  idx2 <- idx2[!duplicated(idx2$site_no), , drop = FALSE]
  idx2 <- idx2 |>
    dplyr::transmute(
      site_no = .data$site_no,
      on_blm_ca_from_index = pt_boolish_nullable_local(.data$on_blm_ca),
      dist_to_blm_mi_from_index = suppressWarnings(as.numeric(.data$dist_to_blm_mi)),
      dist_to_blm_ft_from_index = suppressWarnings(as.numeric(.data$dist_to_blm_ft))
    )

  if (!"on_blm_ca" %in% names(usgs_sw)) usgs_sw$on_blm_ca <- NA
  if (!"dist_to_blm_mi" %in% names(usgs_sw)) usgs_sw$dist_to_blm_mi <- NA_real_
  if (!"dist_to_blm_ft" %in% names(usgs_sw)) usgs_sw$dist_to_blm_ft <- NA_real_

  usgs_sw$site_no <- pt_clean_site_no_local(usgs_sw$site_no)
  out <- dplyr::left_join(usgs_sw, idx2, by = "site_no")

  out$on_blm_ca <- dplyr::coalesce(
    pt_boolish_nullable_local(out$on_blm_ca),
    out$on_blm_ca_from_index
  )
  out$dist_to_blm_mi <- dplyr::coalesce(
    suppressWarnings(as.numeric(out$dist_to_blm_mi)),
    out$dist_to_blm_mi_from_index
  )
  out$dist_to_blm_ft <- dplyr::coalesce(
    suppressWarnings(as.numeric(out$dist_to_blm_ft)),
    out$dist_to_blm_ft_from_index
  )

  out$on_blm_ca_from_index <- NULL
  out$dist_to_blm_mi_from_index <- NULL
  out$dist_to_blm_ft_from_index <- NULL

  message(
    "USGS streamgage BLM-distance fields joined for Local filters: ",
    sum(!is.na(out$dist_to_blm_mi)),
    " of ", nrow(out), " streamgages have distance-to-BLM values; ",
    sum(out$on_blm_ca %in% TRUE, na.rm = TRUE), " are on BLM. Source: ", index_path
  )

  out
}


pt_enrich_usgs_wells_local_with_blm_distance <- function(usgs_gw) {
  if (!inherits(usgs_gw, "sf") && !is.data.frame(usgs_gw)) return(usgs_gw)

  site_col <- if ("site_no" %in% names(usgs_gw)) {
    "site_no"
  } else if ("gwpop_site_no" %in% names(usgs_gw)) {
    "gwpop_site_no"
  } else {
    NA_character_
  }

  if (is.na(site_col)) {
    message("USGS Wells Local BLM-distance cache join skipped: site_no not found in Local GW layer.")
    if (!"on_blm_ca" %in% names(usgs_gw)) usgs_gw$on_blm_ca <- NA
    if (!"dist_to_blm_mi" %in% names(usgs_gw)) usgs_gw$dist_to_blm_mi <- NA_real_
    if (!"dist_to_blm_ft" %in% names(usgs_gw)) usgs_gw$dist_to_blm_ft <- NA_real_
    return(usgs_gw)
  }

  cache_path <- pt_first_existing_path_local(c(
    Sys.getenv("USGS_GW_LOCAL_BLM_DISTANCE_CSV", unset = ""),
    file.path(DIR$cache_last, "usgs_gw_local_blm_distance_fields.csv"),
    file.path("04_processed_data", "cache", "latest", "usgs_gw_local_blm_distance_fields.csv")
  ))

  if (is.na(cache_path)) {
    message("USGS Wells Local BLM-distance cache not found; Local GW BLM filters will be unavailable. Run 60_update_usgs_gw_local_blm_distance_fields.R after refreshing BLM lands or Local USGS Wells.")
    if (!"on_blm_ca" %in% names(usgs_gw)) usgs_gw$on_blm_ca <- NA
    if (!"dist_to_blm_mi" %in% names(usgs_gw)) usgs_gw$dist_to_blm_mi <- NA_real_
    if (!"dist_to_blm_ft" %in% names(usgs_gw)) usgs_gw$dist_to_blm_ft <- NA_real_
    return(usgs_gw)
  }

  dist <- tryCatch(
    utils::read.csv(cache_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      warning("Could not read USGS Wells Local BLM-distance cache: ", conditionMessage(e))
      NULL
    }
  )

  needed <- c("site_no", "on_blm_ca", "dist_to_blm_mi", "dist_to_blm_ft")
  if (is.null(dist) || !"site_no" %in% names(dist) || !any(c("on_blm_ca", "dist_to_blm_mi", "dist_to_blm_ft") %in% names(dist))) {
    message("USGS Wells Local BLM-distance cache lacks distance fields; Local GW BLM filters will be unavailable. Re-run 60_update_usgs_gw_local_blm_distance_fields.R.")
    if (!"on_blm_ca" %in% names(usgs_gw)) usgs_gw$on_blm_ca <- NA
    if (!"dist_to_blm_mi" %in% names(usgs_gw)) usgs_gw$dist_to_blm_mi <- NA_real_
    if (!"dist_to_blm_ft" %in% names(usgs_gw)) usgs_gw$dist_to_blm_ft <- NA_real_
    return(usgs_gw)
  }

  for (nm in setdiff(needed, names(dist))) dist[[nm]] <- NA

  dist2 <- dist[, needed, drop = FALSE]
  dist2$site_no <- pt_clean_site_no_local(dist2$site_no)
  dist2 <- dist2[!is.na(dist2$site_no) & dist2$site_no != "", , drop = FALSE]
  dist2 <- dist2[!duplicated(dist2$site_no), , drop = FALSE]
  dist2 <- dist2 |>
    dplyr::transmute(
      site_no = .data$site_no,
      on_blm_ca_from_local_cache = pt_boolish_nullable_local(.data$on_blm_ca),
      dist_to_blm_mi_from_local_cache = suppressWarnings(as.numeric(.data$dist_to_blm_mi)),
      dist_to_blm_ft_from_local_cache = suppressWarnings(as.numeric(.data$dist_to_blm_ft))
    )

  if (!"on_blm_ca" %in% names(usgs_gw)) usgs_gw$on_blm_ca <- NA
  if (!"dist_to_blm_mi" %in% names(usgs_gw)) usgs_gw$dist_to_blm_mi <- NA_real_
  if (!"dist_to_blm_ft" %in% names(usgs_gw)) usgs_gw$dist_to_blm_ft <- NA_real_

  usgs_gw$site_no_for_blm_join <- pt_clean_site_no_local(usgs_gw[[site_col]])
  dist2_for_join <- dist2
  names(dist2_for_join)[names(dist2_for_join) == "site_no"] <- "site_no_for_blm_join"

  out <- dplyr::left_join(
    usgs_gw,
    dist2_for_join,
    by = "site_no_for_blm_join"
  )

  out$on_blm_ca <- dplyr::coalesce(
    pt_boolish_nullable_local(out$on_blm_ca),
    out$on_blm_ca_from_local_cache
  )
  out$dist_to_blm_mi <- dplyr::coalesce(
    suppressWarnings(as.numeric(out$dist_to_blm_mi)),
    out$dist_to_blm_mi_from_local_cache
  )
  out$dist_to_blm_ft <- dplyr::coalesce(
    suppressWarnings(as.numeric(out$dist_to_blm_ft)),
    out$dist_to_blm_ft_from_local_cache
  )

  out$site_no_for_blm_join <- NULL
  out$on_blm_ca_from_local_cache <- NULL
  out$dist_to_blm_mi_from_local_cache <- NULL
  out$dist_to_blm_ft_from_local_cache <- NULL

  message(
    "USGS Wells Local BLM-distance cache joined: ",
    sum(!is.na(out$dist_to_blm_mi)),
    " of ", nrow(out), " Local wells have distance-to-BLM values; ",
    sum(out$on_blm_ca %in% TRUE, na.rm = TRUE), " are on BLM. Source: ", cache_path
  )

  out
}

layers$usgs_sw <- pt_enrich_usgs_streamgages_with_current_ops_data(layers$usgs_sw)
layers$usgs_sw <- pt_enrich_usgs_streamgages_with_blm_distance(layers$usgs_sw)
layers$usgs_gw <- pt_enrich_usgs_wells_local_with_blm_distance(layers$usgs_gw)


# ==== 5A. Register feature-count suffixes for selected local layers ===========
##
## PURPOSE:
##   Add compact feature counts to high-value Points/Basins layer-control names
##   so users can immediately see the scale of local datasets without opening
##   popups or reading documentation.  Counts are intentionally registered once
##   here, after cache products are loaded, and then appended by
##   pt_layer_group_name() so the visible overlay list and actual Leaflet group
##   names remain identical.
##
## FORMAT:
##   < 1,000 features  -> (304)
##   >= 1,000 features -> (~44.5k)

pt_count_sf_rows <- function(x) {
  if (inherits(x, "sf")) {
    return(nrow(x))
  }
  NA_real_
}

pt_count_data_rows <- function(x) {
  if (is.data.frame(x)) {
    return(nrow(x))
  }
  NA_real_
}

pt_has_data_rows <- function(x) {
  is.data.frame(x) && nrow(x) > 0
}

pt_count_huc_rows <- function(huc_all, nm) {
  if (is.list(huc_all) && nm %in% names(huc_all) && inherits(huc_all[[nm]], "sf")) {
    return(nrow(huc_all[[nm]]))
  }
  NA_real_
}

pt_count_swrcb_official_rows <- function(x) {
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(NA_real_)
  }

  if (!"swrcb_2026_blm_wr_list" %in% names(x)) {
    return(nrow(x))
  }

  flag <- suppressWarnings(as.logical(x$swrcb_2026_blm_wr_list))
  flag[is.na(flag)] <- FALSE
  sum(flag)
}

pt_count_swrcb_additional_spatial_rows <- function(x) {
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(NA_real_)
  }

  if (!all(c("swrcb_2026_blm_wr_list", "blm_include_reason_display") %in% names(x))) {
    return(NA_real_)
  }

  flag <- suppressWarnings(as.logical(x$swrcb_2026_blm_wr_list))
  flag[is.na(flag)] <- FALSE
  reason <- trimws(as.character(x$blm_include_reason_display))
  sum(!flag & reason %in% c("Spatial + name match", "Spatial only"), na.rm = TRUE)
}

pt_count_swrcb_name_candidate_rows <- function(x) {
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(NA_real_)
  }

  if (!all(c("swrcb_2026_blm_wr_list", "blm_include_reason_display") %in% names(x))) {
    return(NA_real_)
  }

  flag <- suppressWarnings(as.logical(x$swrcb_2026_blm_wr_list))
  flag[is.na(flag)] <- FALSE
  reason <- trimws(as.character(x$blm_include_reason_display))
  sum(!flag & reason %in% c("Name match only"), na.rm = TRUE)
}

pt_count_reference_layer_rows <- function(reference_layers, layer_name) {
  if (!is.list(reference_layers) || length(reference_layers) == 0) {
    return(NA_real_)
  }

  if (layer_name %in% names(reference_layers) && inherits(reference_layers[[layer_name]], "sf")) {
    return(nrow(reference_layers[[layer_name]]))
  }

  for (x in reference_layers) {
    if (!inherits(x, "sf") || nrow(x) == 0 || !"pt_display_name" %in% names(x)) next
    display <- as.character(x$pt_display_name[1])
    if (identical(display, layer_name)) {
      return(nrow(x))
    }
  }

  NA_real_
}

pt_count_calsim3_network_rows <- function(arcs, nodes) {
  vals <- c(pt_count_sf_rows(arcs), pt_count_sf_rows(nodes))
  if (all(is.na(vals))) return(NA_real_)
  sum(vals, na.rm = TRUE)
}

pt_count_unique_sf_field <- function(x, field) {
  if (
    !inherits(x, "sf") ||
    nrow(x) == 0 ||
    !field %in% names(x)
  ) {
    return(NA_real_)
  }

  values <- trimws(as.character(x[[field]]))
  values <- values[!is.na(values) & values != ""]

  if (length(values) == 0) {
    return(NA_real_)
  }

  length(unique(values))
}

## Counts are shown for the most useful Points/Basins controls.  The two
## add'l SWRCB candidate layers now use shortened display names, so they can
## carry count suffixes without crowding the Local layer panel.
REFERENCE_OVERLAY_GROUPS_ALL <- if (
  isTRUE(MAP_DISPLAY$add_reference_layers)
) {
  pt_reference_overlay_groups(layers$reference_layers)
} else {
  character(0)
}

WSR_CHANNEL_GROUPS <- REFERENCE_OVERLAY_GROUPS_ALL[
  grepl("^Wild & Scenic", REFERENCE_OVERLAY_GROUPS_ALL)
]

REFERENCE_OVERLAY_GROUPS_NON_WSR <- setdiff(
  REFERENCE_OVERLAY_GROUPS_ALL,
  WSR_CHANNEL_GROUPS
)

pt_register_local_layer_feature_counts(c(
  "Basins – GW Basins, Bulletin 118" = pt_count_sf_rows(layers$gw),
  ## Keep the older CNRFC basin geography layer and add product availability as
  ## a separate Local layer when the map-ready RDS exists.
  "Basins – CNRFC Basins" = pt_count_sf_rows(layers$cnrfc_basins),
  "Basins – CNRFC Product Availability" = pt_count_sf_rows(layers$cnrfc_basin_availability),
  "Basins – CNRFC FNF Sha/Tri/west Sierra Basins" = pt_count_sf_rows(layers$cnrfc_fnf_delta),
  "Basins – Groundwater Sustainability Plan Areas" = pt_count_reference_layer_rows(layers$reference_layers, "gsps"),
  "Basins – Adjudicated Groundwater Basins" = pt_count_reference_layer_rows(layers$reference_layers, "gwbasins_adjd"),
  "Basins – HUC2 – PRISM/BCMv8" = pt_count_huc_rows(layers$huc_all, "huc2"),
  "Basins – HUC4 – PRISM/BCMv8" = pt_count_huc_rows(layers$huc_all, "huc4"),
  "Basins – HUC6 – PRISM/BCMv8" = pt_count_huc_rows(layers$huc_all, "huc6"),
  "Basins – HUC8 – PRISM/BCMv8" = pt_count_huc_rows(layers$huc_all, "huc8"),
  "Basins – HUC10 – PRISM/BCMv8" = pt_count_huc_rows(layers$huc_all, "huc10"),
  "Basins – HUC12 – PRISM/BCMv8" = pt_count_huc_rows(layers$huc_all, "huc12"),
  "Points – CNRFC river/reservoir catalog" = pt_count_sf_rows(layers$cnrfc_stream),
  "Points – CNRFC Precip Gages" = pt_count_sf_rows(layers$cnrfc_precip),
  "Points – CNRFC weather station catalog" = pt_count_data_rows(layers$cnrfc_precip_weather_station_catalog_local),
  "Points – CDEC Reservoir Stations" = pt_count_sf_rows(layers$cdec_reservoir_stations),
  "Points – USGS streamgages" = pt_count_sf_rows(layers$usgs_sw),
  "Points – USGS monitoring wells" = pt_count_sf_rows(layers$usgs_gw),
  "Points – BLM-drilled wells | NOC" = pt_count_sf_rows(layers$blm_noc_drilled_wells),
  "Points – GW wells | 2025 Mojave-BLM limited field check" = pt_count_sf_rows(layers$mojave_2025_gw_well_inventory),
  "Points – Water rights POD | SWRCB 2026 BLM list" = pt_count_swrcb_official_rows(layers$swrcb_pod_wr_blm),
  "Points – Water rights POD | BRIM spatial BLM match" = pt_count_swrcb_additional_spatial_rows(layers$swrcb_pod_wr_blm),
  "Points – Water rights POD | BRIM name/text BLM candidate" = pt_count_swrcb_name_candidate_rows(layers$swrcb_pod_wr_blm),
  "Points – Springs" = pt_count_sf_rows(layers$springs),
  "Points – SCAN Stations" = pt_count_sf_rows(layers$scan_stations),
  "Points – Snow Pillows" = pt_count_sf_rows(layers$snow_pillows),
  "Channels – CalSim3.0" = pt_count_calsim3_network_rows(layers$calsim3_arcs, layers$calsim3_nodes),
  "Channels – Water conveyance | BRIM mapped" = pt_count_unique_sf_field(
    layers$brim_mapped_conveyance,
    "facility_id"
  )
))

# ==== 6. Build overlay group list ===========================================
##
## The order here controls the order in the layer-control box.

CORE_OVERLAY_GROUPS <- c(
  if (MAP_DISPLAY$add_blm_offices) "BLM Offices",
  if (MAP_DISPLAY$add_field_office_outer) "BLM Field Office (outer)",
  "BLM-CA Managed (core)",
  "BLM Held/Managed Differences",
  
  "GW – Bull. 118",
  if (MAP_DISPLAY$add_cnrfc_basins) "CNRFC Basins",
  if (isTRUE(MAP_DISPLAY$add_cnrfc_basin_product_availability) && inherits(layers$cnrfc_basin_availability, "sf")) "CNRFC Product Availability",
  if (MAP_DISPLAY$add_cnrfc_fnf_delta) "CNRFC FNF Sha/Tri/west Sierra Basins",
  REFERENCE_OVERLAY_GROUPS_NON_WSR,
  "HUC2",
  "HUC4",
  "HUC6",
  "HUC8",
  if (MAP_DISPLAY$add_huc10) "HUC10",
  if (MAP_DISPLAY$add_huc12) "HUC12",
  
  if (isTRUE(MAP_DISPLAY$add_cnrfc_precip_weather_station_catalog) && pt_has_data_rows(layers$cnrfc_precip_weather_station_catalog_local)) "CNRFC weather station catalog",
  if (MAP_DISPLAY$add_cnrfc_stream) "CNRFC river/reservoir catalog",
  if (MAP_DISPLAY$add_cnrfc_precip) "CNRFC Precip Gages",
  if (isTRUE(MAP_DISPLAY$add_cdec_reservoir_stations)) "CDEC Reservoir Stations",
  if (MAP_DISPLAY$add_usgs_streamgages) "USGS streamgages",
  if (MAP_DISPLAY$add_usgs_wells) "USGS monitoring wells",
  if (isTRUE(MAP_DISPLAY$add_blm_noc_drilled_wells) && pt_has_data_rows(layers$blm_noc_drilled_wells)) "BLM-drilled wells | NOC",
  if (isTRUE(MAP_DISPLAY$add_mojave_2025_gw_well_inventory) && pt_has_data_rows(layers$mojave_2025_gw_well_inventory)) "GW wells | 2025 Mojave-BLM limited field check",
  if (isTRUE(MAP_DISPLAY$add_swrcb_pod_wr_blm)) "Water rights POD | SWRCB 2026 BLM list",
  if (isTRUE(MAP_DISPLAY$add_swrcb_pod_wr_blm)) "Water rights POD | BRIM spatial BLM match",
  if (isTRUE(MAP_DISPLAY$add_swrcb_pod_wr_blm)) "Water rights POD | BRIM name/text BLM candidate",
  if (MAP_DISPLAY$add_x2_km) "CVP/SWP X2 km points",
  if (MAP_DISPLAY$add_springs) "Springs",
  if (MAP_DISPLAY$add_scan_stations) "SCAN Stations",
  if (MAP_DISPLAY$add_snow_pillows) "Snow Pillows",
  
  if (isTRUE(MAP_DISPLAY$add_brim_mapped_conveyance) &&
      pt_has_data_rows(layers$brim_mapped_conveyance)) {
    "Water conveyance | BRIM mapped"
  },
  WSR_CHANNEL_GROUPS,
  if (MAP_DISPLAY$add_major_conveyance) "Major Conveyance",
  if (MAP_DISPLAY$add_deltamapr_canals) "Deltamapr Conveyance",
  if (MAP_DISPLAY$add_calsim3_arcs) "CalSim3.0",
  
  "Counties",
  if (MAP_DISPLAY$add_rwqcb_regions) "RWQCB Regions",
  if (MAP_DISPLAY$add_water_districts) "Water Districts",
  if (MAP_DISPLAY$add_cgs_geology) "CA Geology (visual only)"
)

LABEL_OVERLAY_GROUPS <- if (isTRUE(MAP_DISPLAY$add_labels)) {
  c(
    pt_label_overlay_groups(layers$labels_all),
    if (isTRUE(MAP_DISPLAY$add_cnrfc_fnf_delta) && pt_has_data_rows(layers$cnrfc_fnf_delta)) {
      "Labels: CNRFC FNF Sha/Tri/west Sierra Basins"
    },
    if (isTRUE(MAP_DISPLAY$add_reference_layers) &&
        is.list(layers$reference_layers) &&
        "gsps" %in% names(layers$reference_layers) &&
        pt_has_data_rows(layers$reference_layers[["gsps"]])) {
      "Labels: Groundwater Sustainability Plan Areas"
    },
    if (isTRUE(MAP_DISPLAY$add_reference_layers) &&
        is.list(layers$reference_layers) &&
        "gwbasins_adjd" %in% names(layers$reference_layers) &&
        pt_has_data_rows(layers$reference_layers[["gwbasins_adjd"]])) {
      "Labels: Adjudicated Groundwater Basins"
    },
    if (isTRUE(MAP_DISPLAY$add_usgs_streamgages) && isTRUE(MAP_DISPLAY$add_labels)) {
      "Labels: USGS streamgages"
    },
    if (isTRUE(MAP_DISPLAY$add_blm_noc_drilled_wells) && pt_has_data_rows(layers$blm_noc_drilled_wells) && isTRUE(MAP_DISPLAY$add_labels)) {
      "Labels: BLM-drilled wells | NOC"
    },
    if (isTRUE(MAP_DISPLAY$add_mojave_2025_gw_well_inventory) && pt_has_data_rows(layers$mojave_2025_gw_well_inventory) && isTRUE(MAP_DISPLAY$add_labels)) {
      "Labels: GW wells | 2025 Mojave-BLM limited field check"
    },
    if (isTRUE(MAP_DISPLAY$add_springs) && isTRUE(MAP_DISPLAY$add_labels)) {
      "Labels: Springs"
    },
    if (isTRUE(MAP_DISPLAY$add_swrcb_pod_wr_blm) && isTRUE(MAP_DISPLAY$add_labels)) {
      "Labels: Water rights POD | SWRCB 2026 BLM list"
    },
    if (isTRUE(MAP_DISPLAY$add_swrcb_pod_wr_blm) && isTRUE(MAP_DISPLAY$add_labels)) {
      "Labels: Water rights POD | BRIM spatial BLM match"
    },
    if (isTRUE(MAP_DISPLAY$add_swrcb_pod_wr_blm) && isTRUE(MAP_DISPLAY$add_labels)) {
      "Labels: Water rights POD | BRIM name/text BLM candidate"
    },
    if (
      isTRUE(MAP_DISPLAY$add_brim_mapped_conveyance) &&
      isTRUE(MAP_DISPLAY$add_labels) &&
      pt_has_data_rows(layers$brim_mapped_conveyance_labels)
    ) {
      "Labels: Water conveyance | BRIM mapped"
    },
    if (isTRUE(MAP_DISPLAY$add_rwqcb_regions)) "Labels: RWQCB Regions"
  )
} else {
  character(0)
}

OVERLAY_GROUPS <- pt_order_overlay_groups(c(
  CORE_OVERLAY_GROUPS,
  LABEL_OVERLAY_GROUPS
))

# ==== 7. Initialize map ======================================================


m <- pt_init_map(MAP_DISPLAY)

## Add a lightweight browser-side startup splash before the main map and
## controls render. This improves perceived loading for large standalone HTML
## builds and starts coarse console timing markers.
m <- pt_add_startup_loading_overlay(m)

m <- pt_add_panes(m)
m <- pt_add_basemaps(m)
m <- pt_add_cgs_geology(m, MAP_DISPLAY)

m <- pt_startup_loading_mark(
  m = m,
  message = "Map shell, panes, and basemaps initialized…",
  step = "shell"
)

# ==== 8. Add cached layers ===================================================

if (isTRUE(MAP_DISPLAY$add_project_areas)) {
  m <- pt_add_project_area_layer(
    m = m,
    project_areas = layers$project_areas
  )
}


m <- pt_add_blm_layers(
  m = m,
  blm_core = layers$blm_core,
  blm_diffs = layers$blm_diffs
)

m <- pt_add_blm_office_layer(
  m = m,
  blm_offices = layers$blm_offices,
  map_display = MAP_DISPLAY
)

m <- pt_add_county_gw_layers(
  m = m,
  county = layers$county,
  gw = layers$gw
)


m <- pt_add_field_office_outer_layer(
  m = m,
  field_office_outer = layers$field_office_outer,
  map_display = MAP_DISPLAY
)

## Add the older CNRFC basin geography layer and, separately, the new product
## availability layer when its map-ready RDS has been created by 52_.
m <- pt_add_cnrfc_basin_layer(
  m = m,
  cnrfc_basins = layers$cnrfc_basins,
  map_display = MAP_DISPLAY
)

if (inherits(layers$cnrfc_basin_availability, "sf")) {
  message("Adding CNRFC basin product availability polygons: ", nrow(layers$cnrfc_basin_availability))
  m <- pt_add_cnrfc_basin_product_availability_layer(
    m = m,
    cnrfc_basin_product_availability = layers$cnrfc_basin_availability,
    map_display = MAP_DISPLAY
  )
} else {
  message("CNRFC basin product availability map-ready RDS not found or not sf; product layer skipped.")
}

m <- pt_add_cnrfc_fnf_delta_layer(
  m = m,
  cnrfc_fnf_delta = layers$cnrfc_fnf_delta,
  map_display = MAP_DISPLAY
)

m <- pt_add_calsim3_arc_layer(
  m = m,
  calsim3_arcs = layers$calsim3_arcs,
  map_display = MAP_DISPLAY
)

m <- pt_add_calsim3_node_layer(
  m = m,
  calsim3_nodes = layers$calsim3_nodes,
  map_display = MAP_DISPLAY
)

m <- pt_add_calsim3_network_legend(m)

m <- pt_add_brim_mapped_conveyance_layer(
  m = m,
  conveyance_segments = layers$brim_mapped_conveyance,
  conveyance_facilities = layers$brim_mapped_conveyance_facilities,
  conveyance_labels = layers$brim_mapped_conveyance_labels,
  map_display = MAP_DISPLAY
)

m <- pt_add_major_conveyance_layer(
  m = m,
  major_conveyance = layers$major_conveyance,
  map_display = MAP_DISPLAY
)

m <- pt_add_deltamapr_canals_layer(
  m = m,
  deltamapr_canals = layers$deltamapr_canals,
  map_display = MAP_DISPLAY
)

m <- pt_add_reference_layers(
  m = m,
  reference_layers = layers$reference_layers,
  map_display = MAP_DISPLAY
)

m <- pt_add_huc_layers(
  m = m,
  huc_all = layers$huc_all,
  map_display = MAP_DISPLAY
)

m <- pt_add_cnrfc_layers(
  m = m,
  cnrfc_stream = layers$cnrfc_stream,
  cnrfc_precip = layers$cnrfc_precip,
  map_display = MAP_DISPLAY
)

m <- pt_add_cnrfc_precip_weather_station_catalog_layer(
  m = m,
  cnrfc_precip_weather_station_catalog = layers$cnrfc_precip_weather_station_catalog_local,
  map_display = MAP_DISPLAY
)

m <- pt_add_cnrfc_local_catalog_legends(
  m = m,
  cnrfc_stream = layers$cnrfc_stream,
  cnrfc_precip_weather_station_catalog = layers$cnrfc_precip_weather_station_catalog_local,
  map_display = MAP_DISPLAY
)

m <- pt_add_cdec_reservoir_station_layer(
  m = m,
  cdec_reservoir_stations = layers$cdec_reservoir_stations,
  map_display = MAP_DISPLAY
)

m <- pt_add_usgs_layers(
  m = m,
  usgs_sw = layers$usgs_sw,
  usgs_gw = layers$usgs_gw,
  map_display = MAP_DISPLAY
)

m <- pt_add_usgs_streamgage_catalog_legend(
  m = m,
  usgs_sw = layers$usgs_sw,
  map_display = MAP_DISPLAY
)

m <- pt_add_usgs_well_catalog_legend(
  m = m,
  usgs_gw = layers$usgs_gw,
  map_display = MAP_DISPLAY
)

m <- pt_add_blm_gw_well_inventory_layers(
  m = m,
  noc_wells = layers$blm_noc_drilled_wells,
  mojave_wells = layers$mojave_2025_gw_well_inventory,
  map_display = MAP_DISPLAY
)

m <- pt_add_swrcb_pod_wr_layer(
  m = m,
  swrcb_pod_wr_blm = layers$swrcb_pod_wr_blm,
  map_display = MAP_DISPLAY
)

m <- pt_add_swrcb_pod_wr_shared_legend(
  m = m,
  swrcb_pod_wr_blm = layers$swrcb_pod_wr_blm,
  map_display = MAP_DISPLAY
)

m <- pt_add_x2_km_layer(
  m = m,
  x2_km = layers$x2_km,
  map_display = MAP_DISPLAY
)

m <- pt_add_springs_layer(
  m = m,
  springs = layers$springs,
  map_display = MAP_DISPLAY
)

m <- pt_add_snow_soil_station_layers(
  m = m,
  scan_stations = layers$scan_stations,
  snow_pillows = layers$snow_pillows,
  map_display = MAP_DISPLAY
)

m <- pt_add_rwqcb_regions_layer(
  m = m,
  rwqcb_regions = layers$rwqcb_regions,
  map_display = MAP_DISPLAY
)

m <- pt_add_water_districts_layer(
  m = m,
  water_districts = layers$water_districts,
  map_display = MAP_DISPLAY
)

m <- pt_startup_loading_mark(
  m = m,
  message = "Cached PT2 map layers added…",
  step = "layers"
)

# ==== 9a. Add controls ========================================================

m <- pt_add_layer_control(
  m = m,
  overlay_groups = OVERLAY_GROUPS,
  map_display = MAP_DISPLAY
)

## Scale, mouse-coordinate, and zoom readout are kept together in the
## lower-right corner. The scale bar is added explicitly here; the CSS in
## pt_add_mouse_coordinates() shifts both the scale bar and coordinate readout
## left enough to avoid the fixed Ops Live panel.

m <- m |>
  leaflet::addScaleBar(
    position = "bottomright",
    options = leaflet::scaleBarOptions(
      metric = TRUE,
      imperial = TRUE,
      maxWidth = 180
    )
  )

m <- pt_add_mouse_coordinates(
  m = m,
  digits = 5
)

m <- pt_add_marquee_zoom_control(m)

m <- pt_add_ops_live_layers(
  m = m,
  map_display = MAP_DISPLAY,
  cnrfc_river_reservoir_forecast_points = layers$cnrfc_active_river_reservoir_forecast_points,
  cnrfc_precip_weather_stations = layers$cnrfc_precip_weather_stations
)

m <- pt_add_tools_adddata_panel(
  m = m,
  map_display = MAP_DISPLAY
)

## Compact conveyance legend is browser-positioned between the External Layers
## and Local GIS Upload controls and appears only while the layer is visible.
m <- pt_add_brim_mapped_conveyance_legend(
  m = m,
  map_display = MAP_DISPLAY
)

## Local GIS file uploads are kept separate from the external URL overlay
## framework. Uploaded layers are temporary browser-session overlays only.
m <- pt_add_local_upload_panel(
  m = m,
  map_display = MAP_DISPLAY
)

m <- pt_startup_loading_mark(
  m = m,
  message = "Layer controls and interactive panels initialized…",
  step = "panels"
)


# ==== 9a.1 Add full-page map-notes control ==================================
##
## PURPOSE:
##   Add one expandable map-notes panel for numbered layer notes.
##
## WHY:
##   The old "*" and "**" alert boxes were too cramped and do not scale well as
##   more layer-specific notes are added. This full-page overlay is easier to
##   read and can grow over time.

m <- htmlwidgets::onRender(
  m,
  "
function(el, x) {

  var map = this;

  function openPtNotes() {

    var old = document.getElementById('pt-map-notes-overlay');
    if (old) old.remove();

    var overlay = document.createElement('div');
    overlay.id = 'pt-map-notes-overlay';
    overlay.style.position = 'fixed';
    overlay.style.top = '0';
    overlay.style.left = '0';
    overlay.style.width = '100vw';
    overlay.style.height = '100vh';
    overlay.style.background = 'rgba(0,0,0,0.62)';
    overlay.style.zIndex = '999999';
    overlay.style.display = 'flex';
    overlay.style.alignItems = 'stretch';
    overlay.style.justifyContent = 'center';
    overlay.style.boxSizing = 'border-box';
    overlay.style.padding = '34px';

    var panel = document.createElement('div');
    panel.style.background = '#ffffff';
    panel.style.color = '#222';
    panel.style.width = '100%';
    panel.style.height = '100%';
    panel.style.maxWidth = '1180px';
    panel.style.borderRadius = '10px';
    panel.style.boxShadow = '0 8px 30px rgba(0,0,0,0.35)';
    panel.style.overflow = 'auto';
    panel.style.boxSizing = 'border-box';
    panel.style.padding = '28px 34px';
    panel.style.fontFamily = 'Arial, sans-serif';
    panel.style.fontSize = '14px';
    panel.style.lineHeight = '1.45';

    var close = document.createElement('button');
    close.innerHTML = 'Close';
    close.style.position = 'sticky';
    close.style.top = '0';
    close.style.float = 'right';
    close.style.zIndex = '2';
    close.style.padding = '8px 14px';
    close.style.border = '1px solid #777';
    close.style.borderRadius = '6px';
    close.style.background = '#f7f7f7';
    close.style.cursor = 'pointer';
    close.onclick = function() {
      overlay.remove();
    };

    var content = document.createElement('div');
    content.innerHTML =
      '<h1 style=\"margin-top:0;\">PortaTreasure2 map notes</h1>' +

      '<p style=\"font-size:13px;color:#555;max-width:900px;\">' +
      'These notes provide screening-level interpretation of map layers, data sources, and symbology.' +
      '</p>' +

      '<hr/>' +

      '<h2>SWRCB / CalWATRS BLM-relevant POD/WR screening layers</h2>' +
      '<p>' +
      'The SWRCB / CalWATRS point layers are intended as screening-level tools for identifying points of diversion and water-right records that may be relevant to BLM. ' +
      'The layers are split by provenance so users can distinguish official SWRCB-provided BLM water-right records from additional BRIM screening candidates.' +
      '</p>' +
      '<p>' +
      '<b>Water rights POD | SWRCB 2026 BLM list:</b> records whose water-right/claim ID appears in the SWRCB-provided 2026 BLM water-right list. For matching rights, BRIM uses the face value supplied in that SWRCB export because the public POD/WR service can contain incomplete zero values; POD geometry and other mapped details still come from the public SWRCB/CalWATRS data. This is the best layer to use when asking, \"What did SWRCB tell BLM is ours or relevant to BLM reporting/review?\"' +
      '</p>' +
      '<p>' +
      '<b>Water rights POD | BRIM spatial BLM match:</b> additional CalWATRS POD points whose mapped coordinate falls on current BLM-managed land, but whose water-right/claim ID does not appear in the SWRCB-provided 2026 BLM list.' +
      '</p>' +
      '<p>' +
      '<b>Water rights POD | BRIM name/text BLM candidate:</b> additional records inferred from owner/holder/name text matching in the SWRCB/CalWATRS data. These are useful QA/context candidates but should be treated as analyst-derived screening records. This layer is hidden by default.' +
      '</p>' +
      '<p>' +
      '<b>Point size:</b> circle radius is scaled by face value in acre-feet per year (AFY). The scaling is intentionally capped so large rights do not dominate the map.' +
      '</p>' +
      '<p>' +
      '<b>Point fill color:</b> blue = SWRCB 2026 BLM WR list record; gold/tan = additional spatial POD match; purple = additional BLM name/text-match candidate.' +
      '</p>' +
      '<p>' +
      '<b>Marker ring color:</b> bright green = active/recognized status; orange = pending; red = inactive/cancelled/revoked/rejected; gray = unknown or unmatched status.' +
      '</p>' +
      '<p>' +
      '<b>Location and interpretation note:</b> POD coordinates are screening-level GIS locations and may not plot exactly on the physical diversion, stream reach, parcel, or land-status boundary. ' +
      'A SWRCB-provided BLM water-right/list match does not necessarily mean every mapped POD coordinate for that water right is spatially on BLM-managed land. ' +
      'Popup fields report the BLM relevance basis, spatial BLM match, POD feature ID, and POD/WR-list ID to support QA.' +
      '</p>' +

      '<h2>HUC climate and recharge summaries</h2>' +
      '<p>' +
      'HUC popups and optional thematic fills use existing summarized PRISM precipitation and BCMv8 recharge values joined to the HUC layers. ' +
      'The HUC geometries are not duplicated for the thematic fill control; the map restyles existing HUC polygons using precomputed color fields.' +
      '</p>' +
      '<p>' +
      '<b>Precipitation:</b> PRISM Group, Oregon State University, PRISM 1991–2020 precipitation normal vM5, accessed 24 April 2026. https://prism.oregonstate.edu' +
      '</p>' +
      '<p>' +
      '<b>Recharge:</b> Flint, L.E., Flint, A.L., Stern, M.A., and Seymour, W.A., 2021, The Basin Characterization Model—A monthly regional water balance software package (BCMv8) data release and model archive for hydrologic California (version 5.0, June 2025): U.S. Geological Survey data release. https://doi.org/10.5066/P9PT36UI' +
      '</p>' +
      '<p>' +
      '<b>Interpretation:</b> inches are area-normalized depth values. KAF/year values are total volume estimates and are strongly influenced by polygon area. ' +
      'HUC thematic colors are scaled separately by HUC level so HUC12 patterns remain readable; colors should not be treated as directly comparable across HUC levels without checking the active legend.' +
      '</p>' +

      '<h2>BLM-focused reference / conservation layers</h2>' +
      '<p>' +
      'These reference layers generally represent BLM GIS-based coverage, BLM-focused subsets, or agency-specific source datasets. ' +
      'They are useful for screening and context but should not be assumed to be complete statewide inventories unless the source documentation says so.' +
      '</p>' +
      '<p>' +
      'This note currently applies to layers such as Wild & Scenic Rivers, Federal Wilderness, Wilderness Study Areas, National Monuments, and National Scenic/Historic Trails.' +
      '</p>' +

      '<h2>Future notes</h2>' +
      '<p>' +
      'Additional layer notes can be added here as more PT2 layers are finalized, including springs, wells, gages, live-data services, and other water-resource layers.' +
      '</p>';

    panel.appendChild(close);
    panel.appendChild(content);
    overlay.appendChild(panel);
    document.body.appendChild(overlay);

    overlay.addEventListener('click', function(e) {
      if (e.target === overlay) {
        overlay.remove();
      }
    });

    document.addEventListener('keydown', function escClose(e) {
      if (e.key === 'Escape') {
        var ov = document.getElementById('pt-map-notes-overlay');
        if (ov) ov.remove();
        document.removeEventListener('keydown', escClose);
      }
    });
  }

  var notesControl = L.control({position: 'topleft'});

  notesControl.onAdd = function(map) {

    var div = L.DomUtil.create('div', 'leaflet-bar pt-map-notes-btn');
    div.id = 'pt-map-notes-btn';
    div.style.position = 'absolute';
    div.style.top = '84px';
    div.style.left = '8px';
    div.style.background = 'rgba(239, 239, 236, 0.97)';
    div.style.cursor = 'pointer';
    div.style.width = '82px';
    div.style.height = '22px';
    div.style.lineHeight = '22px';
    div.style.textAlign = 'center';
    div.style.fontWeight = '700';
    div.style.fontSize = '12px';
    div.style.border = '1px solid rgba(108, 108, 98, 0.76)';
    div.style.borderRadius = '5px';
    div.style.boxShadow = '0 1px 4px rgba(0,0,0,0.26)';
    div.style.color = '#222';
    div.style.boxSizing = 'border-box';
    div.style.margin = '0';
    div.style.padding = '0';
    div.style.zIndex = '10060';
    div.title = 'Map notes';

    div.innerHTML = 'notes';

    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);

    div.onclick = openPtNotes;

    return div;
  };

  notesControl.addTo(map);
}
"
)

m <- m |>
  htmlwidgets::onRender(sprintf(
    "
function(el, x) {
  var map = this;
  map.eachLayer(function(layer) {
    if (layer.options && layer.options.group === '%s') {
      map.addLayer(layer);
    }
  });
}
    ",
MAP_DISPLAY$default_base_group
  ))

# ==== 9a.3 Add HUC thematic-fill dropdown ===================================
##
## HUC thematic colors are scaled separately by HUC level, and the browser-side
## control/legend code is maintained in 03_functions/leaflet_huc_theme_helpers.r
## plus 03_functions/js/brim_huc_theme_control.js.

m <- pt_add_huc_theme_controls(
  m = m,
  huc_all = layers$huc_all
)

m <- pt_startup_loading_mark(
  m = m,
  message = "HUC thematic controls initialized…",
  step = "panels"
)

# ==== 9b. Add labels =========================================================
##
## Labels are added as clustered label-only markers. For now, label layers are
## included as normal overlay groups in the layer control. This is less elegant
## than a single global toggle, but much more browser-stable.

if (isTRUE(MAP_DISPLAY$add_labels)) {
  
  m <- pt_add_label_layers(
    m = m,
    labels_all = layers$labels_all
  )
  
  m <- pt_add_label_css(
    m = m,
    labels_config = LABELS
  )
  
} else {
  
  message("Labels are disabled in MAP_DISPLAY$add_labels.")
}

m <- pt_startup_loading_mark(
  m = m,
  message = "Labels and final map styling initialized…",
  step = "labels"
)

m <- pt_finish_startup_loading(
  m = m,
  message = "PortaTreasure2 is ready.",
  delay_ms = 850
)

# ==== 10. Save HTML ==========================================================
##
## PURPOSE:
##   Save the final map HTML.
##
## WHY THE EXTRA LOGIC?
##   On Windows/OneDrive, htmlwidgets/Pandoc can sometimes leave behind:
##
##     - a *_files support folder
##     - a strange 8.3 short-name file like PORTA~1.HTM
##
##   To avoid clutter, the self-contained HTML is built in a temporary local
##   folder first, then only the finished HTML file is copied to the final
##   output folder.

if (isTRUE(MAP_DISPLAY$self_contained_html)) {
  
  message("Saving self-contained HTML through temporary folder...")
  
  temp_html_dir <- file.path(tempdir(), paste0("pt2_html_", RUN_TS))
  dir.create(temp_html_dir, recursive = TRUE, showWarnings = FALSE)
  
  temp_html <- file.path(
    temp_html_dir,
    basename(OUT_HTML)
  )
  
  htmlwidgets::saveWidget(
    widget = m,
    file = temp_html,
    selfcontained = TRUE,
    title = "BRIM | BLM-California"
  )
  
  file.copy(
    from = temp_html,
    to = OUT_HTML,
    overwrite = TRUE
  )
  
  message("Copied self-contained HTML to final output path.")
  
} else {
  
  htmlwidgets::saveWidget(
    widget = m,
    file = OUT_HTML,
    selfcontained = FALSE,
    title = "BRIM | BLM-California"
  )
}

message("\nSaved PortaTreasure2 core map:")
message("  ", OUT_HTML)

message("\nOutput file size:")
print(file.info(OUT_HTML)$size)

message("\nDone.")
