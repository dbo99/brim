# ==== 03_cache_stream_usgs_points.r ==========================================
##
## PURPOSE:
##   Build CNRFC stream/precip point caches and USGS stream/well caches.
##
## NOTE:
##   This file is sourced by 05_map_build/02_build_core_map_cache.r.
##   It expects objects created earlier in that script and creates map-ready
##   cache objects in the calling environment. Do not source this file alone
##   unless you have already created the required input objects.
## ============================================================================

# ---- 8.1A Popup-size guard ---------------------------------------------------
##
## PURPOSE:
##   Prevent the standalone HTML from ballooning if a popup helper accidentally
##   returns very large strings.  This is intentionally a cache-build guard:
##   if a point-layer popup is unexpectedly large, stop before writing a large
##   cache or embedding it in the final Leaflet HTML.

pt_stop_if_large_popup <- function(popup_html, layer_label, max_bytes = 5000) {
  popup_html <- as.character(popup_html)
  popup_bytes <- nchar(popup_html, type = "bytes", allowNA = TRUE)
  popup_bytes[is.na(popup_bytes)] <- 0L

  max_popup_bytes <- max(popup_bytes, na.rm = TRUE)

  if (is.finite(max_popup_bytes) && max_popup_bytes > max_bytes) {
    stop(
      layer_label,
      " popup_html exceeded the safety limit. Max popup bytes = ",
      max_popup_bytes,
      "; limit = ",
      max_bytes,
      ". This usually indicates a vectorization or join leak in popup construction."
    )
  }

  invisible(max_popup_bytes)
}

# ---- 8.2 CNRFC stream/reservoir/special gages -------------------------------

cnrfc_stream_map <- cnrfc_stream |>
  clean_sf_for_leaflet() |>
  dplyr::mutate(
    is_cnrfc_reservoir = tolower(trimws(as.character(.data$gage_class1))) == "reservoir",
    cnrfc_point_role_display = pt_cnrfc_role_label(
      gage_class1 = .data$gage_class1,
      gage_class2 = .data$gage_class2,
      gage_type = .data$gage_type
    )
  )

# ---- 8.2A Optional CDEC-CNRFC station crosswalk enrichment -------------------
##
## The crosswalk is produced by 02_preprocess/28_reservoir_station_index.r.
## It is optional and is used only to add CDEC portal links/IDs to CNRFC
## river/reservoir popups. It must never control which CNRFC points are drawn.
##
## Preference:
##   1. General station crosswalk, which includes rivers and reservoirs.
##   2. Older reservoir-only crosswalk as a fallback for compatibility.

pt_read_cdec_cnrfc_crosswalk <- function() {
  xwalk_path <- file.path(DIR$rds, "cdec_cnrfc_station_crosswalk.rds")
  fallback_path <- file.path(DIR$rds, "cdec_cnrfc_reservoir_crosswalk.rds")

  if (!file.exists(xwalk_path) && file.exists(fallback_path)) {
    message("General CDEC-CNRFC station crosswalk not found; using reservoir-only fallback: ", fallback_path)
    xwalk_path <- fallback_path
  }

  if (!file.exists(xwalk_path)) {
    message("CDEC-CNRFC station crosswalk not found; CNRFC popups will omit CDEC links: ", xwalk_path)
    return(NULL)
  }

  x <- readRDS(xwalk_path)

  if (!is.data.frame(x) || nrow(x) == 0) {
    message("CDEC-CNRFC station crosswalk is empty; CNRFC popups will omit CDEC links.")
    return(NULL)
  }

  needed <- c(
    "nws_id",
    "cdec_id",
    "cdec_station_name",
    "cdec_station_url",
    "cdec_sensor15_hourly_url",
    "cdec_sensor15_daily_url",
    "match_method",
    "match_confidence",
    "match_distance_m",
    "cnrfc_point_role"
  )

  for (nm in needed) {
    if (!nm %in% names(x)) x[[nm]] <- NA
  }

  x |>
    dplyr::transmute(
      nwsid = toupper(trimws(as.character(.data$nws_id))),
      cdec_id = toupper(trimws(as.character(.data$cdec_id))),
      cdec_station_name = as.character(.data$cdec_station_name),
      cdec_station_url = as.character(.data$cdec_station_url),
      cdec_sensor15_hourly_url = as.character(.data$cdec_sensor15_hourly_url),
      cdec_sensor15_daily_url = as.character(.data$cdec_sensor15_daily_url),
      cdec_cnrfc_match_method = as.character(.data$match_method),
      cdec_cnrfc_match_confidence = as.character(.data$match_confidence),
      cdec_cnrfc_match_distance_m = suppressWarnings(as.numeric(.data$match_distance_m)),
      cdec_cnrfc_point_role = as.character(.data$cnrfc_point_role)
    ) |>
    dplyr::filter(!is.na(.data$nwsid), .data$nwsid != "") |>
    dplyr::arrange(.data$nwsid, .data$cdec_cnrfc_match_confidence, .data$cdec_cnrfc_match_distance_m) |>
    dplyr::distinct(.data$nwsid, .keep_all = TRUE)
}

cdec_cnrfc_xwalk <- pt_read_cdec_cnrfc_crosswalk()

if (!is.null(cdec_cnrfc_xwalk)) {
  cnrfc_stream_map <- cnrfc_stream_map |>
    dplyr::left_join(cdec_cnrfc_xwalk, by = "nwsid")

  message(
    "CNRFC river/reservoir points enriched with CDEC links: ",
    sum(!is.na(cnrfc_stream_map$cdec_id) & cnrfc_stream_map$cdec_id != ""),
    " of ",
    nrow(cnrfc_stream_map),
    " CNRFC point(s); reservoirs with CDEC links: ",
    sum(
      cnrfc_stream_map$is_cnrfc_reservoir &
        !is.na(cnrfc_stream_map$cdec_id) &
        cnrfc_stream_map$cdec_id != "",
      na.rm = TRUE
    ),
    "."
  )
}

cnrfc_stream_map$popup_html <- pt_make_cnrfc_stream_popups(cnrfc_stream_map)
pt_stop_if_large_popup(cnrfc_stream_map$popup_html, "CNRFC river/reservoir", max_bytes = 5000)

## Symbol colors by CNRFC stream-gage class:
##   river     = light blue
##   reservoir = dark blue
##   special   = gray
cnrfc_stream_map$fill_col <- dplyr::case_when(
  tolower(as.character(cnrfc_stream_map$gage_class1)) == "river"     ~ "#6BAED6",
  tolower(as.character(cnrfc_stream_map$gage_class1)) == "reservoir" ~ "#08306B",
  TRUE                                                              ~ "#8C8C8C"
)

cnrfc_stream_map$stroke_col <- dplyr::case_when(
  tolower(as.character(cnrfc_stream_map$gage_class1)) == "river"     ~ "#08519C",
  tolower(as.character(cnrfc_stream_map$gage_class1)) == "reservoir" ~ "#041B3D",
  TRUE                                                              ~ "#4D4D4D"
)

# ---- 8.3 CNRFC precipitation gages ------------------------------------------
##
## Color precipitation gages by datatransmission.
##
## Known values from the current layer include:
##   G, M, P, R, W, Z

cnrfc_precip_map <- cnrfc_precip |>
  clean_sf_for_leaflet()

cnrfc_precip_map$popup_html <- pt_make_cnrfc_precip_popups(cnrfc_precip_map)
pt_stop_if_large_popup(cnrfc_precip_map$popup_html, "CNRFC precip", max_bytes = 5000)

cnrfc_precip_map$fill_col <- dplyr::case_when(
  cnrfc_precip_map$datatransmission == "G" ~ "#1F78B4",  # blue
  cnrfc_precip_map$datatransmission == "R" ~ "#33A02C",  # green
  cnrfc_precip_map$datatransmission == "P" ~ "#6A3D9A",  # purple
  cnrfc_precip_map$datatransmission == "M" ~ "#E31A1C",  # red
  cnrfc_precip_map$datatransmission == "W" ~ "#444444",  # dark gray
  cnrfc_precip_map$datatransmission == "Z" ~ "#FF7F00",  # orange
  TRUE                                       ~ "#000000"
)

cnrfc_precip_map$stroke_col <- dplyr::case_when(
  cnrfc_precip_map$datatransmission == "G" ~ "#0B3C5D",
  cnrfc_precip_map$datatransmission == "R" ~ "#1B7837",
  cnrfc_precip_map$datatransmission == "P" ~ "#3F007D",
  cnrfc_precip_map$datatransmission == "M" ~ "#99000D",
  cnrfc_precip_map$datatransmission == "W" ~ "#222222",
  cnrfc_precip_map$datatransmission == "Z" ~ "#B35806",
  TRUE                                       ~ "#000000"
)




# ---- 8.3A CDEC reservoir station index -------------------------------------
##
## PURPOSE:
##   Build a lightweight local CDEC reservoir-station layer from the current
##   CDEC-first station index created by:
##
##     02_preprocess/28_reservoir_station_index.r
##
## DESIGN:
##   This is station/metadata infrastructure only.  It does not fetch or display
##   current storage values.  Later, a hosted/static latest-storage GeoJSON can
##   join to this station backbone by cdec_id.

if (exists("cdec_reservoir_stations") && inherits(cdec_reservoir_stations, "sf")) {

  cdec_reservoir_stations_map <- cdec_reservoir_stations |>
    clean_sf_for_leaflet() |>
    dplyr::mutate(
      cdec_has_hourly = dplyr::coalesce(as.logical(.data$has_hourly_reservoir_report), FALSE),
      cdec_has_daily = dplyr::coalesce(as.logical(.data$has_daily_reservoir_report), FALSE),
      cdec_report_display = dplyr::case_when(
        .data$cdec_has_hourly & .data$cdec_has_daily ~ "Hourly + daily",
        .data$cdec_has_hourly ~ "Hourly",
        .data$cdec_has_daily ~ "Daily",
        TRUE ~ "Reservoir station"
      ),
      cdec_fill_col = dplyr::case_when(
        .data$cdec_has_hourly ~ "#08306B",
        .data$cdec_has_daily ~ "#6BAED6",
        TRUE ~ "#BDBDBD"
      ),
      cdec_stroke_col = dplyr::case_when(
        .data$cdec_has_hourly ~ "#041B3D",
        .data$cdec_has_daily ~ "#08519C",
        TRUE ~ "#636363"
      ),
      cdec_radius = dplyr::case_when(
        .data$cdec_has_hourly ~ 5.2,
        .data$cdec_has_daily ~ 4.4,
        TRUE ~ 4.0
      ),
      hover_text = paste0(
        .data$cdec_id,
        " - ",
        dplyr::coalesce(as.character(.data$reservoir_name), as.character(.data$cdec_station_name), "CDEC reservoir station"),
        "\nCDEC report: ", .data$cdec_report_display,
        "\nOperator: ", dplyr::coalesce(as.character(.data$operator_agency), "Not available")
      )
    )

  # ---- 8.3A.1 Optional CDEC -> CNRFC link enrichment -----------------------
  ##
  ## PURPOSE:
  ##   The CDEC reservoir station index is CDEC-first and should remain the
  ##   authoritative station/metadata backbone.  However, when the broader
  ##   CDEC-CNRFC station crosswalk exists, use it as portal-link enrichment so
  ##   CDEC reservoir-station popups can point back to the corresponding CNRFC
  ##   pages.  This fixes major stations such as SHA -> SHDC1 and FOL -> FOLC1,
  ##   where the CDEC station index itself may not carry a direct legacy NWS ID.
  ##
  ## IMPORTANT:
  ##   This join must not duplicate CDEC station rows.  If more than one CNRFC
  ##   candidate exists for a CDEC ID, keep the best-ranked match for link
  ##   enrichment and leave the full crosswalk available in the QA outputs.

  if (exists("cdec_cnrfc_xwalk") && !is.null(cdec_cnrfc_xwalk) && nrow(cdec_cnrfc_xwalk) > 0) {

    cdec_cnrfc_by_cdec <- cdec_cnrfc_xwalk |>
      dplyr::mutate(
        cdec_id = toupper(trimws(as.character(.data$cdec_id))),
        cnrfc_nws_id = toupper(trimws(as.character(.data$nwsid))),
        cdec_cnrfc_match_rank = dplyr::case_when(
          tolower(as.character(.data$cdec_cnrfc_match_confidence)) == "high" ~ 1L,
          tolower(as.character(.data$cdec_cnrfc_match_confidence)) == "medium" ~ 2L,
          TRUE ~ 9L
        )
      ) |>
      dplyr::filter(!is.na(.data$cdec_id), .data$cdec_id != "") |>
      dplyr::arrange(
        .data$cdec_id,
        .data$cdec_cnrfc_match_rank,
        .data$cdec_cnrfc_match_distance_m
      ) |>
      dplyr::group_by(.data$cdec_id) |>
      dplyr::summarise(
        cnrfc_nws_id = dplyr::first(.data$cnrfc_nws_id),
        cnrfc_link_match_confidence = dplyr::first(.data$cdec_cnrfc_match_confidence),
        cnrfc_link_match_method = dplyr::first(.data$cdec_cnrfc_match_method),
        cnrfc_link_match_distance_m = dplyr::first(.data$cdec_cnrfc_match_distance_m),
        .groups = "drop"
      )

    cdec_reservoir_stations_map <- cdec_reservoir_stations_map |>
      dplyr::mutate(
        cdec_id = toupper(trimws(as.character(.data$cdec_id)))
      ) |>
      dplyr::left_join(
        cdec_cnrfc_by_cdec,
        by = "cdec_id"
      ) |>
      dplyr::mutate(
        ## Preserve any direct legacy NWS alias already present in the CDEC
        ## index, but fill missing aliases from the broader station crosswalk.
        nws_id = dplyr::coalesce(
          as.character(.data$nws_id),
          as.character(.data$cnrfc_nws_id)
        )
      )

    message(
      "CDEC reservoir stations enriched with CNRFC links: ",
      sum(!is.na(cdec_reservoir_stations_map$nws_id) & cdec_reservoir_stations_map$nws_id != ""),
      " of ",
      nrow(cdec_reservoir_stations_map),
      " CDEC station(s)."
    )
  }

  cdec_reservoir_stations_map$popup_html <- pt_make_cdec_reservoir_station_popups(
    cdec_reservoir_stations_map
  )

  pt_stop_if_large_popup(
    cdec_reservoir_stations_map$popup_html,
    "CDEC reservoir stations",
    max_bytes = 5000
  )

} else {

  warning(
    "CDEC reservoir station index object not available. ",
    "Creating an empty CDEC reservoir-station map cache. ",
    "Run source('02_preprocess/28_reservoir_station_index.r') to build it."
  )

  cdec_reservoir_stations_map <- sf::st_sf(
    cdec_id = character(),
    reservoir_name = character(),
    cdec_station_name = character(),
    cdec_report_display = character(),
    hover_text = character(),
    popup_html = character(),
    cdec_fill_col = character(),
    cdec_stroke_col = character(),
    cdec_radius = numeric(),
    geometry = sf::st_sfc(crs = 4326)
  )
}

message("CDEC reservoir station map cache rows: ", nrow(cdec_reservoir_stations_map))

# ---- 8.4 USGS points --------------------------------------------------------
##
## Streamgage colors:
##   active   = green
##   inactive = red
##   other/NA = gray
##
## Wells are left with their existing/default marker styling unless changed
## later.

usgs_sw_map <- usgs_sw |>
  clean_sf_for_leaflet()

# ---- 8.4A1 Static USGS streamgage browser-template popup fields -------------
##
## PURPOSE:
##   Keep the static USGS streamgage layer lean by avoiding one full repeated
##   popup_html string per gage.  The cache stores compact usgsswpop_* values;
##   leaflet_layer_helpers.r attaches the shared browser-side popup template.
##
## FALLBACK:
##   If an older cache without usgsswpop_* fields is used, the final map builder
##   still falls back to popup_html.

pt_add_usgs_stream_popup_template_fields <- function(x) {

  n <- nrow(x)

  common_params <- if ("param_list" %in% names(x)) {
    pt_summarize_usgs_params(x$param_list)
  } else {
    rep("see USGS site", n)
  }

  pt_stream_col <- function(df, nm, default = NA_character_) {
    if (nm %in% names(df)) return(df[[nm]])
    rep(default, nrow(df))
  }

  site_no <- as.character(pt_stream_col(x, "site_no", NA_character_))
  site_name <- as.character(pt_stream_col(x, "name", NA_character_))
  status <- as.character(pt_stream_col(x, "status", NA_character_))

  x$usgsswpop_site_no     <- site_no
  x$usgsswpop_name        <- site_name
  x$usgsswpop_elev_ft     <- as.character(pt_stream_col(x, "elev_ft", NA_character_))
  x$usgsswpop_site_type   <- as.character(pt_stream_col(x, "site_type", NA_character_))
  x$usgsswpop_status      <- status
  x$usgsswpop_count_nu    <- as.character(pt_stream_col(x, "count_nu", NA_character_))
  x$usgsswpop_start_date  <- as.character(pt_stream_col(x, "start_date", NA_character_))
  x$usgsswpop_end_date    <- as.character(pt_stream_col(x, "end_date", NA_character_))
  x$usgsswpop_common_data <- common_params

  x$hover_text <- paste0(
    "USGS ", dplyr::coalesce(site_no, "streamgage"),
    "\n", dplyr::coalesce(site_name, "Name not available"),
    "\nStatus: ", dplyr::coalesce(status, "unknown")
  )

  ## Do not carry prebuilt popup_html for rebuilt caches.
  if ("popup_html" %in% names(x)) x$popup_html <- NULL

  x
}

usgs_sw_map <- pt_add_usgs_stream_popup_template_fields(usgs_sw_map)

usgs_sw_map$fill_col <- dplyr::case_when(
  tolower(as.character(usgs_sw_map$status)) == "active"   ~ "#33A02C",
  tolower(as.character(usgs_sw_map$status)) == "inactive" ~ "#E31A1C",
  TRUE                                                    ~ "#8C8C8C"
)

usgs_sw_map$stroke_col <- dplyr::case_when(
  tolower(as.character(usgs_sw_map$status)) == "active"   ~ "#1B7837",
  tolower(as.character(usgs_sw_map$status)) == "inactive" ~ "#99000D",
  TRUE                                                    ~ "#4D4D4D"
)

usgs_gw_map <- usgs_gw |>
  clean_sf_for_leaflet()

# ---- 8.4A Join latest cached USGS groundwater levels ------------------------
##
## PURPOSE:
##   Join optional latest groundwater-level lookup table into the map-ready well
##   layer. This does not modify USGS_GW_final.rds.
##
## FILE PREFERENCE:
##   Full table if it exists:
##     usgs_gw_latest_water_levels.rds
##
##   Otherwise pilot table if it exists:
##     usgs_gw_latest_water_levels_pilot.rds

pt_read_latest_gw_wl <- function() {
  
  full_path <- file.path(DIR$rds, "usgs_gw_latest_water_levels.rds")
  pilot_path <- file.path(DIR$rds, "usgs_gw_latest_water_levels_pilot.rds")
  
  if (file.exists(full_path)) {
    message("Joining latest groundwater levels from: ", full_path)
    return(readRDS(full_path))
  }
  
  if (file.exists(pilot_path)) {
    message("Joining PILOT latest groundwater levels from: ", pilot_path)
    return(readRDS(pilot_path))
  }
  
  message("No latest groundwater-level lookup table found; well WL join skipped.")
  NULL
}

# ---- 8.4B Static-well Ops Live and nested-coordinate flags ------------------
##
## PURPOSE:
##   Add lightweight static-layer awareness of:
##     1. whether a static USGS well is also present in the active/recent Ops
##        Live groundwater candidate index, and
##     2. whether multiple USGS well records share the same mapped coordinate.
##
## DESIGN:
##   These are static-cache display fields only. They do not append/delete wells
##   from USGS_GW_final.rds.  The full nested-well cards and history plots remain
##   in Ops Live, while the static layer gets concise flags for hover/popup and
##   a lightweight nested indicator.

pt_cache_site_no <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("\\.0$", "", x)
  x <- gsub("[^0-9]", "", x)
  x[x == ""] <- NA_character_
  x
}

pt_read_live_gw_index_for_static <- function() {
  live_path <- file.path(
    "brim-live-data-feeds",
    "data",
    "input",
    "usgs_groundwater_latest_index_ca.csv"
  )

  if (!file.exists(live_path)) {
    message("Ops Live groundwater candidate CSV not found; static wells will not get recent-feed flags/latest feed values: ", live_path)
    return(tibble::tibble(site_no = character(), well_in_ops_live = logical()))
  }

  message("Joining Ops Live groundwater recent-feed/latest fields from: ", live_path)

  live <- readr::read_csv(
    live_path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )

  ## RF043b:
  ##   The static USGS Wells layer is the broad reference backbone.  It should
  ##   not rely on the older static/source active/inactive status for its green
  ##   ring because recent USGS field measurements can exist for sites whose
  ##   source status is still "inactive".  Instead, the green ring now means the
  ##   well is included in the Ops Live groundwater recent-feed candidate list.
  ##
  ##   Pull the latest Ops Live value here too so the static layer can display
  ##   newer most-recent-water-level values where available, without appending or
  ##   deleting the 44k static reference wells.
  optional_cols <- c(
    "latest_wl_ft_bgs",
    "latest_wl_datetime_utc",
    "latest_wl_datetime",
    "latest_wl_date",
    "latest_age_days",
    "candidate_source",
    "on_blm_ca",
    "dist_to_blm_mi",
    "dist_to_blm_ft"
  )

  for (nm in optional_cols) {
    if (!nm %in% names(live)) live[[nm]] <- NA_character_
  }

  live |>
    dplyr::transmute(
      site_no = pt_cache_site_no(.data$site_no),
      well_in_ops_live = TRUE,
      ops_latest_wl_ft_bgs = suppressWarnings(as.numeric(.data$latest_wl_ft_bgs)),
      ops_latest_wl_datetime = dplyr::coalesce(
        as.character(.data$latest_wl_datetime_utc),
        as.character(.data$latest_wl_datetime)
      ),
      ops_latest_wl_date = as.character(.data$latest_wl_date),
      ops_latest_age_days = suppressWarnings(as.numeric(.data$latest_age_days)),
      ops_candidate_source = as.character(.data$candidate_source),
      gw_on_blm_ca = tolower(trimws(as.character(.data$on_blm_ca))) %in% c("true", "t", "1", "yes", "y"),
      gw_dist_to_blm_mi = suppressWarnings(as.numeric(.data$dist_to_blm_mi)),
      gw_dist_to_blm_ft = suppressWarnings(as.numeric(.data$dist_to_blm_ft))
    ) |>
    dplyr::filter(!is.na(.data$site_no), .data$site_no != "") |>
    dplyr::arrange(.data$site_no, .data$ops_latest_age_days) |>
    dplyr::distinct(.data$site_no, .keep_all = TRUE)
}

pt_add_static_well_group_flags <- function(x) {
  if (!inherits(x, "sf") || nrow(x) == 0) {
    x$well_coord_group_count <- integer(0)
    x$well_is_nested <- logical(0)
    x$well_coord_group_key <- character(0)
    return(x)
  }

  coords <- sf::st_coordinates(x)

  if (nrow(coords) != nrow(x) || ncol(coords) < 2) {
    warning("Could not derive one coordinate pair per USGS well; nested-coordinate flags set to FALSE.")
    x$well_coord_group_count <- 1L
    x$well_is_nested <- FALSE
    x$well_coord_group_key <- NA_character_
    return(x)
  }

  coord_key <- paste0(
    sprintf("%.7f", coords[, 1]),
    "|",
    sprintf("%.7f", coords[, 2])
  )

  coord_counts <- as.integer(table(coord_key)[coord_key])

  x$well_coord_group_key <- coord_key
  x$well_coord_group_count <- coord_counts
  x$well_is_nested <- !is.na(coord_counts) & coord_counts > 1L

  message(
    "Static USGS nested/co-located coordinate groups: ",
    length(unique(coord_key[x$well_is_nested])),
    " group(s); records in groups: ",
    sum(x$well_is_nested, na.rm = TRUE)
  )

  x
}

pt_make_well_hover_text <- function(x) {

  ## RF042/RF043b:
  ##   Keep static USGS groundwater hover labels intentionally short and
  ##   multi-line.  Do not show the older static/source active/inactive status
  ##   in hover.  The bright-green ring is now a display flag for recent-feed
  ##   membership, not the old USGS source-status field.

  wl <- if ("latest_wl_ft_bgs" %in% names(x)) {
    suppressWarnings(as.numeric(x$latest_wl_ft_bgs))
  } else {
    rep(NA_real_, nrow(x))
  }

  dt <- if ("latest_wl_date" %in% names(x)) {
    suppressWarnings(as.Date(x$latest_wl_date))
  } else {
    rep(as.Date(NA), nrow(x))
  }

  dt_txt <- ifelse(
    !is.na(dt),
    paste0(" (", format(dt, "%b %Y"), ")"),
    ""
  )

  site_txt <- if ("site_no" %in% names(x)) {
    as.character(x$site_no)
  } else {
    rep("USGS well", nrow(x))
  }

  site_txt <- ifelse(
    is.na(site_txt) | site_txt == "",
    "USGS well",
    paste0("USGS ", site_txt)
  )

  ## RF043c:
  ##   The Local USGS Wells legend defines MR WL as most recent cached
  ##   groundwater level.  Keep hover strings compact by omitting the repeated
  ##   "MR WL:" prefix from every point.
  out <- ifelse(
    !is.na(wl),
    paste0(
      formatC(wl, format = "f", digits = 1, big.mark = ","),
      " ft bgs",
      dt_txt
    ),
    paste0(
      site_txt,
      "\nNo cached water level"
    )
  )

  in_ops <- if ("well_in_ops_live" %in% names(x)) {
    dplyr::coalesce(as.logical(x$well_in_ops_live), FALSE)
  } else {
    rep(FALSE, nrow(x))
  }

  out <- ifelse(
    in_ops,
    paste0(out, "\nRecent feed: yes"),
    out
  )

  nested_n <- if ("well_coord_group_count" %in% names(x)) {
    suppressWarnings(as.integer(x$well_coord_group_count))
  } else {
    rep(1L, nrow(x))
  }

  out <- ifelse(
    !is.na(nested_n) & nested_n > 1L,
    paste0(out, "\nCo-located/nested: ", nested_n, " wells"),
    out
  )

  out
}

pt_sync_static_well_latest_from_ops_live <- function(x) {

  ## RF043b:
  ##   Where a static well is also in the Ops Live groundwater candidate index,
  ##   use the Ops Live latest field-measurement value for static-layer display
  ##   if it is present and newer than the older static cached lookup.  This
  ##   keeps the broad 44k static layer visually synchronized with the live feed
  ##   without appending/deleting wells from the static reference backbone.

  n <- nrow(x)

  if (!"well_in_ops_live" %in% names(x)) x$well_in_ops_live <- rep(FALSE, n)
  if (!"ops_latest_wl_ft_bgs" %in% names(x)) x$ops_latest_wl_ft_bgs <- rep(NA_real_, n)
  if (!"ops_latest_wl_date" %in% names(x)) x$ops_latest_wl_date <- rep(NA_character_, n)
  if (!"ops_latest_wl_datetime" %in% names(x)) x$ops_latest_wl_datetime <- rep(NA_character_, n)
  if (!"ops_latest_age_days" %in% names(x)) x$ops_latest_age_days <- rep(NA_real_, n)

  if (!"latest_wl_ft_bgs" %in% names(x)) x$latest_wl_ft_bgs <- rep(NA_real_, n)
  if (!"latest_wl_date" %in% names(x)) x$latest_wl_date <- as.Date(rep(NA_character_, n))
  if (!"latest_wl_datetime" %in% names(x)) x$latest_wl_datetime <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = "UTC")
  if (!"latest_wl_source" %in% names(x)) x$latest_wl_source <- rep(NA_character_, n)
  if (!"latest_wl_run_ts" %in% names(x)) x$latest_wl_run_ts <- rep(NA_character_, n)

  in_ops <- dplyr::coalesce(as.logical(x$well_in_ops_live), FALSE)
  ops_wl <- suppressWarnings(as.numeric(x$ops_latest_wl_ft_bgs))
  ops_date <- suppressWarnings(as.Date(x$ops_latest_wl_date))
  static_date <- suppressWarnings(as.Date(x$latest_wl_date))

  use_ops <- in_ops &
    !is.na(ops_wl) &
    !is.na(ops_date) &
    (is.na(static_date) | ops_date >= static_date)

  ops_datetime <- suppressWarnings(as.POSIXct(
    x$ops_latest_wl_datetime,
    tz = "UTC"
  ))

  x$latest_wl_ft_bgs[use_ops] <- ops_wl[use_ops]
  x$latest_wl_date[use_ops] <- ops_date[use_ops]
  x$latest_wl_datetime[use_ops] <- ops_datetime[use_ops]
  x$latest_wl_source[use_ops] <- "Ops Live USGS field-measurements feed"
  x$latest_wl_run_ts[use_ops] <- NA_character_

  x$well_latest_synced_from_ops_live <- use_ops
  x$well_latest_source_display <- dplyr::case_when(
    use_ops ~ "Ops Live groundwater feed",
    !is.na(suppressWarnings(as.numeric(x$latest_wl_ft_bgs))) ~ "static cached USGS lookup",
    TRUE ~ "no cached water-level value"
  )

  x$well_ops_latest_age_days <- suppressWarnings(as.numeric(x$ops_latest_age_days))

  message(
    "Static USGS well latest-water-level display synced from Ops Live feed for ",
    sum(use_ops, na.rm = TRUE),
    " well(s)."
  )

  x
}

pt_add_well_wl_styles <- function(x) {
  
  wl <- if ("latest_wl_ft_bgs" %in% names(x)) {
    suppressWarnings(as.numeric(x$latest_wl_ft_bgs))
  } else {
    rep(NA_real_, nrow(x))
  }
  
  status_lc <- tolower(as.character(x$status))
  
  x$well_fill_col <- dplyr::case_when(
    ## Cached water-level depth classes.
    !is.na(wl) & wl < 0                 ~ "#1F78B4",  # above ground / flowing
    !is.na(wl) & wl >= 0   & wl < 100   ~ "#33A02C",  # shallow
    !is.na(wl) & wl >= 100 & wl < 300   ~ "#FF7F00",  # moderate
    !is.na(wl) & wl >= 300 & wl < 600   ~ "#E31A1C",  # deep
    !is.na(wl) & wl >= 600              ~ "#6A3D9A",  # very deep
    
    ## No cached water level: neutral fill only.
    is.na(wl)                           ~ "#F2F2F2",
    TRUE                                ~ "#F2F2F2"
  )
  
  x$well_stroke_col <- dplyr::case_when(
    ## Cached water-level depth classes get matching outlines.
    !is.na(wl) & wl < 0                 ~ "#08519C",
    !is.na(wl) & wl >= 0   & wl < 100   ~ "#1B7837",
    !is.na(wl) & wl >= 100 & wl < 300   ~ "#B35806",
    !is.na(wl) & wl >= 300 & wl < 600   ~ "#99000D",
    !is.na(wl) & wl >= 600              ~ "#3F007D",
    
    ## No cached water level: outline shows status.
    is.na(wl) & status_lc == "active"   ~ "#238B45",
    is.na(wl) & status_lc == "inactive" ~ "#737373",
    is.na(wl)                           ~ "#BDBDBD",
    TRUE                                ~ "#BDBDBD"
  )
  
  x$well_radius <- dplyr::case_when(
    ## Cached WL points stand out slightly.
    !is.na(wl) & wl < 0               ~ 4.8,
    !is.na(wl) & wl >= 0   & wl < 100 ~ 4.8,
    !is.na(wl) & wl >= 100 & wl < 300 ~ 5.2,
    !is.na(wl) & wl >= 300 & wl < 600 ~ 5.8,
    !is.na(wl) & wl >= 600            ~ 6.3,
    
    ## No cached WL points stay smaller.
    is.na(wl) & status_lc == "active" ~ 3.0,
    TRUE                              ~ 2.6
  )
  
  x$well_nested_dash_array <- dplyr::if_else(
    dplyr::coalesce(as.logical(x$well_is_nested), FALSE),
    "2,2",
    NA_character_
  )

  x$hover_text <- pt_make_well_hover_text(x)
  
  x
}

usgs_gw_map <- pt_add_static_well_group_flags(usgs_gw_map)

live_gw_index <- pt_read_live_gw_index_for_static()

if (nrow(live_gw_index) > 0) {
  usgs_gw_map <- usgs_gw_map |>
    dplyr::mutate(site_no = pt_cache_site_no(.data$site_no)) |>
    dplyr::left_join(live_gw_index, by = "site_no") |>
    dplyr::mutate(
      well_in_ops_live = dplyr::coalesce(.data$well_in_ops_live, FALSE)
    )
} else {
  usgs_gw_map$well_in_ops_live <- FALSE
}

latest_wl <- pt_read_latest_gw_wl()

if (!is.null(latest_wl) && nrow(latest_wl) > 0) {
  
  latest_wl <- latest_wl |>
    dplyr::select(
      dplyr::any_of(c(
        "site_no",
        "monitoring_location_id",
        "latest_wl_ft_bgs",
        "latest_wl_datetime",
        "latest_wl_date",
        "latest_wl_status",
        "latest_wl_procedure",
        "latest_wl_qualifier",
        "latest_wl_units",
        "latest_wl_source",
        "latest_wl_run_ts"
      ))
    ) |>
    dplyr::distinct(.data$site_no, .keep_all = TRUE)
  
  usgs_gw_map <- usgs_gw_map |>
    dplyr::left_join(latest_wl, by = "site_no")
}

usgs_gw_map <- pt_sync_static_well_latest_from_ops_live(usgs_gw_map)

usgs_gw_map <- pt_add_well_wl_styles(usgs_gw_map)

# ---- 8.4D Static-well browser-template popup fields -------------------------
##
## PURPOSE:
##   The static USGS Wells layer has ~44k records.  Prebuilding a complete
##   HTML popup string for every well adds a large amount of repeated markup to
##   the standalone HTML.  Instead, keep compact per-well popup values in the
##   cache and let a shared browser-side JavaScript template build the popup
##   only when a user clicks a well.
##
## USER-FACING BEHAVIOR:
##   The popup content is intentionally the same as pt_make_usgs_well_popups():
##   site ID/name, elevation, type/status, record count, recent-feed note,
##   most-recent groundwater level, well/hole depth, aquifer fields, period,
##   common-data summary, and USGS site link.

pt_bool_for_popup <- function(x) {
  if (is.logical(x)) return(dplyr::coalesce(x, FALSE))
  txt <- tolower(trimws(as.character(x)))
  txt %in% c("true", "t", "1", "yes", "y")
}

pt_popup_col <- function(df, nm, default = NA_character_) {
  if (nm %in% names(df)) return(df[[nm]])
  rep(default, nrow(df))
}

pt_add_usgs_well_popup_template_fields <- function(x) {

  n <- nrow(x)

  common_params <- if ("param_list" %in% names(x)) {
    pt_summarize_usgs_well_params(x$param_list)
  } else {
    rep("see USGS site", n)
  }

  latest_wl <- suppressWarnings(as.numeric(
    pt_popup_col(x, "latest_wl_ft_bgs", NA_real_)
  ))

  latest_dt <- as.character(
    pt_popup_col(x, "latest_wl_date", NA_character_)
  )

  latest_run <- as.character(
    pt_popup_col(x, "latest_wl_run_ts", NA_character_)
  )

  nested_n <- suppressWarnings(as.integer(
    pt_popup_col(x, "well_coord_group_count", 1L)
  ))

  in_ops <- pt_bool_for_popup(pt_popup_col(x, "well_in_ops_live", FALSE))
  synced_ops <- pt_bool_for_popup(pt_popup_col(x, "well_latest_synced_from_ops_live", FALSE))

  source_status <- as.character(pt_popup_col(x, "status", NA_character_))
  source_status <- trimws(source_status)
  source_status[is.na(source_status) | source_status == ""] <- "Status unknown"

  ## Compact, explicit fields used only by browser-side popup templating.
  ## The gwpop_ prefix makes these easy to retain/drop without preserving large
  ## raw/source fields in the final Leaflet object.
  x$gwpop_site_no      <- as.character(pt_popup_col(x, "site_no", NA_character_))
  x$gwpop_name         <- as.character(pt_popup_col(x, "name", NA_character_))
  x$gwpop_elev_ft      <- as.character(pt_popup_col(x, "elev_ft", NA_character_))
  x$gwpop_site_type    <- as.character(pt_popup_col(x, "site_type", NA_character_))
  x$gwpop_status       <- source_status
  x$gwpop_count_nu     <- as.character(pt_popup_col(x, "count_nu", NA_character_))
  x$gwpop_nested_n     <- nested_n
  x$gwpop_in_ops       <- in_ops
  x$gwpop_latest_wl    <- latest_wl
  x$gwpop_latest_date  <- latest_dt
  x$gwpop_latest_run   <- latest_run
  x$gwpop_synced_ops   <- synced_ops
  x$gwpop_well_depth   <- as.character(pt_popup_col(x, "well_depth_ft", NA_character_))
  x$gwpop_hole_depth   <- as.character(pt_popup_col(x, "hole_depth_ft", NA_character_))
  x$gwpop_aquifer      <- as.character(pt_popup_col(x, "aquifer_cd", NA_character_))
  x$gwpop_aquifer_type <- as.character(pt_popup_col(x, "aquifer_type_cd", NA_character_))
  x$gwpop_nat_aqfr     <- as.character(pt_popup_col(x, "nat_aqfr_cd", NA_character_))
  x$gwpop_start_date   <- as.character(pt_popup_col(x, "start_date", NA_character_))
  x$gwpop_end_date     <- as.character(pt_popup_col(x, "end_date", NA_character_))
  x$gwpop_common_data  <- common_params

  ## Do not carry prebuilt popup_html for this dense layer.
  ## Leaflet popups are attached from gwpop_* fields in leaflet_layer_helpers.r.
  if ("popup_html" %in% names(x)) x$popup_html <- NULL

  x
}

usgs_gw_map <- pt_add_usgs_well_popup_template_fields(usgs_gw_map)

# ---- 8.4E Static-well display-schema cleanup --------------------------------
##
## Keep the map-ready USGS Wells cache lean and unambiguous.
##
## Raw/source metadata that is useful in popups remains, including `status` as
## the original USGS/source status. Transient Ops Live join columns and older
## ambiguous helper fields are removed after hover/popup/style fields have been
## created. This reduces cache size and prevents future code from accidentally
## treating stale source-status or temporary join columns as authoritative
## display fields.

pt_prune_static_well_display_schema <- function(x) {
  drop_cols <- c(
    "ops_latest_wl_ft_bgs",
    "ops_latest_wl_datetime",
    "ops_latest_wl_date",
    "ops_latest_age_days",
    "ops_candidate_source",
    "well_latest_synced_from_ops_live",
    "well_latest_source_display",
    "well_ops_latest_age_days",
    "latest_wl_status",
    "latest_wl_procedure",
    "latest_wl_qualifier",
    "latest_wl_units",
    "well_status_clean",
    "well_status_display",
    "well_in_recent_feed",
    "well_active_outline_basis",
    "well_active_outline_col",
    "well_active_outline_weight"
  )

  drop_cols <- intersect(drop_cols, names(x))

  if (length(drop_cols) > 0) {
    message(
      "Pruning transient/ambiguous static USGS well display fields: ",
      paste(drop_cols, collapse = ", ")
    )
    x <- dplyr::select(x, -dplyr::any_of(drop_cols))
  }

  x
}

usgs_gw_map <- pt_prune_static_well_display_schema(usgs_gw_map)

