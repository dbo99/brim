# ==== 05_cache_final_point_tweaks.r ==========================================
##
## PURPOSE:
##   Apply late-stage point-layer styling/hover tweaks that historically lived near the end of section 8.
##
## NOTE:
##   This file is sourced by 05_map_build/02_build_core_map_cache.r.
##   It expects objects created earlier in that script and creates map-ready
##   cache objects in the calling environment. Do not source this file alone
##   unless you have already created the required input objects.
## ============================================================================

# ---- 8.x SCAN stations and snow pillows ------------------------------------
##
## PURPOSE:
##   Build map-ready station layers for:
##     1. SCAN Stations
##     2. Snow Pillows
##
## SWE001 STATIC-LAYER SCHEMA CLEANUP:
##   These cached layers are station/reference layers only. Current/recent soil
##   moisture, SWE, snow depth, and change metrics belong in Ops Live hosted
##   GeoJSON feeds so stale sidecar observations are not embedded in the static
##   core map cache.
##
##   The earlier 18b recent-observation sidecar remains useful as prototype
##   logic for future live feeds, but this cache block intentionally does not
##   expose those values in static hovers, popups, symbology, or written static
##   cache columns.
##
##   For the Snow Pillows layer, SWE001 deliberately removes the old NA-filled
##   latest_swe/latest_snow placeholder columns from the map-ready data frame.
##   That keeps the static cache lean and prevents future confusion between
##   reference metadata and future Ops Live current-value fields.

pt_station_chr_col <- function(x, nm) {
  
  if (nm %in% names(x)) {
    out <- as.character(x[[nm]])
  } else {
    out <- rep(NA_character_, nrow(x))
  }
  
  out <- trimws(out)
  out[out == ""] <- NA_character_
  out
}

pt_station_num_col <- function(x, nm) {
  
  if (nm %in% names(x)) {
    suppressWarnings(as.numeric(gsub(",", "", as.character(x[[nm]]))))
  } else {
    rep(NA_real_, nrow(x))
  }
}

pt_station_fmt_num <- function(x, digits = 1, suffix = "") {
  
  x <- suppressWarnings(as.numeric(x))
  out <- rep("Not available", length(x))
  
  ok <- !is.na(x)
  
  out[ok] <- paste0(
    formatC(
      x[ok],
      format = "f",
      digits = digits,
      big.mark = ","
    ),
    suffix
  )
  
  out <- sub("(\\.\\d*?)0+([^0-9]|$)", "\\1\\2", out)
  out <- sub("\\.([^0-9]|$)", "\\1", out)
  
  out
}

pt_station_fmt_signed <- function(x, digits = 1, suffix = "") {
  
  x <- suppressWarnings(as.numeric(x))
  out <- rep("Not available", length(x))
  
  ok <- !is.na(x)
  vals <- pt_station_fmt_num(abs(x), digits = digits, suffix = suffix)
  
  out[ok] <- paste0(
    ifelse(x[ok] > 0, "+", ifelse(x[ok] < 0, "-", "")),
    vals[ok]
  )
  
  out
}

pt_station_fmt_date <- function(x) {
  
  x <- as.character(x)
  x <- trimws(x)
  x[is.na(x) | x == ""] <- "Not available"
  x
}

pt_station_year_from_any_date <- function(x) {
  
  ## SWE002:
  ##   Station-reference source dates are not consistent across providers.
  ##   CDEC commonly uses ISO-like strings such as 1984-01-01T00:00:00,
  ##   while NRCS/NWCC ArcGIS metadata commonly stores begin/end dates as
  ##   milliseconds since 1970-01-01.  Convert both forms to display years so
  ##   static popups can report a readable period of record without carrying
  ##   raw, provider-specific date columns in the map-ready cache.
  x_chr <- as.character(x)
  x_chr <- trimws(x_chr)
  x_chr[is.na(x_chr) | x_chr == ""] <- NA_character_
  
  out <- rep(NA_integer_, length(x_chr))
  
  ## 1. ISO / plain-year strings.
  lead_year <- suppressWarnings(as.integer(sub("^\\s*([12][0-9]{3}).*$", "\\1", x_chr)))
  ok_lead <- !is.na(lead_year) & lead_year >= 1800 & lead_year <= 2100
  out[ok_lead] <- lead_year[ok_lead]
  
  ## 2. Numeric epoch values.  Values in the snow/soil station source are
  ##    usually milliseconds.  Also tolerate seconds for future-proofing.
  need_epoch <- is.na(out) & !is.na(x_chr)
  n <- suppressWarnings(as.numeric(x_chr))
  ok_ms <- need_epoch & !is.na(n) & abs(n) > 1e11
  if (any(ok_ms)) {
    yr <- suppressWarnings(as.integer(format(
      as.POSIXct(n[ok_ms] / 1000, origin = "1970-01-01", tz = "UTC"),
      "%Y"
    )))
    yr[yr < 1800 | yr > 2200] <- NA_integer_
    out[ok_ms] <- yr
  }
  
  ok_sec <- need_epoch & is.na(out) & !is.na(n) & abs(n) > 1e8 & abs(n) <= 1e11
  if (any(ok_sec)) {
    yr <- suppressWarnings(as.integer(format(
      as.POSIXct(n[ok_sec], origin = "1970-01-01", tz = "UTC"),
      "%Y"
    )))
    yr[yr < 1800 | yr > 2200] <- NA_integer_
    out[ok_sec] <- yr
  }
  
  out
}

pt_station_fmt_period_record <- function(start_date, end_date) {
  
  start_year <- pt_station_year_from_any_date(start_date)
  end_year <- pt_station_year_from_any_date(end_date)
  
  out <- rep("Not available", max(length(start_year), length(end_year)))
  start_year <- rep(start_year, length.out = length(out))
  end_year <- rep(end_year, length.out = length(out))
  
  has_start <- !is.na(start_year)
  has_end <- !is.na(end_year) & end_year < 2099
  active_sentinel <- !is.na(end_year) & end_year >= 2099
  
  out[has_start & has_end] <- paste0(start_year[has_start & has_end], "–", end_year[has_start & has_end])
  out[has_start & (!has_end | active_sentinel)] <- paste0(start_year[has_start & (!has_end | active_sentinel)], "–present")
  out[!has_start & has_end] <- paste0("Through ", end_year[!has_start & has_end])
  
  out
}

pt_station_fmt_elev <- function(x) {
  
  v <- suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
  
  ifelse(
    is.na(v),
    "Not available",
    paste0(formatC(v, format = "f", digits = 0, big.mark = ","), " ft")
  )
}

pt_station_fmt_start_year <- function(x) {
  
  ## RF040/SWE002:
  ##   The static SCAN/Snow station popups are station/reference oriented.
  ##   Report only a readable starting year, and support both ISO-like date
  ##   strings and NRCS epoch-millisecond date values.
  yr <- pt_station_year_from_any_date(x)
  out <- rep("Not available", length(yr))
  ok <- !is.na(yr) & yr >= 1800 & yr <= 2100
  out[ok] <- as.character(yr[ok])
  out
}

pt_station_fmt_lat_lon <- function(lat, lon, digits = 6) {
  
  lat <- suppressWarnings(as.numeric(gsub(",", "", as.character(lat))))
  lon <- suppressWarnings(as.numeric(gsub(",", "", as.character(lon))))
  
  out <- rep("Not available", max(length(lat), length(lon)))
  ok <- !is.na(lat) & !is.na(lon)
  
  out[ok] <- paste0(
    formatC(lat[ok], format = "f", digits = digits),
    ", ",
    formatC(lon[ok], format = "f", digits = digits)
  )
  
  out
}

pt_soil_moisture_fill <- function(x) {
  
  dplyr::case_when(
    is.na(x)        ~ "#BDBDBD",
    x < 5           ~ "#8C510A",
    x < 10          ~ "#D8B365",
    x < 20          ~ "#F6E8C3",
    x < 30          ~ "#C7EAE5",
    x < 40          ~ "#5AB4AC",
    x >= 40         ~ "#01665E",
    TRUE            ~ "#BDBDBD"
  )
}

pt_swe_fill <- function(x) {
  
  dplyr::case_when(
    is.na(x)        ~ "#BDBDBD",
    x <= 0          ~ "#F7FBFF",
    x <= 5          ~ "#DEEBF7",
    x <= 15         ~ "#9ECAE1",
    x <= 30         ~ "#3182BD",
    x <= 50         ~ "#08519C",
    x > 50          ~ "#08306B",
    TRUE            ~ "#BDBDBD"
  )
}

pt_swe_radius <- function(x) {
  
  dplyr::case_when(
    is.na(x) | x <= 0  ~ 3.5,
    x <= 5             ~ 4.2,
    x <= 15            ~ 5.2,
    x <= 30            ~ 6.4,
    x <= 50            ~ 7.6,
    x > 50             ~ 8.8,
    TRUE               ~ 3.5
  )
}

pt_delta_stroke_col <- function(delta_class) {
  
  delta_class <- as.character(delta_class)
  
  dplyr::case_when(
    delta_class == "increase"         ~ "#00A6D6",
    delta_class == "decrease"         ~ "#E6550D",
    delta_class == "little/no change" ~ "#666666",
    TRUE                              ~ "#333333"
  )
}

pt_make_station_map_cache <- function(x, layer_kind = c("scan", "snow")) {
  
  layer_kind <- match.arg(layer_kind)
  
  x <- x |>
    clean_sf_for_leaflet()
  
  station_name <- pt_station_chr_col(x, "station_name")
  station_type <- pt_station_chr_col(x, "station_type")
  station_type_label <- pt_station_chr_col(x, "station_type_label")
  provider <- pt_station_chr_col(x, "provider")
  source_system <- pt_station_chr_col(x, "source_system")
  station_id <- pt_station_chr_col(x, "station_id")
  state <- pt_station_chr_col(x, "state")
  county <- pt_station_chr_col(x, "county")
  basin <- pt_station_chr_col(x, "basin")
  huc <- pt_station_chr_col(x, "huc")
  status <- pt_station_chr_col(x, "status")
  site_page_url <- pt_station_chr_col(x, "site_page_url")
  data_page_url <- pt_station_chr_col(x, "data_page_url")
  elevation_display <- pt_station_fmt_elev(pt_station_chr_col(x, "elevation_ft"))
  station_start_year_display <- pt_station_fmt_start_year(pt_station_chr_col(x, "start_date"))
  station_period_record_display <- pt_station_fmt_period_record(
    pt_station_chr_col(x, "start_date"),
    pt_station_chr_col(x, "end_date")
  )
  lat_lon_display <- pt_station_fmt_lat_lon(
    pt_station_chr_col(x, "latitude"),
    pt_station_chr_col(x, "longitude")
  )
  
  station_name_display <- dplyr::coalesce(
    station_name,
    paste0(
      dplyr::coalesce(station_type_label, "Station"),
      " ",
      dplyr::coalesce(station_id, "")
    )
  )
  
  context_display <- dplyr::case_when(
    !is.na(county) & !is.na(state) ~ paste0(county, ", ", state),
    !is.na(state) ~ state,
    TRUE ~ "Not available"
  )
  
  if (layer_kind == "scan") {
    
    ## RF039:
    ##   Static SCAN stations are now reference-only. Even if a recent sidecar
    ##   object is accidentally supplied, do not expose stale soil-moisture
    ##   values in static hover text, popup text, or symbology.
    ##
    ##   Current soil moisture and depth-specific historical context will be
    ##   handled by a future Ops Live SCAN feed.
    soil_depths_available <- pt_station_chr_col(x, "scan_soil_moisture_depths_available")
    
    radius <- rep(4.4, nrow(x))
    fill_col <- rep("#BDBDBD", nrow(x))
    stroke_col <- rep("#4A1486", nrow(x))
    fill_opacity <- rep(0.70, nrow(x))
    stroke_weight <- rep(1.0, nrow(x))
    
    hover_text <- paste0(
      station_name_display,
      "\nType: ", dplyr::coalesce(station_type_label, "SCAN"),
      "\nStation/reference layer"
    )
    
    popup_html <- paste0(
      "<b>", htmltools::htmlEscape(station_name_display), "</b><br>",
      "<b>Type:</b> ", htmltools::htmlEscape(dplyr::coalesce(station_type_label, "SCAN")), "<br>",
      "<b>Provider:</b> ", htmltools::htmlEscape(dplyr::coalesce(provider, source_system, "Not available")), "<br>",
      "<b>Station ID:</b> ", htmltools::htmlEscape(dplyr::coalesce(station_id, "Not available")), "<br>",
      "<b>Station/record start:</b> ", htmltools::htmlEscape(station_start_year_display), "<br>",
      "<b>Elevation:</b> ", htmltools::htmlEscape(elevation_display), "<br>",
      "<b>Location:</b> ", htmltools::htmlEscape(context_display), "<br>",
      "<b>Lat/Lon:</b> ", htmltools::htmlEscape(lat_lon_display), "<br>",
      "<b>Status:</b> ", htmltools::htmlEscape(dplyr::coalesce(status, "Not available")),
      dplyr::if_else(
        !is.na(soil_depths_available),
        paste0("<br><b>Soil-moisture depths:</b> ", htmltools::htmlEscape(soil_depths_available), " in"),
        ""
      ),
      "<hr style='margin:4px 0;'/>",
      "<b>Current soil moisture:</b> See Ops Live SCAN layer when available.<br>",
      "<span style='font-size:11px; color:#555;'>",
      "This static layer is station/reference metadata only and is not refreshed for current values.",
      "</span>",
      dplyr::if_else(
        !is.na(site_page_url),
        paste0(
          "<br><a href='",
          htmltools::htmlEscape(site_page_url),
          "' target='_blank'>Open station page</a>"
        ),
        ""
      ),
      dplyr::if_else(
        !is.na(data_page_url) & dplyr::coalesce(data_page_url != site_page_url, TRUE),
        paste0(
          "<br><a href='",
          htmltools::htmlEscape(data_page_url),
          "' target='_blank'>Open data page</a>"
        ),
        ""
      )
    )
    
    out <- x |>
      dplyr::mutate(
        station_name_display = station_name_display,
        station_type = station_type,
        station_type_display = dplyr::coalesce(station_type_label, "SCAN"),
        provider_display = dplyr::coalesce(provider, source_system, "Not available"),
        station_id_display = dplyr::coalesce(station_id, "Not available"),
        station_start_year_display = station_start_year_display,
        station_period_record_display = station_period_record_display,
        lat_lon_display = lat_lon_display,
        elevation_display = elevation_display,
        context_display = context_display,
        status_display = dplyr::coalesce(status, "Not available"),
        scan_selected_soil_moisture_pct = NA_real_,
        scan_selected_soil_moisture_depth_in = NA_real_,
        scan_selected_soil_moisture_date = NA_character_,
        scan_soil_moisture_depth_summary = NA_character_,
        station_radius = radius,
        station_fill_col = fill_col,
        station_stroke_col = stroke_col,
        station_fill_opacity = fill_opacity,
        station_stroke_weight = stroke_weight,
        hover_text = hover_text,
        popup_html = popup_html
      )
    
  } else {
    
    ## RF039:
    ##   Snow pillows follow the same static/reference split as SCAN stations.
    ##   Do not expose stale SWE, snow-depth, or delta fields in the static core
    ##   cache. Those time-sensitive values should be handled by Ops Live feeds.
    radius <- rep(4.6, nrow(x))
    fill_col <- rep("#D9D9D9", nrow(x))
    stroke_col <- rep("#08519C", nrow(x))
    fill_opacity <- rep(0.70, nrow(x))
    
    stroke_weight <- dplyr::case_when(
      station_type == "cdec_snow_sensor" ~ 1.8,
      TRUE                               ~ 1.1
    )
    
    hover_text <- paste0(
      station_name_display,
      "\nStation code: ", dplyr::coalesce(station_id, "Not available"),
      "\nSource: ", dplyr::coalesce(provider, source_system, "Not available"),
      "\nElevation: ", elevation_display,
      dplyr::if_else(
        !is.na(basin),
        paste0("\nRiver basin: ", basin),
        ""
      )
    )
    
    popup_html <- paste0(
      "<b>", htmltools::htmlEscape(station_name_display), "</b><br>",
      "<b>Station code:</b> ", htmltools::htmlEscape(dplyr::coalesce(station_id, "Not available")), "<br>",
      "<b>Source:</b> ", htmltools::htmlEscape(dplyr::coalesce(provider, source_system, "Not available")), "<br>",
      "<b>Type:</b> ", htmltools::htmlEscape(dplyr::coalesce(station_type_label, "Snow pillow / snow sensor")), "<br>",
      "<b>Elevation:</b> ", htmltools::htmlEscape(elevation_display), "<br>",
      dplyr::if_else(
        !is.na(basin),
        paste0("<b>River basin:</b> ", htmltools::htmlEscape(basin), "<br>"),
        ""
      ),
      "<b>Period of record:</b> ", htmltools::htmlEscape(station_period_record_display), "<br>",
      "<b>Location:</b> ", htmltools::htmlEscape(context_display), "<br>",
      "<b>Lat/Lon:</b> ", htmltools::htmlEscape(lat_lon_display),
      dplyr::if_else(
        !is.na(huc),
        paste0("<br><b>HUC:</b> ", htmltools::htmlEscape(huc)),
        ""
      ),
      "<br><b>Status:</b> ", htmltools::htmlEscape(dplyr::coalesce(status, "Not available")),
      "<hr style='margin:4px 0;'/>",
      "<b>Current snow data:</b> See Ops Live snow layers when available.<br>",
      "<span style='font-size:11px; color:#555;'>",
      "This static layer is station/reference metadata only and is not refreshed for current values.",
      "</span>",
      dplyr::if_else(
        !is.na(site_page_url),
        paste0(
          "<br><a href='",
          htmltools::htmlEscape(site_page_url),
          "' target='_blank'>Open station page</a>"
        ),
        ""
      ),
      dplyr::if_else(
        !is.na(data_page_url) & dplyr::coalesce(data_page_url != site_page_url, TRUE),
        paste0(
          "<br><a href='",
          htmltools::htmlEscape(data_page_url),
          "' target='_blank'>Open data page</a>"
        ),
        ""
      )
    )
    
    out <- x |>
      dplyr::mutate(
        station_name_display = station_name_display,
        station_type = station_type,
        station_type_display = dplyr::coalesce(station_type_label, "Snow pillow"),
        provider_display = dplyr::coalesce(provider, source_system, "Not available"),
        station_id_display = dplyr::coalesce(station_id, "Not available"),
        station_start_year_display = station_start_year_display,
        station_period_record_display = station_period_record_display,
        lat_lon_display = lat_lon_display,
        elevation_display = elevation_display,
        context_display = context_display,
        status_display = dplyr::coalesce(status, "Not available"),
        station_radius = radius,
        station_fill_col = fill_col,
        station_stroke_col = stroke_col,
        station_fill_opacity = fill_opacity,
        station_stroke_weight = stroke_weight,
        hover_text = hover_text,
        popup_html = popup_html
      )
  }
  
  common_cols <- c(
    "station_uid",
    "station_name_display",
    "station_type",
    "station_type_display",
    "provider_display",
    "station_id_display",
    "station_start_year_display",
    "station_period_record_display",
    "lat_lon_display",
    "elevation_display",
    "context_display",
    "status_display"
  )

  scan_cols <- c(
    "scan_selected_soil_moisture_pct",
    "scan_selected_soil_moisture_depth_in",
    "scan_selected_soil_moisture_date",
    "scan_soil_moisture_depth_summary"
  )

  ## SWE001:
  ##   No Snow Pillow current-value columns are retained in the static cache.
  ##   Future current SWE, snow-depth, age/staleness, and delta fields should
  ##   be created only in hosted Ops Live data products.
  snow_cols <- character(0)

  style_cols <- c(
    "hover_text",
    "popup_html",
    "station_radius",
    "station_fill_col",
    "station_stroke_col",
    "station_fill_opacity",
    "station_stroke_weight",
    "geometry"
  )

  keep_cols <- c(
    common_cols,
    if (layer_kind == "scan") scan_cols else snow_cols,
    style_cols
  )

  out |>
    dplyr::select(dplyr::any_of(keep_cols))
}

scan_stations_map <- pt_make_station_map_cache(
  scan_stations,
  layer_kind = "scan"
)

snow_pillows_map <- pt_make_station_map_cache(
  snow_pillows,
  layer_kind = "snow"
)

message("SCAN station static/reference map cache summary:")
print(
  scan_stations_map |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::summarize(
      rows = dplyr::n(),
      static_reference_rows = dplyr::n(),
      current_soil_moisture_values_embedded = sum(!is.na(scan_selected_soil_moisture_pct))
    ),
  width = 1200
)

snow_static_current_value_fields <- c(
  "latest_swe_in",
  "latest_swe_date",
  "swe_delta_24hr_in",
  "swe_delta_48hr_in",
  "swe_delta_24hr_class",
  "latest_snow_depth_in",
  "latest_snow_depth_date"
)

snow_static_current_value_fields_found <- intersect(
  snow_static_current_value_fields,
  names(snow_pillows_map)
)

if (length(snow_static_current_value_fields_found) > 0) {
  stop(
    "SWE001 schema guard failed: static snow_pillows_map still contains ",
    "current-value field(s): ",
    paste(snow_static_current_value_fields_found, collapse = ", "),
    ". Remove these from the static station/reference cache and keep them ",
    "only in future Ops Live hosted products."
  )
}

message("Snow pillow static/reference map cache summary:")
print(
  snow_pillows_map |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::summarize(
      rows = dplyr::n(),
      static_reference_rows = dplyr::n(),
      current_swe_fields_present = length(snow_static_current_value_fields_found),
      static_swe_schema_clean = length(snow_static_current_value_fields_found) == 0
    ),
  width = 1200
)

message("Snow pillow static/reference map cache columns:")
print(names(sf::st_drop_geometry(snow_pillows_map)))

# ---- 8.11 Generic reference/admin/conservation layers -----------------------
##
## These layers are managed by a manifest and cached together as a named list.
##
## IMPORTANT:
##   Each layer carries its own:
##     - pt_simplify_keep
##     - pt_display_name
##     - pt_geom_type
##     - pt_popup_spec
##     - pt_colorbycolumn
##
## QUICK FIXES / SPECIAL CASES:
##   - Wild & Scenic Rivers: color by CATEGORY_c with category-aware colors.
##   - National Monuments: color by agency, with stable BLM/USFS/NPS colors.
##   - Federal Wilderness: color by ManagingAg, with stable agency colors.
##   - Generic reference layers: color by manifest colorbycolumn.
##   - Layers with no colorbycolumn use one visible default.

reference_default_col <- "#756BB1"

reference_palette <- c(
  "#1F78B4", "#33A02C", "#E31A1C", "#FF7F00", "#6A3D9A",
  "#B15928", "#A6CEE3", "#B2DF8A", "#FB9A99", "#FDBF6F",
  "#CAB2D6", "#8DD3C7", "#80B1D3", "#BC80BD", "#CCEBC5"
)

# ---- WSR display helpers ----------------------------------------------------
##
## BRIM keeps five local WSR source layers because the sources are complementary:
##   - BLM-CA linework and corridor polygons
##   - USFS/interagency segment centerlines with ORV attributes
##   - USFS/LSRS area and legal-status polygons with action/boundary fields
##
## These helpers normalize display/filter fields while keeping each source layer
## separate and explicitly labeled in the Local panel.

pt_wsr_line_layers <- c("wsr_blm_lines", "wsr_segments")
pt_wsr_corridor_layers <- c(
  "wsr_corridor_blm",
  "wsr_corridor_lsrs_area",
  "wsr_corridor_lsrs_status"
)
pt_wsr_layers <- c(pt_wsr_line_layers, pt_wsr_corridor_layers)

pt_wsr_clean_chr <- function(x, fallback = "") {
  x <- trimws(as.character(x))
  x[is.na(x) | x %in% c("", "NA", "N/A", "<NA>", "NULL", "None")] <- fallback
  x
}

pt_wsr_first_good <- function(df, fields, fallback = "") {
  out <- rep(fallback, nrow(df))
  for (field in fields) {
    if (!field %in% names(df)) next
    vals <- pt_wsr_clean_chr(df[[field]], fallback = "")
    fill <- (is.na(out) | trimws(out) == "" | out == fallback) & vals != ""
    out[fill] <- vals[fill]
  }
  out[is.na(out) | trimws(out) == ""] <- fallback
  out
}

pt_wsr_class <- function(x) {
  v <- tolower(pt_wsr_clean_chr(x))
  dplyr::case_when(
    grepl("wild", v) ~ "Wild",
    grepl("scenic", v) ~ "Scenic",
    grepl("recreat|recreation|rec\\b", v) ~ "Recreational",
    TRUE ~ "Other / unknown"
  )
}

pt_wsr_boundary_or_corridor_status <- function(x) {
  v <- tolower(pt_wsr_clean_chr(x))
  dplyr::case_when(
    grepl("designated", v) ~ "Designated",
    grepl("suitable", v) ~ "Suitable",
    grepl("eligible", v) ~ "Eligible",
    grepl("final", v) ~ "Final",
    grepl("provisional", v) ~ "Provisional",
    TRUE ~ "Other / unknown"
  )
}

pt_wsr_class_color <- function(class) {
  dplyr::case_when(
    class == "Wild" ~ "#1B7837",
    class == "Scenic" ~ "#2C7FB8",
    class == "Recreational" ~ "#F0A202",
    TRUE ~ "#737373"
  )
}

pt_wsr_status_dash <- function(status) {
  dplyr::case_when(
    status %in% c("Designated", "Final") ~ "",
    status %in% c("Suitable", "Provisional") ~ "6,4",
    status == "Eligible" ~ "2,4",
    TRUE ~ "3,4"
  )
}

pt_wsr_yes_flag <- function(x) {
  v <- tolower(pt_wsr_clean_chr(x))
  !is.na(v) & v %in% c("yes", "y", "true", "1")
}

pt_wsr_other_orv_flag <- function(x) {
  v <- trimws(as.character(x))
  !is.na(v) & v != "" & !tolower(v) %in% c("no", "n", "false", "0", "na", "n/a", "none", "<na>")
}

pt_wsr_fmt_num <- function(x, digits = 1) {
  v <- suppressWarnings(as.numeric(x))
  out <- ifelse(is.na(v), "", format(round(v, digits), trim = TRUE, big.mark = ",", nsmall = digits))
  out
}

pt_wsr_date_chr <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  pt_wsr_clean_chr(x)
}

pt_wsr_year_int <- function(x) {
  txt <- pt_wsr_date_chr(x)
  yr <- suppressWarnings(as.integer(substr(txt, 1, 4)))
  yr[is.na(yr) | yr < 1800 | yr > 2200] <- NA_integer_
  yr
}

pt_wsr_source_label <- function(nickname) {
  dplyr::case_when(
    nickname == "wsr_blm_lines" ~ "BLM-CA line source",
    nickname == "wsr_segments" ~ "USFS/interagency segment source",
    nickname == "wsr_corridor_blm" ~ "BLM-CA corridor source",
    nickname == "wsr_corridor_lsrs_area" ~ "USFS/LSRS area source",
    nickname == "wsr_corridor_lsrs_status" ~ "USFS/LSRS legal-status source",
    TRUE ~ "WSR source"
  )
}

pt_wsr_nonempty_pair <- function(label, val) {
  val <- pt_wsr_clean_chr(val)
  ifelse(nzchar(val), paste0("<br/><b>", pt_esc(label), ":</b> ", pt_esc(val)), "")
}

pt_make_wsr_line_popups <- function(x) {
  df <- sf::st_drop_geometry(x)
  river <- pt_wsr_first_good(df, c("WSR_RIVER_", "WSR_RIVER1", "NLCS_NAME", "GNIS_NAME"), "Wild & Scenic River")
  short <- pt_wsr_first_good(df, c("WSR_RIVER1", "NLCS_NAME", "GNIS_NAME", "WSR_RIVER_"), "WSR")
  seg <- pt_wsr_first_good(df, c("GNIS_NAME"), "")
  cls <- if ("wsr_class" %in% names(df)) df$wsr_class else pt_wsr_class(pt_wsr_first_good(df, c("CLASSIFICA", "CATEGORY_c"), ""))
  seg_mi <- if ("SEGMENT_MI" %in% names(df)) pt_wsr_fmt_num(df$SEGMENT_MI, 1) else rep("", nrow(df))
  total_mi <- if ("TOTAL_MILE" %in% names(df)) pt_wsr_fmt_num(df$TOTAL_MILE, 1) else rep("", nrow(df))
  agency <- pt_wsr_first_good(df, c("AGENCY"), "")
  admin <- pt_wsr_first_good(df, c("ADMINISTRA"), "")
  st <- pt_wsr_first_good(df, c("STATE"), "California")
  county <- pt_wsr_first_good(df, c("COUNTY"), "")
  begin <- pt_wsr_first_good(df, c("BEGINNING_"), "")
  end <- pt_wsr_first_good(df, c("ENDING_POI"), "")
  orv <- pt_wsr_first_good(df, c("ORV_LIST"), "")
  river_id <- pt_wsr_first_good(df, c("RIVER_ID"), "")
  seg_id <- pt_wsr_first_good(df, c("SEGMENT_ID", "SMA_ID"), "")
  source_label <- if ("wsr_source_label" %in% names(df)) df$wsr_source_label else rep("WSR line source", nrow(df))

  vapply(seq_len(nrow(df)), function(i) {
    rows <- c(
      paste0("<div class=\"pt-popup\"><b>Wild & Scenic River segment</b>"),
      paste0("<br/><b>River:</b> ", pt_esc(river[i])),
      if (nzchar(seg[i]) && seg[i] != short[i] && seg[i] != river[i]) paste0("<br/><b>Segment / stream:</b> ", pt_esc(seg[i])) else "",
      paste0("<br/><b>Classification:</b> ", pt_esc(cls[i])),
      paste0("<br/><b>Source:</b> ", pt_esc(source_label[i])),
      if (nzchar(seg_mi[i])) paste0("<br/><b>Segment miles:</b> ", pt_esc(seg_mi[i])) else "",
      if (nzchar(total_mi[i])) paste0("<br/><b>Total WSR miles:</b> ", pt_esc(total_mi[i])) else "",
      if (nzchar(agency[i])) paste0("<br/><b>Agency:</b> ", pt_esc(agency[i])) else "",
      if (nzchar(admin[i])) paste0("<br/><b>Admin unit:</b> ", pt_esc(admin[i])) else "",
      if (nzchar(st[i]) || nzchar(county[i])) paste0("<br/><b>State / county:</b> ", pt_esc(paste(c(st[i], county[i])[nzchar(c(st[i], county[i]))], collapse = " / "))) else "",
      if (nzchar(begin[i])) paste0("<br/><b>Beginning:</b> ", pt_esc(begin[i])) else "",
      if (nzchar(end[i])) paste0("<br/><b>Ending:</b> ", pt_esc(end[i])) else "",
      if (nzchar(orv[i])) paste0("<br/><b>Outstandingly remarkable values:</b> ", pt_esc(orv[i])) else "",
      if (nzchar(river_id[i]) || nzchar(seg_id[i])) paste0("<br/><span style=\"font-size:11px;color:#666;\"><b>WSR QA IDs:</b> river/source ", pt_esc(river_id[i]), " · segment/source ", pt_esc(seg_id[i]), "</span>") else "",
      "</div>"
    )
    paste0(rows, collapse = "")
  }, character(1))
}

pt_make_wsr_corridor_popups <- function(x) {
  df <- sf::st_drop_geometry(x)
  name <- pt_wsr_first_good(df, c("AREANAME", "RIVER", "DESIGNATED", "NLCS_NAME", "CASENAME"), "Wild & Scenic River corridor")
  cls <- if ("wsr_class" %in% names(df)) df$wsr_class else pt_wsr_class(pt_wsr_first_good(df, c("WSR_CTGY", "CLASSIFICA", "AREATYPE"), ""))
  status <- if ("wsr_corridor_status" %in% names(df)) df$wsr_corridor_status else pt_wsr_boundary_or_corridor_status(pt_wsr_first_good(df, c("WSR_CTGY", "BOUNDARYST"), ""))
  source_label <- if ("wsr_source_label" %in% names(df)) df$wsr_source_label else rep("WSR corridor source", nrow(df))
  action <- if ("ACTIONDATE" %in% names(df)) pt_wsr_date_chr(df$ACTIONDATE) else rep("", nrow(df))
  official_ac <- if ("OFFICIALAC" %in% names(df)) pt_wsr_fmt_num(df$OFFICIALAC, 1) else rep("", nrow(df))
  boundary <- pt_wsr_first_good(df, c("BOUNDARYST"), "")
  comments <- pt_wsr_first_good(df, c("COMMENTS", "Comments_c"), "")
  local_case <- pt_wsr_first_good(df, c("LOCALCASEI"), "")
  areaid <- pt_wsr_first_good(df, c("AREAID", "SMA_ID"), "")
  id1 <- pt_wsr_first_good(df, c("WILDSCENIC", "NLCS_ID"), "")
  id2 <- pt_wsr_first_good(df, c("WILDSCEN_1", "SMA_ID"), "")

  vapply(seq_len(nrow(df)), function(i) {
    rows <- c(
      paste0("<div class=\"pt-popup\"><b>Wild & Scenic River corridor / boundary</b>"),
      paste0("<br/><b>Name:</b> ", pt_esc(name[i])),
      paste0("<br/><b>Classification:</b> ", pt_esc(cls[i])),
      paste0("<br/><b>Status:</b> ", pt_esc(status[i])),
      paste0("<br/><b>Source:</b> ", pt_esc(source_label[i])),
      if (nzchar(boundary[i]) && boundary[i] != status[i]) paste0("<br/><b>Boundary status:</b> ", pt_esc(boundary[i])) else "",
      if (nzchar(action[i])) paste0("<br/><b>Action / designation date:</b> ", pt_esc(action[i])) else "",
      if (nzchar(official_ac[i])) paste0("<br/><b>Official acres:</b> ", pt_esc(official_ac[i])) else "",
      if (nzchar(local_case[i])) paste0("<br/><b>Local case ID:</b> ", pt_esc(local_case[i])) else "",
      if (nzchar(comments[i])) paste0("<br/><b>Comments:</b> ", pt_esc(comments[i])) else "",
      if (nzchar(id1[i]) || nzchar(id2[i]) || nzchar(areaid[i])) paste0("<br/><span style=\"font-size:11px;color:#666;\"><b>WSR corridor QA IDs:</b> source ", pt_esc(id1[i]), " · related ", pt_esc(id2[i]), " · area ", pt_esc(areaid[i]), "</span>") else "",
      "</div>"
    )
    paste0(rows, collapse = "")
  }, character(1))
}

reference_layers_map <- list()

for (nm in names(reference_layers_raw)) {
  
  x <- reference_layers_raw[[nm]]
  
  keep_val <- unique(as.numeric(x$pt_simplify_keep))
  keep_val <- keep_val[!is.na(keep_val)]
  keep_val <- if (length(keep_val) == 0) 0.5 else keep_val[1]
  
  display_name <- as.character(x$pt_display_name[1])
  geom_type    <- tolower(as.character(x$pt_geom_type[1]))
  popup_spec   <- as.character(x$pt_popup_spec[1])
  color_field  <- as.character(x$pt_colorbycolumn[1])

  if (nm == "trails") {
    x <- pt_prepare_local_reference_trails(
      x,
      validate_snapshot = TRUE
    )
  } else if (nm == "monuments") {
    x <- pt_prepare_local_reference_national_monuments(
      x,
      validate_snapshot = TRUE
    )
  } else if (nm == "wildernessstudyarea") {
    x <- pt_prepare_local_reference_wsa(
      x,
      validate_snapshot = TRUE
    )
  } else if (nm == "fedwilderness") {
    x <- pt_prepare_local_reference_federal_wilderness(
      x,
      validate_snapshot = TRUE
    )
  } else if (nm == "acec") {
    x <- pt_prepare_local_reference_acec(
      x,
      validate_snapshot = TRUE
    )
  }
  
  if (!nm %in% c("monuments", "fedwilderness", "acec")) {
    x <- x |>
      simplify_sf_for_web(
        keep = keep_val,
        layer_label = display_name
      )
  }
  
  if (!nm %in% c("trails", "monuments", "wildernessstudyarea", "fedwilderness", "acec")) {
    x$popup_html <- pt_make_reference_layer_popups(
      x = x,
      popup_spec = popup_spec,
      display_name = display_name
    )
  }
  
  # ---- Special case 1: Wild & Scenic Rivers --------------------------------
  ##
  ## Normalize all local WSR line/corridor sources into shared display/filter
  ## fields while preserving source-specific layer names and popup attribution.
  if (nm %in% c("trails", "monuments", "wildernessstudyarea", "fedwilderness", "acec")) {

    ## Trails/National Monuments/WSA/Federal Wilderness/ACEC popup, hover,
    ## category, and style fields
    ## were prepared above
    ## from the shared Local Reference definitions. Do not pass them through
    ## generic palette logic or replace source-backed popup content after
    ## simplification.

  } else if (nm %in% pt_wsr_line_layers) {

    raw_class <- if ("CLASSIFICA" %in% names(x)) x$CLASSIFICA else if ("CATEGORY_c" %in% names(x)) x$CATEGORY_c else rep("", nrow(x))
    x$wsr_class <- pt_wsr_class(raw_class)
    x$wsr_status <- "Designated"
    x$wsr_corridor_status <- ""
    x$wsr_filter_kind <- "line"
    x$wsr_filter_class <- x$wsr_class
    x$wsr_source_label <- pt_wsr_source_label(nm)
    x$wsr_source_kind <- nm
    x$wsr_orv_supported <- nm == "wsr_segments"
    x$wsr_date_supported <- FALSE
    x$wsr_action_year <- NA_integer_

    x$wsr_orv_fish <- if ("ORV_FISH" %in% names(x)) pt_wsr_yes_flag(x$ORV_FISH) else FALSE
    x$wsr_orv_geologic <- if ("ORV_GEOLOG" %in% names(x)) pt_wsr_yes_flag(x$ORV_GEOLOG) else FALSE
    x$wsr_orv_recreation <- if ("ORV_RECREA" %in% names(x)) pt_wsr_yes_flag(x$ORV_RECREA) else FALSE
    x$wsr_orv_scenic <- if ("ORV_SCENIC" %in% names(x)) pt_wsr_yes_flag(x$ORV_SCENIC) else FALSE
    x$wsr_orv_cultural <- if ("ORV_CULTUR" %in% names(x)) pt_wsr_yes_flag(x$ORV_CULTUR) else FALSE
    x$wsr_orv_wildlife <- if ("ORV_WILDLI" %in% names(x)) pt_wsr_yes_flag(x$ORV_WILDLI) else FALSE
    x$wsr_orv_historic <- if ("ORV_HISTOR" %in% names(x)) pt_wsr_yes_flag(x$ORV_HISTOR) else FALSE
    x$wsr_orv_other <- if ("ORV_OTHER" %in% names(x)) pt_wsr_other_orv_flag(x$ORV_OTHER) else FALSE

    wsr_name <- pt_wsr_first_good(sf::st_drop_geometry(x), c("WSR_RIVER1", "NLCS_NAME", "GNIS_NAME", "WSR_RIVER_"), "WSR")
    x$pt_reference_label_text <- wsr_name
    x$pt_reference_hover_text <- paste0(wsr_name, " • ", x$wsr_class)

    x$line_col <- pt_wsr_class_color(x$wsr_class)
    x$fill_col <- x$line_col
    x$line_weight <- ifelse(nm == "wsr_blm_lines", 1.8, 2.2)
    x$line_dash <- ifelse(nm == "wsr_blm_lines", "4,3", "")
    x$fill_opacity <- 0
    x$popup_html <- pt_make_wsr_line_popups(x)

  } else if (nm %in% pt_wsr_corridor_layers) {

    raw_class <- if ("WSR_CTGY" %in% names(x)) {
      as.character(x$WSR_CTGY)
    } else if ("CLASSIFICA" %in% names(x)) {
      as.character(x$CLASSIFICA)
    } else if ("AREATYPE" %in% names(x)) {
      as.character(x$AREATYPE)
    } else {
      rep("", nrow(x))
    }

    raw_status <- if ("WSR_CTGY" %in% names(x)) {
      as.character(x$WSR_CTGY)
    } else if ("BOUNDARYST" %in% names(x)) {
      as.character(x$BOUNDARYST)
    } else {
      rep("", nrow(x))
    }

    x$wsr_class <- pt_wsr_class(raw_class)
    x$wsr_status <- pt_wsr_boundary_or_corridor_status(raw_status)
    x$wsr_corridor_status <- x$wsr_status
    x$wsr_filter_kind <- "corridor"
    x$wsr_filter_class <- x$wsr_class
    x$wsr_source_label <- pt_wsr_source_label(nm)
    x$wsr_source_kind <- nm
    x$wsr_orv_supported <- FALSE
    x$wsr_action_year <- if ("ACTIONDATE" %in% names(x)) pt_wsr_year_int(x$ACTIONDATE) else NA_integer_
    x$wsr_date_supported <- nm == "wsr_corridor_lsrs_status" & !is.na(x$wsr_action_year)

    x$wsr_orv_fish <- FALSE
    x$wsr_orv_geologic <- FALSE
    x$wsr_orv_recreation <- FALSE
    x$wsr_orv_scenic <- FALSE
    x$wsr_orv_cultural <- FALSE
    x$wsr_orv_wildlife <- FALSE
    x$wsr_orv_historic <- FALSE
    x$wsr_orv_other <- FALSE

    wsr_corr_name <- pt_wsr_first_good(sf::st_drop_geometry(x), c("AREANAME", "RIVER", "DESIGNATED", "NLCS_NAME", "CASENAME"), "WSR corridor")
    x$pt_reference_label_text <- wsr_corr_name
    x$pt_reference_hover_text <- paste0(wsr_corr_name, " • ", x$wsr_status, " ", x$wsr_class)

    x$line_col <- pt_wsr_class_color(x$wsr_class)
    x$fill_col <- x$line_col
    x$line_weight <- ifelse(nm == "wsr_corridor_lsrs_status", 1.6, 1.3)
    x$line_dash <- pt_wsr_status_dash(x$wsr_status)
    x$fill_opacity <- 0.045
    x$popup_html <- pt_make_wsr_corridor_popups(x)

    # ---- Special case 2: Federal Wilderness --------------------------------
    ##
    ## Color federal wilderness by managing agency using stable, intentional
    ## colors. Use broad string matching because agency values can vary by
    ## source, abbreviation, punctuation, or truncation.
  } else if (nm == "fedwilderness" && "ManagingAg" %in% names(x)) {
    
    ## Federal Wilderness ManagingAg numeric codes in this dataset:
    ##   4 = USFWS
    ##   5 = NPS
    ##   6 = BLM
    ##   8 = USFS
    ##
    ## Keep this numeric and explicit. The field is not agency-name text.
    
    managing_ag <- suppressWarnings(as.integer(x$ManagingAg))
    
    x$agency_group <- dplyr::case_when(
      managing_ag == 4L ~ "USFWS",
      managing_ag == 5L ~ "NPS",
      managing_ag == 6L ~ "BLM",
      managing_ag == 8L ~ "USFS",
      TRUE              ~ "Other / unknown"
    )
    
    x$line_col <- dplyr::case_when(
      x$agency_group == "BLM"          ~ "#B8860B",  # dark yellow / gold
      x$agency_group == "USFS"         ~ "#228B22",  # forest green
      x$agency_group == "NPS"          ~ "#54278F",  # purple
      x$agency_group == "USFWS"        ~ "#1F78B4",  # blue
      TRUE                             ~ "#737373"   # neutral gray
    )
    
    ## Make fill explicit too, in case the reference-layer drawing helper uses
    ## fill_col for polygon fill.
    x$fill_col <- x$line_col
    
    message("Federal Wilderness colors by numeric ManagingAg code:")
    print(
      tibble::tibble(
        ManagingAg = managing_ag,
        agency_group = x$agency_group,
        line_col = x$line_col
      ) |>
        dplyr::count(ManagingAg, agency_group, line_col, name = "n") |>
        dplyr::arrange(ManagingAg),
      n = Inf
    )
    
    
    # ---- Generic categorical color logic ------------------------------------
  } else if (!is.na(color_field) && color_field != "" && tolower(color_field) != "none") {
    
    vals <- as.character(x[[color_field]])
    uniq_vals <- sort(unique(vals[!is.na(vals) & vals != ""]))
    
    col_map <- setNames(
      rep(reference_palette, length.out = length(uniq_vals)),
      uniq_vals
    )
    
    x$line_col <- col_map[vals]
    x$line_col[is.na(x$line_col)] <- reference_default_col
    
    # ---- No color field: use one visible default ----------------------------
  } else {
    
    x$line_col <- reference_default_col
  }
  
  ## Keep polygon fill synchronized with line color unless a special case has
  ## already created fill_col.
  if (!"fill_col" %in% names(x)) {
    x$fill_col <- x$line_col
  }
  
  if (!"line_weight" %in% names(x)) {
    x$line_weight <- ifelse(geom_type == "polyline", 2.1, 1.5)
  }
  if (!"line_dash" %in% names(x)) {
    x$line_dash <- ""
  }
  if (!"fill_opacity" %in% names(x)) {
    x$fill_opacity <- ifelse(geom_type == "polygon", 0.005, 0)
  }

  reference_layers_map[[nm]] <- x
}

# ==== 8.x Add USGS recent-feed ring field ====================================

## Purpose:
## - Keep the main USGS well-circle fill dedicated to cached/recent groundwater-
##   level depth.
## - Keep the original USGS source status as raw metadata (`status`) only.
## - Add one explicit display field for the bright-green static-well ring.
##
## RF044 schema decision:
##   Do not create several overlapping "active" columns. The broad static
##   USGS Wells layer now uses a single display flag:
##
##     well_recent_feed_ring
##
##   Meaning:
##     TRUE  = the well is included in the Ops Live groundwater recent-feed
##             candidate list.
##     FALSE = the well is not currently in that recent-feed list.
##
##   The original/source `status` field remains in the cache and popup as
##   "USGS source status" because it is useful metadata, but it does not drive
##   the green ring.

message("Adding USGS recent-feed ring field...")

if ("well_in_ops_live" %in% names(usgs_gw_map)) {
  usgs_well_recent_feed_ring <- dplyr::coalesce(as.logical(usgs_gw_map$well_in_ops_live), FALSE)
} else {
  usgs_well_recent_feed_ring <- rep(FALSE, nrow(usgs_gw_map))
}

usgs_gw_map <- usgs_gw_map %>%
  dplyr::mutate(
    well_recent_feed_ring = usgs_well_recent_feed_ring
  )

message(
  "USGS wells flagged for bright-green outline because they are in the Ops Live groundwater recent feed: ",
  sum(usgs_gw_map$well_recent_feed_ring, na.rm = TRUE),
  " of ",
  nrow(usgs_gw_map)
)

# ==== 8.x Slim static USGS Wells cache for Leaflet ============================

## PURPOSE:
##   The static USGS Wells layer is intentionally large (~44k wells), so row
##   count must not be reduced.  However, after popup_html, hover_text, and
##   display styling fields have been built, many raw/source and temporary join
##   fields are no longer needed by Leaflet.
##
## DESIGN:
##   Preserve the complete user-facing popup/hover content, but keep only the
##   small display schema needed to draw the layer, QA duplicate IDs, and style
##   the recent-feed ring/nested cue.  This prevents large fields such as raw
##   param_list/measurements/latest-water-level join columns from being carried
##   into the map-ready cache and final standalone HTML.
##
## IMPORTANT:
##   This is a display-cache slimming step only. It does not alter
##   04_processed_data/rds/USGS_GW_final.rds or reduce the number of wells.

pt_slim_usgs_wells_for_leaflet <- function(x) {

  if (!inherits(x, "sf")) {
    return(x)
  }

  sf_col <- attr(x, "sf_column")
  if (
    is.null(sf_col) ||
    length(sf_col) != 1 ||
    is.na(sf_col) ||
    !sf_col %in% names(x)
  ) {
    geom_cols <- names(x)[
      vapply(x, inherits, logical(1), what = "sfc")
    ]
    sf_col <- if (length(geom_cols) > 0) geom_cols[1] else "geometry"
  }

  keep_cols <- c(
    ## QA/debug ID.  Popup/hover also contain the site number, but keeping the
    ## compact ID column helps duplicate-ID checks remain useful.
    "site_no",

    ## Leaflet hover payload.  Popups for this dense layer are built in the
    ## browser from compact gwpop_* fields rather than repeated popup_html.
    "hover_text",

    ## Compact browser-template popup fields.
    names(x)[grepl("^gwpop_", names(x))],

    ## Leaflet drawing/style fields.
    "well_fill_col",
    "well_stroke_col",
    "well_radius",
    "well_recent_feed_ring",
    "well_is_nested",

    ## Optional Local GW filter fields. These are populated when the Ops Live
    ## groundwater candidate index has been enriched by 48_update_usgs_gw_blm_distance_fields.R.
    "gw_on_blm_ca",
    "gw_dist_to_blm_mi",
    "gw_dist_to_blm_ft",
    "on_blm_ca",
    "dist_to_blm_mi",
    "dist_to_blm_ft"
  )

  keep_cols <- unique(c(intersect(keep_cols, names(x)), sf_col))
  drop_cols <- setdiff(names(x), keep_cols)

  before_mb <- as.numeric(utils::object.size(x)) / 1024^2

  if (length(drop_cols) > 0) {
    message(
      "Slimming static USGS Wells cache for Leaflet; dropping ",
      length(drop_cols),
      " non-display field(s): ",
      paste(drop_cols, collapse = ", ")
    )
  }

  x <- dplyr::select(x, dplyr::any_of(keep_cols))

  after_mb <- as.numeric(utils::object.size(x)) / 1024^2

  message(
    "Static USGS Wells display-cache object size: ",
    round(before_mb, 2),
    " MB -> ",
    round(after_mb, 2),
    " MB; rows retained: ",
    nrow(x)
  )

  x
}

usgs_gw_map <- pt_slim_usgs_wells_for_leaflet(usgs_gw_map)

# ==== 8.x Add CNRFC point hover text =========================================

## Purpose:
## - Add concise two-line mouse-hover labels for CNRFC stream and precip gages.
## - Line 1: NWS/CNRFC code - station/nickname description
## - Line 2: Elevation
##
## FIELD-NAME NOTE:
##   CNRFC preprocessed layers use compact fields such as nwsid, nickname,
##   channel, loc, station, and elev_ft.  Keep those names first in the
##   candidate lists so the late hover pass does not warn or degrade hover text.
##
## These fields are added to the map-ready cache objects before they are saved.

message("Adding CNRFC stream/precip hover text...")

## Small helper: return first existing field from an sf/data.frame object.
pt_first_existing_field <- function(x, candidates) {
  hit <- candidates[candidates %in% names(x)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

## Small helper: safely format elevation.
pt_format_elev <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  
  dplyr::case_when(
    is.na(x_num) ~ "Elevation: not available",
    TRUE ~ paste0("Elevation: ", format(round(x_num, 0), big.mark = ","), " ft")
  )
}

## ---- CNRFC stream gages hover ---------------------------------------------

if (exists("cnrfc_stream_map") && nrow(cnrfc_stream_map) > 0) {
  
  stream_code_field <- pt_first_existing_field(
    cnrfc_stream_map,
    c("nwsid", "NWSID", "nws_id", "NWS_ID", "code", "Code", "station_id", "stationID", "StationID", "id", "ID", "staid", "STAID")
  )
  
  stream_desc_field <- pt_first_existing_field(
    cnrfc_stream_map,
    c("nickname", "Nickname", "station", "Station", "loc", "Loc", "channel", "Channel", "Descript", "descript", "description", "Description", "name", "Name", "station_name", "StationName")
  )
  
  stream_elev_field <- pt_first_existing_field(
    cnrfc_stream_map,
    c("elev", "Elev", "elevation", "Elevation", "elev_ft", "ELEV_FT", "elevation_ft")
  )
  
  if (is.na(stream_code_field)) {
    warning("No CNRFC stream gage code field found. Using 'CNRFC stream gage' in hover text.")
    stream_code <- rep("CNRFC stream gage", nrow(cnrfc_stream_map))
  } else {
    stream_code <- as.character(cnrfc_stream_map[[stream_code_field]])
  }
  
  if (is.na(stream_desc_field)) {
    warning("No CNRFC stream gage description field found. Using blank description in hover text.")
    stream_desc <- rep("", nrow(cnrfc_stream_map))
  } else {
    stream_desc <- as.character(cnrfc_stream_map[[stream_desc_field]])
  }
  
  if (is.na(stream_elev_field)) {
    warning("No CNRFC stream gage elevation field found. Using 'not available' in hover text.")
    stream_elev <- rep(NA_real_, nrow(cnrfc_stream_map))
  } else {
    stream_elev <- cnrfc_stream_map[[stream_elev_field]]
  }
  
  cnrfc_stream_map <- cnrfc_stream_map %>%
    dplyr::mutate(
      hover_text = paste0(
        stream_code,
        dplyr::if_else(
          is.na(stream_desc) | trimws(stream_desc) == "",
          "",
          paste0(" - ", stream_desc)
        ),
        "<br>",
        pt_format_elev(stream_elev)
      )
    )
  
  message(
    "CNRFC stream hover fields used: code = ",
    ifelse(is.na(stream_code_field), "none", stream_code_field),
    "; description = ",
    ifelse(is.na(stream_desc_field), "none", stream_desc_field),
    "; elevation = ",
    ifelse(is.na(stream_elev_field), "none", stream_elev_field)
  )
}

## ---- CNRFC precip gages hover ---------------------------------------------

if (exists("cnrfc_precip_map") && nrow(cnrfc_precip_map) > 0) {
  
  precip_code_field <- pt_first_existing_field(
    cnrfc_precip_map,
    c("nwsid", "NWSID", "nws_id", "NWS_ID", "code", "Code", "station_id", "stationID", "StationID", "id", "ID", "staid", "STAID")
  )
  
  precip_desc_field <- pt_first_existing_field(
    cnrfc_precip_map,
    c("station", "Station", "nickname", "Nickname", "loc", "Loc", "channel", "Channel", "Descript", "descript", "description", "Description", "name", "Name", "station_name", "StationName")
  )
  
  precip_elev_field <- pt_first_existing_field(
    cnrfc_precip_map,
    c("elev", "Elev", "elevation", "Elevation", "elev_ft", "ELEV_FT", "elevation_ft")
  )
  
  if (is.na(precip_code_field)) {
    warning("No CNRFC precip gage code field found. Using 'CNRFC precip gage' in hover text.")
    precip_code <- rep("CNRFC precip gage", nrow(cnrfc_precip_map))
  } else {
    precip_code <- as.character(cnrfc_precip_map[[precip_code_field]])
  }
  
  if (is.na(precip_desc_field)) {
    warning("No CNRFC precip gage description field found. Using blank description in hover text.")
    precip_desc <- rep("", nrow(cnrfc_precip_map))
  } else {
    precip_desc <- as.character(cnrfc_precip_map[[precip_desc_field]])
  }
  
  if (is.na(precip_elev_field)) {
    warning("No CNRFC precip gage elevation field found. Using 'not available' in hover text.")
    precip_elev <- rep(NA_real_, nrow(cnrfc_precip_map))
  } else {
    precip_elev <- cnrfc_precip_map[[precip_elev_field]]
  }
  
  cnrfc_precip_map <- cnrfc_precip_map %>%
    dplyr::mutate(
      hover_text = paste0(
        precip_code,
        dplyr::if_else(
          is.na(precip_desc) | trimws(precip_desc) == "",
          "",
          paste0(" - ", precip_desc)
        ),
        "<br>",
        pt_format_elev(precip_elev)
      )
    )
  
  message(
    "CNRFC precip hover fields used: code = ",
    ifelse(is.na(precip_code_field), "none", precip_code_field),
    "; description = ",
    ifelse(is.na(precip_desc_field), "none", precip_desc_field),
    "; elevation = ",
    ifelse(is.na(precip_elev_field), "none", precip_elev_field)
  )
}

# ==== END ADD: CNRFC point hover text ========================================
