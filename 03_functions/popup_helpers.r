# ==== popup_helpers.r ========================================================
##
## PURPOSE:
##   Reusable popup builders for PortaTreasure2 map layers.
##
## DESIGN:
##   The main map script should not contain long popup-building logic.
##   Each layer type gets a small helper here.
##

# ==== 1. Formatting helpers ==================================================

pt_esc <- function(x) {
  htmltools::htmlEscape(as.character(x))
}

pt_fmt_area <- function(x) {
  v <- as.numeric(x)
  
  ifelse(
    is.na(v),
    "NA",
    ifelse(
      v >= 1000,
      sprintf("%.2f k mi²", v / 1000),
      sprintf("%.2f mi²", v)
    )
  )
}

pt_fmt_pct <- function(x) {
  v <- as.numeric(x)
  ifelse(is.na(v), "NA", sprintf("%.2f%%", v))
}

# ==== 2. HUC popup helper ====================================================

# ==== 2. HUC popup helper ====================================================

pt_make_huc_popups <- function(sfobj, lvl) {
  
  df <- sf::st_drop_geometry(sfobj)
  
  code_col <- paste0("huc", lvl)
  name_col <- paste0("huc", lvl, "_name")
  
  fmt_num <- function(x, digits = 1, suffix = "") {
    if (is.na(x)) return("NA")
    paste0(
      formatC(
        as.numeric(x),
        format = "f",
        digits = digits,
        big.mark = ","
      ),
      suffix
    )
  }
  
  has_climate <- all(
    c("map_in", "ppt_kaf", "rech_in", "rech_kaf", "rech_eff_pct") %in% names(df)
  )
  
  vapply(seq_len(nrow(df)), function(i) {
    
    # ---- Basic HUC information ---------------------------------------------
    
    parts <- c(
      sprintf("<b>HUC%d – %s</b>", lvl, pt_esc(df[[code_col]][i])),
      sprintf("<b>Name:</b> %s", pt_esc(df[[name_col]][i])),
      sprintf("<b>%%BLM-CA:</b> %s", pt_fmt_pct(df$percentBLMland[i])),
      sprintf("<b>Total area:</b> %s", pt_fmt_area(df$total_area_sqmi[i])),
      sprintf("<b>BLM-CA area:</b> %s", pt_fmt_area(df$blm_area_sqmi[i]))
    )
    
    # ---- Parent HUC information --------------------------------------------
    
    for (p in c(2, 4, 6, 8, 10)) {
      
      if (p >= lvl) next
      
      id_col  <- paste0("prnt_huc", p, "_code")
      nm_col  <- paste0("prnt_huc", p, "_name")
      pct_col <- paste0("prnt_huc", p, "_pctBLM")
      tot_col <- paste0("prnt_huc", p, "_tot")
      blm_col <- paste0("prnt_huc", p, "_blm")
      
      needed <- c(id_col, nm_col, pct_col, tot_col, blm_col)
      
      if (!all(needed %in% names(df))) next
      
      parent_id <- df[[id_col]][i]
      
      if (is.na(parent_id) || !nzchar(as.character(parent_id))) next
      
      parts <- c(
        parts,
        sprintf(
          "<b>Parent HUC%d:</b> %s (%s) – %s",
          p,
          pt_esc(df[[nm_col]][i]),
          pt_esc(parent_id),
          pt_fmt_pct(df[[pct_col]][i])
        ),
        sprintf(
          "(%s BLM / %s total)",
          pt_fmt_area(df[[blm_col]][i]),
          pt_fmt_area(df[[tot_col]][i])
        )
      )
    }
    
    # ---- PRISM / BCMv8 climate-recharge metrics -----------------------------
    ##
    ## These fields are only present after HUC10/HUC12 are joined to:
    ##
    ##   huc10_climate_recharge_table.rds
    ##   huc12_climate_recharge_table.rds
    ##
    ## Full citations are intentionally handled by a map-level "**" button so
    ## they are not repeated thousands of times in popup HTML.
    
    if (has_climate) {
      
      parts <- c(
        parts,
        "<hr style='margin:4px 0;'/>",
        "<b>1991–2020 climate / recharge summary</b>",
        "<span style='font-size:11px; color:#555;'>(Precip: PRISM 1991–2020 vM5; recharge: BCMv8 1991–2020)</span>",
        sprintf("<b>Mean annual precip:</b> %s", fmt_num(df$map_in[i], 1, " in/yr")),
        sprintf("<b>Precip volume:</b> %s", fmt_num(df$ppt_kaf[i], 1, " kaf/yr"))
      )
      
      recharge_note <- NA_character_
      
      if ("bcmv8_recharge_note" %in% names(df)) {
        recharge_note <- df$bcmv8_recharge_note[i]
      }
      
      if (!is.na(recharge_note) && nzchar(recharge_note)) {
        
        parts <- c(
          parts,
          sprintf(
            "<span style='font-size:11px; color:#8B0000;'><b>Recharge note:</b> %s</span>",
            pt_esc(recharge_note)
          )
        )
        
      } else {
        
        parts <- c(
          parts,
          sprintf("<b>Mean annual recharge:</b> %s", fmt_num(df$rech_in[i], 2, " in/yr")),
          sprintf("<b>Recharge volume:</b> %s", fmt_num(df$rech_kaf[i], 2, " kaf/yr")),
          sprintf("<b>Recharge efficiency:</b> %s", fmt_num(df$rech_eff_pct[i], 1, "%"))
        )
      }
      
      if (all(c("ppt_valid_frac", "rech_valid_frac") %in% names(df))) {
        parts <- c(
          parts,
          sprintf(
            "<b>Raster valid area:</b> precip %s | recharge %s",
            fmt_num(100 * df$ppt_valid_frac[i], 0, "%"),
            fmt_num(100 * df$rech_valid_frac[i], 0, "%")
          )
        )
      }
    }
    
    # ---- HUC8-only CW3E link ------------------------------------------------
    
    if (lvl == 8) {
      parts <- c(
        parts,
        "<a href='https://cw3e.ucsd.edu/Projects/QPF/QPF.html' target='_blank'>cw3e qpf</a>"
      )
    }
    
    # ---- Generic HUC search link --------------------------------------------
    
    parts <- c(
      parts,
      sprintf(
        "<a href='https://www.google.com/search?q=USGS+HUC+%s' target='_blank'>Google HUC search</a>",
        pt_esc(df[[code_col]][i])
      )
    )
    
    paste(parts, collapse = "<br/>")
    
  }, character(1))
}
# ==== 3. Groundwater / county popups =========================================

pt_make_gw_popups <- function(x) {
  sprintf(
    "<b>%s</b><br/>
     <b>Basin:</b> %s<br/>
     <b>%%BLM-CA:</b> %s<br/>
     <b>BLM:</b> %s | <b>Total:</b> %s<br/>
     <a href='https://www.google.com/search?q=California+Bulletin+118+%s' target='_blank'>Google Search</a>",
    pt_esc(x$label),
    pt_esc(x$basin_name),
    pt_fmt_pct(x$percentBLMland),
    pt_fmt_area(x$blm_area_sqmi),
    pt_fmt_area(x$total_area_sqmi),
    pt_esc(x$subbasin_num)
  )
}

pt_make_county_popups <- function(x) {
  sprintf(
    "<b>%s County</b><br/>
     <b>%%BLM-CA:</b> %s<br/>
     <b>BLM:</b> %s | <b>Total:</b> %s",
    pt_esc(x$county_name),
    pt_fmt_pct(x$percentBLMland),
    pt_fmt_area(x$blm_area_sqmi),
    pt_fmt_area(x$total_area_sqmi)
  )
}

# ==== 4. BLM popups and styling ==============================================

pt_make_blm_core_popups <- function(x) {
  paste0("<b>", pt_esc(x$category), "</b>")
}

pt_enrich_blm_diffs <- function(x) {
  
  x$category_base <- dplyr::case_when(
    grepl("un.?managed.*held", x$category, ignore.case = TRUE) ~ "Unmanaged but held",
    grepl("managed.*not.*held", x$category, ignore.case = TRUE) ~ "Managed but not held",
    TRUE ~ x$category
  )
  
  x$fill_col <- dplyr::case_when(
    x$category_base == "Unmanaged but held" ~ "#FF5EBE",
    x$category_base == "Managed but not held" ~ "#9CF69C",
    TRUE ~ "#F0F0F0"
  )
  
  x$stroke_col <- dplyr::case_when(
    x$category_base == "Unmanaged but held" ~ "#9B1B6A",
    x$category_base == "Managed but not held" ~ "#2E8B57",
    TRUE ~ "#333333"
  )
  
  x$popup_html <- dplyr::case_when(
    x$category_base == "Unmanaged but held" ~ paste0(
      "<b>Held but not managed by BLM</b><br/>",
      "These lands appear to be held/owned by BLM but not managed by BLM in the source dataset.<br/>",
      "<b>Source category:</b> ", pt_esc(x$category)
    ),
    x$category_base == "Managed but not held" ~ paste0(
      "<b>Managed but not held by BLM</b><br/>",
      "These lands appear to be managed by BLM but not held/owned by BLM in the source dataset.<br/>",
      "<b>Source category:</b> ", pt_esc(x$category)
    ),
    TRUE ~ paste0("<b>", pt_esc(x$category), "</b>")
  )
  
  x
}

# ==== 5. CNRFC point popups ==================================================
##
## River/reservoir point link logic:
##   CNRFC product availability can change by point and by time.  Therefore,
##   every CNRFC river/reservoir/special point gets the three core CNRFC links:
##     1. ensemble forecast
##     2. deterministic forecast
##     3. observed river data
##
##   Reservoir points also keep the reservoir-specific operational links:
##     - reservoir inflows
##     - reservoir outflows
##     - USACE data
##
## Precip gage link logic:
##   weather.gov/wrh time series

pt_popup_link <- function(label, url) {
  sprintf(
    "<a href='%s' target='_blank'>%s</a>",
    pt_esc(url),
    pt_esc(label)
  )
}

pt_val <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | x == "NA", "", x)
}

pt_df_val <- function(df, nm, i) {
  if (!nm %in% names(df)) return("")
  pt_val(df[[nm]][i])
}


pt_cnrfc_role_label <- function(gage_class1, gage_class2 = NA_character_, gage_type = NA_character_) {
  ## Return concise, user-facing CNRFC role labels.
  ##
  ## IMPORTANT:
  ##   CNRFC river/reservoir source rows are not all forecast points.  Raw
  ##   source rows commonly include combinations such as:
  ##     River + Forecast
  ##     River + Other
  ##     Reservoir + Reservoir
  ##     Reservoir + Forecast
  ##   Keep the role label derived from the class/type fields instead of
  ##   assuming every reservoir record is a forecast point.
  class1 <- tolower(trimws(pt_val(gage_class1)))
  class2 <- tolower(trimws(pt_val(gage_class2)))
  gtype  <- tolower(trimws(pt_val(gage_type)))

  out <- ifelse(
    class1 == "reservoir" & (class2 == "forecast" | grepl("\\bforecast\\b", gtype)),
    "Reservoir forecast",
    ifelse(
      class1 == "reservoir" & (class2 == "reservoir" | grepl("\\breservoir\\b", gtype)),
      "Reservoir",
      ifelse(
        class1 == "river" & (class2 == "forecast" | grepl("\\bforecast\\b", gtype)),
        "River forecast",
        ifelse(
          class1 == "river" & (class2 == "other" | grepl("\\bother\\b", gtype)),
          "River observation / other",
          ifelse(
            class1 == "special",
            "Special",
            "CNRFC point"
          )
        )
      )
    )
  )

  out[is.na(out) | out == ""] <- "CNRFC point"
  out
}



# ==== CDEC reservoir-station popup helper ====================================
##
## PURPOSE:
##   Build compact popups for the local CDEC Reservoir Stations layer.
##
## DESIGN:
##   This is a station/metadata layer only.  It intentionally does not present
##   current storage values.  Current or near-current storage should come from a
##   separate hosted/static GeoJSON feed later.
##
##   CDEC active-reservoir metadata remains the authoritative backbone.  Legacy
##   CDEC/CNRFC crosswalk fields, when present, are used only as portal/link
##   enrichment.

pt_make_cdec_reservoir_station_popups <- function(x) {

  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(character(0))
  }

  df <- sf::st_drop_geometry(x)

  vapply(seq_len(nrow(df)), function(i) {

    cdec_id <- pt_df_val(df, "cdec_id", i)
    reservoir_name <- pt_df_val(df, "reservoir_name", i)
    cdec_station_name <- pt_df_val(df, "cdec_station_name", i)
    county <- pt_df_val(df, "county", i)
    operator_agency <- pt_df_val(df, "operator_agency", i)
    river_basin_cdec <- pt_df_val(df, "river_basin_cdec", i)
    has_hourly <- tolower(pt_df_val(df, "has_hourly_reservoir_report", i)) %in% c("true", "t", "1", "yes")
    has_daily <- tolower(pt_df_val(df, "has_daily_reservoir_report", i)) %in% c("true", "t", "1", "yes")
    nws_id <- pt_df_val(df, "nws_id", i)
    alias_names <- pt_df_val(df, "alias_names", i)
    data_quality_note <- pt_df_val(df, "data_quality_note", i)
    cdec_station_url <- pt_df_val(df, "cdec_station_url", i)
    cdec_sensor15_hourly_url <- pt_df_val(df, "cdec_sensor15_hourly_url", i)
    cdec_sensor15_daily_url <- pt_df_val(df, "cdec_sensor15_daily_url", i)
    cdec_latest_storage_table_url <- pt_df_val(df, "cdec_latest_storage_table_url", i)

    title_name <- if (nzchar(reservoir_name)) reservoir_name else cdec_station_name
    if (!nzchar(title_name)) title_name <- "CDEC reservoir station"

    report_txt <- paste(
      c(
        if (has_hourly) "HourlyRes" else character(0),
        if (has_daily) "DailyRes" else character(0)
      ),
      collapse = "; "
    )
    if (!nzchar(report_txt)) report_txt <- "Not available"

    lines <- c(
      sprintf("<b>%s</b> – %s", pt_esc(cdec_id), pt_esc(title_name)),
      sprintf("<b>CDEC report:</b> %s", pt_esc(report_txt)),
      sprintf("<b>Operator:</b> %s", pt_esc(if (nzchar(operator_agency)) operator_agency else "Not available")),
      sprintf("<b>County:</b> %s", pt_esc(if (nzchar(county)) county else "Not available")),
      sprintf("<b>CDEC basin/group:</b> %s", pt_esc(if (nzchar(river_basin_cdec)) river_basin_cdec else "Not available"))
    )

    if (nzchar(nws_id)) {
      lines <- c(lines, sprintf("<b>CNRFC/NWS alias:</b> %s", pt_esc(nws_id)))
    }

    if (nzchar(alias_names) && !identical(alias_names, title_name)) {
      lines <- c(lines, sprintf("<b>Known aliases:</b> %s", pt_esc(alias_names)))
    }

    if (nzchar(data_quality_note)) {
      lines <- c(lines, sprintf("<b>Data note:</b> %s", pt_esc(data_quality_note)))
    }

    if (!nzchar(cdec_station_url) && nzchar(cdec_id)) {
      cdec_station_url <- paste0("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=", cdec_id)
    }

    links <- c(
      pt_popup_link(paste0("CDEC station (", cdec_id, ")"), cdec_station_url)
    )

    if (has_hourly && nzchar(cdec_sensor15_hourly_url)) {
      links <- c(links, pt_popup_link("CDEC storage sensor 15 hourly", cdec_sensor15_hourly_url))
    }

    if (has_daily && nzchar(cdec_sensor15_daily_url)) {
      links <- c(links, pt_popup_link("CDEC storage sensor 15 daily", cdec_sensor15_daily_url))
    }

    if (nzchar(cdec_latest_storage_table_url)) {
      links <- c(links, pt_popup_link("CDEC latest storage table", cdec_latest_storage_table_url))
    }

    ## Add CNRFC links only when there is a single unambiguous NWS/CNRFC ID.
    if (nzchar(nws_id) && !grepl(";", nws_id, fixed = TRUE)) {
      links <- c(
        links,
        pt_popup_link(
          paste0("CNRFC ensemble forecast (", nws_id, ")"),
          paste0("https://www.cnrfc.noaa.gov/ensembleProduct.php?id=", nws_id, "&prodID=3")
        ),
        pt_popup_link(
          "CNRFC deterministic forecast",
          paste0("https://www.cnrfc.noaa.gov/graphicalRVF.php?id=", nws_id)
        ),
        pt_popup_link(
          "CNRFC observed river data",
          paste0("https://www.cnrfc.noaa.gov/obsRiver_hc.php?id=", nws_id)
        )
      )
    }

    paste(c(lines, links), collapse = "<br/>")

  }, character(1))
}

pt_make_cnrfc_stream_popups <- function(x) {
  
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(character(0))
  }
  
  df <- sf::st_drop_geometry(x)
  
  vapply(seq_len(nrow(df)), function(i) {
    
    id <- pt_val(df$nwsid[i])
    nickname <- pt_val(df$nickname[i])
    gage_type_raw <- trimws(pt_val(df$gage_type[i]))
    gage_type <- tolower(gage_type_raw)
    gage_class1_raw <- trimws(pt_val(df$gage_class1[i]))
    gage_class1 <- tolower(gage_class1_raw)
    gage_class2_raw <- trimws(pt_val(df$gage_class2[i]))
    gage_class2 <- tolower(gage_class2_raw)
    elev_ft <- pt_val(df$elev_ft[i])

    ## Optional CDEC enrichment fields are joined from the CDEC-CNRFC reservoir
    ## crosswalk when available.  They are absent for most non-reservoir points
    ## and should never be required for popup construction.
    cdec_id <- pt_df_val(df, "cdec_id", i)
    cdec_station_name <- pt_df_val(df, "cdec_station_name", i)
    cdec_station_url <- pt_df_val(df, "cdec_station_url", i)
    cdec_sensor15_hourly_url <- pt_df_val(df, "cdec_sensor15_hourly_url", i)
    cdec_match_confidence <- pt_df_val(df, "cdec_cnrfc_match_confidence", i)
    cdec_match_distance_m <- pt_df_val(df, "cdec_cnrfc_match_distance_m", i)
    
    cnrfc_role <- pt_cnrfc_role_label(
      gage_class1 = gage_class1_raw,
      gage_class2 = gage_class2_raw,
      gage_type = gage_type_raw
    )
    
    raw_type_display <- if (nzchar(gage_type_raw)) {
      gage_type_raw
    } else {
      paste(
        c(gage_class1_raw, gage_class2_raw)[c(nzchar(gage_class1_raw), nzchar(gage_class2_raw))],
        collapse = ", "
      )
    }
    
    if (!nzchar(raw_type_display)) {
      raw_type_display <- "Not available"
    }
    
    elev_display <- if (nzchar(elev_ft)) {
      paste0(pt_esc(elev_ft), " ft")
    } else {
      "Not available"
    }
    
    title <- if (nzchar(nickname)) {
      sprintf("<b>%s</b> – %s", pt_esc(id), pt_esc(nickname))
    } else {
      sprintf("<b>%s</b>", pt_esc(id))
    }
    
    info_lines <- c(
      title,
      sprintf("<b>CNRFC role:</b> %s", pt_esc(cnrfc_role)),
      sprintf("<b>Raw CNRFC type:</b> %s", pt_esc(raw_type_display)),
      sprintf("<b>Elevation:</b> %s", elev_display)
    )

    if (nzchar(cdec_id)) {
      cdec_label <- if (nzchar(cdec_station_name)) {
        paste0(cdec_id, " – ", cdec_station_name)
      } else {
        cdec_id
      }

      match_note <- ""
      if (nzchar(cdec_match_confidence)) {
        match_note <- paste0("; match: ", cdec_match_confidence)
      }
      if (nzchar(cdec_match_distance_m)) {
        dist_num <- suppressWarnings(as.numeric(cdec_match_distance_m))
        if (!is.na(dist_num)) {
          match_note <- paste0(match_note, ", ", format(round(dist_num), big.mark = ","), " m")
        }
      }

      info_lines <- c(
        info_lines,
        sprintf("<b>CDEC cross-reference:</b> %s%s", pt_esc(cdec_label), pt_esc(match_note))
      )
    }
    
    ## Give every CNRFC river/reservoir point the same core CNRFC product
    ## links. Product availability can change over time, so it is better for
    ## users to have all three portals than to infer availability from the raw
    ## source type alone.
    link_lines <- c(
      pt_popup_link(
        "ensemble forecast",
        paste0(
          "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=",
          id,
          "&prodID=3"
        )
      ),
      pt_popup_link(
        "deterministic forecast",
        paste0("https://www.cnrfc.noaa.gov/graphicalRVF.php?id=", id)
      ),
      pt_popup_link(
        "observed river data",
        paste0("https://www.cnrfc.noaa.gov/obsRiver_hc.php?id=", id)
      )
    )

    ## Add CDEC portal links where the optional CDEC-CNRFC crosswalk provides a
    ## match.  Keep these separate from the standard CNRFC links so users can
    ## easily jump between agency portals for the same reservoir.
    if (nzchar(cdec_id)) {
      if (!nzchar(cdec_station_url)) {
        cdec_station_url <- paste0("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=", cdec_id)
      }

      link_lines <- c(
        link_lines,
        pt_popup_link(
          paste0("CDEC station (", cdec_id, ")"),
          cdec_station_url
        )
      )

      if (nzchar(cdec_sensor15_hourly_url)) {
        link_lines <- c(
          link_lines,
          pt_popup_link("CDEC storage sensor 15", cdec_sensor15_hourly_url)
        )
      }
    }

    ## Reservoir points keep their reservoir-specific operations links as
    ## additional context below the three standard CNRFC river/reservoir links.
    if (gage_class1 == "reservoir") {
      link_lines <- c(
        link_lines,
        pt_popup_link(
          "reservoir inflows",
          paste0("https://www.cnrfc.noaa.gov/reservoir.php?id=", id)
        ),
        pt_popup_link(
          "reservoir outflows",
          paste0("https://www.cnrfc.noaa.gov/reservoirRelease.php?id=", id)
        ),
        pt_popup_link(
          "USACE data",
          "https://www.spk-wc.usace.army.mil/plots/california.html"
        )
      )
    }
    
    paste(c(info_lines, link_lines), collapse = "<br/>")
    
  }, character(1))
}

pt_make_cnrfc_precip_popups <- function(x) {
  
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(character(0))
  }
  
  df <- sf::st_drop_geometry(x)
  
  vapply(seq_len(nrow(df)), function(i) {
    
    id <- pt_val(df$nwsid[i])
    station <- pt_val(df$station[i])
    lat <- pt_val(df$lat[i])
    lon <- pt_val(df$lon[i])
    elev_ft <- pt_val(df$elev_ft[i])
    transmit <- pt_val(df$datatransmission[i])
    state <- pt_val(df$state[i])
    
    title <- if (nzchar(station)) {
      sprintf("<b>%s</b> – %s", pt_esc(id), pt_esc(station))
    } else {
      sprintf("<b>%s</b>", pt_esc(id))
    }
    
    lines <- c(
      title,
      sprintf("<b>Transmission:</b> %s", pt_esc(transmit)),
      sprintf("<b>Latitude:</b> %s", pt_esc(lat)),
      sprintf("<b>Longitude:</b> %s", pt_esc(lon)),
      sprintf("<b>Elevation:</b> %s ft", pt_esc(elev_ft)),
      sprintf("<b>State:</b> %s", pt_esc(state)),
      pt_popup_link(
        "weather.gov time series",
        paste0("https://www.weather.gov/wrh/timeseries?site=", id)
      ),
      pt_popup_link(
        "Google search",
        paste0("https://www.google.com/search?q=NWS+", id)
      )
    )
    
    paste(lines, collapse = "<br/>")
    
  }, character(1))
}


# ==== 6. USGS point popups ===================================================
##
## These popups are intentionally informative but compact.
## They preserve record count and site status so users can distinguish
## active sites, inactive sites, and wells with limited measurements.

pt_na_blank <- function(x) {
  x <- as.character(x)
  ifelse(is.na(x) | x == "NA", "", x)
}

# ==== USGS streamgage popup helper ===========================================
##
## PURPOSE:
##   Keep USGS streamgage popups compact.
##
## DESIGN:
##   The raw NWIS parameter/measurement lists can be extremely long, so this
##   popup summarizes common parameter codes instead of printing every code.

pt_summarize_usgs_params <- function(param_list) {
  
  param_list <- as.character(param_list)
  param_list[is.na(param_list)] <- ""
  
  vapply(param_list, function(x) {
    
    hits <- character(0)
    
    if (grepl("00060", x)) hits <- c(hits, "discharge")
    if (grepl("00065", x)) hits <- c(hits, "gage height")
    if (grepl("00010", x)) hits <- c(hits, "water temp")
    if (grepl("00020", x)) hits <- c(hits, "air temp")
    if (grepl("00045", x)) hits <- c(hits, "precip")
    if (grepl("00095", x)) hits <- c(hits, "specific conductance")
    if (grepl("00400", x)) hits <- c(hits, "pH")
    if (grepl("00300", x)) hits <- c(hits, "dissolved oxygen")
    if (grepl("63680", x)) hits <- c(hits, "turbidity")
    
    hits <- unique(hits)
    
    if (length(hits) == 0) {
      return("see USGS site")
    }
    
    paste(hits, collapse = "; ")
    
  }, character(1))
}

### usgs well popups

pt_make_usgs_stream_popups <- function(x) {
  
  common_params <- pt_summarize_usgs_params(x$param_list)
  
  sprintf(
    "<b>%s</b> – %s<br/>
     <b>Elevation:</b> %s ft<br/>
     <b>Type:</b> %s<br/>
     <b>Status:</b> %s<br/>
     <b>Record count:</b> %s<br/>
     <b>Period:</b> %s to %s<br/>
     <b>Common data:</b> %s<br/>
     <span style='font-size:11px; color:#555;'>Additional parameters may be available from USGS.</span><br/>
     <a href='https://waterdata.usgs.gov/monitoring-location/%s' target='_blank'>USGS Site</a><br/>
     <a href='https://dashboard.waterdata.usgs.gov/app/nwd/en/' target='_blank'>usgs dash</a><br/>
     <a href='https://water.noaa.gov/' target='_blank'>noaa nwm</a>",
    pt_esc(x$site_no),
    pt_esc(x$name),
    pt_esc(x$elev_ft),
    pt_esc(x$site_type),
    pt_esc(x$status),
    pt_esc(x$count_nu),
    pt_esc(x$start_date),
    pt_esc(x$end_date),
    pt_esc(common_params),
    pt_esc(x$site_no)
  )
}

# ==== USGS groundwater well popup helper =====================================
##
## PURPOSE:
##   Keep USGS groundwater well popups compact.
##
## DESIGN:
##   Raw NWIS parameter/measurement lists can be extremely long. This helper
##   summarizes common / recognizable data types instead of printing every code.

pt_summarize_usgs_well_params <- function(param_list) {
  
  param_list <- as.character(param_list)
  param_list[is.na(param_list)] <- ""
  
  vapply(param_list, function(x) {
    
    hits <- character(0)
    
    ## Common groundwater / water-level fields.
    if (grepl("72019|72020|62610|62611", x)) hits <- c(hits, "water level")
    
    ## Common field water-quality parameters.
    if (grepl("00010", x)) hits <- c(hits, "water temp")
    if (grepl("00095", x)) hits <- c(hits, "specific conductance")
    if (grepl("00400", x)) hits <- c(hits, "pH")
    if (grepl("00300", x)) hits <- c(hits, "dissolved oxygen")
    
    ## Common chemistry parameters.
    if (grepl("00618|00631", x)) hits <- c(hits, "nitrate")
    if (grepl("00940", x)) hits <- c(hits, "chloride")
    if (grepl("00945", x)) hits <- c(hits, "sulfate")
    if (grepl("70300|70301", x)) hits <- c(hits, "dissolved solids")
    if (grepl("01002", x)) hits <- c(hits, "arsenic")
    if (grepl("01020", x)) hits <- c(hits, "boron")
    
    hits <- unique(hits)
    
    if (length(hits) == 0) {
      return("see USGS site")
    }
    
    paste(hits, collapse = "; ")
    
  }, character(1))
}

pt_make_usgs_well_popups <- function(x) {
  
  ## Small helper so the popup does not fail if a field is missing.
  get_col <- function(df, nm, default = NA_character_) {
    if (nm %in% names(df)) return(df[[nm]])
    rep(default, nrow(df))
  }

  get_bool <- function(df, nm) {
    if (!nm %in% names(df)) return(rep(FALSE, nrow(df)))
    val <- df[[nm]]
    if (is.logical(val)) return(dplyr::coalesce(val, FALSE))
    txt <- tolower(trimws(as.character(val)))
    txt %in% c("true", "t", "1", "yes", "y")
  }
  
  common_params <- if ("param_list" %in% names(x)) {
    pt_summarize_usgs_well_params(x$param_list)
  } else {
    rep("see USGS site", nrow(x))
  }
  
  ## Optional most-recent groundwater-level fields.
  ## RF043b allows the static layer to use newer Ops Live values where a static
  ## well is present in the Ops Live feed. The old static water-level lookup is
  ## still used for wells not present in Ops Live.
  latest_wl <- suppressWarnings(
    as.numeric(get_col(x, "latest_wl_ft_bgs", NA_real_))
  )
  
  latest_dt <- as.character(
    get_col(x, "latest_wl_date", NA_character_)
  )
  
  latest_run <- as.character(
    get_col(x, "latest_wl_run_ts", NA_character_)
  )

  nested_n <- suppressWarnings(as.integer(
    get_col(x, "well_coord_group_count", 1L)
  ))

  in_ops <- get_bool(x, "well_in_ops_live")
  synced_ops <- get_bool(x, "well_latest_synced_from_ops_live")

  source_status <- as.character(get_col(x, "status", NA_character_))
  source_status <- trimws(source_status)
  source_status[is.na(source_status) | source_status == ""] <- "Status unknown"

  nested_line <- ifelse(
    !is.na(nested_n) & nested_n > 1L,
    paste0(
      "<b>Co-located/nested group:</b> ",
      nested_n,
      " USGS well records share this mapped coordinate.<br/>"
    ),
    ""
  )

  ops_line <- ifelse(
    in_ops,
    "<b>Recent feed:</b> included in Ops Live groundwater layer; green ring indicates recent-feed membership.<br/>",
    "<b>Recent feed:</b> not currently included in Ops Live groundwater layer.<br/>"
  )

  source_note <- ifelse(
    synced_ops,
    "Source: Ops Live groundwater feed (USGS field measurements).",
    paste0(
      "Source: static cached USGS lookup",
      ifelse(!is.na(latest_run) & latest_run != "", paste0("; lookup run ", latest_run), ""),
      "."
    )
  )
  
  wl_line <- ifelse(
    !is.na(latest_wl),
    paste0(
      "<b>Most recent groundwater level:</b> ",
      formatC(latest_wl, format = "f", digits = 1, big.mark = ","),
      " ft bgs",
      ifelse(!is.na(latest_dt) & latest_dt != "", paste0(" on ", latest_dt), ""),
      "<br/><span style='font-size:11px; color:#555;'>",
      source_note,
      "</span><br/>"
    ),
    ""
  )
  
  sprintf(
    paste0(
      "<b>%s</b> – %s<br/>",
      "<b>Elevation:</b> %s ft<br/>",
      "<b>Type:</b> %s<br/>",
      "<b>USGS source status:</b> %s<br/>",
      "<b>Record count:</b> %s<br/>",
      "%s",
      "%s",
      "%s",
      "<b>Well depth:</b> %s ft<br/>",
      "<b>Hole depth:</b> %s ft<br/>",
      "<b>Aquifer:</b> %s<br/>",
      "<b>Aquifer type:</b> %s<br/>",
      "<b>National aquifer:</b> %s<br/>",
      "<b>Period:</b> %s to %s<br/>",
      "<b>Common data:</b> %s<br/>",
      "<span style='font-size:11px; color:#555;'>Additional parameters may be available from USGS.</span><br/>",
      "<a href='https://waterdata.usgs.gov/monitoring-location/%s' target='_blank'>USGS Site</a>"
    ),
    pt_esc(get_col(x, "site_no")),
    pt_esc(get_col(x, "name")),
    pt_esc(get_col(x, "elev_ft")),
    pt_esc(get_col(x, "site_type")),
    pt_esc(source_status),
    pt_esc(get_col(x, "count_nu")),
    nested_line,
    ops_line,
    wl_line,
    pt_esc(get_col(x, "well_depth_ft")),
    pt_esc(get_col(x, "hole_depth_ft")),
    pt_esc(get_col(x, "aquifer_cd")),
    pt_esc(get_col(x, "aquifer_type_cd")),
    pt_esc(get_col(x, "nat_aqfr_cd")),
    pt_esc(get_col(x, "start_date")),
    pt_esc(get_col(x, "end_date")),
    pt_esc(common_params),
    pt_esc(get_col(x, "site_no"))
  )
}


# ==== 7. BLM offices and project-area popups =================================
##
## PURPOSE:
##   Popup builders for BLM office points and project-area polygons.
##
## NOTES:
##   Project areas may be empty for now. The helper functions return character(0)
##   cleanly when no features exist.

pt_blm_state_office_display_name <- function(x) {
  
  x_chr <- trimws(as.character(x))
  x_low <- tolower(x_chr)
  
  ifelse(
    is.na(x_chr) | !nzchar(x_chr),
    "",
    ifelse(
      x_low == "california state office",
      "California State Office (CASO)",
      x_chr
    )
  )
}

pt_make_blm_office_map_links <- function(lat, lon, label = "BLM office") {
  
  lat_chr <- as.character(lat)
  lon_chr <- as.character(lon)
  label_chr <- ifelse(is.na(label) | !nzchar(as.character(label)), "BLM office", as.character(label))
  
  google_url <- sprintf(
    "https://www.google.com/maps?q=%s,%s",
    lat_chr,
    lon_chr
  )
  
  bing_label <- utils::URLencode(label_chr, reserved = TRUE)
  
  bing_url <- sprintf(
    paste0(
      "https://www.bing.com/maps/default.aspx?",
      "cp=%s~%s&lvl=16&style=r&sp=point.%s_%s_%s"
    ),
    lat_chr,
    lon_chr,
    lat_chr,
    lon_chr,
    bing_label
  )
  
  osm_url <- sprintf(
    "https://www.openstreetmap.org/?mlat=%s&mlon=%s#map=14/%s/%s",
    lat_chr,
    lon_chr,
    lat_chr,
    lon_chr
  )
  
  paste(
    pt_popup_link("Google", google_url),
    pt_popup_link("Bing", bing_url),
    pt_popup_link("OpenStreetMap", osm_url),
    sep = " &middot; "
  )
}

pt_make_blm_office_popups <- function(x) {
  
  office_title <- pt_blm_state_office_display_name(x$offce_name)
  office_title[is.na(office_title) | !nzchar(office_title)] <- as.character(x$offce_name[
    is.na(office_title) | !nzchar(office_title)
  ])
  
  map_links <- pt_make_blm_office_map_links(
    lat = x$lat,
    lon = x$lon,
    label = office_title
  )
  
  sprintf(
    paste0(
      "<b>%s</b><br/>",
      "<b>Type:</b> %s<br/>",
      "<b>Map:</b> %s"
    ),
    pt_esc(office_title),
    pt_esc(x$office_type_label),
    map_links
  )
}

pt_make_project_area_popups <- function(x) {
  
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(character(0))
  }
  
  df <- sf::st_drop_geometry(x)
  
  ## Avoid showing an old popup_html field if this function is rerun.
  df <- df[, setdiff(names(df), "popup_html"), drop = FALSE]
  
  vapply(seq_len(nrow(df)), function(i) {
    
    row <- df[i, , drop = FALSE]
    
    title <- if ("project_name" %in% names(row)) {
      as.character(row$project_name)
    } else if ("project_id" %in% names(row)) {
      paste0("Project area ", as.character(row$project_id))
    } else {
      paste0("Project area ", i)
    }
    
    table_rows <- vapply(names(row), function(nm) {
      
      val <- row[[nm]][1]
      
      if (is.list(val)) {
        val <- paste(unlist(val), collapse = ", ")
      }
      
      val <- ifelse(is.na(val), "", as.character(val))
      
      sprintf(
        "<tr><th style='text-align:left;padding-right:8px;'>%s</th><td>%s</td></tr>",
        pt_esc(nm),
        pt_esc(val)
      )
      
    }, character(1))
    
    paste0(
      "<div style='max-width:420px;overflow:auto;'>",
      "<b>", pt_esc(title), "</b><br/>",
      "<table style='font-size:12px;border-collapse:collapse;'>",
      paste(table_rows, collapse = "\n"),
      "</table></div>"
    )
    
  }, character(1))
}

# ==== 8. CNRFC basin and field-office boundary popups ========================
##
## Keep these popups compact to avoid bloating the final standalone HTML.

pt_make_cnrfc_basin_popups <- function(x) {
  
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(character(0))
  }
  
  sprintf(
    paste0(
      "<b>CNRFC Basin</b><br/>",
      "%s<br/>",
      "%s<br/>",
      "<b>Forecast group:</b> %s<br/>",
      "%s<br/>",
      "%s"
    ),
    pt_esc(x$Basin),
    pt_esc(x$Descript),
    pt_esc(x$ForecastGr),
    pt_popup_link(
      "water supply fcast",
      paste0(
        "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=",
        x$Basin,
        "&prodID=9"
      )
    ),
    pt_popup_link(
      "basin mean temp fcast",
      paste0(
        "https://www.cnrfc.noaa.gov/temperaturePlots_hc.php?id=",
        x$Basin
      )
    )
  )
}

pt_blm_strip_field_office_suffix <- function(x) {
  
  x_chr <- trimws(as.character(x))
  x_chr <- gsub("\\s+Field Office$", "", x_chr, ignore.case = TRUE)
  x_chr <- gsub("\\s+Office$", "", x_chr, ignore.case = TRUE)
  x_chr
}

pt_blm_field_office_code <- function(x) {
  
  x_short <- pt_blm_strip_field_office_suffix(x)
  key <- tolower(trimws(x_short))
  key <- gsub("\\s+", " ", key)
  
  code_lookup <- c(
    "applegate" = "ALFO",
    "arcata" = "ARFO",
    "bakersfield" = "BKFO",
    "barstow" = "BAFO",
    "bishop" = "BIFO",
    "central coast" = "CCFO",
    "eagle lake" = "ELFO",
    "el centro" = "ECFO",
    "mother lode" = "MLFO",
    "needles" = "NEFO",
    "palm springs/s. coast" = "PSFO",
    "palm springs/south coast" = "PSFO",
    "palm springs-south coast" = "PSFO",
    "palm springs - south coast" = "PSFO",
    "palm springs south coast" = "PSFO",
    "redding" = "REFO",
    "ridgecrest" = "RIFO",
    "ukiah" = "UKFO"
  )
  
  out <- unname(code_lookup[key])
  out[is.na(out)] <- ""
  out
}

pt_blm_field_office_hover_label <- function(x) {
  
  short_name <- pt_blm_strip_field_office_suffix(x)
  code <- pt_blm_field_office_code(x)
  
  ifelse(
    is.na(short_name) | !nzchar(short_name),
    "BLM Field Office",
    ifelse(
      !is.na(code) & nzchar(code),
      paste0(short_name, " \u2022 ", code),
      short_name
    )
  )
}

pt_make_field_office_outer_hover <- function(x) {
  
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(character(0))
  }
  
  pt_blm_field_office_hover_label(x$ADMU_NAME)
}

pt_make_blm_office_hover <- function(x) {
  
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(character(0))
  }
  
  office_type <- tolower(as.character(x$offce_type))
  name <- as.character(x$offce_name)
  
  out <- ifelse(
    office_type == "fo",
    pt_blm_field_office_hover_label(name),
    ifelse(
      office_type == "do",
      pt_blm_district_display_name(gsub("\\s+Office$", "", name, ignore.case = TRUE)),
      pt_blm_state_office_display_name(name)
    )
  )
  
  out[is.na(out) | !nzchar(out)] <- "BLM office"
  out
}

pt_blm_district_display_name <- function(x) {
  
  x_chr <- trimws(as.character(x))
  x_low <- tolower(x_chr)
  
  suffix <- ifelse(
    x_low == "central california district", " (CCD)",
    ifelse(
      x_low == "northern california district", " (NCD)",
      ifelse(
        x_low == "california desert district", " (CDD)",
        ""
      )
    )
  )
  
  ifelse(
    is.na(x) | !nzchar(x_chr),
    "",
    paste0(x_chr, suffix)
  )
}

pt_make_field_office_outer_popups <- function(x) {
  
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(character(0))
  }
  
  district_name <- pt_blm_district_display_name(x$PARENT_NAM)
  
  sprintf(
    paste0(
      "<b>BLM Field Office</b><br/>",
      "%s<br/>",
      "%s"
    ),
    pt_esc(x$ADMU_NAME),
    pt_esc(district_name)
  )
}
# ==== 9. CalSim3 arc popups ==================================================
##
## Compact popup for CalSim3 model arcs.
##
## Format:
##   Name
##   Arc: Arc_ID
##   FromNode to ToNode

pt_make_calsim3_arc_popups <- function(x) {
  
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(character(0))
  }
  
  sprintf(
    paste0(
      "<b>%s</b><br/>",
      "Arc: %s<br/>",
      "%s to %s<br/>",
      "<b>Type:</b> %s"
    ),
    pt_esc(x$Name),
    pt_esc(x$Arc_ID),
    pt_esc(x$FromNode),
    pt_esc(x$ToNode),
    pt_esc(x$Type)
  )
}

# ==== 9A. CalSim3 node popups ================================================
##
## Compact popup for CalSim3 model nodes.
##
## The first row uses CalSim3_ID, stored as node_id_display by the CalSim3
## preprocessor.  Riv_Name and Comment are included when present.
## The CalSim3 documentation link is repeated in each popup for convenience.

pt_make_calsim3_node_popups <- function(x) {

  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(character(0))
  }

  df <- sf::st_drop_geometry(x)

  node_id <- if ("node_id_display" %in% names(df)) {
    pt_val(df$node_id_display)
  } else if ("CalSim3_ID" %in% names(df)) {
    pt_val(df$CalSim3_ID)
  } else {
    rep("", nrow(df))
  }

  node_group <- if ("calsim3_node_group" %in% names(df)) {
    pt_val(df$calsim3_node_group)
  } else {
    rep("", nrow(df))
  }

  node_desc <- if ("node_description" %in% names(df)) {
    pt_val(df$node_description)
  } else if ("NodeDescri" %in% names(df)) {
    pt_val(df$NodeDescri)
  } else {
    rep("", nrow(df))
  }

  riv_name <- if ("riv_name_display" %in% names(df)) {
    pt_val(df$riv_name_display)
  } else if ("Riv_Name" %in% names(df)) {
    pt_val(df$Riv_Name)
  } else {
    rep("", nrow(df))
  }

  comment <- if ("comment_display" %in% names(df)) {
    pt_val(df$comment_display)
  } else if ("Comment" %in% names(df)) {
    pt_val(df$Comment)
  } else {
    rep("", nrow(df))
  }

  node_id[!nzchar(node_id)] <- "CalSim3 node"
  node_group[!nzchar(node_group)] <- "Unknown / Other"
  node_desc[!nzchar(node_desc)] <- "Not available"

  riv_line <- ifelse(
    nzchar(riv_name),
    paste0("<br/><b>River:</b> ", pt_esc(riv_name)),
    ""
  )

  comment_line <- ifelse(
    nzchar(comment),
    paste0("<br/><b>Comment:</b> ", pt_esc(comment)),
    ""
  )

  paste0(
    "<b>", pt_esc(node_id), "</b><br/>",
    "<b>Node group:</b> ", pt_esc(node_group), "<br/>",
    "<b>Node description:</b> ", pt_esc(node_desc),
    riv_line,
    comment_line,
    "<br/><a href='https://data.cnra.ca.gov/dataset/calsim-3' target='_blank'>CalSim3 documentation</a>"
  )
}

# ==== 10. Generic reference-layer popups =====================================
##
## PURPOSE:
##   Build compact popups for manifest-driven reference layers.
##
## POPUP SPEC:
##   The manifest popup field can use literal \n to indicate multiple fields.
##   Example:
##     ACEC_NAME\nLUP_NAME
##
## DISPLAY RULE:
##   - first popup field is shown in bold as the title
##   - subsequent fields are shown on separate lines with field labels
##   - if popup spec is empty/none, the layer display name is used
##
## IMPORTANT:
##   This block is self-contained. It does not rely on helper functions defined
##   only in the batch preprocessor.

pt_ref_manifest_none <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "na", "none", "None", "nonw")] <- NA_character_
  x
}

pt_ref_parse_popup_fields <- function(spec) {
  
  spec <- pt_ref_manifest_none(spec)
  
  if (length(spec) == 0 || is.na(spec)) {
    return(character(0))
  }
  
  out <- unlist(strsplit(spec, "\\\\n"))
  out <- trimws(out)
  out <- out[out != ""]
  out
}

pt_make_reference_layer_popups <- function(x, popup_spec, display_name = "Reference Layer") {
  
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(character(0))
  }
  
  popup_spec <- pt_ref_manifest_none(popup_spec)
  popup_fields <- pt_ref_parse_popup_fields(popup_spec)
  
  if (length(popup_fields) == 0) {
    return(rep(sprintf("<b>%s</b>", pt_esc(display_name)), nrow(x)))
  }
  
  df <- sf::st_drop_geometry(x)
  
  vapply(seq_len(nrow(df)), function(i) {
    
    row <- df[i, , drop = FALSE]
    
    first_field <- popup_fields[1]
    first_val <- if (first_field %in% names(row)) as.character(row[[first_field]][1]) else ""
    first_val <- ifelse(is.na(first_val), "", first_val)
    
    lines <- c(sprintf("<b>%s</b>", pt_esc(first_val)))
    
    if (length(popup_fields) > 1) {
      for (fld in popup_fields[-1]) {
        
        if (!fld %in% names(row)) next
        
        val <- as.character(row[[fld]][1])
        val <- ifelse(is.na(val), "", val)
        
        lines <- c(
          lines,
          sprintf("<b>%s:</b> %s", pt_esc(fld), pt_esc(val))
        )
      }
    }
    
    paste(lines, collapse = "<br/>")
    
  }, character(1))
}

# ==== 11. Major conveyance popups ============================================
##
## PURPOSE:
##   Compact popup for major conveyance linework.
##
## NOTES:
##   The source layer is dissolved by Pname + Operator in:
##
##     02_preprocess/12_major_conveyance.r
##
##   segment_count records how many original undissolved line features were
##   combined into each map feature.

pt_make_major_conveyance_popups <- function(x) {
  
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(character(0))
  }
  
  sprintf(
    paste0(
      "<b>%s</b><br/>",
      "<b>Operator:</b> %s<br/>",
      "<b>Operator group:</b> %s<br/>",
      "<b>Source segments:</b> %s"
    ),
    pt_esc(x$Pname),
    pt_esc(x$Operator),
    pt_esc(x$operator_group),
    pt_esc(x$segment_count)
  )
}

# ==== 12. New static water / Delta layer popups ==============================
##
## PURPOSE:
##   Popup builders for newly added lightweight static layers that are not part
##   of the generic reference-layer manifest because they need custom source
##   links, custom hover text, or special symbology.

pt_popup_na <- function(x, fallback = "Not available") {
  x <- as.character(x)
  x <- trimws(x)
  x[is.na(x) | x == "" | x == "NA"] <- fallback
  x
}

pt_format_x2_km <- function(x) {
  
  v <- suppressWarnings(as.numeric(x))
  out <- rep("Not available", length(v))
  
  whole <- !is.na(v) & abs(v - round(v)) < 0.000001
  out[whole] <- formatC(v[whole], format = "f", digits = 0, big.mark = ",")
  out[!whole & !is.na(v)] <- formatC(v[!whole & !is.na(v)], format = "f", digits = 1, big.mark = ",")
  
  out
}

pt_make_cnrfc_fnf_delta_popups <- function(x) {
  
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(character(0))
  }
  
  river <- pt_popup_na(x$River)
  nws5id <- pt_popup_na(x$nws5id)
  res <- pt_popup_na(x$res)
  
  cnrfc_url <- paste0(
    "https://www.cnrfc.noaa.gov/ensembleProduct.php?id=",
    utils::URLencode(nws5id, reserved = TRUE),
    "&prodID=9"
  )
  
  dwr_url <- "https://cdec.water.ca.gov/reportapp/javareports?name=FNF"
  
  paste0(
    "<b>", pt_esc(river), "</b><br/>",
    "<b>NWS ID:</b> ", pt_esc(nws5id), "<br/>",
    "<b>Reservoir:</b> ", pt_esc(res), "<br/>",
    "<hr style='margin:4px 0;'/>",
    "<a href='", pt_esc(cnrfc_url), "' target='_blank'>FNF, per CNRFC</a><br/>",
    "<a href='", pt_esc(dwr_url), "' target='_blank'>FNF, per DWR</a>"
  )
}

pt_make_x2_km_popups <- function(x) {
  
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(character(0))
  }
  
  x2 <- pt_format_x2_km(x$RKI)
  
  water_ops <- paste0(
    "https://water.ca.gov/-/media/DWR-Website/Web-Pages/Programs/State-Water-Project/",
    "Operations-And-Maintenance/Files/Operations-Control-Office/Delta-Status-And-Operations/",
    "Delta-Operations-Daily-Summary.pdf"
  )
  
  water_quality <- paste0(
    "https://water.ca.gov/-/media/DWR-Website/Web-Pages/Programs/State-Water-Project/",
    "Operations-And-Maintenance/Files/Operations-Control-Office/Delta-Status-And-Operations/",
    "Delta-Water-Quality-Daily-Summary.pdf"
  )
  
  hydrology <- paste0(
    "https://water.ca.gov/-/media/DWR-Website/Web-Pages/Programs/State-Water-Project/",
    "Operations-And-Maintenance/Files/Operations-Control-Office/Delta-Status-And-Operations/",
    "Delta-Hydrologic-Conditions-Daily-Summary.pdf"
  )
  
  misc_monitoring <- paste0(
    "https://water.ca.gov/-/media/DWR-Website/Web-Pages/Programs/State-Water-Project/",
    "Operations-And-Maintenance/Files/Operations-Control-Office/Delta-Status-And-Operations/",
    "Delta-Miscellaneous-Daily-Monitoring-Data.pdf"
  )
  
  cvp <- "https://www.usbr.gov/mp/cvo/"
  
  paste0(
    "<b>X2 position:</b> ", pt_esc(x2), " km<br/>",
    "<hr style='margin:4px 0;'/>",
    "<a href='", pt_esc(water_ops), "' target='_blank'>daily delta water ops, per SWP</a><br/>",
    "<a href='", pt_esc(water_quality), "' target='_blank'>daily delta wq summary, per SWP</a><br/>",
    "<a href='", pt_esc(hydrology), "' target='_blank'>daily delta hydrology, per SWP</a><br/>",
    "<a href='", pt_esc(misc_monitoring), "' target='_blank'>misc delta monitoring data, per SWP</a><br/>",
    "<a href='", pt_esc(cvp), "' target='_blank'>daily CVP data</a>"
  )
}

pt_make_deltamapr_canals_popups <- function(x) {
  
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(character(0))
  }
  
  paste0(
    "<b>", pt_esc(pt_popup_na(x$Name)), "</b><br/>",
    "<b>Operator:</b> ", pt_esc(pt_popup_na(x$Operator)), "<br/>",
    "<b>Type:</b> ", pt_esc(pt_popup_na(x$Conv_Type)), "<br/>",
    "<b>Subtype:</b> ", pt_esc(pt_popup_na(x$Conv_Sub))
  )
}

pt_make_water_district_popups <- function(x) {
  
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(character(0))
  }
  
  agency <- pt_popup_na(x$AGENCYNAME)
  google_url <- paste0(
    "https://www.google.com/search?q=",
    utils::URLencode(paste("California water district", agency), reserved = TRUE)
  )
  
  paste0(
    "<b>", pt_esc(agency), "</b><br/>",
    "<a href='", pt_esc(google_url), "' target='_blank'>Google water district search</a>"
  )
}

# ==== 21. RWQCB region popups ===============================================
##
## PURPOSE:
##   Keep RWQCB region click popups intentionally small.  Hover gives the quick
##   region identity; click gives the stable link to the regional board page.

pt_make_rwqcb_region_popups <- function(x) {
  paste0(
    "<b>Region ", pt_esc(x$rwqcb_region_num), " – ",
    pt_esc(x$rwqcb_region_name), "</b><br/>",
    "<a href='", pt_esc(x$rwqcb_url),
    "' target='_blank'>Open RWQCB page</a>"
  )
}

