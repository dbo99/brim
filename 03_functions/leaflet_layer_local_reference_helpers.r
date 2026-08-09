# ==== leaflet_layer_local_reference_helpers.r ================================================
##
## PURPOSE:
##   SCAN/snow stations, offices, basins, CalSim3, reference layers, Delta, RWQCB, and water-district helpers.
##
## NOTE:
##   Extracted from leaflet_layer_helpers.r as a maintainability-only split.
##   Function names and behavior are intentionally unchanged.

# ==== 4.x SCAN stations and snow pillows =====================================

pt_add_snow_soil_station_layers <- function(m, scan_stations, snow_pillows, map_display) {
  
  add_station_layer <- function(m, x, group_name) {
    
    if (!inherits(x, "sf") || nrow(x) == 0) {
      message(group_name, " layer is empty; no points added.")
      return(m)
    }
    
    if (!"station_radius" %in% names(x)) x$station_radius <- 4.5
    if (!"station_fill_col" %in% names(x)) x$station_fill_col <- "#777777"
    if (!"station_stroke_col" %in% names(x)) x$station_stroke_col <- "#333333"
    if (!"station_fill_opacity" %in% names(x)) x$station_fill_opacity <- 0.75
    if (!"station_stroke_weight" %in% names(x)) x$station_stroke_weight <- 1.0
    if (!"hover_text" %in% names(x)) x$hover_text <- group_name
    if (!"popup_html" %in% names(x)) x$popup_html <- paste0("<b>", group_name, "</b>")
    
    m |>
      leaflet::addCircleMarkers(
        data = x,
        group = group_name,
        radius = ~station_radius,
        stroke = TRUE,
        color = ~station_stroke_col,
        weight = ~station_stroke_weight,
        opacity = 0.95,
        fillColor = ~station_fill_col,
        fillOpacity = ~station_fill_opacity,
        popup = ~popup_html,
        label = ~hover_text,
        labelOptions = leaflet::labelOptions(
          direction = "auto",
          opacity = 0.9,
          textsize = "12px",
          style = list(
            "white-space" = "pre",
            "max-width" = "none"
          )
        ),
        options = leaflet::pathOptions(pane = "pane_points"),
        clusterOptions = leaflet::markerClusterOptions(
          disableClusteringAtZoom = 10,
          spiderfyOnMaxZoom = TRUE,
          showCoverageOnHover = FALSE,
          chunkedLoading = TRUE
        )
      )
  }
  
  if (isTRUE(map_display$add_scan_stations)) {
    m <- add_station_layer(
      m = m,
      x = scan_stations,
      group_name = pt_layer_group_name("SCAN Stations")
    )
  }
  
  if (isTRUE(map_display$add_snow_pillows)) {
    
    ## Draw snow pillows in one layer. The cache already gives CDEC/SNOTEL
    ## distinct source-aware styling.
    m <- add_station_layer(
      m = m,
      x = snow_pillows,
      group_name = pt_layer_group_name("Snow Pillows")
    )
  }
  
  m
}


# ==== 5. Office and project-area layers ======================================

pt_add_project_area_layer <- function(m, project_areas) {
  
  ## Empty project-area layer is allowed. Keep the function safe.
  if (!inherits(project_areas, "sf") || nrow(project_areas) == 0) {
    message("Project area layer is empty; no polygons added.")
    return(m)
  }
  
  m |>
    leaflet::addPolygons(
      data = project_areas,
      group = pt_layer_group_name("Project area(s)"),
      fillColor = "#FF66B2",
      fillOpacity = 0.20,
      color = "#C2185B",
      weight = 2,
      opacity = 1,
      popup = ~popup_html,
      options = leaflet::pathOptions(pane = "pane_huc"),
      highlightOptions = leaflet::highlightOptions(
        weight = 4,
        bringToFront = TRUE
      )
    )
}


# ---- BLM office / field-office boundary legend -----------------------------

pt_blm_office_default_legend_rows <- function() {
  data.frame(
    district = c(
      "Northern California District (NCD)",
      "Central California District (CCD)",
      "California Desert District (CDD)"
    ),
    color = c("#7F2704", "#08519C", "#006D2C"),
    dash = c("", "4 4", ""),
    stringsAsFactors = FALSE
  )
}

pt_blm_office_legend_rows <- function(field_office_outer = NULL) {
  if (!is.null(field_office_outer) && inherits(field_office_outer, "sf") &&
      nrow(field_office_outer) > 0 &&
      all(c("PARENT_NAM", "line_col") %in% names(field_office_outer))) {

    x <- field_office_outer |>
      sf::st_drop_geometry() |>
      dplyr::transmute(
        district_raw = as.character(.data$PARENT_NAM),
        district = pt_blm_district_display_name(.data$district_raw),
        color = as.character(.data$line_col),
        dash = if ("line_dash" %in% names(field_office_outer)) as.character(.data$line_dash) else ""
      ) |>
      dplyr::filter(!is.na(.data$district), .data$district != "") |>
      dplyr::distinct(.data$district, .keep_all = TRUE)

    if (nrow(x) > 0) {
      district_order <- c(
        "Northern California District (NCD)",
        "Central California District (CCD)",
        "California Desert District (CDD)"
      )
      x <- x |>
        dplyr::mutate(.brim_order = match(.data$district, district_order)) |>
        dplyr::arrange(is.na(.data$.brim_order), .data$.brim_order, .data$district)
      x$.brim_order <- NULL
      return(as.data.frame(x[, c("district", "color", "dash")], stringsAsFactors = FALSE))
    }
  }

  pt_blm_office_default_legend_rows()
}

pt_add_blm_office_reference_legend <- function(m, field_office_outer = NULL) {

  office_group <- pt_layer_group_name("BLM Offices")
  fo_group <- pt_layer_group_name("BLM Field Office (outer)")
  legend_rows <- pt_blm_office_legend_rows(field_office_outer)

  rows_js <- jsonlite::toJSON(legend_rows, dataframe = "rows", auto_unbox = TRUE, null = "null")
  groups_js <- jsonlite::toJSON(c(office_group, fo_group), auto_unbox = TRUE)

  js <- r"---(
function(el, x) {
  var map = this;
  var targetGroups = __TARGET_GROUPS__;
  var rows = __DISTRICT_ROWS__;

  function esc(s) {
    return String(s == null ? '' : s)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/\"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  function isTargetGroup(g) {
    return targetGroups.indexOf(g) >= 0;
  }

  function layerGroup(layer) {
    return layer && layer.options ? layer.options.group : null;
  }

  function targetVisible() {
    var visible = false;
    map.eachLayer(function(layer) {
      var g = layerGroup(layer);
      if (g && isTargetGroup(g) && map.hasLayer(layer)) visible = true;
    });
    return visible;
  }

  function boundaryRow(row) {
    var dash = row && row.dash ? String(row.dash) : '';
    var style = dash ? 'dashed' : 'solid';
    return '<div class="pt-blm-office-legend-row">' +
      '<span class="pt-blm-office-boundary" style="border:2px ' + style + ' ' + esc(row.color || '#555') + ';"></span>' +
      '<span>' + esc(row.district || '') + '</span>' +
      '</div>';
  }

  function symbolRows() {
    return '' +
      '<div class="pt-blm-office-subhead">Office symbols</div>' +
      '<div class="pt-blm-office-legend-row"><span class="pt-blm-office-circle"></span><span>Field Office</span></div>' +
      '<div class="pt-blm-office-legend-row"><span class="pt-blm-office-square"></span><span>District Office</span></div>' +
      '<div class="pt-blm-office-legend-row"><span class="pt-blm-office-combo"><span></span></span><span>Co-located field + district office</span></div>';
  }

  function ensureBlmOfficeLegendCss() {
    if (document.getElementById('brim-blm-office-legend-css')) return;
    var style = document.createElement('style');
    style.id = 'brim-blm-office-legend-css';
    style.textContent =
      '.pt-blm-office-legend{display:none;background:rgba(246,239,222,0.96);border:1px solid rgba(112,103,83,0.55);border-radius:7px;box-shadow:0 1px 5px rgba(0,0,0,0.25);padding:7px 9px 8px 9px;max-width:285px;font-family:Arial,sans-serif;font-size:11.5px;line-height:1.22;color:#222;margin-bottom:152px;}' +
      '.pt-blm-office-title-row{display:flex;align-items:flex-start;justify-content:space-between;gap:8px;margin-bottom:3px;}' +
      '.pt-blm-office-title{font-weight:700;font-size:12.5px;line-height:1.15;}' +
      '.pt-blm-office-close{border:0;background:transparent;color:#777;font-size:18px;line-height:14px;padding:0 1px;cursor:pointer;font-weight:700;}' +
      '.pt-blm-office-close:hover{color:#222;}' +
      '.pt-blm-office-subhead{font-weight:700;font-size:11px;color:#333;margin:5px 0 2px 0;}' +
      '.pt-blm-office-legend-row{display:flex;align-items:center;gap:6px;margin:2px 0;}' +
      '.pt-blm-office-boundary{display:inline-block;width:16px;height:11px;background:rgba(255,255,255,0.25);box-sizing:border-box;flex:0 0 16px;}' +
      '.pt-blm-office-circle{display:inline-block;width:10px;height:10px;border-radius:50%;background:#555;border:1.4px solid #fff;box-shadow:0 0 0 1px rgba(0,0,0,0.45);box-sizing:border-box;flex:0 0 10px;}' +
      '.pt-blm-office-square{display:inline-block;width:11px;height:11px;background:#fff;border:2px solid #555;box-sizing:border-box;flex:0 0 11px;}' +
      '.pt-blm-office-combo{display:inline-block;position:relative;width:14px;height:14px;border:2px solid #555;background:#fff;box-sizing:border-box;flex:0 0 14px;}' +
      '.pt-blm-office-combo span{position:absolute;left:50%;top:50%;transform:translate(-50%,-50%);width:6px;height:6px;border-radius:50%;background:#555;border:1px solid #fff;}';
    document.head.appendChild(style);
  }

  function buildHtml() {
    var html = '';
    html += '<div class="pt-blm-office-title-row">' +
      '<div class="pt-blm-office-title">BLM CA offices / field-office boundaries</div>' +
      '<button type="button" class="pt-blm-office-close" title="Hide legend">&times;</button>' +
      '</div>';
    html += '<div class="pt-blm-office-subhead">Field-office boundaries</div>';
    (rows || []).forEach(function(row) { html += boundaryRow(row); });
    html += symbolRows();
    return html;
  }

  function ensureLegend() {
    ensureBlmOfficeLegendCss();
    if (map.__brimBlmOfficeLegendCtl && map.__brimBlmOfficeLegendDiv) {
      map.__brimBlmOfficeLegendRows = rows;
      map.__brimBlmOfficeLegendDiv.innerHTML = buildHtml();
      wireClose(map.__brimBlmOfficeLegendDiv);
      updateLegend();
      return;
    }

    map.__brimBlmOfficeLegendRows = rows;
    map.__brimBlmOfficeLegendHidden = false;

    var ctl = L.control({position: 'bottomleft'});
    ctl.onAdd = function(map) {
      var div = L.DomUtil.create('div', 'leaflet-control pt-blm-office-legend');
      div.innerHTML = buildHtml();

      ensureBlmOfficeLegendCss();

      // Keep core box styling inline as a fallback so the legend never appears as
      // an unstyled/transparent skeleton if injected CSS is delayed or replaced.
      div.style.display = 'none';
      div.style.background = 'rgba(246, 239, 222, 0.96)';
      div.style.border = '1px solid rgba(112, 103, 83, 0.55)';
      div.style.borderRadius = '7px';
      div.style.boxShadow = '0 1px 5px rgba(0,0,0,0.25)';
      div.style.padding = '7px 9px 8px 9px';
      div.style.maxWidth = '285px';
      div.style.fontFamily = 'Arial, sans-serif';
      div.style.fontSize = '11.5px';
      div.style.lineHeight = '1.17';
      div.style.color = '#222';
      div.style.marginBottom = '152px';

      L.DomEvent.disableClickPropagation(div);
      L.DomEvent.disableScrollPropagation(div);
      wireClose(div);
      map.__brimBlmOfficeLegendDiv = div;
      return div;
    };

    ctl.addTo(map);
    map.__brimBlmOfficeLegendCtl = ctl;

    map.on('overlayadd', function(e) {
      if (e && isTargetGroup(e.name)) map.__brimBlmOfficeLegendHidden = false;
      updateLegend();
    });
    map.on('overlayremove', function(e) {
      setTimeout(function() {
        if (!targetVisible()) map.__brimBlmOfficeLegendHidden = false;
        updateLegend();
      }, 0);
    });
    map.on('layeradd layerremove', updateLegend);

    setTimeout(updateLegend, 0);
    setTimeout(updateLegend, 300);
    setTimeout(updateLegend, 1000);
  }

  function wireClose(div) {
    if (!div) return;
    var btn = div.querySelector('.pt-blm-office-close');
    if (!btn || btn.__brimWired) return;
    btn.__brimWired = true;
    btn.addEventListener('click', function(ev) {
      ev.preventDefault();
      ev.stopPropagation();
      map.__brimBlmOfficeLegendHidden = true;
      updateLegend();
    });
  }

  function updateLegend() {
    var div = map.__brimBlmOfficeLegendDiv || el.querySelector('.pt-blm-office-legend');
    if (!div) return;
    var show = targetVisible() && !map.__brimBlmOfficeLegendHidden;
    div.style.display = show ? 'block' : 'none';
  }

  ensureLegend();
}
)---"

  js <- gsub("__TARGET_GROUPS__", groups_js, js, fixed = TRUE)
  js <- gsub("__DISTRICT_ROWS__", rows_js, js, fixed = TRUE)
  htmlwidgets::onRender(m, js)
}

pt_add_blm_office_layer <- function(m, blm_offices, map_display) {
  
  if (!isTRUE(map_display$add_blm_offices)) {
    return(m)
  }
  
  if (!inherits(blm_offices, "sf") || nrow(blm_offices) == 0) {
    message("BLM office layer is empty; no points added.")
    return(m)
  }
  
  ## Split offices by type.
  ##   caso = California State Office
  ##   do   = District Office
  ##   fo   = Field Office
  ##
  ## Some physical locations have both a district office and a field office.
  ## For those, draw a single composite symbol and use one combined hover/popup
  ## so users do not have to fish for the square versus the circle at the same
  ## coordinate.
  
  do_caso <- blm_offices[blm_offices$offce_type == "caso", , drop = FALSE]
  do_dist <- blm_offices[blm_offices$offce_type == "do",   , drop = FALSE]
  fo_pts  <- blm_offices[blm_offices$offce_type == "fo",   , drop = FALSE]
  
  if (!"hover_text" %in% names(do_caso)) do_caso$hover_text <- pt_make_blm_office_hover(do_caso)
  if (!"hover_text" %in% names(do_dist)) do_dist$hover_text <- pt_make_blm_office_hover(do_dist)
  if (!"hover_text" %in% names(fo_pts))  fo_pts$hover_text  <- pt_make_blm_office_hover(fo_pts)
  
  office_label_options <- leaflet::labelOptions(
    direction = "auto",
    opacity = 0.92,
    textsize = "12px",
    sticky = FALSE,
    noHide = FALSE,
    style = list(
      "white-space" = "pre",
      "max-width" = "none"
    )
  )
  
  pt_office_coord_key <- function(x) {
    paste(
      round(suppressWarnings(as.numeric(x$lon)), 6),
      round(suppressWarnings(as.numeric(x$lat)), 6),
      sep = ","
    )
  }
  
  pt_clean_col <- function(x, fallback = "#555555") {
    out <- as.character(x)
    out[is.na(out) | !nzchar(out)] <- fallback
    out
  }
  
  pt_combo_popup <- function(fo_row, do_row) {
    
    fo_name <- as.character(fo_row$offce_name[[1]])
    fo_short <- pt_blm_strip_field_office_suffix(fo_name)
    if (is.na(fo_short) || !nzchar(fo_short)) fo_short <- "Field Office"
    
    district_base <- gsub(
      "\\s+Office$",
      "",
      as.character(do_row$offce_name[[1]]),
      ignore.case = TRUE
    )
    district_display <- pt_blm_district_display_name(district_base)
    if (is.na(district_display) || !nzchar(district_display)) {
      district_display <- as.character(do_row$offce_name[[1]])
    }
    
    map_links <- pt_make_blm_office_map_links(
      lat = fo_row$lat[[1]],
      lon = fo_row$lon[[1]],
      label = paste(fo_short, district_display, sep = " + ")
    )
    
    paste0(
      "<b>BLM Office</b><br/>",
      pt_esc(paste0(fo_short, " Field Office")), "<br/>",
      pt_esc(district_display), "<br/>",
      "<b>Map:</b> ", map_links
    )
  }
  
  pt_combo_hover <- function(fo_row, do_row) {
    
    fo_hover <- pt_blm_field_office_hover_label(fo_row$offce_name[[1]])
    district_base <- gsub(
      "\\s+Office$",
      "",
      as.character(do_row$offce_name[[1]]),
      ignore.case = TRUE
    )
    district_display <- pt_blm_district_display_name(district_base)
    if (is.na(district_display) || !nzchar(district_display)) {
      district_display <- as.character(do_row$offce_name[[1]])
    }
    
    paste0(fo_hover, " · ", district_display)
  }
  
  combo_pts <- fo_pts[FALSE, , drop = FALSE]
  
  if (nrow(do_dist) > 0 && nrow(fo_pts) > 0) {
    
    do_dist$pt_coord_key <- pt_office_coord_key(do_dist)
    fo_pts$pt_coord_key  <- pt_office_coord_key(fo_pts)
    
    combo_keys <- intersect(
      unique(do_dist$pt_coord_key),
      unique(fo_pts$pt_coord_key)
    )
    combo_keys <- combo_keys[!is.na(combo_keys) & nzchar(combo_keys)]
    
    if (length(combo_keys) > 0) {
      
      combo_list <- lapply(combo_keys, function(k) {
        fo_row <- fo_pts[fo_pts$pt_coord_key == k, , drop = FALSE][1, , drop = FALSE]
        do_row <- do_dist[do_dist$pt_coord_key == k, , drop = FALSE][1, , drop = FALSE]
        
        fo_row$pt_combo_hover_text <- pt_combo_hover(fo_row, do_row)
        fo_row$pt_combo_popup_html <- pt_combo_popup(fo_row, do_row)
        fo_row$pt_combo_square_col <- pt_clean_col(do_row$fill_col[[1]], "#333333")
        fo_row$pt_combo_circle_col <- pt_clean_col(fo_row$fill_col[[1]], "#555555")
        fo_row
      })
      
      combo_pts <- do.call(rbind, combo_list)
      
      do_dist <- do_dist[!do_dist$pt_coord_key %in% combo_keys, , drop = FALSE]
      fo_pts  <- fo_pts[!fo_pts$pt_coord_key %in% combo_keys, , drop = FALSE]
    }
    
    if ("pt_coord_key" %in% names(do_dist)) do_dist$pt_coord_key <- NULL
    if ("pt_coord_key" %in% names(fo_pts))  fo_pts$pt_coord_key  <- NULL
  }
  
  ## Inline SVG helpers.
  ## These avoid external image files and keep the HTML portable.
  
  make_svg_url <- function(svg) {
    paste0(
      "data:image/svg+xml;charset=UTF-8,",
      utils::URLencode(svg, reserved = TRUE)
    )
  }
  
  make_square_svg_url <- function(stroke_col = "black") {
    
    stroke_col <- ifelse(
      is.na(stroke_col) | stroke_col == "",
      "black",
      stroke_col
    )
    
    svg <- paste0(
      "<svg xmlns='http://www.w3.org/2000/svg' width='14' height='14'>",
      "<rect x='1' y='1' width='12' height='12' ",
      "fill='none' stroke='", stroke_col, "' stroke-width='2'/>",
      "</svg>"
    )
    
    make_svg_url(svg)
  }
  
  make_combo_svg_url <- function(square_col = "#333333", circle_col = "#555555") {
    
    square_col <- ifelse(is.na(square_col) | square_col == "", "#333333", square_col)
    circle_col <- ifelse(is.na(circle_col) | circle_col == "", "#555555", circle_col)
    
    svg <- paste0(
      "<svg xmlns='http://www.w3.org/2000/svg' width='18' height='18' viewBox='0 0 18 18'>",
      "<rect x='2' y='2' width='14' height='14' rx='1.5' ry='1.5' ",
      "fill='#FFFFFF' fill-opacity='0.78' stroke='", square_col, "' stroke-width='2.2'/>",
      "<circle cx='9' cy='9' r='4.1' fill='", circle_col, "' stroke='white' stroke-width='1.2'/>",
      "</svg>"
    )
    
    make_svg_url(svg)
  }
  
  svg_star <- make_svg_url(paste0(
    "<svg xmlns='http://www.w3.org/2000/svg' width='16' height='16' viewBox='0 0 24 24'>",
    "<path d='M12 2l2.9 6.6 7.1.6-5.3 4.6 1.7 6.9L12 17l-6.4 3.7 1.7-6.9L2 9.2l7.1-.6L12 2z' ",
    "fill='none' stroke='black' stroke-width='2'/>",
    "</svg>"
  ))
  
  icon_star <- leaflet::makeIcon(
    iconUrl = svg_star,
    iconWidth = 16,
    iconHeight = 16,
    iconAnchorX = 8,
    iconAnchorY = 8
  )
  
  ## California State Office: open black star.
  if (nrow(do_caso) > 0) {
    m <- m |>
      leaflet::addMarkers(
        data = do_caso,
        group = pt_layer_group_name("BLM Offices"),
        icon = icon_star,
        popup = ~popup_html,
        label = ~hover_text,
        labelOptions = office_label_options,
        options = leaflet::pathOptions(pane = "pane_office_caso")
      )
  }
  
  ## Co-located district/field-office sites: one composite symbol, one hover,
  ## and one popup.
  if (nrow(combo_pts) > 0) {
    
    combo_icon_urls <- mapply(
      make_combo_svg_url,
      combo_pts$pt_combo_square_col,
      combo_pts$pt_combo_circle_col,
      SIMPLIFY = TRUE,
      USE.NAMES = FALSE
    )
    
    icon_combo <- leaflet::icons(
      iconUrl = combo_icon_urls,
      iconWidth = 18,
      iconHeight = 18,
      iconAnchorX = 9,
      iconAnchorY = 9
    )
    
    m <- m |>
      leaflet::addMarkers(
        data = combo_pts,
        group = pt_layer_group_name("BLM Offices"),
        icon = icon_combo,
        popup = ~pt_combo_popup_html,
        label = ~pt_combo_hover_text,
        labelOptions = office_label_options,
        options = leaflet::pathOptions(pane = "pane_office_fo")
      )
  }
  
  ## District Offices: open squares colored by their district.
  if (nrow(do_dist) > 0) {
    
    if (!"fill_col" %in% names(do_dist)) {
      do_dist$fill_col <- "black"
    }
    
    do_dist_square_urls <- vapply(
      do_dist$fill_col,
      make_square_svg_url,
      character(1)
    )
    
    icon_square <- leaflet::icons(
      iconUrl = do_dist_square_urls,
      iconWidth = 14,
      iconHeight = 14,
      iconAnchorX = 7,
      iconAnchorY = 7
    )
    
    m <- m |>
      leaflet::addMarkers(
        data = do_dist,
        group = pt_layer_group_name("BLM Offices"),
        icon = icon_square,
        popup = ~popup_html,
        label = ~hover_text,
        labelOptions = office_label_options,
        options = leaflet::pathOptions(pane = "pane_office_do")
      )
  }
  
  ## Field Offices: small district-colored circles with a white halo.
  ##
  ## fill_col is prepared in 02_build_core_map_cache.r by joining office
  ## points to field_office_outer_map$line_col. That keeps the point colors
  ## synchronized with the outer-boundary district colors.
  if (nrow(fo_pts) > 0) {
    
    if (!"fill_col" %in% names(fo_pts)) {
      fo_pts$fill_col <- "#555555"
    }
    
    m <- m |>
      leaflet::addCircleMarkers(
        data = fo_pts,
        group = pt_layer_group_name("BLM Offices"),
        radius = 4,
        color = "#FFFFFF",
        weight = 1.1,
        opacity = 0.95,
        fillColor = ~fill_col,
        fillOpacity = 0.95,
        stroke = TRUE,
        popup = ~popup_html,
        label = ~hover_text,
        labelOptions = office_label_options,
        options = leaflet::pathOptions(pane = "pane_office_fo")
      )
  }
  
  m <- pt_add_blm_office_reference_legend(m)
  m
}

# ==== 6. CNRFC basin and field-office outer polygon layers ===================

pt_add_cnrfc_basin_layer <- function(m, cnrfc_basins, map_display) {
  
  if (!isTRUE(map_display$add_cnrfc_basins)) {
    return(m)
  }
  
  if (!inherits(cnrfc_basins, "sf") || nrow(cnrfc_basins) == 0) {
    message("CNRFC basin layer is empty; no polygons added.")
    return(m)
  }
  
  m |>
    leaflet::addPolygons(
      data = cnrfc_basins,
      group = pt_layer_group_name("CNRFC Basins"),
      fill = TRUE,
      fillColor = ~fill_col,
      fillOpacity = 0.16,
      color = ~stroke_col,
      weight = 1.4,
      opacity = 0.95,
      popup = ~popup_html,
      options = leaflet::pathOptions(
        pane = "pane_cnrfc_basins"
      ),
      highlightOptions = leaflet::highlightOptions(
        weight = 2.8,
        opacity = 1,
        bringToFront = TRUE
      )
    )
}

pt_add_cnrfc_basin_product_availability_layer <- function(m, cnrfc_basin_product_availability, map_display) {

  if (!isTRUE(map_display$add_cnrfc_basin_product_availability)) {
    return(m)
  }

  if (!inherits(cnrfc_basin_product_availability, "sf") || nrow(cnrfc_basin_product_availability) == 0) {
    message("CNRFC basin product availability layer is empty or missing; no polygons added.")
    return(m)
  }

  x <- cnrfc_basin_product_availability

  pt_field_default <- function(sf_obj, field, value) {
    if (!field %in% names(sf_obj)) {
      sf_obj[[field]] <- value
    }
    sf_obj
  }

  for (field in c(
    "cnrfc_id",
    "display_name",
    "forecast_group_display",
    "product_bin_observed",
    "product_availability_label_display",
    "fill_product_availability",
    "fill_forecast_group",
    "fill_water_supply_availability",
    "fill_ensemble_availability",
    "fill_qpf_snow_level_availability",
    "fill_temperature_availability",
    "stroke_col",
    "hover_text",
    "popup_html"
  )) {
    x <- pt_field_default(x, field, NA_character_)
  }

  x <- x |>
    dplyr::mutate(
      cnrfc_id = dplyr::coalesce(as.character(.data$cnrfc_id), "CNRFC basin"),
      display_name = dplyr::coalesce(as.character(.data$display_name), .data$cnrfc_id),
      forecast_group_display = dplyr::coalesce(as.character(.data$forecast_group_display), "Not classified"),
      product_bin_observed = dplyr::coalesce(as.character(.data$product_bin_observed), "no confirmed basin products in this sample"),
      product_availability_label_display = dplyr::coalesce(as.character(.data$product_availability_label_display), .data$product_bin_observed),
      fill_product_availability = dplyr::coalesce(as.character(.data$fill_product_availability), "#d9d9d9"),
      fill_forecast_group = dplyr::coalesce(as.character(.data$fill_forecast_group), "#f0f0f0"),
      fill_water_supply_availability = dplyr::coalesce(as.character(.data$fill_water_supply_availability), "#d9d9d9"),
      fill_ensemble_availability = dplyr::coalesce(as.character(.data$fill_ensemble_availability), "#d9d9d9"),
      fill_qpf_snow_level_availability = dplyr::coalesce(as.character(.data$fill_qpf_snow_level_availability), "#d9d9d9"),
      fill_temperature_availability = dplyr::coalesce(as.character(.data$fill_temperature_availability), "#d9d9d9"),
      stroke_col = dplyr::coalesce(as.character(.data$stroke_col), "#4d4d4d"),
      hover_text = dplyr::coalesce(
        as.character(.data$hover_text),
        paste0(.data$display_name, "\n", .data$cnrfc_id, " | ", .data$product_bin_observed)
      ),
      popup_html = dplyr::coalesce(
        as.character(.data$popup_html),
        paste0("<b>", htmltools::htmlEscape(.data$cnrfc_id), "</b> – ", htmltools::htmlEscape(.data$display_name))
      )
    )

  x$pt_cnrfc_basin_availability_layer <- TRUE
  x$pt_cnrfc_basin_availability_group <- pt_layer_group_name("CNRFC Product Availability")

  group_name <- pt_layer_group_name("CNRFC Product Availability")

  m <- m |>
    leaflet::addPolygons(
      data = x,
      group = group_name,
      layerId = ~cnrfc_id,
      fill = TRUE,
      fillColor = ~fill_product_availability,
      fillOpacity = 0.46,
      color = ~stroke_col,
      weight = 1.2,
      opacity = 0.88,
      popup = ~popup_html,
      label = ~hover_text,
      labelOptions = leaflet::labelOptions(
        direction = "auto",
        opacity = 0.92,
        textsize = "11.5px",
        sticky = FALSE,
        noHide = FALSE,
        style = list("white-space" = "pre", "max-width" = "none")
      ),
      options = leaflet::pathOptions(
        pane = "pane_cnrfc_basins"
      ),
      highlightOptions = leaflet::highlightOptions(
        weight = 2.6,
        opacity = 1,
        bringToFront = TRUE
      )
    )

  pt_add_cnrfc_basin_product_availability_control(m, group_name, x)
}

pt_add_cnrfc_basin_product_availability_control <- function(m, group_name, cnrfc_basin_product_availability = NULL) {

  group_js <- gsub("\\", "\\\\", as.character(group_name), fixed = TRUE)
  group_js <- gsub("'", "\\'", group_js, fixed = TRUE)

  x_tbl <- NULL
  if (!is.null(cnrfc_basin_product_availability)) {
    x_tbl <- cnrfc_basin_product_availability
    if (inherits(x_tbl, "sf")) {
      x_tbl <- sf::st_drop_geometry(x_tbl)
    }
  }

  pt_get_chr <- function(tbl, field, default = NA_character_) {
    if (!is.data.frame(tbl) || !field %in% names(tbl)) {
      return(rep(default, if (is.data.frame(tbl)) nrow(tbl) else 0))
    }
    out <- as.character(tbl[[field]])
    out[is.na(out) | !nzchar(out)] <- default
    out
  }

  pt_get_lgl <- function(tbl, field) {
    if (!is.data.frame(tbl) || !field %in% names(tbl)) {
      return(rep(FALSE, if (is.data.frame(tbl)) nrow(tbl) else 0))
    }
    v <- tbl[[field]]
    if (is.logical(v)) {
      return(ifelse(is.na(v), FALSE, v))
    }
    tolower(as.character(v)) %in% c("true", "t", "yes", "y", "1")
  }

  if (is.data.frame(x_tbl) && nrow(x_tbl) > 0) {
    ids <- pt_get_chr(x_tbl, "cnrfc_id", "")
    keep <- nzchar(ids)
    x_tbl <- x_tbl[keep, , drop = FALSE]
    ids <- ids[keep]

    has_wy <- pt_get_lgl(x_tbl, "has_wy_or_water_supply_product")
    has_ens <- pt_get_lgl(x_tbl, "has_any_ensemble_product")
    has_qpf <- pt_get_lgl(x_tbl, "has_qpf_snow_level_row")
    has_tmp <- pt_get_lgl(x_tbl, "has_basin_temperature")

    cnrfc_panel_records <- data.frame(
      cnrfc_id = ids,
      product_availability_label = pt_get_chr(x_tbl, "product_availability_label_display", "No confirmed basin products"),
      product_availability_color = pt_get_chr(x_tbl, "fill_product_availability", "#d9d9d9"),
      forecast_group_label = pt_get_chr(x_tbl, "forecast_group_display", "Not classified"),
      forecast_group_color = pt_get_chr(x_tbl, "fill_forecast_group", "#f0f0f0"),
      water_supply_label = ifelse(has_wy, "Water supply forecast available", "Water supply not confirmed"),
      water_supply_color = pt_get_chr(x_tbl, "fill_water_supply_availability", "#d9d9d9"),
      ensemble_label = ifelse(has_ens, "Any ensemble product available", "Ensemble product not confirmed"),
      ensemble_color = pt_get_chr(x_tbl, "fill_ensemble_availability", "#d9d9d9"),
      qpf_snow_level_label = ifelse(has_qpf, "6-day daily QPF/FrzingLvl available", "6-day daily QPF/FrzingLvl not confirmed"),
      qpf_snow_level_color = pt_get_chr(x_tbl, "fill_qpf_snow_level_availability", "#d9d9d9"),
      temperature_label = ifelse(has_tmp, "Basin mean temp forecast available", "Basin mean temp not confirmed"),
      temperature_color = pt_get_chr(x_tbl, "fill_temperature_availability", "#d9d9d9"),
      stroke_color = pt_get_chr(x_tbl, "stroke_col", "#4d4d4d"),
      stringsAsFactors = FALSE
    )
  } else {
    cnrfc_panel_records <- data.frame(stringsAsFactors = FALSE)
  }

  records_js <- jsonlite::toJSON(
    cnrfc_panel_records,
    dataframe = "rows",
    auto_unbox = TRUE,
    na = "null"
  )

  js <- r"---(
function(el, x) {
  var map = this;
  var targetGroup = '__TARGET_GROUP__';
  var records = __CNRFC_RECORDS__ || [];
  var recordsById = {};
  records.forEach(function(r) {
    if (r && r.cnrfc_id != null) recordsById[String(r.cnrfc_id)] = r;
  });

  var modes = {
    product_availability: {
      label: 'Product availability',
      colorKey: 'product_availability_color',
      labelKey: 'product_availability_label',
      note: 'Availability/intelligence layer, not a hydrologic condition layer.'
    },
    forecast_group: {
      label: 'Forecast group',
      colorKey: 'forecast_group_color',
      labelKey: 'forecast_group_label',
      note: 'Colors basins by CNRFC forecast group / basin system.'
    },
    water_supply: {
      label: 'Water supply',
      colorKey: 'water_supply_color',
      labelKey: 'water_supply_label',
      note: 'Presence/absence of CNRFC WY or water-supply products.'
    },
    ensemble: {
      label: 'Ensemble products',
      colorKey: 'ensemble_color',
      labelKey: 'ensemble_label',
      note: 'Presence/absence of confirmed CNRFC ensemble products.'
    },
    qpf_snow_level: {
      label: '6-day daily QPF/FrzingLvl',
      colorKey: 'qpf_snow_level_color',
      labelKey: 'qpf_snow_level_label',
      note: 'Whether the basin appears in the CNRFC 6-day QPF/snow-level summary.'
    },
    temperature: {
      label: 'Basin mean temp',
      colorKey: 'temperature_color',
      labelKey: 'temperature_label',
      note: 'Presence/absence of CNRFC basin mean temperature forecast.'
    }
  };

  var currentMode = 'product_availability';
  var overlayState = false;
  var cardUserHidden = false;
  var cardControl = null;
  var cardDiv = null;
  var controllerDestroyed = false;
  var mapBindings = [];

  if (
    map.__ptCnrfcBasinAvailabilityController &&
    map.__ptCnrfcBasinAvailabilityController.destroy
  ) {
    map.__ptCnrfcBasinAvailabilityController.destroy();
  }

  function onMap(events, handler) {
    if (!map || !map.on) return;
    map.on(events, handler);
    mapBindings.push({events: events, handler: handler});
  }

  function scheduleCardLayout(card) {
    if (
      window.BRIM && window.BRIM.legendCloseout &&
      window.BRIM.legendCloseout.scheduleLayout
    ) {
      window.BRIM.legendCloseout.scheduleLayout(card || null);
    }
  }

  function esc(s) {
    return String(s == null ? '' : s)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  function getProps(layer) {
    return layer && layer.feature && layer.feature.properties ? layer.feature.properties : {};
  }

  function isKnownId(v) {
    return v != null && Object.prototype.hasOwnProperty.call(recordsById, String(v));
  }

  function getLayerRecord(layer) {
    if (!layer) return null;
    if (layer.__ptCnrfcId && isKnownId(layer.__ptCnrfcId)) return recordsById[String(layer.__ptCnrfcId)];
    if (layer.options) {
      var optionIds = [layer.options.layerId, layer.options.id, layer.options.featureId];
      for (var i = 0; i < optionIds.length; i++) {
        if (isKnownId(optionIds[i])) {
          layer.__ptCnrfcId = String(optionIds[i]);
          return recordsById[String(optionIds[i])];
        }
      }
    }
    var p = getProps(layer);
    var propIds = [p.cnrfc_id, p.Basin, p.nws5id, p.id];
    for (var j = 0; j < propIds.length; j++) {
      if (isKnownId(propIds[j])) {
        layer.__ptCnrfcId = String(propIds[j]);
        return recordsById[String(propIds[j])];
      }
    }
    return null;
  }

  function isTargetLayer(layer) {
    if (!layer || !layer.setStyle) return false;
    if (getLayerRecord(layer)) return true;
    if (layer.options && layer.options.group === targetGroup) return true;
    var p = getProps(layer);
    return !!(
      p.pt_cnrfc_basin_availability_layer === true ||
      String(p.pt_cnrfc_basin_availability_layer).toLowerCase() === 'true' ||
      p.pt_cnrfc_basin_availability_group === targetGroup
    );
  }

  function collectTargetLayersFrom(layer, out, idHint) {
    if (!layer) return;
    if (idHint && isKnownId(idHint)) layer.__ptCnrfcId = String(idHint);
    if (isTargetLayer(layer)) out.push(layer);
    if (layer.eachLayer) {
      layer.eachLayer(function(child) { collectTargetLayersFrom(child, out, idHint); });
    }
  }

  function collectFromLayerManager(out) {
    if (!(map && map.layerManager && map.layerManager._byGroup && map.layerManager._byGroup[targetGroup])) return;
    var tbl = map.layerManager._byGroup[targetGroup];
    Object.keys(tbl).forEach(function(k) {
      collectTargetLayersFrom(tbl[k], out, isKnownId(k) ? k : null);
    });
  }

  function targetGroupPathLayers() {
    var out = [];
    collectFromLayerManager(out);

    if (map && map.eachLayer) {
      map.eachLayer(function(layer) { collectTargetLayersFrom(layer, out, null); });
    }

    var seen = {};
    return out.filter(function(layer) {
      var id = L.stamp(layer);
      if (seen[id]) return false;
      seen[id] = true;
      return !!(layer && layer.setStyle && map.hasLayer && map.hasLayer(layer));
    });
  }

  function isVisible() {
    return targetGroupPathLayers().length > 0;
  }

  function legendRows(modeDef) {
    var counts = {};
    var order = [];

    records.forEach(function(r) {
      var label = r[modeDef.labelKey] || 'Not classified';
      var color = r[modeDef.colorKey] || '#d9d9d9';
      var key = color + '||' + label;
      if (!counts[key]) {
        counts[key] = {color: color, label: label, n: 0};
        order.push(key);
      }
      counts[key].n += 1;
    });

    order.sort(function(a, b) {
      return counts[b].n - counts[a].n || counts[a].label.localeCompare(counts[b].label);
    });

    var maxRows = currentMode === 'forecast_group' ? 12 : 20;
    var html = '';
    order.slice(0, maxRows).forEach(function(k) {
      var r = counts[k];
      html += '<div class="pt-cnrfc-basin-row"><span class="pt-cnrfc-basin-swatch" style="background:' + esc(r.color) + ';"></span><span>' + esc(r.label) + ' <span class="pt-cnrfc-basin-count">(' + r.n + ')</span></span></div>';
    });
    if (order.length > maxRows) {
      html += '<div class="pt-cnrfc-basin-small">+' + (order.length - maxRows) + ' more categories; hover/click polygons for details.</div>';
    }
    return html || '<div class="pt-cnrfc-basin-small">Turn on CNRFC Product Availability to populate legend counts.</div>';
  }

  function closeOtherCnrfcTooltips(currentLayer) {
    targetGroupPathLayers().forEach(function(other) {
      if (other === currentLayer) return;
      try { if (other.closeTooltip) other.closeTooltip(); } catch(e) {}
    });
  }

  function bindHoverClose(layer) {
    if (!layer || layer.__ptCnrfcHoverCloseBound) return;
    layer.__ptCnrfcHoverCloseBound = true;
    if (layer.on) {
      layer.on('mouseover', function() {
        closeOtherCnrfcTooltips(layer);
      });
      layer.on('mousemove', function() {
        closeOtherCnrfcTooltips(layer);
      });
      layer.on('mouseout', function() {
        try { if (layer.closeTooltip) layer.closeTooltip(); } catch(e) {}
      });
      layer.on('remove', function() {
        try { if (layer.closeTooltip) layer.closeTooltip(); } catch(e) {}
      });
    }
  }

  function applyMode() {
    var modeDef = modes[currentMode] || modes.product_availability;
    targetGroupPathLayers().forEach(function(layer) {
      bindHoverClose(layer);
      var r = getLayerRecord(layer);
      if (!r) return;
      var fill = r[modeDef.colorKey] || r.product_availability_color || '#d9d9d9';
      layer.setStyle({
        fillColor: fill,
        fillOpacity: 0.46,
        color: r.stroke_color || '#4d4d4d',
        weight: 1.2,
        opacity: 0.88
      });
    });
    updatePanel();
  }

  function cardActionsHtml() {
    if (
      window.BRIM && window.BRIM.legendCloseout &&
      window.BRIM.legendCloseout.actionsHtml
    ) {
      return window.BRIM.legendCloseout.actionsHtml(
        'pt-cnrfc-basin-dock',
        'pt-cnrfc-basin-close',
        'CNRFC basin catalog availability'
      );
    }
    return '<span class="pt-map-card-actions">' +
      '<button type="button" class="pt-map-card-dock pt-cnrfc-basin-dock" aria-label="Undock CNRFC basin catalog availability" title="Undock CNRFC basin catalog availability">&#x2197;</button>' +
      '<button type="button" class="pt-map-legend-close pt-cnrfc-basin-close" aria-label="Hide CNRFC basin catalog availability" title="Hide CNRFC basin catalog availability">&times;</button>' +
      '</span>';
  }

  function ensureCardCss() {
    if (document.getElementById('pt-cnrfc-basin-panel-style')) return;
    var style = document.createElement('style');
    style.id = 'pt-cnrfc-basin-panel-style';
    style.textContent =
      '.pt-cnrfc-basin-panel{position:relative;background:rgba(246,239,222,0.96);border:1px solid rgba(112,103,83,0.55);border-radius:7px;box-shadow:0 1px 5px rgba(0,0,0,0.25);padding:7px 8px 8px 8px;width:285px;max-height:260px;overflow:auto;font-family:Arial,sans-serif;font-size:11.5px;line-height:1.25;color:#222;box-sizing:border-box;}' +
      '.pt-cnrfc-basin-title{display:flex;align-items:flex-start;justify-content:space-between;gap:8px;margin-bottom:4px;}' +
      '.pt-cnrfc-basin-title-text{font-weight:700;font-size:12.5px;}' +
      '.pt-cnrfc-basin-close,.pt-cnrfc-basin-dock{border:0;background:transparent;color:#776f61;font-weight:700;font-size:14px;line-height:1;padding:0 2px;cursor:pointer;}' +
      '.pt-cnrfc-basin-label{display:block;font-size:10.5px;color:#555;margin-bottom:2px;}' +
      '.pt-cnrfc-basin-select{width:100%;font-size:11.5px;margin-bottom:6px;}' +
      '.pt-cnrfc-basin-row{display:flex;align-items:center;gap:6px;margin:2px 0;}' +
      '.pt-cnrfc-basin-swatch{display:inline-block;width:14px;height:10px;border:1px solid rgba(0,0,0,0.35);flex:0 0 14px;}' +
      '.pt-cnrfc-basin-count{color:#666;}' +
      '.pt-cnrfc-basin-small,.pt-cnrfc-basin-note{font-size:10.5px;color:#555;margin-top:5px;}';
    document.head.appendChild(style);
  }

  function closeCard(div) {
    cardUserHidden = true;
    if (
      div && div.__brimDetachableState &&
      div.__brimDetachableState.floating &&
      div.__brimDetachableState.dock
    ) {
      div.__brimDetachableState.dock();
    }
    if (div) div.style.display = 'none';
    scheduleCardLayout();
  }

  function wireCard(div) {
    if (!div) return;
    var shared = window.BRIM && window.BRIM.legendCloseout;
    if (shared && shared.wire) {
      shared.wire(div, '.pt-cnrfc-basin-close', function() {
        closeCard(div);
      });
    } else {
      var closeButton = div.querySelector('.pt-cnrfc-basin-close');
      if (closeButton && !closeButton.__brimLegendCloseoutWired) {
        closeButton.__brimLegendCloseoutWired = true;
        closeButton.addEventListener('click', function(e) {
          if (e) { e.preventDefault(); e.stopPropagation(); }
          closeCard(div);
        }, false);
      }
    }
    if (shared && shared.makeDetachable) {
      shared.makeDetachable({
        card: div,
        map: map,
        handleSelector: '.pt-cnrfc-basin-title',
        dockSelector: '.pt-cnrfc-basin-dock',
        label: 'CNRFC basin catalog availability'
      });
    }
  }

  function ensureCard() {
    if (controllerDestroyed) return null;
    if (
      cardDiv && map.getContainer &&
      map.getContainer().contains(cardDiv)
    ) {
      wireCard(cardDiv);
      return cardDiv;
    }

    ensureCardCss();
    cardControl = L.control({position: 'bottomleft'});
    cardControl.onAdd = function() {
      var div = L.DomUtil.create(
        'div',
        'leaflet-control pt-cnrfc-basin-panel pt-map-legend-card pt-map-legend-local'
      );
      div.style.display = 'none';
      div.innerHTML =
        '<div class="pt-cnrfc-basin-title pt-map-card-handle">' +
          '<span class="pt-cnrfc-basin-title-text">CNRFC basin catalog availability</span>' +
          cardActionsHtml() +
        '</div>' +
        '<label class="pt-cnrfc-basin-label">Color by</label>' +
        '<select class="pt-cnrfc-basin-select">' +
          '<option value="product_availability">Product availability</option>' +
          '<option value="forecast_group">Forecast group</option>' +
          '<option value="water_supply">Water supply</option>' +
          '<option value="ensemble">Ensemble products</option>' +
          '<option value="qpf_snow_level">6-day daily QPF/FrzingLvl</option>' +
          '<option value="temperature">Basin mean temp</option>' +
        '</select>' +
        '<div class="pt-cnrfc-basin-legend"></div>';

      var select = div.querySelector('.pt-cnrfc-basin-select');
      select.value = currentMode;
      select.addEventListener('change', function() {
        currentMode = select.value || 'product_availability';
        applyMode();
      });

      L.DomEvent.disableClickPropagation(div);
      L.DomEvent.disableScrollPropagation(div);
      cardDiv = div;
      return div;
    };
    cardControl.addTo(map);
    if (!cardDiv && cardControl.getContainer) {
      cardDiv = cardControl.getContainer();
    }
    wireCard(cardDiv);
    return cardDiv;
  }

  function destroyCard(resetHidden) {
    var hadCard = !!(cardControl || cardDiv);
    var div = cardDiv;
    if (div && div.__brimDetachableState && div.__brimDetachableState.destroy) {
      div.__brimDetachableState.destroy(false);
    }
    if (cardControl) {
      try {
        if (map.removeControl) map.removeControl(cardControl);
        else if (cardControl.remove) cardControl.remove();
      } catch (e) {}
    } else if (div && div.parentNode) {
      div.parentNode.removeChild(div);
    }
    cardControl = null;
    cardDiv = null;
    if (resetHidden) cardUserHidden = false;
    if (hadCard) scheduleCardLayout();
  }

  function updatePanel() {
    var visible = isVisible();
    if (!visible) {
      overlayState = false;
      destroyCard(true);
      return;
    }
    overlayState = true;

    var div = ensureCard();
    if (!div) return;
    div.style.display = cardUserHidden ? 'none' : 'block';
    if (cardUserHidden) {
      scheduleCardLayout();
      return;
    }

    var modeDef = modes[currentMode] || modes.product_availability;
    var legend = div.querySelector('.pt-cnrfc-basin-legend');
    if (legend) legend.innerHTML = legendRows(modeDef);
    scheduleCardLayout(div);
  }

  function onOverlayAdd(e) {
    if (e && e.name === targetGroup) {
      if (!overlayState) cardUserHidden = false;
      overlayState = true;
    }
    setTimeout(applyMode, 0);
    setTimeout(applyMode, 150);
  }

  function onOverlayRemove(e) {
    if (e && e.name === targetGroup) overlayState = false;
    setTimeout(updatePanel, 0);
  }

  function onMapLayerChange() {
    setTimeout(applyMode, 0);
  }

  function destroyController() {
    if (controllerDestroyed) return;
    controllerDestroyed = true;
    mapBindings.forEach(function(binding) {
      if (map && map.off) map.off(binding.events, binding.handler);
    });
    mapBindings = [];
    destroyCard(false);
    if (map.__ptCnrfcBasinAvailabilityController === controller) {
      delete map.__ptCnrfcBasinAvailabilityController;
    }
  }

  var controller = {
    destroy: destroyController,
    update: updatePanel,
    getCard: function() { return cardDiv; },
    isUserHidden: function() { return cardUserHidden; }
  };
  map.__ptCnrfcBasinAvailabilityController = controller;

  onMap('overlayadd', onOverlayAdd);
  onMap('overlayremove', onOverlayRemove);
  onMap('layeradd layerremove zoomend moveend', onMapLayerChange);
  onMap('unload', destroyController);
  setTimeout(applyMode, 0);
  setTimeout(applyMode, 400);
  setTimeout(applyMode, 1200);
}
)---"

  js <- gsub("__TARGET_GROUP__", group_js, js, fixed = TRUE)
  js <- gsub("__CNRFC_RECORDS__", as.character(records_js), js, fixed = TRUE)
  htmlwidgets::onRender(m, js)
}


pt_add_field_office_hover_cleanup <- function(m, group_name = pt_layer_group_name("BLM Field Office (outer)")) {

  group_js <- jsonlite::toJSON(group_name, auto_unbox = TRUE)

  js <- paste0("
function(el, x) {
  var map = this;
  var targetGroup = ", group_js, ";

  function isTarget(layer) {
    return layer && layer.options && layer.options.group === targetGroup;
  }

  function closeLayerTooltip(layer) {
    if (layer && layer.closeTooltip) {
      try { layer.closeTooltip(); } catch(e) {}
    }
  }

  function closeAllFoTooltips() {
    map.eachLayer(function(layer) {
      if (isTarget(layer)) closeLayerTooltip(layer);
    });
  }

  function wireFoLayer(layer) {
    if (!isTarget(layer) || layer._brimFoHoverCleanupWired) return;
    layer._brimFoHoverCleanupWired = true;
    layer.on('mouseout', function() { closeLayerTooltip(layer); });
    layer.on('click', function() { closeLayerTooltip(layer); });
    layer.on('popupopen', function() { closeLayerTooltip(layer); });
  }

  function wireAll() {
    map.eachLayer(function(layer) {
      if (isTarget(layer)) wireFoLayer(layer);
    });
  }

  map.on('layeradd overlayadd', function() {
    setTimeout(wireAll, 0);
    setTimeout(wireAll, 250);
  });
  map.on('overlayremove movestart zoomstart popupopen', closeAllFoTooltips);

  setTimeout(wireAll, 0);
  setTimeout(wireAll, 300);
  setTimeout(wireAll, 1000);
}
")

  htmlwidgets::onRender(m, js)
}

pt_add_field_office_outer_layer <- function(m, field_office_outer, map_display) {
  
  if (!isTRUE(map_display$add_field_office_outer)) {
    return(m)
  }
  
  if (!inherits(field_office_outer, "sf") || nrow(field_office_outer) == 0) {
    message("Field-office outer boundary layer is empty; no polygons added.")
    return(m)
  }
  
  if (!"hover_text" %in% names(field_office_outer)) {
    field_office_outer$hover_text <- pt_make_field_office_outer_hover(field_office_outer)
  }
  
  if (!"line_dash" %in% names(field_office_outer)) {
    field_office_outer$line_dash <- ifelse(
      grepl("^Central California District", as.character(field_office_outer$PARENT_NAM), ignore.case = TRUE),
      "4 4",
      ""
    )
  }
  
  m <- m |>
    leaflet::addPolygons(
      data = field_office_outer,
      group = pt_layer_group_name("BLM Field Office (outer)"),
      fill = TRUE,
      fillColor = "#FFFFFF",
      fillOpacity = 0.01,
      color = ~line_col,
      weight = 1.6,
      opacity = 0.7,
      dashArray = ~line_dash,
      popup = ~popup_html,
      label = ~hover_text,
      labelOptions = leaflet::labelOptions(
        direction = "auto",
        opacity = 0.92,
        textsize = "12px",
        sticky = FALSE,
        noHide = FALSE,
        style = list(
          "white-space" = "pre",
          "max-width" = "none"
        )
      ),
      options = leaflet::pathOptions(pane = "pane_huc"),
      highlightOptions = leaflet::highlightOptions(
        weight = 3,
        bringToFront = TRUE
      )
    )
  
  m <- pt_add_field_office_hover_cleanup(m)
  m <- pt_add_blm_office_reference_legend(m, field_office_outer = field_office_outer)
  m
}

# ==== 7. CalSim3 model network shared helpers ================================

PT_CALSIM3_DECLUSTER_ZOOM <- 9L
PT_CALSIM3_CLUSTER_ID <- "pt_calsim3_nodes_cluster"
PT_CALSIM3_PANE <- "pane_calsim3"
PT_CALSIM3_CANVAS_TOLERANCE <- 6
PT_CALSIM3_EXACT_CLUSTER_RADIUS_PX <- 0.000001
PT_CALSIM3_LABEL_MIN_ZOOM <- 11L
PT_CALSIM3_LABEL_CAP <- 160L
PT_CALSIM3_LABEL_NODE_CAP <- 96L
PT_CALSIM3_LABEL_ARC_CAP <- 64L
PT_CALSIM3_LABEL_GRID_DEGREES <- 0.25
PT_CALSIM3_LABEL_VIEWPORT_PAD_RATIO <- 0.15
PT_CALSIM3_ARC_TYPE_ORDER <- c(
  "Channel", "Diversion", "Return", "Inflow"
)
PT_CALSIM3_NODE_GROUP_ORDER <- c(
  "Conveyance",
  "Storage / Reservoir",
  "Project demand – urban",
  "Project demand – ag",
  "Project demand – refuge",
  "Non-project demand – urban",
  "Non-project demand – ag",
  "Non-project demand – refuge",
  "Settlement demand – urban",
  "Settlement demand – ag",
  "Demand – other",
  "Treatment plant",
  "Return flow",
  "External Unit",
  "Major Feature",
  "Unknown / Other"
)

pt_calsim3_group_name <- function() {
  pt_layer_group_name("CalSim3.0")
}

pt_calsim3_label_group_name <- function() {
  pt_layer_group_name("Labels: CalSim3.0")
}

pt_calsim3_hide_before_add <- function(m, map_display) {
  group_name <- pt_calsim3_group_name()
  default_visible <- map_display$default_visible_overlays
  if (is.null(default_visible)) default_visible <- character(0)

  ## Create the combined CalSim3 group in its hidden state before registering
  ## either lines or clustered points. This preserves normal layer-control
  ## behavior while avoiding startup projection and MarkerCluster indexing for
  ## a Local layer that is off by default.
  if (!group_name %in% default_visible) {
    m <- leaflet::hideGroup(m, group_name)
  }

  m
}

pt_calsim3_browser_layer_ids <- function(component, n) {
  if (!component %in% c("arc", "node")) {
    stop("Unknown CalSim3 browser component: ", component, call. = FALSE)
  }
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 0) {
    stop("CalSim3 browser layer-ID count must be one non-negative number.")
  }
  if (n == 0) return(character(0))
  sprintf("pt_calsim3_%s_%05d", component, seq_len(as.integer(n)))
}

pt_calsim3_browser_text <- function(x) {
  out <- trimws(as.character(x))
  out[is.na(out)] <- ""
  out
}

pt_calsim3_first_field <- function(x, fields, fallback = "") {
  available <- fields[fields %in% names(x)]
  out <- rep("", nrow(x))
  for (field in available) {
    candidate <- pt_calsim3_browser_text(x[[field]])
    use <- out == "" & candidate != ""
    out[use] <- candidate[use]
  }
  if (!is.null(fallback)) out[out == ""] <- fallback
  out
}

pt_calsim3_stable_values <- function(x, preferred) {
  values <- unique(pt_calsim3_browser_text(x))
  values <- values[values != ""]
  c(
    preferred[preferred %in% values],
    sort(setdiff(values, preferred))
  )
}

pt_calsim3_arc_browser_records <- function(calsim3_arcs) {
  required <- c("Arc_ID", "FromNode", "ToNode", "Name", "Type")
  missing <- setdiff(required, names(calsim3_arcs))
  if (length(missing)) {
    stop(
      "CalSim3 arc explorer is missing retained field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  attrs <- sf::st_drop_geometry(calsim3_arcs)
  color <- if ("line_col" %in% names(attrs)) {
    pt_calsim3_browser_text(attrs$line_col)
  } else {
    dplyr::case_when(
      attrs$Type == "Channel" ~ "#1F78B4",
      attrs$Type == "Diversion" ~ "#E31A1C",
      TRUE ~ "#777777"
    )
  }
  weight <- if ("line_weight" %in% names(attrs)) {
    suppressWarnings(as.numeric(attrs$line_weight))
  } else {
    dplyr::case_when(
      attrs$Type == "Channel" ~ 1.4,
      attrs$Type == "Diversion" ~ 1.8,
      TRUE ~ 1.2
    )
  }
  type <- pt_calsim3_browser_text(attrs$Type)
  if (any(type == "")) {
    stop("CalSim3 arc explorer cannot filter blank retained Type values.")
  }

  data.frame(
    lid = pt_calsim3_browser_layer_ids("arc", nrow(attrs)),
    id = pt_calsim3_browser_text(attrs$Arc_ID),
    fromNode = pt_calsim3_browser_text(attrs$FromNode),
    toNode = pt_calsim3_browser_text(attrs$ToNode),
    name = pt_calsim3_browser_text(attrs$Name),
    type = type,
    color = color,
    weight = weight,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

pt_calsim3_node_browser_records <- function(calsim3_nodes) {
  required <- "calsim3_node_group"
  missing <- setdiff(required, names(calsim3_nodes))
  if (length(missing)) {
    stop(
      "CalSim3 node explorer is missing retained field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  attrs <- sf::st_drop_geometry(calsim3_nodes)
  group <- pt_calsim3_browser_text(attrs$calsim3_node_group)
  if (any(group == "")) {
    stop("CalSim3 node explorer cannot filter blank retained group values.")
  }
  fill <- if ("node_fill_col" %in% names(attrs)) {
    pt_calsim3_browser_text(attrs$node_fill_col)
  } else {
    rep("#BDBDBD", nrow(attrs))
  }
  stroke <- if ("node_stroke_col" %in% names(attrs)) {
    pt_calsim3_browser_text(attrs$node_stroke_col)
  } else {
    rep("#737373", nrow(attrs))
  }
  radius <- if ("node_radius" %in% names(attrs)) {
    suppressWarnings(as.numeric(attrs$node_radius))
  } else {
    rep(4.7, nrow(attrs))
  }
  treatment <- group == "Treatment plant"
  fill[treatment] <- "#00A6D6"
  stroke[treatment] <- "#006D8F"

  data.frame(
    lid = pt_calsim3_browser_layer_ids("node", nrow(attrs)),
    id = pt_calsim3_first_field(
      attrs,
      c("node_id_display", "CalSim3_ID"),
      fallback = ""
    ),
    description = pt_calsim3_first_field(
      attrs,
      c("node_description", "NodeDescri"),
      fallback = ""
    ),
    river = pt_calsim3_first_field(
      attrs,
      c("riv_name_display", "Riv_Name"),
      fallback = ""
    ),
    comment = pt_calsim3_first_field(
      attrs,
      c("comment_display", "Comment"),
      fallback = ""
    ),
    group = group,
    fill = fill,
    stroke = stroke,
    radius = radius,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

pt_calsim3_node_coordinate_metrics <- function(calsim3_nodes) {
  if (!inherits(calsim3_nodes, "sf") || nrow(calsim3_nodes) == 0) {
    stop(
      "CalSim3 node coordinate metrics require a non-empty sf object.",
      call. = FALSE
    )
  }

  geometry_types <- as.character(sf::st_geometry_type(
    calsim3_nodes,
    by_geometry = TRUE
  ))
  if (!all(geometry_types == "POINT")) {
    stop(
      "CalSim3 browser nodes must all be POINT geometry; found: ",
      paste(sort(unique(geometry_types)), collapse = ", "),
      call. = FALSE
    )
  }

  coordinates <- sf::st_coordinates(calsim3_nodes)
  if (
    nrow(coordinates) != nrow(calsim3_nodes) ||
    !all(c("X", "Y") %in% colnames(coordinates))
  ) {
    stop(
      "CalSim3 node coordinate extraction did not reconcile with retained rows.",
      call. = FALSE
    )
  }

  valid <- is.finite(coordinates[, "X"]) &
    is.finite(coordinates[, "Y"]) &
    abs(coordinates[, "X"]) <= 180 &
    abs(coordinates[, "Y"]) <= 90
  if (!all(valid)) {
    stop(
      "CalSim3 nodes contain ",
      sum(!valid),
      " invalid map coordinate(s); refusing to silently omit records.",
      call. = FALSE
    )
  }

  ## Exact hexadecimal keys group only identical retained numeric coordinate
  ## pairs. Rounded keys are diagnostic only and never alter display geometry.
  exact_key <- paste0(
    sprintf("%a", coordinates[, "X"]),
    "_",
    sprintf("%a", coordinates[, "Y"])
  )
  coordinate_7dp_key <- paste0(
    sprintf("%.7f", coordinates[, "X"]),
    "_",
    sprintf("%.7f", coordinates[, "Y"])
  )
  exact_counts <- table(exact_key)
  coordinate_7dp_counts <- table(coordinate_7dp_key)
  exact_duplicate_sizes <- as.integer(exact_counts[exact_counts > 1L])

  node_id <- if ("node_id_display" %in% names(calsim3_nodes)) {
    trimws(as.character(calsim3_nodes$node_id_display))
  } else if ("CalSim3_ID" %in% names(calsim3_nodes)) {
    trimws(as.character(calsim3_nodes$CalSim3_ID))
  } else {
    rep(NA_character_, nrow(calsim3_nodes))
  }
  valid_node_id <- !is.na(node_id) & node_id != ""
  node_id_counts <- table(node_id[valid_node_id])

  list(
    analyticalRecordCount = nrow(calsim3_nodes),
    validCoordinateCount = sum(valid),
    uniqueExactCoordinateCount = length(exact_counts),
    exactDuplicateLocationCount = length(exact_duplicate_sizes),
    recordsAtExactDuplicateLocations = sum(exact_duplicate_sizes),
    maxRecordsAtExactLocation = if (length(exact_duplicate_sizes)) {
      max(exact_duplicate_sizes)
    } else {
      1L
    },
    uniqueCoordinateCount7dp = length(coordinate_7dp_counts),
    duplicateLocationCount7dp = sum(coordinate_7dp_counts > 1L),
    distinctNodeIdCount = length(node_id_counts),
    duplicateNodeIdValueCount = sum(node_id_counts > 1L),
    missingNodeIdCount = sum(!valid_node_id),
    coordinateGrouping = "exact retained numeric longitude/latitude pairs"
  )
}

pt_calsim3_marker_cluster_options <- function(coordinate_metrics) {
  has_exact_duplicates <- coordinate_metrics$exactDuplicateLocationCount > 0L

  common <- list(
    chunkedLoading = TRUE,
    showCoverageOnHover = FALSE,
    removeOutsideVisibleBounds = TRUE,
    animate = FALSE,
    animateAddingMarkers = FALSE
  )

  if (!has_exact_duplicates) {
    return(do.call(
      leaflet::markerClusterOptions,
      c(
        common,
        list(
          zoomToBoundsOnClick = TRUE,
          spiderfyOnMaxZoom = TRUE,
          disableClusteringAtZoom = PT_CALSIM3_DECLUSTER_ZOOM,
          maxClusterRadius = 55
        )
      )
    ))
  }

  ## A future retained product may contain legitimate records at exactly one
  ## coordinate. Below zoom 9 these remain ordinary MarkerCluster children. At
  ## zoom 9+ the one-millionth-pixel radius is effectively zero at BRIM's
  ## supported zooms and leaves only screen-identical coordinate children in a
  ## bounded same-location cluster; the CalSim controller spiderfies that group
  ## on click. No broad spatial cluster remains above the transition.
  exact_radius_js <- htmlwidgets::JS(sprintf(
    "function(zoom){return zoom < %d ? 55 : %.8f;}",
    PT_CALSIM3_DECLUSTER_ZOOM,
    PT_CALSIM3_EXACT_CLUSTER_RADIUS_PX
  ))

  do.call(
    leaflet::markerClusterOptions,
    c(
      common,
      list(
        zoomToBoundsOnClick = FALSE,
        spiderfyOnMaxZoom = FALSE,
        maxClusterRadius = exact_radius_js
      )
    )
  )
}


# ==== 7A. CalSim3 model network layer: arcs =================================

pt_add_calsim3_arc_layer <- function(m, calsim3_arcs, map_display) {
  
  if (!isTRUE(map_display$add_calsim3_arcs)) {
    return(m)
  }
  
  if (!inherits(calsim3_arcs, "sf") || nrow(calsim3_arcs) == 0) {
    message("CalSim3 arcs layer is empty; no lines added.")
    return(m)
  }
  
  message("Adding CalSim3 arcs: ", nrow(calsim3_arcs))
  
  ## Safety fallbacks in case cache was not rebuilt.
  if (!"line_col" %in% names(calsim3_arcs)) {
    calsim3_arcs$line_col <- dplyr::case_when(
      calsim3_arcs$Type == "Channel"   ~ "#1F78B4",
      calsim3_arcs$Type == "Diversion" ~ "#E31A1C",
      TRUE                             ~ "#777777"
    )
  }
  
  if (!"line_weight" %in% names(calsim3_arcs)) {
    calsim3_arcs$line_weight <- dplyr::case_when(
      calsim3_arcs$Type == "Channel"   ~ 1.4,
      calsim3_arcs$Type == "Diversion" ~ 1.8,
      TRUE                             ~ 1.2
    )
  }
  
  if (!"hover_text" %in% names(calsim3_arcs)) {
    calsim3_arcs$hover_text <- paste0(
      dplyr::coalesce(as.character(calsim3_arcs$Name), as.character(calsim3_arcs$Arc_ID), "CalSim3 arc"),
      "\nType: ", dplyr::coalesce(as.character(calsim3_arcs$Type), "unknown")
    )
  }

  m <- pt_calsim3_hide_before_add(m, map_display)
  calsim3_arcs$pt_calsim3_browser_id <- pt_calsim3_browser_layer_ids(
    "arc",
    nrow(calsim3_arcs)
  )

  m |>
    leaflet::addPolylines(
      data = calsim3_arcs,
      group = pt_calsim3_group_name(),
      layerId = ~pt_calsim3_browser_id,
      color = ~line_col,
      weight = ~line_weight,
      opacity = 0.85,
      popup = ~popup_html,
      label = ~hover_text,
      labelOptions = leaflet::labelOptions(
        direction = "auto",
        opacity = 0.92,
        textsize = "11.5px",
        sticky = TRUE,
        noHide = FALSE,
        style = list("white-space" = "pre", "max-width" = "none")
      ),
      options = leaflet::pathOptions(
        pane = PT_CALSIM3_PANE,
        interactive = TRUE
      ),
      highlightOptions = leaflet::highlightOptions(
        weight = 4,
        opacity = 1,
        bringToFront = TRUE
      )
    )
}


# ==== 7B. CalSim3 model node point layer =====================================
##
## PURPOSE:
##   Add CalSim3 model nodes as a clustered point layer.
##
## DESIGN:
##   NodeDescri is collapsed into a smaller node-group field during cache
##   building.  That grouped value drives color, while the original description,
##   Riv_Name, and Comment remain in hover/popup text.

pt_add_calsim3_node_layer <- function(m, calsim3_nodes, map_display) {

  ## Nodes are now controlled by the single CalSim3.0 checkbox.  Keep the
  ## old add_calsim3_nodes flag in MAP_DISPLAY for backward compatibility, but
  ## do not expose a separate Points checkbox.
  if (!isTRUE(map_display$add_calsim3_arcs)) {
    return(m)
  }

  if (!inherits(calsim3_nodes, "sf") || nrow(calsim3_nodes) == 0) {
    message("CalSim3 nodes layer is empty; no points added.")
    return(m)
  }

  message("Adding CalSim3 nodes: ", nrow(calsim3_nodes))

  ## Safety fallbacks in case the cache was not rebuilt.
  if (!"node_fill_col" %in% names(calsim3_nodes)) {
    calsim3_nodes$node_fill_col <- "#BDBDBD"
  }

  if (!"node_stroke_col" %in% names(calsim3_nodes)) {
    calsim3_nodes$node_stroke_col <- "#737373"
  }

  if (!"node_radius" %in% names(calsim3_nodes)) {
    calsim3_nodes$node_radius <- 4.7
  }

  if (!"hover_text" %in% names(calsim3_nodes)) {
    calsim3_nodes$hover_text <- "CalSim3 node"
  }

  ## June 2026 polish: keep treatment/wastewater visually distinct from
  ## return-flow nodes.  Do this at draw time as well as in the cache builder so
  ## a final-map-only rebuild still shows the corrected symbology.
  if ("calsim3_node_group" %in% names(calsim3_nodes)) {
    calsim3_nodes <- calsim3_nodes |>
      dplyr::mutate(
        node_fill_col = dplyr::case_when(
          .data$calsim3_node_group == "Treatment plant" ~ "#00A6D6",
          TRUE ~ as.character(.data$node_fill_col)
        ),
        node_stroke_col = dplyr::case_when(
          .data$calsim3_node_group == "Treatment plant" ~ "#006D8F",
          TRUE ~ as.character(.data$node_stroke_col)
        )
      )
  }

  coordinate_metrics <- pt_calsim3_node_coordinate_metrics(calsim3_nodes)
  cluster_options <- pt_calsim3_marker_cluster_options(coordinate_metrics)
  m <- pt_calsim3_hide_before_add(m, map_display)
  calsim3_nodes$pt_calsim3_browser_id <- pt_calsim3_browser_layer_ids(
    "node",
    nrow(calsim3_nodes)
  )

  m |>
    leaflet::addCircleMarkers(
      data = calsim3_nodes,
      group = pt_calsim3_group_name(),
      layerId = ~pt_calsim3_browser_id,
      radius = ~node_radius,
      stroke = TRUE,
      color = ~node_stroke_col,
      weight = 1.0,
      fillColor = ~node_fill_col,
      fillOpacity = 0.82,
      popup = ~popup_html,
      label = ~hover_text,
      labelOptions = leaflet::labelOptions(
        direction = "auto",
        opacity = 0.92,
        textsize = "11.5px",
        sticky = TRUE,
        noHide = FALSE,
        style = list("white-space" = "pre", "max-width" = "none")
      ),
      options = leaflet::pathOptions(
        pane = PT_CALSIM3_PANE,
        interactive = TRUE
      ),
      clusterOptions = cluster_options,
      clusterId = PT_CALSIM3_CLUSTER_ID

    )
}

# ==== 7C. CalSim3 browser-label companion ===================================
##
## The standard Local-layer inline `lbl` control requires a registered hidden
## Labels companion overlay.  This off-map, noninteractive marker supplies only
## that shared checkbox/lifecycle hook.  The CalSim browser controller creates
## the bounded viewport label objects after the companion group is enabled.

pt_add_calsim3_label_companion <- function(m, map_display) {
  if (
    !isTRUE(map_display$add_calsim3_arcs) ||
    !isTRUE(map_display$add_labels)
  ) {
    return(m)
  }

  dummy <- data.frame(lng = -170, lat = 10)
  label_group <- pt_calsim3_label_group_name()

  m |>
    leaflet::addCircleMarkers(
      data = dummy,
      lng = ~lng,
      lat = ~lat,
      group = label_group,
      layerId = "pt_calsim3_label_dummy",
      radius = 0.001,
      stroke = FALSE,
      opacity = 0,
      fillOpacity = 0,
      options = leaflet::pathOptions(
        pane = "pane_labels_pts",
        interactive = FALSE
      )
    ) |>
    leaflet::hideGroup(label_group)
}

# ==== 7D. CalSim3 cluster interaction and diagnostics ========================

pt_add_calsim3_cluster_controller <- function(
    m,
    calsim3_arcs,
    calsim3_nodes,
    map_display,
    js_path = "03_functions/js/leaflet_calsim3_local_cluster.js") {

  if (!isTRUE(map_display$add_calsim3_arcs)) {
    return(m)
  }
  if (!inherits(calsim3_nodes, "sf") || nrow(calsim3_nodes) == 0) {
    return(m)
  }
  if (!file.exists(js_path)) {
    stop("Missing CalSim3 browser controller: ", js_path)
  }

  coordinate_metrics <- pt_calsim3_node_coordinate_metrics(calsim3_nodes)
  arc_records <- pt_calsim3_arc_browser_records(calsim3_arcs)
  node_records <- pt_calsim3_node_browser_records(calsim3_nodes)
  controller_data <- c(
    list(
      groupName = pt_calsim3_group_name(),
      labelGroupName = if (isTRUE(map_display$add_labels)) {
        pt_calsim3_label_group_name()
      } else {
        ""
      },
      labelMinZoom = PT_CALSIM3_LABEL_MIN_ZOOM,
      labelCap = PT_CALSIM3_LABEL_CAP,
      labelNodeCap = PT_CALSIM3_LABEL_NODE_CAP,
      labelArcCap = PT_CALSIM3_LABEL_ARC_CAP,
      labelGridDegrees = PT_CALSIM3_LABEL_GRID_DEGREES,
      labelViewportPadRatio = PT_CALSIM3_LABEL_VIEWPORT_PAD_RATIO,
      clusterId = PT_CALSIM3_CLUSTER_ID,
      paneName = PT_CALSIM3_PANE,
      canvasTolerance = PT_CALSIM3_CANVAS_TOLERANCE,
      transitionZoom = PT_CALSIM3_DECLUSTER_ZOOM,
      exactClusterRadiusPx = PT_CALSIM3_EXACT_CLUSTER_RADIUS_PX,
      arcRecordCount = if (inherits(calsim3_arcs, "sf")) {
        nrow(calsim3_arcs)
      } else {
        0L
      },
      sameLocationMode = if (
        coordinate_metrics$exactDuplicateLocationCount > 0L
      ) {
        "exact-coordinate-spiderfy"
      } else {
        "not-required-current-cache"
      },
      arcRecords = arc_records,
      nodeRecords = node_records,
      arcTypeOrder = pt_calsim3_stable_values(
        arc_records$type,
        PT_CALSIM3_ARC_TYPE_ORDER
      ),
      nodeGroupOrder = pt_calsim3_stable_values(
        node_records$group,
        PT_CALSIM3_NODE_GROUP_ORDER
      )
    ),
    coordinate_metrics
  )

  controller_js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")
  htmlwidgets::onRender(m, controller_js, data = controller_data)
}


# ==== 8. Generic reference/admin/conservation layers =========================
##
## PURPOSE:
##   Add manifest-driven reference layers to Leaflet.
##
## DESIGN:
##   All layers in this family are outline-focused:
##     - polygons are drawn with nearly invisible fill
##     - polylines are drawn as line layers
##
##   Symbology comes from cached fields such as:
##     - line_col
##     - line_weight
##     - popup_html
##     - pt_display_name
##     - pt_geom_type



pt_wsr_local_layer_keys <- c(
  "wsr_blm_lines",
  "wsr_segments",
  "wsr_corridor_blm",
  "wsr_corridor_lsrs_area",
  "wsr_corridor_lsrs_status"
)

pt_add_wsr_reference_browser_layers <- function(m, wsr_layers) {

  if (!is.list(wsr_layers) || length(wsr_layers) == 0) {
    return(m)
  }

  wsr_layers <- wsr_layers[names(wsr_layers) %in% pt_wsr_local_layer_keys]
  wsr_layers <- wsr_layers[vapply(wsr_layers, function(x) inherits(x, "sf") && nrow(x) > 0, logical(1))]

  if (length(wsr_layers) == 0) {
    return(m)
  }

  wsr_source_label <- function(nm) {
    dplyr::case_when(
      nm == "wsr_blm_lines" ~ "BLM-CA lines",
      nm == "wsr_segments" ~ "USFS/interagency segments",
      nm == "wsr_corridor_blm" ~ "BLM-CA corridors",
      nm == "wsr_corridor_lsrs_area" ~ "USFS/LSRS areas",
      nm == "wsr_corridor_lsrs_status" ~ "USFS/LSRS legal-status corridors",
      TRUE ~ nm
    )
  }

  wsr_group_names <- purrr::imap_chr(wsr_layers, function(x, nm) {
    pt_wsr_reference_group_name(as.character(x$pt_display_name[1]))
  })

  ## Register a tiny invisible anchor layer for each source group.  The visible
  ## WSR geometry is owned by the browser-side controller below, but BRIM's Local
  ## layer-control rows still need real group anchors so Clear Local / Clear All
  ## and ordinary overlay checkboxes remain stable.
  dummy <- data.frame(lng = -170, lat = 10)
  for (g in unname(wsr_group_names)) {
    m <- m |>
      leaflet::addCircleMarkers(
        data = dummy,
        lng = ~lng,
        lat = ~lat,
        group = g,
        radius = 0.001,
        stroke = FALSE,
        opacity = 0,
        fillOpacity = 0,
        options = leaflet::pathOptions(pane = "pane_lines", interactive = FALSE)
      ) |>
      leaflet::hideGroup(g)
  }

  geojson_text <- function(x) {
    keep <- unique(c(
      "pt_display_name", "pt_nickname", "pt_geom_type",
      "popup_html", "pt_reference_hover_text",
      "wsr_source_label", "wsr_source_kind", "wsr_filter_kind",
      "wsr_class", "wsr_filter_class", "wsr_status", "wsr_corridor_status",
      "wsr_orv_supported", "wsr_date_supported", "wsr_action_year",
      "wsr_orv_fish", "wsr_orv_geologic", "wsr_orv_recreation", "wsr_orv_scenic",
      "wsr_orv_cultural", "wsr_orv_wildlife", "wsr_orv_historic", "wsr_orv_other",
      "line_col", "line_weight", "line_dash", "fill_col", "fill_opacity"
    ))
    keep <- keep[keep %in% names(x)]
    x2 <- x[, keep, drop = FALSE]
    x2 <- suppressWarnings(sf::st_transform(x2, 4326))

    tmp <- tempfile(fileext = ".geojson")
    on.exit(unlink(tmp), add = TRUE)
    if (file.exists(tmp)) unlink(tmp)
    sf::st_write(x2, tmp, driver = "GeoJSON", quiet = TRUE)
    paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "")
  }

  sources <- unname(purrr::imap(wsr_layers, function(x, nm) {
    list(
      key = nm,
      group = unname(wsr_group_names[[nm]]),
      label = wsr_source_label(nm),
      kind = if (nm %in% c("wsr_blm_lines", "wsr_segments")) "line" else "corridor",
      geojson = geojson_text(x)
    )
  }))

  js <- r"---(
function(el, x, data) {
  var map = this;
  data = data || {};

  function rowsToArray(rows) {
    if (!rows) return [];
    if (Array.isArray(rows)) return rows;
    if (typeof rows === 'object') {
      var keys = Object.keys(rows);
      if (!keys.length) return [];

      var objectRows = keys.every(function(k) {
        return rows[k] && typeof rows[k] === 'object' && !Array.isArray(rows[k]);
      });
      if (objectRows) {
        return keys.map(function(k) {
          var r = rows[k] || {};
          if (r.key === null || r.key === undefined || String(r.key).trim() === '') r.key = k;
          return r;
        });
      }

      var n = 0;
      for (var k = 0; k < keys.length; k++) {
        if (Array.isArray(rows[keys[k]])) { n = rows[keys[k]].length; break; }
      }
      var out = [];
      for (var i = 0; i < n; i++) {
        var r = {};
        for (var j = 0; j < keys.length; j++) {
          var key = keys[j];
          r[key] = Array.isArray(rows[key]) ? rows[key][i] : rows[key];
        }
        out.push(r);
      }
      return out;
    }
    return [];
  }

  function has(v) {
    if (v === null || v === undefined) return false;
    var s = String(v).trim();
    return s !== '' && s !== 'NA' && s !== 'NaN' && s !== 'null' && s !== 'undefined';
  }

  function esc(v) {
    if (!has(v)) return '';
    return String(v)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  function norm(s) {
    return String(s == null ? '' : s)
      .replace(/&amp;/g, '&')
      .replace(/[–—]/g, '-')
      .toLowerCase()
      .replace(/channels\s*-\s*/g, '')
      .replace(/\s*\([^)]*\)\s*$/g, '')
      .replace(/\s+/g, ' ')
      .trim();
  }

  function truthy(v) {
    if (v === true) return true;
    var s = norm(v);
    return s === 'true' || s === 't' || s === 'yes' || s === 'y' || s === '1';
  }

  function num(v, fallback) {
    var n = Number(v);
    return isFinite(n) ? n : fallback;
  }

  var CLASS_VALUES = ['wild', 'scenic', 'recreational'];
  var ORV_VALUES = ['fish', 'geologic', 'recreation', 'scenic', 'cultural', 'wildlife', 'historic', 'other'];
  var BLM_STATUS_VALUES = ['designated', 'suitable', 'eligible'];
  var BOUNDARY_STATUS_VALUES = ['final', 'provisional'];

  function allMap(values) {
    var out = {};
    values.forEach(function(v) { out[v] = true; });
    return out;
  }

  function noneMap(values) {
    var out = {};
    values.forEach(function(v) { out[v] = false; });
    return out;
  }

  function sanitizeMap(obj, values, defaultOn) {
    var out = {};
    values.forEach(function(v) {
      out[v] = obj && Object.prototype.hasOwnProperty.call(obj, v) ? !!obj[v] : !!defaultOn;
    });
    return out;
  }

  function allSelected(obj, values) {
    for (var i = 0; i < values.length; i++) {
      if (!obj || obj[values[i]] !== true) return false;
    }
    return true;
  }

  function anySelected(obj, values) {
    for (var i = 0; i < values.length; i++) {
      if (obj && obj[values[i]] === true) return true;
    }
    return false;
  }

  function parseYear(v) {
    if (!has(v)) return null;
    var n = parseInt(String(v).replace(/[^0-9-]/g, ''), 10);
    if (!isFinite(n) || n < 1800 || n > 2200) return null;
    return n;
  }

  function sanitizeYear(y) {
    y = y || {};
    var minY = parseYear(y.min);
    var maxY = parseYear(y.max);
    if (minY !== null && maxY !== null && minY > maxY) {
      var tmp = minY; minY = maxY; maxY = tmp;
    }
    return {
      min: minY,
      max: maxY,
      includeNoDate: y.includeNoDate !== false,
      onlyNoDate: y.onlyNoDate === true
    };
  }

  function defaultFilters() {
    return {
      cls: allMap(CLASS_VALUES),
      orv: allMap(ORV_VALUES),
      statusBlm: allMap(BLM_STATUS_VALUES),
      statusBoundary: allMap(BOUNDARY_STATUS_VALUES),
      year: {min: null, max: null, includeNoDate: true, onlyNoDate: false}
    };
  }

  var SOURCE_DEFS = rowsToArray(data.sources);
  var sourcesByKey = {};
  SOURCE_DEFS.forEach(function(src) {
    if (!src || !src.key) return;
    src.key = String(src.key);
    src.group = String(src.group || '');
    src.label = String(src.label || src.key);
    src.kind = String(src.kind || 'line');
    try {
      src.fc = JSON.parse(String(src.geojson || '{"type":"FeatureCollection","features":[]}'));
    } catch(e) {
      src.fc = {type: 'FeatureCollection', features: []};
    }
    if (!src.fc || !Array.isArray(src.fc.features)) src.fc = {type: 'FeatureCollection', features: []};
    sourcesByKey[src.key] = src;
  });

  function sourceMatchesText(def, text) {
    var ns = norm(text);
    if (!ns) return false;
    var g = norm(def.group || '');
    var label = norm(def.label || '');
    if (g && (ns === g || ns.indexOf(g) >= 0 || g.indexOf(ns) >= 0)) return true;
    if (label && (ns === label || ns.indexOf(label) >= 0 || label.indexOf(ns) >= 0)) return true;
    return false;
  }

  function sourceForText(text) {
    for (var i = 0; i < SOURCE_DEFS.length; i++) {
      if (sourceMatchesText(SOURCE_DEFS[i], text)) return SOURCE_DEFS[i];
    }
    return null;
  }

  function safeControlScan() {
    var active = {};
    SOURCE_DEFS.forEach(function(src) { active[src.key] = false; });
    if (typeof document === 'undefined' || !document.querySelectorAll) return active;
    var labels = document.querySelectorAll('.leaflet-control-layers-overlays label');
    for (var i = 0; i < labels.length; i++) {
      var label = labels[i];
      var full = label.getAttribute ? (label.getAttribute('data-pt-layer-full-name') || '') : '';
      var text = full || label.textContent || label.innerText || '';
      if (/^\s*Labels\s+[–-]\s+/i.test(text)) continue;
      var src = sourceForText(text);
      if (!src) continue;
      var input = label.querySelector ? label.querySelector('input[type="checkbox"]') : null;
      if (input && input.checked) active[src.key] = true;
    }
    return active;
  }

  function eventMatches(evt) {
    if (!evt) return false;
    if (sourceForText(evt.name)) return true;
    if (evt.layer && evt.layer.options) {
      var bits = [evt.layer.options.group, evt.layer.options.name, evt.layer.options.layerId].join(' ');
      return !!sourceForText(bits);
    }
    return false;
  }

  var filters = defaultFilters();
  var activeState = {};
  var layersByKey = {};
  var hiddenByUser = false;
  SOURCE_DEFS.forEach(function(src) { activeState[src.key] = false; });

  function prop(feature, key, fallback) {
    var p = feature && feature.properties ? feature.properties : {};
    var v = p[key];
    if (v === null || v === undefined || String(v).trim() === '') return fallback;
    return v;
  }

  function sourceKind(feature) {
    return norm(prop(feature, 'wsr_source_kind', ''));
  }

  function featureKind(feature) {
    return norm(prop(feature, 'wsr_filter_kind', ''));
  }

  function filterClass(feature) {
    return norm(prop(feature, 'wsr_filter_class', prop(feature, 'wsr_class', '')));
  }

  function filterStatus(feature) {
    return norm(prop(feature, 'wsr_corridor_status', prop(feature, 'wsr_status', '')));
  }

  function supportsOrv(feature) {
    return truthy(prop(feature, 'wsr_orv_supported', false)) || sourceKind(feature) === 'wsr_segments';
  }

  function supportsDate(feature) {
    return truthy(prop(feature, 'wsr_date_supported', false)) || sourceKind(feature) === 'wsr_corridor_lsrs_status';
  }

  function classPasses(feature) {
    var c = filterClass(feature);
    if (CLASS_VALUES.indexOf(c) < 0) return true;
    return filters.cls[c] !== false;
  }

  function statusPasses(feature) {
    var st = filterStatus(feature);
    var sk = sourceKind(feature);
    if (sk === 'wsr_corridor_blm' && BLM_STATUS_VALUES.indexOf(st) >= 0) {
      return filters.statusBlm[st] === true;
    }
    if ((sk === 'wsr_corridor_lsrs_area' || sk === 'wsr_corridor_lsrs_status') && BOUNDARY_STATUS_VALUES.indexOf(st) >= 0) {
      return filters.statusBoundary[st] === true;
    }
    return true;
  }

  function orvPasses(feature) {
    if (!supportsOrv(feature)) return true;
    if (allSelected(filters.orv, ORV_VALUES)) return true;
    if (!anySelected(filters.orv, ORV_VALUES)) return false;
    for (var i = 0; i < ORV_VALUES.length; i++) {
      var key = ORV_VALUES[i];
      if (filters.orv[key] === true && truthy(prop(feature, 'wsr_orv_' + key, false))) return true;
    }
    return false;
  }

  function yearPasses(feature) {
    if (!supportsDate(feature)) return true;
    var y = filters.year || {};
    var yr = parseInt(prop(feature, 'wsr_action_year', null), 10);
    if (!isFinite(yr)) {
      return y.onlyNoDate === true || y.includeNoDate === true;
    }
    if (y.onlyNoDate === true) return false;
    if (y.min !== null && y.min !== undefined && yr < y.min) return false;
    if (y.max !== null && y.max !== undefined && yr > y.max) return false;
    return true;
  }

  function featurePasses(feature) {
    return classPasses(feature) && statusPasses(feature) && orvPasses(feature) && yearPasses(feature);
  }

  function styleForFeature(feature) {
    var p = feature && feature.properties ? feature.properties : {};
    var color = has(p.line_col) ? String(p.line_col) : '#756BB1';
    var fill = has(p.fill_col) ? String(p.fill_col) : color;
    var kind = norm(p.wsr_filter_kind || '');
    return {
      pane: 'pane_lines',
      color: color,
      weight: num(p.line_weight, kind === 'corridor' ? 1.3 : 2.0),
      opacity: kind === 'corridor' ? 0.86 : 0.90,
      dashArray: has(p.line_dash) ? String(p.line_dash) : null,
      fill: kind === 'corridor',
      fillColor: fill,
      fillOpacity: num(p.fill_opacity, kind === 'corridor' ? 0.045 : 0)
    };
  }

  function makeTooltip(feature) {
    var p = feature && feature.properties ? feature.properties : {};
    if (has(p.pt_reference_hover_text)) return esc(p.pt_reference_hover_text);
    if (has(p.wsr_source_label)) return esc(p.wsr_source_label);
    return 'Wild & Scenic River';
  }

  function makePopup(feature) {
    var p = feature && feature.properties ? feature.properties : {};
    if (has(p.popup_html)) return String(p.popup_html);
    return '<div class="pt-popup"><b>Wild & Scenic River</b></div>';
  }

  function bindWsrInteractions(feature, lyr) {
    lyr.bindTooltip(makeTooltip(feature), {
      direction: 'auto',
      opacity: 0.9,
      sticky: true,
      className: 'pt-wsr-tooltip'
    });
    lyr.bindPopup(function() { return makePopup(feature); }, {
      maxWidth: 460,
      maxHeight: 560
    });
  }

  function hitboxStyleForFeature(feature) {
    var p = feature && feature.properties ? feature.properties : {};
    var baseWeight = num(p.line_weight, 2.0);
    return {
      pane: 'pane_lines',
      color: '#000000',
      weight: Math.max(12, baseWeight * 5.5),
      opacity: 0.001,
      dashArray: null,
      fill: false,
      fillOpacity: 0,
      interactive: true
    };
  }

  function makeVisibleFeatureLayer(fc, src) {
    var layer = L.geoJSON(fc, {
      pane: 'pane_lines',
      style: styleForFeature,
      onEachFeature: bindWsrInteractions
    });
    if (src && src.group) layer.options.group = src.group;
    return layer;
  }

  function makeLineHitboxLayer(fc, src) {
    var layer = L.geoJSON(fc, {
      pane: 'pane_lines',
      style: hitboxStyleForFeature,
      onEachFeature: bindWsrInteractions
    });
    if (src && src.group) layer.options.group = src.group;
    return layer;
  }

  function getLayer(key) {
    if (layersByKey[key]) return layersByKey[key];
    var src = sourcesByKey[key];
    var layer = L.layerGroup([]);
    layer.options = layer.options || {};
    if (src && src.group) layer.options.group = src.group;
    layersByKey[key] = layer;
    return layer;
  }

  function filteredFeatureCollection(src) {
    var fc = src && src.fc ? src.fc : {type: 'FeatureCollection', features: []};
    var out = {type: 'FeatureCollection', features: []};
    var arr = Array.isArray(fc.features) ? fc.features : [];
    for (var i = 0; i < arr.length; i++) {
      if (featurePasses(arr[i])) out.features.push(arr[i]);
    }
    return out;
  }

  function rebuildSource(key) {
    var src = sourcesByKey[key];
    if (!src) return;
    var layer = getLayer(key);
    var fc = filteredFeatureCollection(src);
    if (layer.clearLayers) layer.clearLayers();

    // Draw the normal WSR features, then add a nearly invisible wide line
    // hitbox for the two local WSR polyline sources. This keeps cartography
    // thin while making hover/click interaction forgiving.
    layer.addLayer(makeVisibleFeatureLayer(fc, src));
    if (src.kind === 'line') {
      layer.addLayer(makeLineHitboxLayer(fc, src));
    }

    if (activeState[key] && !map.hasLayer(layer)) layer.addTo(map);
  }

  function setSourceActive(key, isActive) {
    var src = sourcesByKey[key];
    if (!src) return;
    var layer = getLayer(key);
    if (isActive) {
      activeState[key] = true;
      rebuildSource(key);
      if (!map.hasLayer(layer)) layer.addTo(map);
    } else {
      activeState[key] = false;
      if (map.hasLayer(layer)) map.removeLayer(layer);
    }
  }

  function anyActive() {
    for (var i = 0; i < SOURCE_DEFS.length; i++) {
      if (activeState[SOURCE_DEFS[i].key]) return true;
    }
    return false;
  }

  function syncActive() {
    var desired = safeControlScan();
    var had = anyActive();
    SOURCE_DEFS.forEach(function(src) {
      setSourceActive(src.key, !!desired[src.key]);
    });
    var hasNow = anyActive();
    if (!hasNow) {
      filters = defaultFilters();
      hiddenByUser = false;
    } else if (!had && hasNow) {
      hiddenByUser = false;
    }
    updateLegend();
  }

  function applyFilters(next) {
    next = next || {};
    if (next.cls) filters.cls = sanitizeMap(next.cls, CLASS_VALUES, true);
    if (next.orv) filters.orv = sanitizeMap(next.orv, ORV_VALUES, true);
    if (next.statusBlm) filters.statusBlm = sanitizeMap(next.statusBlm, BLM_STATUS_VALUES, true);
    if (next.statusBoundary) filters.statusBoundary = sanitizeMap(next.statusBoundary, BOUNDARY_STATUS_VALUES, true);
    if (next.year) filters.year = sanitizeYear(next.year);

    SOURCE_DEFS.forEach(function(src) {
      if (activeState[src.key]) rebuildSource(src.key);
    });
    updateLegend();
  }

  function activeSupport() {
    var out = {
      cls: anyActive(),
      orv: false,
      statusBlm: false,
      statusBoundary: false,
      year: false,
      corridor: false
    };
    SOURCE_DEFS.forEach(function(src) {
      if (!activeState[src.key]) return;
      if (src.key === 'wsr_segments') out.orv = true;
      if (src.key === 'wsr_corridor_blm') { out.statusBlm = true; out.corridor = true; }
      if (src.key === 'wsr_corridor_lsrs_area' || src.key === 'wsr_corridor_lsrs_status') { out.statusBoundary = true; out.corridor = true; }
      if (src.key === 'wsr_corridor_lsrs_status') out.year = true;
    });
    return out;
  }

  function countFeatures() {
    var base = {line: 0, corr: 0}, shown = {line: 0, corr: 0};
    SOURCE_DEFS.forEach(function(src) {
      if (!activeState[src.key]) return;
      var arr = src.fc && Array.isArray(src.fc.features) ? src.fc.features : [];
      for (var i = 0; i < arr.length; i++) {
        var kind = featureKind(arr[i]) === 'corridor' ? 'corr' : 'line';
        base[kind] += 1;
        if (featurePasses(arr[i])) shown[kind] += 1;
      }
    });
    return {base: base, shown: shown};
  }

  function fmt(n) { return Number(n || 0).toLocaleString(); }

  function lineSwatch(color, label, dash) {
    var dashStyle = dash ? 'border-top-style:dashed;' : '';
    return '<div class="pt-wsr-row"><span class="pt-wsr-line-swatch" style="border-top-color:' + color + ';' + dashStyle + '"></span><span>' + esc(label) + '</span></div>';
  }

  function boundarySwatch(label, dashClass) {
    return '<div class="pt-wsr-row"><span class="pt-wsr-boundary-swatch ' + dashClass + '"></span><span>' + esc(label) + '</span></div>';
  }

  function checkBox(group, value, label) {
    return '<label class="pt-wsr-check"><input type="checkbox" data-wsr-check="' + group + '" data-wsr-value="' + value + '" checked> <span>' + esc(label) + '</span></label>';
  }

  function applyButton(group, label) {
    return '<button type="button" class="pt-wsr-apply" data-wsr-apply="' + group + '">' + esc(label || 'Apply') + '</button>';
  }

  function resetButton(group, label) {
    return '<button type="button" class="pt-wsr-reset" data-wsr-reset="' + group + '">' + esc(label || 'All') + '</button>';
  }

  function noneButton(group, label) {
    return '<button type="button" class="pt-wsr-none" data-wsr-none="' + group + '">' + esc(label || 'None') + '</button>';
  }

  function yearPresetButton(preset, label) {
    return '<button type="button" class="pt-wsr-year-preset" data-wsr-year-preset="' + preset + '">' + esc(label) + '</button>';
  }

  function readCheckMap(div, group, values) {
    var out = {};
    values.forEach(function(v) { out[v] = false; });
    div.querySelectorAll('input[data-wsr-check="' + group + '"]').forEach(function(inp) {
      var v = inp.getAttribute('data-wsr-value');
      if (values.indexOf(v) >= 0) out[v] = !!inp.checked;
    });
    return out;
  }

  function setCheckMap(div, group, values, mapObj) {
    div.querySelectorAll('input[data-wsr-check="' + group + '"]').forEach(function(inp) {
      var v = inp.getAttribute('data-wsr-value');
      inp.checked = !!(mapObj && mapObj[v]);
    });
  }

  function readYear(div) {
    var minEl = div.querySelector('.pt-wsr-year-min');
    var maxEl = div.querySelector('.pt-wsr-year-max');
    var noDateEl = div.querySelector('.pt-wsr-year-include-nodate');
    return {
      min: minEl ? minEl.value : null,
      max: maxEl ? maxEl.value : null,
      includeNoDate: noDateEl ? !!noDateEl.checked : false,
      onlyNoDate: false
    };
  }

  function yearPreset(preset) {
    if (preset === 'pre1980') return {min: null, max: 1979, includeNoDate: false, onlyNoDate: false};
    if (preset === '1980_1999') return {min: 1980, max: 1999, includeNoDate: false, onlyNoDate: false};
    if (preset === '2000_2009') return {min: 2000, max: 2009, includeNoDate: false, onlyNoDate: false};
    if (preset === '2010_plus') return {min: 2010, max: null, includeNoDate: false, onlyNoDate: false};
    if (preset === 'nodate') return {min: null, max: null, includeNoDate: true, onlyNoDate: true};
    return {min: null, max: null, includeNoDate: true, onlyNoDate: false};
  }

  function setYearInputs(div, y) {
    y = sanitizeYear(y);
    var minEl = div.querySelector('.pt-wsr-year-min');
    var maxEl = div.querySelector('.pt-wsr-year-max');
    var noDateEl = div.querySelector('.pt-wsr-year-include-nodate');
    var noteEl = div.querySelector('.pt-wsr-year-mode-note');
    if (minEl) minEl.value = y.min === null || y.min === undefined ? '' : String(y.min);
    if (maxEl) maxEl.value = y.max === null || y.max === undefined ? '' : String(y.max);
    if (noDateEl) noDateEl.checked = y.includeNoDate === true;
    if (noteEl) noteEl.textContent = y.onlyNoDate ? 'No-date-only preset active.' : '';
  }

  var legend = L.control({position: 'bottomleft'});
  legend.onAdd = function(map) {
    var div = L.DomUtil.create('div', 'leaflet-control pt-wsr-ref-legend');
    div.style.display = 'none';
    div.style.background = 'rgba(246, 239, 222, 0.97)';
    div.style.border = '1px solid rgba(112, 103, 83, 0.55)';
    div.style.borderRadius = '6px';
    div.style.boxShadow = '0 1px 5px rgba(0,0,0,0.25)';
    div.style.padding = '6px 8px 6px 8px';
    div.style.maxWidth = '395px';
    div.style.fontFamily = 'Arial, sans-serif';
    div.style.fontSize = '12px';
    div.style.lineHeight = '1.17';
    div.style.color = '#222';
    div.style.marginBottom = '58px';

    var html = '';
    html += '<div class="pt-wsr-head"><span>Wild & Scenic Rivers</span><button type="button" class="pt-wsr-close" title="Hide legend">×</button></div>';
    html += '<div class="pt-wsr-intro">Local federal/interagency + BLM-CA sources. California state-designated WSR layers available in External Layers.</div>';
    html += '<div class="pt-wsr-sub pt-wsr-counts"></div>';

    html += '<div class="pt-wsr-section pt-wsr-class-section">';
    html += '<div class="pt-wsr-section-title">WSR type / classification</div>';
    html += '<div class="pt-wsr-grid2">' +
      '<div>' +
        '<div class="pt-wsr-source-minihead">Class color</div>' +
        lineSwatch('#1B7837', 'Wild', false) +
        lineSwatch('#2C7FB8', 'Scenic', false) +
        lineSwatch('#F0A202', 'Recreational', false) +
      '</div>' +
      '<div>' +
        '<div class="pt-wsr-source-minihead">Line source pattern</div>' +
        lineSwatch('#555555', 'solid USFS/interagency', false) +
        lineSwatch('#555555', 'dashed BLM-CA', true) +
      '</div>' +
    '</div>';
    html += '<div class="pt-wsr-check-row">' +
      checkBox('cls', 'wild', 'Wild') +
      checkBox('cls', 'scenic', 'Scenic') +
      checkBox('cls', 'recreational', 'Rec') +
      resetButton('cls', 'All') + noneButton('cls', 'None') + applyButton('cls') +
      '</div>';
    html += '</div>';

    html += '<div class="pt-wsr-section pt-wsr-orv-section">';
    html += '<div class="pt-wsr-section-title">Outstandingly remarkable values <span class="pt-wsr-active-note">USFS/interagency segments</span></div>';
    html += '<div class="pt-wsr-check-row pt-wsr-orv-checks">' +
      checkBox('orv', 'fish', 'Fish') +
      checkBox('orv', 'geologic', 'Geologic') +
      checkBox('orv', 'recreation', 'Recreation') +
      checkBox('orv', 'scenic', 'Scenic') +
      checkBox('orv', 'cultural', 'Cultural') +
      checkBox('orv', 'wildlife', 'Wildlife') +
      checkBox('orv', 'historic', 'Historic') +
      checkBox('orv', 'other', 'Other') +
      resetButton('orv', 'All ORVs') + noneButton('orv', 'None') + applyButton('orv') +
      '</div>';
    html += '</div>';

    html += '<div class="pt-wsr-section pt-wsr-status-section">';
    html += '<div class="pt-wsr-section-title">Corridor / boundary status</div>';
    html += '<div class="pt-wsr-grid2">' +
      '<div class="pt-wsr-status-blm-block">' +
        '<div class="pt-wsr-source-minihead">BLM-CA corridors</div>' +
        boundarySwatch('Designated', 'pt-wsr-solid') +
        boundarySwatch('Suitable', 'pt-wsr-dashed') +
        boundarySwatch('Eligible', 'pt-wsr-dotted') +
        '<div class="pt-wsr-check-row">' +
          checkBox('statusBlm', 'designated', 'Designated') +
          checkBox('statusBlm', 'suitable', 'Suitable') +
          checkBox('statusBlm', 'eligible', 'Eligible') +
          resetButton('statusBlm', 'All') + applyButton('statusBlm') +
        '</div>' +
      '</div>' +
      '<div class="pt-wsr-status-boundary-block">' +
        '<div class="pt-wsr-source-minihead">USFS/LSRS boundaries</div>' +
        boundarySwatch('Final', 'pt-wsr-solid') +
        boundarySwatch('Provisional', 'pt-wsr-dashed') +
        '<div class="pt-wsr-check-row">' +
          checkBox('statusBoundary', 'final', 'Final') +
          checkBox('statusBoundary', 'provisional', 'Provisional') +
          resetButton('statusBoundary', 'All') + applyButton('statusBoundary') +
        '</div>' +
      '</div>' +
    '</div>';
    html += '</div>';

    html += '<div class="pt-wsr-section pt-wsr-date-section">';
    html += '<div class="pt-wsr-section-title">Action / designation date <span class="pt-wsr-active-note">USFS/LSRS legal-status corridors</span></div>';
    html += '<div class="pt-wsr-filter-row">' +
      yearPresetButton('all', 'All dates') +
      yearPresetButton('pre1980', '<1980') +
      yearPresetButton('1980_1999', '1980–99') +
      yearPresetButton('2000_2009', '2000–09') +
      yearPresetButton('2010_plus', '2010+') +
      yearPresetButton('nodate', 'No date') +
      '</div>';
    html += '<div class="pt-wsr-year-custom">' +
      '<label>from <input class="pt-wsr-year-min" data-wsr-year-input="min" type="text" inputmode="numeric" placeholder="year"></label>' +
      '<label>to <input class="pt-wsr-year-max" data-wsr-year-input="max" type="text" inputmode="numeric" placeholder="year"></label>' +
      '<label class="pt-wsr-nodate-label"><input class="pt-wsr-year-include-nodate" data-wsr-year-input="nodate" type="checkbox" checked> include no-date</label>' +
      applyButton('year') +
      '<span class="pt-wsr-year-mode-note"></span>' +
      '</div>';
    html += '</div>';

    html += '<div class="pt-wsr-note">Counts are source features, not unique rivers. Gray sections are inactive until a matching source layer is on. Display vertices thinned; line-part endpoints retained.</div>';
    html += '<div class="pt-wsr-source-links">USFS downloads: ' +
      '<a href="https://data.fs.usda.gov/geodata/edw/edw_resources/shp/BdyDesg_WildScenicRiverSegment_LN.zip" target="_blank">segments</a> · ' +
      '<a href="https://data.fs.usda.gov/geodata/edw/edw_resources/shp/BdyDesg_LSRS_WildScenicRiver.zip" target="_blank">areas</a> · ' +
      '<a href="https://data.fs.usda.gov/geodata/edw/edw_resources/shp/BdyDesg_LSRS_WildScenicRiverStatus.zip" target="_blank">legal status</a>' +
      '</div>';

    div.innerHTML = html;
    setYearInputs(div, filters.year);

    var style = document.createElement('style');
    style.textContent =
      '.pt-wsr-ref-legend .pt-wsr-head{display:flex;align-items:center;justify-content:space-between;gap:10px;font-weight:700;font-size:13px;margin-bottom:2px;}' +
      '.pt-wsr-ref-legend .pt-wsr-close{border:0;background:transparent;color:#777;font-size:18px;font-weight:700;line-height:1;cursor:pointer;padding:0 2px;}' +
      '.pt-wsr-ref-legend .pt-wsr-close:hover{color:#222;}' +
      '.pt-wsr-ref-legend .pt-wsr-intro{font-size:10.5px;color:#555;margin-bottom:3px;max-width:370px;}' +
      '.pt-wsr-ref-legend .pt-wsr-sub{font-size:10.9px;color:#555;margin-bottom:3px;}' +
      '.pt-wsr-ref-legend .pt-wsr-section{margin-top:5px;}' +
      '.pt-wsr-ref-legend .pt-wsr-section.pt-wsr-inactive{opacity:0.42;}' +
      '.pt-wsr-ref-legend .pt-wsr-subblock-inactive{opacity:0.42;}' +
      '.pt-wsr-ref-legend .pt-wsr-section-title{font-weight:700;margin-top:2px;margin-bottom:1px;}' +
      '.pt-wsr-ref-legend .pt-wsr-active-note{font-weight:400;color:#666;font-size:10.5px;margin-left:3px;}' +
      '.pt-wsr-ref-legend .pt-wsr-source-minihead{font-size:10.6px;color:#555;font-weight:700;margin:0 0 1px 0;}' +
      '.pt-wsr-ref-legend .pt-wsr-grid2{display:grid;grid-template-columns:1fr 0.95fr;column-gap:10px;align-items:start;}' +
      '.pt-wsr-ref-legend .pt-wsr-row{display:flex;align-items:center;gap:5px;margin:0;}' +
      '.pt-wsr-ref-legend .pt-wsr-line-swatch{display:inline-block;width:28px;height:0;border-top:3px solid #777;box-sizing:border-box;flex:0 0 28px;}' +
      '.pt-wsr-ref-legend .pt-wsr-boundary-swatch{display:inline-block;width:24px;height:11px;border:2px solid #555;box-sizing:border-box;flex:0 0 24px;background:rgba(255,255,255,0.25);}' +
      '.pt-wsr-ref-legend .pt-wsr-solid{border-style:solid;}' +
      '.pt-wsr-ref-legend .pt-wsr-dashed{border-style:dashed;}' +
      '.pt-wsr-ref-legend .pt-wsr-dotted{border-style:dotted;}' +
      '.pt-wsr-ref-legend .pt-wsr-check-row,.pt-wsr-ref-legend .pt-wsr-filter-row{display:flex;gap:3px;flex-wrap:wrap;align-items:center;margin:2px 0 1px 0;}' +
      '.pt-wsr-ref-legend .pt-wsr-check{display:inline-flex;align-items:center;gap:2px;font-size:10.6px;white-space:nowrap;background:#fbf8ef;border:1px solid #d0c6ad;border-radius:4px;padding:1px 3px;}' +
      '.pt-wsr-ref-legend .pt-wsr-check input{margin:0;}' +
      '.pt-wsr-ref-legend button{font-size:10.6px;line-height:1.05;padding:1px 5px;border:1px solid #9f967f;border-radius:4px;background:#f7f4eb;color:#222;cursor:pointer;}' +
      '.pt-wsr-ref-legend button:hover{background:#ece4d1;}' +
      '.pt-wsr-ref-legend button:disabled,.pt-wsr-ref-legend input:disabled{opacity:0.45;cursor:default;}' +
      '.pt-wsr-ref-legend .pt-wsr-year-preset.pt-wsr-filter-active{font-weight:700;background:#e8dfc8;border-color:#6f664f;}' +
      '.pt-wsr-ref-legend .pt-wsr-year-custom{display:flex;gap:4px;align-items:center;flex-wrap:wrap;margin-top:2px;}' +
      '.pt-wsr-ref-legend .pt-wsr-year-custom label{font-size:11px;color:#333;}' +
      '.pt-wsr-ref-legend .pt-wsr-year-custom input[type="text"]{width:44px;font-size:10.7px;padding:1px 3px;border:1px solid #b8ad95;border-radius:3px;background:#fffdf7;}' +
      '.pt-wsr-ref-legend .pt-wsr-nodate-label{display:inline-flex;align-items:center;gap:2px;}' +
      '.pt-wsr-ref-legend .pt-wsr-year-mode-note{font-size:10.5px;color:#6a4c00;font-style:italic;}' +
      '.pt-wsr-ref-legend .pt-wsr-note{font-size:10.2px;color:#555;margin-top:3px;border-top:1px solid rgba(120,110,90,0.25);padding-top:2px;}' +
      '.pt-wsr-ref-legend .pt-wsr-source-links{font-size:10.1px;color:#555;margin-top:1px;}' +
      '.pt-wsr-ref-legend .pt-wsr-source-links a{color:#29568a;text-decoration:none;font-weight:600;}' +
      '.pt-wsr-ref-legend .pt-wsr-source-links a:hover{text-decoration:underline;}' +
      '.pt-wsr-tooltip{white-space:pre !important;max-width:280px !important;font-size:12px;line-height:1.25;}';
    div.appendChild(style);

    div.addEventListener('click', function(e) {
      var close = e.target.closest('.pt-wsr-close');
      if (close) {
        hiddenByUser = true;
        updateLegend();
        e.preventDefault();
        e.stopPropagation();
        return;
      }

      var reset = e.target.closest('[data-wsr-reset]');
      if (reset && !reset.disabled) {
        var rg = reset.getAttribute('data-wsr-reset');
        var nextR = {};
        if (rg === 'cls') nextR.cls = allMap(CLASS_VALUES);
        if (rg === 'orv') nextR.orv = allMap(ORV_VALUES);
        if (rg === 'statusBlm') nextR.statusBlm = allMap(BLM_STATUS_VALUES);
        if (rg === 'statusBoundary') nextR.statusBoundary = allMap(BOUNDARY_STATUS_VALUES);
        applyFilters(nextR);
        e.preventDefault();
        e.stopPropagation();
        return;
      }

      var none = e.target.closest('[data-wsr-none]');
      if (none && !none.disabled) {
        var ng = none.getAttribute('data-wsr-none');
        var nextN = {};
        if (ng === 'cls') nextN.cls = noneMap(CLASS_VALUES);
        if (ng === 'orv') nextN.orv = noneMap(ORV_VALUES);
        if (ng === 'statusBlm') nextN.statusBlm = noneMap(BLM_STATUS_VALUES);
        if (ng === 'statusBoundary') nextN.statusBoundary = noneMap(BOUNDARY_STATUS_VALUES);
        applyFilters(nextN);
        e.preventDefault();
        e.stopPropagation();
        return;
      }

      var apply = e.target.closest('[data-wsr-apply]');
      if (apply && !apply.disabled) {
        var ag = apply.getAttribute('data-wsr-apply');
        var nextA = {};
        if (ag === 'cls') nextA.cls = readCheckMap(div, 'cls', CLASS_VALUES);
        if (ag === 'orv') nextA.orv = readCheckMap(div, 'orv', ORV_VALUES);
        if (ag === 'statusBlm') nextA.statusBlm = readCheckMap(div, 'statusBlm', BLM_STATUS_VALUES);
        if (ag === 'statusBoundary') nextA.statusBoundary = readCheckMap(div, 'statusBoundary', BOUNDARY_STATUS_VALUES);
        if (ag === 'year') nextA.year = readYear(div);
        applyFilters(nextA);
        e.preventDefault();
        e.stopPropagation();
        return;
      }

      var preset = e.target.closest('[data-wsr-year-preset]');
      if (preset && !preset.disabled) {
        var y = yearPreset(preset.getAttribute('data-wsr-year-preset') || 'all');
        setYearInputs(div, y);
        applyFilters({year: y});
        e.preventDefault();
        e.stopPropagation();
        return;
      }
    });

    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);
    return div;
  };

  legend.addTo(map);

  function updateLegend() {
    var div = el.querySelector('.pt-wsr-ref-legend');
    if (!div) return;

    var visible = anyActive();
    div.style.display = (visible && !hiddenByUser) ? 'block' : 'none';
    if (!visible) return;

    var sup = activeSupport();
    var counts = countFeatures();
    var countEl = div.querySelector('.pt-wsr-counts');
    if (countEl) {
      var parts = [];
      if (counts.base.line > 0) parts.push(fmt(counts.shown.line) + (counts.shown.line !== counts.base.line ? '/' + fmt(counts.base.line) : '') + ' line features');
      if (counts.base.corr > 0) parts.push(fmt(counts.shown.corr) + (counts.shown.corr !== counts.base.corr ? '/' + fmt(counts.base.corr) : '') + ' corridor features');
      countEl.textContent = parts.length ? parts.join(' · ') + ' shown after active filters' : '';
    }

    function section(sel, enabled) {
      var node = div.querySelector(sel);
      if (node) node.classList.toggle('pt-wsr-inactive', !enabled);
    }
    section('.pt-wsr-class-section', sup.cls);
    section('.pt-wsr-orv-section', sup.orv);
    section('.pt-wsr-status-section', sup.corridor);
    section('.pt-wsr-date-section', sup.year);

    function subblock(sel, enabled) {
      var node = div.querySelector(sel);
      if (node) node.classList.toggle('pt-wsr-subblock-inactive', !enabled);
    }
    subblock('.pt-wsr-status-blm-block', sup.statusBlm);
    subblock('.pt-wsr-status-boundary-block', sup.statusBoundary);

    setCheckMap(div, 'cls', CLASS_VALUES, filters.cls);
    setCheckMap(div, 'orv', ORV_VALUES, filters.orv);
    setCheckMap(div, 'statusBlm', BLM_STATUS_VALUES, filters.statusBlm);
    setCheckMap(div, 'statusBoundary', BOUNDARY_STATUS_VALUES, filters.statusBoundary);
    setYearInputs(div, filters.year);

    function disableGroup(group, enabled) {
      div.querySelectorAll('[data-wsr-check="' + group + '"],[data-wsr-apply="' + group + '"],[data-wsr-reset="' + group + '"],[data-wsr-none="' + group + '"]').forEach(function(node) {
        node.disabled = !enabled;
      });
    }
    disableGroup('cls', sup.cls);
    disableGroup('orv', sup.orv);
    disableGroup('statusBlm', sup.statusBlm);
    disableGroup('statusBoundary', sup.statusBoundary);

    div.querySelectorAll('[data-wsr-year-preset],[data-wsr-year-input],[data-wsr-apply="year"]').forEach(function(node) {
      node.disabled = !sup.year;
    });

    div.querySelectorAll('[data-wsr-year-preset]').forEach(function(btn) {
      var p = btn.getAttribute('data-wsr-year-preset') || 'all';
      var y = filters.year || {};
      var active = false;
      if (p === 'all') active = !y.onlyNoDate && y.min === null && y.max === null && y.includeNoDate === true;
      if (p === 'pre1980') active = !y.onlyNoDate && y.min === null && y.max === 1979 && y.includeNoDate === false;
      if (p === '1980_1999') active = !y.onlyNoDate && y.min === 1980 && y.max === 1999 && y.includeNoDate === false;
      if (p === '2000_2009') active = !y.onlyNoDate && y.min === 2000 && y.max === 2009 && y.includeNoDate === false;
      if (p === '2010_plus') active = !y.onlyNoDate && y.min === 2010 && y.max === null && y.includeNoDate === false;
      if (p === 'nodate') active = y.onlyNoDate === true;
      btn.classList.toggle('pt-wsr-filter-active', active);
    });
  }

  map.on('overlayadd', function(evt) {
    if (eventMatches(evt)) {
      hiddenByUser = false;
      window.setTimeout(syncActive, 0);
    }
  });
  map.on('overlayremove', function(evt) {
    if (eventMatches(evt)) window.setTimeout(syncActive, 0);
  });

  if (typeof document !== 'undefined' && document.addEventListener) {
    document.addEventListener('change', function(evt) {
      var t = evt && evt.target;
      if (t && t.matches && t.matches('.leaflet-control-layers-overlays input[type="checkbox"]')) {
        window.setTimeout(syncActive, 0);
      }
    }, true);
  }

  window.BRIM_WSR_LOCAL = {
    syncActive: syncActive,
    applyFilters: applyFilters,
    getFilters: function() { return filters; },
    getActiveState: function() { return activeState; },
    refreshLegend: updateLegend
  };

  setTimeout(syncActive, 0);
  setTimeout(syncActive, 500);
  setTimeout(syncActive, 1200);
}
)---"

  htmlwidgets::onRender(
    m,
    js,
    data = list(sources = sources)
  )
}


pt_wsr_reference_group_name <- function(display_name) {

  display_name <- pt_note_group_name(as.character(display_name))

  out <- pt_layer_group_name(display_name)

  is_wsr <- grepl("^Wild\\s*&\\s*Scenic", display_name, ignore.case = TRUE)
  out[is_wsr] <- paste0("Channels – ", display_name[is_wsr])

  out
}

pt_reference_overlay_groups <- function(reference_layers) {
  
  if (!is.list(reference_layers) || length(reference_layers) == 0) {
    return(character(0))
  }
  
  groups <- purrr::map_chr(reference_layers, function(x) {
    
    if (!inherits(x, "sf") || nrow(x) == 0) {
      return(NA_character_)
    }
    
    as.character(x$pt_display_name[1])
  })
  
  groups <- groups[!is.na(groups) & groups != ""]
  groups <- pt_wsr_reference_group_name(groups)
  
  unique(groups)
}

pt_add_reference_layers <- function(
    m,
    reference_layers,
    map_display,
    labels_all = NULL) {
  
  if (!isTRUE(map_display$add_reference_layers)) {
    return(m)
  }
  
  if (!is.list(reference_layers) || length(reference_layers) == 0) {
    message("No reference layers found; skipping.")
    return(m)
  }
  
  ref_get_chr <- function(x, fields, fallback = "") {
    out <- rep(fallback, nrow(x))
    for (field in fields) {
      if (!field %in% names(x)) next
      vals <- as.character(x[[field]])
      good <- !is.na(vals) & trimws(vals) != ""
      fill <- (is.na(out) | trimws(out) == "" | out == fallback) & good
      out[fill] <- vals[fill]
    }
    out[is.na(out)] <- fallback
    out
  }
  
  ref_pretty_label <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    missing <- is.na(x) | x == ""

    ## Several DWR reference layers store real basin names in all caps.  Convert
    ## those to title case for map labels/hover while leaving already mixed-case
    ## values, basin numbers, and IDs alone.
    has_letter <- grepl("[A-Za-z]", x)
    is_all_caps <- has_letter & !missing & x == toupper(x)
    x[is_all_caps] <- tools::toTitleCase(tolower(x[is_all_caps]))
    x
  }

  ref_popup <- function(title, label, url, link_text) {
    paste0(
      "<div class=\"pt-popup\">",
      "<b>", htmltools::htmlEscape(title), ":</b> ", htmltools::htmlEscape(label),
      "<br><a href=\"", htmltools::htmlEscape(url), "\" target=\"_blank\" rel=\"noopener noreferrer\">",
      htmltools::htmlEscape(link_text),
      "</a>",
      "<br><span style=\"font-size:11px;color:#555;\">Placeholder link. Feature-specific document links can be added in a later BRIM pass.</span>",
      "</div>"
    )
  }
  
  for (nm in names(reference_layers)) {
    
    ## WSR source layers are handled by pt_add_wsr_reference_browser_layers()
    ## below so their legend filters can rebuild browser-owned GeoJSON groups
    ## reliably. Native leaflet-R paths do not expose enough per-feature metadata
    ## for robust browser-side filtering.
    if (nm %in% pt_wsr_local_layer_keys) next

    x <- reference_layers[[nm]]
    
    if (!inherits(x, "sf") || nrow(x) == 0) next
    
    group_name <- pt_wsr_reference_group_name(as.character(x$pt_display_name[1]))
    geom_type  <- tolower(as.character(x$pt_geom_type[1]))
    
    if (!"line_col" %in% names(x)) {
      x$line_col <- "#756BB1"
    }
    
    if (!"line_weight" %in% names(x)) {
      x$line_weight <- ifelse(geom_type == "polyline", 2.0, 1.5)
    }
    if (!"line_dash" %in% names(x)) {
      x$line_dash <- ""
    }
    if (!"fill_col" %in% names(x)) {
      x$fill_col <- x$line_col
    }
    if (!"fill_opacity" %in% names(x)) {
      x$fill_opacity <- ifelse(geom_type == "polygon", 0.005, 0)
    }
    
    ## Narrow 043b polish for SGMA/adjudicated groundwater polygons:
    ##   - add compact hover text matching the label text
    ##   - replace generic attribute popups with a concise placeholder link
    ##   - add companion label-only overlays handled by the inline lbl control
    ## This is intentionally done at map-build time so build_final_map_only()
    ## can pick it up without rebuilding the core cache.
    special_label_group <- NA_character_
    special_ref <- FALSE
    interactive_local_reference <- FALSE
    if (
      nm %in% c("trails", "wildernessstudyarea", "fedwilderness", "acec") &&
      "pt_local_reference_geometry_key" %in% names(x) &&
      "pt_reference_hover_text" %in% names(x) &&
      "pt_reference_hover_html" %in% names(x)
    ) {
      special_ref <- TRUE
      interactive_local_reference <- TRUE
    } else if (nm == "gsps") {
      x$pt_reference_label_text <- ref_pretty_label(ref_get_chr(
        x,
        c("Basin_Su_1", "Basin_Name", "Basin_Subbasin_Name", "Basin", "NAME", "Name"),
        fallback = "GSP area"
      ))
      x$pt_reference_hover_text <- x$pt_reference_label_text
      x$popup_html <- ref_popup(
        title = "Groundwater Sustainability Plan area",
        label = x$pt_reference_label_text,
        url = "https://water.ca.gov/Programs/Groundwater-Management/SGMA-Groundwater-Management/Groundwater-Sustainability-Plans",
        link_text = "DWR Groundwater Sustainability Plans"
      )
      special_label_group <- pt_layer_group_name("Labels: Groundwater Sustainability Plan Areas")
      special_ref <- TRUE
    } else if (nm == "gwbasins_adjd") {
      x$pt_reference_label_text <- ref_pretty_label(ref_get_chr(
        x,
        c("Label", "NAME", "Name", "Basin_Name", "BASIN_NAME", "ADJUDICATION_ID"),
        fallback = "Adjudicated groundwater basin"
      ))
      x$pt_reference_hover_text <- x$pt_reference_label_text
      x$popup_html <- ref_popup(
        title = "Adjudicated groundwater basin",
        label = x$pt_reference_label_text,
        url = "https://water.ca.gov/Programs/Groundwater-Management/SGMA-Groundwater-Management/Adjudicated-Areas",
        link_text = "DWR Adjudicated Areas"
      )
      special_label_group <- pt_layer_group_name("Labels: Adjudicated Groundwater Basins")
      special_ref <- TRUE
    } else if ((nm %in% c("wsr", "wsr_corridor") || grepl("^wsr_", nm)) && "pt_reference_hover_text" %in% names(x)) {
      ## WSR segment/corridor popup and hover text are prepared in the core
      ## cache. Treat them as special reference layers here so hovers are bound,
      ## but do not add lbl companion layers yet; filter-aware labels can be a
      ## follow-up once the new WSR layers pass visual QA.
      special_ref <- TRUE
      special_label_group <- NA_character_
    }
    
    if (geom_type == "polyline") {
      
      if (interactive_local_reference) {
        m <- m |>
          leaflet::addPolylines(
            data = x,
            group = group_name,
            layerId = ~pt_local_reference_geometry_key,
            color = ~line_col,
            weight = ~line_weight,
            opacity = 0.90,
            dashArray = ~line_dash,
            popup = ~popup_html,
            popupOptions = leaflet::popupOptions(
              maxWidth = 460,
              minWidth = 400,
              autoPan = TRUE,
              keepInView = TRUE,
              autoPanPaddingTopLeft = c(16, 84),
              autoPanPaddingBottomRight = c(16, 24),
              className = "pt-local-reference-tabbed-popup"
            ),
            label = lapply(x$pt_reference_hover_html, htmltools::HTML),
            labelOptions = leaflet::labelOptions(
              direction = "auto",
              opacity = 0.9,
              textsize = "12px",
              className = "pt-trails-hover-tooltip",
              style = list(
                "white-space" = "normal",
                "width" = "fit-content",
                "min-width" = "min(220px, calc(100vw - 32px))",
                "max-width" = "min(320px, calc(100vw - 32px))",
                "overflow-wrap" = "break-word",
                "word-break" = "normal",
                "line-height" = "1.3",
                "box-sizing" = "border-box"
              )
            ),
            options = leaflet::pathOptions(pane = "pane_lines"),
            highlightOptions = leaflet::highlightOptions(
              weight = 4,
              opacity = 1,
              bringToFront = TRUE
            )
          )
      } else if (special_ref) {
        m <- m |>
          leaflet::addPolylines(
            data = x,
            group = group_name,
            color = ~line_col,
            weight = ~line_weight,
            opacity = 0.85,
            dashArray = ~line_dash,
            popup = ~popup_html,
            label = ~pt_reference_hover_text,
            labelOptions = leaflet::labelOptions(
              direction = "auto",
              opacity = 0.9,
              textsize = "12px",
              style = list("white-space" = "pre", "max-width" = "260px")
            ),
            options = leaflet::pathOptions(pane = "pane_lines"),
            highlightOptions = leaflet::highlightOptions(
              weight = 4,
              opacity = 1,
              bringToFront = TRUE
            )
          )
      } else {
        m <- m |>
          leaflet::addPolylines(
            data = x,
            group = group_name,
            color = ~line_col,
            weight = ~line_weight,
            opacity = 0.85,
            dashArray = ~line_dash,
            popup = ~popup_html,
            options = leaflet::pathOptions(pane = "pane_lines"),
            highlightOptions = leaflet::highlightOptions(
              weight = 4,
              opacity = 1,
              bringToFront = TRUE
            )
          )
      }
      
    } else {
      
      if (interactive_local_reference) {
        m <- m |>
          leaflet::addPolygons(
            data = x,
            group = group_name,
            layerId = ~pt_local_reference_geometry_key,
            fill = TRUE,
            fillColor = ~fill_col,
            fillOpacity = ~fill_opacity,
            color = ~line_col,
            weight = ~line_weight,
            opacity = 0.90,
            dashArray = ~line_dash,
            popup = if (nm %in% c("fedwilderness", "acec")) NULL else ~popup_html,
            popupOptions = leaflet::popupOptions(
              maxWidth = 460,
              minWidth = 400,
              autoPan = TRUE,
              keepInView = TRUE,
              autoPanPaddingTopLeft = c(16, 84),
              autoPanPaddingBottomRight = c(16, 24),
              className = "pt-local-reference-tabbed-popup"
            ),
            label = lapply(x$pt_reference_hover_html, htmltools::HTML),
            labelOptions = leaflet::labelOptions(
              direction = "auto",
              opacity = 0.9,
              textsize = "12px",
              className = if (identical(nm, "fedwilderness")) {
                "pt-fw-hover-tooltip"
              } else if (identical(nm, "acec")) {
                "pt-acec-hover-tooltip"
              } else {
                "pt-wsa-hover-tooltip"
              },
              style = list(
                "white-space" = "normal",
                "width" = "fit-content",
                "min-width" = "min(220px, calc(100vw - 32px))",
                "max-width" = "min(320px, calc(100vw - 32px))",
                "overflow-wrap" = "break-word",
                "word-break" = "normal",
                "line-height" = "1.3",
                "box-sizing" = "border-box"
              )
            ),
            options = leaflet::pathOptions(pane = "pane_lines"),
            highlightOptions = leaflet::highlightOptions(
              weight = 4,
              opacity = 1,
              bringToFront = TRUE
            )
          )
      } else if (special_ref) {
        m <- m |>
          leaflet::addPolygons(
            data = x,
            group = group_name,
            fill = TRUE,
            fillColor = ~fill_col,
            fillOpacity = ~fill_opacity,
            color = ~line_col,
            weight = ~line_weight,
            opacity = 0.90,
            dashArray = ~line_dash,
            popup = ~popup_html,
            label = ~pt_reference_hover_text,
            labelOptions = leaflet::labelOptions(
              direction = "auto",
              opacity = 0.9,
              textsize = "12px",
              style = list("white-space" = "pre", "max-width" = "260px")
            ),
            options = leaflet::pathOptions(pane = "pane_lines"),
            highlightOptions = leaflet::highlightOptions(
              weight = 4,
              opacity = 1,
              bringToFront = TRUE
            )
          )
      } else {
        m <- m |>
          leaflet::addPolygons(
            data = x,
            group = group_name,
            fill = TRUE,
            fillColor = ~fill_col,
            fillOpacity = ~fill_opacity,
            color = ~line_col,
            weight = ~line_weight,
            opacity = 0.90,
            dashArray = ~line_dash,
            popup = ~popup_html,
            options = leaflet::pathOptions(pane = "pane_lines"),
            highlightOptions = leaflet::highlightOptions(
              weight = 4,
              opacity = 1,
              bringToFront = TRUE
            )
          )
      }
    }
    
    if (isTRUE(map_display$add_labels) && special_ref && !is.na(special_label_group)) {
      label_pts <- suppressWarnings(sf::st_point_on_surface(x))
      label_pts <- label_pts[!is.na(label_pts$pt_reference_label_text) & trimws(label_pts$pt_reference_label_text) != "", , drop = FALSE]
      if (nrow(label_pts) > 0) {
        m <- m |>
          leaflet::addLabelOnlyMarkers(
            data = label_pts,
            group = special_label_group,
            label = ~pt_reference_label_text,
            labelOptions = leaflet::labelOptions(
              noHide = TRUE,
              direction = "center",
              textOnly = TRUE,
              opacity = 1,
              className = "pt-label pt-label-sgma-ref"
            ),
            options = leaflet::markerOptions(
              pane = "pane_labels_poly",
              interactive = FALSE
            )
          ) |>
          leaflet::hideGroup(special_label_group)
      }
    }
  }
  
  m <- pt_add_wsr_reference_browser_layers(
    m,
    reference_layers[names(reference_layers) %in% pt_wsr_local_layer_keys]
  )

  m <- pt_add_local_reference_controller(
    m,
    reference_layers = reference_layers,
    labels_all = labels_all
  )

  m
}

# ==== 9. Major conveyance line layer =========================================
##
## PURPOSE:
##   Add major water-conveyance linework.
##
## INPUT:
##   Cached layer:
##     04_processed_data/cache/latest/major_conveyance_map.rds
##
## SYMBOLOGY:
##   Federal      = blue
##   State        = orange
##   Non-CVP/SWP  = purple
##   Fed/State    = drawn twice with blue/orange dashed overlays
##
## NOTE:
##   Older caches may still contain "Local / CCWD" or "Other / unknown".  The
##   selection logic below treats those legacy groups as Non-CVP/SWP so the map
##   remains backward compatible until the core cache is rebuilt.

pt_add_major_conveyance_layer <- function(m, major_conveyance, map_display) {
  
  if (!isTRUE(map_display$add_major_conveyance)) {
    return(m)
  }
  
  if (!inherits(major_conveyance, "sf") || nrow(major_conveyance) == 0) {
    message("Major conveyance layer is empty; no lines added.")
    return(m)
  }
  
  message("Adding major conveyance: ", nrow(major_conveyance))
  
  federal_col <- "#1F78B4"
  state_col   <- "#FF7F00"
  non_cvp_swp_col <- "#756BB1"
  
  conv_federal <- major_conveyance[
    major_conveyance$operator_group %in% "Federal",
    ,
    drop = FALSE
  ]
  
  conv_state <- major_conveyance[
    major_conveyance$operator_group %in% "State",
    ,
    drop = FALSE
  ]
  
  conv_non_cvp_swp <- major_conveyance[
    major_conveyance$operator_group %in% c("Non-CVP/SWP", "Local / CCWD", "Other / unknown"),
    ,
    drop = FALSE
  ]
  
  conv_fedstate <- major_conveyance[
    major_conveyance$operator_group %in% "Fed/State",
    ,
    drop = FALSE
  ]
  
  # ---- 9.1 Non-CVP/SWP -----------------------------------------------------
  
  if (nrow(conv_non_cvp_swp) > 0) {
    m <- m |>
      leaflet::addPolylines(
        data = conv_non_cvp_swp,
        group = pt_layer_group_name("Major Conveyance"),
        color = non_cvp_swp_col,
        weight = 2.2,
        opacity = 0.85,
        popup = ~popup_html,
        options = leaflet::pathOptions(pane = "pane_lines"),
        highlightOptions = leaflet::highlightOptions(
          weight = 4,
          opacity = 1,
          bringToFront = TRUE
        )
      )
  }
  
  # ---- 9.2 Federal ----------------------------------------------------------
  
  if (nrow(conv_federal) > 0) {
    m <- m |>
      leaflet::addPolylines(
        data = conv_federal,
        group = pt_layer_group_name("Major Conveyance"),
        color = federal_col,
        weight = 2.4,
        opacity = 0.90,
        popup = ~popup_html,
        options = leaflet::pathOptions(pane = "pane_lines"),
        highlightOptions = leaflet::highlightOptions(
          weight = 4,
          opacity = 1,
          bringToFront = TRUE
        )
      )
  }
  
  # ---- 9.3 State ------------------------------------------------------------
  
  if (nrow(conv_state) > 0) {
    m <- m |>
      leaflet::addPolylines(
        data = conv_state,
        group = pt_layer_group_name("Major Conveyance"),
        color = state_col,
        weight = 2.4,
        opacity = 0.90,
        popup = ~popup_html,
        options = leaflet::pathOptions(pane = "pane_lines"),
        highlightOptions = leaflet::highlightOptions(
          weight = 4,
          opacity = 1,
          bringToFront = TRUE
        )
      )
  }
  
  # ---- 9.4 Fed/State --------------------------------------------------------
  ##
  ## Draw the same features twice:
  ##   - Federal blue dashed line
  ##   - State orange dashed line offset by half a dash cycle
  
  if (nrow(conv_fedstate) > 0) {
    
    m <- m |>
      leaflet::addPolylines(
        data = conv_fedstate,
        group = pt_layer_group_name("Major Conveyance"),
        color = federal_col,
        weight = 3.0,
        opacity = 0.95,
        popup = ~popup_html,
        options = leaflet::pathOptions(
          pane = "pane_lines",
          dashArray = "10 10"
        ),
        highlightOptions = leaflet::highlightOptions(
          weight = 5,
          opacity = 1,
          bringToFront = TRUE
        )
      ) |>
      leaflet::addPolylines(
        data = conv_fedstate,
        group = pt_layer_group_name("Major Conveyance"),
        color = state_col,
        weight = 3.0,
        opacity = 0.95,
        popup = ~popup_html,
        options = leaflet::pathOptions(
          pane = "pane_lines",
          dashArray = "10 10",
          dashOffset = "10"
        ),
        highlightOptions = leaflet::highlightOptions(
          weight = 5,
          opacity = 1,
          bringToFront = TRUE
        )
      )
  }
  
  m
}

# ==== 10. CNRFC FNF Sha/Tri/west Sierra basin layer ==================================

pt_add_cnrfc_fnf_delta_layer <- function(m, cnrfc_fnf_delta, map_display) {
  
  if (!isTRUE(map_display$add_cnrfc_fnf_delta)) {
    return(m)
  }
  
  if (!inherits(cnrfc_fnf_delta, "sf") || nrow(cnrfc_fnf_delta) == 0) {
    message("CNRFC FNF Sha/Tri/west Sierra basin layer is empty; no polygons added.")
    return(m)
  }
  
  message("Adding CNRFC FNF Sha/Tri/west Sierra basins: ", nrow(cnrfc_fnf_delta))
  
  group_name <- pt_layer_group_name("CNRFC FNF Sha/Tri/west Sierra Basins")
  label_group <- pt_layer_group_name("Labels: CNRFC FNF Sha/Tri/west Sierra Basins")
  
  ## Label text: use the CNRFC/NWS five-character basin ID. This is the compact
  ## identifier users need from the second popup row, and it keeps the inline
  ## lbl option useful without adding long basin names to the map.
  fnf_get_chr <- function(x, fields, fallback = "") {
    out <- rep(fallback, nrow(x))
    for (field in fields) {
      if (!field %in% names(x)) next
      vals <- as.character(x[[field]])
      good <- !is.na(vals) & trimws(vals) != ""
      fill <- (is.na(out) | trimws(out) == "" | out == fallback) & good
      out[fill] <- vals[fill]
    }
    out[is.na(out)] <- fallback
    out
  }
  cnrfc_fnf_delta$pt_fnf_label_text <- fnf_get_chr(
    cnrfc_fnf_delta,
    c("nws5id", "NWSID", "NWS_ID", "Name"),
    fallback = ""
  )
  
  m <- m |>
    leaflet::addPolygons(
      data = cnrfc_fnf_delta,
      group = group_name,
      fill = TRUE,
      fillColor = "#FFFFFF",
      fillOpacity = 0,
      color = ~line_col,
      weight = ~line_weight,
      opacity = 0.95,
      popup = ~popup_html,
      label = ~hover_text,
      labelOptions = leaflet::labelOptions(
        direction = "auto",
        opacity = 0.9,
        textsize = "12px",
        style = list("white-space" = "pre", "max-width" = "none")
      ),
      options = leaflet::pathOptions(pane = "pane_huc"),
      highlightOptions = leaflet::highlightOptions(
        weight = 3.5,
        opacity = 1,
        bringToFront = TRUE
      )
    ) |>
    leaflet::hideGroup(group_name)
  
  if (isTRUE(map_display$add_labels)) {
    fnf_label_pts <- suppressWarnings(sf::st_point_on_surface(cnrfc_fnf_delta))
    fnf_label_pts <- fnf_label_pts[fnf_label_pts$pt_fnf_label_text != "", , drop = FALSE]
    
    if (nrow(fnf_label_pts) > 0) {
      m <- m |>
        leaflet::addLabelOnlyMarkers(
          data = fnf_label_pts,
          group = label_group,
          label = ~pt_fnf_label_text,
          labelOptions = leaflet::labelOptions(
            noHide = TRUE,
            direction = "center",
            textOnly = TRUE,
            opacity = 1,
            className = "pt-label pt-label-cnrfc-fnf"
          ),
          options = leaflet::markerOptions(
            pane = "pane_labels_poly",
            interactive = FALSE
          )
        ) |>
        leaflet::hideGroup(label_group)
    }
  }
  
  m
}

# ==== 11. CVP/SWP X2 km point layer =========================================

pt_add_x2_km_layer <- function(m, x2_km, map_display) {
  
  if (!isTRUE(map_display$add_x2_km)) {
    return(m)
  }
  
  if (!inherits(x2_km, "sf") || nrow(x2_km) == 0) {
    message("CVP/SWP X2 km point layer is empty; no points added.")
    return(m)
  }
  
  message("Adding CVP/SWP X2 km points: ", nrow(x2_km))
  
  x2_group <- pt_layer_group_name("CVP/SWP X2 km points")
  
  m <- m |>
    leaflet::addCircleMarkers(
      data = x2_km,
      group = x2_group,
      radius = 3.8,
      stroke = TRUE,
      color = "#111111",
      weight = 1.0,
      fillColor = "#FFFFFF",
      fillOpacity = 0.85,
      popup = ~popup_html,
      label = ~hover_text,
      labelOptions = leaflet::labelOptions(
        direction = "auto",
        opacity = 0.9,
        textsize = "12px"
      ),
      options = leaflet::pathOptions(pane = "pane_points")
    ) |>
    leaflet::addLabelOnlyMarkers(
      data = x2_km,
      group = x2_group,
      label = ~x2_km_display,
      labelOptions = leaflet::labelOptions(
        noHide = TRUE,
        direction = "right",
        textOnly = TRUE,
        opacity = 0.95,
        className = "pt-label pt-x2-label"
      ),
      options = leaflet::markerOptions(
        pane = "pane_labels_pts",
        interactive = FALSE
      )
    )
  
  ## X2 labels are useful near the Delta but too dense at regional scale.
  ## Keep the point markers visible whenever the layer is toggled on, but show
  ## the permanent text labels only after users zoom in far enough.
  htmlwidgets::onRender(
    m,
    "
function(el, x) {
  var map = this;
  var minZoom = 10;
  
  function updateX2Labels() {
    var show = map.getZoom() >= minZoom;
    var labels = el.querySelectorAll('.pt-x2-label');
    for (var i = 0; i < labels.length; i++) {
      labels[i].style.display = show ? '' : 'none';
    }
  }
  
  map.on('zoomend overlayadd overlayremove layeradd', updateX2Labels);
  setTimeout(updateX2Labels, 0);
  setTimeout(updateX2Labels, 250);
  setTimeout(updateX2Labels, 1000);
}
"
  )
}

# ==== 12. Deltamapr conveyance line layer ===================================

pt_add_deltamapr_canals_layer <- function(m, deltamapr_canals, map_display) {
  
  if (!isTRUE(map_display$add_deltamapr_canals)) {
    return(m)
  }
  
  if (!inherits(deltamapr_canals, "sf") || nrow(deltamapr_canals) == 0) {
    message("Deltamapr conveyance layer is empty; no lines added.")
    return(m)
  }
  
  message("Adding Deltamapr conveyance: ", nrow(deltamapr_canals))
  
  m |>
    leaflet::addPolylines(
      data = deltamapr_canals,
      group = pt_layer_group_name("Deltamapr Conveyance"),
      color = ~line_col,
      weight = ~line_weight,
      opacity = 0.85,
      popup = ~popup_html,
      label = ~hover_text,
      labelOptions = leaflet::labelOptions(
        direction = "auto",
        opacity = 0.9,
        textsize = "12px",
        style = list("white-space" = "pre", "max-width" = "none")
      ),
      options = leaflet::pathOptions(pane = "pane_lines"),
      highlightOptions = leaflet::highlightOptions(
        weight = 4,
        opacity = 1,
        bringToFront = TRUE
      )
    )
}



# ==== BRIM-mapped water conveyance ===========================================
##
## Browser-managed single-layer controller modeled on the Local WSR panel.
## Normal Local control rows remain ordinary Leaflet anchors, while the browser
## owns visible line and label GeoJSON so hierarchical filtering is robust.

pt_add_brim_mapped_conveyance_layer <- function(
  m,
  conveyance_segments,
  conveyance_facilities,
  conveyance_labels,
  map_display
) {

  if (!isTRUE(map_display$add_brim_mapped_conveyance)) {
    return(m)
  }

  if (
    !inherits(conveyance_segments, "sf") ||
    nrow(conveyance_segments) == 0 ||
    !is.data.frame(conveyance_facilities) ||
    nrow(conveyance_facilities) == 0
  ) {
    message(
      "Water conveyance | BRIM mapped controller inputs are missing; layer skipped."
    )
    return(m)
  }

  main_group <- pt_layer_group_name("Water conveyance | BRIM mapped")
  label_group <- pt_layer_group_name("Labels: Water conveyance | BRIM mapped")
  dummy <- data.frame(lng = -170, lat = 10)

  m <- m |>
    leaflet::addCircleMarkers(
      data = dummy,
      lng = ~lng,
      lat = ~lat,
      group = main_group,
      radius = 0.001,
      stroke = FALSE,
      opacity = 0,
      fillOpacity = 0,
      options = leaflet::pathOptions(
        pane = "pane_conveyance",
        interactive = FALSE
      )
    ) |>
    leaflet::hideGroup(main_group)

  if (
    isTRUE(map_display$add_labels) &&
    inherits(conveyance_labels, "sf") &&
    nrow(conveyance_labels) > 0
  ) {
    m <- m |>
      leaflet::addCircleMarkers(
        data = dummy,
        lng = ~lng,
        lat = ~lat,
        group = label_group,
        radius = 0.001,
        stroke = FALSE,
        opacity = 0,
        fillOpacity = 0,
        options = leaflet::pathOptions(
          pane = "pane_labels_poly",
          interactive = FALSE
        )
      ) |>
      leaflet::hideGroup(label_group)
  }

  geojson_text <- function(x, fields, rename_to) {
    fields <- fields[fields %in% names(x)]
    x2 <- x[, fields, drop = FALSE]
    names(x2)[seq_along(fields)] <- rename_to[seq_along(fields)]
    x2 <- suppressWarnings(sf::st_transform(x2, 4326))

    tmp <- tempfile(fileext = ".geojson")
    on.exit(unlink(tmp), add = TRUE)
    if (file.exists(tmp)) unlink(tmp)

    sf::st_write(
      x2,
      tmp,
      driver = "GeoJSON",
      layer_options = "COORDINATE_PRECISION=6",
      quiet = TRUE
    )

    paste(readLines(tmp, warn = FALSE, encoding = "UTF-8"), collapse = "")
  }

  lines_geojson <- geojson_text(
    conveyance_segments,
    c(
      "facility_id",
      "facility_length_mi_map",
      "source_segment_count",
      "facility_blm_crosses_map",
      "facility_blm_length_mi_map",
      "facility_blm_pct_length_map",
      "facility_blm_nearest_mi_map",
      "facility_blm_crossing_count_map"
    ),
    c("fid", "lm", "sc", "bx", "bl", "bp", "bd", "bc")
  )

  labels_geojson <- '{"type":"FeatureCollection","features":[]}'

  if (
    isTRUE(map_display$add_labels) &&
    inherits(conveyance_labels, "sf") &&
    nrow(conveyance_labels) > 0
  ) {
    labels_geojson <- geojson_text(
      conveyance_labels,
      c("facility_id", "lbl", "lbl_min_zoom", "lbl_max_zoom"),
      c("fid", "lbl", "zmin", "zmax")
    )
  }

  facility_fields <- c(
    "facility_id", "canonical_name", "lbl", "aliases", "parent_system",
    "facility_group", "facility_type", "network_role",
    "ownership_bucket", "ownership_class", "owner_agency", "operator_agency",
    "project_family", "project_name", "project_division", "project_unit",
    "project_subunit", "project_path", "filter_family", "filter_cvp_division",
    "filter_cvp_unit", "filter_swp_system", "filter_other_reclamation_system",
    "filter_local_system", "display_rank", "display_rank_source",
    "display_rank_reason", "status", "length_mi", "segment_count",
    "source_segment_count", "geometry_confidence", "decision_ids",
    "facility_crosses_blm", "facility_length_on_blm_mi",
    "facility_pct_length_on_blm", "facility_min_blm_distance_mi",
    "facility_blm_field_offices", "geometry_source", "geometry_decision",
    "source_ids", "source_major_rows", "source_delta_rows",
    "bbox_xmin", "bbox_ymin", "bbox_xmax", "bbox_ymax"
  )

  facility_fields <- facility_fields[
    facility_fields %in% names(conveyance_facilities)
  ]

  short_names <- c(
    facility_id = "fid", canonical_name = "n", lbl = "lbl", aliases = "a",
    parent_system = "par", facility_group = "fg", facility_type = "ft",
    network_role = "nr", ownership_bucket = "ob", ownership_class = "oc",
    owner_agency = "oa", operator_agency = "op", project_family = "pf",
    project_name = "pn", project_division = "pd", project_unit = "pu",
    project_subunit = "ps", project_path = "pp", filter_family = "ff",
    filter_cvp_division = "cd", filter_cvp_unit = "cu",
    filter_swp_system = "ss", filter_other_reclamation_system = "os",
    filter_local_system = "ls", display_rank = "rk",
    display_rank_source = "rs", display_rank_reason = "rr", status = "st",
    length_mi = "lm", segment_count = "sc", source_segment_count = "ssc",
    geometry_confidence = "gc", decision_ids = "di",
    facility_crosses_blm = "bx", facility_length_on_blm_mi = "bl",
    facility_pct_length_on_blm = "bp", facility_min_blm_distance_mi = "bd",
    facility_blm_field_offices = "bfo", geometry_source = "gs",
    geometry_decision = "gd", source_ids = "si", source_major_rows = "sm",
    source_delta_rows = "sd", bbox_xmin = "xmin", bbox_ymin = "ymin",
    bbox_xmax = "xmax", bbox_ymax = "ymax"
  )

  facility_records <- conveyance_facilities[, facility_fields, drop = FALSE]
  names(facility_records) <- unname(short_names[facility_fields])

  controller_data <- list(
    main_group = main_group,
    label_group = label_group,
    lines_geojson = lines_geojson,
    labels_geojson = labels_geojson,
    facilities = facility_records
  )

  js <- r"---(
function(el, x, data) {
  var map = this;
  data = data || {};

  function rowsToArray(rows) {
    if (!rows) return [];
    if (Array.isArray(rows)) return rows;
    if (typeof rows !== 'object') return [];
    var keys = Object.keys(rows), n = 0;
    keys.forEach(function(k) {
      if (Array.isArray(rows[k])) n = Math.max(n, rows[k].length);
    });
    var out = [];
    for (var i = 0; i < n; i++) {
      var r = {};
      keys.forEach(function(k) {
        r[k] = Array.isArray(rows[k]) ? rows[k][i] : rows[k];
      });
      out.push(r);
    }
    return out;
  }

  function has(v) {
    if (v === null || v === undefined) return false;
    var s = String(v).trim();
    return s !== '' && s !== 'NA' && s !== 'NaN' && s !== 'null' && s !== 'undefined';
  }

  function esc(v) {
    if (!has(v)) return '';
    return String(v)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  function norm(v) {
    return String(v == null ? '' : v)
      .replace(/&amp;/g, '&')
      .replace(/[–—]/g, '-')
      .toLowerCase()
      .replace(/channels\s*-\s*/g, '')
      .replace(/labels\s*-\s*/g, '')
      .replace(/\s*\([^)]*\)\s*$/g, '')
      .replace(/\s+/g, ' ')
      .trim();
  }

  function truthy(v) {
    if (v === true) return true;
    var s = norm(v);
    return s === 'true' || s === 't' || s === 'yes' || s === 'y' || s === '1';
  }

  function num(v, fallback) {
    var n = Number(v);
    return isFinite(n) ? n : fallback;
  }

  function fmt(v, digits) {
    var n = Number(v);
    if (!isFinite(n)) return '';
    return n.toLocaleString(undefined, {
      minimumFractionDigits: digits,
      maximumFractionDigits: digits
    });
  }

  function splitValues(v) {
    if (!has(v)) return [];
    return String(v).split(/\s*;\s*/).map(norm).filter(Boolean);
  }

  function selectedKeys(obj) {
    return Object.keys(obj || {}).filter(function(k) { return obj[k] === true; });
  }

  function facetPass(raw, selected) {
    var keys = selectedKeys(selected);
    if (!keys.length) return true; // Empty = Any / unconstrained.
    var vals = splitValues(raw);
    if (!vals.length) vals = ['unknown'];
    for (var i = 0; i < vals.length; i++) {
      if (selected[vals[i]] === true) return true;
    }
    return false;
  }

  var mainGroup = String(data.main_group || '');
  var labelGroup = String(data.label_group || '');
  var facilityRows = rowsToArray(data.facilities);
  var facilitiesById = {};
  facilityRows.forEach(function(r) {
    if (r && has(r.fid)) facilitiesById[String(r.fid)] = r;
  });

  var lineFC, labelFC;
  try {
    lineFC = JSON.parse(String(data.lines_geojson || '{"type":"FeatureCollection","features":[]}'));
  } catch (e) {
    lineFC = {type:'FeatureCollection', features:[]};
  }
  try {
    labelFC = JSON.parse(String(data.labels_geojson || '{"type":"FeatureCollection","features":[]}'));
  } catch (e) {
    labelFC = {type:'FeatureCollection', features:[]};
  }
  if (!lineFC || !Array.isArray(lineFC.features)) lineFC = {type:'FeatureCollection', features:[]};
  if (!labelFC || !Array.isArray(labelFC.features)) labelFC = {type:'FeatureCollection', features:[]};

  var OWN_VALUES = ['federal','state','joint','local','private','unknown'];
  var PF_VALUES = ['cvp','swp','other reclamation','local/regional public','unknown/unassigned'];
  var GROUP_VALUES = ['open conveyance','closed conveyance','power conveyance','distribution/local','drainage','flood conveyance','other/unknown'];
  var RANK_VALUES = ['statewide_major','regional_major','medium','local_supporting'];
  var CONF_VALUES = ['high','medium','low'];

  function emptyMap() { return {}; }

  function defaultFilters() {
    return {
      ownership: emptyMap(),
      projectFamily: emptyMap(),
      system: '',
      unit: '',
      groups: emptyMap(),
      crossesBlm: false,
      withinBlm: false,
      maxBlmMiles: 5,
      ranks: emptyMap(),
      confidence: emptyMap(),
      focusFacilityId: ''
    };
  }

  var filters = defaultFilters();
  var mainActive = false;
  var labelActive = false;
  var hiddenByUser = false;
  var panel = null;
  var rebuildTimer = null;
  var liveTimer = null;
  var restoreTimer = null;
  var dirty = false;

  var conveyancePane = map.getPane('pane_conveyance');
  if (!conveyancePane) {
    conveyancePane = map.createPane('pane_conveyance');
    conveyancePane.style.zIndex = '485';
  }

  var conveyanceRenderer = L.canvas({
    pane: 'pane_conveyance',
    padding: 0.12,
    tolerance: 8
  });

  var visibleLayer = L.layerGroup([]);
  var labelLayer = L.layerGroup([]);
  visibleLayer.options = visibleLayer.options || {};
  labelLayer.options = labelLayer.options || {};
  visibleLayer.options.group = mainGroup;
  labelLayer.options.group = labelGroup;

  function controlInputFor(group, isLabel) {
    var labels = document.querySelectorAll('.leaflet-control-layers-overlays label');
    var target = norm(group);
    for (var i = 0; i < labels.length; i++) {
      var label = labels[i];
      var full = label.getAttribute ? (label.getAttribute('data-pt-layer-full-name') || '') : '';
      var text = full || label.textContent || label.innerText || '';
      var n = norm(text);
      if (isLabel !== /^\s*Labels\s+[–-]/i.test(full || text)) continue;
      if (n === target || n.indexOf(target) >= 0 || target.indexOf(n) >= 0) {
        return label.querySelector('input[type="checkbox"]');
      }
    }
    return null;
  }

  function syncControlState() {
    var mainInput = controlInputFor(mainGroup, false);
    var labelInput = controlInputFor(labelGroup, true);
    var wasMain = mainActive;
    mainActive = !!(mainInput && mainInput.checked);
    labelActive = !!(labelInput && labelInput.checked);
    if (!wasMain && mainActive) hiddenByUser = false;
    if (!mainActive) hiddenByUser = false;
  }

  function ownershipColor(rec) {
    var bucket = norm(rec.ob);
    if (bucket === 'federal') return '#2166AC';
    if (bucket === 'state') return '#E67E22';
    if (bucket === 'joint') return '#2166AC';
    if (bucket === 'local') return '#2E8B57';
    if (bucket === 'private') return '#7B3294';
    return '#777777';
  }

  function rankWeight(rec) {
    var rank = norm(rec.rk).replace(/\s+/g, '_');
    if (rank === 'statewide_major') return 4.2;
    if (rank === 'regional_major') return 3.15;
    if (rank === 'medium') return 2.05;
    return 1.35;
  }

  function familyContext(rec) {
    var f = splitValues(rec.ff || rec.pf);
    var hasCvp = f.indexOf('cvp') >= 0;
    var hasSwp = f.indexOf('swp') >= 0;
    if (hasCvp && hasSwp) return 'CVP/SWP';
    if (hasCvp) return 'CVP';
    if (hasSwp) return 'SWP';
    if (f.indexOf('other reclamation') >= 0) return 'Reclamation';
    if (f.indexOf('local/regional public') >= 0) {
      return has(rec.pn) && !/^local$/i.test(String(rec.pn)) ? String(rec.pn) :
        (has(rec.op) ? String(rec.op).split(';')[0] : 'Local/regional');
    }
    return 'Unassigned';
  }

  function ownershipLabel(rec) {
    var b = norm(rec.ob);
    if (b === 'federal') return 'Federal';
    if (b === 'state') return 'California state';
    if (b === 'joint') return 'Joint federal/state';
    if (b === 'local') return 'Local/regional public';
    if (b === 'private') return 'Private';
    return 'Unknown ownership';
  }

  var tooltipCache = {};
  var popupCache = {};

  function makeTooltip(feature) {
    var fid = String(feature && feature.properties ? feature.properties.fid || '' : '');
    if (tooltipCache[fid]) return tooltipCache[fid];
    var rec = facilitiesById[fid] || {};
    tooltipCache[fid] = esc(ownershipLabel(rec) + ' | ' + familyContext(rec)) + '<br/>' + esc(rec.n || 'Water conveyance');
    return tooltipCache[fid];
  }

  function popupRow(label, value) {
    if (!has(value)) return '';
    return '<div class="pt-conv-popup-row"><div class="pt-conv-popup-label">' + esc(label) + '</div><div class="pt-conv-popup-value">' + esc(value) + '</div></div>';
  }

  function popupSection(title) {
    return '<div class="pt-conv-popup-section">' + esc(title) + '</div>';
  }

  function makePopup(feature) {
    var fid = String(feature && feature.properties ? feature.properties.fid || '' : '');
    if (popupCache[fid]) return popupCache[fid];
    var p = feature && feature.properties ? feature.properties : {};
    var rec = facilitiesById[fid] || {};
    var ownerOperator = [rec.oa, rec.op].filter(has).join(' / ');
    var hierarchy = [rec.pd, rec.pu, rec.ps].filter(has).join(' → ');
    var blmFacility = truthy(rec.bx) ?
      ('Crosses BLM-managed land; ' + fmt(rec.bl, 2) + ' mi (' + fmt(rec.bp, 1) + '%)') :
      ('Nearest BLM-managed land: ' + fmt(rec.bd, 2) + ' mi');
    var source = has(rec.gs) ? rec.gs : 'curated source geometry';
    var mappedSegments = has(rec.ssc) ? rec.ssc : (has(rec.sc) ? rec.sc : '');

    var html = '<div class="pt-conv-popup">';
    html += '<div class="pt-conv-popup-title">' + esc(rec.n || 'Water conveyance') + '</div>';
    html += popupRow('Aliases', rec.a);
    html += popupRow('Parent system', rec.par);
    html += popupSection('Facility details');
    html += popupRow('Group / type', [rec.fg, rec.ft].filter(has).join(' · '));
    html += popupRow('Network role', rec.nr);
    html += popupRow('Mapped facility length', has(rec.lm) ? fmt(rec.lm, 2) + ' mi' : '');
    html += popupRow('Status', rec.st);
    html += popupSection('Ownership and project');
    html += popupRow('Ownership', rec.oc);
    html += popupRow('Owner / operator', ownerOperator);
    html += popupRow('Project family', rec.pf);
    html += popupRow('Project / system', rec.pn);
    html += popupRow('Division / unit / subunit', hierarchy);
    html += popupSection('BLM relationship');
    html += popupRow('Facility', blmFacility);
    html += popupRow('BLM field office(s)', rec.bfo);
    html += '<div class="pt-conv-popup-qa">GIS: ' + esc(source) + ' · facility ' + esc(fid) + (has(mappedSegments) ? ' · ' + esc(mappedSegments) + ' mapped segments' : '') + '</div>';
    html += '</div>';
    popupCache[fid] = html;
    return html;
  }

  function baseStyle(feature) {
    var fid = String(feature && feature.properties ? feature.properties.fid || '' : '');
    var rec = facilitiesById[fid] || {};
    return {
      pane: 'pane_conveyance',
      renderer: conveyanceRenderer,
      color: ownershipColor(rec),
      weight: rankWeight(rec),
      opacity: 0.91,
      lineCap: 'round',
      lineJoin: 'round',
      smoothFactor: 1.5,
      interactive: true
    };
  }

  function jointStyle(feature) {
    var fid = String(feature && feature.properties ? feature.properties.fid || '' : '');
    var rec = facilitiesById[fid] || {};
    return {
      pane: 'pane_conveyance',
      renderer: conveyanceRenderer,
      color: '#E67E22',
      weight: Math.max(1.9, rankWeight(rec) * 0.76),
      opacity: 0.98,
      dashArray: '8 6',
      lineCap: 'butt',
      smoothFactor: 1.5,
      interactive: false
    };
  }

  function bindInteractions(feature, layer) {
    layer.bindTooltip(makeTooltip(feature), {
      direction: 'auto',
      sticky: true,
      opacity: 0.95,
      className: 'pt-conv-tooltip'
    });
    layer.bindPopup(makePopup(feature), {
      maxWidth: 540,
      maxHeight: 570
    });
  }

  function systemField(rec, family) {
    if (family === 'cvp') return rec.cd || '';
    if (family === 'swp') return rec.ss || '';
    if (family === 'other reclamation') return rec.os || '';
    if (family === 'local/regional public') return rec.ls || '';
    return '';
  }

  function unitField(rec, family) {
    return family === 'cvp' ? (rec.cu || '') : '';
  }

  function oneSelectedFamily(selected) {
    var keys = selectedKeys(selected);
    return keys.length === 1 ? keys[0] : '';
  }

  function recordPass(rec, overrideSection, overrideValue) {
    if (filters.focusFacilityId && String(rec.fid) !== String(filters.focusFacilityId)) return false;

    var own = overrideSection === 'own' ? (function(){ var o={}; o[overrideValue]=true; return o; })() : filters.ownership;
    var pf = overrideSection === 'pf' ? (function(){ var o={}; o[overrideValue]=true; return o; })() : filters.projectFamily;
    var grp = overrideSection === 'grp' ? (function(){ var o={}; o[overrideValue]=true; return o; })() : filters.groups;
    var rank = overrideSection === 'rank' ? (function(){ var o={}; o[overrideValue]=true; return o; })() : filters.ranks;
    var conf = overrideSection === 'conf' ? (function(){ var o={}; o[overrideValue]=true; return o; })() : filters.confidence;

    if (!facetPass(rec.ob, own)) return false;
    if (!facetPass(rec.ff || rec.pf, pf)) return false;
    if (!facetPass(rec.fg, grp)) return false;
    if (!facetPass(rec.rk, rank)) return false;
    if (!facetPass(rec.gc, conf)) return false;

    var family = oneSelectedFamily(filters.projectFamily);
    if (filters.system && norm(systemField(rec, family)) !== filters.system) return false;
    if (filters.unit && norm(unitField(rec, family)) !== filters.unit) return false;

    if (filters.crossesBlm && !truthy(rec.bx)) return false;
    if (filters.withinBlm && num(rec.bd, Infinity) > filters.maxBlmMiles) return false;
    return true;
  }

  function filteredFeatures() {
    return lineFC.features.filter(function(feature) {
      var fid = String(feature && feature.properties ? feature.properties.fid || '' : '');
      return recordPass(facilitiesById[fid] || {}, '', '');
    });
  }

  function filteredFacilityIds(features) {
    var out = {};
    features.forEach(function(feature) {
      var fid = String(feature && feature.properties ? feature.properties.fid || '' : '');
      if (fid) out[fid] = true;
    });
    return out;
  }

  function removeLinesImmediately() {
    conveyancePane.style.display = 'none';
    if (map.hasLayer(visibleLayer)) map.removeLayer(visibleLayer);
    if (map.hasLayer(labelLayer)) map.removeLayer(labelLayer);
    setTimeout(function() {
      visibleLayer.clearLayers();
      labelLayer.clearLayers();
    }, 0);
  }

  function rebuildLines(features) {
    visibleLayer.clearLayers();
    if (!mainActive) return;
    var fc = {type:'FeatureCollection', features:features};
    var joint = {
      type:'FeatureCollection',
      features:features.filter(function(feature) {
        var fid = String(feature && feature.properties ? feature.properties.fid || '' : '');
        return norm((facilitiesById[fid] || {}).ob) === 'joint';
      })
    };
    visibleLayer.addLayer(L.geoJSON(fc, {
      renderer: conveyanceRenderer,
      style: baseStyle,
      onEachFeature: bindInteractions
    }));
    if (joint.features.length) {
      visibleLayer.addLayer(L.geoJSON(joint, {
        renderer: conveyanceRenderer,
        style: jointStyle
      }));
    }
    if (!map.hasLayer(visibleLayer)) visibleLayer.addTo(map);
    conveyancePane.style.display = '';
    conveyancePane.style.visibility = 'visible';
  }

  function rebuildLabels(activeIds) {
    labelLayer.clearLayers();
    if (!(mainActive && labelActive)) {
      if (map.hasLayer(labelLayer)) map.removeLayer(labelLayer);
      return;
    }
    var zoom = map.getZoom();
    labelFC.features.forEach(function(feature) {
      var p = feature && feature.properties ? feature.properties : {};
      if (!activeIds[String(p.fid || '')]) return;
      var zmin = num(p.zmin, 11), zmax = num(p.zmax, 20);
      if (zoom < zmin || zoom > zmax) return;
      var coords = feature && feature.geometry ? feature.geometry.coordinates : null;
      if (!coords || coords.length < 2) return;
      labelLayer.addLayer(L.marker([coords[1], coords[0]], {
        icon: L.divIcon({
          className: 'pt-conv-label-icon',
          html: '<span>' + esc(p.lbl || '') + '</span>',
          iconSize: null
        }),
        pane: 'pane_labels_poly',
        interactive: false
      }));
    });
    if (!map.hasLayer(labelLayer)) labelLayer.addTo(map);
  }

  function setStatus(text, spinning) {
    if (!panel) return;
    var status = panel.querySelector('.pt-conv-status-text');
    var spinner = panel.querySelector('.pt-conv-spinner');
    if (status) status.textContent = text;
    if (spinner) spinner.style.display = spinning ? 'inline-block' : 'none';
  }

  function updateCount(features) {
    if (!panel) return;
    var n = features.length;
    if (filters.focusFacilityId) {
      var rec = facilitiesById[String(filters.focusFacilityId)] || {};
      setStatus('Focused: ' + (rec.n || '1 facility') + ' · ' + (rec.ssc || rec.sc || 1) + ' mapped segments', false);
    } else {
      setStatus('Showing ' + n.toLocaleString() + ' of ' + facilityRows.length.toLocaleString() + ' facilities', false);
    }
  }

  function updateFacetAvailability() {
    if (!panel || filters.focusFacilityId) return;
    panel.querySelectorAll('input[data-filter]').forEach(function(input) {
      var section = input.getAttribute('data-filter');
      var value = input.value;
      var count = 0;
      facilityRows.forEach(function(rec) {
        if (recordPass(rec, section, value)) count++;
      });
      var label = input.closest('label');
      var countNode = label ? label.querySelector('.pt-conv-opt-count') : null;
      if (countNode) countNode.textContent = count ? ' ' + count : ' 0';
      var zero = count === 0 && !input.checked;
      input.disabled = zero;
      if (label) label.classList.toggle('pt-conv-zero', zero);
    });
  }

  function performRebuild() {
    syncControlState();
    if (!mainActive) {
      removeLinesImmediately();
      updatePanelVisibility();
      return;
    }
    var features = filteredFeatures();
    rebuildLines(features);
    var ids = filteredFacilityIds(features);
    rebuildLabels(ids);
    updateCount(features);
    updateFacetAvailability();
    updatePanelVisibility();
  }

  function scheduleRebuild() {
    if (rebuildTimer) clearTimeout(rebuildTimer);
    setStatus('Filtering…', true);
    rebuildTimer = setTimeout(function() {
      rebuildTimer = null;
      performRebuild();
    }, 25);
  }

  function selectedMapFromPanel(filterName) {
    var out = {};
    if (!panel) return out;
    panel.querySelectorAll('input[data-filter="' + filterName + '"]:checked').forEach(function(input) {
      out[input.value] = true;
    });
    return out;
  }

  function familySystemLabel(family) {
    if (family === 'cvp') return 'All CVP divisions';
    if (family === 'swp') return 'All SWP systems/branches';
    if (family === 'other reclamation') return 'All Reclamation projects';
    if (family === 'local/regional public') return 'All local/regional systems';
    return 'Select one project family';
  }

  function populateSelect(select, values, firstLabel, current) {
    if (!select) return;
    var unique = {};
    values.forEach(function(v) {
      if (has(v)) unique[norm(v)] = String(v);
    });
    var keys = Object.keys(unique).sort(function(a,b) {
      return unique[a].localeCompare(unique[b]);
    });
    select.innerHTML = '<option value="">' + esc(firstLabel) + '</option>' + keys.map(function(k) {
      return '<option value="' + esc(k) + '">' + esc(unique[k]) + '</option>';
    }).join('');
    if (current && keys.indexOf(current) >= 0) select.value = current;
  }

  function updateDependentControls() {
    if (!panel) return;
    var draftPf = selectedMapFromPanel('pf');
    var family = oneSelectedFamily(draftPf);
    var systemSelect = panel.querySelector('.pt-conv-system-select');
    var unitSelect = panel.querySelector('.pt-conv-unit-select');
    var rows = facilityRows.filter(function(rec) {
      return family ? facetPass(rec.ff || rec.pf, (function(){var o={};o[family]=true;return o;})()) : false;
    });
    var oldSystem = systemSelect ? systemSelect.value : '';
    populateSelect(systemSelect, rows.map(function(rec){ return systemField(rec, family); }), familySystemLabel(family), oldSystem);
    if (systemSelect) {
      systemSelect.disabled = !family;
      if (!family) systemSelect.value = '';
    }
    var selectedSystem = systemSelect && !systemSelect.disabled ? systemSelect.value : '';
    var unitRows = rows.filter(function(rec) {
      return !selectedSystem || norm(systemField(rec, family)) === selectedSystem;
    });
    var oldUnit = unitSelect ? unitSelect.value : '';
    populateSelect(unitSelect, unitRows.map(function(rec){ return unitField(rec, family); }), 'All units', oldUnit);
    var hasUnits = unitRows.some(function(rec){ return has(unitField(rec, family)); });
    if (unitSelect) {
      unitSelect.disabled = !(family === 'cvp' && selectedSystem && hasUnits);
      if (unitSelect.disabled) unitSelect.value = '';
    }
  }

  function readDraftFilters() {
    filters.ownership = selectedMapFromPanel('own');
    filters.projectFamily = selectedMapFromPanel('pf');
    filters.groups = selectedMapFromPanel('grp');
    filters.ranks = selectedMapFromPanel('rank');
    filters.confidence = selectedMapFromPanel('conf');
    var systemSelect = panel.querySelector('.pt-conv-system-select');
    var unitSelect = panel.querySelector('.pt-conv-unit-select');
    filters.system = systemSelect && !systemSelect.disabled ? systemSelect.value : '';
    filters.unit = unitSelect && !unitSelect.disabled ? unitSelect.value : '';
    filters.crossesBlm = panel.querySelector('.pt-conv-crosses').checked;
    filters.withinBlm = panel.querySelector('.pt-conv-within').checked;
    filters.maxBlmMiles = Math.max(0, num(panel.querySelector('.pt-conv-distance-value').value, 5));
  }

  function setDirty(value) {
    dirty = value;
    if (!panel) return;
    var btn = panel.querySelector('[data-action="apply-filters"]');
    if (btn) btn.textContent = dirty ? 'Apply filters *' : 'Apply filters';
  }

  function applyFilters() {
    if (!panel) return;
    readDraftFilters();
    filters.focusFacilityId = '';
    panel.querySelector('.pt-conv-find').value = '';
    setDirty(false);
    scheduleRebuild();
  }

  function maybeLiveApply() {
    setDirty(true);
    if (!panel || !panel.querySelector('.pt-conv-live').checked) return;
    clearTimeout(liveTimer);
    liveTimer = setTimeout(applyFilters, 180);
  }

  function setChecks(target, checked) {
    panel.querySelectorAll('input[data-filter="' + target + '"]').forEach(function(input) {
      input.disabled = false;
      input.checked = checked;
    });
    if (target === 'pf') updateDependentControls();
    maybeLiveApply();
  }

  function resetDraft() {
    panel.querySelectorAll('input[data-filter]').forEach(function(input) {
      input.disabled = false;
      input.checked = false;
    });
    panel.querySelector('.pt-conv-crosses').checked = false;
    panel.querySelector('.pt-conv-within').checked = false;
    panel.querySelector('.pt-conv-distance-value').value = '5';
    panel.querySelector('.pt-conv-distance-value').disabled = true;
    panel.querySelector('.pt-conv-quick').value = '';
    updateDependentControls();
  }

  function clearFilters() {
    filters = defaultFilters();
    resetDraft();
    panel.querySelector('.pt-conv-find').value = '';
    setDirty(false);
    scheduleRebuild();
  }

  function applyQuickView(value) {
    resetDraft();
    if (value === 'cvp') panel.querySelector('input[data-filter="pf"][value="cvp"]').checked = true;
    if (value === 'swp') panel.querySelector('input[data-filter="pf"][value="swp"]').checked = true;
    if (value === 'local') panel.querySelector('input[data-filter="pf"][value="local/regional public"]').checked = true;
    if (value === 'blm-cross') panel.querySelector('.pt-conv-crosses').checked = true;
    if (value === 'blm-5') {
      panel.querySelector('.pt-conv-within').checked = true;
      panel.querySelector('.pt-conv-distance-value').disabled = false;
      panel.querySelector('.pt-conv-distance-value').value = '5';
    }
    if (value === 'major') {
      panel.querySelector('input[data-filter="rank"][value="statewide_major"]').checked = true;
      panel.querySelector('input[data-filter="rank"][value="regional_major"]').checked = true;
    }
    updateDependentControls();
    applyFilters();
  }

  var findLookup = {};
  facilityRows.forEach(function(rec) {
    if (!has(rec.fid)) return;
    [rec.n, rec.lbl].concat(splitValues(rec.a)).forEach(function(v) {
      if (has(v)) findLookup[norm(v)] = String(rec.fid);
    });
  });

  function focusFacility() {
    var input = panel.querySelector('.pt-conv-find');
    var query = norm(input.value);
    if (!query) return;
    var fid = findLookup[query];
    if (!fid) {
      var matches = facilityRows.filter(function(rec) {
        return norm(rec.n).indexOf(query) >= 0 || norm(rec.a).indexOf(query) >= 0;
      });
      if (matches.length === 1) fid = String(matches[0].fid);
    }
    if (!fid) {
      setStatus('No unique facility match', false);
      return;
    }
    filters.focusFacilityId = fid;
    var rec = facilitiesById[fid] || {};
    input.value = rec.n || input.value;
    scheduleRebuild();
    var xmin=num(rec.xmin,NaN), ymin=num(rec.ymin,NaN), xmax=num(rec.xmax,NaN), ymax=num(rec.ymax,NaN);
    if ([xmin,ymin,xmax,ymax].every(isFinite)) {
      map.fitBounds([[ymin,xmin],[ymax,xmax]], {
        padding:[38,38], maxZoom:11, animate:false
      });
    }
  }

  function clearFocus() {
    filters.focusFacilityId = '';
    panel.querySelector('.pt-conv-find').value = '';
    scheduleRebuild();
  }

  function checkbox(value, label, filter) {
    return '<label class="pt-conv-check"><input type="checkbox" data-filter="' + filter + '" value="' + esc(value) + '"> ' + esc(label) + '<span class="pt-conv-opt-count"></span></label>';
  }

  function facetButtons(filter) {
    return '<span class="pt-conv-mini-buttons"><button type="button" data-action="any" data-target="' + filter + '">Any</button><button type="button" data-action="all" data-target="' + filter + '">All</button></span>';
  }

  function swatch(color, label, joint, weight) {
    if (joint) return '<div class="pt-conv-swatch-row"><span class="pt-conv-joint"><i></i></span><span>' + esc(label) + '</span></div>';
    return '<div class="pt-conv-swatch-row"><span class="pt-conv-line" style="border-top-color:' + color + ';border-top-width:' + (weight || 3) + 'px"></span><span>' + esc(label) + '</span></div>';
  }

  function ensureCss() {
    if (document.getElementById('pt-conveyance-controller-css')) return;
    var style = document.createElement('style');
    style.id = 'pt-conveyance-controller-css';
    style.textContent =
      '.pt-conv-panel{background:rgba(246,239,222,.97);border:1px solid rgba(112,103,83,.55);border-radius:6px;box-shadow:0 1px 5px rgba(0,0,0,.25);padding:6px 7px;width:360px;max-width:calc(100vw - 18px);max-height:calc(100vh - 166px);overflow:auto;font-family:Arial,sans-serif;font-size:11px;line-height:1.12;color:#222;box-sizing:border-box;margin-left:8px!important;margin-bottom:74px!important;}' +
      '.pt-conv-panel.pt-conv-floating{position:absolute!important;margin:0!important;z-index:10020!important;}' +
      '.pt-conv-head{display:flex;justify-content:space-between;align-items:center;gap:7px;font-weight:700;font-size:12.5px;margin-bottom:1px;}' +
      '.pt-conv-head-actions{display:inline-flex;align-items:center;gap:1px;flex:0 0 auto;}' +
      '.pt-conv-float,.pt-conv-close{border:0!important;background:transparent!important;color:#777!important;line-height:1!important;padding:0 2px!important;cursor:pointer!important;}' +
      '.pt-conv-float{font-size:15px!important;cursor:pointer!important;touch-action:none;}' +
      '.pt-conv-close{font-size:18px!important;}' +
      '.pt-conv-float:hover,.pt-conv-close:hover{color:#222!important;background:rgba(112,103,83,.12)!important;}' +
      '.pt-conv-status{display:flex;align-items:center;gap:4px;font-size:10.2px;color:#444;min-height:13px;margin-bottom:2px;}' +
      '.pt-conv-spinner{display:none;width:9px;height:9px;border:2px solid #c6baa1;border-top-color:#555;border-radius:50%;animation:ptConvSpin .65s linear infinite;}' +
      '@keyframes ptConvSpin{to{transform:rotate(360deg)}}' +
      '.pt-conv-toolbar{display:grid;grid-template-columns:minmax(0,1fr) auto auto auto;gap:3px;align-items:center;margin:2px 0;}' +
      '.pt-conv-findrow{display:grid;grid-template-columns:minmax(0,1fr) auto auto auto;gap:3px;align-items:center;margin:2px 0 3px;}' +
      '.pt-conv-panel select,.pt-conv-panel input[type=search],.pt-conv-panel input[type=number]{font-size:10.6px;border:1px solid #b8ad95;border-radius:3px;padding:2px 3px;box-sizing:border-box;min-width:0;height:22px;}' +
      '.pt-conv-find{width:100%;}' +
      '.pt-conv-section{margin-top:4px;padding-top:3px;border-top:1px solid rgba(120,110,90,.25);}' +
      '.pt-conv-section-title{font-weight:700;margin-bottom:1px;display:flex;justify-content:space-between;align-items:center;}' +
      '.pt-conv-check-row{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:1px 5px;align-items:center;}' +
      '.pt-conv-check{white-space:nowrap;display:inline-flex;align-items:center;gap:1px;min-width:0;font-size:10.5px;}' +
      '.pt-conv-check input{margin:1px 2px 1px 0;}' +
      '.pt-conv-opt-count{font-size:8.8px;color:#777;}' +
      '.pt-conv-zero{color:#aaa;}' +
      '.pt-conv-grid2{display:grid;grid-template-columns:1fr 1fr;gap:0 4px;}' +
      '.pt-conv-swatch-row{display:flex;align-items:center;gap:4px;margin:0;}' +
      '.pt-conv-line{display:inline-block;width:18px;height:0;border-top:3px solid #777;flex:0 0 18px;}' +
      '.pt-conv-joint{position:relative;display:inline-block;width:18px;height:5px;border-top:3px solid #2166AC;flex:0 0 18px;}' +
      '.pt-conv-joint i{position:absolute;left:0;right:0;top:-3px;border-top:2px dashed #E67E22;}' +
      '.pt-conv-mini-buttons{display:inline-flex;gap:2px;}' +
      '.pt-conv-panel button{font-size:10px;padding:1px 4px;border:1px solid #b9ad94;border-radius:3px;background:#f8f5ec;cursor:pointer;white-space:nowrap;}' +
      '.pt-conv-panel button:hover{background:#fff;}' +
      '.pt-conv-select-row{display:grid;grid-template-columns:1fr 1fr;gap:4px;margin-top:2px;}' +
      '.pt-conv-distance{display:flex;flex-wrap:wrap;align-items:center;gap:3px 5px;}' +
      '.pt-conv-distance-value{width:48px;}' +
      '.pt-conv-advanced summary{cursor:pointer;font-weight:700;margin-top:4px;}' +
      '.pt-conv-note{font-size:9.3px;color:#5d574c;margin-top:3px;padding-top:2px;border-top:1px solid rgba(112,103,83,.25);}' +
      '.pt-conv-tooltip{white-space:nowrap!important;max-width:none!important;font-size:11.5px!important;line-height:1.22!important;padding:5px 7px!important;}' +
      '.pt-conv-label-icon{background:transparent;border:0;box-shadow:none;}' +
      '.pt-conv-label-icon span{background:rgba(255,255,255,.78);color:#222;font-size:10.5px;font-weight:600;white-space:nowrap;padding:1px 2px;text-shadow:0 0 2px #fff,0 0 2px #fff;}' +
      '.pt-conv-popup{font-size:12px;line-height:1.24;min-width:310px;}' +
      '.pt-conv-popup-title{font-weight:700;font-size:14px;margin-bottom:5px;border-bottom:1px solid #ddd;padding-bottom:3px;}' +
      '.pt-conv-popup-section{font-weight:700;background:#f2eee4;border-left:3px solid #9b8f76;margin:7px 0 3px;padding:2px 5px;}' +
      '.pt-conv-popup-row{display:grid;grid-template-columns:118px 1fr;gap:7px;margin:2px 0;}' +
      '.pt-conv-popup-label{font-weight:600;color:#444;}' +
      '.pt-conv-popup-value{color:#111;}' +
      '.pt-conv-popup-qa{font-size:9.8px;color:#777;margin-top:7px;padding-top:4px;border-top:1px solid #ddd;white-space:normal;}';
    document.head.appendChild(style);
  }

  function buildPanel() {
    ensureCss();
    var ctl = L.control({position:'bottomleft'});
    ctl.onAdd = function() {
      var div = L.DomUtil.create('div','leaflet-control pt-conv-panel');
      div.style.display = 'none';
      var listId = 'pt-conv-find-list-' + Math.random().toString(36).slice(2);
      var html = '';
      html += '<div class="pt-conv-head"><span>Water conveyance | BRIM mapped</span><span class="pt-conv-head-actions"><button type="button" class="pt-conv-float pt-map-card-dock" title="Undock Water Conveyance legend" aria-label="Undock Water Conveyance legend">&#x2197;</button><button type="button" class="pt-conv-close" title="Hide panel">×</button></span></div>';
      html += '<div class="pt-conv-status"><span class="pt-conv-spinner"></span><span class="pt-conv-status-text">Ready</span></div>';
      html += '<div class="pt-conv-toolbar"><select class="pt-conv-quick"><option value="">Quick view…</option><option value="all">All conveyance</option><option value="cvp">CVP</option><option value="swp">SWP</option><option value="local">Local/regional systems</option><option value="blm-cross">Crosses BLM</option><option value="blm-5">Within 5 mi of BLM</option><option value="major">Statewide/regional major</option></select><label class="pt-conv-check" title="Apply filter changes automatically"><input class="pt-conv-live" type="checkbox"> Auto</label><button type="button" data-action="apply-filters">Apply filters</button><button type="button" data-action="clear-filters">Clear</button></div>';
      html += '<div class="pt-conv-findrow"><input class="pt-conv-find" type="search" list="' + listId + '" placeholder="Find facility…"><datalist id="' + listId + '"></datalist><button type="button" data-action="find">Find</button><button type="button" data-action="clear-focus">Clear focus</button><label class="pt-conv-check" title="Show facility labels"><input class="pt-conv-lbl" type="checkbox"> lbl</label></div>';
      html += '<div class="pt-conv-section"><div class="pt-conv-section-title"><span>Ownership / line color</span>' + facetButtons('own') + '</div><div class="pt-conv-grid2"><div>' + swatch('#2166AC','Federal') + swatch('#E67E22','California state') + swatch('#2166AC','Joint federal/state',true) + '</div><div>' + swatch('#2E8B57','Local/regional public') + swatch('#7B3294','Private') + swatch('#777777','Unknown') + '</div></div><div class="pt-conv-check-row">' + checkbox('federal','Federal','own') + checkbox('state','State','own') + checkbox('joint','Joint','own') + checkbox('local','Local','own') + checkbox('private','Private','own') + checkbox('unknown','Unknown','own') + '</div></div>';
      html += '<div class="pt-conv-section"><div class="pt-conv-section-title"><span>Project family and hierarchy</span>' + facetButtons('pf') + '</div><div class="pt-conv-check-row">' + checkbox('cvp','CVP','pf') + checkbox('swp','SWP','pf') + checkbox('other reclamation','Other Reclamation','pf') + checkbox('local/regional public','Local/regional','pf') + checkbox('unknown/unassigned','Unknown','pf') + '</div><div class="pt-conv-select-row"><select class="pt-conv-system-select" disabled><option>Select one project family</option></select><select class="pt-conv-unit-select" disabled><option>All units</option></select></div></div>';
      html += '<div class="pt-conv-section"><div class="pt-conv-section-title"><span>Conveyance group</span>' + facetButtons('grp') + '</div><div class="pt-conv-check-row">' + checkbox('open conveyance','Open','grp') + checkbox('closed conveyance','Closed','grp') + checkbox('power conveyance','Power','grp') + checkbox('distribution/local','Distribution/local','grp') + checkbox('drainage','Drainage','grp') + checkbox('flood conveyance','Flood','grp') + checkbox('other/unknown','Other','grp') + '</div></div>';
      html += '<div class="pt-conv-section"><div class="pt-conv-section-title"><span>BLM-managed-land relationship</span></div><div class="pt-conv-distance"><label class="pt-conv-check"><input class="pt-conv-crosses" type="checkbox"> Crosses BLM</label><label class="pt-conv-check"><input class="pt-conv-within" type="checkbox"> Within</label><input class="pt-conv-distance-value" type="number" min="0" step="0.1" value="5" disabled><span>mi of BLM</span></div></div>';
      html += '<details class="pt-conv-advanced"><summary>Advanced filters</summary><div class="pt-conv-section"><div class="pt-conv-section-title"><span>Display rank / line width</span>' + facetButtons('rank') + '</div><div class="pt-conv-check-row">' + checkbox('statewide_major','Statewide','rank') + checkbox('regional_major','Regional','rank') + checkbox('medium','Medium','rank') + checkbox('local_supporting','Standard','rank') + '</div></div><div class="pt-conv-section"><div class="pt-conv-section-title"><span>Geometry confidence</span>' + facetButtons('conf') + '</div><div class="pt-conv-check-row">' + checkbox('high','High','conf') + checkbox('medium','Medium','conf') + checkbox('low','Low','conf') + '</div></div></details>';
      html += '<div class="pt-conv-note">Empty = Any. Apply once, or use Auto for debounced updates. Find only focuses the map.</div>';
      div.innerHTML = html;
      L.DomEvent.disableClickPropagation(div);
      L.DomEvent.disableScrollPropagation(div);
      return div;
    };
    ctl.addTo(map);
    panel = el.querySelector('.pt-conv-panel');
    var dataList = panel.querySelector('datalist');
    if (dataList) {
      dataList.innerHTML = facilityRows.slice().sort(function(a,b){return String(a.n||'').localeCompare(String(b.n||''));}).map(function(rec){return '<option value="' + esc(rec.n || '') + '"></option>';}).join('');
    }
    wirePanel();
  }

  function enablePanelFloating() {
    if (!panel) return;
    var handle = panel.querySelector('.pt-conv-float');
    if (!handle) return;
    var dockParent = panel.parentNode;
    var dockNext = panel.nextSibling;
    var floating = false;
    var drag = null;

    function clampPosition(left, top) {
      var mapRect = map.getContainer().getBoundingClientRect();
      var panelRect = panel.getBoundingClientRect();
      return {
        left: Math.max(4, Math.min(left, Math.max(4, mapRect.width - panelRect.width - 4))),
        top: Math.max(4, Math.min(top, Math.max(4, mapRect.height - panelRect.height - 4)))
      };
    }

    function beginFloating() {
      if (floating) return;
      var mapContainer = map.getContainer();
      var mapRect = mapContainer.getBoundingClientRect();
      var panelRect = panel.getBoundingClientRect();
      var pos = clampPosition(panelRect.left - mapRect.left, panelRect.top - mapRect.top);
      mapContainer.appendChild(panel);
      panel.classList.add('pt-conv-floating');
      panel.style.left = pos.left + 'px';
      panel.style.top = pos.top + 'px';
      panel.style.right = 'auto';
      panel.style.bottom = 'auto';
      floating = true;
    }

    function dockPanel() {
      if (!floating) return;
      panel.classList.remove('pt-conv-floating');
      panel.style.left = '';
      panel.style.top = '';
      panel.style.right = '';
      panel.style.bottom = '';
      if (dockNext && dockNext.parentNode === dockParent) dockParent.insertBefore(panel, dockNext);
      else dockParent.appendChild(panel);
      floating = false;
    }

    function movePanel(clientX, clientY) {
      if (!drag) return;
      var pos = clampPosition(clientX - drag.mapLeft - drag.offsetX, clientY - drag.mapTop - drag.offsetY);
      panel.style.left = pos.left + 'px';
      panel.style.top = pos.top + 'px';
    }

    handle.addEventListener('pointerdown', function(e) {
      if (e.button !== undefined && e.button !== 0) return;
      e.preventDefault();
      e.stopPropagation();
      beginFloating();
      var mapRect = map.getContainer().getBoundingClientRect();
      var panelRect = panel.getBoundingClientRect();
      drag = {
        offsetX: e.clientX - panelRect.left,
        offsetY: e.clientY - panelRect.top,
        mapLeft: mapRect.left,
        mapTop: mapRect.top
      };
      handle.setPointerCapture && handle.setPointerCapture(e.pointerId);
      document.body.style.userSelect = 'none';
    });

    handle.addEventListener('pointermove', function(e) {
      if (!drag) return;
      e.preventDefault();
      movePanel(e.clientX, e.clientY);
    });

    function endDrag(e) {
      if (!drag) return;
      drag = null;
      document.body.style.userSelect = '';
      if (e && handle.releasePointerCapture) {
        try { handle.releasePointerCapture(e.pointerId); } catch (ignore) {}
      }
    }

    handle.addEventListener('pointerup', endDrag);
    handle.addEventListener('pointercancel', endDrag);
    handle.addEventListener('dblclick', function(e) {
      e.preventDefault();
      e.stopPropagation();
      dockPanel();
    });

    map.on('resize', function() {
      if (!floating || panel.style.display === 'none') return;
      var pos = clampPosition(parseFloat(panel.style.left) || 4, parseFloat(panel.style.top) || 4);
      panel.style.left = pos.left + 'px';
      panel.style.top = pos.top + 'px';
    });
  }

  function wirePanel() {
    if (!panel) return;
    panel.querySelector('.pt-conv-close').addEventListener('click', function(e) {
      e.preventDefault(); e.stopPropagation(); hiddenByUser = true; updatePanelVisibility();
    });
    panel.addEventListener('click', function(e) {
      var btn = e.target.closest('button[data-action]');
      if (!btn) return;
      var action = btn.getAttribute('data-action');
      var target = btn.getAttribute('data-target');
      if (action === 'any') setChecks(target, false);
      if (action === 'all') setChecks(target, true);
      if (action === 'apply-filters') applyFilters();
      if (action === 'clear-filters') clearFilters();
      if (action === 'find') focusFacility();
      if (action === 'clear-focus') clearFocus();
    });
    panel.querySelectorAll('input[data-filter]').forEach(function(input) {
      input.addEventListener('change', function() {
        if (input.getAttribute('data-filter') === 'pf') updateDependentControls();
        maybeLiveApply();
      });
    });
    panel.querySelector('.pt-conv-system-select').addEventListener('change', function() {
      updateDependentControls(); maybeLiveApply();
    });
    panel.querySelector('.pt-conv-unit-select').addEventListener('change', maybeLiveApply);
    panel.querySelector('.pt-conv-crosses').addEventListener('change', maybeLiveApply);
    panel.querySelector('.pt-conv-within').addEventListener('change', function(e) {
      panel.querySelector('.pt-conv-distance-value').disabled = !e.target.checked;
      maybeLiveApply();
    });
    panel.querySelector('.pt-conv-distance-value').addEventListener('input', maybeLiveApply);
    panel.querySelector('.pt-conv-distance-value').addEventListener('keydown', function(e) {
      if (e.key === 'Enter') applyFilters();
    });
    panel.querySelector('.pt-conv-quick').addEventListener('change', function(e) {
      if (e.target.value) applyQuickView(e.target.value);
    });
    panel.querySelector('.pt-conv-live').addEventListener('change', function(e) {
      if (e.target.checked && dirty) applyFilters();
    });
    panel.querySelector('.pt-conv-find').addEventListener('keydown', function(e) {
      if (e.key === 'Enter') { e.preventDefault(); focusFacility(); }
    });
    panel.querySelector('.pt-conv-lbl').addEventListener('change', function(e) {
      var input = controlInputFor(labelGroup, true);
      if (input && input.checked !== e.target.checked) input.click();
      setTimeout(function(){ syncControlState(); performRebuild(); }, 0);
    });
  }

  function updatePanelVisibility() {
    if (!panel) return;
    panel.style.display = (mainActive && !hiddenByUser) ? 'block' : 'none';
    var lbl = panel.querySelector('.pt-conv-lbl');
    if (lbl) lbl.checked = labelActive;
  }

  function hideDuringMovement() {
    if (!mainActive) return;
    conveyancePane.style.visibility = 'hidden';
    if (map.hasLayer(labelLayer)) map.removeLayer(labelLayer);
  }

  function restoreAfterBasemap() {
    clearTimeout(restoreTimer);
    restoreTimer = setTimeout(function() {
      var pending = 0, finished = false;
      function finish() {
        if (finished) return;
        finished = true;
        conveyancePane.style.visibility = 'visible';
        var features = filteredFeatures();
        rebuildLabels(filteredFacilityIds(features));
      }
      map.eachLayer(function(layer) {
        if (layer instanceof L.TileLayer && map.hasLayer(layer) && layer._loading) {
          pending++;
          layer.once('load', function() {
            pending--;
            if (pending <= 0) finish();
          });
        }
      });
      if (pending === 0) setTimeout(finish, 70);
      setTimeout(finish, 900);
    }, 40);
  }

  buildPanel();
  updateDependentControls();
  syncControlState();
  performRebuild();

  map.on('overlayadd', function(evt) {
    if (!evt || (evt.name !== mainGroup && evt.name !== labelGroup)) return;
    syncControlState();
    if (evt.name === mainGroup) hiddenByUser = false;
    scheduleRebuild();
  });

  map.on('overlayremove', function(evt) {
    if (!evt || (evt.name !== mainGroup && evt.name !== labelGroup)) return;
    syncControlState();
    if (evt.name === mainGroup || !mainActive) {
      removeLinesImmediately();
      updatePanelVisibility();
      return;
    }
    if (evt.name === labelGroup) {
      if (map.hasLayer(labelLayer)) map.removeLayer(labelLayer);
      updatePanelVisibility();
    }
  });

  map.on('zoomstart movestart', hideDuringMovement);
  map.on('zoomend moveend', restoreAfterBasemap);

  setTimeout(function(){ syncControlState(); performRebuild(); }, 300);
}

)---"

  htmlwidgets::onRender(m, js, data = controller_data)
}

# Kept as a compatibility no-op because the browser controller above now owns
# both filtering and the bottom-left legend/query panel.
pt_add_brim_mapped_conveyance_legend <- function(m, map_display) {
  m
}


# ==== 20. RWQCB regions layer ===============================================
##
## PURPOSE:
##   Add RWQCB regional boundaries as a cached local Reference layer.
##
## DESIGN:
##   - Polygons use the State Water Board service colors with a transparent fill.
##   - Hover is a one-line region identifier.
##   - Click popup is intentionally tiny and only provides the RWQCB page link.
##   - Labels are a separate toggleable layer so the colored polygons can be
##     used without text clutter.

pt_add_rwqcb_regions_layer <- function(m, rwqcb_regions, map_display) {
  
  if (!isTRUE(map_display$add_rwqcb_regions)) {
    return(m)
  }
  
  if (!inherits(rwqcb_regions, "sf") || nrow(rwqcb_regions) == 0) {
    message("RWQCB regions layer is empty or missing; skipping.")
    return(m)
  }
  
  message("Adding RWQCB regions: ", nrow(rwqcb_regions))
  
  rwqcb_group <- pt_layer_group_name("RWQCB Regions")
  rwqcb_label_group <- pt_layer_group_name("Labels: RWQCB Regions")
  
  ## Use a point guaranteed to be inside each polygon for labels.  This is
  ## evaluated on the cached WGS84 layer at final map build time, not during
  ## browser interaction, so the cost is trivial for nine regions.
  rwqcb_label_pts <- suppressWarnings(sf::st_point_on_surface(rwqcb_regions))
  
  m <- m |>
    leaflet::addPolygons(
      data = rwqcb_regions,
      group = rwqcb_group,
      fill = TRUE,
      fillColor = ~rwqcb_fill_col,
      fillOpacity = ~rwqcb_fill_opacity,
      color = ~rwqcb_stroke_col,
      weight = ~rwqcb_stroke_weight,
      opacity = 0.95,
      label = ~rwqcb_hover_text,
      labelOptions = leaflet::labelOptions(
        direction = "auto",
        textsize = "12px",
        opacity = 0.95,
        sticky = TRUE
      ),
      popup = ~popup_html,
      options = leaflet::pathOptions(
        pane = "pane_county",
        interactive = TRUE
      ),
      highlightOptions = leaflet::highlightOptions(
        color = "#111111",
        weight = 2.8,
        opacity = 1,
        fillOpacity = 0.40,
        bringToFront = TRUE
      )
    )
  
  if (isTRUE(map_display$add_labels)) {
    m <- m |>
      leaflet::addLabelOnlyMarkers(
        data = rwqcb_label_pts,
        group = rwqcb_label_group,
        label = ~rwqcb_label_text,
        labelOptions = leaflet::labelOptions(
          noHide = TRUE,
          direction = "center",
          textOnly = TRUE,
          opacity = 1,
          className = "pt-label pt-label-rwqcb"
        ),
        options = leaflet::markerOptions(
          pane = "pane_labels_poly",
          interactive = FALSE
        )
      )
  }
  
  m
}

# ==== Water district layer ===================================================
##
## PURPOSE:
##   Add water-district polygons as a reference layer.
##
## DESIGN:
##   - Very faint fill preserves clickability without visually dominating.
##   - Colored outlines show district boundaries.
##   - Strong hover highlight makes the active district obvious.
##   - Cache-building code sorts polygons largest-to-smallest so smaller
##     interior polygons draw on top of larger surrounding polygons.
##

pt_add_water_districts_layer <- function(m, water_districts, map_display) {
  
  if (!isTRUE(map_display$add_water_districts)) {
    return(m)
  }
  
  if (!inherits(water_districts, "sf") || nrow(water_districts) == 0) {
    message("Water districts layer is empty or missing; skipping.")
    return(m)
  }
  
  message("Adding water districts: ", nrow(water_districts))
  
  m |>
    leaflet::addPolygons(
      data = water_districts,
      layerId = ~water_district_id,
      group = pt_layer_group_name("Water Districts"),
      
      ## Reference-boundary style:
      ## faint fill for clickability, visible matching outline for geography.
      fill = TRUE,
      fillColor = ~fill_col,
      fillOpacity = 0.025,
      color = ~line_col,
      weight = 1.2,
      opacity = 0.95,
      
      ## Hover and popup.
      label = ~hover_text,
      labelOptions = leaflet::labelOptions(
        direction = "auto",
        textsize = "12px",
        opacity = 0.95,
        sticky = TRUE
      ),
      popup = ~popup_html,
      
      ## Keep in the reference pane. If a pane_reference does not exist in your
      ## pane setup, this will need to match the existing pane used for other
      ## reference polygons. Most current reference polygons are drawn without a
      ## custom pane, but using pane_county keeps this below points/labels.
      options = leaflet::pathOptions(
        pane = "pane_county",
        interactive = TRUE
      ),
      
      ## Strong visual feedback for the polygon that is actually receiving the
      ## hover/click event. This is especially important where districts overlap.
      highlightOptions = leaflet::highlightOptions(
        color = "#111111",
        weight = 3.2,
        opacity = 1,
        fillOpacity = 0.11,
        bringToFront = TRUE
      )
    )
}
