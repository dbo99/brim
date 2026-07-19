# scripts/preview_delta_ops_snapshot_v4.R

pkgs <- c("leaflet", "htmlwidgets", "htmltools", "jsonlite", "readr", "dplyr")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) >= 1 && is.na(x[1])) return(y)
  x
}

first_num <- function(x) {
  x <- as.character(x %||% "")
  x <- x[1]
  m <- regexpr("-?[0-9]+(?:,[0-9]{3})*(?:\\.[0-9]+)?", x, perl = TRUE)
  if (m[1] == -1) return(NA_real_)
  s <- regmatches(x, m)
  as.numeric(gsub(",", "", s))
}

fmt_cfs <- function(x) {
  if (is.na(x)) return("NA")
  if (abs(x) >= 1000) {
    return(paste0(format(round(x, 0), big.mark = ",", trim = TRUE), " cfs"))
  }
  paste0(round(x, 0), " cfs")
}

short_mdy <- function(x) {
  d <- as.Date(x)
  paste0(as.integer(format(d, "%m")), "/", as.integer(format(d, "%d")))
}

color_for <- function(key) {
  dplyr::case_when(
    key == "x2_position_current" ~ "#444444",
    key == "cross_channel_gates" ~ "#e67e22",
    key %in% c("jones_cvp_exports", "banks_swp_exports") ~ "#2b8cbe",
    key %in% c("delta_outflow_index", "omr_index") ~ "#756bb1",
    key %in% c("sacramento_freeport", "san_joaquin_vernalis", "total_delta_inflow") ~ "#31a354",
    key %in% c("delta_conditions", "controlling_factors") ~ "#4d4d4d",
    key == "san_luis_reservoir" ~ "#8c510a",
    TRUE ~ "#333333"
  )
}

symbol_for <- function(key) {
  dplyr::case_when(
    key == "x2_position_current" ~ "●",
    key == "cross_channel_gates" ~ "▱",
    key %in% c("jones_cvp_exports", "banks_swp_exports") ~ "●",
    key %in% c("delta_outflow_index", "omr_index") ~ "◆",
    key %in% c("sacramento_freeport", "san_joaquin_vernalis", "total_delta_inflow") ~ "▲",
    key %in% c("delta_conditions", "controlling_factors") ~ "■",
    key == "san_luis_reservoir" ~ "▣",
    TRUE ~ "●"
  )
}

radius_for <- function(key) {
  dplyr::case_when(
    key == "x2_position_current" ~ 7,
    key == "cross_channel_gates" ~ 6,
    key == "san_luis_reservoir" ~ 5.5,
    TRUE ~ 5
  )
}

label_dir_for <- function(key) {
  dplyr::case_when(
    key == "sacramento_freeport" ~ "left",
    key == "cross_channel_gates" ~ "left",
    key == "delta_outflow_index" ~ "left",
    key == "percent_inflow_diverted" ~ "bottom",
    key == "omr_index" ~ "bottom",
    key == "x2_position_current" ~ "left",
    key == "delta_conditions" ~ "top",
    key == "jones_cvp_exports" ~ "left",
    key == "banks_swp_exports" ~ "right",
    key == "controlling_factors" ~ "bottom",
    key == "san_joaquin_vernalis" ~ "right",
    key == "total_delta_inflow" ~ "top",
    key == "san_luis_reservoir" ~ "right",
    TRUE ~ "right"
  )
}

# ---- Read outputs from sandbox parser ----
gj <- jsonlite::fromJSON(
  "docs/data/delta_ops_daily_summary_features.geojson",
  simplifyVector = FALSE
)
summary_info <- jsonlite::fromJSON("docs/data/delta_ops_daily_summary_summary.json")

pts <- do.call(rbind, lapply(gj$features, function(f) {
  p <- f$properties
  cc <- f$geometry$coordinates
  data.frame(
    feature_key  = p$feature_key %||% "",
    metric_name  = p$metric_name %||% "",
    label_text   = p$label_text %||% "",
    value_raw    = p$value_raw %||% "",
    units        = p$units %||% "",
    report_date  = p$report_date %||% "",
    lon          = as.numeric(cc[[1]]),
    lat          = as.numeric(cc[[2]]),
    stringsAsFactors = FALSE
  )
}))

# ---- Replace Total Delta Inflow display with East Side Streams contribution ----
get_val <- function(key) {
  first_num(pts$value_raw[pts$feature_key == key][1])
}

sac_val   <- get_val("sacramento_freeport")
sj_val    <- get_val("san_joaquin_vernalis")
total_val <- get_val("total_delta_inflow")
east_side_val <- total_val - sac_val - sj_val

pts$metric_name_disp <- pts$metric_name
pts$value_raw_disp   <- pts$value_raw
pts$label_text_disp  <- pts$label_text

i_total <- which(pts$feature_key == "total_delta_inflow")
if (length(i_total) == 1 && !is.na(east_side_val)) {
  pts$metric_name_disp[i_total] <- "East Side Streams contribution"
  pts$value_raw_disp[i_total]   <- paste0(format(round(east_side_val, 0), big.mark = ","), " cfs")
  pts$label_text_disp[i_total]  <- paste0("East side: ", fmt_cfs(east_side_val))
}

# ---- Build pretty labels ----
x2_tag <- paste0("X2 ", short_mdy(summary_info$report_date))

pretty_label <- function(key, lab, raw, report_date) {
  n <- first_num(raw)
  
  if (key == "cross_channel_gates") {
    return(paste0("Cross Channel Gates: ", ifelse(is.na(n), "NA", n), "% open"))
  }
  if (key == "jones_cvp_exports") {
    return(paste0("Jones/CVP: ", fmt_cfs(n)))
  }
  if (key == "banks_swp_exports") {
    return(paste0("Banks/SWP: ", fmt_cfs(n)))
  }
  if (key == "delta_outflow_index") {
    return(paste0("Outflow: ", fmt_cfs(n)))
  }
  if (key == "percent_inflow_diverted") {
    return(paste0("Diverted: ", ifelse(is.na(n), "NA", n), "%"))
  }
  if (key == "omr_index") {
    return(paste0("OMR: ", fmt_cfs(n)))
  }
  if (key == "sacramento_freeport") {
    return(paste0("Sac: ", fmt_cfs(n)))
  }
  if (key == "san_joaquin_vernalis") {
    return(paste0("SJ: ", fmt_cfs(n)))
  }
  if (key == "total_delta_inflow") {
    return(paste0("East side: ", fmt_cfs(n)))
  }
  if (key == "delta_conditions") {
    return(paste0("Delta: ", gsub("^Delta:\\s*", "", lab)))
  }
  if (key == "controlling_factors") {
    return(paste0("Control: ", gsub("^Control:\\s*", "", lab)))
  }
  if (key == "san_luis_reservoir") {
    return(gsub("^San Luis:\\s*", "San Luis: ", lab))
  }
  if (key == "x2_position_current") {
    return(paste0(x2_tag, ": ", ifelse(is.na(n), "NA", n), " km"))
  }
  lab
}

pts$pretty_label <- mapply(
  pretty_label,
  pts$feature_key,
  pts$label_text_disp,
  pts$value_raw_disp,
  pts$report_date,
  USE.NAMES = FALSE
)

pts$popup <- paste0(
  "<b>", pts$metric_name_disp, "</b><br>",
  pts$pretty_label, "<br><br>",
  "Report date: ", pts$report_date, "<br>",
  "<em>", summary_info$preliminary_notice, "</em><br><br>",
  "<a href='", summary_info$source_url, "' target='_blank'>DWR source PDF</a>"
)

pts$color <- color_for(pts$feature_key)
pts$symbol <- symbol_for(pts$feature_key)
pts$radius <- radius_for(pts$feature_key)
pts$label_dir <- label_dir_for(pts$feature_key)

# ---- X2 reference points: keep only faint 10-km context ----
x2_ref <- readr::read_csv("data/input/x2_river_km_lookup.csv", show_col_types = FALSE)
x2_ref <- x2_ref[x2_ref$river_km >= 40 & x2_ref$river_km <= 115, ]
x2_ref10 <- x2_ref[x2_ref$river_km %% 10 == 0, ]

# ---- Styling ----
css <- htmltools::tags$style(htmltools::HTML("
  .delta-ops-title {
    background: rgba(255,255,255,0.68);
    border: 1px solid rgba(70,70,70,0.20);
    border-radius: 5px;
    padding: 5px 7px;
    font-family: Arial, sans-serif;
    font-size: 11px;
    line-height: 1.15;
    box-shadow: none;
  }
  .delta-ops-title b { font-size: 13px; }

  .delta-ops-mini-control {
    margin-top: 4px;
    background: rgba(255,255,255,0.68);
    border: 1px solid rgba(70,70,70,0.20);
    border-radius: 5px;
    padding: 3px 5px;
    font-family: Arial, sans-serif;
    font-size: 11px;
    box-shadow: none;
  }
  .delta-ops-mini-control button {
    font-size: 10px;
    padding: 1px 4px;
    margin-left: 5px;
    cursor: pointer;
  }

  .leaflet-tooltip.delta-ops-label {
    background: transparent;
    border: none;
    box-shadow: none;
    padding: 0;
    pointer-events: none;
  }

  .delta-ops-symbol {
    font-size: 10px;
    margin-right: 2px;
    text-shadow:
      -1px -1px 0 rgba(255,255,255,0.93),
       1px -1px 0 rgba(255,255,255,0.93),
      -1px  1px 0 rgba(255,255,255,0.93),
       1px  1px 0 rgba(255,255,255,0.93);
  }

  .delta-ops-label-text {
    font-family: Arial, sans-serif;
    font-size: 11px;
    font-weight: 700;
    color: #111;
    white-space: nowrap;
    text-shadow:
      -1px -1px 0 rgba(255,255,255,0.96),
       1px -1px 0 rgba(255,255,255,0.96),
      -1px  1px 0 rgba(255,255,255,0.96),
       1px  1px 0 rgba(255,255,255,0.96);
  }

  .leaflet-tooltip.x2-ref-label {
    background: transparent;
    border: none;
    box-shadow: none;
    padding: 0;
    color: #666;
    font-family: Arial, sans-serif;
    font-size: 9px;
    text-shadow:
      -1px -1px 0 rgba(255,255,255,0.80),
       1px -1px 0 rgba(255,255,255,0.80),
      -1px  1px 0 rgba(255,255,255,0.80),
       1px  1px 0 rgba(255,255,255,0.80);
  }
"))

# ---- Map ----
m <- leaflet::leaflet(
  options = leaflet::leafletOptions(preferCanvas = TRUE)
) |>
  leaflet::addTiles(
    urlTemplate = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSHydroCached/MapServer/tile/{z}/{y}/{x}",
    attribution = "USGS The National Map",
    group = "USGS Hydrography"
  ) |>
  leaflet::setView(
    lng = -121.66,
    lat = 38.02,
    zoom = 9.3
  )

# faint X2 context
m <- m |>
  leaflet::addCircleMarkers(
    data = x2_ref10,
    lng = ~lon, lat = ~lat,
    radius = 2.5,
    color = "#777777",
    fillColor = "#ffffff",
    fillOpacity = 0.60,
    opacity = 0.55,
    weight = 0.8,
    options = leaflet::pathOptions(interactive = FALSE),
    group = "X2 reference"
  ) |>
  leaflet::addLabelOnlyMarkers(
    data = x2_ref10,
    lng = ~lon, lat = ~lat,
    label = ~as.character(river_km),
    labelOptions = leaflet::labelOptions(
      noHide = TRUE,
      textOnly = FALSE,
      direction = "center",
      className = "x2-ref-label"
    ),
    options = leaflet::markerOptions(interactive = FALSE),
    group = "X2 reference"
  )

# actual Delta ops points with attached labels
for (i in seq_len(nrow(pts))) {
  row <- pts[i, , drop = FALSE]
  
  lbl_html <- paste0(
    "<span class='delta-ops-symbol' style='color:", row$color, "'>", row$symbol, "</span>",
    "<span class='delta-ops-label-text'>", row$pretty_label, "</span>"
  )
  
  m <- leaflet::addCircleMarkers(
    m,
    data = row,
    lng = ~lon, lat = ~lat,
    radius = row$radius,
    color = row$color,
    fillColor = row$color,
    fillOpacity = 0.82,
    opacity = 0.90,
    weight = 1.2,
    popup = row$popup,
    label = htmltools::HTML(lbl_html),
    labelOptions = leaflet::labelOptions(
      noHide = TRUE,
      textOnly = FALSE,
      direction = row$label_dir,
      className = "delta-ops-label",
      opacity = 1
    ),
    group = "Delta ops"
  )
}

m <- m |>
  leaflet::addControl(
    html = htmltools::HTML(paste0(
      "<div class='delta-ops-title'>",
      "<b>Delta ops snapshot | CVP/SWP</b><br>",
      "DWR Daily Summary: ", summary_info$report_date, "<br>",
      summary_info$preliminary_notice,
      "</div>"
    )),
    position = "topright"
  ) |>
  leaflet::addLayersControl(
    overlayGroups = c("X2 reference"),
    options = leaflet::layersControlOptions(collapsed = TRUE)
  )

# simple label toggle + zoom button
js <- "
function(el, x) {
  var map = this;
  var root = el;

  var ctrl = L.control({position: 'topright'});
  ctrl.onAdd = function(map) {
    var div = L.DomUtil.create('div', 'delta-ops-mini-control');
    div.innerHTML = '<label><input id=\"deltaOpsLabelsToggle\" type=\"checkbox\" checked> labels</label><button id=\"deltaOpsZoomBtn\" type=\"button\">zoom</button>';
    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);
    return div;
  };
  ctrl.addTo(map);

  function setLabels(show) {
    root.querySelectorAll('.delta-ops-label').forEach(function(d) {
      d.style.display = show ? '' : 'none';
    });
  }

  setTimeout(function() {
    var cb = root.querySelector('#deltaOpsLabelsToggle');
    var btn = root.querySelector('#deltaOpsZoomBtn');

    if (cb) {
      cb.addEventListener('change', function() {
        setLabels(cb.checked);
      });
    }

    if (btn) {
      btn.addEventListener('click', function() {
        map.setView([38.02, -121.66], 9.3);
      });
    }
  }, 100);
}
"

m <- htmlwidgets::prependContent(m, css)
m <- htmlwidgets::onRender(m, js)

out_html <- "docs/data/delta_ops_snapshot_preview_v4.html"

htmlwidgets::saveWidget(
  m,
  out_html,
  selfcontained = TRUE,
  title = "BRIM Delta Ops Snapshot Preview v4"
)

cat("Wrote:\n", normalizePath(out_html, winslash = "/"), "\n")