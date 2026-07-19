# scripts/preview_delta_ops_snapshot_v6.R
# BRIM Delta Ops sandbox visual preview
# Purpose: hydrography-first map preview using simple black dots and styled text labels.
# Inputs expected under the sandbox root:
#   docs/data/delta_ops_daily_summary_features.geojson
#   docs/data/delta_ops_daily_summary_summary.json
#   data/input/x2_river_km_lookup.csv

pkgs <- c("leaflet", "htmlwidgets", "htmltools", "jsonlite", "readr", "dplyr")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) >= 1 && is.na(x[1])) return(y)
  x
}

html_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
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
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0 || is.na(x)) return("NA cfs")
  paste0(format(round(x, 0), big.mark = ",", trim = TRUE), " cfs")
}

short_mdy <- function(x) {
  d <- as.Date(x)
  paste0(as.integer(format(d, "%m")), "/", as.integer(format(d, "%d")))
}

label_class_for <- function(key) {
  dplyr::case_when(
    key %in% c("sacramento_freeport", "san_joaquin_vernalis", "total_delta_inflow") ~ "delta-label-inflow",
    key %in% c("jones_cvp_exports", "banks_swp_exports") ~ "delta-label-export",
    key == "delta_outflow_index" ~ "delta-label-outflow",
    key == "omr_index" ~ "delta-label-omr",
    key == "x2_position_current" ~ "delta-label-x2",
    key == "cross_channel_gates" ~ "delta-label-gates",
    key == "percent_inflow_diverted" ~ "delta-label-diverted",
    key == "delta_conditions" ~ "delta-label-status",
    key == "controlling_factors" ~ "delta-label-control",
    key == "san_luis_reservoir" ~ "delta-label-sanluis",
    TRUE ~ "delta-label-default"
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

sac_val <- get_val("sacramento_freeport")
sj_val <- get_val("san_joaquin_vernalis")
total_val <- get_val("total_delta_inflow")
east_side_val <- total_val - sac_val - sj_val

pts$metric_name_disp <- pts$metric_name
pts$value_raw_disp <- pts$value_raw
pts$label_text_disp <- pts$label_text

i_total <- which(pts$feature_key == "total_delta_inflow")
if (length(i_total) == 1 && is.finite(east_side_val)) {
  pts$metric_name_disp[i_total] <- "East Side Streams contribution"
  pts$value_raw_disp[i_total] <- paste0(format(round(east_side_val, 0), big.mark = ",", trim = TRUE), " cfs")
  pts$label_text_disp[i_total] <- paste0("East side: ", fmt_cfs(east_side_val))
}

# ---- Build display labels ----
x2_tag <- paste0("X2 ", short_mdy(summary_info$report_date))

pretty_label <- function(key, lab, raw) {
  n <- first_num(raw)

  if (key == "cross_channel_gates") {
    return(paste0("Delta X-Channel Gates<br>", ifelse(is.na(n), "NA", n), "% open"))
  }
  if (key == "jones_cvp_exports") return(paste0("Jones/CVP: ", fmt_cfs(n)))
  if (key == "banks_swp_exports") return(paste0("Banks/SWP: ", fmt_cfs(n)))
  if (key == "delta_outflow_index") return(paste0("Outflow: ", fmt_cfs(n)))
  if (key == "percent_inflow_diverted") return(paste0("Diverted: ", ifelse(is.na(n), "NA", n), "%"))
  if (key == "omr_index") return(paste0("OMR: ", fmt_cfs(n)))
  if (key == "sacramento_freeport") return(paste0("Sac: ", fmt_cfs(n)))
  if (key == "san_joaquin_vernalis") return(paste0("SJ: ", fmt_cfs(n)))
  if (key == "total_delta_inflow") return(paste0("East side: ", fmt_cfs(n)))
  if (key == "delta_conditions") return(paste0("Delta: ", gsub("^Delta:\\s*", "", lab)))
  if (key == "controlling_factors") return(paste0("Control: ", gsub("^Control:\\s*", "", lab)))
  if (key == "san_luis_reservoir") return(gsub("^San Luis:\\s*", "San Luis: ", lab))
  if (key == "x2_position_current") return(paste0(x2_tag, ": ", ifelse(is.na(n), "NA", n), " km"))
  lab
}

pts$pretty_label <- mapply(
  pretty_label,
  pts$feature_key,
  pts$label_text_disp,
  pts$value_raw_disp,
  USE.NAMES = FALSE
)

pts$label_class <- label_class_for(pts$feature_key)
pts$label_dir <- label_dir_for(pts$feature_key)
pts$show_point <- !pts$feature_key %in% c("delta_conditions", "controlling_factors")

pts$popup <- paste0(
  "<b>", html_escape(pts$metric_name_disp), "</b><br>",
  html_escape(gsub("<br>", " | ", pts$pretty_label, fixed = TRUE)), "<br><br>",
  "Report date: ", html_escape(pts$report_date), "<br>",
  "<em>", html_escape(summary_info$preliminary_notice), "</em><br><br>",
  "<a href='", html_escape(summary_info$source_url), "' target='_blank'>DWR source PDF</a>"
)

pts$label_html <- paste0(
  "<span class='delta-ops-label-text ", pts$label_class, "'>",
  pts$pretty_label,
  "</span>"
)

# ---- X2 reference points ----
x2_ref <- readr::read_csv("data/input/x2_river_km_lookup.csv", show_col_types = FALSE)
x2_ref <- x2_ref[order(x2_ref$river_km), ]
x2_ref_label <- x2_ref[x2_ref$river_km %% 5 == 0, ]

# ---- Styling ----
css <- htmltools::tags$style(htmltools::HTML("
  .delta-ops-title {
    background: rgba(255,255,255,0.62);
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
    background: rgba(255,255,255,0.62);
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

  .delta-ops-label-text {
    font-family: Arial, sans-serif;
    font-size: 11px;
    color: #111;
    white-space: nowrap;
    text-shadow:
      -1px -1px 0 rgba(255,255,255,0.96),
       1px -1px 0 rgba(255,255,255,0.96),
      -1px  1px 0 rgba(255,255,255,0.96),
       1px  1px 0 rgba(255,255,255,0.96);
  }

  .delta-label-inflow {
    color: #238b45;
    font-style: italic;
    font-weight: 700;
  }
  .delta-label-export {
    color: #7a0019;
    font-weight: 700;
  }
  .delta-label-outflow {
    color: #b2182b;
    font-weight: 700;
  }
  .delta-label-omr {
    color: #5e3c99;
    font-weight: 700;
  }
  .delta-label-x2 {
    color: #444444;
    font-weight: 700;
  }
  .delta-label-gates {
    color: #222222;
    font-weight: 700;
    line-height: 1.05;
  }
  .delta-label-diverted {
    color: #222222;
    font-weight: 700;
  }
  .delta-label-status {
    color: #222222;
    font-weight: 800;
    font-size: 12px;
  }
  .delta-label-control {
    color: #333333;
    font-weight: 700;
  }
  .delta-label-sanluis {
    color: #6b4c1f;
    font-weight: 700;
  }

  .leaflet-tooltip.x2-ref-label {
    background: transparent;
    border: none;
    box-shadow: none;
    padding: 0;
    color: #666;
    font-family: Arial, sans-serif;
    font-size: 8px;
    font-weight: 600;
    text-shadow:
      -1px -1px 0 rgba(255,255,255,0.82),
       1px -1px 0 rgba(255,255,255,0.82),
      -1px  1px 0 rgba(255,255,255,0.82),
       1px  1px 0 rgba(255,255,255,0.82);
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
    zoom = 10.3
  )

# X2 reference: every point drawn, every fifth point labeled.
m <- m |>
  leaflet::addCircleMarkers(
    data = x2_ref,
    lng = ~lon, lat = ~lat,
    radius = 2.0,
    color = "#777777",
    fillColor = "#f7f7f7",
    fillOpacity = 0.72,
    opacity = 0.62,
    weight = 0.7,
    options = leaflet::pathOptions(interactive = FALSE),
    group = "X2 reference"
  ) |>
  leaflet::addLabelOnlyMarkers(
    data = x2_ref_label,
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

# Simple black point symbols.
point_pts <- pts[pts$show_point, ]
m <- m |>
  leaflet::addCircleMarkers(
    data = point_pts,
    lng = ~lon, lat = ~lat,
    radius = 4.0,
    color = "#111111",
    fillColor = "#111111",
    fillOpacity = 0.88,
    opacity = 0.92,
    weight = 1,
    popup = ~popup,
    group = "Delta ops"
  )

# Attach labels directly to their actual points / conceptual label coordinates.
for (i in seq_len(nrow(pts))) {
  row <- pts[i, , drop = FALSE]
  m <- leaflet::addLabelOnlyMarkers(
    m,
    data = row,
    lng = ~lon, lat = ~lat,
    label = htmltools::HTML(row$label_html),
    labelOptions = leaflet::labelOptions(
      noHide = TRUE,
      textOnly = FALSE,
      direction = row$label_dir,
      className = "delta-ops-label",
      opacity = 1
    ),
    options = leaflet::markerOptions(interactive = FALSE),
    group = "Delta ops labels"
  )
}

m <- m |>
  leaflet::addControl(
    html = htmltools::HTML(paste0(
      "<div class='delta-ops-title'>",
      "<b>Delta ops snapshot | CVP/SWP</b><br>",
      "DWR Daily Summary: ", html_escape(summary_info$report_date), "<br>",
      html_escape(summary_info$preliminary_notice),
      "</div>"
    )),
    position = "topright"
  ) |>
  leaflet::addLayersControl(
    overlayGroups = c("X2 reference"),
    options = leaflet::layersControlOptions(collapsed = TRUE)
  )

# Simple label toggle + zoom button.
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
        map.setView([38.02, -121.66], 10.3);
      });
    }
  }, 100);
}
"

m <- htmlwidgets::prependContent(m, css)
m <- htmlwidgets::onRender(m, js)

out_html <- "docs/data/delta_ops_snapshot_preview_v6.html"

htmlwidgets::saveWidget(
  m,
  out_html,
  selfcontained = TRUE,
  title = "BRIM Delta Ops Snapshot Preview v6"
)

cat("Wrote:\n", normalizePath(out_html, winslash = "/"), "\n")
