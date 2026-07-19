`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) == 1 && is.na(x)) return(y)
  x
}

num_from_text <- function(x) {
  suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x))))
}

fmt_cfs_short <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- ifelse(is.na(x), "", format(round(x), big.mark = ",", trim = TRUE))
  out
}

arrow_poly <- function(lon1, lat1, lon2, lat2, width_km = 5, head_len_km = 10) {
  lat0 <- mean(c(lat1, lat2)) * pi / 180
  lon0 <- mean(c(lon1, lon2))
  y0 <- mean(c(lat1, lat2))
  kx <- 111.32 * cos(lat0)
  ky <- 111.32

  p1 <- c((lon1 - lon0) * kx, (lat1 - y0) * ky)
  p2 <- c((lon2 - lon0) * kx, (lat2 - y0) * ky)
  v <- p2 - p1
  L <- sqrt(sum(v^2))
  if (!is.finite(L) || L == 0) return(NULL)
  u <- v / L
  n <- c(-u[2], u[1])

  head_len <- min(head_len_km, L * 0.45)
  shaft_w <- width_km / 2
  head_w <- width_km * 1.15
  base <- p2 - u * head_len

  xy <- rbind(
    p1 + n * shaft_w,
    base + n * shaft_w,
    base + n * head_w,
    p2,
    base - n * head_w,
    base - n * shaft_w,
    p1 - n * shaft_w,
    p1 + n * shaft_w
  )

  data.frame(
    lon = xy[, 1] / kx + lon0,
    lat = xy[, 2] / ky + y0
  )
}

flow_width <- function(cfs) {
  if (is.na(cfs)) return(4)
  pmin(18, pmax(4, 3 + sqrt(abs(cfs)) / sqrt(15000) * 15))
}

icon_html <- function(feature_key, color) {
  if (feature_key == "cross_channel_gates") {
    return(paste0(
      "<svg width='18' height='14' viewBox='0 0 18 14' style='vertical-align:-2px;margin-right:3px'>",
      "<rect x='2' y='2' width='2.5' height='10' rx='1' fill='", color, "'></rect>",
      "<rect x='13.5' y='2' width='2.5' height='10' rx='1' fill='", color, "'></rect>",
      "<path d='M4.5 10 L13.5 4' stroke='", color, "' stroke-width='3' stroke-linecap='round'></path>",
      "</svg>"
    ))
  }

  sym <- dplyr::case_when(
    feature_key == "x2_position_current" ~ "●",
    grepl("exports", feature_key) ~ "⬢",
    feature_key %in% c("delta_outflow_index", "omr_index") ~ "◆",
    grepl("inflow|freeport|vernalis", feature_key) ~ "▲",
    feature_key %in% c("delta_conditions", "controlling_factors") ~ "■",
    feature_key == "san_luis_reservoir" ~ "▣",
    TRUE ~ "●"
  )
  paste0("<span style='color:", color, ";font-size:15px;margin-right:3px;'>", sym, "</span>")
}

color_for <- function(k) {
  dplyr::case_when(
    k == "x2_position_current" ~ "#d73027",
    k == "cross_channel_gates" ~ "#f46d43",
    grepl("exports", k) ~ "#2b8cbe",
    k %in% c("delta_outflow_index", "omr_index") ~ "#756bb1",
    grepl("inflow|freeport|vernalis", k) ~ "#238b45",
    k %in% c("delta_conditions", "controlling_factors") ~ "#555555",
    k == "san_luis_reservoir" ~ "#8c510a",
    TRUE ~ "#333333"
  )
}

gj <- jsonlite::fromJSON("docs/data/delta_ops_daily_summary_features.geojson", simplifyVector = FALSE)
summary <- jsonlite::fromJSON("docs/data/delta_ops_daily_summary_summary.json")

pts <- do.call(rbind, lapply(gj$features, function(f) {
  p <- f$properties
  cc <- f$geometry$coordinates
  data.frame(
    feature_key = p$feature_key %||% "",
    metric_name = p$metric_name %||% "",
    label_text = p$label_text %||% "",
    value_raw = p$value_raw %||% "",
    units = p$units %||% "",
    report_date = p$report_date %||% "",
    lon = as.numeric(cc[[1]]),
    lat = as.numeric(cc[[2]]),
    stringsAsFactors = FALSE
  )
}))

# Preview-only label cleanup.
pts$label_text[pts$feature_key == "percent_inflow_diverted"] <- gsub("Diverted: ([0-9.]+) ", "Diverted: \\1% ", pts$label_text[pts$feature_key == "percent_inflow_diverted"])
pts$label_text[pts$feature_key == "cross_channel_gates"] <- gsub("^DCC gates:", "Cross Channel Gates:", pts$label_text[pts$feature_key == "cross_channel_gates"])
pts$label_text[pts$feature_key == "san_luis_reservoir"] <- gsub("^San Luis:", "San Luis off-map:", pts$label_text[pts$feature_key == "san_luis_reservoir"])

pts$color <- color_for(pts$feature_key)
pts$radius <- dplyr::case_when(
  pts$feature_key == "x2_position_current" ~ 11,
  pts$feature_key == "cross_channel_gates" ~ 8,
  TRUE ~ 6
)

# Label coordinates: symbols stay at real/conceptual points; labels get light manual nudges for screenshot readability.
pts$label_lon <- pts$lon
pts$label_lat <- pts$lat

set_label_pos <- function(key, lon, lat) {
  i <- which(pts$feature_key == key)
  if (length(i) == 1) { pts$label_lon[i] <<- lon; pts$label_lat[i] <<- lat }
}

set_label_pos("sacramento_freeport", -121.63, 38.43)
set_label_pos("cross_channel_gates", -121.64, 38.28)
set_label_pos("total_delta_inflow", -121.35, 38.18)
set_label_pos("delta_conditions", -121.74, 38.08)
set_label_pos("delta_outflow_index", -121.88, 38.14)
set_label_pos("percent_inflow_diverted", -121.67, 38.02)
set_label_pos("omr_index", -121.72, 37.94)
set_label_pos("x2_position_current", -121.86, 38.00)
set_label_pos("jones_cvp_exports", -121.76, 37.82)
set_label_pos("banks_swp_exports", -121.43, 37.82)
set_label_pos("controlling_factors", -121.58, 37.72)
set_label_pos("san_joaquin_vernalis", -121.43, 37.66)
set_label_pos("san_luis_reservoir", -121.88, 37.70)

pts$popup <- paste0(
  "<b>", pts$metric_name, "</b><br>",
  pts$label_text, "<br><br>",
  "Report date: ", pts$report_date, "<br>",
  "<em>", summary$preliminary_notice, "</em><br><br>",
  "<a href='", summary$source_url, "' target='_blank'>DWR source PDF</a>"
)

pts$label_html <- mapply(function(k, lab, col) {
  paste0(
    "<div class='delta-ops-label delta-ops-big-label'>",
    icon_html(k, col),
    "<b>", lab, "</b>",
    "</div>"
  )
}, pts$feature_key, pts$label_text, pts$color, USE.NAMES = FALSE)

x2_ref <- readr::read_csv("data/input/x2_river_km_lookup.csv", show_col_types = FALSE)
x2_ref_delta <- x2_ref[x2_ref$river_km >= 40 & x2_ref$river_km <= 115, ]
x2_ref_10 <- x2_ref_delta[x2_ref_delta$river_km %% 10 == 0, ]
x2_ref_5 <- x2_ref_delta[x2_ref_delta$river_km %% 5 == 0 & x2_ref_delta$river_km %% 10 != 0, ]
x2_ref_1 <- x2_ref_delta[x2_ref_delta$river_km %% 5 != 0, ]

get_val <- function(key) {
  v <- pts$value_raw[pts$feature_key == key][1]
  num_from_text(v)
}

sac_cfs <- get_val("sacramento_freeport")
sj_cfs <- get_val("san_joaquin_vernalis")
out_cfs <- get_val("delta_outflow_index")
exp_cfs <- get_val("jones_cvp_exports") + get_val("banks_swp_exports")
omr_cfs <- abs(get_val("omr_index"))

arrow_specs <- data.frame(
  id = c("Sacramento inflow", "San Joaquin inflow", "Delta outflow", "South Delta exports", "OMR index"),
  lon1 = c(-121.50, -121.27, -121.53, -121.60, -121.54),
  lat1 = c(  38.45,   37.68,   38.06,   37.90,   38.02),
  lon2 = c(-121.55, -121.47, -121.91, -121.60, -121.55),
  lat2 = c(  38.18,   37.88,   38.05,   37.75,   37.90),
  cfs = c(sac_cfs, sj_cfs, out_cfs, exp_cfs, omr_cfs),
  fill = c("#238b45", "#41ab5d", "#756bb1", "#2b8cbe", "#b2182b"),
  stringsAsFactors = FALSE
)
arrow_specs$width_km <- vapply(arrow_specs$cfs, flow_width, numeric(1))

arrow_list <- lapply(seq_len(nrow(arrow_specs)), function(i) {
  a <- arrow_specs[i, ]
  poly <- arrow_poly(a$lon1, a$lat1, a$lon2, a$lat2, width_km = a$width_km, head_len_km = 8)
  if (is.null(poly)) return(NULL)
  poly$id <- a$id
  poly$cfs <- a$cfs
  poly$fill <- a$fill
  poly$group_id <- i
  poly
})
arrow_df <- do.call(rbind, arrow_list)

css <- htmltools::tags$style(htmltools::HTML("
  .delta-ops-title {
    background: rgba(255,255,255,0.82);
    border: 1px solid rgba(70,70,70,0.45);
    border-radius: 6px;
    padding: 7px 9px;
    font-family: Arial, sans-serif;
    font-size: 13px;
    line-height: 1.22;
    box-shadow: 0 1px 4px rgba(0,0,0,0.15);
  }
  .delta-ops-title b { font-size: 16px; }
  .delta-ops-mini-control {
    margin-top: 6px;
    background: rgba(255,255,255,0.82);
    border: 1px solid rgba(70,70,70,0.45);
    border-radius: 6px;
    padding: 5px 7px;
    font-family: Arial, sans-serif;
    font-size: 12px;
    box-shadow: 0 1px 4px rgba(0,0,0,0.15);
  }
  .delta-ops-mini-control button {
    font-size: 11px;
    padding: 2px 5px;
    margin-left: 6px;
    cursor: pointer;
  }
  .delta-ops-label {
    background: rgba(255,255,255,0.68);
    border: 1px solid rgba(0,0,0,0.23);
    border-radius: 6px;
    padding: 3px 6px;
    font-family: Arial, sans-serif;
    font-size: 13.5px;
    line-height: 1.12;
    white-space: nowrap;
    box-shadow: 0 1px 2px rgba(0,0,0,0.12);
  }
  .delta-ops-label:hover { background: rgba(255,255,255,0.90); }
  .delta-ops-label-inset {
    background: rgba(255,250,230,0.72);
    border: 1px solid rgba(120,80,0,0.35);
  }
  .x2-ref-label {
    background: rgba(255,255,255,0.44);
    border: 0;
    box-shadow: none;
    color: #555;
    font-size: 10px;
    font-family: Arial, sans-serif;
    white-space: nowrap;
    padding: 1px 2px;
  }
  .delta-ops-zoom-low .x2-label-5,
  .delta-ops-zoom-low .x2-label-1 { display: none !important; }
  .delta-ops-zoom-med .x2-label-1 { display: none !important; }
  .delta-ops-zoom-high .x2-label-1 { display: block !important; }
"))

m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
  leaflet::addTiles(
    urlTemplate = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSHydroCached/MapServer/tile/{z}/{y}/{x}",
    attribution = "USGS The National Map",
    group = "USGS Hydrography"
  ) |>
  leaflet::addProviderTiles("CartoDB.Positron", group = "CartoDB Positron") |>
  leaflet::addPolygons(
    data = arrow_df,
    lng = ~lon, lat = ~lat,
    group = "Delta flow arrows",
    color = ~fill,
    fillColor = ~fill,
    fillOpacity = 0.30,
    opacity = 0.55,
    weight = 1,
    popup = ~paste0("<b>", id, "</b><br>", fmt_cfs_short(cfs), " cfs")
  ) |>
  leaflet::addCircleMarkers(
    data = x2_ref_delta,
    lng = ~lon, lat = ~lat,
    radius = 3.2,
    color = "#777777",
    fillColor = "#ffffff",
    fillOpacity = 0.70,
    weight = 1,
    options = leaflet::pathOptions(interactive = FALSE),
    group = "X2 reference km"
  ) |>
  leaflet::addLabelOnlyMarkers(
    data = x2_ref_10,
    lng = ~lon, lat = ~lat,
    label = ~paste0(river_km, " km"),
    labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = FALSE, direction = "center", className = "x2-ref-label x2-label-10"),
    group = "X2 labels 10 km"
  ) |>
  leaflet::addLabelOnlyMarkers(
    data = x2_ref_5,
    lng = ~lon, lat = ~lat,
    label = ~paste0(river_km, " km"),
    labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = FALSE, direction = "center", className = "x2-ref-label x2-label-5"),
    group = "X2 labels 5 km"
  ) |>
  leaflet::addLabelOnlyMarkers(
    data = x2_ref_1,
    lng = ~lon, lat = ~lat,
    label = ~paste0(river_km),
    labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = FALSE, direction = "center", className = "x2-ref-label x2-label-1"),
    group = "X2 labels 1 km"
  ) |>
  leaflet::addCircleMarkers(
    data = pts,
    lng = ~lon, lat = ~lat,
    radius = ~radius,
    color = ~color,
    fillColor = ~color,
    fillOpacity = 0.86,
    weight = 1.7,
    popup = ~popup,
    group = "Delta ops symbols"
  ) |>
  leaflet::addPolylines(
    data = pts[abs(pts$label_lon - pts$lon) > 0.025 | abs(pts$label_lat - pts$lat) > 0.025, ],
    lng = ~c(lon, label_lon),
    lat = ~c(lat, label_lat),
    color = "#555555",
    opacity = 0.22,
    weight = 1,
    options = leaflet::pathOptions(interactive = FALSE),
    group = "Delta ops label leaders"
  ) |>
  leaflet::addLabelOnlyMarkers(
    data = pts,
    lng = ~label_lon, lat = ~label_lat,
    label = lapply(pts$label_html, htmltools::HTML),
    labelOptions = leaflet::labelOptions(noHide = TRUE, textOnly = FALSE, direction = "center", opacity = 1),
    group = "Delta ops labels"
  ) |>
  leaflet::addControl(
    html = htmltools::HTML(paste0(
      "<div class='delta-ops-title'>",
      "<b>Delta ops snapshot | CVP/SWP</b><br>",
      "DWR Daily Summary: ", summary$report_date, "<br>",
      "<span style='font-size:11px;'>", summary$preliminary_notice, "</span>",
      "</div>"
    )),
    position = "topright"
  ) |>
  leaflet::addLayersControl(
    baseGroups = c("USGS Hydrography", "CartoDB Positron"),
    overlayGroups = c("Delta flow arrows", "X2 reference km"),
    options = leaflet::layersControlOptions(collapsed = TRUE)
  ) |>
  leaflet::fitBounds(-122.08, 37.62, -121.12, 38.58)

js <- '
function(el, x) {
  var map = this;
  var root = el;

  function setZoomClass() {
    var z = map.getZoom();
    root.classList.remove("delta-ops-zoom-low", "delta-ops-zoom-med", "delta-ops-zoom-high");
    if (z < 9) root.classList.add("delta-ops-zoom-low");
    else if (z < 11) root.classList.add("delta-ops-zoom-med");
    else root.classList.add("delta-ops-zoom-high");
  }
  map.on("zoomend", setZoomClass);
  setTimeout(setZoomClass, 50);

  var ctrl = L.control({position: "topright"});
  ctrl.onAdd = function(map) {
    var div = L.DomUtil.create("div", "delta-ops-mini-control");
    div.innerHTML = "<label title=\"Toggle large dashboard labels\"><input id=\"deltaOpsLabelsToggle\" type=\"checkbox\" checked> labels</label><button id=\"deltaOpsZoomBtn\" type=\"button\">zoom</button>";
    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);
    return div;
  };
  ctrl.addTo(map);

  function setLabels(show) {
    root.querySelectorAll(".delta-ops-big-label").forEach(function(d) {
      d.style.display = show ? "" : "none";
    });
    root.querySelectorAll(".leaflet-interactive").forEach(function(d) {
      // no-op; keep symbols/arrows active
    });
  }

  setTimeout(function() {
    var cb = root.querySelector("#deltaOpsLabelsToggle");
    var btn = root.querySelector("#deltaOpsZoomBtn");
    if (cb) cb.addEventListener("change", function() { setLabels(cb.checked); });
    if (btn) btn.addEventListener("click", function() {
      map.fitBounds([[37.62, -122.08], [38.58, -121.12]]);
    });
  }, 100);
}
'

m <- htmlwidgets::prependContent(m, css)
m <- htmlwidgets::onRender(m, js)

out_html <- "docs/data/delta_ops_snapshot_preview_v2.html"
htmlwidgets::saveWidget(
  m,
  out_html,
  selfcontained = TRUE,
  title = "BRIM Delta Ops Snapshot Preview v2"
)

cat("Wrote:\n", normalizePath(out_html, winslash = "/"), "\n")
