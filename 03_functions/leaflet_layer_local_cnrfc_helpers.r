# ==== leaflet_layer_local_cnrfc_helpers.r ================================================
##
## PURPOSE:
##   CNRFC local point catalog layers, browser-managed CNRFC source groups, and CNRFC local legends.
##
## NOTE:
##   Extracted from leaflet_layer_helpers.r as a maintainability-only split.
##   Function names and behavior are intentionally unchanged.

# ==== 3. CNRFC point layers ==================================================

pt_add_cnrfc_layers <- function(m, cnrfc_stream, cnrfc_precip, map_display) {
  
  ## Helper: convert simple popup HTML to plain-ish text.
  pt_popup_to_text <- function(x) {
    x <- as.character(x)
    
    ## Turn common line-break tags into newline text.
    x <- gsub("<br\\s*/?>", "\n", x, ignore.case = TRUE)
    x <- gsub("</p>|</div>|</li>", "\n", x, ignore.case = TRUE)
    
    ## Remove remaining HTML tags.
    x <- gsub("<[^>]+>", "", x)
    
    ## Decode a few common HTML entities that may occur in popup headers.
    x <- gsub("&nbsp;", " ", x, fixed = TRUE)
    x <- gsub("&amp;", "&", x, fixed = TRUE)
    x <- gsub("&lt;", "<", x, fixed = TRUE)
    x <- gsub("&gt;", ">", x, fixed = TRUE)
    x <- gsub("&ndash;", "–", x, fixed = TRUE)
    x <- gsub("&mdash;", "—", x, fixed = TRUE)
    
    ## Normalize whitespace around lines.
    x <- gsub("[\r\t]", " ", x)
    x <- gsub(" *\n *", "\n", x)
    x <- gsub("\n+", "\n", x)
    trimws(x)
  }
  
  ## Helper: pull the first useful line from popup text.
  ## For CNRFC points this should be the popup title, e.g.:
  ##   OTTC1 – Deer Creek - Scott Road
  pt_popup_title_line <- function(popup_html) {
    txt <- pt_popup_to_text(popup_html)
    
    vapply(
      strsplit(txt, "\n", fixed = TRUE),
      function(parts) {
        parts <- trimws(parts)
        parts <- parts[parts != ""]
        if (length(parts) == 0) return("CNRFC gage")
        parts[1]
      },
      character(1)
    )
  }
  
  ## Helper: extract the elevation line already present in the popup.
  pt_popup_elevation_line <- function(popup_html) {
    txt <- pt_popup_to_text(popup_html)
    
    vapply(
      strsplit(txt, "\n", fixed = TRUE),
      function(parts) {
        parts <- trimws(parts)
        elev <- parts[grepl("^Elevation\\s*:", parts, ignore.case = TRUE)]
        if (length(elev) == 0) return("Elevation: not available")
        elev[1]
      },
      character(1)
    )
  }
  
  ## Helper: create exactly two hover rows:
  ##   popup title line
  ##   elevation line
  pt_add_cnrfc_hover_from_popup <- function(x, default_label = "CNRFC gage") {
    
    if (!"popup_html" %in% names(x)) {
      warning(default_label, ": popup_html field not found; using fallback hover text.")
      x$hover_text <- paste0(default_label, "\nElevation: not available")
      return(x)
    }
    
    top_line <- pt_popup_title_line(x$popup_html)
    elev_line <- pt_popup_elevation_line(x$popup_html)
    
    x$hover_text <- paste0(top_line, "\n", elev_line)
    
    x
  }

  ## Helper: add display-time Local-vs-Ops catalog status for the CNRFC
  ## river/reservoir Local catalog.  Prefer the full coordinate-alignment QA
  ## output when present; it is ID-based and is not fooled by clustered marker
  ## centroids or the intentionally symbolic Water Supply Index points.
  pt_cnrfc_stream_catalog_status_df <- function(x) {
    id_col <- c("nwsid", "cnrfc_id", "id", "station_id")
    id_col <- id_col[id_col %in% names(x)]
    ids <- if (length(id_col) > 0) {
      toupper(trimws(as.character(x[[id_col[[1]]]])))
    } else {
      rep(NA_character_, nrow(x))
    }

    out <- data.frame(
      status_text = rep(
        "Catalog/review point; CNRFC product and time-series availability varies by station.",
        nrow(x)
      ),
      status_class = rep("unknown", nrow(x)),
      status_stroke_col = rep("#777777", nrow(x)),
      status_weight = rep(1.1, nrow(x)),
      stringsAsFactors = FALSE
    )

    alignment_path <- file.path(
      "04_processed_data",
      "qa",
      "cnrfc_river_reservoir_coordinate_alignment_latest.csv"
    )

    if (file.exists(alignment_path)) {
      alignment <- tryCatch(
        utils::read.csv(alignment_path, stringsAsFactors = FALSE),
        error = function(e) data.frame()
      )
      if (all(c("cnrfc_id", "in_ops_live") %in% names(alignment))) {
        aid <- toupper(trimws(as.character(alignment$cnrfc_id)))
        in_ops <- alignment$in_ops_live
        if (!is.logical(in_ops)) {
          in_ops <- tolower(trimws(as.character(in_ops))) %in% c("true", "t", "1", "yes")
        }

        ops_ids <- aid[!is.na(aid) & nzchar(aid) & in_ops]
        local_only_ids <- aid[!is.na(aid) & nzchar(aid) & !in_ops]

        is_ops <- !is.na(ids) & ids %in% ops_ids
        out$status_class[is_ops] <- "also_in_ops_live"
        out$status_text[is_ops] <-
          "Also appears in current Ops Live CNRFC forecast-point layer."
        out$status_stroke_col[is_ops] <- "#ff00cc"
        out$status_weight[is_ops] <- 1.5

        is_local_only <- !is.na(ids) & ids %in% local_only_ids
        out$status_class[is_local_only] <- "catalog_only"
        out$status_text[is_local_only] <-
          "Catalog-only in Local layer; not in current Ops Live CNRFC forecast-point layer."
        out$status_stroke_col[is_local_only] <- "#444444"
        out$status_weight[is_local_only] <- 1.1

        return(out)
      }
    }

    ## Fallback to the earlier local-only QA output if the coordinate-alignment
    ## audit has not been run yet.
    local_only_path <- file.path(
      "04_processed_data",
      "qa",
      "cnrfc_river_reservoir_local_only_latest.csv"
    )

    if (file.exists(local_only_path)) {
      local_only <- tryCatch(
        utils::read.csv(local_only_path, stringsAsFactors = FALSE),
        error = function(e) data.frame()
      )
      if ("cnrfc_id" %in% names(local_only)) {
        local_only_ids <- toupper(trimws(as.character(local_only$cnrfc_id)))
        local_only_ids <- local_only_ids[nzchar(local_only_ids) & !is.na(local_only_ids)]

        is_local_only <- !is.na(ids) & ids %in% local_only_ids
        out$status_class[is_local_only] <- "catalog_only"
        out$status_text[is_local_only] <-
          "Catalog-only in Local layer; not in current Ops Live CNRFC forecast-point layer."
        out$status_stroke_col[is_local_only] <- "#444444"
        out$status_weight[is_local_only] <- 1.1

        is_ops <- !is.na(ids) & !is_local_only
        out$status_class[is_ops] <- "also_in_ops_live"
        out$status_text[is_ops] <-
          "Also appears in current Ops Live CNRFC forecast-point layer."
        out$status_stroke_col[is_ops] <- "#ff00cc"
        out$status_weight[is_ops] <- 1.5
      }
    }

    out
  }

  ## Helper: rebuild the Local CNRFC river/reservoir catalog popup from the
  ## underlying station fields, instead of reusing the older product-link-heavy
  ## cached popup_html.  The Local layer is now a catalog/screening layer; active
  ## forecast products belong in Ops Live.
  pt_clean_cnrfc_stream_catalog_popups <- function(x) {
    if (nrow(x) == 0) return(x)

    pt_stream_escape <- function(v) {
      v <- as.character(v)
      v[is.na(v)] <- ""
      v <- gsub("&", "&amp;", v, fixed = TRUE)
      v <- gsub("<", "&lt;", v, fixed = TRUE)
      v <- gsub(">", "&gt;", v, fixed = TRUE)
      v <- gsub('"', "&quot;", v, fixed = TRUE)
      v
    }

    pt_stream_has_text <- function(v) {
      !is.na(v) & nzchar(trimws(as.character(v))) & trimws(as.character(v)) != "NA"
    }

    pt_stream_col <- function(nm) {
      if (!nm %in% names(x)) return(rep("", nrow(x)))
      v <- as.character(x[[nm]])
      v[is.na(v) | v == "NA"] <- ""
      v
    }

    pt_stream_num_text <- function(v, suffix = "") {
      txt <- as.character(v)
      txt[is.na(txt) | txt == "NA"] <- ""
      num <- suppressWarnings(as.numeric(txt))
      out <- ifelse(
        !is.na(num),
        paste0(format(round(num), big.mark = ",", trim = TRUE), suffix),
        ifelse(nzchar(trimws(txt)), paste0(txt, suffix), "Not available")
      )
      out
    }

    id <- pt_stream_col("nwsid")
    if (!any(pt_stream_has_text(id)) && "cnrfc_id" %in% names(x)) id <- pt_stream_col("cnrfc_id")
    id[!pt_stream_has_text(id)] <- "CNRFC"

    nickname <- pt_stream_col("nickname")
    gage_type_raw <- trimws(pt_stream_col("gage_type"))
    gage_class1_raw <- trimws(pt_stream_col("gage_class1"))
    gage_class2_raw <- trimws(pt_stream_col("gage_class2"))
    elev_ft <- pt_stream_col("elev_ft")

    role <- rep("CNRFC catalog point", nrow(x))
    class1 <- tolower(gage_class1_raw)
    class2 <- tolower(gage_class2_raw)
    gtype <- tolower(gage_type_raw)
    role[class1 == "reservoir" & (class2 == "forecast" | grepl("\\bforecast\\b", gtype))] <- "Reservoir forecast catalog point"
    role[class1 == "reservoir" & (class2 == "reservoir" | grepl("\\breservoir\\b", gtype))] <- "Reservoir catalog point"
    role[class1 == "river" & (class2 == "forecast" | grepl("\\bforecast\\b", gtype))] <- "River forecast catalog point"
    role[class1 == "river" & (class2 == "other" | grepl("\\bother\\b", gtype))] <- "River observation/other catalog point"
    role[class1 == "special"] <- "Special CNRFC catalog point"

    raw_type <- gage_type_raw
    fill_raw <- !pt_stream_has_text(raw_type)
    raw_type[fill_raw] <- trimws(paste(gage_class1_raw[fill_raw], gage_class2_raw[fill_raw], sep = ", "))
    raw_type <- gsub("^,\\s*|,\\s*$", "", raw_type)
    raw_type[!pt_stream_has_text(raw_type)] <- "Not available"

    cdec_id <- pt_stream_col("cdec_id")
    cdec_station_name <- pt_stream_col("cdec_station_name")
    cdec_station_url <- pt_stream_col("cdec_station_url")
    cdec_sensor15_hourly_url <- pt_stream_col("cdec_sensor15_hourly_url")
    cdec_match_confidence <- pt_stream_col("cdec_cnrfc_match_confidence")
    cdec_match_distance_m <- pt_stream_col("cdec_cnrfc_match_distance_m")

    status_df <- pt_cnrfc_stream_catalog_status_df(x)
    status <- status_df$status_text
    x$pt_catalog_status_class <- status_df$status_class
    x$pt_catalog_status_stroke_col <- status_df$status_stroke_col
    x$pt_catalog_status_weight <- status_df$status_weight

    popup <- vapply(seq_len(nrow(x)), function(i) {
      title <- if (pt_stream_has_text(nickname[i])) {
        paste0("<b>", pt_stream_escape(id[i]), "</b> – ", pt_stream_escape(nickname[i]))
      } else {
        paste0("<b>", pt_stream_escape(id[i]), "</b>")
      }

      lines <- c(
        title,
        paste0("<b>Catalog layer:</b> CNRFC river/reservoir catalog"),
        paste0("<b>BRIM context:</b> ", pt_stream_escape(status[i])),
        paste0("<b>CNRFC role:</b> ", pt_stream_escape(role[i])),
        paste0("<b>Raw CNRFC type:</b> ", pt_stream_escape(raw_type[i])),
        paste0("<b>Elevation:</b> ", pt_stream_escape(pt_stream_num_text(elev_ft[i], " ft")))
      )

      if (pt_stream_has_text(cdec_id[i])) {
        cdec_label <- if (pt_stream_has_text(cdec_station_name[i])) {
          paste0(cdec_id[i], " – ", cdec_station_name[i])
        } else {
          cdec_id[i]
        }

        match_note <- ""
        if (pt_stream_has_text(cdec_match_confidence[i])) {
          match_note <- paste0("; match: ", cdec_match_confidence[i])
        }
        if (pt_stream_has_text(cdec_match_distance_m[i])) {
          dist_num <- suppressWarnings(as.numeric(cdec_match_distance_m[i]))
          if (!is.na(dist_num)) {
            match_note <- paste0(match_note, ", ", format(round(dist_num), big.mark = ","), " m")
          }
        }

        lines <- c(
          lines,
          paste0("<b>CDEC cross-reference:</b> ", pt_stream_escape(cdec_label), pt_stream_escape(match_note))
        )
      }

      links <- character(0)
      if (pt_stream_has_text(cdec_id[i])) {
        cdec_url <- cdec_station_url[i]
        if (!pt_stream_has_text(cdec_url)) {
          cdec_url <- paste0("https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=", cdec_id[i])
        }
        links <- c(
          links,
          paste0("<a href='", pt_stream_escape(cdec_url), "' target='_blank' rel='noopener'>CDEC station (", pt_stream_escape(cdec_id[i]), ")</a>")
        )
        if (pt_stream_has_text(cdec_sensor15_hourly_url[i])) {
          links <- c(
            links,
            paste0("<a href='", pt_stream_escape(cdec_sensor15_hourly_url[i]), "' target='_blank' rel='noopener'>CDEC storage sensor 15</a>")
          )
        }
      }

      note <- paste0(
        "<div style='margin-top:7px;color:#555;font-size:11px;line-height:1.25;'>",
        "Catalog/screening layer for historical or fragmented CNRFC river/reservoir references. ",
        "Use Ops Live for the current CNRFC forecast-point layer and active operational products.",
        "</div>"
      )

      paste(c(lines, links, note), collapse = "<br/>")
    }, character(1))

    x$popup_html <- popup

    ## Compact hover: station ID, station name, and elevation only.
    ## Detailed catalog context stays in the popup and legend.
    x$hover_text <- vapply(seq_len(nrow(x)), function(i) {
      station_name <- if (pt_stream_has_text(nickname[i])) nickname[i] else "Name not available"
      paste0(
        id[i],
        "\n", station_name,
        "\nElevation: ", pt_stream_num_text(elev_ft[i], " ft")
      )
    }, character(1))

    x
  }

  if (isTRUE(map_display$add_cnrfc_stream)) {
    
    cnrfc_stream <- pt_clean_cnrfc_stream_catalog_popups(cnrfc_stream)
    cnrfc_stream <- pt_join_cnrfc_blm_distance_fields(cnrfc_stream, dataset = "river_reservoir")

    ## Register the overlay-group checkbox with a single invisible dummy marker.
    ## The visible catalog markers are created/rebuilt in the browser so CNRFC
    ## filters can redraw MarkerClusterGroups immediately, matching the USGS
    ## streamgage pattern.
    dummy <- data.frame(lng = -170, lat = 11)
    m <- m |>
      leaflet::addCircleMarkers(
        data = dummy,
        lng = ~lng,
        lat = ~lat,
        group = pt_layer_group_name("CNRFC river/reservoir catalog"),
        layerId = "pt_cnrfc_stream_catalog_dummy",
        radius = 0.001,
        stroke = FALSE,
        opacity = 0,
        fillOpacity = 0,
        options = leaflet::pathOptions(pane = "pane_points", interactive = FALSE)
      )

    m <- pt_add_cnrfc_stream_browser_layer(
      m = m,
      cnrfc_stream = cnrfc_stream,
      group_name = pt_layer_group_name("CNRFC river/reservoir catalog")
    )
  }
  
  if (isTRUE(map_display$add_cnrfc_precip)) {
    
    cnrfc_precip <- pt_add_cnrfc_hover_from_popup(
      cnrfc_precip,
      default_label = "CNRFC precip gage"
    )
    
    m <- m |>
      leaflet::addCircleMarkers(
        data = cnrfc_precip,
        group = pt_layer_group_name("CNRFC Precip Gages"),
        radius = 4,
        stroke = TRUE,
        weight = 0.7,
        color = ~stroke_col,
        fillColor = ~fill_col,
        fillOpacity = 0.75,
        popup = ~popup_html,
        label = ~hover_text,
        labelOptions = leaflet::labelOptions(
          direction = "auto",
          opacity = 0.9,
          textsize = "12px",
          style = list("white-space" = "pre",
                       "max-width" = "none")
        ),
        options = leaflet::pathOptions(pane = "pane_points"),
        clusterOptions = leaflet::markerClusterOptions()
      )
  }
  
  m
}




# ==== 3A.0 Browser-built CNRFC Local catalog layers =========================
##
## PURPOSE:
##   Draw dense Local CNRFC catalog point layers with browser-side filter and
##   MarkerClusterGroup rebuilds. This matches the USGS streamgage approach and
##   avoids stale cluster counts after filters are applied.

pt_add_cnrfc_stream_browser_layer <- function(m,
                                              cnrfc_stream = NULL,
                                              group_name = pt_layer_group_name("CNRFC river/reservoir catalog")) {

  if (!inherits(cnrfc_stream, "sf") && !is.data.frame(cnrfc_stream)) return(m)
  if (nrow(cnrfc_stream) == 0) return(m)

  x <- cnrfc_stream

  coords <- NULL
  if (inherits(x, "sf")) {
    x_ll <- try({
      crs <- sf::st_crs(x)
      if (!is.na(crs)) sf::st_transform(x, 4326) else x
    }, silent = TRUE)
    if (inherits(x_ll, "try-error")) x_ll <- x
    coords <- suppressWarnings(sf::st_coordinates(sf::st_geometry(x_ll)))
  } else if (all(c("lon", "lat") %in% names(x))) {
    coords <- cbind(X = suppressWarnings(as.numeric(x$lon)), Y = suppressWarnings(as.numeric(x$lat)))
  }

  if (is.null(coords) || nrow(coords) != nrow(x)) {
    warning("CNRFC river/reservoir browser layer skipped: coordinates unavailable.")
    return(m)
  }

  sx <- if (inherits(x, "sf")) sf::st_drop_geometry(x) else as.data.frame(x, stringsAsFactors = FALSE)
  sx$pt_lng <- as.numeric(coords[, "X"])
  sx$pt_lat <- as.numeric(coords[, "Y"])

  id_col <- c("nwsid", "cnrfc_id", "id", "station_id")
  id_col <- id_col[id_col %in% names(sx)]
  sx$pt_id <- if (length(id_col) > 0) as.character(sx[[id_col[[1]]]]) else as.character(seq_len(nrow(sx)))

  class1 <- if ("gage_class1" %in% names(sx)) tolower(trimws(as.character(sx$gage_class1))) else rep("", nrow(sx))
  sx$pt_stream_type <- dplyr::case_when(
    class1 == "river" ~ "river",
    class1 == "reservoir" ~ "reservoir",
    TRUE ~ "other"
  )

  keep <- c(
    "pt_id", "pt_lat", "pt_lng", "pt_stream_type",
    "nwsid", "cnrfc_id", "nickname", "gage_class1", "gage_class2", "gage_type", "elev_ft",
    "fill_col", "pt_catalog_status_stroke_col", "pt_catalog_status_weight",
    "hover_text", "popup_html",
    "on_blm_ca", "on_blm", "dist_to_blm_mi", "distance_to_blm_mi", "dist_to_blm_ft"
  )

  rec <- sx[, intersect(keep, names(sx)), drop = FALSE]
  rec$pt_id <- as.character(rec$pt_id)
  rec <- rec[!is.na(rec$pt_id) & rec$pt_id != "" & !is.na(rec$pt_lat) & !is.na(rec$pt_lng), , drop = FALSE]
  if (nrow(rec) == 0) return(m)

  js <- r"---(
function(el, x, data) {
  var map = this;
  var groupName = data && data.groupName ? String(data.groupName) : 'Points – CNRFC river/reservoir catalog';

  function rowsToArray(rows) {
    if (!rows) return [];
    if (Array.isArray(rows)) return rows;
    if (typeof rows === 'object') {
      var keys = Object.keys(rows), n = 0;
      for (var k = 0; k < keys.length; k++) if (Array.isArray(rows[keys[k]])) { n = rows[keys[k]].length; break; }
      var out = [];
      for (var i = 0; i < n; i++) {
        var r = {};
        for (var j = 0; j < keys.length; j++) r[keys[j]] = Array.isArray(rows[keys[j]]) ? rows[keys[j]][i] : rows[keys[j]];
        out.push(r);
      }
      return out;
    }
    return [];
  }

  var records = rowsToArray(data && data.records).filter(function(r) {
    return r && r.pt_id != null && r.pt_lat != null && r.pt_lng != null;
  });

  function has(v) {
    if (v === null || v === undefined) return false;
    var s = String(v).trim();
    return s !== '' && s !== 'NA' && s !== 'NaN' && s !== 'null' && s !== 'undefined';
  }
  function esc(v) {
    if (!has(v)) return 'NA';
    return String(v).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;').replace(/'/g, '&#39;');
  }
  function num(v) {
    if (v === null || v === undefined || v === '') return null;
    var n = Number(v);
    return isNaN(n) ? null : n;
  }
  function bool(v) {
    if (v === true) return true;
    if (v === false || v === null || v === undefined) return false;
    var s = String(v).trim().toLowerCase();
    return ['true','t','1','yes','y'].indexOf(s) >= 0;
  }
  function norm(s) {
    return String(s == null ? '' : s).replace(/&amp;/g, '&').replace(/[–—]/g, '-')
      .toLowerCase().replace(/points\s*-\s*/g, '').replace(/monitoring sites\s*\/\s*records\s*-\s*/g, '')
      .replace(/\s*\([^)]*\)\s*$/g, '').replace(/\s+/g, ' ').trim();
  }
  function isLayerText(s) { return norm(s).indexOf('cnrfc river/reservoir catalog') >= 0; }
  function safeControlScan() {
    if (typeof document === 'undefined' || !document.querySelectorAll) return null;
    var labels = document.querySelectorAll('.leaflet-control-layers-overlays label');
    for (var i = 0; i < labels.length; i++) {
      var label = labels[i], text = label.textContent || label.innerText || '';
      if (!isLayerText(text)) continue;
      var input = label.querySelector ? label.querySelector('input[type="checkbox"]') : null;
      if (input) return !!input.checked;
    }
    return null;
  }
  function eventMatches(evt) {
    if (!evt) return false;
    if (isLayerText(evt.name)) return true;
    if (evt.layer && evt.layer.options) return isLayerText([evt.layer.options.group, evt.layer.options.name, evt.layer.options.layerId].join(' '));
    return false;
  }
  function elevFt(r) { return num(r.elev_ft); }
  function onBlm(r) { return bool(r.on_blm_ca) || bool(r.on_blm); }
  function distMi(r) {
    var n = num(r.dist_to_blm_mi);
    if (n !== null) return n;
    return num(r.distance_to_blm_mi);
  }
  function kind(r) {
    var k = String(r.pt_stream_type || '').toLowerCase();
    return ['river','reservoir','other'].indexOf(k) >= 0 ? k : 'other';
  }
  function recordPasses(r, f) {
    f = f || {};
    if (f.kind && f.kind !== 'all' && kind(r) !== f.kind) return false;
    var e = elevFt(r), dm = distMi(r);
    if (f.elevMin != null && (e === null || e < Number(f.elevMin))) return false;
    if (f.elevMax != null && (e === null || e > Number(f.elevMax))) return false;
    if (f.blmMode === 'on' && !onBlm(r)) return false;
    if (f.blmMode === 'distance' && (dm === null || dm > Number(f.blmMax))) return false;
    return true;
  }
  function makePopup(r) {
    if (has(r.popup_html)) return String(r.popup_html);
    var id = has(r.nwsid) ? r.nwsid : (has(r.cnrfc_id) ? r.cnrfc_id : r.pt_id);
    var parts = [];
    parts.push('<b>' + esc(id) + '</b> – ' + esc(r.nickname));
    parts.push('<b>Type:</b> ' + esc(r.gage_class1 || r.gage_type));
    parts.push('<b>Elevation:</b> ' + esc(r.elev_ft) + ' ft');
    return parts.join('<br/>');
  }
  function fmtElev(v) {
    var e = num(v);
    if (e === null) return 'not available';
    return Math.round(e).toLocaleString() + ' ft';
  }
  function makeTooltip(r) {
    var id = has(r.nwsid) ? r.nwsid : (has(r.cnrfc_id) ? r.cnrfc_id : r.pt_id);
    var nm = has(r.nickname) ? r.nickname : 'Name not available';
    return esc(id) + '<br/>' + esc(nm) + '<br/>Elevation: ' + esc(fmtElev(r.elev_ft));
  }
  function styleFor(r) {
    var k = kind(r);
    var fill = has(r.fill_col) ? String(r.fill_col) : (k === 'river' ? '#6BAED6' : (k === 'reservoir' ? '#08306B' : '#8C8C8C'));
    var stroke = has(r.pt_catalog_status_stroke_col) ? String(r.pt_catalog_status_stroke_col) : '#444444';
    var weight = num(r.pt_catalog_status_weight);
    return {fill: fill, stroke: stroke, weight: weight || 1.1};
  }
  function springLabelText(r) {
    var nm = has(r.spring_name_display) ? String(r.spring_name_display).trim() : '';
    if (!has(nm)) return '';
    // Avoid thousands of visually unhelpful duplicate labels when the source
    // only says the feature is unnamed. The hover/popup still displays those
    // records normally.
    if (/^unnamed\s+spring$/i.test(nm)) return '';
    return nm;
  }

  function makeSpringLabelMarker(r) {
    var lat = Number(r.pt_lat), lng = Number(r.pt_lng);
    if (isNaN(lat) || isNaN(lng)) return null;
    var txt = springLabelText(r);
    if (!has(txt)) return null;
    return L.marker([lat, lng], {
      interactive: false,
      keyboard: false,
      pane: 'pane_labels_pts',
      icon: L.divIcon({
        className: 'pt-springs-local-label-divicon',
        html: '<span>' + esc(txt) + '</span>',
        iconSize: L.point(1, 1),
        iconAnchor: L.point(0, 0)
      })
    });
  }

  function makeMarker(r) {
    var lat = Number(r.pt_lat), lng = Number(r.pt_lng);
    if (isNaN(lat) || isNaN(lng)) return null;
    var st = styleFor(r);
    var marker = L.circleMarker([lat, lng], {
      radius: 4,
      color: st.stroke,
      weight: st.weight,
      opacity: 0.92,
      fillColor: st.fill,
      fillOpacity: 0.76,
      pane: 'pane_points'
    });
    marker.bindTooltip(makeTooltip(r), {direction:'auto', opacity:0.9, sticky:true, className:'pt-cnrfc-local-tooltip'});
    marker.bindPopup(makePopup(r), {maxWidth:420, maxHeight:520});
    return marker;
  }

  var markers = (typeof L.markerClusterGroup === 'function') ? L.markerClusterGroup({
    disableClusteringAtZoom: 12,
    maxClusterRadius: function(z) { if (z <= 6) return 125; if (z <= 8) return 110; if (z <= 10) return 90; if (z <= 11) return 70; return 45; },
    spiderfyOnMaxZoom: true, showCoverageOnHover: false, animate: false,
    removeOutsideVisibleBounds: true, chunkedLoading: true, chunkInterval: 120, chunkDelay: 15
  }) : L.layerGroup();

  var currentFilters = {kind:'all', elevMin:null, elevMax:null, blmMode:'any', blmMax:null};
  var layerActive = false;
  var lastFiltered = records.length;

  function rebuild(filters) {
    currentFilters = filters || currentFilters || {};
    markers.clearLayers();
    var n = 0;
    records.forEach(function(r) {
      if (!recordPasses(r, currentFilters)) return;
      var marker = makeMarker(r);
      if (!marker) return;
      markers.addLayer(marker);
      n += 1;
    });
    lastFiltered = n;
    if (layerActive && !map.hasLayer(markers)) markers.addTo(map);
    return {filtered:n, total:records.length, drawn:n};
  }
  function syncActive() {
    var checked = safeControlScan();
    layerActive = checked === null ? layerActive : checked;
    if (layerActive) { rebuild(currentFilters); if (!map.hasLayer(markers)) markers.addTo(map); }
    else { if (map.hasLayer(markers)) map.removeLayer(markers); }
  }

  window.BRIM_CNRFC_STREAM_LOCAL = {
    applyFilters: function(filters) { currentFilters = filters || currentFilters; return rebuild(currentFilters); },
    setActive: function(active) { layerActive = !!active; syncActive(); },
    stats: function() { return {filtered:lastFiltered, total:records.length, active:layerActive}; }
  };

  map.on('overlayadd', function(evt) { if (eventMatches(evt)) { layerActive = true; syncActive(); } });
  map.on('overlayremove', function(evt) { if (eventMatches(evt)) { layerActive = false; if (map.hasLayer(markers)) map.removeLayer(markers); } });
  if (typeof document !== 'undefined' && document.addEventListener) {
    document.addEventListener('change', function(evt) {
      var t = evt && evt.target;
      if (t && t.matches && t.matches('.leaflet-control-layers-overlays input[type="checkbox"]')) setTimeout(syncActive, 0);
    }, true);
  }
  setTimeout(syncActive, 0);
  setTimeout(syncActive, 500);
}
)---"

  htmlwidgets::onRender(m, js, data = list(groupName = group_name, records = rec))
}

pt_add_cnrfc_weather_browser_layer <- function(m,
                                               cnrfc_weather = NULL,
                                               group_name = pt_layer_group_name("CNRFC weather station catalog")) {

  if (!is.data.frame(cnrfc_weather) || nrow(cnrfc_weather) == 0) return(m)
  if (!all(c("lat", "lon") %in% names(cnrfc_weather))) return(m)

  x <- as.data.frame(cnrfc_weather, stringsAsFactors = FALSE)
  x$pt_lng <- suppressWarnings(as.numeric(x$lon))
  x$pt_lat <- suppressWarnings(as.numeric(x$lat))
  x$pt_id <- if ("cnrfc_id" %in% names(x)) as.character(x$cnrfc_id) else as.character(seq_len(nrow(x)))

  keep <- c(
    "pt_id", "pt_lat", "pt_lng",
    "cnrfc_id", "display_name", "elev_ft",
    "local_source_class", "local_source_label", "local_source_label_full",
    "local_fill_col", "local_stroke_col", "local_catalog_status_weight", "local_catalog_status_stroke_col",
    "local_hover_text", "local_popup_html",
    "on_blm_ca", "on_blm", "dist_to_blm_mi", "distance_to_blm_mi", "dist_to_blm_ft"
  )

  rec <- x[, intersect(keep, names(x)), drop = FALSE]
  rec <- rec[!is.na(rec$pt_id) & rec$pt_id != "" & !is.na(rec$pt_lat) & !is.na(rec$pt_lng), , drop = FALSE]
  if (nrow(rec) == 0) return(m)

  js <- r"---(
function(el, x, data) {
  var map = this;
  var groupName = data && data.groupName ? String(data.groupName) : 'Points – CNRFC weather station catalog';

  function rowsToArray(rows) {
    if (!rows) return [];
    if (Array.isArray(rows)) return rows;
    if (typeof rows === 'object') {
      var keys = Object.keys(rows), n = 0;
      for (var k = 0; k < keys.length; k++) if (Array.isArray(rows[keys[k]])) { n = rows[keys[k]].length; break; }
      var out = [];
      for (var i = 0; i < n; i++) {
        var r = {};
        for (var j = 0; j < keys.length; j++) r[keys[j]] = Array.isArray(rows[keys[j]]) ? rows[keys[j]][i] : rows[keys[j]];
        out.push(r);
      }
      return out;
    }
    return [];
  }
  var records = rowsToArray(data && data.records).filter(function(r) { return r && r.pt_id != null && r.pt_lat != null && r.pt_lng != null; });
  function has(v) { if (v === null || v === undefined) return false; var s=String(v).trim(); return s!=='' && s!=='NA' && s!=='NaN' && s!=='null' && s!=='undefined'; }
  function esc(v) { if (!has(v)) return 'NA'; return String(v).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;').replace(/'/g,'&#39;'); }
  function num(v) { if (v===null || v===undefined || v==='') return null; var n=Number(v); return isNaN(n)?null:n; }
  function bool(v) { if (v===true) return true; if (v===false || v===null || v===undefined) return false; var s=String(v).trim().toLowerCase(); return ['true','t','1','yes','y'].indexOf(s)>=0; }
  function norm(s) { return String(s==null?'':s).replace(/&amp;/g,'&').replace(/[–—]/g,'-').toLowerCase().replace(/points\s*-\s*/g,'').replace(/monitoring sites\s*\/\s*records\s*-\s*/g,'').replace(/\s*\([^)]*\)\s*$/g,'').replace(/\s+/g,' ').trim(); }
  function isLayerText(s) { return norm(s).indexOf('cnrfc weather station catalog') >= 0; }
  function safeControlScan() {
    if (typeof document === 'undefined' || !document.querySelectorAll) return null;
    var labels=document.querySelectorAll('.leaflet-control-layers-overlays label');
    for (var i=0;i<labels.length;i++) { var label=labels[i], text=label.textContent||label.innerText||''; if (!isLayerText(text)) continue; var input=label.querySelector?label.querySelector('input[type="checkbox"]'):null; if (input) return !!input.checked; }
    return null;
  }
  function eventMatches(evt) { if (!evt) return false; if (isLayerText(evt.name)) return true; if (evt.layer && evt.layer.options) return isLayerText([evt.layer.options.group, evt.layer.options.name, evt.layer.options.layerId].join(' ')); return false; }
  function elevFt(r) { return num(r.elev_ft); }
  function onBlm(r) { return bool(r.on_blm_ca) || bool(r.on_blm); }
  function distMi(r) { var n=num(r.dist_to_blm_mi); if (n!==null) return n; return num(r.distance_to_blm_mi); }
  function kind(r) { var k=String(r.local_source_class||'').toLowerCase(); if (k==='goes') return 'raws_goes'; if (['asos','alert','raws_goes','other'].indexOf(k)>=0) return k; return 'other'; }
  function recordPasses(r,f) {
    f=f||{};
    if (f.kind && f.kind!=='all' && kind(r)!==f.kind) return false;
    var e=elevFt(r), dm=distMi(r);
    if (f.elevMin!=null && (e===null || e<Number(f.elevMin))) return false;
    if (f.elevMax!=null && (e===null || e>Number(f.elevMax))) return false;
    if (f.blmMode==='on' && !onBlm(r)) return false;
    if (f.blmMode==='distance' && (dm===null || dm>Number(f.blmMax))) return false;
    return true;
  }
  function makePopup(r) { if (has(r.local_popup_html)) return String(r.local_popup_html); var id=has(r.cnrfc_id)?r.cnrfc_id:r.pt_id; return '<b>'+esc(id)+'</b> – '+esc(r.display_name)+'<br/><b>Type:</b> '+esc(r.local_source_label)+'<br/><b>Elevation:</b> '+esc(r.elev_ft)+' ft'; }
  function fmtElev(v) { var e=num(v); if(e===null)return 'not available'; return Math.round(e).toLocaleString()+' ft'; }
  function makeTooltip(r) { var id=has(r.cnrfc_id)?r.cnrfc_id:r.pt_id; var nm=has(r.display_name)?r.display_name:'Name not available'; return esc(id)+'<br/>'+esc(nm)+'<br/>Elevation: '+esc(fmtElev(r.elev_ft)); }
  function makeMarker(r) {
    var lat=Number(r.pt_lat), lng=Number(r.pt_lng); if (isNaN(lat)||isNaN(lng)) return null;
    var marker=L.circleMarker([lat,lng], {radius:4, color:has(r.local_catalog_status_stroke_col)?String(r.local_catalog_status_stroke_col):(has(r.local_stroke_col)?String(r.local_stroke_col):'#4d4d4d'), weight:num(r.local_catalog_status_weight)||1.1, opacity:0.92, fillColor:has(r.local_fill_col)?String(r.local_fill_col):'#bdbdbd', fillOpacity:0.76, pane:'pane_points'});
    marker.bindTooltip(makeTooltip(r), {direction:'auto', opacity:0.9, sticky:true, className:'pt-cnrfc-local-tooltip'});
    marker.bindPopup(makePopup(r), {maxWidth:420, maxHeight:520});
    return marker;
  }
  var markers=(typeof L.markerClusterGroup==='function')?L.markerClusterGroup({disableClusteringAtZoom:12,maxClusterRadius:function(z){if(z<=6)return 130;if(z<=8)return 115;if(z<=10)return 95;if(z<=11)return 75;return 50;},spiderfyOnMaxZoom:true,showCoverageOnHover:false,animate:false,removeOutsideVisibleBounds:true,chunkedLoading:true,chunkInterval:120,chunkDelay:15}):L.layerGroup();
  var currentFilters={kind:'all',elevMin:null,elevMax:null,blmMode:'any',blmMax:null};
  var layerActive=false, lastFiltered=records.length;
  function rebuild(filters) { currentFilters=filters||currentFilters||{}; markers.clearLayers(); var n=0; records.forEach(function(r){ if(!recordPasses(r,currentFilters)) return; var marker=makeMarker(r); if(!marker)return; markers.addLayer(marker); n+=1; }); lastFiltered=n; if(layerActive && !map.hasLayer(markers)) markers.addTo(map); return {filtered:n,total:records.length,drawn:n}; }
  function syncActive() { var checked=safeControlScan(); layerActive=checked===null?layerActive:checked; if(layerActive){rebuild(currentFilters); if(!map.hasLayer(markers)) markers.addTo(map);} else { if(map.hasLayer(markers)) map.removeLayer(markers); } }
  window.BRIM_CNRFC_WEATHER_LOCAL={applyFilters:function(filters){currentFilters=filters||currentFilters; return rebuild(currentFilters);},setActive:function(active){layerActive=!!active; syncActive();},stats:function(){return {filtered:lastFiltered,total:records.length,active:layerActive};}};
  map.on('overlayadd',function(evt){if(eventMatches(evt)){layerActive=true;rebuild(currentFilters);}});
  map.on('overlayremove',function(evt){if(eventMatches(evt)){layerActive=false;if(map.hasLayer(markers))map.removeLayer(markers);}});
  if(typeof document!=='undefined'&&document.addEventListener){document.addEventListener('change',function(evt){var t=evt&&evt.target;if(t&&t.matches&&t.matches('.leaflet-control-layers-overlays input[type="checkbox"]')) setTimeout(syncActive,0);},true);}
  setTimeout(syncActive,0); setTimeout(syncActive,500);
}
)---"

  htmlwidgets::onRender(m, js, data = list(groupName = group_name, records = rec))
}

# ==== 3A. CNRFC precip/weather station catalog local layer ====================
##
## PURPOSE:
##   Draw the broad local CNRFC precip/weather station-authority catalog built
##   by 54_.  This is intentionally broader than the Ops Live NWS/WRH layer:
##   it includes ASOS/airport, RAWS/GOES-style, ALERT/event, and other catalog
##   stations for project-area screening.  The Ops Live layer remains the
##   verified/current-ish time-series subset.

pt_add_cnrfc_precip_weather_station_catalog_layer <- function(m,
                                                               cnrfc_precip_weather_station_catalog,
                                                               map_display) {

  if (!isTRUE(map_display$add_cnrfc_precip_weather_station_catalog)) {
    return(m)
  }

  x <- cnrfc_precip_weather_station_catalog

  if (!is.data.frame(x) || nrow(x) == 0) {
    return(m)
  }

  if (!all(c("lat", "lon") %in% names(x))) {
    warning("CNRFC weather station catalog local layer skipped: lat/lon fields not available.")
    return(m)
  }

  x <- as.data.frame(x, stringsAsFactors = FALSE)
  x <- x[!is.na(x$lat) & !is.na(x$lon) &
           abs(as.numeric(x$lat)) <= 90 & abs(as.numeric(x$lon)) <= 180 &
           !(as.numeric(x$lat) == 0 & as.numeric(x$lon) == 0), , drop = FALSE]

  if (nrow(x) == 0) {
    return(m)
  }

  pt_html_escape <- function(v) {
    v <- as.character(v)
    v[is.na(v) | v == ""] <- "NA"
    v <- gsub("&", "&amp;", v, fixed = TRUE)
    v <- gsub("<", "&lt;", v, fixed = TRUE)
    v <- gsub(">", "&gt;", v, fixed = TRUE)
    v <- gsub('"', "&quot;", v, fixed = TRUE)
    v
  }

  pt_has_text <- function(v) {
    !is.na(v) & trimws(as.character(v)) != ""
  }

  pt_bool <- function(v) {
    if (is.logical(v)) return(!is.na(v) & v)
    vv <- tolower(trimws(as.character(v)))
    vv %in% c("true", "t", "1", "yes", "y")
  }

  pt_num_text <- function(v, suffix = "") {
    n <- suppressWarnings(as.numeric(v))
    ifelse(is.na(n), "not available", paste0(format(round(n, 0), big.mark = ",", scientific = FALSE), suffix))
  }

  source_class <- if ("source_class" %in% names(x)) as.character(x$source_class) else rep("Other / not parsed", nrow(x))
  raw_code <- if ("raw_precip_source_code" %in% names(x)) as.character(x$raw_precip_source_code) else rep(NA_character_, nrow(x))

  x$local_source_class <- dplyr::case_when(
    source_class == "ASOS (airport)" ~ "asos",
    source_class == "GOES" ~ "goes",
    source_class == "ALERT (event only)" ~ "alert",
    TRUE ~ "other"
  )

  x$local_source_label <- dplyr::case_when(
    x$local_source_class == "asos" ~ "ASOS/airport",
    x$local_source_class == "goes" ~ "RAWS/GOES",
    x$local_source_class == "alert" ~ "ALERT/event",
    TRUE ~ "Other station"
  )

  x$local_source_label_full <- ifelse(
    pt_has_text(raw_code),
    paste0(x$local_source_label, " (", raw_code, ")"),
    x$local_source_label
  )

  x$local_fill_col <- dplyr::case_when(
    x$local_source_class == "goes" ~ "#e6550d",
    x$local_source_class == "asos" ~ "#2b8cbe",
    x$local_source_class == "alert" ~ "#756bb1",
    TRUE ~ "#bdbdbd"
  )

  x$local_stroke_col <- dplyr::case_when(
    x$local_source_class == "goes" ~ "#7f2704",
    x$local_source_class == "asos" ~ "#084081",
    x$local_source_class == "alert" ~ "#3f007d",
    TRUE ~ "#4d4d4d"
  )

  id <- if ("cnrfc_id" %in% names(x)) as.character(x$cnrfc_id) else rep("CNRFC", nrow(x))
  nm <- if ("display_name" %in% names(x)) as.character(x$display_name) else id
  elev <- if ("elev_ft" %in% names(x)) x$elev_ft else rep(NA_real_, nrow(x))
  wrh <- if ("wrh_timeseries_url" %in% names(x)) as.character(x$wrh_timeseries_url) else rep(NA_character_, nrow(x))
  cnrfc_url <- if ("cnrfc_source_url" %in% names(x)) as.character(x$cnrfc_source_url) else rep("https://www.cnrfc.noaa.gov/rainfall_data.php", nrow(x))
  also_river <- if ("also_active_river_reservoir_forecast_point" %in% names(x)) pt_bool(x$also_active_river_reservoir_forecast_point) else rep(FALSE, nrow(x))
  auth_note <- if ("station_authority_note" %in% names(x)) as.character(x$station_authority_note) else rep("CNRFC station catalog record", nrow(x))

  ## Local-vs-Ops status for the broad station catalog.  The Ops Live NWS/WRH
  ## weather layer contains VERIFIED time-series rows only.  Be intentionally
  ## strict here: older logic used grepl("verified", ...), which also matched
  ## "candidate ... not verified" rows and overstated the pink-halo subset.
  ##
  ## Priority order:
  ##   1. explicit boolean audit fields from 54_
  ##   2. exact verified availability classes from 54_
  ##   3. exact map-product class, if present in a future cache
  pt_weather_verified_ops <- function(df) {
    n <- nrow(df)
    out <- rep(FALSE, n)

    if ("is_ops_timeseries_verified" %in% names(df)) {
      out <- out | pt_bool(df$is_ops_timeseries_verified)
    }

    if ("noaa_api_verified" %in% names(df)) {
      out <- out | pt_bool(df$noaa_api_verified)
    }

    if ("noaa_station_data_availability_class" %in% names(df)) {
      cls <- tolower(trimws(as.character(df$noaa_station_data_availability_class)))
      out <- out | cls %in% c(
        "verified_api_latest_observation",
        "verified_api_recent_observation",
        "verified_wrh_static_station_page"
      )
    }

    if ("map_product_class" %in% names(df)) {
      cls <- tolower(trimws(as.character(df$map_product_class)))
      out <- out | cls == "verified_noaa_station_timeseries"
    }

    out
  }

  ## Use the exact Ops Live map-ready RDS when available.  This keeps the
  ## Local pink-halo count aligned with the actual Ops Live NWS/WRH layer.
  x$local_in_ops_live <- pt_cnrfc_weather_in_ops_live_exact(
    x,
    fallback = pt_weather_verified_ops(x)
  )
  x$local_catalog_status_label <- ifelse(
    x$local_in_ops_live,
    "Ops Live NWS/WRH time-series site",
    "Catalog-only"
  )
  x$local_catalog_status_stroke_col <- ifelse(x$local_in_ops_live, "#ff00cc", "#444444")
  x$local_catalog_status_weight <- ifelse(x$local_in_ops_live, 1.5, 1.1)

  x$local_hover_text <- paste0(
    id,
    "\n", ifelse(pt_has_text(nm) & nm != id, nm, "Name not available"),
    "\nElevation: ", pt_num_text(elev, " ft")
  )

  x$local_popup_html <- vapply(seq_len(nrow(x)), function(i) {
    parts <- c(
      paste0("<b>", pt_html_escape(id[i]), "</b>",
             if (pt_has_text(nm[i]) && nm[i] != id[i]) paste0(" – ", pt_html_escape(nm[i])) else ""),
      paste0("<b>Station type:</b> ", pt_html_escape(x$local_source_label_full[i])),
      paste0("<b>BRIM context:</b> ", pt_html_escape(x$local_catalog_status_label[i])),
      paste0("<b>Elevation:</b> ", pt_html_escape(pt_num_text(elev[i], " ft"))),
      paste0("<b>Catalog note:</b> ", pt_html_escape(auth_note[i]))
    )

    if (isTRUE(also_river[i])) {
      parts <- c(parts, "<span style='color:#555;'>Also appears in CNRFC river/reservoir catalog.</span>")
    }

    links <- character(0)
    if (pt_has_text(wrh[i])) {
      links <- c(links, paste0("<a href='", pt_html_escape(wrh[i]), "' target='_blank' rel='noopener'>NWS WRH time series</a>"))
    }
    if (pt_has_text(cnrfc_url[i])) {
      links <- c(links, paste0("<a href='", pt_html_escape(cnrfc_url[i]), "' target='_blank' rel='noopener'>CNRFC rainfall / station data</a>"))
    }

    if (length(links) > 0) {
      parts <- c(parts, paste0("<div style='margin-top:5px;'>", paste(links, collapse = "<br>"), "</div>"))
    }

    parts <- c(
      parts,
      "<div style='margin-top:5px;color:#555;font-size:11px;'>Broad CNRFC precip/weather station catalog for project screening. Some ALERT/event stations may report primarily during storms; time-series availability varies by station.</div>"
    )

    paste0("<div style='font:12px/1.3 Arial, Helvetica, sans-serif; min-width:230px; max-width:380px;'>",
           paste(parts, collapse = "<br>"),
           "</div>")
  }, character(1))

  x <- pt_join_cnrfc_blm_distance_fields(x, dataset = "weather_station", lon_col = "lon", lat_col = "lat")

  group_name <- pt_layer_group_name("CNRFC weather station catalog")

  ## Register the overlay-group checkbox with a single invisible dummy marker.
  ## The visible weather-station markers are rebuilt in the browser so filters
  ## immediately redraw the MarkerClusterGroup.
  dummy <- data.frame(lng = -170, lat = 12)
  m <- m |>
    leaflet::addCircleMarkers(
      data = dummy,
      lng = ~lng,
      lat = ~lat,
      group = group_name,
      layerId = "pt_cnrfc_weather_catalog_dummy",
      radius = 0.001,
      stroke = FALSE,
      opacity = 0,
      fillOpacity = 0,
      options = leaflet::pathOptions(pane = "pane_points", interactive = FALSE)
    )

  pt_add_cnrfc_weather_browser_layer(
    m = m,
    cnrfc_weather = x,
    group_name = group_name
  )


}


# ==== 3B. CNRFC Local catalog dynamic filter legends ========================
##
## Canonical CNRFC Local catalog legend/controller.
##
## PURPOSE:
##   Add one compact bottom-left legend/filter panel for the two Local CNRFC
##   catalog point layers. This is the maintained implementation; older static
##   duplicate helper code was removed to avoid developer confusion.


pt_add_cnrfc_local_catalog_legends <- function(m,
                                               cnrfc_stream = NULL,
                                               cnrfc_precip_weather_station_catalog = NULL,
                                               map_display = NULL) {

  if (is.null(map_display)) map_display <- list()

  pt_legend_bool <- function(v) {
    if (is.logical(v)) return(!is.na(v) & v)
    vv <- tolower(trimws(as.character(v)))
    vv %in% c("true", "t", "1", "yes", "y")
  }

  stream_group <- pt_layer_group_name("CNRFC river/reservoir catalog")
  weather_group <- pt_layer_group_name("CNRFC weather station catalog")

  stream_counts <- list(total = 0L, river = 0L, reservoir = 0L, other = 0L, also_ops = NA_integer_)
  if (isTRUE(map_display$add_cnrfc_stream) && inherits(cnrfc_stream, "sf") && nrow(cnrfc_stream) > 0) {
    sx <- cnrfc_stream
    cls <- if ("gage_class1" %in% names(sx)) tolower(trimws(as.character(sx$gage_class1))) else rep("", nrow(sx))
    stream_counts$total <- as.integer(nrow(sx))
    stream_counts$river <- as.integer(sum(cls == "river", na.rm = TRUE))
    stream_counts$reservoir <- as.integer(sum(cls == "reservoir", na.rm = TRUE))
    stream_counts$other <- as.integer(sum(!cls %in% c("river", "reservoir"), na.rm = TRUE))

    id_col <- c("nwsid", "cnrfc_id", "id", "station_id")
    id_col <- id_col[id_col %in% names(sx)]
    if (length(id_col) > 0) {
      ids <- toupper(trimws(as.character(sx[[id_col[[1]]]])))
      local_only_path <- file.path("04_processed_data", "qa", "cnrfc_river_reservoir_local_only_latest.csv")
      if (file.exists(local_only_path)) {
        local_only <- tryCatch(utils::read.csv(local_only_path, stringsAsFactors = FALSE), error = function(e) data.frame())
        if ("cnrfc_id" %in% names(local_only)) {
          local_only_ids <- toupper(trimws(as.character(local_only$cnrfc_id)))
          local_only_ids <- local_only_ids[nzchar(local_only_ids) & !is.na(local_only_ids)]
          stream_counts$also_ops <- as.integer(sum(!is.na(ids) & nzchar(ids) & !ids %in% local_only_ids))
        }
      }
    }
  }

  weather_counts <- list(total = 0L, asos = 0L, raws_goes = 0L, alert = 0L, other = 0L, also_ops = NA_integer_)
  if (isTRUE(map_display$add_cnrfc_precip_weather_station_catalog) &&
      is.data.frame(cnrfc_precip_weather_station_catalog) &&
      nrow(cnrfc_precip_weather_station_catalog) > 0) {
    wx <- as.data.frame(cnrfc_precip_weather_station_catalog, stringsAsFactors = FALSE)
    if (all(c("lat", "lon") %in% names(wx))) {
      lat <- suppressWarnings(as.numeric(wx$lat)); lon <- suppressWarnings(as.numeric(wx$lon))
      wx <- wx[!is.na(lat) & !is.na(lon) & abs(lat) <= 90 & abs(lon) <= 180 & !(lat == 0 & lon == 0), , drop = FALSE]
    }
    source_class <- if ("source_class" %in% names(wx)) as.character(wx$source_class) else rep("Other / not parsed", nrow(wx))
    local_source_class <- dplyr::case_when(
      source_class == "ASOS (airport)" ~ "asos",
      source_class == "GOES" ~ "raws_goes",
      source_class == "ALERT (event only)" ~ "alert",
      TRUE ~ "other"
    )
    weather_counts$total <- as.integer(length(local_source_class))
    weather_counts$asos <- as.integer(sum(local_source_class == "asos", na.rm = TRUE))
    weather_counts$raws_goes <- as.integer(sum(local_source_class == "raws_goes", na.rm = TRUE))
    weather_counts$alert <- as.integer(sum(local_source_class == "alert", na.rm = TRUE))
    weather_counts$other <- as.integer(sum(local_source_class == "other", na.rm = TRUE))

    in_ops <- rep(FALSE, nrow(wx))
    if ("is_ops_timeseries_verified" %in% names(wx)) {
      in_ops <- in_ops | pt_legend_bool(wx$is_ops_timeseries_verified)
    }
    if ("noaa_api_verified" %in% names(wx)) {
      in_ops <- in_ops | pt_legend_bool(wx$noaa_api_verified)
    }
    if ("noaa_station_data_availability_class" %in% names(wx)) {
      cls <- tolower(trimws(as.character(wx$noaa_station_data_availability_class)))
      in_ops <- in_ops | cls %in% c(
        "verified_api_latest_observation",
        "verified_api_recent_observation",
        "verified_wrh_static_station_page"
      )
    }
    if ("map_product_class" %in% names(wx)) {
      cls <- tolower(trimws(as.character(wx$map_product_class)))
      in_ops <- in_ops | cls == "verified_noaa_station_timeseries"
    }

    ## Prefer exact membership in the Ops Live map-ready RDS over inferred
    ## audit classes so the Local halo count matches the actual Ops layer.
    in_ops <- pt_cnrfc_weather_in_ops_live_exact(wx, fallback = in_ops)
    weather_counts$also_ops <- as.integer(sum(in_ops, na.rm = TRUE))
  }

  pt_jsq <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")

  js <- r"---(
function(el, x) {
  var map = this;
  var streamGroup = __STREAM_GROUP__;
  var weatherGroup = __WEATHER_GROUP__;
  var streamCounts = __STREAM_COUNTS__;
  var weatherCounts = __WEATHER_COUNTS__;
  var active = {stream:false, weather:false};
  var filters = {
    stream: {kind:'all', elevMin:null, elevMax:null, blmMode:'any', blmMax:null},
    weather: {kind:'all', elevMin:null, elevMax:null, blmMode:'any', blmMax:null}
  };
  var lastShown = {stream: streamCounts.total || 0, weather: weatherCounts.total || 0};

  // Closeout pilot for the shared CNRFC Local catalog legend.  Keep the state
  // local to this legend: closing the legend hides it without clearing filters,
  // and only toggling one of this legend's own source layers re-opens it.
  var legendUserHidden = false;

  function esc(s) { return String(s == null ? '' : s).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;').replace(/'/g,'&#39;'); }
  function fmt(n) { if (n == null || isNaN(Number(n))) return '—'; return Number(n).toLocaleString(); }
  function cleanNum(v) { if (v == null) return null; var s=String(v).trim(); if (s==='') return null; var n=Number(s); return isNaN(n)?null:n; }
  function norm(s) { return String(s == null ? '' : s).replace(/&amp;/g,'&').replace(/[–—]/g,'-').toLowerCase().replace(/points\s*-\s*/g,'').replace(/monitoring sites\s*\/\s*records\s*-\s*/g,'').replace(/\s*\([^)]*\)\s*$/g,'').replace(/\s+/g,' ').trim(); }
  function kindFromText(s) { var g=norm(s); if (g.indexOf('cnrfc river/reservoir catalog')>=0) return 'stream'; if (g.indexOf('cnrfc weather station catalog')>=0) return 'weather'; return null; }
  function layerText(layer) { var out=[]; if(layer&&layer.options){out.push(layer.options.group);out.push(layer.options.layerId);out.push(layer.options.id);out.push(layer.options.name);} return out.join(' '); }
  function eventKind(evt){ if(!evt) return null; return kindFromText(evt.name)||kindFromText(layerText(evt.layer)); }
  function markEvent(evt,on){ var k=eventKind(evt); if(k) active[k]=!!on; return k; }
  function safeControlScan(){ var found={stream:null,weather:null}; if(typeof document==='undefined'||!document.querySelectorAll)return found; var labels=document.querySelectorAll('.leaflet-control-layers-overlays label'); for(var i=0;i<labels.length;i++){var label=labels[i],text=label.textContent||label.innerText||'',k=kindFromText(text); if(!k)continue; var input=label.querySelector?label.querySelector('input[type="checkbox"]'):null; if(input)found[k]=!!input.checked;} return found; }
  function isActive(k){ var scan=safeControlScan(); if(scan[k]!==null)return scan[k]; return !!active[k]; }

  function dot(fill, stroke) { return '<span class="pt-cnrfc-local-dot" style="background:'+fill+';border-color:'+stroke+';"></span>'; }
  function ring(stroke) { return '<span class="pt-cnrfc-local-status-ring" style="border-color:'+stroke+';"></span>'; }
  function row(sym,label,count){ return '<div class="pt-cnrfc-local-row">'+sym+'<span class="pt-cnrfc-local-label">'+esc(label)+'</span><span class="pt-cnrfc-local-count">'+fmt(count)+'</span></div>'; }
  function button(target, field, value, label) { var on = String(filters[target][field]) === String(value); return '<button type="button" class="pt-cnrfc-filter-btn'+(on?' active':'')+'" data-target="'+target+'" data-field="'+field+'" data-value="'+value+'">'+esc(label)+'</button>'; }
  function blmButton(target, mode, value, label){ var f=filters[target]; var on=(mode==='any'&&f.blmMode==='any')||(mode==='on'&&f.blmMode==='on')||(mode==='distance'&&f.blmMode==='distance'&&String(f.blmMax)===String(value)); return '<button type="button" class="pt-cnrfc-blm-btn'+(on?' active':'')+'" data-target="'+target+'" data-mode="'+mode+'" data-value="'+(value==null?'':value)+'">'+esc(label)+'</button>'; }
  function filterSummary(target){ var f=filters[target], bits=[]; if(f.kind&&f.kind!=='all')bits.push(f.kind.replace('raws_goes','raws/goes')); if(f.elevMin!=null)bits.push('elev≥'+f.elevMin); if(f.elevMax!=null)bits.push('elev≤'+f.elevMax); if(f.blmMode==='on')bits.push('on BLM'); if(f.blmMode==='distance')bits.push('≤'+f.blmMax+' mi BLM'); return bits.length?bits.join(' · '):'none'; }

  function filtersHtml(target, typeButtons){ var f=filters[target]; var html=''; html+='<div class="pt-cnrfc-filter-box">'; html+='<div class="pt-cnrfc-filter-title">Filters</div>'; html+='<div class="pt-cnrfc-filter-line">Type: '+typeButtons+'</div>'; html+='<div class="pt-cnrfc-filter-line">Elev ft: <input class="pt-cnrfc-filter-input" data-target="'+target+'" data-input="elevMin" placeholder="min" value="'+esc(f.elevMin==null?'':f.elevMin)+'"> <input class="pt-cnrfc-filter-input" data-target="'+target+'" data-input="elevMax" placeholder="max" value="'+esc(f.elevMax==null?'':f.elevMax)+'"></div>'; html+='<div class="pt-cnrfc-filter-line">BLM max mi: <input class="pt-cnrfc-filter-input pt-cnrfc-mi-input" data-target="'+target+'" data-input="blmMax" placeholder="mi" value="'+esc(f.blmMode==='distance'&&f.blmMax!=null?f.blmMax:'')+'"></div>'; html+='<div class="pt-cnrfc-filter-line">'+blmButton(target,'any',null,'any')+blmButton(target,'on',null,'on BLM')+blmButton(target,'distance',1,'≤1 mi')+blmButton(target,'distance',5,'≤5 mi')+'</div>'; html+='<div class="pt-cnrfc-filter-line"><button type="button" class="pt-cnrfc-apply-btn" data-target="'+target+'">Apply</button> <button type="button" class="pt-cnrfc-reset-btn" data-target="'+target+'">Reset</button></div>'; html+='</div>'; return html; }

  function applyToLayer(target){ var api = target==='stream' ? window.BRIM_CNRFC_STREAM_LOCAL : window.BRIM_CNRFC_WEATHER_LOCAL; if(api && typeof api.applyFilters==='function'){ var res=api.applyFilters(filters[target]); if(res&&res.filtered!=null) lastShown[target]=res.filtered; } }
  function readInputs(target, root){ var inputs=root.querySelectorAll('.pt-cnrfc-filter-input[data-target="'+target+'"]'); for(var i=0;i<inputs.length;i++){ var inp=inputs[i], key=inp.getAttribute('data-input'), val=cleanNum(inp.value); if(key==='elevMin')filters[target].elevMin=val; if(key==='elevMax')filters[target].elevMax=val; if(key==='blmMax'&&val!=null){filters[target].blmMode='distance';filters[target].blmMax=val;} } }

  function buildStreamSection(){ var html=''; html+='<div class="pt-cnrfc-local-title">CNRFC river/reservoir catalog</div>'; html+='<div class="pt-cnrfc-local-sub">Local catalog. Ops Live forecast subset highlighted.</div>'; html+=row(dot('#6BAED6','#08519C'),'River',streamCounts.river); html+=row(dot('#08306B','#041B3D'),'Reservoir',streamCounts.reservoir); if(Number(streamCounts.other||0)>0)html+=row(dot('#8C8C8C','#4D4D4D'),'Other',streamCounts.other); if(streamCounts.also_ops!=null){html+='<div class="pt-cnrfc-local-minihead">Ops Live subset</div>'; html+=row(ring('#ff00cc'),'Forecast point in Ops Live',streamCounts.also_ops);} html+='<div class="pt-cnrfc-local-showing">Showing '+fmt(lastShown.stream)+' / '+fmt(streamCounts.total)+' · '+esc(filterSummary('stream'))+'</div>'; html+=filtersHtml('stream', button('stream','kind','all','all')+button('stream','kind','river','river')+button('stream','kind','reservoir','reservoir')+button('stream','kind','other','other')); return html; }
  function buildWeatherSection(){ var html=''; html+='<div class="pt-cnrfc-local-title">CNRFC weather station catalog</div>'; html+='<div class="pt-cnrfc-local-sub">Local catalog. Ops Live NWS/WRH time-series subset highlighted.</div>'; html+=row(dot('#2b8cbe','#084081'),'ASOS',weatherCounts.asos); html+=row(dot('#e6550d','#7f2704'),'RAWS/GOES',weatherCounts.raws_goes); html+=row(dot('#756bb1','#3f007d'),'ALERT',weatherCounts.alert); if(Number(weatherCounts.other||0)>0)html+=row(dot('#bdbdbd','#4d4d4d'),'Other',weatherCounts.other); if(weatherCounts.also_ops!=null){html+='<div class="pt-cnrfc-local-minihead">Ops Live subset</div>'; html+=row(ring('#ff00cc'),'NWS/WRH time-series site',weatherCounts.also_ops);} html+='<div class="pt-cnrfc-local-showing">Showing '+fmt(lastShown.weather)+' / '+fmt(weatherCounts.total)+' · '+esc(filterSummary('weather'))+'</div>'; html+=filtersHtml('weather', button('weather','kind','all','all')+button('weather','kind','asos','ASOS')+button('weather','kind','raws_goes','RAWS/GOES')+button('weather','kind','alert','ALERT')+button('weather','kind','other','Other')); return html; }

  var container=map&&map.getContainer?map.getContainer():el; var old=(container&&container.querySelector)?container.querySelector('.pt-cnrfc-local-catalog-legend'):null; if(!old&&el&&el.querySelector)old=el.querySelector('.pt-cnrfc-local-catalog-legend'); if(old&&old.parentNode)old.parentNode.removeChild(old);
  var legend=L.control({position:'bottomleft'});
  legend.onAdd=function(map){ var div=L.DomUtil.create('div','leaflet-control pt-cnrfc-local-catalog-legend'); div.style.display='none'; div.style.background='rgba(246,239,222,0.96)'; div.style.border='1px solid rgba(112,103,83,0.55)'; div.style.borderRadius='6px'; div.style.boxShadow='0 1px 5px rgba(0,0,0,0.25)'; div.style.padding='7px 9px 8px 9px'; div.style.maxWidth='315px'; div.style.fontFamily='Arial, sans-serif'; div.style.fontSize='11.5px'; div.style.lineHeight='1.25'; div.style.color='#222'; div.style.marginBottom='74px'; div.style.position='relative'; div.innerHTML='<div class="pt-cnrfc-local-inner"></div>'; var style=document.createElement('style'); style.textContent='.pt-cnrfc-local-close{position:absolute;top:3px;right:5px;border:0;background:transparent;color:#776f61;font-weight:700;font-size:14px;line-height:1;padding:0 2px;cursor:pointer;}'+'.pt-cnrfc-local-close:hover{color:#222;background:rgba(112,103,83,0.12);border-radius:3px;}'+'.pt-cnrfc-local-catalog-legend .pt-cnrfc-local-section:first-child .pt-cnrfc-local-title{padding-right:18px;}'+'.pt-cnrfc-local-catalog-legend .pt-cnrfc-local-title{font-weight:700;font-size:12.5px;margin:0 0 2px 0;}'+'.pt-cnrfc-local-catalog-legend .pt-cnrfc-local-sub{font-size:10.5px;color:#4d4d4d;margin:0 0 5px 0;}'+'.pt-cnrfc-local-catalog-legend .pt-cnrfc-local-minihead{font-weight:700;font-size:11px;margin:5px 0 1px 0;color:#333;}'+'.pt-cnrfc-local-catalog-legend .pt-cnrfc-local-section+.pt-cnrfc-local-section{border-top:1px solid rgba(112,103,83,0.30);margin-top:7px;padding-top:7px;}'+'.pt-cnrfc-local-catalog-legend .pt-cnrfc-local-row{display:grid;grid-template-columns:13px minmax(0,1fr) auto;align-items:center;column-gap:6px;margin:2px 0;}'+'.pt-cnrfc-local-catalog-legend .pt-cnrfc-local-dot{width:10px;height:10px;border:1.4px solid #666;border-radius:50%;box-sizing:border-box;display:inline-block;}'+'.pt-cnrfc-local-catalog-legend .pt-cnrfc-local-status-ring{width:12px;height:12px;border:1.4px solid #ff00cc;border-radius:50%;box-sizing:border-box;display:inline-block;background:transparent;}'+'.pt-cnrfc-local-catalog-legend .pt-cnrfc-local-label{white-space:nowrap;overflow:hidden;text-overflow:ellipsis;}'+'.pt-cnrfc-local-catalog-legend .pt-cnrfc-local-count{font-variant-numeric:tabular-nums;color:#333;font-weight:600;}'+'.pt-cnrfc-local-catalog-legend .pt-cnrfc-local-showing{font-size:10.5px;color:#4d4d4d;margin-top:5px;}'+'.pt-cnrfc-filter-box{border-top:1px solid rgba(112,103,83,0.25);margin-top:5px;padding-top:5px;}'+'.pt-cnrfc-filter-title{font-weight:700;font-size:11px;margin-bottom:2px;}'+'.pt-cnrfc-filter-line{margin:3px 0;white-space:normal;}'+'.pt-cnrfc-filter-input{width:48px;font-size:11px;padding:1px 3px;border:1px solid rgba(112,103,83,0.65);border-radius:3px;background:rgba(255,255,255,0.92);}'+'.pt-cnrfc-mi-input{width:52px;}'+'.pt-cnrfc-filter-btn,.pt-cnrfc-blm-btn,.pt-cnrfc-apply-btn,.pt-cnrfc-reset-btn{font-size:11px;line-height:1.1;margin:1px 2px 1px 0;padding:2px 5px;border:1px solid rgba(112,103,83,0.65);border-radius:5px;background:rgba(255,255,255,0.92);cursor:pointer;}'+'.pt-cnrfc-filter-btn.active,.pt-cnrfc-blm-btn.active{background:rgba(221,211,173,0.98);font-weight:700;}'; div.appendChild(style); L.DomEvent.disableClickPropagation(div); L.DomEvent.disableScrollPropagation(div); return div; };
  legend.addTo(map);

  function attachEvents(div){
    if (window.BRIM && window.BRIM.legendCloseout) {
      window.BRIM.legendCloseout.wire(div, '.pt-cnrfc-local-close', function(){ legendUserHidden = true; });
    } else {
      var close=div.querySelector('.pt-cnrfc-local-close');
      if(close){ close.onclick=function(e){ e.preventDefault(); e.stopPropagation(); legendUserHidden=true; div.style.display='none'; }; }
    }
    var btns=div.querySelectorAll('.pt-cnrfc-filter-btn');
    for(var i=0;i<btns.length;i++){btns[i].onclick=function(e){e.preventDefault();var target=this.getAttribute('data-target'),field=this.getAttribute('data-field'),value=this.getAttribute('data-value');filters[target][field]=value;applyToLayer(target);updateLegend();};}
    var bbtns=div.querySelectorAll('.pt-cnrfc-blm-btn');
    for(var j=0;j<bbtns.length;j++){bbtns[j].onclick=function(e){e.preventDefault();var target=this.getAttribute('data-target'),mode=this.getAttribute('data-mode'),value=cleanNum(this.getAttribute('data-value'));filters[target].blmMode=mode;filters[target].blmMax=mode==='distance'?value:null;applyToLayer(target);updateLegend();};}
    var apply=div.querySelectorAll('.pt-cnrfc-apply-btn');
    for(var k=0;k<apply.length;k++){apply[k].onclick=function(e){e.preventDefault();var target=this.getAttribute('data-target');readInputs(target,div);applyToLayer(target);updateLegend();};}
    var reset=div.querySelectorAll('.pt-cnrfc-reset-btn');
    for(var r=0;r<reset.length;r++){reset[r].onclick=function(e){e.preventDefault();var target=this.getAttribute('data-target');filters[target]={kind:'all',elevMin:null,elevMax:null,blmMode:'any',blmMax:null};applyToLayer(target);updateLegend();};}
  }
  function updateLegend(evt){
    if(evt && !eventKind(evt)) return;
    var container=map&&map.getContainer?map.getContainer():el;
    var div=(container&&container.querySelector)?container.querySelector('.pt-cnrfc-local-catalog-legend'):null;
    if(!div&&el&&el.querySelector)div=el.querySelector('.pt-cnrfc-local-catalog-legend');
    if(!div)return;
    var showStream=isActive('stream'), showWeather=isActive('weather');
    if(showStream)applyToLayer('stream');
    if(showWeather)applyToLayer('weather');
    var sections=[];
    if(showStream)sections.push('<div class="pt-cnrfc-local-section">'+buildStreamSection()+'</div>');
    if(showWeather)sections.push('<div class="pt-cnrfc-local-section">'+buildWeatherSection()+'</div>');
    if(sections.length===0){
      div.style.display='none';
      legendUserHidden=false;
    } else {
      sections.unshift('<button type="button" class="pt-cnrfc-local-close" title="Hide legend">&times;</button>');
      sections.push('<div class="pt-cnrfc-local-showing">No halo = catalog-only record. Use Ops Live for current data/product links. Station ID labels (lbl) display at zoom 12+.</div>');
      div.querySelector('.pt-cnrfc-local-inner').innerHTML=sections.join('');
      div.style.display=legendUserHidden?'none':'block';
      attachEvents(div);
    }
  }
  map.on('overlayadd',function(evt){var k=markEvent(evt,true); if(k){legendUserHidden=false; setTimeout(function(){updateLegend(evt);},0);}});
  map.on('overlayremove',function(evt){var k=markEvent(evt,false); if(k){legendUserHidden=false; setTimeout(function(){updateLegend(evt);},0);}});
  map.on('zoomend',function(){updateLegend();});
  if(typeof document!=='undefined'&&document.addEventListener){
    document.addEventListener('change',function(evt){
      var t=evt&&evt.target;
      if(t&&t.matches&&t.matches('.leaflet-control-layers-overlays input[type="checkbox"]')){
        var label=t.closest?t.closest('label'):null;
        var text=label?(label.textContent||label.innerText||''):'';
        if(kindFromText(text)){legendUserHidden=false;setTimeout(updateLegend,0);setTimeout(updateLegend,100);}
      }
    },true);
  }
  setTimeout(updateLegend,0); setTimeout(updateLegend,500); setTimeout(updateLegend,1500);
}
)---"

  js <- gsub("__STREAM_GROUP__", pt_jsq(stream_group), js, fixed = TRUE)
  js <- gsub("__WEATHER_GROUP__", pt_jsq(weather_group), js, fixed = TRUE)
  js <- gsub("__STREAM_COUNTS__", pt_jsq(stream_counts), js, fixed = TRUE)
  js <- gsub("__WEATHER_COUNTS__", pt_jsq(weather_counts), js, fixed = TRUE)

  htmlwidgets::onRender(m, js)
}




