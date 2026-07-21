# ==== leaflet_label_helpers.r ===============================================
##
## PURPOSE:
##   Add PortaTreasure2 labels to Leaflet using a browser-friendlier approach.
##
## DESIGN:
##   The label cache is stored as sf point layers, but Leaflet label rendering is
##   faster and more stable when labels are converted to plain data frames with:
##
##     lng
##     lat
##     label_text
##
##   Labels are added as clustered label-only markers. This follows the older
##   PortaTreasure pattern more closely than the newer global-toggle approach.
##
## IMPORTANT:
##   This version intentionally adds label groups to the standard layer control.
##   That means users will see rows like:
##
##     Labels – HUC8
##     Labels – GW Basins, Bulletin 118
##     Labels – Water Districts
##
##   This is a stability-first approach. Dense labels, such as water districts,
##   use marker clustering with visually hidden cluster icons so they effectively
##   appear only after a chosen zoom threshold.
##

# ==== 1. Convert label sf to plain data frame ================================

pt_label_sf_to_df <- function(label_sf) {
  
  if (!inherits(label_sf, "sf") || nrow(label_sf) == 0) {
    return(data.frame(
      lng = numeric(0),
      lat = numeric(0),
      label_text = character(0),
      label_group = character(0),
      label_id = character(0)
    ))
  }
  
  label_wgs84 <- label_sf |>
    sf::st_transform(4326)
  
  coords <- sf::st_coordinates(label_wgs84)
  
  df <- sf::st_drop_geometry(label_wgs84)
  
  df$lng <- coords[, 1]
  df$lat <- coords[, 2]
  
  ## Keep the data-column group synchronized with the actual Leaflet group.
  ## This prevents old cached names such as "Labels: HUC8" from escaping into
  ## the final map after the category-prefix migration.
  if ("label_group" %in% names(df)) {
    df$label_group <- pt_layer_group_name(df$label_group)
  }
  
  df |>
    dplyr::filter(
      !is.na(.data$lng),
      !is.na(.data$lat),
      !is.na(.data$label_text),
      .data$label_text != ""
    )
}

# ==== 2. Cluster option for label-only markers ===============================
##
## Labels are clustered mostly for performance. The cluster icons are made
## visually unobtrusive because we do not want big marker-count bubbles for
## labels.
##
## IMPORTANT FOR DENSE LABELS:
##   Because cluster icons are visually hidden, a dense label layer is effectively
##   invisible until disableClusteringAtZoom is reached. This is a useful way to
##   make layers such as Water District labels toggleable but high-zoom only.

pt_label_cluster_options <- function(disable_at_zoom = 11) {
  
  leaflet::markerClusterOptions(
    chunkedLoading = TRUE,
    spiderfyOnMaxZoom = FALSE,
    showCoverageOnHover = FALSE,
    zoomToBoundsOnClick = FALSE,
    disableClusteringAtZoom = disable_at_zoom,
    iconCreateFunction = htmlwidgets::JS(
      "function(cluster) {
         return L.divIcon({
           html: '',
           className: 'pt-label-cluster',
           iconSize: [1, 1]
         });
       }"
    )
  )
}

# ==== 2A. Choose decluster / visibility zoom =================================
##
## PURPOSE:
##   Centralize label declustering thresholds.
##
## DESIGN:
##   Most existing labels keep the previous hard-coded thresholds to avoid
##   changing unrelated map behavior. Water districts are the special case added
##   here: their threshold is read from the cached label layer's min_zoom field,
##   which is configured in 00_config/config_labels.r.

pt_label_disable_zoom <- function(label_id, label_sf) {
  
  label_id <- as.character(label_id)
  
  ## Water districts are numerous and should not show at statewide/regional
  ## zooms. Use the configured min_zoom when present, with 12 as a safe default.
  if (identical(label_id, "water_districts")) {
    if ("min_zoom" %in% names(label_sf)) {
      z <- suppressWarnings(as.numeric(label_sf$min_zoom[1]))
      if (is.finite(z)) {
        return(z)
      }
    }
    return(12)
  }
  
  ## Preserve existing behavior for current label layers.
  dplyr::case_when(
    label_id %in% c("cnrfc_stream", "cnrfc_precip", "usgs_streamgages") ~ 12,
    label_id %in% c("huc12") ~ 11,
    label_id %in% c("huc10") ~ 10,
    TRUE ~ 9
  )
}

# ==== 3. Add one label layer =================================================

pt_add_single_label_layer <- function(m, label_sf) {
  
  if (!inherits(label_sf, "sf") || nrow(label_sf) == 0) {
    return(m)
  }
  
  label_id <- as.character(label_sf$label_id[1])
  label_group <- pt_layer_group_name(as.character(label_sf$label_group[1]))
  
  label_df <- pt_label_sf_to_df(label_sf)
  
  if (nrow(label_df) == 0) {
    return(m)
  }
  
  ## Dense labels stay inside visually hidden clusters until the configured
  ## decluster zoom. For Water Districts, this prevents the layer from cluttering
  ## the map at broad scales while still making it useful at local scales.
  disable_zoom <- pt_label_disable_zoom(
    label_id = label_id,
    label_sf = label_sf
  )
  
  css_class <- paste0("pt-label pt-label-", label_id)
  
  m |>
    leaflet::addLabelOnlyMarkers(
      data = label_df,
      lng = ~lng,
      lat = ~lat,
      group = label_group,
      label = ~label_text,
      labelOptions = leaflet::labelOptions(
        noHide = TRUE,
        direction = "center",
        textOnly = TRUE,
        opacity = 1,
        className = css_class
      ),
      options = leaflet::markerOptions(
        pane = "pane_labels_poly",
        interactive = FALSE
      ),
      clusterOptions = pt_label_cluster_options(
        disable_at_zoom = disable_zoom
      )
    )
}

# ==== 4. Add all label layers ================================================

pt_is_retired_label_layer <- function(label_sf, cache_name = "") {
  label_id <- if (inherits(label_sf, "sf") && "label_id" %in% names(label_sf) && nrow(label_sf) > 0) {
    as.character(label_sf$label_id[1])
  } else {
    ""
  }

  identical(as.character(cache_name), "major_conveyance") ||
    identical(label_id, "major_conveyance")
}

pt_add_label_layers <- function(m, labels_all) {
  
  if (!is.list(labels_all) || length(labels_all) == 0) {
    message("No label cache list found; skipping label layers.")
    return(m)
  }
  
  for (nm in names(labels_all)) {
    if (pt_is_retired_label_layer(labels_all[[nm]], nm)) next
    m <- pt_add_single_label_layer(m, labels_all[[nm]])
  }
  
  m
}

# ==== 5. Return label groups for layer control ===============================
##
## These groups are added to the standard Leaflet layer control for now.
## Empty label layers are skipped.

pt_label_overlay_groups <- function(labels_all) {
  
  if (!is.list(labels_all) || length(labels_all) == 0) {
    return(character(0))
  }
  
  groups <- purrr::imap_chr(labels_all, function(x, nm) {
    if (pt_is_retired_label_layer(x, nm)) {
      return(NA_character_)
    }
    
    if (!inherits(x, "sf") || nrow(x) == 0) {
      return(NA_character_)
    }
    
    as.character(x$label_group[1])
  })
  
  groups <- groups[!is.na(groups) & groups != ""]
  groups <- pt_layer_group_name(groups)
  unique(groups)
}

# ==== 6. Add simple label CSS ================================================
##
## This only styles labels. It does not control visibility by zoom or parent
## layer. For dense label layers such as Water Districts, broad-scale visibility
## is controlled by marker clustering / declustering.

pt_add_label_css <- function(m, labels_config) {
  
  js <- sprintf(
    "
function(el, x) {

  if (document.getElementById('pt-label-style')) {
    return;
  }

  var style = document.createElement('style');
  style.id = 'pt-label-style';

  style.innerHTML = `
    .pt-label {
      font-size: %spx;
      color: %s;
      font-weight: 600;
      text-shadow:
        -1px -1px 0 %s,
         1px -1px 0 %s,
        -1px  1px 0 %s,
         1px  1px 0 %s,
         0px  0px 3px %s;
      background: transparent;
      border: none;
      box-shadow: none;
      pointer-events: none;
      white-space: nowrap;
    }

    /* Water district names can be long and numerous. Keep them readable but a
       little smaller than core hydrology/basin labels. */
    .pt-label-water_districts {
      font-size: 10px;
      font-weight: 600;
    }

    .pt-label-cluster {
      background: transparent;
      border: none;
      box-shadow: none;
    }
  `;

  document.head.appendChild(style);
}
    ",
labels_config$default_text_size_px,
labels_config$default_text_color,
labels_config$default_halo_color,
labels_config$default_halo_color,
labels_config$default_halo_color,
labels_config$default_halo_color,
labels_config$default_halo_color
  )
  
  htmlwidgets::onRender(m, js)
}
