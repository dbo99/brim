# ==== leaflet_layer_local_polygon_helpers.r ================================================
##
## PURPOSE:
##   Core polygon/local area layers such as BLM, county/GW, and HUC layers.
##
## NOTE:
##   Extracted from leaflet_layer_helpers.r as a maintainability-only split.
##   Function names and behavior are intentionally unchanged.

# ==== 2. Core polygon layers =================================================

pt_add_blm_layers <- function(m, blm_core, blm_diffs) {
  
  m |>
    leaflet::addPolygons(
      data = blm_core,
      group = pt_layer_group_name("BLM-CA Managed (core)"),
      fillColor = "#FFFF99",
      fillOpacity = 0.45,
      color = "#D4B000",
      weight = 0.7,
      opacity = 0.9,
      popup = ~popup_html,
      options = leaflet::pathOptions(pane = "pane_blm"),
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        bringToFront = TRUE
      )
    ) |>
    leaflet::addPolygons(
      data = blm_diffs,
      group = pt_layer_group_name("BLM Held/Managed Differences"),
      fillColor = ~fill_col,
      fillOpacity = 0.65,
      color = ~stroke_col,
      weight = 1.2,
      opacity = 1,
      popup = ~popup_html,
      options = leaflet::pathOptions(pane = "pane_blm_diff"),
      highlightOptions = leaflet::highlightOptions(
        weight = 3,
        bringToFront = TRUE
      )
    )
}

pt_add_county_gw_layers <- function(m, county, gw) {
  
  m |>
    leaflet::addPolygons(
      data = county,
      group = pt_layer_group_name("Counties"),
      fill = TRUE,
      fillColor = "#FFFFFF",
      fillOpacity = 0.01,
      color = "#666666",
      weight = 1,
      opacity = 0.9,
      popup = ~popup_html,
      options = leaflet::pathOptions(
        pane = "pane_county",
        interactive = TRUE
      ),
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        bringToFront = TRUE
      )
    ) |>
    leaflet::addPolygons(
      data = gw,
      group = pt_layer_group_name("GW – Bull. 118"),
      fill = TRUE,
      fillColor = "#8B5A2B",
      fillOpacity = 0.20,
      color = "#5A381E",
      weight = 1,
      opacity = 0.9,
      popup = ~popup_html,
      options = leaflet::pathOptions(
        pane = "pane_gw",
        interactive = TRUE
      ),
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        bringToFront = TRUE
      )
    )
}

pt_add_huc_layer <- function(m, huc_all, nm) {
  
  if (!nm %in% names(huc_all)) {
    message("Skipping missing HUC layer: ", nm)
    return(m)
  }
  
  huc_sf <- huc_all[[nm]]
  code_col <- nm
  
  if (code_col %in% names(huc_sf)) {
    huc_sf$pt_huc_layer_id <- paste0(nm, "_", as.character(huc_sf[[code_col]]))
  } else {
    huc_sf$pt_huc_layer_id <- paste0(nm, "_", seq_len(nrow(huc_sf)))
  }
  
  ## HUC polygons start as boundary-only layers. The HUC fill dropdown added
  ## in 04_build_portatreasure2_core_map.r restyles these same polygons in
  ## place using precomputed fill-color fields from the cache.
  ##
  ## IMPORTANT:
  ##   layerId is intentionally set to pt_huc_layer_id so the browser-side
  ##   HUC theme control can reliably find and restyle R leaflet polygon
  ##   layers. R leaflet polygons do not reliably expose arbitrary sf columns
  ##   as layer.feature.properties in the browser.
  m |>
    leaflet::addPolygons(
      data = huc_sf,
      layerId = ~pt_huc_layer_id,
      group = pt_layer_group_name(pt_note_group_name(paste0(toupper(nm), " **"))),
      fill = TRUE,
      fillColor = "#FFFFFF",
      fillOpacity = 0,
      color = PT_HUC_COLS[[nm]],
      weight = PT_HUC_WEIGHTS[[nm]],
      opacity = 0.95,
      popup = ~popup_html,
      options = leaflet::pathOptions(
        pane = "pane_huc",
        className = "pt-huc-feature"
      ),
      highlightOptions = leaflet::highlightOptions(
        weight = PT_HUC_WEIGHTS[[nm]] + 2,
        bringToFront = TRUE
      )
    )
}

pt_add_huc_layers <- function(m, huc_all, map_display) {
  
  for (nm in c("huc2", "huc4", "huc6", "huc8")) {
    m <- pt_add_huc_layer(m, huc_all, nm)
  }
  
  if (isTRUE(map_display$add_huc10)) {
    m <- pt_add_huc_layer(m, huc_all, "huc10")
  }
  
  if (isTRUE(map_display$add_huc12)) {
    m <- pt_add_huc_layer(m, huc_all, "huc12")
  }
  
  m
}

