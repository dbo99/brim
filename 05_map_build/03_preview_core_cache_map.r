# ==== 03_preview_core_cache_map.r ===========================================
##
## PURPOSE:
##   Build a QA preview map from the *map-ready cache* layers.
##
## WHY THIS SCRIPT EXISTS:
##   The earlier polygon preview used full-resolution analytical layers and
##   created a very large HTML file. This script instead uses the simplified,
##   popup-enriched cache created by:
##
##     05_map_build/02_build_core_map_cache.r
##
##   This lets us test whether the cache-stage simplification and popup
##   enrichment worked before building the final PortaTreasure2 map.
##
## INPUTS:
##   04_processed_data/cache/latest/blm_core_map.rds
##   04_processed_data/cache/latest/blm_diffs_map.rds
##   04_processed_data/cache/latest/huc_all_map.rds
##   04_processed_data/cache/latest/gw_bull118_map.rds
##   04_processed_data/cache/latest/county_map.rds
##   04_processed_data/cache/latest/cnrfc_stream_map.rds
##   04_processed_data/cache/latest/cnrfc_precip_map.rds
##
## OUTPUT:
##   06_output/html/preview_core_cache_map.html
##
## NOTE:
##   This is still a QA preview, not the final production map.
##

# ==== 1. Load configuration ==================================================

source("00_config/config_paths.r")

# ==== 2. Load packages =======================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(purrr)
  library(leaflet)
  library(htmlwidgets)
})

# ==== 3. Preview switches ====================================================
##
## These switches control what gets added to the preview map.
## HUC12 is available from the cache, but it can still make the preview heavy.

ADD_HUC10 <- TRUE
ADD_HUC12 <- TRUE

ADD_CNRFC_STREAM <- TRUE
ADD_CNRFC_PRECIP <- TRUE

ADD_USGS_STREAMGAGES <- TRUE
ADD_USGS_WELLS       <- TRUE

## For a preview, FALSE is usually better because it avoids creating a huge
## single-file HTML. The final map can switch this back to TRUE when needed.
SELF_CONTAINED <- FALSE

## Output filename for this preview.
OUT_HTML <- file.path(DIR$html, "preview_core_cache_map.html")

# ==== 4. Define cache paths ==================================================

CACHE <- list(
  blm_core     = file.path(DIR$cache_last, "blm_core_map.rds"),
  blm_diffs    = file.path(DIR$cache_last, "blm_diffs_map.rds"),
  huc_all      = file.path(DIR$cache_last, "huc_all_map.rds"),
  gw           = file.path(DIR$cache_last, "gw_bull118_map.rds"),
  county       = file.path(DIR$cache_last, "county_map.rds"),
  cnrfc_stream = file.path(DIR$cache_last, "cnrfc_stream_map.rds"),
  cnrfc_precip = file.path(DIR$cache_last, "cnrfc_precip_map.rds"),
  usgs_sw = file.path(DIR$cache_last, "usgs_streamgages_map.rds"),
  usgs_gw = file.path(DIR$cache_last, "usgs_wells_map.rds")
)

# ==== 5. Check required cache files ==========================================

missing_cache <- unlist(CACHE)[!file.exists(unlist(CACHE))]

if (length(missing_cache) > 0) {
  stop(
    "Missing required cache file(s). Run 05_map_build/02_build_core_map_cache.r first:\n",
    paste(missing_cache, collapse = "\n")
  )
}

message("All required cache files found.")

# ==== 6. Read cached map-ready layers ========================================
##
## These layers should already be:
##   - simplified for web display
##   - transformed to EPSG:4326
##   - enriched with popup_html fields

blm_core     <- readRDS(CACHE$blm_core)
blm_diffs    <- readRDS(CACHE$blm_diffs)
huc_all      <- readRDS(CACHE$huc_all)
gw           <- readRDS(CACHE$gw)
county       <- readRDS(CACHE$county)
cnrfc_stream <- readRDS(CACHE$cnrfc_stream)
cnrfc_precip <- readRDS(CACHE$cnrfc_precip)
usgs_sw <- readRDS(CACHE$usgs_sw)
usgs_gw <- readRDS(CACHE$usgs_gw)

# ==== 7. Basic QA checks =====================================================

## Confirm object classes.
stopifnot(inherits(blm_core, "sf"))
stopifnot(inherits(blm_diffs, "sf"))
stopifnot(is.list(huc_all))
stopifnot(inherits(gw, "sf"))
stopifnot(inherits(county, "sf"))
stopifnot(inherits(cnrfc_stream, "sf"))
stopifnot(inherits(cnrfc_precip, "sf"))
stopifnot(inherits(usgs_sw, "sf"))
stopifnot(inherits(usgs_gw, "sf"))

## Confirm WGS84 CRS for Leaflet.
stopifnot(sf::st_crs(blm_core)$epsg == 4326)
stopifnot(sf::st_crs(blm_diffs)$epsg == 4326)
stopifnot(sf::st_crs(gw)$epsg == 4326)
stopifnot(sf::st_crs(county)$epsg == 4326)
stopifnot(sf::st_crs(cnrfc_stream)$epsg == 4326)
stopifnot(sf::st_crs(cnrfc_precip)$epsg == 4326)
stopifnot(sf::st_crs(usgs_sw)$epsg == 4326)
stopifnot(sf::st_crs(usgs_gw)$epsg == 4326)

purrr::walk(huc_all, function(x) {
  stopifnot(inherits(x, "sf"))
  stopifnot(sf::st_crs(x)$epsg == 4326)
})

## Confirm popup_html exists where expected.
required_popup_layers <- list(
  blm_core = blm_core,
  blm_diffs = blm_diffs,
  gw = gw,
  county = county,
  cnrfc_stream = cnrfc_stream,
  cnrfc_precip = cnrfc_precip,
  usgs_sw = usgs_sw,
  usgs_gw = usgs_gw
)

purrr::iwalk(required_popup_layers, function(x, nm) {
  if (!"popup_html" %in% names(x)) {
    stop("Missing popup_html field in layer: ", nm)
  }
})

purrr::iwalk(huc_all, function(x, nm) {
  if (!"popup_html" %in% names(x)) {
    stop("Missing popup_html field in HUC layer: ", nm)
  }
})

message("Cache QA passed.")
message("BLM core rows:     ", nrow(blm_core))
message("BLM diff rows:     ", nrow(blm_diffs))
message("GW rows:           ", nrow(gw))
message("County rows:       ", nrow(county))
message("CNRFC stream rows: ", nrow(cnrfc_stream))
message("CNRFC precip rows: ", nrow(cnrfc_precip))
message("HUC rows:")
message("USGS stream rows:  ", nrow(usgs_sw))
message("USGS well rows:    ", nrow(usgs_gw))
print(purrr::map_int(huc_all, nrow))

# ==== 8. Style constants =====================================================
##
## These are preview styles only. Final styles will eventually live in a
## dedicated config/style file.

HUC_COLS <- c(
  huc2  = "#d73027",
  huc4  = "#fc8d59",
  huc6  = "#0000CD",
  huc8  = "#555555",
  huc10 = "#006400",
  huc12 = "#8B4513"
)

HUC_WEIGHTS <- c(
  huc2  = 10,
  huc4  = 8,
  huc6  = 6,
  huc8  = 3,
  huc10 = 1.5,
  huc12 = 0.7
)

## Point colors are intentionally simple here.
## More careful gage-class/link logic will be added later in popup helpers.
stream_color <- function(gage_class1) {
  dplyr::case_when(
    gage_class1 %in% c("river", "stream")  ~ "#3182bd",
    gage_class1 %in% c("reservoir", "dam") ~ "#153750",
    TRUE                                   ~ "#8B4500"
  )
}

precip_color <- function(datatransmission) {
  dplyr::case_when(
    datatransmission == "G" ~ "#1f78b4",
    datatransmission == "R" ~ "#33a02c",
    datatransmission == "Z" ~ "#ff7f00",
    datatransmission == "P" ~ "#6a3d9a",
    datatransmission == "M" ~ "#e31a1c",
    datatransmission == "W" ~ "#444444",
    TRUE                    ~ "#000000"
  )
}

cnrfc_stream$preview_color <- stream_color(cnrfc_stream$gage_class1)
cnrfc_precip$preview_color <- precip_color(cnrfc_precip$datatransmission)

usgs_site_status_color <- function(status) {
  dplyr::case_when(
    tolower(as.character(status)) == "active"   ~ "#2ca02c",
    tolower(as.character(status)) == "inactive" ~ "#bdbdbd",
    TRUE                                        ~ "#ff7f00"
  )
}

usgs_sw$preview_color <- usgs_site_status_color(usgs_sw$status)
usgs_gw$preview_color <- usgs_site_status_color(usgs_gw$status)

# ==== 9. Initialize Leaflet map =============================================

m <- leaflet(
  options = leafletOptions(
    preferCanvas = TRUE
  )
) |>
  setView(lng = -119.77, lat = 36.74, zoom = 6)

# ==== 10. Add map panes ======================================================
##
## Panes control drawing order. Lower zIndex values are drawn underneath higher
## zIndex values. This makes it easier to keep BLM fill below HUC outlines and
## point layers above polygons.

m <- m |>
  addMapPane("pane_blm",        zIndex = 300) |>
  addMapPane("pane_blm_diff",   zIndex = 320) |>
  addMapPane("pane_county",     zIndex = 380) |>
  addMapPane("pane_gw",         zIndex = 390) |>
  addMapPane("pane_huc",        zIndex = 430) |>
  addMapPane("pane_points",     zIndex = 520)

# ==== 11. Add basemaps =======================================================
##
## This is a small preview basemap set. The final map can include the larger
## basemap stack from the legacy PortaTreasure map.

m <- m |>
  addProviderTiles(
    provider = providers$CartoDB.Positron,
    group = "CartoDB Positron"
  ) |>
  addProviderTiles(
    provider = providers$Esri.WorldImagery,
    group = "Esri World Imagery"
  ) |>
  addProviderTiles(
    provider = providers$OpenStreetMap,
    group = "OpenStreetMap"
  )

BASE_GROUPS <- c(
  "CartoDB Positron",
  "Esri World Imagery",
  "OpenStreetMap"
)

# ==== 12. Add BLM layers =====================================================

m <- m |>
  addPolygons(
    data = blm_core,
    group = "BLM-CA Managed (core)",
    fillColor = "#FFFF99",
    fillOpacity = 0.45,
    color = "#D4B000",
    weight = 0.7,
    opacity = 0.9,
    popup = ~popup_html,
    options = pathOptions(pane = "pane_blm"),
    highlightOptions = highlightOptions(
      weight = 2,
      bringToFront = TRUE
    )
  ) |>
  addPolygons(
    data = blm_diffs,
    group = "BLM Held/Managed Differences",
    fillColor = ~fill_col,
    fillOpacity = 0.65,
    color = ~stroke_col,
    weight = 1.2,
    opacity = 1,
    popup = ~popup_html,
    options = pathOptions(pane = "pane_blm_diff"),
    highlightOptions = highlightOptions(
      weight = 3,
      bringToFront = TRUE
    )
  )

# ==== 13. Add county and groundwater basin layers ============================

m <- m |>
  addPolygons(
    data = county,
    group = "Counties",
    fill = TRUE,
    fillColor = "#FFFFFF",
    fillOpacity = 0.01,  # nearly invisible, but makes interiors clickable
    color = "#666666",
    weight = 1,
    opacity = 0.9,
    popup = ~popup_html,
    options = pathOptions(pane = "pane_county"),
    highlightOptions = highlightOptions(
      weight = 2,
      bringToFront = TRUE
    )
  ) |>
  addPolygons(
    data = gw,
    group = "GW – Bull. 118",
    fillColor = "#8B5A2B",
    fillOpacity = 0.20,
    color = "#5A381E",
    weight = 1,
    opacity = 0.9,
    popup = ~popup_html,
    options = pathOptions(pane = "pane_gw"),
    highlightOptions = highlightOptions(
      weight = 2,
      bringToFront = TRUE
    )
  )

# ==== 14. Add HUC layers =====================================================
##
## HUC polygons use near-transparent fill so the whole polygon interior is
## clickable. Without fill, users often have to click exactly on the boundary
## to trigger the popup.

add_huc_layer <- function(map, huc_list, nm) {
  
  if (!nm %in% names(huc_list)) {
    message("Skipping missing HUC layer: ", nm)
    return(map)
  }
  
  map |>
    addPolygons(
      data = huc_list[[nm]],
      group = toupper(nm),
      fill = TRUE,
      fillColor = "#FFFFFF",
      fillOpacity = 0.01,
      color = HUC_COLS[[nm]],
      weight = HUC_WEIGHTS[[nm]],
      opacity = 0.95,
      popup = ~popup_html,
      options = pathOptions(pane = "pane_huc"),
      highlightOptions = highlightOptions(
        weight = HUC_WEIGHTS[[nm]] + 2,
        bringToFront = TRUE
      )
    )
}

m <- add_huc_layer(m, huc_all, "huc2")
m <- add_huc_layer(m, huc_all, "huc4")
m <- add_huc_layer(m, huc_all, "huc6")
m <- add_huc_layer(m, huc_all, "huc8")

if (ADD_HUC10) {
  m <- add_huc_layer(m, huc_all, "huc10")
}

if (ADD_HUC12) {
  m <- add_huc_layer(m, huc_all, "huc12")
}

# ==== 15. Add CNRFC point layers ============================================
##
## These are clustered to keep the preview usable at statewide scale.

if (ADD_CNRFC_STREAM) {
  
  m <- m |>
    addCircleMarkers(
      data = cnrfc_stream,
      group = "CNRFC Stream Gages",
      radius = 4,
      stroke = TRUE,
      color = "#222222",
      weight = 0.7,
      fillColor = ~preview_color,
      fillOpacity = 0.75,
      popup = ~popup_html,
      options = pathOptions(pane = "pane_points"),
      clusterOptions = markerClusterOptions()
    )
}

if (ADD_CNRFC_PRECIP) {
  
  m <- m |>
    addCircleMarkers(
      data = cnrfc_precip,
      group = "CNRFC Precip Gages",
      radius = 4,
      stroke = TRUE,
      color = "#222222",
      weight = 0.7,
      fillColor = ~preview_color,
      fillOpacity = 0.75,
      popup = ~popup_html,
      options = pathOptions(pane = "pane_points"),
      clusterOptions = markerClusterOptions()
    )
}

# ==== 15b. Add USGS point layers ============================================
##
## USGS wells are numerous, so clustering is essential. Both USGS layers are
## hidden by default later in the script.

if (ADD_USGS_STREAMGAGES) {
  
  m <- m |>
    addCircleMarkers(
      data = usgs_sw,
      group = "USGS Streamgages",
      radius = 4,
      stroke = TRUE,
      color = "#222222",
      weight = 0.7,
      fillColor = ~preview_color,
      fillOpacity = 0.75,
      popup = ~popup_html,
      options = pathOptions(pane = "pane_points"),
      clusterOptions = markerClusterOptions()
    )
}

if (ADD_USGS_WELLS) {
  
  m <- m |>
    addCircleMarkers(
      data = usgs_gw,
      group = "USGS Wells",
      radius = 3,
      stroke = TRUE,
      color = "#222222",
      weight = 0.5,
      fillColor = ~preview_color,
      fillOpacity = 0.65,
      popup = ~popup_html,
      options = pathOptions(pane = "pane_points"),
      clusterOptions = markerClusterOptions(
        chunkedLoading = TRUE
      )
    )
}

# ==== 16. Build overlay group list ==========================================
##
## This list controls the order of layers in the layer-control box.

OVERLAY_GROUPS <- c(
  "BLM-CA Managed (core)",
  "BLM Held/Managed Differences",
  "Counties",
  "GW – Bull. 118",
  "HUC2",
  "HUC4",
  "HUC6",
  "HUC8",
  if (ADD_HUC10) "HUC10",
  if (ADD_HUC12) "HUC12",
  if (ADD_CNRFC_STREAM) "CNRFC Stream Gages",
  if (ADD_CNRFC_PRECIP) "CNRFC Precip Gages",
  if (ADD_USGS_STREAMGAGES) "USGS Streamgages",
  if (ADD_USGS_WELLS) "USGS Wells"
)

# ==== 17. Add controls and default visibility ================================

m <- m |>
  addLayersControl(
    baseGroups = BASE_GROUPS,
    overlayGroups = OVERLAY_GROUPS,
    options = layersControlOptions(
      collapsed = FALSE
    )
  )

## Default view:
##   - BLM core on
##   - HUC8 on
##   - everything else off
##
## HUC12 remains available in the control if ADD_HUC12 = TRUE, but it is hidden
## by default because it can be visually busy and heavier to render.

hide_by_default <- setdiff(
  OVERLAY_GROUPS,
  c("BLM-CA Managed (core)", "HUC8")
)

for (grp in hide_by_default) {
  m <- hideGroup(m, grp)
}

# ==== 18. Add scale bar ======================================================

m <- m |>
  addScaleBar(
    position = "bottomleft",
    options = scaleBarOptions(
      imperial = TRUE,
      metric = TRUE
    )
  )

# ==== 19. Save preview HTML ==================================================
##
## If SELF_CONTAINED = FALSE, htmlwidgets will save a small HTML file plus a
## dependency folder. That is often better for testing. For final sharing,
## we can switch back to a standalone self-contained HTML.

htmlwidgets::saveWidget(
  widget = m,
  file = OUT_HTML,
  selfcontained = SELF_CONTAINED
)

message("\nSaved cache-based core preview map:")
message("  ", OUT_HTML)

message("\nOutput file size:")
print(file.info(OUT_HTML)$size)

message("\nDone: cache-based core preview map complete.")