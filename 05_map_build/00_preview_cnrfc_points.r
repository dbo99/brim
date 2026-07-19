# ==== 00_preview_cnrfc_points.r =============================================
##
## PURPOSE:
##   Quick QA preview of the processed CNRFC stream-gage and precipitation-gage
##   point layers.
##
## NOTE:
##   This is not the final PortaTreasure2 map builder. It is only a lightweight
##   visual check that the first two preprocessing scripts worked correctly.
##

# ==== 1. Load configuration ==================================================

source("00_config/config_paths.r")

# ==== 2. Load packages =======================================================

suppressPackageStartupMessages({
  library(sf)
  library(leaflet)
  library(htmlwidgets)
})

# ==== 3. Read processed CNRFC layers =========================================

stream <- readRDS(file.path(
  DIR$rds,
  "CNRFC_allstreamgages_mostlyCaonly_wgs84.rds"
))

precip <- readRDS(file.path(
  DIR$rds,
  "CNRFC_allprecipstations_mostlyCaonly_wgs84.rds"
))

# ==== 4. Basic checks ========================================================

stopifnot(inherits(stream, "sf"))
stopifnot(inherits(precip, "sf"))
stopifnot(sf::st_crs(stream)$epsg == 4326)
stopifnot(sf::st_crs(precip)$epsg == 4326)

message("Stream gages: ", nrow(stream))
message("Precip gages: ", nrow(precip))

# ==== 5. Build simple preview map ===========================================

m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
  addProviderTiles(
    providers$CartoDB.Positron,
    group = "CartoDB Positron"
  ) |>
  addCircleMarkers(
    data = stream,
    group = "CNRFC Stream Gages",
    radius = 4,
    stroke = TRUE,
    weight = 1,
    fillOpacity = 0.75,
    popup = ~paste0(
      "<b>", nwsid, "</b><br/>",
      nickname, "<br/>",
      "Elev: ", elev_ft, " ft<br/>",
      "Type: ", gage_type
    ),
    clusterOptions = markerClusterOptions()
  ) |>
  addCircleMarkers(
    data = precip,
    group = "CNRFC Precip Gages",
    radius = 4,
    stroke = TRUE,
    weight = 1,
    fillOpacity = 0.75,
    popup = ~paste0(
      "<b>", nwsid, "</b><br/>",
      station, "<br/>",
      "Elev: ", elev_ft, " ft<br/>",
      "Transmission: ", datatransmission
    ),
    clusterOptions = markerClusterOptions()
  ) |>
  addLayersControl(
    baseGroups = c("CartoDB Positron"),
    overlayGroups = c("CNRFC Stream Gages", "CNRFC Precip Gages"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  setView(lng = -119.5, lat = 37.2, zoom = 6)

# ==== 6. Save preview HTML ===================================================

out_html <- file.path(DIR$html, "preview_cnrfc_points.html")

htmlwidgets::saveWidget(
  widget = m,
  file = out_html,
  selfcontained = TRUE
)

message("Saved CNRFC preview map:")
message("  ", out_html)