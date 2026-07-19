# ==== 01_preview_blm_huc_gw_county.r ========================================
##
## PURPOSE:
##   Quick QA preview of the processed BLM, HUC, groundwater basin, and county
##   layers before building the full PortaTreasure2 map.
##
## INPUTS:
##   04_processed_data/rds/blm_managed_core_3310.rds
##   04_processed_data/rds/blm_held_vs_managed_diffs_3310.rds
##   04_processed_data/rds/huc_all_full.rds
##   04_processed_data/rds/bull118gw_full.rds
##   04_processed_data/rds/county_full.rds
##
## OUTPUT:
##   06_output/html/preview_blm_huc_gw_county.html
##
## NOTE:
##   This is not the final map builder. It is only a visual QA map.
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
  library(htmltools)
})

# ==== 3. User switches =======================================================

## HUC12 can make the preview map large. Keep FALSE unless specifically testing it.
ADD_HUC10 <- TRUE
ADD_HUC12 <- FALSE

SELF_CONTAINED <- TRUE

# ==== 4. Read processed layers ===============================================

blm_core <- readRDS(file.path(DIR$rds, "blm_managed_core_3310.rds")) |>
  sf::st_transform(4326)

blm_diffs <- readRDS(file.path(DIR$rds, "blm_held_vs_managed_diffs_3310.rds")) |>
  sf::st_transform(4326)

huc_all <- readRDS(file.path(DIR$rds, "huc_all_full.rds"))
gw      <- readRDS(file.path(DIR$rds, "bull118gw_full.rds"))
county  <- readRDS(file.path(DIR$rds, "county_full.rds"))

# ==== 5. Basic QA checks =====================================================

stopifnot(inherits(blm_core, "sf"))
stopifnot(inherits(blm_diffs, "sf"))
stopifnot(is.list(huc_all))
stopifnot(inherits(gw, "sf"))
stopifnot(inherits(county, "sf"))

message("BLM core rows: ", nrow(blm_core))
message("BLM diff rows: ", nrow(blm_diffs))
message("HUC layers: ", paste(names(huc_all), collapse = ", "))
message("GW rows: ", nrow(gw))
message("County rows: ", nrow(county))

# ==== 6. Formatting helpers ==================================================

fmt_area <- function(x) {
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

fmt_pct <- function(x) {
  v <- as.numeric(x)
  ifelse(is.na(v), "NA", sprintf("%.2f%%", v))
}

esc <- htmltools::htmlEscape

# ==== 7. Popup helpers =======================================================

build_huc_popup <- function(sfobj, lvl) {
  
  df <- sf::st_drop_geometry(sfobj)
  
  code_col <- paste0("huc", lvl)
  name_col <- paste0("huc", lvl, "_name")
  
  vapply(seq_len(nrow(df)), function(i) {
    
    parts <- c(
      sprintf("<b>HUC%d – %s</b>", lvl, esc(df[[code_col]][i])),
      sprintf("<b>Name:</b> %s", esc(df[[name_col]][i])),
      sprintf("<b>%%BLM-CA:</b> %s", fmt_pct(df$percentBLMland[i])),
      sprintf("<b>Total area:</b> %s", fmt_area(df$total_area_sqmi[i])),
      sprintf("<b>BLM-CA area:</b> %s", fmt_area(df$blm_area_sqmi[i]))
    )
    
    ## Add parent HUC information when present.
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
          esc(df[[nm_col]][i]),
          esc(parent_id),
          fmt_pct(df[[pct_col]][i])
        ),
        sprintf(
          "(%s BLM / %s total)",
          fmt_area(df[[blm_col]][i]),
          fmt_area(df[[tot_col]][i])
        )
      )
    }
    
    parts <- c(
      parts,
      sprintf(
        "<a href='https://www.google.com/search?q=USGS+HUC+%s' target='_blank'>Google HUC search</a>",
        esc(df[[code_col]][i])
      )
    )
    
    paste(parts, collapse = "<br/>")
    
  }, character(1))
}

build_gw_popup <- function(x) {
  sprintf(
    "<b>%s</b><br/>
     <b>Basin:</b> %s<br/>
     <b>%%BLM-CA:</b> %s<br/>
     <b>BLM:</b> %s | <b>Total:</b> %s<br/>
     <a href='https://www.google.com/search?q=California+Bulletin+118+%s' target='_blank'>Google Search</a>",
    esc(x$label),
    esc(x$basin_name),
    fmt_pct(x$percentBLMland),
    fmt_area(x$blm_area_sqmi),
    fmt_area(x$total_area_sqmi),
    esc(x$subbasin_num)
  )
}

build_county_popup <- function(x) {
  sprintf(
    "<b>%s County</b><br/>
     <b>%%BLM-CA:</b> %s<br/>
     <b>BLM:</b> %s | <b>Total:</b> %s",
    esc(x$county_name),
    fmt_pct(x$percentBLMland),
    fmt_area(x$blm_area_sqmi),
    fmt_area(x$total_area_sqmi)
  )
}

# ==== 8. Add popup fields ====================================================

huc_all <- purrr::imap(huc_all, function(x, nm) {
  lvl <- as.integer(gsub("^huc", "", nm))
  x$popup_html <- build_huc_popup(x, lvl)
  x
})

gw$popup_html <- build_gw_popup(gw)
county$popup_html <- build_county_popup(county)

blm_core$popup_html <- paste0("<b>", esc(blm_core$category), "</b>")

blm_diffs$category_base <- dplyr::case_when(
  grepl("un.?managed.*held", blm_diffs$category, ignore.case = TRUE) ~ "Unmanaged but held",
  grepl("managed.*not.*held", blm_diffs$category, ignore.case = TRUE) ~ "Managed but not held",
  TRUE ~ blm_diffs$category
)

blm_diffs$fill_col <- dplyr::case_when(
  blm_diffs$category_base == "Unmanaged but held" ~ "#FF5EBE",
  blm_diffs$category_base == "Managed but not held" ~ "#9CF69C",
  TRUE ~ "#F0F0F0"
)

blm_diffs$stroke_col <- dplyr::case_when(
  blm_diffs$category_base == "Unmanaged but held" ~ "#9B1B6A",
  blm_diffs$category_base == "Managed but not held" ~ "#2E8B57",
  TRUE ~ "#333333"
)

blm_diffs$popup_html <- paste0("<b>", esc(blm_diffs$category), "</b>")

# ==== 9. Style constants =====================================================

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

# ==== 10. Initialize map =====================================================

m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
  setView(lng = -119.77, lat = 36.74, zoom = 6) |>
  addProviderTiles(providers$CartoDB.Positron, group = "CartoDB Positron") |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") |>
  addMapPane("pane_blm", 300) |>
  addMapPane("pane_blm_diff", 320) |>
  addMapPane("pane_county", 390) |>
  addMapPane("pane_gw", 400) |>
  addMapPane("pane_huc", 430)

# ==== 11. Add BLM layers =====================================================

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
    highlightOptions = highlightOptions(weight = 2, bringToFront = TRUE)
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
    highlightOptions = highlightOptions(weight = 3, bringToFront = TRUE)
  )

# ==== 12. Add county and groundwater layers ==================================

m <- m |>
  addPolygons(
    data = county,
    group = "Counties",
    fill = FALSE,
    color = "#666666",
    weight = 1,
    opacity = 0.9,
    popup = ~popup_html,
    options = pathOptions(pane = "pane_county"),
    highlightOptions = highlightOptions(weight = 2, bringToFront = TRUE)
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
    highlightOptions = highlightOptions(weight = 2, bringToFront = TRUE)
  )

# ==== 13. Add HUC layers =====================================================

add_huc_layer <- function(map, huc_list, nm) {
  
  if (!nm %in% names(huc_list)) return(map)
  
  map |>
    addPolygons(
      data = huc_list[[nm]],
      group = toupper(nm),
      fill = FALSE,
      color = HUC_COLS[[nm]],
      weight = HUC_WEIGHTS[[nm]],
      opacity = 0.95,
      popup = ~popup_html,
      options = pathOptions(pane = "pane_huc"),
      highlightOptions = highlightOptions(weight = HUC_WEIGHTS[[nm]] + 2, bringToFront = TRUE)
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

# ==== 14. Layer control and default visibility ===============================

overlay_groups <- c(
  "BLM-CA Managed (core)",
  "BLM Held/Managed Differences",
  "Counties",
  "GW – Bull. 118",
  "HUC2",
  "HUC4",
  "HUC6",
  "HUC8",
  if (ADD_HUC10) "HUC10",
  if (ADD_HUC12) "HUC12"
)

m <- m |>
  addLayersControl(
    baseGroups = c("CartoDB Positron", "Esri World Imagery"),
    overlayGroups = overlay_groups,
    options = layersControlOptions(collapsed = FALSE)
  )

## Hide noisier layers by default. Keep BLM core and HUC8 visible.
hide_by_default <- setdiff(
  overlay_groups,
  c("BLM-CA Managed (core)", "HUC8")
)

for (grp in hide_by_default) {
  m <- hideGroup(m, grp)
}

# ==== 15. Save preview HTML ==================================================

out_html <- file.path(DIR$html, "preview_blm_huc_gw_county.html")

htmlwidgets::saveWidget(
  widget = m,
  file = out_html,
  selfcontained = SELF_CONTAINED
)

message("\nSaved BLM/HUC/GW/county preview map:")
message("  ", out_html)