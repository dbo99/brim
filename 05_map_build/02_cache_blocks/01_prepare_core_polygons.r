# ==== 01_prepare_core_polygons.r =============================================
##
## PURPOSE:
##   Prepare/simplify BLM, HUC, groundwater basin, and county polygons.
##
## NOTE:
##   This file is sourced by 05_map_build/02_build_core_map_cache.r.
##   It expects objects created earlier in that script and creates map-ready
##   cache objects in the calling environment. Do not source this file alone
##   unless you have already created the required input objects.
## ============================================================================

# ==== 7. Transform and simplify polygon layers ===============================

message("Preparing BLM layers...")

blm_core_map <- blm_core |>
  sf::st_transform(4326) |>
  simplify_sf_for_web(
    keep = keep_for("blm_core"),
    layer_label = "BLM managed core"
  )

blm_diffs_map <- blm_diffs |>
  sf::st_transform(4326) |>
  simplify_sf_for_web(
    keep = keep_for("blm_diffs"),
    layer_label = "BLM held/managed differences"
  )

# ---- 7A. Add BLM popup and style fields -------------------------------------
##
## REQUIRED:
##   The final Leaflet drawing helper expects:
##
##     blm_core_map$popup_html
##     blm_diffs_map$popup_html
##     blm_diffs_map$fill_col
##     blm_diffs_map$stroke_col
##
## If any of these are missing, the final HTML build can fail with:
##   object 'popup_html' not found
##   object 'stroke_col' not found

blm_core_map$popup_html <- pt_make_blm_core_popups(blm_core_map)

blm_diffs_map <- pt_enrich_blm_diffs(blm_diffs_map)

## Safety fallbacks in case pt_enrich_blm_diffs() did not create all fields.
if (!"popup_html" %in% names(blm_diffs_map)) {
  blm_diffs_map$popup_html <- paste0(
    "<b>",
    htmltools::htmlEscape(as.character(blm_diffs_map$category)),
    "</b>"
  )
}

if (!"fill_col" %in% names(blm_diffs_map)) {
  blm_diffs_map$fill_col <- "#F0F0F0"
}

if (!"stroke_col" %in% names(blm_diffs_map)) {
  blm_diffs_map$stroke_col <- "#333333"
}

message("Preparing HUC layers...")

huc_keep_names <- c("huc2", "huc4", "huc6", "huc8")

if (BUILD_HUC10 && "huc10" %in% names(huc_all)) {
  huc_keep_names <- c(huc_keep_names, "huc10")
}

if (BUILD_HUC12 && "huc12" %in% names(huc_all)) {
  huc_keep_names <- c(huc_keep_names, "huc12")
}

huc_map <- huc_all[huc_keep_names]

huc_map <- purrr::imap(huc_map, function(x, nm) {
  
  simplify_sf_for_web(
    x = x,
    keep = keep_for(nm),
    layer_label = toupper(nm)
  )
})

message("Preparing GW and county layers...")

gw_map <- gw |>
  simplify_sf_for_web(
    keep = keep_for("gw"),
    layer_label = "Bulletin 118 groundwater basins"
  )

county_map <- county |>
  simplify_sf_for_web(
    keep = keep_for("county"),
    layer_label = "Counties"
  )

