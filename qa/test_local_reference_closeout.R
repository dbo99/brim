#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(sf))

source("00_config/config_local_reference_interactions.r")
source("03_functions/local_reference_interaction_helpers.r")

square <- function(xmin, ymin, size = 0.2) {
  sf::st_polygon(list(matrix(c(
    xmin, ymin, xmin + size, ymin, xmin + size, ymin + size,
    xmin, ymin + size, xmin, ymin
  ), ncol = 2, byrow = TRUE)))
}

make_sf <- function(data, x = -120, y = 35) {
  sf::st_sf(data, geometry = sf::st_sfc(
    lapply(seq_len(nrow(data)), function(i) square(x + i, y + i)),
    crs = 4326
  ))
}

pt_validate_local_reference_config()
closeout_rows <- LOCAL_REFERENCE_INTERACTION_REGISTRY$implementation_status ==
  "reference_closeout"
stopifnot(
  identical(which(closeout_rows), c(6L, 8L, 9L, 10L, 11L)),
  !isTRUE(LOCAL_REFERENCE_INTERACTION_REGISTRY$legend_lbl_available[6]),
  all(LOCAL_REFERENCE_INTERACTION_REGISTRY$legend_lbl_available[c(8:11)]),
  identical(
    which(LOCAL_REFERENCE_INTERACTION_REGISTRY$legend_lbl_zoom_visible),
    c(8L, 9L, 10L, 11L)
  ),
  length(LOCAL_REFERENCE_INTERACTION_REGISTRY$filter_facets[[8]]) == 0L,
  identical(LOCAL_REFERENCE_INTERACTION_REGISTRY$filter_mode[9], "numeric_minimum"),
  identical(
    LOCAL_REFERENCE_INTERACTION_REGISTRY$card_caution[6],
    paste(
      "This is the dissolved outer DRECP planning-area boundary used for",
      "screening. It does not depict DRECP land-use allocations or designations."
    )
  ),
  isTRUE(LOCAL_REFERENCE_INTERACTION_REGISTRY$distinguish_units_supported[11]),
  identical(
    LOCAL_REFERENCE_INTERACTION_REGISTRY$distinguish_label[11],
    "Distinguish districts"
  ),
  identical(
    LOCAL_REFERENCE_INTERACTION_REGISTRY$runtime_presentation[[11]]$restore_input_order,
    TRUE
  ),
  identical(
    LOCAL_REFERENCE_INTERACTION_REGISTRY$runtime_presentation[[11]]$bring_selected_to_front,
    TRUE
  ),
  grepl(
    "colors are deterministic and have no legal or management meaning",
    LOCAL_REFERENCE_INTERACTION_REGISTRY$card_caution[11],
    fixed = TRUE
  )
)

compact <- pt_local_reference_compact_search_payload(
  "water_districts",
  list(list(
    semantic_feature_key = "water:name:alpha",
    feature_key = "water:name:alpha",
    display_name = "Alpha Water District",
    category_keys = "water_district",
    search_text = "alpha water district water name alpha",
    semantic_feature_bounds = c(34, -119, 35, -118),
    geometry_component_count = 1L
  )),
  list(list(
    geometry_key = "water-1",
    feature_key = "water:name:alpha",
    semantic_feature_key = "water:name:alpha",
    category_key = "water_district",
    geometry_component_count = 1L,
    facet_values = list(),
    numeric_value = NULL
  ))
)
stopifnot(
  identical(
    names(compact$features[[1]]),
    c(
      "semantic_feature_key", "display_name", "category_keys",
      "semantic_feature_bounds"
    )
  ),
  identical(
    names(compact$records[[1]]),
    c("geometry_key", "semantic_feature_key", "category_key")
  )
)

drecp_raw <- make_sf(data.frame(source_internal = "keep", stringsAsFactors = FALSE))
drecp_wkb <- sf::st_as_binary(sf::st_geometry(drecp_raw))
drecp <- pt_prepare_local_reference_closeout_layer(drecp_raw, "drecp")
stopifnot(
  identical(sf::st_as_binary(sf::st_geometry(drecp)), drecp_wkb),
  identical(drecp$source_internal, "keep"),
  identical(unique(drecp$pt_display_name), "DRECP Planning Area Boundary"),
  !"pt_reference_hover_html" %in% names(drecp),
  grepl("outer planning-area boundary", drecp$popup_html, fixed = TRUE),
  grepl("eplanning.blm.gov/eplanning-ui/project/66949/510", drecp$popup_html, fixed = TRUE),
  !grepl("source_internal", drecp$popup_html, fixed = TRUE)
)

grazing_raw <- make_sf(data.frame(
  ALLOT_NAME = c("Alpha Allotment", "Alpha Allotment", "Beta Allotment"),
  ALLOT_NO = c("00142", "00142", "00999"),
  source_internal = 1:3,
  stringsAsFactors = FALSE
))
grazing <- pt_prepare_local_reference_closeout_layer(
  grazing_raw, "grazing_allotments"
)
stopifnot(
  identical(grazing$pt_reference_feature_display[1], "Alpha Allotment · #00142"),
  length(unique(grazing$pt_local_reference_semantic_key)) == 2L,
  !anyDuplicated(grazing$pt_local_reference_geometry_key),
  grepl("Allotment #00142", grazing$pt_reference_hover_html[1], fixed = TRUE),
  !any(grepl("field.office|field_office", names(grazing), ignore.case = TRUE))
)

county_raw <- make_sf(data.frame(
  county_name = c("Alpha County", "Beta County"),
  percentBLMland = c(12.34, 67.89),
  popup_html = c("<b>Alpha County</b>", "<b>Beta County</b>"),
  stringsAsFactors = FALSE
))
county <- pt_prepare_local_reference_closeout_layer(county_raw, "counties")
stopifnot(
  grepl("BLM land: 12.3%", county$pt_reference_hover_html[1], fixed = TRUE),
  identical(county$popup_html, county_raw$popup_html),
  identical(county$percentBLMland, county_raw$percentBLMland)
)

rwqcb_raw <- make_sf(data.frame(
  rwqcb_region_num = c(1, 2),
  rwqcb_region_name = c("North Coast", "San Francisco Bay"),
  rwqcb_label_text = c("1 · North Coast", "2 · San Francisco Bay"),
  rwqcb_hover_text = c("Region 1 · North Coast", "Region 2 · San Francisco Bay"),
  rwqcb_fill_col = c("#A6CEE3", "#1F78B4"),
  rwqcb_fill_opacity = c(0.28, 0.28),
  rwqcb_stroke_col = c("#4682A9", "#155A86"),
  rwqcb_stroke_weight = c(1.2, 1.2),
  popup_html = c("north", "bay"),
  stringsAsFactors = FALSE
))
rwqcb <- pt_prepare_local_reference_closeout_layer(rwqcb_raw, "rwqcb_regions")
stopifnot(
  identical(rwqcb$fill_col, rwqcb_raw$rwqcb_fill_col),
  identical(rwqcb$pt_reference_label_text, rwqcb_raw$rwqcb_label_text),
  identical(rwqcb$popup_html, rwqcb_raw$popup_html)
)

water_raw <- make_sf(data.frame(
  agency_display = c("Alpha Water District", "Alpha Water District", "Beta ID"),
  water_district_id = c("water-1", "water-2", "water-3"),
  pt_area_sqmi = c(100, 30, 10),
  fill_col = c("#D9D9D9", "#D9D9D9", "#D9D9D9"),
  line_col = c("#737373", "#737373", "#737373"),
  stringsAsFactors = FALSE
))
water <- pt_prepare_local_reference_closeout_layer(water_raw, "water_districts")
stopifnot(
  length(unique(water$pt_local_reference_semantic_key)) == 2L,
  identical(water$pt_local_reference_geometry_key, water_raw$water_district_id),
  identical(water$pt_area_sqmi, water_raw$pt_area_sqmi),
  identical(water$pt_reference_hover_text, water_raw$agency_display),
  !any(grepl("google", water$popup_html, ignore.case = TRUE))
)

message("Local Reference closeout source/config tests passed.")
