# ==== 04_cache_admin_water_reference_layers.r ================================
##
## PURPOSE:
##   Build BLM offices, project areas, basins, field offices, conveyance, SWRCB, springs/stations if present, and reference-layer caches.
##
## NOTE:
##   This file is sourced by 05_map_build/02_build_core_map_cache.r.
##   It expects objects created earlier in that script and creates map-ready
##   cache objects in the calling environment. Do not source this file alone
##   unless you have already created the required input objects.
## ============================================================================

# ---- 8.5 BLM office points --------------------------------------------------

blm_offices_map <- blm_offices |>
  clean_sf_for_leaflet()

blm_offices_map$popup_html <- pt_make_blm_office_popups(blm_offices_map)
blm_offices_map$hover_text <- pt_make_blm_office_hover(blm_offices_map)

# ---- 8.5A BLM field-office outer boundaries --------------------------------

## IMPORTANT ORDERING NOTE:
##   Field-office outer boundaries must be prepared before BLM office point
##   colors are assigned below, because office-point colors inherit the
##   district/field-office colors from field_office_outer_map$line_col.
##   Keeping this block before the BLM office color lookup prevents
##   object-not-found errors during a full cache rebuild.

field_office_outer_map <- field_office_outer |>
  simplify_sf_for_web(
    keep = 0.20,
    layer_label = "field-office outer boundaries"
  )

field_office_outer_map$popup_html <- pt_make_field_office_outer_popups(field_office_outer_map)
field_office_outer_map$hover_text <- pt_make_field_office_outer_hover(field_office_outer_map)

## Color field-office outer boundary outlines by parent district / parent unit.
parent_names <- sort(unique(as.character(field_office_outer_map$PARENT_NAM)))
parent_names <- parent_names[!is.na(parent_names) & parent_names != ""]

parent_palette <- c(
  "#006D2C", "#08519C", "#7F2704", "#54278F",
  "#99000D", "#636363", "#B15928", "#238B45",
  "#2171B5", "#D95F0E"
)

parent_cols <- setNames(
  rep(parent_palette, length.out = length(parent_names)),
  parent_names
)

field_office_outer_map$line_col <- parent_cols[
  as.character(field_office_outer_map$PARENT_NAM)
]

field_office_outer_map$line_col[is.na(field_office_outer_map$line_col)] <- "#006D2C"

## Dash Central California District field-office boundaries so adjacent/shared
## district-color boundaries are easier to read at a glance. Keep other
## districts solid. The legend uses this same field.
field_office_outer_map$line_dash <- dplyr::case_when(
  grepl("^Central California District", as.character(field_office_outer_map$PARENT_NAM), ignore.case = TRUE) ~ "4 4",
  TRUE ~ ""
)


# ---- BLM office point colors ------------------------------------------------

## Office point colors inherit the district color already assigned to
## field-office outer boundaries.
##
## Explicit source:
##   field_office_outer_map$ADMU_NAME   = field office name
##   field_office_outer_map$PARENT_NAM  = parent district name
##   field_office_outer_map$line_col    = district color used for outer boundary
##
## Field-office points:
##   blm_offices_map$offce_name joins to ADMU_NAME, with the two Applegate
##   office locations mapped to the combined Applegate Field Office polygon.
##
## District-office points:
##   blm_offices_map$offce_name is converted from e.g.
##   "California Desert District Office" to "California Desert District",
##   then joined to PARENT_NAM to inherit the same line_col.

field_office_color_lookup <- field_office_outer_map |>
  sf::st_drop_geometry() |>
  dplyr::transmute(
    offce_name_key = tolower(trimws(as.character(ADMU_NAME))),
    parent_district = as.character(PARENT_NAM),
    fo_fill_col = line_col
  ) |>
  dplyr::distinct(offce_name_key, .keep_all = TRUE)

district_color_lookup <- field_office_outer_map |>
  sf::st_drop_geometry() |>
  dplyr::transmute(
    district_name_key = tolower(trimws(as.character(PARENT_NAM))),
    district_fill_col = line_col
  ) |>
  dplyr::distinct(district_name_key, .keep_all = TRUE)

blm_offices_map <- blm_offices_map |>
  dplyr::mutate(
    ## Field-office join key.
    ## Keep both Applegate point features, but map both to the shared
    ## Applegate Field Office polygon color.
    offce_name_key = dplyr::case_when(
      offce_name == "Applegate Field Office - Alturas" ~ "applegate field office",
      offce_name == "Applegate Field Office - Cedarville" ~ "applegate field office",
      TRUE ~ tolower(trimws(as.character(offce_name)))
    ),
    
    ## District-office join key.
    ## Example:
    ##   "California Desert District Office" -> "california desert district"
    district_name_key = tolower(trimws(as.character(offce_name))),
    district_name_key = gsub("\\s+office$", "", district_name_key)
  ) |>
  dplyr::left_join(
    field_office_color_lookup,
    by = "offce_name_key"
  ) |>
  dplyr::left_join(
    district_color_lookup,
    by = "district_name_key"
  ) |>
  dplyr::mutate(
    fill_col = dplyr::case_when(
      offce_type == "fo" & !is.na(fo_fill_col) ~ fo_fill_col,
      offce_type == "do" & !is.na(district_fill_col) ~ district_fill_col,
      offce_type == "caso" ~ "#d73027",
      TRUE ~ "#555555"
    )
  ) |>
  dplyr::select(-offce_name_key, -district_name_key)

message("BLM office point colors inherited from field-office outer line_col:")
print(
  blm_offices_map |>
    sf::st_drop_geometry() |>
    dplyr::select(offce_name, offce_type, parent_district, fill_col) |>
    dplyr::arrange(offce_type, parent_district, offce_name),
  n = Inf
)

# ---- 8.6 Project areas ------------------------------------------------------

project_areas_map <- project_areas |>
  clean_sf_for_leaflet() |>
  simplify_sf_for_web(
    keep = 0.30,
    layer_label = "Project areas"
  )

project_areas_map$popup_html <- pt_make_project_area_popups(project_areas_map)

# ---- 8.7 CNRFC basin polygons ----------------------------------------------

cnrfc_basins_map <- cnrfc_basins |>
  simplify_sf_for_web(
    keep = 0.12,
    layer_label = "CNRFC basins"
  )

cnrfc_basins_map$popup_html <- pt_make_cnrfc_basin_popups(cnrfc_basins_map)

## Color CNRFC basin polygons by forecast group.
forecast_groups <- sort(unique(as.character(cnrfc_basins_map$ForecastGr)))
forecast_groups <- forecast_groups[!is.na(forecast_groups) & forecast_groups != ""]

forecast_group_palette <- c(
  "#1F78B4", "#33A02C", "#E31A1C", "#FF7F00",
  "#6A3D9A", "#A6CEE3", "#B2DF8A", "#FDBF6F",
  "#CAB2D6", "#B15928"
)

forecast_group_cols <- setNames(
  rep(forecast_group_palette, length.out = length(forecast_groups)),
  forecast_groups
)

cnrfc_basins_map$fill_col <- forecast_group_cols[
  as.character(cnrfc_basins_map$ForecastGr)
]

cnrfc_basins_map$stroke_col <- cnrfc_basins_map$fill_col

cnrfc_basins_map$fill_col[is.na(cnrfc_basins_map$fill_col)] <- "#BDBDBD"
cnrfc_basins_map$stroke_col[is.na(cnrfc_basins_map$stroke_col)] <- "#636363"

# ---- 8.9 CalSim3 model arcs -------------------------------------------------

calsim3_arcs_map <- calsim3_arcs |>
  simplify_sf_for_web(
    keep = keep_for("calsim3"),
    layer_label = "CalSim3 arcs"
  )

calsim3_arcs_map$popup_html <- pt_make_calsim3_arc_popups(calsim3_arcs_map)

## CalSim3 arc styling:
##   Channel   = blue
##   Diversion = red
calsim3_arcs_map$line_col <- dplyr::case_when(
  calsim3_arcs_map$Type == "Channel"   ~ "#1F78B4",  # blue
  calsim3_arcs_map$Type == "Diversion" ~ "#E31A1C",  # red
  calsim3_arcs_map$Type == "Return"    ~ "#33A02C",  # green
  calsim3_arcs_map$Type == "Inflow"    ~ "#A6CEE3",  # baby blue
  TRUE                                 ~ "#777777"
)

calsim3_arcs_map$line_weight <- dplyr::case_when(
  calsim3_arcs_map$Type == "Channel"   ~ 1.4,
  calsim3_arcs_map$Type == "Diversion" ~ 1.4,
  calsim3_arcs_map$Type == "Return"    ~ 1.0,
  calsim3_arcs_map$Type == "Inflow"    ~ 1.0,
  TRUE                                 ~ 1.2
)

## Compact hover text is intentionally useful without turning on permanent
## labels, which would add visual clutter and size to the standalone HTML.
calsim3_arcs_map$hover_text <- paste0(
  dplyr::coalesce(as.character(calsim3_arcs_map$Name), as.character(calsim3_arcs_map$Arc_ID), "CalSim3 arc"),
  "\n",
  "Type: ", dplyr::coalesce(as.character(calsim3_arcs_map$Type), "unknown"),
  "\n",
  dplyr::coalesce(as.character(calsim3_arcs_map$FromNode), "?"),
  " → ",
  dplyr::coalesce(as.character(calsim3_arcs_map$ToNode), "?")
)

# ---- 8.9A CalSim3 model nodes ------------------------------------------------
##
## PURPOSE:
##   Build a map-ready CalSim3 node point layer from:
##
##     04_processed_data/rds/calsim3_nodes_wgs84.rds
##
## DESIGN:
##   NodeDescri carries detailed model-node descriptions.  For map readability,
##   the detailed descriptions are collapsed into a smaller set of groups for
##   symbology, while the original description is retained in hover/popup text.
##
## POPUP/HOVER FIELDS:
##   node_id_display   = CalSim3_ID from the source shapefile
##   riv_name_display  = Riv_Name, where available
##   comment_display   = Comment, where available
##   node_description  = NodeDescri

calsim3_nodes_map <- calsim3_nodes |>
  clean_sf_for_leaflet()

pt_calsim3_node_demand_code <- function(node_id) {
  id <- toupper(trimws(as.character(node_id)))
  out <- rep(NA_character_, length(id))

  ## CalSim3 demand nodes commonly look like 03_PU1, 04_PA2, 08N_SA1,
  ## 17S_PR, etc.  Capture the demand suffix so QA and symbology can separate
  ## project, settlement, and non-project demand nodes without hard-coding every
  ## individual demand node name.
  hit <- grepl("^[0-9]{2}[A-Z]?_(PA|PU|PR|SA|SU|NA|NU|NR)[0-9]*$", id)
  out[hit] <- sub("^[0-9]{2}[A-Z]?_(PA|PU|PR|SA|SU|NA|NU|NR)[0-9]*$", "\\1", id[hit])
  out
}

pt_calsim3_node_group <- function(node_description, node_id = NULL) {
  desc <- tolower(trimws(as.character(node_description)))
  id <- toupper(trimws(as.character(node_id)))
  demand_code <- pt_calsim3_node_demand_code(id)

  dplyr::case_when(
    demand_code == "PU" ~ "Project demand – urban",
    demand_code == "PA" ~ "Project demand – ag",
    demand_code == "PR" ~ "Project demand – refuge",
    demand_code == "NU" ~ "Non-project demand – urban",
    demand_code == "NA" ~ "Non-project demand – ag",
    demand_code == "NR" ~ "Non-project demand – refuge",
    demand_code == "SU" ~ "Settlement demand – urban",
    demand_code == "SA" ~ "Settlement demand – ag",

    desc %in% c(
      "water treatment plant",
      "wastewater treatment plant"
    ) | grepl("(^|_)WTP|WWTP", id) ~ "Treatment plant",

    desc == "return flow" | grepl("^R_", id) ~ "Return flow",

    desc %in% c(
      "conveyance-canal",
      "conveyance-channel",
      "conveyance-channel-streamgage",
      "conveyance-canal-streamgage",
      "conveyence-channel-streamgage"
    ) ~ "Conveyance",

    desc %in% c(
      "storage- reservoir or lake",
      "storage-  reservoir or lake"
    ) | grepl("^S_", id) ~ "Storage / Reservoir",

    grepl("^demand-", desc) ~ "Demand – other",

    desc %in% c(
      "external unit",
      "external-unit"
    ) | grepl("^E_", id) ~ "External Unit",

    desc == "major feature" ~ "Major Feature",

    is.na(desc) | desc == "" | desc == "na" ~ "Unknown / Other",

    TRUE ~ "Unknown / Other"
  )
}

calsim3_node_cols <- c(
  "Conveyance" = "#1F78B4",
  "Storage / Reservoir" = "#6A3D9A",
  "Project demand – urban" = "#D95F02",
  "Project demand – ag" = "#E66101",
  "Project demand – refuge" = "#7570B3",
  "Non-project demand – urban" = "#A6761D",
  "Non-project demand – ag" = "#8C510A",
  "Non-project demand – refuge" = "#8073AC",
  "Settlement demand – urban" = "#FDB863",
  "Settlement demand – ag" = "#DFC27D",
  "Demand – other" = "#F1A340",
  "Treatment plant" = "#00A6D6",
  "Return flow" = "#33A02C",
  "External Unit" = "#777777",
  "Major Feature" = "#E31A1C",
  "Unknown / Other" = "#BDBDBD"
)

calsim3_nodes_map <- calsim3_nodes_map |>
  dplyr::mutate(
    calsim3_node_group = pt_calsim3_node_group(.data$node_description, .data$node_id_display),
    calsim3_node_demand_code = pt_calsim3_node_demand_code(.data$node_id_display),
    node_fill_col = unname(calsim3_node_cols[.data$calsim3_node_group]),
    node_stroke_col = dplyr::case_when(
      .data$calsim3_node_group == "Conveyance" ~ "#08519C",
      .data$calsim3_node_group == "Storage / Reservoir" ~ "#3F007D",
      grepl("demand", .data$calsim3_node_group, ignore.case = TRUE) ~ "#7F3B08",
      .data$calsim3_node_group == "Treatment plant" ~ "#006D8F",
      .data$calsim3_node_group == "Return flow" ~ "#1B7837",
      .data$calsim3_node_group == "External Unit" ~ "#4D4D4D",
      .data$calsim3_node_group == "Major Feature" ~ "#99000D",
      TRUE ~ "#737373"
    ),
    node_radius = dplyr::case_when(
      .data$calsim3_node_group %in% c("Storage / Reservoir", "Major Feature") ~ 5.5,
      grepl("demand", .data$calsim3_node_group, ignore.case = TRUE) ~ 5.0,
      .data$calsim3_node_group == "External Unit" ~ 4.0,
      TRUE ~ 4.5
    ),
    hover_text = paste0(
      dplyr::coalesce(as.character(.data$node_id_display), "CalSim3 node"), "\n",
      "Group: ", .data$calsim3_node_group, "\n",
      "Description: ", dplyr::coalesce(as.character(.data$node_description), "Not available"),
      dplyr::if_else(
        !is.na(.data$riv_name_display) & .data$riv_name_display != "",
        paste0("\nRiver: ", .data$riv_name_display),
        ""
      ),
      dplyr::if_else(
        !is.na(.data$comment_display) & .data$comment_display != "",
        paste0("\nComment: ", .data$comment_display),
        ""
      )
    )
  )

calsim3_nodes_map$popup_html <- pt_make_calsim3_node_popups(calsim3_nodes_map)

message("CalSim3 node groups:")
print(table(calsim3_nodes_map$calsim3_node_group, useNA = "always"))

# ---- 8.10 Major conveyance lines -------------------------------------------
##
## Major conveyance was dissolved by Pname + Operator in:
##
##   02_preprocess/12_major_conveyance.r
##
## Symbology is handled in leaflet_layer_helpers.r because Fed/State lines are
## drawn twice with alternating Federal/State dash colors.

major_conveyance_map <- major_conveyance |>
  simplify_sf_for_web(
    keep = keep_for("major_conveyance"),
    layer_label = "Major conveyance"
  )

## Normalize the old "Local / CCWD" grouping into the clearer display group
## requested for this map.  Any major conveyance feature that is not clearly
## Federal/CVP, State/SWP, or joint Federal/State is treated as Non-CVP/SWP for
## the operator-group summary, while the original Operator field remains in the
## popup for specificity.
if ("operator_group" %in% names(major_conveyance_map)) {
  major_conveyance_map <- major_conveyance_map |>
    dplyr::mutate(
      operator_group = dplyr::case_when(
        .data$operator_group %in% c("Federal", "State", "Fed/State") ~
          as.character(.data$operator_group),
        TRUE ~ "Non-CVP/SWP"
      )
    )
}

major_conveyance_map$popup_html <- pt_make_major_conveyance_popups(
  major_conveyance_map
)

# ---- 8.10A CNRFC FNF Delta basins ------------------------------------------
##
## Basin polygons above major reservoirs connected to the Delta.  These are
## drawn as outline-only basin/context features and link to CNRFC FNF ensemble
## products using each feature's nws5id.

cnrfc_fnf_delta_map <- cnrfc_fnf_delta |>
  simplify_sf_for_web(
    keep = keep_for("cnrfc_fnf_delta"),
    layer_label = "CNRFC FNF Delta basins"
  )

cnrfc_fnf_delta_map <- cnrfc_fnf_delta_map |>
  dplyr::mutate(
    popup_html = pt_make_cnrfc_fnf_delta_popups(cnrfc_fnf_delta_map),
    hover_text = paste0(
      "River: ", dplyr::coalesce(as.character(.data$River), "Not available"), "\n",
      "NWS ID: ", dplyr::coalesce(as.character(.data$nws5id), "Not available"), "\n",
      "Reservoir: ", dplyr::coalesce(as.character(.data$res), "Not available")
    ),
    line_col = "#666666",
    line_weight = 1.8
  )

# ---- 8.10B CVP/SWP X2 kilometer reference points ---------------------------
##
## These are lightweight point references for X2 position in kilometers.  The
## final Leaflet helper draws both small clickable markers and always-visible
## text labels because the label value itself is the point of the layer.

x2_km_map <- x2_km |>
  clean_sf_for_leaflet()

x2_km_map <- x2_km_map |>
  dplyr::mutate(
    x2_km_num = suppressWarnings(as.numeric(.data$RKI)),
    x2_km_display = pt_format_x2_km(.data$RKI),
    popup_html = pt_make_x2_km_popups(x2_km_map),
    hover_text = paste0(.data$x2_km_display, " km")
  )


# ---- 8.10B.1 RWQCB regional boundaries -------------------------------------
##
## PURPOSE:
##   Build a snappy cached local layer for the nine RWQCB regions from the
##   downloaded State Water Board boundary layer.
##
## DESIGN:
##   - No geometry simplification is applied here because these are
##     jurisdictional boundaries.
##   - Colors match the State Water Board service renderer used in existing
##     maps, with transparency controlled by cached fields.
##   - Hover is intentionally one line; click popup is only a link to the
##     RWQCB page.

rwqcb_regions_map <- rwqcb_regions |>
  clean_sf_for_leaflet()

pt_rwqcb_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}

pt_rwqcb_region_num <- function(x) {
  if ("RB" %in% names(x)) {
    return(suppressWarnings(as.integer(as.character(x$RB))))
  }
  if ("rb" %in% names(x)) {
    return(suppressWarnings(as.integer(as.character(x$rb))))
  }
  rep(NA_integer_, nrow(x))
}

rwqcb_lookup <- tibble::tibble(
  rwqcb_region_num = 1:9,
  rwqcb_region_name = c(
    "North Coast",
    "San Francisco Bay",
    "Central Coast",
    "Los Angeles",
    "Central Valley",
    "Lahontan",
    "Colorado River Basin",
    "Santa Ana",
    "San Diego"
  ),
  rwqcb_url = c(
    "https://www.waterboards.ca.gov/northcoast/",
    "https://www.waterboards.ca.gov/sanfranciscobay/",
    "https://www.waterboards.ca.gov/centralcoast/",
    "https://www.waterboards.ca.gov/losangeles/",
    "https://www.waterboards.ca.gov/centralvalley/",
    "https://www.waterboards.ca.gov/lahontan/",
    "https://www.waterboards.ca.gov/coloradoriver/",
    "https://www.waterboards.ca.gov/santaana/",
    "https://www.waterboards.ca.gov/sandiego/"
  ),
  ## Official-style unique-value fill colors from the State Water Board service.
  rwqcb_fill_col = c(
    "#89CD66",  # 1 North Coast
    "#BA5F27",  # 2 San Francisco Bay
    "#A8A800",  # 3 Central Coast
    "#704489",  # 4 Los Angeles
    "#397DBD",  # 5 Central Valley
    "#267300",  # 6 Lahontan
    "#BA328B",  # 7 Colorado River Basin
    "#B0893F",  # 8 Santa Ana
    "#002673"   # 9 San Diego
  ),
  rwqcb_stroke_col = c(
    "#4F8A2F", "#6B3515", "#666600", "#3C2450", "#1F4E7A",
    "#174400", "#731F56", "#6F5525", "#001545"
  )
)

rwqcb_regions_map <- rwqcb_regions_map |>
  dplyr::mutate(
    rwqcb_region_num = pt_rwqcb_region_num(rwqcb_regions_map)
  ) |>
  dplyr::left_join(
    rwqcb_lookup,
    by = "rwqcb_region_num"
  ) |>
  dplyr::mutate(
    ## Prefer the service RB_NAME field only as a fallback; the lookup above
    ## keeps naming stable for labels and links.
    rwqcb_region_name = dplyr::coalesce(
      .data$rwqcb_region_name,
      if ("RB_NAME" %in% names(rwqcb_regions_map)) pt_rwqcb_chr(rwqcb_regions_map$RB_NAME) else NA_character_
    ),
    rwqcb_hover_text = paste0(
      "Region ", .data$rwqcb_region_num, " – ", .data$rwqcb_region_name
    ),
    rwqcb_label_text = paste0(
      "R", .data$rwqcb_region_num, " – ", .data$rwqcb_region_name
    ),
    rwqcb_fill_opacity = 0.28,
    rwqcb_stroke_weight = 1.2
  ) |>
  dplyr::arrange(.data$rwqcb_region_num)

## Build the small click popup after the cached RWQCB fields exist.
## This avoids passing the pre-mutate object into the popup helper.
rwqcb_regions_map$popup_html <- pt_make_rwqcb_region_popups(rwqcb_regions_map)

message("RWQCB regions prepared for map cache: ", nrow(rwqcb_regions_map))

# ---- 8.10C Deltamapr canals -------------------------------------------------
##
## Secondary conveyance/canal layer from deltamapr.  Symbology uses the same
## CVP blue and SWP orange colors used for Major Conveyance.  Other operators
## keep their true Operator value in the popup but are styled as Non-CVP/SWP.

deltamapr_canals_map <- deltamapr_canals |>
  simplify_sf_for_web(
    keep = keep_for("deltamapr_canals"),
    layer_label = "Deltamapr conveyance"
  )

deltamapr_canals_map <- deltamapr_canals_map |>
  dplyr::mutate(
    ## Normalize operator text only for symbology.  The true Operator value is
    ## preserved in hover/popup text.  This catches common CVP/SWP variants,
    ## including Bureau/Reclamation wording that appears in deltamapr records.
    operator_lc = tolower(trimws(as.character(.data$Operator))),
    deltamapr_operator_group = dplyr::case_when(
      grepl("central valley project|\\bcvp\\b|bureau of reclamation|\\busbr\\b|reclamation", .data$operator_lc) ~ "Central Valley Project",
      grepl("state water project|\\bswp\\b|department of water resources|\\bdwr\\b|california dwr|ca dwr", .data$operator_lc) ~ "State Water Project",
      TRUE ~ "Non-CVP/SWP"
    ),
    line_col = dplyr::case_when(
      .data$deltamapr_operator_group == "Central Valley Project" ~ "#1F78B4",
      .data$deltamapr_operator_group == "State Water Project" ~ "#FF7F00",
      TRUE ~ "#777777"
    ),
    line_weight = dplyr::case_when(
      .data$deltamapr_operator_group %in% c("Central Valley Project", "State Water Project") ~ 1.7,
      TRUE ~ 1.25
    ),
    popup_html = pt_make_deltamapr_canals_popups(deltamapr_canals_map),
    hover_text = paste0(
      dplyr::coalesce(as.character(.data$Name), "Deltamapr conveyance"), "\n",
      "Operator: ", dplyr::coalesce(as.character(.data$Operator), "Not available"), "\n",
      "Type: ", dplyr::coalesce(as.character(.data$Conv_Type), "Not available"), "\n",
      "Subtype: ", dplyr::coalesce(as.character(.data$Conv_Sub), "Not available")
    )
  )

# ---- 8.10D Water districts --------------------------------------------------
##
## PURPOSE:
##   Build a map-ready water-district polygon layer from:
##
##     04_processed_data/rds/water_districts_wgs84.rds
##
## WHY AREA SORTING MATTERS:
##   Many water-district polygons overlap or contain smaller "island" districts.
##   If large surrounding polygons are drawn after small polygons, the large
##   polygon can intercept hover/click events and make the smaller district
##   difficult or impossible to identify.
##
##   To improve usability, polygon area is calculated in EPSG:3310, then the
##   layer is sorted from largest to smallest. Leaflet draws features in row
##   order, so this causes large polygons to be drawn first and smaller polygons
##   to be drawn last/on top.
##
## DISPLAY DESIGN:
##   Water districts are treated as a reference-boundary layer:
##     - very faint fill for clickability
##     - matching colored outline for readability
##     - strong hover highlight to make the active polygon obvious
##

message("Preparing water districts...")

## Small helper for robust character handling.
pt_wd_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[is.na(x) | x == "" | x == "NA"] <- NA_character_
  x
}

## Small helper for safe Google-search URLs.
pt_wd_google_url <- function(x) {
  q <- paste("California water district", x)
  paste0(
    "https://www.google.com/search?q=",
    utils::URLencode(q, reserved = TRUE)
  )
}

## Local rotating palette. Keeping this local avoids relying on the order or
## existence of palettes used elsewhere in the script.
pt_water_district_palette <- c(
  "#1F78B4", "#33A02C", "#E31A1C", "#FF7F00", "#6A3D9A",
  "#B15928", "#A6CEE3", "#B2DF8A", "#FB9A99", "#FDBF6F",
  "#CAB2D6", "#8DD3C7", "#80B1D3", "#BEBADA", "#FFFFB3",
  "#FDB462", "#FCCDE5", "#BC80BD", "#CCEBC5", "#FFED6F"
)

## Calculate area before web simplification so draw order reflects the original
## source geometry rather than simplified geometry.
water_districts_area_tbl <- water_districts |>
  sf::st_transform(3310) |>
  dplyr::mutate(
    pt_area_sqmi = as.numeric(sf::st_area(geometry)) / 2589988.110336
  ) |>
  sf::st_drop_geometry() |>
  dplyr::select(
    AGENCYNAME,
    pt_area_sqmi
  ) |>
  dplyr::mutate(
    pt_source_row = dplyr::row_number()
  )

water_districts_map <- water_districts |>
  dplyr::mutate(
    pt_source_row = dplyr::row_number()
  ) |>
  simplify_sf_for_web(
    keep = keep_for("water_districts"),
    layer_label = "Water districts"
  ) |>
  dplyr::left_join(
    water_districts_area_tbl,
    by = c("pt_source_row", "AGENCYNAME")
  )

## Clean and standardize agency-name display text.
water_districts_map$agency_display <- pt_wd_chr(water_districts_map$AGENCYNAME)
water_districts_map$agency_display[is.na(water_districts_map$agency_display)] <-
  "Water district name not available"

## Create stable per-feature IDs for debugging and future JS work.
water_districts_map$water_district_id <- paste0(
  "water_district_",
  seq_len(nrow(water_districts_map))
)

## Assign a rotating color by agency name.
water_district_agencies <- sort(unique(water_districts_map$agency_display))
water_district_cols <- setNames(
  rep(
    pt_water_district_palette,
    length.out = length(water_district_agencies)
  ),
  water_district_agencies
)

water_districts_map$fill_col <- water_district_cols[
  water_districts_map$agency_display
]

water_districts_map$line_col <- water_districts_map$fill_col

water_districts_map$fill_col[is.na(water_districts_map$fill_col)] <- "#BDBDBD"
water_districts_map$line_col[is.na(water_districts_map$line_col)] <- "#737373"

## Hover text is intentionally short. The popup carries the link.
water_districts_map$hover_text <- water_districts_map$agency_display

## Popup includes agency name and a generic Google search.
water_districts_map$popup_html <- paste0(
  "<b>", htmltools::htmlEscape(water_districts_map$agency_display), "</b><br/>",
  "<a href='",
  htmltools::htmlEscape(pt_wd_google_url(water_districts_map$agency_display)),
  "' target='_blank'>Google water district search</a>"
)

## Draw largest polygons first, smallest polygons last/on top.
## This is the key fix for small/interior districts being hidden by larger
## surrounding polygons.
water_districts_map <- water_districts_map |>
  dplyr::arrange(
    dplyr::desc(.data$pt_area_sqmi),
    .data$agency_display
  )

message("Water districts prepared for map cache: ", nrow(water_districts_map))
message(
  "Water districts area-sorted largest-to-smallest so smaller polygons draw on top."
)

# ---- 8.10E SWRCB / CalWATRS POD water rights on BLM-managed land ------------

swrcb_pod_wr_blm_map <- swrcb_pod_wr_blm |>
  clean_sf_for_leaflet()

## Helper: safely pull a column as character.
pt_chr_col <- function(x, nm) {
  if (nm %in% names(x)) {
    out <- as.character(x[[nm]])
  } else {
    out <- rep(NA_character_, nrow(x))
  }
  
  out <- trimws(out)
  out[out == ""] <- NA_character_
  out
}

## Helper: first nonblank value across candidate fields.
pt_first_nonblank <- function(...) {
  vals <- list(...)
  n <- length(vals[[1]])
  out <- rep(NA_character_, n)
  
  for (v in vals) {
    v <- as.character(v)
    v <- trimws(v)
    v[v == ""] <- NA_character_
    
    fill <- is.na(out) & !is.na(v)
    out[fill] <- v[fill]
  }
  
  out
}

pt_num <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}

pt_format_amount <- function(x, max_digits = 6L) {
  out <- rep(NA_character_, length(x))

  is_whole <- !is.na(x) & abs(x - round(x)) < 0.0000005

  out[is_whole] <- formatC(
    x[is_whole],
    format = "f",
    digits = 0,
    big.mark = ","
  )

  out[!is_whole & !is.na(x)] <- formatC(
    x[!is_whole & !is.na(x)],
    format = "f",
    digits = max_digits,
    big.mark = ","
  )

  ## Remove trailing zeroes only after a decimal point.  Six decimal places are
  ## retained before trimming so small positive rights never display as zero.
  out <- sub("(\\.\\d*?)0+$", "\\1", out)
  out <- sub("\\.$", "", out)

  tiny_positive <- !is.na(x) & x > 0 & out == "0"
  out[tiny_positive] <- format(
    x[tiny_positive],
    scientific = TRUE,
    digits = max_digits,
    trim = TRUE
  )

  out
}

wr_id_display <- pt_first_nonblank(
  pt_chr_col(swrcb_pod_wr_blm_map, "wr_application_number"),
  pt_chr_col(swrcb_pod_wr_blm_map, "wr_id")
)

pod_id_display <- pt_first_nonblank(
  pt_chr_col(swrcb_pod_wr_blm_map, "pod_id"),
  pt_chr_col(swrcb_pod_wr_blm_map, "wr_pod_id"),
  pt_chr_col(swrcb_pod_wr_blm_map, "pod_id_gis"),
  pt_chr_col(swrcb_pod_wr_blm_map, "wr_pod_id_gis"),
  pt_chr_col(swrcb_pod_wr_blm_map, "objectid")
)

## True mapped point-feature identifier.  This is different from the WR/POD-list
## ID in some CalWATRS records and is useful for debugging duplicate WR/POD IDs
## plotted at different coordinates.
pod_feature_id_display <- pt_first_nonblank(
  pt_chr_col(swrcb_pod_wr_blm_map, "objectid"),
  pt_chr_col(swrcb_pod_wr_blm_map, "globalid")
)

holder_display <- pt_first_nonblank(
  pt_chr_col(swrcb_pod_wr_blm_map, "wr_primary_owner_name"),
  pt_chr_col(swrcb_pod_wr_blm_map, "wr_application_primary_owner"),
  pt_chr_col(swrcb_pod_wr_blm_map, "primary_owner")
)

source_display <- pt_first_nonblank(
  pt_chr_col(swrcb_pod_wr_blm_map, "source_name"),
  pt_chr_col(swrcb_pod_wr_blm_map, "wr_source_name")
)

county_display <- pt_first_nonblank(
  pt_chr_col(swrcb_pod_wr_blm_map, "county"),
  pt_chr_col(swrcb_pod_wr_blm_map, "wr_county")
)

blm_include_reason_display <- pt_chr_col(
  swrcb_pod_wr_blm_map,
  "blm_include_reason"
)

pod_status_display <- pt_first_nonblank(
  pt_chr_col(swrcb_pod_wr_blm_map, "pod_status"),
  pt_chr_col(swrcb_pod_wr_blm_map, "wr_pod_status")
)

wr_status_display <- pt_chr_col(swrcb_pod_wr_blm_map, "wr_water_right_status")
wr_type_display <- pt_chr_col(swrcb_pod_wr_blm_map, "wr_water_right_type")

# ---- 8.10A.1 SWRCB-provided 2026 BLM correction overlay --------------------
##
## SOURCE ROLES (keep these distinct):
##   - Public SWRCB/CalWATRS POD + WR data remain the statewide spatial and
##     relational backbone for all three BRIM POD screening layers.
##   - The SWRCB-provided 2026 BLM export is an authoritative subset supplied
##     directly to BLM.  For matching normalized water-right IDs, its
##     "Face value" replaces the materially incomplete public face-value field.
##   - Records absent from the 2026 export continue using the public value.
##
## IMPORTANT:
##   The 2026 face value is treated as acre-feet per year (AFY), consistent with
##   the SWRCB export definition and BLM's use of the file.  The overlay is
##   applied once here, before symbol size, AFY filters, legend counts, hover,
##   popup, and the official/spatial/name-candidate layer split.  Do not add
##   separate source-precedence logic to the three Leaflet layer branches.
##
## DUPLICATES AND BUILD GATE:
##   Reusable parsing, provenance, zero-truncation resolution, reviewed
##   override validation, and unresolved-conflict blocking live in
##   03_functions/swrcb_face_value_helpers.r. The core-cache orchestrator
##   sources that helper before this block. Public values must never resolve an
##   authoritative duplicate conflict.

swrcb_2026_lookup_result <- pt_read_swrcb_2026_blm_wr_lookup(
  path = SRC$swrcb_2026_blm_wr_csv,
  override_path = SRC$swrcb_2026_face_value_conflict_resolutions,
  stop_on_unresolved = TRUE
)

swrcb_2026_blm_wr_lookup <- swrcb_2026_lookup_result$lookup
swrcb_2026_blm_wr_ids <- swrcb_2026_blm_wr_lookup$swrcb_wr_id_norm

face_afy_public <- pt_num(
  pt_chr_col(swrcb_pod_wr_blm_map, "wr_face_value_amount")
)
face_units_public_raw <- pt_clean_swrcb_value_text(
  pt_chr_col(swrcb_pod_wr_blm_map, "wr_face_value_units")
)

swrcb_face_value_reconciliation <- pt_reconcile_swrcb_face_values(
  water_right_ids = wr_id_display,
  public_face_afy = face_afy_public,
  public_face_units = face_units_public_raw,
  lookup = swrcb_2026_blm_wr_lookup,
  stop_on_unresolved = TRUE,
  override_path = SRC$swrcb_2026_face_value_conflict_resolutions
)

swrcb_wr_id_norm <- swrcb_face_value_reconciliation$swrcb_wr_id_norm
swrcb_2026_blm_wr_list <-
  swrcb_face_value_reconciliation$authoritative_2026_present
swrcb_2026_blm_wr_list_display <- dplyr::if_else(
  swrcb_2026_blm_wr_list,
  "Yes",
  "No"
)
face_afy_2026 <-
  swrcb_face_value_reconciliation$authoritative_face_afy
face_value_2026_raw_values <-
  swrcb_face_value_reconciliation$authoritative_raw_values
face_value_2026_numeric_values <-
  swrcb_face_value_reconciliation$authoritative_numeric_values
face_value_2026_spreadsheet_row_count <-
  swrcb_face_value_reconciliation$authoritative_row_count
face_value_2026_distinct_count <-
  swrcb_face_value_reconciliation$authoritative_distinct_value_count
face_value_2026_conflict <-
  swrcb_face_value_reconciliation$authoritative_original_conflict
face_value_2026_automatic_resolution <-
  swrcb_face_value_reconciliation$authoritative_automatic_resolution
face_value_2026_reviewed_override_used <-
  swrcb_face_value_reconciliation$authoritative_reviewed_override_used
face_value_2026_resolution <-
  swrcb_face_value_reconciliation$authoritative_resolution
face_value_2026_source_reference <-
  swrcb_face_value_reconciliation$authoritative_source_reference
face_value_selected_from_2026 <-
  swrcb_face_value_reconciliation$selected_from_authoritative_2026
face_value_public_allowed <-
  swrcb_face_value_reconciliation$public_value_allowed
face_afy <- swrcb_face_value_reconciliation$selected_face_afy
face_units_display <-
  swrcb_face_value_reconciliation$selected_face_units
face_value_changed_by_2026 <-
  swrcb_face_value_reconciliation$changed_from_public
face_value_numeric_equal_public <-
  swrcb_face_value_reconciliation[[
    "numerically_equal_to_public_with_corrected_provenance"
  ]]

face_amount_display <- pt_format_amount(face_afy)
face_value_display <- dplyr::if_else(
  !is.na(face_amount_display),
  paste(face_amount_display, face_units_display),
  "Not available"
)

face_value_source_display <-
  swrcb_face_value_reconciliation$selected_face_value_source

status_lc <- tolower(wr_status_display)

swrcb_status_group <- dplyr::case_when(
  status_lc %in% c(
    "claimed",
    "claimed - local oversight",
    "licensed",
    "certified",
    "permitted",
    "registered"
  ) ~ "Active / recognized",

  status_lc == "pending" ~ "Pending",

  status_lc %in% c(
    "inactive",
    "revoked",
    "cancelled",
    "rejected"
  ) ~ "Inactive / cancelled",

  TRUE ~ "Unknown"
)

spatial_blm_match_display <- dplyr::case_when(
  blm_include_reason_display %in% c("Spatial + name match", "Spatial only") ~ "Yes",
  blm_include_reason_display == "Name match only" ~ "No",
  TRUE ~ "Unknown"
)

swrcb_screening_source_display <- dplyr::case_when(
  swrcb_2026_blm_wr_list ~
    "SWRCB 2026 BLM WR list",
  !swrcb_2026_blm_wr_list &
    blm_include_reason_display %in% c("Spatial + name match", "Spatial only") ~
    "PT2 additional spatial POD match",
  !swrcb_2026_blm_wr_list &
    blm_include_reason_display == "Name match only" ~
    "PT2 additional BLM name/text candidate",
  TRUE ~
    "Other BLM-relevant screening record"
)

blm_relevance_note <- dplyr::case_when(
  swrcb_2026_blm_wr_list &
    blm_include_reason_display %in% c("Spatial + name match", "Spatial only") ~
    "Water-right/claim ID appears in the SWRCB-provided 2026 BLM list; mapped POD point also spatially matched to BLM-managed land.",
  swrcb_2026_blm_wr_list ~
    "Water-right/claim ID appears in the SWRCB-provided 2026 BLM list; mapped POD point was not spatially matched to BLM-managed land.",
  !swrcb_2026_blm_wr_list &
    blm_include_reason_display %in% c("Spatial + name match", "Spatial only") ~
    "Additional PT2 spatial screen: mapped POD point spatially matched to BLM-managed land, but WR/claim ID was not in the SWRCB-provided 2026 BLM list.",
  !swrcb_2026_blm_wr_list &
    blm_include_reason_display == "Name match only" ~
    "Additional PT2 text/name screen: owner/holder/name text appears to reference BLM, but mapped POD point was not spatially matched to BLM-managed land and WR/claim ID was not in the SWRCB-provided 2026 BLM list.",
  TRUE ~
    "BLM relevance screening basis not available."
)


## POD-row reconciliation table used only for console/CSV QA.  It is created
## before the lean map-cache select so the public, 2026, and selected values can
## be compared without carrying all provenance columns into the final HTML.
swrcb_face_value_overlay_qa <- tibble::tibble(
  water_right_id = wr_id_display,
  swrcb_wr_id_norm = swrcb_wr_id_norm,
  pod_feature_id = pod_feature_id_display,
  pod_id = pod_id_display,
  official_2026_blm_list = swrcb_2026_blm_wr_list,
  public_face_afy = face_afy_public,
  authoritative_face_afy = face_afy_2026,
  authoritative_face_value_raw_values = face_value_2026_raw_values,
  authoritative_face_value_numeric_values = face_value_2026_numeric_values,
  authoritative_row_count = face_value_2026_spreadsheet_row_count,
  authoritative_distinct_value_count = face_value_2026_distinct_count,
  authoritative_resolution = face_value_2026_resolution,
  authoritative_original_conflict = face_value_2026_conflict,
  authoritative_automatic_resolution =
    face_value_2026_automatic_resolution,
  authoritative_reviewed_override_used =
    face_value_2026_reviewed_override_used,
  authoritative_source_reference = face_value_2026_source_reference,
  selected_face_afy = face_afy,
  selected_face_value = face_value_display,
  selected_face_value_source = face_value_source_display,
  selected_from_2026 = face_value_selected_from_2026,
  public_value_allowed = face_value_public_allowed,
  changed_by_2026 = face_value_changed_by_2026,
  numerically_equal_public_with_corrected_provenance =
    face_value_numeric_equal_public
)

swrcb_face_value_pod_summary <- swrcb_face_value_overlay_qa |>
  dplyr::summarize(
    retained_pod_rows = dplyr::n(),
    authoritative_present_retained_pod_rows = sum(
      .data$official_2026_blm_list,
      na.rm = TRUE
    ),
    authoritative_selected_retained_pod_rows = sum(
      .data$official_2026_blm_list & .data$selected_from_2026,
      na.rm = TRUE
    ),
    conflict_resolution_affected_retained_pod_rows = sum(
      .data$authoritative_original_conflict,
      na.rm = TRUE
    ),
    automatic_resolution_retained_pod_rows = sum(
      .data$authoritative_automatic_resolution,
      na.rm = TRUE
    ),
    reviewed_override_retained_pod_rows = sum(
      .data$authoritative_reviewed_override_used,
      na.rm = TRUE
    ),
    authoritative_changed_from_public_retained_pod_rows = sum(
      .data$official_2026_blm_list & .data$changed_by_2026,
      na.rm = TRUE
    ),
    authoritative_numeric_equal_public_corrected_provenance_pod_rows = sum(
      .data$official_2026_blm_list &
        .data$numerically_equal_public_with_corrected_provenance,
      na.rm = TRUE
    ),
    public_fallback_absent_id_pod_rows = sum(
      !.data$official_2026_blm_list &
        .data$public_value_allowed,
      na.rm = TRUE
    ),
    public_policy_present_blank_pod_rows = sum(
      .data$official_2026_blm_list &
        .data$public_value_allowed &
        .data$authoritative_resolution == "blank_or_missing",
      na.rm = TRUE
    ),
    authoritative_unique_retained_wr_ids = dplyr::n_distinct(
      .data$swrcb_wr_id_norm[.data$official_2026_blm_list],
      na.rm = TRUE
    )
  )

swrcb_face_value_overlay_summary <- dplyr::bind_cols(
  tibble::as_tibble(swrcb_2026_lookup_result$summary),
  swrcb_face_value_pod_summary
)

swrcb_face_value_conflict_resolution_qa <-
  swrcb_face_value_overlay_qa |>
  dplyr::filter(.data$authoritative_original_conflict) |>
  dplyr::group_by(.data$swrcb_wr_id_norm) |>
  dplyr::summarize(
    authoritative_numeric_values =
      dplyr::first(.data$authoritative_face_value_numeric_values),
    public_face_afy_values = pt_format_swrcb_numeric_values(
      .data$public_face_afy
    ),
    selected_face_afy = dplyr::first(.data$selected_face_afy),
    resolution = dplyr::first(.data$authoritative_resolution),
    automatic_resolution =
      dplyr::first(.data$authoritative_automatic_resolution),
    reviewed_override_used =
      dplyr::first(.data$authoritative_reviewed_override_used),
    retained_pod_rows = dplyr::n(),
    selected_face_value_source =
      dplyr::first(.data$selected_face_value_source),
    authoritative_source_reference =
      dplyr::first(.data$authoritative_source_reference),
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$swrcb_wr_id_norm)

message("SWRCB 2026 BLM face-value correction overlay summary:")
print(swrcb_face_value_overlay_summary, width = 1200)

message("SWRCB 2026 BLM conflicting-ID resolution QA:")
print(swrcb_face_value_conflict_resolution_qa, n = Inf, width = 1200)


swrcb_pod_wr_blm_map <- swrcb_pod_wr_blm_map |>
  dplyr::mutate(
    water_right_id_display = wr_id_display,
    pod_id_display = pod_id_display,
    pod_feature_id_display = pod_feature_id_display,
    holder_display = holder_display,
    source_display = source_display,
    county_display = county_display,
    pod_status_display = pod_status_display,
    wr_status_display = wr_status_display,
    wr_type_display = wr_type_display,
    face_afy = face_afy,
    face_value_display = face_value_display,
    face_value_source_display = face_value_source_display,
    swrcb_status_group = swrcb_status_group,
    blm_include_reason_display = blm_include_reason_display,
    swrcb_screening_source_display = swrcb_screening_source_display,
    spatial_blm_match_display = spatial_blm_match_display,
    blm_relevance_note = blm_relevance_note,
    swrcb_wr_id_norm = swrcb_wr_id_norm,
    swrcb_2026_blm_wr_list = swrcb_2026_blm_wr_list,
    swrcb_2026_blm_wr_list_display = swrcb_2026_blm_wr_list_display,
    face_afy_2026 = face_afy_2026,
    face_value_2026_raw_values = face_value_2026_raw_values,
    face_value_2026_numeric_values = face_value_2026_numeric_values,
    face_value_2026_spreadsheet_row_count =
      face_value_2026_spreadsheet_row_count,
    face_value_2026_distinct_count = face_value_2026_distinct_count,
    face_value_2026_conflict = face_value_2026_conflict,
    face_value_2026_automatic_resolution =
      face_value_2026_automatic_resolution,
    face_value_2026_reviewed_override_used =
      face_value_2026_reviewed_override_used,
    face_value_2026_resolution = face_value_2026_resolution,
    face_value_2026_source_reference = face_value_2026_source_reference,

    swrcb_fill_col = dplyr::case_when(
      swrcb_status_group == "Active / recognized"   ~ "#33A02C",
      swrcb_status_group == "Pending"               ~ "#FF7F00",
      swrcb_status_group == "Inactive / cancelled"  ~ "#E31A1C",
      TRUE                                          ~ "#8C8C8C"
    ),
    
    swrcb_stroke_col = dplyr::case_when(
      swrcb_status_group == "Active / recognized"   ~ "#1B7837",
      swrcb_status_group == "Pending"               ~ "#B35806",
      swrcb_status_group == "Inactive / cancelled"  ~ "#99000D",
      TRUE                                          ~ "#4D4D4D"
    ),
    
    ## Moderate face-value scaling driven by the canonical selected AFY value.
    ## For official-list rights, that value comes from the SWRCB-provided 2026
    ## correction export whenever an unambiguous value is available.  Zeros
    ## stay small; positive rights grow by bin; very large rights remain capped.
    swrcb_radius = dplyr::case_when(
      is.na(face_afy) | face_afy <= 0       ~ 2.8,
      face_afy > 0      & face_afy <= 0.1   ~ 3.6,
      face_afy > 0.1    & face_afy <= 1     ~ 4.2,
      face_afy > 1      & face_afy <= 10    ~ 5.0,
      face_afy > 10     & face_afy <= 100   ~ 5.9,
      face_afy > 100    & face_afy <= 1000  ~ 6.9,
      face_afy > 1000   & face_afy <= 10000 ~ 7.9,
      face_afy > 10000                      ~ 9.0,
      TRUE                                  ~ 2.8
    ),
    
    hover_text = paste0(
      "WR: ", dplyr::coalesce(water_right_id_display, "Not available"),
      "\nHolder: ", dplyr::coalesce(holder_display, "Not available"),
      "\nFace Value: ", face_value_display,
      "\nSource: ", dplyr::coalesce(swrcb_screening_source_display, "Not available")
    ),

    ## Browser-template popup fields.  Avoid carrying a full repeated
    ## popup_html string for each SWRCB POD/WR record; leaflet_layer_helpers.r
    ## builds the same popup layout on click from these compact values.
    pt_swrcb_layer_id = paste0("swrcb_pod_wr_", dplyr::row_number()),
    swrcbpop_water_right = dplyr::coalesce(water_right_id_display, "Not available"),
    swrcbpop_pod_feature_id = dplyr::coalesce(pod_feature_id_display, "Not available"),
    swrcbpop_pod_id = dplyr::coalesce(pod_id_display, "Not available"),
    swrcbpop_holder = dplyr::coalesce(holder_display, "Not available"),
    swrcbpop_face_value = face_value_display,
    swrcbpop_face_value_source = face_value_source_display,
    swrcbpop_screening_source = dplyr::coalesce(swrcb_screening_source_display, "Not available"),
    swrcbpop_blm_match_detail = dplyr::coalesce(blm_include_reason_display, "Not available"),
    swrcbpop_spatial_blm_match = dplyr::coalesce(spatial_blm_match_display, "Not available"),
    swrcbpop_interpretation = dplyr::coalesce(blm_relevance_note, "Not available"),
    swrcbpop_2026_blm_wr_list = swrcb_2026_blm_wr_list_display,
    swrcbpop_wr_status = dplyr::coalesce(wr_status_display, "Not available"),
    swrcbpop_wr_type = dplyr::coalesce(wr_type_display, "Not available"),
    swrcbpop_pod_status = dplyr::coalesce(pod_status_display, "Not available"),
    swrcbpop_source = dplyr::coalesce(source_display, "Not available"),
    swrcbpop_county = dplyr::coalesce(county_display, "Not available")
  ) |>
  dplyr::select(
    dplyr::any_of(c(
      "water_right_id_display",
      "pod_id_display",
      "pod_feature_id_display",
      "holder_display",
      "face_afy",
      "face_value_display",
      "face_value_source_display",
      "swrcb_status_group",
      "swrcb_wr_id_norm",
      "swrcb_2026_blm_wr_list",
      "swrcb_2026_blm_wr_list_display",
      "face_afy_2026",
      "face_value_2026_raw_values",
      "face_value_2026_numeric_values",
      "face_value_2026_spreadsheet_row_count",
      "face_value_2026_distinct_count",
      "face_value_2026_conflict",
      "face_value_2026_automatic_resolution",
      "face_value_2026_reviewed_override_used",
      "face_value_2026_resolution",
      "face_value_2026_source_reference",
      "wr_status_display",
      "wr_type_display",
      "pod_status_display",
      "blm_include_reason_display",
      "swrcb_screening_source_display",
      "spatial_blm_match_display",
      "blm_relevance_note",
      "source_display",
      "county_display",
      "hover_text",
      "pt_swrcb_layer_id",
      "swrcbpop_water_right",
      "swrcbpop_pod_feature_id",
      "swrcbpop_pod_id",
      "swrcbpop_holder",
      "swrcbpop_face_value",
      "swrcbpop_face_value_source",
      "swrcbpop_screening_source",
      "swrcbpop_blm_match_detail",
      "swrcbpop_spatial_blm_match",
      "swrcbpop_interpretation",
      "swrcbpop_2026_blm_wr_list",
      "swrcbpop_wr_status",
      "swrcbpop_wr_type",
      "swrcbpop_pod_status",
      "swrcbpop_source",
      "swrcbpop_county",
      "swrcb_fill_col",
      "swrcb_stroke_col",
      "swrcb_radius",
      "geometry"
    ))
  )

message("SWRCB / CalWATRS BLM POD map cache summary:")
print(
  swrcb_pod_wr_blm_map |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::count(swrcb_status_group, swrcb_fill_col, name = "n") |>
    dplyr::arrange(swrcb_status_group),
  n = Inf
)

message("SWRCB 2026 BLM WR lookup match summary:")
swrcb_2026_match_summary <- swrcb_pod_wr_blm_map |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  dplyr::summarize(
    pod_rows = dplyr::n(),
    pod_rows_with_wr_id = sum(!is.na(.data$swrcb_wr_id_norm)),
    pod_rows_in_swrcb_2026_blm_wr_list = sum(.data$swrcb_2026_blm_wr_list, na.rm = TRUE),
    unique_pod_wr_ids = dplyr::n_distinct(.data$swrcb_wr_id_norm, na.rm = TRUE),
    unique_matched_pod_wr_ids = dplyr::n_distinct(
      .data$swrcb_wr_id_norm[.data$swrcb_2026_blm_wr_list],
      na.rm = TRUE
    ),
    csv_unique_wr_ids = length(swrcb_2026_blm_wr_ids),
    csv_unique_wr_ids_matched_to_pods = length(intersect(
      swrcb_2026_blm_wr_ids,
      stats::na.omit(.data$swrcb_wr_id_norm)
    )),
    csv_unique_wr_ids_not_matched_to_pods = length(setdiff(
      swrcb_2026_blm_wr_ids,
      stats::na.omit(.data$swrcb_wr_id_norm)
    ))
  )

print(swrcb_2026_match_summary, width = 1200)

if (exists("WRITE_QA") && isTRUE(WRITE_QA)) {

  swrcb_2026_qa_path <- file.path(
    DIR$qa,
    paste0("swrcb_2026_blm_wr_lookup_match_summary_", RUN_TS, ".csv")
  )

  readr::write_csv(
    swrcb_2026_match_summary,
    swrcb_2026_qa_path
  )

  swrcb_face_value_summary_path <- file.path(
    DIR$qa,
    paste0("swrcb_2026_blm_face_value_overlay_summary_", RUN_TS, ".csv")
  )
  swrcb_face_value_reconciliation_path <- file.path(
    DIR$qa,
    paste0("swrcb_2026_blm_face_value_reconciliation_", RUN_TS, ".csv")
  )
  swrcb_face_value_conflict_path <- file.path(
    DIR$qa,
    paste0("swrcb_2026_blm_face_value_duplicate_conflicts_", RUN_TS, ".csv")
  )
  swrcb_face_value_conflict_resolution_path <- file.path(
    DIR$qa,
    paste0(
      "swrcb_2026_blm_face_value_conflict_resolutions_",
      RUN_TS,
      ".csv"
    )
  )

  readr::write_csv(
    swrcb_face_value_overlay_summary,
    swrcb_face_value_summary_path
  )
  readr::write_csv(
    swrcb_face_value_overlay_qa,
    swrcb_face_value_reconciliation_path
  )
  readr::write_csv(
    swrcb_2026_lookup_result$duplicate_conflicts,
    swrcb_face_value_conflict_path
  )
  readr::write_csv(
    swrcb_face_value_conflict_resolution_qa,
    swrcb_face_value_conflict_resolution_path
  )

  message("Saved SWRCB 2026 BLM WR lookup QA CSV: ", swrcb_2026_qa_path)
  message("Saved SWRCB face-value overlay summary: ", swrcb_face_value_summary_path)
  message("Saved SWRCB face-value reconciliation: ", swrcb_face_value_reconciliation_path)
  message("Saved SWRCB duplicate-conflict QA: ", swrcb_face_value_conflict_path)
  message(
    "Saved SWRCB conflicting-ID resolution QA: ",
    swrcb_face_value_conflict_resolution_path
  )
}


# ---- 8.10w BLM groundwater-well inventory ---------------------------------
##
## PURPOSE:
##   Prepare the two small BLM groundwater-well inventory layers normalized by
##   02_preprocess/18_blm_groundwater_well_inventory.r:
##
##     1. BLM-drilled wells | NOC
##     2. GW wells | 2025 Mojave-BLM limited field check
##
## DESIGN:
##   - Keep the two sources as separate Local layers to preserve provenance.
##   - Join the optional current-BLM distance sidecar created by 63_.
##   - Keep browser display simple here; final-map helpers own clustering,
##     icons, hover, popup, and later legend/filter/label behavior.
##   - Missing source/distance files should not break a generic cache rebuild;
##     empty layers are written instead.

blm_gw_well_inventory_map <- blm_gw_well_inventory_combined |>
  clean_sf_for_leaflet()

if (!"record_uid" %in% names(blm_gw_well_inventory_map)) {
  blm_gw_well_inventory_map$record_uid <- character(nrow(blm_gw_well_inventory_map))
}

blm_gw_distance_path <- file.path(
  DIR$cache_last,
  "blm_gw_well_inventory_blm_distance_fields.csv"
)

if (file.exists(blm_gw_distance_path) && nrow(blm_gw_well_inventory_map) > 0) {
  message("Joining BLM GW well inventory BLM-distance sidecar: ", blm_gw_distance_path)

  blm_gw_distance <- readr::read_csv(
    blm_gw_distance_path,
    show_col_types = FALSE
  ) |>
    dplyr::select(
      dplyr::any_of(c(
        "record_uid",
        "on_blm_ca",
        "on_blm_ca_chr",
        "dist_to_blm_mi",
        "dist_to_blm_ft",
        "blm_distance_label",
        "blm_distance_bin",
        "blm_distance_run_time"
      ))
    ) |>
    dplyr::distinct(record_uid, .keep_all = TRUE)

  blm_gw_well_inventory_map <- blm_gw_well_inventory_map |>
    dplyr::left_join(blm_gw_distance, by = "record_uid")
} else {
  blm_gw_well_inventory_map$on_blm_ca <- NA
  blm_gw_well_inventory_map$on_blm_ca_chr <- NA_character_
  blm_gw_well_inventory_map$dist_to_blm_mi <- NA_real_
  blm_gw_well_inventory_map$dist_to_blm_ft <- NA_real_
  blm_gw_well_inventory_map$blm_distance_label <- NA_character_
  blm_gw_well_inventory_map$blm_distance_bin <- NA_character_
  blm_gw_well_inventory_map$blm_distance_run_time <- NA_character_
}

## Add compact, browser-friendly display fields used by 048c.  Keep the raw,
## normalized popup_html from 18_ so source-specific long notes remain intact.
if (nrow(blm_gw_well_inventory_map) > 0) {
  blm_gw_well_inventory_map <- blm_gw_well_inventory_map |>
    dplyr::mutate(
      layer_key = dplyr::case_when(
        .data$source_key == "noc_blm_drilled" ~ "noc",
        .data$source_key == "mojave_2025_blm_field_check" ~ "mojave_2025",
        TRUE ~ "other"
      ),
      well_hover_text = dplyr::if_else(
        !is.na(.data$hover_line2) & .data$hover_line2 != "",
        paste0(.data$hover_line1, "\n", .data$hover_line2),
        .data$hover_line1
      ),
      blm_distance_popup = dplyr::case_when(
        !is.na(.data$blm_distance_label) ~ .data$blm_distance_label,
        .data$on_blm_ca == TRUE ~ "on BLM",
        !is.na(.data$dist_to_blm_mi) ~ paste0(
          format(round(.data$dist_to_blm_mi, 2), trim = TRUE, scientific = FALSE),
          " mi"
        ),
        TRUE ~ NA_character_
      )
    )
}

blm_noc_drilled_wells_map <- blm_gw_well_inventory_map |>
  dplyr::filter(.data$source_key == "noc_blm_drilled")

mojave_2025_gw_well_inventory_map <- blm_gw_well_inventory_map |>
  dplyr::filter(.data$source_key == "mojave_2025_blm_field_check")

message("BLM groundwater-well inventory Local cache summary:")
print(
  blm_gw_well_inventory_map |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::count(source_key, source_display, name = "n") |>
    dplyr::arrange(source_key),
  n = Inf
)

# ---- 8.10s Springs ----------------------------------------------------------
##
## PURPOSE:
##   Build a lightweight map-ready Springs layer from the combined NHD +
##   2015-16 Mojave Desert Spring Survey processed Springs RDS.
##
## DESIGN:
##   - No deduplication.  Different source records may share coordinates.
##   - Preserve stable source keys for future source filters.
##   - Preserve neutral user-facing source display text.
##   - Join the optional BLM-distance sidecar if it exists; if it does not,
##     keep distance fields as NA so build_final_map_only() remains usable.
##   - Do not expose an elevation filter yet.  NHD currently has no usable local
##     elevation attribute, so elevation is carried for future source assimilation
##     and popup context only.

springs_map <- springs |>
  clean_sf_for_leaflet()

spr_chr_col <- function(x, nm) {
  if (nm %in% names(x)) {
    out <- as.character(x[[nm]])
  } else {
    out <- rep(NA_character_, nrow(x))
  }

  out <- trimws(out)
  out[out == ""] <- NA_character_
  out
}

spr_num_col <- function(x, nm) {
  if (nm %in% names(x)) {
    suppressWarnings(as.numeric(gsub(",", "", as.character(x[[nm]]))))
  } else {
    rep(NA_real_, nrow(x))
  }
}

spr_name <- spr_chr_col(springs_map, "spring_name_display")
spr_source <- spr_chr_col(springs_map, "spring_source")
spr_source_key_raw <- spr_chr_col(springs_map, "spring_source_key")
spr_source_display_raw <- spr_chr_col(springs_map, "spring_source_display")
spr_source_short_raw <- spr_chr_col(springs_map, "spring_source_short")
spr_gnis <- spr_chr_col(springs_map, "gnis_id")
spr_elev_ft <- spr_num_col(springs_map, "elevation_ft")
spr_elev <- spr_chr_col(springs_map, "elevation_display")
spr_report_url_raw <- spr_chr_col(springs_map, "source_report_url")
spr_report_label_raw <- spr_chr_col(springs_map, "source_report_label")

spr_source_combo <- tolower(paste(spr_source_key_raw, spr_source_display_raw, spr_source))
spr_is_nhd <- spr_source_key_raw == "nhd" | grepl("\\bnhd\\b", spr_source_combo)
spr_is_mojave_survey <- spr_source_key_raw %in% c("survey_2015_16", "mojave_2015_16") |
  grepl("zdon|2015.?16|2015|2016|mojave", spr_source_combo, ignore.case = TRUE)
spr_is_mojave_survey <- spr_is_mojave_survey & !spr_is_nhd

spr_source_key <- dplyr::case_when(
  spr_is_nhd           ~ "nhd",
  spr_is_mojave_survey ~ "survey_2015_16",
  !is.na(spr_source_key_raw) ~ spr_source_key_raw,
  TRUE                 ~ "other"
)

spr_source_display <- dplyr::case_when(
  spr_source_key == "nhd" ~ "NHD",
  spr_source_key == "survey_2015_16" ~ "2015–16 Mojave survey",
  !is.na(spr_source_display_raw) ~ spr_source_display_raw,
  !is.na(spr_source) ~ spr_source,
  TRUE ~ "Unknown source"
)

spr_source_short <- dplyr::case_when(
  spr_source_key == "nhd" ~ "NHD",
  spr_source_key == "survey_2015_16" ~ "2015–16 survey",
  !is.na(spr_source_short_raw) ~ spr_source_short_raw,
  TRUE ~ spr_source_display
)

spr_report_url <- dplyr::case_when(
  !is.na(spr_report_url_raw) ~ spr_report_url_raw,
  spr_source_key == "survey_2015_16" ~ "https://www.scienceforconservation.org/products/mojave-desert-spring-survey",
  TRUE ~ NA_character_
)

spr_report_label <- dplyr::case_when(
  !is.na(spr_report_label_raw) ~ spr_report_label_raw,
  spr_source_key == "survey_2015_16" ~ "Survey report",
  TRUE ~ NA_character_
)

spr_has_real_name <- !is.na(spr_name) &
  !grepl("^Unnamed", spr_name, ignore.case = TRUE)

spr_google_query <- ifelse(
  spr_has_real_name,
  paste(spr_name, "California"),
  NA_character_
)

spr_google_url <- ifelse(
  !is.na(spr_google_query),
  paste0(
    "https://www.google.com/search?q=",
    utils::URLencode(spr_google_query, reserved = TRUE)
  ),
  NA_character_
)

springs_map <- springs_map |>
  dplyr::mutate(
    spring_name_display = dplyr::coalesce(spr_name, "Unnamed spring"),
    spring_source = dplyr::coalesce(spr_source_display, "Unknown source"),
    spring_source_key = spr_source_key,
    spring_source_display = dplyr::coalesce(spr_source_display, "Unknown source"),
    spring_source_short = dplyr::coalesce(spr_source_short, spring_source_display),
    gnis_id = spr_gnis,
    elevation_ft = spr_elev_ft,
    elevation_display = spr_elev,
    elevation_known = !is.na(spr_elev_ft),
    source_report_url = spr_report_url,
    source_report_label = spr_report_label,

    ## Source-specific display fields used by the browser-managed Springs layer.
    spring_symbol_group = dplyr::case_when(
      spring_source_key == "survey_2015_16" ~ "survey_2015_16",
      spring_source_key == "nhd"            ~ "NHD",
      TRUE                                  ~ "Other"
    ),

    spring_radius = dplyr::case_when(
      spring_symbol_group == "survey_2015_16" ~ 6.0,
      spring_symbol_group == "NHD"            ~ 3.2,
      TRUE                                    ~ 3.5
    ),

    spring_fill_col = dplyr::case_when(
      spring_symbol_group == "survey_2015_16" ~ "#FFFFFF",
      spring_symbol_group == "NHD"            ~ "#2B6CB0",
      TRUE                                    ~ "#777777"
    ),

    spring_fill_opacity = dplyr::case_when(
      spring_symbol_group == "survey_2015_16" ~ 0.0,
      spring_symbol_group == "NHD"            ~ 0.80,
      TRUE                                    ~ 0.70
    ),

    spring_stroke_col = dplyr::case_when(
      spring_symbol_group == "survey_2015_16" ~ "#E6550D",
      spring_symbol_group == "NHD"            ~ "#FFFFFF",
      TRUE                                    ~ "#333333"
    ),

    spring_stroke_weight = dplyr::case_when(
      spring_symbol_group == "survey_2015_16" ~ 2.0,
      spring_symbol_group == "NHD"            ~ 0.7,
      TRUE                                    ~ 1.0
    ),

    hover_text = paste0(
      spring_name_display,
      "\nSource: ",
      spring_source
    ),

    google_search_url = spr_google_url,
    survey_report_url = dplyr::if_else(
      spring_symbol_group == "survey_2015_16",
      dplyr::coalesce(source_report_url, "https://www.scienceforconservation.org/products/mojave-desert-spring-survey"),
      NA_character_
    ),

    popup_html = paste0(
      "<b>Spring:</b> ",
      htmltools::htmlEscape(spring_name_display),
      "<br>",
      "<b>Source:</b> ",
      htmltools::htmlEscape(spring_source),
      dplyr::if_else(
        !is.na(elevation_display),
        paste0(
          "<br><b>Elevation:</b> ",
          htmltools::htmlEscape(elevation_display)
        ),
        ""
      ),
      dplyr::if_else(
        !is.na(gnis_id),
        paste0(
          "<br><b>GNIS ID:</b> ",
          htmltools::htmlEscape(gnis_id)
        ),
        ""
      ),
      dplyr::if_else(
        !is.na(survey_report_url),
        paste0(
          "<br><a href='",
          htmltools::htmlEscape(survey_report_url),
          "' target='_blank'>Survey report</a>"
        ),
        ""
      ),
      dplyr::if_else(
        !is.na(google_search_url),
        paste0(
          "<br><a href='",
          htmltools::htmlEscape(google_search_url),
          "' target='_blank'>Google search</a>"
        ),
        ""
      )
    )
  )

## Optional BLM-distance sidecar.  047b creates the updater and output; later
## legend/filter patches can use these fields without adding spatial work to
## build_final_map_only().  Missing sidecar is not an error because the current
## visible Springs layer does not require it yet.
springs_distance_path <- file.path(
  DIR$cache_last,
  "springs_blm_distance_fields.csv"
)

if (file.exists(springs_distance_path) && "spring_id" %in% names(springs_map)) {
  message("Joining Springs BLM distance sidecar: ", springs_distance_path)

  springs_distance <- readr::read_csv(
    springs_distance_path,
    show_col_types = FALSE
  ) |>
    dplyr::select(
      dplyr::any_of(c(
        "spring_id",
        "on_blm_ca",
        "dist_to_blm_mi",
        "dist_to_blm_ft",
        "blm_distance_run_time",
        "blm_source_modified"
      ))
    ) |>
    dplyr::distinct(spring_id, .keep_all = TRUE)

  springs_map <- springs_map |>
    dplyr::left_join(springs_distance, by = "spring_id")
} else {
  springs_map$on_blm_ca <- NA_character_
  springs_map$dist_to_blm_mi <- NA_real_
  springs_map$dist_to_blm_ft <- NA_real_
  springs_map$blm_distance_run_time <- NA_character_
  springs_map$blm_source_modified <- NA_character_
}

springs_map <- springs_map |>
  dplyr::select(
    dplyr::any_of(c(
      "spring_id",
      "spring_name_display",
      "spring_source",
      "spring_source_key",
      "spring_source_display",
      "spring_source_short",
      "spring_symbol_group",
      "gnis_id",
      "elevation_ft",
      "elevation_display",
      "elevation_known",
      "hover_text",
      "popup_html",
      "spring_radius",
      "spring_fill_col",
      "spring_fill_opacity",
      "spring_stroke_col",
      "spring_stroke_weight",
      "source_report_url",
      "source_report_label",
      "survey_report_url",
      "on_blm_ca",
      "dist_to_blm_mi",
      "dist_to_blm_ft",
      "blm_distance_run_time",
      "blm_source_modified",
      "geometry"
    ))
  )

message("Springs map cache summary:")
print(
  springs_map |>
    sf::st_drop_geometry() |>
    tibble::as_tibble() |>
    dplyr::count(spring_source_key, spring_source_display, spring_symbol_group, name = "n") |>
    dplyr::arrange(spring_source_key),
  n = Inf
)
