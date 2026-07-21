# ==== config_labels.r ========================================================
##
## PURPOSE:
##   Central label settings for PortaTreasure2.
##
## WHY THIS FILE EXISTS:
##   Label behavior is one of the easiest parts of a web map to overtune or
##   accidentally break. This file keeps label switches, zoom thresholds, and
##   label-field choices in one obvious place.
##
## EDIT THIS FILE WHEN:
##   - labels appear too early or too late while zooming
##   - a label layer is too cluttered
##   - project-area labels should use a different field
##   - a future layer such as ACEC, wilderness, W&S rivers, or conveyance
##     needs a label threshold
##

# ==== 1. Master label switches ===============================================

LABELS <- list(
  
  # ---- Global label behavior ------------------------------------------------
  enabled_by_default = TRUE,
  
  ## Later, the map will include a custom global label toggle.
  ## When TRUE, labels can appear if their parent layer is visible and the
  ## zoom threshold is met.
  use_global_toggle = TRUE,
  
  # ---- Label text styling ---------------------------------------------------
  default_text_size_px = 11,
  default_text_color   = "#333333",
  default_halo_color   = "#FFFFFF",
  default_halo_weight  = 2,
  
  # ---- Project-area label field ---------------------------------------------
  ## Project-area shapefiles may vary by project.
  ## Change this to whichever field should be used for project-area labels.
  ##
  ## Examples:
  ##   "project_name"
  ##   "NAME"
  ##   "Project"
  ##   "Title"
  project_area_label_field = "project_name"
)

# ==== 2. Label zoom thresholds ===============================================
##
## min_zoom = first zoom level where labels should appear.
## max_zoom = optional upper cutoff; Inf means no upper cutoff.
##
## These values are intentionally easy to edit.
##
## Suggested starting points:
##   statewide / regional polygon labels: lower zooms
##   small basin / gage labels: higher zooms
##   dense point labels: high zooms only

LABEL_ZOOM <- tibble::tribble(
  ~label_id,              ~parent_group,           ~label_group,                    ~min_zoom, ~max_zoom,
  
  # ---- HUC polygon labels ---------------------------------------------------
  "huc2",                 "HUC2",                  "Labels: HUC2",                  4.0,       Inf,
  "huc4",                 "HUC4",                  "Labels: HUC4",                  5.0,       Inf,
  "huc6",                 "HUC6",                  "Labels: HUC6",                  6.0,       Inf,
  "huc8",                 "HUC8",                  "Labels: HUC8",                  7.0,       Inf,
  "huc10",                "HUC10",                 "Labels: HUC10",                 8.5,       Inf,
  "huc12",                "HUC12",                 "Labels: HUC12",                10.0,       Inf,
  
  # ---- Other polygon labels -------------------------------------------------
  "gw_bull118",           "GW – Bull. 118",        "Labels: GW – Bull. 118",        7.0,       Inf,
  "county",               "Counties",              "Labels: Counties",              6.0,       Inf,
  "project_areas",        "Project area(s)",       "Labels: Project area(s)",       7.0,       Inf,
  
  # ---- Point labels ---------------------------------------------------------
  "cnrfc_stream",         "CNRFC Stream Gages",    "Labels: CNRFC Stream Gages",    10.5,      Inf,
  "cnrfc_precip",         "CNRFC Precip Gages",    "Labels: CNRFC Precip Gages",    10.5,      Inf,
  "usgs_streamgages",     "USGS Streamgages",      "Labels: USGS Streamgages",      10.5,      Inf,
  
  # ---- Future polygon / line labels ----------------------------------------
  "cnrfc_basins",         "CNRFC Product Availability", "Labels: CNRFC Product Availability", 7.0,       Inf,
  "acec",                 "ACECs",                 "Labels: ACECs",                  8.0,       Inf,
  "wilderness",           "Wilderness",            "Labels: Wilderness",             8.0,       Inf,
  "wild_scenic_corridor", "Wild & Scenic Rivers",  "Labels: Wild & Scenic Rivers",   8.5,       Inf,
  "wild_scenic_reaches",  "W&S River Reaches",     "Labels: W&S River Reaches",     10.0,       Inf,
  
  # Water districts are numerous, so labels should only appear at close zooms.
  # They are still added through clustered label-only markers for browser performance.
  "water_districts",      "Water Districts",       "Labels: Water Districts",       12.0,       Inf,
  
  "river_miles",          "River Miles",           "Labels: River Miles",           11.0,       Inf,
  "conservation_lands",   "Conservation Lands",    "Labels: Conservation Lands",     8.0,       Inf,
  "field_office_outer", "BLM Field Office (outer)", "Labels: BLM Field Office (outer)", 6.0, Inf
)

# ==== 3. Label layer inclusion switches ======================================
##
## These control which label layers are built/added when source layers exist.
## USGS wells are intentionally excluded because the layer is too dense.

LABEL_INCLUDE <- list(
  
  huc2 = TRUE,
  huc4 = TRUE,
  huc6 = TRUE,
  huc8 = TRUE,
  huc10 = TRUE,
  huc12 = TRUE,
  
  gw_bull118 = TRUE,
  county = TRUE,
  project_areas = TRUE,
  
  cnrfc_basins = TRUE,
  field_office_outer = TRUE,
  
  cnrfc_stream = TRUE,
  cnrfc_precip = TRUE,
  usgs_streamgages = FALSE,
  
  # Explicitly disabled.
  usgs_wells = FALSE,
  blm_offices = FALSE,
  
  # Future layers. These can stay TRUE; label-building functions will skip them
  # until the corresponding cached layers exist.
  cnrfc_basins = TRUE,
  field_office_outer = TRUE,
  acec = TRUE,
  wilderness = TRUE,
  wild_scenic_corridor = TRUE,
  wild_scenic_reaches = TRUE,
  
  ## Water-district labels are intentionally optional and high-zoom only.
  ## They help users identify districts where overlapping polygons make
  ## hover/click access unreliable.
  water_districts = TRUE,
  
  river_miles = TRUE,
  conservation_lands = TRUE
)


# ==== 4. Explicit label fields ===============================================
##
## PURPOSE:
##   Define the exact attribute field used for each label layer.
##
## IMPORTANT:
##   These are intentionally explicit. The label-cache script should not guess
##   from possible field names. If a layer's label field changes, update it here.

LABEL_FIELDS <- list(
  
  # ---- Current polygon layers -----------------------------------------------
  huc2  = "huc2_name",
  huc4  = "huc4_name",
  huc6  = "huc6_name",
  huc8  = "huc8_name",
  huc10 = "huc10_name",
  huc12 = "huc12_name",
  
  gw_bull118 = "subbasin_name",
  county     = "county_name",
  
  cnrfc_basins       = "Basin",
  field_office_outer = "ADMU_NAME",
  
  project_areas = "project_name",
  
  # ---- Manifest-driven reference-layer labels -------------------------------
  ##
  ## These fields must match the cached reference layer fields exactly.
  fedwilderness = "NLCS_NAME",
  acec          = "ACEC_NAME",
  allotments    = "ALLOT_NAME",
  
  ## Water districts are cached with a cleaned display field created during
  ## core-cache building. This avoids labeling blanks/NA values and keeps the
  ## label text synchronized with hover/popup text.
  water_districts = "agency_display",
  
  # ---- Current point layers -------------------------------------------------
  cnrfc_stream     = "nwsid",
  cnrfc_precip     = "nwsid",
  usgs_streamgages = "site_no",
  
  # ---- Future / currently inactive layers ----------------------------------
  wilderness           = "NAME",
  wild_scenic_corridor = "NAME",
  wild_scenic_reaches  = "NAME",
  water_conveyance     = "NAME",
  river_miles          = "RIVER_NAME",
  conservation_lands   = "NAME"
)


# ==== 5. Inline label-pair registry ==========================================
##
## PURPOSE:
##   Define which visible Local-layer rows receive an inline "lbl" checkbox and
##   which hidden Labels companion row that checkbox controls.
##
## WHY THIS IS SEPARATE FROM LABEL_ZOOM:
##   LABEL_ZOOM controls when label features are visible once their label group
##   is active.  INLINE_LABEL_PAIRS controls the Local Layers panel UI itself:
##   source row + hidden companion Labels row + inline lbl checkbox.
##
## MAINTENANCE NOTES:
##   - main_name must match the user-facing source layer row after Local panel
##     prefixes are stripped.
##   - label_name must match the user-facing hidden Labels companion row after
##     the "Labels –" prefix is stripped.
##   - Do not add USGS monitoring wells here unless a real companion label layer
##     exists and the layer is intentionally made labelable.
##   - Browser-managed dense layers such as SWRCB/NOC/Mojave/Springs can use
##     this UI registry even when their label features are managed by custom
##     client-side controllers rather than LABEL_ZOOM.

INLINE_LABEL_PAIRS <- data.frame(
  main_name = c(
    "BLM Field Office Boundaries",
    "GW Basins, Bulletin 118",
    "Counties",
    "HUC2 – PRISM/BCMv8",
    "HUC4 – PRISM/BCMv8",
    "HUC6 – PRISM/BCMv8",
    "HUC8 – PRISM/BCMv8",
    "HUC10 – PRISM/BCMv8",
    "HUC12 – PRISM/BCMv8",
    "CNRFC Product Availability",
    "CNRFC weather station catalog",
    "CNRFC river/reservoir catalog",
    "USGS streamgages",
    "BLM-drilled wells | NOC",
    "GW wells | 2025 Mojave-BLM limited field check",
    "Springs",
    "Water rights POD | SWRCB 2026 BLM list",
    "Water rights POD | BRIM spatial BLM match",
    "Water rights POD | BRIM name/text BLM candidate",
    "CNRFC FNF Sha/Tri/west Sierra Basins",
    "Groundwater Sustainability Plan Areas",
    "Adjudicated Groundwater Basins",
    "ACECs",
    "Water Districts",
    "RWQCB Regions"
  ),
  label_name = c(
    "BLM Field Office (outer)",
    "GW – Bull. 118",
    "Counties",
    "HUC2",
    "HUC4",
    "HUC6",
    "HUC8",
    "HUC10",
    "HUC12",
    "CNRFC Product Availability",
    "CNRFC Precip Gages",
    "CNRFC Stream Gages",
    "USGS streamgages",
    "BLM-drilled wells | NOC",
    "GW wells | 2025 Mojave-BLM limited field check",
    "Springs",
    "Water rights POD | SWRCB 2026 BLM list",
    "Water rights POD | BRIM spatial BLM match",
    "Water rights POD | BRIM name/text BLM candidate",
    "CNRFC FNF Sha/Tri/west Sierra Basins",
    "Groundwater Sustainability Plan Areas",
    "Adjudicated Groundwater Basins",
    "ACECs",
    "Water Districts",
    "RWQCB Regions"
  ),
  stringsAsFactors = FALSE
)

# ==== 6. Console confirmation ================================================

message("PortaTreasure2 label configuration loaded.")
message("Configured label zoom thresholds: ", nrow(LABEL_ZOOM))
message("Configured inline lbl pairs: ", nrow(INLINE_LABEL_PAIRS))
