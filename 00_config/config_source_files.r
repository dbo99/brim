# ==== config_source_files.r ==================================================
##
## PURPOSE:
##   Define explicit raw source filenames used by PortaTreasure2 preprocessing.
##
## WHY EXPLICIT FILENAMES?
##   BLM land-status datasets change over time. We do NOT want scripts to guess
##   the newest shapefile automatically, because that makes outputs harder to
##   audit later. Instead, the active source file is declared here.
##
## IMPORTANT:
##   This file assumes that 00_config/config_paths.r has already been sourced,
##   so the DIR list is available.

# ==== 1. Source file registry ================================================
##
## All active raw source files should be listed inside this single SRC list.
## Commas go between list items, but there should be no comma after the final
## item in the list.

SRC <- list(
  
  ## 11 layers added in bulk, mostly just reference layers (acecs, wilderness, GSPs, etc)
  
  reference_layers_manifest = file.path(
    DIR$root,
    "00_config",
    "reference_layers_manifest.csv"
  ),
  
  # ---- BLM / federal lands / BLM reference layers ---------------------------
  
  blm_fedlands = file.path(
    DIR$raw,
    "blm",
    "fedlands_blmca_undsslvd_calalb_13Aug2025.shp"
  ),
  
  blm_offices = file.path(
    DIR$raw,
    "blm",
    "blmcaoffices.csv"
  ),
  
  field_office_outer = file.path(
    DIR$raw,
    "blm",
    "fo_outer.shp"
  ),
  
  # ---- CNRFC source files ---------------------------------------------------
  
  cnrfc_stream_gages = file.path(
    DIR$raw,
    "cnrfc",
    "gage_data_river.txt"
  ),
  
  cnrfc_precip_gages = file.path(
    DIR$raw,
    "cnrfc",
    "gage_data_precip.txt"
  ),
  
  cnrfc_clip_buffer = file.path(
    DIR$raw,
    "cnrfc",
    "cnrfc_points_cabuffer.shp"
  ),
  
  cnrfc_basins = file.path(
    DIR$raw,
    "cnrfc",
    "CNRFCbasins_noHumboldt.shp"
  ),
  
  # ---- HUC source files ------------------------------------------------------
  
  huc2 = file.path(DIR$raw, "huc", "ca_huc2_calalb.shp"),
  huc4 = file.path(DIR$raw, "huc", "ca_huc4_calalb.shp"),
  huc6 = file.path(DIR$raw, "huc", "ca_huc6_calalb.shp"),
  huc8 = file.path(DIR$raw, "huc", "ca_huc8_calalb.shp"),
  huc10 = file.path(DIR$raw, "huc", "ca_huc10_calalb.shp"),
  huc12 = file.path(DIR$raw, "huc", "ca_huc12_calalb.shp"),
  
  # ---- calsim3 source files ------------------------------------------------------
  calsim3_arcs = file.path(DIR$raw,"calsim3","calsim3arcs.shp"),
  
  calsim3_nodes = file.path(
    DIR$raw,
    "calsim3",
    "calsim3nodes.shp"
  ),
  
  # ---- major canals source files ------------------------------------------------------
  
  major_conveyance = file.path(
    DIR$raw,
    "conveyance",
    "majorconveyance.shp"
  ),
  
  # ---- Groundwater / counties ----------------------------------------------
  
  bull118_gw = file.path(
    DIR$raw,
    "groundwater",
    "i08_B118_v6_2_calalb.shp"
  ),

  ## Attribute-only snapshot from DWR's final 2019 SGMA basin-prioritization
  ## table. The refresh script validates the authoritative 515-row table and
  ## writes this small tracked crosswalk; no DWR geometry is downloaded.
  bull118_sgma_2019_priority = file.path(
    DIR$root,
    "00_config",
    "bulletin118_sgma_2019_priority_crosswalk.csv"
  ),
  
  counties = file.path(
    DIR$raw,
    "counties",
    "CaCounties_nad83calalb.shp"
  ),
  # ---- Springs ---------------------------------------------------------------
  
  springs_nhd_ftype458 = file.path(
    DIR$raw,
    "springs",
    "NHDPoint_Ftype458_springs.shp"
  ),
  
  springs_zdon_2020 = file.path(
    DIR$raw,
    "springs",
    "zdonsprings2020sob.csv"
  ),
  
  # ---- New static water / Delta reference layers -----------------------------
  ##
  ## These are lightweight source shapefiles added to the core map as explicit
  ## cached layers rather than manifest-driven reference layers because they
  ## need custom popups, hovers, symbology, or TOC placement.
  
  cnrfc_fnf_delta = file.path(
    DIR$raw,
    "cnrfc",
    "FNF_NorCalDelta.shp"
  ),
  
  x2_km = file.path(
    DIR$raw,
    "misc_reference",
    "x2_km.shp"
  ),
  
  deltamapr_canals = file.path(
    DIR$raw,
    "conveyance",
    "WW_Canals.shp"
  ),
  
  water_districts = file.path(
    DIR$raw,
    "water_districts",
    "i03_WaterDistricts.shp"
  ),
  
  # ---- RWQCB regional boundaries -------------------------------------------
  ##
  ## These are downloaded from the State Water Board public FeatureServer by:
  ##
  ##   02_preprocess/27_rwqcb_regions.r
  ##
  ## The shapefile is kept as an auditable local copy.  The map build reads the
  ## processed WGS84 RDS created by that preprocessor.
  
  rwqcb_regions_query_url = paste0(
    "https://gispublic.waterboards.ca.gov/arcgis/rest/services/",
    "Administrative/Regional_Board_Boundaries/FeatureServer/1/query?",
    "where=1%3D1&outFields=*&returnGeometry=true&f=geojson"
  ),
  
  rwqcb_regions_shp = file.path(
    DIR$raw,
    "waterboards",
    "rwqcb_regional_board_boundaries.shp"
  ),
  
  # ---- SWRCB / CalWATRS BLM water-right correction overlay ------------------
  ##
  ## CSV provided directly to BLM-CA by SWRCB during the 2026 CalWATRS
  ## transition.  It serves two related purposes in the centralized POD cache:
  ##
  ##   1. authoritative membership for the SWRCB 2026 BLM water-right layer;
  ##   2. authoritative face-value (AFY) correction for matching right IDs.
  ##
  ## The public SWRCB/CalWATRS POD service remains the spatial backbone and the
  ## public WR attributes remain the fallback for IDs absent from this export.
  ## The CSV is not drawn as a separate map layer.

  swrcb_2026_blm_wr_csv = file.path(
    DIR$raw,
    "swrcb_water_rights",
    "All_your_water_right_7_1_2026_8_56_30.csv"
  ),

  # ---- Project areas --------------------------------------------------------
  ##
  ## This file is allowed to be missing for now.
  ## The project-area placeholder preprocessor will create an empty layer if
  ## no shapefile exists at this path.
  
  project_areas = file.path(
    DIR$raw,
    "project_areas",
    "project_areas.shp"
  )
)

# ==== 2. Optional quick source summary =======================================

message("PortaTreasure2 source-file registry loaded.")
message("Registered source files: ", length(SRC))
