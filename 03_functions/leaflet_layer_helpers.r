# ==== leaflet_layer_helpers.r ===============================================
##
## PURPOSE:
##   Source local/static Leaflet layer helper modules.
##
## DESIGN:
##   This file used to contain all Local-layer drawing code directly and had
##   grown past 10,000 lines.  It is now a lightweight compatibility wrapper so
##   existing build scripts can keep sourcing:
##
##     source("03_functions/leaflet_layer_helpers.r")
##
##   The extracted modules preserve the original function names and source order.
##   This patch is intended to be structural only; no map behavior should change.
##

# Shared popup/display helpers are required by several Local-layer modules
# during final-map-only builds as well as full core-cache rebuilds.  Keep this
# dependency centralized here rather than duplicating fallback helper functions
# inside downstream layer modules.
source("03_functions/bulletin118_data_helpers.r")
source("03_functions/popup_helpers.r")
source("00_config/config_local_reference_interactions.r")
source("03_functions/polygon_generalization_helpers.r")
LOCAL_REFERENCE_INTERACTION_REGISTRY <-
  pt_apply_polygon_generalization_disclosures(
    LOCAL_REFERENCE_INTERACTION_REGISTRY
  )
source("03_functions/local_reference_interaction_helpers.r")
pt_validate_local_reference_config()

source("03_functions/leaflet_layer_local_core_helpers.r")
source("03_functions/leaflet_layer_local_polygon_helpers.r")
source("03_functions/leaflet_layer_local_cnrfc_helpers.r")
source("03_functions/leaflet_layer_local_usgs_helpers.r")
source("03_functions/leaflet_layer_local_swrcb_helpers.r")
source("03_functions/leaflet_layer_local_well_spring_helpers.r")
source("03_functions/leaflet_layer_local_reference_helpers.r")
