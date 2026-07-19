# ==== config_run_flags.r =====================================================
##
## PURPOSE:
##   Central switches controlling expensive processing, cache reuse, and export.
##
## DESIGN GOAL:
##   The main build script should not require editing low-level code just to
##   decide whether to rebuild geometries, refresh popups, or use cached layers.
##

RUN <- list(
  
  # ---- Preprocessing switches ----------------------------------------------
  ## TRUE only when raw source data changed or a preprocessing script changed.
  rebuild_preprocess = FALSE,
  
  # ---- Map-cache switches ---------------------------------------------------
  ## Heavy geometry work: CRS conversion, validation, simplification.
  rebuild_geom_cache = FALSE,
  
  ## Popup and label rebuilding.
  ## Set TRUE when popup wording, fields, links, or label logic changes.
  rebuild_popups = TRUE,
  rebuild_labels = TRUE,
  
  # ---- Build/export switches ------------------------------------------------
  build_map = TRUE,
  
  ## Timestamp outputs so old products are not overwritten.
  use_timestamped_outputs = TRUE,
  
  ## Make the Leaflet HTML standalone.
  ## TRUE is best for sharing, but can create larger files.
  self_contained_html = TRUE,
  
  ## Open the map automatically after saving.
  open_after_save = TRUE,
  
  # ---- Development/performance switches ------------------------------------
  ## Use simplified/approximate geometry math during development.
  ## Keep FALSE for final agency-shareable products.
  dev_fast_mode = FALSE,
  
  ## Cluster large point layers such as streamgages, precip gages, and wells.
  cluster_points = TRUE,
  
  ## Add HUC10 and HUC12 layers.
  show_huc10_12 = TRUE
)