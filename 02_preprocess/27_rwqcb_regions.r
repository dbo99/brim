# ==== 27_rwqcb_regions.r =====================================================
##
## PURPOSE:
##   Download the current RWQCB regional boundary layer from the State Water
##   Board public FeatureServer and save both:
##
##     1. an auditable local shapefile copy in 01_raw_data/waterboards/
##     2. a WGS84 processed RDS for the PT2 core-map cache builder
##
## WHY THIS EXISTS:
##   RWQCB regions are likely to be used often enough that they should be a
##   cached local Reference layer rather than only an External Layers option.
##
## UPDATE WORKFLOW:
##   Run from run_build_map.r with:
##
##     refresh_rwqcb_regions_and_map()
##
##   That refreshes the source copy, rebuilds the core cache, and rebuilds the
##   final HTML map.

# ==== 1. Load configuration ==================================================

source("00_config/config_paths.r")
source("00_config/config_source_files.r")

# ==== 2. Load packages =======================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

# ==== 3. Download current FeatureServer layer ================================

message("Reading RWQCB regions from State Water Board FeatureServer...")
message("  ", SRC$rwqcb_regions_query_url)

rwqcb <- sf::st_read(
  SRC$rwqcb_regions_query_url,
  quiet = FALSE
)

if (!inherits(rwqcb, "sf") || nrow(rwqcb) == 0) {
  stop("RWQCB FeatureServer query returned no spatial features.")
}

# ==== 4. Basic validation and WGS84 output object ============================

required_cols <- c("RB", "RB_NAME")
missing_cols <- setdiff(required_cols, names(rwqcb))

if (length(missing_cols) > 0) {
  stop(
    "RWQCB FeatureServer layer is missing expected field(s): ",
    paste(missing_cols, collapse = ", "),
    ". Review the service schema before updating the local cache."
  )
}

rwqcb_wgs84 <- rwqcb |>
  sf::st_transform(4326) |>
  dplyr::arrange(suppressWarnings(as.integer(as.character(.data$RB))))

# ==== 5. Write auditable shapefile copy ======================================

shp_path <- SRC$rwqcb_regions_shp
shp_dir <- dirname(shp_path)

if (!dir.exists(shp_dir)) {
  dir.create(shp_dir, recursive = TRUE, showWarnings = FALSE)
}

## Shapefiles are multi-file datasets.  Remove old sidecar files first so a
## changed schema does not leave stale files behind.
shp_stub <- tools::file_path_sans_ext(shp_path)
unlink(
  paste0(shp_stub, c(".shp", ".shx", ".dbf", ".prj", ".cpg")),
  force = TRUE
)

sf::st_write(
  rwqcb_wgs84,
  shp_path,
  delete_layer = TRUE,
  quiet = TRUE
)

message("Saved RWQCB shapefile copy: ", shp_path)

# ==== 6. Write processed RDS for PT2 cache builder ===========================

rds_path <- file.path(DIR$rds, "rwqcb_regions_wgs84.rds")

saveRDS(rwqcb_wgs84, rds_path)

message("Saved RWQCB processed RDS: ", rds_path)
message("RWQCB features: ", nrow(rwqcb_wgs84))
rwqcb_wgs84 |>
  sf::st_drop_geometry() |>
  dplyr::select(RB, RB_NAME) |>
  tibble::as_tibble() |>
  print(n = Inf)
