# ==== 00_export_deltamapr_layers.R ====
# Purpose:
#   Export all internal sf layers from the deltamapr R package so they can be
#   opened in ArcGIS Pro, QGIS, or R.
#
# Recommendation:
#   Use the GeoPackage output first. ArcGIS Pro can open it, and it preserves
#   long field names better than ESRI shapefiles.
#
# Notes:
#   - ESRI shapefiles truncate field names to 10 characters.
#   - Shapefiles create multiple sidecar files per layer.
#   - GeoPackage keeps everything in one file with multiple layers.

# ==== 1: User settings ====

# Change this to your preferred folder.
OUTPUT_DIR <- "C:/Users/doconnor/Downloads/deltamapr_export"

# Export CRS:
#   4326 = WGS84, good for web maps and general ArcGIS visualization.
#   NA   = keep each layer's original CRS.
OUTPUT_CRS <- 4326

# Write one multi-layer GeoPackage?
WRITE_GPKG <- TRUE

# Write individual ESRI shapefiles?
WRITE_SHP <- TRUE

# Try to repair invalid geometries before writing?
# Usually not necessary, but can help with finicky polygon layers.
MAKE_VALID <- FALSE


# ==== 2: Package setup ====

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# deltamapr is not on CRAN, so add its R-universe repo.
options(repos = c(
  sbashevkin = "https://sbashevkin.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))

install_if_missing("deltamapr")
install_if_missing("sf")
install_if_missing("dplyr")
install_if_missing("purrr")
install_if_missing("tibble")
install_if_missing("readr")
install_if_missing("stringr")

library(deltamapr)
library(sf)
library(dplyr)
library(purrr)
library(tibble)
library(readr)
library(stringr)


# ==== 3: Output folders ====

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

GPKG_DIR <- file.path(OUTPUT_DIR, "gpkg")
SHP_DIR  <- file.path(OUTPUT_DIR, "shp")

if (WRITE_GPKG) dir.create(GPKG_DIR, recursive = TRUE, showWarnings = FALSE)
if (WRITE_SHP)  dir.create(SHP_DIR,  recursive = TRUE, showWarnings = FALSE)

gpkg_file <- file.path(GPKG_DIR, "deltamapr_all_layers.gpkg")

# Start fresh so stale layers do not remain from a previous run.
if (WRITE_GPKG && file.exists(gpkg_file)) {
  unlink(gpkg_file)
}


# ==== 4: Helper functions ====

clean_layer_name <- function(x) {
  x |>
    str_replace_all("[^A-Za-z0-9_]+", "_") |>
    str_replace_all("_+", "_") |>
    str_replace_all("^_|_$", "")
}

load_deltamapr_object <- function(object_name) {
  env <- new.env(parent = emptyenv())
  data(list = object_name, package = "deltamapr", envir = env)
  get(object_name, envir = env)
}

prep_sf_for_export <- function(x, output_crs = OUTPUT_CRS, make_valid = MAKE_VALID) {
  # Drop Z/M dimensions if present. This makes output friendlier for older GIS tools.
  x <- sf::st_zm(x, drop = TRUE, what = "ZM")
  
  # Optional geometry repair.
  if (make_valid) {
    x <- sf::st_make_valid(x)
  }
  
  # Reproject if requested.
  if (!is.na(output_crs)) {
    x <- sf::st_transform(x, output_crs)
  }
  
  x
}

prep_for_shapefile <- function(x) {
  # Shapefiles cannot safely store list columns.
  # This removes non-geometry list columns if any exist.
  geom_col <- attr(x, "sf_column")
  
  keep_cols <- names(x)[vapply(x, function(col) !is.list(col), logical(1))]
  keep_cols <- union(keep_cols, geom_col)
  
  x <- x[, keep_cols, drop = FALSE]
  
  # Shapefile drivers will truncate field names, but this makes names cleaner.
  names(x) <- make.names(names(x), unique = TRUE)
  
  x
}

get_layer_summary <- function(x, object_name, export_name) {
  geom_types <- paste(sort(unique(as.character(sf::st_geometry_type(x)))), collapse = "; ")
  crs_text <- sf::st_crs(x)$input
  if (is.na(crs_text)) crs_text <- NA_character_
  
  bb <- sf::st_bbox(x)
  
  tibble(
    object_name = object_name,
    export_name = export_name,
    n_features = nrow(x),
    geometry_types = geom_types,
    crs = crs_text,
    xmin = unname(bb["xmin"]),
    ymin = unname(bb["ymin"]),
    xmax = unname(bb["xmax"]),
    ymax = unname(bb["ymax"])
  )
}


# ==== 5: Discover all deltamapr datasets ====

pkg_data <- data(package = "deltamapr")$results

object_names <- pkg_data[, "Item"] |>
  as.character() |>
  unique() |>
  sort()

message("Found ", length(object_names), " data objects in deltamapr.")


# ==== 6: Export sf objects ====

inventory <- list()
skipped <- list()

for (obj_name in object_names) {
  
  message("\n--- Checking: ", obj_name)
  
  obj <- tryCatch(
    load_deltamapr_object(obj_name),
    error = function(e) {
      skipped[[obj_name]] <<- paste("Could not load:", conditionMessage(e))
      return(NULL)
    }
  )
  
  if (is.null(obj)) next
  
  if (!inherits(obj, "sf")) {
    skipped[[obj_name]] <- paste("Skipped: object is not sf; class =", paste(class(obj), collapse = ", "))
    message("Skipped; not an sf object.")
    next
  }
  
  export_name <- clean_layer_name(obj_name)
  
  obj_export <- tryCatch(
    prep_sf_for_export(obj),
    error = function(e) {
      skipped[[obj_name]] <<- paste("Could not prepare sf object:", conditionMessage(e))
      return(NULL)
    }
  )
  
  if (is.null(obj_export)) next
  
  # ---- 6a: Write to GeoPackage ----
  if (WRITE_GPKG) {
    tryCatch(
      {
        sf::st_write(
          obj_export,
          dsn = gpkg_file,
          layer = export_name,
          delete_layer = TRUE,
          quiet = TRUE
        )
        message("Wrote GeoPackage layer: ", export_name)
      },
      error = function(e) {
        skipped[[obj_name]] <<- paste("GeoPackage write failed:", conditionMessage(e))
        message("GeoPackage write failed: ", conditionMessage(e))
      }
    )
  }
  
  # ---- 6b: Write to individual shapefile folder ----
  if (WRITE_SHP) {
    shp_layer_dir <- file.path(SHP_DIR, export_name)
    dir.create(shp_layer_dir, recursive = TRUE, showWarnings = FALSE)
    
    shp_file <- file.path(shp_layer_dir, paste0(export_name, ".shp"))
    
    obj_shp <- prep_for_shapefile(obj_export)
    
    tryCatch(
      {
        if (file.exists(shp_file)) {
          unlink(list.files(shp_layer_dir, full.names = TRUE))
        }
        
        sf::st_write(
          obj_shp,
          dsn = shp_file,
          delete_layer = TRUE,
          quiet = TRUE
        )
        
        message("Wrote shapefile: ", shp_file)
      },
      error = function(e) {
        skipped[[obj_name]] <<- paste("Shapefile write failed:", conditionMessage(e))
        message("Shapefile write failed: ", conditionMessage(e))
      }
    )
  }
  
  inventory[[obj_name]] <- get_layer_summary(obj_export, obj_name, export_name)
}


# ==== 7: Write inventory and skipped-object logs ====

inventory_tbl <- bind_rows(inventory)

inventory_file <- file.path(OUTPUT_DIR, "deltamapr_layer_inventory.csv")
readr::write_csv(inventory_tbl, inventory_file)

skipped_tbl <- tibble(
  object_name = names(skipped),
  reason = unlist(skipped, use.names = FALSE)
)

skipped_file <- file.path(OUTPUT_DIR, "deltamapr_skipped_objects.csv")
readr::write_csv(skipped_tbl, skipped_file)

message("\nDone.")
message("Inventory CSV: ", inventory_file)

if (WRITE_GPKG) {
  message("GeoPackage: ", gpkg_file)
}

if (WRITE_SHP) {
  message("Shapefile folder: ", SHP_DIR)
}