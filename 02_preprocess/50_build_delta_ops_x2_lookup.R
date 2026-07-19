# 02_preprocess/50_build_delta_ops_x2_lookup.R
# Purpose: build the Delta Ops live-feed X2 lookup CSV from the raw X2 shapefile.
#
# IMPORTANT:
# This script must NOT overwrite the local/static X2 layer RDS files:
#   04_processed_data/rds/x2_km_wgs84.rds
#   04_processed_data/cache/latest/x2_km_map.rds
#
# Those files belong to the Local Layers X2 reference layer and are enriched elsewhere
# with popup/hover/display fields. This script only writes Delta Ops feed inputs.

message("Building Delta Ops X2 river-km lookup CSV...")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

needed <- c("sf", "readr")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Missing required packages: ", paste(missing, collapse = ", "))
}

raw_x2 <- file.path(root, "01_raw_data", "misc_reference", "x2_km.shp")
if (!file.exists(raw_x2)) {
  stop("Raw X2 shapefile not found: ", raw_x2)
}

message("Reading: ", raw_x2)
x <- sf::st_read(raw_x2, quiet = TRUE)

if (!inherits(x, "sf") || nrow(x) == 0) {
  stop("Raw X2 shapefile did not read as a non-empty sf object.")
}

attrs <- sf::st_drop_geometry(x)

# Prefer RKI, but keep a defensible fallback if the field is renamed someday.
km_field <- NA_character_
for (nm in c("RKI", "river_km", "x2_km", "km", "KM")) {
  if (nm %in% names(attrs)) {
    vals <- suppressWarnings(as.numeric(as.character(attrs[[nm]])))
    if (sum(!is.na(vals)) > 0) {
      km_field <- nm
      break
    }
  }
}

if (is.na(km_field)) {
  stop("Could not identify an X2 river-km field. Expected RKI or similar.")
}

x4326 <- sf::st_transform(x, 4326)
coords <- sf::st_coordinates(x4326)
km <- suppressWarnings(as.numeric(as.character(attrs[[km_field]])))

out <- data.frame(
  river_km = km,
  lon = coords[, 1],
  lat = coords[, 2],
  label = paste0("X2 ", km, " km"),
  source_file = "01_raw_data/misc_reference/x2_km.shp",
  source_km_field = km_field,
  build_time_local = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
  stringsAsFactors = FALSE
)

out <- out[!is.na(out$river_km), ]
out <- out[order(out$river_km), ]

if (nrow(out) == 0) {
  stop("No valid X2 river-km rows were produced.")
}

# Primary live-feed input.
live_csv <- file.path(root, "brim-live-data-feeds", "data", "input", "x2_river_km_lookup.csv")
dir.create(dirname(live_csv), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(out, live_csv)
message("Wrote: ", live_csv)

# Optional sandbox copy, if the sandbox folder exists.
sandbox_dir <- file.path(root, "05_map_build", "dev_sandbox", "delta_ops_sandbox", "data", "input")
if (dir.exists(sandbox_dir)) {
  sandbox_csv <- file.path(sandbox_dir, "x2_river_km_lookup.csv")
  readr::write_csv(out, sandbox_csv)
  message("Wrote: ", sandbox_csv)
}

message("Rows: ", nrow(out))
message("River-km range: ", min(out$river_km), " to ", max(out$river_km))
message("Done. Local/static X2 RDS files were not modified.")
