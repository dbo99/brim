# Focused Phase C source/configuration checks. No live network is used.

mapping <- utils::read.csv(file.path("00_config", "major_water_supply_basin_product_mapping.csv"), stringsAsFactors = FALSE, check.names = FALSE)
catalog <- utils::read.csv(file.path("00_config", "major_water_supply_basin_geometry_catalog.csv"), stringsAsFactors = FALSE, check.names = FALSE)
components <- utils::read.csv(file.path("00_config", "major_water_supply_basin_component_manifest.csv"), stringsAsFactors = FALSE, check.names = FALSE)
crosswalk <- utils::read.csv(file.path("00_config", "major_water_supply_basin_reservoir_crosswalk.csv"), stringsAsFactors = FALSE, check.names = FALSE)
links <- utils::read.csv(file.path("00_config", "major_water_supply_basin_related_links.csv"), stringsAsFactors = FALSE, check.names = FALSE)
product7 <- mapping[mapping$source_family == "CNRFC" & mapping$product_family == "april_july", , drop = FALSE]

stopifnot(
  nrow(mapping) == 54L,
  length(unique(mapping$forecast_key)) == 54L,
  sum(mapping$source_family == "CNRFC") == 51L,
  sum(mapping$source_family == "CBRFC") == 3L,
  nrow(product7) == 18L,
  all(grepl("(^|\\|)percent_median(\\||$)", product7$measure_applicability)),
  all(grepl("(^|\\|)percent_median(\\||$)", product7$allowed_popup_metrics)),
  nrow(catalog) == 23L,
  nrow(components) == 18L,
  setequal(unique(components$derived_geometry_id), c("BDBC1_FNF", "SACC0_FNF", "VNSC0_FNF", "MLIC0_FNF")),
  all(components$operation == "union"),
  all(components$required),
  all(unique(components$derived_geometry_id) %in% catalog$geometry_id),
  all(nzchar(catalog$source_url[match(unique(components$derived_geometry_id), catalog$geometry_id)])),
  sum(catalog$geometry_role == "context_only") == 2L,
  nrow(crosswalk) == 14L,
  nrow(links) == 21L,
  !"BDBC1_FNF" %in% mapping$geometry_id,
  !any(grepl("^HUC2_", mapping$geometry_id)),
  identical(tail(mapping$forecast_key, 3L), c(
    "CBRFC:GLDA3:APR_JUL_WSUP",
    "CBRFC:GLDA3:WATER_YEAR_INFLOW",
    "CBRFC:LKSA3:LOCAL_INTERVENING_MONTHLY"
  ))
)

read_text <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
helper_text <- read_text(file.path("03_functions", "leaflet_ops_live_major_water_supply_basin_helpers.r"))
ops_text <- read_text(file.path("03_functions", "leaflet_ops_live_helpers.r"))
builder_text <- read_text(file.path("05_map_build", "04_build_portatreasure2_core_map.r"))
local_text <- read_text(file.path("03_functions", "leaflet_layer_local_reference_helpers.r"))
registry_text <- read_text(file.path("00_config", "config_local_layer_registry.r"))

stopifnot(
  grepl("Major Water-Supply Basin Forecasts", helper_text, fixed = TRUE),
  grepl("major_water_supply_basin_geometry_map.rds", builder_text, fixed = TRUE),
  grepl("__PT_OPS_LIVE_MAJOR_WATER_SUPPLY_BASIN_HELPERS_JS__", ops_text, fixed = TRUE),
  grepl("majorWaterSupplyComponentManifest", ops_text, fixed = TRUE),
  grepl("major_water_supply_basin_component_manifest.csv", helper_text, fixed = TRUE),
  grepl("BRIM geometry assembly", helper_text, fixed = TRUE),
  !grepl("SHDC1 = PITC1", helper_text, fixed = TRUE),
  grepl("pt_layer_group_name(\"CNRFC FNF Sha/Tri/west Sierra Basins\")", local_text, fixed = TRUE),
  grepl("Basins – CNRFC FNF Sha/Tri/west Sierra Basins", registry_text, fixed = TRUE),
  grepl("bindPopup(function()", helper_text, fixed = TRUE),
  !grepl("cdec_reservoir_latest.geojson", helper_text, fixed = TRUE)
)

message("Major water-supply basin Phase C source/configuration tests passed.")
