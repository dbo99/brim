# ==== config_local_layer_registry.r ==========================================
##
## PURPOSE:
##   Central registry for BRIM local/static layer metadata that is reused across
##   map-build scripts, helper functions, and future local-layer additions.
##
## WHY THIS FILE EXISTS:
##   Local layers were historically registered in several places at once:
##     - MAP_DISPLAY switches in 00_config/config_map_display.r
##     - cache reads/writes in 05_map_build/02_build_core_map_cache.r
##     - overlay-group lists in 05_map_build/04_build_portatreasure2_core_map.r
##     - group-name translation in 03_functions/leaflet_layer_helpers.r
##     - label settings in 00_config/config_labels.r
##
##   This first registry pass is intentionally conservative.  It centralizes the
##   user-facing Leaflet group-name mapping and records layer/cache metadata, but
##   it does NOT yet replace the existing explicit cache loading or layer drawing
##   code.  That keeps this cleanup low-risk and build-compatible.
##
## HOW TO USE WHEN ADDING A LOCAL LAYER:
##   1. Add a row to LOCAL_LAYER_REGISTRY with a stable layer_id.
##   2. Add one or more aliases to LOCAL_LAYER_GROUP_ALIASES if old/internal
##      names need to map to the same user-facing group.
##   3. Add the preprocessing/cache/drawing/popup pieces in the usual scripts.
##   4. Once this registry has proven stable, future cleanup can use it to drive
##      more of the cache read/write and overlay-list boilerplate.
##
## IMPORTANT:
##   - This file is config only: no package dependencies, no data reads/writes.
##   - canonical_group values must match the actual Leaflet group names used in
##     addPolygons()/addPolylines()/addCircleMarkers() and overlayGroups.
##   - Keep local/static layers separate from Ops Live feed outputs.
##

# ==== 1. Local layer registry ================================================

LOCAL_LAYER_REGISTRY <- data.frame(
  layer_id = c(
    # ---- Core ---------------------------------------------------------------
    "project_areas",
    "blm_offices",
    "field_office_outer",
    "blm_core",
    "blm_diffs",

    # ---- Basins -------------------------------------------------------------
    "gw_bull118",
    "cnrfc_basins",
    "cnrfc_fnf_delta",
    "gsp_areas",
    "adjudicated_gw_basins",
    "huc2",
    "huc4",
    "huc6",
    "huc8",
    "huc10",
    "huc12",

    # ---- Monitoring Sites / Records -------------------------------------------------------------
    "cnrfc_stream",
    "cnrfc_precip",
    "cnrfc_precip_weather_station_catalog",
    "cdec_reservoir_stations",
    "usgs_streamgages",
    "usgs_wells",
    "blm_noc_drilled_wells",
    "mojave_2025_gw_well_inventory",
    "swrcb_wr_list_official",
    "swrcb_pod_spatial_matches",
    "swrcb_name_text_candidates",
    "x2_km",
    "springs",
    "scan_stations",
    "snow_pillows",
    "calsim3_nodes",

    # ---- Channels -----------------------------------------------------------
    "major_conveyance",
    "calsim3_arcs",
    "deltamapr_canals",
    "wild_scenic_rivers",

    # ---- Reference ----------------------------------------------------------
    "county",
    "national_scenic_historic_trails",
    "national_monuments",
    "ca_desert_ncl",
    "wilderness_study_areas",
    "federal_wilderness",
    "drecp",
    "acec",
    "grazing_allotments",
    "cgs_geology",
    "rwqcb_regions",
    "water_districts"
  ),

  display_name = c(
    # ---- Core ---------------------------------------------------------------
    "Project area(s)",
    "BLM Offices",
    "BLM Field Office (outer)",
    "BLM-CA Managed (core)",
    "BLM Held/Managed Differences",

    # ---- Basins -------------------------------------------------------------
    "GW – Bull. 118",
    "CNRFC Basins",
    "CNRFC FNF Sha/Tri/west Sierra Basins",
    "Groundwater Sustainability Plan Areas",
    "Adjudicated Groundwater Basins",
    "HUC2",
    "HUC4",
    "HUC6",
    "HUC8",
    "HUC10",
    "HUC12",

    # ---- Monitoring Sites / Records -------------------------------------------------------------
    "CNRFC river/reservoir catalog",
    "CNRFC Precip Gages",
    "CNRFC weather station catalog",
    "CDEC Reservoir Stations",
    "USGS streamgages",
    "USGS monitoring wells",
    "BLM-drilled wells | NOC",
    "GW wells | 2025 Mojave-BLM limited field check",
    "SWRCB 2026 BLM WR list records",
    "SWRCB add'l PODs spatially matched to BLM",
    "SWRCB add'l BLM name/text-match candidates",
    "CVP/SWP X2 km points",
    "Springs",
    "SCAN Stations",
    "Snow Pillows",
    "CalSim3.0",

    # ---- Channels -----------------------------------------------------------
    "Major Conveyance",
    "CalSim3.0",
    "Deltamapr Conveyance",
    "Wild & Scenic Rivers",

    # ---- Reference ----------------------------------------------------------
    "Counties",
    "National Scenic/Historic Trails",
    "National Monuments",
    "CA Desert National Conservation Lands",
    "Wilderness Study Areas",
    "Federal Wilderness",
    "DRECP Planning Area Boundary",
    "ACECs",
    "Grazing Allotments",
    "CA Geology (visual only)",
    "RWQCB Regions",
    "Water Districts"
  ),

  category = c(
    rep("Core", 5),
    rep("Basins", 11),
    rep("Points", 16),
    rep("Channels", 4),
    rep("Reference", 12)
  ),

  canonical_group = c(
    # ---- Core ---------------------------------------------------------------
    "Core – Project area(s)",
    "Core – BLM Offices",
    "Core – BLM Field Office Boundaries",
    "Core – BLM-CA Managed",
    "Core – BLM Held/Managed Differences",

    # ---- Basins -------------------------------------------------------------
    "Basins – GW Basins, Bulletin 118",
    "Basins – CNRFC Basins",
    "Basins – CNRFC FNF Sha/Tri/west Sierra Basins",
    "Basins – Groundwater Sustainability Plan Areas",
    "Basins – Adjudicated Groundwater Basins",
    "Basins – HUC2 – PRISM/BCMv8",
    "Basins – HUC4 – PRISM/BCMv8",
    "Basins – HUC6 – PRISM/BCMv8",
    "Basins – HUC8 – PRISM/BCMv8",
    "Basins – HUC10 – PRISM/BCMv8",
    "Basins – HUC12 – PRISM/BCMv8",

    # ---- Monitoring Sites / Records -------------------------------------------------------------
    "Points – CNRFC river/reservoir catalog",
    "Points – CNRFC Precip Gages",
    "Points – CNRFC weather station catalog",
    "Points – CDEC Reservoir Stations",
    "Points – USGS streamgages",
    "Points – USGS monitoring wells",
    "Points – BLM-drilled wells | NOC",
    "Points – GW wells | 2025 Mojave-BLM limited field check",
    "Points – SWRCB 2026 BLM WR list records",
    "Points – SWRCB add'l PODs spatially matched to BLM",
    "Points – SWRCB add'l BLM name/text-match candidates",
    "Reference – CVP/SWP X2 km points",
    "Points – Springs",
    "Points – SCAN Stations",
    "Points – Snow Pillows",
    "Channels – CalSim3.0",

    # ---- Channels -----------------------------------------------------------
    "Channels – Major Conveyance",
    "Channels – CalSim3.0",
    "Channels – Deltamapr Conveyance",
    "Channels – Wild & Scenic Rivers",

    # ---- Reference ----------------------------------------------------------
    "Reference – Counties",
    "Reference – National Scenic/Historic Trails",
    "Reference – National Monuments",
    "Reference – CA Desert National Conservation Lands",
    "Reference – Wilderness Study Areas",
    "Reference – Federal Wilderness",
    "Reference – DRECP Planning Area Boundary",
    "Reference – ACECs",
    "Reference – Grazing Allotments",
    "Reference – CA Geology",
    "Reference – RWQCB Regions",
    "Reference – Water Districts"
  ),

  cache_file = c(
    # ---- Core ---------------------------------------------------------------
    "project_areas_map.rds",
    "blm_offices_map.rds",
    "field_office_outer_map.rds",
    "blm_core_map.rds",
    "blm_diffs_map.rds",

    # ---- Basins -------------------------------------------------------------
    "gw_bull118_map.rds",
    "cnrfc_basins_map.rds",
    "cnrfc_fnf_delta_map.rds",
    NA_character_,
    NA_character_,
    rep("huc_all_map.rds", 6),

    # ---- Monitoring Sites / Records -------------------------------------------------------------
    "cnrfc_stream_map.rds",
    "cnrfc_precip_map.rds",
    "cnrfc_precip_weather_station_catalog_local_map.rds",
    "cdec_reservoir_stations_map.rds",
    "usgs_streamgages_map.rds",
    "usgs_wells_map.rds",
    "blm_noc_drilled_wells_map.rds",
    "mojave_2025_gw_well_inventory_map.rds",
    rep("swrcb_pod_wr_blm_map.rds", 3),
    "x2_km_map.rds",
    "springs_map.rds",
    "scan_stations_map.rds",
    "snow_pillows_map.rds",
    "calsim3_nodes_map.rds",

    # ---- Channels -----------------------------------------------------------
    "major_conveyance_map.rds",
    "calsim3_arcs_map.rds",
    "deltamapr_canals_map.rds",
    "reference_layers_all_map.rds",

    # ---- Reference ----------------------------------------------------------
    "county_map.rds",
    rep("reference_layers_all_map.rds", 8),
    NA_character_,
    "rwqcb_regions_map.rds",
    "water_districts_map.rds"
  ),

  map_display_flag = c(
    # ---- Core ---------------------------------------------------------------
    "add_project_areas",
    "add_blm_offices",
    "add_field_office_outer",
    NA_character_,
    NA_character_,

    # ---- Basins -------------------------------------------------------------
    NA_character_,
    "add_cnrfc_basins",
    "add_cnrfc_fnf_delta",
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    NA_character_,
    "add_huc10",
    "add_huc12",

    # ---- Monitoring Sites / Records -------------------------------------------------------------
    "add_cnrfc_stream",
    "add_cnrfc_precip",
    "add_cnrfc_precip_weather_station_catalog",
    "add_cdec_reservoir_stations",
    "add_usgs_streamgages",
    "add_usgs_wells",
    "add_blm_noc_drilled_wells",
    "add_mojave_2025_gw_well_inventory",
    "add_swrcb_pod_wr_blm",
    "add_swrcb_pod_wr_blm",
    "add_swrcb_pod_wr_blm",
    "add_x2_km",
    "add_springs",
    "add_scan_stations",
    "add_snow_pillows",
    "add_calsim3_nodes",

    # ---- Channels -----------------------------------------------------------
    "add_major_conveyance",
    "add_calsim3_arcs",
    "add_deltamapr_canals",
    NA_character_,

    # ---- Reference ----------------------------------------------------------
    NA_character_,
    "add_reference_layers",
    "add_reference_layers",
    "add_reference_layers",
    "add_reference_layers",
    "add_reference_layers",
    "add_reference_layers",
    "add_reference_layers",
    "add_reference_layers",
    "add_cgs_geology",
    "add_rwqcb_regions",
    "add_water_districts"
  ),

  geometry_family = c(
    # ---- Core ---------------------------------------------------------------
    "polygon",
    "point",
    "polygon",
    "polygon",
    "polygon",

    # ---- Basins -------------------------------------------------------------
    rep("polygon", 11),

    # ---- Monitoring Sites / Records -------------------------------------------------------------
    rep("point", 16),

    # ---- Channels -----------------------------------------------------------
    "line",
    "line",
    "line",
    "line",

    # ---- Reference ----------------------------------------------------------
    rep("polygon", 12)
  ),

  stringsAsFactors = FALSE
)

## These sources remain available to their preprocessors and non-Local BRIM
## implementations, but they are no longer registered in the Local panel.
LOCAL_LAYER_REGISTRY <- LOCAL_LAYER_REGISTRY[
  !LOCAL_LAYER_REGISTRY$layer_id %in% c("x2_km", "cgs_geology"),
  ,
  drop = FALSE
]

# ---- Consolidated BRIM conveyance layer -------------------------------------
##
## Append this row after the conservative first-pass registry is constructed.
## The legacy Major Conveyance and Deltamapr rows remain available for rollback
## or source-specific QA, but the normal map uses this curated combined layer.

LOCAL_LAYER_REGISTRY <- rbind(
  LOCAL_LAYER_REGISTRY,
  data.frame(
    layer_id = "brim_mapped_conveyance",
    display_name = "Water conveyance | BRIM mapped",
    category = "Channels",
    canonical_group = "Channels – Water conveyance | BRIM mapped",
    cache_file = "conveyance_segments_brim_map.rds",
    map_display_flag = "add_brim_mapped_conveyance",
    geometry_family = "line",
    stringsAsFactors = FALSE
  )
)


# ==== 2. Backward-compatible group-name aliases ==============================
##
## Names are old/plain/internal layer names. Values are canonical Leaflet group
## names. Keep aliases here instead of scattering old-name compatibility checks
## through leaflet_layer_helpers.r.

LOCAL_LAYER_GROUP_ALIASES <- c(
  stats::setNames(
    LOCAL_LAYER_REGISTRY$canonical_group,
    LOCAL_LAYER_REGISTRY$display_name
  ),

  # ---- HUC aliases after note expansion ------------------------------------
  "HUC2 – PRISM/BCMv8 summaries [2]"  = "Basins – HUC2 – PRISM/BCMv8",
  "HUC4 – PRISM/BCMv8 summaries [2]"  = "Basins – HUC4 – PRISM/BCMv8",
  "HUC6 – PRISM/BCMv8 summaries [2]"  = "Basins – HUC6 – PRISM/BCMv8",
  "HUC8 – PRISM/BCMv8 summaries [2]"  = "Basins – HUC8 – PRISM/BCMv8",
  "HUC10 – PRISM/BCMv8 summaries [2]" = "Basins – HUC10 – PRISM/BCMv8",
  "HUC12 – PRISM/BCMv8 summaries [2]" = "Basins – HUC12 – PRISM/BCMv8",

  # ---- CNRFC basin product-availability naming ------------------------------
  "CNRFC FNF Sha/Tri/west Sierra Basins" = "Basins – CNRFC FNF Sha/Tri/west Sierra Basins",
  "CNRFC FNF Delta basins" = "Basins – CNRFC FNF Sha/Tri/west Sierra Basins",
  "CNRFC Product Availability" = "Basins – CNRFC Product Availability",
  "CNRFC Basin Product Availability" = "Basins – CNRFC Product Availability",
  "CNRFC Basin Product Availability Polygons" = "Basins – CNRFC Product Availability",

  # ---- Recent Local-panel display-name aliases ------------------------------
  ## Preserve compatibility with names used by earlier patches/builds while the
  ## user-facing Local panel now uses shorter labels.
  "USGS Streamgages" = "Points – USGS streamgages",
  "USGS Wells" = "Points – USGS monitoring wells",
  "BLM-drilled wells | NOC database" = "Points – BLM-drilled wells | NOC",
  "GW wells | 2025 Mojave-BLM field check" = "Points – GW wells | 2025 Mojave-BLM limited field check",

  # ---- CNRFC catalog names --------------------------------------------------
  ## Current CNRFC point-catalog names are supplied by LOCAL_LAYER_REGISTRY above.
  ## Do not re-alias older CNRFC Stream Gages / River & Reservoir / Precip Gages
  ## names here; those old labels should not drive the new catalog legends.

  # ---- SWRCB naming history -------------------------------------------------
  "SWRCB 2026 BLM WR list records" =
    "Points – SWRCB 2026 BLM WR list records",
  "SWRCB official BLM WR list records [1]" =
    "Points – SWRCB 2026 BLM WR list records",
  "SWRCB additional PODs spatially matched to BLM" =
    "Points – SWRCB add'l PODs spatially matched to BLM",
  "SWRCB additional BLM name/text-match candidates" =
    "Points – SWRCB add'l BLM name/text-match candidates",
  "SWRCB additional PODs spatially matched to BLM" =
    "Points – SWRCB add'l PODs spatially matched to BLM",
  "SWRCB PODs spatially matched to BLM [1]" =
    "Points – SWRCB add'l PODs spatially matched to BLM",
  "SWRCB PODs spatially matched to BLM" =
    "Points – SWRCB add'l PODs spatially matched to BLM",
  "SWRCB spatial POD matches to BLM [1]" =
    "Points – SWRCB add'l PODs spatially matched to BLM",
  "SWRCB additional BLM name/text-match candidates" =
    "Points – SWRCB add'l BLM name/text-match candidates",
  "SWRCB BLM-associated WR/POD records [1]" =
    "Points – SWRCB add'l BLM name/text-match candidates",
  "SWRCB BLM-associated WR/POD records" =
    "Points – SWRCB add'l BLM name/text-match candidates",
  "SWRCB WR/name-list matches [1]" =
    "Points – SWRCB add'l BLM name/text-match candidates",
  "SWRCB PODs relevant to BLM [1]" =
    "Points – SWRCB PODs/WRs relevant to BLM",
  "SWRCB PODs/WRs relevant to BLM [1]" =
    "Points – SWRCB PODs/WRs relevant to BLM",



  # ---- Legacy note-marker aliases ------------------------------------------
  ## Older builds/configs used bracketed note IDs in visible layer names.  Keep
  ## these as input aliases, but route them to the clean no-bracket names.
  "HUC2 [2]"  = "Basins – HUC2 – PRISM/BCMv8",
  "HUC4 [2]"  = "Basins – HUC4 – PRISM/BCMv8",
  "HUC6 [2]"  = "Basins – HUC6 – PRISM/BCMv8",
  "HUC8 [2]"  = "Basins – HUC8 – PRISM/BCMv8",
  "HUC10 [2]" = "Basins – HUC10 – PRISM/BCMv8",
  "HUC12 [2]" = "Basins – HUC12 – PRISM/BCMv8",
  "HUC2 – PRISM/BCMv8 summaries [2]"  = "Basins – HUC2 – PRISM/BCMv8",
  "HUC4 – PRISM/BCMv8 summaries [2]"  = "Basins – HUC4 – PRISM/BCMv8",
  "HUC6 – PRISM/BCMv8 summaries [2]"  = "Basins – HUC6 – PRISM/BCMv8",
  "HUC8 – PRISM/BCMv8 summaries [2]"  = "Basins – HUC8 – PRISM/BCMv8",
  "HUC10 – PRISM/BCMv8 summaries [2]" = "Basins – HUC10 – PRISM/BCMv8",
  "HUC12 – PRISM/BCMv8 summaries [2]" = "Basins – HUC12 – PRISM/BCMv8",
  "SWRCB 2026 BLM WR list records [1]" = "Points – SWRCB 2026 BLM WR list records",
  "SWRCB official BLM WR list records [1]" = "Points – SWRCB 2026 BLM WR list records",
  "SWRCB additional PODs spatially matched to BLM [1]" = "Points – SWRCB add'l PODs spatially matched to BLM",
  "SWRCB PODs spatially matched to BLM [1]" = "Points – SWRCB add'l PODs spatially matched to BLM",
  "SWRCB spatial POD matches to BLM [1]" = "Points – SWRCB add'l PODs spatially matched to BLM",
  "SWRCB additional BLM name/text-match candidates [1]" = "Points – SWRCB add'l BLM name/text-match candidates",
  "SWRCB BLM-associated WR/POD records [1]" = "Points – SWRCB add'l BLM name/text-match candidates",
  "SWRCB WR/name-list matches [1]" = "Points – SWRCB add'l BLM name/text-match candidates",
  "Wild & Scenic Rivers [3]" = "Channels – Wild & Scenic Rivers",
  "National Scenic/Historic Trails [3]" = "Reference – National Scenic/Historic Trails",
  "National Monuments [3]" = "Reference – National Monuments",
  "Wilderness Study Areas [3]" = "Reference – Wilderness Study Areas",
  "Federal Wilderness [3]" = "Reference – Federal Wilderness",

  # ---- CalSim3 naming history -----------------------------------------------
  "CalSim3 Arcs" = "Channels – CalSim3.0",
  "CalSim3 Nodes" = "Channels – CalSim3.0",
  "CalSim3 Network" = "Channels – CalSim3.0",
  "CalSim3.0" = "Channels – CalSim3.0",

  # ---- Delta/conveyance aliases --------------------------------------------
  "Deltamapr Canals" = "Channels – Deltamapr Conveyance",
  "Deltamapr Conveyance" = "Channels – Deltamapr Conveyance"
)

# ==== 3. Registry lookup helpers =============================================

pt_lookup_local_layer_group <- function(group_name) {

  group_name <- as.character(group_name)
  out <- rep(NA_character_, length(group_name))

  if (length(group_name) == 0) {
    return(out)
  }

  hit <- match(group_name, names(LOCAL_LAYER_GROUP_ALIASES))
  ok <- !is.na(hit)
  out[ok] <- unname(LOCAL_LAYER_GROUP_ALIASES[hit[ok]])

  out
}

pt_local_layer_registry_row <- function(layer_id) {

  layer_id <- as.character(layer_id)
  LOCAL_LAYER_REGISTRY[LOCAL_LAYER_REGISTRY$layer_id %in% layer_id, , drop = FALSE]
}

pt_local_layer_cache_files <- function() {

  x <- LOCAL_LAYER_REGISTRY[, c("layer_id", "cache_file"), drop = FALSE]
  x <- x[!is.na(x$cache_file) & x$cache_file != "", , drop = FALSE]
  unique(x)
}


# ==== 4. Feature-count display helpers =======================================
##
## PURPOSE:
##   Let the final map builder append compact feature counts to selected local
##   layer-control names without changing each add*() function by hand.
##
## DESIGN:
##   04_build_portatreasure2_core_map.r creates a named vector called
##   LOCAL_LAYER_FEATURE_COUNT_LABELS after it reads the local cache products.
##   Names are canonical Leaflet groups, values are compact suffixes such as
##   "(~44.5k)" or "(304)".  pt_layer_group_name() applies the suffix as the
##   final step so overlayGroups and actual layer groups stay synchronized.

pt_format_local_layer_count <- function(n) {

  n <- suppressWarnings(as.numeric(n))
  if (length(n) == 0 || is.na(n) || n < 0) {
    return(NA_character_)
  }

  if (n >= 1000) {
    ## Keep one decimal for all thousand-style local layer counts so labels are
    ## visually consistent: (~2.0k), (~3.8k), (~44.2k), etc.
    k <- round(n / 1000, 1)
    k_txt <- format(k, nsmall = 1, trim = TRUE)
    return(paste0("(~", k_txt, "k)"))
  }

  paste0("(", format(as.integer(round(n)), big.mark = ",", scientific = FALSE), ")")
}

pt_format_local_layer_count_exact <- function(n) {

  n <- suppressWarnings(as.numeric(n))
  if (length(n) == 0 || is.na(n) || n < 0) {
    return(NA_character_)
  }

  paste0(
    "(",
    format(as.integer(round(n)), big.mark = ",", scientific = FALSE),
    ")"
  )
}

pt_normalize_local_layer_count_key <- function(group_name) {

  group_name <- as.character(group_name)
  group_name <- trimws(group_name)

  ## Strip any count suffix before matching.  This keeps pt_layer_group_name()
  ## idempotent and avoids labels such as "USGS Wells (~44.2k) (~44.2k)".
  group_name <- sub("\\s+\\((~?[0-9][0-9.,]*k?|[0-9][0-9,]*)\\)$", "", group_name)

  ## 042N: The map builder still registers feature-count labels under the older
  ## "Points – ..." keys.  Normalize those and any mixed-case point-section
  ## labels to the current TOC header before lookup.
  group_name <- sub("^Points\\s+–\\s+", "Points – ", group_name)
  group_name <- sub("^Monitoring sites / records\\s+–\\s+", "Points – ", group_name)
  group_name <- sub("^Monitoring Sites / Records\\s+–\\s+", "Points – ", group_name)

  group_name
}

pt_register_local_layer_feature_counts <- function(counts, exact_names = character(0)) {

  if (is.null(counts) || length(counts) == 0) {
    LOCAL_LAYER_FEATURE_COUNT_LABELS <<- character(0)
    return(invisible(LOCAL_LAYER_FEATURE_COUNT_LABELS))
  }

  counts <- counts[!is.na(names(counts)) & names(counts) != ""]
  count_names <- names(counts)
  suffix <- vapply(seq_along(counts), function(i) {
    if (count_names[i] %in% exact_names) {
      pt_format_local_layer_count_exact(counts[i])
    } else {
      pt_format_local_layer_count(counts[i])
    }
  }, character(1))
  names(suffix) <- count_names
  suffix <- suffix[!is.na(suffix) & suffix != ""]

  ## Store both the original keys and normalized keys so older build scripts
  ## that still register "Points – ..." counts continue to work after the Local
  ## TOC section was renamed to "Monitoring Sites / Records".
  original_keys <- names(suffix)
  normalized_keys <- pt_normalize_local_layer_count_key(original_keys)

  out <- suffix
  names(out) <- original_keys

  norm_out <- suffix
  names(norm_out) <- normalized_keys

  LOCAL_LAYER_FEATURE_COUNT_LABELS <<- c(out, norm_out[!normalized_keys %in% original_keys])
  invisible(LOCAL_LAYER_FEATURE_COUNT_LABELS)
}

pt_apply_local_layer_count_suffix <- function(group_name) {

  group_name <- as.character(group_name)

  if (!exists("LOCAL_LAYER_FEATURE_COUNT_LABELS", inherits = TRUE)) {
    return(group_name)
  }

  count_labels <- get("LOCAL_LAYER_FEATURE_COUNT_LABELS", inherits = TRUE)
  if (is.null(count_labels) || length(count_labels) == 0) {
    return(group_name)
  }

  base_name <- pt_normalize_local_layer_count_key(group_name)
  hit <- match(base_name, names(count_labels))
  ok <- !is.na(hit)

  out <- base_name
  out[ok] <- paste(out[ok], unname(count_labels[hit[ok]]))
  out
}



message("BRIM local-layer registry loaded: ", nrow(LOCAL_LAYER_REGISTRY), " rows.")
