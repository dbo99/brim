# ==== leaflet_layer_local_core_helpers.r ================================================
##
## PURPOSE:
##   Local layer shared constants, registry/category helpers, BLM-distance helpers, and popup templates.
##
## NOTE:
##   Extracted from leaflet_layer_helpers.r as a maintainability-only split.
##   Function names and behavior are intentionally unchanged.

# ==== 1. Style constants =====================================================

PT_HUC_COLS <- c(
  huc2  = "#d73027",
  huc4  = "#fc8d59",
  huc6  = "#0000CD",
  huc8  = "#555555",
  huc10 = "#006400",
  huc12 = "#8B4513"
)

PT_HUC_WEIGHTS <- c(
  huc2  = 10,
  huc4  = 8,
  huc6  = 6,
  huc8  = 3,
  huc10 = 1.5,
  huc12 = 0.7
)

# ---- Local-layer registry ---------------------------------------------------
##
## The registry centralizes local-layer group-name metadata.  This source call
## is intentionally defensive so older/development contexts that source this
## helper before config files still work.  The hard-coded compatibility mapping
## below remains as a fallback.
if (!exists("pt_lookup_local_layer_group", mode = "function") &&
    file.exists("00_config/config_local_layer_registry.r")) {
  source("00_config/config_local_layer_registry.r")
}

# ==== 1A. Layer-note label helpers ===========================================

pt_note_group_name <- function(group_name) {
  
  group_name <- as.character(group_name)
  
  ## Older reference-layer/HUC names used trailing asterisks in the manifest
  ## to trigger visible bracketed note IDs such as [2] and [3].  Those bracket
  ## IDs are now intentionally removed from the layer-control labels; the notes
  ## panel carries the explanatory text instead.  Keep this helper so existing
  ## manifest markup is stripped consistently in both overlayGroups and add*().
  group_name <- gsub(" \\*\\*$", "", group_name)
  group_name <- gsub(" \\*$",  "", group_name)
  trimws(group_name)
}

# ==== 1B. Layer-control category helpers =====================================
##
## PURPOSE:
##   Convert plain internal Leaflet group names into user-facing grouped names
##   for the layer-control checklist.
##
## WHY THIS EXISTS:
##   Leaflet requires the overlay group name used in addPolygons(),
##   addPolylines(), addCircleMarkers(), etc. to match the name listed in
##   overlayGroups. Rather than manually editing dozens of names in multiple
##   places, this helper provides one centralized translation layer.
##
## DESIGN:
##   - Plain group names are still easy to read in layer helper code.
##   - User-facing group names get category prefixes:
##       Ops –
##       Core –
##       Basins –
##       Points –
##       Channels –
##       Reference –
##       Labels –
##   - Old HUC/reference note markers are stripped from layer names.
##   - Optional feature counts are appended centrally when the map builder registers them.
##   - Unknown/unlisted layer names fall through as Reference – <name>.
##
## IMPORTANT:
##   Use this helper consistently in BOTH places:
##     1. overlay group lists in 04_build_portatreasure2_core_map.r
##     2. actual Leaflet layer group names in leaflet drawing helpers
##
##   If a layer appears in the control but does not toggle correctly, the most
##   likely cause is a mismatch between overlayGroups and the add*() group name.

pt_layer_group_name <- function(group_name) {
  
  group_name <- as.character(group_name)
  group_name <- trimws(group_name)

  ## Remove old visible bracket-note IDs from local layer group names before
  ## category detection. This also cleans already-categorized legacy names such
  ## as "Points – ... [1]" that would otherwise bypass registry aliases.
  group_name <- sub("\\s+\\[[123]\\]$", "", group_name)
  group_name <- sub(" summaries \\[2\\]$", " summaries", group_name)
  
  ## Normalize old or accidentally double-prefixed label group names first.
  ## This makes the helper safe to apply repeatedly and also repairs older
  ## generated names such as "Reference – Labels – HUC8".
  group_name <- sub("^Labels:\\s*", "Labels – ", group_name)
  group_name <- sub("^Reference\\s+–\\s+Labels\\s+–\\s+", "Labels – ", group_name)
  group_name <- sub("^Reference\\s+–\\s+Reference\\s+–\\s+", "Reference – ", group_name)
  group_name <- sub("^Monitoring Sites / Records\\s+–\\s+", "Points – ", group_name)
  group_name <- sub("^Monitoring sites / records\\s+–\\s+", "Points – ", group_name)

  ## Normalize older SWRCB water-right/POD display names before category
  ## detection, so already-categorized old names cannot bypass the newer,
  ## more explicit Water rights POD names.
  group_name <- sub("^Points\\s+–\\s+SWRCB 2026 BLM WR list records$", "Water rights POD | SWRCB 2026 BLM list", group_name)
  group_name <- sub("^Points\\s+–\\s+SWRCB add'l PODs spatially matched to BLM$", "Water rights POD | BRIM spatial BLM match", group_name)
  group_name <- sub("^Points\\s+–\\s+SWRCB add'l BLM name/text-match candidates$", "Water rights POD | BRIM name/text BLM candidate", group_name)
  
  category_prefixes <- c(
    "Ops –",
    "Core –",
    "Basins –",
    "Points –",
    "Channels –",
    "Reference –",
    "Labels –"
  )
  
  already_categorized <- Reduce(
    `|`,
    lapply(category_prefixes, function(prefix) startsWith(group_name, prefix))
  )
  
  registry_group_name <- if (exists("pt_lookup_local_layer_group", mode = "function")) {
    pt_lookup_local_layer_group(group_name)
  } else {
    rep(NA_character_, length(group_name))
  }
  
  registry_hit <- !is.na(registry_group_name) & registry_group_name != ""
  
  out <- dplyr::case_when(
    
    # ---- Already categorized ------------------------------------------------
    
    already_categorized ~ group_name,
    
    # ---- Local-layer registry ------------------------------------------------
    ##
    ## Keep the explicit case_when blocks below as a temporary safety net while
    ## the registry is phased in.  Registry hits should preserve current group
    ## names and reduce future one-off edits when local layers are added.
    
    ## SWRCB / CalWATRS water-right POD rows are intentionally handled before
    ## registry hits so older registry aliases cannot preserve less-obvious
    ## SWRCB-first layer names in the Local panel.
    group_name %in% c(
      "Water rights POD | SWRCB 2026 BLM list",
      "SWRCB 2026 BLM WR list records",
      "SWRCB official BLM WR list records"
    ) ~
      "Points – Water rights POD | SWRCB 2026 BLM list",

    group_name %in% c(
      "Water rights POD | BRIM spatial BLM match",
      "SWRCB additional PODs spatially matched to BLM",
      "SWRCB PODs spatially matched to BLM",
      "SWRCB spatial POD matches to BLM"
    ) ~
      "Points – Water rights POD | BRIM spatial BLM match",

    group_name %in% c(
      "Water rights POD | BRIM name/text BLM candidate",
      "SWRCB additional BLM name/text-match candidates",
      "SWRCB BLM-associated WR/POD records",
      "SWRCB WR/name-list matches"
    ) ~
      "Points – Water rights POD | BRIM name/text BLM candidate",
    
    registry_hit ~ registry_group_name,
    
    # ---- Ops -----------------------------------------------------------------
    
    group_name == "NEXRAD Radar" ~
      "Ops – NEXRAD Radar",
    
    # ---- Core ----------------------------------------------------------------
    
    group_name == "Project area(s)" ~
      "Core – Project area(s)",
    
    group_name == "BLM Offices" ~
      "Core – BLM Offices",
    
    group_name == "BLM Field Office (outer)" ~
      "Core – BLM Field Office Boundaries",
    
    group_name == "BLM-CA Managed (core)" ~
      "Core – BLM-CA Managed",
    
    group_name == "BLM Held/Managed Differences" ~
      "Core – BLM Held/Managed Differences",
    
    # ---- Basins --------------------------------------------------------------
    
    group_name == "GW – Bull. 118" ~
      "Basins – GW Basins, Bulletin 118",
    
    group_name == "CNRFC Basins" ~
      "Basins – CNRFC Basins",
    
    group_name == "CNRFC FNF Sha/Tri/west Sierra Basins" ~
      "Basins – CNRFC FNF Sha/Tri/west Sierra Basins",

    group_name %in% c("CNRFC Product Availability", "CNRFC Basin Product Availability") ~
      "Basins – CNRFC Product Availability",
    
    group_name == "Groundwater Sustainability Plan Areas" ~
      "Basins – Groundwater Sustainability Plan Areas",
    
    group_name == "Adjudicated Groundwater Basins" ~
      "Basins – Adjudicated Groundwater Basins",
    
    group_name %in% c(
      "HUC2",
      "HUC4",
      "HUC6",
      "HUC8",
      "HUC10",
      "HUC12"
    ) ~
      paste0("Basins – ", group_name, " – PRISM/BCMv8"),
    
    group_name %in% c(
      "HUC2 – PRISM/BCMv8",
      "HUC4 – PRISM/BCMv8",
      "HUC6 – PRISM/BCMv8",
      "HUC8 – PRISM/BCMv8",
      "HUC10 – PRISM/BCMv8",
      "HUC12 – PRISM/BCMv8",
      "HUC2 – PRISM/BCMv8 summaries",
      "HUC4 – PRISM/BCMv8 summaries",
      "HUC6 – PRISM/BCMv8 summaries",
      "HUC8 – PRISM/BCMv8 summaries",
      "HUC10 – PRISM/BCMv8 summaries",
      "HUC12 – PRISM/BCMv8 summaries"
    ) ~
      paste0("Basins – ", sub(" summaries$", "", group_name)),
    
    # ---- Points --------------------------------------------------------------
    
    group_name == "CNRFC river/reservoir catalog" ~
      "Points – CNRFC river/reservoir catalog",
    
    ## Legacy CNRFC Precip Gages remains hidden by MAP_DISPLAY$add_cnrfc_precip = FALSE.
    ## Do not alias it to the new weather-station catalog.
    group_name == "CNRFC Precip Gages" ~
      "Points – CNRFC Precip Gages",

    group_name == "CNRFC weather station catalog" ~
      "Points – CNRFC weather station catalog",

    group_name == "CDEC Reservoir Stations" ~
      "Points – CDEC Reservoir Stations",
    
    group_name %in% c("USGS streamgages", "USGS Streamgages") ~
      "Points – USGS streamgages",
    
    group_name %in% c("USGS monitoring wells", "USGS Wells") ~
      "Points – USGS monitoring wells",
    
    group_name %in% c("BLM-drilled wells | NOC", "BLM-drilled wells | NOC database") ~
      "Points – BLM-drilled wells | NOC",

    group_name %in% c(
      "GW wells | 2025 Mojave-BLM limited field check",
      "GW wells | 2025 Mojave-BLM field check"
    ) ~
      "Points – GW wells | 2025 Mojave-BLM limited field check",

    group_name %in% c(
      "Water rights POD | SWRCB 2026 BLM list",
      "SWRCB 2026 BLM WR list records",
      "SWRCB official BLM WR list records"
    ) ~
      "Points – Water rights POD | SWRCB 2026 BLM list",

    group_name %in% c(
      "Water rights POD | BRIM spatial BLM match",
      "SWRCB additional PODs spatially matched to BLM",
      "SWRCB PODs spatially matched to BLM",
      "SWRCB spatial POD matches to BLM"
    ) ~
      "Points – Water rights POD | BRIM spatial BLM match",

    group_name %in% c(
      "Water rights POD | BRIM name/text BLM candidate",
      "SWRCB additional BLM name/text-match candidates",
      "SWRCB BLM-associated WR/POD records",
      "SWRCB WR/name-list matches"
    ) ~
      "Points – Water rights POD | BRIM name/text BLM candidate",

    ## Backward compatibility for older map builds.  New builds should use the
    ## three clearer SWRCB provenance groups above.
    group_name %in% c(
      "SWRCB PODs relevant to BLM",
      "SWRCB PODs/WRs relevant to BLM"
    ) ~
      "Points – SWRCB PODs/WRs relevant to BLM",
    
    group_name == "CVP/SWP X2 km points" ~
      "Reference – CVP/SWP X2 km points",
    
    group_name == "Springs" ~
      "Points – Springs",
    
    group_name == "SCAN Stations" ~
      "Points – SCAN Stations",
    
    group_name == "Snow Pillows" ~
      "Points – Snow Pillows",
    
    # ---- Channels ------------------------------------------------------------
    
    group_name == "Major Conveyance" ~
      "Channels – Major Conveyance",
    
    group_name %in% c("CalSim3 Arcs", "CalSim3 Nodes", "CalSim3 Network", "CalSim3.0") ~
      "Channels – CalSim3.0",
    
    group_name %in% c("Deltamapr Canals", "Deltamapr Conveyance") ~
      "Channels – Deltamapr Conveyance",
    
    group_name == "Wild & Scenic Rivers" ~
      "Channels – Wild & Scenic Rivers",

    group_name == "Wild & Scenic River corridors" ~
      "Channels – Wild & Scenic River corridors",
    
    # ---- Labels --------------------------------------------------------------
    
    grepl("^Labels:", group_name) ~
      sub("^Labels:\\s*", "Labels – ", group_name),
    
    # ---- Reference -----------------------------------------------------------
    
    group_name == "Counties" ~
      "Reference – Counties",
    
    group_name == "National Scenic/Historic Trails" ~
      "Reference – National Scenic/Historic Trails",
    
    group_name == "National Monuments" ~
      "Reference – National Monuments",
    
    group_name == "CA Desert National Conservation Lands" ~
      "Reference – CA Desert National Conservation Lands",
    
    group_name == "Wilderness Study Areas" ~
      "Reference – Wilderness Study Areas",
    
    group_name == "Federal Wilderness" ~
      "Reference – Federal Wilderness",
    
    group_name %in% c("DRECP", "DRECP Planning Area Boundary") ~
      "Reference – DRECP Planning Area Boundary",
    
    group_name == "ACECs" ~
      "Reference – ACECs",
    
    group_name == "Grazing Allotments" ~
      "Reference – Grazing Allotments",
    
    group_name == "CA Geology (visual only)" ~
      "Reference – CA Geology",
    
    group_name == "RWQCB Regions" ~
      "Reference – RWQCB Regions",
    
    group_name == "Water Districts" ~
      "Reference – Water Districts",
    
    # ---- Fallback ------------------------------------------------------------
    
    TRUE ~ paste0("Reference – ", group_name)
  )

  if (exists("pt_apply_local_layer_count_suffix", mode = "function")) {
    out <- pt_apply_local_layer_count_suffix(out)
  }

  out
}


# ==== 1B.1 BLM-distance helper for local point catalogs ======================
##
## PURPOSE:
##   Add BLM proximity fields to local point catalogs using the current core
##   BLM-managed lands RDS. This keeps local catalog filters aligned with the
##   same quarterly refreshed BLM geometry used elsewhere in BRIM.

pt_add_blm_distance_fields_to_points <- function(x,
                                                 lon_col = NULL,
                                                 lat_col = NULL,
                                                 blm_path = file.path("04_processed_data", "rds", "blm_managed_core_3310.rds")) {
  if (is.null(x) || nrow(x) == 0) return(x)

  have_dist <- all(c("on_blm_ca", "dist_to_blm_mi", "dist_to_blm_ft") %in% names(x)) &&
    any(!is.na(suppressWarnings(as.numeric(x$dist_to_blm_mi))))
  if (isTRUE(have_dist)) return(x)

  x$on_blm_ca <- NA
  x$dist_to_blm_mi <- NA_real_
  x$dist_to_blm_ft <- NA_real_

  if (!file.exists(blm_path)) {
    message("BLM distance fields not computed: missing ", blm_path)
    return(x)
  }

  blm <- tryCatch(readRDS(blm_path), error = function(e) NULL)
  if (!inherits(blm, "sf") || nrow(blm) == 0) {
    message("BLM distance fields not computed: invalid BLM core RDS.")
    return(x)
  }

  pts <- NULL
  if (inherits(x, "sf")) {
    pts <- tryCatch({
      if (is.na(sf::st_crs(x))) {
        sf::st_set_crs(x, 4326)
      } else {
        x
      }
    }, error = function(e) NULL)
  } else if (!is.null(lon_col) && !is.null(lat_col) && all(c(lon_col, lat_col) %in% names(x))) {
    lon <- suppressWarnings(as.numeric(x[[lon_col]]))
    lat <- suppressWarnings(as.numeric(x[[lat_col]]))
    ok <- !is.na(lon) & !is.na(lat) & abs(lat) <= 90 & abs(lon) <= 180 & !(lat == 0 & lon == 0)
    if (!any(ok)) return(x)
    pts <- tryCatch(
      sf::st_as_sf(x, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE),
      error = function(e) NULL
    )
  }

  if (!inherits(pts, "sf") || nrow(pts) != nrow(x)) {
    message("BLM distance fields not computed: point geometry/lat-lon unavailable.")
    return(x)
  }

  pts3310 <- tryCatch(sf::st_transform(pts, 3310), error = function(e) NULL)
  blm3310 <- tryCatch(sf::st_transform(blm, 3310), error = function(e) NULL)
  if (!inherits(pts3310, "sf") || !inherits(blm3310, "sf")) {
    message("BLM distance fields not computed: transform to EPSG:3310 failed.")
    return(x)
  }

  blm3310 <- tryCatch(sf::st_make_valid(blm3310), error = function(e) blm3310)

  inside <- tryCatch(
    lengths(sf::st_intersects(pts3310, blm3310, sparse = TRUE)) > 0,
    error = function(e) rep(FALSE, nrow(pts3310))
  )

  nearest <- tryCatch(sf::st_nearest_feature(pts3310, blm3310), error = function(e) NULL)
  if (is.null(nearest) || length(nearest) != nrow(pts3310)) {
    message("BLM distance fields not computed: nearest-feature lookup failed.")
    return(x)
  }

  dist_m <- tryCatch(
    as.numeric(sf::st_distance(sf::st_geometry(pts3310), sf::st_geometry(blm3310)[nearest], by_element = TRUE)),
    error = function(e) rep(NA_real_, nrow(pts3310))
  )
  dist_m[inside] <- 0

  x$on_blm_ca <- inside
  x$dist_to_blm_mi <- dist_m / 1609.344
  x$dist_to_blm_ft <- dist_m * 3.280839895

  message(
    "BLM distance fields computed for local point catalog: ",
    format(sum(!is.na(x$dist_to_blm_mi)), big.mark = ","), " rows; ",
    format(sum(x$on_blm_ca %in% TRUE, na.rm = TRUE), big.mark = ","), " on BLM."
  )

  x
}

# ==== 1C. Layer-control category ordering helper =============================
##
## PURPOSE:
##   Reorder already-categorized overlay group names so the flat Leaflet layer
##   control behaves like a simple grouped table of contents.
##
## IMPORTANT:
##   This changes only the checkbox order in the layer control. It does not
##   control map draw order. Draw order is controlled by add*() order and panes.

pt_order_overlay_groups <- function(groups) {
  
  groups <- as.character(groups)
  groups <- groups[!is.na(groups) & groups != ""]
  groups <- pt_layer_group_name(groups)
  groups <- unique(groups)
  
  category_order <- c(
    "Ops –",
    "Core –",
    "Basins –",
    "Points –",
    "Channels –",
    "Reference –",
    "Labels –"
  )
  
  group_category <- vapply(groups, function(g) {
    
    hit <- category_order[startsWith(g, category_order)]
    
    if (length(hit) == 0) {
      "Reference –"
    } else {
      hit[1]
    }
  }, character(1))
  
  category_rank <- match(group_category, category_order)
  category_rank[is.na(category_rank)] <- length(category_order) + 1

  ## Keep the Channels block intentionally curated instead of relying on the
  ## order in which source helpers happened to add their Leaflet groups.
  ## CalSim3.0 stays at the bottom of Channels; legacy source layers, when
  ## temporarily enabled for QA, remain between WSR and CalSim3.0.
  channel_rank <- rep(0L, length(groups))
  is_channel <- group_category == "Channels –"
  channel_label <- sub("^Channels\\s+–\\s+", "", groups)
  channel_rank[is_channel] <- 500L
  channel_rank[is_channel & grepl("^Water conveyance \\| BRIM mapped", channel_label)] <- 10L
  channel_rank[is_channel & grepl("^Wild & Scenic Rivers", channel_label)] <- 20L
  channel_rank[is_channel & channel_label == "Major Conveyance"] <- 30L
  channel_rank[is_channel & channel_label == "Deltamapr Conveyance"] <- 40L
  channel_rank[is_channel & channel_label == "CalSim3.0"] <- 999L

  groups[order(category_rank, channel_rank, seq_along(groups))]
}


# ==== 1D. Exact Ops Live membership helpers ==================================
##
## PURPOSE:
##   Local catalog legends sometimes need to highlight the exact subset that is
##   present in an Ops Live map-ready RDS.  Use the Ops RDS as the source of
##   truth when it is available, rather than inferring membership from audit
##   status strings in the broader Local catalog.

pt_cnrfc_weather_norm_id <- function(v) {
  v <- toupper(trimws(as.character(v)))
  v[is.na(v) | v %in% c("", "NA", "NAN", "NULL")] <- NA_character_
  v
}

pt_cnrfc_weather_id_variants <- function(v) {
  id <- pt_cnrfc_weather_norm_id(v)
  out <- id

  ## Some ASOS/METAR-style station links use either ABC or KABC.  Keep both
  ## variants so the Local catalog and Ops map-ready RDS can still line up when
  ## one side stores the WRH K-prefixed form.
  k3 <- !is.na(id) & grepl("^K[A-Z0-9]{3}$", id)
  out[k3] <- sub("^K", "", id[k3])

  id3 <- !is.na(id) & grepl("^[A-Z0-9]{3}$", id)
  out2 <- id
  out2[id3] <- paste0("K", id[id3])

  unique(c(out, out2))
}

pt_cnrfc_weather_id_vec <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return(character(0))

  id_col <- c("cnrfc_id", "station_id", "site_id", "nwsid", "id")
  id_col <- id_col[id_col %in% names(df)]
  if (length(id_col) == 0) return(rep(NA_character_, nrow(df)))

  pt_cnrfc_weather_norm_id(df[[id_col[[1]]]])
}

pt_cnrfc_weather_ops_live_rds <- function() {
  paths <- c(
    file.path("04_processed_data", "cache", "latest", "cnrfc_precip_weather_stations_map.rds"),
    file.path("04_processed_data", "rds", "cnrfc_precip_weather_stations_map.rds")
  )

  for (path in paths) {
    if (!file.exists(path)) next
    obj <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.data.frame(obj) && nrow(obj) > 0) return(as.data.frame(obj, stringsAsFactors = FALSE))
  }

  NULL
}

pt_cnrfc_weather_ops_live_ids <- function() {
  ops <- pt_cnrfc_weather_ops_live_rds()
  if (!is.data.frame(ops) || nrow(ops) == 0) return(character(0))

  ids <- pt_cnrfc_weather_id_vec(ops)
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (length(ids) == 0) return(character(0))

  unique(pt_cnrfc_weather_id_variants(ids))
}

pt_cnrfc_weather_in_ops_live_exact <- function(df, fallback = NULL) {
  n <- if (is.data.frame(df)) nrow(df) else 0L
  if (n == 0) return(logical(0))

  ids <- pt_cnrfc_weather_id_vec(df)
  ops_ids <- pt_cnrfc_weather_ops_live_ids()

  if (length(ops_ids) > 0) {
    id_variants <- lapply(ids, pt_cnrfc_weather_id_variants)
    return(vapply(id_variants, function(v) any(!is.na(v) & v %in% ops_ids), logical(1)))
  }

  if (!is.null(fallback) && length(fallback) == n) {
    return(as.logical(fallback))
  }

  rep(FALSE, n)
}


# ==== 1B.2 Cached BLM-distance joins for CNRFC local catalogs =================
##
## PURPOSE:
##   Final HTML builds should not calculate point-to-BLM distances.  CNRFC
##   Local catalog filters read precomputed distance fields written by:
##
##     02_preprocess/59_update_cnrfc_local_blm_distance_fields.R
##
## DESIGN:
##   - Join by a stable station key: station ID + rounded lat/lon.
##   - If the cache is missing, keep fields as NA and print a clear message.
##   - Do not fall back to expensive sf distance math in the final map build.

pt_cnrfc_blm_cache_path <- function(dataset) {
  dataset <- match.arg(dataset, c("river_reservoir", "weather_station"))

  file.path(
    "04_processed_data", "cache", "latest",
    switch(
      dataset,
      river_reservoir = "cnrfc_river_reservoir_blm_distance_fields.csv",
      weather_station = "cnrfc_weather_station_blm_distance_fields.csv"
    )
  )
}

pt_cnrfc_first_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(rep(NA_character_, nrow(df)))
  as.character(df[[hit[[1]]]])
}

pt_cnrfc_norm_key_part <- function(x) {
  x <- toupper(trimws(as.character(x)))
  x <- gsub("\\s+", "", x)
  x[x %in% c("", "NA", "NAN", "NULL", "UNDEFINED")] <- NA_character_
  x
}

pt_cnrfc_point_latlon <- function(x, lon_col = NULL, lat_col = NULL) {
  n <- if (is.data.frame(x)) nrow(x) else 0L
  out <- data.frame(
    lon = rep(NA_real_, n),
    lat = rep(NA_real_, n),
    stringsAsFactors = FALSE
  )

  if (n == 0) return(out)

  if (!is.null(lon_col) && !is.null(lat_col) && all(c(lon_col, lat_col) %in% names(x))) {
    out$lon <- suppressWarnings(as.numeric(x[[lon_col]]))
    out$lat <- suppressWarnings(as.numeric(x[[lat_col]]))
    return(out)
  }

  lon_candidates <- c("lon", "longitude", "dec_long_va", "site_longitude", "lng", "x")
  lat_candidates <- c("lat", "latitude", "dec_lat_va", "site_latitude", "y")
  lon_hit <- lon_candidates[lon_candidates %in% names(x)]
  lat_hit <- lat_candidates[lat_candidates %in% names(x)]

  if (length(lon_hit) > 0 && length(lat_hit) > 0) {
    out$lon <- suppressWarnings(as.numeric(x[[lon_hit[[1]]]]))
    out$lat <- suppressWarnings(as.numeric(x[[lat_hit[[1]]]]))
    return(out)
  }

  if (inherits(x, "sf")) {
    coords <- tryCatch({
      g <- x
      if (is.na(sf::st_crs(g))) g <- sf::st_set_crs(g, 4326)
      g <- sf::st_transform(g, 4326)
      as.data.frame(sf::st_coordinates(g))
    }, error = function(e) NULL)

    if (is.data.frame(coords) && all(c("X", "Y") %in% names(coords)) && nrow(coords) == n) {
      out$lon <- suppressWarnings(as.numeric(coords$X))
      out$lat <- suppressWarnings(as.numeric(coords$Y))
    }
  }

  out
}

pt_cnrfc_blm_join_key <- function(x, dataset = c("river_reservoir", "weather_station"),
                                  lon_col = NULL, lat_col = NULL) {
  dataset <- match.arg(dataset)
  df <- as.data.frame(x, stringsAsFactors = FALSE)
  ll <- pt_cnrfc_point_latlon(x, lon_col = lon_col, lat_col = lat_col)

  id <- if (dataset == "river_reservoir") {
    pt_cnrfc_first_col(df, c("nwsid", "cnrfc_id", "station_id", "id", "site_id"))
  } else {
    pt_cnrfc_first_col(df, c("cnrfc_id", "station_id", "nwsid", "site_id", "id"))
  }

  id <- pt_cnrfc_norm_key_part(id)
  id[is.na(id)] <- "NOID"

  lon_txt <- ifelse(is.na(ll$lon), "NA", sprintf("%.5f", ll$lon))
  lat_txt <- ifelse(is.na(ll$lat), "NA", sprintf("%.5f", ll$lat))

  paste(id, lat_txt, lon_txt, sep = "|")
}

pt_join_cnrfc_blm_distance_fields <- function(x,
                                              dataset = c("river_reservoir", "weather_station"),
                                              lon_col = NULL,
                                              lat_col = NULL) {
  dataset <- match.arg(dataset)
  if (is.null(x) || nrow(x) == 0) return(x)

  if (!"on_blm_ca" %in% names(x)) x$on_blm_ca <- NA
  if (!"dist_to_blm_mi" %in% names(x)) x$dist_to_blm_mi <- NA_real_
  if (!"dist_to_blm_ft" %in% names(x)) x$dist_to_blm_ft <- NA_real_

  cache_path <- pt_cnrfc_blm_cache_path(dataset)
  if (!file.exists(cache_path)) {
    message(
      "CNRFC ", dataset, " BLM-distance cache not found; BLM filters will be unavailable. Run 59_update_cnrfc_local_blm_distance_fields.R. Checked: ",
      cache_path
    )
    return(x)
  }

  dist <- tryCatch(
    utils::read.csv(cache_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      warning("Could not read CNRFC BLM-distance cache: ", conditionMessage(e))
      NULL
    }
  )

  needed <- c("pt_cnrfc_blm_join_key", "on_blm_ca", "dist_to_blm_mi", "dist_to_blm_ft")
  if (is.null(dist) || !all(needed %in% names(dist))) {
    message("CNRFC ", dataset, " BLM-distance cache lacks required fields; BLM filters will be unavailable. Cache: ", cache_path)
    return(x)
  }

  key <- pt_cnrfc_blm_join_key(x, dataset = dataset, lon_col = lon_col, lat_col = lat_col)
  dist <- dist[!is.na(dist$pt_cnrfc_blm_join_key) & nzchar(dist$pt_cnrfc_blm_join_key), , drop = FALSE]
  dist <- dist[!duplicated(dist$pt_cnrfc_blm_join_key), , drop = FALSE]
  idx <- match(key, dist$pt_cnrfc_blm_join_key)
  hit <- !is.na(idx)

  boolish <- function(v) {
    if (is.logical(v)) return(v)
    tolower(trimws(as.character(v))) %in% c("true", "t", "1", "yes", "y")
  }

  if (any(hit)) {
    x$on_blm_ca[hit] <- boolish(dist$on_blm_ca[idx[hit]])
    x$dist_to_blm_mi[hit] <- suppressWarnings(as.numeric(dist$dist_to_blm_mi[idx[hit]]))
    x$dist_to_blm_ft[hit] <- suppressWarnings(as.numeric(dist$dist_to_blm_ft[idx[hit]]))
  }

  message(
    "CNRFC ", dataset, " BLM-distance cache joined: ",
    format(sum(!is.na(suppressWarnings(as.numeric(x$dist_to_blm_mi)))), big.mark = ","),
    " of ", format(nrow(x), big.mark = ","), " rows have distance-to-BLM values; ",
    format(sum(x$on_blm_ca %in% TRUE, na.rm = TRUE), big.mark = ","), " are on BLM. Source: ", cache_path
  )

  x
}


# ==== 1C. Dense-layer browser popup templating ===============================
##
## PURPOSE:
##   Some dense local layers are expensive because every feature carries a full
##   prebuilt popup_html string.  USGS Wells is the first browser-template
##   prototype: keep compact per-well gwpop_* values in the cache, attach a
##   shared JavaScript popup template once, and build popup HTML only when a
##   user clicks a well.

pt_add_usgs_well_template_popups <- function(m, popup_data) {

  if (is.null(popup_data) || nrow(popup_data) == 0) {
    return(m)
  }

  popup_data <- as.data.frame(popup_data, stringsAsFactors = FALSE)

  if (!"site_no" %in% names(popup_data) && "gwpop_site_no" %in% names(popup_data)) {
    popup_data$site_no <- popup_data$gwpop_site_no
  }

  if (!"site_no" %in% names(popup_data)) {
    warning("USGS Wells template popups skipped: site_no not available.")
    return(m)
  }

  popup_data$site_no <- as.character(popup_data$site_no)
  popup_data <- popup_data[!is.na(popup_data$site_no) & popup_data$site_no != "", , drop = FALSE]

  if (nrow(popup_data) == 0) {
    return(m)
  }

  js <- r"(
function(el, x, data) {
  var map = this;

  function rowsToArray(rows) {
    if (!rows) return [];
    if (Array.isArray(rows)) return rows;

    if (typeof rows === 'object') {
      var keys = Object.keys(rows);
      if (keys.length === 0) return [];

      var n = 0;
      for (var k = 0; k < keys.length; k++) {
        if (Array.isArray(rows[keys[k]])) {
          n = rows[keys[k]].length;
          break;
        }
      }

      var out = [];
      for (var i = 0; i < n; i++) {
        var rec = {};
        for (var j = 0; j < keys.length; j++) {
          var key = keys[j];
          rec[key] = Array.isArray(rows[key]) ? rows[key][i] : rows[key];
        }
        out.push(rec);
      }
      return out;
    }

    return [];
  }

  function has(v) {
    if (v === null || v === undefined) return false;
    var s = String(v).trim();
    return s !== '' && s !== 'NA' && s !== 'NaN' && s !== 'null' && s !== 'undefined';
  }

  function esc(v) {
    if (!has(v)) return 'NA';
    return String(v)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  function isTrue(v) {
    if (v === true) return true;
    if (v === false || v === null || v === undefined) return false;
    var s = String(v).trim().toLowerCase();
    return s === 'true' || s === 't' || s === '1' || s === 'yes' || s === 'y';
  }

  function fmtNum(v, digits) {
    var n = parseFloat(v);
    if (!isFinite(n)) return 'NA';
    return n.toLocaleString(undefined, {
      minimumFractionDigits: digits,
      maximumFractionDigits: digits
    });
  }

  function makePopup(r) {
    var parts = [];
    var sid = has(r.gwpop_site_no) ? r.gwpop_site_no : r.site_no;
    var nestedN = parseInt(r.gwpop_nested_n, 10);
    var inOps = isTrue(r.gwpop_in_ops);
    var syncedOps = isTrue(r.gwpop_synced_ops);
    var wl = parseFloat(r.gwpop_latest_wl);

    parts.push('<b>' + esc(sid) + '</b> – ' + esc(r.gwpop_name));
    parts.push('<b>Elevation:</b> ' + esc(r.gwpop_elev_ft) + ' ft');
    parts.push('<b>Type:</b> ' + esc(r.gwpop_site_type));
    parts.push('<b>USGS source status:</b> ' + esc(r.gwpop_status));
    parts.push('<b>Record count:</b> ' + esc(r.gwpop_count_nu));

    if (isFinite(nestedN) && nestedN > 1) {
      parts.push('<b>Co-located/nested group:</b> ' + nestedN + ' USGS well records share this mapped coordinate.');
    }

    if (inOps) {
      parts.push('<b>Recent feed:</b> included in Ops Live groundwater layer; pink ring indicates Ops Live membership.');
    } else {
      parts.push('<b>Recent feed:</b> not currently included in Ops Live groundwater layer.');
    }

    if (isFinite(wl)) {
      var sourceNote;
      if (syncedOps) {
        sourceNote = 'Source: Ops Live groundwater feed (USGS field measurements).';
      } else {
        sourceNote = 'Source: static cached USGS lookup' +
          (has(r.gwpop_latest_run) ? '; lookup run ' + esc(r.gwpop_latest_run) : '') +
          '.';
      }

      parts.push(
        '<b>Most recent groundwater level:</b> ' +
        fmtNum(wl, 1) +
        ' ft bgs' +
        (has(r.gwpop_latest_date) ? ' on ' + esc(r.gwpop_latest_date) : '') +
        '<br/><span style="font-size:11px; color:#555;">' + sourceNote + '</span>'
      );
    }

    parts.push('<b>Well depth:</b> ' + esc(r.gwpop_well_depth) + ' ft');
    parts.push('<b>Hole depth:</b> ' + esc(r.gwpop_hole_depth) + ' ft');
    parts.push('<b>Aquifer:</b> ' + esc(r.gwpop_aquifer));
    parts.push('<b>Aquifer type:</b> ' + esc(r.gwpop_aquifer_type));
    parts.push('<b>National aquifer:</b> ' + esc(r.gwpop_nat_aqfr));
    parts.push('<b>Period:</b> ' + esc(r.gwpop_start_date) + ' to ' + esc(r.gwpop_end_date));
    parts.push('<b>Common data:</b> ' + esc(r.gwpop_common_data));
    parts.push('<span style="font-size:11px; color:#555;">Additional parameters may be available from USGS.</span>');

    if (has(sid)) {
      parts.push('<a href="https://waterdata.usgs.gov/monitoring-location/' + encodeURIComponent(String(sid)) + '" target="_blank">USGS Site</a>');
    }

    return parts.join('<br/>');
  }

  function getLayerId(layer) {
    if (!layer || !layer.options) return null;
    if (layer.options.layerId !== undefined && layer.options.layerId !== null) return String(layer.options.layerId);
    if (layer.options.layer_id !== undefined && layer.options.layer_id !== null) return String(layer.options.layer_id);
    return null;
  }

  var rows = rowsToArray(data && data.rows);
  var lookup = {};

  for (var i = 0; i < rows.length; i++) {
    var rec = rows[i];
    var id = has(rec.site_no) ? rec.site_no : rec.gwpop_site_no;
    if (has(id)) lookup[String(id)] = rec;
  }

  function bindOne(layer) {
    var id = getLayerId(layer);
    if (!id || !lookup[id] || layer._pt2UsgsWellTemplatePopupBound) return;
    if (typeof layer.bindPopup !== 'function') return;

    layer._pt2UsgsWellTemplatePopupBound = true;
    layer.bindPopup(function() {
      return makePopup(lookup[id]);
    }, {
      maxWidth: 360
    });
  }

  function visit(layer) {
    if (!layer) return;
    bindOne(layer);
    if (typeof layer.eachLayer === 'function') {
      layer.eachLayer(visit);
    }
  }

  function bindAll() {
    if (!map || typeof map.eachLayer !== 'function') return;
    map.eachLayer(visit);
  }

  if (map && typeof map.whenReady === 'function') {
    map.whenReady(function() {
      window.setTimeout(bindAll, 0);
      window.setTimeout(bindAll, 500);
    });
  } else {
    window.setTimeout(bindAll, 0);
    window.setTimeout(bindAll, 500);
  }

  if (map && typeof map.on === 'function') {
    map.on('overlayadd', function(e) {
      window.setTimeout(function() {
        if (e && e.layer) visit(e.layer);
        bindAll();
      }, 50);
    });
  }
}
)"

  htmlwidgets::onRender(
    m,
    js,
    data = list(rows = popup_data)
  )
}


pt_add_usgs_stream_template_popups <- function(m, popup_data) {

  if (is.null(popup_data) || nrow(popup_data) == 0) {
    return(m)
  }

  popup_data <- as.data.frame(popup_data, stringsAsFactors = FALSE)

  if (!"site_no" %in% names(popup_data) && "usgsswpop_site_no" %in% names(popup_data)) {
    popup_data$site_no <- popup_data$usgsswpop_site_no
  }

  if (!"site_no" %in% names(popup_data)) {
    warning("USGS Streamgages template popups skipped: site_no not available.")
    return(m)
  }

  popup_data$site_no <- as.character(popup_data$site_no)
  popup_data <- popup_data[!is.na(popup_data$site_no) & popup_data$site_no != "", , drop = FALSE]

  if (nrow(popup_data) == 0) {
    return(m)
  }

  js <- r"(
function(el, x, data) {
  var map = this;

  function rowsToArray(rows) {
    if (!rows) return [];
    if (Array.isArray(rows)) return rows;

    if (typeof rows === 'object') {
      var keys = Object.keys(rows);
      if (keys.length === 0) return [];

      var n = 0;
      for (var k = 0; k < keys.length; k++) {
        if (Array.isArray(rows[keys[k]])) {
          n = rows[keys[k]].length;
          break;
        }
      }

      var out = [];
      for (var i = 0; i < n; i++) {
        var rec = {};
        for (var j = 0; j < keys.length; j++) {
          var key = keys[j];
          rec[key] = Array.isArray(rows[key]) ? rows[key][i] : rows[key];
        }
        out.push(rec);
      }
      return out;
    }

    return [];
  }

  function has(v) {
    if (v === null || v === undefined) return false;
    var s = String(v).trim();
    return s !== '' && s !== 'NA' && s !== 'NaN' && s !== 'null' && s !== 'undefined';
  }

  function esc(v) {
    if (!has(v)) return 'NA';
    return String(v)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  function makePopup(r) {
    var sid = has(r.usgsswpop_site_no) ? r.usgsswpop_site_no : r.site_no;
    var parts = [];

    parts.push('<b>' + esc(sid) + '</b> – ' + esc(r.usgsswpop_name));
    parts.push('<b>Elevation:</b> ' + esc(r.usgsswpop_elev_ft) + ' ft');
    parts.push('<b>Type:</b> ' + esc(r.usgsswpop_site_type));
    parts.push('<b>Status:</b> ' + esc(r.usgsswpop_status));
    parts.push('<b>Record count:</b> ' + esc(r.usgsswpop_count_nu));
    parts.push('<b>Period:</b> ' + esc(r.usgsswpop_start_date) + ' to ' + esc(r.usgsswpop_end_date));
    parts.push('<b>Common data:</b> ' + esc(r.usgsswpop_common_data));
    parts.push('<span style="font-size:11px; color:#555;">Additional parameters may be available from USGS.</span>');

    if (has(sid)) {
      parts.push('<a href="https://waterdata.usgs.gov/monitoring-location/' + encodeURIComponent(String(sid)) + '" target="_blank">USGS Site</a>');
    }

    parts.push('<a href="https://dashboard.waterdata.usgs.gov/app/nwd/en/" target="_blank">usgs dash</a>');
    parts.push('<a href="https://water.noaa.gov/" target="_blank">noaa nwm</a>');

    return parts.join('<br/>');
  }

  function getLayerId(layer) {
    if (!layer || !layer.options) return null;
    if (layer.options.layerId !== undefined && layer.options.layerId !== null) return String(layer.options.layerId);
    if (layer.options.layer_id !== undefined && layer.options.layer_id !== null) return String(layer.options.layer_id);
    return null;
  }

  var rows = rowsToArray(data && data.rows);
  var lookup = {};

  for (var i = 0; i < rows.length; i++) {
    var rec = rows[i];
    var id = has(rec.site_no) ? rec.site_no : rec.usgsswpop_site_no;
    if (has(id)) lookup[String(id)] = rec;
  }

  function bindOne(layer) {
    var id = getLayerId(layer);
    if (!id || !lookup[id] || layer._pt2UsgsStreamTemplatePopupBound) return;
    if (typeof layer.bindPopup !== 'function') return;

    layer._pt2UsgsStreamTemplatePopupBound = true;
    layer.bindPopup(function() {
      return makePopup(lookup[id]);
    }, {
      maxWidth: 420
    });
  }

  function visit(layer) {
    if (!layer) return;
    bindOne(layer);
    if (typeof layer.eachLayer === 'function') {
      layer.eachLayer(visit);
    }
  }

  function bindAll() {
    if (!map || typeof map.eachLayer !== 'function') return;
    map.eachLayer(visit);
  }

  if (map && typeof map.whenReady === 'function') {
    map.whenReady(function() {
      window.setTimeout(bindAll, 0);
      window.setTimeout(bindAll, 500);
    });
  } else {
    window.setTimeout(bindAll, 0);
    window.setTimeout(bindAll, 500);
  }

  if (map && typeof map.on === 'function') {
    map.on('overlayadd', function(e) {
      window.setTimeout(function() {
        if (e && e.layer) visit(e.layer);
        bindAll();
      }, 50);
    });
  }
}
)"

  htmlwidgets::onRender(
    m,
    js,
    data = list(rows = popup_data)
  )
}
