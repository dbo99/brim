# ==== leaflet_guide_helpers.r ===============================================
##
## Build-time Guide inventory, profile projection, and browser embedding.
## Runtime registries remain authoritative; the descriptive catalog can enrich
## a record but can never create, activate, or suppress a map layer.
# ============================================================================

pt_guide_or <- function(x, fallback = "") {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]]) || !nzchar(trimws(as.character(x[[1]])))) {
    return(fallback)
  }
  trimws(as.character(x[[1]]))
}

pt_guide_first <- function(...) {
  values <- list(...)
  for (value in values) {
    value <- pt_guide_or(value)
    if (nzchar(value)) return(value)
  }
  ""
}

pt_guide_basemap_ids <- function() {
  c(
    "basemap_usgs_hydrography", "basemap_usgs_topo", "basemap_usgs_national_map_imagery",
    "basemap_usgs_national_map_imagery_topo", "basemap_esri_world_topographic",
    "basemap_esri_world_street", "basemap_esri_world_imagery", "basemap_cartodb_positron",
    "basemap_openstreetmap", "basemap_none"
  )
}

pt_guide_supported_profiles <- function() {
  list(
    default = list(
      profile_id = "default",
      current_authority_path = paste0(
        "00_config/config_map_display.r::MAP_DISPLAY + ",
        "05_map_build/04_build_portatreasure2_core_map.r::OVERLAY_GROUPS"
      ),
      excluded_ids = pt_guide_basemap_ids()
    )
  )
}

pt_guide_strip_count <- function(x) {
  x <- trimws(as.character(x))
  sub("\\s+\\((~?[0-9][0-9.,]*k?|[0-9][0-9,]*)\\)$", "", x)
}

pt_guide_path_label <- function(path) paste(path, collapse = " / ")

pt_guide_aliases <- function(stable_id, title) {
  aliases <- list(
    huc2 = c("HUC 2", "hydrologic unit code 2"),
    huc4 = c("HUC 4", "hydrologic unit code 4"),
    huc6 = c("HUC 6", "hydrologic unit code 6"),
    huc8 = c("HUC 8", "hydrologic unit code 8", "watershed boundary"),
    huc10 = c("HUC 10", "hydrologic unit code 10"),
    huc12 = c("HUC 12", "hydrologic unit code 12"),
    gw_bull118 = c("Bulletin 118", "B118", "groundwater basins"),
    product_ops_usgs_groundwater = c("USGS groundwater", "USGS wells"),
    winter_storm_levels = c("snow levels", "winter storm levels"),
    nbm_qpf = c("NBM QPF", "quantitative precipitation forecast"),
    ops_radar_noaa_mrms = c("MRMS", "NOAA radar"),
    ops_scan_soil_moisture = c("SCAN", "soil climate analysis network"),
    ops_snow_pillow_swe = c("SWE", "snow water equivalent"),
    ops_cnrfc_forecast_points = c("CNRFC forecast points"),
    ops_major_water_supply_forecasts = c("water supply forecasts"),
    EXT070 = c("recent large fire perimeters", "fire and burn-scar context"),
    EXT074 = c("current operational wildfire perimeters", "WFIGS current perimeters")
  )
  key <- gsub("-", "_", stable_id, fixed = TRUE)
  out <- aliases[[key]]
  if (is.null(out)) out <- character(0)
  unique(out[nzchar(trimws(out)) & tolower(trimws(out)) != tolower(trimws(title))])
}

pt_guide_subject_vocabulary <- function() {
  c(
    "Groundwater", "Surface Water", "Water Quality", "Snow & SWE", "Soil Moisture",
    "Precipitation", "Weather & Forecasts", "Fire Weather", "Climate & Drought",
    "Fire & Burn Areas", "Ecology & Habitat", "Air Quality", "Water Rights",
    "Geology & Geophysics", "Conservation Lands & Designations",
    "Land Ownership & Administration", "Energy & Minerals", "Infrastructure & Conveyance"
  )
}

pt_guide_information_type_vocabulary <- function() {
  c(
    "Static Reference", "Live Observation", "Forecast / Outlook", "Model / Simulation",
    "Historical Context", "Screening / Derived", "External On-Demand Service", "Tool / Workflow"
  )
}

pt_guide_subject_tags <- function(stable_id, brim_section, source_theme = "") {
  stable_id <- pt_guide_or(stable_id)
  source_theme <- pt_guide_or(source_theme)
  local_rules <- list(
    "Groundwater" = c("gw_bull118", "gsp_areas", "adjudicated_gw_basins", "usgs_wells",
                      "blm_noc_drilled_wells", "mojave_2025_gw_well_inventory"),
    "Surface Water" = c(
      "cnrfc_fnf_delta", "huc2", "huc4", "huc6", "huc8", "huc10", "huc12",
      "cnrfc_stream", "usgs_streamgages", "springs", "calsim3_network", "water_districts",
      "brim_mapped_conveyance", "wild_scenic_rivers_blm_ca_lines",
      "wild_scenic_rivers_usfs_interagency_segments", "wild_scenic_river_corridors_blm_ca",
      "wild_scenic_river_corridors_usfs_lsrs",
      "wild_scenic_river_legal_status_corridors_usfs_lsrs", "cnrfc_basin_product_availability"
    ),
    "Water Quality" = "rwqcb_regions",
    "Precipitation" = c("huc2", "huc4", "huc6", "huc8", "huc10", "huc12",
                        "cnrfc_precip_weather_station_catalog"),
    "Climate & Drought" = c("huc2", "huc4", "huc6", "huc8", "huc10", "huc12"),
    "Water Rights" = c("swrcb_wr_list_official", "swrcb_pod_spatial_matches", "swrcb_name_text_candidates"),
    "Conservation Lands & Designations" = c(
      "national_scenic_historic_trails", "national_monuments", "ca_desert_ncl",
      "wilderness_study_areas", "federal_wilderness", "acec",
      "wild_scenic_rivers_blm_ca_lines", "wild_scenic_rivers_usfs_interagency_segments",
      "wild_scenic_river_corridors_blm_ca", "wild_scenic_river_corridors_usfs_lsrs",
      "wild_scenic_river_legal_status_corridors_usfs_lsrs"
    ),
    "Land Ownership & Administration" = c(
      "blm_offices", "field_office_outer", "blm_core", "blm_diffs", "gw_bull118",
      "huc2", "huc4", "huc6", "huc8", "huc10", "huc12", "county",
      "national_monuments", "ca_desert_ncl", "wilderness_study_areas", "federal_wilderness",
      "drecp", "acec", "grazing_allotments", "rwqcb_regions", "water_districts",
      "wild_scenic_rivers_blm_ca_lines", "wild_scenic_rivers_usfs_interagency_segments",
      "wild_scenic_river_corridors_blm_ca", "wild_scenic_river_corridors_usfs_lsrs",
      "wild_scenic_river_legal_status_corridors_usfs_lsrs"
    ),
    "Infrastructure & Conveyance" = c("calsim3_network", "brim_mapped_conveyance")
  )
  ops_rules <- list(
    "Groundwater" = "product-ops-usgs-groundwater",
    "Surface Water" = c(
      "ops_streamflow_multiagency", "ops_delta_snapshot", "ops_streamflow_usgs_ca",
      "ops_snow_pillow_swe", "ops_cnrfc_forecast_points",
      "ops_major_water_supply_forecasts", "ops_cdec_reservoir_storage"
    ),
    "Snow & SWE" = c("ops_snow_pillow_swe", "winter_storm_levels"),
    "Soil Moisture" = "ops_scan_soil_moisture",
    "Precipitation" = c(
      "ops_radar_iem_nexrad", "ops_radar_noaa_mrms", "ops_qpe_mrms_1hr", "ops_qpe_mrms_1day",
      "ops_qpe_mrms_3day", "ops_qpe_rfc_1day", "ops_qpe_rfc_7day",
      "product-ops-cocorahs-ca-daily", "ops_cocorahs_conus_daily", "ops_wpc_qpf_day_1",
      "ops_wpc_qpf_day_2", "ops_wpc_qpf_day_3", "ops_wpc_qpf_3day", "ops_wpc_qpf_7day",
      "ops_cpc_6_10_precipitation", "ops_cpc_8_14_precipitation",
      "product-ops-nbm-accumulated-qpf", "nbm_qpf"
    ),
    "Weather & Forecasts" = c(
      "ops_hrrr_surface_wind", "ops_alertcalifornia_cameras", "ops_alertcalifornia_viewsheds",
      "ops_radar_iem_nexrad", "ops_radar_noaa_mrms", "ops_goes_geocolor", "ops_goes_infrared",
      "ops_goes_water_vapor", "ops_modis_terra_true_color", "ops_cnrfc_forecast_points",
      "ops_major_water_supply_forecasts", "ops_nws_weather_stations", "ops_wpc_qpf_day_1",
      "ops_wpc_qpf_day_2", "ops_wpc_qpf_day_3", "ops_wpc_qpf_3day", "ops_wpc_qpf_7day",
      "ops_nws_watches_warnings_advisories", "ops_wpc_ero_day_1", "ops_wpc_ero_day_2",
      "ops_wpc_ero_day_3", "ops_nws_wfo_boundaries", "ops_cpc_6_10_temperature",
      "ops_cpc_6_10_precipitation", "ops_cpc_8_14_temperature", "ops_cpc_8_14_precipitation",
      "product-ops-nbm-accumulated-qpf", "winter_storm_levels", "nbm_qpf",
      "ops_nbm_wind_guidance", "ops_observed_metar_wind", "ops_gfs_surface_wind"
    ),
    "Climate & Drought" = c(
      "ops_us_drought_monitor", "ops_cpc_6_10_temperature", "ops_cpc_6_10_precipitation",
      "ops_cpc_8_14_temperature", "ops_cpc_8_14_precipitation"
    ),
    "Fire & Burn Areas" = c("ops_alertcalifornia_cameras", "ops_alertcalifornia_viewsheds"),
    "Land Ownership & Administration" = "ops_nws_wfo_boundaries",
    "Infrastructure & Conveyance" = "ops_delta_snapshot"
  )
  external_theme_rules <- list(
    "Groundwater" = c("Groundwater", "DWR GW", "Wells", "SWRCB GW", "C2VSIM",
                      "Land subsidence", "UIC / Aquifer Exemptions"),
    "Surface Water" = c(
      "Surface water", "Gages", "Watershed condition", "Wild & Scenic Rivers", "NOHRSC",
      "Water quality / impaired waters",
      "refuges; delivery points; water infrastructure; habitat",
      "refuges; wetlands; water deliveries; habitat"
    ),
    "Water Quality" = c("Water quality / impaired waters", "SWRCB GW", "DWR 1W"),
    "Snow & SWE" = c("NOHRSC", "Winter weather"),
    "Weather & Forecasts" = c("Convective storms", "CPC monthly / seasonal", "Fire weather",
                              "Misc./Discussion", "NOHRSC", "Winter weather"),
    "Fire Weather" = "Fire weather",
    "Climate & Drought" = c("CPC monthly / seasonal", "Sea Level Rise"),
    "Fire & Burn Areas" = c("Burn Areas", "Fire / active incidents", "Thermal hotspots",
                            "Post-fire debris flow", "Post-fire vegetation condition"),
    "Ecology & Habitat" = c(
      "BLM", "CDFW", "CDFW lands; refuges; wildlife areas; ecological reserves; conservation easements",
      "Invasive vegetation", "Post-fire vegetation condition",
      "refuges; delivery points; water infrastructure; habitat",
      "refuges; FWS administered lands; habitat; land status",
      "refuges; wetlands; water deliveries; habitat", "USFWS", "Watershed condition"
    ),
    "Air Quality" = "Air Quality / Smoke",
    "Water Rights" = "Points of Diversion",
    "Geology & Geophysics" = c("Faults / seismicity", "Geology / maps", "Land subsidence",
                               "Post-fire debris flow", "Quaternary faults", "Recent earthquakes",
                               "Recent seismicity", "Regulatory fault traces"),
    "Conservation Lands & Designations" = c(
      "CDFW lands; refuges; wildlife areas; ecological reserves; conservation easements",
      "refuges; FWS administered lands; habitat; land status",
      "refuges; wetlands; water deliveries; habitat", "Wild & Scenic Rivers"
    ),
    "Land Ownership & Administration" = c("classification", "Districts / Service", "Planning", "PLSS"),
    "Energy & Minerals" = c("Energy / Minerals", "Mines / AML", "Permitting", "Rnwble ROW",
                             "UIC / Aquifer Exemptions", "utility"),
    "Infrastructure & Conveyance" = c("transportation", "utility",
                                      "refuges; delivery points; water infrastructure; habitat")
  )
  external_id_rules <- list(
    "Surface Water" = c("EXT005", "EXT029", "EXT030", "EXT057", "EXT107", "EXT108", "EXT109",
                        "EXT110", "EXT111", "EXT112", "EXT113"),
    "Water Quality" = c("EXT029", "EXT030", "EXT053"),
    "Soil Moisture" = "EXT124",
    "Precipitation" = c("EXT140", "EXT142"),
    "Climate & Drought" = c("EXT040", "EXT041", "EXT042", "EXT055"),
    "Ecology & Habitat" = "EXT136",
    "Geology & Geophysics" = "EXT033",
    "Conservation Lands & Designations" = c("EXT131", "EXT132", "EXT135", "EXT136"),
    "Land Ownership & Administration" = c(
      "EXT006", "EXT007", "EXT015", "EXT016", "EXT017", "EXT053", "EXT131", "EXT132",
      "EXT134", "EXT135", "EXT136", "CDFW_OWNED_OPERATED_LANDS", "USFWS_NWR_BOUNDARIES"
    ),
    "Energy & Minerals" = "EXT133",
    "Infrastructure & Conveyance" = c("EXT017", "EXT045")
  )
  tool_rules <- list()
  rules <- switch(brim_section, Local = local_rules, `Ops Live` = ops_rules,
                  External = external_id_rules, Tools = tool_rules, list())
  tags <- names(Filter(function(ids) stable_id %in% ids, rules))
  if (identical(brim_section, "External")) {
    tags <- c(tags, names(Filter(function(themes) source_theme %in% themes, external_theme_rules)))
  }
  vocabulary <- pt_guide_subject_vocabulary()
  vocabulary[vocabulary %in% unique(tags)]
}

pt_guide_information_types <- function(stable_id, brim_section, source_theme = "") {
  stable_id <- pt_guide_or(stable_id)
  source_theme <- pt_guide_or(source_theme)
  forecast_ops <- c(
    "ops_hrrr_surface_wind", "ops_cnrfc_forecast_points", "ops_major_water_supply_forecasts",
    "ops_wpc_qpf_day_1", "ops_wpc_qpf_day_2", "ops_wpc_qpf_day_3", "ops_wpc_qpf_3day",
    "ops_wpc_qpf_7day", "ops_wpc_ero_day_1", "ops_wpc_ero_day_2", "ops_wpc_ero_day_3",
    "ops_cpc_6_10_temperature", "ops_cpc_6_10_precipitation", "ops_cpc_8_14_temperature",
    "ops_cpc_8_14_precipitation", "product-ops-nbm-accumulated-qpf", "winter_storm_levels",
    "nbm_qpf", "ops_nbm_wind_guidance", "ops_gfs_surface_wind"
  )
  out <- if (identical(brim_section, "External")) {
    "External On-Demand Service"
  } else if (identical(brim_section, "Ops Live")) {
    if (stable_id %in% forecast_ops) "Forecast / Outlook" else if (stable_id == "ops_nws_wfo_boundaries") "Static Reference" else "Live Observation"
  } else if (identical(brim_section, "Tools")) {
    "Tool / Workflow"
  } else {
    "Static Reference"
  }
  if (identical(brim_section, "External") && source_theme %in% c(
    "Convective storms", "CPC monthly / seasonal", "Fire weather", "Misc./Discussion", "Winter weather"
  )) out <- c(out, "Forecast / Outlook")
  if (identical(brim_section, "External") && source_theme %in% c(
    "Air Quality / Smoke", "Fire / active incidents", "Gages", "Thermal hotspots",
    "Recent earthquakes", "Recent seismicity"
  )) out <- c(out, "Live Observation")
  if (stable_id == "EXT050") out <- c(out, "Live Observation")
  if (stable_id %in% c(
    "huc8", "EXT070", "EXT072", "ops_scan_soil_moisture", "ops_snow_pillow_swe",
    "ops_streamflow_usgs_ca", "product-ops-usgs-groundwater"
  )) out <- c(out, "Historical Context")
  if (stable_id %in% c(
    "blm_diffs", "gw_bull118", "huc2", "huc4", "huc6", "huc8", "huc10", "huc12",
    "swrcb_pod_spatial_matches", "swrcb_name_text_candidates", "brim_mapped_conveyance",
    "calsim3_network", "ops_delta_snapshot", "ops_major_water_supply_forecasts",
    paste0("EXT", 108:113)
  )) out <- c(out, "Screening / Derived")
  if (stable_id %in% c(
    "calsim3_network", sprintf("EXT%03d", 31:38), "EXT057", "EXT058", "EXT059", "EXT124",
    "ops_hrrr_surface_wind", "product-ops-nbm-accumulated-qpf", "winter_storm_levels", "nbm_qpf",
    "ops_nbm_wind_guidance", "ops_gfs_surface_wind"
  )) out <- c(out, "Model / Simulation")
  if (stable_id == "tool_external_gis_overlay") out <- c(out, "External On-Demand Service")
  vocabulary <- pt_guide_information_type_vocabulary()
  vocabulary[vocabulary %in% unique(out)]
}

pt_guide_section <- function(id, title, paragraphs = character(0), items = character(0), table = NULL) {
  list(
    id = as.character(id),
    title = as.character(title),
    paragraphs = unname(as.character(paragraphs[nzchar(trimws(paragraphs))])),
    items = unname(as.character(items[nzchar(trimws(items))])),
    table = table
  )
}

pt_guide_product <- function(
  id, title, subsystem, brim_section, provider, path, subjects, information_types, family,
  summary = "", aliases = character(0), search_terms = character(0),
  sections = list(), related_article_ids = character(0),
  custom_or_non_generic = FALSE, content_tier = "STRUCTURED_BASIC",
  entity_type = "", access_hint = ""
) {
  subjects <- unique(as.character(subjects[nzchar(trimws(subjects))]))
  information_types <- unique(as.character(information_types[nzchar(trimws(information_types))]))
  list(
    kind = "Product",
    entityType = pt_guide_or(
      entity_type,
      if (identical(subsystem, "Tools")) "Tool" else "Layer"
    ),
    id = as.character(id),
    title = as.character(title),
    subsystem = as.character(subsystem),
    brimSection = as.character(brim_section),
    provider = as.character(provider),
    path = unname(as.character(path)),
    pathLabel = pt_guide_path_label(path),
    subject = pt_guide_or(subjects),
    subjectTags = unname(subjects),
    mode = as.character(information_types[[1]]),
    informationTypes = unname(information_types),
    family = as.character(family),
    summary = pt_guide_or(summary),
    accessHint = pt_guide_or(access_hint),
    aliases = unname(unique(as.character(aliases[nzchar(trimws(aliases))]))),
    searchTerms = unname(unique(as.character(search_terms[nzchar(trimws(search_terms))]))),
    sections = unname(sections),
    relatedArticleIds = unname(unique(as.character(related_article_ids))),
    relatedResourceIds = character(0),
    relatedResources = list(),
    customOrNonGeneric = isTRUE(custom_or_non_generic),
    contentTier = as.character(content_tier)
  )
}

pt_guide_descriptive_markers <- function(
  catalog_path = file.path("08_docs", "catalog", "BRIM_LAYER_CATALOG.csv")
) {
  if (!file.exists(catalog_path)) return(list())
  x <- utils::read.csv(
    catalog_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    colClasses = "character",
    na.strings = character(),
    fileEncoding = "UTF-8",
    comment.char = ""
  )
  if (nrow(x) != 26L || ncol(x) != 15L ||
      !all(c("candidate_stable_id", "architecture") %in% names(x))) {
    stop("BRIM descriptive catalog must retain its maintained 26-row, 15-field contract.", call. = FALSE)
  }
  if (anyDuplicated(x$candidate_stable_id) || any(!nzchar(trimws(x$candidate_stable_id)))) {
    stop("BRIM descriptive catalog stable IDs must be nonblank and unique.", call. = FALSE)
  }
  markers <- grepl("CUSTOM_CONTROLLER|CUSTOM_LOADER|NON_GENERIC_RENDERER", x$architecture)
  stats::setNames(as.list(markers), x$candidate_stable_id)
}

pt_guide_local_provider <- function(title) {
  text <- tolower(title)
  if (grepl("usgs", text)) return("U.S. Geological Survey")
  if (grepl("blm|acec|drecp|wilderness|grazing", text)) return("Bureau of Land Management")
  if (grepl("cnrfc|nws", text)) return("NOAA / National Weather Service")
  if (grepl("swrcb|water rights", text)) return("California State Water Resources Control Board")
  if (grepl("cdec|calsim|conveyance", text)) return("California Department of Water Resources / BRIM")
  "BRIM source data"
}

pt_guide_local_products <- function(overlay_groups, catalog_markers = list()) {
  if (!exists("LOCAL_LAYER_REGISTRY", inherits = TRUE)) {
    stop("LOCAL_LAYER_REGISTRY must be loaded before building BRIM Guide.", call. = FALSE)
  }
  groups <- unique(pt_guide_strip_count(overlay_groups))
  groups <- groups[nzchar(groups) & !grepl("^Labels\\s*(–|:)", groups)]
  registry_groups <- pt_guide_strip_count(LOCAL_LAYER_REGISTRY$canonical_group)
  if (exists("pt_layer_group_name", mode = "function")) {
    registry_groups <- pt_guide_strip_count(pt_layer_group_name(registry_groups))
  }

  products <- list()
  used_ids <- character(0)
  component_overrides <- c(
    "Channels – Wild & Scenic Rivers | BLM-CA lines" = "wild_scenic_rivers_blm_ca_lines",
    "Channels – Wild & Scenic Rivers | USFS/interagency segments" = "wild_scenic_rivers_usfs_interagency_segments",
    "Channels – Wild & Scenic River corridors | BLM-CA" = "wild_scenic_river_corridors_blm_ca",
    "Channels – Wild & Scenic River corridors | USFS/LSRS areas" = "wild_scenic_river_corridors_usfs_lsrs",
    "Channels – Wild & Scenic River legal-status corridors | USFS/LSRS" = "wild_scenic_river_legal_status_corridors_usfs_lsrs"
  )
  for (group in groups) {
    if (group %in% names(component_overrides)) {
      row_id <- unname(component_overrides[[group]])
      title <- trimws(sub("^[^–]+–\\s*", "", group))
    } else if (identical(group, "Basins – CNRFC Product Availability")) {
      row_id <- "cnrfc_basin_product_availability"
      title <- "CNRFC Product Availability"
    } else {
      hits <- which(registry_groups == group)
      if (!length(hits)) {
        stop("Unreconciled visible Local Product group: ", group, call. = FALSE)
      }
      if (length(hits) > 1L && identical(group, "Channels – CalSim3.0")) {
        row_id <- "calsim3_network"
      } else if (length(hits) == 1L) {
        row_id <- LOCAL_LAYER_REGISTRY$layer_id[[hits]]
      } else {
        stop("Ambiguous visible Local Product group: ", group, call. = FALSE)
      }
      title <- trimws(sub("^[^–]+–\\s*", "", group))
    }
    if (row_id %in% used_ids) next
    used_ids <- c(used_ids, row_id)
    category <- trimws(sub("\\s*–.*$", "", group))
    if (identical(category, "Points")) category <- "Monitoring Sites/Records"
    path <- c("Basemaps / Local Layers", category, title)
    provider <- pt_guide_local_provider(title)
    products[[length(products) + 1L]] <- pt_guide_product(
      id = row_id,
      title = title,
      subsystem = "Basemaps / Local Layers",
      brim_section = "Local",
      provider = provider,
      path = path,
      subjects = pt_guide_subject_tags(row_id, "Local"),
      information_types = pt_guide_information_types(row_id, "Local"),
      family = if (identical(category, "Monitoring Sites/Records")) "Local monitoring record" else "Local reference layer",
      aliases = pt_guide_aliases(row_id, title),
      custom_or_non_generic = isTRUE(catalog_markers[[row_id]])
    )
  }
  products
}

pt_guide_external_products <- function(
  catalog_path = file.path("00_config", "external_service_catalog.csv"),
  catalog_markers = list()
) {
  if (!file.exists(catalog_path)) stop("Missing External service catalog: ", catalog_path, call. = FALSE)
  x <- utils::read.csv(catalog_path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = character())
  required <- c(
    "agency", "program", "theme", "external_group", "external_subgroup",
    "external_layer_id", "display_name", "primary_panel", "service_type",
    "notes", "pt2_usage_note", "best_use", "useful_for_visualization"
  )
  if (!all(required %in% names(x))) stop("External service catalog is missing Guide source fields.", call. = FALSE)
  panel <- tolower(trimws(x$primary_panel))
  x <- x[panel %in% c("external", "both"), , drop = FALSE]
  if (any(!nzchar(trimws(x$external_layer_id))) || anyDuplicated(x$external_layer_id)) {
    stop("Visible External Products require unique, nonblank external_layer_id values.", call. = FALSE)
  }
  lapply(seq_len(nrow(x)), function(i) {
    row <- x[i, , drop = FALSE]
    title <- pt_guide_or(row$display_name)
    group <- pt_guide_or(row$external_group, "External")
    subgroup <- pt_guide_or(row$external_subgroup)
    path <- c("External Layers", group, if (nzchar(subgroup)) subgroup, title)
    provider <- pt_guide_first(row$agency, row$program)
    summary <- pt_guide_first(row$pt2_usage_note, row$best_use, row$notes)
    search_terms <- c(row$agency, row$program, row$service_type)
    pt_guide_product(
      id = row$external_layer_id,
      title = title,
      subsystem = "External Layers",
      brim_section = "External",
      provider = provider,
      path = path,
      subjects = pt_guide_subject_tags(
        row$external_layer_id, "External",
        row$theme
      ),
      information_types = pt_guide_information_types(
        row$external_layer_id, "External",
        row$theme
      ),
      family = paste("External", pt_guide_or(row$service_type, "service")),
      summary = summary,
      aliases = pt_guide_aliases(row$external_layer_id, title),
      search_terms = search_terms,
      custom_or_non_generic = isTRUE(catalog_markers[[row$external_layer_id]])
    )
  })
}

pt_guide_js_field <- function(prefix, field, allow_symbol = FALSE) {
  quoted <- paste0(field, "\\s*:\\s*'((?:\\\\.|[^'])*)'")
  hit <- regexec(quoted, prefix, perl = TRUE)
  value <- regmatches(prefix, hit)[[1]]
  if (length(value) >= 2L) return(gsub("\\'", "'", value[[2]], fixed = TRUE))
  if (isTRUE(allow_symbol)) {
    hit <- regexec(paste0(field, "\\s*:\\s*([A-Z][A-Z0-9_]*)"), prefix, perl = TRUE)
    value <- regmatches(prefix, hit)[[1]]
    if (length(value) >= 2L) return(value[[2]])
  }
  ""
}

pt_guide_ops_source_definitions <- function(
  source_paths = Sys.glob(file.path("03_functions", "leaflet_ops_live_*helpers.r"))
) {
  out <- list()
  for (path in sort(source_paths)) {
    source_text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    parts <- strsplit(source_text, "addOpsLayer({", fixed = TRUE)[[1]]
    if (length(parts) < 2L) next
    for (part in parts[-1]) {
      layer_pos <- regexpr("layer:", part, fixed = TRUE)[[1]]
      if (layer_pos < 1L) next
      prefix <- substr(part, 1L, layer_pos - 1L)
      token <- pt_guide_js_field(prefix, "name", allow_symbol = TRUE)
      category <- pt_guide_js_field(prefix, "category")
      subgroup <- pt_guide_js_field(prefix, "subgroup")
      if (!nzchar(token) || !nzchar(category)) {
        stop("Could not parse an addOpsLayer Product definition in ", path, call. = FALSE)
      }
      out[[length(out) + 1L]] <- list(
        source_token = token,
        category = category,
        subgroup = subgroup,
        source_file = basename(path)
      )
    }
  }
  out
}

pt_guide_ops_products <- function(map_display, catalog_markers = list()) {
  if (!isTRUE(map_display$add_ops_live_layers)) return(list())
  registry <- pt_ops_live_guide_identity_registry()
  definitions <- pt_guide_ops_source_definitions()
  tokens <- vapply(definitions, `[[`, character(1), "source_token")
  if (length(tokens) != nrow(registry) || anyDuplicated(tokens) ||
      !setequal(tokens, registry$source_token)) {
    stop("Ops Live Guide identity is not in one-to-one parity with addOpsLayer definitions.", call. = FALSE)
  }
  constant_names <- c(
    PT_ASOS_WIND_LAYER_NAME = "Observed wind | METAR/ASOS speed + gusts",
    PT_SNOW_PRODUCT_NAME = "NBM Snow Levels",
    PT_QPF_PRODUCT_NAME = "NBM 6-Hour QPF",
    PT_ACCUM_PRODUCT_NAME = "NBM Accumulated QPF (0–10 d)"
  )
  products <- list()
  for (definition in definitions) {
    i <- match(definition$source_token, registry$source_token)
    flag <- registry$map_display_flag[[i]]
    included <- isTRUE(registry$included_by_default[[i]]) &&
      (is.na(flag) || !nzchar(flag) || isTRUE(map_display[[flag]]))
    if (!included) next
    runtime_title <- definition$source_token
    if (runtime_title %in% names(constant_names)) {
      runtime_title <- unname(constant_names[[runtime_title]])
    }
    title <- runtime_title
    id <- registry$stable_id[[i]]
    if (identical(id, "ops_scan_soil_moisture")) title <- "SCAN Soil Moisture"
    path <- c("Ops Live", definition$category,
              if (nzchar(definition$subgroup)) definition$subgroup, runtime_title)
    provider <- if (grepl("USGS", title)) {
      "U.S. Geological Survey"
    } else if (grepl("USDA|SCAN", title)) {
      "USDA Natural Resources Conservation Service"
    } else if (grepl("CDEC|CalSim|CVP|SWP", title)) {
      "California water agencies / BRIM"
    } else {
      "NOAA / National Weather Service"
    }
    products[[length(products) + 1L]] <- pt_guide_product(
      id = id,
      title = title,
      subsystem = "Ops Live",
      brim_section = "Ops Live",
      provider = provider,
      path = path,
      subjects = pt_guide_subject_tags(
        id, "Ops Live"
      ),
      information_types = pt_guide_information_types(
        id, "Ops Live"
      ),
      family = definition$category,
      aliases = pt_guide_aliases(id, title),
      custom_or_non_generic = isTRUE(catalog_markers[[id]])
    )
  }
  products
}

pt_guide_basemap_products <- function() {
  if (!exists("pt_base_groups", mode = "function")) stop("pt_base_groups() is required for BRIM Guide.", call. = FALSE)
  titles <- pt_base_groups()
  ids <- pt_guide_basemap_ids()
  if (length(titles) != length(ids)) stop("Basemap Guide identity requires review after pt_base_groups() changed.", call. = FALSE)
  lapply(seq_along(titles), function(i) {
    provider <- if (grepl("USGS", titles[[i]])) "U.S. Geological Survey" else if (grepl("Esri", titles[[i]])) "Esri" else "Basemap provider"
    path <- c("Basemaps / Local Layers", "Basemaps", titles[[i]])
    pt_guide_product(
      id = ids[[i]], title = titles[[i]], subsystem = "Basemaps / Local Layers",
      brim_section = "Basemap", provider = provider, path = path,
      subjects = "Land & Administrative Context",
      information_types = "Static Reference", family = "Basemap"
    )
  })
}

pt_guide_tool_products <- function(map_display) {
  if (!isTRUE(map_display$add_tools_adddata_panel)) return(list())
  definitions <- list(
    c("tool_measure", "Distance and area measurement", "Measurement"),
    c("tool_teaching_markup", "Draw and label markup", "Teaching / markup"),
    c("tool_external_gis_overlay", "External GIS URL Overlay", "Add Data"),
    c("tool_local_gis_upload", "Local GIS File Upload", "Add Data")
  )
  products <- lapply(definitions, function(definition) {
    pt_guide_product(
      id = definition[[1]], title = definition[[2]], subsystem = "Tools",
      brim_section = "Tools", provider = "BRIM", path = character(0),
      subjects = pt_guide_subject_tags(definition[[1]], "Tools"),
      information_types = pt_guide_information_types(definition[[1]], "Tools"),
      family = definition[[3]]
    )
  })
  if (isTRUE(map_display$add_blm_sma_context_overlay)) {
    products[[length(products) + 1L]] <- pt_guide_product(
      id = "tool_blm_sma_context",
      title = "BLM Surface Management Agency context",
      subsystem = "External Layers",
      brim_section = "External",
      provider = "Bureau of Land Management",
      path = c(
        "External Layers", "Federal Land Status",
        "Fed/State Surface Management Agency (SMA)"
      ),
      subjects = pt_guide_subject_tags(
        "tool_blm_sma_context", "External", "classification"
      ),
      information_types = pt_guide_information_types(
        "tool_blm_sma_context", "External", "classification"
      ),
      family = "Land status context",
      aliases = c("BLM SMA", "Federal/State Surface Management Agency", "BLM CA land status"),
      custom_or_non_generic = TRUE,
      entity_type = "Layer"
    )
  }
  products
}

pt_guide_generalization_parameter <- function(row) {
  type <- pt_guide_or(row$parameter_type)
  value <- suppressWarnings(as.numeric(row$parameter_value))
  number <- function(x) formatC(x, format = "fg", digits = 8, drop0trailing = TRUE)
  if (identical(type, "distance_tolerance_m") && is.finite(value)) {
    return(paste0(number(value), " m distance tolerance"))
  }
  if (identical(type, "vertex_keep_fraction") && is.finite(value)) {
    return(paste0(number(100 * value), "% vertex retention"))
  }
  paste(trimws(c(type, if (is.finite(value)) number(value) else "")), collapse = ": ")
}

pt_guide_generalization_detail <- function(row) {
  paste0(
    "Current display treatment: ", pt_guide_or(row$accepted_method),
    "; ", pt_guide_generalization_parameter(row), "."
  )
}

pt_guide_generalization_table <- function(registry) {
  shown <- registry[
    tolower(registry$disclosure_required) == "yes" & nzchar(registry$public_disclosure),
    ,
    drop = FALSE
  ]
  list(
    caption = "Current public display-geometry portfolio",
    columns = list(
      list(key = "layer", label = "Layer"),
      list(key = "parameter", label = "Display parameter"),
      list(key = "disclosure", label = "Boundary-use disclosure")
    ),
    rows = lapply(seq_len(nrow(shown)), function(i) {
      row <- shown[i, , drop = FALSE]
      list(
        layer = as.character(row$brim_layer_name),
        parameter = pt_guide_generalization_parameter(row),
        disclosure = as.character(row$public_disclosure)
      )
    })
  )
}

pt_guide_article <- function(id, title, summary, sections, related_product_ids = character(0), aliases = character(0), external_links = list()) {
  list(
    kind = "Article",
    id = id,
    title = title,
    section = "Methods & Guides",
    summary = summary,
    sections = unname(sections),
    relatedProductIds = unname(unique(as.character(related_product_ids))),
    aliases = unname(unique(as.character(aliases))),
    externalLinks = unname(external_links)
  )
}

pt_guide_resource_registry_assert_fields <- function(
    record, required, allowed = required, label = "Guide Resource registry record") {
  if (!is.list(record) || is.null(names(record)) || anyDuplicated(names(record))) {
    stop(label, " must be an object with unique field names.", call. = FALSE)
  }
  prohibited <- grep(
    "(^raw_|bookmark|candidate|provenance|source_record|machine_path|local_path|relationship|related.*product|profile|lifecycle|freshness|status)",
    names(record), ignore.case = TRUE, perl = TRUE, value = TRUE
  )
  if (length(prohibited)) {
    stop(label, " contains prohibited authority/provenance field(s): ",
         paste(prohibited, collapse = ", "), call. = FALSE)
  }
  missing <- setdiff(required, names(record))
  unknown <- setdiff(names(record), allowed)
  if (length(missing)) {
    stop(label, " is missing required field(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (length(unknown)) {
    stop(label, " contains unsupported field(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  invisible(record)
}

pt_guide_resource_registry_scalar <- function(value, label, allow_empty = FALSE) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      (!allow_empty && !nzchar(value)) || !identical(value, trimws(value))) {
    stop(label, " must be one ", if (allow_empty) "trimmed" else "nonblank trimmed",
         " string.", call. = FALSE)
  }
  value
}

pt_guide_resource_registry_string_array <- function(value, label) {
  if (!is.list(value) || (!is.null(names(value)) && any(nzchar(names(value))))) {
    stop(label, " must be an array of strings.", call. = FALSE)
  }
  if (!length(value)) return(character(0))
  unname(vapply(value, pt_guide_resource_registry_scalar, character(1), label = label))
}

pt_guide_resource_metadata_vocabularies <- function() {
  list(
    resource_type = c(
      program_or_mission = "Program or mission",
      dataset_or_collection = "Dataset or collection",
      data_portal_or_catalog = "Data portal or catalog",
      viewer_or_explorer = "Viewer or explorer",
      dashboard = "Dashboard",
      analysis_tool = "Analysis tool",
      data_service_or_api = "Data service or API",
      documentation_or_guide = "Documentation or guide",
      organization_homepage = "Organization homepage",
      report_or_publication = "Report or publication"
    ),
    temporal_character = c(
      current_or_near_real_time = "Current or near-real-time",
      forecast = "Forecast",
      historical_archive = "Historical archive",
      climatology_or_normals = "Climatology or normals",
      static_reference = "Static reference",
      mixed = "Mixed",
      unknown = "Unknown"
    ),
    geographic_scope_class = c(
      global = "Global",
      multinational = "Multinational",
      national = "National",
      multi_state = "Multi-state",
      state = "State",
      regional = "Regional",
      local = "Local",
      unknown = "Unknown"
    )
  )
}

pt_guide_resource_metadata_label <- function(vocabulary, value, label) {
  result <- unname(vocabulary[[value]])
  if (!is.character(result) || length(result) != 1L || !nzchar(result)) {
    stop(label, " has no controlled display label.", call. = FALSE)
  }
  result
}

pt_guide_validate_public_https_url <- function(url, label) {
  url <- pt_guide_resource_registry_scalar(url, label)
  if (!grepl("^https://[^/?#]+(?:[/?#]|$)", url, perl = TRUE)) {
    stop(label, " must use a public https:// URL.", call. = FALSE)
  }
  authority <- sub("^https://", "", url)
  authority <- sub("[/?#].*$", "", authority)
  if (grepl("@|%40|%3a", authority, ignore.case = TRUE, perl = TRUE)) {
    stop(label, " must not contain URL credentials.", call. = FALSE)
  }
  host <- tolower(sub(":(?:[0-9]+)$", "", authority, perl = TRUE))
  host <- sub("\\.$", "", host)
  private_host <- grepl(
    paste0(
      "^(localhost|0(?:\\.|$)|127(?:\\.|$)|10(?:\\.|$)|192\\.168(?:\\.|$)|",
      "172\\.(?:1[6-9]|2[0-9]|3[01])(?:\\.|$)|169\\.254(?:\\.|$)|",
      "100\\.(?:6[4-9]|[7-9][0-9]|1[01][0-9]|12[0-7])(?:\\.|$)|",
      "\\[(?:::1|f[cd][0-9a-f:]*|fe[89ab][0-9a-f:]*)\\]$)"
    ),
    host, ignore.case = TRUE, perl = TRUE
  ) || grepl("(?:^|[.-])(?:internal|private|restricted)(?:[.-]|$)|\\.(?:local|lan|home|test|invalid)$",
             host, ignore.case = TRUE, perl = TRUE) ||
    (!grepl("\\.", host) && !grepl("^\\[[0-9a-f:]+\\]$", host, ignore.case = TRUE, perl = TRUE))
  if (private_host) stop(label, " must not use a local, private, or restricted host.", call. = FALSE)
  query <- if (grepl("?", url, fixed = TRUE)) sub("^[^?]*\\?", "", url) else ""
  if (nzchar(query) && grepl(
    "(?:^|&)(?:x-amz-[^=]*|x-goog-[^=]*|awsaccesskeyid|googleaccessid|signature|sig|token|access_token|api[_-]?key|credential|authorization|auth|expires?|policy|key-pair-id|se|sp|sv)=",
    query, ignore.case = TRUE, perl = TRUE
  )) stop(label, " must not contain credentials, tokens, or signed-query material.", call. = FALSE)
  url
}

pt_guide_http_only_resource_url_exceptions <- function() {
  c(
    resource_tid_turlock_irrigation_district_wiski_web_platform =
      "http://wiskiweb.tid.org/index.htm",
    resource_krwa_kings_river_water_association_platform =
      "http://kingsriverwater.org/",
    resource_ocpw_orange_county_hydrology_data_portal_platform =
      "http://hydstra.ocpublicworks.com/web.htm"
  )
}

pt_guide_validate_public_resource_url <- function(url, label, resource_id) {
  url <- pt_guide_resource_registry_scalar(url, label)
  resource_id <- pt_guide_resource_registry_scalar(
    resource_id, paste0(label, " Resource ID")
  )
  if (grepl("^https://", url)) {
    return(pt_guide_validate_public_https_url(url, label))
  }

  exceptions <- pt_guide_http_only_resource_url_exceptions()
  approved_url <- unname(exceptions[resource_id])
  if (length(approved_url) != 1L || is.na(approved_url) ||
      !identical(url, approved_url)) {
    stop(
      label,
      " must use a public https:// URL or an exact reviewed HTTP-only Resource exception.",
      call. = FALSE
    )
  }

  pt_guide_validate_public_https_url(
    sub("^http://", "https://", url), paste0(label, " HTTP-only exception")
  )
  url
}

pt_guide_read_resource_registry <- function(
    path = file.path("00_config", "guide_resources.json")) {
  if (!file.exists(path)) {
    stop("Missing canonical Guide Resource registry: ", path, call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required by the existing BRIM Guide compiler.", call. = FALSE)
  }
  source <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(error) stop(
      "Malformed canonical Guide Resource registry: ", conditionMessage(error), call. = FALSE
    )
  )
  pt_guide_resource_registry_assert_fields(
    source, c("schema_version", "resources"), label = "Guide Resource registry"
  )
  if (!identical(source$schema_version, 3L) || !is.list(source$resources) ||
      !length(source$resources)) {
    stop("Guide Resource registry must use schema_version 3 and a nonempty resources array.",
         call. = FALSE)
  }

  required <- c(
    "id", "aliases", "migration_aliases", "search_aliases", "order", "title",
    "providers", "summary", "canonical_url", "access_points", "resource_type",
    "temporal_character", "resource_granularity", "subject_tags",
    "information_type_tags", "variables", "use_scopes", "geographic_scope",
    "access_class", "public_source_references", "publication_state"
  )
  id_pattern <- "^resource_[a-z0-9]+(?:_[a-z0-9]+)*$"
  migration_id_pattern <- "^res\\.[a-z0-9]+(?:[.-][a-z0-9]+)*$"
  provider_roles <- c(
    "display_provider", "publisher", "maintainer", "partner", "data_owner", "program"
  )
  access_roles <- c("canonical", "configured_view", "archive", "comparison_viewer")
  reference_roles <- c("official_source", "documentation")
  metadata_vocabularies <- pt_guide_resource_metadata_vocabularies()
  resource_types <- names(metadata_vocabularies$resource_type)
  temporal_characters <- names(metadata_vocabularies$temporal_character)
  resource_granularities <- c(
    "unknown", "collection", "dashboard", "dataset", "mission", "platform",
    "product", "program", "viewer"
  )
  publication_states <- c("published", "staged")
  geographic_scope_types <- names(metadata_vocabularies$geographic_scope_class)
  ids <- character(length(source$resources))
  aliases <- vector("list", length(source$resources))
  migration_aliases <- vector("list", length(source$resources))
  orders <- integer(length(source$resources))

  for (index in seq_along(source$resources)) {
    record <- source$resources[[index]]
    label <- paste0("Guide Resource registry record ", index)
    pt_guide_resource_registry_assert_fields(record, required, label = label)
    ids[[index]] <- pt_guide_resource_registry_scalar(record$id, paste0(label, " id"))
    if (!grepl(id_pattern, ids[[index]], perl = TRUE)) {
      stop(label, " id must use the resource_* stable-ID syntax.", call. = FALSE)
    }
    aliases[[index]] <- pt_guide_resource_registry_string_array(
      record$aliases, paste0(label, " aliases")
    )
    if (any(!grepl(id_pattern, aliases[[index]], perl = TRUE)) ||
        anyDuplicated(aliases[[index]])) {
      stop(label, " aliases must be unique resource_* stable IDs.", call. = FALSE)
    }
    migration_aliases[[index]] <- pt_guide_resource_registry_string_array(
      record$migration_aliases, paste0(label, " migration_aliases")
    )
    if (any(!grepl(migration_id_pattern, migration_aliases[[index]], perl = TRUE)) ||
        anyDuplicated(migration_aliases[[index]])) {
      stop(label, " migration_aliases must be unique reviewed res.* intake keys.",
           call. = FALSE)
    }
    search_aliases <- pt_guide_resource_registry_string_array(
      record$search_aliases, paste0(label, " search_aliases")
    )
    if (anyDuplicated(search_aliases)) {
      stop(label, " search_aliases must be unique within the Resource.", call. = FALSE)
    }
    if (!is.numeric(record$order) || length(record$order) != 1L ||
        is.na(record$order) || !is.finite(record$order) || record$order != as.integer(record$order) ||
        record$order < 1) {
      stop(label, " order must be one positive integer.", call. = FALSE)
    }
    orders[[index]] <- as.integer(record$order)
    pt_guide_resource_registry_scalar(record$title, paste0(label, " title"))
    pt_guide_resource_registry_scalar(record$summary, paste0(label, " summary"))
    canonical_url <- pt_guide_validate_public_resource_url(
      record$canonical_url, paste0(label, " canonical_url"), ids[[index]]
    )
    resource_type <- pt_guide_resource_registry_scalar(
      record$resource_type, paste0(label, " resource_type")
    )
    temporal_character <- pt_guide_resource_registry_scalar(
      record$temporal_character, paste0(label, " temporal_character")
    )
    granularity <- pt_guide_resource_registry_scalar(
      record$resource_granularity, paste0(label, " resource_granularity")
    )
    if (!resource_type %in% resource_types ||
        !temporal_character %in% temporal_characters ||
        !granularity %in% resource_granularities) {
      stop(label, paste0(
        " contains an uncontrolled Resource type, temporal character, or granularity."
      ), call. = FALSE)
    }

    if (!is.list(record$providers) || !length(record$providers)) {
      stop(label, " requires a providers array.", call. = FALSE)
    }
    providers <- lapply(seq_along(record$providers), function(provider_index) {
      provider <- record$providers[[provider_index]]
      provider_label <- paste0(label, " provider ", provider_index)
      pt_guide_resource_registry_assert_fields(
        provider, c("name", "role"), label = provider_label
      )
      name <- pt_guide_resource_registry_scalar(
        provider$name, paste0(provider_label, " name")
      )
      role <- pt_guide_resource_registry_scalar(provider$role, paste0(provider_label, " role"))
      if (!role %in% provider_roles) stop(provider_label, " has an uncontrolled role.", call. = FALSE)
      list(name = name, role = role)
    })
    provider_role_values <- vapply(providers, `[[`, character(1), "role")
    provider_pairs <- vapply(
      providers, function(provider) paste(provider$name, provider$role, sep = "\r"), character(1)
    )
    if (sum(provider_role_values == "display_provider") != 1L ||
        anyDuplicated(provider_pairs)) {
      stop(label, paste0(
        " requires exactly one display_provider and unique provider (name, role) pairs."
      ), call. = FALSE)
    }

    if (!is.list(record$access_points) || !length(record$access_points)) {
      stop(label, " requires an access_points array.", call. = FALSE)
    }
    access <- lapply(seq_along(record$access_points), function(access_index) {
      point <- record$access_points[[access_index]]
      point_label <- paste0(label, " access point ", access_index)
      pt_guide_resource_registry_assert_fields(
        point, c("role", "label", "url"), label = point_label
      )
      role <- pt_guide_resource_registry_scalar(point$role, paste0(point_label, " role"))
      if (!role %in% access_roles) stop(point_label, " has an uncontrolled role.", call. = FALSE)
      list(
        role = role,
        label = pt_guide_resource_registry_scalar(point$label, paste0(point_label, " label")),
        url = pt_guide_validate_public_resource_url(
          point$url, paste0(point_label, " url"), ids[[index]]
        )
      )
    })
    access_role_values <- vapply(access, `[[`, character(1), "role")
    access_urls <- vapply(access, `[[`, character(1), "url")
    if (sum(access_role_values == "canonical") != 1L || anyDuplicated(access_urls) ||
        !identical(access[[match("canonical", access_role_values)]]$url, canonical_url)) {
      stop(label, paste0(
        " requires unique access-point URLs and one canonical access point matching canonical_url."
      ), call. = FALSE)
    }

    subject_tags <- pt_guide_resource_registry_string_array(
      record$subject_tags, paste0(label, " subject_tags")
    )
    if (any(!subject_tags %in% pt_guide_subject_vocabulary()) || anyDuplicated(subject_tags)) {
      stop(label, " contains an uncontrolled or duplicate subject tag.", call. = FALSE)
    }
    information_type_tags <- pt_guide_resource_registry_string_array(
      record$information_type_tags, paste0(label, " information_type_tags")
    )
    if (any(!information_type_tags %in% pt_guide_information_type_vocabulary()) ||
        anyDuplicated(information_type_tags)) {
      stop(label, " contains an uncontrolled or duplicate Information Type tag.",
           call. = FALSE)
    }
    variables <- pt_guide_resource_registry_string_array(
      record$variables, paste0(label, " variables")
    )
    use_scopes <- pt_guide_resource_registry_string_array(
      record$use_scopes, paste0(label, " use_scopes")
    )
    if (anyDuplicated(variables) || anyDuplicated(use_scopes)) {
      stop(label, " variables and use_scopes must be unique within the Resource.",
           call. = FALSE)
    }
    pt_guide_resource_registry_assert_fields(
      record$geographic_scope, c("scope_type", "names"),
      label = paste0(label, " geographic_scope")
    )
    geographic_scope_type <- pt_guide_resource_registry_scalar(
      record$geographic_scope$scope_type, paste0(label, " geographic_scope scope_type")
    )
    geographic_names <- pt_guide_resource_registry_string_array(
      record$geographic_scope$names, paste0(label, " geographic_scope names")
    )
    geographic_name_keys <- tolower(iconv(
      geographic_names, from = "UTF-8", to = "ASCII//TRANSLIT", sub = ""
    ))
    if (!geographic_scope_type %in% geographic_scope_types ||
        any(is.na(geographic_name_keys)) || anyDuplicated(geographic_name_keys)) {
      stop(label, " contains an uncontrolled or inconsistent geographic scope.", call. = FALSE)
    }
    access_class <- pt_guide_resource_registry_scalar(
      record$access_class, paste0(label, " access_class")
    )
    if (!identical(access_class, "public")) {
      stop(label, " access_class must be public.", call. = FALSE)
    }
    publication_state <- pt_guide_resource_registry_scalar(
      record$publication_state, paste0(label, " publication_state")
    )
    if (!publication_state %in% publication_states) {
      stop(label, " has an uncontrolled publication_state.", call. = FALSE)
    }

    if (!is.list(record$public_source_references)) {
      stop(label, " public_source_references must be an array.", call. = FALSE)
    }
    if (length(record$public_source_references)) {
      reference_roles_seen <- vapply(
        seq_along(record$public_source_references),
        function(reference_index) {
          reference <- record$public_source_references[[reference_index]]
          reference_label <- paste0(label, " public source reference ", reference_index)
          pt_guide_resource_registry_assert_fields(
            reference, c("url", "role"), label = reference_label
          )
          role <- pt_guide_resource_registry_scalar(
            reference$role, paste0(reference_label, " role")
          )
          if (!role %in% reference_roles) {
            stop(reference_label, " has an uncontrolled role.", call. = FALSE)
          }
          pt_guide_validate_public_resource_url(
            reference$url, paste0(reference_label, " url"), ids[[index]]
          )
          role
        },
        character(1)
      )
      if (anyDuplicated(reference_roles_seen)) {
        stop(label, " public source reference roles must be unique.", call. = FALSE)
      }
    }
  }

  all_ids <- c(ids, unlist(aliases, use.names = FALSE))
  if (anyDuplicated(ids) || anyDuplicated(all_ids)) {
    stop("Guide Resource registry primary IDs and aliases must be globally unique.", call. = FALSE)
  }
  all_migration_aliases <- unlist(migration_aliases, use.names = FALSE)
  if (anyDuplicated(all_migration_aliases)) {
    stop("Guide Resource registry migration aliases must be globally unique.", call. = FALSE)
  }
  if (anyDuplicated(orders) || !identical(orders, seq_along(source$resources))) {
    stop("Guide Resource registry order must be unique, complete, and match file order.", call. = FALSE)
  }
  values <- as.character(unlist(source, recursive = TRUE, use.names = FALSE))
  if (any(grepl("(^|[[:space:]\"'=])/(Users|home|private|tmp|var|Volumes)/|(^|[[:space:]\"'=])[A-Za-z]:[\\\\/]",
                values, ignore.case = TRUE, perl = TRUE))) {
    stop("Guide Resource registry contains a machine-local filesystem path.", call. = FALSE)
  }
  if (any(grepl("BEGIN (?:RSA |EC |OPENSSH )?PRIVATE KEY|bearer[[:space:]]+[A-Za-z0-9._~-]+|password[[:space:]]*[:=]|secret[[:space:]]*[:=]",
                values, ignore.case = TRUE, perl = TRUE))) {
    stop("Guide Resource registry contains credentials or secret material.", call. = FALSE)
  }
  structure(unname(source$resources), class = c("pt_guide_resource_registry", "list"))
}

pt_guide_resource_published_records <- function(registry) {
  if (!inherits(registry, "pt_guide_resource_registry")) {
    stop("Guide Resource publication projection requires a validated registry.", call. = FALSE)
  }
  publication_states <- vapply(registry, function(record) {
    pt_guide_or(record$publication_state)
  }, character(1))
  if (any(!publication_states %in% c("published", "staged"))) {
    stop("Guide Resource publication projection encountered an unknown publication state.",
         call. = FALSE)
  }
  records <- unclass(registry)
  structure(
    unname(records[publication_states == "published"]),
    class = c("pt_guide_resource_registry", "list")
  )
}

pt_guide_normalize_resource_search <- function(values) {
  value <- paste(unname(as.character(unlist(values, recursive = TRUE, use.names = FALSE))),
                 collapse = " ")
  value <- iconv(value, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  if (is.na(value)) {
    stop("Guide Resource search text could not be normalized.", call. = FALSE)
  }
  value <- gsub("[^a-z0-9]+", " ", tolower(value), perl = TRUE)
  trimws(gsub("[[:space:]]+", " ", value, perl = TRUE))
}

pt_guide_resource_browser_records <- function(
    registry, products, relationship_registry) {
  if (!inherits(registry, "pt_guide_resource_registry")) {
    stop("Guide Resource browser adaptation requires a validated registry.",
         call. = FALSE)
  }
  if (any(vapply(registry, function(record) {
    !identical(pt_guide_or(record$publication_state), "published")
  }, logical(1)))) {
    stop("Guide Resource browser adaptation accepts published records only.", call. = FALSE)
  }
  if (!is.list(products)) {
    stop("Guide Resource browser adaptation requires eligible projected Products.",
         call. = FALSE)
  }
  if (!inherits(
        relationship_registry,
        "pt_guide_product_resource_relationship_registry"
      )) {
    stop("Guide Resource browser adaptation requires the validated relationship authority.",
         call. = FALSE)
  }
  product_ids <- vapply(products, function(product) pt_guide_or(product$id), character(1))
  if (any(!nzchar(product_ids)) || anyDuplicated(product_ids)) {
    stop("Guide Resource browser adaptation requires unique eligible Product IDs.",
         call. = FALSE)
  }
  metadata_vocabularies <- pt_guide_resource_metadata_vocabularies()
  relationship_products <- relationship_registry$products
  relationship_resources <- relationship_registry$resources
  relationship_product_ids <- vapply(
    relationship_products, `[[`, character(1), "product_id"
  )
  relationship_resource_ids <- vapply(
    relationship_resources, `[[`, character(1), "resource_id"
  )
  published_resource_ids <- vapply(registry, `[[`, character(1), "id")
  resource_titles <- stats::setNames(
    vapply(registry, `[[`, character(1), "title"),
    published_resource_ids
  )
  lapply(registry, function(record) {
    display_providers <- Filter(
      function(provider) identical(provider$role, "display_provider"), record$providers
    )
    represented_products <- list()
    for (product in products) {
      relationship_record <- relationship_products[[match(
        product$id, relationship_product_ids
      )]]
      if (is.null(relationship_record)) {
        stop("Guide Resource browser adaptation found a Product outside relationship authority.",
             call. = FALSE)
      }
      source_resource_ids <- intersect(
        published_resource_ids,
        vapply(
          relationship_record$resource_links,
          `[[`,
          character(1),
          "resource_id"
        )
      )
      for (relationship in relationship_record$resource_links) {
        if (!identical(relationship$resource_id, record$id)) next
        represented_products[[length(represented_products) + 1L]] <- list(
          productId = product$id,
          title = product$title,
          deliveryClass = relationship_record$delivery_class,
          coverageDisposition = relationship_record$coverage_disposition,
          relationshipRole = relationship$relationship_role,
          sourceResourceIds = unname(source_resource_ids)
        )
      }
    }
    representation_record <- relationship_resources[[match(
      record$id, relationship_resource_ids
    )]]
    if (is.null(representation_record)) {
      stop("Guide Resource browser adaptation found a Resource outside relationship authority.",
           call. = FALSE)
    }
    aliases <- unname(as.character(unlist(record$search_aliases, use.names = FALSE)))
    providers <- lapply(record$providers, function(provider) list(
      name = provider$name,
      role = provider$role
    ))
    access_points <- lapply(record$access_points, function(point) list(
      role = point$role,
      label = if (identical(point$role, "canonical")) "Official Resource" else point$label,
      url = point$url
    ))
    resource_type_label <- pt_guide_resource_metadata_label(
      metadata_vocabularies$resource_type, record$resource_type,
      paste0("Guide Resource ", record$id, " resource_type")
    )
    temporal_character_label <- pt_guide_resource_metadata_label(
      metadata_vocabularies$temporal_character, record$temporal_character,
      paste0("Guide Resource ", record$id, " temporal_character")
    )
    geographic_scope <- list(
      scopeType = record$geographic_scope$scope_type,
      scopeLabel = pt_guide_resource_metadata_label(
        metadata_vocabularies$geographic_scope_class,
        record$geographic_scope$scope_type,
        paste0("Guide Resource ", record$id, " geographic_scope scope_type")
      ),
      names = unname(as.character(unlist(record$geographic_scope$names, use.names = FALSE)))
    )
    search_text <- pt_guide_normalize_resource_search(list(
      record$title,
      aliases,
      display_providers[[1]]$name,
      vapply(providers, `[[`, character(1), "name"),
      record$summary,
      vapply(access_points, `[[`, character(1), "label"),
      resource_type_label,
      record$resource_granularity,
      unlist(record$subject_tags, use.names = FALSE),
      unlist(record$information_type_tags, use.names = FALSE),
      unlist(record$variables, use.names = FALSE),
      unlist(record$use_scopes, use.names = FALSE),
      geographic_scope$scopeLabel,
      geographic_scope$names,
      vapply(represented_products, `[[`, character(1), "title"),
      unname(resource_titles[published_resource_ids %in% unlist(lapply(
        Filter(function(product) {
          identical(product$coverageDisposition, "multiple_source_resources")
        }, represented_products),
        `[[`, "sourceResourceIds"
      ), use.names = FALSE)])
    ))
    list(
      kind = "Resource",
      id = record$id,
      title = record$title,
      aliases = aliases,
      provider = display_providers[[1]]$name,
      providers = unname(providers),
      summary = record$summary,
      canonicalUrl = record$canonical_url,
      accessPoints = unname(access_points),
      resourceType = record$resource_type,
      resourceTypeLabel = resource_type_label,
      temporalCharacter = record$temporal_character,
      temporalCharacterLabel = temporal_character_label,
      resourceGranularity = record$resource_granularity,
      subjectTags = unname(as.character(unlist(record$subject_tags, use.names = FALSE))),
      informationTypeTags = unname(as.character(unlist(
        record$information_type_tags, use.names = FALSE
      ))),
      variables = unname(as.character(unlist(record$variables, use.names = FALSE))),
      useScopes = unname(as.character(unlist(record$use_scopes, use.names = FALSE))),
      geographicScope = geographic_scope,
      mapReviewState = representation_record$map_review_state,
      mapRepresentation = representation_record$map_representation,
      representedProducts = unname(represented_products),
      searchText = search_text
    )
  })
}

pt_guide_relationship_registry_evidence_refs <- function(
    value, label, repository_root = ".", tracked_paths = NULL) {
  refs <- pt_guide_resource_registry_string_array(value, label)
  if (anyDuplicated(refs)) {
    stop(label, " must not contain duplicate paths.", call. = FALSE)
  }
  if (!length(refs)) return(refs)
  path_pattern <- "^[A-Za-z0-9][A-Za-z0-9._/-]*$"
  path_parts <- strsplit(refs, "/", fixed = TRUE)
  unsafe_path <- !grepl(path_pattern, refs, perl = TRUE) |
    grepl("//|\\\\|^[./]|(?:^|/)\\.{1,2}(?:/|$)|(?:^|/)(?:Users|home|private|tmp|var|Volumes)(?:/|$)",
          refs, ignore.case = TRUE, perl = TRUE) |
    vapply(path_parts, function(parts) any(!nzchar(parts)), logical(1))
  if (any(unsafe_path)) {
    stop(label, " must contain normalized repository-relative source paths.",
         call. = FALSE)
  }
  root <- normalizePath(repository_root, winslash = "/", mustWork = TRUE)
  resolved <- normalizePath(
    file.path(root, refs), winslash = "/", mustWork = FALSE
  )
  if (any(!file.exists(resolved)) || any(dir.exists(resolved)) ||
      any(!startsWith(resolved, paste0(root, "/")))) {
    stop(label, " must reference existing repository source files.", call. = FALSE)
  }
  if (!is.null(tracked_paths) && any(!refs %in% tracked_paths)) {
    stop(label, " must reference tracked repository source files.", call. = FALSE)
  }
  refs
}

pt_guide_relationship_registry_tracked_paths <- function(repository_root = ".") {
  git_marker <- file.path(repository_root, ".git")
  if (!file.exists(git_marker) && !dir.exists(git_marker)) return(NULL)
  paths <- suppressWarnings(tryCatch(
    system2("git", c("-C", repository_root, "ls-files"), stdout = TRUE, stderr = FALSE),
    error = function(error) character(0)
  ))
  status <- attr(paths, "status")
  if (!length(paths) || (!is.null(status) && status != 0L)) {
    stop("Guide Product-Resource relationship evidence could not verify tracked paths.",
         call. = FALSE)
  }
  unname(paths)
}

pt_guide_validate_product_resource_relationship_registry <- function(
    source, product_universe_ids, resource_registry,
    repository_root = ".", tracked_paths = NULL) {
  top_fields <- c("schema_version", "products", "resources")
  product_fields <- c(
    "product_id", "delivery_class", "delivery_evidence_refs",
    "coverage_review_state", "coverage_disposition", "coverage_evidence_basis",
    "coverage_evidence_refs", "resource_links"
  )
  link_fields <- c("resource_id", "relationship_role", "evidence_refs")
  resource_fields <- c(
    "resource_id", "map_review_state", "map_representation", "evidence_refs"
  )
  delivery_values <- c(
    "provider_hosted", "brim_enhanced", "brim_managed", "not_applicable"
  )
  review_values <- c("reviewed", "not_yet_reviewed")
  coverage_values <- c(
    "direct_resource_match", "selected_product_from_broader_resource",
    "multiple_source_resources", "provenance_only_no_public_resource",
    "internal_no_external_resource", "missing_resource_candidate"
  )
  evidence_basis_values <- c(
    "legacy_exact_relationship", "maintainer_calibration",
    "maintainer_clarification", "tracked_product_definition",
    "reviewed_evidence", "not_yet_reviewed"
  )
  link_role_values <- c(
    "direct_match_in_brim", "selected_product_from_broader_resource",
    "source_reference"
  )
  representation_values <- c(
    "direct_match_in_brim", "selected_products_in_brim",
    "not_currently_mapped_in_brim"
  )

  assert_fields <- function(record, required, allowed = required, label) {
    if (!is.list(record) || is.null(names(record)) || anyDuplicated(names(record))) {
      stop(label, " must be an object with unique field names.", call. = FALSE)
    }
    missing <- setdiff(required, names(record))
    unknown <- setdiff(names(record), allowed)
    if (length(missing)) {
      stop(label, " is missing required field(s): ", paste(missing, collapse = ", "),
           call. = FALSE)
    }
    if (length(unknown)) {
      stop(label, " contains unsupported field(s): ", paste(unknown, collapse = ", "),
           call. = FALSE)
    }
  }
  assert_array <- function(value, label) {
    if (!is.list(value) || (!is.null(names(value)) && any(nzchar(names(value))))) {
      stop(label, " must be an array.", call. = FALSE)
    }
  }

  assert_fields(source, top_fields, label = "Guide Product-Resource relationship registry")
  assert_array(source$products, "Guide Product-Resource relationship registry products")
  assert_array(source$resources, "Guide Product-Resource relationship registry resources")
  if (!identical(source$schema_version, 2L)) {
    stop("Guide Product-Resource relationship registry must use schema_version 2.",
         call. = FALSE)
  }
  product_universe_ids <- unname(as.character(product_universe_ids))
  if (!length(product_universe_ids) || any(!nzchar(product_universe_ids)) ||
      anyDuplicated(product_universe_ids)) {
    stop("Guide Product-Resource relationship validation requires a unique Product universe.",
         call. = FALSE)
  }
  if (!inherits(resource_registry, "pt_guide_resource_registry")) {
    stop("Guide Product-Resource relationships require the validated canonical Resource registry.",
         call. = FALSE)
  }
  resource_ids <- vapply(resource_registry, `[[`, character(1), "id")
  if (is.null(tracked_paths)) {
    tracked_paths <- pt_guide_relationship_registry_tracked_paths(repository_root)
  }

  ids <- character(length(source$products))
  validated <- vector("list", length(source$products))
  for (index in seq_along(source$products)) {
    record <- source$products[[index]]
    label <- paste0("Guide Product-Resource relationship record ", index)
    assert_fields(record, product_fields, label = label)
    product_id <- pt_guide_resource_registry_scalar(
      record$product_id, paste0(label, " product_id")
    )
    ids[[index]] <- product_id
    delivery_class <- pt_guide_resource_registry_scalar(
      record$delivery_class, paste0(label, " delivery_class")
    )
    review_state <- pt_guide_resource_registry_scalar(
      record$coverage_review_state, paste0(label, " coverage_review_state")
    )
    evidence_basis <- pt_guide_resource_registry_scalar(
      record$coverage_evidence_basis, paste0(label, " coverage_evidence_basis")
    )
    if (!delivery_class %in% delivery_values || !review_state %in% review_values ||
        !evidence_basis %in% evidence_basis_values) {
      stop(label, " contains an uncontrolled delivery, review, or evidence value.",
           call. = FALSE)
    }
    delivery_refs <- pt_guide_relationship_registry_evidence_refs(
      record$delivery_evidence_refs, paste0(label, " delivery_evidence_refs"),
      repository_root, tracked_paths
    )
    if (!length(delivery_refs)) {
      stop(label, " requires delivery evidence.", call. = FALSE)
    }
    coverage_refs <- pt_guide_relationship_registry_evidence_refs(
      record$coverage_evidence_refs, paste0(label, " coverage_evidence_refs"),
      repository_root, tracked_paths
    )
    coverage_disposition <- record$coverage_disposition
    if (!is.null(coverage_disposition)) {
      coverage_disposition <- pt_guide_resource_registry_scalar(
        coverage_disposition, paste0(label, " coverage_disposition")
      )
      if (!coverage_disposition %in% coverage_values) {
        stop(label, " has an uncontrolled coverage_disposition.", call. = FALSE)
      }
    }
    assert_array(record$resource_links, paste0(label, " resource_links"))
    links <- vector("list", length(record$resource_links))
    link_keys <- character(length(record$resource_links))
    for (link_index in seq_along(record$resource_links)) {
      link <- record$resource_links[[link_index]]
      link_label <- paste0(label, " resource link ", link_index)
      assert_fields(link, link_fields, label = link_label)
      resource_id <- pt_guide_resource_registry_scalar(
        link$resource_id, paste0(link_label, " resource_id")
      )
      relationship_role <- pt_guide_resource_registry_scalar(
        link$relationship_role, paste0(link_label, " relationship_role")
      )
      if (!resource_id %in% resource_ids) {
        stop(link_label, " references an unavailable canonical Resource.", call. = FALSE)
      }
      if (!relationship_role %in% link_role_values) {
        stop(link_label, " has an uncontrolled relationship_role.", call. = FALSE)
      }
      evidence_refs <- pt_guide_relationship_registry_evidence_refs(
        link$evidence_refs, paste0(link_label, " evidence_refs"),
        repository_root, tracked_paths
      )
      if (!length(evidence_refs)) stop(link_label, " requires evidence.", call. = FALSE)
      link_keys[[link_index]] <- paste(product_id, resource_id, relationship_role, sep = "\r")
      links[[link_index]] <- list(
        resource_id = resource_id,
        relationship_role = relationship_role,
        evidence_refs = unname(evidence_refs)
      )
    }
    if (anyDuplicated(link_keys)) {
      stop(label, " contains a duplicate Product/Resource/role link.", call. = FALSE)
    }
    if (identical(review_state, "not_yet_reviewed")) {
      if (!is.null(coverage_disposition) || length(links) || length(coverage_refs) ||
          !identical(evidence_basis, "not_yet_reviewed")) {
        stop(label, " not_yet_reviewed requires null disposition, no links, and no coverage evidence.",
             call. = FALSE)
      }
    } else {
      if (is.null(coverage_disposition) || !length(coverage_refs) ||
          identical(evidence_basis, "not_yet_reviewed")) {
        stop(label, " reviewed coverage requires a disposition and evidence.",
             call. = FALSE)
      }
      roles <- if (length(links)) vapply(links, `[[`, character(1), "relationship_role") else character(0)
      if (identical(coverage_disposition, "direct_resource_match") &&
          !"direct_match_in_brim" %in% roles) {
        stop(label, " direct coverage requires a direct_match_in_brim link.",
             call. = FALSE)
      }
      if (identical(coverage_disposition, "selected_product_from_broader_resource") &&
          !length(links)) {
        stop(label, " selected coverage requires at least one Resource link.",
             call. = FALSE)
      }
      if (identical(coverage_disposition, "multiple_source_resources") && length(links) < 2L) {
        stop(label, " multiple-source coverage requires at least two Resource links.",
             call. = FALSE)
      }
      if (coverage_disposition %in% c(
            "provenance_only_no_public_resource", "internal_no_external_resource",
            "missing_resource_candidate"
          ) && length(links)) {
        stop(label, " no-public-Resource coverage requires zero Resource links.",
             call. = FALSE)
      }
    }
    validated[[index]] <- list(
      product_id = product_id,
      delivery_class = delivery_class,
      delivery_evidence_refs = unname(delivery_refs),
      coverage_review_state = review_state,
      coverage_disposition = coverage_disposition,
      coverage_evidence_basis = evidence_basis,
      coverage_evidence_refs = unname(coverage_refs),
      resource_links = unname(links)
    )
  }
  if (anyDuplicated(ids)) {
    stop("Guide Product-Resource relationship registry contains duplicate Product records.",
         call. = FALSE)
  }
  missing_ids <- setdiff(product_universe_ids, ids)
  extra_ids <- setdiff(ids, product_universe_ids)
  if (length(missing_ids) || length(extra_ids)) {
    stop(
      "Guide Product-Resource relationship Product IDs must equal the complete Product universe; missing: ",
      paste(missing_ids, collapse = ", "), "; extra: ", paste(extra_ids, collapse = ", "),
      call. = FALSE
    )
  }

  validated_resources <- vector("list", length(source$resources))
  representation_ids <- character(length(source$resources))
  for (index in seq_along(source$resources)) {
    record <- source$resources[[index]]
    label <- paste0("Guide Product-Resource representation record ", index)
    assert_fields(record, resource_fields, label = label)
    resource_id <- pt_guide_resource_registry_scalar(
      record$resource_id, paste0(label, " resource_id")
    )
    review_state <- pt_guide_resource_registry_scalar(
      record$map_review_state, paste0(label, " map_review_state")
    )
    if (!review_state %in% review_values) {
      stop(label, " has an uncontrolled map_review_state.", call. = FALSE)
    }
    representation <- record$map_representation
    evidence_refs <- pt_guide_relationship_registry_evidence_refs(
      record$evidence_refs, paste0(label, " evidence_refs"),
      repository_root, tracked_paths
    )
    reverse_roles <- unlist(lapply(validated, function(product) {
      vapply(
        Filter(function(link) identical(link$resource_id, resource_id),
               product$resource_links),
        `[[`, character(1), "relationship_role"
      )
    }), use.names = FALSE)
    if (identical(review_state, "not_yet_reviewed")) {
      if (!is.null(representation) || length(evidence_refs) || length(reverse_roles)) {
        stop(label, " not_yet_reviewed requires null representation, no evidence, and no links.",
             call. = FALSE)
      }
    } else {
      representation <- pt_guide_resource_registry_scalar(
        representation, paste0(label, " map_representation")
      )
      if (!representation %in% representation_values || !length(evidence_refs)) {
        stop(label, " reviewed representation requires a controlled value and evidence.",
             call. = FALSE)
      }
      if (identical(representation, "direct_match_in_brim") &&
          !"direct_match_in_brim" %in% reverse_roles) {
        stop(label, " direct representation requires an exact direct Product link.",
             call. = FALSE)
      }
      if (identical(representation, "selected_products_in_brim") &&
          !length(reverse_roles)) {
        stop(label, " selected representation requires at least one exact Product link.",
             call. = FALSE)
      }
      if (identical(representation, "not_currently_mapped_in_brim") &&
          length(reverse_roles)) {
        stop(label, " not-currently-mapped representation cannot have a Product link.",
             call. = FALSE)
      }
    }
    representation_ids[[index]] <- resource_id
    validated_resources[[index]] <- list(
      resource_id = resource_id,
      map_review_state = review_state,
      map_representation = representation,
      evidence_refs = unname(evidence_refs)
    )
  }
  if (anyDuplicated(representation_ids)) {
    stop("Guide Product-Resource relationship registry contains duplicate Resource records.",
         call. = FALSE)
  }
  missing_resource_ids <- setdiff(resource_ids, representation_ids)
  extra_resource_ids <- setdiff(representation_ids, resource_ids)
  if (length(missing_resource_ids) || length(extra_resource_ids)) {
    stop(
      "Guide Product-Resource relationship Resource IDs must equal the complete Resource universe; missing: ",
      paste(missing_resource_ids, collapse = ", "), "; extra: ",
      paste(extra_resource_ids, collapse = ", "), call. = FALSE
    )
  }
  values <- as.character(unlist(source, recursive = TRUE, use.names = FALSE))
  if (any(grepl(
        "(^|[[:space:]\"'=])/(Users|home|private|tmp|var|Volumes)/|(^|[[:space:]\"'=])[A-Za-z]:[\\\\/]",
        values, ignore.case = TRUE, perl = TRUE
      )) || any(grepl(
        "BEGIN (?:RSA |EC |OPENSSH )?PRIVATE KEY|bearer[[:space:]]+[A-Za-z0-9._~-]+|password[[:space:]]*[:=]|secret[[:space:]]*[:=]|bookmark[ _-]?(export|provenance)|raw[ _-]?bookmark",
        values, ignore.case = TRUE, perl = TRUE
      ))) {
    stop("Guide Product-Resource relationship registry contains unsafe, local, credential, or raw bookmark material.",
         call. = FALSE)
  }
  structure(
    list(products = unname(validated), resources = unname(validated_resources)),
    class = c("pt_guide_product_resource_relationship_registry", "list")
  )
}

pt_guide_read_product_resource_relationship_registry <- function(
    product_universe_ids, resource_registry,
    path = file.path("00_config", "guide_product_resource_relationships.json"),
    repository_root = ".") {
  if (!file.exists(path)) {
    stop("Missing canonical Guide Product-Resource relationship registry: ", path,
         call. = FALSE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required by the existing BRIM Guide compiler.", call. = FALSE)
  }
  source <- tryCatch(
    jsonlite::fromJSON(path, simplifyVector = FALSE),
    error = function(error) stop(
      "Malformed canonical Guide Product-Resource relationship registry: ",
      conditionMessage(error), call. = FALSE
    )
  )
  pt_guide_validate_product_resource_relationship_registry(
    source, product_universe_ids, resource_registry,
    repository_root = repository_root
  )
}

pt_guide_read_product_enrichment <- function(
    path = file.path("00_config", "guide_product_enrichment.json")) {
  if (!file.exists(path)) return(list())
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite is required by the existing BRIM Guide compiler.", call. = FALSE)
  }
  source <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!identical(source$schema_version, 1L) || !is.list(source$products)) {
    stop("Guide Product enrichment must use schema_version 1 and a products array.", call. = FALSE)
  }
  allowed <- c(
    "stable_id", "summary", "access_hint", "subject_tags", "information_type_tags",
    "capabilities", "processing", "timing", "geometry_limitations",
    "method_ids", "editorial_state", "source_refs"
  )
  ids <- vapply(source$products, function(x) pt_guide_or(x$stable_id), character(1))
  if (any(!nzchar(ids)) || anyDuplicated(ids) ||
      !identical(ids, sort(ids, method = "radix"))) {
    stop("Guide Product enrichment stable IDs must be nonblank, unique, and sorted.", call. = FALSE)
  }
  for (record in source$products) {
    unknown <- setdiff(names(record), allowed)
    if (length(unknown)) {
      stop("Guide Product enrichment contains unsupported field(s): ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    if (!pt_guide_or(record$editorial_state) %in% c(
      "SOURCE_BACKED_RICH", "STRUCTURED_BASIC", "EDITORIAL_REVIEW_REQUIRED"
    )) stop("Guide Product enrichment has an invalid editorial_state.", call. = FALSE)
    if (!length(record$source_refs) || any(!nzchar(trimws(as.character(unlist(record$source_refs)))))) {
      stop("Guide Product enrichment requires nonblank source_refs for every record.", call. = FALSE)
    }
    subjects <- unname(as.character(unlist(record$subject_tags, use.names = FALSE)))
    information_types <- unname(as.character(unlist(record$information_type_tags, use.names = FALSE)))
    if (any(!subjects %in% pt_guide_subject_vocabulary()) || anyDuplicated(subjects)) {
      stop("Guide Product enrichment contains an uncontrolled or duplicate subject tag.", call. = FALSE)
    }
    if (!length(information_types) || any(!information_types %in% pt_guide_information_type_vocabulary()) ||
        anyDuplicated(information_types)) {
      stop("Guide Product enrichment requires controlled, unique Information Type tags.", call. = FALSE)
    }
  }
  stats::setNames(source$products, ids)
}

pt_guide_enrichment_sections <- function(record) {
  fields <- list(
    capabilities = c("capabilities", "Capabilities"),
    timing = c("time_period", "Time / period"),
    processing = c("preparation", "How BRIM prepares it"),
    geometry_limitations = c("geometry_limitations", "Geometry / limitations")
  )
  sections <- list()
  for (field in names(fields)) {
    values <- unname(as.character(unlist(record[[field]], use.names = FALSE)))
    values <- values[nzchar(trimws(values))]
    if (!length(values)) next
    sections[[length(sections) + 1L]] <- pt_guide_section(
      fields[[field]][[1]], fields[[field]][[2]], items = values
    )
  }
  sections
}

pt_guide_apply_product_enrichment <- function(
    products, enrichment, relationship_registry,
    resource_registry = pt_guide_read_resource_registry(),
    product_universe_ids = NULL) {
  product_ids <- vapply(products, `[[`, character(1), "id")
  if (any(!nzchar(product_ids)) || anyDuplicated(product_ids)) {
    stop("Guide Product universe IDs must be nonblank and unique.", call. = FALSE)
  }
  if (is.null(product_universe_ids)) product_universe_ids <- product_ids
  product_universe_ids <- unname(as.character(product_universe_ids))
  if (any(!nzchar(product_universe_ids)) || anyDuplicated(product_universe_ids) ||
      any(!product_universe_ids %in% product_ids)) {
    stop("Guide Product enrichment requires an exact unique Product universe.", call. = FALSE)
  }
  missing <- setdiff(names(enrichment), product_universe_ids)
  if (length(missing)) {
    stop("Guide Product enrichment references unavailable Product(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!inherits(resource_registry, "pt_guide_resource_registry")) {
    stop("Guide Product enrichment requires the validated canonical Resource registry.",
         call. = FALSE)
  }
  if (!inherits(
        relationship_registry,
        "pt_guide_product_resource_relationship_registry"
      )) {
    stop("Guide Product enrichment requires the validated sole relationship authority.",
         call. = FALSE)
  }
  published_resource_ids <- vapply(
    pt_guide_resource_published_records(resource_registry), `[[`, character(1), "id"
  )
  relationship_products <- relationship_registry$products
  relationship_product_ids <- vapply(
    relationship_products, `[[`, character(1), "product_id"
  )
  if (!setequal(relationship_product_ids, product_universe_ids)) {
    stop("Guide Product enrichment relationship authority must equal the Product universe.",
         call. = FALSE)
  }
  lapply(products, function(product) {
    product$relatedResourceIds <- character(0)
    product$relatedResources <- list()
    if (!product$id %in% product_universe_ids) return(product)
    record <- enrichment[[product$id]]
    if (!is.null(record)) {
      if (nzchar(pt_guide_or(record$summary))) product$summary <- pt_guide_or(record$summary)
      product$accessHint <- pt_guide_or(record$access_hint, product$accessHint)
      if (!is.null(record$subject_tags)) {
        subjects <- unname(as.character(unlist(record$subject_tags, use.names = FALSE)))
        product$subjectTags <- unique(subjects[nzchar(trimws(subjects))])
        product$subject <- pt_guide_or(product$subjectTags)
      }
      information_types <- unname(as.character(unlist(
        record$information_type_tags, use.names = FALSE
      )))
      if (length(information_types)) {
        product$informationTypes <- unique(information_types)
        product$mode <- product$informationTypes[[1]]
      }
      capabilities <- unname(as.character(unlist(record$capabilities, use.names = FALSE)))
      product$searchTerms <- unique(c(
        product$searchTerms,
        capabilities[nzchar(trimws(capabilities))]
      ))
      product$sections <- pt_guide_enrichment_sections(record)
      product$relatedArticleIds <- unname(unique(as.character(unlist(
        record$method_ids, use.names = FALSE
      ))))
      product$contentTier <- pt_guide_or(record$editorial_state)
    }
    relationship_record <- relationship_products[[match(
      product$id, relationship_product_ids
    )]]
    if (is.null(relationship_record)) {
      stop("Guide Product enrichment found a Product outside relationship authority.",
           call. = FALSE)
    }
    relationships <- Filter(function(relationship) {
      relationship$resource_id %in% published_resource_ids
    }, relationship_record$resource_links)
    if (length(relationships)) {
      source_resource_ids <- intersect(
        published_resource_ids,
        vapply(relationships, `[[`, character(1), "resource_id")
      )
      product$relatedResources <- lapply(relationships, function(relationship) list(
        id = relationship$resource_id,
        relationshipRole = relationship$relationship_role,
        deliveryClass = relationship_record$delivery_class,
        coverageDisposition = relationship_record$coverage_disposition,
        sourceResourceIds = unname(source_resource_ids)
      ))
      product$relatedResourceIds <- unname(vapply(
        product$relatedResources, `[[`, character(1), "id"
      ))
    }
    product
  })
}

pt_guide_authored_content <- function(resources) {
  if (!exists("pt_polygon_generalization_read_registry", mode = "function")) {
    stop("Polygon generalization authority must be loaded before compiling curated Guide content.", call. = FALSE)
  }
  generalization <- pt_polygon_generalization_read_registry()

  articles <- list(
    pt_guide_article(
      "method_how_brim_works",
      "How BRIM Works",
      "A practical guide to where BRIM layers and tools come from, how they behave, and what to check before using them.",
      list(
        pt_guide_section(
          "layer_tool_families",
          "Layers and tools in BRIM",
          items = c(
            "LOCAL LAYERS are packaged, cached, or prepared with BRIM for fast and consistent use in the map.",
            "OPS LIVE layers show current observations, conditions, forecasts, or other time-aware information from feed-specific sources.",
            "EXTERNAL LAYERS are hosted by other organizations, so drawing speed and availability depend on the provider service.",
            "TOOLS support upload, measurement, drawing, filtering, and other workflows; they are not source datasets."
          )
        ),
        pt_guide_section(
          "using_the_guide",
          "Using BRIM Guide",
          paragraphs = "Use BRIM Guide to find layers and tools and to understand their sources, preparation, timing, and limitations. BRIM Guide explains map content but does not turn layers on or change map settings."
        )
      ),
      aliases = c("BRIM basics", "layer types", "tool types")
    ),
    pt_guide_article(
      "method_display_geometry_generalization",
      "Display Geometry & Generalization",
      "Why BRIM keeps browser display geometry separate from authoritative or analytical geometry, with the current public portfolio values.",
      list(
        pt_guide_section(
          "display_vs_authority",
          "Display geometry versus authority",
          paragraphs = c(
            "BRIM keeps high-fidelity or source geometry separate from fit-for-purpose browser display geometry. Geometry-only replacement preserves the retained identifiers and map attributes while reducing browser weight.",
            "Generalized display boundaries support screening and navigation. Use the authoritative source geometry for boundary-sensitive analysis, legal interpretation, or precise acreage work."
          )
        ),
        pt_guide_section(
          "current_portfolio",
          "Current public portfolio",
          table = pt_guide_generalization_table(generalization)
        )
      ),
      related_product_ids = c("huc8", "gw_bull118"),
      aliases = c("simplification", "generalization", "display geometry")
    ),
    pt_guide_article(
      "method_scan_soil_moisture_statistical_context",
      "SCAN Soil Moisture Statistical Context",
      "How the SCAN map separates fresh observations from station/depth historical reference context.",
      list(
        pt_guide_section(
          "scan_context",
          "Current values and reference context",
          paragraphs = "Current/latest soil-moisture values and the current-water-year trace are refreshed separately from the historical percentile ribbons and monthly context.",
          items = c(
            "Context classes use station/depth p10, p30, p70, and p90 reference thresholds.",
            "Daily percentile ribbons require the current 7-year mature-reference threshold and at least 200 supported water-days.",
            "When daily ribbons are not eligible, BRIM may show usable prior-water-year fallback traces instead.",
            "The period of analysis is station-specific reference context, not a formal climatology."
          )
        )
      ),
      related_product_ids = "ops_scan_soil_moisture",
      aliases = c("SCAN percentiles", "soil moisture context")
    ),
    pt_guide_article(
      "method_snow_pillow_swe_statistical_context",
      "Snow-Pillow SWE Statistical Context",
      "How current SWE, changes, percentile context, and median comparisons remain distinct in BRIM.",
      list(
        pt_guide_section(
          "swe_context",
          "SWE display and reference statistics",
          items = c(
            "Map modes include current daily SWE, 1-/3-/7-day change, and current SWE relative to station/day historical context.",
            "Context classes use station/day p10, p30, p70, and p90 thresholds and only color fresh/current observations.",
            "Daily percentile ribbons require at least 10 years supporting the water day; usable prior-water-year traces may appear when ribbons are unavailable.",
            "Fixed WY1991–WY2020 and rolling 30-complete-water-year median comparisons are separate from the period-of-analysis percentile classes.",
            "Historical context is station-specific and is not a formal climatology."
          )
        )
      ),
      related_product_ids = "ops_snow_pillow_swe",
      aliases = c("SWE percentiles", "snow pillow context")
    ),
    pt_guide_article(
      "method_usgs_groundwater_history_summaries",
      "USGS Groundwater History Summaries",
      "How BRIM relates the latest USGS field measurement to compact period-of-record, seasonal, and water-year summaries.",
      list(
        pt_guide_section(
          "groundwater_history",
          "Latest measurement and history",
          items = c(
            "The Ops Live feed carries the latest field-measurement depth to water for active/recent candidate wells and joins a compact local history summary when available.",
            "History fields include period-of-record and seasonal percentile context plus completed water-year mean depth-to-water series.",
            "The latest field measurement is displayed separately from the water-year means; the two are not interchangeable.",
            "Negative depth-to-water values remain reported artesian or above-land-surface values.",
            "History percentiles and mini plots are screening context, not a groundwater-storage calculation."
          )
        )
      ),
      related_product_ids = "product-ops-usgs-groundwater",
      aliases = c("USGS wells", "groundwater percentiles", "water-year means")
    ),
    pt_guide_article(
      "method_brim_live_update_timing",
      "BRIM Live Update Timing",
      "How to read source times, forecast cycles, valid times, freshness, and refresh behavior across Ops Live layers.",
      list(
        pt_guide_section(
          "timing_semantics",
          "Feed-specific timing",
          items = c(
            "The separate feed repository acquires, normalizes, publishes, and monitors live artifacts; BRIM fetches and renders those documented contracts.",
            "Observation layers expose provider/source observation times and feed-specific freshness or age status when available.",
            "Forecast layers preserve their own cycle time, valid time, and lead; an exact unavailable target is not replaced with a nearby time or another cycle.",
            "Activating or refreshing one layer does not make neighboring feeds share its update cadence or currency.",
            "The linked product table is the current schedule reference; fetch attempts, successful publication, model cycles, and valid times remain distinct and vary by product."
          )
        )
      ),
      related_product_ids = c(
        "ops_scan_soil_moisture", "ops_snow_pillow_swe",
        "product-ops-usgs-groundwater", "winter_storm_levels", "nbm_qpf",
        "product-ops-nbm-accumulated-qpf"
      ),
      aliases = c("freshness", "feed cadence", "cycle time", "valid time"),
      external_links = list(list(
        label = "View current BRIM Live schedule",
        role = "Current product schedule",
        url = "https://github.com/dbo99/brim-live-data-feeds/blob/main/docs/PRODUCTS.md#inventory-at-a-glance"
      ))
    ),
    pt_guide_article(
      "method_brim_under_the_hood",
      "BRIM Under the Hood",
      "The build-time and browser boundaries behind the self-contained BRIM map and Guide.",
      list(
        pt_guide_section(
          "build_and_browser",
          "Build and browser responsibilities",
          items = c(
            "R assembles the current registries, retained map products, controls, compact Guide bundle, and assets into one standalone Leaflet HTML file.",
            "Guide coverage is assembled after the current map profile and visible layer groups are known, then embedded before browser startup.",
            "Browser controllers own interaction and teardown for their layer families; BRIM Guide does not activate map layers or fetch Guide content at runtime.",
            "Large raw inputs, processed products, caches, realistic HTML, and screenshots remain external to the tracked source repository."
          )
        )
      ),
      aliases = c("technical architecture", "standalone Leaflet", "build pipeline")
    )
  )

  updates <- list(
    list(kind = "Update", id = "update_read_only_layer_explorer", title = "Read-only Layer Explorer added",
         date = "2026-08-24", updateType = "Interface", summary = "Added a read-only view of embedded descriptive catalog metadata; current runtime construction remains authoritative and the explorer cannot control map layers.", relatedProductIds = character(0)),
    list(kind = "Update", id = "update_nbm_accumulated_qpf", title = "NBM accumulated QPF forecast windows added",
         date = "2026-08-20", updateType = "Forecast layer", summary = "Added exact-cycle 0–10 day accumulated-QPF windows computed from verified six-hour numeric companions; partial totals are not rendered.", relatedProductIds = "product-ops-nbm-accumulated-qpf"),
    list(kind = "Update", id = "update_nbm_legend_links", title = "NBM legend links simplified",
         date = "2026-08-19", updateType = "Usability", summary = "Removed redundant per-row NBM legend links while retaining the shared forecast-guidance control behavior.", relatedProductIds = c("winter_storm_levels", "nbm_qpf"))
  )

  quick_entry <- function(id, label, entry_kind, product_ids, summary, type_label = "") {
    item <- list(
      id = id, label = label, entryKind = entry_kind,
      typeLabel = pt_guide_or(type_label, tools::toTitleCase(entry_kind)), summary = summary
    )
    if (identical(entry_kind, "collection")) item$memberIds <- product_ids else item$productId <- product_ids[[1]]
    item
  }
  quick_access <- list(
    quick_entry("quick_huc8", "HUC8 – PRISM/BCMv8", "layer", "huc8",
                "HUC8 boundaries with BLM-managed-land, PRISM precipitation, and BCMv8 recharge display context."),
    quick_entry("quick_groundwater_basins", "Groundwater Basins – Bulletin 118", "layer", "gw_bull118",
                "Bulletin 118 groundwater basins with SGMA 2019 priority and BLM-managed-land context."),
    quick_entry("quick_fire_perimeters", "Fire Perimeters", "collection", c("EXT070", "EXT072", "EXT074"),
                "Three complementary perimeter layers: CAL FIRE recent large-fire and full historical coverage, plus NIFC current operational wildfire/complex perimeters. Their coverage and currency differ; review each layer before use.",
                "Collection · 3 layers"),
    quick_entry("quick_nbm_snow_levels", "NBM Snow Levels", "layer", "winter_storm_levels",
                "NBM freezing-level and snow-level guidance by exact model cycle and valid time."),
    quick_entry("quick_water_supply_forecasts", "Water-Supply Basin Forecasts", "layer", "ops_major_water_supply_forecasts",
                "Reviewed major-basin and index water-supply forecast guidance from CNRFC sources."),
    quick_entry("quick_delta_operations", "Delta Operations", "layer", "ops_delta_snapshot",
                "DWR Delta Operations Daily Summary metrics with mapped facility and X2 context."),
    quick_entry("quick_usgs_streamflow", "USGS Streamflow", "collection", c("usgs_streamgages", "ops_streamflow_usgs_ca"),
                "Two distinct BRIM views retain the Local streamgage layer and the Ops Live provisional streamflow layer with their own paths and detail pages.",
                "Collection · 2 BRIM views"),
    quick_entry("quick_usgs_groundwater", "USGS Groundwater", "collection", c("usgs_wells", "product-ops-usgs-groundwater"),
                "Two distinct BRIM views retain the Local monitoring-well layer and the Ops Live latest-measurement layer with their own paths and detail pages.",
                "Collection · 2 BRIM views"),
    quick_entry("quick_scan_soil_moisture", "USDA / SCAN Soil Moisture", "layer", "ops_scan_soil_moisture",
                "Latest SCAN soil-moisture observations by depth with current-water-year and historical context."),
    quick_entry("quick_snow_pillow_swe", "Snow-Pillow SWE", "layer", "ops_snow_pillow_swe",
                "Latest NRCS and CDEC snow-water equivalent with water-year and historical context."),
    quick_entry("quick_brim_mapped_conveyance", "Water conveyance | BRIM mapped", "layer", "brim_mapped_conveyance",
                "BRIM's curated statewide conveyance network assembled from multiple reviewed source datasets.")
  )

  list(
    articles = articles,
    resources = resources,
    updates = updates,
    quickAccess = quick_access
  )
}

pt_project_guide_bundle <- function(bundle, excluded_ids = character(0), profile_id = bundle$profileId) {
  excluded_ids <- unique(as.character(excluded_ids))
  keep_product <- !vapply(bundle$products, function(x) x$id %in% excluded_ids, logical(1))
  bundle$products <- bundle$products[keep_product]
  bundle$articles <- Filter(function(record) !record$id %in% excluded_ids, bundle$articles)
  bundle$resources <- Filter(function(record) !record$id %in% excluded_ids, bundle$resources)
  bundle$updates <- Filter(function(record) !record$id %in% excluded_ids, bundle$updates)
  bundle$quickAccess <- Filter(function(item) !item$id %in% excluded_ids, bundle$quickAccess)
  product_ids <- vapply(bundle$products, `[[`, character(1), "id")
  prune_related <- function(record) {
    if (!is.null(record$relatedProductIds)) record$relatedProductIds <- intersect(record$relatedProductIds, product_ids)
    record
  }
  bundle$articles <- lapply(bundle$articles, prune_related)
  bundle$updates <- lapply(bundle$updates, prune_related)
  bundle$quickAccess <- lapply(bundle$quickAccess, function(item) {
    if (identical(item$entryKind, "collection")) {
      item$memberIds <- intersect(item$memberIds, product_ids)
    }
    item
  })
  bundle$quickAccess <- Filter(function(item) {
    if (identical(item$entryKind, "collection")) return(length(item$memberIds) > 0L)
    pt_guide_or(item$productId) %in% product_ids
  }, bundle$quickAccess)
  resource_ids <- vapply(bundle$resources, `[[`, character(1), "id")
  article_ids <- vapply(bundle$articles, `[[`, character(1), "id")
  bundle$products <- lapply(bundle$products, function(product) {
    product$relatedArticleIds <- intersect(product$relatedArticleIds, article_ids)
    product$relatedResources <- Filter(function(relationship) {
      pt_guide_or(relationship$id) %in% resource_ids
    }, product$relatedResources)
    product$relatedResources <- lapply(product$relatedResources, function(relationship) {
      relationship$sourceResourceIds <- intersect(
        relationship$sourceResourceIds, resource_ids
      )
      relationship
    })
    product$relatedResourceIds <- unname(vapply(
      product$relatedResources,
      `[[`,
      character(1),
      "id"
    ))
    product
  })
  bundle$resources <- lapply(bundle$resources, function(resource) {
    resource$representedProducts <- Filter(function(product) {
      pt_guide_or(product$productId) %in% product_ids
    }, resource$representedProducts)
    resource$representedProducts <- lapply(
      resource$representedProducts,
      function(product) {
        product$sourceResourceIds <- intersect(product$sourceResourceIds, resource_ids)
        product
      }
    )
    resource$searchText <- pt_guide_normalize_resource_search(list(
      resource$title,
      resource$aliases,
      resource$provider,
      vapply(resource$providers, `[[`, character(1), "name"),
      resource$summary,
      vapply(resource$accessPoints, `[[`, character(1), "label"),
      resource$resourceTypeLabel,
      resource$resourceGranularity,
      resource$subjectTags,
      resource$informationTypeTags,
      resource$variables,
      resource$useScopes,
      resource$geographicScope$scopeLabel,
      resource$geographicScope$names,
      vapply(resource$representedProducts, `[[`, character(1), "title"),
      vapply(Filter(function(candidate) {
        candidate$id %in% unique(unlist(lapply(
          Filter(function(product) {
            identical(product$coverageDisposition, "multiple_source_resources")
          }, resource$representedProducts),
          `[[`, "sourceResourceIds"
        ), use.names = FALSE))
      }, bundle$resources), `[[`, character(1), "title")
    ))
    resource
  })
  bundle$profileId <- profile_id
  bundle$counts <- list(
    products = length(bundle$products), articles = length(bundle$articles),
    resources = length(bundle$resources), updates = length(bundle$updates),
    quickAccess = length(bundle$quickAccess)
  )
  bundle
}

pt_validate_guide_bundle <- function(bundle) {
  collections <- c(bundle$products, bundle$articles, bundle$resources, bundle$updates)
  ids <- vapply(collections, function(x) pt_guide_or(x$id), character(1))
  if (any(!nzchar(ids)) || anyDuplicated(ids)) stop("BRIM Guide record IDs must be nonblank and unique.", call. = FALSE)
  product_ids <- vapply(bundle$products, `[[`, character(1), "id")
  paths <- vapply(bundle$products, `[[`, character(1), "pathLabel")
  shown_paths <- paths[nzchar(paths)]
  if (anyDuplicated(shown_paths)) stop("BRIM Guide nonblank Product paths must be unique.", call. = FALSE)
  if (any(grepl("(^| / )Points( / |$)", paths))) stop("BRIM Guide paths must use Monitoring Sites/Records terminology.", call. = FALSE)
  quick_ids <- unlist(lapply(bundle$quickAccess, function(item) {
    if (identical(item$entryKind, "collection")) item$memberIds else item$productId
  }), use.names = FALSE)
  if (any(!quick_ids %in% product_ids)) stop("BRIM Guide Quick Access references an unavailable Product.", call. = FALSE)
  quick_record_ids <- vapply(bundle$quickAccess, function(x) pt_guide_or(x$id), character(1))
  if (any(!nzchar(quick_record_ids)) || anyDuplicated(quick_record_ids) ||
      any(vapply(bundle$quickAccess, function(x) {
        !pt_guide_or(x$entryKind) %in% c("layer", "tool", "collection") ||
          !nzchar(pt_guide_or(x$label)) || !nzchar(pt_guide_or(x$typeLabel)) ||
          !nzchar(pt_guide_or(x$summary)) ||
          (identical(x$entryKind, "collection") &&
             (!length(x$memberIds) || any(!nzchar(x$memberIds)) || anyDuplicated(x$memberIds))) ||
          (!identical(x$entryKind, "collection") && !nzchar(pt_guide_or(x$productId)))
      }, logical(1)))) {
    stop("BRIM Guide Quick Access entries require valid typed identity, labels, summaries, and exact Product membership.", call. = FALSE)
  }
  for (item in bundle$quickAccess) {
    if (identical(item$entryKind, "collection")) next
    product <- bundle$products[[match(item$productId, product_ids)]]
    expected_type <- if (identical(item$entryKind, "tool")) "Tool" else "Layer"
    if (is.null(product) || !identical(product$entityType, expected_type)) {
      stop("BRIM Guide Quick Access single-entry type does not match its Product.", call. = FALSE)
    }
  }
  resource_ids <- vapply(bundle$resources, `[[`, character(1), "id")
  article_ids <- vapply(bundle$articles, `[[`, character(1), "id")
  resource_fields <- c(
    "kind", "id", "title", "aliases", "provider", "providers", "summary",
    "canonicalUrl", "accessPoints", "resourceType", "resourceTypeLabel",
    "temporalCharacter", "temporalCharacterLabel", "resourceGranularity",
    "subjectTags", "informationTypeTags", "variables", "useScopes", "geographicScope",
    "mapReviewState", "mapRepresentation", "representedProducts", "searchText"
  )
  metadata_vocabularies <- pt_guide_resource_metadata_vocabularies()
  for (resource in bundle$resources) {
    if (!identical(names(resource), resource_fields) ||
        !identical(resource$kind, "Resource") ||
        any(!nzchar(c(resource$id, resource$title, resource$provider,
                      resource$summary, resource$canonicalUrl, resource$resourceType,
                      resource$resourceTypeLabel, resource$temporalCharacter,
                      resource$temporalCharacterLabel, resource$resourceGranularity,
                      resource$searchText)))) {
      stop("BRIM Guide Resources require the exact schema-v2 public projection.",
           call. = FALSE)
    }
    if (!resource$resourceType %in% names(metadata_vocabularies$resource_type) ||
        !identical(
          resource$resourceTypeLabel,
          unname(metadata_vocabularies$resource_type[[resource$resourceType]])
        ) ||
        !resource$temporalCharacter %in% names(metadata_vocabularies$temporal_character) ||
        !identical(
          resource$temporalCharacterLabel,
          unname(metadata_vocabularies$temporal_character[[resource$temporalCharacter]])
        )) {
      stop("BRIM Guide Resource type or temporal labels are not controlled projections.",
           call. = FALSE)
    }
    if (!is.list(resource$providers) || !length(resource$providers) ||
        any(vapply(resource$providers, function(provider) {
          !identical(names(provider), c("name", "role")) ||
            any(!nzchar(c(provider$name, provider$role)))
        }, logical(1)))) {
      stop("BRIM Guide Resource providers require exact nonblank name/role pairs.",
           call. = FALSE)
    }
    display_providers <- Filter(function(provider) {
      identical(provider$role, "display_provider")
    }, resource$providers)
    if (length(display_providers) != 1L ||
        !identical(display_providers[[1]]$name, resource$provider)) {
      stop("BRIM Guide Resource display provider is inconsistent.", call. = FALSE)
    }
    if (!is.list(resource$accessPoints) || !length(resource$accessPoints) ||
        any(vapply(resource$accessPoints, function(point) {
          !identical(names(point), c("role", "label", "url")) ||
            any(!nzchar(c(point$role, point$label, point$url))) ||
            !grepl("^https://", point$url)
        }, logical(1)))) {
      stop("BRIM Guide Resource access points require exact labeled HTTPS actions.",
           call. = FALSE)
    }
    canonical_points <- Filter(function(point) {
      identical(point$role, "canonical")
    }, resource$accessPoints)
    if (length(canonical_points) != 1L ||
        !identical(canonical_points[[1]]$label, "Official Resource") ||
        !identical(canonical_points[[1]]$url, resource$canonicalUrl)) {
      stop("BRIM Guide Resource canonical access-point contract changed.", call. = FALSE)
    }
    if (!identical(
          names(resource$geographicScope), c("scopeType", "scopeLabel", "names")
        ) ||
        !resource$geographicScope$scopeType %in%
          names(metadata_vocabularies$geographic_scope_class) ||
        !identical(
          resource$geographicScope$scopeLabel,
          unname(metadata_vocabularies$geographic_scope_class[[
            resource$geographicScope$scopeType
          ]])
        )) {
      stop("BRIM Guide Resource geographic scope shape changed.", call. = FALSE)
    }
    if (!identical(resource$mapReviewState, "reviewed") ||
        !resource$mapRepresentation %in% c(
          "direct_match_in_brim", "selected_products_in_brim",
          "not_currently_mapped_in_brim"
        )) {
      stop("Published BRIM Guide Resources require reviewed map representation.",
           call. = FALSE)
    }
    expected_represented <- list()
    for (product in bundle$products) {
      for (relationship in product$relatedResources) {
        if (!identical(pt_guide_or(relationship$id), resource$id)) next
        expected_represented[[length(expected_represented) + 1L]] <- list(
          productId = product$id,
          title = product$title,
          deliveryClass = relationship$deliveryClass,
          coverageDisposition = relationship$coverageDisposition,
          relationshipRole = relationship$relationshipRole,
          sourceResourceIds = unname(relationship$sourceResourceIds)
        )
      }
    }
    if (!identical(resource$representedProducts, unname(expected_represented))) {
      stop("BRIM Guide Resource relationships must reverse-index exact eligible schema-v2 Product links.",
           call. = FALSE)
    }
    expected_search <- pt_guide_normalize_resource_search(list(
      resource$title,
      resource$aliases,
      resource$provider,
      vapply(resource$providers, `[[`, character(1), "name"),
      resource$summary,
      vapply(resource$accessPoints, `[[`, character(1), "label"),
      resource$resourceTypeLabel,
      resource$resourceGranularity,
      resource$subjectTags,
      resource$informationTypeTags,
      resource$variables,
      resource$useScopes,
      resource$geographicScope$scopeLabel,
      resource$geographicScope$names,
      vapply(resource$representedProducts, `[[`, character(1), "title"),
      unname(vapply(Filter(function(candidate) {
        candidate$id %in% unique(unlist(lapply(
          Filter(function(product) {
            identical(product$coverageDisposition, "multiple_source_resources")
          }, resource$representedProducts),
          `[[`, "sourceResourceIds"
        ), use.names = FALSE))
      }, bundle$resources), `[[`, character(1), "title"))
    ))
    if (!identical(resource$searchText, expected_search)) {
      stop("BRIM Guide Resource search text is not the deterministic allowed-field projection.",
           call. = FALSE)
    }
  }
  for (article in bundle$articles) {
    links <- article$externalLinks
    if (!length(links)) next
    labels <- vapply(links, function(x) pt_guide_or(x$label), character(1))
    roles <- vapply(links, function(x) pt_guide_or(x$role), character(1))
    urls <- vapply(links, function(x) pt_guide_or(x$url), character(1))
    if (any(!nzchar(labels)) || any(!nzchar(roles)) || any(!grepl("^https://", urls)) || anyDuplicated(urls)) {
      stop("BRIM Guide Method external links require unique HTTPS URLs and nonblank labels and roles.", call. = FALSE)
    }
  }
  for (product in bundle$products) {
    mandatory <- c("id", "title", "entityType", "brimSection", "provider")
    if (any(!vapply(mandatory, function(field) nzchar(pt_guide_or(product[[field]])), logical(1)))) {
      stop("BRIM Guide Products require stable identity, entity type, section, and provider.", call. = FALSE)
    }
    if (!product$entityType %in% c("Layer", "Tool")) {
      stop("BRIM Guide Products require a controlled user-facing entity type.", call. = FALSE)
    }
    if (!nzchar(product$pathLabel) && !nzchar(product$summary)) {
      stop("BRIM Guide Products require a verified path or source-backed purpose/action summary.", call. = FALSE)
    }
    if (identical(product$entityType, "Tool") && nzchar(product$pathLabel)) {
      stop("BRIM Guide Tools use action summaries and must not expose synthetic layer paths.", call. = FALSE)
    }
    subjects <- unname(as.character(product$subjectTags))
    information_types <- unname(as.character(product$informationTypes))
    if (any(!subjects %in% pt_guide_subject_vocabulary()) || anyDuplicated(subjects) ||
        (!length(subjects) && nzchar(product$subject)) ||
        (length(subjects) && !identical(product$subject, subjects[[1]])) ||
        !length(information_types) || any(!information_types %in% pt_guide_information_type_vocabulary()) ||
        anyDuplicated(information_types) || !identical(product$mode, information_types[[1]])) {
      stop("BRIM Guide Products require controlled, unique taxonomy values; zero subjects are allowed when no confident domain assignment exists.", call. = FALSE)
    }
    if (!product$contentTier %in% c(
      "SOURCE_BACKED_RICH", "STRUCTURED_BASIC", "EDITORIAL_REVIEW_REQUIRED"
    )) stop("BRIM Guide Product content tier is invalid.", call. = FALSE)
    if (nzchar(product$summary) && grepl(
      "this (product|layer) is available in brim|may be useful for analysis|see the map for more information",
      product$summary, ignore.case = TRUE, perl = TRUE
    )) stop("BRIM Guide contains a generic filler summary.", call. = FALSE)
    related_articles <- as.character(product$relatedArticleIds)
    if (any(!related_articles %in% article_ids)) {
      stop("BRIM Guide Product references an unavailable Method.", call. = FALSE)
    }
    relationships <- product$relatedResources
    relationship_ids <- if (length(relationships)) {
      vapply(relationships, function(x) pt_guide_or(x$id), character(1))
    } else {
      character(0)
    }
    relationship_roles <- if (length(relationships)) {
      vapply(relationships, function(x) pt_guide_or(x$relationshipRole), character(1))
    } else {
      character(0)
    }
    relationship_deliveries <- if (length(relationships)) vapply(
      relationships, function(x) pt_guide_or(x$deliveryClass), character(1)
    ) else character(0)
    relationship_dispositions <- if (length(relationships)) vapply(
      relationships, function(x) pt_guide_or(x$coverageDisposition), character(1)
    ) else character(0)
    relationship_sources_valid <- if (length(relationships)) vapply(
      relationships, function(x) {
        source_ids <- unname(as.character(x$sourceResourceIds))
        length(source_ids) > 0L && !anyDuplicated(source_ids) &&
          all(source_ids %in% resource_ids) && x$id %in% source_ids
      }, logical(1)
    ) else logical(0)
    if (any(!nzchar(relationship_ids)) || any(!nzchar(relationship_roles)) ||
        any(!relationship_roles %in% c(
          "direct_match_in_brim", "selected_product_from_broader_resource",
          "source_reference"
        )) ||
        any(!relationship_deliveries %in% c(
          "provider_hosted", "brim_enhanced", "brim_managed", "not_applicable"
        )) ||
        any(!relationship_dispositions %in% c(
          "direct_resource_match", "selected_product_from_broader_resource",
          "multiple_source_resources", "provenance_only_no_public_resource",
          "internal_no_external_resource", "missing_resource_candidate"
        )) || any(!relationship_sources_valid) ||
        any(!relationship_ids %in% resource_ids) ||
        !identical(unname(as.character(product$relatedResourceIds)), unname(relationship_ids))) {
      stop("BRIM Guide Product Resource relationships must resolve exact schema-v2 links.",
           call. = FALSE)
    }
  }
  section_records <- c(bundle$products, bundle$articles)
  for (record in section_records) {
    sections <- record$sections
    if (!length(sections)) next
    section_ids <- vapply(sections, function(x) pt_guide_or(x$id), character(1))
    section_titles <- vapply(sections, function(x) pt_guide_or(x$title), character(1))
    section_has_content <- vapply(sections, function(x) {
      length(x$paragraphs) > 0L || length(x$items) > 0L || !is.null(x$table)
    }, logical(1))
    if (any(!nzchar(section_ids)) || anyDuplicated(section_ids) ||
        any(!nzchar(section_titles)) || any(!section_has_content)) {
      stop("BRIM Guide structured sections require unique IDs and nonblank titles.", call. = FALSE)
    }
  }
  update_dates <- as.Date(vapply(bundle$updates, `[[`, character(1), "date"))
  if (anyNA(update_dates) || is.unsorted(rev(update_dates), strictly = TRUE)) {
    stop("BRIM Guide Updates must be dated and ordered reverse chronologically.", call. = FALSE)
  }
  values <- unname(unlist(bundle, recursive = TRUE, use.names = FALSE))
  values <- as.character(values)
  forbidden <- c(
    "(^|[[:space:];=])/(Users|home|private|tmp|var|opt|Volumes)/",
    "(^|[[:space:]])[A-Za-z]:[\\\\/]", "BRIM_rehabilitation_(audit_staging|worktrees|builds)",
    "prototype diagnostic", "source_file", "editorial review"
  )
  for (pattern in forbidden) {
    if (any(grepl(pattern, values, ignore.case = TRUE, perl = TRUE))) {
      stop("BRIM Guide browser payload contains a forbidden source/audit value.", call. = FALSE)
    }
  }
  invisible(bundle)
}

pt_build_guide_bundle <- function(overlay_groups, map_display, profile_id = "default") {
  profiles <- pt_guide_supported_profiles()
  if (!profile_id %in% names(profiles)) stop("Unsupported current BRIM Guide profile: ", profile_id, call. = FALSE)
  if (!exists("pt_brim_application_identity", mode = "function")) stop("BRIM application identity seam is not loaded.", call. = FALSE)
  resource_registry <- pt_guide_read_resource_registry()
  published_resources <- pt_guide_resource_published_records(resource_registry)
  markers <- pt_guide_descriptive_markers()
  products <- c(
    pt_guide_local_products(overlay_groups, markers),
    pt_guide_external_products(catalog_markers = markers),
    pt_guide_ops_products(map_display, markers),
    pt_guide_basemap_products(),
    pt_guide_tool_products(map_display)
  )
  enrichment <- pt_guide_read_product_enrichment()
  product_universe_ids <- setdiff(
    vapply(products, `[[`, character(1), "id"),
    profiles[[profile_id]]$excluded_ids
  )
  relationship_registry <- pt_guide_read_product_resource_relationship_registry(
    product_universe_ids, resource_registry
  )
  products <- pt_guide_apply_product_enrichment(
    products, enrichment, relationship_registry,
    resource_registry = resource_registry,
    product_universe_ids = product_universe_ids
  )
  eligible_products <- Filter(function(product) {
    product$id %in% product_universe_ids
  }, products)
  browser_resources <- pt_guide_resource_browser_records(
    published_resources,
    eligible_products,
    relationship_registry
  )
  content <- pt_guide_authored_content(browser_resources)
  bundle <- c(
    list(
      schemaVersion = 4L,
      profileId = profile_id,
      identity = pt_brim_application_identity(),
      authority = list(
        catalogAuthority = "DESCRIPTIVE_ONLY",
        runtimeAuthority = "UNCHANGED",
        mapActions = "DEFERRED_TO_I2",
        coverage = "ALL_INCLUDED_NON_BASEMAP_VISIBLE_PRODUCTS_WITH_SOURCE_BACKED_VITALS"
      ),
      products = unname(products)
    ),
    content
  )
  bundle <- pt_project_guide_bundle(bundle, profiles[[profile_id]]$excluded_ids, profile_id)
  pt_validate_guide_bundle(bundle)
  bundle
}

pt_guide_embed_asset <- function(
  path, extension, mime_type, signature, label, header_bytes = 2048L
) {
  info <- file.info(path)
  if (!file.exists(path) || is.na(info$size) || info$size <= 0L ||
      tolower(tools::file_ext(path)) != extension) {
    stop("Missing or invalid approved ", label, ": ", path, call. = FALSE)
  }
  bytes <- readBin(path, what = "raw", n = as.integer(info$size))
  header <- rawToChar(bytes[seq_len(min(length(bytes), header_bytes))])
  if (!grepl(signature, header, ignore.case = TRUE, perl = TRUE)) {
    stop("Approved ", label, " has an invalid signature.", call. = FALSE)
  }
  paste0("data:", mime_type, ";base64,", base64enc::base64encode(bytes))
}

pt_add_brim_guide <- function(m, guide_bundle) {
  if (!requireNamespace("base64enc", quietly = TRUE)) stop("Package 'base64enc' is required to embed BRIM Guide assets.", call. = FALSE)
  js_path <- file.path("03_functions", "js", "leaflet_brim_guide.js")
  css_path <- file.path("03_functions", "css", "leaflet_brim_guide.css")
  if (!file.exists(js_path) || !file.exists(css_path)) stop("Missing BRIM Guide browser source.", call. = FALSE)
  js <- paste(readLines(js_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  css <- paste(readLines(css_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  assets <- list(
    topo = pt_guide_embed_asset(file.path("00_config", "Topo3whiteReversed.svg"), "svg", "image/svg+xml", "<svg[[:space:]>]", "Guide topo background"),
    doi = pt_guide_embed_asset(file.path("00_config", "doicolor.gif"), "gif", "image/gif", "^GIF8[79]a", "Guide DOI mark", header_bytes = 6L),
    blm = pt_guide_embed_asset(file.path("00_config", "blmlogo.svg"), "svg", "image/svg+xml", "<svg[[:space:]>]", "Guide BLM mark")
  )
  m <- htmlwidgets::prependContent(m, htmltools::tags$style(id = "brim-guide-style", htmltools::HTML(css)))
  htmlwidgets::onRender(m, js, data = list(bundle = guide_bundle, assets = assets))
}
