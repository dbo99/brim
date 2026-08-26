# ==== leaflet_guide_helpers.r ===============================================
##
## Build-time Product inventory, profile projection, and browser embedding for
## BRIM Guide. Runtime registries remain authoritative; the descriptive catalog
## can enrich a Product but can never create, activate, or suppress a map layer.
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

pt_guide_supported_profiles <- function() {
  list(
    default = list(
      profile_id = "default",
      current_authority_path = paste0(
        "00_config/config_map_display.r::MAP_DISPLAY + ",
        "05_map_build/04_build_portatreasure2_core_map.r::OVERLAY_GROUPS"
      ),
      excluded_ids = character(0)
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

pt_guide_subject <- function(title, path = character(0), theme = "") {
  text <- tolower(paste(c(title, path, theme), collapse = " "))
  rules <- list(
    "Groundwater" = "groundwater|monitoring well|bulletin 118|subsidence|well inventory|drilled well|gsp",
    "Snow & SWE" = "snow|swe|snow pillow|winter storm level",
    "Soil Moisture" = "soil moisture|\\bscan\\b",
    "Precipitation" = "precip|qpf|qpe|rain|cocorahs",
    "Fire & Burn Areas" = "fire|burn|smoke|wildfire",
    "Climate & Drought" = "drought|climate|temperature outlook|prism|bcmv8",
    "Weather & Forecasts" = "forecast|outlook|radar|nexrad|mrms|wind|weather|goes|warning|advis|wfo",
    "Water Rights" = "water right|swrcb|calwatrs|point of diversion|\\bpod\\b",
    "Geology & Subsidence" = "geolog|subsidence|seismic|fault",
    "Ecology & Habitat" = "ecolog|habitat|species|invasive|wetland|riparian",
    "Air Quality" = "air quality|air district|airnow|particulate",
    "Infrastructure" = "conveyance|canal|reservoir|infrastructure|calsim",
    "Land & Administrative Context" = "blm|office|county|district|boundary|wilderness|monument|grazing|drecp|acec|administrative|land status|surface management",
    "Surface Water" = "stream|river|surface water|hydro|huc|basin|spring|watershed|water supply|reservoir"
  )
  for (label in names(rules)) {
    if (grepl(rules[[label]], text, perl = TRUE)) return(label)
  }
  "Land & Administrative Context"
}

pt_guide_mode <- function(subsystem, title, service_type = "") {
  text <- tolower(paste(title, service_type))
  if (identical(subsystem, "Ops Live")) {
    if (grepl("forecast|outlook|nbm|hrrr|gfs|qpf|warning|advis", text)) return("Forecast / outlook")
    return("Live observation")
  }
  if (identical(subsystem, "External Layers")) return("External on-demand service")
  if (identical(subsystem, "Tools")) return("Guidance / method")
  if (identical(subsystem, "Basemaps / Local Layers") && grepl("basemap", text)) return("Static reference")
  if (grepl("histor|past|archive", text)) return("Historical context")
  if (grepl("screen|derived|model|prism|bcm|match|candidate", text)) return("Screening / derived")
  "Static reference"
}

pt_guide_related_resources <- function(provider, title) {
  text <- tolower(paste(provider, title))
  out <- character(0)
  if (grepl("blm|bureau of land management", text)) out <- c(out, "resource_blm_california")
  if (grepl("usgs|geological survey", text)) out <- c(out, "resource_usgs_water_dashboard")
  if (grepl("noaa|nws|cnrfc|wpc|cpc|mrms", text)) out <- c(out, "resource_noaa_nwps")
  unique(out)
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

pt_guide_resource_relationships <- function(ids, roles = character(0)) {
  ids <- unique(as.character(ids[nzchar(trimws(ids))]))
  if (!length(ids)) return(list())
  roles <- as.character(roles)
  lapply(seq_along(ids), function(i) {
    role <- if (length(roles) && !is.null(names(roles)) && ids[[i]] %in% names(roles)) {
      roles[[ids[[i]]]]
    } else if (length(roles) >= i) {
      roles[[i]]
    } else {
      "Related agency resource"
    }
    list(id = ids[[i]], role = pt_guide_or(role, "Related agency resource"))
  })
}

pt_guide_product <- function(
  id, title, subsystem, provider, path, subject, mode, family,
  summary = "", aliases = character(0), search_terms = character(0),
  sections = list(), related_article_ids = character(0),
  related_resource_ids = character(0), related_resource_roles = character(0),
  custom_or_non_generic = FALSE, content_tier = "basic"
) {
  resource_relationships <- pt_guide_resource_relationships(
    related_resource_ids,
    related_resource_roles
  )
  list(
    kind = "Product",
    id = as.character(id),
    title = as.character(title),
    subsystem = as.character(subsystem),
    provider = as.character(provider),
    path = unname(as.character(path)),
    pathLabel = pt_guide_path_label(path),
    subject = as.character(subject),
    mode = as.character(mode),
    family = as.character(family),
    summary = pt_guide_or(summary),
    aliases = unname(unique(as.character(aliases[nzchar(trimws(aliases))]))),
    searchTerms = unname(unique(as.character(search_terms[nzchar(trimws(search_terms))]))),
    sections = unname(sections),
    relatedArticleIds = unname(unique(as.character(related_article_ids))),
    relatedResourceIds = vapply(resource_relationships, `[[`, character(1), "id"),
    relatedResources = unname(resource_relationships),
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
      provider = provider,
      path = path,
      subject = pt_guide_subject(title, path),
      mode = pt_guide_mode("Basemaps / Local Layers", title),
      family = if (identical(category, "Monitoring Sites/Records")) "Local monitoring record" else "Local reference layer",
      aliases = pt_guide_aliases(row_id, title),
      related_resource_ids = pt_guide_related_resources(provider, title),
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
    search_terms <- c(row$agency, row$program, row$theme, row$pt2_usage_note,
                      row$best_use, row$notes, row$useful_for_visualization)
    pt_guide_product(
      id = row$external_layer_id,
      title = title,
      subsystem = "External Layers",
      provider = provider,
      path = path,
      subject = pt_guide_subject(title, path, row$theme),
      mode = pt_guide_mode("External Layers", title, row$service_type),
      family = paste("External", pt_guide_or(row$service_type, "service")),
      summary = summary,
      aliases = pt_guide_aliases(row$external_layer_id, title),
      search_terms = search_terms,
      related_resource_ids = pt_guide_related_resources(provider, title),
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
    title <- definition$source_token
    if (title %in% names(constant_names)) title <- unname(constant_names[[title]])
    path <- c("Ops Live", definition$category,
              if (nzchar(definition$subgroup)) definition$subgroup, title)
    provider <- if (grepl("USGS", title)) {
      "U.S. Geological Survey"
    } else if (grepl("USDA|SCAN", title)) {
      "USDA Natural Resources Conservation Service"
    } else if (grepl("CDEC|CalSim|CVP|SWP", title)) {
      "California water agencies / BRIM"
    } else {
      "NOAA / National Weather Service"
    }
    id <- registry$stable_id[[i]]
    products[[length(products) + 1L]] <- pt_guide_product(
      id = id,
      title = title,
      subsystem = "Ops Live",
      provider = provider,
      path = path,
      subject = pt_guide_subject(title, path),
      mode = pt_guide_mode("Ops Live", title),
      family = definition$category,
      aliases = pt_guide_aliases(id, title),
      related_resource_ids = pt_guide_related_resources(provider, title),
      custom_or_non_generic = isTRUE(catalog_markers[[id]])
    )
  }
  products
}

pt_guide_basemap_products <- function() {
  if (!exists("pt_base_groups", mode = "function")) stop("pt_base_groups() is required for BRIM Guide.", call. = FALSE)
  titles <- pt_base_groups()
  ids <- c(
    "basemap_usgs_hydrography", "basemap_usgs_topo", "basemap_usgs_national_map_imagery",
    "basemap_usgs_national_map_imagery_topo", "basemap_esri_world_topographic",
    "basemap_esri_world_street", "basemap_esri_world_imagery", "basemap_cartodb_positron",
    "basemap_openstreetmap", "basemap_none"
  )
  if (length(titles) != length(ids)) stop("Basemap Guide identity requires review after pt_base_groups() changed.", call. = FALSE)
  lapply(seq_along(titles), function(i) {
    provider <- if (grepl("USGS", titles[[i]])) "U.S. Geological Survey" else if (grepl("Esri", titles[[i]])) "Esri" else "Basemap provider"
    path <- c("Basemaps / Local Layers", "Basemaps", titles[[i]])
    pt_guide_product(
      id = ids[[i]], title = titles[[i]], subsystem = "Basemaps / Local Layers",
      provider = provider, path = path, subject = "Land & Administrative Context",
      mode = "Static reference", family = "Basemap",
      related_resource_ids = pt_guide_related_resources(provider, titles[[i]])
    )
  })
}

pt_guide_tool_products <- function(map_display) {
  if (!isTRUE(map_display$add_tools_adddata_panel)) return(list())
  definitions <- list(
    c("tool_measure", "Distance and area measurement", "Measurement"),
    c("tool_teaching_markup", "Draw and label markup", "Teaching / markup"),
    c("tool_external_gis_overlay", "External GIS URL overlay", "Add Data"),
    c("tool_local_gis_upload", "Local GIS file upload", "Add Data")
  )
  if (isTRUE(map_display$add_blm_sma_context_overlay)) {
    definitions <- c(definitions, list(c("tool_blm_sma_context", "BLM Surface Management Agency context", "Reference tool")))
  }
  lapply(definitions, function(definition) {
    path <- c("Tools", definition[[3]], definition[[2]])
    pt_guide_product(
      id = definition[[1]], title = definition[[2]], subsystem = "Tools",
      provider = "BRIM", path = path,
      subject = pt_guide_subject(definition[[2]], path),
      mode = "Guidance / method", family = definition[[3]],
      related_resource_ids = if (definition[[1]] == "tool_blm_sma_context") "resource_blm_california" else character(0)
    )
  })
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

pt_guide_external_catalog_row <- function(
    external_layer_id,
    catalog_path = file.path("00_config", "external_service_catalog.csv")) {
  x <- utils::read.csv(
    catalog_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
  row <- x[x$external_layer_id == external_layer_id, , drop = FALSE]
  if (nrow(row) != 1L) {
    stop("Expected one External catalog row for curated Guide Product ", external_layer_id, ".", call. = FALSE)
  }
  row
}

pt_guide_external_source_resource <- function(row, id, title) {
  list(
    kind = "Resource",
    id = id,
    title = title,
    provider = pt_guide_first(row$agency, row$program),
    summary = pt_guide_first(row$notes, row$pt2_usage_note),
    url = pt_guide_first(row$source_page, row$service_url),
    relatedProductIds = as.character(row$external_layer_id)
  )
}

pt_guide_article <- function(id, title, summary, sections, related_product_ids = character(0), aliases = character(0)) {
  list(
    kind = "Article",
    id = id,
    title = title,
    section = "Methods & Guides",
    summary = summary,
    sections = unname(sections),
    relatedProductIds = unname(unique(as.character(related_product_ids))),
    aliases = unname(unique(as.character(aliases)))
  )
}

pt_guide_curated_product <- function(
    summary, sections, related_article_ids, related_resource_ids, related_resource_roles) {
  relationships <- pt_guide_resource_relationships(
    related_resource_ids,
    related_resource_roles
  )
  list(
    summary = summary,
    sections = unname(sections),
    relatedArticleIds = unname(unique(as.character(related_article_ids))),
    relatedResourceIds = vapply(relationships, `[[`, character(1), "id"),
    relatedResources = unname(relationships),
    contentTier = "curated"
  )
}

pt_guide_authored_content <- function() {
  if (!exists("PT_HUC_THEME_REGISTRY", inherits = TRUE)) {
    stop("PT_HUC_THEME_REGISTRY must be loaded before compiling curated Guide content.", call. = FALSE)
  }
  if (!exists("pt_polygon_generalization_read_registry", mode = "function")) {
    stop("Polygon generalization authority must be loaded before compiling curated Guide content.", call. = FALSE)
  }
  if (!exists("PT_BULLETIN118_SGMA_SOURCE_PAGE", inherits = TRUE)) {
    stop("Bulletin 118 source authority must be loaded before compiling curated Guide content.", call. = FALSE)
  }

  generalization <- pt_polygon_generalization_read_registry()
  huc8_geometry <- pt_polygon_generalization_registry_row("huc8", generalization)
  bulletin_geometry <- pt_polygon_generalization_registry_row("bulletin118", generalization)
  huc_modes <- unname(vapply(PT_HUC_THEME_REGISTRY, `[[`, character(1), "label"))
  fire_recent <- pt_guide_external_catalog_row("EXT070")
  fire_current <- pt_guide_external_catalog_row("EXT074")

  articles <- list(
    pt_guide_article(
      "method_how_brim_works",
      "How BRIM Works",
      "How BRIM combines embedded reference layers, on-demand services, live feeds, and map tools without treating them as one loading or lifecycle system.",
      list(
        pt_guide_section(
          "product_families",
          "Product families",
          items = c(
            "Local layers are embedded or retained reference Products assembled into the standalone map.",
            "External layers are provider services loaded on demand, often for the current map view.",
            "Ops Live Products consume feed-specific current-observation or forecast contracts when activated.",
            "Tools provide measurement, drawing, upload, and other map workflows rather than data layers."
          )
        ),
        pt_guide_section(
          "guide_boundary",
          "Guide boundary",
          paragraphs = "Guide I1 is a read-only index into current BRIM authority. Product existence and paths come from runtime/build definitions; the descriptive catalog can enrich records but cannot control the map."
        ),
        pt_guide_section(
          "huc_climate_recharge",
          "HUC climate and recharge summaries",
          paragraphs = c(
            "BRIM summarizes 1991–2020 PRISM precipitation and BCMv8 recharge rasters to HUC polygons using exact polygon weights, then joins compact geometry-free tables to the existing map geometry.",
            "The map precomputes level-specific theme bins, labels, and colors and restyles the existing HUC polygons in place; it does not embed duplicate HUC geometry for each theme."
          )
        )
      ),
      related_product_ids = c("huc8", "gw_bull118", "EXT070", "EXT074"),
      aliases = c("BRIM architecture", "BRIM basics", "HUC climate recharge method")
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
      "SCAN Soil-Moisture Statistical Context",
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
      "How to read source times, forecast cycles, valid times, freshness, and refresh behavior across Ops Live Products.",
      list(
        pt_guide_section(
          "timing_semantics",
          "Feed-specific timing",
          items = c(
            "The separate feed repository acquires, normalizes, publishes, and monitors live artifacts; BRIM fetches and renders those documented contracts.",
            "Observation Products expose provider/source observation times and feed-specific freshness or age status when available.",
            "Forecast Products preserve their own cycle time, valid time, and lead; an exact unavailable target is not replaced with a nearby time or another cycle.",
            "Activating or refreshing one Product does not make neighboring feeds share its update cadence or currency."
          )
        )
      ),
      related_product_ids = c(
        "ops_scan_soil_moisture", "ops_snow_pillow_swe",
        "product-ops-usgs-groundwater", "winter_storm_levels", "nbm_qpf",
        "product-ops-nbm-accumulated-qpf"
      ),
      aliases = c("freshness", "feed cadence", "cycle time", "valid time")
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
            "Guide Product coverage is compiled after the current map profile and visible layer groups are known, then embedded before browser startup.",
            "Browser controllers own interaction and teardown for their layer families; Guide I1 does not activate map layers or fetch Guide content at runtime.",
            "Large raw inputs, processed products, caches, realistic HTML, and screenshots remain external to the tracked source repository."
          )
        )
      ),
      aliases = c("technical architecture", "standalone Leaflet", "build pipeline")
    )
  )

  resources <- list(
    list(kind = "Resource", id = "resource_doi", title = "U.S. Department of the Interior",
         provider = "U.S. Department of the Interior", summary = "Department-level information and programs.", url = "https://www.doi.gov/", relatedProductIds = character(0)),
    list(kind = "Resource", id = "resource_blm_california", title = "BLM California",
         provider = "Bureau of Land Management", summary = "Official BLM California programs, offices, and public information.", url = "https://www.blm.gov/california", relatedProductIds = c("huc8", "gw_bull118")),
    list(kind = "Resource", id = "resource_prism_normals", title = "PRISM 1991–2020 Climate Normals",
         provider = "PRISM Climate Group, Oregon State University", summary = "Official PRISM 30-year normals access and documentation for the 1991–2020 period.", url = "https://prism.oregonstate.edu/normals/", relatedProductIds = "huc8"),
    list(kind = "Resource", id = "resource_usgs_bcmv8", title = "USGS Basin Characterization Model (BCMv8)",
         provider = "U.S. Geological Survey", summary = "Official BCMv8 model and data-release context for hydrologic California.", url = "https://www.sciencebase.gov/catalog/item/5f29c62d82cef313ed9edb39", relatedProductIds = "huc8"),
    list(kind = "Resource", id = "resource_dwr_bulletin118_sgma_2019", title = "DWR Bulletin 118 SGMA 2019 Basin Prioritization",
         provider = "California Department of Water Resources", summary = "Official final 2019 SGMA basin-prioritization service used for BRIM's exact code-based attribute join.", url = PT_BULLETIN118_SGMA_SOURCE_PAGE, relatedProductIds = "gw_bull118"),
    pt_guide_external_source_resource(fire_recent, "resource_calfire_fire_perimeters", "CAL FIRE FRAP Fire Perimeters"),
    pt_guide_external_source_resource(fire_current, "resource_nifc_wfigs_current", "NIFC WFIGS Current Interagency Fire Perimeters"),
    list(kind = "Resource", id = "resource_usgs_water_dashboard", title = "USGS National Water Dashboard",
         provider = "U.S. Geological Survey", summary = "Official current water information and station context from USGS.", url = "https://dashboard.waterdata.usgs.gov/", relatedProductIds = "product-ops-usgs-groundwater"),
    list(kind = "Resource", id = "resource_noaa_nwps", title = "NOAA National Water Prediction Service",
         provider = "NOAA / National Weather Service", summary = "Official river observations, forecasts, and water-prediction context.", url = "https://water.noaa.gov/", relatedProductIds = character(0))
  )

  updates <- list(
    list(kind = "Update", id = "update_read_only_layer_explorer", title = "Read-only Layer Explorer added",
         date = "2026-08-24", updateType = "Interface", summary = "Added a read-only view of embedded descriptive catalog metadata; current runtime construction remains authoritative and the explorer cannot control map layers.", relatedProductIds = character(0)),
    list(kind = "Update", id = "update_nbm_accumulated_qpf", title = "NBM accumulated QPF forecast windows added",
         date = "2026-08-20", updateType = "Forecast Product", summary = "Added exact-cycle 0–10 day accumulated-QPF windows computed from verified six-hour numeric companions; partial totals are not rendered.", relatedProductIds = "product-ops-nbm-accumulated-qpf"),
    list(kind = "Update", id = "update_nbm_legend_links", title = "NBM legend links simplified",
         date = "2026-08-19", updateType = "Usability", summary = "Removed redundant per-row NBM legend links while retaining the shared forecast-guidance control behavior.", relatedProductIds = c("winter_storm_levels", "nbm_qpf"))
  )

  quick_access <- list(
    list(id = "quick_huc8", label = "HUC8 – PRISM/BCMv8", productIds = "huc8",
         summary = "HUC8 boundaries with BLM-managed-land, PRISM precipitation, and BCMv8 recharge display context."),
    list(id = "quick_groundwater_basins", label = "Groundwater Basins – Bulletin 118", productIds = "gw_bull118",
         summary = "Bulletin 118 groundwater basins with SGMA 2019 priority and BLM-managed-land context."),
    list(id = "quick_fire_perimeters", label = "Fire Perimeters", productIds = c("EXT070", "EXT074"),
         summary = "Two complementary perimeter Products: CAL FIRE recent large-fire context and NIFC current operational wildfire/complex perimeters. Their coverage and currency differ; review each Product before use.")
  )

  curated_products <- list(
    huc8 = pt_guide_curated_product(
      "HUC8 watershed boundaries with BLM-managed-land percentages and 1991–2020 PRISM precipitation and BCMv8 recharge summaries.",
      list(
        pt_guide_section("huc8_display_modes", "Available display modes", items = huc_modes),
        pt_guide_section(
          "huc8_controls", "Controls",
          items = c(
            "Minimum BLM-managed land filter: 0–100% in one-percentage-point steps; 0% includes every retained HUC8 feature.",
            "The shared HUC card follows the focused active HUC level and shows that level's visible count and legend."
          )
        ),
        pt_guide_section(
          "huc8_processing", "How BRIM prepares it",
          items = c(
            "HUC and BLM-intersection areas are calculated in EPSG:3310; PRISM and BCMv8 rasters are summarized with exact polygon weights.",
            "Geometry-free climate/recharge tables are joined to the existing simplified HUC map geometry.",
            "Theme bins, labels, and colors are precomputed by HUC level; the browser restyles the existing polygons in place rather than creating duplicate geometry."
          )
        ),
        pt_guide_section(
          "huc8_limitations", "Interpretation & limitations",
          items = c(
            "Precipitation and recharge color classes are calculated separately for each HUC level; compare values through the active legend rather than comparing colors across levels.",
            "Inches per year are area-normalized depths; thousand acre-feet per year are total-volume estimates and are strongly influenced by watershed area."
          )
        ),
        pt_guide_section(
          "huc8_geometry", "Display geometry",
          items = c(
            pt_guide_generalization_detail(huc8_geometry),
            as.character(huc8_geometry$public_disclosure),
            "Canonical analytical HUC geometry remains unsimplified; the map-facing display geometry is delivered in EPSG:4326."
          )
        ),
        pt_guide_section(
          "huc8_provenance", "Period & provenance",
          items = c(
            "Precipitation: PRISM 1991–2020 precipitation normal vM5.",
            "Recharge: USGS Basin Characterization Model version 8 (BCMv8), 1991–2020 recharge."
          )
        )
      ),
      c("method_how_brim_works", "method_display_geometry_generalization"),
      c("resource_prism_normals", "resource_usgs_bcmv8", "resource_blm_california"),
      c(
        resource_prism_normals = "Precipitation source",
        resource_usgs_bcmv8 = "Recharge model and source",
        resource_blm_california = "BLM program context"
      )
    ),
    gw_bull118 = pt_guide_curated_product(
      "California DWR Bulletin 118 groundwater basin and subbasin boundaries with final SGMA 2019 prioritization and BLM-managed-land percentage context.",
      list(
        pt_guide_section(
          "bulletin118_display_modes", "Available display modes",
          items = c("Basins only", "DWR SGMA 2019 Basin Prioritization", "BLM-managed land — %")
        ),
        pt_guide_section(
          "bulletin118_controls", "Controls",
          items = c(
            "Minimum BLM-managed land filter: 0–100% in one-percentage-point steps.",
            "Find basin or subbasin searches all 515 retained basin records and can reveal a result hidden by the active threshold."
          )
        ),
        pt_guide_section(
          "bulletin118_processing", "How BRIM prepares it",
          items = c(
            "BRIM joins the tracked 515-row DWR SGMA 2019 table to 515 retained basin geometries by exact basin/subbasin code; names do not participate in the join.",
            "The SGMA and fixed percentage-BLM themes restyle the original basin polygons in place; no second polygon population or runtime DWR request is created."
          )
        ),
        pt_guide_section(
          "bulletin118_limitations", "Interpretation & limitations",
          items = c(
            "The priority theme represents DWR's final 2019 SGMA categories, not a newly inferred or continuously updated BRIM priority.",
            "BLM-managed-land percentage is screening context calculated from retained BRIM geometry; use authoritative sources for boundary-sensitive decisions."
          )
        ),
        pt_guide_section(
          "bulletin118_geometry", "Display geometry",
          items = c(
            pt_guide_generalization_detail(bulletin_geometry),
            as.character(bulletin_geometry$public_disclosure),
            "Full-resolution analytical basin geometry remains separate from the map-facing display geometry."
          )
        )
      ),
      c("method_how_brim_works", "method_display_geometry_generalization"),
      c("resource_dwr_bulletin118_sgma_2019", "resource_blm_california"),
      c(
        resource_dwr_bulletin118_sgma_2019 = "Basin-priority source",
        resource_blm_california = "BLM program context"
      )
    ),
    EXT070 = pt_guide_curated_product(
      pt_guide_first(fire_recent$pt2_usage_note, fire_recent$notes),
      list(
        pt_guide_section(
          "fire_recent_controls", "Loading & refresh",
          items = pt_guide_or(fire_recent$large_layer_warning)
        ),
        pt_guide_section(
          "fire_recent_processing", "How BRIM prepares it",
          items = c(
            "BRIM requests the configured CAL FIRE FRAP feature layer for the current map view.",
            pt_guide_or(fire_recent$legend_note)
          )
        ),
        pt_guide_section(
          "fire_recent_limitations", "Currency & limitations",
          items = c(pt_guide_or(fire_recent$notes), pt_guide_or(fire_recent$pt2_usage_note))
        )
      ),
      "method_how_brim_works",
      "resource_calfire_fire_perimeters",
      c(resource_calfire_fire_perimeters = "Perimeter source and limitations")
    ),
    EXT074 = pt_guide_curated_product(
      pt_guide_first(fire_current$pt2_usage_note, fire_current$notes),
      list(
        pt_guide_section(
          "fire_current_controls", "Loading & refresh",
          items = pt_guide_or(fire_current$large_layer_warning)
        ),
        pt_guide_section(
          "fire_current_processing", "How BRIM prepares it",
          items = c(
            "BRIM requests a current-view WFIGS snapshot and filters to wildfire and complex categories where the service supports SQL; prescribed-fire records are excluded from this Product.",
            pt_guide_or(fire_current$legend_note)
          )
        ),
        pt_guide_section(
          "fire_current_limitations", "Currency & limitations",
          items = c(pt_guide_or(fire_current$notes), pt_guide_or(fire_current$pt2_usage_note))
        )
      ),
      "method_how_brim_works",
      "resource_nifc_wfigs_current",
      c(resource_nifc_wfigs_current = "Operational perimeter source")
    )
  )

  list(
    articles = articles,
    resources = resources,
    updates = updates,
    quickAccess = quick_access,
    curatedProducts = curated_products
  )
}

pt_guide_apply_curated_content <- function(products, curated_products) {
  product_ids <- vapply(products, `[[`, character(1), "id")
  missing <- setdiff(names(curated_products), product_ids)
  if (length(missing)) {
    stop("Curated Guide content references unavailable Product(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  lapply(products, function(product) {
    enrichment <- curated_products[[product$id]]
    if (is.null(enrichment)) return(product)
    for (field in names(enrichment)) product[[field]] <- enrichment[[field]]
    product
  })
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
  bundle$resources <- lapply(bundle$resources, prune_related)
  bundle$updates <- lapply(bundle$updates, prune_related)
  bundle$quickAccess <- Filter(function(item) all(item$productIds %in% product_ids), bundle$quickAccess)
  resource_ids <- vapply(bundle$resources, `[[`, character(1), "id")
  article_ids <- vapply(bundle$articles, `[[`, character(1), "id")
  bundle$products <- lapply(bundle$products, function(product) {
    product$relatedArticleIds <- intersect(product$relatedArticleIds, article_ids)
    product$relatedResources <- Filter(function(relationship) {
      pt_guide_or(relationship$id) %in% resource_ids
    }, product$relatedResources)
    product$relatedResourceIds <- unname(vapply(
      product$relatedResources,
      `[[`,
      character(1),
      "id"
    ))
    product
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
  if (any(!nzchar(paths)) || anyDuplicated(paths)) stop("BRIM Guide Product paths must be nonblank and unique.", call. = FALSE)
  if (any(grepl("(^| / )Points( / |$)", paths))) stop("BRIM Guide paths must use Monitoring Sites/Records terminology.", call. = FALSE)
  quick_ids <- unlist(lapply(bundle$quickAccess, `[[`, "productIds"), use.names = FALSE)
  if (any(!quick_ids %in% product_ids)) stop("BRIM Guide Quick Access references an unavailable Product.", call. = FALSE)
  resource_ids <- vapply(bundle$resources, `[[`, character(1), "id")
  article_ids <- vapply(bundle$articles, `[[`, character(1), "id")
  for (product in bundle$products) {
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
      vapply(relationships, function(x) pt_guide_or(x$role), character(1))
    } else {
      character(0)
    }
    if (any(!nzchar(relationship_ids)) || any(!nzchar(relationship_roles)) ||
        any(!relationship_ids %in% resource_ids) ||
        !identical(unname(as.character(product$relatedResourceIds)), unname(relationship_ids))) {
      stop("BRIM Guide Product Resource relationships must resolve with nonblank roles.", call. = FALSE)
    }
  }
  section_records <- c(bundle$products, bundle$articles)
  for (record in section_records) {
    sections <- record$sections
    if (!length(sections)) next
    section_ids <- vapply(sections, function(x) pt_guide_or(x$id), character(1))
    section_titles <- vapply(sections, function(x) pt_guide_or(x$title), character(1))
    if (any(!nzchar(section_ids)) || anyDuplicated(section_ids) || any(!nzchar(section_titles))) {
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
  markers <- pt_guide_descriptive_markers()
  products <- c(
    pt_guide_local_products(overlay_groups, markers),
    pt_guide_external_products(catalog_markers = markers),
    pt_guide_ops_products(map_display, markers),
    pt_guide_basemap_products(),
    pt_guide_tool_products(map_display)
  )
  content <- pt_guide_authored_content()
  products <- pt_guide_apply_curated_content(products, content$curatedProducts)
  content$curatedProducts <- NULL
  bundle <- c(
    list(
      schemaVersion = 2L,
      profileId = profile_id,
      identity = pt_brim_application_identity(),
      authority = list(
        catalogAuthority = "DESCRIPTIVE_ONLY",
        runtimeAuthority = "UNCHANGED",
        mapActions = "DEFERRED_TO_I2",
        coverage = "ALL_INCLUDED_VISIBLE_PRODUCTS_WITH_CURATED_QUICK_ACCESS_CONTENT_FLOOR"
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
