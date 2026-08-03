# ==== layer_capability_helpers.r ============================================
##
## Panel-neutral layer-capability resolution and QA output. External Layers is
## the first consumer, but the schema and definitions deliberately avoid
## External-specific names.

PT_LAYER_CAPABILITY_SCHEMA_VERSION <- "1.2"
PT_LAYER_INVENTORY_SCHEMA_VERSION <- "1.2"
PT_CAPABILITY_GIT_HEAD_UNAVAILABLE <- "unavailable — build workspace is not a Git checkout"

pt_layer_capability_definitions <- function() {
  list(
    LGND = list(
      code = "LGND",
      label = "Map legend available",
      capability = "has_legend"
    ),
    INFO = list(
      code = "INFO",
      label = "Feature details available by hover or click",
      capability = "has_feature_info"
    )
  )
}

pt_capability_text <- function(x) {
  if (is.null(x) || length(x) == 0L) x <- ""
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

pt_capability_truth <- function(x) {
  tolower(pt_capability_text(x)) %in% c("true", "t", "1", "yes", "y")
}

pt_capability_slug <- function(x) {
  x <- iconv(pt_capability_text(x), to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(gsub("[^a-zA-Z0-9]+", "_", x))
  x <- gsub("^_+|_+$", "", x)
  ifelse(nzchar(x), x, "other")
}

pt_capability_git_head_display <- function(git_head) {
  git_head <- pt_capability_text(git_head)[1]
  if (nzchar(git_head)) git_head else PT_CAPABILITY_GIT_HEAD_UNAVAILABLE
}

pt_capability_git_head <- function(work_dir = getwd()) {
  work_dir <- pt_capability_text(work_dir)[1]
  if (!nzchar(work_dir) || !dir.exists(work_dir)) {
    return(PT_CAPABILITY_GIT_HEAD_UNAVAILABLE)
  }
  out <- tryCatch(
    suppressWarnings(system2(
      "git",
      c("-C", shQuote(normalizePath(work_dir, mustWork = TRUE)), "rev-parse", "HEAD"),
      stdout = TRUE,
      stderr = FALSE
    )),
    error = function(e) character()
  )
  if (length(out) == 1L && grepl("^[0-9a-fA-F]{40}$", out)) {
    unname(out)
  } else {
    PT_CAPABILITY_GIT_HEAD_UNAVAILABLE
  }
}

pt_layer_capability_registration_columns <- function() {
  c(
    "panel", "stable_layer_id", "layer_key", "has_legend",
    "has_feature_info", "feature_info_hover", "feature_info_popup",
    "feature_info_identify", "feature_info_equivalent", "legend_type",
    "legend_adapter", "info_adapter", "registration_source", "content_basis"
  )
}

pt_new_layer_capability_registry <- function() {
  out <- as.data.frame(
    setNames(rep(list(character()), length(pt_layer_capability_registration_columns())),
             pt_layer_capability_registration_columns()),
    stringsAsFactors = FALSE
  )
  for (nm in c("has_legend", "has_feature_info", "feature_info_hover",
               "feature_info_popup", "feature_info_identify",
               "feature_info_equivalent")) out[[nm]] <- logical()
  out
}

pt_register_layer_capability <- function(
    registry,
    panel,
    stable_layer_id,
    has_legend = FALSE,
    has_feature_info = FALSE,
    feature_info_hover = FALSE,
    feature_info_popup = FALSE,
    feature_info_identify = FALSE,
    feature_info_equivalent = FALSE,
    legend_type = "",
    legend_adapter = "",
    info_adapter = "",
    registration_source = "explicit_registration",
    content_basis = "") {
  panel <- tolower(pt_capability_text(panel))
  if (!panel %in% c("external", "local", "ops_live")) {
    stop("Unsupported layer-capability panel: ", panel, call. = FALSE)
  }
  stable_layer_id <- pt_capability_text(stable_layer_id)
  if (!nzchar(stable_layer_id)) stop("Capability registration requires a stable layer ID.", call. = FALSE)
  row <- data.frame(
    panel = panel,
    stable_layer_id = stable_layer_id,
    layer_key = paste0(panel, ":", stable_layer_id),
    has_legend = isTRUE(has_legend),
    has_feature_info = isTRUE(has_feature_info),
    feature_info_hover = isTRUE(feature_info_hover),
    feature_info_popup = isTRUE(feature_info_popup),
    feature_info_identify = isTRUE(feature_info_identify),
    feature_info_equivalent = isTRUE(feature_info_equivalent),
    legend_type = pt_capability_text(legend_type),
    legend_adapter = pt_capability_text(legend_adapter),
    info_adapter = pt_capability_text(info_adapter),
    registration_source = pt_capability_text(registration_source),
    content_basis = pt_capability_text(content_basis),
    stringsAsFactors = FALSE
  )
  rbind(registry, row)
}

pt_layer_legend_adapter_catalog <- function() {
  out <- data.frame(
    adapter = c(
      "drought_monitor", "cpc_outlook", "stream_gauge_flow",
      "wcr_completed_depth", "fire_year", "mlrs_mineral_cases",
      "sgma_prioritization", "nifc_current_fire", "aml_status",
      "calipc_ramp", "swrcb_ir_status", "subsidence_observation",
      "dwr_tre_insar_points", "generic_categorical", "alert_camera",
      "alert_camera_viewshed", "uic_explorer"
    ),
    renderer_function = c(
      "ptDroughtMonitorLegendHtml", "ptCpcOutlookLegendHtml",
      "ptStreamGaugeFlowLegendHtml", "ptWcrCompletedDepthLegendHtml",
      "ptFireYearLegendHtml", "ptMlrsMineralCasesLegendHtml",
      "ptSgmaPrioritizationLegendHtml", "ptNifcCurrentFireLegendHtml",
      "ptAmlStatusLegendHtml", "ptCalIpcRampLegendHtml",
      "ptSwrcbIrListingStatusLegendHtml", "ptSubsidenceObservationLegendHtml",
      "ptDwrTreInsarPointLegendHtml", "ptGenericCategoricalLegendHtml",
      "ptAlertCameraLegendHtml", "ptAlertCameraViewshedLegendHtml",
      "sourceRowsHtml"
    ),
    renderer_source = c(rep("external", 16L), "uic"),
    legend_type = c(
      rep("inline", 5L), "dynamic_map_card", "dynamic_map_card",
      rep("inline", 4L), "dynamic_map_card", "inline",
      "style_note", rep("inline", 2L),
      "dynamic_map_card"
    ),
    legend_status = c(
      "brim_hidden_or_manual", "partial_brim_legend",
      "brim_hidden_or_manual", "brim_automatic",
      "brim_hidden_or_manual", "brim_shared_automatic",
      "brim_automatic", "style_note_only", "brim_hidden_or_manual",
      "style_note_only", "brim_hidden_or_manual", "brim_automatic",
      "brim_hidden_or_manual", "style_note_only",
      "brim_hidden_or_manual", "brim_hidden_or_manual",
      "brim_shared_automatic"
    ),
    legend_surface = c(
      rep("collapsed_active_external_details", 3L), "map_card",
      "collapsed_active_external_details", "shared_map_card", "map_card",
      rep("collapsed_active_external_details", 4L), "map_card",
      rep("collapsed_active_external_details", 4L), "leaflet_control"
    ),
    legend_mount_path = c(
      rep("ptRenderCustomLayerList -> #pt-active-external-body", 3L),
      "ptUpdateWcrCompletedDepthMapLegend -> .pt-wcr-completed-depth-map-legend",
      "ptRenderCustomLayerList -> #pt-active-external-body",
      "ptUpdateMlrsMineralCasesMapLegend -> .pt-mlrs-mineral-cases-map-legend",
      "ptUpdateSgmaPrioritizationMapLegend -> .pt-sgma-prioritization-map-legend",
      rep("ptRenderCustomLayerList -> #pt-active-external-body", 4L),
      "ptUpdateSubsidenceObservationMapLegend -> .pt-subsidence-observation-map-legend",
      rep("ptRenderCustomLayerList -> #pt-active-external-body", 4L),
      "BRIM.uicExplorer.upsertExternal -> L.control(bottomleft)"
    ),
    legend_shared_with = c(
      rep("", 5L), "MLRS mineral-case layer family", "", rep("", 9L),
      "External UIC Explorer source rows"
    ),
    legend_automatic_mount = c(
      FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE,
      FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE
    ),
    legend_keyed_symbology = c(
      TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE,
      FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE
    ),
    structured_style_mapping_available = TRUE,
    legend_candidate_basis = c(
      "keyed renderer is confined to collapsed Active external details",
      "partial probability legend does not key all rendered bins",
      "keyed renderer is confined to collapsed Active external details",
      "activation automatically mounts a keyed WCR completed-depth map card",
      "keyed renderer is confined to collapsed Active external details",
      "activation automatically mounts the shared keyed MLRS map card",
      "activation automatically mounts a keyed SGMA map card",
      "prose style description without a concrete key",
      "keyed renderer is confined to collapsed Active external details",
      "prose style description without a concrete key",
      "keyed renderer is confined to collapsed Active external details",
      "activation automatically mounts a keyed subsidence-observation map card",
      "keyed renderer is confined to collapsed Active external details",
      "runtime categorical palette without a concrete value key",
      "keyed renderer is confined to collapsed Active external details",
      "keyed renderer is confined to collapsed Active external details",
      "activation automatically mounts the shared keyed UIC Leaflet control"
    ),
    stringsAsFactors = FALSE
  )
  out$legend_renderer_key <- out$renderer_function
  out$qualifies_lgnd <- out$legend_status %in% c(
    "brim_automatic", "brim_shared_automatic"
  ) & out$legend_automatic_mount & out$legend_keyed_symbology
  out
}

pt_layer_legend_status_values <- function() {
  c(
    "brim_automatic", "brim_shared_automatic", "brim_hidden_or_manual",
    "partial_brim_legend", "renderer_available_unmounted",
    "style_mapping_available", "style_note_only",
    "provider_reference_only", "none"
  )
}

pt_strict_has_legend <- function(status, automatic_mount, keyed_symbology) {
  automatic_mount <- as.logical(automatic_mount)
  keyed_symbology <- as.logical(keyed_symbology)
  automatic_mount[is.na(automatic_mount)] <- FALSE
  keyed_symbology[is.na(keyed_symbology)] <- FALSE
  pt_capability_text(status) %in% c("brim_automatic", "brim_shared_automatic") &
    automatic_mount & keyed_symbology
}

pt_validate_legend_adapter_implementations <- function(
    external_js_path = file.path("03_functions", "js", "leaflet_tools_adddata_panel.js"),
    uic_js_path = file.path("03_functions", "js", "brim_uic_explorer.js")) {
  paths <- c(external = external_js_path, uic = uic_js_path)
  missing_paths <- paths[!file.exists(paths)]
  if (length(missing_paths)) stop("Missing legend renderer source: ", paste(missing_paths, collapse = ", "), call. = FALSE)
  source_text <- lapply(paths, function(path) paste(readLines(path, warn = FALSE), collapse = "\n"))
  adapters <- pt_layer_legend_adapter_catalog()
  missing <- vapply(seq_len(nrow(adapters)), function(i) {
    fn <- adapters$renderer_function[i]
    src <- adapters$renderer_source[i]
    !grepl(paste0("function\\s+", fn, "\\s*\\("), source_text[[src]], perl = TRUE)
  }, logical(1))
  if (any(missing)) {
    stop(
      "Registered legend adapters have no renderer implementation: ",
      paste(adapters$adapter[missing], collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

pt_record_value <- function(record, name) {
  if (!name %in% names(record)) return("")
  pt_capability_text(record[[name]][1])
}

pt_external_legend_adapter_keys <- function(record) {
  field <- tolower(pt_record_value(record, "default_style_field"))
  method <- tolower(pt_record_value(record, "default_style_method"))
  units <- tolower(pt_record_value(record, "style_units"))
  title <- tolower(pt_record_value(record, "style_legend_title"))
  name <- tolower(pt_record_value(record, "display_name"))
  url <- tolower(pt_record_value(record, "service_url"))
  legend_url <- tolower(pt_record_value(record, "legend_url"))

  has <- function(x, needle) nzchar(x) && grepl(needle, x, fixed = TRUE)
  drought <- field == "dm" || has(method, "usdm") || has(name, "drought monitor")
  cpc <- has(method, "cpc") || has(name, "cpc ") || has(name, "climate prediction center") || has(title, "cpc")
  stream <- field == "flow_cfs" || has(method, "flow_cfs") || has(method, "streamflow") || has(name, "live stream") || (has(title, "flow") && has(title, "cfs"))
  wcr <- has(method, "wcr_completed_depth") || (field == "totalcompleteddepth" && has(name, "well completion")) || (has(name, "well completion reports") && has(title, "completed depth"))
  fire <- field %in% c("year_", "year") || has(title, "fire year") || (has(units, "year") && any(vapply(c("fire", "burn", "perimeter"), function(x) has(name, x), logical(1)))) || has(method, "fire_year")
  mlrs <- has(method, "mlrs_case_status") || has(name, "blm mlrs") || has(url, "mlrs") || has(url, "miningclaims")
  sgma <- has(method, "sgma_basin_prioritization") || has(name, "sgma 2019 basin prioritization") || has(title, "sgma 2019 basin prioritization") || has(paste(url, legend_url), "sgma_2019_basin_prioritization")
  nifc <- has(method, "nifc_current_fire") || has(name, "nifc current wildfire") || has(url, "wfigs_interagency_perimeters_current")
  aml <- has(method, "aml_status") || has(name, "abandoned mine") || has(name, "aml") || has(url, "mapped_abandoned_mine_features")
  calipc <- has(method, "calipc_species_count") || has(method, "calipc_invasive_threat") || (has(name, "cal-ipc") && (has(name, "species count") || has(name, "invasion level")))
  swrcb <- has(method, "swrcb_ir_listing_status") || has(name, "2024 integrated report") || has(title, "integrated report listing status")
  subsidence <- has(method, "subsidence_observation") || has(method, "subsidence_obs") || (has(name, "land subsidence observations") && field == "data_source") || has(title, "subsidence observation")
  dwr_tre <- has(method, "dwr_tre_insar_point_location") || has(method, "tre_insar_point_location") || has(title, "dwr/tre 2026q1 insar point") || (has(name, "dwr/tre 2026q1") && has(name, "insar point")) || has(url, "vertical_displacement_point_data_locations_2026q1")
  alert <- has(method, "alert_camera") || has(name, "alertcalifornia") || has(title, "alertcalifornia")
  alert_viewshed <- has(method, "alert_camera_viewshed") || (has(name, "viewshed") && has(name, "alertcalifornia"))
  uic <- grepl("^uic_", method)
  spc <- has(method, "spc") || has(name, "spc fire wx") || has(name, "spc fire weather") || has(name, "spc convective") || has(name, "spc mesoscale")
  airnow <- has(method, "airnow_aqi") || has(name, "airnow") || has(url, "air%20now%20current%20monitor%20data%20public") || has(url, "airnowlatestcontours")
  earthquake <- has(method, "usgs_earthquake") || has(name, "earthquake") || has(url, "earthquake.usgs.gov/earthquakes/feed")

  flags <- c(
    drought_monitor = drought,
    cpc_outlook = cpc,
    stream_gauge_flow = stream,
    wcr_completed_depth = wcr,
    fire_year = fire,
    mlrs_mineral_cases = mlrs,
    sgma_prioritization = sgma,
    nifc_current_fire = nifc,
    aml_status = aml,
    calipc_ramp = calipc,
    swrcb_ir_status = swrcb,
    subsidence_observation = subsidence,
    dwr_tre_insar_points = dwr_tre,
    alert_camera = alert,
    alert_camera_viewshed = alert_viewshed,
    uic_explorer = uic
  )

  generic_excluded <- any(c(drought, cpc, fire, spc, alert, wcr, mlrs, stream,
                            airnow, nifc, aml, earthquake, calipc, swrcb,
                            subsidence))
  generic <- nzchar(field) && !generic_excluded &&
    any(vapply(c("categorical", "category", "distinct"), function(x) has(method, x), logical(1)))
  flags <- append(flags, c(generic_categorical = generic), after = 13L)
  names(flags)[flags]
}

pt_external_provider_legend_reference <- function(record) {
  explicit_url <- pt_record_value(record, "legend_url")
  if (nzchar(explicit_url)) {
    return(list(available = TRUE, url = explicit_url, scope = "catalog_explicit"))
  }
  service_url <- pt_record_value(record, "service_url")
  is_map <- identical(tolower(pt_record_value(record, "service_type")), "map") ||
    grepl("/MapServer", service_url, ignore.case = TRUE)
  if (!is_map) return(list(available = FALSE, url = "", scope = ""))
  endpoint <- sub("[?#].*$", "", service_url)
  endpoint <- sub("/+$", "", endpoint)
  match <- regexec("^(.*?/MapServer)(?:/\\d+)?$", endpoint, ignore.case = TRUE, perl = TRUE)
  parts <- regmatches(endpoint, match)[[1]]
  if (length(parts) < 2L) return(list(available = FALSE, url = "", scope = ""))
  list(available = TRUE, url = paste0(parts[2], "/legend"), scope = "map_service")
}

pt_external_provider_legend_available <- function(record) {
  isTRUE(pt_external_provider_legend_reference(record)$available)
}

pt_external_legend_state <- function(record, adapters) {
  adapter_catalog <- pt_layer_legend_adapter_catalog()
  adapter_rows <- adapter_catalog[match(adapters, adapter_catalog$adapter), , drop = FALSE]
  adapter_rows <- adapter_rows[!is.na(adapter_rows$adapter), , drop = FALSE]
  specialized_family <- pt_external_specialized_style_family(record)
  provider <- pt_external_provider_legend_reference(record)

  if (nrow(adapter_rows)) {
    priority <- match(adapter_rows$legend_status, c(
      "brim_automatic", "brim_shared_automatic", "brim_hidden_or_manual",
      "partial_brim_legend", "renderer_available_unmounted",
      "style_mapping_available", "style_note_only"
    ))
    selected <- adapter_rows[order(priority, adapter_rows$adapter)[1], , drop = FALSE]
    status <- selected$legend_status
    return(list(
      legend_status = status,
      legend_surface = selected$legend_surface,
      legend_renderer_key = selected$legend_renderer_key,
      legend_mount_path = selected$legend_mount_path,
      legend_shared_with = selected$legend_shared_with,
      legend_automatic_mount = isTRUE(selected$legend_automatic_mount),
      legend_keyed_symbology = isTRUE(selected$legend_keyed_symbology),
      structured_style_mapping_available = any(adapter_rows$structured_style_mapping_available),
      legend_candidate_basis = selected$legend_candidate_basis,
      provider_legend_available = isTRUE(provider$available),
      provider_legend_url = provider$url,
      provider_legend_scope = provider$scope
    ))
  }

  if (nzchar(specialized_family)) {
    return(list(
      legend_status = "style_mapping_available",
      legend_surface = "unmounted_style_adapter",
      legend_renderer_key = specialized_family,
      legend_mount_path = "",
      legend_shared_with = "",
      legend_automatic_mount = FALSE,
      legend_keyed_symbology = FALSE,
      structured_style_mapping_available = TRUE,
      legend_candidate_basis = paste0("deterministic ", specialized_family, " style mapping has no mounted BRIM legend"),
      provider_legend_available = isTRUE(provider$available),
      provider_legend_url = provider$url,
      provider_legend_scope = provider$scope
    ))
  }

  list(
    legend_status = if (isTRUE(provider$available)) "provider_reference_only" else "none",
    legend_surface = if (isTRUE(provider$available)) "external_provider_page" else "",
    legend_renderer_key = "",
    legend_mount_path = "",
    legend_shared_with = "",
    legend_automatic_mount = FALSE,
    legend_keyed_symbology = FALSE,
    structured_style_mapping_available = FALSE,
    legend_candidate_basis = if (isTRUE(provider$available)) "provider legend reference only" else "",
    provider_legend_available = isTRUE(provider$available),
    provider_legend_url = provider$url,
    provider_legend_scope = provider$scope
  )
}

pt_external_specialized_style_family <- function(record) {
  method <- tolower(pt_record_value(record, "default_style_method"))
  name <- tolower(pt_record_value(record, "display_name"))
  if (grepl("spc", method, fixed = TRUE) || grepl("spc ", name, fixed = TRUE)) return("spc_forecast")
  if (grepl("airnow_aqi", method, fixed = TRUE) || grepl("airnow", name, fixed = TRUE)) return("airnow_aqi")
  if (grepl("usgs_earthquake", method, fixed = TRUE) || grepl("recent earthquake", name, fixed = TRUE)) return("usgs_earthquake")
  ""
}

pt_external_info_resolution <- function(record) {
  service_type <- tolower(pt_record_value(record, "service_type"))
  load_mode <- tolower(pt_record_value(record, "default_load_mode"))
  method <- tolower(pt_record_value(record, "default_style_method"))
  name <- tolower(pt_record_value(record, "display_name"))
  url <- tolower(pt_record_value(record, "service_url"))
  hover_config <- nzchar(pt_record_value(record, "hover_fields"))
  popup_config <- nzchar(pt_record_value(record, "popup_fields"))
  label_config <- nzchar(pt_record_value(record, "default_label_field"))
  link_config <- nzchar(pt_record_value(record, "popup_link_template"))
  clickable <- pt_capability_truth(pt_record_value(record, "default_clickable")) &&
    pt_capability_truth(pt_record_value(record, "supports_popups"))
  vector_route <- service_type %in% c("feature", "geojson") ||
    (service_type == "map" && load_mode == "current_view")
  visual_map_route <- service_type == "map" && !vector_route
  image_route <- service_type == "image"
  uic <- grepl("^uic_", method)
  mlrs <- grepl("mlrs_case_status", method, fixed = TRUE)
  dwr_tre_raster <- image_route &&
    (grepl("dwr/tre", name, fixed = TRUE) || grepl("tre_altamira", url, fixed = TRUE)) &&
    nzchar(pt_record_value(record, "identify_url")) &&
    grepl("Raster_Value_Display", pt_record_value(record, "popup_fields"), fixed = TRUE)

  hover <- FALSE
  popup <- FALSE
  identify <- FALSE
  equivalent <- FALSE
  adapter <- ""
  basis <- character()
  info_content_basis <- "none"
  info_fields_curated <- FALSE
  info_content_quality <- "none"
  generic_popup_fields_uncurated <- FALSE

  if (uic) {
    hover <- TRUE
    popup <- TRUE
    equivalent <- TRUE
    adapter <- "uic_bespoke"
    basis <- c(basis, "registered UIC tooltip/popup interaction")
    info_content_basis <- "bespoke_feature_interaction"
    info_fields_curated <- TRUE
    info_content_quality <- "bespoke"
  } else if (dwr_tre_raster) {
    hover <- hover_config
    popup <- clickable || popup_config || link_config
    identify <- TRUE
    equivalent <- TRUE
    adapter <- "dwr_tre_raster_identify"
    basis <- c(basis, "BRIM-formatted DWR/TRE ImageServer identify value")
    info_content_basis <- "formatted_raster_identify"
    info_fields_curated <- TRUE
    info_content_quality <- "formatted"
  } else if (image_route) {
    adapter <- "image_identify_unqualified"
  } else if (vector_route) {
    hover <- hover_config || label_config
    curated_config <- popup_config || hover_config || label_config || link_config || mlrs
    popup <- clickable
    equivalent <- mlrs && popup
    adapter <- if (mlrs) "mlrs_aggregate" else "shared_vector_feature"
    if (hover_config) basis <- c(basis, "configured hover fields")
    if (!hover_config && label_config) basis <- c(basis, "usable default label")
    if (popup_config && clickable) basis <- c(basis, "configured popup fields")
    if (link_config && clickable) basis <- c(basis, "feature-specific popup link")
    if (clickable && !curated_config) {
      basis <- c(basis, "shared vector handler binds ptPopupFromProperties to returned feature attributes")
      info_content_basis <- "generic_attribute_popup"
      info_content_quality <- "uncurated"
      generic_popup_fields_uncurated <- TRUE
    } else if (mlrs && popup) {
      info_content_basis <- "bespoke_aggregate_popup"
      info_fields_curated <- TRUE
      info_content_quality <- "bespoke"
    } else if (hover || popup) {
      info_content_basis <- if (popup) "curated_feature_popup" else "curated_hover_or_tooltip"
      info_fields_curated <- popup_config || hover_config || label_config
      info_content_quality <- "curated"
    }
    if (mlrs && popup) basis <- c(basis, "registered MLRS aggregate click interaction")
  } else if (visual_map_route) {
    hover <- hover_config
    meaningful_config <- popup_config || hover_config || link_config
    popup <- meaningful_config
    identify <- hover || popup
    adapter <- "shared_visual_map_identify"
    if (hover_config) basis <- c(basis, "configured MapServer identify hover fields")
    if (popup_config) basis <- c(basis, "configured MapServer identify popup fields")
    if (link_config) basis <- c(basis, "feature-specific MapServer identify link")
    if (hover || popup || identify) {
      info_content_basis <- "curated_visual_identify"
      info_fields_curated <- popup_config || hover_config
      info_content_quality <- "curated"
    }
  }

  list(
    has_feature_info = hover || popup || identify || equivalent,
    feature_info_hover = hover,
    feature_info_popup = popup,
    feature_info_identify = identify,
    feature_info_equivalent = equivalent,
    info_adapter = adapter,
    content_basis = paste(unique(basis), collapse = "; "),
    info_content_basis = info_content_basis,
    info_fields_curated = info_fields_curated,
    info_content_quality = info_content_quality,
    generic_popup_fields_uncurated = generic_popup_fields_uncurated,
    bare_info_flags = clickable && !(hover || popup || identify || equivalent)
  )
}

pt_capability_diagnostic_severities <- function() c("info", "warning", "error")

pt_info_content_basis_values <- function() {
  c(
    "none", "generic_attribute_popup", "curated_feature_popup",
    "curated_hover_or_tooltip", "curated_visual_identify",
    "formatted_raster_identify", "bespoke_feature_interaction",
    "bespoke_aggregate_popup", "explicit_registration"
  )
}

pt_info_content_quality_values <- function() {
  c("none", "uncurated", "curated", "formatted", "bespoke", "registered")
}

pt_add_capability_diagnostic <- function(codes, severities, details, code, severity, detail) {
  if (!severity %in% pt_capability_diagnostic_severities()) {
    stop("Unsupported capability diagnostic severity: ", severity, call. = FALSE)
  }
  list(
    codes = c(codes, code),
    severities = c(severities, severity),
    details = c(details, detail)
  )
}

pt_layer_capability_output_columns <- function() {
  c(
    "schema_version", "build_timestamp", "git_head", "record_type", "panel",
    "layer_key", "group_key", "group", "subgroup_key", "subgroup",
    "stable_layer_id", "display_name", "panel_visible", "has_legend",
    "has_feature_info", "feature_info_hover", "feature_info_popup",
    "feature_info_identify", "feature_info_equivalent", "legend_type",
    "legend_status", "legend_surface", "legend_renderer_key",
    "legend_mount_path", "legend_shared_with", "legend_automatic_mount",
    "legend_keyed_symbology", "structured_style_mapping_available",
    "legend_candidate_basis", "provider_legend_available",
    "provider_legend_url", "provider_legend_scope",
    "provider_legend_reference_only", "legend_adapter", "info_adapter",
    "info_content_basis", "info_fields_curated", "info_content_quality",
    "registration_source", "content_basis",
    "legend_renderable", "info_content_meaningful", "layer_count",
    "legend_count", "info_count", "group_layer_count", "group_legend_count",
    "group_info_count", "subgroup_layer_count", "subgroup_legend_count",
    "subgroup_info_count", "diagnostic_code", "diagnostic_severity", "diagnostic_detail"
  )
}

pt_validate_capability_registrations <- function(registrations, valid_keys) {
  if (is.null(registrations) || !nrow(registrations)) return(invisible(TRUE))
  unknown <- setdiff(unique(registrations$layer_key), valid_keys)
  if (length(unknown)) stop("Capability registrations reference unknown layer keys: ", paste(unknown, collapse = ", "), call. = FALSE)
  duplicate_keys <- unique(registrations$layer_key[duplicated(registrations$layer_key)])
  conflicts <- vapply(duplicate_keys, function(key) {
    rows <- registrations[registrations$layer_key == key, , drop = FALSE]
    nrow(unique(rows[setdiff(names(rows), c("registration_source", "content_basis"))])) > 1L
  }, logical(1))
  if (any(conflicts)) stop("Conflicting duplicate capability registrations: ", paste(duplicate_keys[conflicts], collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

pt_apply_capability_registrations <- function(layer_rows, registrations) {
  if (is.null(registrations) || !nrow(registrations)) return(layer_rows)
  registrations <- unique(registrations)
  adapter_catalog <- pt_layer_legend_adapter_catalog()

  for (i in seq_len(nrow(registrations))) {
    registration <- registrations[i, , drop = FALSE]
    row_idx <- match(registration$layer_key, layer_rows$layer_key)
    adapters <- strsplit(pt_capability_text(registration$legend_adapter), ";", fixed = TRUE)[[1]]
    adapters <- adapters[nzchar(adapters)]
    unknown_adapters <- setdiff(adapters, adapter_catalog$adapter)
    if (length(unknown_adapters)) {
      stop("Capability registration uses unknown legend adapters: ", paste(unknown_adapters, collapse = ", "), call. = FALSE)
    }
    if (isTRUE(registration$has_legend) && !length(adapters)) {
      stop("LGND capability registration requires a registered legend adapter: ", registration$layer_key, call. = FALSE)
    }
    if (isTRUE(registration$has_legend) && any(!adapter_catalog$qualifies_lgnd[match(adapters, adapter_catalog$adapter)])) {
      stop("LGND capability registration uses a non-qualifying style-note adapter: ", registration$layer_key, call. = FALSE)
    }
    if (isTRUE(registration$has_legend) && !pt_strict_has_legend(
      layer_rows$legend_status[row_idx],
      layer_rows$legend_automatic_mount[row_idx],
      layer_rows$legend_keyed_symbology[row_idx]
    )) {
      stop("LGND capability registration requires normalized automatic, keyed mount evidence: ", registration$layer_key, call. = FALSE)
    }
    info_modes <- unlist(registration[c(
      "feature_info_hover", "feature_info_popup", "feature_info_identify",
      "feature_info_equivalent"
    )], use.names = FALSE)
    if (isTRUE(registration$has_feature_info) &&
        (!any(info_modes) || !nzchar(pt_capability_text(registration$info_adapter)))) {
      stop("INFO capability registration requires a supported mode and info adapter: ", registration$layer_key, call. = FALSE)
    }

    if (isTRUE(registration$has_legend)) {
      current <- strsplit(pt_capability_text(layer_rows$legend_adapter[row_idx]), ";", fixed = TRUE)[[1]]
      current <- current[nzchar(current)]
      resolved <- unique(c(current, adapters))
      resolved_types <- unique(adapter_catalog$legend_type[match(resolved, adapter_catalog$adapter)])
      layer_rows$has_legend[row_idx] <- TRUE
      layer_rows$legend_renderable[row_idx] <- TRUE
      layer_rows$legend_adapter[row_idx] <- paste(resolved, collapse = ";")
      layer_rows$legend_type[row_idx] <- if (length(resolved_types) == 1L) resolved_types else "mixed"
      layer_rows$legend_count[row_idx] <- 1L
    }
    if (isTRUE(registration$has_feature_info)) {
      current_info <- strsplit(pt_capability_text(layer_rows$info_adapter[row_idx]), ";", fixed = TRUE)[[1]]
      layer_rows$has_feature_info[row_idx] <- TRUE
      layer_rows$info_content_meaningful[row_idx] <- TRUE
      if (!nzchar(pt_capability_text(layer_rows$info_content_basis[row_idx])) ||
          identical(layer_rows$info_content_basis[row_idx], "none")) {
        layer_rows$info_content_basis[row_idx] <- "explicit_registration"
      }
      if (!nzchar(pt_capability_text(layer_rows$info_content_quality[row_idx])) ||
          identical(layer_rows$info_content_quality[row_idx], "none")) {
        layer_rows$info_content_quality[row_idx] <- "registered"
      }
      layer_rows$feature_info_hover[row_idx] <- layer_rows$feature_info_hover[row_idx] || isTRUE(registration$feature_info_hover)
      layer_rows$feature_info_popup[row_idx] <- layer_rows$feature_info_popup[row_idx] || isTRUE(registration$feature_info_popup)
      layer_rows$feature_info_identify[row_idx] <- layer_rows$feature_info_identify[row_idx] || isTRUE(registration$feature_info_identify)
      layer_rows$feature_info_equivalent[row_idx] <- layer_rows$feature_info_equivalent[row_idx] || isTRUE(registration$feature_info_equivalent)
      layer_rows$info_adapter[row_idx] <- paste(unique(c(
        current_info, pt_capability_text(registration$info_adapter)
      )[nzchar(c(current_info, pt_capability_text(registration$info_adapter)))]), collapse = ";")
      layer_rows$info_count[row_idx] <- 1L
    }
    layer_rows$registration_source[row_idx] <- paste(unique(c(
      strsplit(layer_rows$registration_source[row_idx], ";", fixed = TRUE)[[1]],
      pt_capability_text(registration$registration_source)
    )), collapse = ";")
    layer_rows$content_basis[row_idx] <- paste(unique(c(
      layer_rows$content_basis[row_idx], pt_capability_text(registration$content_basis)
    )[nzchar(c(layer_rows$content_basis[row_idx], pt_capability_text(registration$content_basis)))]), collapse = "; ")
  }
  layer_rows
}

pt_external_layer_capabilities <- function(catalog_df, build_timestamp, git_head) {
  visible <- tolower(pt_capability_text(catalog_df$primary_panel)) %in% c("external", "both")
  ids <- pt_capability_text(catalog_df$external_layer_id[visible])
  if (any(!nzchar(ids))) stop("External-visible catalog records require stable external_layer_id values.", call. = FALSE)
  if (anyDuplicated(ids)) {
    dupes <- unique(ids[duplicated(ids)])
    stop("Duplicate External-visible stable layer IDs: ", paste(dupes, collapse = ", "), call. = FALSE)
  }
  rows <- catalog_df[visible, , drop = FALSE]
  out <- vector("list", nrow(rows))
  adapter_catalog <- pt_layer_legend_adapter_catalog()

  for (i in seq_len(nrow(rows))) {
    record <- rows[i, , drop = FALSE]
    stable_id <- pt_record_value(record, "external_layer_id")
    group <- pt_record_value(record, "external_group")
    if (!nzchar(group)) group <- pt_record_value(record, "theme")
    if (!nzchar(group)) group <- pt_record_value(record, "agency")
    if (!nzchar(group)) group <- "Other"
    subgroup <- pt_record_value(record, "external_subgroup")
    if (!nzchar(subgroup)) subgroup <- pt_record_value(record, "theme")
    if (!nzchar(subgroup)) subgroup <- "Other"
    group_key <- paste0("external:group:", pt_capability_slug(group))
    subgroup_key <- paste0(group_key, ":subgroup:", pt_capability_slug(subgroup))
    adapters <- setdiff(pt_external_legend_adapter_keys(record), c("alert_camera", "alert_camera_viewshed"))
    adapter_match <- match(adapters, adapter_catalog$adapter)
    legend_state <- pt_external_legend_state(record, adapters)
    has_legend <- pt_strict_has_legend(
      legend_state$legend_status,
      legend_state$legend_automatic_mount,
      legend_state$legend_keyed_symbology
    )
    legend_renderable <- length(adapters) > 0L
    legend_type <- ""
    if (legend_renderable) {
      types <- unique(adapter_catalog$legend_type[adapter_match])
      legend_type <- if (has_legend && "dynamic_map_card" %in% types) {
        "dynamic_map_card"
      } else {
        paste(types, collapse = ";")
      }
    }
    provider_link <- isTRUE(legend_state$provider_legend_available)
    provider_reference_only <- provider_link && !has_legend
    if (!has_legend && !legend_renderable && provider_link) legend_type <- "provider_link"
    info <- pt_external_info_resolution(record)
    codes <- severities <- details <- character()
    if (provider_reference_only) {
      diagnostic <- pt_add_capability_diagnostic(codes, severities, details, "provider_legend_reference_only", "info", "Provider legend availability is recorded but does not establish LGND.")
      codes <- diagnostic$codes; severities <- diagnostic$severities; details <- diagnostic$details
    }
    specialized_family <- pt_external_specialized_style_family(record)
    if (nzchar(specialized_family) && !has_legend) {
      diagnostic <- pt_add_capability_diagnostic(codes, severities, details, "specialized_style_without_registered_legend", "warning", paste0("Specialized ", specialized_family, " styling has no registered usable BRIM legend."))
      codes <- diagnostic$codes; severities <- diagnostic$severities; details <- diagnostic$details
    }
    if (isTRUE(info$generic_popup_fields_uncurated)) {
      diagnostic <- pt_add_capability_diagnostic(codes, severities, details, "generic_popup_fields_uncurated", "info", "The shared vector popup presents returned non-system attributes, but its fields are not curated.")
      codes <- diagnostic$codes; severities <- diagnostic$severities; details <- diagnostic$details
    }
    if (tolower(pt_record_value(record, "service_type")) == "image" &&
        !identical(info$info_adapter, "dwr_tre_raster_identify") &&
        (nzchar(pt_record_value(record, "hover_fields")) || nzchar(pt_record_value(record, "popup_fields")))) {
      diagnostic <- pt_add_capability_diagnostic(codes, severities, details, "info_flags_without_content_basis", "warning", "Generic ImageServer pixel identify is not meaningful INFO without a registered formatter.")
      codes <- diagnostic$codes; severities <- diagnostic$severities; details <- diagnostic$details
    }
    registration_source <- c()
    if (legend_renderable) registration_source <- c(registration_source, paste0("legend_adapter:", adapters))
    if (nzchar(info$info_adapter)) registration_source <- c(registration_source, paste0("info_adapter:", info$info_adapter))
    if (!length(registration_source)) registration_source <- "effective_catalog"
    content_basis <- c()
    if (legend_renderable) content_basis <- c(content_basis, paste0("BRIM renderer: ", adapters))
    if (nzchar(legend_state$legend_candidate_basis)) {
      content_basis <- c(content_basis, paste0("Legend state: ", legend_state$legend_candidate_basis))
    }
    if (nzchar(info$content_basis)) content_basis <- c(content_basis, info$content_basis)

    out[[i]] <- data.frame(
      schema_version = PT_LAYER_CAPABILITY_SCHEMA_VERSION,
      build_timestamp = build_timestamp,
      git_head = git_head,
      record_type = "layer",
      panel = "external",
      layer_key = paste0("external:", stable_id),
      group_key = group_key,
      group = group,
      subgroup_key = subgroup_key,
      subgroup = subgroup,
      stable_layer_id = stable_id,
      display_name = pt_record_value(record, "display_name"),
      panel_visible = TRUE,
      has_legend = has_legend,
      has_feature_info = isTRUE(info$has_feature_info),
      feature_info_hover = isTRUE(info$feature_info_hover),
      feature_info_popup = isTRUE(info$feature_info_popup),
      feature_info_identify = isTRUE(info$feature_info_identify),
      feature_info_equivalent = isTRUE(info$feature_info_equivalent),
      legend_type = legend_type,
      legend_status = legend_state$legend_status,
      legend_surface = legend_state$legend_surface,
      legend_renderer_key = legend_state$legend_renderer_key,
      legend_mount_path = legend_state$legend_mount_path,
      legend_shared_with = legend_state$legend_shared_with,
      legend_automatic_mount = isTRUE(legend_state$legend_automatic_mount),
      legend_keyed_symbology = isTRUE(legend_state$legend_keyed_symbology),
      structured_style_mapping_available = isTRUE(legend_state$structured_style_mapping_available),
      legend_candidate_basis = legend_state$legend_candidate_basis,
      provider_legend_available = provider_link,
      provider_legend_url = legend_state$provider_legend_url,
      provider_legend_scope = legend_state$provider_legend_scope,
      provider_legend_reference_only = provider_reference_only,
      legend_adapter = paste(adapters, collapse = ";"),
      info_adapter = info$info_adapter,
      info_content_basis = info$info_content_basis,
      info_fields_curated = isTRUE(info$info_fields_curated),
      info_content_quality = info$info_content_quality,
      registration_source = paste(registration_source, collapse = ";"),
      content_basis = paste(unique(content_basis), collapse = "; "),
      legend_renderable = legend_renderable,
      info_content_meaningful = isTRUE(info$has_feature_info),
      layer_count = 1L,
      legend_count = as.integer(has_legend),
      info_count = as.integer(info$has_feature_info),
      group_layer_count = NA_integer_,
      group_legend_count = NA_integer_,
      group_info_count = NA_integer_,
      subgroup_layer_count = NA_integer_,
      subgroup_legend_count = NA_integer_,
      subgroup_info_count = NA_integer_,
      diagnostic_code = paste(codes, collapse = ";"),
      diagnostic_severity = paste(severities, collapse = ";"),
      diagnostic_detail = paste(details, collapse = " | "),
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, out)
  group_identity_count <- vapply(split(out$group, out$group_key), function(x) length(unique(x)), integer(1))
  subgroup_identity <- paste(out$group, out$subgroup, sep = "\r")
  subgroup_identity_count <- vapply(split(subgroup_identity, out$subgroup_key), function(x) length(unique(x)), integer(1))
  if (any(group_identity_count != 1L) || any(subgroup_identity_count != 1L)) {
    stop("Capability hierarchy keys are not deterministic and collision-free.", call. = FALSE)
  }
  out
}

pt_capability_aggregate_rows <- function(layer_rows) {
  group_keys <- unique(layer_rows$group_key)
  subgroup_keys <- unique(layer_rows$subgroup_key)
  group_rows <- lapply(group_keys, function(key) {
    x <- layer_rows[layer_rows$group_key == key, , drop = FALSE]
    row <- x[1, , drop = FALSE]
    row$record_type <- "group"
    row$layer_key <- key
    row$subgroup_key <- ""
    row$subgroup <- ""
    row$stable_layer_id <- ""
    row$display_name <- row$group
    row$has_legend <- any(x$has_legend)
    row$has_feature_info <- any(x$has_feature_info)
    row$feature_info_hover <- any(x$feature_info_hover)
    row$feature_info_popup <- any(x$feature_info_popup)
    row$feature_info_identify <- any(x$feature_info_identify)
    row$feature_info_equivalent <- any(x$feature_info_equivalent)
    row$provider_legend_available <- any(x$provider_legend_available)
    row$provider_legend_reference_only <- any(x$provider_legend_reference_only)
    row$provider_legend_url <- row$provider_legend_scope <- ""
    row$legend_status <- row$legend_surface <- row$legend_renderer_key <- ""
    row$legend_mount_path <- row$legend_shared_with <- row$legend_candidate_basis <- ""
    row$legend_automatic_mount <- row$legend_keyed_symbology <- FALSE
    row$structured_style_mapping_available <- any(x$structured_style_mapping_available)
    row$legend_type <- row$legend_adapter <- row$info_adapter <- "aggregate"
    row$info_content_basis <- row$info_content_quality <- "aggregate"
    row$info_fields_curated <- any(x$info_fields_curated)
    row$registration_source <- "derived_full_catalog_aggregate"
    row$content_basis <- "All panel-visible descendants"
    row$legend_renderable <- any(x$legend_renderable)
    row$info_content_meaningful <- any(x$info_content_meaningful)
    row$layer_count <- nrow(x)
    row$legend_count <- sum(x$has_legend)
    row$info_count <- sum(x$has_feature_info)
    row$group_layer_count <- nrow(x)
    row$group_legend_count <- sum(x$has_legend)
    row$group_info_count <- sum(x$has_feature_info)
    row$subgroup_layer_count <- row$subgroup_legend_count <- row$subgroup_info_count <- NA_integer_
    row$diagnostic_code <- row$diagnostic_severity <- row$diagnostic_detail <- ""
    row
  })
  subgroup_rows <- lapply(subgroup_keys, function(key) {
    x <- layer_rows[layer_rows$subgroup_key == key, , drop = FALSE]
    row <- x[1, , drop = FALSE]
    row$record_type <- "subgroup"
    row$layer_key <- key
    row$stable_layer_id <- ""
    row$display_name <- row$subgroup
    row$has_legend <- any(x$has_legend)
    row$has_feature_info <- any(x$has_feature_info)
    row$feature_info_hover <- any(x$feature_info_hover)
    row$feature_info_popup <- any(x$feature_info_popup)
    row$feature_info_identify <- any(x$feature_info_identify)
    row$feature_info_equivalent <- any(x$feature_info_equivalent)
    row$provider_legend_available <- any(x$provider_legend_available)
    row$provider_legend_reference_only <- any(x$provider_legend_reference_only)
    row$provider_legend_url <- row$provider_legend_scope <- ""
    row$legend_status <- row$legend_surface <- row$legend_renderer_key <- ""
    row$legend_mount_path <- row$legend_shared_with <- row$legend_candidate_basis <- ""
    row$legend_automatic_mount <- row$legend_keyed_symbology <- FALSE
    row$structured_style_mapping_available <- any(x$structured_style_mapping_available)
    row$legend_type <- row$legend_adapter <- row$info_adapter <- "aggregate"
    row$info_content_basis <- row$info_content_quality <- "aggregate"
    row$info_fields_curated <- any(x$info_fields_curated)
    row$registration_source <- "derived_full_catalog_aggregate"
    row$content_basis <- "All panel-visible descendants"
    row$legend_renderable <- any(x$legend_renderable)
    row$info_content_meaningful <- any(x$info_content_meaningful)
    row$layer_count <- nrow(x)
    row$legend_count <- sum(x$has_legend)
    row$info_count <- sum(x$has_feature_info)
    group_x <- layer_rows[layer_rows$group_key == row$group_key, , drop = FALSE]
    row$group_layer_count <- nrow(group_x)
    row$group_legend_count <- sum(group_x$has_legend)
    row$group_info_count <- sum(group_x$has_feature_info)
    row$subgroup_layer_count <- nrow(x)
    row$subgroup_legend_count <- sum(x$has_legend)
    row$subgroup_info_count <- sum(x$has_feature_info)
    row$diagnostic_code <- row$diagnostic_severity <- row$diagnostic_detail <- ""
    row
  })
  list(
    group = do.call(rbind, group_rows),
    subgroup = do.call(rbind, subgroup_rows)
  )
}

pt_finalize_layer_capability_rows <- function(
    layer_rows,
    registrations = pt_new_layer_capability_registry(),
    write_qa = FALSE,
    qa_path = file.path("04_processed_data", "qa", "layer_capability_coverage_latest.csv")) {
  required <- pt_layer_capability_output_columns()
  missing <- setdiff(required, names(layer_rows))
  if (length(missing)) stop("Normalized capability rows are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!nrow(layer_rows) || any(layer_rows$record_type != "layer")) stop("Generic capability finalization requires normalized layer records.", call. = FALSE)
  expected_keys <- paste0(pt_capability_text(layer_rows$panel), ":", pt_capability_text(layer_rows$stable_layer_id))
  if (any(!nzchar(layer_rows$stable_layer_id)) || any(layer_rows$layer_key != expected_keys) || anyDuplicated(layer_rows$layer_key)) {
    stop("Normalized capability layer identities are missing, inconsistent, or duplicated.", call. = FALSE)
  }
  adapter_catalog <- pt_layer_legend_adapter_catalog()
  row_adapters <- lapply(strsplit(pt_capability_text(layer_rows$legend_adapter), ";", fixed = TRUE), function(x) x[nzchar(x)])
  unknown_adapters <- setdiff(unique(unlist(row_adapters)), adapter_catalog$adapter)
  if (length(unknown_adapters)) stop("Normalized capability rows use unknown legend adapters: ", paste(unknown_adapters, collapse = ", "), call. = FALSE)
  invalid_status <- setdiff(unique(pt_capability_text(layer_rows$legend_status)), pt_layer_legend_status_values())
  if (length(invalid_status)) stop("Normalized capability rows use unsupported legend statuses: ", paste(invalid_status, collapse = ", "), call. = FALSE)
  invalid_info_basis <- setdiff(unique(pt_capability_text(layer_rows$info_content_basis)), pt_info_content_basis_values())
  if (length(invalid_info_basis)) stop("Normalized capability rows use unsupported INFO content bases: ", paste(invalid_info_basis, collapse = ", "), call. = FALSE)
  invalid_info_quality <- setdiff(unique(pt_capability_text(layer_rows$info_content_quality)), pt_info_content_quality_values())
  if (length(invalid_info_quality)) stop("Normalized capability rows use unsupported INFO content quality values: ", paste(invalid_info_quality, collapse = ", "), call. = FALSE)
  incomplete_info <- layer_rows$has_feature_info & (
    layer_rows$info_content_basis == "none" | layer_rows$info_content_quality == "none"
  )
  if (any(incomplete_info)) stop("INFO rows require a factual content basis and quality classification.", call. = FALSE)
  invalid_generic_info <- layer_rows$info_content_basis == "generic_attribute_popup" & (
    layer_rows$info_fields_curated | layer_rows$info_content_quality != "uncurated"
  )
  if (any(invalid_generic_info)) stop("Generic attribute popup INFO must remain explicitly uncurated.", call. = FALSE)
  pt_validate_legend_adapter_implementations()
  pt_validate_capability_registrations(registrations, layer_rows$layer_key)
  layer_rows <- pt_apply_capability_registrations(layer_rows, registrations)
  layer_rows$has_legend <- pt_strict_has_legend(
    layer_rows$legend_status,
    layer_rows$legend_automatic_mount,
    layer_rows$legend_keyed_symbology
  )
  layer_rows$legend_count <- as.integer(layer_rows$has_legend)
  incomplete_automatic <- layer_rows$has_legend & (
    !nzchar(pt_capability_text(layer_rows$legend_renderer_key)) |
      !nzchar(pt_capability_text(layer_rows$legend_surface)) |
      !nzchar(pt_capability_text(layer_rows$legend_mount_path))
  )
  if (any(incomplete_automatic)) {
    stop("Strict LGND rows require a renderer key, automatic surface, and mount path.", call. = FALSE)
  }
  aggregates <- pt_capability_aggregate_rows(layer_rows)

  expected_group <- aggregate(cbind(layer_count, legend_count, info_count) ~ group_key, layer_rows, sum)
  actual_group <- aggregates$group[, c("group_key", "layer_count", "legend_count", "info_count")]
  expected_group <- expected_group[order(expected_group$group_key), ]
  actual_group <- actual_group[order(actual_group$group_key), ]
  rownames(expected_group) <- rownames(actual_group) <- NULL
  if (!identical(expected_group, actual_group)) stop("Layer capability group aggregate count mismatch.", call. = FALSE)
  expected_subgroup <- aggregate(cbind(layer_count, legend_count, info_count) ~ subgroup_key, layer_rows, sum)
  actual_subgroup <- aggregates$subgroup[, c("subgroup_key", "layer_count", "legend_count", "info_count")]
  expected_subgroup <- expected_subgroup[order(expected_subgroup$subgroup_key), ]
  actual_subgroup <- actual_subgroup[order(actual_subgroup$subgroup_key), ]
  rownames(expected_subgroup) <- rownames(actual_subgroup) <- NULL
  if (!identical(expected_subgroup, actual_subgroup)) stop("Layer capability subgroup aggregate count mismatch.", call. = FALSE)

  group_idx <- match(layer_rows$group_key, aggregates$group$group_key)
  subgroup_idx <- match(layer_rows$subgroup_key, aggregates$subgroup$subgroup_key)
  layer_rows$group_layer_count <- aggregates$group$layer_count[group_idx]
  layer_rows$group_legend_count <- aggregates$group$legend_count[group_idx]
  layer_rows$group_info_count <- aggregates$group$info_count[group_idx]
  layer_rows$subgroup_layer_count <- aggregates$subgroup$layer_count[subgroup_idx]
  layer_rows$subgroup_legend_count <- aggregates$subgroup$legend_count[subgroup_idx]
  layer_rows$subgroup_info_count <- aggregates$subgroup$info_count[subgroup_idx]
  coverage <- rbind(layer_rows, aggregates$subgroup, aggregates$group)
  coverage <- coverage[, required, drop = FALSE]

  if (isTRUE(write_qa)) {
    dir.create(dirname(qa_path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(coverage, qa_path, row.names = FALSE, na = "")
  }
  list(layer_rows = layer_rows, coverage = coverage, aggregates = aggregates, qa_path = if (isTRUE(write_qa)) qa_path else "")
}

pt_legend_summary_markdown_escape <- function(x) {
  x <- pt_capability_text(x)
  x <- gsub("|", "&#124;", x, fixed = TRUE)
  x <- gsub("\r", " ", x, fixed = TRUE)
  gsub("\n", " ", x, fixed = TRUE)
}

pt_legend_summary_markdown_table <- function(headers, rows) {
  headers <- pt_legend_summary_markdown_escape(headers)
  header <- paste0("| ", paste(headers, collapse = " | "), " |")
  divider <- paste0("| ", paste(rep("---", length(headers)), collapse = " | "), " |")
  if (!nrow(rows)) {
    empty <- c("_None_", rep("", length(headers) - 1L))
    return(c(header, divider, paste0("| ", paste(empty, collapse = " | "), " |")))
  }
  body <- vapply(seq_len(nrow(rows)), function(i) {
    values <- pt_legend_summary_markdown_escape(unlist(rows[i, , drop = FALSE], use.names = FALSE))
    paste0("| ", paste(values, collapse = " | "), " |")
  }, character(1))
  c(header, divider, body)
}

pt_layer_legend_summary_markdown <- function(layer_rows, catalog_df = NULL) {
  if (!nrow(layer_rows) || any(layer_rows$record_type != "layer")) {
    stop("Legend summary requires normalized layer records.", call. = FALSE)
  }
  required <- c(
    "legend_status", "legend_surface", "legend_renderer_key",
    "legend_mount_path", "legend_shared_with", "legend_automatic_mount",
    "legend_keyed_symbology", "structured_style_mapping_available",
    "legend_candidate_basis", "provider_legend_available",
    "provider_legend_url", "provider_legend_scope",
    "provider_legend_reference_only"
  )
  missing <- setdiff(required, names(layer_rows))
  if (length(missing)) stop("Legend summary rows are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)

  rows <- layer_rows
  style_adapter <- rep("", nrow(rows))
  if (!is.null(catalog_df) && nrow(catalog_df)) {
    catalog_keys <- paste0("external:", pt_capability_text(catalog_df$external_layer_id))
    idx <- match(rows$layer_key, catalog_keys)
    matched <- which(!is.na(idx))
    if (length(matched)) {
      style_adapter[matched] <- pt_capability_text(catalog_df$default_style_method[idx[matched]])
    }
  }
  rows$renderer_or_style_adapter <- ifelse(
    nzchar(pt_capability_text(rows$legend_adapter)),
    pt_capability_text(rows$legend_adapter),
    style_adapter
  )

  status_values <- pt_layer_legend_status_values()
  status_counts <- table(factor(rows$legend_status, levels = status_values))
  candidate <- !rows$has_legend & (
    rows$structured_style_mapping_available |
      rows$legend_status %in% c(
        "brim_hidden_or_manual", "partial_brim_legend",
        "renderer_available_unmounted", "style_mapping_available",
        "style_note_only"
      )
  )
  metrics <- data.frame(
    Metric = c(
      "External-visible layers", "Automatic BRIM legends",
      "Shared automatic BRIM legends", "Provider-reference-only layers",
      "Hidden/manual legend renderers", "Partial BRIM legends",
      "Structured-style-mapping candidates", "Prose style notes",
      "No known legend"
    ),
    Count = c(
      nrow(rows), sum(rows$has_legend),
      sum(rows$legend_status == "brim_shared_automatic"),
      sum(rows$provider_legend_reference_only),
      sum(rows$legend_status == "brim_hidden_or_manual"),
      sum(rows$legend_status == "partial_brim_legend"),
      sum(candidate & rows$structured_style_mapping_available),
      sum(rows$legend_status == "style_note_only"),
      sum(rows$legend_status == "none")
    ),
    stringsAsFactors = FALSE
  )
  status_table <- data.frame(
    Status = status_values,
    Count = as.integer(status_counts),
    stringsAsFactors = FALSE
  )

  hierarchy_counts <- function(keys, labels) {
    ordered_keys <- unique(keys)
    out <- lapply(ordered_keys, function(key) {
      idx <- keys == key
      data.frame(
        Label = labels[which(idx)[1]],
        Layers = sum(idx),
        Automatic = sum(rows$has_legend[idx]),
        ProviderOnly = sum(rows$provider_legend_reference_only[idx]),
        Candidates = sum(candidate[idx]),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, out)
  }
  group_counts <- hierarchy_counts(rows$group_key, rows$group)
  names(group_counts)[1] <- "Group"
  subgroup_counts <- hierarchy_counts(
    rows$subgroup_key,
    paste(rows$group, rows$subgroup, sep = " / ")
  )
  names(subgroup_counts)[1] <- "Group / Subgroup"

  automatic <- rows[rows$has_legend, , drop = FALSE]
  automatic_table <- data.frame(
    `Layer key` = automatic$layer_key,
    `Display name` = automatic$display_name,
    `Group / subgroup` = paste(automatic$group, automatic$subgroup, sep = " / "),
    Status = automatic$legend_status,
    Surface = automatic$legend_surface,
    Renderer = automatic$legend_renderer_key,
    `Mount path` = automatic$legend_mount_path,
    `Shared with` = automatic$legend_shared_with,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  provider <- rows[rows$provider_legend_reference_only, , drop = FALSE]
  provider_table <- data.frame(
    `Layer key` = provider$layer_key,
    `Display name` = provider$display_name,
    `Group / subgroup` = paste(provider$group, provider$subgroup, sep = " / "),
    Scope = provider$provider_legend_scope,
    `Provider legend URL` = ifelse(
      nzchar(provider$provider_legend_url),
      paste0("<", provider$provider_legend_url, ">"),
      ""
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  future <- rows[candidate, , drop = FALSE]
  candidate_table <- data.frame(
    `Layer key` = future$layer_key,
    `Display name` = future$display_name,
    `Group / subgroup` = paste(future$group, future$subgroup, sep = " / "),
    `Current status` = future$legend_status,
    `Renderer / style adapter` = future$renderer_or_style_adapter,
    `Candidate basis` = future$legend_candidate_basis,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  c(
    "# BRIM External Layer Legend Summary", "",
    "## Build metadata", "",
    paste0("- Build timestamp: `", pt_capability_text(rows$build_timestamp[1]), "`"),
    paste0("- Git HEAD: `", pt_capability_git_head_display(rows$git_head[1]), "`"), "",
    "## Overall counts", "", pt_legend_summary_markdown_table(names(metrics), metrics), "",
    "## Legend status counts", "", pt_legend_summary_markdown_table(names(status_table), status_table), "",
    "## Counts by group", "", pt_legend_summary_markdown_table(names(group_counts), group_counts), "",
    "## Counts by subgroup", "", pt_legend_summary_markdown_table(names(subgroup_counts), subgroup_counts), "",
    "## Automatic BRIM legends", "", pt_legend_summary_markdown_table(names(automatic_table), automatic_table), "",
    "## Provider-reference-only layers", "", pt_legend_summary_markdown_table(names(provider_table), provider_table), "",
    "## Future legend candidates", "", pt_legend_summary_markdown_table(names(candidate_table), candidate_table), ""
  )
}

pt_finalize_external_layer_capabilities <- function(
    catalog_df,
    registrations = pt_new_layer_capability_registry(),
    write_qa = TRUE,
    qa_path = file.path("04_processed_data", "qa", "layer_capability_coverage_latest.csv"),
    legend_summary_path = file.path("04_processed_data", "qa", "layer_legend_summary_latest.md"),
    build_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    git_head = pt_capability_git_head()) {
  git_head <- pt_capability_git_head_display(git_head)
  resolved_rows <- pt_external_layer_capabilities(catalog_df, build_timestamp, git_head)
  finalized <- pt_finalize_layer_capability_rows(resolved_rows, registrations, write_qa, qa_path)
  layer_rows <- finalized$layer_rows

  match_idx <- match(paste0("external:", pt_capability_text(catalog_df$external_layer_id)), layer_rows$layer_key)
  capability_cols <- c(
    "layer_key", "panel_visible", "has_legend", "has_feature_info",
    "feature_info_hover", "feature_info_popup", "feature_info_identify",
    "feature_info_equivalent", "legend_type", "legend_status",
    "legend_surface", "legend_renderer_key", "legend_mount_path",
    "legend_shared_with", "legend_automatic_mount", "legend_keyed_symbology",
    "structured_style_mapping_available", "legend_candidate_basis",
    "provider_legend_available", "provider_legend_url",
    "provider_legend_scope", "provider_legend_reference_only",
    "legend_adapter", "info_adapter", "info_content_basis",
    "info_fields_curated", "info_content_quality",
    "registration_source", "content_basis",
    "legend_renderable", "info_content_meaningful", "group_layer_count",
    "group_legend_count", "group_info_count", "subgroup_layer_count",
    "subgroup_legend_count", "subgroup_info_count", "diagnostic_code",
    "diagnostic_severity", "diagnostic_detail"
  )
  logical_cols <- c(
    "panel_visible", "has_legend", "has_feature_info", "feature_info_hover",
    "feature_info_popup", "feature_info_identify", "feature_info_equivalent",
    "legend_automatic_mount", "legend_keyed_symbology",
    "structured_style_mapping_available", "provider_legend_available",
    "provider_legend_reference_only", "info_fields_curated", "legend_renderable",
    "info_content_meaningful"
  )
  for (nm in capability_cols) catalog_df[[nm]] <- if (nm %in% logical_cols) FALSE else if (grepl("_count$", nm)) NA_integer_ else ""
  visible_idx <- which(!is.na(match_idx))
  for (nm in capability_cols) catalog_df[[nm]][visible_idx] <- layer_rows[[nm]][match_idx[visible_idx]]

  joined <- sum(catalog_df$panel_visible)
  if (joined != nrow(layer_rows)) stop("Capability records could not join one-to-one to the finalized External catalog.", call. = FALSE)
  summary_markdown <- pt_layer_legend_summary_markdown(layer_rows, catalog_df)
  written_summary_path <- ""
  if (isTRUE(write_qa)) {
    dir.create(dirname(legend_summary_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(summary_markdown, legend_summary_path, useBytes = TRUE)
    written_summary_path <- legend_summary_path
    message(
      "External legend capabilities: ", sum(layer_rows$has_legend),
      " automatic BRIM; ", sum(layer_rows$provider_legend_reference_only),
      " provider-reference-only; ",
      sum(!layer_rows$has_legend & layer_rows$structured_style_mapping_available),
      " structured-style candidates."
    )
    message("Layer legend summary: ", legend_summary_path)
  }
  list(
    catalog = catalog_df,
    layer_rows = layer_rows,
    coverage = finalized$coverage,
    definitions = pt_layer_capability_definitions(),
    legend_adapters = pt_layer_legend_adapter_catalog(),
    qa_path = finalized$qa_path,
    legend_summary_path = written_summary_path,
    legend_summary_markdown = summary_markdown
  )
}

pt_layer_inventory_output_columns <- function() {
  c(
    "schema_version", "build_timestamp", "git_head", "panel", "layer_key",
    "stable_layer_id", "display_name", "group_key", "group", "subgroup_key",
    "subgroup", "display_order", "primary_panel", "panel_visible",
    "enabled_status", "source_organization", "source_url", "service_url",
    "service_host", "normalized_service_endpoint", "normalized_service_root",
    "service_type", "service_layer_number", "service_family_key",
    "service_family_layer_count", "exact_endpoint_match_count", "load_method",
    "load_scope", "load_hint_classification", "default_clickable",
    "supports_popups", "uses_hover", "uses_identify", "custom_loader",
    "style_adapter", "legend_adapter", "info_adapter", "info_content_basis",
    "info_fields_curated", "info_content_quality",
    "legend_status", "legend_surface", "legend_renderer_key",
    "legend_mount_path", "legend_shared_with", "legend_automatic_mount",
    "legend_keyed_symbology", "structured_style_mapping_available",
    "legend_candidate_basis", "provider_legend_available",
    "provider_legend_url", "provider_legend_scope",
    "provider_legend_reference_only", "has_legend", "has_feature_info",
    "has_both", "has_neither", "feature_info_hover", "feature_info_popup",
    "feature_info_identify", "feature_info_equivalent", "legend_type",
    "description_present", "source_note_present", "update_note_present",
    "official_source_link_present", "diagnostic_code", "diagnostic_severity",
    "diagnostic_detail", "inventory_flag"
  )
}

pt_normalize_service_url <- function(service_url) {
  original <- pt_capability_text(service_url)[1]
  if (!nzchar(original)) {
    return(list(
      host = "", endpoint = "", root = "", service_layer_number = "",
      family_key = ""
    ))
  }

  endpoint <- sub("[?#].*$", "", original)
  endpoint <- sub("/+$", "", endpoint)
  url_match <- regexec("^([A-Za-z][A-Za-z0-9+.-]*://)([^/]+)(.*)$", endpoint, perl = TRUE)
  url_parts <- regmatches(endpoint, url_match)[[1]]
  host <- ""
  if (length(url_parts) == 4L) {
    host <- tolower(url_parts[3])
    endpoint <- paste0(tolower(url_parts[2]), host, url_parts[4])
  }

  arcgis_match <- regexec(
    "^(.*)/(FeatureServer|MapServer|ImageServer)(?:/([0-9]+))?$",
    endpoint,
    ignore.case = TRUE,
    perl = TRUE
  )
  arcgis_parts <- regmatches(endpoint, arcgis_match)[[1]]
  if (length(arcgis_parts)) {
    service_kind <- switch(
      tolower(arcgis_parts[3]),
      featureserver = "FeatureServer",
      mapserver = "MapServer",
      imageserver = "ImageServer"
    )
    root <- paste0(arcgis_parts[2], "/", service_kind)
    layer_number <- if (length(arcgis_parts) >= 4L) arcgis_parts[4] else ""
    return(list(
      host = host,
      endpoint = tolower(endpoint),
      root = root,
      service_layer_number = layer_number,
      family_key = paste0("arcgis:", tolower(root))
    ))
  }

  list(
    host = host,
    endpoint = endpoint,
    root = endpoint,
    service_layer_number = "",
    family_key = if (nzchar(endpoint)) paste0("endpoint:", endpoint) else ""
  )
}

pt_inventory_load_scope <- function(load_method) {
  method <- tolower(pt_capability_text(load_method)[1])
  scopes <- c(
    current_view = "current_view",
    live_snapshot = "full_service_snapshot",
    live = "full_service_live",
    visual = "full_service_visual",
    tiled = "full_service_tiles"
  )
  if (method %in% names(scopes)) unname(scopes[[method]]) else ""
}

pt_inventory_flag_codes <- function() {
  c(
    "no_lgnd_no_info", "provider_legend_reference_only",
    "generic_popup_fields_uncurated",
    "specialized_style_without_registered_legend", "shared_service_family",
    "exact_duplicate_endpoint", "custom_loader", "metadata_incomplete"
  )
}

pt_inventory_flags_for_row <- function(record) {
  flags <- character()
  truth <- function(name) name %in% names(record) && pt_capability_truth(record[[name]][1])
  count <- function(name) {
    if (!name %in% names(record)) return(0L)
    value <- suppressWarnings(as.integer(record[[name]][1]))
    if (is.na(value)) 0L else value
  }
  if (!truth("has_legend") && !truth("has_feature_info")) {
    flags <- c(flags, "no_lgnd_no_info")
  }
  diagnostic_codes <- strsplit(
    pt_record_value(record, "diagnostic_code"), ";", fixed = TRUE
  )[[1]]
  flags <- c(flags, intersect(
    c(
      "provider_legend_reference_only", "generic_popup_fields_uncurated",
      "specialized_style_without_registered_legend"
    ),
    diagnostic_codes
  ))
  if (count("service_family_layer_count") > 1L) {
    flags <- c(flags, "shared_service_family")
  }
  if (count("exact_endpoint_match_count") > 1L) {
    flags <- c(flags, "exact_duplicate_endpoint")
  }
  if (truth("custom_loader")) flags <- c(flags, "custom_loader")
  completeness <- c(
    "description_present", "source_note_present", "update_note_present",
    "official_source_link_present"
  )
  if (any(!vapply(completeness, truth, logical(1)))) {
    flags <- c(flags, "metadata_incomplete")
  }
  paste(pt_inventory_flag_codes()[pt_inventory_flag_codes() %in% unique(flags)], collapse = ";")
}

pt_finalize_layer_inventory_rows <- function(
    inventory_rows,
    write_qa = FALSE,
    qa_path = file.path("04_processed_data", "qa", "layer_inventory_latest.csv")) {
  required <- pt_layer_inventory_output_columns()
  missing <- setdiff(required, names(inventory_rows))
  if (length(missing)) {
    stop("Normalized inventory rows are missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!nrow(inventory_rows)) {
    stop("Generic inventory finalization requires normalized layer records.", call. = FALSE)
  }
  expected_keys <- paste0(
    pt_capability_text(inventory_rows$panel), ":",
    pt_capability_text(inventory_rows$stable_layer_id)
  )
  if (any(!nzchar(inventory_rows$stable_layer_id)) ||
      any(inventory_rows$layer_key != expected_keys) ||
      anyDuplicated(inventory_rows$layer_key)) {
    stop("Normalized inventory layer identities are missing, inconsistent, or duplicated.", call. = FALSE)
  }

  endpoint_counts <- table(inventory_rows$normalized_service_endpoint[nzchar(
    inventory_rows$normalized_service_endpoint
  )])
  family_counts <- table(inventory_rows$service_family_key[nzchar(
    inventory_rows$service_family_key
  )])
  inventory_rows$exact_endpoint_match_count <- as.integer(endpoint_counts[
    inventory_rows$normalized_service_endpoint
  ])
  inventory_rows$service_family_layer_count <- as.integer(family_counts[
    inventory_rows$service_family_key
  ])
  inventory_rows$exact_endpoint_match_count[is.na(inventory_rows$exact_endpoint_match_count)] <- 0L
  inventory_rows$service_family_layer_count[is.na(inventory_rows$service_family_layer_count)] <- 0L

  inventory_rows$has_both <- inventory_rows$has_legend & inventory_rows$has_feature_info
  inventory_rows$has_neither <- !inventory_rows$has_legend & !inventory_rows$has_feature_info
  inventory_rows$inventory_flag <- vapply(
    seq_len(nrow(inventory_rows)),
    function(i) pt_inventory_flags_for_row(inventory_rows[i, , drop = FALSE]),
    character(1)
  )
  inventory_rows <- inventory_rows[, required, drop = FALSE]

  if (isTRUE(write_qa)) {
    dir.create(dirname(qa_path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(inventory_rows, qa_path, row.names = FALSE, na = "")
  }
  list(rows = inventory_rows, qa_path = if (isTRUE(write_qa)) qa_path else "")
}

pt_external_layer_inventory_rows <- function(catalog_df, capability_layer_rows) {
  if (!nrow(capability_layer_rows)) {
    stop("External inventory requires finalized capability layer records.", call. = FALSE)
  }
  catalog_keys <- paste0(
    "external:", pt_capability_text(catalog_df$external_layer_id)
  )
  catalog_idx <- match(capability_layer_rows$layer_key, catalog_keys)
  if (anyNA(catalog_idx) || anyDuplicated(catalog_idx)) {
    stop("External inventory could not join one-to-one to the finalized capability layers.", call. = FALSE)
  }
  catalog_rows <- catalog_df[catalog_idx, , drop = FALSE]

  rows <- lapply(seq_len(nrow(capability_layer_rows)), function(i) {
    capability <- capability_layer_rows[i, , drop = FALSE]
    record <- catalog_rows[i, , drop = FALSE]
    normalized <- pt_normalize_service_url(pt_record_value(record, "service_url"))
    load_method <- tolower(pt_record_value(record, "default_load_mode"))
    load_hint <- pt_record_value(record, "load_badge_override")
    if (!nzchar(load_hint)) load_hint <- pt_record_value(record, "load_badge")
    custom_loader <- identical(load_method, "live_snapshot")

    data.frame(
      schema_version = PT_LAYER_INVENTORY_SCHEMA_VERSION,
      build_timestamp = capability$build_timestamp,
      git_head = capability$git_head,
      panel = capability$panel,
      layer_key = capability$layer_key,
      stable_layer_id = capability$stable_layer_id,
      display_name = capability$display_name,
      group_key = capability$group_key,
      group = capability$group,
      subgroup_key = capability$subgroup_key,
      subgroup = capability$subgroup,
      display_order = suppressWarnings(as.integer(pt_record_value(record, "external_display_num"))),
      primary_panel = pt_record_value(record, "primary_panel"),
      panel_visible = capability$panel_visible,
      enabled_status = if (isTRUE(capability$panel_visible)) "enabled" else "disabled",
      source_organization = pt_record_value(record, "agency"),
      source_url = pt_record_value(record, "source_page"),
      service_url = pt_record_value(record, "service_url"),
      service_host = normalized$host,
      normalized_service_endpoint = normalized$endpoint,
      normalized_service_root = normalized$root,
      service_type = tolower(pt_record_value(record, "service_type")),
      service_layer_number = normalized$service_layer_number,
      service_family_key = normalized$family_key,
      service_family_layer_count = 0L,
      exact_endpoint_match_count = 0L,
      load_method = load_method,
      load_scope = pt_inventory_load_scope(load_method),
      load_hint_classification = tolower(load_hint),
      default_clickable = pt_capability_truth(pt_record_value(record, "default_clickable")),
      supports_popups = pt_capability_truth(pt_record_value(record, "supports_popups")),
      uses_hover = capability$feature_info_hover,
      uses_identify = capability$feature_info_identify,
      custom_loader = custom_loader,
      style_adapter = pt_record_value(record, "default_style_method"),
      legend_adapter = capability$legend_adapter,
      info_adapter = capability$info_adapter,
      info_content_basis = capability$info_content_basis,
      info_fields_curated = capability$info_fields_curated,
      info_content_quality = capability$info_content_quality,
      legend_status = capability$legend_status,
      legend_surface = capability$legend_surface,
      legend_renderer_key = capability$legend_renderer_key,
      legend_mount_path = capability$legend_mount_path,
      legend_shared_with = capability$legend_shared_with,
      legend_automatic_mount = capability$legend_automatic_mount,
      legend_keyed_symbology = capability$legend_keyed_symbology,
      structured_style_mapping_available = capability$structured_style_mapping_available,
      legend_candidate_basis = capability$legend_candidate_basis,
      provider_legend_available = capability$provider_legend_available,
      provider_legend_url = capability$provider_legend_url,
      provider_legend_scope = capability$provider_legend_scope,
      provider_legend_reference_only = capability$provider_legend_reference_only,
      has_legend = capability$has_legend,
      has_feature_info = capability$has_feature_info,
      has_both = FALSE,
      has_neither = FALSE,
      feature_info_hover = capability$feature_info_hover,
      feature_info_popup = capability$feature_info_popup,
      feature_info_identify = capability$feature_info_identify,
      feature_info_equivalent = capability$feature_info_equivalent,
      legend_type = capability$legend_type,
      description_present = nzchar(pt_record_value(record, "notes")),
      source_note_present = nzchar(pt_record_value(record, "pt2_usage_note")),
      update_note_present = nzchar(pt_record_value(record, "load_note")),
      official_source_link_present = nzchar(pt_record_value(record, "source_page")),
      diagnostic_code = capability$diagnostic_code,
      diagnostic_severity = capability$diagnostic_severity,
      diagnostic_detail = capability$diagnostic_detail,
      inventory_flag = "",
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

pt_finalize_external_layer_inventory <- function(
    catalog_df,
    capability_layer_rows,
    write_qa = TRUE,
    qa_path = file.path("04_processed_data", "qa", "layer_inventory_latest.csv")) {
  rows <- pt_external_layer_inventory_rows(catalog_df, capability_layer_rows)
  finalized <- pt_finalize_layer_inventory_rows(rows, write_qa, qa_path)
  if (nrow(finalized$rows) != nrow(capability_layer_rows) ||
      !identical(finalized$rows$layer_key, capability_layer_rows$layer_key)) {
    stop("External inventory does not reconcile one-to-one with capability layers.", call. = FALSE)
  }
  finalized
}
