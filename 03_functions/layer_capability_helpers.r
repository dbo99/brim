# ==== layer_capability_helpers.r ============================================
##
## Panel-neutral layer-capability resolution and QA output. External Layers is
## the first consumer, but the schema and definitions deliberately avoid
## External-specific names.

PT_LAYER_CAPABILITY_SCHEMA_VERSION <- "1.0"
PT_LAYER_INVENTORY_SCHEMA_VERSION <- "1.0"

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

pt_capability_git_head <- function() {
  out <- tryCatch(
    suppressWarnings(system2("git", c("rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE)),
    error = function(e) character()
  )
  if (length(out) == 1L && grepl("^[0-9a-fA-F]{40}$", out)) out else ""
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
  data.frame(
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
    qualifies_lgnd = c(rep(TRUE, 13L), FALSE, TRUE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )
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

pt_external_provider_legend_available <- function(record) {
  if (nzchar(pt_record_value(record, "legend_url"))) return(TRUE)
  identical(tolower(pt_record_value(record, "service_type")), "map") &&
    grepl("/MapServer(?:/\\d+)?/?(?:\\?.*)?$", pt_record_value(record, "service_url"), ignore.case = TRUE)
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
  generic_popup_unverified <- FALSE

  if (uic) {
    hover <- TRUE
    popup <- TRUE
    equivalent <- TRUE
    adapter <- "uic_bespoke"
    basis <- c(basis, "registered UIC tooltip/popup interaction")
  } else if (dwr_tre_raster) {
    hover <- hover_config
    popup <- clickable || popup_config || link_config
    identify <- TRUE
    equivalent <- TRUE
    adapter <- "dwr_tre_raster_identify"
    basis <- c(basis, "BRIM-formatted DWR/TRE ImageServer identify value")
  } else if (image_route) {
    adapter <- "image_identify_unqualified"
  } else if (vector_route) {
    hover <- hover_config || label_config
    meaningful_config <- popup_config || hover_config || label_config || link_config || mlrs
    popup <- clickable && meaningful_config
    equivalent <- mlrs && popup
    adapter <- if (mlrs) "mlrs_aggregate" else "shared_vector_feature"
    if (hover_config) basis <- c(basis, "configured hover fields")
    if (!hover_config && label_config) basis <- c(basis, "usable default label")
    if (popup_config && clickable) basis <- c(basis, "configured popup fields")
    if (link_config && clickable) basis <- c(basis, "feature-specific popup link")
    if (clickable && !meaningful_config) {
      basis <- c(basis, "generic vector popup route present; meaningful content unverified")
      generic_popup_unverified <- TRUE
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
    if (clickable && !meaningful_config) {
      basis <- c(basis, "generic MapServer popup route present; meaningful content unverified")
      generic_popup_unverified <- TRUE
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
    generic_popup_unverified = generic_popup_unverified,
    bare_info_flags = clickable && !(hover || popup || identify || equivalent)
  )
}

pt_capability_diagnostic_severities <- function() c("info", "warning", "error")

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
    "provider_legend_available", "legend_adapter", "info_adapter",
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
    qualifying_adapters <- adapters[adapter_catalog$qualifies_lgnd[adapter_match]]
    has_legend <- length(qualifying_adapters) > 0L
    legend_renderable <- length(adapters) > 0L
    legend_type <- ""
    if (has_legend) {
      types <- adapter_catalog$legend_type[match(qualifying_adapters, adapter_catalog$adapter)]
      legend_type <- if ("dynamic_map_card" %in% types) "dynamic_map_card" else "inline"
    } else if (legend_renderable) {
      legend_type <- paste(unique(adapter_catalog$legend_type[adapter_match]), collapse = ";")
    }
    provider_link <- pt_external_provider_legend_available(record)
    if (!has_legend && !legend_renderable && provider_link) legend_type <- "provider_link"
    info <- pt_external_info_resolution(record)
    codes <- severities <- details <- character()
    if (provider_link) {
      diagnostic <- pt_add_capability_diagnostic(codes, severities, details, "provider_legend_reference_only", "info", "Provider legend availability is recorded but does not establish LGND.")
      codes <- diagnostic$codes; severities <- diagnostic$severities; details <- diagnostic$details
    }
    specialized_family <- pt_external_specialized_style_family(record)
    if (nzchar(specialized_family) && !has_legend) {
      diagnostic <- pt_add_capability_diagnostic(codes, severities, details, "specialized_style_without_registered_legend", "warning", paste0("Specialized ", specialized_family, " styling has no registered usable BRIM legend."))
      codes <- diagnostic$codes; severities <- diagnostic$severities; details <- diagnostic$details
    }
    if (isTRUE(info$generic_popup_unverified)) {
      diagnostic <- pt_add_capability_diagnostic(codes, severities, details, "generic_popup_content_unverified", "warning", "A generic popup route exists, but meaningful returned content is not deterministically established.")
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
    if (has_legend) content_basis <- c(content_basis, paste0("BRIM renderer: ", qualifying_adapters))
    if (legend_renderable && !has_legend) content_basis <- c(content_basis, paste0("BRIM style note renderer: ", adapters))
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
      provider_legend_available = provider_link,
      legend_adapter = paste(adapters, collapse = ";"),
      info_adapter = info$info_adapter,
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
    row$legend_type <- row$legend_adapter <- row$info_adapter <- "aggregate"
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
    row$legend_type <- row$legend_adapter <- row$info_adapter <- "aggregate"
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
  invalid_lgnd <- vapply(seq_len(nrow(layer_rows)), function(i) {
    if (!isTRUE(layer_rows$has_legend[i])) return(FALSE)
    adapters <- row_adapters[[i]]
    !length(adapters) || !any(adapter_catalog$qualifies_lgnd[match(adapters, adapter_catalog$adapter)])
  }, logical(1))
  if (any(invalid_lgnd)) stop("Normalized LGND rows lack a qualifying registered legend adapter.", call. = FALSE)
  pt_validate_legend_adapter_implementations()
  pt_validate_capability_registrations(registrations, layer_rows$layer_key)
  layer_rows <- pt_apply_capability_registrations(layer_rows, registrations)
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

pt_finalize_external_layer_capabilities <- function(
    catalog_df,
    registrations = pt_new_layer_capability_registry(),
    write_qa = TRUE,
    qa_path = file.path("04_processed_data", "qa", "layer_capability_coverage_latest.csv"),
    build_timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    git_head = pt_capability_git_head()) {
  resolved_rows <- pt_external_layer_capabilities(catalog_df, build_timestamp, git_head)
  finalized <- pt_finalize_layer_capability_rows(resolved_rows, registrations, write_qa, qa_path)
  layer_rows <- finalized$layer_rows

  match_idx <- match(paste0("external:", pt_capability_text(catalog_df$external_layer_id)), layer_rows$layer_key)
  capability_cols <- c(
    "layer_key", "panel_visible", "has_legend", "has_feature_info",
    "feature_info_hover", "feature_info_popup", "feature_info_identify",
    "feature_info_equivalent", "legend_type", "provider_legend_available",
    "legend_adapter", "info_adapter", "registration_source", "content_basis",
    "legend_renderable", "info_content_meaningful", "group_layer_count",
    "group_legend_count", "group_info_count", "subgroup_layer_count",
    "subgroup_legend_count", "subgroup_info_count", "diagnostic_code",
    "diagnostic_severity", "diagnostic_detail"
  )
  logical_cols <- c("panel_visible", "has_legend", "has_feature_info", "feature_info_hover", "feature_info_popup", "feature_info_identify", "feature_info_equivalent", "provider_legend_available", "legend_renderable", "info_content_meaningful")
  for (nm in capability_cols) catalog_df[[nm]] <- if (nm %in% logical_cols) FALSE else if (grepl("_count$", nm)) NA_integer_ else ""
  visible_idx <- which(!is.na(match_idx))
  for (nm in capability_cols) catalog_df[[nm]][visible_idx] <- layer_rows[[nm]][match_idx[visible_idx]]

  joined <- sum(catalog_df$panel_visible)
  if (joined != nrow(layer_rows)) stop("Capability records could not join one-to-one to the finalized External catalog.", call. = FALSE)
  list(
    catalog = catalog_df,
    layer_rows = layer_rows,
    coverage = finalized$coverage,
    definitions = pt_layer_capability_definitions(),
    legend_adapters = pt_layer_legend_adapter_catalog(),
    qa_path = finalized$qa_path
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
    "style_adapter", "legend_adapter", "info_adapter",
    "provider_legend_available", "has_legend", "has_feature_info", "has_both",
    "has_neither", "feature_info_hover", "feature_info_popup",
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
    "generic_popup_content_unverified",
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
      "provider_legend_reference_only", "generic_popup_content_unverified",
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
      provider_legend_available = capability$provider_legend_available,
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
