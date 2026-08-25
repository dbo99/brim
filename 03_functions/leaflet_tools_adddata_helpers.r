# ==== leaflet_tools_adddata_helpers.r ========================================
##
## PURPOSE:
##   Add the left-side PortaTreasure2 "Tools / Add Data" drawer.
##
## DESIGN:
##   This helper is intentionally browser-side only. It does not alter cached
##   map layers, write files, or change any preprocessing behavior.
##
##   Current tools:
##     1. Simple distance measurement.
##     2. Simple polygon-area measurement.
##     3. Temporary user-added external GIS layers.
##     4. Built-in Federal Land Status / Surface Management Agency context overlay.
##     5. Curated source-links modal for finding public GIS services.
##
## IMPORTANT:
##   External GIS layers are temporary screening overlays. They are added only
##   in the browser session. They are not saved into the PortaTreasure2 cache
##   and should not be interpreted as validated PT2 source layers.
##
##   Some external services may fail from local file:// HTML because of CORS,
##   service permissions, or agency/security restrictions. The panel reports
##   these failures rather than silently failing.
##

# ==== 1. Build the read-only descriptive Layer Explorer payload =============

pt_build_layer_explorer_metadata <- function(
  catalog_path = file.path("08_docs", "catalog", "BRIM_LAYER_CATALOG.csv")
) {
  expected_columns <- c(
    "candidate_stable_id",
    "runtime_display_name",
    "architecture",
    "catalog_path",
    "definition_authority",
    "assembly_authority",
    "legend_authority",
    "label_authority",
    "popup_authority",
    "filter_authority",
    "lifecycle_summary",
    "notes_or_resources",
    "domain_review_fields",
    "confidence",
    "gap"
  )

  if (!file.exists(catalog_path)) {
    stop("Missing descriptive Layer Explorer catalog: ", catalog_path, call. = FALSE)
  }

  catalog_df <- utils::read.csv(
    catalog_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character(),
    colClasses = "character",
    fileEncoding = "UTF-8",
    comment.char = "",
    quote = "\"",
    strip.white = FALSE,
    blank.lines.skip = FALSE,
    fill = FALSE
  )

  if (!identical(names(catalog_df), expected_columns)) {
    stop(
      "Descriptive Layer Explorer catalog schema changed; expected the maintained 15-field schema.",
      call. = FALSE
    )
  }
  if (nrow(catalog_df) != 26L) {
    stop("Descriptive Layer Explorer requires exactly 26 current catalog rows.", call. = FALSE)
  }

  stable_ids <- trimws(catalog_df$candidate_stable_id)
  stable_ids[is.na(stable_ids)] <- ""
  if (any(stable_ids == "") || anyDuplicated(stable_ids)) {
    stop("Descriptive Layer Explorer stable IDs must be nonblank and unique.", call. = FALSE)
  }

  architecture_tokens <- strsplit(catalog_df$architecture, ";", fixed = TRUE)
  architecture_family <- vapply(architecture_tokens, function(tokens) {
    family <- intersect(tokens, c("LOCAL", "EXTERNAL", "OPS_LIVE"))
    if (length(family) != 1L) {
      stop("Layer Explorer record has an unknown architecture family.", call. = FALSE)
    }
    switch(
      family,
      LOCAL = "Local",
      EXTERNAL = "External",
      OPS_LIVE = "Ops Live"
    )
  }, character(1))

  implementation_marker <- vapply(architecture_tokens, function(tokens) {
    markers <- intersect(
      tokens,
      c("CUSTOM_CONTROLLER", "CUSTOM_LOADER", "SHARED_CONTROLLER", "ORDINARY_LEAFLET")
    )
    if (length(markers) != 1L) {
      stop("Layer Explorer record has an unknown implementation marker.", call. = FALSE)
    }
    switch(
      markers,
      CUSTOM_CONTROLLER = "Custom controller",
      CUSTOM_LOADER = "Custom loader",
      SHARED_CONTROLLER = "Shared controller",
      ORDINARY_LEAFLET = "Ordinary Leaflet"
    )
  }, character(1))

  renderer_marker <- vapply(architecture_tokens, function(tokens) {
    markers <- intersect(
      tokens,
      c(
        "NON_GENERIC_RENDERER",
        "GENERIC_CATEGORICAL_RENDERER",
        "GENERIC_FEATURE_RENDERER",
        "OFFICIAL_MAPSERVER_RENDERER",
        "IMAGE_SERVER_RENDERER"
      )
    )
    if (length(markers) != 1L) {
      stop("Layer Explorer record has an unknown renderer marker.", call. = FALSE)
    }
    switch(
      markers,
      NON_GENERIC_RENDERER = "Non-generic renderer",
      GENERIC_CATEGORICAL_RENDERER = "Generic categorical renderer",
      GENERIC_FEATURE_RENDERER = "Generic feature renderer",
      OFFICIAL_MAPSERVER_RENDERER = "Official MapServer renderer",
      IMAGE_SERVER_RENDERER = "ImageServer renderer"
    )
  }, character(1))

  domain_review_status <- vapply(catalog_df$domain_review_fields, function(value) {
    value <- trimws(value)
    if (is.na(value) || value == "") return("UNKNOWN")
    if (identical(value, "NOT_APPLICABLE")) return("NOT_APPLICABLE")
    "DOMAIN_REVIEW_REQUIRED"
  }, character(1))

  records <- lapply(seq_len(nrow(catalog_df)), function(i) {
    list(
      stableId = stable_ids[[i]],
      displayName = trimws(catalog_df$runtime_display_name[[i]]),
      architecture = architecture_family[[i]],
      catalogPath = trimws(catalog_df$catalog_path[[i]]),
      implementation = implementation_marker[[i]],
      renderer = renderer_marker[[i]],
      domainReview = domain_review_status[[i]],
      confidence = trimws(catalog_df$confidence[[i]])
    )
  })
  records <- unname(records)

  embedded_values <- unname(unlist(records, recursive = TRUE, use.names = FALSE))
  executable_patterns <- c(
    "<script", "javascript:", "source[[:space:]]*\\(",
    "sys\\.source[[:space:]]*\\(", "eval[[:space:]]*\\(",
    "function[[:space:]]*\\(", "=>", "on(click|load)[[:space:]]*="
  )
  for (pattern in executable_patterns) {
    if (any(grepl(pattern, embedded_values, ignore.case = TRUE, perl = TRUE))) {
      stop("Layer Explorer metadata contains executable content.", call. = FALSE)
    }
  }

  machine_path_patterns <- c(
    "(^|[[:space:];=])/(Users|home|private|tmp|var|opt|Volumes)/",
    "[A-Za-z]:[\\\\/]",
    "^\\\\\\\\",
    "BRIM_rehabilitation_(audit_staging|worktrees|builds)"
  )
  for (pattern in machine_path_patterns) {
    if (any(grepl(pattern, embedded_values, perl = TRUE))) {
      stop("Layer Explorer metadata contains a machine-local path.", call. = FALSE)
    }
  }

  list(
    coverage = "PARTIAL",
    catalogAuthority = "DESCRIPTIVE_ONLY",
    uiConsumption = "READ_ONLY_METADATA",
    runtimeControl = "NONE",
    runtimeAuthority = "UNCHANGED",
    records = records
  )
}

# ==== 1. Add left-side tools / add-data panel =================================

pt_add_tools_adddata_panel <- function(m, map_display) {

  if (!isTRUE(map_display$add_tools_adddata_panel)) {
    return(m)
  }

  add_blm_sma <- isTRUE(map_display$add_blm_sma_context_overlay)
  layer_explorer_metadata <- pt_build_layer_explorer_metadata()

  ## Pass small R configuration values through htmlwidgets data rather than
  ## interpolating them into the JavaScript string with sprintf().
  ##
  ## WHY:
  ##   This JavaScript block contains CSS percentages (for example width: 100%)
  ##   and JavaScript modulo operators (%). Using sprintf() around a large JS
  ##   string is fragile because every literal percent sign must be escaped as
  ##   %%. Passing a small config list through onRender(data = ...) is safer.
  # ---- Optional starter catalog for the Tools / Add Data panel ---------------
  ##
  ## The catalog is intentionally optional.  If the CSV is missing, the panel
  ## still works with manual URL entry.  If the CSV exists, its rows are embedded
  ## into the standalone HTML at build time so the map does not need to query a
  ## live catalog service just to populate the dropdowns.
  catalog_path <- file.path("00_config", "external_service_catalog.csv")
  catalog_records <- list()
  capability_coverage_records <- list()
  capability_definitions <- pt_layer_capability_definitions()

  if (file.exists(catalog_path)) {

    catalog_df <- utils::read.csv(
      catalog_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    required_catalog_cols <- c(
      "agency",
      "program",
      "theme",
      "external_group",
      "external_group_order",
      "external_subgroup",
      "external_subgroup_order",
      "external_layer_id",
      "display_name",
      "service_type",
      "service_url",
      "supports_popups",
      "default_clickable",
      "default_opacity",
      "notes",
      "source_page",
      "geographic_scope",
      "pt2_usage_note",
      "primary_panel",
      "priority",
      "default_load_mode",
      "where_clause",
      "large_layer_warning",
      "min_zoom_live",
      "min_zoom_current_view",
      "legend_url",
      "legend_note",
      "best_use",
      "useful_for_visualization",
      "popup_fields",
      "popup_aliases",
      "popup_link_template",
      "popup_link_label",
      "identify_url",
      "hover_fields",
      "hover_aliases",
      "hover_bold_fields",
      "hover_no_label_fields",
      "hover_round_fields",
      "hover_show_native_field_names",
      "default_label_field",
      "out_fields",
      "style_field_candidates",
      "default_style_field",
      "default_style_method",
      "style_units",
      "style_legend_title",
      "style_direction",
      "field_curation_notes",
      "show_native_field_names",
      "load_badge",
      "load_badge_override",
      "load_score",
      "load_score_override",
      "load_note",
      "load_audit_basis",
      "load_n_tests",
      "load_success_rate",
      "load_median_seconds",
      "load_p90_seconds",
      "load_max_seconds",
      "load_feature_cap_rate"
    )

    missing_catalog_cols <- setdiff(required_catalog_cols, names(catalog_df))

    if (length(missing_catalog_cols) > 0) {
      for (nm in missing_catalog_cols) {
        catalog_df[[nm]] <- ""
      }
    }

    ## ---- Optional field-curation overrides ---------------------------------
    ##
    ## Human-reviewed popup/hover/style decisions can live in:
    ##
    ##   00_config/external_service_field_overrides.csv
    ##
    ## This lets the external-overlay catalog remain mostly about services while
    ## keeping field curation in a small, reviewable config file.  Only nonblank
    ## override cells are applied.  Matching uses service_url first when present,
    ## then display_name.
    field_overrides_path <- file.path("00_config", "external_service_field_overrides.csv")

    if (file.exists(field_overrides_path)) {

      field_overrides_df <- utils::read.csv(
        field_overrides_path,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      for (nm in required_catalog_cols) {
        if (!nm %in% names(field_overrides_df)) {
          field_overrides_df[[nm]] <- ""
        }
      }

      if (!"service_url" %in% names(field_overrides_df)) {
        field_overrides_df$service_url <- ""
      }

      if (!"display_name" %in% names(field_overrides_df)) {
        field_overrides_df$display_name <- ""
      }

      field_override_cols <- intersect(
        c(
          "best_use",
          "useful_for_visualization",
          "popup_fields",
          "popup_aliases",
          "popup_link_template",
          "popup_link_label",
          "identify_url",
          "hover_fields",
      "hover_aliases",
      "hover_bold_fields",
      "hover_no_label_fields",
      "hover_round_fields",
      "hover_show_native_field_names",
          "default_label_field",
          "out_fields",
          "style_field_candidates",
          "default_style_field",
          "default_style_method",
          "style_units",
          "style_legend_title",
          "style_direction",
          "field_curation_notes",
          "show_native_field_names"
        ),
        required_catalog_cols
      )

      n_field_override_rows <- 0

      for (ii in seq_len(nrow(field_overrides_df))) {

        service_url_i <- trimws(as.character(field_overrides_df$service_url[ii]))
        display_name_i <- trimws(as.character(field_overrides_df$display_name[ii]))

        if (is.na(service_url_i)) service_url_i <- ""
        if (is.na(display_name_i)) display_name_i <- ""

        idx <- integer()

        if (service_url_i != "") {
          idx <- which(trimws(as.character(catalog_df$service_url)) == service_url_i)
        }

        if (length(idx) == 0 && display_name_i != "") {
          idx <- which(trimws(as.character(catalog_df$display_name)) == display_name_i)
        }

        if (length(idx) == 0) {
          next
        }

        applied_this_row <- FALSE

        for (nm in field_override_cols) {
          val <- as.character(field_overrides_df[[nm]][ii])

          if (!is.na(val) && trimws(val) != "") {
            catalog_df[[nm]][idx] <- val
            applied_this_row <- TRUE
          }
        }

        if (isTRUE(applied_this_row)) {
          n_field_override_rows <- n_field_override_rows + 1
        }
      }

      message("External service field override rows applied: ", n_field_override_rows)
    }

    catalog_df <- catalog_df[required_catalog_cols]

    catalog_df$service_url <- trimws(as.character(catalog_df$service_url))
    catalog_df$display_name <- trimws(as.character(catalog_df$display_name))
    catalog_df$agency <- trimws(as.character(catalog_df$agency))
    catalog_df$theme <- trimws(as.character(catalog_df$theme))
    catalog_df$external_group <- trimws(as.character(catalog_df$external_group))
    catalog_df$external_group_order <- trimws(as.character(catalog_df$external_group_order))
    catalog_df$external_subgroup <- trimws(as.character(catalog_df$external_subgroup))
    catalog_df$external_subgroup_order <- trimws(as.character(catalog_df$external_subgroup_order))
    catalog_df$external_layer_id <- trimws(as.character(catalog_df$external_layer_id))
    catalog_df$program <- trimws(as.character(catalog_df$program))
    catalog_df$service_type <- trimws(tolower(as.character(catalog_df$service_type)))

    catalog_df <- catalog_df[
      !is.na(catalog_df$service_url) &
        catalog_df$service_url != "" &
        !is.na(catalog_df$display_name) &
        catalog_df$display_name != "",
      ,
      drop = FALSE
    ]

    catalog_df$priority_num <- suppressWarnings(as.numeric(catalog_df$priority))
    catalog_df$priority_num[is.na(catalog_df$priority_num)] <- 999
    catalog_df$external_group_order_num <- suppressWarnings(as.numeric(catalog_df$external_group_order))
    catalog_df$external_group_order_num[is.na(catalog_df$external_group_order_num)] <- 999
    catalog_df$external_subgroup_order_num <- suppressWarnings(as.numeric(catalog_df$external_subgroup_order))
    catalog_df$external_subgroup_order_num[is.na(catalog_df$external_subgroup_order_num)] <- 999

    if (nrow(catalog_df) > 0) {
      # Keep this canonical sort in sync with ptRenderQuickCatalog() in
      # 03_functions/js/leaflet_tools_adddata_panel.js.  External display
      # numbers are assigned from this order, so the browser panel should not
      # apply a different row-level ordering that makes badge numbers appear
      # out of sequence within a subgroup.
      catalog_df <- catalog_df[order(
        catalog_df$external_group_order_num,
        catalog_df$external_group,
        catalog_df$external_subgroup_order_num,
        catalog_df$external_subgroup,
        catalog_df$priority_num,
        catalog_df$display_name
      ), , drop = FALSE]

      # Stable External Layer IDs are build contracts for panel-visible rows.
      # Hidden legacy rows may retain the old fallback solely so they can still
      # be serialized; they cannot enter capability coverage without a real ID.
      if (!"external_layer_id" %in% names(catalog_df)) catalog_df$external_layer_id <- ""
      missing_ext_id <- is.na(catalog_df$external_layer_id) | trimws(as.character(catalog_df$external_layer_id)) == ""
      external_visible <- tolower(trimws(as.character(catalog_df$primary_panel))) %in% c("external", "both")
      if (any(missing_ext_id & external_visible)) {
        stop(
          "External-visible catalog rows require maintained stable external_layer_id values.",
          call. = FALSE
        )
      }
      hidden_missing <- missing_ext_id & !external_visible
      catalog_df$external_layer_id[hidden_missing] <- sprintf("HIDDEN%03d", which(hidden_missing))

      # User-facing display numbers are assigned after the same sort used by
      # the External Layers panel, but only to rows that are actually visible
      # in the External panel. Disabled/Ops-only rows retain stable IDs but do
      # not consume user-facing layer numbers.
      catalog_df$external_display_num <- ""
      catalog_df$external_display_num[external_visible] <- as.character(seq_len(sum(external_visible)))
      visible_display_numbers <- suppressWarnings(as.integer(
        catalog_df$external_display_num[external_visible]
      ))
      expected_display_numbers <- seq_len(sum(external_visible))
      if (
        length(visible_display_numbers) != length(expected_display_numbers) ||
        anyNA(visible_display_numbers) ||
        anyDuplicated(visible_display_numbers) ||
        !identical(visible_display_numbers, expected_display_numbers)
      ) {
        stop(
          "External catalog display-number validation failed: visible map ",
          "layers must have one continuous, registry-derived 1..N sequence.",
          call. = FALSE
        )
      }

      capability_result <- pt_finalize_external_layer_capabilities(catalog_df)
      catalog_df <- capability_result$catalog
      pt_finalize_external_layer_inventory(
        catalog_df,
        capability_result$layer_rows
      )
      capability_definitions <- capability_result$definitions
      capability_coverage_records <- lapply(
        seq_len(nrow(capability_result$coverage)),
        function(i) as.list(capability_result$coverage[i, , drop = FALSE])
      )

      catalog_records <- lapply(seq_len(nrow(catalog_df)), function(i) {
        list(
          agency = as.character(catalog_df$agency[i]),
          program = as.character(catalog_df$program[i]),
          theme = as.character(catalog_df$theme[i]),
          external_group = as.character(catalog_df$external_group[i]),
          external_group_order = as.character(catalog_df$external_group_order[i]),
          external_subgroup = as.character(catalog_df$external_subgroup[i]),
          external_subgroup_order = as.character(catalog_df$external_subgroup_order[i]),
          external_layer_id = as.character(catalog_df$external_layer_id[i]),
          external_display_num = as.character(catalog_df$external_display_num[i]),
          display_name = as.character(catalog_df$display_name[i]),
          service_type = as.character(catalog_df$service_type[i]),
          service_url = as.character(catalog_df$service_url[i]),
          supports_popups = as.character(catalog_df$supports_popups[i]),
          default_clickable = as.character(catalog_df$default_clickable[i]),
          default_opacity = as.character(catalog_df$default_opacity[i]),
          notes = as.character(catalog_df$notes[i]),
          source_page = as.character(catalog_df$source_page[i]),
          geographic_scope = as.character(catalog_df$geographic_scope[i]),
          pt2_usage_note = as.character(catalog_df$pt2_usage_note[i]),
          primary_panel = as.character(catalog_df$primary_panel[i]),
          priority = as.character(catalog_df$priority[i]),
          default_load_mode = as.character(catalog_df$default_load_mode[i]),
          where_clause = as.character(catalog_df$where_clause[i]),
          large_layer_warning = as.character(catalog_df$large_layer_warning[i]),
          min_zoom_live = as.character(catalog_df$min_zoom_live[i]),
          min_zoom_current_view = as.character(catalog_df$min_zoom_current_view[i]),
          legend_url = as.character(catalog_df$legend_url[i]),
          legend_note = as.character(catalog_df$legend_note[i]),
          best_use = as.character(catalog_df$best_use[i]),
          useful_for_visualization = as.character(catalog_df$useful_for_visualization[i]),
          popup_fields = as.character(catalog_df$popup_fields[i]),
          popup_aliases = as.character(catalog_df$popup_aliases[i]),
          popup_link_template = as.character(catalog_df$popup_link_template[i]),
          popup_link_label = as.character(catalog_df$popup_link_label[i]),
          identify_url = as.character(catalog_df$identify_url[i]),
          hover_fields = as.character(catalog_df$hover_fields[i]),
          hover_aliases = as.character(catalog_df$hover_aliases[i]),
          hover_bold_fields = as.character(catalog_df$hover_bold_fields[i]),
          hover_no_label_fields = as.character(catalog_df$hover_no_label_fields[i]),
          hover_round_fields = as.character(catalog_df$hover_round_fields[i]),
          hover_show_native_field_names = as.character(catalog_df$hover_show_native_field_names[i]),
          default_label_field = as.character(catalog_df$default_label_field[i]),
          out_fields = as.character(catalog_df$out_fields[i]),
          style_field_candidates = as.character(catalog_df$style_field_candidates[i]),
          default_style_field = as.character(catalog_df$default_style_field[i]),
          default_style_method = as.character(catalog_df$default_style_method[i]),
          style_units = as.character(catalog_df$style_units[i]),
          style_legend_title = as.character(catalog_df$style_legend_title[i]),
          style_direction = as.character(catalog_df$style_direction[i]),
          field_curation_notes = as.character(catalog_df$field_curation_notes[i]),
          show_native_field_names = as.character(catalog_df$show_native_field_names[i]),
          load_badge = as.character(catalog_df$load_badge[i]),
          load_badge_override = as.character(catalog_df$load_badge_override[i]),
          load_score = as.character(catalog_df$load_score[i]),
          load_score_override = as.character(catalog_df$load_score_override[i]),
          load_note = as.character(catalog_df$load_note[i]),
          load_audit_basis = as.character(catalog_df$load_audit_basis[i]),
          load_n_tests = as.character(catalog_df$load_n_tests[i]),
          load_success_rate = as.character(catalog_df$load_success_rate[i]),
          load_median_seconds = as.character(catalog_df$load_median_seconds[i]),
          load_p90_seconds = as.character(catalog_df$load_p90_seconds[i]),
          load_max_seconds = as.character(catalog_df$load_max_seconds[i]),
          load_feature_cap_rate = as.character(catalog_df$load_feature_cap_rate[i]),
          layer_key = as.character(catalog_df$layer_key[i]),
          panel_visible = isTRUE(catalog_df$panel_visible[i]),
          has_legend = isTRUE(catalog_df$has_legend[i]),
          has_feature_info = isTRUE(catalog_df$has_feature_info[i]),
          feature_info_hover = isTRUE(catalog_df$feature_info_hover[i]),
          feature_info_popup = isTRUE(catalog_df$feature_info_popup[i]),
          feature_info_identify = isTRUE(catalog_df$feature_info_identify[i]),
          feature_info_equivalent = isTRUE(catalog_df$feature_info_equivalent[i]),
          legend_type = as.character(catalog_df$legend_type[i]),
          legend_status = as.character(catalog_df$legend_status[i]),
          legend_surface = as.character(catalog_df$legend_surface[i]),
          legend_renderer_key = as.character(catalog_df$legend_renderer_key[i]),
          legend_mount_path = as.character(catalog_df$legend_mount_path[i]),
          legend_shared_with = as.character(catalog_df$legend_shared_with[i]),
          legend_automatic_mount = isTRUE(catalog_df$legend_automatic_mount[i]),
          legend_keyed_symbology = isTRUE(catalog_df$legend_keyed_symbology[i]),
          structured_style_mapping_available = isTRUE(catalog_df$structured_style_mapping_available[i]),
          legend_candidate_basis = as.character(catalog_df$legend_candidate_basis[i]),
          provider_legend_available = isTRUE(catalog_df$provider_legend_available[i]),
          provider_legend_url = as.character(catalog_df$provider_legend_url[i]),
          provider_legend_scope = as.character(catalog_df$provider_legend_scope[i]),
          provider_legend_reference_only = isTRUE(catalog_df$provider_legend_reference_only[i]),
          legend_adapter = as.character(catalog_df$legend_adapter[i]),
          info_adapter = as.character(catalog_df$info_adapter[i]),
          info_content_basis = as.character(catalog_df$info_content_basis[i]),
          info_fields_curated = isTRUE(catalog_df$info_fields_curated[i]),
          info_content_quality = as.character(catalog_df$info_content_quality[i]),
          registration_source = as.character(catalog_df$registration_source[i]),
          content_basis = as.character(catalog_df$content_basis[i]),
          legend_renderable = isTRUE(catalog_df$legend_renderable[i]),
          info_content_meaningful = isTRUE(catalog_df$info_content_meaningful[i]),
          group_layer_count = as.integer(catalog_df$group_layer_count[i]),
          group_legend_count = as.integer(catalog_df$group_legend_count[i]),
          group_info_count = as.integer(catalog_df$group_info_count[i]),
          subgroup_layer_count = as.integer(catalog_df$subgroup_layer_count[i]),
          subgroup_legend_count = as.integer(catalog_df$subgroup_legend_count[i]),
          subgroup_info_count = as.integer(catalog_df$subgroup_info_count[i]),
          capability_diagnostic_code = as.character(catalog_df$diagnostic_code[i]),
          capability_diagnostic_severity = as.character(catalog_df$diagnostic_severity[i]),
          capability_diagnostic_detail = as.character(catalog_df$diagnostic_detail[i])
        )
      })

      catalog_records <- unname(catalog_records)
    }

    message("External service catalog rows embedded: ", length(catalog_records))

  } else {

    message(
      "External service catalog not found; Tools / Add Data manual URL entry will still work: ",
      catalog_path
    )
  }

  tools_data <- list(
    enable_blm_sma = add_blm_sma,
    catalog = catalog_records,
    layer_explorer = layer_explorer_metadata,
    capability_definitions = capability_definitions,
    capability_coverage = capability_coverage_records
  )

  js_path <- file.path(
    "03_functions",
    "js",
    "leaflet_tools_adddata_panel.js"
  )
  uic_explorer_js_path <- file.path(
    "03_functions",
    "js",
    "brim_uic_explorer.js"
  )

  if (!file.exists(js_path)) {
    stop("Missing External Layers panel JavaScript helper: ", js_path)
  }
  if (!file.exists(uic_explorer_js_path)) {
    stop("Missing External-only UIC explorer JavaScript helper: ", uic_explorer_js_path)
  }

  js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")
  uic_explorer_js <- paste(
    readLines(uic_explorer_js_path, warn = FALSE),
    collapse = "\n"
  )

  m <- htmlwidgets::onRender(
    m,
    uic_explorer_js
  )

  htmlwidgets::onRender(
    m,
    js,
    data = tools_data
  )
}
