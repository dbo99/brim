# ==== local_reference_interaction_helpers.r ================================
##
## Shared contracts for the bounded Local > Reference interaction framework.
## Phase 6 executes California Desert NCL beside the accepted Trails, National
## Monuments, Wilderness Study Areas, Federal Wilderness, and ACEC
## implementations. The remaining five rows stay validation-only.

pt_local_reference_clean_chr <- function(x, fallback = "") {
  value <- trimws(as.character(x))
  value[
    is.na(value) | value %in% c("", "NA", "N/A", "<NA>", "NULL")
  ] <- fallback
  value
}

pt_local_reference_normalize_text <- function(x) {
  value <- tolower(pt_local_reference_clean_chr(x))
  value <- gsub("&", " and ", value, fixed = TRUE)
  value <- gsub("\\bwilderness study area\\b", " ", value)
  value <- gsub("[^a-z0-9]+", " ", value)
  trimws(gsub("[[:space:]]+", " ", value))
}

pt_local_reference_config_row <- function(layer_id = NULL, source_nickname = NULL) {
  registry <- LOCAL_REFERENCE_INTERACTION_REGISTRY
  keep <- rep(TRUE, nrow(registry))
  if (!is.null(layer_id)) keep <- keep & registry$layer_id == layer_id
  if (!is.null(source_nickname)) {
    keep <- keep & registry$source_nickname == source_nickname
  }
  row <- registry[keep, , drop = FALSE]
  if (nrow(row) != 1L) {
    stop(
      "Expected exactly one Local Reference registry row; found ", nrow(row),
      ". layer_id=", paste(layer_id, collapse = ","),
      ", source_nickname=", paste(source_nickname, collapse = ",")
    )
  }
  row
}

pt_local_reference_group_name <- function(display_name) {
  clean_name <- trimws(gsub(" \\*+$", "", as.character(display_name)))
  if (exists("pt_note_group_name", mode = "function")) {
    clean_name <- pt_note_group_name(clean_name)
  }
  if (exists("pt_layer_group_name", mode = "function")) {
    return(pt_layer_group_name(clean_name))
  }
  ifelse(
    grepl("^(Reference|Channels|Labels|Points|Basins|Core|Ops)\\s+–\\s+", clean_name),
    clean_name,
    paste0("Reference – ", clean_name)
  )
}

pt_local_reference_categories <- function(layer_id) {
  row <- pt_local_reference_config_row(layer_id = layer_id)
  definition <- PT_LOCAL_REFERENCE_CATEGORY_DEFINITIONS[[row$category_definition]]
  if (is.null(definition)) {
    stop("Missing category definition for Local Reference layer: ", layer_id)
  }
  definition[order(definition$sort_order), , drop = FALSE]
}

pt_validate_local_reference_config <- function() {
  registry <- LOCAL_REFERENCE_INTERACTION_REGISTRY
  required_registry <- c(
    "layer_id", "source_nickname", "display_name", "implementation_status",
    "enrichment_depth",
    "color_basis", "color_source_field", "palette_key",
    "category_definition", "unknown_style", "shared_management_style",
    "legend_mode", "filter_mode", "auto_supported", "auto_default",
    "count_mode", "primary_count_mode", "primary_count_label",
    "show_component_count", "show_category_count", "category_count_mode",
    "component_count_label", "category_filter_visible",
    "category_heading", "card_caution", "popup_layout",
    "feature_selection_supported", "feature_selection_mode",
    "feature_search_fields", "feature_display_field",
    "auto_zoom_supported", "auto_zoom_default", "zoom_padding", "zoom_max",
    "preserve_view_on_reset", "retention_enabled",
    "distinguish_units_supported", "search_fields", "filter_facets",
    "quick_views",
    "category_sort_order"
  )
  missing_registry <- setdiff(required_registry, names(registry))
  if (length(missing_registry)) {
    stop("Local Reference registry is missing: ", paste(missing_registry, collapse = ", "))
  }
  if (!identical(as.character(registry$layer_id), PT_LOCAL_REFERENCE_LAYER_IDS)) {
    stop("Local Reference registry must contain the exact 11 approved rows in display order.")
  }
  if (anyDuplicated(registry$layer_id) || anyDuplicated(registry$source_nickname)) {
    stop("Local Reference registry layer IDs and source nicknames must be unique.")
  }
  if (any(registry$auto_default & !registry$auto_supported)) {
    stop("Local Reference Auto cannot default on where Auto is unsupported.")
  }
  if (any(registry$auto_zoom_default & !registry$auto_zoom_supported)) {
    stop("Local Reference Auto-zoom cannot default on where Auto-zoom is unsupported.")
  }
  if (any(registry$auto_zoom_supported & !registry$feature_selection_supported)) {
    stop("Local Reference Auto-zoom requires named-feature selection support.")
  }
  if (any(!registry$feature_selection_mode %in% c(
    "none", "semantic_feature_multi"
  ))) {
    stop("Local Reference feature_selection_mode contains an unsupported value.")
  }
  if (any(registry$feature_selection_supported &
          registry$feature_selection_mode == "none")) {
    stop("Enabled Local Reference feature selection requires a named selection mode.")
  }
  if (any(registry$feature_selection_supported &
          !nzchar(trimws(registry$feature_display_field)))) {
    stop("Enabled Local Reference feature selection requires a display field.")
  }
  feature_search_fields <- unclass(registry$feature_search_fields)
  if (length(feature_search_fields) != nrow(registry) ||
      any(registry$feature_selection_supported & lengths(feature_search_fields) == 0L)) {
    stop("Enabled Local Reference feature selection requires approved search fields.")
  }
  if (any(!is.finite(registry$zoom_padding)) || any(registry$zoom_padding < 0) ||
      any(!is.finite(registry$zoom_max)) ||
      any(registry$zoom_max < 1 | registry$zoom_max > 22)) {
    stop("Local Reference zoom padding and maximums must be finite and usable.")
  }
  if (anyNA(registry$preserve_view_on_reset)) {
    stop("Local Reference preserve_view_on_reset must be explicit for all 11 rows.")
  }
  if (any(!registry$primary_count_mode %in% c(
    "semantic_feature", "record", "geometry_component"
  ))) {
    stop("Local Reference primary_count_mode contains an unsupported value.")
  }
  if (any(!registry$category_count_mode %in% c(
    "semantic_feature", "record", "geometry_component"
  ))) {
    stop("Local Reference category_count_mode contains an unsupported value.")
  }
  if (any(!nzchar(trimws(registry$primary_count_label)))) {
    stop("Local Reference primary_count_label values must be nonblank.")
  }
  if (any(registry$show_component_count &
          !nzchar(trimws(registry$component_count_label)))) {
    stop("Visible Local Reference component counts require a full component label.")
  }
  category_heading_rows <- which(nzchar(trimws(registry$category_heading)))
  if (!identical(category_heading_rows, c(1L, 4L, 5L)) ||
      !identical(registry$category_heading[[1]], "Trail") ||
      !identical(
        registry$category_heading[[4]],
        "BLM recommendation for wilderness designation"
      ) ||
      !identical(registry$category_heading[[5]], "Managing agency")) {
    stop("Active Local Reference category headings differ from the approved contract.")
  }
  if (!all(registry$primary_count_mode == "semantic_feature")) {
    stop("Default Local Reference visible counts must use semantic features.")
  }
  if (any(!registry$popup_layout %in% c("standard", "tabbed_card")) ||
      !identical(which(registry$popup_layout == "tabbed_card"), c(1L, 2L, 3L, 4L, 5L, 7L))) {
    stop("Tabbed Local Reference popup layout differs from the six active focused layers.")
  }

  expected_auto_supported <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE)
  expected_auto_default <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)
  if (!identical(as.logical(registry$auto_supported), expected_auto_supported) ||
      !identical(as.logical(registry$auto_default), expected_auto_default)) {
    stop("Local Reference Auto support/default contract differs from the approved 11-layer matrix.")
  }
  if (!identical(
    which(registry$implementation_status %in% c(
      "phase2_trails", "phase5_national_monuments", "phase6_desert_ncl", "phase1_wsa",
      "phase3_federal_wilderness", "phase4_acec"
    )),
    c(1L, 2L, 3L, 4L, 5L, 7L)
  )) {
    stop("Local Reference execution differs from the six approved focused layers.")
  }
  if (!identical(which(registry$feature_selection_supported), c(1L, 2L, 3L, 4L, 5L, 7L)) ||
      !identical(which(registry$auto_zoom_supported), c(1L, 2L, 3L, 4L, 5L, 7L)) ||
      !identical(which(registry$auto_zoom_default), c(1L, 2L, 3L, 4L, 5L, 7L))) {
    stop("Named-feature selection and Auto-zoom differ from the six approved focused layers.")
  }
  if (!identical(
    unlist(registry$feature_search_fields[[1]], use.names = FALSE),
    c(
      "pt_trails_official_name", "pt_trails_common_name",
      "pt_trails_abbreviation", "pt_trails_alias_search", "pt_trails_nlcs_id"
    )
  )) {
    stop("Phase 2 Trails named-feature search fields differ from the approved contract.")
  }
  if (!identical(
    unlist(registry$feature_search_fields[[2]], use.names = FALSE),
    c(
      "pt_nm_canonical_name", "pt_nm_aliases", "pt_nm_administering_agencies",
      "pt_nm_original_authority", "pt_nm_source_identifiers",
      "pt_nm_component_names", "monument_id", "component_id"
    )
  )) {
    stop("Phase 5 National Monuments named-feature search fields differ from the approved contract.")
  }
  if (!identical(
    unlist(registry$feature_search_fields[[3]], use.names = FALSE),
    c(
      "pt_cdncl_display_name", "pt_cdncl_raw_name", "pt_cdncl_aliases",
      "NLCS_ID", "pt_cdncl_global_id", "pt_cdncl_unit_type_label",
      "pt_cdncl_field_office_names"
    )
  )) {
    stop("California Desert NCL named-feature search fields differ from the approved contract.")
  }
  if (!identical(
    unlist(registry$feature_search_fields[[4]], use.names = FALSE),
    c("NLCS_NAME", "WSACODE_ca", "CASEFILE_N", "NLCS_ID", "GlobalID")
  )) {
    stop("Phase 1 WSA named-feature search fields differ from the approved contract.")
  }
  if (!identical(
    unlist(registry$feature_search_fields[[5]], use.names = FALSE),
    c(
      "pt_fw_official_name", "NLCS_NAME", "wilderness_id", "component_id",
      "GlobalID", "FAU_ID", "pt_fw_agency_name", "pt_fw_alternate_names",
      "pt_fw_wilderness_abbreviation", "pt_fw_original_public_law"
    )
  )) {
    stop("Federal Wilderness named-feature search fields differ from the approved contract.")
  }
  if (!identical(
    unlist(registry$feature_search_fields[[7]], use.names = FALSE),
    c(
      "pt_acec_official_name", "pt_acec_legacy_name", "pt_acec_aliases",
      "pt_acec_governing_plan", "pt_acec_planning_framework",
      "pt_acec_source_admin_unit", "pt_acec_field_office_context_names",
      "acec_id", "component_id", "pt_acec_global_id"
    )
  )) {
    stop("ACEC named-feature search fields differ from the approved contract.")
  }
  if (!identical(which(registry$retention_enabled), c(1L, 2L, 3L, 4L, 5L, 7L))) {
    stop("Field retention differs from the six approved focused layers.")
  }
  if (!identical(which(registry$distinguish_units_supported), c(3L, 5L))) {
    stop("Distinguish named units must remain California Desert NCL/Federal Wilderness-only.")
  }
  facets <- unclass(registry$filter_facets)
  if (length(facets) != nrow(registry) ||
      !identical(which(lengths(facets) > 0L), c(2L, 3L, 5L, 7L)) ||
      !identical(
        vapply(facets[[2]], `[[`, character(1), "facet_key"),
        c(
          "administering_agency", "designation_authority",
          "management_pattern", "recent_change", "blm_usfs_quick_view"
        )
      ) ||
      !identical(
        vapply(facets[[3]], `[[`, character(1), "facet_key"),
        c("field_office_context", "related_designation_overlap")
      ) ||
      !identical(
        vapply(facets[[5]], `[[`, character(1), "facet_key"),
        c("management_pattern", "designation_history", "geographic_context")
      ) ||
      !identical(
        vapply(facets[[7]], `[[`, character(1), "facet_key"),
        c("relevant_value_family", "planning_framework", "field_office_context")
      )) {
    stop("Active Local Reference filter facets differ from their approved contracts.")
  }
  quick_views <- unclass(registry$quick_views)
  if (length(quick_views) != nrow(registry) ||
      !identical(which(lengths(quick_views) > 0L), c(2L, 7L)) ||
      !identical(
        vapply(quick_views[[2]], `[[`, character(1), "quick_view_key"),
        c("blm_involved", "shared_blm_usfs", "recent_2024_2025")
      ) ||
      !identical(
        vapply(quick_views[[7]], `[[`, character(1), "quick_view_key"),
        c(
          "fish_aquatic", "drecp", "wildlife_habitat", "cultural_historic",
          "scenic", "natural_systems"
        )
      )) {
    stop("National Monuments and ACEC quick views differ from their approved contracts, or California Desert NCL unexpectedly exposes one.")
  }
  acec_value_styles <- facets[[7]][[1]]$values
  if (!identical(acec_value_styles, PT_LOCAL_REFERENCE_ACEC_VALUE_FAMILY_STYLES) ||
      !identical(
        as.character(acec_value_styles$swatch_color),
        c("#3B82A0", "#7A9A4A", "#4F8C68", "#A66A43", "#8A6DAA", "#B58A3D")
      ) ||
      anyDuplicated(acec_value_styles$swatch_color) ||
      any(!grepl("^#[0-9A-F]{6}$", acec_value_styles$swatch_color)) ||
      !identical(
        facets[[7]][[1]]$thematic_style,
        PT_LOCAL_REFERENCE_ACEC_VALUE_THEMATIC_STYLE
      ) ||
      !identical(PT_LOCAL_REFERENCE_ACEC_VALUE_THEMATIC_STYLE$multiple_selection, "neutral")) {
    stop("ACEC value-family colors and thematic style must retain their centralized contract.")
  }
  acec_overlap_style <- PT_LOCAL_REFERENCE_ACEC_OVERLAP_STYLE
  if (!is.list(acec_overlap_style) ||
      !identical(
        names(acec_overlap_style),
        c(
          "palette", "fill_opacity", "stroke_weight", "stroke_darken",
          "minimum_overlap_area_m2"
        )
      ) ||
      length(acec_overlap_style$palette) != 6L ||
      anyDuplicated(acec_overlap_style$palette) ||
      any(!grepl("^#[0-9A-F]{6}$", acec_overlap_style$palette)) ||
      !isTRUE(acec_overlap_style$fill_opacity > 0 &&
                acec_overlap_style$fill_opacity <= 1) ||
      !isTRUE(acec_overlap_style$stroke_weight > 0) ||
      !isTRUE(acec_overlap_style$stroke_darken >= 0 &&
                acec_overlap_style$stroke_darken <= 1) ||
      !identical(acec_overlap_style$minimum_overlap_area_m2, 1)) {
    stop("ACEC overlap colors and display style differ from the reviewed contract.")
  }
  current_offices <- PT_LOCAL_REFERENCE_ACEC_CURRENT_FIELD_OFFICES
  current_office_text <- paste(
    current_offices$office_key,
    current_offices$current_official_name,
    current_offices$boundary_source_name
  )
  if (nrow(current_offices) != 14L ||
      anyDuplicated(current_offices$office_key) ||
      anyDuplicated(current_offices$office_code) ||
      !setequal(
        current_offices$office_code,
        c(
          "CAD05000", "CAD06000", "CAD07000", "CAD08000", "CAD09000",
          "CAC05000", "CAC06000", "CAC07000", "CAC08000", "CAC09000",
          "CAN02000", "CAN03000", "CAN05000", "CAN06000"
        )
      ) || any(grepl(
        "Hollister|Alturas|Susanville",
        current_office_text,
        ignore.case = TRUE
      ))) {
    stop("ACEC current field-office lookup differs from the verified 14-office roster.")
  }
  if (anyNA(registry$category_filter_visible) ||
      !identical(which(!registry$category_filter_visible), c(2L, 3L, 7L))) {
    stop("National Monuments, California Desert NCL, and ACEC must hide redundant category filters.")
  }
  expected_depth <- c(
    "rich", "rich", "rich", "rich", "rich", "moderate",
    "rich", "moderate", "basic", "basic", "basic"
  )
  if (!identical(as.character(registry$enrichment_depth), expected_depth)) {
    stop("Local Reference enrichment-depth tiers differ from the approved 11-layer matrix.")
  }
  expected_management_fields <- c(
    "layer_id", "semantic_feature_key", "geometry_key",
    "designation_authority", "administering_agency",
    "local_managing_agency", "co_managing_agencies", "blm_role",
    "blm_role_summary", "blm_office", "blm_office_url",
    "management_role_source", "management_role_source_url",
    "management_role_verified_on", "management_role_confidence",
    "provenance_class", "limitations"
  )
  if (!identical(
    as.character(PT_LOCAL_REFERENCE_MANAGEMENT_ROLE_SCHEMA$field_name),
    expected_management_fields
  )) {
    stop("Local Reference management-role schema differs from the approved contract.")
  }
  if (!identical(
    as.character(PT_LOCAL_REFERENCE_MANAGEMENT_RECOMMENDATIONS$layer_id),
    PT_LOCAL_REFERENCE_LAYER_IDS
  )) {
    stop("Local Reference management recommendations must cover the exact 11 layers.")
  }
  expected_blm_roles <- c(
    "sole_manager", "co_manager", "administering_partner",
    "local_land_manager", "planning_authority_on_blm_lands",
    "program_administrator", "data_steward_only",
    "no_identified_management_role", "unknown"
  )
  if (!identical(PT_LOCAL_REFERENCE_BLM_ROLE_VALUES, expected_blm_roles)) {
    stop("Local Reference controlled BLM-role vocabulary differs from its contract.")
  }
  if (!identical(names(PT_LOCAL_REFERENCE_BLM_ROLE_LABELS), expected_blm_roles)) {
    stop("Local Reference BLM-role labels differ from the controlled vocabulary.")
  }

  required_category <- c(
    "category_key", "label", "source_values", "fill_color", "stroke_color",
    "fill_opacity", "stroke_weight", "dash_array", "legend_swatch_style",
    "sort_order", "include_when_absent", "provisional"
  )
  color_pattern <- "^(#[0-9A-Fa-f]{6}|transparent)$"
  for (layer_id in PT_LOCAL_REFERENCE_LAYER_IDS) {
    definition <- pt_local_reference_categories(layer_id)
    missing_category <- setdiff(required_category, names(definition))
    if (length(missing_category)) {
      stop(layer_id, " category definition is missing: ", paste(missing_category, collapse = ", "))
    }
    if (!nrow(definition) || anyDuplicated(definition$category_key) ||
        anyDuplicated(definition$sort_order)) {
      stop(layer_id, " category keys and sort orders must be non-empty and unique.")
    }
    if (!"unknown" %in% definition$category_key) {
      stop(layer_id, " category definition must include an explicit unknown category.")
    }
    if (any(!grepl(color_pattern, definition$fill_color)) ||
        any(!grepl(color_pattern, definition$stroke_color))) {
      stop(layer_id, " category definition contains an invalid color token.")
    }
    if (any(!is.finite(definition$fill_opacity)) ||
        any(definition$fill_opacity < 0 | definition$fill_opacity > 1)) {
      stop(layer_id, " fill_opacity must be between zero and one.")
    }
    if (any(!is.finite(definition$stroke_weight)) || any(definition$stroke_weight < 0)) {
      stop(layer_id, " stroke_weight must be finite and non-negative.")
    }
    if (any(!definition$legend_swatch_style %in% c(
      "polygon", "dashed_polygon", "dotted_polygon", "line"
    ))) {
      stop(layer_id, " contains an unsupported legend_swatch_style token.")
    }
    if (identical(layer_id, "federal_wilderness")) {
      known <- definition$category_key %in% c("blm", "usfs", "nps", "fws")
      expected_colors <- stats::setNames(
        pt_local_reference_accepted_agency_color(c("blm", "usfs", "nps", "fws")),
        c("blm", "usfs", "nps", "fws")
      )
      actual_colors <- stats::setNames(
        definition$fill_color[known], definition$category_key[known]
      )
      if (!identical(actual_colors[names(expected_colors)], expected_colors) ||
          any(definition$provisional)) {
        stop("Federal Wilderness must retain its accepted four-agency palette.")
      }
    } else if (identical(layer_id, "national_monuments")) {
      expected_colors <- stats::setNames(
        pt_local_reference_accepted_agency_color(c("blm", "usfs", "nps")),
        c("blm", "usfs", "nps")
      )
      actual_colors <- stats::setNames(
        definition$fill_color, definition$category_key
      )
      if (!identical(actual_colors[names(expected_colors)], expected_colors) ||
          !identical(
            unname(actual_colors[["shared_multi"]]),
            pt_local_reference_accepted_agency_color("blm_usfs_shared")
          ) ||
          any(definition$provisional)) {
        stop(
          "National Monuments must reuse the accepted agency/shared palette."
        )
      }
    } else if (any(!definition$provisional)) {
      stop(layer_id, " palette tokens must remain provisional until BRIM basemap review.")
    }
    expected_order <- unlist(registry$category_sort_order[registry$layer_id == layer_id])
    if (!identical(as.character(definition$category_key), as.character(expected_order))) {
      stop(layer_id, " category_sort_order differs from its category definition.")
    }
  }

  trails <- pt_local_reference_categories("national_scenic_historic_trails")
  known_trails <- trails$category_key != "unknown"
  if (any(trails$dash_array[known_trails] != "") ||
      length(unique(trails$stroke_color[known_trails])) != 6L) {
    stop("Trail contract must retain six fixed identity colors and no initial line patterns.")
  }
  invisible(TRUE)
}

pt_local_reference_resolve_aliases <- function(
  layer_id,
  available_fields,
  require_all = TRUE
) {
  aliases <- PT_LOCAL_REFERENCE_RETAINED_FIELD_ALIASES[[layer_id]]
  if (is.null(aliases)) return(character(0))
  resolved <- vapply(aliases, function(candidates) {
    hit <- candidates[candidates %in% available_fields]
    if (length(hit)) hit[[1]] else NA_character_
  }, character(1))
  if (isTRUE(require_all) && anyNA(resolved)) {
    stop(
      layer_id, " source is missing retained field group(s): ",
      paste(names(resolved)[is.na(resolved)], collapse = ", "),
      ". Available fields: ", paste(available_fields, collapse = ", ")
    )
  }
  resolved
}

pt_local_reference_retained_source_fields <- function(
  source_nickname,
  available_fields,
  require_all = TRUE
) {
  row <- pt_local_reference_config_row(source_nickname = source_nickname)
  if (!isTRUE(row$retention_enabled)) return(character(0))
  unname(pt_local_reference_resolve_aliases(
    row$layer_id,
    available_fields,
    require_all = require_all
  ))
}

pt_local_reference_category_key <- function(layer_id, source_value) {
  definition <- pt_local_reference_categories(layer_id)
  value <- toupper(trimws(pt_local_reference_clean_chr(source_value)))
  resolved <- rep("unknown", length(value))
  for (i in seq_len(nrow(definition))) {
    options <- trimws(unlist(strsplit(definition$source_values[[i]], "\\|")))
    options <- toupper(options[nzchar(options)])
    if (length(options)) resolved[value %in% options] <- definition$category_key[[i]]
  }
  resolved
}

pt_local_reference_apply_category_tokens <- function(x, layer_id, category_key) {
  definition <- pt_local_reference_categories(layer_id)
  idx <- match(category_key, definition$category_key)
  if (anyNA(idx)) stop("Unresolved category key for ", layer_id, ".")
  x$pt_local_reference_category_key <- definition$category_key[idx]
  x$pt_local_reference_category_label <- definition$label[idx]
  x$fill_col <- definition$fill_color[idx]
  x$line_col <- definition$stroke_color[idx]
  x$fill_opacity <- definition$fill_opacity[idx]
  x$line_weight <- definition$stroke_weight[idx]
  x$line_dash <- definition$dash_array[idx]
  x$pt_legend_swatch_style <- definition$legend_swatch_style[idx]
  x
}

pt_local_reference_read_csv <- function(path, required_fields) {
  if (!file.exists(path)) stop("Missing Local Reference sidecar: ", path)
  out <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("", "NA")
  )
  missing_fields <- setdiff(required_fields, names(out))
  if (length(missing_fields)) {
    stop(path, " is missing required field(s): ", paste(missing_fields, collapse = ", "))
  }
  out
}

pt_local_reference_wsa_seed <- function(path = PT_LOCAL_REFERENCE_WSA_SEED_PATH) {
  pt_local_reference_read_csv(path, c(
    "seed_order", "reference_name", "source_name_candidates",
    "date_established_reference", "established_year_month", "reference_acres",
    "designation_subtype_reference", "geographic_scope_note"
  ))
}

pt_local_reference_wsa_curated <- function(path = PT_LOCAL_REFERENCE_WSA_CURATED_PATH) {
  pt_local_reference_read_csv(path, c(
    "reference_name", "managing_office", "managing_office_url",
    "official_page_url", "notable_values", "access_note",
    "curation_source_urls", "curation_verified_date"
  ))
}

pt_local_reference_wsa_alias_table <- function(seed) {
  rows <- lapply(seq_len(nrow(seed)), function(i) {
    aliases <- trimws(unlist(strsplit(seed$source_name_candidates[[i]], "\\|")))
    aliases <- unique(c(seed$reference_name[[i]], aliases[nzchar(aliases)]))
    data.frame(
      seed_row = i,
      reference_name = seed$reference_name[[i]],
      source_alias = aliases,
      normalized_alias = pt_local_reference_normalize_text(aliases),
      stringsAsFactors = FALSE
    )
  })
  aliases <- do.call(rbind, rows)
  aliases <- aliases[nzchar(aliases$normalized_alias), , drop = FALSE]
  duplicates <- unique(aliases$normalized_alias[duplicated(aliases$normalized_alias)])
  if (length(duplicates)) {
    duplicate_rows <- aliases[aliases$normalized_alias %in% duplicates, , drop = FALSE]
    ambiguous <- tapply(duplicate_rows$seed_row, duplicate_rows$normalized_alias, function(x) length(unique(x)) > 1L)
    if (any(ambiguous)) {
      stop("WSA seed aliases resolve to multiple records: ", paste(names(ambiguous)[ambiguous], collapse = ", "))
    }
  }
  aliases[!duplicated(aliases$normalized_alias), , drop = FALSE]
}

pt_local_reference_first_value <- function(df, aliases, fallback = "") {
  out <- rep(fallback, nrow(df))
  for (field in aliases) {
    if (!field %in% names(df)) next
    value <- pt_local_reference_clean_chr(df[[field]])
    fill <- !nzchar(out) & nzchar(value)
    out[fill] <- value[fill]
  }
  out
}

pt_local_reference_format_number <- function(x, digits = 0) {
  value <- suppressWarnings(as.numeric(x))
  ifelse(
    is.na(value),
    "Not stated",
    format(round(value, digits), big.mark = ",", trim = TRUE, nsmall = digits)
  )
}

pt_local_reference_format_square_miles_from_acres <- function(
  acres,
  significant_digits = 3L
) {
  value <- suppressWarnings(as.numeric(acres))
  out <- rep("", length(value))
  valid <- is.finite(value) & value > 0
  if (!any(valid)) return(out)

  square_miles <- value[valid] / 640
  format_significant <- function(x) {
    round_digits <- as.integer(significant_digits) - 1L -
      floor(log10(abs(x)))
    format(
      round(x, round_digits),
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE,
      nsmall = max(0L, round_digits),
      drop0trailing = TRUE
    )
  }
  out[valid] <- paste0(
    "~",
    vapply(square_miles, format_significant, character(1)),
    " mi²"
  )
  out
}

pt_local_reference_format_source_gis_acres <- function(acres) {
  value <- suppressWarnings(as.numeric(acres))
  ifelse(
    is.na(value),
    "Not stated",
    ifelse(
      value == 0,
      "0 acres",
      paste0(pt_local_reference_format_number(value, 1), " acres")
    )
  )
}

pt_local_reference_format_date <- function(x) {
  if (inherits(x, "Date")) {
    return(pt_local_reference_clean_chr(format(x, "%Y-%m-%d")))
  }
  value <- pt_local_reference_clean_chr(x)
  value
}

pt_local_reference_geometry_components <- function(x) {
  if (!inherits(x, "sf")) return(rep(1L, nrow(x)))
  vapply(seq_len(nrow(x)), function(i) {
    geometry <- sf::st_geometry(x[i, , drop = FALSE])
    geometry_type <- as.character(sf::st_geometry_type(geometry, by_geometry = TRUE))
    cast_type <- if (any(grepl("LINESTRING", geometry_type))) {
      "LINESTRING"
    } else if (any(grepl("POLYGON", geometry_type))) {
      "POLYGON"
    } else {
      NA_character_
    }
    count <- tryCatch(
      if (is.na(cast_type)) 1L else
        length(suppressWarnings(sf::st_cast(geometry, cast_type))),
      error = function(e) 1L
    )
    max(1L, as.integer(count))
  }, integer(1))
}

pt_local_reference_geometry_acres <- function(x) {
  if (!inherits(x, "sf")) return(rep(NA_real_, nrow(x)))
  projected <- tryCatch(
    suppressWarnings(sf::st_transform(x, 3310)),
    error = function(e) NULL
  )
  if (is.null(projected)) return(rep(NA_real_, nrow(x)))
  as.numeric(sf::st_area(projected)) / 4046.8564224
}

pt_local_reference_trails_reference <- function(
  path = PT_LOCAL_REFERENCE_TRAILS_REFERENCE_PATH
) {
  pt_local_reference_read_csv(path, c(
    "source_nlcs_id", "source_segment_number", "official_name",
    "common_name", "abbreviation", "designation_class",
    "designation_date", "designation_year", "public_law", "legal_authority",
    "designation_authority", "administering_agency",
    "administering_agency_url", "local_managing_agency",
    "co_managing_agencies", "blm_role", "blm_role_summary",
    "management_evidence_title", "management_evidence_url",
    "management_verified_on", "management_confidence", "trail_status",
    "route_representation", "total_length_miles", "length_qualifier",
    "states_crossed", "trail_summary_short",
    "historic_or_scenic_significance", "indigenous_context",
    "official_page_url", "official_map_url", "management_plan_title",
    "management_plan_url", "management_plan_date", "management_plan_status",
    "gis_download_url", "partner_organization", "partner_url",
    "conditions_url", "permit_url", "closest_final_study_title",
    "closest_final_study_url", "hover_admin_summary", "last_verified",
    "source_notes"
  ))
}

pt_local_reference_trails_aliases <- function(
  path = PT_LOCAL_REFERENCE_TRAILS_ALIASES_PATH
) {
  aliases <- pt_local_reference_read_csv(path, c(
    "source_nlcs_id", "source_name_candidate", "alias_type",
    "evidence_url", "last_verified"
  ))
  if (any(aliases$alias_type == "internal_package_alias")) {
    stop("Trails production aliases must exclude package-internal aliases.")
  }
  aliases
}

pt_local_reference_trails_curated_overrides <- function(
  path = PT_LOCAL_REFERENCE_TRAILS_CURATED_OVERRIDES_PATH
) {
  pt_local_reference_read_csv(path, c(
    "source_nlcs_id", "field_name", "override_value", "reason",
    "evidence_url", "review_status", "last_verified"
  ))
}

pt_local_reference_trails_narrative_provenance <- function(
  path = PT_LOCAL_REFERENCE_TRAILS_NARRATIVE_PROVENANCE_PATH
) {
  pt_local_reference_read_csv(path, c(
    "source_nlcs_id", "narrative_field", "narrative_text",
    "source_register_id", "source_title", "source_agency", "source_url",
    "source_register_url", "source_scope", "source_applicability",
    "text_treatment", "verified_on", "confidence",
    "normal_popup_approved", "package_origin_file", "package_origin_line",
    "package_origin_field", "override_origin_file", "override_origin_line",
    "audit_note"
  ))
}

pt_validate_local_reference_trails_research <- function(
  reference = pt_local_reference_trails_reference(),
  aliases = pt_local_reference_trails_aliases(),
  overrides = pt_local_reference_trails_curated_overrides(),
  narrative = pt_local_reference_trails_narrative_provenance()
) {
  expected_ids <- sprintf("NLCS%06d", 280:285)
  narrative_fields <- c(
    "trail_summary_short", "historic_or_scenic_significance",
    "indigenous_context"
  )
  if (!identical(as.character(reference$source_nlcs_id), expected_ids) ||
      anyDuplicated(reference$source_nlcs_id)) {
    stop("Trails reference must contain the six exact NLCS IDs in canonical order.")
  }
  if (!setequal(unique(aliases$source_nlcs_id), expected_ids) ||
      !setequal(unique(overrides$source_nlcs_id), expected_ids)) {
    stop("Trails aliases and curated overrides must cover all six semantic trails.")
  }
  if (nrow(aliases) != 28L || nrow(overrides) != 24L) {
    stop("Trails compact intake must retain 28 approved aliases and 24 curated overrides.")
  }
  if (any(overrides$review_status != "human_verified")) {
    stop("Every Trails curated override must remain human verified.")
  }
  expected_narrative_keys <- as.vector(outer(
    expected_ids, narrative_fields, paste, sep = "/"
  ))
  narrative_keys <- paste(
    narrative$source_nlcs_id, narrative$narrative_field, sep = "/"
  )
  if (nrow(narrative) != 18L || anyDuplicated(narrative_keys) ||
      !setequal(narrative_keys, expected_narrative_keys)) {
    stop("Trails narrative provenance must contain three unique fields for all six trails.")
  }
  if (any(!narrative$text_treatment %in% c(
    "direct_quote", "attributed_paraphrase", "curated_summary"
  )) || any(!narrative$source_applicability %in% c(
    "direct_trail", "general_context"
  ))) {
    stop("Trails narrative provenance contains an unsupported treatment or applicability.")
  }
  approved <- narrative$normal_popup_approved %in% TRUE
  approved_required <- c(
    "narrative_text", "source_title", "source_agency", "source_url",
    "source_scope", "verified_on", "confidence"
  )
  if (any(!approved) || any(vapply(approved_required, function(field) {
    any(!nzchar(pt_local_reference_clean_chr(narrative[[field]][approved])))
  }, logical(1))) ||
      any(!grepl("^https://", narrative$source_url[approved])) ||
      any(grepl(
        "Interpretation should|Visitor language should|should not be reduced",
        narrative$narrative_text[approved],
        ignore.case = TRUE
      ))) {
    stop("Approved Trails popup narratives must be sourced, non-prescriptive, and directly linked.")
  }
  for (i in seq_len(nrow(overrides))) {
    id_index <- match(overrides$source_nlcs_id[[i]], reference$source_nlcs_id)
    field <- overrides$field_name[[i]]
    expected_value <- if (identical(field, "indigenous_context")) {
      narrative$narrative_text[
        narrative$source_nlcs_id == overrides$source_nlcs_id[[i]] &
          narrative$narrative_field == field & approved
      ]
    } else if (!is.na(id_index) && field %in% names(reference)) {
      reference[[field]][[id_index]]
    } else {
      character(0)
    }
    if (length(expected_value) != 1L ||
        !identical(
          pt_local_reference_clean_chr(expected_value),
          pt_local_reference_clean_chr(overrides$override_value[[i]])
        )) {
      stop(
        "Trails curated override does not reconcile to compact reference: ",
        overrides$source_nlcs_id[[i]], " / ", field
      )
    }
  }
  url_fields <- grep("(_url|_urls)$", names(reference), value = TRUE)
  urls <- unlist(reference[url_fields], use.names = FALSE)
  urls <- pt_local_reference_clean_chr(urls)
  urls <- urls[nzchar(urls)]
  forbidden <- c(
    "congress.*search", "courtlistener", "google.*scholar", "wikipedia",
    "web_search", "nepa.*search"
  )
  if (any(!grepl("^https://", urls)) ||
      any(grepl(paste(forbidden, collapse = "|"), urls, ignore.case = TRUE))) {
    stop("Trails compact reference contains a non-approved or discovery URL.")
  }
  invisible(TRUE)
}

pt_local_reference_trails_hover_html <- function(
  official_name,
  designation_class,
  designation_year,
  admin_summary
) {
  vapply(seq_along(official_name), function(i) {
    values <- c(
      pt_local_reference_clean_chr(official_name[[i]], "Unnamed national trail"),
      paste(
        c(
          pt_local_reference_clean_chr(designation_class[[i]]),
          pt_local_reference_clean_chr(designation_year[[i]])
        )[nzchar(c(
          pt_local_reference_clean_chr(designation_class[[i]]),
          pt_local_reference_clean_chr(designation_year[[i]])
        ))],
        collapse = " · "
      ),
      pt_local_reference_clean_chr(admin_summary[[i]])
    )
    classes <- c(
      "pt-trails-hover-name", "pt-trails-hover-designation",
      "pt-trails-hover-administration"
    )
    rows <- paste0(
      "<div class=\"pt-trails-hover-line ", classes, "\">",
      htmltools::htmlEscape(values),
      "</div>"
    )
    paste0(
      "<div class=\"pt-trails-hover-lines\">",
      paste(rows[nzchar(values)], collapse = ""),
      "</div>"
    )
  }, character(1), USE.NAMES = FALSE)
}

pt_local_reference_trails_link <- function(url, label) {
  url <- pt_local_reference_clean_chr(url)
  label <- pt_local_reference_clean_chr(label)
  if (!nzchar(url) || !nzchar(label)) return("")
  paste0(
    "<a href=\"", htmltools::htmlEscape(url),
    "\" target=\"_blank\" rel=\"noopener noreferrer\">",
    htmltools::htmlEscape(label), "</a>"
  )
}

pt_local_reference_popup_row <- function(label, value) {
  value <- pt_local_reference_clean_chr(value)
  if (!nzchar(value)) return("")
  paste0(
    "<div class=\"pt-lr-popup-row\"><span class=\"pt-lr-popup-label\">",
    htmltools::htmlEscape(label), ":</span> ",
    htmltools::htmlEscape(value), "</div>"
  )
}

pt_local_reference_popup_section <- function(heading, content, class_name = "") {
  content <- content[nzchar(content)]
  if (!length(content)) return("")
  class_token <- if (nzchar(class_name)) paste0(" ", class_name) else ""
  paste0(
    "<section class=\"pt-lr-popup-section", class_token, "\">",
    "<h3>", htmltools::htmlEscape(heading), "</h3>",
    paste(content, collapse = ""),
    "</section>"
  )
}

pt_local_reference_tabbed_popup <- function(
  popup_key,
  title,
  designation_badge,
  tabs,
  popup_class = "",
  tablist_label = "Details"
) {
  tabs <- Filter(function(tab) {
    nzchar(pt_local_reference_clean_chr(tab$html))
  }, tabs)
  safe_key <- gsub("[^A-Za-z0-9_-]+", "-", tolower(popup_key))
  tab_keys <- vapply(tabs, `[[`, character(1), "key")
  if (!length(tabs) || anyDuplicated(tab_keys)) {
    stop("Local Reference tabbed popup requires unique tab keys.")
  }
  tab_buttons <- vapply(seq_along(tabs), function(index) {
    tab <- tabs[[index]]
    tab_id <- paste0("pt-lr-tab-", safe_key, "-", tab$key)
    panel_id <- paste0("pt-lr-panel-", safe_key, "-", tab$key)
    selected <- index == 1L
    paste0(
      "<button type=\"button\" id=\"", tab_id,
      "\" class=\"pt-lr-popup-tab\" role=\"tab\" data-pt-lr-popup-tab=\"",
      htmltools::htmlEscape(tab$key), "\" aria-controls=\"", panel_id,
      "\" aria-selected=\"", tolower(selected), "\" tabindex=\"",
      if (selected) "0" else "-1", "\">",
      htmltools::htmlEscape(tab$label), "</button>"
    )
  }, character(1), USE.NAMES = FALSE)
  tab_panels <- vapply(seq_along(tabs), function(index) {
    tab <- tabs[[index]]
    tab_id <- paste0("pt-lr-tab-", safe_key, "-", tab$key)
    panel_id <- paste0("pt-lr-panel-", safe_key, "-", tab$key)
    paste0(
      "<section id=\"", panel_id,
      "\" class=\"pt-lr-popup-panel\" role=\"tabpanel\" aria-labelledby=\"",
      tab_id, "\" data-pt-lr-popup-panel=\"",
      htmltools::htmlEscape(tab$key), "\"",
      if (index == 1L) "" else " hidden",
      ">", tab$html, "</section>"
    )
  }, character(1), USE.NAMES = FALSE)
  paste0(
    "<article class=\"pt-popup pt-local-reference-popup pt-local-reference-tabbed-popup-card",
    if (nzchar(popup_class)) paste0(" ", htmltools::htmlEscape(popup_class)) else "",
    "\" ",
    "data-pt-lr-tabbed-popup data-pt-lr-popup-key=\"", htmltools::htmlEscape(safe_key), "\">",
    "<div class=\"pt-lr-popup-sticky\">",
    "<header class=\"pt-lr-popup-header\"><div class=\"pt-lr-popup-title\" role=\"heading\" aria-level=\"2\">",
    htmltools::htmlEscape(title), "</div><span class=\"pt-lr-popup-badge\">",
    htmltools::htmlEscape(designation_badge), "</span></header>",
    "<div class=\"pt-lr-popup-tabs\" role=\"tablist\" aria-label=\"",
    htmltools::htmlEscape(tablist_label), "\">",
    paste(tab_buttons, collapse = ""), "</div></div>",
    "<div class=\"pt-lr-popup-panel-scroll\">",
    paste(tab_panels, collapse = ""),
    "</div></article>"
  )
}

pt_local_reference_trails_narrative_section <- function(
  narrative,
  source_nlcs_id,
  narrative_field,
  heading
) {
  row <- narrative[
    narrative$source_nlcs_id == source_nlcs_id &
      narrative$narrative_field == narrative_field &
      narrative$normal_popup_approved %in% TRUE,
    ,
    drop = FALSE
  ]
  if (nrow(row) != 1L ||
      !nzchar(pt_local_reference_clean_chr(row$narrative_text))) {
    return("")
  }
  source_link <- pt_local_reference_trails_link(
    row$source_url,
    paste0(row$source_title, " — ", row$source_agency)
  )
  treatment <- switch(
    row$text_treatment,
    direct_quote = "Direct quotation",
    attributed_paraphrase = "Attributed paraphrase",
    curated_summary = "Curated summary",
    "Source-backed narrative"
  )
  pt_local_reference_popup_section(
    heading,
    c(
      paste0(
        "<p>", htmltools::htmlEscape(row$narrative_text), "</p>"
      ),
      paste0(
        "<div class=\"pt-lr-narrative-source\"><span>",
        htmltools::htmlEscape(treatment), ":</span> ", source_link, "</div>"
      )
    ),
    class_name = "pt-lr-popup-narrative"
  )
}

pt_local_reference_trails_popup <- function(
  df,
  narrative = pt_local_reference_trails_narrative_provenance()
) {
  esc <- function(x, fallback = "Not stated") {
    htmltools::htmlEscape(pt_local_reference_clean_chr(x, fallback))
  }
  present <- function(x) nzchar(pt_local_reference_clean_chr(x))
  vapply(seq_len(nrow(df)), function(i) {
    row <- df[i, , drop = FALSE]
    is_scenic <- identical(
      pt_local_reference_clean_chr(row$pt_trails_designation_class),
      "National Scenic Trail"
    )
    caution <- if (is_scenic) {
      PT_LOCAL_REFERENCE_TRAILS_SCENIC_CAUTION
    } else {
      PT_LOCAL_REFERENCE_TRAILS_HISTORIC_CAUTION
    }
    source_nlcs_id <- pt_local_reference_clean_chr(row$pt_trails_nlcs_id)
    summary_row <- narrative[
      narrative$source_nlcs_id == source_nlcs_id &
        narrative$narrative_field == "trail_summary_short" &
        narrative$normal_popup_approved %in% TRUE,
      ,
      drop = FALSE
    ]
    summary_text <- if (nrow(summary_row) == 1L) {
      pt_local_reference_clean_chr(summary_row$narrative_text)
    } else {
      ""
    }
    length_value <- suppressWarnings(as.numeric(row$pt_trails_total_length_miles))
    overview <- c(
      if (nzchar(summary_text)) paste0(
        "<p class=\"pt-lr-popup-summary\">", esc(summary_text), "</p>"
      ) else "",
      pt_local_reference_popup_row("Common name", row$pt_trails_common_name),
      pt_local_reference_popup_row("Abbreviation", row$pt_trails_abbreviation),
      pt_local_reference_popup_row(
        "Designated",
        paste(
          c(
            pt_local_reference_clean_chr(row$pt_trails_designation_date),
            pt_local_reference_clean_chr(row$pt_trails_public_law)
          )[nzchar(c(
            pt_local_reference_clean_chr(row$pt_trails_designation_date),
            pt_local_reference_clean_chr(row$pt_trails_public_law)
          ))],
          collapse = " · "
        )
      ),
      if (is.finite(length_value)) pt_local_reference_popup_row(
        "Approximate trail-wide length",
        paste0(pt_local_reference_format_number(length_value, 0), " miles")
      ) else "",
      pt_local_reference_popup_row("Length context", row$pt_trails_length_qualifier),
      pt_local_reference_popup_row("States", row$pt_trails_states_crossed),
      pt_local_reference_popup_section(
        "Route representation",
        if (present(row$pt_trails_route_representation)) {
          paste0("<p>", esc(row$pt_trails_route_representation), "</p>")
        } else {
          ""
        }
      ),
      paste0(
        "<div class=\"pt-trails-caution\"><span>Route and access context:</span> ",
        esc(caution), "</div>"
      )
    )
    administering_link <- pt_local_reference_trails_link(
      row$pt_trails_administering_agency_url,
      row$pt_trails_administering_agency
    )
    management <- c(
      if (nzchar(administering_link)) paste0(
        "<div class=\"pt-lr-popup-row\"><span class=\"pt-lr-popup-label\">Trail-wide administering agency:</span> ",
        administering_link, "</div>"
      ) else pt_local_reference_popup_row(
        "Trail-wide administering agency", row$pt_trails_administering_agency
      ),
      pt_local_reference_popup_row(
        "BLM role", PT_LOCAL_REFERENCE_BLM_ROLE_LABELS[[row$pt_trails_blm_role]]
      ),
      if (present(row$pt_trails_blm_role_summary)) paste0(
        "<p>", esc(row$pt_trails_blm_role_summary), "</p>"
      ) else "",
      pt_local_reference_popup_row(
        "Local managing responsibility", row$pt_trails_local_managing_agency
      ),
      pt_local_reference_popup_row(
        "Other management participants", row$pt_trails_co_managing_agencies
      ),
      if (present(row$pt_trails_management_evidence_url)) paste0(
        "<div class=\"pt-lr-popup-evidence\"><span>Management evidence:</span> ",
        pt_local_reference_trails_link(
          row$pt_trails_management_evidence_url,
          row$pt_trails_management_evidence_title
        ), "</div>"
      ) else "",
      pt_local_reference_popup_row(
        "Evidence verification",
        paste(
          c(
            pt_local_reference_clean_chr(row$pt_trails_management_verified_on),
            pt_local_reference_clean_chr(row$pt_trails_management_confidence)
          )[nzchar(c(
            pt_local_reference_clean_chr(row$pt_trails_management_verified_on),
            pt_local_reference_clean_chr(row$pt_trails_management_confidence)
          ))],
          collapse = " · "
        )
      )
    )
    significance_heading <- if (is_scenic) {
      "History and cultural context"
    } else {
      "Historical significance"
    }
    history_context <- c(
      pt_local_reference_trails_narrative_section(
        narrative, source_nlcs_id,
        "historic_or_scenic_significance", significance_heading
      ),
      pt_local_reference_trails_narrative_section(
        narrative, source_nlcs_id,
        "indigenous_context", "Indigenous and Tribal context"
      )
    )
    resource_links <- c(
      pt_local_reference_trails_link(row$pt_trails_official_page_url, "Official trail page"),
      pt_local_reference_trails_link(row$pt_trails_official_map_url, "Official maps"),
      pt_local_reference_trails_link(row$pt_trails_gis_download_url, "Official GIS resource"),
      pt_local_reference_trails_link(row$pt_trails_management_plan_url, paste0(
        row$pt_trails_management_plan_title,
        ifelse(row$pt_trails_management_plan_status == "draft_not_final", " — draft, not final", "")
      )),
      pt_local_reference_trails_link(
        row$pt_trails_closest_final_study_url,
        row$pt_trails_closest_final_study_title
      ),
      pt_local_reference_trails_link(row$pt_trails_conditions_url, "Current conditions"),
      pt_local_reference_trails_link(row$pt_trails_permit_url, "Permit information"),
      pt_local_reference_trails_link(
        row$pt_trails_partner_url,
        if (present(row$pt_trails_partner_organization)) {
          paste0("Partner: ", row$pt_trails_partner_organization)
        } else {
          ""
        }
      )
    )
    resource_links <- resource_links[nzchar(resource_links)]
    resources <- if (length(resource_links)) paste0(
      "<ul class=\"pt-lr-popup-resource-list\"><li>",
      paste(resource_links, collapse = "</li><li>"),
      "</li></ul>"
    ) else ""
    technical <- c(
      pt_local_reference_popup_row("NLCS ID", row$pt_trails_nlcs_id),
      pt_local_reference_popup_row("Raw NLCS_NAME", row$pt_trails_source_name),
      pt_local_reference_popup_row("Source segment number", row$pt_trails_source_segment),
      pt_local_reference_popup_row(
        "GlobalID (geometry/audit only)", row$pt_trails_global_id
      ),
      if (present(row$pt_trails_management_agency)) paste0(
        pt_local_reference_popup_row("Raw MNG_AGCY", row$pt_trails_management_agency),
        "<div class=\"pt-lr-popup-note\">Raw MNG_AGCY is not used to infer management.</div>"
      ) else "",
      pt_local_reference_popup_row(
        "Geometry components", row$pt_local_reference_geometry_components
      ),
      pt_local_reference_popup_row(
        "Designation authority", row$pt_trails_designation_authority
      ),
      if (present(row$pt_trails_source_notes)) pt_local_reference_popup_row(
        "Research note", row$pt_trails_source_notes
      ) else ""
    )
    resources <- c(
      resources,
      paste0(
        "<details class=\"pt-popup-technical\"><summary>Source and verification details</summary>",
        paste(technical[nzchar(technical)], collapse = ""), "</details>"
      )
    )
    pt_local_reference_tabbed_popup(
      popup_key = source_nlcs_id,
      title = row$pt_trails_official_name,
      designation_badge = row$pt_trails_designation_class,
      popup_class = "pt-trails-popup",
      tablist_label = "Trail details",
      tabs = list(
        list(key = "overview", label = "Overview", html = paste(overview, collapse = "")),
        list(key = "management", label = "Management", html = paste(management[nzchar(management)], collapse = "")),
        list(key = "history", label = "History & context", html = paste(history_context[nzchar(history_context)], collapse = "")),
        list(key = "resources", label = "Resources", html = paste(resources[nzchar(resources)], collapse = ""))
      )
    )
  }, character(1), USE.NAMES = FALSE)
}

pt_prepare_local_reference_trails <- function(
  x,
  validate_snapshot = FALSE,
  build_display = TRUE,
  reference_path = PT_LOCAL_REFERENCE_TRAILS_REFERENCE_PATH,
  aliases_path = PT_LOCAL_REFERENCE_TRAILS_ALIASES_PATH,
  overrides_path = PT_LOCAL_REFERENCE_TRAILS_CURATED_OVERRIDES_PATH,
  narrative_path = PT_LOCAL_REFERENCE_TRAILS_NARRATIVE_PROVENANCE_PATH
) {
  if (!inherits(x, "sf") || !nrow(x)) {
    stop("Trails preparation requires a non-empty sf object.")
  }
  resolved <- pt_local_reference_resolve_aliases(
    "national_scenic_historic_trails",
    names(x),
    require_all = TRUE
  )
  get_field <- function(name) x[[resolved[[name]]]]
  x$pt_trails_nlcs_id <- pt_local_reference_clean_chr(get_field("nlcs_id"))
  x$pt_trails_global_id <- pt_local_reference_clean_chr(get_field("global_id"))
  x$pt_trails_source_name <- pt_local_reference_clean_chr(get_field("name"))
  x$pt_trails_source_segment <- pt_local_reference_clean_chr(get_field("source_segment"))
  x$pt_trails_trail_type_raw <- pt_local_reference_clean_chr(get_field("trail_type"))
  x$pt_trails_management_agency <- pt_local_reference_clean_chr(get_field("management_agency"))
  x$pt_trails_admin_state <- pt_local_reference_clean_chr(get_field("admin_state"))
  x$pt_trails_condition_category <- pt_local_reference_clean_chr(get_field("condition_category"))
  x$pt_trails_create_date <- pt_local_reference_format_date(get_field("create_date"))
  x$pt_trails_modify_date <- pt_local_reference_format_date(get_field("modify_date"))

  reference <- pt_local_reference_trails_reference(reference_path)
  aliases <- pt_local_reference_trails_aliases(aliases_path)
  overrides <- pt_local_reference_trails_curated_overrides(overrides_path)
  narrative <- pt_local_reference_trails_narrative_provenance(narrative_path)
  pt_validate_local_reference_trails_research(
    reference, aliases, overrides, narrative
  )
  reference_index <- match(x$pt_trails_nlcs_id, reference$source_nlcs_id)
  if (anyNA(reference_index)) {
    stop(
      "Trails enrichment requires exact NLCS_ID joins; unmatched: ",
      paste(unique(x$pt_trails_nlcs_id[is.na(reference_index)]), collapse = ", ")
    )
  }
  x$pt_trails_join_method <- "exact_nlcs_id"
  fields <- setdiff(names(reference), c("source_nlcs_id", "source_segment_number"))
  for (field in fields) {
    output_field <- paste0("pt_trails_", field)
    x[[output_field]] <- reference[[field]][reference_index]
  }
  for (field in unique(narrative$narrative_field)) {
    approved <- narrative[
      narrative$narrative_field == field &
        narrative$normal_popup_approved %in% TRUE,
      ,
      drop = FALSE
    ]
    approved_index <- match(x$pt_trails_nlcs_id, approved$source_nlcs_id)
    if (anyNA(approved_index)) {
      stop("Trails approved narrative coverage is incomplete for ", field, ".")
    }
    x[[paste0("pt_trails_", field)]] <-
      approved$narrative_text[approved_index]
  }
  alias_search <- vapply(x$pt_trails_nlcs_id, function(id) {
    paste(unique(pt_local_reference_clean_chr(
      aliases$source_name_candidate[aliases$source_nlcs_id == id]
    )), collapse = " | ")
  }, character(1), USE.NAMES = FALSE)
  x$pt_trails_alias_search <- alias_search

  normalized_global <- tolower(gsub("[{}[:space:]]", "", x$pt_trails_global_id))
  x$pt_local_reference_semantic_key <- paste0(
    "trail:nlcs_id:", tolower(x$pt_trails_nlcs_id)
  )
  x$pt_local_reference_feature_key <- x$pt_local_reference_semantic_key
  x$pt_local_reference_geometry_key <- paste0("trail:globalid:", normalized_global)
  if (any(!nzchar(normalized_global)) ||
      anyDuplicated(x$pt_local_reference_semantic_key) ||
      anyDuplicated(x$pt_local_reference_geometry_key)) {
    stop("Trails requires unique NLCS_ID semantic keys and populated unique GlobalID geometry keys.")
  }
  x$pt_local_reference_geometry_components <- pt_local_reference_geometry_components(x)
  category_key <- pt_local_reference_category_key(
    "national_scenic_historic_trails",
    x$pt_trails_nlcs_id
  )
  x <- pt_local_reference_apply_category_tokens(
    x,
    "national_scenic_historic_trails",
    category_key
  )

  if (isTRUE(build_display)) {
    designation_line <- paste(
      x$pt_trails_designation_class,
      x$pt_trails_designation_year,
      sep = " · "
    )
    x$pt_reference_label_text <- x$pt_trails_official_name
    x$pt_reference_hover_text <- paste(
      x$pt_trails_official_name,
      designation_line,
      x$pt_trails_hover_admin_summary,
      sep = "\n"
    )
    x$pt_reference_hover_html <- pt_local_reference_trails_hover_html(
      x$pt_trails_official_name,
      x$pt_trails_designation_class,
      x$pt_trails_designation_year,
      x$pt_trails_hover_admin_summary
    )
    x$popup_html <- pt_local_reference_trails_popup(x, narrative)
  }

  if (isTRUE(validate_snapshot)) {
    expected_ids <- sprintf("NLCS%06d", 280:285)
    if (nrow(x) != 6L ||
        !setequal(x$pt_trails_nlcs_id, expected_ids) ||
        any(x$pt_trails_join_method != "exact_nlcs_id") ||
        sum(x$pt_local_reference_geometry_components) != 177L ||
        sum(x$pt_trails_designation_class == "National Historic Trail") != 5L ||
        sum(x$pt_trails_designation_class == "National Scenic Trail") != 1L ||
        any(x$pt_trails_source_segment != reference$source_segment_number[reference_index]) ||
        any(!sf::st_is_valid(x)) || any(sf::st_is_empty(x))) {
      stop("Trails source snapshot differs from the accepted six-trail/177-component contract.")
    }
    butterfield <- x[x$pt_trails_nlcs_id == "NLCS000285", , drop = FALSE]
    if (nrow(butterfield) != 1L ||
        butterfield$pt_trails_blm_role != "unknown" ||
        !grepl("data stewardship", butterfield$pt_trails_blm_role_summary, fixed = TRUE) ||
        grepl("office", butterfield$pt_trails_hover_admin_summary, ignore.case = TRUE)) {
      stop("Butterfield management must remain unverified and data-steward-context only.")
    }
  }
  x
}

pt_local_reference_trails_reconciliation_qa <- function(x) {
  data.frame(
    nlcs_id = x$pt_trails_nlcs_id,
    raw_brim_source_name = x$pt_trails_source_name,
    source_segment_number = x$pt_trails_source_segment,
    source_globalid = x$pt_trails_global_id,
    geometry_type = as.character(sf::st_geometry_type(x, by_geometry = TRUE)),
    geometry_component_count = x$pt_local_reference_geometry_components,
    package_match = TRUE,
    official_display_name = x$pt_trails_official_name,
    research_readiness = "ready_with_documented_unknowns",
    join_method = x$pt_trails_join_method,
    unresolved_discrepancies = ifelse(
      x$pt_trails_nlcs_id == "NLCS000285",
      "",
      "Package raw_source_name was officialized; authoritative BRIM raw NLCS_NAME preserved."
    ),
    stringsAsFactors = FALSE
  )
}

pt_write_local_reference_trails_qa <- function(
  x,
  output_dir,
  prefix = "local_reference_trails_phase2"
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  reconciliation <- pt_local_reference_trails_reconciliation_qa(x)
  categories <- aggregate(
    cbind(
      semantic_feature_count = rep(1L, nrow(x)),
      geometry_component_count = x$pt_local_reference_geometry_components
    ),
    by = list(
      category_key = x$pt_local_reference_category_key,
      category_label = x$pt_local_reference_category_label
    ),
    FUN = sum
  )
  categories$record_count <- categories$semantic_feature_count
  paths <- c(
    reconciliation = file.path(output_dir, paste0(prefix, "_reconciliation.csv")),
    category_counts = file.path(output_dir, paste0(prefix, "_category_counts.csv"))
  )
  utils::write.csv(reconciliation, paths[["reconciliation"]], row.names = FALSE, na = "")
  utils::write.csv(categories, paths[["category_counts"]], row.names = FALSE, na = "")
  paths
}

pt_local_reference_wsa_compact_office <- function(x) {
  value <- pt_local_reference_clean_chr(x)
  vapply(value, function(office) {
    if (!nzchar(office)) return("")
    parts <- trimws(unlist(strsplit(office, "\\s*/\\s*")))
    mapped <- unname(PT_LOCAL_REFERENCE_WSA_HOVER_OFFICE_ABBREVIATIONS[parts])
    mapped[is.na(mapped) | !nzchar(mapped)] <- parts[is.na(mapped) | !nzchar(mapped)]
    paste(mapped, collapse = " / ")
  }, character(1), USE.NAMES = FALSE)
}

pt_local_reference_wsa_hover_text <- function(
  name,
  flpma_section,
  recommendation_label,
  source_gis_acres,
  managing_office = ""
) {
  name <- pt_local_reference_clean_chr(name, "Unnamed Wilderness Study Area")
  flpma_section <- pt_local_reference_clean_chr(flpma_section)
  recommendation_label <- pt_local_reference_clean_chr(
    recommendation_label,
    "Unknown / not stated"
  )
  area <- pt_local_reference_format_square_miles_from_acres(
    source_gis_acres
  )
  office <- pt_local_reference_wsa_compact_office(managing_office)
  authority <- ifelse(
    nzchar(flpma_section),
    paste0("FLPMA §", flpma_section),
    "FLPMA not stated"
  )
  status <- vapply(seq_along(name), function(i) {
    paste(
      c(authority[[i]], recommendation_label[[i]], area[[i]])[
        nzchar(c(authority[[i]], recommendation_label[[i]], area[[i]]))
      ],
      collapse = " · "
    )
  }, character(1))
  paste0(
    name,
    "\n", status,
    ifelse(nzchar(office), paste0("\nBLM office: ", office), "")
  )
}

pt_local_reference_wsa_hover_html <- function(hover_text) {
  vapply(hover_text, function(value) {
    lines <- strsplit(pt_local_reference_clean_chr(value), "\n", fixed = TRUE)[[1]]
    line_classes <- c(
      "pt-wsa-hover-name",
      "pt-wsa-hover-status",
      "pt-wsa-hover-office"
    )[seq_along(lines)]
    escaped_lines <- htmltools::htmlEscape(lines)
    rows <- paste0(
      "<div class=\"pt-wsa-hover-line ", line_classes, "\">",
      escaped_lines,
      "</div>"
    )
    paste0(
      "<div class=\"pt-wsa-hover-lines\">",
      paste(rows, collapse = ""),
      "</div>"
    )
  }, character(1), USE.NAMES = FALSE)
}

pt_local_reference_wsa_link <- function(url, label) {
  url <- pt_local_reference_clean_chr(url)
  if (!nzchar(url)) return("")
  paste0(
    "<a href=\"", htmltools::htmlEscape(url),
    "\" target=\"_blank\" rel=\"noopener noreferrer\">",
    htmltools::htmlEscape(label), "</a>"
  )
}

pt_local_reference_wsa_popup <- function(df) {
  esc <- function(x, fallback = "Not stated") {
    htmltools::htmlEscape(pt_local_reference_clean_chr(x, fallback))
  }
  present <- function(x) nzchar(pt_local_reference_clean_chr(x))
  link_list <- function(links) {
    links <- links[nzchar(links)]
    if (!length(links)) return("")
    paste0(
      "<ul class=\"pt-lr-popup-resource-list\"><li>",
      paste(links, collapse = "</li><li>"),
      "</li></ul>"
    )
  }
  vapply(seq_len(nrow(df)), function(i) {
    row <- df[i, , drop = FALSE]
    raw_acres <- suppressWarnings(as.numeric(row$pt_wsa_source_gis_acres))
    calculated_acres <- suppressWarnings(as.numeric(row$pt_wsa_calculated_geometry_acres))
    square_miles <- pt_local_reference_format_square_miles_from_acres(raw_acres)
    area_rows <- c(
      pt_local_reference_popup_row(
        "GIS acreage",
        pt_local_reference_format_source_gis_acres(raw_acres)
      )
    )
    if (!is.na(raw_acres) && raw_acres == 0 && !is.na(calculated_acres) && calculated_acres > 0) {
      area_rows <- c(
        area_rows,
        paste0(
          "<div class=\"pt-wsa-source-anomaly\"><b>",
          "Approximate geometry-derived anomaly:</b> ",
          esc(pt_local_reference_format_number(calculated_acres, 1)),
          " acres — source anomaly only; the raw source value above remains ",
          "authoritative.</div>"
        )
      )
    }

    flpma_value <- if (present(row$pt_wsa_flpma_section)) {
      paste0("§", pt_local_reference_clean_chr(row$pt_wsa_flpma_section))
    } else {
      "Not stated"
    }
    overview <- c(
      pt_local_reference_popup_row("FLPMA section", flpma_value),
      if (nzchar(square_miles)) {
        pt_local_reference_popup_row("Approximate mapped area", square_miles)
      } else {
        ""
      },
      pt_local_reference_popup_row(
        "Reference subtype", row$designation_subtype_reference
      ),
      pt_local_reference_popup_row(
        "Reference established date", row$date_established_reference
      ),
      pt_local_reference_popup_row("Geographic scope", row$geographic_scope_note),
      paste0(
        "<div class=\"pt-wsa-status-context\">",
        esc(pt_local_reference_config_row("wilderness_study_areas")$card_caution),
        "</div>"
      )
    )
    raw_recommendation <- if (present(row$pt_wsa_recommendation_raw)) {
      esc(row$pt_wsa_recommendation_raw)
    } else {
      "<i>blank / not stated</i>"
    }
    recommendation_links <- link_list(c(
      pt_local_reference_wsa_link(
        PT_LOCAL_REFERENCE_WSA_DETAIL_URL,
        "BLM WSA detail table"
      ),
      pt_local_reference_wsa_link(
        PT_LOCAL_REFERENCE_WSA_DOCUMENTS_URL,
        "BLM state wilderness documents"
      )
    ))
    recommendation <- c(
      pt_local_reference_popup_row(
        "BLM recommendation", row$pt_local_reference_category_label
      ),
      paste0(
        "<div class=\"pt-lr-popup-row\"><span class=\"pt-lr-popup-label\">",
        "Raw WSA_RCMND source value:</span> ", raw_recommendation, "</div>"
      ),
      pt_local_reference_popup_row(
        "Record of Decision date", row$pt_wsa_rod_date
      ),
      paste0(
        "<div class=\"pt-wsa-caution\"><span>Recommendation context:</span> ",
        esc(PT_LOCAL_REFERENCE_WSA_POPUP_CAUTION), "</div>"
      ),
      recommendation_links
    )
    management_links <- link_list(c(
      if (present(row$managing_office_url)) {
        pt_local_reference_wsa_link(
          row$managing_office_url,
          "Official managing-office page"
        )
      } else {
        ""
      },
      pt_local_reference_wsa_link(
        row$pt_management_role_source_url,
        "Official management-role source"
      )
    ))
    management <- c(
      pt_local_reference_popup_row(
        "Local managing agency", row$pt_management_local_managing_agency
      ),
      pt_local_reference_popup_row(
        "BLM role", row$pt_management_blm_role_label
      ),
      if (present(row$pt_management_blm_role_summary)) {
        paste0("<p>", esc(row$pt_management_blm_role_summary), "</p>")
      } else {
        ""
      },
      pt_local_reference_popup_row("Administrative state", row$pt_wsa_admin_state),
      pt_local_reference_popup_row("Verified responsible office", row$managing_office),
      management_links,
      pt_local_reference_popup_row(
        "Management-role evidence verified",
        paste(
          c(
            pt_local_reference_clean_chr(row$pt_management_role_verified_on),
            pt_local_reference_clean_chr(row$pt_management_role_confidence)
          )[nzchar(c(
            pt_local_reference_clean_chr(row$pt_management_role_verified_on),
            pt_local_reference_clean_chr(row$pt_management_role_confidence)
          ))],
          collapse = " · "
        )
      )
    )
    research <- pt_local_reference_popup_section(
      "Official research notes",
      c(
        pt_local_reference_popup_row("Notable values", row$notable_values),
        pt_local_reference_popup_row("Access note", row$access_note),
        pt_local_reference_popup_row(
          "Curated evidence verified", row$curation_verified_date
        )
      )
    )
    source_links <- link_list(c(
      pt_local_reference_wsa_link(
        PT_LOCAL_REFERENCE_WSA_SOURCE_URL,
        "BLM source feature service"
      ),
      pt_local_reference_wsa_link(
        PT_LOCAL_REFERENCE_WSA_DETAIL_URL,
        "BLM WSA detail table"
      ),
      pt_local_reference_wsa_link(
        PT_LOCAL_REFERENCE_WSA_DOCUMENTS_URL,
        "BLM state wilderness documents"
      ),
      if (present(row$official_page_url)) {
        pt_local_reference_wsa_link(row$official_page_url, "Official unit page")
      } else {
        ""
      }
    ))
    technical_rows <- c(
      pt_local_reference_popup_row(
        "Stable feature key", row$pt_local_reference_feature_key
      ),
      pt_local_reference_popup_row("Case file", row$pt_wsa_casefile),
      pt_local_reference_popup_row("WSA code", row$pt_wsa_code),
      pt_local_reference_popup_row("NLCS ID", row$pt_wsa_nlcs_id),
      if (!present(row$pt_wsa_nlcs_id)) {
        pt_local_reference_popup_row("GlobalID fallback", row$pt_wsa_global_id)
      } else {
        ""
      },
      pt_local_reference_popup_row("SMA ID", row$pt_wsa_sma_id),
      pt_local_reference_popup_row("FAU ID", row$pt_wsa_fau_id),
      pt_local_reference_popup_row("Modify date", row$pt_wsa_modify_date),
      pt_local_reference_popup_row("Seed join", row$pt_wsa_join_status),
      pt_local_reference_popup_row(
        "Geometry components", row$pt_local_reference_geometry_components
      )
    )
    sources_details <- c(
      paste(area_rows[nzchar(area_rows)], collapse = ""),
      research,
      source_links,
      paste0(
        "<details class=\"pt-popup-technical\"><summary>Technical details</summary>",
        paste(technical_rows[nzchar(technical_rows)], collapse = ""),
        "</details>"
      )
    )
    pt_local_reference_tabbed_popup(
      popup_key = row$pt_local_reference_feature_key,
      title = row$pt_wsa_name,
      designation_badge = "Wilderness Study Area",
      popup_class = "pt-wsa-popup",
      tablist_label = "Wilderness Study Area details",
      tabs = list(
        list(
          key = "overview", label = "Overview",
          html = paste(overview[nzchar(overview)], collapse = "")
        ),
        list(
          key = "recommendation", label = "Recommendation",
          html = paste(recommendation[nzchar(recommendation)], collapse = "")
        ),
        list(
          key = "management", label = "Management",
          html = paste(management[nzchar(management)], collapse = "")
        ),
        list(
          key = "sources", label = "Sources & details",
          html = paste(sources_details[nzchar(sources_details)], collapse = "")
        )
      )
    )
  }, character(1))
}

pt_prepare_local_reference_wsa <- function(
  x,
  seed_path = PT_LOCAL_REFERENCE_WSA_SEED_PATH,
  curated_path = PT_LOCAL_REFERENCE_WSA_CURATED_PATH,
  validate_snapshot = FALSE,
  build_display = TRUE
) {
  pt_validate_local_reference_config()
  if (!is.data.frame(x)) stop("WSA preparation requires an sf or data.frame source object.")

  aliases <- PT_LOCAL_REFERENCE_RETAINED_FIELD_ALIASES$wilderness_study_areas
  resolved <- pt_local_reference_resolve_aliases(
    "wilderness_study_areas",
    names(x),
    require_all = TRUE
  )
  source <- if (inherits(x, "sf")) sf::st_drop_geometry(x) else x
  get_field <- function(alias_name) source[[resolved[[alias_name]]]]

  x$pt_wsa_nlcs_id <- pt_local_reference_clean_chr(get_field("nlcs_id"))
  x$pt_wsa_global_id <- pt_local_reference_clean_chr(get_field("global_id"))
  x$pt_wsa_name <- pt_local_reference_clean_chr(get_field("name"), "Unnamed Wilderness Study Area")
  x$pt_wsa_recommendation_raw <- pt_local_reference_clean_chr(get_field("recommendation"))
  x$pt_wsa_casefile <- pt_local_reference_clean_chr(get_field("casefile"))
  x$pt_wsa_admin_state <- pt_local_reference_clean_chr(get_field("admin_state"))
  x$pt_wsa_rod_date <- pt_local_reference_format_date(get_field("record_date"))
  x$pt_wsa_code <- pt_local_reference_clean_chr(get_field("wsa_code"))
  x$pt_wsa_flpma_section <- pt_local_reference_clean_chr(get_field("flpma_section"))
  x$pt_wsa_flpma_section[x$pt_wsa_flpma_section %in% c("0", "0.0")] <- ""
  x$pt_wsa_designation_subtitle <- ifelse(
    nzchar(x$pt_wsa_flpma_section),
    paste0("Wilderness Study Area • FLPMA §", x$pt_wsa_flpma_section),
    "Wilderness Study Area • FLPMA section not stated"
  )
  x$pt_wsa_modify_date <- pt_local_reference_format_date(get_field("modify_date"))
  x$pt_wsa_source_gis_acres <- suppressWarnings(as.numeric(get_field("gis_acres")))
  x$pt_wsa_sma_id <- pt_local_reference_clean_chr(get_field("sma_id"))
  x$pt_wsa_fau_id <- pt_local_reference_clean_chr(get_field("fau_id"))
  x$pt_management_local_managing_agency <- "Bureau of Land Management"
  x$pt_management_blm_role <- "local_land_manager"
  x$pt_management_blm_role_label <- unname(
    PT_LOCAL_REFERENCE_BLM_ROLE_LABELS[x$pt_management_blm_role]
  )
  x$pt_management_blm_role_summary <- paste(
    "BLM manages Wilderness Study Areas to preserve wilderness suitability",
    "pending Congressional action."
  )
  x$pt_management_role_source <- "BLM California National Conservation Lands"
  x$pt_management_role_source_url <- PT_LOCAL_REFERENCE_WSA_CALIFORNIA_URL
  x$pt_management_role_verified_on <- PT_LOCAL_REFERENCE_WSA_MANAGEMENT_VERIFIED_ON
  x$pt_management_role_confidence <- "verified_layer_family"

  global_key <- tolower(gsub("[{}[:space:]]", "", x$pt_wsa_global_id))
  x$pt_local_reference_feature_key <- ifelse(
    nzchar(x$pt_wsa_nlcs_id),
    paste0("wsa:nlcs_id:", tolower(x$pt_wsa_nlcs_id)),
    ifelse(nzchar(global_key), paste0("wsa:globalid:", global_key), "")
  )
  if (any(!nzchar(x$pt_local_reference_feature_key))) {
    stop("Every WSA source feature requires NLCS_ID or GlobalID for a stable key.")
  }
  if (anyDuplicated(x$pt_local_reference_feature_key)) {
    stop("WSA stable feature keys must be unique at the source-feature level.")
  }
  x$pt_local_reference_semantic_key <- x$pt_local_reference_feature_key
  x$pt_local_reference_geometry_key <- paste0(
    x$pt_local_reference_feature_key,
    ":geometry:1"
  )
  x$pt_local_reference_geometry_components <- pt_local_reference_geometry_components(x)
  x$pt_wsa_calculated_geometry_acres <- pt_local_reference_geometry_acres(x)

  seed <- pt_local_reference_wsa_seed(seed_path)
  seed_aliases <- pt_local_reference_wsa_alias_table(seed)
  normalized_source <- pt_local_reference_normalize_text(x$pt_wsa_name)
  manual_source <- pt_local_reference_normalize_text(PT_LOCAL_REFERENCE_WSA_MANUAL_REVIEW$source_name)
  source_only <- pt_local_reference_normalize_text(PT_LOCAL_REFERENCE_WSA_SOURCE_ONLY)
  matched_alias <- match(normalized_source, seed_aliases$normalized_alias)
  matched_seed <- seed_aliases$seed_row[matched_alias]
  x$pt_wsa_join_status <- ifelse(
    normalized_source %in% manual_source,
    "unmatched_manual_review",
    ifelse(
      normalized_source %in% source_only,
      "source_only",
      ifelse(!is.na(matched_seed), "matched_exact_alias", "unexpected_unmatched")
    )
  )
  accepted <- x$pt_wsa_join_status == "matched_exact_alias"
  accepted_seed <- ifelse(accepted, matched_seed, NA_integer_)
  if (anyDuplicated(accepted_seed[accepted])) {
    stop("WSA exact aliases must map source records one-to-one with seed records.")
  }
  x$pt_wsa_seed_reference_name <- ifelse(
    accepted,
    seed$reference_name[accepted_seed],
    ""
  )

  seed_fields <- setdiff(names(seed), c("seed_order", "source_name_candidates"))
  for (field in seed_fields) {
    value <- rep(NA, nrow(x))
    value[accepted] <- seed[[field]][accepted_seed[accepted]]
    x[[field]] <- value
  }

  curated <- pt_local_reference_wsa_curated(curated_path)
  curated_index <- match(x$pt_wsa_seed_reference_name, curated$reference_name)
  curated_fields <- setdiff(names(curated), "reference_name")
  for (field in curated_fields) {
    x[[field]] <- curated[[field]][curated_index]
  }

  category_key <- pt_local_reference_category_key(
    "wilderness_study_areas",
    x$pt_wsa_recommendation_raw
  )
  x <- pt_local_reference_apply_category_tokens(
    x,
    "wilderness_study_areas",
    category_key
  )
  if (isTRUE(build_display)) {
    x$pt_wsa_hover_office_label <- pt_local_reference_wsa_compact_office(
      x$managing_office
    )
    x$pt_reference_label_text <- x$pt_wsa_name
    x$pt_reference_hover_text <- pt_local_reference_wsa_hover_text(
      name = x$pt_wsa_name,
      flpma_section = x$pt_wsa_flpma_section,
      recommendation_label = x$pt_local_reference_category_label,
      source_gis_acres = x$pt_wsa_source_gis_acres,
      managing_office = x$managing_office
    )
    x$pt_reference_hover_html <- pt_local_reference_wsa_hover_html(
      x$pt_reference_hover_text
    )
    x$popup_html <- pt_local_reference_wsa_popup(x)
  }

  if (isTRUE(validate_snapshot)) {
    id_count <- sum(nzchar(x$pt_wsa_nlcs_id))
    global_fallback_count <- sum(!nzchar(x$pt_wsa_nlcs_id) & nzchar(x$pt_wsa_global_id))
    expected_status <- c(
      matched_exact_alias = 59L,
      unmatched_manual_review = 2L,
      source_only = 2L
    )
    actual_status <- table(factor(x$pt_wsa_join_status, levels = names(expected_status)))
    expected_category <- c(
      suitable = 4L,
      non_suitable = 46L,
      no_recommendation = 11L,
      unknown = 2L
    )
    actual_category <- table(factor(
      x$pt_local_reference_category_key,
      levels = names(expected_category)
    ))
    joins <- pt_local_reference_wsa_join_qa(x, seed_path = seed_path)
    expected_seed_status <- c(
      matched_exact_alias = 59L,
      unmatched_manual_review = 2L,
      seed_only = 2L
    )
    actual_seed_status <- table(factor(
      joins$seed$seed_status,
      levels = names(expected_seed_status)
    ))
    if (nrow(x) != 63L || id_count != 61L || global_fallback_count != 2L ||
        !identical(as.integer(actual_status), unname(expected_status)) ||
        !identical(as.integer(actual_category), unname(expected_category)) ||
        !identical(as.integer(actual_seed_status), unname(expected_seed_status)) ||
        sum(x$pt_local_reference_geometry_components) != 104L ||
        any(x$pt_wsa_join_status == "unexpected_unmatched")) {
      stop(
        "WSA snapshot contract failed: rows=", nrow(x),
        ", NLCS IDs=", id_count,
        ", GlobalID fallbacks=", global_fallback_count,
        ", joins=", paste(names(expected_status), as.integer(actual_status), collapse = "; "),
        ", categories=", paste(names(expected_category), as.integer(actual_category), collapse = "; "),
        ", geometry components=", sum(x$pt_local_reference_geometry_components)
      )
    }
    red <- x[pt_local_reference_normalize_text(x$pt_wsa_name) == "red mountain", , drop = FALSE]
    if (nrow(red) != 1L || is.na(red$pt_wsa_source_gis_acres) || red$pt_wsa_source_gis_acres != 0) {
      stop("Red Mountain must retain raw source GIS_ACRES=0.")
    }
  }
  x
}

pt_local_reference_wsa_join_qa <- function(x, seed_path = PT_LOCAL_REFERENCE_WSA_SEED_PATH) {
  required <- c(
    "pt_wsa_name", "pt_wsa_seed_reference_name", "pt_wsa_join_status",
    "pt_local_reference_feature_key", "pt_local_reference_geometry_key",
    "pt_wsa_nlcs_id", "pt_wsa_global_id",
    "pt_local_reference_category_key", "pt_local_reference_geometry_components",
    "pt_wsa_source_gis_acres", "pt_wsa_calculated_geometry_acres",
    "pt_wsa_recommendation_raw"
  )
  missing_fields <- setdiff(required, names(x))
  if (length(missing_fields)) {
    stop("Prepared WSA data is missing QA fields: ", paste(missing_fields, collapse = ", "))
  }
  source_qa <- data.frame(
    source_name = x$pt_wsa_name,
    stable_feature_key = x$pt_local_reference_feature_key,
    stable_geometry_key = x$pt_local_reference_geometry_key,
    key_basis = ifelse(nzchar(x$pt_wsa_nlcs_id), "NLCS_ID", "GlobalID"),
    seed_reference_name = x$pt_wsa_seed_reference_name,
    join_status = x$pt_wsa_join_status,
    local_managing_agency = x$pt_management_local_managing_agency,
    blm_role = x$pt_management_blm_role,
    management_role_confidence = x$pt_management_role_confidence,
    managing_office = x$managing_office,
    official_page_url = x$official_page_url,
    curation_verified_date = x$curation_verified_date,
    raw_wsa_rcmnd = x$pt_wsa_recommendation_raw,
    category_key = x$pt_local_reference_category_key,
    geometry_components = x$pt_local_reference_geometry_components,
    raw_source_gis_acres = x$pt_wsa_source_gis_acres,
    calculated_geometry_acres = x$pt_wsa_calculated_geometry_acres,
    stringsAsFactors = FALSE
  )

  seed <- pt_local_reference_wsa_seed(seed_path)
  matched_refs <- unique(x$pt_wsa_seed_reference_name[nzchar(x$pt_wsa_seed_reference_name)])
  manual_refs <- PT_LOCAL_REFERENCE_WSA_MANUAL_REVIEW$seed_reference_name
  seed_status <- ifelse(
    seed$reference_name %in% matched_refs,
    "matched_exact_alias",
    ifelse(
      seed$reference_name %in% manual_refs,
      "unmatched_manual_review",
      ifelse(seed$reference_name %in% PT_LOCAL_REFERENCE_WSA_SEED_ONLY, "seed_only", "unexpected_unmatched")
    )
  )
  seed_qa <- data.frame(
    seed_order = seed$seed_order,
    seed_reference_name = seed$reference_name,
    seed_status = seed_status,
    stringsAsFactors = FALSE
  )
  list(source = source_qa, seed = seed_qa)
}

pt_local_reference_category_qa <- function(x, layer_id = "wilderness_study_areas") {
  definition <- pt_local_reference_categories(layer_id)
  category_key <- factor(
    x$pt_local_reference_category_key,
    levels = definition$category_key
  )
  data.frame(
    layer_id = layer_id,
    category_key = definition$category_key,
    category_label = definition$label,
    total_category_records = as.integer(table(category_key)),
    semantic_feature_count = vapply(definition$category_key, function(key) {
      length(unique(x$pt_local_reference_semantic_key[x$pt_local_reference_category_key == key]))
    }, integer(1)),
    geometry_component_count = vapply(definition$category_key, function(key) {
      sum(x$pt_local_reference_geometry_components[x$pt_local_reference_category_key == key])
    }, integer(1)),
    stringsAsFactors = FALSE
  )
}

pt_write_local_reference_wsa_qa <- function(x, output_dir, prefix = "local_reference_wsa_phase1") {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  joins <- pt_local_reference_wsa_join_qa(x)
  category <- pt_local_reference_category_qa(x)
  paths <- c(
    source_join = file.path(output_dir, paste0(prefix, "_source_join.csv")),
    seed_coverage = file.path(output_dir, paste0(prefix, "_seed_coverage.csv")),
    category_counts = file.path(output_dir, paste0(prefix, "_category_counts.csv"))
  )
  utils::write.csv(joins$source, paths[["source_join"]], row.names = FALSE, na = "")
  utils::write.csv(joins$seed, paths[["seed_coverage"]], row.names = FALSE, na = "")
  utils::write.csv(category, paths[["category_counts"]], row.names = FALSE, na = "")
  paths
}

pt_local_reference_fw_components <- function(
  path = PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_COMPONENTS_PATH
) {
  pt_local_reference_read_csv(path, c(
    "source_layer", "component_id", "wilderness_id", "source_globalid", "source_semantic_id",
    "source_nlcs_id", "source_name", "standardized_name",
    "managing_agency_code", "managing_agency", "co_managing_agencies",
    "local_managing_unit", "local_unit_url", "blm_role", "blm_role_summary",
    "geographic_state", "source_gis_acres", "calculated_acres",
    "component_description", "geometry_caveat",
    "source_designation_date", "source_public_law_code", "last_verified"
  ))
}

pt_local_reference_fw_reference <- function(
  path = PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_REFERENCE_PATH
) {
  pt_local_reference_read_csv(path, c(
    "wilderness_id", "source_semantic_id", "official_name", "alternate_names",
    "wilderness_abbreviation",
    "states", "designation_date", "designation_year", "original_public_law",
    "subsequent_public_laws", "legal_authority", "official_reference_acres",
    "source_component_count", "managing_agencies", "shared_management",
    "wilderness_summary_short", "management_access_summary",
    "official_page_url", "direct_official_agency_page_url", "official_map_url",
    "primary_management_plan_title", "primary_management_plan_url",
    "local_unit_pages", "wilderness_connect_url", "congress_search_url",
    "nepa_search_url", "courtlistener_search_url",
    "google_scholar_case_search_url", "web_search_url", "acreage_source",
    "last_verified"
  ))
}

pt_local_reference_fw_designations <- function(
  path = PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_DESIGNATION_VALIDATION_PATH
) {
  pt_local_reference_read_csv(path, c(
    "wilderness_id", "official_wilderness_name",
    "resolved_original_designation_date", "resolved_designation_year",
    "original_public_law", "original_law_enactment_date",
    "subsequent_public_laws", "subsequent_law_count",
    "evidence_source", "evidence_url", "validation_status",
    "explanatory_note", "prior_reference_designation_date",
    "prior_reference_designation_year", "prior_reference_original_public_law",
    "prior_reference_subsequent_public_laws"
  ))
}

pt_local_reference_fw_documents <- function(
  path = PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_DOCUMENTS_PATH
) {
  pt_local_reference_read_csv(path, c(
    "document_id", "wilderness_id", "component_id", "document_title",
    "document_type", "agency", "publication_date", "geographic_scope",
    "document_url", "landing_page_url", "authority_level",
    "wilderness_wide_or_component_specific", "last_verified", "notes"
  ))
}

pt_local_reference_fw_common_policy <- function(
  path = PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_COMMON_POLICY_PATH
) {
  pt_local_reference_read_csv(path, c(
    "policy_id", "topic", "recommended_language", "use", "source_basis"
  ))
}

pt_local_reference_fw_source_register <- function(
  path = PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_SOURCE_REGISTER_PATH
) {
  pt_local_reference_read_csv(path, c(
    "source_id", "title", "agency", "url", "source_type", "use",
    "verification_date", "limitations"
  ))
}

pt_local_reference_fw_law_token <- function(x) {
  value <- toupper(pt_local_reference_clean_chr(x))
  match_value <- regmatches(
    value,
    regexpr("PUBLIC LAW[[:space:]]+[0-9]+-[0-9]+", value)
  )
  gsub("[[:space:]]+", " ", match_value)
}

pt_validate_local_reference_fw_research <- function(
  components = pt_local_reference_fw_components(),
  reference = pt_local_reference_fw_reference(),
  designations = pt_local_reference_fw_designations(),
  documents = pt_local_reference_fw_documents(),
  policy = pt_local_reference_fw_common_policy(),
  sources = pt_local_reference_fw_source_register()
) {
  if (nrow(components) != 197L || anyDuplicated(components$component_id) ||
      length(unique(components$wilderness_id)) != 158L) {
    stop("Federal Wilderness component crosswalk must retain 197 unique components and 158 wildernesses.")
  }
  if (nrow(reference) != 158L || anyDuplicated(reference$wilderness_id) ||
      !setequal(unique(components$wilderness_id), reference$wilderness_id)) {
    stop("Federal Wilderness semantic reference must be a complete one-row-per-wilderness lookup.")
  }
  designation_dates <- pt_local_reference_clean_chr(
    designations$resolved_original_designation_date
  )
  designation_years <- suppressWarnings(as.integer(
    designations$resolved_designation_year
  ))
  allowed_status <- c("PASS", "PASS WITH DOCUMENTED EXPLANATION")
  if (nrow(designations) != 158L || anyDuplicated(designations$wilderness_id) ||
      !setequal(reference$wilderness_id, designations$wilderness_id) ||
      any(!nzchar(designation_dates)) ||
      any(!nzchar(pt_local_reference_clean_chr(designations$original_public_law))) ||
      any(designation_years != suppressWarnings(as.integer(substr(designation_dates, 1, 4)))) ||
      any(!designations$validation_status %in% allowed_status) ||
      sum(designations$validation_status == "PASS") != 154L ||
      sum(designations$validation_status == "PASS WITH DOCUMENTED EXPLANATION") != 4L) {
    stop("Federal Wilderness designation validation must retain 158 resolved records, 154 PASS and four documented explanations.")
  }
  expected_agencies <- c(BLM = 105L, FWS = 2L, NPS = 15L, USFS = 75L)
  actual_agencies <- table(factor(components$managing_agency, levels = names(expected_agencies)))
  if (!identical(as.integer(actual_agencies), unname(expected_agencies)) ||
      sum(components$geographic_state == "CA", na.rm = TRUE) != 194L ||
      sum(components$geographic_state == "NV", na.rm = TRUE) != 3L) {
    stop("Federal Wilderness agency or geographic component counts differ from the accepted snapshot.")
  }
  shared <- tolower(pt_local_reference_clean_chr(reference$shared_management)) == "true"
  if (sum(shared) != 14L || sum(!shared) != 144L) {
    stop("Federal Wilderness management-pattern counts must remain 144 single and 14 shared.")
  }
  has_later <- nzchar(pt_local_reference_clean_chr(designations$subsequent_public_laws))
  if (sum(has_later) != 22L || sum(!has_later) != 136L) {
    stop("Federal Wilderness resolved designation-history counts must remain 136 original-only and 22 with later law(s).")
  }
  if (nrow(documents) != 360L || anyDuplicated(documents$document_id) ||
      sum(documents$document_type == "public law", na.rm = TRUE) != 176L ||
      !setequal(setdiff(unique(documents$wilderness_id), "ALL"), reference$wilderness_id)) {
    stop("Federal Wilderness documents must retain the accepted 360-row keyed lookup.")
  }
  public_laws <- documents[documents$document_type == "public law", , drop = FALSE]
  public_laws$law_token <- pt_local_reference_fw_law_token(public_laws$document_title)
  expected_laws <- lapply(seq_len(nrow(designations)), function(i) {
    later <- trimws(unlist(strsplit(
      pt_local_reference_clean_chr(designations$subsequent_public_laws[[i]]),
      ";",
      fixed = TRUE
    )))
    unique(pt_local_reference_fw_law_token(c(
      designations$original_public_law[[i]], later[nzchar(later)]
    )))
  })
  laws_ok <- vapply(seq_len(nrow(designations)), function(i) {
    actual <- unique(public_laws$law_token[
      public_laws$wilderness_id == designations$wilderness_id[[i]]
    ])
    all(actual[nzchar(actual)] %in% expected_laws[[i]][nzchar(expected_laws[[i]])])
  }, logical(1))
  if (!all(laws_ok)) {
    stop(
      "Federal Wilderness public-law lookup contradicts resolved designation records for: ",
      paste(designations$wilderness_id[!laws_ok], collapse = ", ")
    )
  }
  if (nrow(policy) != 10L || anyDuplicated(policy$policy_id)) {
    stop("Federal Wilderness common-policy lookup must retain ten unique rows.")
  }
  required_source_titles <- c(
    "BLM CA Federal Wilderness Areas Polygon FeatureServer",
    "National Wilderness Preservation System Law Library"
  )
  if (!all(required_source_titles %in% sources$title)) {
    stop("Federal Wilderness source register is missing its GIS or Wilderness Act authority.")
  }
  invisible(TRUE)
}

pt_local_reference_fw_hover_html <- function(df) {
  vapply(seq_len(nrow(df)), function(i) {
    row <- df[i, , drop = FALSE]
    named_area <- pt_local_reference_format_square_miles_from_acres(
      row$pt_fw_official_reference_acres
    )
    named_area <- sub("^~", "", named_area)
    cues <- c(
      if (identical(row$pt_fw_management_pattern, "shared_multi_agency")) {
        paste0("Shared: ", row$pt_fw_managing_agencies)
      } else {
        row$pt_fw_agency_name
      },
      paste0("Designated ", row$pt_fw_designation_year),
      if (nzchar(named_area)) paste0("Approx. area: ", named_area) else "",
      if (identical(row$pt_fw_geographic_context, "western_nevada_context")) {
        "Western Nevada context"
      } else {
        ""
      }
    )
    cues <- cues[nzchar(cues)]
    paste0(
      "<div class=\"pt-fw-hover-lines\">",
      "<div class=\"pt-fw-hover-line pt-fw-hover-title\">",
      htmltools::htmlEscape(row$pt_fw_official_name), "</div>",
      paste0(
        "<div class=\"pt-fw-hover-line\">",
        htmltools::htmlEscape(cues), "</div>",
        collapse = ""
      ),
      "</div>"
    )
  }, character(1), USE.NAMES = FALSE)
}

pt_local_reference_fw_runtime_geometry <- function(x) {
  sf_column <- attr(x, "sf_column")
  if (is.null(sf_column) || !length(sf_column) || !sf_column %in% names(x)) {
    sf_column <- names(x)[vapply(x, inherits, logical(1), what = "sfc")][[1]]
  }
  keep <- c(
    "pt_nickname", "pt_display_name", "pt_geom_type",
    "component_id", "wilderness_id",
    "pt_local_reference_feature_key", "pt_local_reference_semantic_key",
    "pt_local_reference_geometry_key", "pt_local_reference_geometry_components",
    "pt_local_reference_category_key", "pt_local_reference_category_label",
    "fill_col", "line_col", "fill_opacity", "line_weight", "line_dash",
    "pt_legend_swatch_style",
    "pt_fw_management_pattern", "pt_fw_designation_history",
    "pt_fw_geographic_context",
    "pt_reference_label_text", "pt_reference_hover_html",
    "pt_reference_hover_text", sf_column
  )
  missing <- setdiff(keep, names(x))
  if (length(missing)) {
    stop("Federal Wilderness runtime geometry is missing: ", paste(missing, collapse = ", "))
  }
  x[, keep, drop = FALSE]
}

pt_prepare_local_reference_federal_wilderness <- function(
  x,
  validate_snapshot = FALSE,
  build_display = TRUE,
  components_path = PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_COMPONENTS_PATH,
  reference_path = PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_REFERENCE_PATH,
  designations_path = PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_DESIGNATION_VALIDATION_PATH,
  documents_path = PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_DOCUMENTS_PATH,
  policy_path = PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_COMMON_POLICY_PATH,
  sources_path = PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_SOURCE_REGISTER_PATH
) {
  pt_validate_local_reference_config()
  if (!inherits(x, "sf") || !nrow(x)) {
    stop("Federal Wilderness preparation requires a non-empty sf object.")
  }
  resolved <- pt_local_reference_resolve_aliases(
    "federal_wilderness", names(x), require_all = TRUE
  )
  get_field <- function(name) x[[resolved[[name]]]]
  components <- pt_local_reference_fw_components(components_path)
  reference <- pt_local_reference_fw_reference(reference_path)
  designations <- pt_local_reference_fw_designations(designations_path)
  documents <- pt_local_reference_fw_documents(documents_path)
  policy <- pt_local_reference_fw_common_policy(policy_path)
  sources <- pt_local_reference_fw_source_register(sources_path)
  pt_validate_local_reference_fw_research(
    components, reference, designations, documents, policy, sources
  )

  source_component_id <- tolower(gsub(
    "[{}[:space:]]", "", pt_local_reference_clean_chr(get_field("global_id"))
  ))
  source_component_id <- ifelse(
    startsWith(source_component_id, "blmca-"),
    source_component_id,
    paste0("blmca-", source_component_id)
  )
  source_wilderness_id <- paste0("fw-", pt_local_reference_clean_chr(get_field("fau_id")))
  if ("component_id" %in% names(x) &&
      !identical(as.character(x$component_id), source_component_id)) {
    stop("Federal Wilderness component_id differs from normalized GlobalID.")
  }
  if ("wilderness_id" %in% names(x) &&
      !identical(as.character(x$wilderness_id), source_wilderness_id)) {
    stop("Federal Wilderness wilderness_id differs from FAU_ID.")
  }
  x$component_id <- source_component_id
  x$wilderness_id <- source_wilderness_id
  component_index <- match(x$component_id, components$component_id)
  reference_index <- match(x$wilderness_id, reference$wilderness_id)
  designation_index <- match(x$wilderness_id, designations$wilderness_id)
  if (anyNA(component_index) || anyNA(reference_index) || anyNA(designation_index)) {
    stop("Federal Wilderness geometry IDs are not fully covered by accepted lookups.")
  }
  if (any(components$wilderness_id[component_index] != x$wilderness_id)) {
    stop("Federal Wilderness component-to-semantic lookup relationship changed.")
  }

  x$pt_fw_global_id <- pt_local_reference_clean_chr(get_field("global_id"))
  x$pt_fw_fau_id <- pt_local_reference_clean_chr(get_field("fau_id"))
  x$pt_fw_nlcs_id <- pt_local_reference_clean_chr(get_field("nlcs_id"))
  x$pt_fw_source_name <- pt_local_reference_clean_chr(get_field("name"))
  x$pt_fw_agency_code <- pt_local_reference_clean_chr(get_field("agency_code"))
  x$pt_fw_agency_name <- components$managing_agency[component_index]
  x$pt_fw_admin_state <- components$geographic_state[component_index]
  x$pt_fw_source_gis_acres <- suppressWarnings(as.numeric(get_field("gis_acres")))
  x$pt_fw_source_modify_date <- pt_local_reference_format_date(get_field("modify_date"))
  x$pt_fw_official_name <- reference$official_name[reference_index]
  x$pt_fw_alternate_names <- pt_local_reference_clean_chr(
    reference$alternate_names[reference_index]
  )
  x$pt_fw_wilderness_abbreviation <- pt_local_reference_clean_chr(
    reference$wilderness_abbreviation[reference_index]
  )
  x$pt_fw_states <- reference$states[reference_index]
  x$pt_fw_designation_date <- designations$resolved_original_designation_date[designation_index]
  x$pt_fw_designation_year <- designations$resolved_designation_year[designation_index]
  x$pt_fw_original_public_law <- designations$original_public_law[designation_index]
  x$pt_fw_subsequent_public_laws <- pt_local_reference_clean_chr(
    designations$subsequent_public_laws[designation_index]
  )
  x$pt_fw_designation_validation_status <- designations$validation_status[designation_index]
  x$pt_fw_designation_explanatory_note <- pt_local_reference_clean_chr(
    designations$explanatory_note[designation_index]
  )
  x$pt_fw_designation_evidence_url <- designations$evidence_url[designation_index]
  x$pt_fw_official_reference_acres <- suppressWarnings(as.numeric(
    reference$official_reference_acres[reference_index]
  ))
  x$pt_fw_acreage_source <- pt_local_reference_clean_chr(
    reference$acreage_source[reference_index]
  )
  x$pt_fw_source_component_count <- as.integer(reference$source_component_count[reference_index])
  x$pt_fw_managing_agencies <- reference$managing_agencies[reference_index]
  x$pt_fw_summary_short <- pt_local_reference_clean_chr(
    reference$wilderness_summary_short[reference_index]
  )
  x$pt_fw_management_access_summary <- pt_local_reference_clean_chr(
    reference$management_access_summary[reference_index]
  )
  x$pt_fw_local_managing_unit <- pt_local_reference_clean_chr(
    components$local_managing_unit[component_index]
  )
  x$pt_fw_local_unit_url <- pt_local_reference_clean_chr(
    components$local_unit_url[component_index]
  )
  x$pt_fw_blm_office <- pt_local_reference_clean_chr(
    components$blm_office[component_index]
  )
  x$pt_fw_source_layer <- pt_local_reference_clean_chr(
    components$source_layer[component_index]
  )
  x$pt_fw_geometry_caveat <- pt_local_reference_clean_chr(
    components$geometry_caveat[component_index]
  )
  x$pt_fw_component_description <- pt_local_reference_clean_chr(
    components$component_description[component_index]
  )
  x$pt_fw_direct_official_agency_page_url <- pt_local_reference_clean_chr(
    reference$direct_official_agency_page_url[reference_index]
  )
  x$pt_fw_official_page_url <- pt_local_reference_clean_chr(
    reference$official_page_url[reference_index]
  )
  x$pt_fw_official_map_url <- pt_local_reference_clean_chr(
    reference$official_map_url[reference_index]
  )
  x$pt_fw_primary_management_plan_title <- pt_local_reference_clean_chr(
    reference$primary_management_plan_title[reference_index]
  )
  x$pt_fw_primary_management_plan_url <- pt_local_reference_clean_chr(
    reference$primary_management_plan_url[reference_index]
  )
  x$pt_fw_wilderness_connect_url <- pt_local_reference_clean_chr(
    reference$wilderness_connect_url[reference_index]
  )
  x$pt_fw_congress_search_url <- pt_local_reference_clean_chr(
    reference$congress_search_url[reference_index]
  )
  x$pt_fw_nepa_search_url <- pt_local_reference_clean_chr(
    reference$nepa_search_url[reference_index]
  )
  x$pt_fw_courtlistener_search_url <- pt_local_reference_clean_chr(
    reference$courtlistener_search_url[reference_index]
  )
  x$pt_fw_google_scholar_case_search_url <- pt_local_reference_clean_chr(
    reference$google_scholar_case_search_url[reference_index]
  )
  x$pt_fw_web_search_url <- pt_local_reference_clean_chr(
    reference$web_search_url[reference_index]
  )
  x$pt_fw_last_verified <- pmax(
    pt_local_reference_clean_chr(components$last_verified[component_index]),
    pt_local_reference_clean_chr(reference$last_verified[reference_index])
  )
  x$pt_fw_management_pattern <- ifelse(
    tolower(pt_local_reference_clean_chr(reference$shared_management[reference_index])) == "true",
    "shared_multi_agency",
    "single_agency"
  )
  x$pt_fw_designation_history <- ifelse(
    nzchar(x$pt_fw_subsequent_public_laws),
    "has_subsequent_law",
    "original_only"
  )
  x$pt_fw_geographic_context <- ifelse(
    x$pt_fw_admin_state == "NV",
    "western_nevada_context",
    "california"
  )
  x$pt_fw_state_label <- ifelse(x$pt_fw_admin_state == "NV", "Nevada", "California")
  x$pt_fw_management_jurisdiction <- ifelse(
    x$pt_fw_geographic_context == "western_nevada_context",
    "BLM Nevada",
    ""
  )
  x$pt_fw_management_district <- ifelse(
    x$pt_fw_geographic_context == "western_nevada_context",
    "Winnemucca District",
    ""
  )
  x$pt_fw_brim_inclusion <- ifelse(
    x$pt_fw_geographic_context == "western_nevada_context",
    "Western Nevada context",
    "California source context"
  )
  x$pt_management_local_managing_agency <- x$pt_fw_agency_name
  x$pt_management_co_managing_agencies <- pt_local_reference_clean_chr(
    components$co_managing_agencies[component_index]
  )
  x$pt_management_blm_role <- components$blm_role[component_index]
  x$pt_management_blm_role_summary <- components$blm_role_summary[component_index]
  x$pt_local_reference_feature_key <- x$wilderness_id
  x$pt_local_reference_semantic_key <- x$wilderness_id
  x$pt_local_reference_geometry_key <- x$component_id
  x$pt_local_reference_geometry_components <- 1L
  x$pt_fw_calculated_geometry_acres <- suppressWarnings(as.numeric(
    components$calculated_acres[component_index]
  ))
  x <- pt_local_reference_apply_category_tokens(
    x,
    "federal_wilderness",
    pt_local_reference_category_key("federal_wilderness", x$pt_fw_agency_code)
  )
  if (isTRUE(build_display)) {
    x$pt_reference_label_text <- x$pt_fw_official_name
    x$pt_reference_hover_html <- pt_local_reference_fw_hover_html(x)
    x$pt_reference_hover_text <- paste(
      x$pt_fw_official_name,
      x$pt_fw_agency_name,
      x$pt_fw_designation_year,
      sep = " · "
    )
  }
  if (isTRUE(validate_snapshot)) {
    expected_categories <- c(blm = 105L, usfs = 75L, nps = 15L, fws = 2L)
    actual_categories <- table(factor(
      x$pt_local_reference_category_key,
      levels = names(expected_categories)
    ))
    if (nrow(x) != 197L || length(unique(x$wilderness_id)) != 158L ||
        anyDuplicated(x$component_id) ||
        !identical(as.integer(actual_categories), unname(expected_categories)) ||
        sum(x$pt_fw_geographic_context == "western_nevada_context") != 3L ||
        length(unique(x$wilderness_id[x$pt_fw_management_pattern == "shared_multi_agency"])) != 14L ||
        length(unique(x$wilderness_id[x$pt_fw_designation_history == "has_subsequent_law"])) != 22L) {
      stop("Federal Wilderness prepared snapshot differs from the accepted 197/158 contract.")
    }
  }
  if (isTRUE(build_display)) pt_local_reference_fw_runtime_geometry(x) else x
}

pt_local_reference_fw_qa <- function(x) {
  data.frame(
    metric = c(
      "mapped_components", "named_wildernesses", "california_components",
      "western_nevada_components", "single_agency_wildernesses",
      "shared_multi_agency_wildernesses", "original_only_wildernesses",
      "wildernesses_with_subsequent_law"
    ),
    value = c(
      nrow(x), length(unique(x$wilderness_id)),
      sum(x$pt_fw_geographic_context == "california"),
      sum(x$pt_fw_geographic_context == "western_nevada_context"),
      length(unique(x$wilderness_id[x$pt_fw_management_pattern == "single_agency"])),
      length(unique(x$wilderness_id[x$pt_fw_management_pattern == "shared_multi_agency"])),
      length(unique(x$wilderness_id[x$pt_fw_designation_history == "original_only"])),
      length(unique(x$wilderness_id[x$pt_fw_designation_history == "has_subsequent_law"]))
    ),
    stringsAsFactors = FALSE
  )
}

pt_write_local_reference_fw_qa <- function(x, output_dir, prefix = "local_reference_federal_wilderness") {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  paths <- c(
    snapshot = file.path(output_dir, paste0(prefix, "_snapshot.csv")),
    category_counts = file.path(output_dir, paste0(prefix, "_category_counts.csv"))
  )
  utils::write.csv(pt_local_reference_fw_qa(x), paths[["snapshot"]], row.names = FALSE, na = "")
  utils::write.csv(
    pt_local_reference_category_qa(x, "federal_wilderness"),
    paths[["category_counts"]],
    row.names = FALSE,
    na = ""
  )
  paths
}

pt_local_reference_fw_feature_catalog <- function(x, reference, components) {
  if (!inherits(x, "sf")) stop("Federal Wilderness feature bounds require an sf object.")
  semantic_keys <- as.character(x$pt_local_reference_semantic_key)
  semantic_order <- unique(semantic_keys)
  reference_index <- match(semantic_order, reference$wilderness_id)
  if (anyNA(reference_index)) stop("Federal Wilderness search catalog lacks semantic references.")
  x_wgs84 <- if (isTRUE(sf::st_crs(x) == sf::st_crs(4326))) x else sf::st_transform(x, 4326)
  lapply(seq_along(semantic_order), function(i) {
    semantic_key <- semantic_order[[i]]
    index <- which(semantic_keys == semantic_key)
    ref <- reference[reference_index[[i]], , drop = FALSE]
    component_index <- match(x$component_id[index], components$component_id)
    search_text <- tolower(paste(unique(pt_local_reference_clean_chr(c(
      ref$official_name, ref$alternate_names, ref$wilderness_abbreviation,
      ref$states, ref$managing_agencies, semantic_key,
      components$standardized_name[component_index],
      components$local_managing_unit[component_index],
      components$blm_office[component_index]
    ))), collapse = " "))
    bounds <- sf::st_bbox(x_wgs84[index, , drop = FALSE])
    bounds_vector <- unname(as.numeric(c(
      bounds[["ymin"]], bounds[["xmin"]], bounds[["ymax"]], bounds[["xmax"]]
    )))
    if (length(bounds_vector) != 4L || any(!is.finite(bounds_vector))) {
      stop("Invalid Federal Wilderness semantic-feature bounds for ", semantic_key, ".")
    }
    list(
      semantic_feature_key = semantic_key,
      feature_key = semantic_key,
      display_name = pt_local_reference_clean_chr(ref$official_name),
      category_keys = unique(as.character(x$pt_local_reference_category_key[index])),
      search_text = search_text,
      semantic_feature_bounds = bounds_vector,
      geometry_component_count = sum(as.integer(x$pt_local_reference_geometry_components[index]))
    )
  })
}

pt_local_reference_fw_popup_payload <- function(
  components = pt_local_reference_fw_components(),
  reference = pt_local_reference_fw_reference(),
  designations = pt_local_reference_fw_designations(),
  documents = pt_local_reference_fw_documents(),
  policy = pt_local_reference_fw_common_policy(),
  sources = pt_local_reference_fw_source_register()
) {
  clean <- pt_local_reference_clean_chr
  office_identity <- paste(
    clean(components$local_managing_unit), clean(components$local_unit_url),
    clean(components$blm_office), clean(components$blm_office_url), sep = "\r"
  )
  nonblank_office <- nzchar(gsub("\r", "", office_identity, fixed = TRUE))
  unique_offices <- unique(office_identity[nonblank_office])
  office_key_lookup <- stats::setNames(
    sprintf("office-%03d", seq_along(unique_offices)), unique_offices
  )
  component_office_keys <- rep("", nrow(components))
  component_office_keys[nonblank_office] <- unname(office_key_lookup[
    office_identity[nonblank_office]
  ])
  offices <- lapply(seq_along(unique_offices), function(i) {
    row <- components[match(unique_offices[[i]], office_identity), , drop = FALSE]
    list(
      office_key = sprintf("office-%03d", i),
      local_managing_unit = clean(row$local_managing_unit),
      local_unit_url = clean(row$local_unit_url),
      blm_office = clean(row$blm_office),
      blm_office_url = clean(row$blm_office_url)
    )
  })
  agency_codes <- unique(clean(components$managing_agency_code))
  agencies <- lapply(agency_codes, function(code) {
    row <- components[match(code, clean(components$managing_agency_code)), , drop = FALSE]
    list(agency_key = code, name = clean(row$managing_agency))
  })
  designation_index <- match(reference$wilderness_id, designations$wilderness_id)
  semantics <- lapply(seq_len(nrow(reference)), function(i) {
    ref <- reference[i, , drop = FALSE]
    designation <- designations[designation_index[[i]], , drop = FALSE]
    list(
      wilderness_id = clean(ref$wilderness_id),
      official_name = clean(ref$official_name),
      alternate_names = clean(ref$alternate_names),
      wilderness_abbreviation = clean(ref$wilderness_abbreviation),
      states = clean(ref$states),
      designation_date = clean(designation$resolved_original_designation_date),
      designation_year = as.integer(designation$resolved_designation_year),
      original_public_law = clean(designation$original_public_law),
      subsequent_public_laws = clean(designation$subsequent_public_laws),
      official_reference_acres = suppressWarnings(as.numeric(ref$official_reference_acres)),
      source_component_count = as.integer(ref$source_component_count),
      managing_agencies = clean(ref$managing_agencies),
      shared_management = tolower(clean(ref$shared_management)) == "true",
      summary_short = clean(ref$wilderness_summary_short),
      management_access_summary = clean(ref$management_access_summary),
      official_page_url = clean(ref$official_page_url),
      direct_official_agency_page_url = clean(ref$direct_official_agency_page_url),
      official_map_url = clean(ref$official_map_url),
      primary_management_plan_title = clean(ref$primary_management_plan_title),
      primary_management_plan_url = clean(ref$primary_management_plan_url),
      wilderness_connect_url = clean(ref$wilderness_connect_url),
      congress_search_url = clean(ref$congress_search_url),
      nepa_search_url = clean(ref$nepa_search_url),
      courtlistener_search_url = clean(ref$courtlistener_search_url),
      google_scholar_case_search_url = clean(ref$google_scholar_case_search_url),
      web_search_url = clean(ref$web_search_url),
      acreage_source = clean(ref$acreage_source),
      validation_status = clean(designation$validation_status),
      explanatory_note = clean(designation$explanatory_note),
      evidence_source = clean(designation$evidence_source),
      evidence_url = clean(designation$evidence_url)
    )
  })
  component_records <- lapply(seq_len(nrow(components)), function(i) {
    row <- components[i, , drop = FALSE]
    list(
      component_id = clean(row$component_id),
      wilderness_id = clean(row$wilderness_id),
      agency_key = clean(row$managing_agency_code),
      office_key = component_office_keys[[i]],
      co_managing_agencies = clean(row$co_managing_agencies),
      blm_role = clean(row$blm_role),
      blm_role_summary = clean(row$blm_role_summary),
      geographic_state = clean(row$geographic_state),
      source_gis_acres = suppressWarnings(as.numeric(row$source_gis_acres)),
      calculated_acres = suppressWarnings(as.numeric(row$calculated_acres)),
      component_description = clean(row$component_description),
      geometry_caveat = clean(row$geometry_caveat)
    )
  })
  document_records <- lapply(seq_len(nrow(documents)), function(i) {
    row <- documents[i, , drop = FALSE]
    list(
      document_id = clean(row$document_id),
      wilderness_id = clean(row$wilderness_id),
      component_id = clean(row$component_id),
      title = clean(row$document_title),
      type = clean(row$document_type),
      agency = clean(row$agency),
      publication_date = clean(row$publication_date),
      url = clean(row$document_url),
      authority_level = clean(row$authority_level),
      scope = clean(row$wilderness_wide_or_component_specific)
    )
  })
  list(
    semantics = semantics,
    components = component_records,
    documents = document_records,
    agencies = agencies,
    offices = offices,
    policy = lapply(seq_len(nrow(policy)), function(i) list(
      topic = clean(policy$topic[[i]]),
      language = clean(policy$recommended_language[[i]])
    )),
    sources = lapply(seq_len(nrow(sources)), function(i) list(
      title = clean(sources$title[[i]]), agency = clean(sources$agency[[i]]),
      url = clean(sources$url[[i]]), type = clean(sources$source_type[[i]]),
      use = clean(sources$use[[i]]), limitations = clean(sources$limitations[[i]])
    )),
    templates = list(
      govinfo_public_law = "https://www.govinfo.gov/content/pkg/PLAW-{congress}publ{number}/html/PLAW-{congress}publ{number}.htm"
    )
  )
}

pt_local_reference_nm_reference <- function(
  path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_REFERENCE_PATH
) {
  pt_local_reference_read_csv(path, c(
    "monument_id", "canonical_name", "states", "scope_basis",
    "designation_status", "original_designation_date", "designation_year",
    "original_authority_type", "original_authority_citation",
    "administering_agencies", "primary_public_acres", "acreage_basis",
    "current_official_page", "geometry_source_agencies",
    "geometry_source_ids", "recent_material_change", "semantic_notes",
    "confidence", "verification_date"
  ))
}

pt_local_reference_nm_aliases <- function(
  path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_ALIASES_PATH
) {
  pt_local_reference_read_csv(path, c(
    "monument_id", "alias", "alias_type", "note", "verification_date"
  ))
}

pt_local_reference_nm_components <- function(
  path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_COMPONENTS_PATH
) {
  pt_local_reference_read_csv(path, c(
    "monument_id", "component_key", "component_name", "component_type",
    "agency", "managing_unit", "source_identifier",
    "relationship_to_monument", "notes", "verification_date"
  ))
}

pt_local_reference_nm_history <- function(
  path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_HISTORY_PATH
) {
  pt_local_reference_read_csv(path, c(
    "monument_id", "document_id", "instrument_type", "date", "title",
    "official_citation", "president_or_congress", "relationship_to_monument",
    "direct_official_url", "evidence_source_url", "verification_date",
    "confidence", "explanatory_note"
  ))
}

pt_local_reference_nm_documents <- function(
  path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_DOCUMENTS_PATH
) {
  pt_local_reference_read_csv(path, c(
    "monument_id", "document_id", "document_type", "title", "direct_url",
    "issuing_authority", "current_or_historical", "verification_date",
    "validation_status", "note"
  ))
}

pt_local_reference_nm_management <- function(
  path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_MANAGEMENT_PATH
) {
  pt_local_reference_read_csv(path, c(
    "monument_id", "agency", "management_role", "current_managing_unit",
    "official_unit_page", "evidence_basis", "confidence", "note",
    "verification_date"
  ))
}

pt_local_reference_nm_relationships <- function(
  path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_RELATIONSHIPS_PATH
) {
  pt_local_reference_read_csv(path, c(
    "monument_id", "relationship_id", "relationship_type", "target_name",
    "evidence_basis", "note", "verification_date"
  ))
}

pt_local_reference_nm_sources <- function(
  path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_SOURCES_PATH
) {
  pt_local_reference_read_csv(path, c(
    "source_id", "publisher", "service_title", "endpoint", "layer",
    "geographic_scope", "source_update_date", "identifiers", "geometry_type",
    "crs", "schema_summary", "source_role", "currentness", "suitability",
    "caveat", "verification_date"
  ))
}

pt_local_reference_nm_source_geometry_roles <- function(
  path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_SOURCE_GEOMETRY_ROLES_PATH
) {
  pt_local_reference_read_csv(path, c(
    "source_key", "source_object_id", "source_identifier", "source_name",
    "monument_id", "geometry_role", "use_for_semantic_display", "role_basis"
  ))
}

pt_local_reference_nm_values <- function(
  path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_VALUES_PATH
) {
  pt_local_reference_read_csv(path, c(
    "monument_id", "value_id", "raw_designation_object_or_purpose",
    "normalized_value_family", "concise_public_description",
    "evidence_document_id", "source_section_or_page", "confidence",
    "verification_date"
  ))
}

pt_local_reference_nm_ui_filters <- function(
  path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_UI_FILTERS_PATH
) {
  pt_local_reference_read_csv(path, c(
    "facet", "category", "expected_semantic_count", "exposure",
    "within_facet_semantics", "across_facets_semantics",
    "recommendation_note", "verification_date"
  ))
}

pt_validate_local_reference_nm_research <- function(
  reference = pt_local_reference_nm_reference(),
  aliases = pt_local_reference_nm_aliases(),
  components = pt_local_reference_nm_components(),
  history = pt_local_reference_nm_history(),
  documents = pt_local_reference_nm_documents(),
  management = pt_local_reference_nm_management(),
  relationships = pt_local_reference_nm_relationships(),
  sources = pt_local_reference_nm_sources(),
  values = pt_local_reference_nm_values(),
  ui_filters = pt_local_reference_nm_ui_filters()
) {
  expected_ids <- sort(reference$monument_id)
  if (nrow(reference) != 20L || anyDuplicated(reference$monument_id) ||
      any(reference$designation_status != "Current") ||
      !all(startsWith(reference$monument_id, "nm_ca_"))) {
    stop("National Monuments reference must retain 20 unique current semantic records.")
  }
  expected_counts <- c(
    aliases = 59L, components = 34L, history = 37L, documents = 62L,
    management = 24L, relationships = 17L, sources = 27L, values = 74L,
    ui_filters = 9L
  )
  actual_counts <- c(
    aliases = nrow(aliases), components = nrow(components), history = nrow(history),
    documents = nrow(documents), management = nrow(management),
    relationships = nrow(relationships), sources = nrow(sources),
    values = nrow(values), ui_filters = nrow(ui_filters)
  )
  if (!identical(actual_counts, expected_counts)) {
    stop("National Monuments normalized-table counts differ from the reviewed contract.")
  }
  child_tables <- list(
    aliases = aliases, components = components, history = history,
    documents = documents, management = management,
    values = values
  )
  incomplete <- vapply(child_tables, function(table) {
    !all(table$monument_id %in% expected_ids) ||
      !setequal(unique(table$monument_id), expected_ids)
  }, logical(1))
  if (any(incomplete)) {
    stop(
      "National Monuments child-table ID coverage is incomplete: ",
      paste(names(incomplete)[incomplete], collapse = ", ")
    )
  }
  if (!all(relationships$monument_id %in% expected_ids)) {
    stop("National Monuments relationships contain an unknown semantic ID.")
  }
  agency_count <- function(agency) sum(grepl(
    paste0("(^|\\|)", agency, "(\\||$)"),
    reference$administering_agencies
  ))
  authority <- table(reference$original_authority_type)
  if (!identical(
        c(
          BLM = agency_count("BLM"), USFS = agency_count("USFS"),
          NPS = agency_count("NPS"), USFWS = agency_count("USFWS")
        ),
        c(BLM = 9L, USFS = 7L, NPS = 7L, USFWS = 1L)
      ) ||
      !identical(
        as.integer(authority[c("Act of Congress", "Presidential proclamation")]),
        c(3L, 17L)
      ) ||
      sum(grepl("\\|", reference$administering_agencies)) != 4L ||
      sum(grepl("2024|2025", reference$recent_material_change)) != 4L) {
    stop("National Monuments 9/7/7/1 agency, 17/3 authority, 4 shared, or 4 recent contract changed.")
  }
  filter_counts <- suppressWarnings(as.integer(ui_filters$expected_semantic_count))
  if (!identical(filter_counts, c(9L, 7L, 7L, 1L, 17L, 3L, 4L, 16L, 4L)) ||
      any(ui_filters$within_facet_semantics != "OR") ||
      any(ui_filters$across_facets_semantics != "AND")) {
    stop("National Monuments UI filter lookup differs from its reviewed OR/AND count contract.")
  }
  invisible(TRUE)
}

pt_local_reference_nm_aggregate <- function(table, id, field, collapse = " | ") {
  values <- unique(pt_local_reference_clean_chr(
    table[[field]][table$monument_id == id]
  ))
  paste(values[nzchar(values)], collapse = collapse)
}

pt_local_reference_nm_agency_labels <- function(value) {
  labels <- c(
    blm = "Bureau of Land Management",
    usfs = "U.S. Forest Service",
    nps = "National Park Service",
    usfws = "U.S. Fish and Wildlife Service"
  )
  vapply(as.character(value), function(item) {
    keys <- trimws(unlist(strsplit(tolower(item), "|", fixed = TRUE)))
    keys <- keys[nzchar(keys)]
    resolved <- unname(labels[keys])
    if (length(resolved) != length(keys) || anyNA(resolved)) {
      stop("National Monuments administration contains an unknown agency key.")
    }
    paste(resolved, collapse = " · ")
  }, character(1), USE.NAMES = FALSE)
}

pt_local_reference_nm_html_list <- function(
  values,
  class_name = "",
  escape_values = TRUE
) {
  values <- unique(pt_local_reference_clean_chr(values))
  values <- values[nzchar(values)]
  if (!length(values)) return("")
  class_token <- if (nzchar(class_name)) paste0(" class=\"", class_name, "\"") else ""
  rendered_values <- if (isTRUE(escape_values)) {
    htmltools::htmlEscape(values)
  } else {
    values
  }
  paste0(
    "<ul", class_token, ">",
    paste0("<li>", rendered_values, "</li>", collapse = ""),
    "</ul>"
  )
}

pt_local_reference_nm_hover_html <- function(df) {
  vapply(seq_len(nrow(df)), function(i) {
    area <- pt_local_reference_format_square_miles_from_acres(
      df$pt_nm_calculated_geometry_acres[[i]]
    )
    value_families <- strsplit(
      pt_local_reference_clean_chr(df$pt_nm_value_families[[i]]),
      " | ", fixed = TRUE
    )[[1]]
    value_families <- value_families[nzchar(value_families)]
    cues <- c(
      if (df$pt_nm_display_geometry_role[[i]] == "agency_component") {
        paste0("Selected component: ", df$pt_nm_selected_component_label[[i]])
      } else if (df$pt_nm_management_pattern[[i]] == "shared_multi_agency") {
        "Complete shared monument boundary"
      } else {
        paste0("Administered by: ", df$pt_nm_administering_agency_label[[i]])
      },
      if (df$pt_nm_management_pattern[[i]] == "shared_multi_agency") {
        paste0("Administering agencies: ", df$pt_nm_administering_agency_label[[i]])
      } else "",
      paste0("Designated ", df$pt_nm_designation_year[[i]]),
      if (nzchar(area)) paste0("Approx. mapped area: ", sub("^~", "", area)) else "",
      if (length(value_families)) {
        paste0("Values: ", paste(utils::head(value_families, 3L), collapse = " · "))
      } else "",
      if (df$pt_nm_states[[i]] == "CA|OR") "Complete California–Oregon boundary" else ""
    )
    cues <- cues[nzchar(cues)]
    paste0(
      "<div class=\"pt-nm-hover-lines\"><div class=\"pt-nm-hover-line pt-nm-hover-title\">",
      htmltools::htmlEscape(df$pt_nm_canonical_name[[i]]), "</div>",
      paste0(
        "<div class=\"pt-nm-hover-line\">",
        htmltools::htmlEscape(cues), "</div>", collapse = ""
      ),
      "</div>"
    )
  }, character(1), USE.NAMES = FALSE)
}

pt_local_reference_nm_popup <- function(
  df,
  reference,
  components,
  history,
  documents,
  management,
  relationships,
  values
) {
  esc <- function(x, fallback = "Not stated") {
    htmltools::htmlEscape(pt_local_reference_clean_chr(x, fallback))
  }
  vapply(seq_len(nrow(df)), function(i) {
    row <- df[i, , drop = FALSE]
    id <- row$monument_id[[1]]
    ref <- reference[reference$monument_id == id, , drop = FALSE]
    comp <- components[components$monument_id == id, , drop = FALSE]
    hist <- history[history$monument_id == id, , drop = FALSE]
    docs <- documents[documents$monument_id == id, , drop = FALSE]
    mgmt <- management[management$monument_id == id, , drop = FALSE]
    rel <- relationships[relationships$monument_id == id, , drop = FALSE]
    vals <- values[values$monument_id == id, , drop = FALSE]

    public_acres_numeric <- suppressWarnings(as.numeric(ref$primary_public_acres[[1]]))
    public_acres <- if (is.finite(public_acres_numeric)) {
      paste0(pt_local_reference_format_number(public_acres_numeric, 0), " acres")
    } else {
      pt_local_reference_clean_chr(ref$primary_public_acres[[1]])
    }
    mapped_area <- pt_local_reference_format_square_miles_from_acres(
      row$pt_nm_calculated_geometry_acres[[1]]
    )
    overview <- c(
      pt_local_reference_popup_section("Designation", c(
        pt_local_reference_popup_row("Status", ref$designation_status[[1]]),
        pt_local_reference_popup_row(
          "Original designation",
          paste(ref$original_designation_date[[1]], ref$original_authority_type[[1]], sep = " · ")
        ),
        pt_local_reference_popup_row("Authority", ref$original_authority_citation[[1]]),
        pt_local_reference_popup_row("States", gsub("\\|", " + ", ref$states[[1]])),
        pt_local_reference_popup_row(
          "Administering agencies",
          row$pt_nm_administering_agency_label[[1]]
        ),
        pt_local_reference_popup_row(
          "Visible geometry",
          if (row$pt_nm_display_geometry_role[[1]] == "agency_component") {
            "Agency-administered component of one semantic National Monument"
          } else {
            "Complete semantic National Monument boundary"
          }
        ),
        if (row$pt_nm_display_geometry_role[[1]] == "agency_component") {
          pt_local_reference_popup_row(
            "Selected component", row$pt_nm_selected_component_label[[1]]
          )
        } else ""
      )),
      pt_local_reference_popup_section("Area", c(
        pt_local_reference_popup_row("Public reference", public_acres),
        pt_local_reference_popup_row("Public acreage basis", ref$acreage_basis[[1]]),
        pt_local_reference_popup_row("Mapped boundary", sub("^~", "Approx. ", mapped_area))
      )),
      pt_local_reference_popup_section("Context", c(
        paste0("<p>", esc(ref$semantic_notes[[1]]), "</p>"),
        if (nzchar(ref$recent_material_change[[1]])) {
          paste0("<p><strong>Recent material change:</strong> ", esc(ref$recent_material_change[[1]]), "</p>")
        } else ""
      ))
    )

    value_items <- paste0(
      esc(vals$normalized_value_family), ": ",
      esc(vals$concise_public_description)
    )
    resources <- c(
      pt_local_reference_popup_section(
        "Objects and purposes",
        pt_local_reference_nm_html_list(value_items, "pt-nm-values")
      ),
      pt_local_reference_popup_section("Official resources", c(
        pt_local_reference_trails_link(ref$current_official_page[[1]], "Current official monument page"),
        pt_local_reference_nm_html_list(vapply(seq_len(nrow(docs)), function(j) {
          url <- pt_local_reference_clean_chr(docs$direct_url[[j]])
          if (!nzchar(url)) return("")
          pt_local_reference_trails_link(url, docs$title[[j]])
        }, character(1)), "pt-nm-resource-links", escape_values = FALSE)
      ))
    )

    management_items <- vapply(seq_len(nrow(mgmt)), function(j) {
      unit <- pt_local_reference_clean_chr(mgmt$current_managing_unit[[j]])
      role <- paste(mgmt$agency[[j]], mgmt$management_role[[j]], sep = " — ")
      link <- pt_local_reference_trails_link(
        mgmt$official_unit_page[[j]],
        if (nzchar(unit)) unit else mgmt$agency[[j]]
      )
      paste0("<div class=\"pt-nm-management-row\"><strong>", esc(role),
             "</strong>", if (nzchar(link)) paste0(": ", link) else "", "</div>")
    }, character(1))
    component_items <- paste0(
      esc(comp$component_name), " — ", esc(comp$relationship_to_monument)
    )
    relationship_items <- paste0(
      esc(rel$target_name), " — ", esc(rel$note)
    )
    management_html <- c(
      pt_local_reference_popup_section("Administration", management_items),
      pt_local_reference_popup_section(
        "Named or agency components",
        pt_local_reference_nm_html_list(component_items)
      ),
      if (nrow(rel)) pt_local_reference_popup_section(
        "Relationships",
        pt_local_reference_nm_html_list(relationship_items)
      ) else "",
      paste0("<div class=\"pt-lr-popup-note\">", esc(
        PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_BOUNDARY_CAVEAT
      ), "</div>")
    )

    history_items <- vapply(seq_len(nrow(hist)), function(j) {
      url <- pt_local_reference_clean_chr(hist$direct_official_url[[j]])
      if (!nzchar(url)) url <- pt_local_reference_clean_chr(hist$evidence_source_url[[j]])
      title <- paste(
        hist$date[[j]], hist$official_citation[[j]], hist$relationship_to_monument[[j]],
        sep = " · "
      )
      link <- pt_local_reference_trails_link(url, hist$title[[j]])
      paste0("<div class=\"pt-nm-history-row\"><strong>", esc(title),
             "</strong>", if (nzchar(link)) paste0("<br>", link) else "", "</div>")
    }, character(1))
    geometry_note <- paste0(
      "<details class=\"pt-popup-technical\"><summary>Mapped geometry</summary>",
      pt_local_reference_popup_row(
        "Display generalization",
        "Topology-preserving, 1 metre in EPSG:3310"
      ),
      "</details>"
    )
    history_html <- c(
      pt_local_reference_popup_section("Designation history", history_items),
      geometry_note
    )

    pt_local_reference_tabbed_popup(
      popup_key = id,
      title = row$pt_nm_canonical_name[[1]],
      designation_badge = "National Monument",
      popup_class = "pt-nm-popup",
      tablist_label = "National Monument details",
      tabs = list(
        list(key = "overview", label = "Overview", html = paste(overview[nzchar(overview)], collapse = "")),
        list(key = "values", label = "Values & resources", html = paste(resources[nzchar(resources)], collapse = "")),
        list(key = "management", label = "Management", html = paste(management_html[nzchar(management_html)], collapse = "")),
        list(key = "history", label = "History", html = paste(history_html[nzchar(history_html)], collapse = ""))
      )
    )
  }, character(1), USE.NAMES = FALSE)
}

pt_local_reference_nm_runtime_geometry <- function(x) {
  sf_column <- attr(x, "sf_column")
  if (is.null(sf_column) || !length(sf_column) || !sf_column %in% names(x)) {
    sf_column <- names(x)[vapply(x, inherits, logical(1), what = "sfc")][[1]]
  }
  keep <- c(
    "pt_nickname", "pt_display_name", "pt_geom_type", "monument_id", "component_id",
    "pt_local_reference_feature_key", "pt_local_reference_semantic_key",
    "pt_local_reference_geometry_key", "pt_local_reference_geometry_components",
    "pt_local_reference_category_key", "pt_local_reference_category_label",
    "fill_col", "line_col", "fill_opacity", "line_weight", "line_dash",
    "pt_legend_swatch_style", "pt_nm_canonical_name", "pt_nm_aliases",
    "pt_nm_display_agency_key", "pt_nm_display_agency_label",
    "pt_nm_administering_agency_label", "pt_nm_selected_component_label",
    "pt_nm_display_geometry_role", "pt_nm_selected_geometry_acres",
    "pt_nm_administering_agencies", "pt_nm_authority_key",
    "pt_nm_original_authority", "pt_nm_management_pattern",
    "pt_nm_blm_usfs_quick_view", "pt_nm_recent_change",
    "pt_nm_component_names", "pt_nm_source_identifiers",
    "pt_nm_states", "pt_nm_designation_year", "pt_nm_calculated_geometry_acres",
    "pt_nm_value_families", "pt_reference_label_text",
    "pt_reference_hover_html", "pt_reference_hover_text", "popup_html", sf_column
  )
  missing <- setdiff(keep, names(x))
  if (length(missing)) {
    stop("National Monuments runtime geometry is missing: ", paste(missing, collapse = ", "))
  }
  out <- x[, keep, drop = FALSE]
  attr(out, "pt_national_monuments_candidate_metadata") <-
    attr(x, "pt_national_monuments_candidate_metadata")
  out
}

pt_prepare_local_reference_national_monuments <- function(
  x,
  validate_snapshot = FALSE,
  build_display = TRUE,
  reference_path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_REFERENCE_PATH,
  aliases_path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_ALIASES_PATH,
  components_path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_COMPONENTS_PATH,
  history_path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_HISTORY_PATH,
  documents_path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_DOCUMENTS_PATH,
  management_path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_MANAGEMENT_PATH,
  relationships_path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_RELATIONSHIPS_PATH,
  sources_path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_SOURCES_PATH,
  values_path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_VALUES_PATH,
  ui_filters_path = PT_LOCAL_REFERENCE_NATIONAL_MONUMENTS_UI_FILTERS_PATH
) {
  pt_validate_local_reference_config()
  if (!inherits(x, "sf") || !nrow(x)) {
    stop("National Monuments preparation requires a non-empty sf object.")
  }
  required_source <- c(
    "monument_id", "component_id", "canonical_name", "source_key",
    "source_agency", "source_object_id", "source_identifier", "source_name",
    "source_boundary_status", "source_gis_acres", "geometry_role",
    "display_agency_key"
  )
  missing_source <- setdiff(required_source, names(x))
  if (length(missing_source)) {
    stop("National Monuments candidate is missing: ", paste(missing_source, collapse = ", "))
  }
  reference <- pt_local_reference_nm_reference(reference_path)
  aliases <- pt_local_reference_nm_aliases(aliases_path)
  components <- pt_local_reference_nm_components(components_path)
  history <- pt_local_reference_nm_history(history_path)
  documents <- pt_local_reference_nm_documents(documents_path)
  management <- pt_local_reference_nm_management(management_path)
  relationships <- pt_local_reference_nm_relationships(relationships_path)
  sources <- pt_local_reference_nm_sources(sources_path)
  values <- pt_local_reference_nm_values(values_path)
  ui_filters <- pt_local_reference_nm_ui_filters(ui_filters_path)
  pt_validate_local_reference_nm_research(
    reference, aliases, components, history, documents, management,
    relationships, sources, values, ui_filters
  )

  reference_index <- match(x$monument_id, reference$monument_id)
  if (anyNA(reference_index) || anyDuplicated(x$component_id) ||
      !setequal(x$monument_id, reference$monument_id)) {
    stop(
      "National Monuments geometry requires complete 20-ID semantic coverage ",
      "and unique display-component IDs."
    )
  }
  if (any(x$canonical_name != reference$canonical_name[reference_index])) {
    stop("National Monuments candidate names differ from the reviewed canonical names.")
  }
  x$pt_nm_canonical_name <- reference$canonical_name[reference_index]
  x$pt_nm_aliases <- vapply(x$monument_id, function(id) {
    pt_local_reference_nm_aggregate(aliases, id, "alias")
  }, character(1))
  x$pt_nm_administering_agencies <- tolower(
    reference$administering_agencies[reference_index]
  )
  x$pt_nm_original_authority <- paste(
    reference$original_authority_type[reference_index],
    reference$original_authority_citation[reference_index], sep = " · "
  )
  x$pt_nm_authority_key <- ifelse(
    reference$original_authority_type[reference_index] == "Act of Congress",
    "act_of_congress", "presidential_proclamation"
  )
  x$pt_nm_management_pattern <- ifelse(
    grepl("\\|", reference$administering_agencies[reference_index]),
    "shared_multi_agency", "single_agency"
  )
  x$pt_nm_blm_usfs_quick_view <- ifelse(
    x$pt_nm_administering_agencies == "blm|usfs",
    "shared_blm_usfs", "other"
  )
  x$pt_nm_administering_agency_label <- pt_local_reference_nm_agency_labels(
    x$pt_nm_administering_agencies
  )
  x$pt_nm_recent_change <- ifelse(
    grepl("2024|2025", reference$recent_material_change[reference_index]),
    "recent_2024_2025", "not_recent"
  )
  x$pt_nm_states <- reference$states[reference_index]
  x$pt_nm_designation_year <- as.integer(reference$designation_year[reference_index])
  x$pt_nm_source_identifiers <- reference$geometry_source_ids[reference_index]
  x$pt_nm_component_names <- vapply(x$monument_id, function(id) {
    pt_local_reference_nm_aggregate(components, id, "component_name")
  }, character(1))
  x$pt_nm_value_families <- vapply(x$monument_id, function(id) {
    pt_local_reference_nm_aggregate(values, id, "normalized_value_family")
  }, character(1))
  x$pt_nm_selected_geometry_acres <- pt_local_reference_geometry_acres(x)
  x$pt_nm_calculated_geometry_acres <- ave(
    x$pt_nm_selected_geometry_acres,
    x$monument_id,
    FUN = sum
  )
  is_agency_component <- x$geometry_role == "agency_component_primary"
  x$pt_nm_display_geometry_role <- ifelse(
    is_agency_component, "agency_component", "complete_semantic_boundary"
  )
  x$pt_nm_display_agency_key <- ifelse(
    is_agency_component,
    pt_local_reference_clean_chr(x$display_agency_key),
    ifelse(
      x$pt_nm_management_pattern == "shared_multi_agency",
      "shared_multi",
      x$pt_nm_administering_agencies
    )
  )
  agency_labels <- c(
    blm = "Bureau of Land Management",
    usfs = "U.S. Forest Service",
    nps = "National Park Service",
    fws = "U.S. Fish and Wildlife Service",
    shared_multi = "Shared BLM–USFS"
  )
  x$pt_nm_display_agency_label <- unname(
    agency_labels[x$pt_nm_display_agency_key]
  )
  if (anyNA(x$pt_nm_display_agency_label)) {
    stop("National Monuments display geometry has an unresolved agency style.")
  }
  component_agency <- c(blm = "BLM", usfs = "USFS", nps = "NPS", fws = "USFWS")
  x$pt_nm_selected_component_label <- vapply(seq_len(nrow(x)), function(index) {
    if (!is_agency_component[[index]]) return("")
    agency <- unname(component_agency[x$pt_nm_display_agency_key[[index]]])
    matched <- components[
      components$monument_id == x$monument_id[[index]] &
        components$agency == agency &
        components$component_type != "agency management context",
      , drop = FALSE
    ]
    component_names <- unique(pt_local_reference_clean_chr(matched$component_name))
    component_names <- component_names[nzchar(component_names)]
    if (length(component_names)) {
      paste0(
        paste(component_names, collapse = " · "), " — ",
        x$pt_nm_display_agency_label[[index]]
      )
    } else {
      x$pt_nm_display_agency_label[[index]]
    }
  }, character(1), USE.NAMES = FALSE)
  x$pt_local_reference_feature_key <- x$monument_id
  x$pt_local_reference_semantic_key <- x$monument_id
  x$pt_local_reference_geometry_key <- x$component_id
  x$pt_local_reference_geometry_components <-
    pt_local_reference_geometry_components(x)
  x <- pt_local_reference_apply_category_tokens(
    x, "national_monuments", x$pt_nm_display_agency_key
  )

  if (isTRUE(build_display)) {
    x$pt_reference_label_text <- x$pt_nm_canonical_name
    x$pt_reference_hover_html <- pt_local_reference_nm_hover_html(x)
    x$pt_reference_hover_text <- paste(
      x$pt_nm_canonical_name,
      gsub("\\|", " + ", toupper(x$pt_nm_administering_agencies)),
      x$pt_nm_designation_year,
      sep = " · "
    )
    x$popup_html <- pt_local_reference_nm_popup(
      x, reference, components, history, documents, management,
      relationships, values
    )
  }

  if (isTRUE(validate_snapshot)) {
    metadata <- attr(x, "pt_national_monuments_candidate_metadata")
    semantic_count <- function(condition) length(unique(x$monument_id[condition]))
    agency_membership <- c(
      blm = semantic_count(grepl("(^|\\|)blm(\\||$)", x$pt_nm_administering_agencies)),
      usfs = semantic_count(grepl("(^|\\|)usfs(\\||$)", x$pt_nm_administering_agencies)),
      nps = semantic_count(grepl("(^|\\|)nps(\\||$)", x$pt_nm_administering_agencies)),
      usfws = semantic_count(grepl("(^|\\|)usfws(\\||$)", x$pt_nm_administering_agencies))
    )
    if (nrow(x) != 22L || length(unique(x$monument_id)) != 20L ||
        anyDuplicated(x$component_id) ||
        !identical(
          agency_membership,
          c(blm = 9L, usfs = 7L, nps = 7L, usfws = 1L)
        ) ||
        semantic_count(x$pt_nm_authority_key == "presidential_proclamation") != 17L ||
        semantic_count(x$pt_nm_authority_key == "act_of_congress") != 3L ||
        semantic_count(x$pt_nm_management_pattern == "shared_multi_agency") != 4L ||
        semantic_count(x$pt_nm_blm_usfs_quick_view == "shared_blm_usfs") != 3L ||
        length(unique(x$monument_id[
          x$pt_nm_recent_change == "recent_2024_2025"
        ])) != 4L ||
        sum(x$pt_nm_display_geometry_role == "agency_component") != 4L ||
        !identical(
          sort(x$pt_nm_display_agency_key[
            x$monument_id == "nm_ca_sand_to_snow"
          ]),
          c("blm", "usfs")
        ) ||
        !identical(
          sort(x$pt_nm_display_agency_key[
            x$monument_id == "nm_ca_tule_lake"
          ]),
          c("fws", "nps")
        ) ||
        any(!sf::st_is_valid(x)) || any(sf::st_is_empty(x)) ||
        is.null(metadata) ||
        !identical(as.numeric(metadata$simplify_tolerance_m), 1) ||
        !identical(as.integer(metadata$display_vertices), 175540L) ||
        !identical(as.integer(metadata$display_polygon_parts), 24432L)) {
      stop(
        "National Monuments prepared snapshot differs from the accepted ",
        "20 semantics / 22 display geometries / 24,432 parts / 175,540 vertices contract."
      )
    }
  }
  if (isTRUE(build_display)) pt_local_reference_nm_runtime_geometry(x) else x
}

pt_local_reference_nm_qa <- function(x) {
  semantic_count <- function(condition) {
    length(unique(x$pt_local_reference_semantic_key[condition]))
  }
  data.frame(
    metric = c(
      "semantic_monuments", "display_geometry_records", "polygon_parts",
      "blm_involved", "usfs_involved", "nps_involved", "usfws_involved",
      "presidential_proclamation", "act_of_congress",
      "shared_multi_agency", "shared_blm_usfs", "recent_2024_2025",
      "invalid", "empty"
    ),
    value = c(
      length(unique(x$pt_local_reference_semantic_key)), nrow(x),
      sum(x$pt_local_reference_geometry_components),
      semantic_count(grepl("(^|\\|)blm(\\||$)", x$pt_nm_administering_agencies)),
      semantic_count(grepl("(^|\\|)usfs(\\||$)", x$pt_nm_administering_agencies)),
      semantic_count(grepl("(^|\\|)nps(\\||$)", x$pt_nm_administering_agencies)),
      semantic_count(grepl("(^|\\|)usfws(\\||$)", x$pt_nm_administering_agencies)),
      semantic_count(x$pt_nm_authority_key == "presidential_proclamation"),
      semantic_count(x$pt_nm_authority_key == "act_of_congress"),
      semantic_count(x$pt_nm_management_pattern == "shared_multi_agency"),
      semantic_count(x$pt_nm_blm_usfs_quick_view == "shared_blm_usfs"),
      semantic_count(x$pt_nm_recent_change == "recent_2024_2025"),
      sum(!sf::st_is_valid(x)), sum(sf::st_is_empty(x))
    ),
    stringsAsFactors = FALSE
  )
}

pt_local_reference_acec_components <- function(
  path = PT_LOCAL_REFERENCE_ACEC_COMPONENTS_PATH
) {
  pt_local_reference_read_csv(path, c(
    "component_id", "acec_id", "source_globalid", "source_globalid_normalized",
    "source_objectid", "source_name", "source_feature_type",
    "source_designation_code", "source_status_code", "source_agency_code",
    "component_gis_acres", "component_calculated_acres",
    "component_blm_district", "component_blm_field_office",
    "component_governing_plan", "component_source_modified_date",
    "component_supplied_geometry_part_count", "component_supplied_geometry_valid",
    "component_live_shape_area_m2", "component_live_shape_length_m"
  ))
}

pt_local_reference_acec_reference <- function(
  path = PT_LOCAL_REFERENCE_ACEC_REFERENCE_PATH
) {
  pt_local_reference_read_csv(path, c(
    "acec_id", "official_acec_name", "alternate_names", "source_semantic_key",
    "designation_status", "designation_status_label", "designation_type",
    "rna_relationship_status", "is_currently_designated", "is_proposed_only",
    "is_historical_or_superseded", "source_component_count",
    "source_geometry_part_count", "official_acres", "current_gis_acres",
    "calculated_source_geometry_acres", "blm_districts", "blm_field_offices",
    "source_administrative_unit", "source_admin_unit_code",
    "current_governing_plans", "designation_decision_date", "designation_year",
    "designation_authority", "designation_document_id",
    "designation_document_url", "designation_document_page",
    "designation_history_summary", "relevant_and_important_values_summary",
    "value_families", "management_direction_summary",
    "access_and_land_status_summary", "counties", "states",
    "planning_framework", "water_resource_relevance",
    "water_resource_relevance_summary", "current_official_page_url",
    "current_official_map_url", "current_plan_url", "current_office_url",
    "research_status", "last_verified", "record_confidence",
    "unit_summary_short", "source_geometry_provenance"
  ))
}

pt_local_reference_acec_values <- function(path = PT_LOCAL_REFERENCE_ACEC_VALUES_PATH) {
  pt_local_reference_read_csv(path, c(
    "acec_id", "component_id", "value_record_id", "value_family", "value_type",
    "value_name", "value_scope", "value_is_primary", "value_description",
    "value_source_document", "value_source_url", "value_source_page",
    "value_source_section", "value_last_verified", "value_confidence",
    "water_resource_connection", "provenance_class"
  ))
}

pt_local_reference_acec_documents <- function(path = PT_LOCAL_REFERENCE_ACEC_DOCUMENTS_PATH) {
  pt_local_reference_read_csv(path, c(
    "document_id", "document_type", "document_title", "nepa_number",
    "document_date", "document_status", "source_agency", "document_url",
    "url_status", "relevant_pages", "retrieval_date", "associated_acec_count"
  ))
}

pt_local_reference_acec_management <- function(path = PT_LOCAL_REFERENCE_ACEC_MANAGEMENT_PATH) {
  pt_local_reference_read_csv(path, c(
    "acec_id", "component_id", "prescription_id", "management_category",
    "management_action_type", "management_direction", "management_scope",
    "geographic_scope", "seasonal_scope", "exception_or_condition",
    "is_prohibition", "is_restriction", "is_required_action",
    "is_management_objective", "current_or_historical", "governing_document",
    "governing_document_url", "governing_document_page",
    "governing_document_section", "last_verified", "confidence", "provenance_class"
  ))
}

pt_local_reference_acec_planning <- function(path = PT_LOCAL_REFERENCE_ACEC_PLANNING_PATH) {
  pt_local_reference_read_csv(path, c(
    "acec_id", "component_id", "planning_action_id", "planning_action_type",
    "planning_action_title", "planning_action_date", "planning_action_status",
    "planning_action_effect", "prior_status", "resulting_status",
    "boundary_effect", "acreage_before", "acreage_after", "document_id",
    "document_title", "document_url", "document_page", "eplanning_project_id",
    "eplanning_project_url", "source_agency", "last_verified", "confidence"
  ))
}

pt_local_reference_acec_relationships <- function(
  path = PT_LOCAL_REFERENCE_ACEC_RELATIONSHIPS_PATH
) {
  pt_local_reference_read_csv(path, c(
    "acec_id", "component_id", "related_layer_family", "related_feature_id",
    "related_feature_name", "relationship_type", "relationship_scope",
    "relationship_method", "relationship_source", "relationship_source_url",
    "spatial_derivation_required", "intersection_area_acres",
    "intersection_percent_of_component", "intersection_percent_of_acec",
    "relationship_last_verified", "confidence", "relationship_context_class"
  ))
}

pt_local_reference_acec_sources <- function(path = PT_LOCAL_REFERENCE_ACEC_SOURCES_PATH) {
  source <- pt_local_reference_read_csv(path, c(
    "source_title", "source_type", "agency", "url", "retrieved_or_verified",
    "authoritative_scope", "local_snapshot", "status"
  ))
  names(source)[[1]] <- sub("^\\ufeff", "", names(source)[[1]])
  source
}

pt_local_reference_acec_offices <- function(path = PT_LOCAL_REFERENCE_ACEC_OFFICES_PATH) {
  pt_local_reference_read_csv(path, c(
    "acec_id", "component_id", "source_admin_unit_code", "source_admin_unit_label",
    "responsible_blm_district", "responsible_blm_field_office", "assignment_type",
    "boundary_dataset", "dataset_date", "intersection_method", "overlap_acres",
    "overlap_percent", "multiple_offices_intersect", "crosses_office_boundary",
    "evidence_url", "derivation_date", "confidence", "manual_review_status"
  ))
}

pt_local_reference_acec_current_field_offices <- function(
  path = PT_LOCAL_REFERENCE_ACEC_CURRENT_FIELD_OFFICES_PATH
) {
  pt_local_reference_read_csv(path, c(
    "office_key", "office_code", "current_official_name", "boundary_source_name",
    "parent_district_code", "parent_district_name", "official_office_url",
    "roster_source_url", "roster_verified_on", "boundary_globalid",
    "boundary_source", "boundary_source_url", "boundary_snapshot_date",
    "boundary_raw_shp_sha256", "boundary_derivative_rds_sha256",
    "current_roster_status", "sort_order"
  ))
}

pt_local_reference_acec_field_office_context <- function(
  path = PT_LOCAL_REFERENCE_ACEC_FIELD_OFFICE_CONTEXT_PATH
) {
  pt_local_reference_read_csv(path, c(
    "acec_id", "component_id", "current_field_office_key",
    "current_field_office_code", "current_field_office_name",
    "intersection_area_m2", "intersection_area_acres", "percent_of_acec_area",
    "acec_context_class", "source_context_qa_class", "relationship_method",
    "minimum_intersection_area_m2", "complete_coverage_percent",
    "boundary_snapshot_date", "derivation_date", "confidence"
  ))
}

pt_local_reference_acec_access <- function(path = PT_LOCAL_REFERENCE_ACEC_ACCESS_PATH) {
  pt_local_reference_read_csv(path, c(
    "acec_id", "component_id", "boundary_represents", "designation_applies_to",
    "surface_management_scope", "mineral_estate_scope", "includes_non_blm_land",
    "non_blm_land_summary", "public_access_status", "public_access_scope",
    "access_information_source", "access_information_source_url",
    "access_information_verified_on", "access_information_confidence",
    "land_status_caveat", "last_verified"
  ))
}

pt_local_reference_acec_overrides <- function(path = PT_LOCAL_REFERENCE_ACEC_OVERRIDES_PATH) {
  pt_local_reference_read_csv(path, c(
    "override_id", "acec_id", "component_id", "target_field", "source_value",
    "proposed_value", "reason", "evidence_title", "evidence_url", "confidence",
    "review_status"
  ))
}

pt_local_reference_acec_ui_filters <- function(path = PT_LOCAL_REFERENCE_ACEC_UI_FILTERS_PATH) {
  pt_local_reference_read_csv(path, c(
    "filter_group_id", "value_key", "user_label", "current_semantic_count",
    "filter_scope", "sort_order", "default_selected", "tooltip"
  ))
}

pt_local_reference_acec_wsa_name_context <- function(
  path = PT_LOCAL_REFERENCE_ACEC_WSA_NAME_CONTEXT_PATH
) {
  pt_local_reference_read_csv(path, c(
    "acec_id", "official_acec_name", "wsa_name_context_status",
    "current_wsa_name", "current_wsa_nlcs_id", "current_wsa_global_id",
    "current_wsa_feature_key", "comparison_inventory", "comparison_source_url",
    "comparison_date", "match_method", "popup_wording"
  ))
}

pt_local_reference_acec_overlap_pairs <- function(
  path = PT_LOCAL_REFERENCE_ACEC_OVERLAP_PAIRS_PATH
) {
  pt_local_reference_read_csv(path, c(
    "acec_id_a", "acec_id_b", "overlap_area_m2", "overlap_area_acres",
    "percent_of_smaller_acec", "relationship_type"
  ))
}

pt_local_reference_acec_normalize_globalid <- function(x) {
  tolower(gsub("[{}[:space:]]", "", pt_local_reference_clean_chr(x)))
}

pt_local_reference_acec_component_id <- function(global_id) {
  value <- pt_local_reference_acec_normalize_globalid(global_id)
  ifelse(startsWith(value, "blmca-"), value, paste0("blmca-", value))
}

pt_validate_local_reference_acec_research <- function(
  components = pt_local_reference_acec_components(),
  reference = pt_local_reference_acec_reference(),
  values = pt_local_reference_acec_values(),
  documents = pt_local_reference_acec_documents(),
  management = pt_local_reference_acec_management(),
  planning = pt_local_reference_acec_planning(),
  relationships = pt_local_reference_acec_relationships(),
  sources = pt_local_reference_acec_sources(),
  offices = pt_local_reference_acec_offices(),
  current_field_offices = pt_local_reference_acec_current_field_offices(),
  field_office_context = pt_local_reference_acec_field_office_context(),
  access = pt_local_reference_acec_access(),
  overrides = pt_local_reference_acec_overrides(),
  ui_filters = pt_local_reference_acec_ui_filters(),
  wsa_name_context = pt_local_reference_acec_wsa_name_context(),
  overlap_pairs = pt_local_reference_acec_overlap_pairs()
) {
  expected_rows <- c(
    components = 238L, reference = 238L, values = 867L, documents = 32L,
    management = 246L, planning = 238L, relationships = 476L, sources = 7L,
    offices = 238L, current_field_offices = 14L, field_office_context = 276L,
    access = 238L, overrides = 3L, ui_filters = 27L,
    wsa_name_context = 11L, overlap_pairs = 27L
  )
  actual_rows <- c(
    components = nrow(components), reference = nrow(reference), values = nrow(values),
    documents = nrow(documents), management = nrow(management), planning = nrow(planning),
    relationships = nrow(relationships), sources = nrow(sources), offices = nrow(offices),
    current_field_offices = nrow(current_field_offices),
    field_office_context = nrow(field_office_context), access = nrow(access),
    overrides = nrow(overrides), ui_filters = nrow(ui_filters),
    wsa_name_context = nrow(wsa_name_context), overlap_pairs = nrow(overlap_pairs)
  )
  if (!identical(actual_rows, expected_rows)) {
    stop("ACEC research-table row counts differ from the reviewed enrichment package.")
  }
  if (anyDuplicated(components$component_id) || anyDuplicated(components$source_globalid_normalized) ||
      anyDuplicated(reference$acec_id) || anyDuplicated(values$value_record_id) ||
      anyDuplicated(documents$document_id) || anyDuplicated(management$prescription_id) ||
      anyDuplicated(planning$planning_action_id) || anyDuplicated(wsa_name_context$acec_id)) {
    stop("ACEC research keys must remain unique at their declared grain.")
  }
  overlap_a <- pt_local_reference_clean_chr(overlap_pairs$acec_id_a)
  overlap_b <- pt_local_reference_clean_chr(overlap_pairs$acec_id_b)
  overlap_key <- paste(overlap_a, overlap_b, sep = "|")
  overlap_area_m2 <- suppressWarnings(as.numeric(overlap_pairs$overlap_area_m2))
  overlap_area_acres <- suppressWarnings(as.numeric(overlap_pairs$overlap_area_acres))
  overlap_percent <- suppressWarnings(as.numeric(overlap_pairs$percent_of_smaller_acec))
  overlap_type <- pt_local_reference_clean_chr(overlap_pairs$relationship_type)
  overlap_nodes <- sort(unique(c(overlap_a, overlap_b)))
  overlap_neighbors <- stats::setNames(lapply(overlap_nodes, function(node) {
    unique(c(
      overlap_b[overlap_a == node],
      overlap_a[overlap_b == node]
    ))
  }), overlap_nodes)
  overlap_seen <- character(0)
  overlap_component_sizes <- integer(0)
  for (node in overlap_nodes) {
    if (node %in% overlap_seen) next
    queue <- node
    component <- character(0)
    while (length(queue)) {
      current <- queue[[1]]
      queue <- queue[-1]
      if (current %in% overlap_seen) next
      overlap_seen <- c(overlap_seen, current)
      component <- c(component, current)
      queue <- c(queue, setdiff(overlap_neighbors[[current]], overlap_seen))
    }
    overlap_component_sizes <- c(overlap_component_sizes, length(component))
  }
  if (any(!nzchar(overlap_a) | !nzchar(overlap_b)) ||
      any(overlap_a >= overlap_b) || anyDuplicated(overlap_key) ||
      any(!overlap_a %in% reference$acec_id) ||
      any(!overlap_b %in% reference$acec_id) ||
      any(!is.finite(overlap_area_m2) | overlap_area_m2 <=
            PT_LOCAL_REFERENCE_ACEC_OVERLAP_STYLE$minimum_overlap_area_m2) ||
      any(!is.finite(overlap_area_acres) | overlap_area_acres <= 0) ||
      any(abs(overlap_area_acres - overlap_area_m2 / 4046.8564224) > 1e-8) ||
      any(!is.finite(overlap_percent) | overlap_percent <= 0 | overlap_percent > 100) ||
      any(!overlap_type %in% c("overlap", "containment")) ||
      sum(overlap_type == "containment") != 2L ||
      length(overlap_nodes) != 39L || length(overlap_component_sizes) != 13L ||
      !identical(sort(overlap_component_sizes, decreasing = TRUE),
                 c(7L, 6L, 5L, 3L, rep(2L, 9L))) ||
      max(lengths(overlap_neighbors)) != 4L) {
    stop("ACEC current-geometry overlap graph differs from the reviewed 27-pair contract.")
  }
  normalized <- pt_local_reference_acec_normalize_globalid(components$source_globalid)
  if (!identical(normalized, pt_local_reference_clean_chr(components$source_globalid_normalized)) ||
      !identical(components$component_id, paste0("blmca-", normalized))) {
    stop("ACEC component IDs must be exact normalized source GlobalIDs.")
  }
  ids <- reference$acec_id
  related_tables <- list(components, values, management, planning, relationships, offices, access)
  if (any(vapply(related_tables, function(x) {
    any(!pt_local_reference_clean_chr(x$acec_id) %in% ids)
  }, logical(1))) || !setequal(components$acec_id, ids) ||
      !setequal(planning$acec_id, ids) || !setequal(offices$acec_id, ids) ||
      !setequal(access$acec_id, ids)) {
    stop("ACEC one-to-many lookup keys do not reconcile to the semantic reference.")
  }
  current_office_text <- paste(
    current_field_offices$office_key,
    current_field_offices$current_official_name,
    current_field_offices$boundary_source_name
  )
  current_office_order <- order(as.integer(current_field_offices$sort_order))
  ordered_current_office_names <-
    current_field_offices$current_official_name[current_office_order]
  context_key <- paste(
    field_office_context$acec_id,
    field_office_context$current_field_office_code,
    sep = "|"
  )
  context_area_m2 <- suppressWarnings(as.numeric(
    field_office_context$intersection_area_m2
  ))
  context_area_acres <- suppressWarnings(as.numeric(
    field_office_context$intersection_area_acres
  ))
  context_percent <- suppressWarnings(as.numeric(
    field_office_context$percent_of_acec_area
  ))
  context_maximum <- ave(
    context_percent,
    field_office_context$acec_id,
    FUN = function(x) x == max(x)
  )
  context_presentation_keep <- context_maximum == 1 |
    context_percent >=
      PT_LOCAL_REFERENCE_ACEC_FIELD_OFFICE_CONTEXT_METADATA$
        presentation_additional_office_minimum_percent
  context_semantic <- field_office_context[!duplicated(
    field_office_context$acec_id
  ), c("acec_id", "acec_context_class", "source_context_qa_class"), drop = FALSE]
  context_classes <- table(factor(
    context_semantic$acec_context_class,
    levels = c(
      "wholly_within_one_field_office", "crosses_field_office_boundaries",
      "partial_spatial_match_review_required", "no_spatial_match_review_required"
    )
  ))
  source_context_classes <- table(factor(
    context_semantic$source_context_qa_class,
    levels = c(
      "exact_single", "source_in_multiple", "district_only_contextualized",
      "source_spatial_disagreement", "no_spatial_match"
    )
  ))
  if (nrow(current_field_offices) != 14L ||
      anyDuplicated(current_field_offices$office_key) ||
      anyDuplicated(current_field_offices$office_code) ||
      !identical(
        as.integer(current_field_offices$sort_order[current_office_order]),
        seq_len(14L)
      ) ||
      !identical(
        ordered_current_office_names,
        sort(ordered_current_office_names, method = "radix")
      ) ||
      anyDuplicated(context_key) ||
      !setequal(field_office_context$acec_id, ids) ||
      !setequal(
        field_office_context$current_field_office_code,
        current_field_offices$office_code
      ) || !all(field_office_context$current_field_office_key %in%
                   current_field_offices$office_key) ||
      any(!is.finite(context_area_m2) | context_area_m2 <= 100) ||
      any(!is.finite(context_area_acres) | context_area_acres <= 0) ||
      any(abs(context_area_acres - context_area_m2 / 4046.8564224) > 1e-7) ||
      any(!is.finite(context_percent) | context_percent <= 0 |
            context_percent > 100.000001) ||
      !all(field_office_context$relationship_method ==
             "positive-area intersection in EPSG:3310") ||
      !all(as.numeric(field_office_context$minimum_intersection_area_m2) == 100) ||
      !identical(
        PT_LOCAL_REFERENCE_ACEC_FIELD_OFFICE_CONTEXT_METADATA$
          presentation_additional_office_minimum_percent,
        1
      ) ||
      sum(context_presentation_keep) != 254L ||
      sum(!context_presentation_keep) != 22L ||
      length(unique(field_office_context$acec_id[!context_presentation_keep])) != 20L ||
      sum(table(field_office_context$acec_id[context_presentation_keep]) > 1L) != 15L ||
      !identical(as.integer(context_classes), c(202L, 35L, 1L, 0L)) ||
      !identical(as.integer(source_context_classes), c(106L, 6L, 126L, 0L, 0L)) ||
      any(grepl(
        "Hollister|Alturas|Susanville",
        paste(current_office_text, field_office_context$current_field_office_name),
        ignore.case = TRUE
      ))) {
    stop("ACEC current field-office context differs from the reviewed spatial contract.")
  }
  acreage_difference <- abs(
    as.numeric(reference$current_gis_acres) -
      as.numeric(reference$calculated_source_geometry_acres)
  )
  acreage_difference_percent <-
    acreage_difference / as.numeric(reference$current_gis_acres) * 100
  material_area_difference <-
    acreage_difference >=
      PT_LOCAL_REFERENCE_ACEC_AREA_PRESENTATION$material_difference_minimum_acres &
    acreage_difference_percent >=
      PT_LOCAL_REFERENCE_ACEC_AREA_PRESENTATION$material_difference_minimum_percent
  if (sum(material_area_difference) != 1L ||
      !identical(reference$official_acec_name[material_area_difference], "Massacre Rim")) {
    stop("ACEC acreage presentation audit differs from the reviewed one-record exception.")
  }
  designated <- tolower(pt_local_reference_clean_chr(reference$is_currently_designated)) == "true"
  if (!all(designated) || any(tolower(reference$designation_type) != "acec") ||
      any(tolower(pt_local_reference_clean_chr(reference$is_proposed_only)) == "true") ||
      any(tolower(pt_local_reference_clean_chr(reference$is_historical_or_superseded)) == "true")) {
    stop("The reviewed ACEC reference must retain all 238 current designated ACECs without status/type filtering.")
  }
  black_mountain <- reference$acec_id[reference$official_acec_name == "Black Mountain"]
  if (length(black_mountain) != 2L || length(unique(black_mountain)) != 2L) {
    stop("The two Black Mountain ACECs must remain distinct semantic features.")
  }
  expected_frameworks <- c(
    california_desert_other_plan = 13L, central_california_plan = 53L,
    drecp = 128L, northern_california_other_plan = 19L,
    northwest_california_integrated_plan = 25L
  )
  actual_frameworks <- table(factor(reference$planning_framework, levels = names(expected_frameworks)))
  if (!identical(as.integer(actual_frameworks), unname(expected_frameworks))) {
    stop("ACEC planning-framework counts differ from the reviewed package.")
  }
  expected_value_counts <- c(
    botanical_or_ecological = 192L, cultural_archaeological_historic = 126L,
    other_or_unresolved = 5L, scenic = 73L, water_aquatic = 34L,
    wildlife_and_habitat = 156L
  )
  actual_value_counts <- table(factor(
    unlist(strsplit(reference$value_families, ";", fixed = TRUE)),
    levels = names(expected_value_counts)
  ))
  if (!identical(as.integer(actual_value_counts), unname(expected_value_counts))) {
    stop("ACEC value-family semantic counts differ from the reviewed package.")
  }
  if (sum(reference$rna_relationship_status == "research_candidate_from_source_name_only") != 3L ||
      any(overrides$review_status == "rejected")) {
    stop("ACEC RNA candidates or curated-override review states changed unexpectedly.")
  }
  named_wsa <- reference[
    grepl("(^|[^A-Za-z])WSA([^A-Za-z]|$)", reference$official_acec_name),
    c("acec_id", "official_acec_name"), drop = FALSE
  ]
  wsa_index <- match(wsa_name_context$acec_id, named_wsa$acec_id)
  current_wsa <- wsa_name_context$wsa_name_context_status == "current_wsa"
  historical_name <- wsa_name_context$wsa_name_context_status == "historical_name_only"
  if (nrow(named_wsa) != 11L || anyNA(wsa_index) ||
      !identical(
        pt_local_reference_clean_chr(wsa_name_context$official_acec_name),
        pt_local_reference_clean_chr(named_wsa$official_acec_name[wsa_index])
      ) || sum(current_wsa) != 6L || sum(historical_name) != 5L ||
      any(!(current_wsa | historical_name)) ||
      any(!nzchar(pt_local_reference_clean_chr(
        wsa_name_context$current_wsa_nlcs_id[current_wsa]
      ))) || any(nzchar(pt_local_reference_clean_chr(
        wsa_name_context$current_wsa_nlcs_id[historical_name]
      ))) || any(!nzchar(pt_local_reference_clean_chr(
        wsa_name_context$popup_wording
      )))) {
    stop("ACEC WSA-name context must retain 11 exact names: 6 current and 5 historical-name-only.")
  }
  invisible(TRUE)
}

pt_local_reference_acec_hover_html <- function(df) {
  value_labels <- c(
    water_aquatic = "Fish/aquatic",
    wildlife_and_habitat = "Wildlife/habitat",
    botanical_or_ecological = "Natural systems/processes",
    cultural_archaeological_historic = "Cultural/historic",
    scenic = "Scenic",
    other_or_unresolved = "Natural hazard/other"
  )
  vapply(seq_len(nrow(df)), function(i) {
    area <- pt_local_reference_format_square_miles_from_acres(df$pt_acec_gis_acres[[i]])
    family_keys <- strsplit(
      pt_local_reference_clean_chr(df$pt_acec_value_families[[i]]),
      ";", fixed = TRUE
    )[[1]]
    family_keys <- family_keys[nzchar(family_keys)]
    family_text <- unname(value_labels[family_keys])
    family_text <- family_text[!is.na(family_text) & nzchar(family_text)]
    details <- c(
      if (nzchar(df$pt_acec_governing_plan[[i]])) {
        paste0("Plan: ", df$pt_acec_governing_plan[[i]])
      } else "",
      if (length(family_text)) {
        paste0("Values: ", paste(family_text, collapse = " · "))
      } else "",
      if (nzchar(area)) paste0("Approx. mapped area: ", sub("^~", "", area)) else ""
    )
    details <- details[nzchar(details)]
    paste0(
      "<div class=\"pt-acec-hover-lines\"><div class=\"pt-acec-hover-line pt-acec-hover-title\">",
      htmltools::htmlEscape(df$pt_acec_official_name[[i]]), "</div>",
      paste0("<div class=\"pt-acec-hover-line\">", htmltools::htmlEscape(details), "</div>", collapse = ""),
      "</div>"
    )
  }, character(1), USE.NAMES = FALSE)
}

pt_local_reference_acec_runtime_geometry <- function(x) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("ACEC runtime geometry requires sf.")
  invisible(sf::st_geometry(x))
  sf_column <- attr(x, "sf_column")
  if (is.null(sf_column) || !length(sf_column) || !sf_column %in% names(x)) {
    sf_column <- names(x)[vapply(x, inherits, logical(1), what = "sfc")][[1]]
  }
  keep <- c(
    "pt_nickname", "pt_display_name", "pt_geom_type", "component_id", "acec_id",
    "pt_local_reference_feature_key", "pt_local_reference_semantic_key",
    "pt_local_reference_geometry_key", "pt_local_reference_geometry_components",
    "pt_local_reference_category_key", "pt_local_reference_category_label",
    "fill_col", "line_col", "fill_opacity", "line_weight", "line_dash",
    "pt_legend_swatch_style", "pt_acec_official_name", "pt_acec_legacy_name",
    "pt_acec_aliases", "pt_acec_governing_plan", "pt_acec_planning_framework",
    "pt_acec_source_admin_unit", "pt_acec_field_office_context",
    "pt_acec_field_office_context_names", "pt_acec_value_families",
    "pt_acec_global_id", "pt_reference_label_text", "pt_reference_hover_html",
    "pt_reference_hover_text", sf_column
  )
  missing <- setdiff(keep, names(x))
  if (length(missing)) stop("ACEC runtime geometry is missing: ", paste(missing, collapse = ", "))
  out <- x[, keep, drop = FALSE]
  attr(out, "pt_acec_candidate_metadata") <- attr(x, "pt_acec_candidate_metadata")
  out
}

pt_prepare_local_reference_acec <- function(
  x,
  validate_snapshot = FALSE,
  build_display = TRUE,
  components_path = PT_LOCAL_REFERENCE_ACEC_COMPONENTS_PATH,
  reference_path = PT_LOCAL_REFERENCE_ACEC_REFERENCE_PATH,
  field_office_context_path = PT_LOCAL_REFERENCE_ACEC_FIELD_OFFICE_CONTEXT_PATH,
  current_field_offices_path = PT_LOCAL_REFERENCE_ACEC_CURRENT_FIELD_OFFICES_PATH
) {
  pt_validate_local_reference_config()
  if (!inherits(x, "sf") || !nrow(x)) stop("ACEC preparation requires a non-empty sf object.")
  resolved <- pt_local_reference_resolve_aliases("acec", names(x), require_all = TRUE)
  get_field <- function(name) x[[resolved[[name]]]]
  components <- pt_local_reference_acec_components(components_path)
  reference <- pt_local_reference_acec_reference(reference_path)
  field_office_context <- pt_local_reference_acec_field_office_context(
    field_office_context_path
  )
  current_field_offices <- pt_local_reference_acec_current_field_offices(
    current_field_offices_path
  )
  pt_validate_local_reference_acec_research(components = components, reference = reference)

  source_component_id <- pt_local_reference_acec_component_id(get_field("global_id"))
  if (anyDuplicated(source_component_id)) stop("ACEC source GlobalIDs must be complete and unique.")
  component_index <- match(source_component_id, components$component_id)
  if (anyNA(component_index)) stop("ACEC geometry GlobalIDs are not fully covered by the reviewed component crosswalk.")
  x$component_id <- source_component_id
  x$acec_id <- components$acec_id[component_index]
  reference_index <- match(x$acec_id, reference$acec_id)
  if (anyNA(reference_index)) stop("ACEC semantic IDs are not fully covered by the reviewed reference.")

  x$pt_acec_global_id <- pt_local_reference_clean_chr(get_field("global_id"))
  x$pt_acec_source_name <- pt_local_reference_clean_chr(get_field("name"))
  x$pt_acec_official_name <- reference$official_acec_name[reference_index]
  x$pt_acec_legacy_name <- ifelse(
    pt_local_reference_normalize_text(x$pt_acec_source_name) !=
      pt_local_reference_normalize_text(x$pt_acec_official_name),
    x$pt_acec_source_name, ""
  )
  x$pt_acec_aliases <- pt_local_reference_clean_chr(reference$alternate_names[reference_index])
  x$pt_acec_governing_plan <- pt_local_reference_clean_chr(reference$current_governing_plans[reference_index])
  x$pt_acec_planning_framework <- reference$planning_framework[reference_index]
  x$pt_acec_source_admin_unit <- reference$source_administrative_unit[reference_index]
  office_order <- stats::setNames(
    as.integer(current_field_offices$sort_order),
    current_field_offices$office_key
  )
  context_rows <- split(
    seq_len(nrow(field_office_context)),
    field_office_context$acec_id
  )
  context_values <- lapply(x$acec_id, function(acec_id) {
    rows <- context_rows[[acec_id]]
    if (is.null(rows) || !length(rows)) {
      return(list(keys = "", names = ""))
    }
    keys <- pt_local_reference_clean_chr(
      field_office_context$current_field_office_key[rows]
    )
    names <- pt_local_reference_clean_chr(
      field_office_context$current_field_office_name[rows]
    )
    order_index <- order(office_order[keys])
    list(
      keys = paste(unique(keys[order_index]), collapse = ";"),
      names = paste(unique(names[order_index]), collapse = "; ")
    )
  })
  x$pt_acec_field_office_context <- vapply(
    context_values, `[[`, character(1), "keys"
  )
  x$pt_acec_field_office_context_names <- vapply(
    context_values, `[[`, character(1), "names"
  )
  if (any(!nzchar(x$pt_acec_field_office_context)) ||
      any(!nzchar(x$pt_acec_field_office_context_names))) {
    stop("Every ACEC must retain at least one reviewed current field-office context.")
  }
  x$pt_acec_value_families <- reference$value_families[reference_index]
  x$pt_acec_designation_status <- reference$designation_status[reference_index]
  x$pt_acec_designation_status_label <- reference$designation_status_label[reference_index]
  x$pt_acec_designation_year <- pt_local_reference_clean_chr(reference$designation_year[reference_index])
  x$pt_acec_gis_acres <- suppressWarnings(as.numeric(get_field("gis_acres")))
  x$pt_acec_calculated_acres <- suppressWarnings(as.numeric(reference$calculated_source_geometry_acres[reference_index]))
  x$pt_acec_source_modified_date <- pt_local_reference_format_date(get_field("modify_date"))
  x$pt_acec_last_edited_date <- pt_local_reference_format_date(get_field("last_edited_date"))
  x$pt_local_reference_feature_key <- x$acec_id
  x$pt_local_reference_semantic_key <- x$acec_id
  x$pt_local_reference_geometry_key <- x$component_id
  x$pt_local_reference_geometry_components <- as.integer(
    components$component_supplied_geometry_part_count[component_index]
  )
  x <- pt_local_reference_apply_category_tokens(
    x, "acec", rep("acec", nrow(x))
  )
  if (isTRUE(build_display)) {
    x$pt_reference_label_text <- x$pt_acec_official_name
    x$pt_reference_hover_html <- pt_local_reference_acec_hover_html(x)
    x$pt_reference_hover_text <- paste(
      x$pt_acec_official_name,
      pt_local_reference_format_square_miles_from_acres(x$pt_acec_gis_acres),
      sep = " · "
    )
  }
  if (isTRUE(validate_snapshot)) {
    if (nrow(x) != 238L || length(unique(x$acec_id)) != 238L ||
        anyDuplicated(x$component_id) ||
        sum(x$pt_local_reference_geometry_components) != 613L ||
        !all(x$pt_local_reference_category_key == "acec")) {
      stop("Prepared ACEC snapshot differs from the reviewed 238-designation/613-part contract.")
    }
  }
  if (isTRUE(build_display)) pt_local_reference_acec_runtime_geometry(x) else x
}

pt_local_reference_acec_qa <- function(x) {
  data.frame(
    metric = c(
      "mapped_components", "semantic_acecs", "geometry_parts",
      "multipart_acecs", "drecp_acecs", "fish_or_aquatic_acecs",
      "rna_name_candidates_not_verified"
    ),
    value = c(
      nrow(x), length(unique(x$acec_id)),
      sum(x$pt_local_reference_geometry_components),
      sum(x$pt_local_reference_geometry_components > 1L),
      length(unique(x$acec_id[x$pt_acec_planning_framework == "drecp"])),
      length(unique(x$acec_id[grepl("(^|;)water_aquatic(;|$)", x$pt_acec_value_families)])),
      3L
    ),
    stringsAsFactors = FALSE
  )
}

pt_local_reference_acec_popup_payload <- function(
  components = pt_local_reference_acec_components(),
  reference = pt_local_reference_acec_reference(),
  values = pt_local_reference_acec_values(),
  documents = pt_local_reference_acec_documents(),
  management = pt_local_reference_acec_management(),
  planning = pt_local_reference_acec_planning(),
  relationships = pt_local_reference_acec_relationships(),
  sources = pt_local_reference_acec_sources(),
  offices = pt_local_reference_acec_offices(),
  current_field_offices = pt_local_reference_acec_current_field_offices(),
  field_office_context = pt_local_reference_acec_field_office_context(),
  access = pt_local_reference_acec_access(),
  wsa_name_context = pt_local_reference_acec_wsa_name_context(),
  overlap_pairs = pt_local_reference_acec_overlap_pairs()
) {
  clean <- pt_local_reference_clean_chr
  to_records <- function(df, fields) {
    lapply(seq_len(nrow(df)), function(i) {
      stats::setNames(lapply(fields, function(field) {
        value <- df[[field]][[i]]
        if (is.numeric(value) || is.integer(value) || is.logical(value)) value else clean(value)
      }), fields)
    })
  }
  wsa_index <- match(reference$acec_id, wsa_name_context$acec_id)
  wsa_fields <- c(
    "wsa_name_context_status", "current_wsa_name", "current_wsa_nlcs_id",
    "current_wsa_global_id", "current_wsa_feature_key", "comparison_inventory",
    "comparison_source_url", "comparison_date", "match_method", "popup_wording"
  )
  for (field in wsa_fields) {
    reference[[field]] <- ""
    matched <- !is.na(wsa_index)
    reference[[field]][matched] <- clean(wsa_name_context[[field]][wsa_index[matched]])
  }
  semantic_fields <- c(
    "acec_id", "official_acec_name", "alternate_names", "designation_status_label",
    "rna_relationship_status", "source_geometry_part_count", "official_acres",
    "current_gis_acres", "calculated_source_geometry_acres", "blm_districts",
    "blm_field_offices", "source_administrative_unit", "current_governing_plans",
    "designation_decision_date", "designation_year", "designation_authority",
    "designation_document_url", "designation_document_page",
    "designation_history_summary", "relevant_and_important_values_summary",
    "value_families", "management_direction_summary", "access_and_land_status_summary",
    "counties", "states", "planning_framework", "water_resource_relevance",
    "water_resource_relevance_summary", "current_official_page_url",
    "current_official_map_url", "current_plan_url", "current_office_url",
    "research_status", "last_verified", "record_confidence", "unit_summary_short",
    "source_geometry_provenance", "wsa_name_context_status", "current_wsa_name",
    "current_wsa_nlcs_id", "current_wsa_global_id", "current_wsa_feature_key",
    "comparison_inventory", "comparison_source_url", "comparison_date",
    "match_method", "popup_wording"
  )
  list(
    semantics = to_records(reference, semantic_fields),
    components = to_records(components, c(
      "component_id", "acec_id", "source_globalid", "source_objectid", "source_name",
      "source_feature_type", "source_designation_code", "source_status_code",
      "source_agency_code", "component_gis_acres", "component_calculated_acres",
      "component_blm_district", "component_blm_field_office", "component_governing_plan",
      "component_source_modified_date", "component_blm_modify_date",
      "component_supplied_geometry_part_count", "component_supplied_geometry_valid"
    )),
    values = to_records(values, c(
      "acec_id", "component_id", "value_record_id", "value_family", "value_type",
      "value_name", "value_scope", "value_is_primary", "value_description",
      "value_source_document", "value_source_url", "value_source_page",
      "value_source_section", "value_last_verified", "value_confidence",
      "water_resource_connection", "provenance_class"
    )),
    documents = to_records(documents, c(
      "document_id", "document_type", "document_title", "nepa_number",
      "document_date", "document_status", "source_agency", "document_url",
      "url_status", "relevant_pages", "retrieval_date", "associated_acec_count"
    )),
    management = to_records(management, c(
      "acec_id", "component_id", "prescription_id", "management_category",
      "management_action_type", "management_direction", "management_scope",
      "geographic_scope", "seasonal_scope", "exception_or_condition",
      "current_or_historical", "governing_document", "governing_document_url",
      "governing_document_page", "governing_document_section", "last_verified",
      "confidence", "provenance_class"
    )),
    planning = to_records(planning, c(
      "acec_id", "component_id", "planning_action_id", "planning_action_type",
      "planning_action_title", "planning_action_date", "planning_action_status",
      "planning_action_effect", "prior_status", "resulting_status", "boundary_effect",
      "acreage_before", "acreage_after", "document_id", "document_title",
      "document_url", "document_page", "eplanning_project_id",
      "eplanning_project_url", "source_agency", "last_verified", "confidence"
    )),
    relationships = to_records(relationships, c(
      "acec_id", "component_id", "related_layer_family", "related_feature_id",
      "related_feature_name", "relationship_type", "relationship_scope",
      "relationship_method", "relationship_source", "relationship_source_url",
      "spatial_derivation_required", "intersection_area_acres",
      "intersection_percent_of_component", "intersection_percent_of_acec",
      "relationship_last_verified", "confidence", "relationship_context_class"
    )),
    offices = to_records(offices, c(
      "acec_id", "component_id", "source_admin_unit_code", "source_admin_unit_label",
      "responsible_blm_district", "responsible_blm_field_office", "assignment_type",
      "boundary_dataset", "dataset_date", "intersection_method", "overlap_acres",
      "overlap_percent", "multiple_offices_intersect", "crosses_office_boundary",
      "evidence_url", "derivation_date", "confidence", "manual_review_status"
    )),
    current_field_offices = to_records(current_field_offices, c(
      "office_key", "office_code", "current_official_name", "boundary_source_name",
      "parent_district_code", "parent_district_name", "official_office_url",
      "roster_source_url", "roster_verified_on", "boundary_globalid",
      "boundary_source", "boundary_source_url", "boundary_snapshot_date",
      "current_roster_status", "sort_order"
    )),
    field_office_context = to_records(field_office_context, c(
      "acec_id", "component_id", "current_field_office_key",
      "current_field_office_code", "current_field_office_name",
      "intersection_area_m2", "intersection_area_acres", "percent_of_acec_area",
      "acec_context_class", "source_context_qa_class", "relationship_method",
      "minimum_intersection_area_m2", "complete_coverage_percent",
      "boundary_snapshot_date", "derivation_date", "confidence"
    )),
    access = to_records(access, c(
      "acec_id", "component_id", "boundary_represents", "designation_applies_to",
      "surface_management_scope", "mineral_estate_scope", "includes_non_blm_land",
      "non_blm_land_summary", "public_access_status", "public_access_scope",
      "access_information_source", "access_information_source_url",
      "access_information_verified_on", "access_information_confidence",
      "land_status_caveat", "last_verified"
    )),
    overlap_pairs = to_records(overlap_pairs, c(
      "acec_id_a", "acec_id_b", "overlap_area_m2", "overlap_area_acres",
      "percent_of_smaller_acec", "relationship_type"
    )),
    overlap_style = PT_LOCAL_REFERENCE_ACEC_OVERLAP_STYLE,
    field_office_presentation = list(
      additional_office_minimum_percent =
        PT_LOCAL_REFERENCE_ACEC_FIELD_OFFICE_CONTEXT_METADATA$
          presentation_additional_office_minimum_percent,
      rule = PT_LOCAL_REFERENCE_ACEC_FIELD_OFFICE_CONTEXT_METADATA$presentation_rule
    ),
    area_presentation = PT_LOCAL_REFERENCE_ACEC_AREA_PRESENTATION,
    sources = to_records(sources, intersect(c(
      "source_title", "source_type", "agency", "url", "retrieved_or_verified",
      "authoritative_scope", "local_snapshot", "status", "notes"
    ), names(sources))),
    caveats = list(
      boundary = PT_LOCAL_REFERENCE_ACEC_BOUNDARY_CAVEAT,
      management = PT_LOCAL_REFERENCE_ACEC_MANAGEMENT_CAVEAT,
      field_office_context = paste(
        "Derived from spatial intersection with verified current BLM field-office",
        "boundaries; this does not by itself establish administrative responsibility."
      ),
      rna = paste(
        "Three ACEC names are RNA research candidates only.",
        "RNA status is not verified and is not offered as a map filter."
      )
    )
  )
}

# ---- California Desert National Conservation Lands -------------------------

pt_local_reference_desert_ncl_reference <- function(
  path = PT_LOCAL_REFERENCE_DESERT_NCL_REFERENCE_PATH
) {
  pt_local_reference_read_csv(path, c(
    "nlcs_id", "semantic_unit_id", "raw_source_name",
    "standardized_display_name", "mapped_unit_class",
    "mapped_unit_interpretation", "semantic_interpretation_confidence",
    "individual_establishment_status", "system_level_statutory_authority",
    "drecp_decision_authority", "named_unit_or_boundary_authority",
    "planning_context", "geographic_description",
    "directly_supported_conservation_values",
    "directly_supported_management_objectives", "administering_agency",
    "blm_role", "blm_role_summary", "responsible_blm_district",
    "responsible_blm_office", "responsible_office_status",
    "intersecting_offices_summary", "official_reported_acres",
    "official_reported_area_scope", "area_provenance", "program_page_url",
    "unit_specific_page_url", "office_page_url", "primary_plan_title",
    "primary_plan_url", "official_source_layer_url",
    "access_and_route_caveat", "land_status_caveat", "boundary_caveat",
    "relationship_summary", "source_limitations", "last_verification_date"
  ))
}

pt_local_reference_desert_ncl_aliases <- function(
  path = PT_LOCAL_REFERENCE_DESERT_NCL_ALIASES_PATH
) {
  pt_local_reference_read_csv(path, c(
    "nlcs_id", "alias", "alias_type", "source", "recommended_for_search",
    "replaces_source_name", "confidence", "notes"
  ))
}

pt_local_reference_desert_ncl_policy <- function(
  path = PT_LOCAL_REFERENCE_DESERT_NCL_POLICY_PATH
) {
  pt_local_reference_read_csv(path, c(
    "policy_id", "recommended_language", "use", "last_verified"
  ))
}

pt_local_reference_desert_ncl_documents <- function(
  path = PT_LOCAL_REFERENCE_DESERT_NCL_DOCUMENTS_PATH
) {
  pt_local_reference_read_csv(path, c(
    "document_id", "exact_title", "agency_or_publisher", "document_type",
    "publication_date", "decision_date", "status", "direct_document_url",
    "official_landing_page_url", "applicability", "verification_date",
    "limitation_notes"
  ))
}

pt_local_reference_desert_ncl_unit_documents <- function(
  path = PT_LOCAL_REFERENCE_DESERT_NCL_UNIT_DOCUMENTS_PATH
) {
  pt_local_reference_read_csv(path, c(
    "nlcs_id", "document_id", "relationship_type", "applicability",
    "unit_specific", "normal_popup_suitable", "evidence_locator", "notes"
  ))
}

pt_local_reference_desert_ncl_sources <- function(
  path = PT_LOCAL_REFERENCE_DESERT_NCL_SOURCES_PATH
) {
  pt_local_reference_read_csv(path, c(
    "source_id", "source_title", "publisher", "source_type",
    "authority_level", "url", "applicable_units", "claims_or_fields_supported",
    "verification_date", "limitations"
  ))
}

pt_local_reference_desert_ncl_research_relationships <- function(
  path = PT_LOCAL_REFERENCE_DESERT_NCL_RESEARCH_RELATIONSHIPS_PATH
) {
  pt_local_reference_read_csv(path, c(
    "relationship_id", "nlcs_id", "relationship_type",
    "related_brim_layer_family", "related_feature_identifier",
    "related_feature_name", "source_or_spatial_derivation_method",
    "evidence_title", "evidence_url", "verification_date", "confidence",
    "brim_should_calculate_from_current_local_data", "normal_popup_suitable",
    "notes"
  ))
}

pt_local_reference_desert_ncl_field_offices <- function(
  path = PT_LOCAL_REFERENCE_DESERT_NCL_FIELD_OFFICE_LOOKUP_PATH
) {
  pt_local_reference_read_csv(path, c(
    "office_key", "office_code", "office_name", "office_globalid"
  ))
}

pt_local_reference_desert_ncl_field_office_context <- function(
  path = PT_LOCAL_REFERENCE_DESERT_NCL_FIELD_OFFICE_CONTEXT_PATH
) {
  pt_local_reference_read_csv(path, c(
    "nlcs_id", "office_key", "office_code", "office_name", "office_globalid",
    "intersection_area_m2", "intersection_area_acres", "percent_of_unit_area",
    "relationship_method", "context_only_not_management_assignment",
    "display_context"
  ))
}

pt_local_reference_desert_ncl_related_context <- function(
  path = PT_LOCAL_REFERENCE_DESERT_NCL_RELATED_CONTEXT_PATH
) {
  pt_local_reference_read_csv(path, c(
    "nlcs_id", "related_layer_key", "related_layer_label",
    "related_feature_id", "related_feature_name", "relationship_type",
    "overlap_area_m2", "overlap_area_acres", "percent_of_unit_area",
    "intersection_length_m", "intersection_length_miles", "derivation_method",
    "normal_popup_suitable", "identity_and_geometry_remain_in_related_layer",
    "management_inference_prohibited"
  ))
}

pt_validate_local_reference_desert_ncl_research <- function(
  reference = pt_local_reference_desert_ncl_reference(),
  aliases = pt_local_reference_desert_ncl_aliases(),
  policy = pt_local_reference_desert_ncl_policy(),
  documents = pt_local_reference_desert_ncl_documents(),
  unit_documents = pt_local_reference_desert_ncl_unit_documents(),
  sources = pt_local_reference_desert_ncl_sources(),
  research_relationships = pt_local_reference_desert_ncl_research_relationships(),
  offices = pt_local_reference_desert_ncl_field_offices(),
  office_context = pt_local_reference_desert_ncl_field_office_context(),
  related_context = pt_local_reference_desert_ncl_related_context()
) {
  expected_ids <- sprintf("NLCS%06d", 2009:2019)
  if (nrow(reference) != 11L || anyDuplicated(reference$nlcs_id) ||
      !identical(sort(reference$nlcs_id), expected_ids)) {
    stop("California Desert NCL reference must retain the exact 11 semantic IDs.")
  }
  if (sum(reference$mapped_unit_class == "drecp_ecoregion_subarea_allocation") != 10L ||
      reference$mapped_unit_class[reference$nlcs_id == "NLCS002012"] !=
        "existing_ncl_component_candidate") {
    stop("California Desert NCL must retain ten DRECP subareas and the separate Desert Lily source record.")
  }
  if (nrow(aliases) != 24L || nrow(policy) != 9L || nrow(documents) != 13L ||
      nrow(unit_documents) != 112L || nrow(sources) != 19L ||
      nrow(research_relationships) != 112L) {
    stop("California Desert NCL canonical package lookup counts changed.")
  }
  if (anyDuplicated(aliases[c("nlcs_id", "alias", "alias_type")]) ||
      anyDuplicated(documents$document_id) ||
      anyDuplicated(unit_documents[c("nlcs_id", "document_id")]) ||
      anyDuplicated(policy$policy_id) || anyDuplicated(sources$source_id) ||
      anyDuplicated(research_relationships$relationship_id)) {
    stop("California Desert NCL lookup keys are not unique at their declared grain.")
  }
  if (!all(aliases$nlcs_id %in% expected_ids) ||
      !all(unit_documents$nlcs_id %in% expected_ids) ||
      !all(unit_documents$document_id %in% documents$document_id) ||
      !all(research_relationships$nlcs_id %in% expected_ids)) {
    stop("California Desert NCL package lookup coverage is inconsistent.")
  }
  if (nrow(offices) != 7L || anyDuplicated(offices$office_key) ||
      nrow(office_context) != 26L ||
      !setequal(unique(office_context$nlcs_id), expected_ids) ||
      any(suppressWarnings(as.numeric(office_context$intersection_area_m2)) <= 0) ||
      !setequal(unique(office_context$office_key), offices$office_key)) {
    stop("California Desert NCL current field-office context differs from the focused derivation.")
  }
  display_context <- tolower(pt_local_reference_clean_chr(
    office_context$display_context
  )) == "true"
  if (!setequal(unique(office_context$nlcs_id[display_context]), expected_ids)) {
    stop("Every California Desert NCL unit requires a displayed field-office context.")
  }
  management_prohibited <- tolower(pt_local_reference_clean_chr(
    related_context$management_inference_prohibited
  )) == "true"
  if (nrow(related_context) != 277L ||
      !setequal(unique(related_context$nlcs_id), expected_ids) ||
      !all(management_prohibited)) {
    stop("California Desert NCL related-layer context differs from the focused no-management-inference contract.")
  }
  desert_lily <- related_context[
    related_context$nlcs_id == "NLCS002012" &
      related_context$related_layer_key == "acec" &
      grepl("^Desert Lily Preserve$", related_context$related_feature_name),
    , drop = FALSE
  ]
  if (nrow(desert_lily) != 1L ||
      suppressWarnings(as.numeric(desert_lily$percent_of_unit_area)) < 99) {
    stop("Desert Lily source-record/ACEC spatial relationship changed unexpectedly.")
  }
  invisible(TRUE)
}

pt_local_reference_desert_ncl_part_count <- function(x) {
  crs <- sf::st_crs(x)
  vapply(sf::st_geometry(x), function(geometry) {
    length(suppressWarnings(sf::st_cast(
      sf::st_sfc(geometry, crs = crs), "POLYGON"
    )))
  }, integer(1))
}

pt_local_reference_desert_ncl_runtime_geometry <- function(x) {
  sf_column <- attr(x, "sf_column")
  if (is.null(sf_column) || !sf_column %in% names(x)) {
    sf_column <- names(x)[vapply(x, inherits, logical(1), what = "sfc")][[1]]
  }
  keep <- c(
    "pt_nickname", "pt_display_name", "pt_geom_type", "NLCS_ID",
    "semantic_unit_id", "component_id", "pt_local_reference_feature_key",
    "pt_local_reference_semantic_key", "pt_local_reference_geometry_key",
    "pt_local_reference_geometry_components", "pt_local_reference_category_key",
    "pt_local_reference_category_label", "fill_col", "line_col",
    "fill_opacity", "line_weight", "line_dash", "pt_legend_swatch_style",
    "pt_cdncl_display_name", "pt_cdncl_raw_name", "pt_cdncl_aliases",
    "pt_cdncl_global_id", "pt_cdncl_source_objectid",
    "pt_cdncl_unit_type_key", "pt_cdncl_unit_type_label",
    "pt_cdncl_field_office_keys", "pt_cdncl_field_office_names",
    "pt_cdncl_field_office_context_class", "pt_cdncl_relationship_families",
    "pt_cdncl_related_designation_facets",
    "pt_cdncl_monument_overlap", "pt_cdncl_source_shape_area_m2",
    "pt_cdncl_calculated_raw_area_acres", "pt_cdncl_official_reported_acres",
    "pt_cdncl_last_verified", "pt_reference_label_text",
    "pt_reference_hover_html", "pt_reference_hover_text", sf_column
  )
  missing <- setdiff(keep, names(x))
  if (length(missing)) {
    stop("California Desert NCL runtime geometry is missing: ", paste(missing, collapse = ", "))
  }
  metadata <- attr(x, "pt_desert_ncl_candidate_metadata")
  out <- x[, keep, drop = FALSE]
  attr(out, "pt_desert_ncl_candidate_metadata") <- metadata
  out
}

pt_local_reference_desert_ncl_related_facet_values <- function(
  nlcs_ids,
  related_context = pt_local_reference_desert_ncl_related_context()
) {
  required <- c(
    "nlcs_id", "related_layer_key", "normal_popup_suitable"
  )
  missing <- setdiff(required, names(related_context))
  if (length(missing)) {
    stop(
      "California Desert NCL related-facet context is missing: ",
      paste(missing, collapse = ", ")
    )
  }
  suitable <- tolower(pt_local_reference_clean_chr(
    related_context$normal_popup_suitable
  )) == "true"
  rows <- related_context[suitable, , drop = FALSE]
  keys <- pt_local_reference_clean_chr(rows$related_layer_key)
  keys[grepl("^wsr_", keys)] <- "wild_scenic_river"
  accepted_order <- c(
    "acec", "federal_wilderness", "national_monuments",
    "wilderness_study_areas", "national_trails", "wild_scenic_river"
  )
  unknown <- setdiff(unique(keys[nzchar(keys)]), accepted_order)
  if (length(unknown)) {
    stop(
      "California Desert NCL related-facet context has unknown families: ",
      paste(sort(unknown), collapse = ", ")
    )
  }
  vapply(nlcs_ids, function(nlcs_id) {
    present <- unique(keys[rows$nlcs_id == nlcs_id])
    paste(accepted_order[accepted_order %in% present], collapse = "|")
  }, character(1))
}

pt_prepare_local_reference_desert_ncl <- function(
  x,
  validate_snapshot = FALSE,
  build_display = TRUE
) {
  pt_validate_local_reference_config()
  if (!inherits(x, "sf") || nrow(x) != 11L) {
    stop("California Desert NCL preparation requires the 11-row focused sf candidate.")
  }
  required <- c(
    "NLCS_ID", "NLCS_NAME", "GlobalID", "OBJECTID", "semantic_unit_id",
    "component_id", "pt_cdncl_display_name", "pt_cdncl_raw_name",
    "pt_cdncl_aliases", "pt_cdncl_global_id", "pt_cdncl_source_objectid",
    "pt_cdncl_unit_type_key", "pt_cdncl_unit_type_label",
    "pt_cdncl_field_office_keys", "pt_cdncl_field_office_names",
    "pt_cdncl_field_office_context_class", "pt_cdncl_relationship_families",
    "pt_cdncl_monument_overlap", "pt_cdncl_source_shape_area_m2",
    "pt_cdncl_calculated_raw_area_acres", "pt_cdncl_official_reported_acres",
    "pt_cdncl_last_verified", "pt_reference_label_text",
    "pt_reference_hover_text"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("California Desert NCL candidate is missing: ", paste(missing, collapse = ", "))
  }
  expected_ids <- sprintf("NLCS%06d", 2009:2019)
  metadata <- attr(x, "pt_desert_ncl_candidate_metadata")
  if (!identical(as.character(x$NLCS_ID), expected_ids) ||
      anyDuplicated(x$component_id) || anyDuplicated(x$GlobalID) ||
      is.null(metadata) || !identical(as.numeric(metadata$simplify_tolerance_m), 2) ||
      !identical(as.integer(metadata$raw_polygon_parts), 173L) ||
      !identical(as.integer(metadata$raw_holes), 32L) ||
      !identical(as.integer(metadata$raw_vertices), 84155L) ||
      !identical(as.integer(metadata$display_polygon_parts), 173L) ||
      !identical(as.integer(metadata$display_holes), 32L) ||
      !identical(as.integer(metadata$display_vertices), 32168L) ||
      !isTRUE(metadata$exact_part_retention) ||
      !isTRUE(metadata$exact_hole_retention) ||
      any(!sf::st_is_valid(x)) || any(sf::st_is_empty(x))) {
    stop("California Desert NCL candidate metadata or geometry differs from the accepted focused contract.")
  }
  if (isTRUE(validate_snapshot)) pt_validate_local_reference_desert_ncl_research()
  categories <- pt_local_reference_categories("ca_desert_ncl")
  category <- categories[categories$category_key == "ca_desert_ncl", , drop = FALSE]
  x$pt_nickname <- "cadesert_ncl"
  x$pt_display_name <- "CA Desert National Conservation Lands"
  x$pt_geom_type <- "polygon"
  x$pt_local_reference_feature_key <- x$NLCS_ID
  x$pt_local_reference_semantic_key <- x$NLCS_ID
  x$pt_local_reference_geometry_key <- x$component_id
  x$pt_local_reference_geometry_components <- pt_local_reference_desert_ncl_part_count(x)
  x$pt_local_reference_category_key <- "ca_desert_ncl"
  x$pt_local_reference_category_label <- category$label[[1]]
  x$fill_col <- category$fill_color[[1]]
  x$line_col <- category$stroke_color[[1]]
  x$fill_opacity <- category$fill_opacity[[1]]
  x$line_weight <- category$stroke_weight[[1]]
  x$line_dash <- category$dash_array[[1]]
  x$pt_legend_swatch_style <- category$legend_swatch_style[[1]]
  x$pt_cdncl_related_designation_facets <-
    pt_local_reference_desert_ncl_related_facet_values(x$NLCS_ID)
  x$pt_reference_hover_html <- paste0(
    "<div class=\"pt-cdncl-hover\"><strong>",
    htmltools::htmlEscape(x$pt_cdncl_display_name), "</strong><br>",
    htmltools::htmlEscape(x$pt_cdncl_unit_type_label),
    "<br>Field-office context: ",
    htmltools::htmlEscape(x$pt_cdncl_field_office_names), "</div>"
  )
  attr(x, "pt_desert_ncl_candidate_metadata") <- metadata
  if (isTRUE(build_display)) pt_local_reference_desert_ncl_runtime_geometry(x) else x
}

pt_local_reference_desert_ncl_qa <- function(x) {
  required <- c(
    "NLCS_ID", "component_id", "pt_cdncl_display_name",
    "pt_cdncl_unit_type_key", "pt_cdncl_field_office_names",
    "pt_cdncl_monument_overlap", "pt_cdncl_related_designation_facets",
    "pt_local_reference_geometry_components"
  )
  missing <- setdiff(required, names(x))
  if (!inherits(x, "sf") || length(missing)) {
    stop("California Desert NCL QA requires prepared runtime geometry: ", paste(missing, collapse = ", "))
  }
  metadata <- attr(x, "pt_desert_ncl_candidate_metadata")
  data.frame(
    nlcs_id = x$NLCS_ID,
    component_id = x$component_id,
    display_name = x$pt_cdncl_display_name,
    mapped_unit_type = x$pt_cdncl_unit_type_key,
    field_office_context = x$pt_cdncl_field_office_names,
    monument_overlap = x$pt_cdncl_monument_overlap,
    related_designation_facets = x$pt_cdncl_related_designation_facets,
    geometry_parts = x$pt_local_reference_geometry_components,
    geometry_valid = sf::st_is_valid(x),
    geometry_empty = sf::st_is_empty(x),
    simplify_tolerance_m = as.numeric(metadata$simplify_tolerance_m),
    raw_vertices = as.integer(metadata$raw_vertices),
    display_vertices = as.integer(metadata$display_vertices),
    stringsAsFactors = FALSE
  )
}

pt_local_reference_desert_ncl_popup_payload <- function(
  x,
  reference = pt_local_reference_desert_ncl_reference(),
  aliases = pt_local_reference_desert_ncl_aliases(),
  policy = pt_local_reference_desert_ncl_policy(),
  documents = pt_local_reference_desert_ncl_documents(),
  unit_documents = pt_local_reference_desert_ncl_unit_documents(),
  sources = pt_local_reference_desert_ncl_sources(),
  research_relationships = pt_local_reference_desert_ncl_research_relationships(),
  offices = pt_local_reference_desert_ncl_field_offices(),
  office_context = pt_local_reference_desert_ncl_field_office_context(),
  related_context = pt_local_reference_desert_ncl_related_context()
) {
  pt_validate_local_reference_desert_ncl_research(
    reference, aliases, policy, documents, unit_documents, sources,
    research_relationships, offices, office_context, related_context
  )
  to_records <- function(data, fields = names(data)) {
    data <- data[, intersect(fields, names(data)), drop = FALSE]
    lapply(seq_len(nrow(data)), function(i) as.list(data[i, , drop = FALSE]))
  }
  research_popup <- research_relationships[
    grepl("^true", tolower(research_relationships$normal_popup_suitable)),
    , drop = FALSE
  ]
  list(
    semantics = to_records(reference),
    components = to_records(sf::st_drop_geometry(x), c(
      "component_id", "NLCS_ID", "pt_cdncl_global_id",
      "pt_cdncl_source_objectid", "pt_cdncl_raw_name",
      "pt_cdncl_calculated_raw_area_acres", "pt_cdncl_official_reported_acres",
      "pt_cdncl_unit_type_key", "pt_cdncl_unit_type_label",
      "pt_cdncl_last_verified"
    )),
    aliases = to_records(aliases),
    policy = to_records(policy),
    documents = to_records(documents),
    unit_documents = to_records(unit_documents),
    sources = to_records(sources),
    research_relationships = to_records(research_popup),
    offices = to_records(offices),
    field_office_context = to_records(office_context),
    related_context = to_records(related_context),
    caveats = list(
      office = paste(
        "Field-office names are positive-area spatial context from current",
        "accepted BRIM boundaries, not management assignments."
      ),
      relationships = paste(
        "Spatial relationships do not merge designation identities, copy",
        "related geometry, or infer management authority."
      ),
      desert_lily = paste(
        "The BLM source record, current ACEC, and statutory Desert Lily",
        "Sanctuary are related but remain separate identities."
      )
    )
  )
}

pt_local_reference_semantic_feature_catalog <- function(x, registry_row) {
  if (!inherits(x, "sf")) {
    stop("Local Reference semantic-feature bounds require an sf object.")
  }
  required <- c(
    "pt_local_reference_feature_key", "pt_local_reference_semantic_key",
    "pt_local_reference_category_key", "pt_local_reference_geometry_components"
  )
  missing_fields <- setdiff(required, names(x))
  if (length(missing_fields)) {
    stop(
      "Local Reference semantic-feature catalog is missing: ",
      paste(missing_fields, collapse = ", ")
    )
  }

  display_field <- as.character(registry_row$feature_display_field[[1]])
  if (!nzchar(display_field) || !display_field %in% names(x)) {
    stop(
      registry_row$display_name[[1]],
      " requires feature display field `", display_field, "`."
    )
  }
  search_fields <- unlist(
    registry_row$feature_search_fields[[1]],
    use.names = FALSE
  )
  missing_search <- setdiff(search_fields, names(x))
  if (length(missing_search)) {
    stop(
      registry_row$display_name[[1]],
      " is missing approved feature-search field(s): ",
      paste(missing_search, collapse = ", ")
    )
  }

  semantic_keys <- as.character(x$pt_local_reference_semantic_key)
  if (any(!nzchar(semantic_keys))) {
    stop("Local Reference semantic feature keys must be nonblank.")
  }
  semantic_order <- unique(semantic_keys)
  x_wgs84 <- if (isTRUE(sf::st_crs(x) == sf::st_crs(4326))) {
    x
  } else {
    sf::st_transform(x, 4326)
  }
  attributes <- sf::st_drop_geometry(x)

  lapply(semantic_order, function(semantic_key) {
    index <- which(semantic_keys == semantic_key)
    display_values <- pt_local_reference_clean_chr(
      attributes[[display_field]][index]
    )
    display_values <- display_values[nzchar(display_values)]
    display_name <- if (length(display_values)) {
      display_values[[1]]
    } else {
      semantic_key
    }
    search_values <- unlist(
      attributes[index, search_fields, drop = FALSE],
      use.names = FALSE
    )
    search_text <- tolower(paste(unique(pt_local_reference_clean_chr(c(
      display_name,
      search_values,
      attributes$pt_local_reference_feature_key[index],
      semantic_key
    ))), collapse = " "))
    bounds <- sf::st_bbox(x_wgs84[index, , drop = FALSE])
    bounds_vector <- unname(as.numeric(c(
      bounds[["ymin"]], bounds[["xmin"]],
      bounds[["ymax"]], bounds[["xmax"]]
    )))
    if (length(bounds_vector) != 4L || any(!is.finite(bounds_vector))) {
      stop("Invalid semantic-feature bounds for ", semantic_key, ".")
    }
    list(
      semantic_feature_key = semantic_key,
      feature_key = as.character(
        attributes$pt_local_reference_feature_key[index[[1]]]
      ),
      display_name = display_name,
      category_keys = unique(as.character(
        attributes$pt_local_reference_category_key[index]
      )),
      search_text = search_text,
      semantic_feature_bounds = bounds_vector,
      geometry_component_count = sum(as.integer(
        attributes$pt_local_reference_geometry_components[index]
      ))
    )
  })
}

pt_local_reference_semantic_label_payload <- function(
    labels_all,
    x,
    registry_row) {
  layer_id <- as.character(registry_row$layer_id[[1]])
  registration <- pt_local_reference_label_registration(layer_id = layer_id)
  if (!isTRUE(registration$lbl_available[[1]])) return(NULL)
  label_id <- as.character(registration$label_id[[1]])
  if (!is.list(labels_all) || !label_id %in% names(labels_all)) {
    stop(
      registry_row$display_name[[1]],
      " requires registered semantic label child `", label_id, "`."
    )
  }
  labels <- labels_all[[label_id]]
  required <- c(
    "label_id", "label_group", "label_text", "semantic_feature_key",
    "geometry_key", "label_record_key", "anchor_strategy",
    "anchor_priority", "min_zoom", "max_zoom"
  )
  missing <- setdiff(required, names(labels))
  if (!inherits(labels, "sf") || !nrow(labels) || length(missing)) {
    stop(
      registry_row$display_name[[1]],
      " semantic label child is invalid; missing: ",
      paste(missing, collapse = ", ")
    )
  }
  expected_semantic <- unique(as.character(
    x$pt_local_reference_semantic_key
  ))
  label_semantic <- unique(as.character(labels$semantic_feature_key))
  if (!setequal(expected_semantic, label_semantic)) {
    stop(
      registry_row$display_name[[1]],
      " semantic label IDs do not match the map cache."
    )
  }
  visible_component_aware <- isTRUE(
    registration$visible_component_aware[[1]]
  )
  if (visible_component_aware && !all(
    labels$geometry_key %in% x$pt_local_reference_geometry_key
  )) {
    stop(
      registry_row$display_name[[1]],
      " label anchors contain unknown geometry-component IDs."
    )
  }
  coords <- sf::st_coordinates(sf::st_transform(labels, 4326))
  if (nrow(coords) != nrow(labels) || any(!is.finite(coords[, 1:2]))) {
    stop(registry_row$display_name[[1]], " label anchors are not valid points.")
  }
  label_groups <- unique(as.character(labels$label_group))
  strategies <- unique(as.character(labels$anchor_strategy))
  min_zoom <- unique(as.numeric(labels$min_zoom))
  max_zoom <- unique(as.numeric(labels$max_zoom))
  if (length(label_groups) != 1L || length(strategies) != 1L ||
      length(min_zoom) != 1L || length(max_zoom) != 1L) {
    stop(registry_row$display_name[[1]], " label child metadata is inconsistent.")
  }
  records <- lapply(seq_len(nrow(labels)), function(index) {
    list(
      label_record_key = as.character(labels$label_record_key[[index]]),
      semantic_feature_key =
        as.character(labels$semantic_feature_key[[index]]),
      geometry_key = as.character(labels$geometry_key[[index]]),
      anchor_priority = as.integer(labels$anchor_priority[[index]])
    )
  })
  list(
    available = TRUE,
    label_id = label_id,
    label_group = sub(
      "^Labels:\\s*", "Labels – ", label_groups[[1]],
      perl = TRUE
    ),
    anchor_strategy = strategies[[1]],
    visible_component_aware = visible_component_aware,
    min_zoom = min_zoom[[1]],
    max_zoom = max_zoom[[1]],
    semantic_feature_count = length(label_semantic),
    anchor_count = nrow(labels),
    records = records
  )
}

pt_validate_local_reference_nps_context <- function(x) {
  if (is.null(x)) return(invisible(FALSE))
  if (!is.list(x) || !identical(sort(names(x)), c("boundaries", "land_interest"))) {
    stop("NPS Park/Preserve context must be a boundaries/land_interest list.")
  }
  required <- c(
    "unit_code", "unit_name", "unit_type_key", "unit_type_label", "states",
    "raw_tract_count", "nps_fee_tract_count",
    "nps_less_than_fee_tract_count", "other_federal_tract_count",
    "public_nonfederal_tract_count", "private_tract_count",
    "legislative_boundary_area_sq_mi", "displayed_land_interest_area_sq_mi",
    "official_page", "context_geometry_role", "context_geometry_key"
  )
  for (role in names(x)) {
    layer <- x[[role]]
    missing <- setdiff(required, names(layer))
    if (!inherits(layer, "sf") || nrow(layer) != 10L || length(missing) ||
        anyDuplicated(layer$unit_code) || anyDuplicated(layer$context_geometry_key) ||
        any(sf::st_is_empty(layer)) || any(!sf::st_is_valid(layer))) {
      stop(
        "NPS Park/Preserve context `", role,
        "` is invalid; missing fields: ", paste(missing, collapse = ", ")
      )
    }
    if (!setequal(
      as.character(layer$unit_code),
      c("CHIS", "DEVA", "JOTR", "KICA", "LAVO", "MOJA", "PINN", "REDW", "SEQU", "YOSE")
    ) || sum(layer$unit_type_key == "national_park") != 9L ||
        sum(layer$unit_type_key == "national_preserve") != 1L) {
      stop("NPS Park/Preserve context unit universe differs from 9 parks / 1 preserve.")
    }
  }
  metadata <- attr(x, "pt_nps_context_candidate_metadata")
  if (is.null(metadata) || as.integer(metadata$raw_tract_count) != 6207L ||
      as.integer(metadata$total_display_feature_count) != 20L ||
      !identical(as.numeric(metadata$selected_boundary_simplify_tolerance_m), 2) ||
      !identical(as.numeric(metadata$selected_land_interest_simplify_tolerance_m), 5) ||
      !isTRUE(metadata$exact_part_retention) ||
      !isTRUE(metadata$exact_hole_retention) ||
      isTRUE(metadata$production_release_authorized)) {
    stop("NPS Park/Preserve context metadata differs from the reviewed candidate contract.")
  }
  invisible(TRUE)
}

pt_local_reference_nps_context_group_name <- function(unit_type_key) {
  groups <- c(
    national_park = "Reference – National Monuments – NPS context – National Parks",
    national_preserve = "Reference – National Monuments – NPS context – National Preserves"
  )
  values <- unname(groups[as.character(unit_type_key)])
  if (anyNA(values)) stop("Unknown NPS context unit type key.")
  values
}

pt_local_reference_nps_context_payload <- function(x) {
  if (is.null(x)) return(NULL)
  pt_validate_local_reference_nps_context(x)
  units <- sf::st_drop_geometry(x$boundaries)
  groups <- lapply(c("national_park", "national_preserve"), function(key) {
    list(
      context_key = key,
      label = if (key == "national_park") "National Parks" else "National Preserves",
      group_name = pt_local_reference_nps_context_group_name(key),
      unit_count = sum(units$unit_type_key == key),
      expected_layer_count = 2L * sum(units$unit_type_key == key)
    )
  })
  stats::setNames(groups, c("national_park", "national_preserve"))
}

pt_local_reference_controller_payload <- function(
    reference_layers,
    labels_all = NULL,
    nps_context = NULL) {
  active <- LOCAL_REFERENCE_INTERACTION_REGISTRY[
    LOCAL_REFERENCE_INTERACTION_REGISTRY$implementation_status %in% c(
      "phase2_trails", "phase5_national_monuments", "phase6_desert_ncl", "phase1_wsa",
      "phase3_federal_wilderness", "phase4_acec"
    ),
    , drop = FALSE
  ]
  payload <- lapply(seq_len(nrow(active)), function(i) {
    row <- active[i, , drop = FALSE]
    x <- reference_layers[[row$source_nickname]]
    if (is.null(x) || !nrow(x)) return(NULL)
    is_federal_wilderness <- identical(
      as.character(row$layer_id[[1]]), "federal_wilderness"
    )
    is_acec <- identical(as.character(row$layer_id[[1]]), "acec")
    is_desert_ncl <- identical(
      as.character(row$layer_id[[1]]), "ca_desert_ncl"
    )
    is_national_monuments <- identical(
      as.character(row$layer_id[[1]]), "national_monuments"
    )
    fw_components <- fw_reference <- fw_designations <- fw_documents <-
      fw_policy <- fw_sources <- NULL
    if (is_federal_wilderness) {
      fw_components <- pt_local_reference_fw_components()
      fw_reference <- pt_local_reference_fw_reference()
      fw_designations <- pt_local_reference_fw_designations()
      fw_documents <- pt_local_reference_fw_documents()
      fw_policy <- pt_local_reference_fw_common_policy()
      fw_sources <- pt_local_reference_fw_source_register()
      pt_validate_local_reference_fw_research(
        fw_components, fw_reference, fw_designations, fw_documents,
        fw_policy, fw_sources
      )
    }
    acec_components <- acec_reference <- acec_values <- acec_documents <-
      acec_management <- acec_planning <- acec_relationships <- acec_sources <-
      acec_offices <- acec_current_field_offices <-
      acec_field_office_context <- acec_access <- acec_wsa_name_context <-
      acec_overlap_pairs <- NULL
    if (is_acec) {
      acec_components <- pt_local_reference_acec_components()
      acec_reference <- pt_local_reference_acec_reference()
      acec_values <- pt_local_reference_acec_values()
      acec_documents <- pt_local_reference_acec_documents()
      acec_management <- pt_local_reference_acec_management()
      acec_planning <- pt_local_reference_acec_planning()
      acec_relationships <- pt_local_reference_acec_relationships()
      acec_sources <- pt_local_reference_acec_sources()
      acec_offices <- pt_local_reference_acec_offices()
      acec_current_field_offices <-
        pt_local_reference_acec_current_field_offices()
      acec_field_office_context <-
        pt_local_reference_acec_field_office_context()
      acec_access <- pt_local_reference_acec_access()
      acec_wsa_name_context <- pt_local_reference_acec_wsa_name_context()
      acec_overlap_pairs <- pt_local_reference_acec_overlap_pairs()
      pt_validate_local_reference_acec_research(
        acec_components, acec_reference, acec_values, acec_documents,
        acec_management, acec_planning, acec_relationships, acec_sources,
        offices = acec_offices,
        current_field_offices = acec_current_field_offices,
        field_office_context = acec_field_office_context,
        access = acec_access,
        wsa_name_context = acec_wsa_name_context,
        overlap_pairs = acec_overlap_pairs
      )
    }
    if (is_desert_ncl) {
      pt_validate_local_reference_desert_ncl_research()
      if (!"pt_cdncl_related_designation_facets" %in% names(x)) {
        x$pt_cdncl_related_designation_facets <-
          pt_local_reference_desert_ncl_related_facet_values(
            x$pt_local_reference_semantic_key
          )
      }
    }
    required <- c(
      "pt_local_reference_feature_key", "pt_local_reference_geometry_key",
      "pt_local_reference_semantic_key", "pt_local_reference_category_key",
      "pt_local_reference_geometry_components"
    )
    if (isTRUE(row$feature_selection_supported) && !is_federal_wilderness) {
      required <- c(required, as.character(row$feature_display_field))
    }
    facet_definitions <- row$filter_facets[[1]]
    facet_fields <- if (length(facet_definitions)) {
      vapply(facet_definitions, `[[`, character(1), "record_field")
    } else {
      character(0)
    }
    required <- c(required, facet_fields)
    missing_fields <- setdiff(required, names(x))
    if (length(missing_fields)) {
      stop(
        row$display_name, " is missing controller field(s): ",
        paste(missing_fields, collapse = ", "),
        ". Rebuild its approved source preprocessor before a realistic HTML build."
      )
    }
    definition <- pt_local_reference_categories(row$layer_id)
    definition <- definition[
      definition$include_when_absent |
        definition$category_key %in% unique(x$pt_local_reference_category_key),
      , drop = FALSE
    ]
    features <- if (isTRUE(row$feature_selection_supported)) {
      if (is_federal_wilderness) {
        pt_local_reference_fw_feature_catalog(x, fw_reference, fw_components)
      } else {
        pt_local_reference_semantic_feature_catalog(x, row)
      }
    } else {
      list()
    }
    records <- lapply(seq_len(nrow(x)), function(j) {
      facet_values <- if (length(facet_definitions)) {
        stats::setNames(
          lapply(facet_definitions, function(facet) {
            value <- as.character(x[[facet$record_field]][[j]])
            delimiter <- if (!is.null(facet$multivalue_delimiter)) {
              as.character(facet$multivalue_delimiter)
            } else {
              ""
            }
            if (nzchar(delimiter)) {
              values <- trimws(unlist(strsplit(value, delimiter, fixed = TRUE)))
              values[nzchar(values)]
            } else {
              value
            }
          }),
          vapply(facet_definitions, `[[`, character(1), "facet_key")
        )
      } else {
        list()
      }
      list(
        geometry_key = x$pt_local_reference_geometry_key[[j]],
        feature_key = x$pt_local_reference_feature_key[[j]],
        semantic_feature_key = x$pt_local_reference_semantic_key[[j]],
        category_key = x$pt_local_reference_category_key[[j]],
        geometry_component_count = as.integer(x$pt_local_reference_geometry_components[[j]]),
        facet_values = facet_values
      )
    })
    categories <- lapply(seq_len(nrow(definition)), function(j) {
      as.list(definition[j, c(
        "category_key", "label", "fill_color", "stroke_color",
        "fill_opacity", "stroke_weight", "dash_array",
        "legend_swatch_style", "sort_order", "include_when_absent",
        "provisional"
      ), drop = FALSE])
    })
    facets <- lapply(facet_definitions, function(facet) {
      values <- facet$values[order(facet$values$sort_order), , drop = FALSE]
      value_fields <- intersect(
        c("value_key", "label", "swatch_color", "sort_order"),
        names(values)
      )
      list(
        facet_key = facet$facet_key,
        label = facet$label,
        record_field = facet$record_field,
        count_mode = facet$count_mode,
        collapsible = isTRUE(facet$collapsible),
        open_default = isTRUE(facet$open_default),
        visible = if (is.null(facet$visible)) TRUE else isTRUE(facet$visible),
        show_toolbar = if (is.null(facet$show_toolbar)) {
          TRUE
        } else {
          isTRUE(facet$show_toolbar)
        },
        layout_columns = if (is.null(facet$layout_columns)) {
          1L
        } else {
          as.integer(facet$layout_columns)
        },
        context_cue = if (is.null(facet$context_cue)) "" else {
          as.character(facet$context_cue)
        },
        context_title = if (is.null(facet$context_title)) "" else {
          as.character(facet$context_title)
        },
        thematic_style = if (is.list(facet$thematic_style)) {
          facet$thematic_style
        } else {
          list()
        },
        values = lapply(seq_len(nrow(values)), function(j) {
          as.list(values[j, value_fields, drop = FALSE])
        })
      )
    })
    semantic_labels <- pt_local_reference_semantic_label_payload(
      labels_all,
      x,
      row
    )
    list(
      layer_id = row$layer_id,
      source_nickname = row$source_nickname,
      display_name = row$display_name,
      group_name = pt_local_reference_group_name(as.character(x$pt_display_name[[1]])),
      color_basis = row$color_basis,
      auto_supported = isTRUE(row$auto_supported),
      auto_default = isTRUE(row$auto_default),
      feature_selection_supported = isTRUE(row$feature_selection_supported),
      feature_selection_mode = row$feature_selection_mode,
      feature_search_fields = unlist(
        row$feature_search_fields[[1]],
        use.names = FALSE
      ),
      feature_display_field = row$feature_display_field,
      auto_zoom_supported = isTRUE(row$auto_zoom_supported),
      auto_zoom_default = isTRUE(row$auto_zoom_default),
      zoom_padding = as.numeric(row$zoom_padding),
      zoom_max = as.numeric(row$zoom_max),
      preserve_view_on_reset = isTRUE(row$preserve_view_on_reset),
      count_mode = row$count_mode,
      primary_count_mode = row$primary_count_mode,
      primary_count_label = row$primary_count_label,
      show_component_count = isTRUE(row$show_component_count),
      show_category_count = isTRUE(row$show_category_count),
      category_filter_visible = isTRUE(row$category_filter_visible),
      category_count_mode = row$category_count_mode,
      component_count_label = row$component_count_label,
      category_heading = row$category_heading,
      dashboard_summary = row$dashboard_summary,
      caution = row$card_caution,
      popup_layout = row$popup_layout,
      distinguish_units_supported = isTRUE(row$distinguish_units_supported),
      categories = categories,
      facets = facets,
      quick_views = lapply(row$quick_views[[1]], identity),
      features = features,
      records = records,
      semantic_labels = semantic_labels,
      nps_context = if (is_national_monuments) {
        pt_local_reference_nps_context_payload(nps_context)
      } else {
        NULL
      },
      federal_wilderness = if (is_federal_wilderness) {
        pt_local_reference_fw_popup_payload(
          fw_components, fw_reference, fw_designations, fw_documents,
          fw_policy, fw_sources
        )
      } else {
        NULL
      },
      acec = if (is_acec) {
        pt_local_reference_acec_popup_payload(
          acec_components, acec_reference, acec_values, acec_documents,
          acec_management, acec_planning, acec_relationships, acec_sources,
          offices = acec_offices,
          current_field_offices = acec_current_field_offices,
          field_office_context = acec_field_office_context,
          access = acec_access,
          wsa_name_context = acec_wsa_name_context,
          overlap_pairs = acec_overlap_pairs
        )
      } else {
        NULL
      },
      desert_ncl = if (is_desert_ncl) {
        pt_local_reference_desert_ncl_popup_payload(x)
      } else {
        NULL
      }
    )
  })
  Filter(Negate(is.null), payload)
}

pt_add_local_reference_controller <- function(
  m,
  reference_layers,
  labels_all = NULL,
  nps_context = NULL,
  engine_js_path = file.path(
    "03_functions", "js", "brim_local_reference_filter_engine.js"
  ),
  controller_js_path = file.path(
    "03_functions", "js", "brim_local_reference_controller.js"
  )
) {
  payload <- pt_local_reference_controller_payload(
    reference_layers,
    labels_all,
    nps_context = nps_context
  )
  if (!length(payload)) return(m)
  missing_js <- c(engine_js_path, controller_js_path)[
    !file.exists(c(engine_js_path, controller_js_path))
  ]
  if (length(missing_js)) {
    stop("Missing Local Reference browser controller file(s): ", paste(missing_js, collapse = ", "))
  }
  engine_js <- paste(readLines(engine_js_path, warn = FALSE), collapse = "\n")
  controller_js <- paste(readLines(controller_js_path, warn = FALSE), collapse = "\n")
  m <- htmlwidgets::onRender(
    m,
    paste0("function(el, x) {\n", engine_js, "\n}")
  )
  htmlwidgets::onRender(m, controller_js, data = payload)
}
