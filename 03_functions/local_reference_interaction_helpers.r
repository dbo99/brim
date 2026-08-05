# ==== local_reference_interaction_helpers.r ================================
##
## Shared contracts for the bounded Local > Reference interaction framework.
## Phase 1 executes only the Wilderness Study Areas contract. The remaining
## ten registry rows are intentionally validation-only until separately
## approved.

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
    "show_component_count", "component_count_label", "category_heading",
    "feature_selection_supported", "feature_selection_mode",
    "feature_search_fields", "feature_display_field",
    "auto_zoom_supported", "auto_zoom_default", "zoom_padding", "zoom_max",
    "preserve_view_on_reset", "retention_enabled", "search_fields",
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
    stop("Phase 1 Local Reference Auto-zoom requires named-feature selection support.")
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
  if (any(!nzchar(trimws(registry$primary_count_label)))) {
    stop("Local Reference primary_count_label values must be nonblank.")
  }
  if (any(registry$show_component_count &
          !nzchar(trimws(registry$component_count_label)))) {
    stop("Visible Local Reference component counts require a full component label.")
  }
  category_heading_rows <- which(nzchar(trimws(registry$category_heading)))
  if (!identical(category_heading_rows, 4L) || !identical(
    registry$category_heading[[4]],
    "BLM recommendation for wilderness designation"
  )) {
    stop("Phase 1 category-heading contract must remain WSA-only and exact.")
  }
  if (!all(registry$primary_count_mode == "semantic_feature")) {
    stop("Default Local Reference visible counts must use semantic features.")
  }

  expected_auto_supported <- c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  expected_auto_default <- c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
  if (!identical(as.logical(registry$auto_supported), expected_auto_supported) ||
      !identical(as.logical(registry$auto_default), expected_auto_default)) {
    stop("Local Reference Auto support/default contract differs from the approved 11-layer matrix.")
  }
  if (!identical(which(registry$implementation_status == "phase1_wsa"), 4L)) {
    stop("Phase 1 execution must remain limited to Wilderness Study Areas.")
  }
  if (!identical(which(registry$feature_selection_supported), 4L) ||
      !identical(which(registry$auto_zoom_supported), 4L) ||
      !identical(which(registry$auto_zoom_default), 4L)) {
    stop("Phase 1 named-feature selection and Auto-zoom must remain WSA-only.")
  }
  if (!identical(
    unlist(registry$feature_search_fields[[4]], use.names = FALSE),
    c("NLCS_NAME", "WSACODE_ca", "CASEFILE_N", "NLCS_ID", "GlobalID")
  )) {
    stop("Phase 1 WSA named-feature search fields differ from the approved contract.")
  }
  if (!identical(which(registry$retention_enabled), 4L)) {
    stop("Phase 1 field retention must remain limited to Wilderness Study Areas.")
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
    if (any(!definition$provisional)) {
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
    count <- tryCatch(
      length(suppressWarnings(sf::st_cast(geometry, "POLYGON"))),
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
  esc <- function(x) htmltools::htmlEscape(pt_local_reference_clean_chr(x, "Not stated"))
  vapply(seq_len(nrow(df)), function(i) {
    row <- df[i, , drop = FALSE]
    raw_acres <- suppressWarnings(as.numeric(row$pt_wsa_source_gis_acres))
    calculated_acres <- suppressWarnings(as.numeric(row$pt_wsa_calculated_geometry_acres))
    area_row <- paste0(
      "<div><b>GIS acreage:</b> ",
      esc(pt_local_reference_format_source_gis_acres(raw_acres)), "</div>"
    )
    if (!is.na(raw_acres) && raw_acres == 0 && !is.na(calculated_acres) && calculated_acres > 0) {
      area_row <- paste0(
        area_row,
        "<div class=\"pt-wsa-source-anomaly\"><b>Approximate geometry-derived anomaly:</b> ",
        esc(pt_local_reference_format_number(calculated_acres, 1)),
        " acres — source anomaly only; the raw source value above remains authoritative.</div>"
      )
    }

    core_rows <- c(
      paste0("<div><b>BLM recommendation:</b> ", esc(row$pt_local_reference_category_label), "</div>"),
      if (nzchar(pt_local_reference_clean_chr(row$pt_wsa_rod_date))) paste0("<div><b>Record of Decision date:</b> ", esc(row$pt_wsa_rod_date), "</div>") else "",
      area_row,
      if (nzchar(pt_local_reference_clean_chr(row$designation_subtype_reference))) paste0("<div><b>Reference subtype:</b> ", esc(row$designation_subtype_reference), "</div>") else "",
      if (nzchar(pt_local_reference_clean_chr(row$date_established_reference))) paste0("<div><b>Reference established date:</b> ", esc(row$date_established_reference), "</div>") else "",
      if (nzchar(pt_local_reference_clean_chr(row$geographic_scope_note))) paste0("<div><b>Geographic scope:</b> ", esc(row$geographic_scope_note), "</div>") else ""
    )
    management_rows <- c(
      paste0("<div><b>Local managing agency:</b> ", esc(row$pt_management_local_managing_agency), "</div>"),
      paste0("<div><b>BLM role:</b> ", esc(row$pt_management_blm_role_label), "</div>"),
      paste0("<div>", esc(row$pt_management_blm_role_summary), "</div>"),
      paste0("<div><b>Administrative state:</b> ", esc(row$pt_wsa_admin_state), "</div>"),
      if (nzchar(pt_local_reference_clean_chr(row$pt_wsa_casefile))) paste0("<div><b>Case file:</b> ", esc(row$pt_wsa_casefile), "</div>") else "",
      if (nzchar(pt_local_reference_clean_chr(row$pt_wsa_code))) paste0("<div><b>WSA code:</b> ", esc(row$pt_wsa_code), "</div>") else "",
      if (nzchar(pt_local_reference_clean_chr(row$managing_office))) paste0("<div><b>Verified responsible office:</b> ", esc(row$managing_office), "</div>") else "",
      if (nzchar(pt_local_reference_clean_chr(row$managing_office_url))) pt_local_reference_wsa_link(row$managing_office_url, "Official managing-office page") else "",
      pt_local_reference_wsa_link(row$pt_management_role_source_url, "Official management-role source")
    )
    research_rows <- c(
      if (nzchar(pt_local_reference_clean_chr(row$notable_values))) paste0("<div><b>Notable values:</b> ", esc(row$notable_values), "</div>") else "",
      if (nzchar(pt_local_reference_clean_chr(row$access_note))) paste0("<div><b>Access note:</b> ", esc(row$access_note), "</div>") else "",
      if (nzchar(pt_local_reference_clean_chr(row$official_page_url))) pt_local_reference_wsa_link(row$official_page_url, "Official unit page") else "",
      if (nzchar(pt_local_reference_clean_chr(row$curation_verified_date))) paste0("<div><b>Curated evidence verified:</b> ", esc(row$curation_verified_date), "</div>") else ""
    )
    source_links <- paste(
      c(
        pt_local_reference_wsa_link(PT_LOCAL_REFERENCE_WSA_SOURCE_URL, "BLM source service"),
        pt_local_reference_wsa_link(PT_LOCAL_REFERENCE_WSA_DETAIL_URL, "BLM WSA detail table"),
        pt_local_reference_wsa_link(PT_LOCAL_REFERENCE_WSA_DOCUMENTS_URL, "BLM state wilderness documents")
      ),
      collapse = " · "
    )
    technical_rows <- c(
      paste0("<div><b>Stable feature key:</b> ", esc(row$pt_local_reference_feature_key), "</div>"),
      paste0(
        "<div><b>Raw WSA_RCMND source value:</b> ",
        if (nzchar(pt_local_reference_clean_chr(row$pt_wsa_recommendation_raw))) {
          esc(row$pt_wsa_recommendation_raw)
        } else {
          "<i>blank / not stated</i>"
        },
        "</div>"
      ),
      if (nzchar(pt_local_reference_clean_chr(row$pt_wsa_nlcs_id))) paste0("<div><b>NLCS ID:</b> ", esc(row$pt_wsa_nlcs_id), "</div>") else "",
      if (!nzchar(pt_local_reference_clean_chr(row$pt_wsa_nlcs_id)) && nzchar(pt_local_reference_clean_chr(row$pt_wsa_global_id))) paste0("<div><b>GlobalID fallback:</b> ", esc(row$pt_wsa_global_id), "</div>") else "",
      if (nzchar(pt_local_reference_clean_chr(row$pt_wsa_sma_id))) paste0("<div><b>SMA ID:</b> ", esc(row$pt_wsa_sma_id), "</div>") else "",
      if (nzchar(pt_local_reference_clean_chr(row$pt_wsa_fau_id))) paste0("<div><b>FAU ID:</b> ", esc(row$pt_wsa_fau_id), "</div>") else "",
      if (nzchar(pt_local_reference_clean_chr(row$pt_wsa_modify_date))) paste0("<div><b>Modify date:</b> ", esc(row$pt_wsa_modify_date), "</div>") else "",
      paste0("<div><b>Management-role evidence verified:</b> ", esc(row$pt_management_role_verified_on), " (", esc(row$pt_management_role_confidence), ")</div>"),
      paste0("<div><b>Seed join:</b> ", esc(row$pt_wsa_join_status), "</div>"),
      paste0("<div><b>Geometry components:</b> ", esc(row$pt_local_reference_geometry_components), "</div>"),
      source_links
    )
    paste0(
      "<div class=\"pt-popup pt-local-reference-popup pt-wsa-popup\">",
      "<div class=\"pt-popup-title\"><b>", esc(row$pt_wsa_name), "</b></div>",
      "<div class=\"pt-popup-subtitle\">", esc(row$pt_wsa_designation_subtitle), "</div>",
      "<div class=\"pt-popup-section\"><b>Designation and recommendation</b>", paste0(core_rows, collapse = ""), "</div>",
      "<div class=\"pt-popup-section\"><b>Management and planning</b>", paste0(management_rows, collapse = ""), "</div>",
      if (any(nzchar(research_rows))) paste0("<div class=\"pt-popup-section\"><b>Official research notes</b>", paste0(research_rows, collapse = ""), "</div>") else "",
      "<div class=\"pt-wsa-caution\"><b>Recommendation context:</b> ", esc(PT_LOCAL_REFERENCE_WSA_POPUP_CAUTION), "</div>",
      "<details class=\"pt-popup-technical\"><summary>Source and technical details</summary>", paste0(technical_rows, collapse = ""), "</details>",
      "</div>"
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

pt_local_reference_controller_payload <- function(reference_layers) {
  active <- LOCAL_REFERENCE_INTERACTION_REGISTRY[
    LOCAL_REFERENCE_INTERACTION_REGISTRY$implementation_status == "phase1_wsa",
    , drop = FALSE
  ]
  payload <- lapply(seq_len(nrow(active)), function(i) {
    row <- active[i, , drop = FALSE]
    x <- reference_layers[[row$source_nickname]]
    if (is.null(x) || !nrow(x)) return(NULL)
    required <- c(
      "pt_local_reference_feature_key", "pt_local_reference_geometry_key",
      "pt_local_reference_semantic_key", "pt_local_reference_category_key",
      "pt_local_reference_geometry_components", "pt_wsa_name"
    )
    missing_fields <- setdiff(required, names(x))
    if (length(missing_fields)) {
      stop(
        row$display_name, " is missing controller field(s): ",
        paste(missing_fields, collapse = ", "),
        ". Rebuild its approved source preprocessor before a realistic HTML build."
      )
    }
    definition <- pt_local_reference_categories(row$layer_id)
    features <- if (isTRUE(row$feature_selection_supported)) {
      pt_local_reference_semantic_feature_catalog(x, row)
    } else {
      list()
    }
    records <- lapply(seq_len(nrow(x)), function(j) list(
      geometry_key = x$pt_local_reference_geometry_key[[j]],
      feature_key = x$pt_local_reference_feature_key[[j]],
      semantic_feature_key = x$pt_local_reference_semantic_key[[j]],
      category_key = x$pt_local_reference_category_key[[j]],
      geometry_component_count = as.integer(x$pt_local_reference_geometry_components[[j]])
    ))
    categories <- lapply(seq_len(nrow(definition)), function(j) {
      as.list(definition[j, c(
        "category_key", "label", "fill_color", "stroke_color",
        "fill_opacity", "stroke_weight", "dash_array",
        "legend_swatch_style", "sort_order", "include_when_absent",
        "provisional"
      ), drop = FALSE])
    })
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
      component_count_label = row$component_count_label,
      category_heading = row$category_heading,
      caution = PT_LOCAL_REFERENCE_WSA_CARD_CAUTION,
      categories = categories,
      features = features,
      records = records
    )
  })
  Filter(Negate(is.null), payload)
}

pt_add_local_reference_controller <- function(
  m,
  reference_layers,
  engine_js_path = file.path(
    "03_functions", "js", "brim_local_reference_filter_engine.js"
  ),
  controller_js_path = file.path(
    "03_functions", "js", "brim_local_reference_controller.js"
  )
) {
  payload <- pt_local_reference_controller_payload(reference_layers)
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
