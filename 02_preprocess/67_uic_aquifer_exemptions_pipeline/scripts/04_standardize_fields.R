# Create non-destructive standardized analytical products and lean map caches.

uic_parse_date <- function(x) {
  raw <- uic_blank_to_na(x)
  numeric <- suppressWarnings(as.numeric(raw))
  out <- as.Date(rep(NA_real_, length(raw)), origin = "1970-01-01")
  milliseconds <- !is.na(numeric) & abs(numeric) > 100000
  out[milliseconds] <- as.Date(numeric[milliseconds] / 86400000, origin = "1970-01-01")
  text <- !milliseconds & !is.na(raw)
  out[text] <- as.Date(raw[text], format = "%m/%d/%Y")
  out
}

uic_join_reported_values <- function(..., sep = " – ") {
  values <- list(...)
  n <- max(vapply(values, length, integer(1)))
  values <- lapply(values, function(value) {
    rep_len(uic_blank_to_na(value), n)
  })
  vapply(seq_len(n), function(i) {
    row <- vapply(values, `[[`, character(1), i)
    row <- row[!is.na(row)]
    if (length(row)) paste(row, collapse = sep) else NA_character_
  }, character(1))
}

uic_standardize_source <- function(x, source, approved_snapshot_utc = NA_character_) {
  key <- source$source_key
  n <- nrow(x)
  object_id_field <- if ("OBJECTID" %in% names(x)) {
    "OBJECTID"
  } else if ("OBJECTID_1" %in% names(x)) {
    "OBJECTID_1"
  } else {
    source$source_id_field
  }
  object_id <- uic_col(x, object_id_field)
  empty <- rep(NA_character_, n)
  source_id <- field_project <- oil_field <- county_source <- county <- empty
  area_name <- formation <- zone <- empty
  decision_raw <- well_class <- injection_activity <- criterion <- document <- empty
  approved <- length(approved_snapshot_utc) &&
    !is.na(approved_snapshot_utc[[1]]) &&
    nzchar(approved_snapshot_utc[[1]])
  snapshot_label <- if (approved) {
    "Approved research snapshot"
  } else {
    "Candidate snapshot · not approved"
  }
  source_status <- rep(
    if (approved) {
      "approved research snapshot; current service status not checked"
    } else {
      "candidate snapshot; not approved"
    },
    n
  )
  historic <- identical(key, "calgem_primacy_shaded_subset")

  if (identical(key, "epa_2025_ca_polygons") ||
      identical(key, "epa_2025_ca_points") ||
      identical(key, "epa_2025_ca_county_locations")) {
    source_id <- uic_first_nonblank(
      uic_col(x, if (identical(key, "epa_2025_ca_polygons")) "ID_1" else "ID"),
      object_id
    )
    field_project <- uic_blank_to_na(uic_col(x, "Injection_Well_ID"))
    county_source <- uic_blank_to_na(uic_col(x, "County"))
    county <- uic_normalize_california_counties(county_source)
    zone <- uic_blank_to_na(uic_col(x, "Injection_Zone"))
    decision_raw <- uic_blank_to_na(uic_col(x, "Decision_Date"))
    well_class <- uic_blank_to_na(uic_col(x, "Well_Class"))
    injection_activity <- uic_blank_to_na(uic_col(x, "Injection_Activity"))
  } else if (identical(key, "calgem_post_primacy")) {
    source_id <- uic_first_nonblank(uic_col(x, "ID"), object_id)
    field_project <- uic_first_nonblank(uic_col(x, "Field_Labe"), uic_col(x, "Name"))
    oil_field <- field_project
    county_source <- uic_blank_to_na(uic_col(x, "County"))
    county <- uic_normalize_california_counties(county_source)
    formation <- uic_blank_to_na(uic_col(x, "Formation"))
    zone <- uic_first_nonblank(
      uic_col(x, "Zone"), uic_col(x, "Inj_Zone"), uic_col(x, "Zone_Label")
    )
    decision_raw <- uic_blank_to_na(uic_col(x, "Approve"))
    well_class <- uic_blank_to_na(uic_col(x, "Well_Class"))
    injection_activity <- uic_blank_to_na(uic_col(x, "Injectate"))
    criterion <- uic_blank_to_na(uic_col(x, "Exemption_"))
    document <- uic_blank_to_na(uic_col(x, "Documentat"))
  } else if (historic) {
    source_id <- paste0(
      "CalGEM_PRIMACY_",
      uic_first_nonblank(uic_col(x, "OBJECTID_1"), object_id)
    )
    field_project <- uic_blank_to_na(uic_col(x, "FieldName"))
    oil_field <- field_project
    area_name <- uic_blank_to_na(uic_col(x, "AreaName"))
    zone_fields <- intersect(paste0("FormZone", 1:18), names(x))
    zone <- vapply(seq_len(n), function(i) {
      values <- uic_blank_to_na(vapply(
        zone_fields,
        function(field) as.character(x[[field]][i]),
        character(1)
      ))
      values <- values[!is.na(values)]
      if (length(values)) paste(values, collapse = "; ") else NA_character_
    }, character(1))
    document <- uic_blank_to_na(uic_col(x, "Doc_Source"))
  }

  decision_date <- if (historic) {
    as.Date(rep(NA_real_, n), origin = "1970-01-01")
  } else {
    uic_parse_date(decision_raw)
  }
  formation_zone <- uic_first_nonblank(
    ifelse(!is.na(formation) & !is.na(zone), paste(formation, zone, sep = " — "), NA_character_),
    formation,
    zone
  )
  label <- if (identical(key, "epa_2025_ca_polygons")) {
    uic_first_nonblank(field_project, zone, source_id)
  } else {
    uic_first_nonblank(oil_field, field_project, source_id)
  }
  home <- rep(source$source_homepage_url, n)
  service <- rep(source$layer_url, n)
  warning <- rep(uic_warning(historic), n)
  layer_label <- rep(source$source_layer, n)
  retrieval <- x$.uic_retrieval_utc
  geometry_hash <- x$.uic_geometry_hash
  attribute_fields <- setdiff(names(x), attr(x, "sf_column") %||% "geometry")
  attribute_hash <- uic_attribute_hash(x, setdiff(attribute_fields, grep("^\\.uic_", attribute_fields, value = TRUE)))

  x$source_family <- rep(source$source_family, n)
  x$source_layer <- layer_label
  x$source_status <- source_status
  x$source_id <- source_id
  x$field_project <- field_project
  x$oil_field <- oil_field
  x$county_source <- county_source
  x$county_standard <- county
  x$area_name <- area_name
  x$formation_standard <- formation
  x$zone_member <- zone
  x$formation_zone_display <- formation_zone
  x$decision_approval_date_raw <- decision_raw
  x$decision_approval_date <- decision_date
  x$decision_approval_year <- ifelse(is.na(decision_date), NA_integer_, as.integer(format(decision_date, "%Y")))
  x$well_class_standard <- well_class
  x$injection_activity_standard <- injection_activity
  x$exemption_criterion <- criterion
  x$documentation_url <- document
  x$source_service_url <- service
  x$source_homepage_url <- home
  x$record_decision_url <- if (grepl("^epa_", key)) rep(
    "https://www.epa.gov/uic/california-uic-program-oversight-arods", n
  ) else document
  x$has_document <- !is.na(document)
  x$retrieval_utc <- retrieval
  x$approved_snapshot_utc <- rep(approved_snapshot_utc, n)
  x$service_check_utc <- rep(NA_character_, n)
  x$geometry_hash <- geometry_hash
  x$attribute_hash <- attribute_hash
  x$label_text <- label
  x$interpretation_warning <- warning
  x$geometry_role <- rep(source$geometry_role, n)
  x$is_historic_partial_subset <- rep(historic, n)

  title <- uic_html_escape(uic_first_nonblank(field_project, source_id))
  doc_link <- uic_link_html(document, "Open source documentation")
  source_link <- uic_link_html(home, "Source information")
  decision_link <- if (grepl("^epa_", key)) {
    uic_link_html(x$record_decision_url, "EPA California decisions")
  } else {
    rep("", n)
  }
  footer_links <- unlist(Map(function(...) {
    values <- c(...)
    paste(values[nzchar(values)], collapse = " · ")
  }, source_link, decision_link), use.names = FALSE)
  x$hover_html <- paste0(
    "<div class='pt-uic-hover'><strong>", title, "</strong>",
    ifelse(is.na(formation_zone), "", paste0("<br>", uic_html_escape(formation_zone))),
    "<br><span>", uic_html_escape(source$source_family), " · ",
    uic_html_escape(ifelse(historic, "Historic partial subset", "Mapped footprint")),
    "</span></div>"
  )
  decision_display <- ifelse(
    historic,
    NA_character_,
    ifelse(
      is.na(decision_date),
      decision_raw,
      as.character(decision_date)
    )
  )
  source_detail_rows <- rep("", n)
  if (identical(key, "epa_2025_ca_polygons")) {
    source_detail_rows <- paste0(
      uic_popup_row(
        "Reported depth",
        uic_join_reported_values(
          uic_col(x, "Depth"),
          uic_col(x, "Depth_Units"),
          sep = " "
        )
      ),
      uic_popup_row(
        "Mapped area",
        uic_join_reported_values(
          uic_col(x, "AE_Area"),
          uic_col(x, "AE_Area_Units"),
          sep = " "
        )
      ),
      uic_popup_row("Data quality", uic_col(x, "Data_Quality_Category"))
    )
  } else if (identical(key, "calgem_post_primacy")) {
    source_detail_rows <- paste0(
      uic_popup_row("Pool", uic_col(x, "Pool")),
      uic_popup_row("Reported acreage", uic_col(x, "Acreage")),
      uic_popup_row(
        "Reported depth range (source values)",
        uic_join_reported_values(
          uic_col(x, "DepthMin_Z"),
          uic_col(x, "DepthMax_Z")
        )
      ),
      uic_popup_row(
        "USDW range (as reported)",
        uic_join_reported_values(
          uic_col(x, "USDW_MinDe"),
          uic_col(x, "USDW_MaxDe")
        )
      ),
      uic_popup_row(
        "TDS range (as reported)",
        uic_join_reported_values(
          uic_col(x, "TDS_Min"),
          uic_col(x, "TDS_Max")
        )
      ),
      uic_popup_row(
        "Boron range (as reported)",
        uic_join_reported_values(
          uic_col(x, "Boron_Min"),
          uic_col(x, "Boron_Max")
        )
      ),
      uic_popup_row("Source comments", uic_col(x, "Comments"))
    )
  } else if (historic) {
    source_detail_rows <- paste0(
      uic_popup_row("Source volume", uic_col(x, "Doc_Source")),
      uic_popup_row("Reported acres", uic_col(x, "AcresTable")),
      uic_popup_row("Calculated acres", uic_col(x, "AcresCalc")),
      uic_popup_row("RMS error", uic_col(x, "RMS_Error")),
      uic_popup_row("GIS comments", uic_col(x, "GIS_Comments"))
    )
  }
  rows <- Map(
    paste0,
    uic_popup_row("Source record", source_id),
    uic_popup_row("Field / project", field_project),
    if (historic) {
      uic_popup_row("Area", area_name)
    } else {
      uic_popup_row("County", county_source)
    },
    uic_popup_row("Formation", formation),
    uic_popup_row("Zone / member", zone),
    uic_popup_row("Decision / approval date", decision_display),
    uic_popup_row("Well class", well_class),
    uic_popup_row("Injection activity", injection_activity),
    uic_popup_row("Exemption criterion", criterion),
    source_detail_rows,
    uic_popup_row("Documentation", doc_link, allow_html = TRUE)
  )
  rows <- unlist(rows, use.names = FALSE)
  x$popup_html <- paste0(
    "<div class='pt-uic-popup'><div class='pt-uic-popup-badge'>",
    uic_html_escape(source$source_family), " · ",
    uic_html_escape(snapshot_label), "</div>",
    "<div class='pt-uic-popup-title'>", title, "</div><table>", rows, "</table>",
    "<div class='pt-uic-popup-warning'><strong>Interpretation warning:</strong> ",
    uic_html_escape(warning), "</div><div class='pt-uic-popup-links'>",
    footer_links, "</div></div>"
  )
  x
}

uic_build_label_cache <- function(map_object, source_key, min_zoom) {
  label <- uic_blank_to_na(map_object$label_text)
  normalized <- tolower(gsub("[^[:alnum:]]+", " ", label))
  key <- paste(trimws(normalized), map_object$geometry_hash, sep = "\u001f")
  keep <- !is.na(label) & !duplicated(key)
  labels <- suppressWarnings(sf::st_point_on_surface(map_object[keep, ]))
  labels$label_min_zoom <- as.integer(min_zoom)
  labels$label_source_key <- source_key
  labels
}

uic_standardize_outputs <- function(objects, candidate_id) {
  registry <- uic_read_config("source_registry.csv")
  display <- uic_read_config("display_config.csv")
  paths <- uic_candidate_paths(candidate_id)
  output <- list()
  map_fields <- c(
    "source_family", "source_layer", "source_status", "source_id",
    "field_project", "oil_field", "county_source", "county_standard",
    "area_name", "formation_standard",
    "zone_member", "formation_zone_display", "decision_approval_date_raw",
    "decision_approval_date", "decision_approval_year", "well_class_standard",
    "injection_activity_standard", "exemption_criterion", "documentation_url",
    "source_service_url", "source_homepage_url", "record_decision_url",
    "has_document", "retrieval_utc", "approved_snapshot_utc",
    "service_check_utc", "geometry_hash", "attribute_hash", "popup_html",
    "hover_html", "label_text", "interpretation_warning", "geometry_role",
    "is_historic_partial_subset", "geometry"
  )
  for (i in seq_len(nrow(registry))) {
    source <- registry[i, , drop = FALSE]
    key <- source$source_key
    full <- uic_standardize_source(objects[[key]], source)
    saveRDS(full, file.path(paths$full, paste0(key, "_standardized.rds")))
    output[[key]] <- full
    if (isTRUE(source$production_local_layer)) {
      lean_fields <- intersect(
        setdiff(map_fields, attr(full, "sf_column") %||% "geometry"),
        names(full)
      )
      lean <- sf::st_sf(
        sf::st_drop_geometry(full)[, lean_fields, drop = FALSE],
        geometry = sf::st_geometry(full),
        crs = sf::st_crs(full)
      )
      lean <- sf::st_transform(lean, 4326)
      saveRDS(lean, file.path(paths$map_ready, paste0(key, "_map.rds")))
      style <- display[display$source_key == key, , drop = FALSE]
      labels <- uic_build_label_cache(lean, key, style$min_label_zoom[[1]])
      saveRDS(labels, file.path(paths$labels, paste0(key, "_labels.rds")))
    }
  }
  output
}
