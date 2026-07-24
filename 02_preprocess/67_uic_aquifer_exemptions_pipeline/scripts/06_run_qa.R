# Focused data-integrity and source-comparison QA.

uic_missing_count <- function(x) {
  if (inherits(x, "Date")) return(sum(is.na(x)))
  value <- uic_blank_to_na(x)
  sum(is.na(value))
}

uic_run_qa <- function(objects, candidate_id, service_inventory) {
  paths <- uic_candidate_paths(candidate_id)
  registry <- uic_read_config("source_registry.csv")
  inventory <- lapply(names(objects), function(key) {
    x <- objects[[key]]
    source <- registry[registry$source_key == key, , drop = FALSE]
    geometry_types <- as.character(sf::st_geometry_type(x))
    expected_geometry_types <- if (
      identical(source$required_geometry_type[[1]], "esriGeometryPoint")
    ) {
      c("POINT", "MULTIPOINT")
    } else {
      c("POLYGON", "MULTIPOLYGON")
    }
    data.frame(
      source_key = key,
      production_local_layer = source$production_local_layer,
      feature_count = nrow(x),
      geometry_types = paste(sort(unique(geometry_types)), collapse = ";"),
      unexpected_geometry_type_count = sum(
        !geometry_types %in% expected_geometry_types
      ),
      empty_geometry_count = sum(sf::st_is_empty(x)),
      invalid_geometry_count = sum(!sf::st_is_valid(x), na.rm = TRUE),
      missing_source_id_count = uic_missing_count(x$source_id),
      duplicate_source_id_count = sum(duplicated(x$source_id[!is.na(x$source_id)])),
      duplicate_geometry_feature_count = sum(duplicated(x$geometry_hash) | duplicated(x$geometry_hash, fromLast = TRUE)),
      exact_geometry_group_count = sum(table(x$geometry_hash) > 1),
      full_rds_bytes = file.info(
        file.path(paths$full, paste0(key, "_standardized.rds"))
      )$size,
      map_rds_bytes = if (isTRUE(source$production_local_layer)) file.info(
        file.path(paths$map_ready, paste0(key, "_map.rds"))
      )$size else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  inventory <- do.call(rbind, inventory)
  uic_csv_write(inventory, file.path(paths$qa, "candidate_inventory.csv"))

  completeness_fields <- c(
    "source_id", "field_project", "county_standard", "formation_standard",
    "zone_member", "formation_zone_display", "decision_approval_date",
    "well_class_standard", "injection_activity_standard",
    "exemption_criterion", "documentation_url", "label_text"
  )
  completeness <- do.call(rbind, lapply(names(objects), function(key) {
    x <- objects[[key]]
    do.call(rbind, lapply(completeness_fields, function(field) {
      missing <- uic_missing_count(x[[field]])
      data.frame(
        source_key = key,
        field = field,
        populated_count = nrow(x) - missing,
        missing_count = missing,
        populated_percent = if (nrow(x)) round(100 * (nrow(x) - missing) / nrow(x), 2) else NA_real_,
        stringsAsFactors = FALSE
      )
    }))
  }))
  uic_csv_write(completeness, file.path(paths$qa, "standardized_field_completeness.csv"))

  official_counties <- uic_california_counties()
  county_audit <- do.call(rbind, lapply(names(objects), function(key) {
    x <- objects[[key]]
    historic <- identical(key, "calgem_primacy_shaded_subset")
    source_values <- uic_blank_to_na(x$county_source)
    normalized_values <- uic_blank_to_na(x$county_standard)
    members <- unlist(strsplit(
      normalized_values[!is.na(normalized_values)],
      ";",
      fixed = TRUE
    ))
    members <- trimws(members)
    invalid_members <- members[
      nzchar(members) & !members %in% official_counties
    ]
    data.frame(
      source_key = key,
      county_source_field = if (historic) NA_character_ else "County",
      county_applicability = if (historic) {
        "unavailable; AreaName is exemption area"
      } else {
        "verified source County field"
      },
      individual_county_records = sum(
        !is.na(normalized_values) & !grepl(";", normalized_values, fixed = TRUE)
      ),
      multiple_county_records = sum(
        !is.na(normalized_values) & grepl(";", normalized_values, fixed = TRUE)
      ),
      missing_county_records = sum(is.na(source_values)),
      malformed_county_records = sum(
        !is.na(source_values) & is.na(normalized_values)
      ),
      invalid_standardized_member_count = length(invalid_members),
      stringsAsFactors = FALSE
    )
  }))
  uic_csv_write(
    county_audit,
    file.path(paths$qa, "county_field_audit.csv")
  )

  post <- objects$calgem_post_primacy
  epa <- objects$epa_2025_ca_polygons
  shared_ids <- intersect(uic_blank_to_na(post$source_id), uic_blank_to_na(epa$source_id))
  shared_ids <- shared_ids[!is.na(shared_ids)]
  cross_source <- data.frame(
    metric = c(
      "CalGEM post-primacy records",
      "EPA mapped records",
      "Direct shared source IDs",
      "CalGEM IDs without EPA direct match",
      "EPA IDs without CalGEM direct match"
    ),
    value = c(
      nrow(post),
      nrow(epa),
      length(shared_ids),
      sum(!post$source_id %in% epa$source_id),
      sum(!epa$source_id %in% post$source_id)
    ),
    stringsAsFactors = FALSE
  )
  uic_csv_write(cross_source, file.path(paths$qa, "cross_source_id_summary.csv"))

  document_urls <- uic_blank_to_na(post$documentation_url)
  documents <- data.frame(
    source_id = post$source_id,
    documentation_url = document_urls,
    scheme = ifelse(
      grepl("^ftp://", document_urls, ignore.case = TRUE),
      "legacy_ftp",
      ifelse(grepl("^https?://", document_urls, ignore.case = TRUE), "http", NA_character_)
    ),
    stringsAsFactors = FALSE
  )
  uic_csv_write(documents, file.path(paths$qa, "document_link_inventory.csv"))

  geometry_repairs <- utils::read.csv(
    file.path(paths$qa, "geometry_repairs.csv"),
    stringsAsFactors = FALSE
  )
  count_match <- merge(
    inventory[, c("source_key", "feature_count")],
    service_inventory[, c("source_key", "feature_count")],
    by = "source_key",
    suffixes = c("_candidate", "_service")
  )
  count_match$matches <- count_match$feature_count_candidate ==
    count_match$feature_count_service
  uic_csv_write(count_match, file.path(paths$qa, "count_reconciliation.csv"))

  failures <- c(
    if (any(inventory$invalid_geometry_count > 0)) "invalid geometry remains" else NULL,
    if (any(inventory$unexpected_geometry_type_count > 0)) "unexpected geometry type found" else NULL,
    if (any(inventory$empty_geometry_count > 0)) "empty geometry found" else NULL,
    if (any(inventory$missing_source_id_count > 0)) "missing standardized source IDs" else NULL,
    if (any(county_audit$malformed_county_records > 0)) {
      "unrecognized source County value"
    } else NULL,
    if (any(county_audit$invalid_standardized_member_count > 0)) {
      "non-California value in standardized County"
    } else NULL,
    if (
      any(!is.na(objects$calgem_primacy_shaded_subset$county_standard)) ||
      any(!is.na(objects$calgem_primacy_shaded_subset$county_source))
    ) {
      "historic AreaName leaked into County"
    } else NULL,
    if (!all(count_match$matches)) "service/candidate count mismatch" else NULL
  )
  status <- data.frame(
    candidate_id = candidate_id,
    qa_utc = uic_utc_now(),
    passed = !length(failures),
    failures = paste(failures, collapse = "; "),
    total_features = sum(inventory$feature_count),
    repaired_geometry_records = nrow(geometry_repairs),
    full_standardized_bytes = sum(inventory$full_rds_bytes, na.rm = TRUE),
    map_ready_bytes = sum(inventory$map_rds_bytes, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  uic_csv_write(status, file.path(paths$qa, "qa_status.csv"))
  uic_text_write(
    c(
      "# UIC candidate QA",
      "",
      paste0("Candidate: `", candidate_id, "`"),
      paste0("QA UTC: ", status$qa_utc),
      paste0("Result: **", ifelse(status$passed, "PASS", "FAIL"), "**"),
      if (length(failures)) paste0("Failures: ", paste(failures, collapse = "; ")) else "",
      "",
      "| Source | Features | Invalid | Missing ID | Exact-footprint features | Full RDS bytes | Map RDS bytes |",
      "|---|---:|---:|---:|---:|---:|---:|",
      vapply(seq_len(nrow(inventory)), function(i) {
        x <- inventory[i, ]
        paste0(
          "| ", x$source_key, " | ", x$feature_count, " | ",
          x$invalid_geometry_count, " | ", x$missing_source_id_count, " | ",
          x$duplicate_geometry_feature_count, " | ", x$full_rds_bytes, " | ",
          ifelse(is.na(x$map_rds_bytes), "QA only", x$map_rds_bytes), " |"
        )
      }, character(1)),
      "",
      paste0("Geometry repairs recorded: ", nrow(geometry_repairs), "."),
      paste0("Direct EPA/CalGEM post-primacy shared IDs: ", length(shared_ids), "."),
      paste0(
        "County audit: ",
        sum(county_audit$individual_county_records),
        " individual, ",
        sum(county_audit$multiple_county_records),
        " multiple, ",
        sum(county_audit$missing_county_records),
        " missing, and ",
        sum(county_audit$malformed_county_records),
        " malformed source records."
      ),
      "Repeated footprints are reported and preserved; they are not dissolved or deleted.",
      "Historic FormZone date fields remain excluded from standardized display fields."
    ),
    file.path(paths$qa, "qa_summary.md")
  )
  status
}
