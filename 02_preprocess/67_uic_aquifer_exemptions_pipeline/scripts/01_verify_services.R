# Verify authoritative services without replacing approved data.

uic_verify_services <- function(report_dir, write_metadata = TRUE) {
  registry <- uic_read_config("source_registry.csv")
  checked <- lapply(seq_len(nrow(registry)), function(i) {
    source <- registry[i, , drop = FALSE]
    info <- uic_service_metadata(source, fatal = FALSE)
    required <- uic_required_fields(source)
    missing_fields <- if (isTRUE(info$ok)) setdiff(required, info$fields) else required
    geometry_ok <- isTRUE(info$ok) &&
      identical(info$geometry_type, source$required_geometry_type)
    ids_complete <- isTRUE(info$ok) &&
      length(info$object_ids) == info$feature_count
    row <- data.frame(
      source_key = source$source_key,
      source_family = source$source_family,
      layer_url = source$layer_url,
      checked_utc = info$checked_utc %||% uic_utc_now(),
      available = isTRUE(info$ok),
      http_status = info$status_code %||% NA_integer_,
      geometry_type = info$geometry_type %||% NA_character_,
      required_geometry_type = source$required_geometry_type,
      geometry_ok = geometry_ok,
      object_id_field = info$object_id_field %||% NA_character_,
      maximum_record_count = info$max_record_count %||% NA_integer_,
      supports_pagination = info$supports_pagination %||% FALSE,
      feature_count = info$feature_count %||% NA_integer_,
      object_id_count = length(info$object_ids %||% numeric()),
      ids_complete = ids_complete,
      spatial_reference = info$spatial_reference %||% NA_integer_,
      last_edit_ms = info$last_edit_ms %||% NA_real_,
      field_count = length(info$fields %||% character()),
      required_fields_ok = !length(missing_fields),
      missing_required_fields = paste(missing_fields, collapse = ";"),
      message = info$message %||% "",
      stringsAsFactors = FALSE
    )
    if (write_metadata && isTRUE(info$ok)) {
      uic_json_write(
        info$metadata,
        file.path(report_dir, "service_metadata", paste0(source$source_key, ".json"))
      )
      uic_csv_write(
        data.frame(
          source_key = source$source_key,
          object_id = info$object_ids,
          stringsAsFactors = FALSE
        ),
        file.path(report_dir, "service_ids", paste0(source$source_key, ".csv"))
      )
      fields <- info$metadata$fields %||% list()
      field_inventory <- if (length(fields)) {
        do.call(rbind, lapply(fields, function(field) {
          data.frame(
            source_key = source$source_key,
            field_name = field$name %||% NA_character_,
            field_alias = field$alias %||% NA_character_,
            field_type = field$type %||% NA_character_,
            field_length = field$length %||% NA_integer_,
            stringsAsFactors = FALSE
          )
        }))
      } else {
        data.frame()
      }
      uic_csv_write(
        field_inventory,
        file.path(report_dir, "service_fields", paste0(source$source_key, ".csv"))
      )
    }
    list(row = row, info = info)
  })
  inventory <- do.call(rbind, lapply(checked, `[[`, "row"))
  uic_csv_write(inventory, file.path(report_dir, "service_inventory.csv"))
  summary_lines <- c(
    "# UIC authoritative service check",
    "",
    paste0("Checked UTC: ", uic_utc_now()),
    "",
    "| Source | Available | Features | Geometry | Required fields | IDs complete |",
    "|---|---:|---:|---|---:|---:|",
    vapply(seq_len(nrow(inventory)), function(i) {
      x <- inventory[i, ]
      paste0(
        "| ", x$source_key,
        " | ", ifelse(x$available, "yes", "NO"),
        " | ", ifelse(is.na(x$feature_count), "—", x$feature_count),
        " | ", x$geometry_type,
        " | ", ifelse(x$required_fields_ok, "yes", "NO"),
        " | ", ifelse(x$ids_complete, "yes", "NO"), " |"
      )
    }, character(1)),
    "",
    "Counts are current observations, not permanent acceptance thresholds.",
    "Any schema, ID, count, attribute, or geometry difference is reviewable and is never promoted by this check."
  )
  uic_text_write(summary_lines, file.path(report_dir, "service_check.md"))
  list(
    inventory = inventory,
    services = setNames(lapply(checked, `[[`, "info"), registry$source_key),
    ok = all(
      inventory$available &
        inventory$geometry_ok &
        inventory$required_fields_ok &
        inventory$ids_complete
    )
  )
}
