# ---- Run summary -------------------------------------------------------------

summary_table <- data.frame(
  metric = c(
    "script_version",
    "source_major_features",
    "source_deltamapp_features",
    "canonical_facilities",
    "canonical_segments",
    "label_points",
    "auto_exact_pairs",
    "unresolved_decisions_omitted",
    "expected_cvp_facilities_missing",
    "reclamation_facilities_project_unassigned",
    "supplements_ready_for_ingest",
    "segments_crosses_blm_calculated",
    "segments_crossing_blm",
    "facilities_crossing_blm",
    "conveyance_length_on_blm_mi"
  ),
  value = c(
    SCRIPT_VERSION,
    nrow(major),
    nrow(delta),
    nrow(facilities),
    nrow(segments_out),
    nrow(labels_out),
    nrow(exact_pairs),
    nrow(qa_unresolved),
    sum(qa_expected_cvp_facilities$audit_status == "missing_geometry"),
    nrow(qa_reclamation_unassigned),
    sum(qa_supplemental_registry$ready_for_ingest, na.rm = TRUE),
    "Yes — using BRIM BLM-CA Managed",
    sum(segments_out$blm_crosses, na.rm = TRUE),
    sum(facilities$facility_crosses_blm, na.rm = TRUE),
    round(sum(segments_out$blm_length_mi, na.rm = TRUE), 6)
  ),
  stringsAsFactors = FALSE
)

readr::write_csv(
  summary_table,
  file.path(CSV_DIR, "pilot_run_summary.csv"),
  na = ""
)

message("\nConveyance pipeline outputs rebuilt successfully.")
message("GeoPackage:\n  ", GPKG_PATH)
message("HTML:\n  ", HTML_PATH)
message("CSV tables:\n  ", CSV_DIR)
message(
  "\nProject classification now uses the reviewed seeds plus ",
  "02_project_membership_crosswalk.csv."
)
message(
  "Review qa_expected_cvp_facilities.csv and ",
  "qa_reclamation_unassigned.csv for remaining gaps."
)
message(
  "Supplemental geometry remains registry-driven; enter the CalSim3 ",
  "intertie source and model ID in 04_supplemental_geometry_registry.csv ",
  "when available."
)
message(
  "BLM on/off, length, percentage, nearest-distance, and crossing-count ",
  "fields were calculated from BRIM's BLM-CA Managed polygon."
)

if (
  isTRUE(AUTO_OPEN_HTML) &&
  interactive() &&
  file.exists(HTML_PATH)
) {
  utils::browseURL(
    normalizePath(
      HTML_PATH,
      winslash = "/",
      mustWork = TRUE
    )
  )
}
