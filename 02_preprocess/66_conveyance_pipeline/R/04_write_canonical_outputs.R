# ---- Convert output geometry to WGS84 ---------------------------------------

segments_out <- suppressWarnings(
  sf::st_transform(segments, 4326)
)

labels_out <- suppressWarnings(
  sf::st_transform(labels, 4326)
)

segment_field_order <- c(
  "segment_id",
  "facility_id",
  "canonical_name",
  "lbl",
  "lbl_full",
  "aliases",
  "parent_system",
  "facility_group",
  "facility_type",
  "network_role",
  "ownership_class",
  "owner_agency",
  "operator_agency",
  "project_family",
  "project_name",
  "project_division",
  "project_unit",
  "project_subunit",
  "project_path",
  "display_rank",
  "display_rank_source",
  "display_rank_reason",
  "status",
  "length_mi",
  "geometry_source",
  "geometry_decision",
  "geometry_confidence",
  "decision_id",
  "source_major_rows",
  "source_delta_rows",
  "source_ids",
  "blm_crosses",
  "blm_length_mi",
  "blm_pct_length",
  "blm_nearest_mi",
  "blm_crossing_count",
  "blm_field_offices",
  "search_text"
)

segments_out <- segments_out |>
  dplyr::select(
    dplyr::all_of(segment_field_order),
    geometry
  )

# ---- Lean BRIM controller exports -------------------------------------------
##
## Canonical outputs retain all 1,811 source/canonical line segments and full
## calculation precision. The standalone BRIM map instead receives one combined
## line geometry per logical facility. This is the polyline equivalent of
## clustering: 623 interactive facility features rather than 1,811 segment
## features, without altering canonical geometry or lineage.

conveyance_map_clean_text <- function(x) {
  out <- trimws(as.character(x))
  out[
    is.na(out) |
      out %in% c("", "NA", "NaN", "NULL", "null")
  ] <- ""
  out
}

conveyance_ownership_bucket <- function(x) {
  raw <- tolower(conveyance_map_clean_text(x))

  dplyr::case_when(
    grepl("joint federal/state|cvp; swp|swp; cvp", raw) |
      (grepl("federal", raw) & grepl("california state", raw)) ~ "joint",
    grepl("federal", raw) ~ "federal",
    grepl("california state|state water project", raw) ~ "state",
    grepl("local/regional public|irrigation district|water district|water authority|city|county", raw) ~ "local",
    grepl("private|company|corporation|mutual water", raw) ~ "private",
    TRUE ~ "unknown"
  )
}

conveyance_first_piece <- function(x, pattern = NULL) {
  value <- conveyance_map_clean_text(x)
  if (!nzchar(value)) return("")
  pieces <- trimws(unlist(strsplit(value, "\\s*;\\s*")))
  pieces <- pieces[nzchar(pieces)]
  if (!is.null(pattern)) {
    hit <- pieces[grepl(pattern, pieces, ignore.case = TRUE)]
    if (length(hit) > 0L) return(hit[[1]])
  }
  if (length(pieces) == 0L) "" else pieces[[1]]
}

conveyance_cvp_division <- function(project_family, project_division) {
  if (!grepl("CVP", project_family, ignore.case = TRUE)) return("")
  pieces <- trimws(unlist(strsplit(project_division, "\\s*;\\s*")))
  pieces <- pieces[nzchar(pieces)]
  hit <- pieces[
    grepl("Division", pieces, ignore.case = TRUE) &
      !grepl("California Aqueduct", pieces, ignore.case = TRUE)
  ]
  if (length(hit) == 0L) "" else hit[[1]]
}

conveyance_cvp_unit <- function(division, text) {
  division_norm <- tolower(conveyance_map_clean_text(division))
  text_norm <- tolower(conveyance_map_clean_text(text))

  if (grepl("american river", division_norm)) {
    if (grepl("sly park|camino|camp creek", text_norm)) return("Sly Park Unit")
    if (grepl("folsom", text_norm)) return("Folsom Unit")
  }
  if (grepl("friant", division_norm)) return("Friant Unit")
  if (grepl("sacramento river", division_norm)) return("Sacramento Canals Unit")
  if (grepl("west san joaquin", division_norm)) return("San Luis Unit")

  # Delta, Trinity, and San Felipe are useful division-level filters but their
  # current lower-level values mostly repeat individual facilities.
  ""
}

conveyance_swp_system <- function(text, project_unit, project_name) {
  z <- tolower(conveyance_map_clean_text(text))
  unit <- conveyance_map_clean_text(project_unit)
  project <- conveyance_map_clean_text(project_name)

  if (grepl("east branch", z)) return("California Aqueduct – East Branch")
  if (grepl("west branch", z)) return("California Aqueduct – West Branch")
  if (grepl("coastal branch", z)) return("Coastal Branch Aqueduct")
  if (grepl("north bay aqueduct", z)) return("North Bay Aqueduct")
  if (grepl("south bay aqueduct", z)) return("South Bay Aqueduct")
  if (grepl("san luis canal|san luis field", z)) return("San Luis Joint-Use Facilities")
  if (grepl("california aqueduct", z)) return("California Aqueduct mainline")
  if (nzchar(unit) && !grepl("California Aqueduct", unit, ignore.case = TRUE)) return(conveyance_first_piece(unit))
  if (nzchar(project) && !grepl("^State Water Project$", project, ignore.case = TRUE)) return(conveyance_first_piece(project))
  "Other State Water Project conveyance"
}

conveyance_local_system <- function(text, parent_system, project_name,
                                    operator_agency, canonical_name) {
  z <- tolower(conveyance_map_clean_text(text))

  if (grepl("los angeles aqueduct|lee vining", z)) return("Los Angeles Aqueduct system")
  if (grepl("hetch[ -]?hetchy|bay division pipeline", z)) return("Hetch Hetchy Regional Water System")
  if (grepl("mokelumne|lafayette water tunnel", z)) return("Mokelumne Aqueduct system")
  if (grepl("colorado river aqueduct", z)) return("Colorado River Aqueduct")
  if (grepl("san diego aqueduct", z)) return("San Diego Aqueduct system")

  parent <- conveyance_map_clean_text(parent_system)
  project <- conveyance_map_clean_text(project_name)
  operator <- conveyance_first_piece(operator_agency)
  name <- conveyance_map_clean_text(canonical_name)

  if (nzchar(parent)) return(parent)
  if (nzchar(project) && !grepl("^Local$|^Unassigned$", project, ignore.case = TRUE)) return(conveyance_first_piece(project))
  if (nzchar(operator) && !grepl("^Local$", operator, ignore.case = TRUE)) return(operator)
  name
}

# One combined geometry and one facility-level BLM summary per facility.
facility_line_summary <- segments_out |>
  sf::st_drop_geometry() |>
  dplyr::group_by(facility_id) |>
  dplyr::summarise(
    source_segment_count = dplyr::n(),
    facility_length_mi_map = round(sum(as.numeric(length_mi), na.rm = TRUE), 2),
    facility_blm_crosses_map = any(dplyr::coalesce(as.logical(blm_crosses), FALSE)),
    facility_blm_length_mi_map = round(sum(as.numeric(blm_length_mi), na.rm = TRUE), 2),
    facility_blm_pct_length_map = round(
      100 * sum(as.numeric(blm_length_mi), na.rm = TRUE) /
        pmax(sum(as.numeric(length_mi), na.rm = TRUE), 1e-9),
      1
    ),
    facility_blm_nearest_mi_map = round(min(as.numeric(blm_nearest_mi), na.rm = TRUE), 2),
    facility_blm_crossing_count_map = sum(as.integer(blm_crossing_count), na.rm = TRUE),
    .groups = "drop"
  )

facility_geometry <- lapply(
  facility_line_summary$facility_id,
  function(current_id) {
    current_geometry <- sf::st_geometry(
      segments_out[segments_out$facility_id == current_id, , drop = FALSE]
    )
    sf::st_combine(current_geometry)[[1]]
  }
)

segments_brim_map <- sf::st_sf(
  facility_line_summary,
  geometry = sf::st_sfc(facility_geometry, crs = 4326)
)

facility_bbox_matrix <- t(vapply(
  seq_len(nrow(segments_brim_map)),
  function(i) {
    as.numeric(sf::st_bbox(segments_brim_map[i, , drop = FALSE]))
  },
  numeric(4)
))

facility_bbox <- data.frame(
  facility_id = segments_brim_map$facility_id,
  bbox_xmin = round(facility_bbox_matrix[, 1], 5),
  bbox_ymin = round(facility_bbox_matrix[, 2], 5),
  bbox_xmax = round(facility_bbox_matrix[, 3], 5),
  bbox_ymax = round(facility_bbox_matrix[, 4], 5),
  stringsAsFactors = FALSE
)

facility_source_map <- segments_out |>
  sf::st_drop_geometry() |>
  dplyr::group_by(facility_id) |>
  dplyr::summarise(
    geometry_source = collapse_unique(geometry_source),
    geometry_decision = collapse_unique(geometry_decision),
    source_ids = collapse_unique(source_ids),
    source_major_rows = collapse_unique(source_major_rows),
    source_delta_rows = collapse_unique(source_delta_rows),
    .groups = "drop"
  )

facilities_brim_map <- facilities |>
  dplyr::left_join(facility_source_map, by = "facility_id") |>
  dplyr::left_join(facility_line_summary, by = "facility_id") |>
  dplyr::left_join(facility_bbox, by = "facility_id") |>
  dplyr::mutate(
    ownership_bucket = conveyance_ownership_bucket(ownership_class),
    length_mi = round(as.numeric(length_mi), 2),
    facility_length_on_blm_mi = round(as.numeric(facility_length_on_blm_mi), 2),
    facility_pct_length_on_blm = round(
      100 * as.numeric(facility_length_on_blm_mi) /
        pmax(as.numeric(length_mi), 1e-9),
      1
    ),
    facility_min_blm_distance_mi = round(as.numeric(facility_min_blm_distance_mi), 2),
    facility_crosses_blm = dplyr::coalesce(as.logical(facility_crosses_blm), FALSE)
  )

ui_combined_text <- paste(
  facilities_brim_map$canonical_name,
  facilities_brim_map$aliases,
  facilities_brim_map$parent_system,
  facilities_brim_map$project_name,
  facilities_brim_map$project_division,
  facilities_brim_map$project_unit,
  facilities_brim_map$project_subunit,
  facilities_brim_map$project_path,
  facilities_brim_map$owner_agency,
  facilities_brim_map$operator_agency
)

facilities_brim_map$filter_family <- dplyr::case_when(
  grepl("CVP", facilities_brim_map$project_family, ignore.case = TRUE) &
    grepl("SWP", facilities_brim_map$project_family, ignore.case = TRUE) ~ "CVP; SWP",
  grepl("CVP", facilities_brim_map$project_family, ignore.case = TRUE) ~ "CVP",
  grepl("SWP", facilities_brim_map$project_family, ignore.case = TRUE) ~ "SWP",
  grepl("Other Reclamation", facilities_brim_map$project_family, ignore.case = TRUE) ~ "Other Reclamation",
  grepl("Local/regional public", facilities_brim_map$project_family, ignore.case = TRUE) |
    facilities_brim_map$ownership_bucket == "local" ~ "Local/regional public",
  TRUE ~ "Unknown/unassigned"
)

facilities_brim_map$filter_cvp_division <- mapply(
  conveyance_cvp_division,
  facilities_brim_map$project_family,
  facilities_brim_map$project_division,
  USE.NAMES = FALSE
)

facilities_brim_map$filter_cvp_unit <- mapply(
  conveyance_cvp_unit,
  facilities_brim_map$filter_cvp_division,
  ui_combined_text,
  USE.NAMES = FALSE
)

facilities_brim_map$filter_swp_system <- mapply(
  conveyance_swp_system,
  ui_combined_text,
  facilities_brim_map$project_unit,
  facilities_brim_map$project_name,
  USE.NAMES = FALSE
)
facilities_brim_map$filter_swp_system[
  !grepl("SWP", facilities_brim_map$filter_family, ignore.case = TRUE)
] <- ""

facilities_brim_map$filter_other_reclamation_system <- mapply(
  function(family, project_name, parent_system, canonical_name) {
    if (!grepl("Other Reclamation", family, ignore.case = TRUE)) return("")
    project <- conveyance_map_clean_text(project_name)
    parent <- conveyance_map_clean_text(parent_system)
    if (nzchar(project) && !grepl("^Unassigned$", project, ignore.case = TRUE)) return(conveyance_first_piece(project))
    if (nzchar(parent)) return(parent)
    conveyance_map_clean_text(canonical_name)
  },
  facilities_brim_map$filter_family,
  facilities_brim_map$project_name,
  facilities_brim_map$parent_system,
  facilities_brim_map$canonical_name,
  USE.NAMES = FALSE
)

facilities_brim_map$filter_local_system <- mapply(
  conveyance_local_system,
  ui_combined_text,
  facilities_brim_map$parent_system,
  facilities_brim_map$project_name,
  facilities_brim_map$operator_agency,
  facilities_brim_map$canonical_name,
  USE.NAMES = FALSE
)
facilities_brim_map$filter_local_system[
  !grepl("Local/regional public", facilities_brim_map$filter_family, ignore.case = TRUE)
] <- ""

facilities_brim_map <- facilities_brim_map |>
  dplyr::select(
    facility_id,
    canonical_name,
    lbl,
    aliases,
    parent_system,
    facility_group,
    facility_type,
    network_role,
    ownership_bucket,
    ownership_class,
    owner_agency,
    operator_agency,
    project_family,
    project_name,
    project_division,
    project_unit,
    project_subunit,
    project_path,
    filter_family,
    filter_cvp_division,
    filter_cvp_unit,
    filter_swp_system,
    filter_other_reclamation_system,
    filter_local_system,
    display_rank,
    display_rank_source,
    display_rank_reason,
    status,
    length_mi,
    segment_count,
    source_segment_count,
    geometry_confidence,
    decision_ids,
    facility_crosses_blm,
    facility_length_on_blm_mi,
    facility_pct_length_on_blm,
    facility_min_blm_distance_mi,
    facility_blm_field_offices,
    geometry_source,
    geometry_decision,
    source_ids,
    source_major_rows,
    source_delta_rows,
    bbox_xmin,
    bbox_ymin,
    bbox_xmax,
    bbox_ymax
  ) |>
  dplyr::mutate(
    dplyr::across(where(is.character), conveyance_map_clean_text)
  )

labels_brim_map <- labels_out |>
  dplyr::filter(!is.na(lbl), trimws(as.character(lbl)) != "") |>
  dplyr::mutate(
    facility_id = as.character(facility_id),
    lbl = trimws(as.character(lbl)),
    lbl_min_zoom = as.integer(dplyr::coalesce(suppressWarnings(as.numeric(lbl_min_zoom)), 11)),
    lbl_max_zoom = as.integer(dplyr::coalesce(suppressWarnings(as.numeric(lbl_max_zoom)), 20))
  ) |>
  dplyr::select(facility_id, lbl, lbl_min_zoom, lbl_max_zoom, geometry)

message(
  "Lean BRIM facility controller exports prepared: ",
  nrow(segments_brim_map),
  " combined facility lines from ",
  nrow(segments_out),
  " canonical segments; ",
  nrow(facilities_brim_map),
  " facility records; ",
  nrow(labels_brim_map),
  " facility labels."
)

message("Canonical tables and label geometry prepared.")

# ---- GeoPackage and CSV outputs ---------------------------------------------

message("Preparing GeoPackage in a local temporary file...")

temporary_gpkg <- tempfile(
  pattern = "brim_conveyance_master_",
  fileext = ".gpkg"
)

if (file.exists(temporary_gpkg)) {
  unlink(temporary_gpkg, force = TRUE)
}

message("Writing conveyance_segments spatial layer...")

sf::st_write(
  segments_out,
  temporary_gpkg,
  layer = "conveyance_segments",
  quiet = TRUE,
  delete_dsn = TRUE
)

message("Writing conveyance_labels spatial layer...")

sf::st_write(
  labels_out,
  temporary_gpkg,
  layer = "conveyance_labels",
  append = TRUE,
  quiet = TRUE
)

message("Opening temporary GeoPackage attribute-table connection...")

connection <- DBI::dbConnect(
  RSQLite::SQLite(),
  temporary_gpkg
)

tryCatch(
  {
    message("Writing normalized attribute and QA tables...")

    DBI::dbWriteTable(
      connection,
      "facilities",
      facilities,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      connection,
      "facility_aliases",
      facility_aliases,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      connection,
      "projects",
      projects,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      connection,
      "facility_projects",
      facility_projects |>
        dplyr::select(
          facility_id,
          project_id,
          project_family,
          project_name,
          project_division,
          project_unit,
          project_subunit,
          project_path,
          membership_role,
          assignment_confidence,
          evidence_rule_id,
          evidence_url
        ),
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      connection,
      "source_crosswalk",
      source_crosswalk,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      connection,
      "qa_unresolved",
      qa_unresolved,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      connection,
      "qa_exact_name_choices",
      qa_exact_name_choices,
      overwrite = TRUE
    )


    DBI::dbWriteTable(
      connection,
      "qa_same_name_multiple_facilities",
      qa_same_name_multiple_facilities,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      connection,
      "facility_identity_overrides",
      identity_overrides,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      connection,
      "label_overrides",
      label_overrides,
      overwrite = TRUE
    )


    DBI::dbWriteTable(
      connection,
      "project_membership_crosswalk",
      project_crosswalk,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      connection,
      "qa_expected_cvp_facilities",
      qa_expected_cvp_facilities,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      connection,
      "qa_reclamation_unassigned",
      qa_reclamation_unassigned,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      connection,
      "supplemental_geometry_registry",
      qa_supplemental_registry,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      connection,
      "qa_blm_spatial_summary",
      qa_blm_spatial_summary,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      connection,
      "qa_blm_spatial_distance_bins",
      qa_blm_spatial_distance_bins,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      connection,
      "qa_blm_spatial_touch_or_sliver",
      qa_blm_spatial_touch_or_sliver,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      connection,
      "qa_blm_spatial_facility_preview",
      qa_blm_spatial_facility_preview,
      overwrite = TRUE
    )

    DBI::dbWriteTable(
      connection,
      "schema_dictionary",
      readr::read_csv(
        SCHEMA_CSV,
        show_col_types = FALSE
      ),
      overwrite = TRUE
    )

    attribute_tables <- c(
      "facilities",
      "facility_aliases",
      "projects",
      "facility_projects",
      "source_crosswalk",
      "qa_unresolved",
      "qa_exact_name_choices",
      "qa_same_name_multiple_facilities",
      "facility_identity_overrides",
      "label_overrides",
      "project_membership_crosswalk",
      "qa_expected_cvp_facilities",
      "qa_reclamation_unassigned",
      "supplemental_geometry_registry",
      "qa_blm_spatial_summary",
      "qa_blm_spatial_distance_bins",
      "qa_blm_spatial_touch_or_sliver",
      "qa_blm_spatial_facility_preview",
      "schema_dictionary"
    )

    message("Registering attribute tables in gpkg_contents...")

    for (table_name in attribute_tables) {
      DBI::dbExecute(
        connection,
        paste0(
          "INSERT OR REPLACE INTO gpkg_contents ",
          "(table_name, data_type, identifier, description, ",
          "last_change, srs_id) ",
          "VALUES (?, 'attributes', ?, ?, ",
          "strftime('%Y-%m-%dT%H:%M:%fZ','now'), NULL)"
        ),
        params = list(
          table_name,
          table_name,
          paste(
            "BRIM conveyance pilot table:",
            table_name
          )
        )
      )
    }
  },
  finally = {
    if (
      exists("connection", inherits = FALSE) &&
      DBI::dbIsValid(connection)
    ) {
      DBI::dbDisconnect(connection)
    }
  }
)

if (!file.exists(temporary_gpkg)) {
  stop(
    "Temporary GeoPackage was not created:\n  ",
    temporary_gpkg
  )
}

message("Copying completed GeoPackage into the project output folder...")

if (file.exists(GPKG_PATH)) {
  unlink(
    GPKG_PATH,
    force = TRUE
  )
}

copied_gpkg <- file.copy(
  from = temporary_gpkg,
  to = GPKG_PATH,
  overwrite = TRUE,
  copy.mode = TRUE,
  copy.date = TRUE
)

if (!isTRUE(copied_gpkg) || !file.exists(GPKG_PATH)) {
  stop(
    "The completed temporary GeoPackage could not be copied to:\n  ",
    GPKG_PATH,
    "\nTemporary file remains at:\n  ",
    temporary_gpkg
  )
}

unlink(
  temporary_gpkg,
  force = TRUE
)

message("GeoPackage completed successfully.")
message("Writing CSV exports...")

readr::write_csv(
  sf::st_drop_geometry(segments_out),
  file.path(CSV_DIR, "conveyance_segments.csv"),
  na = ""
)

readr::write_csv(
  facilities,
  file.path(CSV_DIR, "facilities.csv"),
  na = ""
)

readr::write_csv(
  facility_aliases,
  file.path(CSV_DIR, "facility_aliases.csv"),
  na = ""
)

readr::write_csv(
  projects,
  file.path(CSV_DIR, "projects.csv"),
  na = ""
)

readr::write_csv(
  facility_projects,
  file.path(CSV_DIR, "facility_projects.csv"),
  na = ""
)

readr::write_csv(
  source_crosswalk,
  file.path(CSV_DIR, "source_crosswalk.csv"),
  na = ""
)

readr::write_csv(
  qa_unresolved,
  file.path(CSV_DIR, "qa_unresolved.csv"),
  na = ""
)

readr::write_csv(
  qa_exact_name_choices,
  file.path(CSV_DIR, "qa_exact_name_choices.csv"),
  na = ""
)


readr::write_csv(
  qa_same_name_multiple_facilities,
  file.path(CSV_DIR, "qa_same_name_multiple_facilities.csv"),
  na = ""
)

readr::write_csv(
  identity_overrides,
  file.path(CSV_DIR, "facility_identity_overrides.csv"),
  na = ""
)

readr::write_csv(
  label_overrides,
  file.path(CSV_DIR, "label_overrides.csv"),
  na = ""
)


readr::write_csv(
  project_crosswalk,
  file.path(CSV_DIR, "project_membership_crosswalk.csv"),
  na = ""
)

readr::write_csv(
  qa_expected_cvp_facilities,
  file.path(CSV_DIR, "qa_expected_cvp_facilities.csv"),
  na = ""
)

readr::write_csv(
  qa_reclamation_unassigned,
  file.path(CSV_DIR, "qa_reclamation_unassigned.csv"),
  na = ""
)

readr::write_csv(
  qa_supplemental_registry,
  file.path(CSV_DIR, "qa_supplemental_geometry_registry.csv"),
  na = ""
)

readr::write_csv(
  qa_blm_spatial_summary,
  file.path(CSV_DIR, "qa_blm_spatial_summary.csv"),
  na = ""
)

readr::write_csv(
  qa_blm_spatial_distance_bins,
  file.path(CSV_DIR, "qa_blm_spatial_distance_bins.csv"),
  na = ""
)

readr::write_csv(
  qa_blm_spatial_touch_or_sliver,
  file.path(CSV_DIR, "qa_blm_spatial_touch_or_sliver.csv"),
  na = ""
)

readr::write_csv(
  qa_blm_spatial_facility_preview,
  file.path(CSV_DIR, "qa_blm_spatial_facility_preview.csv"),
  na = ""
)


message("Writing BRIM-ready RDS exports...")

saveRDS(
  segments_out,
  file.path(
    BRIM_EXPORT_DIR,
    "conveyance_segments_brim.rds"
  )
)

saveRDS(
  labels_out,
  file.path(
    BRIM_EXPORT_DIR,
    "conveyance_labels_brim.rds"
  )
)

saveRDS(
  facilities,
  file.path(
    BRIM_EXPORT_DIR,
    "conveyance_facilities_brim.rds"
  )
)


saveRDS(
  segments_brim_map,
  file.path(
    BRIM_EXPORT_DIR,
    "conveyance_segments_brim_map.rds"
  )
)

saveRDS(
  labels_brim_map,
  file.path(
    BRIM_EXPORT_DIR,
    "conveyance_labels_brim_map.rds"
  )
)

saveRDS(
  facilities_brim_map,
  file.path(
    BRIM_EXPORT_DIR,
    "conveyance_facilities_brim_map.rds"
  )
)

message(
  "Lean BRIM controller RDS sizes (MB): segments = ",
  round(
    file.info(
      file.path(
        BRIM_EXPORT_DIR,
        "conveyance_segments_brim_map.rds"
      )
    )$size / 1024^2,
    3
  ),
  "; facilities = ",
  round(
    file.info(
      file.path(
        BRIM_EXPORT_DIR,
        "conveyance_facilities_brim_map.rds"
      )
    )$size / 1024^2,
    3
  ),
  "; labels = ",
  round(
    file.info(
      file.path(
        BRIM_EXPORT_DIR,
        "conveyance_labels_brim_map.rds"
      )
    )$size / 1024^2,
    3
  )
)

