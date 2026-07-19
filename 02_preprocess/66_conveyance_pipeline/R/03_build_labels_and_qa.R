# ---- Label anchors -----------------------------------------------------------

make_label_point <- function(segment_sf) {
  segment_sf <- standardize_lines(segment_sf)

  if (nrow(segment_sf) == 0L) {
    return(sf::st_sfc(crs = sf::st_crs(segment_sf)))
  }

  lengths <- as.numeric(sf::st_length(segment_sf))
  chosen <- segment_sf[which.max(lengths), ]

  point <- tryCatch(
    suppressWarnings(
      sf::st_line_sample(
        sf::st_geometry(chosen),
        sample = 0.5
      )
    ),
    error = function(e) {
      sf::st_centroid(sf::st_geometry(chosen))
    }
  )

  if (length(point) == 0L || any(sf::st_is_empty(point))) {
    point <- sf::st_centroid(sf::st_geometry(chosen))
  }

  point <- suppressWarnings(sf::st_cast(point, "POINT"))

  if (length(point) > 1L) {
    point <- point[1]
  }

  point
}

label_rows <- list()

for (facility_value in unique(segments$facility_id)) {
  facility_segments <- segments[
    segments$facility_id == facility_value,
  ]

  point <- make_label_point(facility_segments)

  if (
    length(point) == 0L ||
      all(sf::st_is_empty(point))
  ) {
    next
  }

  facility_record <- facilities[
    facilities$facility_id == facility_value,
  ]

  label_rows[[length(label_rows) + 1L]] <- sf::st_sf(
    label_id = paste0("LBL_", sub("^CVY_", "", facility_value)),
    facility_id = facility_value,
    segment_id = facility_segments$segment_id[
      which.max(facility_segments$length_mi)
    ],
    lbl = facility_record$lbl,
    lbl_full = facility_record$lbl_full,
    lbl_priority = dplyr::case_when(
      facility_record$display_rank == "statewide_major" ~ 100L,
      facility_record$display_rank == "regional_major" ~ 75L,
      facility_record$display_rank == "medium" ~ 50L,
      TRUE ~ 25L
    ),
    lbl_min_zoom = dplyr::case_when(
      facility_record$display_rank == "statewide_major" ~ 5L,
      facility_record$display_rank == "regional_major" ~ 7L,
      facility_record$display_rank == "medium" ~ 9L,
      TRUE ~ 11L
    ),
    lbl_max_zoom = 20L,
    label_class = facility_record$display_rank,
    show_default = facility_record$display_rank != "local_supporting",
    canonical_name = facility_record$canonical_name,
    aliases = facility_record$aliases,
    parent_system = facility_record$parent_system,
    project_family = facility_record$project_family,
    project_name = facility_record$project_name,
    project_division = facility_record$project_division,
    project_unit = facility_record$project_unit,
    project_subunit = facility_record$project_subunit,
    project_path = facility_record$project_path,
    ownership_class = facility_record$ownership_class,
    facility_group = facility_record$facility_group,
    display_rank = facility_record$display_rank,
    geometry_confidence = facility_record$geometry_confidence,
    search_text = tolower(paste(
      facility_record$canonical_name,
      facility_record$aliases,
      facility_record$parent_system,
      facility_record$project_name,
      facility_record$project_division,
      facility_record$project_unit,
      facility_record$project_subunit
    )),
    geometry = point,
    crs = 3310
  )
}

labels <- dplyr::bind_rows(label_rows)


if (nrow(label_overrides) > 0L && nrow(labels) > 0L) {
  for (override_index in seq_len(nrow(label_overrides))) {
    override <- label_overrides[override_index, ]

    pattern <- clean_text(override$canonical_name_regex)

    if (is.na(pattern)) {
      next
    }

    matched <- grepl(
      pattern,
      labels$canonical_name,
      ignore.case = TRUE,
      perl = TRUE
    )

    if (!any(matched)) {
      next
    }

    lbl_override <- clean_text(override$lbl_override)
    show_default_override <- clean_text(override$show_default)

    if (!is.na(lbl_override)) {
      labels$lbl[matched] <- lbl_override
    }

    if (!is.na(override$lbl_min_zoom)) {
      labels$lbl_min_zoom[matched] <- as.integer(
        override$lbl_min_zoom
      )
    }

    if (!is.na(override$lbl_max_zoom)) {
      labels$lbl_max_zoom[matched] <- as.integer(
        override$lbl_max_zoom
      )
    }

    if (!is.na(show_default_override)) {
      labels$show_default[matched] <- tolower(
        show_default_override
      ) %in% c("true", "yes", "1")
    }
  }
}

# ---- Source crosswalk and QA -------------------------------------------------

used_crosswalk <- segments |>
  sf::st_drop_geometry() |>
  dplyr::select(
    facility_id,
    segment_id,
    decision_id,
    source_ids,
    geometry_decision
  ) |>
  tidyr::separate_rows(
    source_ids,
    sep = ";"
  ) |>
  dplyr::filter(source_ids != "") |>
  dplyr::transmute(
    source_id = source_ids,
    facility_id = facility_id,
    segment_id = segment_id,
    source_action = "used",
    decision_id = decision_id,
    qa_notes = geometry_decision
  )

all_sources <- dplyr::bind_rows(
  major_m |>
    sf::st_drop_geometry() |>
    dplyr::transmute(
      source_id,
      source_name,
      source_row,
      source_feature_name,
      source_length_mi = source_length_m / 1609.344
    ),

  delta_m |>
    sf::st_drop_geometry() |>
    dplyr::transmute(
      source_id,
      source_name,
      source_row,
      source_feature_name,
      source_length_mi = source_length_m / 1609.344
    )
)

decision_source_rows <- dplyr::bind_rows(
  decisions |>
    dplyr::select(
      decision_id,
      include_curated,
      major_source_rows,
      notes
    ) |>
    tidyr::separate_rows(
      major_source_rows,
      sep = "\\|"
    ) |>
    dplyr::filter(
      !is.na(major_source_rows),
      major_source_rows != ""
    ) |>
    dplyr::transmute(
      source_id = sprintf(
        "MAJOR_%04d",
        as.integer(major_source_rows)
      ),
      decision_id,
      include_curated,
      notes
    ),

  decisions |>
    dplyr::select(
      decision_id,
      include_curated,
      delta_source_rows,
      notes
    ) |>
    tidyr::separate_rows(
      delta_source_rows,
      sep = "\\|"
    ) |>
    dplyr::filter(
      !is.na(delta_source_rows),
      delta_source_rows != ""
    ) |>
    dplyr::transmute(
      source_id = sprintf(
        "DELTA_%04d",
        as.integer(delta_source_rows)
      ),
      decision_id,
      include_curated,
      notes
    )
) |>
  dplyr::distinct()

exact_suppressed_ids <- c(
  sprintf("MAJOR_%04d", unique(suppress_major_exact)),
  sprintf("DELTA_%04d", unique(suppress_delta_exact))
)

source_crosswalk <- all_sources |>
  dplyr::left_join(
    used_crosswalk,
    by = "source_id"
  ) |>
  dplyr::left_join(
    decision_source_rows |>
      dplyr::rename(
        decision_id_planned = decision_id,
        include_curated_planned = include_curated,
        decision_notes_planned = notes
      ),
    by = "source_id"
  ) |>
  dplyr::mutate(
    source_action = dplyr::case_when(
      !is.na(segment_id) ~ "used",
      source_id %in% exact_suppressed_ids ~ "exact_duplicate_suppressed",
      !is.na(decision_id_planned) &
        !include_curated_planned ~ "unresolved_omitted",
      !is.na(decision_id_planned) ~ "decision_alternate_suppressed",
      is.na(source_feature_name) &
        source_length_mi < MIN_UNNAMED_LENGTH_MI ~
        "short_unnamed_provisional_omitted",
      TRUE ~ "not_used_check_required"
    ),
    decision_id = dplyr::coalesce(
      decision_id,
      decision_id_planned,
      ""
    ),
    qa_notes = dplyr::coalesce(
      qa_notes,
      decision_notes_planned,
      ""
    )
  ) |>
  dplyr::select(
    source_id,
    source_name,
    source_row,
    source_feature_name,
    source_length_mi,
    facility_id,
    segment_id,
    source_action,
    decision_id,
    qa_notes
  )

qa_unresolved <- decisions |>
  dplyr::filter(!include_curated) |>
  dplyr::transmute(
    decision_id,
    source_rows = paste0(
      "Major: ",
      dplyr::coalesce(major_source_rows, ""),
      " | Delta: ",
      dplyr::coalesce(delta_source_rows, "")
    ),
    canonical_or_candidate_name = canonical_name,
    issue = notes,
    recommended_next_action = dplyr::case_when(
      component_mode == "case35_split_or_omit" ~
        "Split into West Interception Canal and Live Oak Canal where independently supported; otherwise retain omission.",
      TRUE ~ "Manual visual/authoritative review before inclusion."
    )
  )

qa_exact_name_choices <- exact_pairs |>
  dplyr::mutate(
    selected_source = dplyr::case_when(
      major_source_row %in% suppress_major_exact ~ "DeltaMAPP",
      delta_source_row %in% suppress_delta_exact ~ "Major Conveyance",
      TRUE ~ "Check"
    )
  )


qa_same_name_multiple_facilities <- facilities |>
  dplyr::mutate(
    canonical_name_norm = normalize_name(canonical_name)
  ) |>
  dplyr::group_by(canonical_name_norm) |>
  dplyr::filter(
    canonical_name_norm != "",
    dplyr::n_distinct(facility_id) > 1L,
    !canonical_name_norm %in% generic_names
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(
    canonical_name_norm,
    canonical_name,
    facility_id
  )


facility_qa_text <- facilities |>
  dplyr::mutate(
    qa_match_text = paste(
      canonical_name,
      aliases,
      parent_system,
      project_family,
      project_name,
      project_division,
      project_unit,
      project_subunit,
      owner_agency,
      operator_agency
    )
  )

# Base R ignores ignore.case when fixed = TRUE. Normalize both sides once
# instead so expected-project QA remains literal but genuinely case-insensitive.
conveyance_fixed_contains_ci <- function(needle, haystack) {
  needle <- tolower(dplyr::coalesce(as.character(needle), ""))
  haystack <- tolower(dplyr::coalesce(as.character(haystack), ""))
  grepl(needle, haystack, fixed = TRUE)
}

expected_qa_rows <- list()

for (expected_index in seq_len(nrow(expected_cvp))) {
  expected <- expected_cvp[expected_index, ]

  pattern <- clean_text(expected$match_regex)

  matched_facilities <- if (is.na(pattern)) {
    facility_qa_text[0, ]
  } else {
    facility_qa_text[
      grepl(
        pattern,
        facility_qa_text$qa_match_text,
        ignore.case = TRUE,
        perl = TRUE
      ),
    ]
  }

  present <- nrow(matched_facilities) > 0L

  assigned_correctly <- if (!present) {
    FALSE
  } else {
    family_ok <- conveyance_fixed_contains_ci(
      dplyr::coalesce(expected$expected_project_family, "CVP"),
      matched_facilities$project_family
    )

    name_ok <- conveyance_fixed_contains_ci(
      dplyr::coalesce(expected$expected_project_name, "Central Valley Project"),
      matched_facilities$project_name
    )

    division_value <- clean_text(expected$expected_project_division)
    division_ok <- if (is.na(division_value)) {
      rep(TRUE, nrow(matched_facilities))
    } else {
      conveyance_fixed_contains_ci(
        division_value,
        matched_facilities$project_division
      )
    }

    unit_value <- clean_text(expected$expected_project_unit)
    unit_ok <- if (is.na(unit_value)) {
      rep(TRUE, nrow(matched_facilities))
    } else {
      conveyance_fixed_contains_ci(
        unit_value,
        paste(
          dplyr::coalesce(matched_facilities$project_unit, ""),
          dplyr::coalesce(matched_facilities$project_subunit, "")
        )
      )
    }

    any(family_ok & name_ok & division_ok & unit_ok)
  }

  expected_qa_rows[[length(expected_qa_rows) + 1L]] <- data.frame(
    expected_id = expected$expected_id,
    expected_name = expected$expected_name,
    expected_level = expected$expected_level,
    present_in_canonical_geometry = present,
    assigned_to_expected_project = assigned_correctly,
    audit_status = dplyr::case_when(
      !present ~ "missing_geometry",
      present && !assigned_correctly ~ "present_but_misassigned",
      TRUE ~ "present_and_assigned"
    ),
    matched_facility_ids = if (present) {
      collapse_unique(matched_facilities$facility_id)
    } else {
      ""
    },
    matched_canonical_names = if (present) {
      collapse_unique(matched_facilities$canonical_name)
    } else {
      ""
    },
    matched_project_paths = if (present) {
      collapse_unique(matched_facilities$project_path)
    } else {
      ""
    },
    evidence_url = expected$evidence_url,
    stringsAsFactors = FALSE
  )
}

qa_expected_cvp_facilities <- dplyr::bind_rows(expected_qa_rows)

qa_reclamation_unassigned <- facilities |>
  dplyr::filter(
    grepl(
      "Reclamation|Bureau of Reclamation|\\bBOR\\b|Federal",
      paste(owner_agency, operator_agency),
      ignore.case = TRUE
    ),
    grepl(
      "Reclamation project \\(unit unassigned\\)|Unassigned",
      paste(project_name, project_unit),
      ignore.case = TRUE
    )
  ) |>
  dplyr::select(
    facility_id,
    canonical_name,
    aliases,
    owner_agency,
    operator_agency,
    project_family,
    project_name,
    geometry_confidence
  ) |>
  dplyr::mutate(
    recommended_action = "Add or refine an authoritative project crosswalk rule."
  )

qa_supplemental_registry <- supplement_registry |>
  dplyr::mutate(
    source_path_resolved = dplyr::case_when(
      is.na(clean_text(source_path_or_url)) ~ "",
      grepl("^https?://", source_path_or_url, ignore.case = TRUE) ~
        source_path_or_url,
      TRUE ~ file.path(PROJECT_ROOT, source_path_or_url)
    ),
    source_exists_or_is_url = dplyr::case_when(
      source_path_resolved == "" ~ FALSE,
      grepl("^https?://", source_path_resolved, ignore.case = TRUE) ~ TRUE,
      TRUE ~ file.exists(source_path_resolved)
    ),
    ready_for_ingest =
      tolower(dplyr::coalesce(status, "")) == "ready" &
      source_exists_or_is_url &
      (
        (
          !is.na(clean_text(id_field)) &
            !is.na(clean_text(id_value))
        ) |
          (
            !is.na(clean_text(name_field)) &
              !is.na(clean_text(name_value))
          )
      )
  )

