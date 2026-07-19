# ---- Classify types and form logical facilities ------------------------------

combined_type_text <- paste(
  dplyr::coalesce(segments$canonical_name, ""),
  dplyr::coalesce(segments$source_type, ""),
  dplyr::coalesce(segments$parent_system, "")
)

segments$facility_type <- dplyr::case_when(
  grepl("tailrace", combined_type_text, ignore.case = TRUE) ~ "Tailrace",
  grepl("penstock", combined_type_text, ignore.case = TRUE) ~ "Penstock",
  grepl("siphon", combined_type_text, ignore.case = TRUE) ~ "Siphon",
  grepl("tunnel", combined_type_text, ignore.case = TRUE) ~ "Tunnel",
  grepl("pipeline", combined_type_text, ignore.case = TRUE) ~ "Pipeline",
  grepl("conduit", combined_type_text, ignore.case = TRUE) ~ "Conduit",
  grepl("intertie", combined_type_text, ignore.case = TRUE) ~ "Intertie",
  grepl("wasteway", combined_type_text, ignore.case = TRUE) ~ "Wasteway",
  grepl("drain", combined_type_text, ignore.case = TRUE) ~ "Drain",
  grepl("flood canal", combined_type_text, ignore.case = TRUE) ~ "Flood canal",
  grepl("lateral", combined_type_text, ignore.case = TRUE) ~ "Lateral",
  grepl("ditch", combined_type_text, ignore.case = TRUE) ~ "Ditch",
  grepl("aqueduct", combined_type_text, ignore.case = TRUE) ~ "Aqueduct",
  grepl("canal", combined_type_text, ignore.case = TRUE) ~ "Canal",
  TRUE ~ "Unknown"
)

segments$facility_group <- dplyr::case_when(
  segments$facility_type %in% c(
    "Penstock",
    "Tailrace"
  ) ~ "Power conveyance",

  segments$facility_type %in% c(
    "Pipeline",
    "Tunnel",
    "Conduit",
    "Siphon",
    "Intertie"
  ) ~ "Closed conveyance",

  segments$facility_type %in% c(
    "Drain",
    "Wasteway"
  ) ~ "Drainage",

  segments$facility_type == "Flood canal" ~ "Flood conveyance",

  segments$facility_type %in% c(
    "Lateral",
    "Ditch"
  ) ~ "Distribution/local",

  segments$facility_type %in% c(
    "Canal",
    "Aqueduct"
  ) ~ "Open conveyance",

  TRUE ~ "Other/unknown"
)

segments$network_role <- dplyr::case_when(
  grepl("intake", combined_type_text, ignore.case = TRUE) ~ "Intake",
  grepl("tailrace", combined_type_text, ignore.case = TRUE) ~ "Return flow",
  grepl("penstock", combined_type_text, ignore.case = TRUE) ~ "Power delivery",
  grepl("lateral|ditch", combined_type_text, ignore.case = TRUE) ~ "Distribution",
  grepl("branch", combined_type_text, ignore.case = TRUE) ~ "Branch",
  grepl("drain|wasteway", combined_type_text, ignore.case = TRUE) ~ "Drainage",
  grepl("main|aqueduct", combined_type_text, ignore.case = TRUE) ~ "Mainline",
  TRUE ~ "Unknown"
)

generic_names <- c(
  "canal",
  "main canal",
  "north canal",
  "south canal",
  "east canal",
  "west canal",
  "central canal",
  "north main canal",
  "south main canal",
  "east main canal",
  "west main canal",
  "drain",
  "main drain",
  "ditch",
  "lateral",
  "pipeline",
  "aqueduct"
)

segments$facility_name_key <- normalize_name(
  segments$canonical_name
)

is_generic <- segments$facility_name_key %in% generic_names |
  grepl(
    "^unnamed conveyance|^unnamed$|^unknown",
    segments$canonical_name,
    ignore.case = TRUE
  )

# Specifically named facilities are identified primarily by their normalized
# name. Operator and capitalization are attributes, not identity. Generic names
# remain disambiguated by system/operator and source record.
segments$facility_key <- segments$facility_name_key

segments$facility_key[is_generic] <- paste(
  segments$facility_name_key[is_generic],
  normalize_name(segments$parent_system[is_generic]),
  normalize_name(segments$source_operator[is_generic]),
  segments$source_ids[is_generic],
  sep = "|"
)

if (nrow(identity_overrides) > 0L) {
  for (override_index in seq_len(nrow(identity_overrides))) {
    override <- identity_overrides[override_index, ]

    name_pattern <- clean_text(override$canonical_name_regex)
    parent_pattern <- clean_text(override$parent_system_regex)
    operator_pattern <- clean_text(override$operator_regex)
    source_pattern <- clean_text(override$source_ids_regex)

    matched <- rep(TRUE, nrow(segments))

    if (!is.na(name_pattern)) {
      matched <- matched & grepl(
        name_pattern,
        segments$canonical_name,
        ignore.case = TRUE,
        perl = TRUE
      )
    }

    if (!is.na(parent_pattern)) {
      matched <- matched & grepl(
        parent_pattern,
        dplyr::coalesce(segments$parent_system, ""),
        ignore.case = TRUE,
        perl = TRUE
      )
    }

    if (!is.na(operator_pattern)) {
      matched <- matched & grepl(
        operator_pattern,
        dplyr::coalesce(segments$source_operator, ""),
        ignore.case = TRUE,
        perl = TRUE
      )
    }

    if (!is.na(source_pattern)) {
      matched <- matched & grepl(
        source_pattern,
        dplyr::coalesce(segments$source_ids, ""),
        ignore.case = TRUE,
        perl = TRUE
      )
    }

    if (!any(matched)) {
      next
    }

    key_override <- clean_text(override$facility_key_override)
    name_override <- clean_text(override$canonical_name_override)

    if (!is.na(key_override)) {
      segments$facility_key[matched] <- normalize_name(key_override)
    }

    if (!is.na(name_override)) {
      segments$canonical_name[matched] <- name_override
    }
  }
}

segments$facility_id <- paste0(
  "CVY_",
  stable_hash8(segments$facility_key)
)

# Consolidate capitalization variants such as South Coast Conduit /
# SOUTH COAST CONDUIT under one public canonical name.
facility_display_names <- segments |>
  sf::st_drop_geometry() |>
  dplyr::group_by(facility_id) |>
  dplyr::summarise(
    facility_display_name = choose_display_name(canonical_name),
    .groups = "drop"
  )

segments <- segments |>
  dplyr::left_join(
    facility_display_names,
    by = "facility_id"
  ) |>
  dplyr::mutate(
    canonical_name = dplyr::coalesce(
      facility_display_name,
      canonical_name
    )
  ) |>
  dplyr::select(-facility_display_name)

segments$length_mi <- as.numeric(sf::st_length(segments)) / 1609.344

# ---- Project memberships -----------------------------------------------------

facility_seed <- segments |>
  sf::st_drop_geometry() |>
  dplyr::group_by(facility_id) |>
  dplyr::summarise(
    canonical_name = choose_display_name(canonical_name),
    aliases = collapse_unique(aliases),
    parent_system = collapse_unique(parent_system),
    source_operator = collapse_unique(source_operator),
    source_owner = collapse_unique(source_owner),
    project_family_seed = collapse_unique(project_family_seed),
    project_name_seed = collapse_unique(project_name_seed),
    project_unit_seed = collapse_unique(project_unit_seed),
    .groups = "drop"
  )

build_project_path <- function(
  project_division = "",
  project_unit = "",
  project_subunit = ""
) {
  values <- clean_text(c(
    project_division,
    project_unit,
    project_subunit
  ))

  values <- values[!is.na(values)]

  if (length(values) == 0L) {
    return("")
  }

  paste(unique(values), collapse = " > ")
}

project_rows <- list()

append_project <- function(
  facility_id,
  project_family,
  project_name,
  project_division = "",
  project_unit = "",
  project_subunit = "",
  membership_role = "facility",
  confidence = "provisional",
  evidence_rule_id = "",
  evidence_url = ""
) {
  project_rows[[length(project_rows) + 1L]] <<- data.frame(
    facility_id = facility_id,
    project_family = project_family,
    project_name = project_name,
    project_division = project_division,
    project_unit = project_unit,
    project_subunit = project_subunit,
    project_path = build_project_path(
      project_division,
      project_unit,
      project_subunit
    ),
    membership_role = membership_role,
    assignment_confidence = confidence,
    evidence_rule_id = evidence_rule_id,
    evidence_url = evidence_url,
    stringsAsFactors = FALSE
  )
}

facility_has_project <- function(facility_id) {
  if (length(project_rows) == 0L) {
    return(FALSE)
  }

  any(vapply(
    project_rows,
    function(row) identical(row$facility_id[[1]], facility_id),
    logical(1)
  ))
}

for (i in seq_len(nrow(facility_seed))) {
  f <- facility_seed[i, ]

  seeded_families <- split_values(f$project_family_seed)
  seeded_names <- split_values(f$project_name_seed)
  seeded_units <- split_values(f$project_unit_seed)

  if (length(seeded_families) > 0L) {
    count <- max(
      length(seeded_families),
      length(seeded_names),
      length(seeded_units),
      1L
    )

    for (j in seq_len(count)) {
      append_project(
        f$facility_id,
        seeded_families[pmin(j, length(seeded_families))],
        if (length(seeded_names) > 0L) {
          seeded_names[pmin(j, length(seeded_names))]
        } else {
          seeded_families[pmin(j, length(seeded_families))]
        },
        project_unit = if (length(seeded_units) > 0L) {
          seeded_units[pmin(j, length(seeded_units))]
        } else {
          ""
        },
        membership_role = ifelse(
          length(seeded_families) > 1L,
          "joint_use",
          "facility"
        ),
        confidence = "reviewed",
        evidence_rule_id = "DECISION_LEDGER_SEED"
      )
    }
  }

  text_to_match <- paste(
    f$canonical_name,
    f$aliases,
    f$parent_system,
    f$source_operator,
    f$source_owner
  )

  for (rule_index in seq_len(nrow(project_crosswalk))) {
    rule <- project_crosswalk[rule_index, ]

    pattern <- clean_text(rule$match_regex)

    if (is.na(pattern)) {
      next
    }

    matched <- isTRUE(grepl(
      pattern,
      text_to_match,
      ignore.case = TRUE,
      perl = TRUE
    ))

    exclude_pattern <- clean_text(rule$exclude_regex)

    excluded <- if (is.na(exclude_pattern)) {
      FALSE
    } else {
      isTRUE(grepl(
        exclude_pattern,
        text_to_match,
        ignore.case = TRUE,
        perl = TRUE
      ))
    }

    if (matched && !excluded) {
      append_project(
        facility_id = f$facility_id,
        project_family = dplyr::coalesce(
          clean_text(rule$project_family),
          "Unknown/unassigned"
        ),
        project_name = dplyr::coalesce(
          clean_text(rule$project_name),
          "Unassigned"
        ),
        project_division = dplyr::coalesce(
          clean_text(rule$project_division),
          ""
        ),
        project_unit = dplyr::coalesce(
          clean_text(rule$project_unit),
          ""
        ),
        project_subunit = dplyr::coalesce(
          clean_text(rule$project_subunit),
          ""
        ),
        membership_role = dplyr::coalesce(
          clean_text(rule$membership_role),
          "facility"
        ),
        confidence = dplyr::coalesce(
          clean_text(rule$assignment_confidence),
          "authoritative_crosswalk"
        ),
        evidence_rule_id = dplyr::coalesce(
          clean_text(rule$rule_id),
          ""
        ),
        evidence_url = dplyr::coalesce(
          clean_text(rule$evidence_url),
          ""
        )
      )
    }
  }

  if (!facility_has_project(f$facility_id)) {
    if (grepl(
      "Irrigation District|Water District|Water Authority|City|County|SFPUC|District system|conveyance system",
      text_to_match,
      ignore.case = TRUE
    )) {
      project_name <- clean_text(f$parent_system)

      if (is.na(project_name)) {
        project_name <- clean_text(f$source_operator)
      }

      if (is.na(project_name)) {
        project_name <- "Local/regional public system"
      }

      append_project(
        f$facility_id,
        "Local/regional public",
        project_name,
        confidence = "provisional",
        evidence_rule_id = "LOCAL_FALLBACK"
      )
    } else if (grepl(
      "Reclamation|Bureau of Reclamation|\\bBOR\\b",
      text_to_match,
      ignore.case = TRUE
    )) {
      append_project(
        f$facility_id,
        "Other Reclamation",
        "Reclamation project (unit unassigned)",
        confidence = "needs_project_crosswalk",
        evidence_rule_id = "RECLAMATION_UNASSIGNED_FALLBACK"
      )
    } else {
      append_project(
        f$facility_id,
        "Unknown/unassigned",
        "Unassigned",
        confidence = "unassigned",
        evidence_rule_id = "UNASSIGNED_FALLBACK"
      )
    }
  }
}

facility_projects <- dplyr::bind_rows(project_rows) |>
  dplyr::arrange(
    facility_id,
    dplyr::desc(assignment_confidence == "reviewed"),
    dplyr::desc(assignment_confidence == "authoritative_crosswalk"),
    project_family,
    project_name,
    project_path
  ) |>
  dplyr::distinct(
    facility_id,
    project_family,
    project_name,
    project_division,
    project_unit,
    project_subunit,
    membership_role,
    .keep_all = TRUE
  )

facility_projects$project_key <- paste(
  facility_projects$project_family,
  facility_projects$project_name,
  facility_projects$project_division,
  facility_projects$project_unit,
  facility_projects$project_subunit,
  sep = "|"
)

facility_projects$project_id <- paste0(
  "PRJ_",
  stable_hash8(facility_projects$project_key)
)

projects <- facility_projects |>
  dplyr::select(
    project_id,
    project_family,
    project_name,
    project_division,
    project_unit,
    project_subunit,
    project_path
  ) |>
  dplyr::distinct() |>
  dplyr::mutate(
    parent_project_id = NA_character_
  )

facility_project_summary <- facility_projects |>
  dplyr::group_by(facility_id) |>
  dplyr::summarise(
    project_family = collapse_unique(project_family),
    project_name = collapse_unique(project_name),
    project_division = collapse_unique(project_division),
    project_unit = collapse_unique(project_unit),
    project_subunit = collapse_unique(project_subunit),
    project_path = collapse_unique(project_path),
    .groups = "drop"
  )

segments <- segments |>
  dplyr::left_join(
    facility_project_summary,
    by = "facility_id"
  )

# ---- Ownership, rank, labels, and final segment IDs --------------------------

ownership_text <- paste(
  dplyr::coalesce(segments$project_family, ""),
  dplyr::coalesce(segments$source_owner, ""),
  dplyr::coalesce(segments$source_operator, ""),
  dplyr::coalesce(segments$parent_system, "")
)

segments$ownership_class <- dplyr::case_when(
  grepl("CVP", segments$project_family, fixed = TRUE) &
    grepl("SWP", segments$project_family, fixed = TRUE) ~ "Joint federal/state",

  grepl("CVP|Other Reclamation|Federal", ownership_text, ignore.case = TRUE) ~ "Federal",

  grepl("SWP|State Water Project|California state", ownership_text, ignore.case = TRUE) ~ "California state",

  grepl(
    "Irrigation District|Water District|Water Authority|City|County|SFPUC|Local/regional public|Local",
    ownership_text,
    ignore.case = TRUE
  ) ~ "Local/regional public",

  grepl(
    "Company|Corporation|Private|Mutual Water",
    ownership_text,
    ignore.case = TRUE
  ) ~ "Private",

  grepl("Fed/State|Joint", ownership_text, ignore.case = TRUE) ~ "Joint/multiple",

  TRUE ~ "Unknown"
)

facility_lengths <- segments |>
  sf::st_drop_geometry() |>
  dplyr::group_by(facility_id) |>
  dplyr::summarise(
    facility_length_mi = sum(length_mi, na.rm = TRUE),
    .groups = "drop"
  )

segments <- segments |>
  dplyr::left_join(
    facility_lengths,
    by = "facility_id"
  )

# Automatic rank is intentionally conservative. Only the reviewed override
# table may assign statewide_major or regional_major. This prevents every long
# local canal or every CVP/SWP component from visually reading as a statewide
# backbone merely because of project membership or mapped length.
segments$display_rank <- dplyr::case_when(
  segments$facility_length_mi >= 10 |
    segments$facility_type %in% c(
      "Aqueduct",
      "Tunnel",
      "Intertie"
    ) ~ "medium",
  TRUE ~ "local_supporting"
)

segments$display_rank_source <- "automatic"
segments$display_rank_reason <- dplyr::case_when(
  segments$display_rank == "medium" ~
    "Automatic medium rank from mapped length or facility type.",
  TRUE ~ "Automatic standard/local rank."
)

rank_match <- match(
  segments$facility_id,
  display_rank_overrides$facility_id
)

rank_override_hit <- !is.na(rank_match)

segments$display_rank[rank_override_hit] <-
  display_rank_overrides$display_rank_override[
    rank_match[rank_override_hit]
  ]

segments$display_rank_reason[rank_override_hit] <-
  display_rank_overrides$display_rank_reason[
    rank_match[rank_override_hit]
  ]

segments$display_rank_source[rank_override_hit] <-
  "reviewed_override"

# Keep all mapped pieces of the California Aqueduct East and West branches at
# one reviewed public display rank. The source datasets divide these branches
# among several canonical facilities, which otherwise produces alternating
# thick/thin sections along the same operational branch.
california_aqueduct_branch_text <- paste(
  dplyr::coalesce(segments$canonical_name, ""),
  dplyr::coalesce(segments$aliases, ""),
  dplyr::coalesce(segments$parent_system, ""),
  dplyr::coalesce(segments$project_division, ""),
  dplyr::coalesce(segments$project_unit, ""),
  dplyr::coalesce(segments$project_path, "")
)

california_aqueduct_branch_rule <-
  grepl("SWP", dplyr::coalesce(segments$project_family, ""), ignore.case = TRUE) &
  grepl("California Aqueduct", california_aqueduct_branch_text, ignore.case = TRUE) &
  grepl("East Branch|West Branch", california_aqueduct_branch_text, ignore.case = TRUE)

segments$display_rank[california_aqueduct_branch_rule] <- "statewide_major"
segments$display_rank_source[california_aqueduct_branch_rule] <-
  "reviewed_system_rule"
segments$display_rank_reason[california_aqueduct_branch_rule] <-
  paste(
    "California Aqueduct East/West Branch system rule:",
    "keep all mapped branch pieces at one statewide-major width."
  )

missing_rank_override_ids <- setdiff(
  display_rank_overrides$facility_id,
  unique(segments$facility_id)
)

if (length(missing_rank_override_ids) > 0L) {
  warning(
    "Reviewed display-rank override IDs not present in this build: ",
    paste(missing_rank_override_ids, collapse = ", ")
  )
}

rank_name_check <- segments |>
  sf::st_drop_geometry() |>
  dplyr::filter(rank_override_hit) |>
  dplyr::distinct(facility_id, canonical_name) |>
  dplyr::left_join(
    display_rank_overrides |>
      dplyr::select(
        facility_id,
        expected_canonical_name = canonical_name
      ),
    by = "facility_id"
  ) |>
  dplyr::filter(
    canonical_name != expected_canonical_name
  )

if (nrow(rank_name_check) > 0L) {
  stop(
    "Reviewed display-rank facility ID/name mismatch detected for: ",
    paste(rank_name_check$facility_id, collapse = ", ")
  )
}

message(
  "Reviewed display ranks applied: ",
  sum(rank_override_hit),
  " segment rows across ",
  dplyr::n_distinct(segments$facility_id[rank_override_hit]),
  " facilities."
)

segments$status <- "unknown"
segments$lbl <- mapply(
  make_lbl,
  segments$canonical_name,
  segments$aliases,
  USE.NAMES = FALSE
)

segments$lbl_full <- segments$canonical_name

segments$owner_agency <- dplyr::coalesce(
  clean_text(segments$source_owner),
  ""
)

segments$operator_agency <- dplyr::coalesce(
  clean_text(segments$source_operator),
  ""
)

segments$blm_crosses <- NA
segments$blm_length_mi <- NA_real_
segments$blm_pct_length <- NA_real_
segments$blm_nearest_mi <- NA_real_
segments$blm_crossing_count <- NA_integer_
segments$blm_field_offices <- NA_character_

segments <- segments |>
  dplyr::arrange(
    facility_id,
    canonical_name,
    geometry_source,
    source_ids
  ) |>
  dplyr::group_by(facility_id) |>
  dplyr::mutate(
    segment_sequence = dplyr::row_number(),
    segment_id = paste0(
      facility_id,
      "_S",
      sprintf("%03d", segment_sequence)
    )
  ) |>
  dplyr::ungroup()

segments$search_text <- tolower(paste(
  segments$canonical_name,
  segments$lbl,
  segments$aliases,
  segments$parent_system,
  segments$project_family,
  segments$project_name,
  segments$project_division,
  segments$project_unit,
  segments$project_subunit,
  segments$project_path,
  segments$owner_agency,
  segments$operator_agency,
  segments$facility_type
))

# ---- Facility table ----------------------------------------------------------

rank_order <- c(
  "local_supporting" = 1,
  "medium" = 2,
  "regional_major" = 3,
  "statewide_major" = 4
)

facilities <- segments |>
  sf::st_drop_geometry() |>
  dplyr::group_by(facility_id) |>
  dplyr::summarise(
    canonical_name = dplyr::first(canonical_name),
    lbl = dplyr::first(lbl),
    lbl_full = dplyr::first(lbl_full),
    aliases = collapse_unique(aliases),
    parent_system = collapse_unique(parent_system),
    project_family = collapse_unique(project_family),
    project_name = collapse_unique(project_name),
    project_division = collapse_unique(project_division),
    project_unit = collapse_unique(project_unit),
    project_subunit = collapse_unique(project_subunit),
    project_path = collapse_unique(project_path),
    ownership_class = collapse_unique(ownership_class),
    owner_agency = collapse_unique(owner_agency),
    operator_agency = collapse_unique(operator_agency),
    facility_group = collapse_unique(facility_group),
    facility_type = collapse_unique(facility_type),
    network_role = collapse_unique(network_role),
    display_rank = dplyr::first(display_rank),
    display_rank_source = dplyr::first(display_rank_source),
    display_rank_reason = dplyr::first(display_rank_reason),
    status = collapse_unique(status),
    length_mi = sum(length_mi, na.rm = TRUE),
    segment_count = dplyr::n(),
    geometry_confidence = ifelse(
      any(geometry_confidence == "low"),
      "low",
      ifelse(
        any(geometry_confidence == "medium"),
        "medium",
        "high"
      )
    ),
    decision_ids = collapse_unique(decision_id),
    facility_crosses_blm = NA,
    facility_length_on_blm_mi = NA_real_,
    facility_min_blm_distance_mi = NA_real_,
    facility_blm_field_offices = NA_character_,
    .groups = "drop"
  )

# ---- Alias table -------------------------------------------------------------

alias_rows <- list()

for (i in seq_len(nrow(facilities))) {
  f <- facilities[i, ]
  aliases <- split_values(f$aliases)

  if (length(aliases) == 0L) {
    next
  }

  for (alias in aliases) {
    alias_type <- dplyr::case_when(
      grepl(
        "parent|system|aqueduct",
        alias,
        ignore.case = TRUE
      ) ~ "parent_or_system_name",

      grepl(
        "\\b[A-Z]{2,6}\\b",
        alias
      ) ~ "abbreviation_or_source_name",

      grepl(
        "unresolved|source",
        alias,
        ignore.case = TRUE
      ) ~ "source_or_qa_name",

      TRUE ~ "alternate_name"
    )

    alias_rows[[length(alias_rows) + 1L]] <- data.frame(
      facility_id = f$facility_id,
      alias = alias,
      alias_type = alias_type,
      stringsAsFactors = FALSE
    )
  }
}

facility_aliases <- if (length(alias_rows) > 0L) {
  dplyr::bind_rows(alias_rows) |>
    dplyr::distinct()
} else {
  data.frame(
    facility_id = character(),
    alias = character(),
    alias_type = character()
  )
}

