# ---- Source preparation -----------------------------------------------------

prepare_major <- function(x) {
  x$source_name <- "Major Conveyance"
  x$source_row <- seq_len(nrow(x))
  x$source_id <- sprintf("MAJOR_%04d", x$source_row)

  x$source_feature_name <- first_nonmissing(
    x,
    c("Pname", "CANAL", "RNAME", "Sub_Name")
  )

  x$source_owner <- first_nonmissing(
    x,
    c("Owner")
  )

  x$source_operator <- first_nonmissing(
    x,
    c("Operator")
  )

  x$source_type <- first_nonmissing(
    x,
    c("Type", "Sub_Name")
  )

  x$name_norm <- normalize_name(x$source_feature_name)
  x
}

prepare_delta <- function(x) {
  x$source_name <- "DeltaMAPP"
  x$source_row <- seq_len(nrow(x))
  x$source_id <- sprintf("DELTA_%04d", x$source_row)

  x$source_feature_name <- first_nonmissing(
    x,
    c("Name", "Conv_Sub")
  )

  x$source_owner <- rep(NA_character_, nrow(x))

  x$source_operator <- first_nonmissing(
    x,
    c("Operator")
  )

  x$source_type <- first_nonmissing(
    x,
    c("Conv_Type", "Conv_Sub")
  )

  x$name_norm <- normalize_name(x$source_feature_name)
  x
}

message("Reading and preparing source shapefiles...")

major <- prepare_major(
  sf::st_read(
    MAJOR_SHP,
    quiet = TRUE,
    stringsAsFactors = FALSE
  )
)

delta <- prepare_delta(
  sf::st_read(
    DELTA_SHP,
    quiet = TRUE,
    stringsAsFactors = FALSE
  )
)

major <- suppressWarnings(sf::st_transform(major, 4326))
delta <- suppressWarnings(sf::st_transform(delta, 4326))

major_m <- suppressWarnings(sf::st_transform(major, 3310))
delta_m <- suppressWarnings(sf::st_transform(delta, 3310))

major_m$source_length_m <- as.numeric(sf::st_length(major_m))
delta_m$source_length_m <- as.numeric(sf::st_length(delta_m))

decisions <- readr::read_csv(
  DECISION_CSV,
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character()
  )
)

decisions$include_curated <- toupper(decisions$include_curated) == "TRUE"


project_crosswalk <- readr::read_csv(
  PROJECT_CROSSWALK_CSV,
  show_col_types = FALSE,
  col_types = readr::cols(
    priority = readr::col_double(),
    .default = readr::col_character()
  )
) |>
  dplyr::arrange(priority, rule_id)

expected_cvp <- readr::read_csv(
  EXPECTED_CVP_CSV,
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character()
  )
)

supplement_registry <- readr::read_csv(
  SUPPLEMENT_REGISTRY_CSV,
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character()
  )
)


identity_overrides <- readr::read_csv(
  IDENTITY_OVERRIDES_CSV,
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character()
  )
)

label_overrides <- readr::read_csv(
  LABEL_OVERRIDES_CSV,
  show_col_types = FALSE,
  col_types = readr::cols(
    lbl_min_zoom = readr::col_double(),
    lbl_max_zoom = readr::col_double(),
    .default = readr::col_character()
  )
)

display_rank_overrides <- readr::read_csv(
  DISPLAY_RANK_OVERRIDES_CSV,
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character()
  )
)

if (anyDuplicated(display_rank_overrides$facility_id)) {
  stop("07_display_rank_overrides.csv contains duplicate facility_id values.")
}

valid_display_ranks <- c(
  "statewide_major",
  "regional_major",
  "medium",
  "local_supporting"
)

invalid_rank_rows <- display_rank_overrides |>
  dplyr::filter(!display_rank_override %in% valid_display_ranks)

if (nrow(invalid_rank_rows) > 0L) {
  stop(
    "07_display_rank_overrides.csv contains invalid display ranks: ",
    paste(unique(invalid_rank_rows$display_rank_override), collapse = ", ")
  )
}

# ---- Segment constructors ---------------------------------------------------

empty_segment_sf <- function() {
  sf::st_sf(
    canonical_name = character(),
    aliases = character(),
    parent_system = character(),
    project_family_seed = character(),
    project_name_seed = character(),
    project_unit_seed = character(),
    source_owner = character(),
    source_operator = character(),
    source_type = character(),
    geometry_source = character(),
    geometry_decision = character(),
    geometry_confidence = character(),
    decision_id = character(),
    source_major_rows = character(),
    source_delta_rows = character(),
    source_ids = character(),
    geometry = sf::st_sfc(crs = 3310)
  )
}

standardize_lines <- function(x) {
  # Work one source row at a time. Casting an sf object directly can expand a
  # MULTILINESTRING into several line parts without safely recycling every
  # attribute column on some sf/GDAL combinations. This explicit row-wise
  # method preserves and duplicates the source attributes for each line part.
  if (!inherits(x, "sf")) {
    stop("standardize_lines() requires an sf object.")
  }

  if (nrow(x) == 0L) {
    return(x)
  }

  input_crs <- sf::st_crs(x)
  output <- list()

  for (row_index in seq_len(nrow(x))) {
    one <- x[row_index, , drop = FALSE]
    one_geometry <- suppressWarnings(
      sf::st_make_valid(sf::st_geometry(one))
    )

    if (
      length(one_geometry) == 0L ||
      all(sf::st_is_empty(one_geometry))
    ) {
      next
    }

    geometry_type <- as.character(
      sf::st_geometry_type(one_geometry, by_geometry = TRUE)
    )

    line_parts <- tryCatch(
      {
        if (all(geometry_type == "LINESTRING")) {
          one_geometry
        } else if (all(geometry_type == "MULTILINESTRING")) {
          suppressWarnings(
            sf::st_cast(one_geometry, "LINESTRING", warn = FALSE)
          )
        } else {
          extracted <- suppressWarnings(
            sf::st_collection_extract(
              one_geometry,
              "LINESTRING",
              warn = FALSE
            )
          )

          if (length(extracted) == 0L) {
            sf::st_sfc(crs = input_crs)
          } else {
            suppressWarnings(
              sf::st_cast(extracted, "LINESTRING", warn = FALSE)
            )
          }
        }
      },
      error = function(e) {
        warning(
          "Could not extract line geometry from source row ",
          row_index,
          ": ",
          conditionMessage(e)
        )
        sf::st_sfc(crs = input_crs)
      }
    )

    if (length(line_parts) == 0L) {
      next
    }

    keep <- !sf::st_is_empty(line_parts)
    line_parts <- line_parts[keep]

    if (length(line_parts) == 0L) {
      next
    }

    attributes <- sf::st_drop_geometry(one)
    attributes <- attributes[
      rep(1L, length(line_parts)),
      ,
      drop = FALSE
    ]
    rownames(attributes) <- NULL

    output[[length(output) + 1L]] <- sf::st_sf(
      attributes,
      geometry = line_parts,
      crs = input_crs
    )
  }

  if (length(output) == 0L) {
    return(x[0, , drop = FALSE])
  }

  result <- do.call(rbind, output)
  rownames(result) <- NULL
  result
}

scalar_decision_value <- function(
  decision,
  field,
  default = ""
) {
  if (!field %in% names(decision)) {
    return(default)
  }

  value <- decision[[field]]

  if (length(value) == 0L || is.na(value[[1]])) {
    return(default)
  }

  as.character(value[[1]])
}

safe_source_column <- function(
  source_sf,
  field,
  default = NA_character_
) {
  n <- nrow(source_sf)

  if (!field %in% names(source_sf)) {
    return(rep(default, n))
  }

  value <- source_sf[[field]]

  if (length(value) == n) {
    return(value)
  }

  if (length(value) == 1L) {
    return(rep(value, n))
  }

  warning(
    "Column '", field, "' had length ", length(value),
    " for ", n, " geometry rows; using fallback values."
  )
  rep(default, n)
}

make_segments_from_source <- function(
  source_sf,
  decision,
  canonical_name = decision$canonical_name,
  geometry_source = NULL,
  geometry_decision = decision$geometry_rule,
  name_from_source = FALSE
) {
  if (nrow(source_sf) == 0L) {
    return(empty_segment_sf())
  }

  source_sf <- standardize_lines(source_sf)

  if (nrow(source_sf) == 0L) {
    return(empty_segment_sf())
  }

  n <- nrow(source_sf)

  if (length(sf::st_geometry(source_sf)) != n) {
    stop(
      "Geometry/attribute row mismatch after line standardization: ",
      n,
      " attribute rows versus ",
      length(sf::st_geometry(source_sf)),
      " geometries."
    )
  }

  if (isTRUE(name_from_source)) {
    names_out <- smart_title(
      safe_source_column(
        source_sf,
        "source_feature_name",
        default = NA_character_
      )
    )
  } else {
    names_out <- rep(as.character(canonical_name[[1]]), n)
  }

  if (
    is.null(geometry_source) ||
      length(geometry_source) == 0L ||
      is.na(geometry_source[[1]]) ||
      geometry_source[[1]] == ""
  ) {
    source_names <- safe_source_column(
      source_sf,
      "source_name",
      default = "Unknown source"
    )
    geometry_source <- source_names[[1]]
  } else {
    geometry_source <- as.character(geometry_source[[1]])
  }

  attributes <- data.frame(
    canonical_name = names_out,
    aliases = rep(
      scalar_decision_value(decision, "aliases_to_retain", ""),
      n
    ),
    parent_system = rep(
      scalar_decision_value(decision, "parent_system", ""),
      n
    ),
    project_family_seed = rep(
      scalar_decision_value(decision, "project_family_seed", ""),
      n
    ),
    project_name_seed = rep(
      scalar_decision_value(decision, "project_name_seed", ""),
      n
    ),
    project_unit_seed = rep(
      scalar_decision_value(decision, "project_unit_seed", ""),
      n
    ),
    source_owner = safe_source_column(
      source_sf,
      "source_owner",
      default = NA_character_
    ),
    source_operator = safe_source_column(
      source_sf,
      "source_operator",
      default = NA_character_
    ),
    source_type = safe_source_column(
      source_sf,
      "source_type",
      default = NA_character_
    ),
    geometry_source = rep(geometry_source, n),
    geometry_decision = rep(as.character(geometry_decision[[1]]), n),
    geometry_confidence = rep(
      scalar_decision_value(decision, "confidence", "medium"),
      n
    ),
    decision_id = rep(
      scalar_decision_value(decision, "decision_id", "UNSPECIFIED"),
      n
    ),
    source_major_rows = rep(
      scalar_decision_value(decision, "major_source_rows", ""),
      n
    ),
    source_delta_rows = rep(
      scalar_decision_value(decision, "delta_source_rows", ""),
      n
    ),
    source_ids = safe_source_column(
      source_sf,
      "source_id",
      default = ""
    ),
    stringsAsFactors = FALSE
  )

  sf::st_sf(
    attributes,
    geometry = sf::st_geometry(source_sf),
    crs = sf::st_crs(source_sf)
  )
}

difference_lines_by_mask <- function(
  source_sf,
  mask_geometry,
  operation_label = "difference"
) {
  if (nrow(source_sf) == 0L) {
    return(source_sf)
  }

  output <- list()
  input_crs <- sf::st_crs(source_sf)

  for (row_index in seq_len(nrow(source_sf))) {
    one <- source_sf[row_index, , drop = FALSE]

    difference_geometry <- tryCatch(
      suppressWarnings(
        sf::st_difference(
          sf::st_geometry(one),
          mask_geometry
        )
      ),
      error = function(e) {
        warning(
          operation_label,
          " failed for source row ",
          safe_source_column(one, "source_row", row_index)[[1]],
          ": ",
          conditionMessage(e)
        )
        sf::st_sfc(crs = input_crs)
      }
    )

    if (
      length(difference_geometry) == 0L ||
      all(sf::st_is_empty(difference_geometry))
    ) {
      next
    }

    attributes <- sf::st_drop_geometry(one)
    attributes <- attributes[
      rep(1L, length(difference_geometry)),
      ,
      drop = FALSE
    ]
    rownames(attributes) <- NULL

    rebuilt <- sf::st_sf(
      attributes,
      geometry = difference_geometry,
      crs = input_crs
    )

    rebuilt <- standardize_lines(rebuilt)

    if (nrow(rebuilt) > 0L) {
      output[[length(output) + 1L]] <- rebuilt
    }
  }

  if (length(output) == 0L) {
    return(source_sf[0, , drop = FALSE])
  }

  result <- do.call(rbind, output)
  rownames(result) <- NULL
  result
}

extract_extensions <- function(
  secondary_sf,
  primary_sf,
  trim_buffer_m = EXTENSION_TRIM_BUFFER_M,
  minimum_length_m = MIN_EXTENSION_M
) {
  if (nrow(secondary_sf) == 0L || nrow(primary_sf) == 0L) {
    return(secondary_sf[0, , drop = FALSE])
  }

  primary_union <- sf::st_union(sf::st_geometry(primary_sf))
  primary_buffer <- sf::st_buffer(primary_union, trim_buffer_m)

  result <- difference_lines_by_mask(
    secondary_sf,
    primary_buffer,
    operation_label = "Extension trimming"
  )

  if (nrow(result) == 0L) {
    return(result)
  }

  lengths <- as.numeric(sf::st_length(result))
  result[lengths >= minimum_length_m, , drop = FALSE]
}

clip_primary_away_from_component <- function(
  primary_sf,
  component_sf,
  buffer_m = COMPONENT_SPLIT_BUFFER_M
) {
  if (nrow(primary_sf) == 0L || nrow(component_sf) == 0L) {
    return(primary_sf)
  }

  component_buffer <- sf::st_buffer(
    sf::st_union(sf::st_geometry(component_sf)),
    buffer_m
  )

  difference_lines_by_mask(
    primary_sf,
    component_buffer,
    operation_label = "Component splitting"
  )
}

segments_list <- list()
consumed_major_rows <- integer()
consumed_delta_rows <- integer()
omitted_decision_ids <- character()

append_segments <- function(x) {
  if (inherits(x, "sf") && nrow(x) > 0L) {
    segments_list[[length(segments_list) + 1L]] <<- x
  }
}

for (i in seq_len(nrow(decisions))) {
  decision <- decisions[i, ]

  message(
    "Building reviewed decision ",
    i,
    " of ",
    nrow(decisions),
    " [",
    scalar_decision_value(decision, "decision_id", as.character(i)),
    "]: ",
    scalar_decision_value(decision, "canonical_name", "(unnamed)")
  )

  major_rows <- parse_rows(decision$major_source_rows)
  delta_rows <- parse_rows(decision$delta_source_rows)

  consumed_major_rows <- unique(c(consumed_major_rows, major_rows))
  consumed_delta_rows <- unique(c(consumed_delta_rows, delta_rows))

  selected_major <- major_m[
    major_m$source_row %in% major_rows,
  ]

  selected_delta <- delta_m[
    delta_m$source_row %in% delta_rows,
  ]

  if (!isTRUE(decision$include_curated) || decision$geometry_rule == "omit") {
    omitted_decision_ids <- c(
      omitted_decision_ids,
      decision$decision_id
    )
    next
  }

  rule <- decision$geometry_rule
  component_mode <- dplyr::coalesce(decision$component_mode, "")

  if (component_mode == "case16_tunnel_penstocks") {
    tunnel <- clip_primary_away_from_component(
      selected_major,
      selected_delta
    )

    decision_tunnel <- decision
    decision_tunnel$canonical_name <- "Clear Creek Tunnel"

    decision_penstocks <- decision
    decision_penstocks$canonical_name <- "Judge Francis Carr Powerplant Penstocks"

    append_segments(
      make_segments_from_source(
        tunnel,
        decision_tunnel,
        geometry_source = "Major Conveyance",
        geometry_decision = "split_components_major_tunnel"
      )
    )

    append_segments(
      make_segments_from_source(
        selected_delta,
        decision_penstocks,
        geometry_source = "DeltaMAPP",
        geometry_decision = "split_components_delta_penstocks"
      )
    )

    next
  }

  if (component_mode == "case17_penstocks_tailrace") {
    penstocks <- selected_delta[
      selected_delta$source_row == 285,
    ]

    tailrace <- selected_delta[
      selected_delta$source_row == 282,
    ]

    decision_penstocks <- decision
    decision_penstocks$canonical_name <- "Spring Creek Penstocks"

    decision_tailrace <- decision
    decision_tailrace$canonical_name <- "Spring Creek Powerplant Tailrace"

    append_segments(
      make_segments_from_source(
        penstocks,
        decision_penstocks,
        geometry_source = "DeltaMAPP",
        geometry_decision = "split_components_penstocks"
      )
    )

    append_segments(
      make_segments_from_source(
        tailrace,
        decision_tailrace,
        geometry_source = "DeltaMAPP",
        geometry_decision = "split_components_tailrace"
      )
    )

    next
  }

  if (component_mode == "case20_separate") {
    decision_drain <- decision
    decision_drain$canonical_name <- "San Luis Drain"
    decision_drain$aliases_to_retain <- ""

    decision_canal <- decision
    decision_canal$canonical_name <- "San Luis Canal"
    decision_canal$aliases_to_retain <- ""

    append_segments(
      make_segments_from_source(
        selected_major,
        decision_drain,
        geometry_source = "Major Conveyance",
        geometry_decision = "separate_facility_drain"
      )
    )

    append_segments(
      make_segments_from_source(
        selected_delta,
        decision_canal,
        geometry_source = "DeltaMAPP",
        geometry_decision = "separate_facility_canal"
      )
    )

    next
  }

  if (component_mode == "case30_pipeline_split") {
    decision_old <- decision
    decision_old$canonical_name <- "Old River Pipeline"
    decision_old$aliases_to_retain <- ""

    decision_intake <- decision
    decision_intake$canonical_name <- "Los Vaqueros Intake Pipeline"
    decision_intake$aliases_to_retain <- ""

    extension <- extract_extensions(
      selected_delta,
      selected_major
    )

    append_segments(
      make_segments_from_source(
        selected_major,
        decision_old,
        geometry_source = "Major Conveyance",
        geometry_decision = "pipeline_split_overlap_old_river"
      )
    )

    append_segments(
      make_segments_from_source(
        extension,
        decision_intake,
        geometry_source = "Derived from DeltaMAPP",
        geometry_decision = "pipeline_split_delta_extension"
      )
    )

    next
  }

  if (component_mode == "delta_component_names") {
    append_segments(
      make_segments_from_source(
        selected_delta,
        decision,
        geometry_source = "DeltaMAPP",
        geometry_decision = "retain_named_components",
        name_from_source = TRUE
      )
    )

    next
  }

  if (rule == "major") {
    append_segments(
      make_segments_from_source(
        selected_major,
        decision,
        geometry_source = "Major Conveyance"
      )
    )
    next
  }

  if (rule == "delta") {
    append_segments(
      make_segments_from_source(
        selected_delta,
        decision,
        geometry_source = "DeltaMAPP"
      )
    )
    next
  }

  if (rule == "major_plus_delta_extension") {
    extension <- extract_extensions(
      selected_delta,
      selected_major
    )

    append_segments(
      make_segments_from_source(
        selected_major,
        decision,
        geometry_source = "Major Conveyance",
        geometry_decision = "major_spine"
      )
    )

    append_segments(
      make_segments_from_source(
        extension,
        decision,
        geometry_source = "Derived from DeltaMAPP",
        geometry_decision = "delta_nonduplicate_extension"
      )
    )

    next
  }

  if (rule == "delta_plus_major_extension") {
    extension <- extract_extensions(
      selected_major,
      selected_delta
    )

    append_segments(
      make_segments_from_source(
        selected_delta,
        decision,
        geometry_source = "DeltaMAPP",
        geometry_decision = "delta_spine"
      )
    )

    append_segments(
      make_segments_from_source(
        extension,
        decision,
        geometry_source = "Derived from Major Conveyance",
        geometry_decision = "major_nonduplicate_extension"
      )
    )

    next
  }

  append_segments(
    make_segments_from_source(
      selected_major,
      decision,
      geometry_source = "Major Conveyance",
      geometry_decision = "retain_both_sources"
    )
  )

  append_segments(
    make_segments_from_source(
      selected_delta,
      decision,
      geometry_source = "DeltaMAPP",
      geometry_decision = "retain_both_sources"
    )
  )
}

message("Reviewed-decision geometry construction complete.")

# ---- Automatic handling of exact-name overlaps ------------------------------

remaining_major <- major_m[
  !major_m$source_row %in% consumed_major_rows,
]

remaining_delta <- delta_m[
  !delta_m$source_row %in% consumed_delta_rows,
]

find_exact_overlap_pairs <- function(major_sf, delta_sf) {
  common_names <- intersect(
    unique(major_sf$name_norm[major_sf$name_norm != ""]),
    unique(delta_sf$name_norm[delta_sf$name_norm != ""])
  )

  output <- list()

  for (name_value in common_names) {
    major_part <- major_sf[
      major_sf$name_norm == name_value,
    ]

    delta_part <- delta_sf[
      delta_sf$name_norm == name_value,
    ]

    if (nrow(major_part) == 0L || nrow(delta_part) == 0L) {
      next
    }

    candidates <- sf::st_intersects(
      sf::st_buffer(major_part, NEAR_BUFFER_M),
      delta_part
    )

    for (mi in seq_along(candidates)) {
      if (length(candidates[[mi]]) == 0L) {
        next
      }

      for (di in candidates[[mi]]) {
        major_geom <- sf::st_geometry(major_part[mi, ])
        delta_geom <- sf::st_geometry(delta_part[di, ])

        major_length <- as.numeric(sf::st_length(major_geom))
        delta_length <- as.numeric(sf::st_length(delta_geom))

        major_near <- suppressWarnings(
          sf::st_intersection(
            major_geom,
            sf::st_buffer(delta_geom, NEAR_BUFFER_M)
          )
        )

        delta_near <- suppressWarnings(
          sf::st_intersection(
            delta_geom,
            sf::st_buffer(major_geom, NEAR_BUFFER_M)
          )
        )

        major_pct <- if (major_length > 0) {
          100 * sum(as.numeric(sf::st_length(major_near))) / major_length
        } else {
          0
        }

        delta_pct <- if (delta_length > 0) {
          100 * sum(as.numeric(sf::st_length(delta_near))) / delta_length
        } else {
          0
        }

        if (
          max(major_pct, delta_pct) >= 60 &&
          min(major_pct, delta_pct) >= 20
        ) {
          output[[length(output) + 1L]] <- data.frame(
            name_norm = name_value,
            major_source_row = major_part$source_row[mi],
            delta_source_row = delta_part$source_row[di],
            major_near_delta_pct = major_pct,
            delta_near_major_pct = delta_pct,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  if (length(output) == 0L) {
    return(data.frame(
      name_norm = character(),
      major_source_row = integer(),
      delta_source_row = integer(),
      major_near_delta_pct = numeric(),
      delta_near_major_pct = numeric()
    ))
  }

  dplyr::bind_rows(output)
}

message("Finding exact-name spatial duplicates among unreviewed rows...")
exact_pairs <- find_exact_overlap_pairs(
  remaining_major,
  remaining_delta
)

suppress_major_exact <- integer()
suppress_delta_exact <- integer()

if (nrow(exact_pairs) > 0L) {
  exact_name_choices <- exact_pairs |>
    dplyr::group_by(name_norm) |>
    dplyr::summarise(
      major_rows = list(unique(major_source_row)),
      delta_rows = list(unique(delta_source_row)),
      .groups = "drop"
    )

  for (i in seq_len(nrow(exact_name_choices))) {
    item <- exact_name_choices[i, ]

    major_rows <- item$major_rows[[1]]
    delta_rows <- item$delta_rows[[1]]

    major_total <- sum(
      remaining_major$source_length_m[
        remaining_major$source_row %in% major_rows
      ],
      na.rm = TRUE
    )

    delta_total <- sum(
      remaining_delta$source_length_m[
        remaining_delta$source_row %in% delta_rows
      ],
      na.rm = TRUE
    )

    if (major_total > delta_total * 1.05) {
      suppress_delta_exact <- c(
        suppress_delta_exact,
        delta_rows
      )
    } else {
      suppress_major_exact <- c(
        suppress_major_exact,
        major_rows
      )
    }
  }
}

remaining_major_keep <- remaining_major[
  !remaining_major$source_row %in% suppress_major_exact,
]

remaining_delta_keep <- remaining_delta[
  !remaining_delta$source_row %in% suppress_delta_exact,
]

# ---- Include the rest as provisional source-derived segments ----------------

make_provisional_segments <- function(source_sf) {
  if (nrow(source_sf) == 0L) {
    return(empty_segment_sf())
  }

  source_sf <- standardize_lines(source_sf)

  if (nrow(source_sf) == 0L) {
    return(empty_segment_sf())
  }

  length_mi <- as.numeric(sf::st_length(source_sf)) / 1609.344
  usable_name <- clean_text(source_sf$source_feature_name)

  include <- !is.na(usable_name) |
    length_mi >= MIN_UNNAMED_LENGTH_MI

  source_sf <- source_sf[include, ]
  usable_name <- usable_name[include]

  if (nrow(source_sf) == 0L) {
    return(empty_segment_sf())
  }

  generated_names <- ifelse(
    is.na(usable_name),
    paste0(
      "Unnamed conveyance — ",
      source_sf$source_name,
      " row ",
      source_sf$source_row
    ),
    usable_name
  )

  source_major_rows <- ifelse(
    source_sf$source_name == "Major Conveyance",
    as.character(source_sf$source_row),
    ""
  )

  source_delta_rows <- ifelse(
    source_sf$source_name == "DeltaMAPP",
    as.character(source_sf$source_row),
    ""
  )

  confidence <- ifelse(
    is.na(usable_name),
    "low",
    "medium"
  )

  sf::st_sf(
    canonical_name = generated_names,
    aliases = rep("", nrow(source_sf)),
    parent_system = rep("", nrow(source_sf)),
    project_family_seed = rep("", nrow(source_sf)),
    project_name_seed = rep("", nrow(source_sf)),
    project_unit_seed = rep("", nrow(source_sf)),
    source_owner = source_sf$source_owner,
    source_operator = source_sf$source_operator,
    source_type = source_sf$source_type,
    geometry_source = source_sf$source_name,
    geometry_decision = rep("provisional_source_inclusion", nrow(source_sf)),
    geometry_confidence = confidence,
    decision_id = rep("UNREVIEWED", nrow(source_sf)),
    source_major_rows = source_major_rows,
    source_delta_rows = source_delta_rows,
    source_ids = source_sf$source_id,
    geometry = sf::st_geometry(source_sf),
    crs = 3310
  )
}

append_segments(
  make_provisional_segments(remaining_major_keep)
)

append_segments(
  make_provisional_segments(remaining_delta_keep)
)

if (length(segments_list) == 0L) {
  stop("No canonical segments were produced.")
}

message("Combining reviewed and provisional segment geometry...")
segments <- dplyr::bind_rows(segments_list)
segments <- standardize_lines(segments)
message("Canonical segment parts produced: ", nrow(segments))
message("Classifying canonical segments and forming logical facilities...")

