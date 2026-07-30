# ==== swrcb_face_value_helpers.r =============================================
##
## PURPOSE:
##   Resolve face values from the immutable SWRCB-provided 2026 BLM
##   water-right export, validate any separately reviewed conflict decisions,
##   and reconcile the authoritative result with public CalWATRS values.
##
## POLICY:
##   - The 2026 export is authoritative for IDs present in that export.
##   - Public values are fallback only for IDs absent from the export, plus the
##     separately reported present-but-blank policy retained from v0.38.
##   - A duplicate {0, one positive value < 1 AFY} conflict is the only
##     automatic conflict resolution.
##   - Every other authoritative conflict must have a validated reviewed
##     override or stop the core-cache build.

pt_norm_swrcb_wr_id <- function(x) {
  x <- trimws(toupper(as.character(x)))
  x <- gsub("\\s+", "", x)
  x[x == "" | x %in% c("NA", "N/A")] <- NA_character_
  x
}

pt_clean_swrcb_face_value_text <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | toupper(x) %in% c("NA", "N/A")] <- NA_character_
  x
}

## Backward-compatible alias for the cache block's public-unit cleanup.
pt_clean_swrcb_value_text <- pt_clean_swrcb_face_value_text

pt_collapse_unique_swrcb_text <- function(x) {
  values <- sort(
    unique(stats::na.omit(pt_clean_swrcb_face_value_text(x))),
    method = "radix"
  )
  if (!length(values)) NA_character_ else paste(values, collapse = " | ")
}

## Backward-compatible alias used by older QA code.
pt_collapse_unique_text <- pt_collapse_unique_swrcb_text

pt_parse_swrcb_face_values <- function(x, ids = NULL, label = "Face value") {
  raw <- pt_clean_swrcb_face_value_text(x)
  normalized <- gsub(",", "", raw, fixed = TRUE)
  plain_pattern <- paste0(
    "^[+-]?(?:",
    "(?:[0-9]+(?:\\.[0-9]*)?)|",
    "(?:\\.[0-9]+)",
    ")(?:[eE][+-]?[0-9]+)?$"
  )
  grouped_pattern <- paste0(
    "^[+-]?",
    "[0-9]{1,3}(?:,[0-9]{3})+",
    "(?:\\.[0-9]*)?",
    "(?:[eE][+-]?[0-9]+)?$"
  )
  valid <- is.na(raw) |
    grepl(plain_pattern, raw, perl = TRUE) |
    grepl(grouped_pattern, raw, perl = TRUE)
  parsed <- suppressWarnings(as.numeric(normalized))
  valid <- valid & (is.na(normalized) | is.finite(parsed))

  if (any(!valid)) {
    bad <- which(!valid)
    id_text <- if (is.null(ids)) {
      paste0("row ", bad)
    } else {
      id_value <- pt_norm_swrcb_wr_id(ids)[bad]
      ifelse(
        is.na(id_value),
        paste0("row ", bad),
        paste0(id_value, " (row ", bad, ")")
      )
    }
    stop(
      "Could not strictly parse ", length(bad), " nonblank SWRCB ",
      label, " value(s): ",
      paste(utils::head(paste0(id_text, " = '", raw[bad], "'"), 10L),
            collapse = "; "),
      call. = FALSE
    )
  }

  negative <- !is.na(parsed) & parsed < 0
  if (any(negative)) {
    bad <- which(negative)
    id_text <- if (is.null(ids)) {
      paste0("row ", bad)
    } else {
      dplyr::coalesce(pt_norm_swrcb_wr_id(ids)[bad], paste0("row ", bad))
    }
    stop(
      "SWRCB ", label, " values must be nonnegative. Affected: ",
      paste(utils::head(paste0(id_text, " = ", raw[bad]), 10L),
            collapse = "; "),
      call. = FALSE
    )
  }

  parsed
}

pt_format_swrcb_numeric_values <- function(x) {
  values <- sort(unique(stats::na.omit(as.numeric(x))))
  if (!length(values)) return(NA_character_)
  formatted <- vapply(values, function(value) {
    text <- format(
      value,
      scientific = FALSE,
      trim = TRUE,
      digits = 15
    )
    if (grepl(".", text, fixed = TRUE)) {
      text <- sub("0+$", "", text)
      text <- sub("[.]$", "", text)
    }
    if (identical(text, "-0")) "0" else text
  }, character(1))
  paste(formatted, collapse = " | ")
}

pt_swrcb_override_required_columns <- function() {
  c(
    "swrcb_wr_id_norm",
    "resolved_face_afy",
    "resolution_type",
    "value_basis",
    "resolution_note",
    "source_reference",
    "reviewed_date"
  )
}

pt_empty_swrcb_face_value_overrides <- function() {
  data.frame(
    swrcb_wr_id_norm = character(),
    resolved_face_afy = numeric(),
    resolution_type = character(),
    value_basis = character(),
    resolution_note = character(),
    source_reference = character(),
    reviewed_date = as.Date(character()),
    stringsAsFactors = FALSE
  )
}

pt_validate_swrcb_face_value_override_table <- function(overrides) {
  if (is.null(overrides)) {
    return(pt_empty_swrcb_face_value_overrides())
  }

  overrides <- as.data.frame(overrides, stringsAsFactors = FALSE)
  required <- pt_swrcb_override_required_columns()
  missing <- setdiff(required, names(overrides))
  extra <- setdiff(names(overrides), required)

  if (length(missing) || length(extra)) {
    stop(
      "SWRCB face-value override table schema mismatch.",
      if (length(missing)) {
        paste0(" Missing: ", paste(missing, collapse = ", "), ".")
      } else "",
      if (length(extra)) {
        paste0(" Unexpected: ", paste(extra, collapse = ", "), ".")
      } else "",
      call. = FALSE
    )
  }

  overrides <- overrides[, required, drop = FALSE]
  if (!nrow(overrides)) {
    return(pt_empty_swrcb_face_value_overrides())
  }

  raw_ids <- trimws(as.character(overrides$swrcb_wr_id_norm))
  ids <- pt_norm_swrcb_wr_id(raw_ids)
  if (any(is.na(ids))) {
    stop("SWRCB face-value overrides contain blank water-right IDs.",
         call. = FALSE)
  }
  if (anyDuplicated(ids)) {
    duplicate_ids <- unique(ids[duplicated(ids) | duplicated(ids, fromLast = TRUE)])
    stop(
      "SWRCB face-value overrides contain duplicate IDs: ",
      paste(sort(duplicate_ids, method = "radix"), collapse = ", "),
      call. = FALSE
    )
  }

  raw_values <- trimws(as.character(overrides$resolved_face_afy))
  placeholder_value <- grepl(
    "^(?:TBD|TODO|PLACEHOLDER|UNKNOWN|N/?A|-)$",
    raw_values,
    ignore.case = TRUE,
    perl = TRUE
  )
  if (any(placeholder_value)) {
    stop(
      "SWRCB face-value overrides contain placeholder resolved values for: ",
      paste(ids[placeholder_value], collapse = ", "),
      call. = FALSE
    )
  }
  resolved <- pt_parse_swrcb_face_values(
    raw_values,
    ids = ids,
    label = "override resolved_face_afy"
  )
  invalid_resolved <- is.na(resolved) | !is.finite(resolved) | resolved < 0
  if (any(invalid_resolved)) {
    stop(
      "SWRCB face-value overrides require finite nonnegative values for: ",
      paste(ids[invalid_resolved], collapse = ", "),
      call. = FALSE
    )
  }

  resolution_type <- trimws(as.character(overrides$resolution_type))
  invalid_resolution_type <- is.na(resolution_type) |
    resolution_type != "reviewed_authoritative_override"
  if (any(invalid_resolution_type)) {
    stop(
      "SWRCB override resolution_type must be ",
      "'reviewed_authoritative_override'. Affected: ",
      paste(ids[invalid_resolution_type], collapse = ", "),
      call. = FALSE
    )
  }

  value_basis <- trimws(as.character(overrides$value_basis))
  allowed_basis <- c(
    "conflicting_authoritative_row",
    "independent_authoritative_source"
  )
  if (any(!value_basis %in% allowed_basis)) {
    stop(
      "SWRCB override value_basis must be one of: ",
      paste(allowed_basis, collapse = ", "),
      ". Affected: ",
      paste(ids[!value_basis %in% allowed_basis], collapse = ", "),
      call. = FALSE
    )
  }

  note <- trimws(as.character(overrides$resolution_note))
  source_reference <- trimws(as.character(overrides$source_reference))
  placeholder_text <- paste0(
    "(?:\\bTBD\\b|\\bTODO\\b|\\bPLACEHOLDER\\b|",
    "\\bUNKNOWN\\b|\\bN/?A\\b)"
  )
  invalid_note <- is.na(note) | nchar(note) < 20L |
    grepl(placeholder_text, note, ignore.case = TRUE, perl = TRUE)
  invalid_source <- is.na(source_reference) | nchar(source_reference) < 8L |
    grepl(placeholder_text, source_reference, ignore.case = TRUE, perl = TRUE)
  if (any(invalid_note)) {
    stop(
      "SWRCB overrides require a meaningful resolution_note for: ",
      paste(ids[invalid_note], collapse = ", "),
      call. = FALSE
    )
  }
  if (any(invalid_source)) {
    stop(
      "SWRCB overrides require a meaningful source_reference for: ",
      paste(ids[invalid_source], collapse = ", "),
      call. = FALSE
    )
  }

  reviewed_text <- trimws(as.character(overrides$reviewed_date))
  reviewed_date <- suppressWarnings(as.Date(reviewed_text, format = "%Y-%m-%d"))
  invalid_date <- is.na(reviewed_date) |
    format(reviewed_date, "%Y-%m-%d") != reviewed_text
  if (any(invalid_date)) {
    stop(
      "SWRCB overrides require ISO reviewed_date values (YYYY-MM-DD) for: ",
      paste(ids[invalid_date], collapse = ", "),
      call. = FALSE
    )
  }

  data.frame(
    swrcb_wr_id_norm = ids,
    resolved_face_afy = resolved,
    resolution_type = resolution_type,
    value_basis = value_basis,
    resolution_note = note,
    source_reference = source_reference,
    reviewed_date = reviewed_date,
    stringsAsFactors = FALSE
  )[order(ids, method = "radix"), , drop = FALSE]
}

pt_read_swrcb_face_value_overrides <- function(path) {
  if (is.null(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    return(pt_empty_swrcb_face_value_overrides())
  }
  if (!file.exists(path)) {
    stop(
      "Required SWRCB face-value override table not found: ", path,
      call. = FALSE
    )
  }

  overrides <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character()),
    name_repair = "minimal"
  )
  pt_validate_swrcb_face_value_override_table(overrides)
}

pt_build_swrcb_face_value_lookup <- function(
    raw_rows,
    overrides = NULL,
    source_reference = "SWRCB-provided 2026 BLM export") {
  raw_rows <- as.data.frame(raw_rows, stringsAsFactors = FALSE)
  required <- c("Water Right/Claim ID", "Face value")
  missing <- setdiff(required, names(raw_rows))
  if (length(missing)) {
    stop(
      "Required column(s) missing from SWRCB 2026 BLM water-right export: ",
      paste(missing, collapse = ", "),
      "\nAvailable columns: ", paste(names(raw_rows), collapse = ", "),
      call. = FALSE
    )
  }

  raw_rows$.source_row <- seq_len(nrow(raw_rows))
  raw_rows$swrcb_wr_id_norm <- pt_norm_swrcb_wr_id(
    raw_rows[["Water Right/Claim ID"]]
  )
  raw_rows$face_value_2026_raw <- pt_clean_swrcb_face_value_text(
    raw_rows[["Face value"]]
  )
  raw_rows$face_value_2026_numeric <- pt_parse_swrcb_face_values(
    raw_rows$face_value_2026_raw,
    ids = raw_rows[["Water Right/Claim ID"]],
    label = "Face value"
  )

  invalid_id_rows <- raw_rows[is.na(raw_rows$swrcb_wr_id_norm), , drop = FALSE]
  if (nrow(invalid_id_rows)) {
    warning(
      "SWRCB 2026 BLM export contains ", nrow(invalid_id_rows),
      " row(s) without a usable Water Right/Claim ID; they are retained in ",
      "invalid-ID QA and excluded from ID resolution.",
      call. = FALSE
    )
  }

  usable_rows <- raw_rows[!is.na(raw_rows$swrcb_wr_id_norm), , drop = FALSE]
  ids <- sort(unique(usable_rows$swrcb_wr_id_norm), method = "radix")

  lookup_rows <- lapply(ids, function(id) {
    rows <- usable_rows[usable_rows$swrcb_wr_id_norm == id, , drop = FALSE]
    numeric_values <- sort(unique(stats::na.omit(
      rows$face_value_2026_numeric
    )))
    usable_value_row_count <- sum(!is.na(rows$face_value_2026_numeric))
    distinct_count <- length(numeric_values)
    original_conflict <- distinct_count > 1L
    zero_truncation <- distinct_count == 2L &&
      sum(numeric_values == 0) == 1L &&
      sum(numeric_values > 0 & numeric_values < 1) == 1L

    resolution <- if (distinct_count == 0L) {
      "blank_or_missing"
    } else if (distinct_count == 1L && usable_value_row_count == 1L) {
      "single_row"
    } else if (distinct_count == 1L) {
      "duplicate_rows_same_value"
    } else if (zero_truncation) {
      "duplicate_zero_truncation_resolved"
    } else {
      "unresolved_authoritative_conflict"
    }

    resolved <- if (distinct_count == 1L) {
      numeric_values[[1]]
    } else if (zero_truncation) {
      numeric_values[numeric_values > 0 & numeric_values < 1][[1]]
    } else {
      NA_real_
    }

    raw_unique <- sort(
      unique(stats::na.omit(rows$face_value_2026_raw)),
      method = "radix"
    )

    data.frame(
      swrcb_wr_id_norm = id,
      spreadsheet_row_count = nrow(rows),
      face_value_2026_usable_value_row_count = usable_value_row_count,
      face_value_2026_raw = if (length(raw_unique) == 1L) {
        raw_unique[[1]]
      } else {
        NA_character_
      },
      face_value_2026_raw_values =
        pt_collapse_unique_swrcb_text(rows$face_value_2026_raw),
      face_value_2026_numeric_values =
        pt_format_swrcb_numeric_values(numeric_values),
      face_value_2026_distinct_count = distinct_count,
      face_value_2026_conflict = original_conflict,
      face_value_2026_automatic_resolution = zero_truncation,
      face_value_2026_reviewed_override_used = FALSE,
      face_afy_2026 = resolved,
      face_value_2026_resolution = resolution,
      face_value_2026_source_reference = source_reference,
      face_value_2026_override_note = NA_character_,
      face_value_2026_override_reviewed_date = as.Date(NA),
      stringsAsFactors = FALSE
    )
  })

  lookup <- if (length(lookup_rows)) {
    do.call(rbind, lookup_rows)
  } else {
    data.frame(
      swrcb_wr_id_norm = character(),
      spreadsheet_row_count = integer(),
      face_value_2026_usable_value_row_count = integer(),
      face_value_2026_raw = character(),
      face_value_2026_raw_values = character(),
      face_value_2026_numeric_values = character(),
      face_value_2026_distinct_count = integer(),
      face_value_2026_conflict = logical(),
      face_value_2026_automatic_resolution = logical(),
      face_value_2026_reviewed_override_used = logical(),
      face_afy_2026 = numeric(),
      face_value_2026_resolution = character(),
      face_value_2026_source_reference = character(),
      face_value_2026_override_note = character(),
      face_value_2026_override_reviewed_date = as.Date(character()),
      stringsAsFactors = FALSE
    )
  }

  overrides <- pt_validate_swrcb_face_value_override_table(overrides)
  if (nrow(overrides)) {
    unresolved_ids <- lookup$swrcb_wr_id_norm[
      lookup$face_value_2026_resolution ==
        "unresolved_authoritative_conflict"
    ]
    irrelevant <- setdiff(overrides$swrcb_wr_id_norm, unresolved_ids)
    if (length(irrelevant)) {
      stop(
        "SWRCB face-value overrides include IDs that are not current ",
        "unresolved authoritative conflicts: ",
        paste(sort(irrelevant, method = "radix"), collapse = ", "),
        call. = FALSE
      )
    }

    for (i in seq_len(nrow(overrides))) {
      override <- overrides[i, , drop = FALSE]
      lookup_row <- match(override$swrcb_wr_id_norm, lookup$swrcb_wr_id_norm)
      source_values <- usable_rows$face_value_2026_numeric[
        usable_rows$swrcb_wr_id_norm == override$swrcb_wr_id_norm
      ]
      source_values <- unique(stats::na.omit(source_values))
      represented <- any(abs(
        source_values - override$resolved_face_afy
      ) <= 0.0000000000005)

      if (!represented &&
          override$value_basis != "independent_authoritative_source") {
        stop(
          "SWRCB override value for ", override$swrcb_wr_id_norm,
          " is not represented in the conflicting authoritative rows. ",
          "Use value_basis='independent_authoritative_source' only with a ",
          "specifically documented independent authoritative source.",
          call. = FALSE
        )
      }
      if (!represented &&
          override$value_basis == "independent_authoritative_source" &&
          identical(
            trimws(override$source_reference),
            trimws(source_reference)
          )) {
        stop(
          "SWRCB override value for ", override$swrcb_wr_id_norm,
          " claims an independent source but cites only the conflicting export.",
          call. = FALSE
        )
      }

      lookup$face_afy_2026[lookup_row] <- override$resolved_face_afy
      lookup$face_value_2026_resolution[lookup_row] <-
        "reviewed_authoritative_override"
      lookup$face_value_2026_reviewed_override_used[lookup_row] <- TRUE
      lookup$face_value_2026_source_reference[lookup_row] <-
        override$source_reference
      lookup$face_value_2026_override_note[lookup_row] <-
        override$resolution_note
      lookup$face_value_2026_override_reviewed_date[lookup_row] <-
        override$reviewed_date
    }
  }

  lookup <- lookup[order(lookup$swrcb_wr_id_norm, method = "radix"),
                   , drop = FALSE]
  row.names(lookup) <- NULL

  conflict_lookup <- lookup[
    lookup$face_value_2026_conflict,
    c(
      "swrcb_wr_id_norm",
      "face_value_2026_numeric_values",
      "face_value_2026_resolution",
      "face_value_2026_automatic_resolution",
      "face_value_2026_reviewed_override_used",
      "face_afy_2026",
      "face_value_2026_source_reference"
    ),
    drop = FALSE
  ]
  duplicate_conflicts <- usable_rows[
    usable_rows$swrcb_wr_id_norm %in% conflict_lookup$swrcb_wr_id_norm,
    ,
    drop = FALSE
  ]
  if (nrow(duplicate_conflicts)) {
    qa_names <- names(duplicate_conflicts)
    unnamed <- which(is.na(qa_names) | !nzchar(qa_names))
    qa_names[unnamed] <- paste0(".unnamed_source_column_", unnamed)
    names(duplicate_conflicts) <- make.unique(qa_names, sep = "_")

    conflict_index <- match(
      duplicate_conflicts$swrcb_wr_id_norm,
      conflict_lookup$swrcb_wr_id_norm
    )
    added_fields <- setdiff(
      names(conflict_lookup),
      "swrcb_wr_id_norm"
    )
    for (field in added_fields) {
      duplicate_conflicts[[field]] <-
        conflict_lookup[[field]][conflict_index]
    }
    duplicate_conflicts <- duplicate_conflicts[
      order(
        duplicate_conflicts$swrcb_wr_id_norm,
        duplicate_conflicts$.source_row,
        method = "radix"
      ),
      ,
      drop = FALSE
    ]
  }

  unresolved_conflicts <- lookup[
    lookup$face_value_2026_resolution ==
      "unresolved_authoritative_conflict",
    ,
    drop = FALSE
  ]

  summary <- data.frame(
    authoritative_source_rows = nrow(raw_rows),
    unique_authoritative_ids = nrow(lookup),
    ids_with_one_usable_value = sum(
      lookup$face_value_2026_resolution == "single_row"
    ),
    duplicate_rows_same_value = sum(
      lookup$face_value_2026_resolution == "duplicate_rows_same_value"
    ),
    original_conflicting_ids = sum(lookup$face_value_2026_conflict),
    automatically_resolved_zero_truncation_conflicts = sum(
      lookup$face_value_2026_automatic_resolution
    ),
    reviewed_override_resolutions = sum(
      lookup$face_value_2026_reviewed_override_used
    ),
    unresolved_blocking_conflicts = nrow(unresolved_conflicts),
    ids_present_with_blank_or_missing_values = sum(
      lookup$face_value_2026_resolution == "blank_or_missing"
    ),
    invalid_id_source_rows = nrow(invalid_id_rows),
    stringsAsFactors = FALSE
  )

  list(
    lookup = lookup,
    raw_rows = raw_rows,
    duplicate_conflicts = duplicate_conflicts,
    unresolved_conflicts = unresolved_conflicts,
    invalid_id_rows = invalid_id_rows,
    parse_failures = raw_rows[FALSE, , drop = FALSE],
    overrides = overrides,
    summary = summary
  )
}

pt_abort_on_unresolved_swrcb_face_values <- function(
    unresolved,
    override_path = NULL) {
  if (is.list(unresolved) && !is.data.frame(unresolved) &&
      !is.null(unresolved$unresolved_conflicts)) {
    unresolved <- unresolved$unresolved_conflicts
  }
  unresolved <- as.data.frame(unresolved, stringsAsFactors = FALSE)
  if (!nrow(unresolved)) return(invisible(TRUE))

  details <- paste0(
    unresolved$swrcb_wr_id_norm,
    " = [",
    unresolved$face_value_2026_numeric_values,
    "]"
  )
  override_instruction <- if (
    !is.null(override_path) &&
      length(override_path) == 1L &&
      !is.na(override_path) &&
      nzchar(override_path)
  ) {
    paste0(
      "\nAdd only an authoritatively supported reviewed decision to: ",
      override_path
    )
  } else {
    "\nAdd only an authoritatively supported reviewed override."
  }

  stop(
    "Unresolved authoritative SWRCB 2026 face-value conflict(s): ",
    paste(details, collapse = "; "),
    ". Public CalWATRS fallback is prohibited for IDs present in the ",
    "authoritative export.",
    override_instruction,
    call. = FALSE
  )
}

pt_read_swrcb_2026_blm_wr_lookup <- function(
    path,
    override_path = NULL,
    stop_on_unresolved = TRUE,
    source_reference = NULL) {
  if (is.null(path) || length(path) != 1L || is.na(path) ||
      !file.exists(path)) {
    stop(
      "Required SWRCB 2026 BLM water-right correction CSV not found: ",
      path,
      "\nThe map build is stopped rather than silently dropping official ",
      "membership and face-value corrections.",
      call. = FALSE
    )
  }

  if (is.null(source_reference)) {
    source_reference <- paste0(
      "SWRCB-provided 2026 BLM export: ", basename(path)
    )
  }

  raw_rows <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character()),
    name_repair = "minimal"
  )
  overrides <- pt_read_swrcb_face_value_overrides(override_path)
  result <- pt_build_swrcb_face_value_lookup(
    raw_rows = raw_rows,
    overrides = overrides,
    source_reference = source_reference
  )

  summary <- result$summary
  message(
    "SWRCB 2026 BLM correction lookup loaded: ",
    summary$unique_authoritative_ids, " unique IDs; ",
    summary$automatically_resolved_zero_truncation_conflicts,
    " zero-truncation conflict(s) automatically resolved; ",
    summary$reviewed_override_resolutions,
    " reviewed conflict resolution(s); ",
    summary$unresolved_blocking_conflicts,
    " unresolved conflict(s)."
  )

  if (isTRUE(stop_on_unresolved)) {
    pt_abort_on_unresolved_swrcb_face_values(
      result$unresolved_conflicts,
      override_path = override_path
    )
  }

  result
}

pt_reconcile_swrcb_face_values <- function(
    water_right_ids,
    public_face_afy,
    public_face_units = NULL,
    lookup,
    stop_on_unresolved = TRUE,
    override_path = NULL) {
  lookup <- as.data.frame(lookup, stringsAsFactors = FALSE)
  n <- length(water_right_ids)
  if (length(public_face_afy) != n) {
    stop("public_face_afy must have the same length as water_right_ids.",
         call. = FALSE)
  }
  if (is.null(public_face_units)) {
    public_face_units <- rep(NA_character_, n)
  }
  if (length(public_face_units) != n) {
    stop("public_face_units must have the same length as water_right_ids.",
         call. = FALSE)
  }

  ids <- pt_norm_swrcb_wr_id(water_right_ids)
  index <- match(ids, lookup$swrcb_wr_id_norm)
  present <- !is.na(index)
  resolution <- lookup$face_value_2026_resolution[index]
  authoritative_value <- lookup$face_afy_2026[index]
  unresolved_rows <- present &
    resolution == "unresolved_authoritative_conflict"

  if (isTRUE(stop_on_unresolved) && any(unresolved_rows)) {
    unresolved_ids <- unique(ids[unresolved_rows])
    pt_abort_on_unresolved_swrcb_face_values(
      lookup[lookup$swrcb_wr_id_norm %in% unresolved_ids, , drop = FALSE],
      override_path = override_path
    )
  }

  selected_from_authoritative <- present &
    !is.na(authoritative_value) &
    resolution %in% c(
      "single_row",
      "duplicate_rows_same_value",
      "duplicate_zero_truncation_resolved",
      "reviewed_authoritative_override"
    )
  present_blank <- present & resolution == "blank_or_missing"
  public_allowed <- !present | present_blank

  selected_face_afy <- rep(NA_real_, n)
  selected_face_afy[selected_from_authoritative] <-
    authoritative_value[selected_from_authoritative]
  selected_face_afy[public_allowed] <-
    suppressWarnings(as.numeric(public_face_afy[public_allowed]))

  public_units <- pt_clean_swrcb_face_value_text(public_face_units)
  selected_units <- ifelse(
    selected_from_authoritative,
    "AFY",
    ifelse(
      tolower(public_units) == "acre-feet per year",
      "AFY",
      ifelse(is.na(public_units), "AFY", public_units)
    )
  )

  source_display <- rep(
    "Unresolved SWRCB face-value provenance",
    n
  )
  source_display[!present] <-
    "SWRCB/CalWATRS public data; ID absent from 2026 export"
  source_display[present_blank] <-
    paste0(
      "SWRCB/CalWATRS public data; ",
      "2026 export value blank or missing"
    )
  source_display[present & resolution %in% c(
    "single_row",
    "duplicate_rows_same_value"
  )] <- "SWRCB-provided 2026 BLM export"
  source_display[present &
    resolution == "duplicate_zero_truncation_resolved"] <-
    paste0(
      "SWRCB-provided 2026 BLM export; ",
      "fractional zero conflict resolved"
    )
  source_display[present &
    resolution == "reviewed_authoritative_override"] <-
    paste0(
      "SWRCB-provided 2026 BLM export; ",
      "reviewed conflict resolution"
    )
  source_display[unresolved_rows] <-
    "Unresolved SWRCB-provided 2026 BLM export conflict"

  public_numeric <- suppressWarnings(as.numeric(public_face_afy))
  changed <- selected_from_authoritative & (
    is.na(public_numeric) |
      abs(authoritative_value - public_numeric) > 0.0000005
  )
  numeric_equal_public <- selected_from_authoritative &
    !is.na(public_numeric) &
    abs(authoritative_value - public_numeric) <= 0.0000005

  get_lookup <- function(name, default = NA) {
    if (!name %in% names(lookup)) return(rep(default, n))
    lookup[[name]][index]
  }

  data.frame(
    swrcb_wr_id_norm = ids,
    authoritative_2026_present = present,
    authoritative_face_afy = authoritative_value,
    authoritative_raw_values = get_lookup(
      "face_value_2026_raw_values",
      NA_character_
    ),
    authoritative_numeric_values = get_lookup(
      "face_value_2026_numeric_values",
      NA_character_
    ),
    authoritative_row_count = get_lookup(
      "spreadsheet_row_count",
      NA_integer_
    ),
    authoritative_distinct_value_count = get_lookup(
      "face_value_2026_distinct_count",
      NA_integer_
    ),
    authoritative_original_conflict = dplyr::coalesce(
      get_lookup("face_value_2026_conflict", FALSE),
      FALSE
    ),
    authoritative_automatic_resolution = dplyr::coalesce(
      get_lookup("face_value_2026_automatic_resolution", FALSE),
      FALSE
    ),
    authoritative_reviewed_override_used = dplyr::coalesce(
      get_lookup("face_value_2026_reviewed_override_used", FALSE),
      FALSE
    ),
    authoritative_resolution = resolution,
    authoritative_source_reference = get_lookup(
      "face_value_2026_source_reference",
      NA_character_
    ),
    selected_face_afy = selected_face_afy,
    selected_face_units = selected_units,
    selected_face_value_source = source_display,
    selected_from_authoritative_2026 = selected_from_authoritative,
    public_value_allowed = public_allowed,
    changed_from_public = changed,
    numerically_equal_to_public_with_corrected_provenance =
      numeric_equal_public,
    stringsAsFactors = FALSE
  )
}
