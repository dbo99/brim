# ==== build_acec_field_office_context.R =====================================
## Focused, fail-closed derivation of current field-office spatial context for
## California ACECs. The geometry source is BRIM's existing field-office RDS,
## not a second administrative-boundary download. This file performs no
## network requests and never modifies ACEC or field-office geometry.

pt_acec_fo_required_packages <- c("sf", "digest")
pt_acec_fo_missing_packages <- pt_acec_fo_required_packages[
  !vapply(pt_acec_fo_required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(pt_acec_fo_missing_packages)) {
  stop(
    "ACEC field-office context derivation requires: ",
    paste(pt_acec_fo_missing_packages, collapse = ", ")
  )
}

pt_acec_fo_assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

pt_acec_fo_normalize_guid <- function(x) {
  value <- tolower(gsub("[{}[:space:]]", "", trimws(as.character(x))))
  value[is.na(value) | !nzchar(value)] <- NA_character_
  value
}

pt_acec_fo_component_id <- function(x) {
  value <- pt_acec_fo_normalize_guid(x)
  ifelse(startsWith(value, "blmca-"), value, paste0("blmca-", value))
}

pt_acec_fo_read_csv <- function(path, required) {
  pt_acec_fo_assert(file.exists(path), paste("Required ACEC lookup is missing:", path))
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(out)[[1]] <- sub("^\ufeff", "", names(out)[[1]])
  missing <- setdiff(required, names(out))
  pt_acec_fo_assert(!length(missing), paste(
    basename(path), "is missing:", paste(missing, collapse = ", ")
  ))
  out
}

build_acec_field_office_context <- function(
  current_raw_geojson,
  field_office_rds,
  current_offices_path = file.path(
    "00_config", "local_reference_acec_current_field_offices.csv"
  ),
  components_path = file.path("00_config", "local_reference_acec_components.csv"),
  reference_path = file.path("00_config", "local_reference_acec_reference.csv"),
  source_offices_path = file.path(
    "00_config", "local_reference_acec_office_assignments.csv"
  ),
  relationship_output_path,
  qa_output_path,
  minimum_intersection_area_m2 = 100,
  complete_coverage_percent = 99.5,
  derivation_date = "2026-08-08",
  expected_acec_sha256 =
    "0e2658c269476fa629fa7da83b93097e76655042a56bc1cc9d96e10fa6193d00",
  expected_field_office_rds_sha256 =
    "89884eb36cafda640ed13165b47beb18005ac086f0549bb5c0dbf1b46ef197c6",
  expected_acecs = 238L,
  expected_offices = 14L,
  expected_relationships = 276L
) {
  pt_acec_fo_assert(file.exists(current_raw_geojson), paste(
    "Current ACEC raw snapshot is missing:", current_raw_geojson
  ))
  pt_acec_fo_assert(file.exists(field_office_rds), paste(
    "BRIM field-office RDS is missing:", field_office_rds
  ))
  pt_acec_fo_assert(!file.exists(relationship_output_path), paste(
    "Refusing to overwrite ACEC field-office relationship output:",
    relationship_output_path
  ))
  pt_acec_fo_assert(!file.exists(qa_output_path), paste(
    "Refusing to overwrite ACEC field-office QA output:", qa_output_path
  ))
  pt_acec_fo_assert(
    is.finite(minimum_intersection_area_m2) && minimum_intersection_area_m2 > 0,
    "Field-office intersection threshold must be positive and finite."
  )
  pt_acec_fo_assert(
    is.finite(complete_coverage_percent) &&
      complete_coverage_percent > 0 && complete_coverage_percent <= 100,
    "Field-office complete-coverage threshold must be in (0, 100]."
  )
  pt_acec_fo_assert(
    identical(
      digest::digest(current_raw_geojson, "sha256", file = TRUE, serialize = FALSE),
      expected_acec_sha256
    ),
    "Current ACEC raw snapshot hash differs from the reviewed input."
  )
  pt_acec_fo_assert(
    identical(
      digest::digest(field_office_rds, "sha256", file = TRUE, serialize = FALSE),
      expected_field_office_rds_sha256
    ),
    "BRIM field-office RDS hash differs from the reviewed single-source input."
  )

  current_offices <- pt_acec_fo_read_csv(current_offices_path, c(
    "office_key", "office_code", "current_official_name", "boundary_source_name",
    "parent_district_code", "parent_district_name", "official_office_url",
    "roster_source_url", "roster_verified_on", "boundary_globalid",
    "boundary_source", "boundary_source_url", "boundary_snapshot_date",
    "boundary_derivative_rds_sha256", "current_roster_status", "sort_order"
  ))
  expected_codes <- c(
    "CAD05000", "CAD06000", "CAD07000", "CAD08000", "CAD09000",
    "CAC05000", "CAC06000", "CAC07000", "CAC08000", "CAC09000",
    "CAN02000", "CAN03000", "CAN05000", "CAN06000"
  )
  roster_text <- paste(
    current_offices$office_key,
    current_offices$current_official_name,
    current_offices$boundary_source_name
  )
  pt_acec_fo_assert(
    nrow(current_offices) == expected_offices &&
      !anyDuplicated(current_offices$office_key) &&
      !anyDuplicated(current_offices$office_code) &&
      !anyDuplicated(pt_acec_fo_normalize_guid(current_offices$boundary_globalid)) &&
      setequal(current_offices$office_code, expected_codes) &&
      all(current_offices$current_roster_status == "current") &&
      all(current_offices$boundary_derivative_rds_sha256 ==
            expected_field_office_rds_sha256) &&
      !any(grepl("Hollister|Alturas|Susanville", roster_text, ignore.case = TRUE)) &&
      all(grepl("^https://www\\.blm\\.gov/office/", current_offices$official_office_url)),
    "Current field-office roster must retain exactly 14 verified offices and no superseded names."
  )

  source_raw <- sf::st_read(
    current_raw_geojson,
    quiet = TRUE,
    stringsAsFactors = FALSE
  )
  field_offices <- readRDS(field_office_rds)
  pt_acec_fo_assert(
    inherits(source_raw, "sf") && nrow(source_raw) == expected_acecs &&
      all(c("GlobalID", "ACEC_NAME") %in% names(source_raw)),
    "Current ACEC snapshot must contain exactly 238 spatial records with source IDs."
  )
  pt_acec_fo_assert(
    inherits(field_offices, "sf") && nrow(field_offices) == expected_offices &&
      all(c(
        "ADM_UNIT_C", "ADMU_NAME", "PARENT_CD", "PARENT_NAM", "GlobalID"
      ) %in% names(field_offices)),
    "BRIM field-office RDS must contain the reviewed 14-record source schema."
  )

  office_index <- match(field_offices$ADM_UNIT_C, current_offices$office_code)
  pt_acec_fo_assert(
    !anyNA(office_index) &&
      !anyDuplicated(field_offices$ADM_UNIT_C) &&
      identical(
        as.character(field_offices$ADMU_NAME),
        current_offices$boundary_source_name[office_index]
      ) &&
      identical(
        pt_acec_fo_normalize_guid(field_offices$GlobalID),
        pt_acec_fo_normalize_guid(current_offices$boundary_globalid[office_index])
      ) &&
      identical(
        as.character(field_offices$PARENT_CD),
        current_offices$parent_district_code[office_index]
      ),
    "BRIM field-office RDS does not reconcile exactly to the verified current roster."
  )

  components <- pt_acec_fo_read_csv(components_path, c(
    "component_id", "acec_id", "source_globalid"
  ))
  reference <- pt_acec_fo_read_csv(reference_path, c(
    "acec_id", "official_acec_name", "source_administrative_unit",
    "source_admin_unit_code"
  ))
  source_offices <- pt_acec_fo_read_csv(source_offices_path, c(
    "acec_id", "component_id", "source_admin_unit_code", "source_admin_unit_label"
  ))
  pt_acec_fo_assert(
    nrow(components) == expected_acecs && !anyDuplicated(components$component_id) &&
      nrow(reference) == expected_acecs && !anyDuplicated(reference$acec_id) &&
      nrow(source_offices) == expected_acecs && !anyDuplicated(source_offices$acec_id),
    "ACEC context derivation requires exact 238-row identity and source-office tables."
  )
  source_raw$component_id <- pt_acec_fo_component_id(source_raw$GlobalID)
  component_index <- match(source_raw$component_id, components$component_id)
  pt_acec_fo_assert(!anyNA(component_index),
    "Current ACEC geometry is not fully covered by the component crosswalk."
  )
  source_raw$acec_id <- components$acec_id[component_index]
  pt_acec_fo_assert(
    setequal(source_raw$acec_id, reference$acec_id) &&
      identical(
        as.character(reference$source_admin_unit_code),
        as.character(source_offices$source_admin_unit_code[
          match(reference$acec_id, source_offices$acec_id)
        ])
      ),
    "Source administrative-unit coding must remain exact and separate from spatial context."
  )

  acec_3310 <- sf::st_make_valid(sf::st_transform(
    source_raw[, c("acec_id", "component_id", "ACEC_NAME")],
    3310
  ))
  field_office_3310 <- sf::st_make_valid(sf::st_transform(
    field_offices[, c(
      "ADM_UNIT_C", "ADMU_NAME", "PARENT_CD", "PARENT_NAM", "GlobalID"
    )],
    3310
  ))
  pt_acec_fo_assert(
    all(sf::st_is_valid(acec_3310)) && !any(sf::st_is_empty(acec_3310)) &&
      all(sf::st_is_valid(field_office_3310)) &&
      !any(sf::st_is_empty(field_office_3310)),
    "Repaired EPSG:3310 derivation geometry must be valid and non-empty."
  )

  intersections <- suppressWarnings(sf::st_intersection(
    acec_3310,
    field_office_3310
  ))
  intersections$intersection_area_m2 <- as.numeric(sf::st_area(intersections))
  all_positive <- sf::st_drop_geometry(intersections[
    is.finite(intersections$intersection_area_m2) &
      intersections$intersection_area_m2 > 0,
    , drop = FALSE
  ])
  retained <- all_positive[
    all_positive$intersection_area_m2 > minimum_intersection_area_m2,
    , drop = FALSE
  ]
  pt_acec_fo_assert(
    nrow(all_positive) == 277L &&
      sum(all_positive$intersection_area_m2 <= minimum_intersection_area_m2) == 1L &&
      nrow(retained) == expected_relationships,
    "Positive-area field-office intersections differ from the reviewed 277/276 contract."
  )

  acec_area_m2 <- stats::setNames(
    as.numeric(sf::st_area(acec_3310)),
    acec_3310$acec_id
  )
  retained$percent_of_acec_area <- 100 * retained$intersection_area_m2 /
    acec_area_m2[retained$acec_id]
  retained_office_index <- match(retained$ADM_UNIT_C, current_offices$office_code)
  pt_acec_fo_assert(!anyNA(retained_office_index),
    "A retained intersection references an office outside the current roster."
  )

  split_rows <- split(seq_len(nrow(retained)), retained$acec_id)
  reference_order <- reference$acec_id
  qa_rows <- lapply(reference_order, function(acec_id) {
    rows <- split_rows[[acec_id]]
    if (is.null(rows)) rows <- integer(0)
    codes <- retained$ADM_UNIT_C[rows]
    office_order <- current_offices$sort_order[match(codes, current_offices$office_code)]
    rows <- rows[order(office_order)]
    codes <- retained$ADM_UNIT_C[rows]
    names <- current_offices$current_official_name[
      match(codes, current_offices$office_code)
    ]
    source_index <- match(acec_id, source_offices$acec_id)
    source_code <- source_offices$source_admin_unit_code[[source_index]]
    source_is_current_field_office <- source_code %in% current_offices$office_code
    coverage_percent <- if (length(rows)) {
      sum(retained$percent_of_acec_area[rows])
    } else 0
    context_class <- if (!length(rows)) {
      "no_spatial_match_review_required"
    } else if (coverage_percent < complete_coverage_percent) {
      "partial_spatial_match_review_required"
    } else if (length(rows) == 1L) {
      "wholly_within_one_field_office"
    } else {
      "crosses_field_office_boundaries"
    }
    source_context_qa_class <- if (!source_is_current_field_office) {
      "district_only_contextualized"
    } else if (!length(rows)) {
      "no_spatial_match"
    } else if (!source_code %in% codes) {
      "source_spatial_disagreement"
    } else if (length(rows) == 1L) {
      "exact_single"
    } else {
      "source_in_multiple"
    }
    data.frame(
      acec_id = acec_id,
      component_id = source_offices$component_id[[source_index]],
      official_acec_name = reference$official_acec_name[
        match(acec_id, reference$acec_id)
      ],
      source_admin_unit_code = source_code,
      source_admin_unit_label = source_offices$source_admin_unit_label[[source_index]],
      source_is_current_field_office = source_is_current_field_office,
      current_field_office_count = length(rows),
      current_field_office_codes = paste(codes, collapse = ";"),
      current_field_office_names = paste(names, collapse = "; "),
      acec_area_m2 = acec_area_m2[[acec_id]],
      retained_context_area_m2 = sum(retained$intersection_area_m2[rows]),
      retained_context_percent = coverage_percent,
      acec_context_class = context_class,
      source_context_qa_class = source_context_qa_class,
      review_required = grepl("review_required$", context_class),
      stringsAsFactors = FALSE
    )
  })
  qa <- do.call(rbind, qa_rows)
  rownames(qa) <- NULL
  context_index <- match(retained$acec_id, qa$acec_id)

  relationships <- data.frame(
    acec_id = retained$acec_id,
    component_id = retained$component_id,
    current_field_office_key = current_offices$office_key[retained_office_index],
    current_field_office_code = retained$ADM_UNIT_C,
    current_field_office_name =
      current_offices$current_official_name[retained_office_index],
    intersection_area_m2 = round(retained$intersection_area_m2, 6),
    intersection_area_acres = round(retained$intersection_area_m2 / 4046.8564224, 9),
    percent_of_acec_area = round(retained$percent_of_acec_area, 8),
    acec_context_class = qa$acec_context_class[context_index],
    source_context_qa_class = qa$source_context_qa_class[context_index],
    relationship_method = "positive-area intersection in EPSG:3310",
    minimum_intersection_area_m2 = minimum_intersection_area_m2,
    complete_coverage_percent = complete_coverage_percent,
    boundary_snapshot_date = current_offices$boundary_snapshot_date[retained_office_index],
    derivation_date = derivation_date,
    confidence = ifelse(
      qa$acec_context_class[context_index] ==
        "partial_spatial_match_review_required",
      "partial_match_review_required",
      "high_for_spatial_context_only"
    ),
    stringsAsFactors = FALSE
  )
  relationships <- relationships[order(
    match(relationships$acec_id, reference_order),
    current_offices$sort_order[
      match(relationships$current_field_office_code, current_offices$office_code)
    ]
  ), , drop = FALSE]
  rownames(relationships) <- NULL

  context_counts <- table(factor(
    qa$acec_context_class,
    levels = c(
      "wholly_within_one_field_office",
      "crosses_field_office_boundaries",
      "partial_spatial_match_review_required",
      "no_spatial_match_review_required"
    )
  ))
  source_counts <- table(factor(
    qa$source_context_qa_class,
    levels = c(
      "exact_single", "source_in_multiple", "district_only_contextualized",
      "source_spatial_disagreement", "no_spatial_match"
    )
  ))
  pt_acec_fo_assert(
    identical(as.integer(context_counts), c(202L, 35L, 1L, 0L)) &&
      identical(as.integer(source_counts), c(106L, 6L, 126L, 0L, 0L)) &&
      !anyDuplicated(paste(
        relationships$acec_id,
        relationships$current_field_office_code,
        sep = "|"
      )) &&
      setequal(unique(relationships$acec_id), reference$acec_id) &&
      setequal(unique(relationships$current_field_office_code), expected_codes) &&
      all(relationships$intersection_area_m2 > minimum_intersection_area_m2) &&
      all(relationships$percent_of_acec_area > 0) &&
      !any(grepl(
        "Hollister|Alturas|Susanville",
        relationships$current_field_office_name,
        ignore.case = TRUE
      )),
    "Derived ACEC field-office context differs from the reviewed relationship contract."
  )

  dir.create(dirname(relationship_output_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(qa_output_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(relationships, relationship_output_path, row.names = FALSE, na = "")
  utils::write.csv(qa, qa_output_path, row.names = FALSE, na = "")
  message(
    "Wrote ", nrow(relationships), " ACEC-current-field-office relationships for ",
    nrow(qa), " semantic ACECs from BRIM's existing field-office geometry source."
  )
  invisible(list(
    relationships = relationships,
    qa = qa,
    context_counts = context_counts,
    source_counts = source_counts,
    relationship_output_path = relationship_output_path,
    qa_output_path = qa_output_path
  ))
}
