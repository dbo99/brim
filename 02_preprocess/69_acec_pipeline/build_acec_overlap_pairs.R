# ==== build_acec_overlap_pairs.R ==========================================
## Focused, fail-closed derivation of the ACEC semantic overlap graph.
## Geometry is read from an explicit current raw snapshot, repaired in
## EPSG:3310, and never simplified. This file performs no network requests.

pt_acec_overlap_required_packages <- c("sf", "digest")
pt_acec_overlap_missing_packages <- pt_acec_overlap_required_packages[
  !vapply(
    pt_acec_overlap_required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]
if (length(pt_acec_overlap_missing_packages)) {
  stop(
    "ACEC overlap derivation requires: ",
    paste(pt_acec_overlap_missing_packages, collapse = ", ")
  )
}

pt_acec_overlap_assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

pt_acec_overlap_normalize_guid <- function(x) {
  value <- tolower(gsub("[{}[:space:]]", "", trimws(as.character(x))))
  value[is.na(value) | !nzchar(value)] <- NA_character_
  value
}

pt_acec_overlap_component_id <- function(x) {
  value <- pt_acec_overlap_normalize_guid(x)
  ifelse(startsWith(value, "blmca-"), value, paste0("blmca-", value))
}

pt_acec_overlap_graph_metrics <- function(pairs) {
  nodes <- sort(unique(c(pairs$acec_id_a, pairs$acec_id_b)))
  neighbors <- stats::setNames(lapply(nodes, function(node) {
    unique(c(
      pairs$acec_id_b[pairs$acec_id_a == node],
      pairs$acec_id_a[pairs$acec_id_b == node]
    ))
  }), nodes)
  seen <- character(0)
  component_sizes <- integer(0)
  for (node in nodes) {
    if (node %in% seen) next
    queue <- node
    component <- character(0)
    while (length(queue)) {
      current <- queue[[1]]
      queue <- queue[-1]
      if (current %in% seen) next
      seen <- c(seen, current)
      component <- c(component, current)
      queue <- c(queue, setdiff(neighbors[[current]], seen))
    }
    component_sizes <- c(component_sizes, length(component))
  }
  list(
    pair_count = nrow(pairs),
    participant_count = length(nodes),
    connected_component_count = length(component_sizes),
    connected_component_sizes = sort(component_sizes, decreasing = TRUE),
    maximum_degree = max(lengths(neighbors))
  )
}

build_acec_overlap_pairs <- function(
  current_raw_geojson,
  components_path = file.path("00_config", "local_reference_acec_components.csv"),
  reference_path = file.path("00_config", "local_reference_acec_reference.csv"),
  output_path,
  minimum_overlap_area_m2 = 1,
  expected_snapshot_sha256 =
    "0e2658c269476fa629fa7da83b93097e76655042a56bc1cc9d96e10fa6193d00",
  expected_records = 238L,
  expected_pairs = 27L
) {
  pt_acec_overlap_assert(file.exists(current_raw_geojson), paste(
    "Current ACEC raw snapshot is missing:", current_raw_geojson
  ))
  pt_acec_overlap_assert(file.exists(components_path), paste(
    "ACEC component crosswalk is missing:", components_path
  ))
  pt_acec_overlap_assert(file.exists(reference_path), paste(
    "ACEC semantic reference is missing:", reference_path
  ))
  pt_acec_overlap_assert(!file.exists(output_path), paste(
    "Refusing to overwrite ACEC overlap output:", output_path
  ))
  pt_acec_overlap_assert(
    identical(
      digest::digest(
        current_raw_geojson,
        algo = "sha256",
        file = TRUE,
        serialize = FALSE
      ),
      expected_snapshot_sha256
    ),
    "Current ACEC raw snapshot hash differs from the reviewed derivation input."
  )
  pt_acec_overlap_assert(
    is.finite(minimum_overlap_area_m2) && minimum_overlap_area_m2 > 0,
    "ACEC overlap minimum area must be a positive finite number."
  )

  source_raw <- sf::st_read(
    current_raw_geojson,
    quiet = TRUE,
    stringsAsFactors = FALSE
  )
  pt_acec_overlap_assert(
    inherits(source_raw, "sf") && nrow(source_raw) == expected_records,
    "Current ACEC raw snapshot must contain exactly 238 spatial records."
  )
  pt_acec_overlap_assert(
    all(c("GlobalID", "ACEC_NAME") %in% names(source_raw)),
    "Current ACEC raw snapshot is missing GlobalID or ACEC_NAME."
  )
  source_raw$component_id <- pt_acec_overlap_component_id(source_raw$GlobalID)
  pt_acec_overlap_assert(
    !anyNA(source_raw$component_id) && !anyDuplicated(source_raw$component_id),
    "Current ACEC GlobalIDs must be complete and unique."
  )

  components <- utils::read.csv(
    components_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  reference <- utils::read.csv(
    reference_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  pt_acec_overlap_assert(
    nrow(components) == expected_records &&
      !anyDuplicated(components$component_id) &&
      nrow(reference) == expected_records &&
      !anyDuplicated(reference$acec_id),
    "ACEC overlap derivation requires exact 238-row component and semantic tables."
  )
  pt_acec_overlap_assert(
    setequal(source_raw$component_id, components$component_id),
    "Current geometry GlobalIDs do not match the ACEC component crosswalk."
  )
  source_raw$acec_id <- components$acec_id[
    match(source_raw$component_id, components$component_id)
  ]
  pt_acec_overlap_assert(
    setequal(source_raw$acec_id, reference$acec_id),
    "Current ACEC geometry does not reconcile to all semantic ACEC IDs."
  )

  repaired <- sf::st_make_valid(sf::st_transform(
    source_raw[, c("acec_id", "component_id", "ACEC_NAME")],
    3310
  ))
  pt_acec_overlap_assert(
    all(sf::st_is_valid(repaired)) && !any(sf::st_is_empty(repaired)),
    "Repaired ACEC overlap geometry must be valid and non-empty."
  )
  candidates <- sf::st_intersects(repaired)
  pair_index <- do.call(rbind, lapply(seq_len(nrow(repaired)), function(i) {
    j <- candidates[[i]]
    j <- j[j > i]
    if (length(j)) cbind(i = i, j = j) else NULL
  }))
  pt_acec_overlap_assert(
    is.matrix(pair_index) && ncol(pair_index) == 2L,
    "ACEC overlap candidate search did not produce a pair matrix."
  )
  overlap_area_m2 <- vapply(seq_len(nrow(pair_index)), function(k) {
    intersection <- suppressWarnings(sf::st_intersection(
      sf::st_geometry(repaired[pair_index[k, "i"], , drop = FALSE]),
      sf::st_geometry(repaired[pair_index[k, "j"], , drop = FALSE])
    ))
    if (!length(intersection) || all(sf::st_is_empty(intersection))) return(0)
    sum(as.numeric(sf::st_area(intersection)))
  }, numeric(1))
  keep <- is.finite(overlap_area_m2) &
    overlap_area_m2 > minimum_overlap_area_m2
  pair_index <- pair_index[keep, , drop = FALSE]
  overlap_area_m2 <- overlap_area_m2[keep]
  id_left <- repaired$acec_id[pair_index[, "i"]]
  id_right <- repaired$acec_id[pair_index[, "j"]]
  acec_id_a <- pmin(id_left, id_right)
  acec_id_b <- pmax(id_left, id_right)
  acec_area_m2 <- stats::setNames(
    as.numeric(sf::st_area(repaired)),
    repaired$acec_id
  )
  percent_of_smaller <- 100 * overlap_area_m2 /
    pmin(acec_area_m2[acec_id_a], acec_area_m2[acec_id_b])
  pairs <- data.frame(
    acec_id_a = acec_id_a,
    acec_id_b = acec_id_b,
    overlap_area_m2 = round(overlap_area_m2, 6),
    overlap_area_acres = round(overlap_area_m2 / 4046.8564224, 9),
    percent_of_smaller_acec = round(percent_of_smaller, 8),
    relationship_type = ifelse(
      percent_of_smaller >= 99.99,
      "containment",
      "overlap"
    ),
    stringsAsFactors = FALSE
  )
  pairs <- pairs[order(pairs$acec_id_a, pairs$acec_id_b), , drop = FALSE]
  rownames(pairs) <- NULL

  metrics <- pt_acec_overlap_graph_metrics(pairs)
  pt_acec_overlap_assert(
    metrics$pair_count == expected_pairs &&
      metrics$participant_count == 39L &&
      metrics$connected_component_count == 13L &&
      identical(
        metrics$connected_component_sizes,
        c(7L, 6L, 5L, 3L, rep(2L, 9L))
      ) &&
      metrics$maximum_degree == 4L &&
      sum(pairs$relationship_type == "containment") == 2L &&
      !anyDuplicated(paste(pairs$acec_id_a, pairs$acec_id_b, sep = "|")),
    "Derived ACEC overlap graph differs from the reviewed graph contract."
  )

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(pairs, output_path, row.names = FALSE, na = "")
  message(
    "Wrote ", nrow(pairs), " ACEC overlap pairs from repaired, unsimplified ",
    "current geometry; no source or processed geometry was changed."
  )
  invisible(list(pairs = pairs, metrics = metrics, output_path = output_path))
}
