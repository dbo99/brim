# Shared well-family cache preparation. Loading this file performs no I/O.
# Normal and focused callers use the same joins and display projection.

pt_read_blm_gw_well_distances <- function(path) {
  readr::read_csv(path, col_types = readr::cols(record_uid = readr::col_character()),
    show_col_types = FALSE, progress = FALSE)
}

pt_prepare_blm_gw_well_inventory_cache <- function(wells, distances = NULL,
                                                 require_distances = FALSE) {
  needed <- c("record_uid", "source_key", "source_display", "hover_line1", "hover_line2")
  if (!inherits(wells, "sf") || !all(needed %in% names(wells))) {
    stop("Invalid normalized well inventory schema.", call. = FALSE)
  }
  keys <- as.character(wells$record_uid)
  if (anyNA(keys) || any(!nzchar(trimws(keys))) || anyDuplicated(keys) ||
      any(!wells$source_key %in% c("noc_blm_drilled", "mojave_2025_blm_field_check"))) {
    stop("Invalid or colliding well inventory keys/sources.", call. = FALSE)
  }
  if (nrow(wells) && (any(sf::st_is_empty(wells)) ||
      any(sf::st_geometry_type(wells) != "POINT") ||
      !identical(sf::st_crs(wells)$epsg, 4326L) || any(!sf::st_is_valid(wells)))) {
    stop("Well cache requires valid nonempty WGS84 points.", call. = FALSE)
  }
  observation_fields <- c("water_level_recorded", "lab_sample_documented")
  albion <- wells$source_key == "mojave_2025_blm_field_check"
  if (any(albion) && (!all(observation_fields %in% names(wells)) ||
      any(vapply(observation_fields, function(nm) {
        !is.logical(wells[[nm]]) || anyNA(wells[[nm]][albion])
      }, logical(1))))) {
    stop("Albion cache requires complete logical observation fields.", call. = FALSE)
  }
  fields <- c("on_blm_ca", "on_blm_ca_chr", "dist_to_blm_mi", "dist_to_blm_ft",
    "blm_distance_label", "blm_distance_bin", "blm_distance_run_time")
  if (any(fields %in% names(wells))) {
    stop("Expected normalized inventory, not an already enriched cache.", call. = FALSE)
  }
  if (is.null(distances)) {
    if (require_distances && nrow(wells)) stop("Missing well distance sidecar.", call. = FALSE)
  } else {
    required <- c("record_uid", "source_key", "longitude", "latitude", fields)
    if (!all(required %in% names(distances)) || anyDuplicated(names(distances)) ||
        anyNA(distances$record_uid) || any(!nzchar(trimws(distances$record_uid))) ||
        anyDuplicated(distances$record_uid) || !setequal(keys, distances$record_uid)) {
      stop("Incomplete, stale, or duplicate well distance keys/schema.", call. = FALSE)
    }
    ii <- match(keys, distances$record_uid)
    xy <- sf::st_coordinates(wells)
    lon <- suppressWarnings(as.numeric(distances$longitude[ii]))
    lat <- suppressWarnings(as.numeric(distances$latitude[ii]))
    if (any(wells$source_key != distances$source_key[ii]) ||
        any(!is.finite(lon) | !is.finite(lat)) ||
        any(abs(xy[, "X"] - lon) > 1e-10 | abs(xy[, "Y"] - lat) > 1e-10) ||
        anyNA(distances$on_blm_ca[ii]) ||
        any(!is.finite(distances$dist_to_blm_mi[ii]) | distances$dist_to_blm_mi[ii] < 0) ||
        any(!is.finite(distances$dist_to_blm_ft[ii]) | distances$dist_to_blm_ft[ii] < 0)) {
      stop("Well distance sidecar source/coordinate/value mismatch.", call. = FALSE)
    }
  }
  out <- clean_sf_for_leaflet(wells)
  if (!is.null(distances)) {
    out <- dplyr::left_join(out, distances[, c("record_uid", fields), drop = FALSE], by = "record_uid")
  } else {
    out$on_blm_ca <- rep(NA, nrow(out))
    out$on_blm_ca_chr <- rep(NA_character_, nrow(out))
    out$dist_to_blm_mi <- rep(NA_real_, nrow(out))
    out$dist_to_blm_ft <- rep(NA_real_, nrow(out))
    for (nm in fields[5:7]) out[[nm]] <- rep(NA_character_, nrow(out))
  }
  if (nrow(out)) {
    out <- out |>
      dplyr::mutate(
        layer_key = dplyr::case_when(
          .data$source_key == "noc_blm_drilled" ~ "noc",
          .data$source_key == "mojave_2025_blm_field_check" ~ "mojave_2025",
          TRUE ~ "other"
        ),
        well_hover_text = dplyr::if_else(
          !is.na(.data$hover_line2) & .data$hover_line2 != "",
          paste0(.data$hover_line1, "\n", .data$hover_line2), .data$hover_line1
        ),
        blm_distance_popup = dplyr::case_when(
          !is.na(.data$blm_distance_label) ~ .data$blm_distance_label,
          .data$on_blm_ca == TRUE ~ "on BLM",
          !is.na(.data$dist_to_blm_mi) ~ paste0(
            format(round(.data$dist_to_blm_mi, 2), trim = TRUE, scientific = FALSE), " mi"
          ),
          TRUE ~ NA_character_
        )
      )
  }
  noc <- dplyr::filter(out, .data$source_key == "noc_blm_drilled")
  # sf's name-based projection preserves geometry position and retained agr
  # metadata when geometry precedes the Albion-only columns.
  noc <- noc[, setdiff(names(noc), observation_fields), drop = FALSE]
  list(
    blm_noc_drilled_wells_map = noc,
    mojave_2025_gw_well_inventory_map = dplyr::filter(out, .data$source_key == "mojave_2025_blm_field_check")
  )
}

pt_refresh_blm_gw_well_inventory_cache <- function(dirs, run_ts,
    read_inventory = readRDS, read_distances = pt_read_blm_gw_well_distances,
    save_cache = save_rds_cached) {
  # Check every input and target before the first save. Only fixed well-family
  # basenames are writable; symlinked directories/files and archive collisions fail.
  if (length(run_ts) != 1L || is.na(run_ts) || !grepl("^[0-9]{8}_[0-9]{6}$", run_ts)) {
    stop("Invalid cache timestamp.", call. = FALSE)
  }
  paths <- c(dirs$rds, dirs$cache_last, dirs$cache_enr)
  if (length(paths) != 3L || any(!dir.exists(paths)) ||
      any(normalizePath(paths, winslash = "/") != paths) || anyDuplicated(paths)) {
    stop("Cache directories must be distinct existing canonical paths.", call. = FALSE)
  }
  input <- file.path(dirs$rds, "blm_gw_well_inventory_combined_wgs84.rds")
  sidecar <- file.path(dirs$cache_last, "blm_gw_well_inventory_blm_distance_fields.csv")
  if (!all(file.exists(c(input, sidecar))) || any(nzchar(Sys.readlink(c(input, sidecar))))) {
    stop("Missing or symlinked well cache prerequisite.", call. = FALSE)
  }
  prepared <- pt_prepare_blm_gw_well_inventory_cache(
    read_inventory(input), read_distances(sidecar), require_distances = TRUE
  )
  if (any(vapply(prepared, nrow, integer(1)) == 0L)) {
    stop("Focused refresh requires both independent well inventories.", call. = FALSE)
  }
  latest <- file.path(dirs$cache_last, paste0(names(prepared), ".rds"))
  archive <- file.path(dirs$cache_enr, paste0(names(prepared), "_", run_ts, ".rds"))
  targets <- c(latest, archive)
  target_links <- Sys.readlink(targets)
  if (anyDuplicated(targets) || any(dir.exists(targets)) ||
      any(!is.na(target_links) & nzchar(target_links)) || any(file.exists(archive))) {
    stop("Well cache output collision.", call. = FALSE)
  }
  for (i in seq_along(prepared)) save_cache(prepared[[i]], archive[i], latest[i])
  invisible(prepared)
}
