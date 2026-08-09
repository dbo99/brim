# ==== 05_build_label_cache.r =================================================
##
## PURPOSE:
##   Build map-ready label layers for PortaTreasure2 from cached map layers.
##
## INPUTS:
##   04_processed_data/cache/latest/*.rds
##
## OUTPUTS:
##   04_processed_data/cache/enriched/labels_all_map_<timestamp>.rds
##   04_processed_data/cache/latest/labels_all_map.rds
##   04_processed_data/qa/labels_all_map_qa_<timestamp>.csv
##
## IMPORTANT:
##   Label fields are defined explicitly in:
##
##     00_config/config_labels.r
##
##   The script does not guess field names. If a label field is wrong for a
##   non-empty included layer, the script stops with a clear error.
##

# ==== 1. Load configuration and helper functions =============================

source("00_config/config_paths.r")
source("00_config/config_labels.r")
source("03_functions/cache_helpers.r")
source("03_functions/spatial_helpers.r")
source("03_functions/label_helpers.r")

# ==== 2. Load packages =======================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(purrr)
  library(readr)
})

if (!requireNamespace("digest", quietly = TRUE)) {
  stop("Canonical label-cache generation requires the digest package.")
}

# ==== 3. Run settings ========================================================

RUN_TS <- make_timestamp()
WRITE_QA <- TRUE

## If TRUE, a non-empty layer with a missing explicit label field will stop.
## This is preferred for reproducibility.
STRICT_LABEL_FIELDS <- TRUE

# ==== 4. Confirm label-field config exists ===================================

if (!exists("LABEL_FIELDS")) {
  stop(
    "LABEL_FIELDS was not found. Add the explicit LABEL_FIELDS list to ",
    "00_config/config_labels.r before running this script."
  )
}

# ==== 5. Helper functions ====================================================

get_explicit_label_field <- function(layer_id) {
  
  field <- LABEL_FIELDS[[layer_id]]
  
  if (is.null(field)) {
    return(NULL)
  }
  
  if (!is.character(field) || length(field) != 1) {
    stop(
      "LABEL_FIELDS$", layer_id,
      " must be either NULL or a single character field name."
    )
  }
  
  field
}

check_label_field <- function(x, layer_id, label_field) {
  
  if (is.null(label_field)) {
    message("Label field for ", layer_id, " is NULL; skipping labels.")
    return(FALSE)
  }
  
  if (!inherits(x, "sf")) {
    stop("Layer is not an sf object: ", layer_id)
  }
  
  if (nrow(x) == 0) {
    message("Layer is empty; labels skipped for ", layer_id, ".")
    return(FALSE)
  }
  
  if (!label_field %in% names(x)) {
    
    msg <- paste0(
      "Explicit label field not found for ", layer_id, ": ", label_field,
      "\nAvailable fields: ",
      paste(names(x), collapse = ", "),
      "\n\nFix by editing LABEL_FIELDS$", layer_id,
      " in 00_config/config_labels.r."
    )
    
    if (STRICT_LABEL_FIELDS) {
      stop(msg)
    } else {
      warning(msg)
      return(FALSE)
    }
  }
  
  TRUE
}

add_polygon_label_layer <- function(label_layers, layer_id, x) {
  
  label_field <- get_explicit_label_field(layer_id)
  
  if (!check_label_field(x, layer_id, label_field)) {
    label_layers[[layer_id]] <- pt_empty_label_sf()
    return(label_layers)
  }
  
  label_layers[[layer_id]] <- pt_make_polygon_labels(
    x = x,
    label_id = layer_id,
    label_field = label_field
  )
  
  label_layers
}

add_point_label_layer <- function(label_layers, layer_id, x) {
  
  label_field <- get_explicit_label_field(layer_id)
  
  if (!check_label_field(x, layer_id, label_field)) {
    label_layers[[layer_id]] <- pt_empty_label_sf()
    return(label_layers)
  }
  
  label_layers[[layer_id]] <- pt_make_point_labels(
    x = x,
    label_id = layer_id,
    label_field = label_field
  )
  
  label_layers
}

read_optional_cached_layer <- function(path, label) {
  
  if (!file.exists(path)) {
    message("Optional cached layer not found; skipping labels for ", label, ": ", path)
    return(NULL)
  }
  
  readRDS(path)
}

# ==== 6. Read cached map layers ==============================================
##
## These are map-ready cached layers, not raw/full-resolution preprocessing
## outputs. Labels should be generated from the same cached geometry used by
## the final map.

huc_all <- read_rds_checked(
  file.path(DIR$cache_last, "huc_all_map.rds"),
  "cached HUC layers"
)

gw <- read_rds_checked(
  file.path(DIR$cache_last, "gw_bull118_map.rds"),
  "cached groundwater basins"
)

county <- read_rds_checked(
  file.path(DIR$cache_last, "county_map.rds"),
  "cached counties"
)

project_areas <- read_rds_checked(
  file.path(DIR$cache_last, "project_areas_map.rds"),
  "cached project areas"
)

cnrfc_basins <- read_rds_checked(
  file.path(DIR$cache_last, "cnrfc_basins_map.rds"),
  "cached CNRFC basins"
)

field_office_outer <- read_rds_checked(
  file.path(DIR$cache_last, "field_office_outer_map.rds"),
  "cached field-office outer boundaries"
)

cnrfc_stream <- read_rds_checked(
  file.path(DIR$cache_last, "cnrfc_stream_map.rds"),
  "cached CNRFC stream gages"
)

cnrfc_precip <- read_rds_checked(
  file.path(DIR$cache_last, "cnrfc_precip_map.rds"),
  "cached CNRFC precip gages"
)

usgs_sw <- read_rds_checked(
  file.path(DIR$cache_last, "usgs_streamgages_map.rds"),
  "cached USGS streamgages"
)

reference_layers <- read_rds_checked(
  file.path(DIR$cache_last, "reference_layers_all_map.rds"),
  "cached reference layers"
)

water_districts <- read_optional_cached_layer(
  file.path(DIR$cache_last, "water_districts_map.rds"),
  "water districts"
)

# ==== 7. Build HUC labels ====================================================
##
## HUC label fields are explicitly defined in LABEL_FIELDS:
##
##   LABEL_FIELDS$huc2
##   LABEL_FIELDS$huc4
##   ...
##   LABEL_FIELDS$huc12

label_layers <- list()

for (nm in c("huc2", "huc4", "huc6", "huc8", "huc10", "huc12")) {
  
  if (!isTRUE(LABEL_INCLUDE[[nm]])) {
    message("LABEL_INCLUDE$", nm, " is FALSE; skipping.")
    next
  }
  
  if (!nm %in% names(huc_all)) {
    message("Cached HUC layer not found: ", nm, "; skipping.")
    label_layers[[nm]] <- pt_empty_label_sf()
    next
  }
  
  label_layers <- add_polygon_label_layer(
    label_layers = label_layers,
    layer_id = nm,
    x = huc_all[[nm]]
  )
}

# ==== 8. Build other polygon labels ==========================================

if (isTRUE(LABEL_INCLUDE$gw_bull118)) {
  label_layers <- add_polygon_label_layer(
    label_layers = label_layers,
    layer_id = "gw_bull118",
    x = gw
  )
}

if (isTRUE(LABEL_INCLUDE$county)) {
  label_layers <- add_polygon_label_layer(
    label_layers = label_layers,
    layer_id = "county",
    x = county
  )
}

if (isTRUE(LABEL_INCLUDE$project_areas)) {
  label_layers <- add_polygon_label_layer(
    label_layers = label_layers,
    layer_id = "project_areas",
    x = project_areas
  )
}

if (isTRUE(LABEL_INCLUDE$cnrfc_basins)) {
  label_layers <- add_polygon_label_layer(
    label_layers = label_layers,
    layer_id = "cnrfc_basins",
    x = cnrfc_basins
  )
}

if (isTRUE(LABEL_INCLUDE$field_office_outer)) {
  label_layers <- add_polygon_label_layer(
    label_layers = label_layers,
    layer_id = "field_office_outer",
    x = field_office_outer
  )
}

# ---- 8A. Build registered Local Reference semantic labels ------------------
##
## The registry defines semantic identity, public label text, and the anchor
## strategy. Federal Wilderness intentionally retains one precomputed anchor
## per source component so the browser can select the largest currently visible
## component while still rendering exactly one label per named wilderness.

local_reference_anchor_qa <- list()
for (i in seq_len(nrow(LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY))) {
  registration <- LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY[i, , drop = FALSE]
  nm <- as.character(registration$source_nickname[[1]])
  if (!isTRUE(registration$lbl_available[[1]]) ||
      !isTRUE(LABEL_INCLUDE[[nm]])) {
    next
  }
  if (!nm %in% names(reference_layers)) {
    stop("Registered Local Reference label source is missing: ", nm)
  }
  labels <- pt_make_local_reference_labels(
    reference_layers[[nm]],
    registration
  )
  local_reference_anchor_qa[[nm]] <-
    pt_validate_local_reference_label_anchors(
      labels,
      reference_layers[[nm]],
      registration
    )
  label_layers[[nm]] <- labels
}

## Allotments remain outside the semantic-label controller until their planned
## Local Reference upgrade is accepted. Preserve the legacy opt-in path only if
## an explicit inclusion switch and source child are both present.
if (isTRUE(LABEL_INCLUDE$allotments)) {
  if (!"allotments" %in% names(reference_layers)) {
    stop("Included allotment label source is missing from the reference cache.")
  }
  label_layers <- add_polygon_label_layer(
    label_layers,
    "allotments",
    reference_layers[["allotments"]]
  )
}

# ---- 8B. Build water district labels ---------------------------------------
##
## Water districts can be difficult to identify by polygon hover/click alone
## because many districts overlap or contain smaller interior districts. A
## separate high-zoom label layer gives users a practical identification tool
## without requiring geometry surgery on the polygon layer.
##
## The layer remains toggleable through the Water Districts row's inline lbl
## control. It is clustered with visually hidden cluster icons, so labels do
## not appear until the configured high zoom threshold is reached.

if (isTRUE(LABEL_INCLUDE$water_districts)) {
  
  if (inherits(water_districts, "sf") && nrow(water_districts) > 0) {
    label_layers <- add_polygon_label_layer(
      label_layers = label_layers,
      layer_id = "water_districts",
      x = water_districts
    )
  } else {
    message("Water district label source is missing or empty; labels skipped.")
    label_layers[["water_districts"]] <- pt_empty_label_sf()
  }
}

# ==== 9. Build point labels ==================================================
##
## USGS wells and BLM offices are intentionally excluded in config_labels.r.
## These point label layers should be high-zoom only because they can be dense.

if (isTRUE(LABEL_INCLUDE$cnrfc_stream)) {
  label_layers <- add_point_label_layer(
    label_layers = label_layers,
    layer_id = "cnrfc_stream",
    x = cnrfc_stream
  )
}

if (isTRUE(LABEL_INCLUDE$cnrfc_precip)) {
  label_layers <- add_point_label_layer(
    label_layers = label_layers,
    layer_id = "cnrfc_precip",
    x = cnrfc_precip
  )
}

if (isTRUE(LABEL_INCLUDE$usgs_streamgages)) {
  label_layers <- add_point_label_layer(
    label_layers = label_layers,
    layer_id = "usgs_streamgages",
    x = usgs_sw
  )
}

# ==== 9A. Canonical child-set and ordering contract ==========================

expected_children <- c(
  c("huc2", "huc4", "huc6", "huc8", "huc10", "huc12")[
    vapply(
      c("huc2", "huc4", "huc6", "huc8", "huc10", "huc12"),
      function(id) isTRUE(LABEL_INCLUDE[[id]]),
      logical(1)
    )
  ],
  c("gw_bull118", "county", "project_areas", "cnrfc_basins", "field_office_outer")[
    vapply(
      c("gw_bull118", "county", "project_areas", "cnrfc_basins", "field_office_outer"),
      function(id) isTRUE(LABEL_INCLUDE[[id]]),
      logical(1)
    )
  ],
  LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY$source_nickname[
    LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY$lbl_available &
      vapply(
        LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY$source_nickname,
        function(id) isTRUE(LABEL_INCLUDE[[id]]),
        logical(1)
      )
  ],
  if (isTRUE(LABEL_INCLUDE$allotments)) "allotments",
  if (isTRUE(LABEL_INCLUDE$water_districts)) "water_districts",
  c("cnrfc_stream", "cnrfc_precip", "usgs_streamgages")[
    vapply(
      c("cnrfc_stream", "cnrfc_precip", "usgs_streamgages"),
      function(id) isTRUE(LABEL_INCLUDE[[id]]),
      logical(1)
    )
  ]
)
if (!identical(names(label_layers), expected_children)) {
  stop(
    "Canonical label child-set/order contract failed. Expected: ",
    paste(expected_children, collapse = ", "),
    "; built: ", paste(names(label_layers), collapse = ", ")
  )
}
if ("major_conveyance" %in% names(label_layers)) {
  stop("Retired major_conveyance label child must not enter the canonical cache.")
}

# ==== 10. Save label cache ===================================================

out_timestamped <- file.path(
  DIR$cache_enr,
  timestamped_name("labels_all_map", "rds", RUN_TS)
)

out_latest <- file.path(
  DIR$cache_last,
  "labels_all_map.rds"
)

save_rds_cached(
  x = label_layers,
  timestamped_path = out_timestamped,
  latest_path = out_latest
)

# ==== 11. Save QA summary ====================================================

if (WRITE_QA) {
  local_registration_by_nickname <- stats::setNames(
    seq_len(nrow(LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY)),
    LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY$source_nickname
  )
  source_cache_paths <- c(
    huc2 = file.path(DIR$cache_last, "huc_all_map.rds"),
    huc4 = file.path(DIR$cache_last, "huc_all_map.rds"),
    huc6 = file.path(DIR$cache_last, "huc_all_map.rds"),
    huc8 = file.path(DIR$cache_last, "huc_all_map.rds"),
    huc10 = file.path(DIR$cache_last, "huc_all_map.rds"),
    huc12 = file.path(DIR$cache_last, "huc_all_map.rds"),
    gw_bull118 = file.path(DIR$cache_last, "gw_bull118_map.rds"),
    county = file.path(DIR$cache_last, "county_map.rds"),
    project_areas = file.path(DIR$cache_last, "project_areas_map.rds"),
    cnrfc_basins = file.path(DIR$cache_last, "cnrfc_basins_map.rds"),
    field_office_outer = file.path(DIR$cache_last, "field_office_outer_map.rds"),
    acec = file.path(DIR$cache_last, "reference_layers_all_map.rds"),
    fedwilderness = file.path(DIR$cache_last, "reference_layers_all_map.rds"),
    wildernessstudyarea = file.path(DIR$cache_last, "reference_layers_all_map.rds"),
    trails = file.path(DIR$cache_last, "reference_layers_all_map.rds"),
    allotments = file.path(DIR$cache_last, "reference_layers_all_map.rds"),
    water_districts = file.path(DIR$cache_last, "water_districts_map.rds"),
    cnrfc_stream = file.path(DIR$cache_last, "cnrfc_stream_map.rds"),
    cnrfc_precip = file.path(DIR$cache_last, "cnrfc_precip_map.rds"),
    usgs_streamgages = file.path(DIR$cache_last, "usgs_streamgages_map.rds")
  )
  source_hashes <- stats::setNames(
    vapply(unique(source_cache_paths), function(path) {
      digest::digest(path, algo = "sha256", serialize = FALSE, file = TRUE)
    }, character(1)),
    unique(source_cache_paths)
  )
  
  qa <- tibble::tibble(
    label_id = names(label_layers),
    status = purrr::map_chr(names(label_layers), function(id) {
      if (id %in% names(local_registration_by_nickname)) {
        "canonical_semantic_generated"
      } else if (nrow(label_layers[[id]]) == 0L) {
        "canonical_empty"
      } else {
        "canonical_generated"
      }
    }),
    provenance = purrr::map_chr(names(label_layers), function(id) {
      if (id %in% names(local_registration_by_nickname)) {
        "registered Local Reference semantic labels from accepted reference cache"
      } else {
        "deterministic labels from accepted map-ready cache"
      }
    }),
    explicit_label_field = purrr::map_chr(names(label_layers), function(id) {
      field <- LABEL_FIELDS[[id]]
      if (is.null(field)) return(NA_character_)
      as.character(field)
    }),
    rows = purrr::map_int(label_layers, nrow),
    semantic_features = purrr::map_int(label_layers, function(x) {
      if (!"semantic_feature_key" %in% names(x)) return(NA_integer_)
      length(unique(as.character(x$semantic_feature_key)))
    }),
    anchor_strategy = purrr::map_chr(label_layers, function(x) {
      if (!"anchor_strategy" %in% names(x) || !nrow(x)) return(NA_character_)
      as.character(x$anchor_strategy[[1]])
    }),
    visible_component_aware = purrr::map_lgl(names(label_layers), function(id) {
      if (!id %in% names(local_registration_by_nickname)) return(FALSE)
      isTRUE(LOCAL_REFERENCE_SEMANTIC_LABEL_REGISTRY$
        visible_component_aware[[local_registration_by_nickname[[id]]]])
    }),
    max_anchor_distance_m = purrr::map_dbl(names(label_layers), function(id) {
      qa_row <- local_reference_anchor_qa[[id]]
      if (is.null(qa_row)) return(NA_real_)
      as.numeric(qa_row$max_anchor_distance_m[[1]])
    }),
    anchor_tolerance_m = purrr::map_dbl(names(label_layers), function(id) {
      qa_row <- local_reference_anchor_qa[[id]]
      if (is.null(qa_row)) return(NA_real_)
      as.numeric(qa_row$tolerance_m[[1]])
    }),
    parent_group = purrr::map_chr(label_layers, function(x) {
      if (nrow(x) == 0) return(NA_character_)
      as.character(x$parent_group[1])
    }),
    label_group = purrr::map_chr(label_layers, function(x) {
      if (nrow(x) == 0) return(NA_character_)
      as.character(x$label_group[1])
    }),
    min_zoom = purrr::map_dbl(label_layers, function(x) {
      if (nrow(x) == 0) return(NA_real_)
      as.numeric(x$min_zoom[1])
    }),
    max_zoom = purrr::map_dbl(label_layers, function(x) {
      if (nrow(x) == 0) return(NA_real_)
      as.numeric(x$max_zoom[1])
    }),
    child_object_sha256 = purrr::map_chr(
      label_layers,
      digest::digest,
      algo = "sha256",
      serialize = TRUE
    ),
    source_cache = unname(source_cache_paths[names(label_layers)]),
    source_cache_sha256 = unname(source_hashes[
      source_cache_paths[names(label_layers)]
    ]),
    run_timestamp = RUN_TS
  )
  qa <- dplyr::bind_rows(
    qa,
    tibble::tibble(
      label_id = "major_conveyance",
      status = "retired_omitted",
      provenance = "stale legacy child; renderer already excludes it",
      explicit_label_field = NA_character_,
      rows = 0L,
      semantic_features = NA_integer_,
      anchor_strategy = NA_character_,
      visible_component_aware = FALSE,
      max_anchor_distance_m = NA_real_,
      anchor_tolerance_m = NA_real_,
      parent_group = NA_character_,
      label_group = NA_character_,
      min_zoom = NA_real_,
      max_zoom = NA_real_,
      child_object_sha256 = NA_character_,
      source_cache = NA_character_,
      source_cache_sha256 = NA_character_,
      run_timestamp = RUN_TS
    )
  )
  
  out_qa <- file.path(
    DIR$qa,
    paste0("labels_all_map_qa_", RUN_TS, ".csv")
  )
  
  readr::write_csv(qa, out_qa)
  
  message("Saved label QA CSV: ", out_qa)
  print(qa)
}

# ==== 12. Final summary ======================================================

message("\nDone: label cache built.")
message("Latest label cache:")
message("  ", out_latest)

message("\nLabel rows by layer:")
print(purrr::map_int(label_layers, nrow))
