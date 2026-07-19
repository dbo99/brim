# ==== qa_local_layer_payload_size_check.r ====================================
##
## PURPOSE:
##   Estimate which BRIM local/static layers are contributing most to the final
##   standalone HTML payload.
##
## WHY THIS EXISTS:
##   Cache RDS size can be misleading.  RDS files are compressed R objects, while
##   the final Leaflet/htmlwidgets output has to embed geometry coordinates,
##   attributes, popup HTML, hover/label text, and style fields as browser-readable
##   text/JavaScript.  A modest-looking RDS can expand substantially in the final
##   self-contained HTML.
##
## HOW TO RUN:
##   From the BRIM project root:
##
##     source("run_build_map.r")
##     qa_local_layer_payload_size_check()
##
## OUTPUTS:
##   qa/local_layer_payload_size_by_layer_YYYYMMDD_HHMMSS.csv
##   qa/local_layer_payload_size_by_cache_file_YYYYMMDD_HHMMSS.csv
##   qa/local_layer_payload_column_bytes_YYYYMMDD_HHMMSS.csv
##   qa/html_output_file_sizes_YYYYMMDD_HHMMSS.csv
##
## NOTES:
##   - Read-only.  Never modifies caches or map outputs.
##   - Payload values are estimates/proxies, not exact final HTML byte counts.
##   - This script now reports two different payload estimates:
##
##       full_cache_text_payload_mb
##         Geometry + every attribute column in the cached sf object. This is
##         useful for finding fat cache objects, but it can overstate final HTML
##         size when Leaflet formulas only serialize a draw-time subset.
##
##       likely_leaflet_text_payload_mb
##         Geometry + popup/hover/style fields that are likely to be serialized
##         into Leaflet. For USGS Wells this uses the same slim draw subset added
##         in Patch 005, so it is a better explanation of actual HTML size.
##
##   - WKT geometry size is used as a geometry-text proxy because it avoids adding
##     a new GeoJSON dependency.  It is close enough for ranking likely bloat.

# ==== 1. Project and package setup ===========================================

if (!dir.exists("00_config") || !dir.exists("04_processed_data")) {
  stop(
    "Run this script from the BRIM project root. Expected folders like ",
    "00_config/ and 04_processed_data/ were not found."
  )
}

source("00_config/config_paths.r")
if (file.exists("00_config/config_local_layer_registry.r")) {
  source("00_config/config_local_layer_registry.r")
}

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
})

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
}

pt_payload_timestamp <- function() {
  format(Sys.time(), "%Y%m%d_%H%M%S")
}

pt_payload_mb <- function(bytes) {
  if (is.na(bytes)) return(NA_real_)
  round(as.numeric(bytes) / 1024^2, 3)
}

pt_payload_file_size_mb <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return(NA_real_)
  round(file.info(path)$size / 1024^2, 3)
}

pt_payload_object_size_mb <- function(x) {
  out <- tryCatch(as.numeric(utils::object.size(x)) / 1024^2, error = function(e) NA_real_)
  round(out, 3)
}

pt_payload_cache_dir <- function() {
  if (exists("DIR") && !is.null(DIR$cache_last) && dir.exists(DIR$cache_last)) {
    return(DIR$cache_last)
  }
  "04_processed_data/cache/latest"
}

pt_payload_safe_names <- function(x) {
  tryCatch(names(x), error = function(e) character(0))
}

pt_payload_chr <- function(x) {
  if (length(x) == 0) return(NA_character_)
  paste(unique(as.character(x)), collapse = ";")
}

pt_payload_blank_fraction <- function(x) {
  if (length(x) == 0) return(NA_real_)
  x <- as.character(x)
  mean(is.na(x) | trimws(x) == "")
}

# ==== 2. Layer inventory =====================================================

pt_payload_fallback_registry <- function() {
  data.frame(
    layer_id = c(
      "blm_core", "blm_diffs", "gw_bull118", "county",
      "huc2", "huc4", "huc6", "huc8", "huc10", "huc12",
      "cnrfc_basins", "cnrfc_fnf_delta",
      "cnrfc_stream", "cnrfc_precip", "cdec_reservoir_stations",
      "usgs_streamgages", "usgs_wells", "swrcb_pod_wr_blm",
      "springs", "scan_stations", "snow_pillows", "x2_km",
      "calsim3_nodes", "major_conveyance", "calsim3_arcs",
      "deltamapr_canals", "water_districts", "rwqcb_regions", "reference_layers"
    ),
    display_name = c(
      "BLM-CA Managed (core)", "BLM Held/Managed Differences",
      "GW – Bull. 118", "Counties",
      "HUC2", "HUC4", "HUC6", "HUC8", "HUC10", "HUC12",
      "CNRFC Basins", "CNRFC FNF Delta Basins",
      "CNRFC River & Reservoir", "CNRFC Precip Gages", "CDEC Reservoir Stations",
      "USGS Streamgages", "USGS Wells", "SWRCB POD/WR BLM records",
      "Springs", "SCAN Stations", "Snow Pillows", "CVP/SWP X2 km points",
      "CalSim3 Nodes", "Major Conveyance", "CalSim3 Arcs",
      "Deltamapr Conveyance", "Water Districts", "RWQCB Regions", "Reference Layers"
    ),
    cache_file = c(
      "blm_core_map.rds", "blm_diffs_map.rds", "gw_bull118_map.rds", "county_map.rds",
      rep("huc_all_map.rds", 6),
      "cnrfc_basins_map.rds", "cnrfc_fnf_delta_map.rds",
      "cnrfc_stream_map.rds", "cnrfc_precip_map.rds", "cdec_reservoir_stations_map.rds",
      "usgs_streamgages_map.rds", "usgs_wells_map.rds", "swrcb_pod_wr_blm_map.rds",
      "springs_map.rds", "scan_stations_map.rds", "snow_pillows_map.rds", "x2_km_map.rds",
      "calsim3_nodes_map.rds", "major_conveyance_map.rds", "calsim3_arcs_map.rds",
      "deltamapr_canals_map.rds", "water_districts_map.rds", "rwqcb_regions_map.rds", "reference_layers_all_map.rds"
    ),
    geometry_family = c(
      "polygon", "polygon", "polygon", "polygon",
      rep("polygon", 6),
      "polygon", "polygon",
      "point", "point", "point", "point", "point", "point",
      "point", "point", "point", "point",
      "point", "line", "line", "line", "polygon", "polygon", "mixed"
    ),
    stringsAsFactors = FALSE
  )
}

pt_payload_layer_registry <- function() {
  if (exists("LOCAL_LAYER_REGISTRY")) {
    out <- LOCAL_LAYER_REGISTRY
    out <- out[!is.na(out$cache_file) & out$cache_file != "", , drop = FALSE]
    keep <- intersect(
      c("layer_id", "display_name", "category", "canonical_group", "cache_file", "geometry_family"),
      names(out)
    )
    out <- out[, keep, drop = FALSE]
    if (!"geometry_family" %in% names(out)) out$geometry_family <- NA_character_
    if (!"category" %in% names(out)) out$category <- NA_character_
    if (!"canonical_group" %in% names(out)) out$canonical_group <- NA_character_
    return(out)
  }

  pt_payload_fallback_registry()
}

# ==== 3. Cache reads and extraction ==========================================

pt_payload_read_cache <- function(cache_file) {
  path <- file.path(pt_payload_cache_dir(), cache_file)
  if (!file.exists(path)) {
    return(list(ok = FALSE, path = path, object = NULL, error = "missing cache file"))
  }

  out <- tryCatch(readRDS(path), error = function(e) e)
  if (inherits(out, "error")) {
    return(list(ok = FALSE, path = path, object = NULL, error = out$message))
  }

  list(ok = TRUE, path = path, object = out, error = NA_character_)
}

pt_payload_extract_layer <- function(cache_obj, layer_id, display_name) {
  if (inherits(cache_obj, "sf")) return(cache_obj)

  if (is.list(cache_obj)) {
    nms <- names(cache_obj)
    candidates <- unique(c(layer_id, display_name))
    candidates <- candidates[!is.na(candidates) & candidates != ""]
    hit <- candidates[candidates %in% nms]
    if (length(hit) > 0) return(cache_obj[[hit[1]]])

    key <- tolower(layer_id)
    if (key %in% nms) return(cache_obj[[key]])

    # Common static reference caches sometimes use display names that differ
    # only by case/punctuation.  Try a normalized match before giving up.
    normalize <- function(z) gsub("[^a-z0-9]+", "", tolower(z))
    norm_nms <- normalize(nms)
    norm_candidates <- normalize(candidates)
    idx <- match(norm_candidates, norm_nms)
    idx <- idx[!is.na(idx)]
    if (length(idx) > 0) return(cache_obj[[idx[1]]])

    return(cache_obj)
  }

  cache_obj
}

pt_payload_list_children <- function(cache_obj) {
  if (!is.list(cache_obj) || inherits(cache_obj, "sf")) {
    return(list())
  }

  nms <- names(cache_obj)
  if (is.null(nms)) nms <- paste0("child_", seq_along(cache_obj))

  out <- list()
  for (i in seq_along(cache_obj)) {
    child <- cache_obj[[i]]
    if (inherits(child, "sf")) {
      out[[nms[i]]] <- child
    }
  }
  out
}

# ==== 4. Payload estimation helpers ==========================================

pt_payload_geometry_wkt_bytes <- function(x) {
  if (!inherits(x, "sf")) return(NA_real_)
  geom <- tryCatch(sf::st_geometry(x), error = function(e) NULL)
  if (is.null(geom)) return(NA_real_)

  wkt <- tryCatch(sf::st_as_text(geom), error = function(e) NULL)
  if (is.null(wkt)) return(NA_real_)

  sum(nchar(wkt, type = "bytes"), na.rm = TRUE)
}

pt_payload_coordinate_count <- function(x) {
  if (!inherits(x, "sf")) return(NA_real_)
  coords <- tryCatch(sf::st_coordinates(x), error = function(e) NULL)
  if (is.null(coords)) return(NA_real_)
  nrow(coords)
}

pt_payload_column_bytes <- function(v) {
  if (inherits(v, "sfc")) return(NA_real_)

  out <- tryCatch({
    if (is.factor(v)) v <- as.character(v)
    if (inherits(v, c("POSIXct", "POSIXlt", "Date"))) v <- as.character(v)

    if (is.character(v)) {
      return(sum(nchar(v, type = "bytes"), na.rm = TRUE))
    }

    if (is.list(v) && !is.data.frame(v)) {
      vv <- vapply(
        v,
        function(z) {
          if (is.null(z) || length(z) == 0) return(0)
          zz <- tryCatch(paste(as.character(z), collapse = ";"), error = function(e) "")
          nchar(zz, type = "bytes")
        },
        numeric(1)
      )
      return(sum(vv, na.rm = TRUE))
    }

    0
  }, error = function(e) NA_real_)

  out
}

pt_payload_attr_json_bytes <- function(x) {
  if (!inherits(x, "sf")) return(NA_real_)
  attrs <- tryCatch(sf::st_drop_geometry(x), error = function(e) NULL)
  if (is.null(attrs)) return(NA_real_)

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(NA_real_)
  }

  txt <- tryCatch(
    jsonlite::toJSON(
      attrs,
      dataframe = "rows",
      na = "null",
      auto_unbox = TRUE,
      POSIXt = "ISO8601"
    ),
    error = function(e) NULL
  )

  if (is.null(txt)) return(NA_real_)
  nchar(as.character(txt), type = "bytes")
}

pt_payload_json_bytes_df <- function(attrs) {
  if (is.null(attrs)) return(NA_real_)
  if (!is.data.frame(attrs)) attrs <- as.data.frame(attrs, stringsAsFactors = FALSE)
  if (ncol(attrs) == 0) return(0)

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(NA_real_)
  }

  txt <- tryCatch(
    jsonlite::toJSON(
      attrs,
      dataframe = "rows",
      na = "null",
      auto_unbox = TRUE,
      POSIXt = "ISO8601"
    ),
    error = function(e) NULL
  )

  if (is.null(txt)) return(NA_real_)
  nchar(as.character(txt), type = "bytes")
}

pt_payload_existing_fields <- function(attrs, fields) {
  fields <- unique(fields)
  fields[fields %in% names(attrs)]
}

pt_payload_bool <- function(x, default = FALSE, n = NULL) {
  if (is.null(x)) {
    if (is.null(n)) return(logical(0))
    return(rep(default, n))
  }
  out <- tryCatch(as.logical(x), error = function(e) rep(default, length(x)))
  if (length(out) == 0 && !is.null(n)) out <- rep(default, n)
  out[is.na(out)] <- default
  out
}

pt_payload_safe_vector <- function(attrs, field, default) {
  n <- nrow(attrs)
  if (field %in% names(attrs)) return(attrs[[field]])
  rep(default, n)
}

pt_payload_likely_leaflet_payload <- function(x, layer_id) {
  empty <- list(
    attr_bytes = NA_real_,
    attr_mb = NA_real_,
    fields = NA_character_,
    method = NA_character_,
    note = NA_character_
  )

  if (!inherits(x, "sf")) return(empty)
  attrs <- tryCatch(sf::st_drop_geometry(x), error = function(e) NULL)
  if (is.null(attrs)) return(empty)

  n <- nrow(attrs)
  lid <- tolower(as.character(layer_id %||% ""))

  ## Exact-ish Patch 007 draw subset for static USGS Wells.
  ## The marker layer carries only style/hover/site_no fields.  Popup content is
  ## passed once as compact gwpop_* fields and rendered by a shared browser-side
  ## JavaScript template on click.  This is a better estimate of actual HTML
  ## payload than counting the old repeated popup_html field.
  if (lid == "usgs_wells" || grepl("(^|::)usgs_wells($|::)", lid)) {
    recent <- if ("well_recent_feed_ring" %in% names(attrs)) {
      pt_payload_bool(attrs$well_recent_feed_ring, FALSE, n)
    } else if ("well_is_active" %in% names(attrs)) {
      pt_payload_bool(attrs$well_is_active, FALSE, n)
    } else {
      rep(FALSE, n)
    }

    nested <- if ("well_is_nested" %in% names(attrs)) {
      pt_payload_bool(attrs$well_is_nested, FALSE, n)
    } else {
      rep(FALSE, n)
    }

    well_stroke_col <- as.character(pt_payload_safe_vector(attrs, "well_stroke_col", "#4D4D4D"))

    marker_slim <- data.frame(
      site_no = pt_payload_safe_vector(attrs, "site_no", NA_character_),
      well_radius = pt_payload_safe_vector(attrs, "well_radius", 3),
      well_stroke_col_display = ifelse(recent, "#39FF14", well_stroke_col),
      well_stroke_weight_display = ifelse(recent, 2.2, 1.0),
      well_dash_array_display = ifelse(nested, "2,2", NA_character_),
      well_fill_col = pt_payload_safe_vector(attrs, "well_fill_col", "#BDBDBD"),
      hover_text = pt_payload_safe_vector(attrs, "hover_text", NA_character_),
      stringsAsFactors = FALSE
    )

    gwpop_fields <- grep("^gwpop_", names(attrs), value = TRUE)

    if (length(gwpop_fields) > 0) {
      popup_slim <- attrs[c("site_no", gwpop_fields[gwpop_fields %in% names(attrs)])]
      slim <- cbind(marker_slim, popup_slim[setdiff(names(popup_slim), names(marker_slim))])
      method <- "usgs_wells_patch007_template_popup_fields"
      note <- "Marker draw fields plus compact gwpop_* fields used by browser-template popup builder."
    } else {
      slim <- cbind(
        marker_slim,
        popup_html = pt_payload_safe_vector(attrs, "popup_html", "<b>USGS well</b>")
      )
      method <- "usgs_wells_legacy_popup_fallback"
      note <- "No gwpop_* fields found; estimates legacy prebuilt popup_html fallback."
    }

    bytes <- pt_payload_json_bytes_df(slim)
    return(list(
      attr_bytes = bytes,
      attr_mb = pt_payload_mb(bytes),
      fields = paste(names(slim), collapse = ";"),
      method = method,
      note = note
    ))
  }

  ## USGS streamgages now use compact usgsswpop_* popup-template fields when
  ## the core cache has been rebuilt. Older caches fall back to popup_html.
  if (lid == "usgs_streamgages" || grepl("(^|::)usgs_streamgages($|::)", lid)) {
    marker_slim <- data.frame(
      site_no = pt_payload_safe_vector(attrs, "site_no", NA_character_),
      fill_col = pt_payload_safe_vector(attrs, "fill_col", "#8C8C8C"),
      stroke_col = pt_payload_safe_vector(attrs, "stroke_col", "#4D4D4D"),
      hover_text = pt_payload_safe_vector(attrs, "hover_text", NA_character_),
      stringsAsFactors = FALSE
    )

    usgsswpop_fields <- grep("^usgsswpop_", names(attrs), value = TRUE)

    if (length(usgsswpop_fields) > 0) {
      popup_slim <- attrs[c("site_no", usgsswpop_fields[usgsswpop_fields %in% names(attrs)])]
      slim <- cbind(marker_slim, popup_slim[setdiff(names(popup_slim), names(marker_slim))])
      method <- "usgs_streamgages_template_popup_fields"
      note <- "Marker draw fields plus compact usgsswpop_* fields used by browser-template popup builder."
    } else {
      slim <- cbind(
        marker_slim,
        popup_html = pt_payload_safe_vector(attrs, "popup_html", "<b>USGS streamgage</b>")
      )
      method <- "usgs_streamgages_legacy_popup_fallback"
      note <- "No usgsswpop_* fields found; estimates legacy prebuilt popup_html fallback."
    }

    bytes <- pt_payload_json_bytes_df(slim)
    return(list(
      attr_bytes = bytes,
      attr_mb = pt_payload_mb(bytes),
      fields = paste(names(slim), collapse = ";"),
      method = method,
      note = note
    ))
  }

  ## SWRCB POD/WR layers now use compact swrcbpop_* popup-template fields.
  if (grepl("swrcb", lid) && any(grepl("^swrcbpop_", names(attrs)))) {
    marker_slim <- data.frame(
      pt_swrcb_layer_id = pt_payload_safe_vector(attrs, "pt_swrcb_layer_id", NA_character_),
      pt_swrcb_radius = pt_payload_safe_vector(attrs, "swrcb_radius", 3.5),
      pt_swrcb_ring_col = pt_payload_safe_vector(attrs, "swrcb_stroke_col", "#4D4D4D"),
      pt_swrcb_fill_col = pt_payload_safe_vector(attrs, "swrcb_fill_col", "#8C8C8C"),
      hover_text = pt_payload_safe_vector(attrs, "hover_text", NA_character_),
      stringsAsFactors = FALSE
    )

    swrcbpop_fields <- grep("^swrcbpop_", names(attrs), value = TRUE)
    popup_slim <- attrs[c("pt_swrcb_layer_id", swrcbpop_fields[swrcbpop_fields %in% names(attrs)])]
    slim <- cbind(marker_slim, popup_slim[setdiff(names(popup_slim), names(marker_slim))])

    bytes <- pt_payload_json_bytes_df(slim)
    return(list(
      attr_bytes = bytes,
      attr_mb = pt_payload_mb(bytes),
      fields = paste(names(slim), collapse = ";"),
      method = "swrcb_template_popup_fields",
      note = "Marker draw fields plus compact swrcbpop_* fields used by browser-template popup builder."
    ))
  }

  ## Generic estimate for other layers: popup/hover/tooltip/label and explicit
  ## style columns are the fields most likely to travel into Leaflet after
  ## formula evaluation. This intentionally ignores source/join fields when a
  ## popup_html column already exists.
  nms <- names(attrs)
  popup_fields <- pt_payload_existing_fields(attrs, c("popup_html", "popup", "popupContent"))
  hover_fields <- grep("hover|tooltip", nms, ignore.case = TRUE, value = TRUE)

  ## Include label fields, but avoid huge descriptive source fields whose names
  ## merely contain labels in metadata-like contexts.
  label_fields <- grep("(^label$|_label$|label_text|label_html)", nms, ignore.case = TRUE, value = TRUE)

  style_fields <- pt_payload_existing_fields(attrs, c(
    "fillColor", "fill_color", "fill_col", "fill",
    "color", "strokeColor", "stroke_color", "stroke_col",
    "radius", "weight", "opacity", "fillOpacity", "dashArray", "dash_array"
  ))

  likely_fields <- unique(c(popup_fields, hover_fields, label_fields, style_fields))

  if (length(likely_fields) == 0) {
    return(list(
      attr_bytes = 0,
      attr_mb = 0,
      fields = "",
      method = "generic_geometry_only",
      note = "No obvious popup/hover/style fields found; likely estimate is geometry only."
    ))
  }

  slim <- attrs[likely_fields]
  bytes <- pt_payload_json_bytes_df(slim)
  list(
    attr_bytes = bytes,
    attr_mb = pt_payload_mb(bytes),
    fields = paste(likely_fields, collapse = ";"),
    method = "generic_popup_hover_style_subset",
    note = "Generic estimate using popup/hover/tooltip/label/style fields only."
  )
}

pt_payload_attribute_char_bytes <- function(x) {
  if (!inherits(x, "sf")) return(NA_real_)
  attrs <- tryCatch(sf::st_drop_geometry(x), error = function(e) NULL)
  if (is.null(attrs)) return(NA_real_)

  vals <- vapply(attrs, pt_payload_column_bytes, numeric(1))
  sum(vals, na.rm = TRUE)
}

pt_payload_field_bytes <- function(x, field_pattern = NULL, field_names = NULL) {
  if (!inherits(x, "sf")) return(NA_real_)
  attrs <- tryCatch(sf::st_drop_geometry(x), error = function(e) NULL)
  if (is.null(attrs) || ncol(attrs) == 0) return(0)

  nms <- names(attrs)
  fields <- character(0)

  if (!is.null(field_names)) {
    fields <- c(fields, intersect(field_names, nms))
  }
  if (!is.null(field_pattern)) {
    fields <- c(fields, grep(field_pattern, nms, ignore.case = TRUE, value = TRUE))
  }

  fields <- unique(fields)
  if (length(fields) == 0) return(0)

  vals <- vapply(attrs[fields], pt_payload_column_bytes, numeric(1))
  sum(vals, na.rm = TRUE)
}

pt_payload_column_summary <- function(x, layer_id, display_name, cache_file, source_kind) {
  empty <- data.frame(
    layer_id = character(0),
    display_name = character(0),
    cache_file = character(0),
    source_kind = character(0),
    column_name = character(0),
    column_class = character(0),
    object_size_mb = numeric(0),
    text_bytes = numeric(0),
    text_mb = numeric(0),
    blank_fraction = numeric(0),
    stringsAsFactors = FALSE
  )

  if (!inherits(x, "sf")) return(empty)
  attrs <- tryCatch(sf::st_drop_geometry(x), error = function(e) NULL)
  if (is.null(attrs) || ncol(attrs) == 0) return(empty)

  rows <- lapply(names(attrs), function(nm) {
    v <- attrs[[nm]]
    bytes <- pt_payload_column_bytes(v)
    data.frame(
      layer_id = layer_id,
      display_name = display_name,
      cache_file = cache_file,
      source_kind = source_kind,
      column_name = nm,
      column_class = paste(class(v), collapse = ";"),
      object_size_mb = pt_payload_object_size_mb(v),
      text_bytes = bytes,
      text_mb = pt_payload_mb(bytes),
      blank_fraction = pt_payload_blank_fraction(v),
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(rows)
}

pt_payload_one_sf <- function(
    x,
    layer_id,
    display_name,
    cache_file,
    cache_path,
    cache_read_ok,
    read_error,
    source_kind,
    category = NA_character_,
    canonical_group = NA_character_,
    geometry_family = NA_character_
) {

  base <- data.frame(
    layer_id = layer_id,
    display_name = display_name,
    category = category %||% NA_character_,
    canonical_group = canonical_group %||% NA_character_,
    cache_file = cache_file,
    cache_path = cache_path,
    cache_exists = file.exists(cache_path),
    cache_size_mb = pt_payload_file_size_mb(cache_path),
    cache_read_ok = isTRUE(cache_read_ok),
    read_error = read_error %||% NA_character_,
    source_kind = source_kind,
    object_class = NA_character_,
    is_sf = FALSE,
    is_list_cache = FALSE,
    list_names = NA_character_,
    n_features = NA_integer_,
    n_columns = NA_integer_,
    geometry_family_expected = geometry_family %||% NA_character_,
    geometry_types = NA_character_,
    crs_epsg = NA_character_,
    layer_object_size_mb = NA_real_,
    n_coordinates = NA_real_,
    geometry_wkt_mb = NA_real_,
    attributes_json_mb = NA_real_,
    attributes_character_mb = NA_real_,
    popup_html_mb = NA_real_,
    hover_tooltip_mb = NA_real_,
    label_candidate_mb = NA_real_,
    style_candidate_mb = NA_real_,
    estimated_text_payload_mb = NA_real_,
    full_cache_text_payload_mb = NA_real_,
    likely_leaflet_attr_json_mb = NA_real_,
    likely_leaflet_text_payload_mb = NA_real_,
    full_minus_likely_mb = NA_real_,
    likely_leaflet_fields = NA_character_,
    likely_leaflet_method = NA_character_,
    likely_leaflet_note = NA_character_,
    geom_mb_per_1000_features = NA_real_,
    attr_json_mb_per_1000_features = NA_real_,
    popup_mb_per_1000_features = NA_real_,
    payload_note = NA_character_,
    stringsAsFactors = FALSE
  )

  base$object_class <- paste(class(x), collapse = ";")
  base$is_list_cache <- is.list(x) && !inherits(x, "sf")
  base$list_names <- if (base$is_list_cache) pt_payload_chr(pt_payload_safe_names(x)) else NA_character_

  if (!cache_read_ok) {
    base$payload_note <- "cache missing or unreadable"
    return(base)
  }

  if (base$is_list_cache) {
    base$payload_note <- "list cache not expanded by registry row; see list_child rows if present"
    return(base)
  }

  base$is_sf <- inherits(x, "sf")
  if (!base$is_sf) {
    base$payload_note <- "object is not sf"
    return(base)
  }

  base$n_features <- tryCatch(nrow(x), error = function(e) NA_integer_)
  base$n_columns <- tryCatch(ncol(x), error = function(e) NA_integer_)
  base$layer_object_size_mb <- pt_payload_object_size_mb(x)

  geom_types <- tryCatch(
    unique(as.character(sf::st_geometry_type(x, by_geometry = TRUE))),
    error = function(e) NA_character_
  )
  base$geometry_types <- pt_payload_chr(geom_types)
  base$crs_epsg <- as.character(sf::st_crs(x)$epsg %||% NA_character_)

  n_coord <- pt_payload_coordinate_count(x)
  geom_bytes <- pt_payload_geometry_wkt_bytes(x)
  attr_json_bytes <- pt_payload_attr_json_bytes(x)
  attr_char_bytes <- pt_payload_attribute_char_bytes(x)
  popup_bytes <- pt_payload_field_bytes(x, field_names = c("popup_html", "popup", "popupContent"))
  hover_bytes <- pt_payload_field_bytes(x, field_pattern = "hover|tooltip")
  label_bytes <- pt_payload_field_bytes(x, field_pattern = "label")
  style_bytes <- pt_payload_field_bytes(
    x,
    field_names = c("fillColor", "fill_color", "color", "strokeColor", "radius", "weight", "opacity", "fillOpacity")
  )

  likely <- pt_payload_likely_leaflet_payload(x, layer_id)

  base$n_coordinates <- n_coord
  base$geometry_wkt_mb <- pt_payload_mb(geom_bytes)
  base$attributes_json_mb <- pt_payload_mb(attr_json_bytes)
  base$attributes_character_mb <- pt_payload_mb(attr_char_bytes)
  base$popup_html_mb <- pt_payload_mb(popup_bytes)
  base$hover_tooltip_mb <- pt_payload_mb(hover_bytes)
  base$label_candidate_mb <- pt_payload_mb(label_bytes)
  base$style_candidate_mb <- pt_payload_mb(style_bytes)

  if (!is.na(attr_json_bytes)) {
    base$estimated_text_payload_mb <- pt_payload_mb(geom_bytes + attr_json_bytes)
  } else {
    # Fallback if jsonlite is unavailable or JSON serialization failed.
    # This underestimates because it omits JSON punctuation and non-character
    # attribute rendering, but still helps identify popup/string bloat.
    base$estimated_text_payload_mb <- pt_payload_mb(geom_bytes + attr_char_bytes)
  }

  ## Keep the old column name for backward compatibility, but also expose a
  ## clearer name so users do not mistake the full-cache estimate for the exact
  ## final Leaflet/HTML contribution.
  base$full_cache_text_payload_mb <- base$estimated_text_payload_mb
  base$likely_leaflet_attr_json_mb <- likely$attr_mb
  base$likely_leaflet_text_payload_mb <- pt_payload_mb(geom_bytes + likely$attr_bytes)
  base$likely_leaflet_fields <- likely$fields
  base$likely_leaflet_method <- likely$method
  base$likely_leaflet_note <- likely$note
  base$full_minus_likely_mb <- round(
    base$full_cache_text_payload_mb - base$likely_leaflet_text_payload_mb,
    3
  )

  if (!is.na(base$n_features) && base$n_features > 0) {
    base$geom_mb_per_1000_features <- round(base$geometry_wkt_mb / base$n_features * 1000, 3)
    base$attr_json_mb_per_1000_features <- round(base$attributes_json_mb / base$n_features * 1000, 3)
    base$popup_mb_per_1000_features <- round(base$popup_html_mb / base$n_features * 1000, 3)
  }

  if (is.na(attr_json_bytes)) {
    base$payload_note <- "attributes_json_mb unavailable; full-cache estimate uses character-only fallback"
  } else {
    base$payload_note <- "OK; full-cache uses WKT geometry proxy plus all JSON attributes; likely estimate uses Leaflet draw subset"
  }

  base
}

# ==== 5. HTML output inventory ===============================================

pt_payload_html_outputs <- function() {
  html_dirs <- c("06_output/html", "06_output", ".")
  files <- character(0)
  for (d in html_dirs) {
    if (dir.exists(d)) {
      files <- c(files, list.files(d, pattern = "\\.html$", full.names = TRUE, recursive = d != "."))
    }
  }
  files <- unique(normalizePath(files, winslash = "/", mustWork = FALSE))
  files <- files[file.exists(files)]

  if (length(files) == 0) {
    return(data.frame(
      html_file = character(0),
      file_size_mb = numeric(0),
      modified_time = as.POSIXct(character(0)),
      stringsAsFactors = FALSE
    ))
  }

  info <- file.info(files)
  out <- data.frame(
    html_file = files,
    file_size_mb = round(info$size / 1024^2, 3),
    modified_time = info$mtime,
    stringsAsFactors = FALSE
  )
  out[order(out$modified_time, decreasing = TRUE), , drop = FALSE]
}

# ==== 6. Main runner ==========================================================

pt_qa_local_layer_payload_size_check <- function() {
  ts <- pt_payload_timestamp()
  dir.create("qa", showWarnings = FALSE, recursive = TRUE)

  registry <- pt_payload_layer_registry()
  registry <- registry[!is.na(registry$cache_file) & registry$cache_file != "", , drop = FALSE]
  registry <- registry[!duplicated(registry[, c("layer_id", "cache_file")]), , drop = FALSE]

  cache_files <- unique(registry$cache_file)
  cache_by_file <- lapply(cache_files, pt_payload_read_cache)
  names(cache_by_file) <- cache_files

  layer_rows <- list()
  column_rows <- list()

  # ---- Registry rows --------------------------------------------------------
  for (i in seq_len(nrow(registry))) {
    rr <- registry[i, , drop = FALSE]
    cache_file <- as.character(rr$cache_file)
    cr <- cache_by_file[[cache_file]]
    x <- if (isTRUE(cr$ok)) {
      pt_payload_extract_layer(cr$object, rr$layer_id, rr$display_name)
    } else {
      NULL
    }

    layer_key <- paste0("registry_", i)
    layer_rows[[layer_key]] <- pt_payload_one_sf(
      x = x,
      layer_id = as.character(rr$layer_id),
      display_name = as.character(rr$display_name %||% rr$layer_id),
      cache_file = cache_file,
      cache_path = cr$path,
      cache_read_ok = cr$ok,
      read_error = cr$error,
      source_kind = "registry_row",
      category = rr$category %||% NA_character_,
      canonical_group = rr$canonical_group %||% NA_character_,
      geometry_family = rr$geometry_family %||% NA_character_
    )

    if (isTRUE(cr$ok) && inherits(x, "sf")) {
      column_rows[[layer_key]] <- pt_payload_column_summary(
        x = x,
        layer_id = as.character(rr$layer_id),
        display_name = as.character(rr$display_name %||% rr$layer_id),
        cache_file = cache_file,
        source_kind = "registry_row"
      )
    }
  }

  # ---- Named sf children in list caches ------------------------------------
  # This is especially useful for reference_layers_all_map.rds, where the cache
  # can contain many source-specific sf layers that do not map one-to-one to the
  # registry rows.
  child_i <- 0L
  for (cache_file in cache_files) {
    cr <- cache_by_file[[cache_file]]
    if (!isTRUE(cr$ok)) next
    children <- pt_payload_list_children(cr$object)
    if (length(children) == 0) next

    for (child_name in names(children)) {
      child_i <- child_i + 1L
      layer_id <- paste0(tools::file_path_sans_ext(cache_file), "::", child_name)
      display_name <- paste0(child_name, " [cache child]")
      key <- paste0("list_child_", child_i)

      layer_rows[[key]] <- pt_payload_one_sf(
        x = children[[child_name]],
        layer_id = layer_id,
        display_name = display_name,
        cache_file = cache_file,
        cache_path = cr$path,
        cache_read_ok = cr$ok,
        read_error = cr$error,
        source_kind = "list_child",
        category = NA_character_,
        canonical_group = NA_character_,
        geometry_family = NA_character_
      )

      column_rows[[key]] <- pt_payload_column_summary(
        x = children[[child_name]],
        layer_id = layer_id,
        display_name = display_name,
        cache_file = cache_file,
        source_kind = "list_child"
      )
    }
  }

  layer_df <- dplyr::bind_rows(layer_rows)
  column_df <- dplyr::bind_rows(column_rows)

  # Avoid double-counting list-child and registry rows in cache-file totals by
  # reporting both all rows and a list-child-preferred estimate.
  file_df <- layer_df %>%
    dplyr::group_by(cache_file, cache_path) %>%
    dplyr::summarise(
      cache_exists = any(cache_exists),
      cache_size_mb = dplyr::first(cache_size_mb),
      registry_rows = sum(source_kind == "registry_row"),
      list_child_rows = sum(source_kind == "list_child"),
      sf_rows = sum(is_sf),
      total_features_all_rows = sum(n_features, na.rm = TRUE),
      max_layer_payload_mb = max(estimated_text_payload_mb, na.rm = TRUE),
      max_likely_leaflet_payload_mb = max(likely_leaflet_text_payload_mb, na.rm = TRUE),
      sum_payload_registry_rows_mb = sum(estimated_text_payload_mb[source_kind == "registry_row"], na.rm = TRUE),
      sum_payload_list_child_rows_mb = sum(estimated_text_payload_mb[source_kind == "list_child"], na.rm = TRUE),
      sum_likely_registry_rows_mb = sum(likely_leaflet_text_payload_mb[source_kind == "registry_row"], na.rm = TRUE),
      sum_likely_list_child_rows_mb = sum(likely_leaflet_text_payload_mb[source_kind == "list_child"], na.rm = TRUE),
      sum_popup_registry_rows_mb = sum(popup_html_mb[source_kind == "registry_row"], na.rm = TRUE),
      sum_popup_list_child_rows_mb = sum(popup_html_mb[source_kind == "list_child"], na.rm = TRUE),
      largest_layer_id = layer_id[which.max(replace(likely_leaflet_text_payload_mb, is.na(likely_leaflet_text_payload_mb), -Inf))][1],
      largest_display_name = display_name[which.max(replace(likely_leaflet_text_payload_mb, is.na(likely_leaflet_text_payload_mb), -Inf))][1],
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      preferred_sum_payload_mb = dplyr::if_else(
        list_child_rows > 0,
        sum_payload_list_child_rows_mb,
        sum_payload_registry_rows_mb
      ),
      preferred_sum_likely_leaflet_mb = dplyr::if_else(
        list_child_rows > 0,
        sum_likely_list_child_rows_mb,
        sum_likely_registry_rows_mb
      ),
      preferred_sum_popup_mb = dplyr::if_else(
        list_child_rows > 0,
        sum_popup_list_child_rows_mb,
        sum_popup_registry_rows_mb
      ),
      full_minus_likely_mb = round(preferred_sum_payload_mb - preferred_sum_likely_leaflet_mb, 3)
    ) %>%
    dplyr::arrange(dplyr::desc(preferred_sum_likely_leaflet_mb))

  # max(..., na.rm=TRUE) returns -Inf when all are NA; clean that up for CSV.
  file_df$max_layer_payload_mb[is.infinite(file_df$max_layer_payload_mb)] <- NA_real_
  file_df$max_likely_leaflet_payload_mb[is.infinite(file_df$max_likely_leaflet_payload_mb)] <- NA_real_

  html_df <- pt_payload_html_outputs()

  layer_path <- file.path("qa", paste0("local_layer_payload_size_by_layer_", ts, ".csv"))
  file_path <- file.path("qa", paste0("local_layer_payload_size_by_cache_file_", ts, ".csv"))
  column_path <- file.path("qa", paste0("local_layer_payload_column_bytes_", ts, ".csv"))
  html_path <- file.path("qa", paste0("html_output_file_sizes_", ts, ".csv"))

  utils::write.csv(layer_df[order(-layer_df$likely_leaflet_text_payload_mb), , drop = FALSE], layer_path, row.names = FALSE, na = "")
  utils::write.csv(file_df, file_path, row.names = FALSE, na = "")
  utils::write.csv(column_df[order(-column_df$text_mb), , drop = FALSE], column_path, row.names = FALSE, na = "")
  utils::write.csv(html_df, html_path, row.names = FALSE, na = "")

  message("Local layer payload-size QA complete.")
  message("  By layer:    ", layer_path)
  message("  By cache:    ", file_path)
  message("  By column:   ", column_path)
  message("  HTML files:  ", html_path)

  if (nrow(html_df) > 0) {
    message("  Newest HTML output: ", basename(html_df$html_file[1]), " = ", html_df$file_size_mb[1], " MB")
  }

  if (nrow(file_df) > 0) {
    message("\nTop likely Leaflet cache-file payloads:")
    top_file <- utils::head(file_df, 10)
    for (i in seq_len(nrow(top_file))) {
      message(
        "  ", i, ". ", top_file$cache_file[i],
        " | likely ", top_file$preferred_sum_likely_leaflet_mb[i], " MB",
        " | full-cache ", top_file$preferred_sum_payload_mb[i], " MB",
        " | RDS ", top_file$cache_size_mb[i], " MB",
        " | largest layer: ", top_file$largest_layer_id[i]
      )
    }
  }

  if (nrow(layer_df) > 0) {
    layer_df_sorted <- layer_df[order(-layer_df$likely_leaflet_text_payload_mb), , drop = FALSE]
    message("\nTop likely Leaflet layer payloads:")
    top_layer <- utils::head(layer_df_sorted, 12)
    for (i in seq_len(nrow(top_layer))) {
      message(
        "  ", i, ". ", top_layer$layer_id[i],
        " | likely ", top_layer$likely_leaflet_text_payload_mb[i], " MB",
        " | full-cache ", top_layer$full_cache_text_payload_mb[i], " MB",
        " | geom ", top_layer$geometry_wkt_mb[i], " MB",
        " | popup ", top_layer$popup_html_mb[i], " MB",
        " | n=", top_layer$n_features[i]
      )
    }
  }

  invisible(list(
    by_layer = layer_df,
    by_cache_file = file_df,
    by_column = column_df,
    html_files = html_df,
    layer_path = layer_path,
    file_path = file_path,
    column_path = column_path,
    html_path = html_path
  ))
}

# Run when sourced by run_build_map.r/run_step or directly from RStudio.
qa_local_layer_payload_size_check_results <- pt_qa_local_layer_payload_size_check()
