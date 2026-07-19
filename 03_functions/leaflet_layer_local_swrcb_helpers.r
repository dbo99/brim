# ==== leaflet_layer_local_swrcb_helpers.r ================================================
##
## PURPOSE:
##   SWRCB/CalWATRS water-right POD local layers, filters, labels, and shared legend.
##
## NOTE:
##   Extracted from leaflet_layer_helpers.r as a maintainability-only split.
##   Function names and behavior are intentionally unchanged.

# ==== 4.3 SWRCB / CalWATRS water-right POD layers =============================
##
## DESIGN:
##   The SWRCB / CalWATRS BLM-relevant point cache is split into three mutually
##   exclusive Leaflet layers by screening/provenance mechanism:
##
##     1. SWRCB 2026 BLM WR list records
##        - swrcb_2026_blm_wr_list == TRUE
##
##     2. SWRCB additional PODs spatially matched to BLM
##        - swrcb_2026_blm_wr_list == FALSE
##        - blm_include_reason_display is Spatial + name match or Spatial only
##
##     3. SWRCB additional BLM name/text-match candidates
##        - swrcb_2026_blm_wr_list == FALSE
##        - blm_include_reason_display is Name match only
##
## SYMBOLOGY:
##   - Point radius = binned face value / AFY, biased toward common BLM-scale
##     values and capped at a top >1,000 AFY bin so very large State/project
##     records do not flatten the rest of the symbol scale.
##   - Fill color = provenance / layer.
##       SWRCB 2026 BLM WR list = blue
##       additional spatial match = gold/tan
##       additional name/text candidate = purple
##   - Stroke/ring color = WR status.
##       active/recognized = green
##       pending = orange
##       inactive/cancelled = red
##       unknown = gray
##
## WHY:
##   Field-office staff may need to see exactly which records SWRCB provided to
##   BLM for reporting/review.  Those official-list records should be visible as
##   their own layer, separate from BRIM's additional spatial and name/text
##   screening candidates.


## Helper: join monthly/refreshed SWRCB POD distance-to-BLM cache.
##
## The expensive spatial work is done by:
##   02_preprocess/61_update_swrcb_pod_blm_distance_fields.R
##
## Final map builds should only join the compact CSV sidecar.  Keep this helper
## defensive so the SWRCB layers still draw when the distance cache has not been
## generated yet.
pt_swrcb_pod_boolish <- function(v) {
  if (is.logical(v)) return(v)
  tolower(trimws(as.character(v))) %in% c("true", "t", "1", "yes", "y")
}

pt_join_swrcb_pod_blm_distance_fields <- function(x) {
  if (is.null(x) || nrow(x) == 0) return(x)

  if (!"pt_swrcb_layer_id" %in% names(x)) {
    x$pt_swrcb_layer_id <- paste0("swrcb_pod_wr_", seq_len(nrow(x)))
  }

  if (!"on_blm_ca" %in% names(x)) x$on_blm_ca <- NA
  if (!"dist_to_blm_mi" %in% names(x)) x$dist_to_blm_mi <- NA_real_
  if (!"dist_to_blm_ft" %in% names(x)) x$dist_to_blm_ft <- NA_real_

  cache_candidates <- c(
    Sys.getenv("SWRCB_POD_BLM_DISTANCE_CSV", unset = ""),
    file.path("04_processed_data", "cache", "latest", "swrcb_pod_blm_distance_fields.csv")
  )

  cache_path <- cache_candidates[file.exists(cache_candidates) & nzchar(cache_candidates)]
  cache_path <- if (length(cache_path) > 0) cache_path[[1]] else NA_character_

  if (is.na(cache_path) || !nzchar(cache_path)) {
    message(
      "SWRCB POD BLM-distance cache not found; SWRCB BLM filters will be unavailable. ",
      "Run 61_update_swrcb_pod_blm_distance_fields.R after refreshing BLM lands or SWRCB POD data."
    )
    return(x)
  }

  dist <- tryCatch(
    utils::read.csv(cache_path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) {
      warning("Could not read SWRCB POD BLM-distance cache: ", conditionMessage(e))
      NULL
    }
  )

  needed <- c("pt_swrcb_layer_id", "on_blm_ca", "dist_to_blm_mi", "dist_to_blm_ft")
  if (is.null(dist) || !all(needed %in% names(dist))) {
    message(
      "SWRCB POD BLM-distance cache lacks required fields; SWRCB BLM filters will be unavailable. ",
      "Re-run 61_update_swrcb_pod_blm_distance_fields.R."
    )
    return(x)
  }

  dist2 <- dist[, needed, drop = FALSE]
  dist2$pt_swrcb_layer_id <- as.character(dist2$pt_swrcb_layer_id)
  dist2 <- dist2[!is.na(dist2$pt_swrcb_layer_id) & nzchar(dist2$pt_swrcb_layer_id), , drop = FALSE]
  dist2 <- dist2[!duplicated(dist2$pt_swrcb_layer_id), , drop = FALSE]
  dist2 <- dist2 |>
    dplyr::transmute(
      pt_swrcb_layer_id = .data$pt_swrcb_layer_id,
      on_blm_ca_from_cache = pt_swrcb_pod_boolish(.data$on_blm_ca),
      dist_to_blm_mi_from_cache = suppressWarnings(as.numeric(.data$dist_to_blm_mi)),
      dist_to_blm_ft_from_cache = suppressWarnings(as.numeric(.data$dist_to_blm_ft))
    )

  x$pt_swrcb_layer_id <- as.character(x$pt_swrcb_layer_id)
  out <- dplyr::left_join(x, dist2, by = "pt_swrcb_layer_id")

  out$on_blm_ca <- dplyr::coalesce(
    pt_swrcb_pod_boolish(out$on_blm_ca),
    out$on_blm_ca_from_cache
  )
  out$dist_to_blm_mi <- dplyr::coalesce(
    suppressWarnings(as.numeric(out$dist_to_blm_mi)),
    out$dist_to_blm_mi_from_cache
  )
  out$dist_to_blm_ft <- dplyr::coalesce(
    suppressWarnings(as.numeric(out$dist_to_blm_ft)),
    out$dist_to_blm_ft_from_cache
  )

  out$on_blm_ca_from_cache <- NULL
  out$dist_to_blm_mi_from_cache <- NULL
  out$dist_to_blm_ft_from_cache <- NULL

  message(
    "SWRCB POD BLM-distance cache joined: ",
    format(sum(!is.na(out$dist_to_blm_mi)), big.mark = ","),
    " of ", format(nrow(out), big.mark = ","), " POD records have distance-to-BLM values; ",
    format(sum(out$on_blm_ca %in% TRUE, na.rm = TRUE), big.mark = ","),
    " are on BLM. Source: ", cache_path
  )

  out
}

pt_add_swrcb_pod_wr_layer <- function(m, swrcb_pod_wr_blm, map_display) {

  if (!isTRUE(map_display$add_swrcb_pod_wr_blm)) {
    return(m)
  }

  if (!inherits(swrcb_pod_wr_blm, "sf") || nrow(swrcb_pod_wr_blm) == 0) {
    message("SWRCB POD water-right layer is empty; no points added.")
    return(m)
  }

  official_group_name <- pt_layer_group_name("SWRCB 2026 BLM WR list records")
  spatial_group_name <- pt_layer_group_name("SWRCB additional PODs spatially matched to BLM")
  name_candidate_group_name <- pt_layer_group_name("SWRCB additional BLM name/text-match candidates")

  ## Patch 045e:
  ##   Draw the three SWRCB / CalWATRS POD source layers with the same
  ##   browser-managed local-point scaffold used by the fast CNRFC catalog
  ##   layers, while keeping the three separate layer-control rows for clear
  ##   provenance.  The previous native CircleMarker path is deliberately
  ##   replaced here so future legend filters can rebuild only this layer's own
  ##   marker groups without touching Leaflet's global layer-control internals.
  ##
  ##   045f adds compact status/AFY filters using this same browser-owned
  ##   source-group scaffold; filters rebuild only active SWRCB groups and do
  ##   not touch global Leaflet layer-control internals.
  ##
  ##   045h adds a BLM-distance filter using the monthly/refreshed distance
  ##   cache from 61_update_swrcb_pod_blm_distance_fields.R.  Keep this as a
  ##   normal field-level filter inside the same source-group controller so it
  ##   remains maintainable and avoids the fragile native-marker style-walking
  ##   path that was abandoned in 045c/045d.
  ##
  ##   Stability remains the first requirement:
  ##   - three source rows still toggle independently;
  ##   - existing shared legend still works;
  ##   - compact hover remains four rows;
  ##   - detailed popup is preserved;
  ##   - Clear Local / Clear All should work through checkbox state changes.
  x <- swrcb_pod_wr_blm
  x <- pt_join_swrcb_pod_blm_distance_fields(x)

  ## Fallbacks keep the map build robust if the core cache was not rebuilt.
  if (!"swrcb_status_group" %in% names(x)) {
    x$swrcb_status_group <- "Unknown"
  }

  if (!"blm_include_reason_display" %in% names(x)) {
    x$blm_include_reason_display <- "Unknown"
  }

  if (!"swrcb_2026_blm_wr_list" %in% names(x)) {
    x$swrcb_2026_blm_wr_list <- FALSE
  }

  if (!"hover_text" %in% names(x)) {
    x$hover_text <- "SWRCB POD"
  }

  reason <- trimws(as.character(x$blm_include_reason_display))
  status <- trimws(as.character(x$swrcb_status_group))

  swrcb_list_flag <- x$swrcb_2026_blm_wr_list
  swrcb_list_flag[is.na(swrcb_list_flag)] <- FALSE

  x$pt_swrcb_is_official_list <- isTRUE(swrcb_list_flag) | swrcb_list_flag

  x$pt_swrcb_is_additional_spatial <- 
    !x$pt_swrcb_is_official_list &
    reason %in% c("Spatial + name match", "Spatial only")

  x$pt_swrcb_is_additional_name_candidate <- 
    !x$pt_swrcb_is_official_list &
    reason %in% c("Name match only")

  x$pt_swrcb_source_key <- dplyr::case_when(
    x$pt_swrcb_is_official_list              ~ "official",
    x$pt_swrcb_is_additional_spatial        ~ "spatial",
    x$pt_swrcb_is_additional_name_candidate ~ "name_candidate",
    TRUE                                    ~ "other"
  )

  x$pt_swrcb_fill_col <- dplyr::case_when(
    x$pt_swrcb_is_official_list              ~ "#1F78B4", # official SWRCB list = blue
    x$pt_swrcb_is_additional_spatial        ~ "#F2C94C", # additional spatial = gold/tan
    x$pt_swrcb_is_additional_name_candidate ~ "#7B3294", # text/name candidate = purple
    TRUE                                    ~ "#8C8C8C"
  )

  x$pt_swrcb_fill_opacity <- dplyr::case_when(
    x$pt_swrcb_is_official_list              ~ 0.78,
    x$pt_swrcb_is_additional_spatial        ~ 0.76,
    x$pt_swrcb_is_additional_name_candidate ~ 0.58,
    TRUE                                    ~ 0.60
  )

  ## Ring/stroke communicates status.  Keep it intentionally thin so status
  ## does not dominate the provenance fill color.
  x$pt_swrcb_ring_col <- dplyr::case_when(
    status == "Active / recognized"  ~ "#39D353",
    status == "Pending"              ~ "#FFB000",
    status == "Inactive / cancelled" ~ "#E31A1C",
    TRUE                             ~ "#737373"
  )

  x$pt_swrcb_ring_weight <- dplyr::case_when(
    status == "Active / recognized"  ~ 1.25,
    status == "Pending"              ~ 1.15,
    status == "Inactive / cancelled" ~ 1.15,
    TRUE                             ~ 1.00
  )

  ## Use a presentation-oriented binned size scale.  The upper bin is capped at
  ## >1,000 AFY because a few very large State/project records reach millions of
  ## AFY and should not flatten the BLM-scale differences in the rest of the map.
  face_afy <- if ("face_afy" %in% names(x)) {
    suppressWarnings(as.numeric(x$face_afy))
  } else {
    rep(NA_real_, nrow(x))
  }

  x$pt_swrcb_face_bin <- dplyr::case_when(
    is.na(face_afy)                    ~ "missing",
    !is.na(face_afy) & face_afy == 0   ~ "0",
    face_afy > 0    & face_afy <= 1    ~ ">0–1",
    face_afy > 1    & face_afy <= 10   ~ ">1–10",
    face_afy > 10   & face_afy <= 100  ~ ">10–100",
    face_afy > 100  & face_afy <= 1000 ~ ">100–1,000",
    face_afy > 1000                    ~ ">1,000",
    TRUE                               ~ "other"
  )

  x$pt_swrcb_radius <- dplyr::case_when(
    x$pt_swrcb_face_bin == "missing"      ~ 2.8,
    x$pt_swrcb_face_bin == "0"            ~ 3.1,
    x$pt_swrcb_face_bin == ">0–1"         ~ 3.8,
    x$pt_swrcb_face_bin == ">1–10"        ~ 4.8,
    x$pt_swrcb_face_bin == ">10–100"      ~ 5.9,
    x$pt_swrcb_face_bin == ">100–1,000"   ~ 7.1,
    x$pt_swrcb_face_bin == ">1,000"       ~ 8.4,
    TRUE                                  ~ 3.5
  )

  ## If face_afy is absent in an older cache, preserve the prior cached radius.
  if (!"face_afy" %in% names(x) && "swrcb_radius" %in% names(x)) {
    cached_radius <- suppressWarnings(as.numeric(x$swrcb_radius))
    x$pt_swrcb_radius[!is.na(cached_radius)] <- cached_radius[!is.na(cached_radius)]
  }

  if (!"pt_swrcb_layer_id" %in% names(x)) {
    x$pt_swrcb_layer_id <- paste0("swrcb_pod_wr_", seq_len(nrow(x)))
  }

  ## Browser-managed layers need explicit WGS84 coordinates.
  x_ll <- tryCatch(sf::st_transform(x, 4326), error = function(e) x)
  coords <- tryCatch(sf::st_coordinates(x_ll), error = function(e) NULL)
  if (is.null(coords) || !all(c("X", "Y") %in% colnames(coords)) || nrow(coords) < nrow(x_ll)) {
    warning("SWRCB POD browser layer skipped: could not derive point coordinates.")
    return(m)
  }

  x$pt_lng <- suppressWarnings(as.numeric(coords[seq_len(nrow(x)), "X"]))
  x$pt_lat <- suppressWarnings(as.numeric(coords[seq_len(nrow(x)), "Y"]))

  x <- x |>
    dplyr::filter(
      .data$pt_swrcb_source_key %in% c("official", "spatial", "name_candidate"),
      !is.na(.data$pt_lat),
      !is.na(.data$pt_lng),
      abs(.data$pt_lat) <= 90,
      abs(.data$pt_lng) <= 180
    )

  if (nrow(x) == 0) {
    message("SWRCB POD water-right browser layer has no usable point records.")
    return(m)
  }

  message("Adding Water rights POD | SWRCB 2026 BLM list: ", sum(x$pt_swrcb_source_key == "official", na.rm = TRUE))
  message("Adding Water rights POD | BRIM spatial BLM match: ", sum(x$pt_swrcb_source_key == "spatial", na.rm = TRUE))
  message("Adding Water rights POD | BRIM name/text BLM candidate: ", sum(x$pt_swrcb_source_key == "name_candidate", na.rm = TRUE))

  ## Lightweight build-time QA: print the status split by source so legend
  ## counts can be checked against the actual red/orange/green ring colors.
  ## This is intentionally compact, but it makes status/source surprises visible
  ## during build_final_map_only() without opening a CSV.
  swrcb_status_qa <- x |>
    sf::st_drop_geometry() |>
    dplyr::count(.data$pt_swrcb_source_key, .data$swrcb_status_group, name = "n") |>
    dplyr::arrange(.data$pt_swrcb_source_key, .data$swrcb_status_group)

  if (nrow(swrcb_status_qa) > 0) {
    swrcb_status_msg <- paste(
      paste0(
        swrcb_status_qa$pt_swrcb_source_key, ": ",
        swrcb_status_qa$swrcb_status_group, "=", swrcb_status_qa$n
      ),
      collapse = "; "
    )
    message("SWRCB POD status QA by source: ", swrcb_status_msg)
  }

  message("SWRCB POD Local display: browser-managed source groups for faster future filtering.")

  keep <- c(
    "pt_swrcb_layer_id",
    "pt_swrcb_source_key",
    "pt_lat", "pt_lng",
    "pt_swrcb_radius",
    "pt_swrcb_ring_col",
    "pt_swrcb_ring_weight",
    "pt_swrcb_fill_col",
    "pt_swrcb_fill_opacity",
    "pt_swrcb_face_bin",
    "face_afy",
    "on_blm_ca",
    "dist_to_blm_mi",
    "dist_to_blm_ft",
    "swrcb_status_group",
    "hover_text",
    "popup_html",
    names(x)[grepl("^swrcbpop_", names(x))]
  )

  rec <- x |>
    sf::st_drop_geometry() |>
    dplyr::select(dplyr::any_of(unique(keep)))

  ## RF045n-fix3:
  ##   Register companion label overlay rows for BRIM's existing inline lbl
  ##   control. The visible WR-ID labels are still drawn by the browser-managed
  ##   SWRCB controller, so labels follow source visibility, current filters,
  ##   viewport, and zoom. This mirrors the USGS Streamgages pattern and avoids
  ##   custom one-off layer-control row rewriting.
  official_label_group_name <- pt_layer_group_name("Labels: Water rights POD | SWRCB 2026 BLM list")
  spatial_label_group_name <- pt_layer_group_name("Labels: Water rights POD | BRIM spatial BLM match")
  name_candidate_label_group_name <- pt_layer_group_name("Labels: Water rights POD | BRIM name/text BLM candidate")

  if (isTRUE(map_display$add_labels)) {
    dummy <- data.frame(lng = -170, lat = 10)
    m <- m |>
      leaflet::addCircleMarkers(
        data = dummy,
        lng = ~lng,
        lat = ~lat,
        group = official_label_group_name,
        layerId = "pt_swrcb_official_label_dummy",
        radius = 0.001,
        stroke = FALSE,
        opacity = 0,
        fillOpacity = 0,
        options = leaflet::pathOptions(pane = "pane_labels_pts", interactive = FALSE)
      ) |>
      leaflet::addCircleMarkers(
        data = dummy,
        lng = ~lng,
        lat = ~lat,
        group = spatial_label_group_name,
        layerId = "pt_swrcb_spatial_label_dummy",
        radius = 0.001,
        stroke = FALSE,
        opacity = 0,
        fillOpacity = 0,
        options = leaflet::pathOptions(pane = "pane_labels_pts", interactive = FALSE)
      ) |>
      leaflet::addCircleMarkers(
        data = dummy,
        lng = ~lng,
        lat = ~lat,
        group = name_candidate_label_group_name,
        layerId = "pt_swrcb_name_candidate_label_dummy",
        radius = 0.001,
        stroke = FALSE,
        opacity = 0,
        fillOpacity = 0,
        options = leaflet::pathOptions(pane = "pane_labels_pts", interactive = FALSE)
      ) |>
      leaflet::hideGroup(official_label_group_name) |>
      leaflet::hideGroup(spatial_label_group_name) |>
      leaflet::hideGroup(name_candidate_label_group_name)
  }

  js <- r"---(
function(el, x, data) {
  var map = this;
  data = data || {};

  var SOURCE_DEFS = [
    {
      key: 'official',
      group: data.groups && data.groups[0] ? String(data.groups[0]) : 'Points – Water rights POD | SWRCB 2026 BLM list',
      labelGroup: data.labelGroups && data.labelGroups[0] ? String(data.labelGroups[0]) : 'Labels – Water rights POD | SWRCB 2026 BLM list',
      aliases: ['water rights pod swrcb 2026 blm list', 'swrcb 2026 blm wr list records', 'swrcb official blm wr list records']
    },
    {
      key: 'spatial',
      group: data.groups && data.groups[1] ? String(data.groups[1]) : "Points – Water rights POD | BRIM spatial BLM match",
      labelGroup: data.labelGroups && data.labelGroups[1] ? String(data.labelGroups[1]) : "Labels – Water rights POD | BRIM spatial BLM match",
      aliases: [
        'water rights pod brim spatial blm match',
        'swrcb additional pods spatially matched to blm',
        "swrcb add'l pods spatially matched to blm",
        'swrcb pods spatially matched to blm',
        'swrcb spatial pod matches to blm'
      ]
    },
    {
      key: 'name_candidate',
      group: data.groups && data.groups[2] ? String(data.groups[2]) : "Points – Water rights POD | BRIM name/text BLM candidate",
      labelGroup: data.labelGroups && data.labelGroups[2] ? String(data.labelGroups[2]) : "Labels – Water rights POD | BRIM name/text BLM candidate",
      aliases: [
        'water rights pod brim name/text blm candidate',
        'swrcb additional blm name/text-match candidates',
        "swrcb add'l blm name/text-match candidates",
        'swrcb blm-associated wr/pod records',
        'swrcb wr/name-list matches'
      ]
    }
  ];

  function rowsToArray(rows) {
    if (!rows) return [];
    if (Array.isArray(rows)) return rows;
    if (typeof rows === 'object') {
      var keys = Object.keys(rows), n = 0;
      for (var k = 0; k < keys.length; k++) {
        if (Array.isArray(rows[keys[k]])) { n = rows[keys[k]].length; break; }
      }
      var out = [];
      for (var i = 0; i < n; i++) {
        var r = {};
        for (var j = 0; j < keys.length; j++) {
          var key = keys[j];
          r[key] = Array.isArray(rows[key]) ? rows[key][i] : rows[key];
        }
        out.push(r);
      }
      return out;
    }
    return [];
  }

  function has(v) {
    if (v === null || v === undefined) return false;
    var s = String(v).trim();
    return s !== '' && s !== 'NA' && s !== 'NaN' && s !== 'null' && s !== 'undefined';
  }

  function esc(v) {
    if (!has(v)) return 'NA';
    return String(v)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  function cleanSwrcbDisplayText(v) {
    if (v === null || v === undefined) return v;
    var out = String(v);

    // Replace the old internal PT2 label in user-facing SWRCB hover/popup text.
    // Keep the provenance meaning, but use BRIM language that makes sense to
    // field-office users.
    out = out.replace(/PT2\s+additional\s+BLM\s+name\/text[- ]match\s+candidate/g, 'BRIM name/text candidate');
    out = out.replace(/PT2\s+additional\s+spatial\s+POD\s+match/g, 'BRIM spatial POD match');
    out = out.replace(/Additional\s+PT2\s+text\/name\s+screen/g, 'Additional BRIM text/name screen');
    out = out.replace(/Additional\s+PT2\s+spatial\s+screen/g, 'Additional BRIM spatial screen');

    // The raw CalWATRS/SWRCB source-code field can contain terse codes such as
    // UNST. Label it as a raw source code so it is not confused with BRIM's
    // screening-source/provenance label.
    out = out.replace(/<b>Source:<\/b>/g, '<b>SWRCB source code:<\/b>');
    return out;
  }

  function isAdditionalBrimSource(r) {
    var key = String(r && r.pt_swrcb_source_key != null ? r.pt_swrcb_source_key : '').toLowerCase();
    if (key === 'spatial' || key === 'name_candidate') return true;
    var src = String(r && r.swrcbpop_screening_source != null ? r.swrcbpop_screening_source : '').toLowerCase();
    return src.indexOf('brim') >= 0 || src.indexOf('pt2') >= 0 || src.indexOf('additional') >= 0;
  }

  function brimRestDataNoteHtml() {
    return '<b>BRIM screen data:</b> SWRCB/CalWATRS public REST API';
  }

  function swrcbQaIdsHtml(r) {
    var bits = [];
    if (has(r.swrcbpop_pod_feature_id)) bits.push('feature ' + esc(r.swrcbpop_pod_feature_id));
    if (has(r.swrcbpop_pod_id)) bits.push('POD/list ' + esc(r.swrcbpop_pod_id));
    if (bits.length === 0) return '';
    return '<span style="font-size:11px;color:#666;"><b>SWRCB/CalWATRS QA IDs:</b> ' + bits.join(' · ') + '</span>';
  }

  function stripSwrcbBackendIdRows(html) {
    var out = String(html == null ? '' : html);

    // Keep backend/API IDs out of the main popup body.  They remain available
    // as a compact QA line at the bottom for audit/reconciliation work.
    out = out.replace(/(^|<br\s*\/?>)\s*<b>POD feature ID:<\/b>\s*[^<]*(?=<br\s*\/?>|$)/gi, '');
    out = out.replace(/(^|<br\s*\/?>)\s*<b>POD\/WR-list ID:<\/b>\s*[^<]*(?=<br\s*\/?>|$)/gi, '');
    out = out.replace(/(<br\s*\/?>\s*){2,}/gi, '<br/>');
    out = out.replace(/^\s*<br\s*\/?>/i, '');
    out = out.replace(/<br\s*\/?>\s*$/i, '');
    return out;
  }

  function addBrimRestDataNote(html, r) {
    var out = stripSwrcbBackendIdRows(html);

    if (isAdditionalBrimSource(r)) {
      var note = brimRestDataNoteHtml();
      if (String(out).indexOf(note) < 0) {
        // Insert near the screening-source line when possible; otherwise append.
        var pattern = /(<b>Screening source:<\/b>.*?(?:<br\s*\/?>|$))/i;
        if (pattern.test(out)) {
          out = String(out).replace(pattern, '$1' + note + '<br/>');
        } else {
          out = String(out) + '<br/>' + note;
        }
      }
    }

    var qa = swrcbQaIdsHtml(r);
    if (qa && String(out).indexOf('SWRCB/CalWATRS QA IDs:') < 0) {
      out = String(out) + '<br/>' + qa;
    }

    return out;
  }

  function num(v, fallback) {
    var n = Number(v);
    return isFinite(n) ? n : fallback;
  }

  function norm(s) {
    return String(s == null ? '' : s)
      .replace(/&amp;/g, '&')
      .replace(/[–—]/g, '-')
      .toLowerCase()
      .replace(/points\s*-\s*/g, '')
      .replace(/monitoring sites\s*\/\s*records\s*-\s*/g, '')
      .replace(/\s*\([^)]*\)\s*$/g, '')
      .replace(/\s+/g, ' ')
      .trim();
  }

  function sourceMatchesText(def, text) {
    var ns = norm(text);
    if (!ns) return false;
    var g = norm(def.group);
    if (g && (ns === g || ns.indexOf(g) >= 0 || g.indexOf(ns) >= 0)) return true;
    for (var i = 0; i < def.aliases.length; i++) {
      var a = norm(def.aliases[i]);
      if (a && (ns === a || ns.indexOf(a) >= 0 || a.indexOf(ns) >= 0)) return true;
    }
    return false;
  }

  function sourceForText(text) {
    for (var i = 0; i < SOURCE_DEFS.length; i++) {
      if (sourceMatchesText(SOURCE_DEFS[i], text)) return SOURCE_DEFS[i];
    }
    return null;
  }

  function labelMatchesText(def, text) {
    var ns = norm(text);
    if (!ns) return false;
    var lg = norm(def.labelGroup || '');
    var base = norm(String(def.labelGroup || '').replace(/^Labels\s+[–-]\s+/i, ''));
    return (lg && (ns === lg || ns.indexOf(lg) >= 0 || lg.indexOf(ns) >= 0)) ||
      (base && (ns === base || ns.indexOf(base) >= 0 || base.indexOf(ns) >= 0));
  }

  function labelSourceForText(text) {
    for (var i = 0; i < SOURCE_DEFS.length; i++) {
      if (labelMatchesText(SOURCE_DEFS[i], text)) return SOURCE_DEFS[i];
    }
    return null;
  }

  function safeControlScan() {
    var active = {official: false, spatial: false, name_candidate: false};
    if (typeof document === 'undefined' || !document.querySelectorAll) return active;
    var labels = document.querySelectorAll('.leaflet-control-layers-overlays label');
    for (var i = 0; i < labels.length; i++) {
      var label = labels[i];
      var full = label.getAttribute ? (label.getAttribute('data-pt-layer-full-name') || '') : '';
      var text = label.textContent || label.innerText || '';
      if (/^\s*Labels\s+[–-]\s+/i.test(full || text)) continue;
      var def = sourceForText(full || text);
      if (!def) continue;
      var input = label.querySelector ? label.querySelector('input[type="checkbox"]') : null;
      if (input && input.checked) active[def.key] = true;
    }
    return active;
  }

  function eventMatches(evt) {
    if (!evt) return false;
    var nm = String(evt.name == null ? '' : evt.name);
    if (/^\s*Labels\s+[–-]\s+/i.test(nm)) return false;
    if (sourceForText(nm)) return true;
    if (evt.layer && evt.layer.options) {
      var bits = [evt.layer.options.group, evt.layer.options.name, evt.layer.options.layerId].join(' ');
      if (/^\s*Labels\s+[–-]\s+/i.test(String(evt.layer.options.group || ''))) return false;
      return !!sourceForText(bits);
    }
    return false;
  }

  function safeLabelControlScan() {
    var active = {official: false, spatial: false, name_candidate: false};
    if (typeof document === 'undefined' || !document.querySelectorAll) return active;
    var labels = document.querySelectorAll('.leaflet-control-layers-overlays label');
    for (var i = 0; i < labels.length; i++) {
      var label = labels[i];
      var full = label.getAttribute ? (label.getAttribute('data-pt-layer-full-name') || '') : '';
      var text = full || label.textContent || label.innerText || '';
      if (!/^\s*Labels\s+[–-]\s+/i.test(text)) continue;
      var def = labelSourceForText(text);
      if (!def) continue;
      var input = label.querySelector ? label.querySelector('input[type="checkbox"]') : null;
      if (input && input.checked) active[def.key] = true;
    }
    return active;
  }

  function labelEventSource(evt) {
    if (!evt) return null;
    var byName = labelSourceForText(evt.name);
    if (byName) return byName;
    if (evt.layer && evt.layer.options) {
      var bits = [evt.layer.options.group, evt.layer.options.name, evt.layer.options.layerId].join(' ');
      return labelSourceForText(bits);
    }
    return null;
  }

  var records = rowsToArray(data.records).filter(function(r) {
    return r && has(r.pt_swrcb_source_key) && has(r.pt_lat) && has(r.pt_lng);
  });

  var recordsBySource = {official: [], spatial: [], name_candidate: []};
  records.forEach(function(r) {
    var key = String(r.pt_swrcb_source_key);
    if (recordsBySource[key]) recordsBySource[key].push(r);
  });

  function makeTooltip(r) {
    if (has(r.hover_text)) {
      return esc(cleanSwrcbDisplayText(r.hover_text)).replace(/\n/g, '<br/>');
    }
    var wr = has(r.swrcbpop_water_right) ? r.swrcbpop_water_right : r.pt_swrcb_layer_id;
    return 'WR: ' + esc(wr) +
      '<br/>Holder: ' + esc(r.swrcbpop_holder) +
      '<br/>Face Value: ' + esc(r.swrcbpop_face_value) +
      '<br/>Source: ' + esc(cleanSwrcbDisplayText(r.swrcbpop_screening_source));
  }

  function makePopup(r) {
    if (has(r.popup_html)) return addBrimRestDataNote(cleanSwrcbDisplayText(String(r.popup_html)), r);

    var parts = [];
    parts.push('<b>SWRCB / CalWATRS POD</b>');
    parts.push('<b>Water Right:</b> ' + esc(r.swrcbpop_water_right));
    parts.push('<b>Holder:</b> ' + esc(r.swrcbpop_holder));
    parts.push('<b>Face Value:</b> ' + esc(r.swrcbpop_face_value));
    if (has(r.swrcbpop_face_value_source)) {
      parts.push('<b>Face-value source:</b> ' + esc(r.swrcbpop_face_value_source));
    }
    parts.push('<b>Screening source:</b> ' + esc(cleanSwrcbDisplayText(r.swrcbpop_screening_source)));
    if (isAdditionalBrimSource(r)) parts.push(brimRestDataNoteHtml());
    parts.push('<b>BLM match detail:</b> ' + esc(r.swrcbpop_blm_match_detail));
    parts.push('<b>Spatial BLM match:</b> ' + esc(r.swrcbpop_spatial_blm_match));
    parts.push('<b>Interpretation:</b> ' + esc(cleanSwrcbDisplayText(r.swrcbpop_interpretation)));
    parts.push('<b>SWRCB 2026 BLM WR list:</b> ' + esc(r.swrcbpop_2026_blm_wr_list));
    parts.push('<b>WR Status:</b> ' + esc(r.swrcbpop_wr_status));
    parts.push('<b>WR Type:</b> ' + esc(r.swrcbpop_wr_type));
    parts.push('<b>POD Status:</b> ' + esc(r.swrcbpop_pod_status));
    parts.push('<b>SWRCB source code:</b> ' + esc(r.swrcbpop_source));
    parts.push('<b>County:</b> ' + esc(r.swrcbpop_county));
    var qa = swrcbQaIdsHtml(r);
    if (qa) parts.push(qa);
    return parts.join('<br/>');
  }

  function makeMarker(r) {
    var lat = num(r.pt_lat, NaN), lng = num(r.pt_lng, NaN);
    if (!isFinite(lat) || !isFinite(lng)) return null;

    var marker = L.circleMarker([lat, lng], {
      radius: num(r.pt_swrcb_radius, 3.5),
      stroke: true,
      color: has(r.pt_swrcb_ring_col) ? String(r.pt_swrcb_ring_col) : '#737373',
      weight: num(r.pt_swrcb_ring_weight, 1.1),
      opacity: 0.96,
      fillColor: has(r.pt_swrcb_fill_col) ? String(r.pt_swrcb_fill_col) : '#8C8C8C',
      fillOpacity: num(r.pt_swrcb_fill_opacity, 0.72),
      pane: 'pane_points'
    });

    marker.bindTooltip(makeTooltip(r), {
      direction: 'auto',
      opacity: 0.9,
      sticky: true,
      className: 'pt-swrcb-pod-tooltip'
    });

    marker.bindPopup(function() { return makePopup(r); }, {
      maxWidth: 440,
      maxHeight: 560
    });

    return marker;
  }

  var clusters = {};
  var built = {};
  var activeState = {official: false, spatial: false, name_candidate: false};

  // RF045n-fix3: label state is driven by normal hidden companion label
  // overlay groups, not one-off SWRCB layer-control row editing.
  var labelGroups = {};
  var labelEnabled = {official: false, spatial: false, name_candidate: false};
  var labelMinZoom = 11;

  function defaultFilters() {
    return {status: 'all', afy: 'all', minAfy: '', blm: 'any'};
  }

  var currentFilters = defaultFilters();

  function filterIsActive(f) {
    f = f || currentFilters || defaultFilters();
    return (f.status && f.status !== 'all') ||
      (f.afy && f.afy !== 'all') ||
      (f.blm && f.blm !== 'any') ||
      (String(f.minAfy || '').trim() !== '');
  }

  function statusKey(r) {
    var s = String(r && r.swrcb_status_group != null ? r.swrcb_status_group : '').toLowerCase();

    // Important: test inactive/cancelled before active.
    // "Inactive / cancelled" contains the substring "active", so the
    // reverse order misclassified red inactive rings as active in browser-side
    // legend counts and filters.
    if (s.indexOf('inactive') >= 0 || s.indexOf('cancel') >= 0) return 'inactive';
    if (s.indexOf('pending') >= 0) return 'pending';
    if (s.indexOf('active') >= 0 || s.indexOf('recognized') >= 0) return 'active';
    return 'unknown';
  }

  function faceValue(r) {
    var v = Number(r && r.face_afy);
    return isFinite(v) ? v : NaN;
  }

  function boolish(v) {
    if (v === true) return true;
    if (v === false || v === null || v === undefined) return false;
    var s = String(v).trim().toLowerCase();
    return s === 'true' || s === 't' || s === '1' || s === 'yes' || s === 'y';
  }

  function blmDistanceMi(r) {
    var v = Number(r && r.dist_to_blm_mi);
    return isFinite(v) ? v : NaN;
  }

  function recordPassesFilters(r, f) {
    f = f || currentFilters || defaultFilters();

    if (f.status && f.status !== 'all' && statusKey(r) !== f.status) return false;

    var afyMode = String(f.afy || 'all');
    var afy = faceValue(r);
    var minAfy = Number(String(f.minAfy || '').trim());

    if (String(f.minAfy || '').trim() !== '' && isFinite(minAfy)) {
      if (!isFinite(afy) || afy < minAfy) return false;
    } else if (afyMode === 'zero') {
      if (!isFinite(afy) || afy !== 0) return false;
    } else if (afyMode === 'gt0') {
      if (!isFinite(afy) || afy <= 0) return false;
    } else if (afyMode === 'gte10') {
      if (!isFinite(afy) || afy < 10) return false;
    } else if (afyMode === 'gte100') {
      if (!isFinite(afy) || afy < 100) return false;
    } else if (afyMode === 'gte1000') {
      if (!isFinite(afy) || afy < 1000) return false;
    }

    var blmMode = String(f.blm || 'any');
    if (blmMode !== 'any') {
      var onBlm = boolish(r && r.on_blm_ca);
      var distMi = blmDistanceMi(r);
      if (blmMode === 'on') {
        if (!onBlm) return false;
      } else if (blmMode === 'off') {
        if (onBlm || !isFinite(distMi)) return false;
      } else if (blmMode === 'within1') {
        if (!isFinite(distMi) || distMi > 1) return false;
      } else if (blmMode === 'within5') {
        if (!isFinite(distMi) || distMi > 5) return false;
      }
    }

    return true;
  }

  function emptyCount() {
    return {
      total: 0,
      status_active: 0,
      status_pending: 0,
      status_inactive: 0,
      status_unknown: 0,
      face_missing: 0,
      face_zero: 0,
      face_0_1: 0,
      face_1_10: 0,
      face_10_100: 0,
      face_100_1000: 0,
      face_1000_plus: 0
    };
  }

  function addRecordToCount(out, r) {
    out.total += 1;
    var sk = statusKey(r);
    if (sk === 'active') out.status_active += 1;
    else if (sk === 'pending') out.status_pending += 1;
    else if (sk === 'inactive') out.status_inactive += 1;
    else out.status_unknown += 1;

    var bin = String(r && r.pt_swrcb_face_bin != null ? r.pt_swrcb_face_bin : 'missing');
    if (bin === 'missing') out.face_missing += 1;
    else if (bin === '0') out.face_zero += 1;
    else if (bin === '>0–1' || bin === '>0-1') out.face_0_1 += 1;
    else if (bin === '>1–10' || bin === '>1-10') out.face_1_10 += 1;
    else if (bin === '>10–100' || bin === '>10-100') out.face_10_100 += 1;
    else if (bin === '>100–1,000' || bin === '>100-1,000') out.face_100_1000 += 1;
    else if (bin === '>1,000') out.face_1000_plus += 1;
    else out.face_missing += 1;
    return out;
  }

  function filteredCountsForActive(active, f) {
    active = active || activeState || {};
    f = f || currentFilters || defaultFilters();
    var out = emptyCount();
    ['official', 'spatial', 'name_candidate'].forEach(function(key) {
      if (!active[key]) return;
      var arr = recordsBySource[key] || [];
      for (var i = 0; i < arr.length; i++) {
        if (recordPassesFilters(arr[i], f)) addRecordToCount(out, arr[i]);
      }
    });
    return out;
  }

  function notifyLegend() {
    try { if (map && map.fire) map.fire('pt:swrcbfilterchange'); } catch(e) {}
    try {
      if (window.BRIM_SWRCB_POD_LOCAL_REFRESH_LEGEND &&
          typeof window.BRIM_SWRCB_POD_LOCAL_REFRESH_LEGEND === 'function') {
        window.BRIM_SWRCB_POD_LOCAL_REFRESH_LEGEND();
      }
    } catch(e2) {}
  }

  function clusterOptionsFor(key) {
    return {
      disableClusteringAtZoom: 10,
      spiderfyOnMaxZoom: true,
      showCoverageOnHover: false,
      animate: false,
      removeOutsideVisibleBounds: true,
      chunkedLoading: true,
      chunkInterval: 120,
      chunkDelay: 15,
      maxClusterRadius: function(z) {
        if (z <= 6) return 110;
        if (z <= 8) return 90;
        if (z <= 9) return 70;
        return 50;
      }
    };
  }

  function getCluster(key) {
    if (!clusters[key]) {
      clusters[key] = (typeof L.markerClusterGroup === 'function') ?
        L.markerClusterGroup(clusterOptionsFor(key)) :
        L.layerGroup();
    }
    return clusters[key];
  }

  function getLabelGroup(key) {
    if (!labelGroups[key]) labelGroups[key] = L.layerGroup();
    return labelGroups[key];
  }

  function labelText(r) {
    var wr = has(r && r.swrcbpop_water_right) ? String(r.swrcbpop_water_right) : '';
    if (!has(wr)) wr = has(r && r.pt_swrcb_layer_id) ? String(r.pt_swrcb_layer_id) : '';
    return wr;
  }

  function labelsAllowedByZoom() {
    return map && typeof map.getZoom === 'function' && map.getZoom() >= labelMinZoom;
  }

  function labelBounds() {
    try {
      if (map && typeof map.getBounds === 'function') return map.getBounds().pad(0.25);
    } catch(e) {}
    return null;
  }

  function labelCoordKey(r) {
    var lat = num(r.pt_lat, NaN), lng = num(r.pt_lng, NaN);
    if (!isFinite(lat) || !isFinite(lng)) return null;

    // Group true co-located POD records without accidentally merging nearby
    // stream/ditch records. Six decimals is intentionally tight for display
    // deconfliction of identical/near-identical map coordinates.
    return lat.toFixed(6) + '|' + lng.toFixed(6);
  }

  function groupedLabelHtml(rows) {
    rows = rows || [];
    var seen = {}, ids = [];

    for (var i = 0; i < rows.length; i++) {
      var txt = labelText(rows[i]);
      if (!has(txt)) continue;
      txt = String(txt);
      if (seen[txt]) continue;
      seen[txt] = true;
      ids.push(txt);
    }

    if (ids.length === 0) return '';

    var maxShown = 3;
    var shown = ids.slice(0, maxShown).map(esc).join('<br/>');
    if (ids.length > maxShown) {
      shown += '<br/><span class="pt-swrcb-pod-id-label-more">+' + (ids.length - maxShown) + ' more</span>';
    }
    return shown;
  }

  function makeGroupedLabelMarker(rows) {
    rows = rows || [];
    if (rows.length === 0) return null;

    var r = rows[0];
    var lat = num(r.pt_lat, NaN), lng = num(r.pt_lng, NaN);
    if (!isFinite(lat) || !isFinite(lng)) return null;

    var html = groupedLabelHtml(rows);
    if (!has(html)) return null;

    return L.marker([lat, lng], {
      interactive: false,
      keyboard: false,
      pane: 'pane_labels_pts',
      icon: L.divIcon({
        className: 'pt-swrcb-pod-id-label',
        html: '<span>' + html + '</span>',
        iconSize: L.point(1, 1),
        iconAnchor: L.point(0, 0)
      })
    });
  }

  function rebuildLabelsForSource(key) {
    var group = getLabelGroup(key);
    if (group.clearLayers) group.clearLayers();

    if (!labelEnabled[key] || !activeState[key] || !labelsAllowedByZoom()) {
      if (map.hasLayer(group)) map.removeLayer(group);
      return;
    }

    var bounds = labelBounds();
    var arr = recordsBySource[key] || [];
    var grouped = {};
    var layers = [];

    for (var i = 0; i < arr.length; i++) {
      var r = arr[i];
      if (!recordPassesFilters(r, currentFilters)) continue;
      var lat = num(r.pt_lat, NaN), lng = num(r.pt_lng, NaN);
      if (!isFinite(lat) || !isFinite(lng)) continue;
      if (bounds && !bounds.contains([lat, lng])) continue;
      if (!has(labelText(r))) continue;
      var key2 = labelCoordKey(r);
      if (!key2) continue;
      if (!grouped[key2]) grouped[key2] = [];
      grouped[key2].push(r);
    }

    Object.keys(grouped).forEach(function(k) {
      var marker = makeGroupedLabelMarker(grouped[k]);
      if (marker) layers.push(marker);
    });

    for (var j = 0; j < layers.length; j++) group.addLayer(layers[j]);
    if (!map.hasLayer(group)) group.addTo(map);
  }

  function rebuildActiveLabels() {
    ['official', 'spatial', 'name_candidate'].forEach(rebuildLabelsForSource);
  }

  function syncLabelsFromControls() {
    var desired = safeLabelControlScan();
    ['official', 'spatial', 'name_candidate'].forEach(function(key) {
      labelEnabled[key] = !!desired[key] && !!activeState[key];
    });
    rebuildActiveLabels();
  }

  function buildSource(key) {
    if (built[key]) return;
    var cluster = getCluster(key);
    cluster.clearLayers();
    var layers = [];
    var arr = recordsBySource[key] || [];
    for (var i = 0; i < arr.length; i++) {
      if (!recordPassesFilters(arr[i], currentFilters)) continue;
      var marker = makeMarker(arr[i]);
      if (marker) layers.push(marker);
    }
    if (typeof cluster.addLayers === 'function') {
      cluster.addLayers(layers);
    } else {
      for (var j = 0; j < layers.length; j++) cluster.addLayer(layers[j]);
    }
    built[key] = true;
  }

  function rebuildActiveSources() {
    ['official', 'spatial', 'name_candidate'].forEach(function(key) {
      built[key] = false;
      var cluster = getCluster(key);
      if (cluster.clearLayers) cluster.clearLayers();
      if (activeState[key]) {
        buildSource(key);
        if (!map.hasLayer(cluster)) cluster.addTo(map);
      }
    });
    syncLabelsFromControls();
    notifyLegend();
  }

  function setSourceActive(key, isActive) {
    var cluster = getCluster(key);
    var currently = !!activeState[key];

    if (isActive) {
      buildSource(key);
      if (!map.hasLayer(cluster)) cluster.addTo(map);
    } else {
      if (map.hasLayer(cluster)) map.removeLayer(cluster);
      labelEnabled[key] = false;
      var labelGroup = getLabelGroup(key);
      if (map.hasLayer(labelGroup)) map.removeLayer(labelGroup);
    }

    activeState[key] = !!isActive;
  }

  function syncActive() {
    var desired = safeControlScan();
    var anyDesired = !!(desired.official || desired.spatial || desired.name_candidate);
    setSourceActive('official', desired.official);
    setSourceActive('spatial', desired.spatial);
    setSourceActive('name_candidate', desired.name_candidate);
    if (!anyDesired && filterIsActive(currentFilters)) {
      currentFilters = defaultFilters();
      built.official = false;
      built.spatial = false;
      built.name_candidate = false;
    }
    syncLabelsFromControls();
    notifyLegend();
  }

  function applyFilters(filters) {
    filters = filters || {};
    var next = defaultFilters();
    next.status = String(filters.status || currentFilters.status || 'all');
    next.afy = String(filters.afy || currentFilters.afy || 'all');
    next.blm = String(filters.blm || currentFilters.blm || 'any');
    next.minAfy = String(filters.minAfy != null ? filters.minAfy : (currentFilters.minAfy || '')).trim();
    if (next.minAfy !== '') next.afy = 'min';
    if (!['all', 'active', 'pending', 'inactive'].includes(next.status)) next.status = 'all';
    if (!['all', 'zero', 'gt0', 'gte10', 'gte100', 'gte1000', 'min'].includes(next.afy)) next.afy = 'all';
    if (!['any', 'on', 'off', 'within1', 'within5'].includes(next.blm)) next.blm = 'any';
    currentFilters = next;
    rebuildActiveSources();
  }

  function resetFilters() {
    currentFilters = defaultFilters();
    rebuildActiveSources();
  }

  window.BRIM_SWRCB_POD_LOCAL = {
    setActiveSources: function(active) {
      active = active || {};
      setSourceActive('official', !!active.official);
      setSourceActive('spatial', !!active.spatial);
      setSourceActive('name_candidate', !!active.name_candidate);
    },
    syncActive: syncActive,
    applyFilters: applyFilters,
    resetFilters: resetFilters,
    getFilters: function() {
      return {
        status: currentFilters.status || 'all',
        afy: currentFilters.afy || 'all',
        minAfy: currentFilters.minAfy || '',
        blm: currentFilters.blm || 'any'
      };
    },
    getFilteredCounts: function(active, filters) {
      return filteredCountsForActive(active || activeState, filters || currentFilters);
    },
    stats: function() {
      return {
        official: {records: (recordsBySource.official || []).length, active: activeState.official, labels: labelEnabled.official},
        spatial: {records: (recordsBySource.spatial || []).length, active: activeState.spatial, labels: labelEnabled.spatial},
        name_candidate: {records: (recordsBySource.name_candidate || []).length, active: activeState.name_candidate, labels: labelEnabled.name_candidate},
        filters: currentFilters,
        filtered: filteredCountsForActive(activeState, currentFilters)
      };
    }
  };

  map.on('overlayadd', function(evt) {
    if (eventMatches(evt)) window.setTimeout(syncActive, 0);
    if (labelEventSource(evt)) window.setTimeout(syncLabelsFromControls, 0);
  });
  map.on('overlayremove', function(evt) {
    if (eventMatches(evt)) window.setTimeout(syncActive, 0);
    if (labelEventSource(evt)) window.setTimeout(syncLabelsFromControls, 0);
  });

  if (typeof document !== 'undefined' && document.addEventListener) {
    document.addEventListener('change', function(evt) {
      var t = evt && evt.target;
      if (t && t.matches && t.matches('.leaflet-control-layers-overlays input[type="checkbox"]')) {
        window.setTimeout(syncActive, 0);
        window.setTimeout(syncLabelsFromControls, 0);
      }
    }, true);
  }

  if (!document.getElementById('pt-swrcb-pod-browser-style')) {
    var style = document.createElement('style');
    style.id = 'pt-swrcb-pod-browser-style';
    style.textContent =
      '.pt-swrcb-pod-tooltip{white-space:pre !important;max-width:none !important;font-size:12px;line-height:1.25;}' +
      '.pt-swrcb-pod-id-label{background:transparent;border:0;white-space:nowrap;pointer-events:none;}' +
      '.pt-swrcb-pod-id-label span{display:inline-block;transform:translate(7px,-7px);font:700 10.5px/1.12 Arial,sans-serif;color:#222;background:rgba(255,255,255,0.72);border:1px solid rgba(70,70,70,0.28);border-radius:3px;padding:1px 3px;text-shadow:0 1px 2px #fff,1px 0 2px #fff,-1px 0 2px #fff,0 -1px 2px #fff;box-shadow:0 1px 2px rgba(0,0,0,0.10);max-width:170px;overflow:hidden;text-overflow:ellipsis;}' +
      '.pt-swrcb-pod-id-label-more{font-weight:600;color:#555;font-size:9.7px;}';
    document.head.appendChild(style);
  }

  if (map && typeof map.on === 'function') {
    map.on('zoomend moveend', function() {
      if (labelEnabled.official || labelEnabled.spatial || labelEnabled.name_candidate) {
        window.setTimeout(rebuildActiveLabels, 0);
      }
    });
  }

  setTimeout(function() { syncActive(); syncLabelsFromControls(); }, 0);
  setTimeout(function() { syncActive(); syncLabelsFromControls(); }, 500);
}
)---"

  htmlwidgets::onRender(
    m,
    js,
    data = list(
      groups = list(official_group_name, spatial_group_name, name_candidate_group_name),
      labelGroups = list(official_label_group_name, spatial_label_group_name, name_candidate_label_group_name),
      records = rec
    )
  )
}



# ---- 4.3a Shared SWRCB / CalWATRS POD legend --------------------------------
##
## Patch 045a: one compact, shared legend for the three Local SWRCB / CalWATRS
## point layers.  These layers share the same symbology and differ primarily by
## screening provenance, so a single legend is clearer than three separate boxes.
##
## Legend rules:
##   - show when any of the three SWRCB POD layers is active;
##   - fill color explains screening source / layer;
##   - ring color explains water-right status;
##   - size explains reported face value / AFY;
##   - compact filters are handled by the browser-owned SWRCB source groups;
##   - filters rebuild only active SWRCB groups, preserving source provenance.

pt_add_swrcb_pod_wr_shared_legend <- function(m, swrcb_pod_wr_blm, map_display) {

  if (!isTRUE(map_display$add_swrcb_pod_wr_blm)) {
    return(m)
  }

  if (!inherits(swrcb_pod_wr_blm, "sf") || nrow(swrcb_pod_wr_blm) == 0) {
    return(m)
  }

  official_group_name <- pt_layer_group_name("SWRCB 2026 BLM WR list records")
  spatial_group_name <- pt_layer_group_name("SWRCB additional PODs spatially matched to BLM")
  name_candidate_group_name <- pt_layer_group_name("SWRCB additional BLM name/text-match candidates")

  x <- swrcb_pod_wr_blm

  if (!"swrcb_status_group" %in% names(x)) {
    x$swrcb_status_group <- "Unknown"
  }
  if (!"blm_include_reason_display" %in% names(x)) {
    x$blm_include_reason_display <- "Unknown"
  }
  if (!"swrcb_2026_blm_wr_list" %in% names(x)) {
    x$swrcb_2026_blm_wr_list <- FALSE
  }

  reason <- trimws(as.character(x$blm_include_reason_display))
  status <- trimws(as.character(x$swrcb_status_group))
  list_flag <- x$swrcb_2026_blm_wr_list
  list_flag[is.na(list_flag)] <- FALSE
  is_official <- isTRUE(list_flag) | list_flag
  is_spatial <- !is_official & reason %in% c("Spatial + name match", "Spatial only")
  is_name <- !is_official & reason %in% c("Name match only")

  face_afy <- if ("face_afy" %in% names(x)) {
    suppressWarnings(as.numeric(x$face_afy))
  } else {
    rep(NA_real_, nrow(x))
  }
  face_bin <- dplyr::case_when(
    is.na(face_afy)                   ~ "missing",
    !is.na(face_afy) & face_afy == 0  ~ "0",
    face_afy > 0    & face_afy <= 1   ~ ">0–1",
    face_afy > 1    & face_afy <= 10  ~ ">1–10",
    face_afy > 10   & face_afy <= 100 ~ ">10–100",
    face_afy > 100  & face_afy <= 1000 ~ ">100–1,000",
    face_afy > 1000                   ~ ">1,000",
    TRUE                              ~ "other"
  )

  ## Counts are stored by source so the shared legend can update the status and
  ## face-value totals based only on whichever of the three SWRCB layers are
  ## currently checked.  Source-row counts remain the full source totals.
  make_swrcb_counts <- function(idx) {
    idx[is.na(idx)] <- FALSE
    this_status <- status[idx]
    this_face <- face_bin[idx]
    status_count <- function(lbl) sum(this_status == lbl, na.rm = TRUE)
    unknown_status <- sum(
      is.na(this_status) |
        !this_status %in% c("Active / recognized", "Pending", "Inactive / cancelled"),
      na.rm = TRUE
    )
    face_count <- function(lbl) sum(this_face == lbl, na.rm = TRUE)
    list(
      total = sum(idx, na.rm = TRUE),
      status_active = status_count("Active / recognized"),
      status_pending = status_count("Pending"),
      status_inactive = status_count("Inactive / cancelled"),
      status_unknown = unknown_status,
      face_missing = face_count("missing"),
      face_zero = face_count("0"),
      face_0_1 = face_count(">0–1"),
      face_1_10 = face_count(">1–10"),
      face_10_100 = face_count(">10–100"),
      face_100_1000 = face_count(">100–1,000"),
      face_1000_plus = face_count(">1,000")
    )
  }

  counts <- list(
    all = make_swrcb_counts(rep(TRUE, nrow(x))),
    official = make_swrcb_counts(is_official),
    spatial = make_swrcb_counts(is_spatial),
    name_candidate = make_swrcb_counts(is_name)
  )

  js <- r"---(
function(el, x, data) {
  var map = this;
  data = data || {};
  var groups = data.groups || [];
  var counts = data.counts || {};

  var SOURCE_DEFS = [
    {
      key: 'official',
      group: groups[0] || 'Water rights POD | SWRCB 2026 BLM list',
      label: 'SWRCB 2026 BLM WR list',
      fill: '#1F78B4',
      aliases: ['water rights pod swrcb 2026 blm list', 'swrcb 2026 blm wr list records', 'swrcb official blm wr list records']
    },
    {
      key: 'spatial',
      group: groups[1] || "Water rights POD | BRIM spatial BLM match",
      label: "Add'l spatial POD match",
      fill: '#F2C94C',
      aliases: [
        'water rights pod brim spatial blm match',
        'swrcb additional pods spatially matched to blm',
        "swrcb add'l pods spatially matched to blm",
        'swrcb pods spatially matched to blm',
        'swrcb spatial pod matches to blm'
      ]
    },
    {
      key: 'name_candidate',
      group: groups[2] || "Water rights POD | BRIM name/text BLM candidate",
      label: "Add'l BLM name/text candidate",
      fill: '#7B3294',
      aliases: [
        'water rights pod brim name/text blm candidate',
        'swrcb additional blm name/text-match candidates',
        "swrcb add'l blm name/text-match candidates",
        'swrcb blm-associated wr/pod records',
        'swrcb wr/name-list matches'
      ]
    }
  ];

  var legendUserHidden = false;

  function esc(s) {
    return String(s == null ? '' : s)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  function fmt(n) {
    var v = Number(n || 0);
    return isFinite(v) ? v.toLocaleString() : '0';
  }

  function norm(s) {
    return String(s == null ? '' : s)
      .replace(/&amp;/g, '&')
      .replace(/[–—]/g, '-')
      .toLowerCase()
      .replace(/points\s*-\s*/g, '')
      .replace(/monitoring sites\s*\/\s*records\s*-\s*/g, '')
      .replace(/\s*\([^)]*\)\s*$/g, '')
      .replace(/\s+/g, ' ')
      .trim();
  }

  function sourceMatchesText(def, text) {
    var ns = norm(text);
    if (!ns) return false;
    var g = norm(def.group);
    if (g && (ns === g || ns.indexOf(g) >= 0 || g.indexOf(ns) >= 0)) return true;
    for (var i = 0; i < def.aliases.length; i++) {
      var a = norm(def.aliases[i]);
      if (a && (ns === a || ns.indexOf(a) >= 0 || a.indexOf(ns) >= 0)) return true;
    }
    return false;
  }

  function sourceForText(text) {
    for (var i = 0; i < SOURCE_DEFS.length; i++) {
      if (sourceMatchesText(SOURCE_DEFS[i], text)) return SOURCE_DEFS[i];
    }
    return null;
  }

  function isSwrcbGroupText(s) {
    return !!sourceForText(s);
  }

  function getActiveSources() {
    var active = {official: false, spatial: false, name_candidate: false};
    if (typeof document === 'undefined' || !document.querySelectorAll) return active;
    var labels = document.querySelectorAll('.leaflet-control-layers-overlays label');
    for (var i = 0; i < labels.length; i++) {
      var label = labels[i];
      var text = label.textContent || label.innerText || '';
      var def = sourceForText(text);
      if (!def) continue;
      var input = label.querySelector ? label.querySelector('input[type="checkbox"]') : null;
      if (input && input.checked) active[def.key] = true;
    }
    return active;
  }

  function anyActive(active) {
    return !!(active.official || active.spatial || active.name_candidate);
  }

  function eventMatches(evt) {
    if (!evt) return false;
    if (isSwrcbGroupText(evt.name)) return true;
    if (evt.layer && evt.layer.options) {
      var bits = [evt.layer.options.group, evt.layer.options.name, evt.layer.options.layerId].join(' ');
      return isSwrcbGroupText(bits);
    }
    return false;
  }

  function addCounts(a, b) {
    a.total += Number(b.total || 0);
    a.status_active += Number(b.status_active || 0);
    a.status_pending += Number(b.status_pending || 0);
    a.status_inactive += Number(b.status_inactive || 0);
    a.status_unknown += Number(b.status_unknown || 0);
    a.face_missing += Number(b.face_missing || 0);
    a.face_zero += Number(b.face_zero || 0);
    a.face_0_1 += Number(b.face_0_1 || 0);
    a.face_1_10 += Number(b.face_1_10 || 0);
    a.face_10_100 += Number(b.face_10_100 || 0);
    a.face_100_1000 += Number(b.face_100_1000 || 0);
    a.face_1000_plus += Number(b.face_1000_plus || 0);
    return a;
  }

  function aggregateCounts(active) {
    var out = {
      total: 0,
      status_active: 0,
      status_pending: 0,
      status_inactive: 0,
      status_unknown: 0,
      face_missing: 0,
      face_zero: 0,
      face_0_1: 0,
      face_1_10: 0,
      face_10_100: 0,
      face_100_1000: 0,
      face_1000_plus: 0
    };
    for (var i = 0; i < SOURCE_DEFS.length; i++) {
      var def = SOURCE_DEFS[i];
      if (active[def.key]) addCounts(out, counts[def.key] || {});
    }
    return out;
  }

  function getCurrentFilters() {
    var ctl = window.BRIM_SWRCB_POD_LOCAL;
    if (ctl && typeof ctl.getFilters === 'function') {
      return ctl.getFilters();
    }
    return {status: 'all', afy: 'all', minAfy: '', blm: 'any'};
  }

  function filtersAreActive(f) {
    f = f || getCurrentFilters();
    return (f.status && f.status !== 'all') ||
      (f.afy && f.afy !== 'all') ||
      (f.blm && f.blm !== 'any') ||
      (String(f.minAfy || '').trim() !== '');
  }

  function activeFilteredCounts(active) {
    var ctl = window.BRIM_SWRCB_POD_LOCAL;
    if (ctl && typeof ctl.getFilteredCounts === 'function') {
      return ctl.getFilteredCounts(active, getCurrentFilters());
    }
    return aggregateCounts(active);
  }

  function sourceDot(fill) {
    return '<span class="pt-swrcb-pod-dot" style="background:' + fill + ';border-color:#333;"></span>';
  }
  function statusRing(stroke) {
    return '<span class="pt-swrcb-pod-ring" style="border-color:' + stroke + ';"></span>';
  }
  function faceDot(cls, missing) {
    return '<span class="pt-swrcb-pod-face ' + cls + (missing ? ' missing' : '') + '"></span>';
  }
  function row(sym, label, count, cls) {
    return '<div class="pt-swrcb-pod-row ' + (cls || '') + '">' + sym +
      '<span class="pt-swrcb-pod-label">' + esc(label) + '</span>' +
      '<span class="pt-swrcb-pod-count">' + (count == null ? '' : fmt(count)) + '</span>' +
      '</div>';
  }

  function buildSourceRows(active) {
    var html = '';
    for (var i = 0; i < SOURCE_DEFS.length; i++) {
      var def = SOURCE_DEFS[i];
      var c = counts[def.key] || {};
      html += row(
        sourceDot(def.fill),
        def.label,
        c.total,
        active[def.key] ? 'is-active' : 'is-inactive'
      );
    }
    return html;
  }

  function filterBtn(kind, value, label, filters) {
    var active = String(filters[kind] || 'all') === String(value);
    return '<button type="button" class="pt-swrcb-pod-filter-btn ' + (active ? 'active' : '') + '" data-kind="' + kind + '" data-value="' + value + '">' + label + '</button>';
  }

  function buildHtml(active) {
    var filters = getCurrentFilters();
    var shown = activeFilteredCounts(active);
    var base = aggregateCounts(active);
    var activeFilter = filtersAreActive(filters);
    var html = '';
    html += '<div class="pt-swrcb-pod-title-row"><div class="pt-swrcb-pod-title">SWRCB / CalWATRS points of diversion (POD)</div><button type="button" class="pt-swrcb-pod-close" title="Hide legend">&times;</button></div>';
    html += '<div class="pt-swrcb-pod-sub">' + fmt(shown.total) + (activeFilter ? ' filtered / ' + fmt(base.total) : '') + ' PODs · fill=source · ring=status · size=AFY</div>';
    html += '<div class="pt-swrcb-pod-minihead">Screening source / layer</div>';
    html += buildSourceRows(active);
    html += '<div class="pt-swrcb-pod-minihead">Water-right status ring <span class="pt-swrcb-pod-active-note">active filters</span></div>';
    html += row(statusRing('#39D353'), 'Active / recognized', shown.status_active);
    html += row(statusRing('#FFB000'), 'Pending', shown.status_pending);
    html += row(statusRing('#E31A1C'), 'Inactive / cancelled', shown.status_inactive);
    html += row(statusRing('#737373'), 'Unknown / other', shown.status_unknown);
    html += '<div class="pt-swrcb-pod-minihead">Face value | acre-feet per year (AFY) <span class="pt-swrcb-pod-active-note">active filters</span></div>';
    html += row(faceDot('fv-missing', true), 'missing', shown.face_missing);
    html += row(faceDot('fv-zero', false), '0', shown.face_zero);
    html += row(faceDot('fv-0-1', false), '>0–1', shown.face_0_1);
    html += row(faceDot('fv-1-10', false), '>1–10', shown.face_1_10);
    html += row(faceDot('fv-10-100', false), '>10–100', shown.face_10_100);
    html += row(faceDot('fv-100-1000', false), '>100–1,000', shown.face_100_1000);
    html += row(faceDot('fv-1000', false), '>1,000', shown.face_1000_plus);
    html += '<div class="pt-swrcb-pod-filter-title">Filters</div>';
    html += '<div class="pt-swrcb-pod-filter-row"><b>Status:</b> ' +
      filterBtn('status', 'all', 'all', filters) +
      filterBtn('status', 'active', 'active', filters) +
      filterBtn('status', 'pending', 'pending', filters) +
      filterBtn('status', 'inactive', 'inactive', filters) + '</div>';
    html += '<div class="pt-swrcb-pod-filter-row"><b>BLM max mi:</b> ' +
      filterBtn('blm', 'any', 'any', filters) +
      filterBtn('blm', 'on', 'on', filters) +
      filterBtn('blm', 'off', 'off', filters) +
      filterBtn('blm', 'within1', '&le;1', filters) +
      filterBtn('blm', 'within5', '&le;5', filters) + '</div>';
    html += '<div class="pt-swrcb-pod-filter-note">BLM distance is screening-only; verify points near boundaries.</div>';
    html += '<div class="pt-swrcb-pod-filter-row"><b>AFY:</b> <input class="pt-swrcb-pod-min-afy" placeholder="min" value="' + esc(filters.minAfy || '') + '" /> ' +
      filterBtn('afy', 'all', 'all', filters) +
      filterBtn('afy', 'zero', '0', filters) +
      filterBtn('afy', 'gt0', '&gt;0', filters) +
      filterBtn('afy', 'gte10', '&ge;10', filters) +
      filterBtn('afy', 'gte100', '&ge;100', filters) +
      filterBtn('afy', 'gte1000', '&ge;1k', filters) + '</div>';
    html += '<div class="pt-swrcb-pod-filter-row pt-swrcb-pod-filter-actions"><button type="button" class="pt-swrcb-pod-apply">Apply</button><button type="button" class="pt-swrcb-pod-reset">Reset</button><span>' + (activeFilter ? 'filters on' : 'filters off') + '</span></div>';
    html += '<div class="pt-swrcb-pod-note">Add&rsquo;l = BRIM screening from public SWRCB/CalWATRS REST data; verify before reporting. Water-right ID labels (lbl) display at zoom 11+.</div>';
    return html;
  }

  var container = map && map.getContainer ? map.getContainer() : el;
  var old = (container && container.querySelector) ? container.querySelector('.pt-swrcb-pod-legend') : null;
  if (!old && el && el.querySelector) old = el.querySelector('.pt-swrcb-pod-legend');
  if (old && old.parentNode) old.parentNode.removeChild(old);

  var legend = L.control({position: 'bottomleft'});
  legend.onAdd = function(map) {
    var div = L.DomUtil.create('div', 'leaflet-control pt-swrcb-pod-legend');
    div.style.display = 'none';
    div.style.marginBottom = '54px';
    div.innerHTML = buildHtml(getActiveSources());
    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);
    return div;
  };
  legend.addTo(map);

  if (!document.getElementById('pt-swrcb-pod-legend-style')) {
    var style = document.createElement('style');
    style.id = 'pt-swrcb-pod-legend-style';
    style.textContent =
      '.pt-swrcb-pod-legend{background:rgba(246,239,222,0.96);border:1px solid rgba(112,103,83,0.55);border-radius:7px;box-shadow:0 1px 5px rgba(0,0,0,0.25);padding:7px 8px 8px 8px;max-width:320px;width:300px;font-family:Arial,sans-serif;font-size:11px;line-height:1.17;color:#222;margin-bottom:54px;}' +
      '.pt-swrcb-pod-title-row{display:flex;align-items:flex-start;justify-content:space-between;gap:6px;margin:0 0 2px 0;}' +
      '.pt-swrcb-pod-title{font-weight:700;font-size:12.1px;margin:0;}' +
      '.pt-swrcb-pod-close{border:0;background:transparent;color:#776f61;font-weight:700;font-size:14px;line-height:1;padding:0 2px;margin:-1px -2px 0 0;cursor:pointer;}' +
      '.pt-swrcb-pod-close:hover{color:#222;background:rgba(112,103,83,0.12);border-radius:3px;}' +
      '.pt-swrcb-pod-sub{font-size:10.1px;color:#4d4d4d;margin:0 0 5px 0;}' +
      '.pt-swrcb-pod-minihead{font-weight:700;font-size:10.7px;margin:5px 0 1px 0;color:#333;line-height:1.12;}' +
      '.pt-swrcb-pod-active-note{font-weight:400;font-size:9.8px;color:#666;margin-left:3px;}' +
      '.pt-swrcb-pod-row{display:grid;grid-template-columns:16px minmax(0,1fr) min-content;align-items:center;column-gap:4px;margin:1.6px 0;min-height:14px;}' +
      '.pt-swrcb-pod-row.is-inactive{opacity:0.46;}' +
      '.pt-swrcb-pod-dot{width:10px;height:10px;border:1.2px solid #333;border-radius:50%;box-sizing:border-box;display:inline-block;}' +
      '.pt-swrcb-pod-ring{width:12px;height:12px;border:1.25px solid #737373;border-radius:50%;box-sizing:border-box;display:inline-block;background:rgba(255,255,255,0.45);}' +
      '.pt-swrcb-pod-label{white-space:nowrap;overflow:hidden;text-overflow:ellipsis;}' +
      '.pt-swrcb-pod-count{font-variant-numeric:tabular-nums;color:#333;font-weight:600;}' +
      '.pt-swrcb-pod-face{border:1.15px solid #555;border-radius:50%;box-sizing:border-box;display:inline-block;vertical-align:middle;justify-self:center;opacity:1;background:transparent;}' +
      '.pt-swrcb-pod-face.missing{border-color:#777;border-style:dashed;background:transparent;opacity:1;}' +
      '.pt-swrcb-pod-face.fv-missing{width:6px;height:6px;}' +
      '.pt-swrcb-pod-face.fv-zero{width:6px;height:6px;}' +
      '.pt-swrcb-pod-face.fv-0-1{width:7px;height:7px;}' +
      '.pt-swrcb-pod-face.fv-1-10{width:9px;height:9px;}' +
      '.pt-swrcb-pod-face.fv-10-100{width:11px;height:11px;}' +
      '.pt-swrcb-pod-face.fv-100-1000{width:13px;height:13px;}' +
      '.pt-swrcb-pod-face.fv-1000{width:16px;height:16px;}' +
      '.pt-swrcb-pod-filter-title{font-weight:700;font-size:10.9px;margin:5px 0 2px 0;padding-top:4px;border-top:1px solid rgba(112,103,83,0.30);}' +
      '.pt-swrcb-pod-filter-row{display:flex;align-items:center;gap:3px;flex-wrap:wrap;margin:2px 0;font-size:10.5px;white-space:normal;}' +
      '.pt-swrcb-pod-filter-actions{justify-content:flex-start;margin-top:3px;}' +
      '.pt-swrcb-pod-filter-spacer{display:inline-block;width:5px;flex:0 0 5px;}' +
      '.pt-swrcb-pod-filter-btn,.pt-swrcb-pod-apply,.pt-swrcb-pod-reset{font-size:10.1px;line-height:1.05;padding:1px 4px;border:1px solid #999;border-radius:4px;background:#f7f3e8;color:#222;cursor:pointer;}' +
      '.pt-swrcb-pod-filter-btn.active{background:#d7c592;font-weight:700;}' +
      '.pt-swrcb-pod-min-afy{width:36px;height:15px;font-size:10.3px;border:1px solid #aaa;border-radius:3px;padding:0 3px;background:#fffdf6;}' +
      '.pt-swrcb-pod-filter-row span:not(.pt-swrcb-pod-filter-spacer){font-size:10px;color:#555;margin-left:3px;}' +
      '.pt-swrcb-pod-filter-note{font-size:9.5px;color:#5a5447;margin:0 0 2px 0;line-height:1.15;font-style:italic;}' +
      '.pt-swrcb-pod-note{font-size:9.5px;color:#4d4d4d;margin-top:4px;padding-top:3px;border-top:1px solid rgba(112,103,83,0.30);line-height:1.15;}';
    document.head.appendChild(style);
  }

  function applyLegendFilters(div, extra) {
    var ctl = window.BRIM_SWRCB_POD_LOCAL;
    if (!ctl || typeof ctl.applyFilters !== 'function') return;
    var cur = getCurrentFilters();
    var minInput = div && div.querySelector ? div.querySelector('.pt-swrcb-pod-min-afy') : null;
    var next = {
      status: cur.status || 'all',
      afy: cur.afy || 'all',
      blm: cur.blm || 'any',
      minAfy: minInput ? String(minInput.value || '').trim() : (cur.minAfy || '')
    };
    extra = extra || {};
    if (extra.status) next.status = extra.status;
    if (extra.blm) next.blm = extra.blm;
    if (extra.afy) {
      next.afy = extra.afy;
      if (extra.afy !== 'min') next.minAfy = '';
    }
    ctl.applyFilters(next);
  }

  function wireSwrcbLegendCloseButton(div) {
    if (!div || !div.querySelector) return;

    var shared = window.BRIM &&
      window.BRIM.legendCloseout &&
      typeof window.BRIM.legendCloseout.wire === 'function';

    if (shared) {
      window.BRIM.legendCloseout.wire(div, '.pt-swrcb-pod-close', function() {
        legendUserHidden = true;
      }, {hide: true});
    }
  }

  function attachEvents(div) {
    if (!div) return;

    // The close button is regenerated whenever the legend HTML refreshes, so
    // wire it each update.  Filter buttons stay on the SWRCB delegated handler
    // because they are legend-specific controls.
    wireSwrcbLegendCloseButton(div);

    if (div._ptSwrcbPodEventsAttached) return;
    div._ptSwrcbPodEventsAttached = true;
    div.addEventListener('click', function(evt) {
      var t = evt.target;
      if (!t) return;
      if (t.classList && t.classList.contains('pt-swrcb-pod-close')) {
        // Fallback path if the shared BRIM closeout helper is unavailable.
        evt.preventDefault();
        evt.stopPropagation();
        legendUserHidden = true;
        div.style.display = 'none';
        return;
      }
      if (t.classList && t.classList.contains('pt-swrcb-pod-filter-btn')) {
        evt.preventDefault();
        evt.stopPropagation();
        var kind = t.getAttribute('data-kind');
        var val = t.getAttribute('data-value');
        var extra = {};
        extra[kind] = val;
        applyLegendFilters(div, extra);
      } else if (t.classList && t.classList.contains('pt-swrcb-pod-apply')) {
        evt.preventDefault();
        evt.stopPropagation();
        applyLegendFilters(div, {});
      } else if (t.classList && t.classList.contains('pt-swrcb-pod-reset')) {
        evt.preventDefault();
        evt.stopPropagation();
        var ctl = window.BRIM_SWRCB_POD_LOCAL;
        if (ctl && typeof ctl.resetFilters === 'function') ctl.resetFilters();
      }
    });
  }

  function updateLegend(evt) {
    if (evt && !eventMatches(evt) && evt.type !== 'pt:swrcbfilterchange') return;

    // The close button hides this shared legend for the current view, but any
    // explicit SWRCB source-layer toggle re-shows it. Filter changes alone do
    // not re-open a legend the user intentionally closed.
    if (evt && eventMatches(evt) && evt.type !== 'pt:swrcbfilterchange') {
      legendUserHidden = false;
    }

    var container = map && map.getContainer ? map.getContainer() : el;
    var div = (container && container.querySelector) ? container.querySelector('.pt-swrcb-pod-legend') : null;
    if (!div && el && el.querySelector) div = el.querySelector('.pt-swrcb-pod-legend');
    if (!div) return;
    var active = getActiveSources();
    var activeAny = anyActive(active);
    div.style.display = (activeAny && !legendUserHidden) ? 'block' : 'none';
    if (activeAny) {
      div.innerHTML = buildHtml(active);
      attachEvents(div);
    } else {
      legendUserHidden = false;
    }
  }

  window.BRIM_SWRCB_POD_LOCAL_REFRESH_LEGEND = updateLegend;

  map.on('overlayadd overlayremove pt:swrcbfilterchange', updateLegend);
  if (typeof document !== 'undefined' && document.addEventListener) {
    document.addEventListener('change', function(evt) {
      var t = evt && evt.target;
      if (t && t.matches && t.matches('.leaflet-control-layers-overlays input[type="checkbox"]')) {
        var label = t.closest ? t.closest('label') : null;
        var text = label ? (label.textContent || label.innerText || '') : '';
        if (sourceForText(text)) {
          legendUserHidden = false;
          window.setTimeout(updateLegend, 0);
        }
      }
    }, true);
  }
  setTimeout(updateLegend, 0);
  setTimeout(updateLegend, 300);
  setTimeout(updateLegend, 1000);
}
)---"

  htmlwidgets::onRender(
    m,
    js,
    data = list(
      groups = list(official_group_name, spatial_group_name, name_candidate_group_name),
      counts = counts
    )
  )
}

