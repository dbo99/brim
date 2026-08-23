# ==== leaflet_layer_local_usgs_helpers.r ================================================
##
## PURPOSE:
##   USGS groundwater/streamgage local legends and browser-managed local USGS layers.
##
## NOTE:
##   Extracted from leaflet_layer_helpers.r as a maintainability-only split.
##   Function names and behavior are intentionally unchanged.

# ==== 2A. USGS groundwater Local catalog legend ==============================

pt_add_usgs_well_catalog_legend <- function(m, usgs_gw = NULL, map_display = NULL) {

  if (is.null(usgs_gw) || !inherits(usgs_gw, "sf") || nrow(usgs_gw) == 0) {
    return(m)
  }

  group_name <- pt_layer_group_name("USGS monitoring wells")

  pt_bool <- function(v) {
    if (is.logical(v)) return(dplyr::coalesce(v, FALSE))
    raw <- tolower(trimws(as.character(v)))
    raw %in% c("true", "t", "1", "yes", "y")
  }

  pt_site_no_norm <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- gsub("\\.0$", "", x)
    x <- gsub("[^0-9]", "", x)
    x[x == ""] <- NA_character_
    x
  }

  pt_num_col <- function(df, candidates) {
    for (cc in candidates) {
      if (cc %in% names(df)) return(suppressWarnings(as.numeric(gsub(",", "", as.character(df[[cc]])))))
    }
    rep(NA_real_, nrow(df))
  }

  latest_wl <- if ("gwpop_latest_wl" %in% names(usgs_gw)) {
    suppressWarnings(as.numeric(usgs_gw$gwpop_latest_wl))
  } else if ("latest_wl_ft_bgs" %in% names(usgs_gw)) {
    suppressWarnings(as.numeric(usgs_gw$latest_wl_ft_bgs))
  } else {
    rep(NA_real_, nrow(usgs_gw))
  }

  in_ops <- if ("gwpop_in_ops" %in% names(usgs_gw)) {
    pt_bool(usgs_gw$gwpop_in_ops)
  } else if ("well_recent_feed_ring" %in% names(usgs_gw)) {
    pt_bool(usgs_gw$well_recent_feed_ring)
  } else {
    rep(FALSE, nrow(usgs_gw))
  }

  is_nested <- if ("well_is_nested" %in% names(usgs_gw)) {
    pt_bool(usgs_gw$well_is_nested)
  } else if ("gwpop_nested_n" %in% names(usgs_gw)) {
    suppressWarnings(as.integer(usgs_gw$gwpop_nested_n)) > 1L
  } else {
    rep(FALSE, nrow(usgs_gw))
  }
  is_nested[is.na(is_nested)] <- FALSE

  elev_ft <- pt_num_col(usgs_gw, c("gwpop_elev_ft", "elev_ft", "elevation_ft"))
  well_depth_ft <- pt_num_col(usgs_gw, c("gwpop_well_depth", "well_depth_ft"))
  dist_to_blm_mi <- pt_num_col(usgs_gw, c("dist_to_blm_mi", "distance_to_blm_mi", "gw_dist_to_blm_mi"))
  on_blm <- if ("on_blm_ca" %in% names(usgs_gw)) {
    pt_bool(usgs_gw$on_blm_ca)
  } else if ("on_blm" %in% names(usgs_gw)) {
    pt_bool(usgs_gw$on_blm)
  } else if ("gw_on_blm_ca" %in% names(usgs_gw)) {
    pt_bool(usgs_gw$gw_on_blm_ca)
  } else {
    rep(FALSE, nrow(usgs_gw))
  }

  counts <- list(
    total = nrow(usgs_gw),
    ops = sum(in_ops, na.rm = TRUE),
    nested = sum(is_nested, na.rm = TRUE),
    no_wl = sum(is.na(latest_wl)),
    has_wl = sum(!is.na(latest_wl)),
    artesian = sum(!is.na(latest_wl) & latest_wl < 0),
    bin_0_25 = sum(!is.na(latest_wl) & latest_wl >= 0 & latest_wl < 25),
    bin_25_100 = sum(!is.na(latest_wl) & latest_wl >= 25 & latest_wl < 100),
    bin_100_250 = sum(!is.na(latest_wl) & latest_wl >= 100 & latest_wl < 250),
    bin_250_500 = sum(!is.na(latest_wl) & latest_wl >= 250 & latest_wl < 500),
    bin_500_1000 = sum(!is.na(latest_wl) & latest_wl >= 500 & latest_wl < 1000),
    bin_gt_1000 = sum(!is.na(latest_wl) & latest_wl >= 1000),
    elev_available = sum(!is.na(elev_ft)),
    well_depth_available = sum(!is.na(well_depth_ft)),
    blm_distance_available = sum(!is.na(dist_to_blm_mi) | on_blm, na.rm = TRUE),
    on_blm = sum(on_blm, na.rm = TRUE)
  )

  ## Lightweight exact-ID audit against the Ops Live candidate CSV, when present.
  ## This clarifies the expected mismatch between the Local CA catalog domain and
  ## the broader Ops Live groundwater query area without changing either domain.
  audit <- list(
    live_csv_present = FALSE,
    live_unique_sites = NA_integer_,
    live_sites_matching_local = NA_integer_,
    live_sites_missing_from_local = NA_integer_,
    audit_note = "Ops Live candidate CSV not found during final-map build."
  )

  live_path <- file.path(
    "brim-live-data-feeds",
    "data",
    "input",
    "usgs_groundwater_latest_index_ca.csv"
  )

  if (file.exists(live_path) && "site_no" %in% names(usgs_gw)) {
    live <- try(
      utils::read.csv(live_path, stringsAsFactors = FALSE, colClasses = "character"),
      silent = TRUE
    )
    if (!inherits(live, "try-error") && nrow(live) > 0) {
      live_site_col <- c("site_no", "monitoring_location_id", "site")
      live_site_col <- live_site_col[live_site_col %in% names(live)][1]
      if (!is.na(live_site_col)) {
        local_ids <- unique(na.omit(pt_site_no_norm(usgs_gw$site_no)))
        live_ids <- unique(na.omit(pt_site_no_norm(live[[live_site_col]])))
        missing_ids <- setdiff(live_ids, local_ids)
        matched_ids <- intersect(live_ids, local_ids)

        audit <- list(
          live_csv_present = TRUE,
          live_unique_sites = length(live_ids),
          live_sites_matching_local = length(matched_ids),
          live_sites_missing_from_local = length(missing_ids),
          audit_note = "Pink count is Local catalog records also found in the Ops Live candidate index; Ops Live uses a broader border-state query area, so totals are expected to differ."
        )

        qa_dir <- if (exists("DIR", inherits = TRUE) && !is.null(get("DIR", inherits = TRUE)$qa)) {
          get("DIR", inherits = TRUE)$qa
        } else {
          file.path("04_processed_data", "qa")
        }
        dir.create(qa_dir, recursive = TRUE, showWarnings = FALSE)

        summary_tbl <- data.frame(
          metric = c(
            "local_catalog_records",
            "local_catalog_records_flagged_in_ops_live",
            "ops_live_unique_candidate_sites",
            "ops_live_sites_matching_local_catalog",
            "ops_live_sites_missing_from_local_catalog",
            "local_nested_or_colocated_records",
            "local_records_with_cached_water_level",
            "local_records_with_blm_distance_fields",
            "local_records_on_blm"
          ),
          value = as.character(c(
            counts$total,
            counts$ops,
            audit$live_unique_sites,
            audit$live_sites_matching_local,
            audit$live_sites_missing_from_local,
            counts$nested,
            counts$has_wl,
            counts$blm_distance_available,
            counts$on_blm
          )),
          note = c(
            "Rows in the Local USGS monitoring wells catalog currently loaded for map build.",
            "Local rows flagged by exact site_no join to the Ops Live candidate index.",
            "Unique site_no values in brim-live-data-feeds/data/input/usgs_groundwater_latest_index_ca.csv.",
            "Unique Ops Live site_no values found in the Local catalog.",
            "Likely domain difference; Ops Live includes Nevada/Oregon border-context beyond Local CA catalog.",
            "Local rows whose mapped coordinate is shared by multiple USGS well records.",
            "Rows with cached/latest groundwater-level depth available in the Local catalog.",
            "Rows carrying BLM distance/on-BLM fields in the current Local Wells cache.",
            "Rows flagged on BLM where BLM relationship fields are available."
          ),
          stringsAsFactors = FALSE
        )
        utils::write.csv(
          summary_tbl,
          file.path(qa_dir, "usgs_groundwater_local_ops_overlap_audit_latest.csv"),
          row.names = FALSE
        )

        if (length(missing_ids) > 0) {
          miss <- live[pt_site_no_norm(live[[live_site_col]]) %in% missing_ids, , drop = FALSE]
          keep_miss <- intersect(
            c("site_no", "monitoring_location_id", "station_nm", "station_name", "state", "state_cd", "dec_lat_va", "dec_long_va", "latitude", "longitude", "latest_wl_ft_bgs", "latest_wl_date", "latest_age_days"),
            names(miss)
          )
          if (length(keep_miss) > 0) miss <- miss[, keep_miss, drop = FALSE]
          utils::write.csv(
            miss,
            file.path(qa_dir, "usgs_groundwater_ops_sites_missing_from_local_latest.csv"),
            row.names = FALSE
          )
        }

        message(
          "USGS groundwater Local/Ops audit: Local records flagged in Ops Live = ",
          format(counts$ops, big.mark = ","),
          "; Ops Live unique candidate sites = ",
          format(audit$live_unique_sites, big.mark = ","),
          "; exact unique site matches = ",
          format(audit$live_sites_matching_local, big.mark = ","),
          "; Ops sites missing from Local = ",
          format(audit$live_sites_missing_from_local, big.mark = ","),
          ". QA: ",
          file.path(qa_dir, "usgs_groundwater_local_ops_overlap_audit_latest.csv")
        )
      }
    }
  }

  counts$ops_live_total <- audit$live_unique_sites
  counts$ops_live_matching_local <- audit$live_sites_matching_local
  counts$ops_live_missing_local <- audit$live_sites_missing_from_local
  counts$ops_domain_note <- audit$audit_note

  group_js <- jsonlite::toJSON(group_name, auto_unbox = TRUE)
  counts_js <- jsonlite::toJSON(counts, auto_unbox = TRUE, null = "null", na = "null")

  js <- r"---(
function(el, x) {
  var map = this;
  var targetGroup = __TARGET_GROUP__;
  var counts = __COUNTS__ || {};
  var filters = {
    opsOnly:false,
    nestedOnly:false,
    wlMode:'all',
    wlAgeMax:null,
    dtwMin:null,
    dtwMax:null,
    wellDepthMin:null,
    wellDepthMax:null,
    minPor:null,
    startMax:null,
    endMin:null,
    blmMode:'any',
    blmMax:null,
    elevMin:null,
    elevMax:null
  };
  var lastShown = counts.total || 0;
  var lastDrawn = null;
  var blmAvailable = Number(counts.blm_distance_available || 0) > 0;

  // Closeout for the USGS groundwater Local catalog legend.  This hides only
  // the legend and preserves filters/drawn state.  Re-open only when the USGS
  // Wells source layer itself is toggled off/on, not when unrelated layers move.
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

  function num(v) {
    if (v == null || v === '') return null;
    var n = Number(v);
    return isNaN(n) ? null : n;
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

  function isTargetText(s) {
    var n = norm(s);
    return n.indexOf('usgs wells') >= 0 || n.indexOf('usgs monitoring wells') >= 0;
  }

  function controlChecked() {
    if (typeof document === 'undefined' || !document.querySelectorAll) return false;
    var labels = document.querySelectorAll('.leaflet-control-layers-overlays label');
    for (var i = 0; i < labels.length; i++) {
      var label = labels[i];
      var text = label.textContent || label.innerText || '';
      if (!isTargetText(text)) continue;
      var input = label.querySelector ? label.querySelector('input[type="checkbox"]') : null;
      if (input) return !!input.checked;
    }
    return false;
  }

  function eventMatches(evt) {
    if (!evt) return false;
    if (isTargetText(evt.name)) return true;
    if (evt.layer && evt.layer.options) {
      var bits = [evt.layer.options.group, evt.layer.options.name, evt.layer.options.layerId].join(' ');
      return isTargetText(bits);
    }
    return false;
  }

  function row(sym, label, count) {
    return '<div class="pt-usgs-gw-local-row">' + sym +
      '<span class="pt-usgs-gw-local-label">' + esc(label) + '</span>' +
      '<span class="pt-usgs-gw-local-count">' + fmt(count) + '</span>' +
      '</div>';
  }

  function dot(fill, stroke) { return '<span class="pt-usgs-gw-local-dot" style="background:' + fill + ';border-color:' + stroke + ';"></span>'; }
  function ring(stroke) { return '<span class="pt-usgs-gw-local-ring" style="border-color:' + stroke + ';"></span>'; }
  function nestedSym() { return '<span class="pt-usgs-gw-local-nested"><i></i><i></i><i></i><i></i></span>'; }

  function btn(label, action, selected) {
    return '<button type="button" data-pt-gw-action="' + action + '" class="pt-usgs-gw-local-btn' + (selected ? ' active' : '') + '">' + esc(label) + '</button>';
  }

  function input(id, value, placeholder, width) {
    return '<input id="' + id + '" type="number" value="' + (value == null ? '' : esc(value)) + '" placeholder="' + esc(placeholder) + '" style="width:' + width + 'px;">';
  }

  function filterSummaryText() {
    var bits = [];
    if (filters.opsOnly) bits.push('Ops Live subset');
    if (filters.nestedOnly) bits.push('nested/co-located');
    if (filters.wlMode === 'has') bits.push('has cached WL');
    if (filters.wlMode === 'none') bits.push('no cached WL');
    if (filters.wlMode === 'artesian') bits.push('artesian/<0');
    if (filters.wlAgeMax != null) bits.push('WL≤' + filters.wlAgeMax + 'd old');
    if (filters.dtwMin != null && filters.dtwMax != null) bits.push('DTW ' + filters.dtwMin + '–' + filters.dtwMax);
    else if (filters.dtwMin != null) bits.push('DTW≥' + filters.dtwMin);
    else if (filters.dtwMax != null) bits.push('DTW≤' + filters.dtwMax);
    if (filters.wellDepthMin != null || filters.wellDepthMax != null) bits.push('well depth');
    if (filters.minPor != null) bits.push('POR≥' + filters.minPor + 'y');
    if (filters.startMax != null && filters.endMin != null) bits.push('covers WY' + filters.startMax + '–' + filters.endMin);
    else if (filters.startMax != null) bits.push('starts by WY' + filters.startMax);
    else if (filters.endMin != null) bits.push('ends after WY' + filters.endMin);
    if (blmAvailable && filters.blmMode === 'on') bits.push('on BLM');
    if (blmAvailable && filters.blmMode === 'distance') bits.push('≤' + filters.blmMax + ' mi BLM');
    if (filters.elevMin != null || filters.elevMax != null) bits.push('elev ft');
    return bits.length ? bits.join(' · ') : 'none';
  }

  function setInputValues(div) {
    var ids = {
      'pt-usgs-gw-wlage': filters.wlAgeMax,
      'pt-usgs-gw-dtwmin': filters.dtwMin,
      'pt-usgs-gw-dtwmax': filters.dtwMax,
      'pt-usgs-gw-wdmin': filters.wellDepthMin,
      'pt-usgs-gw-wdmax': filters.wellDepthMax,
      'pt-usgs-gw-minpor': filters.minPor,
      'pt-usgs-gw-startmax': filters.startMax,
      'pt-usgs-gw-endmin': filters.endMin,
      'pt-usgs-gw-blmmax': filters.blmMode === 'distance' ? filters.blmMax : null,
      'pt-usgs-gw-elevmin': filters.elevMin,
      'pt-usgs-gw-elevmax': filters.elevMax
    };
    Object.keys(ids).forEach(function(id) { var el = div.querySelector('#' + id); if (el) el.value = ids[id] == null ? '' : ids[id]; });
  }

  function readTextFilters(div) {
    filters.wlAgeMax = num((div.querySelector('#pt-usgs-gw-wlage') || {}).value);
    filters.dtwMin = num((div.querySelector('#pt-usgs-gw-dtwmin') || {}).value);
    filters.dtwMax = num((div.querySelector('#pt-usgs-gw-dtwmax') || {}).value);
    filters.wellDepthMin = num((div.querySelector('#pt-usgs-gw-wdmin') || {}).value);
    filters.wellDepthMax = num((div.querySelector('#pt-usgs-gw-wdmax') || {}).value);
    filters.minPor = num((div.querySelector('#pt-usgs-gw-minpor') || {}).value);
    filters.startMax = num((div.querySelector('#pt-usgs-gw-startmax') || {}).value);
    filters.endMin = num((div.querySelector('#pt-usgs-gw-endmin') || {}).value);
    filters.elevMin = num((div.querySelector('#pt-usgs-gw-elevmin') || {}).value);
    filters.elevMax = num((div.querySelector('#pt-usgs-gw-elevmax') || {}).value);
    var blmMax = num((div.querySelector('#pt-usgs-gw-blmmax') || {}).value);
    if (!blmAvailable) {
      filters.blmMode = 'any';
      filters.blmMax = null;
    } else if (blmMax != null) {
      filters.blmMode = 'distance';
      filters.blmMax = blmMax;
    } else if (filters.blmMode === 'distance') {
      filters.blmMode = 'any';
      filters.blmMax = null;
    }
  }

  function applyFiltersToBrowserLayer() {
    if (window.BRIM_USGS_GW_LOCAL && typeof window.BRIM_USGS_GW_LOCAL.applyFilters === 'function') {
      var res = window.BRIM_USGS_GW_LOCAL.applyFilters(filters);
      if (res && res.filtered != null) lastShown = res.filtered;
      if (res && res.drawn != null) lastDrawn = res.drawn;
    }
  }

  function refreshStatsFromBrowserLayer() {
    if (window.BRIM_USGS_GW_LOCAL && typeof window.BRIM_USGS_GW_LOCAL.stats === 'function') {
      var res = window.BRIM_USGS_GW_LOCAL.stats();
      if (res && res.filtered != null) lastShown = res.filtered;
      if (res && res.drawn != null) lastDrawn = res.drawn;
    }
  }

  function defaultFilters() {
    filters = {opsOnly:false,nestedOnly:false,wlMode:'all',wlAgeMax:null,dtwMin:null,dtwMax:null,wellDepthMin:null,wellDepthMax:null,minPor:null,startMax:null,endMin:null,blmMode:'any',blmMax:null,elevMin:null,elevMax:null};
  }

  function smallCheck(id, checked, label) {
    return '<label class="pt-usgs-gw-local-check"><input id="' + id + '" type="checkbox" ' + (checked ? 'checked' : '') + '> ' + esc(label) + '</label>';
  }

  function filtersHtml() {
    var h = '';
    h += '<div class="pt-usgs-gw-filter-box">';
    h += '<div class="pt-usgs-gw-filter-title">Filters</div>';
    h += '<div class="pt-usgs-gw-filter-line"><span class="pt-usgs-gw-filter-label">Ops:</span>' + btn('all','ops-all',!filters.opsOnly) + btn('Ops Live','ops-only',filters.opsOnly) + '<span class="pt-usgs-gw-filter-label pt-usgs-gw-filter-label-inline">Latest WL:</span>' + btn('all','wl-all',filters.wlMode==='all') + btn('has','wl-has',filters.wlMode==='has') + btn('no','wl-none',filters.wlMode==='none') + btn('artesian','wl-artesian',filters.wlMode==='artesian') + '</div>';
    h += '<div class="pt-usgs-gw-filter-line"><span class="pt-usgs-gw-filter-label">Latest WL age:</span>' + input('pt-usgs-gw-wlage', filters.wlAgeMax, 'days', 43) + ' max days ' + btn('5y','wlage-5y',filters.wlAgeMax===1825) + btn('10y','wlage-10y',filters.wlAgeMax===3650) + '</div>';
    h += '<div class="pt-usgs-gw-filter-line"><span class="pt-usgs-gw-filter-label">Latest DTW:</span>' + input('pt-usgs-gw-dtwmin', filters.dtwMin, 'min', 43) + '–' + input('pt-usgs-gw-dtwmax', filters.dtwMax, 'max', 43) + ' ft bgs ' + btn('≤25','dtw25',filters.dtwMax===25 && filters.dtwMin==null) + btn('25–100','dtw25_100',filters.dtwMin===25 && filters.dtwMax===100) + btn('≥500','dtw500',filters.dtwMin===500 && filters.dtwMax==null) + '</div>';
    h += '<div class="pt-usgs-gw-filter-line"><span class="pt-usgs-gw-filter-label">Well depth:</span>' + input('pt-usgs-gw-wdmin', filters.wellDepthMin, 'min', 43) + '–' + input('pt-usgs-gw-wdmax', filters.wellDepthMax, 'max', 43) + ' ft ' + smallCheck('pt-usgs-gw-nestedonly', filters.nestedOnly, 'nested only') + '</div>';
    h += '<div class="pt-usgs-gw-filter-line"><span class="pt-usgs-gw-filter-label">POR:</span>yrs≥' + input('pt-usgs-gw-minpor', filters.minPor, 'yrs', 35) + ' covers WY ' + input('pt-usgs-gw-startmax', filters.startMax, 'from', 44) + '–' + input('pt-usgs-gw-endmin', filters.endMin, 'to', 44) + ' ' + btn('≥20','por20',filters.minPor===20) + btn('WY1995','wy1995',filters.startMax===1995 && filters.endMin===1995) + '</div>';
    if (blmAvailable) {
      h += '<div class="pt-usgs-gw-filter-line"><span class="pt-usgs-gw-filter-label">BLM:</span>' + input('pt-usgs-gw-blmmax', filters.blmMode==='distance' ? filters.blmMax : null, 'mi', 39) + ' mi ' + btn('any','blm-any',filters.blmMode==='any') + btn('on BLM','blm-on',filters.blmMode==='on') + btn('≤1','blm1',filters.blmMode==='distance' && filters.blmMax===1) + btn('≤5','blm5',filters.blmMode==='distance' && filters.blmMax===5) + '</div>';
    } else {
      h += '<div class="pt-usgs-gw-filter-line pt-usgs-gw-filter-muted"><span class="pt-usgs-gw-filter-label">BLM:</span> distance cache not yet available for this Local catalog</div>';
    }
    h += '<div class="pt-usgs-gw-filter-line"><span class="pt-usgs-gw-filter-label">Elev:</span>' + input('pt-usgs-gw-elevmin', filters.elevMin, 'min', 43) + '–' + input('pt-usgs-gw-elevmax', filters.elevMax, 'max', 43) + ' ft <span class="pt-usgs-gw-filter-actions">' + btn('Apply','apply',false) + btn('Reset','reset',false) + '</span></div>';
    h += '</div>';
    return h;
  }

  function buildHtml() {
    refreshStatsFromBrowserLayer();
    var html = '';
    html += '<div class="pt-usgs-gw-local-head pt-map-card-handle"><div class="pt-usgs-gw-local-title">USGS monitoring wells catalog</div><span class="pt-map-card-actions"><button type="button" class="pt-map-card-dock pt-usgs-gw-local-dock" aria-label="Undock USGS groundwater catalog legend" title="Undock USGS groundwater catalog legend">&#x2197;</button><button type="button" class="pt-usgs-gw-local-close" aria-label="Hide USGS groundwater catalog legend" title="Hide USGS groundwater catalog legend">&times;</button></span></div>';
    html += '<div class="pt-usgs-gw-local-sub">Static Local catalog. Color = most recent cached groundwater level (MR WL). Use Ops Live for current values.</div>';
    var statObj = (window.BRIM_USGS_GW_LOCAL && window.BRIM_USGS_GW_LOCAL.stats) ? window.BRIM_USGS_GW_LOCAL.stats() : null;
    var statusText = statObj && statObj.status ? String(statObj.status) : '';
    var statusBusy = !!(statObj && statObj.loading);
    if (statusBusy && !statusText) statusText = 'Rendering groundwater wells…';
    html += '<div class="pt-usgs-gw-local-showing">Showing ' + fmt(lastShown) + ' / ' + fmt(counts.total) + ' well records' + (lastDrawn != null ? ' (' + fmt(lastDrawn) + ' display objects in current view)' : '') + '</div>';
    if (statusText) html += '<div class="pt-usgs-gw-local-status' + (statusBusy ? ' busy' : '') + '"><span class="pt-usgs-gw-local-spinner"></span><span>' + esc(statusText) + '</span></div>';
    html += '<div class="pt-usgs-gw-local-filter-summary">Filters: ' + esc(filterSummaryText()) + '</div>';
    html += '<div class="pt-usgs-gw-local-minihead">Most recent cached groundwater level</div>';
    html += '<div class="pt-usgs-gw-local-bin-grid">';
    html += row(dot('#756BB1', '#4A1486'), 'Reported artesian / <0 ft bgs', counts.artesian);
    html += row(dot('#2B8CBE', '#08589E'), '0–25 ft bgs', counts.bin_0_25);
    html += row(dot('#7BCCC4', '#08589E'), '25–100 ft bgs', counts.bin_25_100);
    html += row(dot('#A1D99B', '#238B45'), '100–250 ft bgs', counts.bin_100_250);
    html += row(dot('#FEE391', '#B8860B'), '250–500 ft bgs', counts.bin_250_500);
    html += row(dot('#FEC44F', '#A63603'), '500–1,000 ft bgs', counts.bin_500_1000);
    html += row(dot('#BD0026', '#7F0000'), '>1,000 ft bgs', counts.bin_gt_1000);
    if (Number(counts.no_wl || 0) > 0) html += row(dot('#F2F2F2', '#BDBDBD'), 'No cached WL', counts.no_wl);
    html += '</div>';
    html += '<div class="pt-usgs-gw-local-minihead">Catalog cues</div>';
    html += row(ring('#ff00cc'), 'In Ops Live', counts.ops);
    html += row(nestedSym(), 'Nested / co-located records', counts.nested);
    html += '<div class="pt-usgs-gw-local-note">Pink count = Local subset also in Ops Live; Ops Live uses a broader NV/OR border-context domain. ' + (blmAvailable ? ('BLM filter fields present for ' + fmt(counts.blm_distance_available) + ' records.') : 'BLM proximity filtering needs a Local GW distance cache.') + '</div>';
    html += filtersHtml();
    return html;
  }

  function attachEvents(div) {
    if (window.BRIM && window.BRIM.legendCloseout && window.BRIM.legendCloseout.makeDetachable) {
      window.BRIM.legendCloseout.makeDetachable({card: div, map: map, handleSelector: '.pt-usgs-gw-local-head', dockSelector: '.pt-usgs-gw-local-dock', label: 'USGS groundwater catalog legend'});
    }
    if (window.BRIM && window.BRIM.legendCloseout) {
      window.BRIM.legendCloseout.wire(div, '.pt-usgs-gw-local-close', function(){
        legendUserHidden = true;
      });
    } else {
      var closeBtn = div.querySelector('.pt-usgs-gw-local-close');
      if (closeBtn) {
        closeBtn.addEventListener('click', function(e) {
          if (e) { e.preventDefault(); e.stopPropagation(); }
          legendUserHidden = true;
          div.style.display = 'none';
        }, false);
      }
    }

    var nestedBox = div.querySelector('#pt-usgs-gw-nestedonly');
    if (nestedBox) {
      nestedBox.addEventListener('change', function(e) {
        if (e) { e.preventDefault(); e.stopPropagation(); }
        filters.nestedOnly = !!nestedBox.checked;
        applyFiltersToBrowserLayer();
        updateLegend();
      }, false);
    }

    var buttons = div.querySelectorAll('[data-pt-gw-action]');
    buttons.forEach(function(b) {
      b.addEventListener('click', function(e) {
        if (e) { e.preventDefault(); e.stopPropagation(); }
        var a = b.getAttribute('data-pt-gw-action');
        if (a === 'ops-all') filters.opsOnly = false;
        if (a === 'ops-only') filters.opsOnly = true;
        if (a === 'wl-all') filters.wlMode = 'all';
        if (a === 'wl-has') filters.wlMode = 'has';
        if (a === 'wl-none') filters.wlMode = 'none';
        if (a === 'wl-artesian') filters.wlMode = 'artesian';
        if (a === 'wlage-5y') filters.wlAgeMax = 1825;
        if (a === 'wlage-10y') filters.wlAgeMax = 3650;
        if (a === 'dtw25') { filters.dtwMin = null; filters.dtwMax = 25; filters.wlMode = 'has'; }
        if (a === 'dtw25_100') { filters.dtwMin = 25; filters.dtwMax = 100; filters.wlMode = 'has'; }
        if (a === 'dtw500') { filters.dtwMin = 500; filters.dtwMax = null; filters.wlMode = 'has'; }
        if (a === 'por20') filters.minPor = 20;
        if (a === 'wy1995') { filters.startMax = 1995; filters.endMin = 1995; }
        if (!blmAvailable && a.indexOf('blm') === 0) { filters.blmMode = 'any'; filters.blmMax = null; }
        else if (a === 'blm-any') { filters.blmMode = 'any'; filters.blmMax = null; }
        else if (a === 'blm-on') { filters.blmMode = 'on'; filters.blmMax = null; }
        else if (a === 'blm1') { filters.blmMode = 'distance'; filters.blmMax = 1; }
        else if (a === 'blm5') { filters.blmMode = 'distance'; filters.blmMax = 5; }
        if (a === 'apply') readTextFilters(div);
        if (a === 'reset') defaultFilters();
        applyFiltersToBrowserLayer();
        updateLegend();
      }, false);
    });
  }

  var old = document.querySelector('.pt-usgs-gw-local-legend');
  if (old && old.parentNode) old.parentNode.removeChild(old);

  var legend = L.control({position: 'bottomleft'});
  legend.onAdd = function(map) {
    var div = L.DomUtil.create('div', 'leaflet-control pt-usgs-gw-local-legend');
    div.style.display = 'none';
    div.style.marginBottom = '74px';
    div.innerHTML = buildHtml();
    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);
    return div;
  };
  legend.addTo(map);

  if (!document.getElementById('pt-usgs-gw-local-legend-style')) {
    var style = document.createElement('style');
    style.id = 'pt-usgs-gw-local-legend-style';
    style.textContent =
      '.pt-usgs-gw-local-legend{background:rgba(246,239,222,0.96);border:1px solid rgba(112,103,83,0.55);border-radius:7px;box-shadow:0 1px 5px rgba(0,0,0,0.25);padding:6px 8px 7px 8px;max-width:380px;width:365px;font-family:Arial,sans-serif;font-size:10.4px;line-height:1.14;color:#222;margin-bottom:74px;position:relative;}' +
      '.pt-usgs-gw-local-head{display:flex;align-items:flex-start;justify-content:space-between;gap:8px;}' +
      '.pt-usgs-gw-local-legend.pt-map-card-undocked .pt-usgs-gw-local-head{cursor:move;}' +
      '.pt-usgs-gw-local-close,.pt-usgs-gw-local-dock{border:0;background:transparent;color:#776f61;font-weight:700;font-size:14px;line-height:1;padding:0 2px;cursor:pointer;}' +
      '.pt-usgs-gw-local-close:hover{color:#222;background:rgba(112,103,83,0.12);border-radius:3px;}' +
      '.pt-usgs-gw-local-dock:hover{color:#222;background:rgba(112,103,83,0.12);border-radius:3px;}' +
      '.pt-usgs-gw-local-title{font-weight:700;font-size:12.4px;margin:0 0 2px 0;}' +
      '.pt-usgs-gw-local-sub{font-size:10.2px;color:#4d4d4d;margin:0 0 3px 0;}' +
      '.pt-usgs-gw-local-minihead{font-weight:700;font-size:10.6px;margin:4px 0 1px 0;color:#333;}' +
      '.pt-usgs-gw-local-bin-grid{display:grid;grid-template-columns:1fr 1fr;column-gap:12px;}' +
      '.pt-usgs-gw-local-row{display:grid;grid-template-columns:13px minmax(0,1fr) auto;align-items:center;column-gap:5px;margin:1px 0;}' +
      '.pt-usgs-gw-local-dot{width:9px;height:9px;border:1.3px solid #666;border-radius:50%;box-sizing:border-box;display:inline-block;}' +
      '.pt-usgs-gw-local-ring{width:11px;height:11px;border:1.3px solid #ff00cc;border-radius:50%;box-sizing:border-box;display:inline-block;background:transparent;}' +
      '.pt-usgs-gw-local-nested{display:inline-grid;grid-template-columns:repeat(2,2.2px);grid-template-rows:repeat(2,2.2px);place-content:center;gap:2.3px;width:12px;height:12px;border-radius:4px;border:1.3px solid #7B241C;background:rgba(246,190,103,0.72);box-sizing:border-box;}' +
      '.pt-usgs-gw-local-nested i{display:block;width:2.2px;height:2.2px;border-radius:50%;background:#222;opacity:0.80;}' +
      '.pt-usgs-gw-local-label{white-space:nowrap;overflow:hidden;text-overflow:ellipsis;}' +
      '.pt-usgs-gw-local-count{font-variant-numeric:tabular-nums;color:#333;font-weight:600;}' +
      '.pt-usgs-gw-local-showing,.pt-usgs-gw-local-filter-summary{font-size:10.1px;color:#4d4d4d;margin-top:2px;}' +
      '.pt-usgs-gw-local-status{display:flex;align-items:center;gap:5px;font-size:10px;color:#5a4a24;background:rgba(255,248,220,0.78);border:1px solid rgba(155,134,74,0.42);border-radius:5px;padding:2px 5px;margin:3px 0 2px 0;}' +
      '.pt-usgs-gw-local-spinner{width:9px;height:9px;border:2px solid rgba(123,36,28,0.22);border-top-color:#7B241C;border-radius:50%;display:none;box-sizing:border-box;animation:pt-usgs-gw-local-spin 0.8s linear infinite;}' +
      '.pt-usgs-gw-local-status.busy .pt-usgs-gw-local-spinner{display:inline-block;flex:0 0 auto;}' +
      '@keyframes pt-usgs-gw-local-spin{to{transform:rotate(360deg);}}' +
      '.pt-usgs-gw-local-note{font-size:9.4px;color:#4d4d4d;margin-top:3px;}' +
      '.pt-usgs-gw-filter-box{margin-top:4px;border-top:1px solid rgba(120,110,90,0.28);padding-top:4px;}' +
      '.pt-usgs-gw-filter-title{font-weight:700;font-size:10.7px;margin-bottom:1px;}' +
      '.pt-usgs-gw-filter-line{display:flex;align-items:center;flex-wrap:wrap;gap:1px;margin:1px 0;}' +
      '.pt-usgs-gw-filter-label{font-weight:700;margin-right:3px;}' +
      '.pt-usgs-gw-filter-label-inline{margin-left:7px;}' +
      '.pt-usgs-gw-filter-muted{font-size:9.7px;color:#666;}' +
      '.pt-usgs-gw-filter-actions{margin-left:6px;}' +
      '.pt-usgs-gw-filter-box input[type="number"]{font-size:10px;line-height:1.0;padding:1px 3px;margin:0 1px;border:1px solid #aaa;border-radius:3px;background:#fff;}' +
      '.pt-usgs-gw-local-check{font-size:10.2px;margin-left:5px;white-space:nowrap;}' +
      '.pt-usgs-gw-local-check input{vertical-align:-1px;margin:0 2px 0 0;}' +
      '.pt-usgs-gw-local-btn{font-size:10px;line-height:1.0;padding:1px 4px;margin:0 1px 1px 0;border-radius:4px;border:1px solid #9b9278;background:#fbf8ec;cursor:pointer;}' +
      '.pt-usgs-gw-local-btn.active{background:#d7c592;font-weight:700;}';
    document.head.appendChild(style);
  }

  function updateLegend(evt) {
    if (evt && !eventMatches(evt) && evt.type !== 'zoomend' && evt.type !== 'pt:usgsgwlocalstatus') return;
    var div = document.querySelector('.pt-usgs-gw-local-legend');
    if (!div) return;
    if (!controlChecked()) {
      if (div.__brimDetachableState && div.__brimDetachableState.destroy) div.__brimDetachableState.destroy(false);
      div.style.display = 'none';
      legendUserHidden = false;
      return;
    }
    div.innerHTML = buildHtml();
    div.style.display = legendUserHidden ? 'none' : 'block';
    setInputValues(div);
    attachEvents(div);
  }

  map.on('overlayadd', function(evt) { if (eventMatches(evt)) legendUserHidden = false; updateLegend(evt); });
  map.on('overlayremove', function(evt) { if (eventMatches(evt)) legendUserHidden = false; updateLegend(evt); });
  map.on('layeradd layerremove pt:usgsgwlocalstatus', updateLegend);
  window.BRIM_USGS_GW_LOCAL_REFRESH_LEGEND = updateLegend;
  setTimeout(updateLegend, 0);
  setTimeout(updateLegend, 300);
  setTimeout(updateLegend, 1000);
}
  )---"

  js <- gsub("__TARGET_GROUP__", group_js, js, fixed = TRUE)
  js <- gsub("__COUNTS__", counts_js, js, fixed = TRUE)

  htmlwidgets::onRender(m, js)
}


# ==== 2B. USGS streamgage Local catalog legend ==============================
##
## PURPOSE:
##   Add a compact Local-panel-style legend for the Local USGS streamgage
##   catalog.  This layer is a catalog/history layer, not the current-value Ops
##   layer.  Active/inactive status and period-of-record context belong here;
##   current/recent discharge or stage values belong in Ops Live USGS streamflow.

pt_add_usgs_streamgage_catalog_legend <- function(m, usgs_sw = NULL, map_display = NULL) {

  if (is.null(map_display) || !isTRUE(map_display$add_usgs_streamgages)) {
    return(m)
  }
  if (!inherits(usgs_sw, "sf") && !is.data.frame(usgs_sw)) {
    return(m)
  }
  if (nrow(usgs_sw) == 0) {
    return(m)
  }

  group_name <- pt_layer_group_name("USGS streamgages")

  sx <- as.data.frame(usgs_sw, stringsAsFactors = FALSE)
  site_no <- if ("site_no" %in% names(sx)) as.character(sx$site_no) else as.character(seq_len(nrow(sx)))

  status <- if ("status" %in% names(sx)) {
    tolower(trimws(as.character(sx$status)))
  } else if ("usgsswpop_status" %in% names(sx)) {
    tolower(trimws(as.character(sx$usgsswpop_status)))
  } else {
    rep("", nrow(sx))
  }

  is_active <- grepl("active", status) & !grepl("inactive", status)
  is_inactive <- grepl("inactive|discontinued", status)

  count_nu <- NULL
  for (cc in c("usgsswpop_count_nu", "count_nu", "measurements")) {
    if (cc %in% names(sx)) {
      count_nu <- suppressWarnings(as.numeric(sx[[cc]]))
      break
    }
  }
  if (is.null(count_nu)) count_nu <- rep(NA_real_, nrow(sx))

  pt_streamgage_water_year <- function(v) {
    s <- as.character(v)
    y <- suppressWarnings(as.integer(substr(s, 1, 4)))
    m <- suppressWarnings(as.integer(substr(s, 6, 7)))
    out <- y
    out[!is.na(y) & !is.na(m) & m >= 10] <- y[!is.na(y) & !is.na(m) & m >= 10] + 1L
    out
  }

  start_year <- NULL
  for (cc in c("usgsswpop_start_date", "start_date", "begin_date")) {
    if (cc %in% names(sx)) {
      start_year <- pt_streamgage_water_year(sx[[cc]])
      break
    }
  }
  if (is.null(start_year)) start_year <- rep(NA_integer_, nrow(sx))

  end_year <- NULL
  for (cc in c("usgsswpop_end_date", "end_date")) {
    if (cc %in% names(sx)) {
      end_year <- pt_streamgage_water_year(sx[[cc]])
      break
    }
  }
  if (is.null(end_year)) end_year <- rep(NA_integer_, nrow(sx))

  por_years <- ifelse(!is.na(start_year) & !is.na(end_year), end_year - start_year + 1L, NA_integer_)

  on_blm <- rep(NA, nrow(sx))
  for (cc in c("on_blm_ca", "on_blm")) {
    if (cc %in% names(sx)) {
      raw <- tolower(trimws(as.character(sx[[cc]])))
      on_blm <- raw %in% c("true", "t", "1", "yes", "y")
      break
    }
  }

  dist_mi <- rep(NA_real_, nrow(sx))
  for (cc in c("dist_to_blm_mi", "distance_to_blm_mi")) {
    if (cc %in% names(sx)) {
      dist_mi <- suppressWarnings(as.numeric(sx[[cc]]))
      break
    }
  }

  current_ops_data <- if ("current_ops_data_value_available" %in% names(sx)) {
    raw <- tolower(trimws(as.character(sx$current_ops_data_value_available)))
    raw %in% c("true", "t", "1", "yes", "y")
  } else if ("current_ops_discharge_value_available" %in% names(sx)) {
    raw <- tolower(trimws(as.character(sx$current_ops_discharge_value_available)))
    raw %in% c("true", "t", "1", "yes", "y")
  } else {
    rep(FALSE, nrow(sx))
  }

  elev_ft <- rep(NA_real_, nrow(sx))
  for (cc in c("usgsswpop_elev_ft", "elev_ft", "elevation_ft")) {
    if (cc %in% names(sx)) {
      elev_ft <- suppressWarnings(as.numeric(gsub(",", "", as.character(sx[[cc]]))))
      break
    }
  }

  records <- data.frame(
    site_no = site_no,
    active = is_active,
    inactive = is_inactive,
    current_data = current_ops_data,
    por_years = as.numeric(por_years),
    start_year = as.numeric(start_year),
    end_year = as.numeric(end_year),
    count_nu = as.numeric(count_nu),
    on_blm = as.logical(on_blm),
    dist_mi = as.numeric(dist_mi),
    elev_ft = as.numeric(elev_ft),
    stringsAsFactors = FALSE
  )

  counts <- list(
    total = as.integer(nrow(sx)),
    active = as.integer(sum(is_active, na.rm = TRUE)),
    inactive = as.integer(sum(is_inactive, na.rm = TRUE)),
    unknown = as.integer(sum(!is_active & !is_inactive, na.rm = TRUE)),
    current_ops_data = as.integer(sum(current_ops_data, na.rm = TRUE))
  )

  pt_jsq <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null")

  js <- r"---(
function(el, x) {
  var map = this;
  var groupName = __USGS_SW_GROUP__;
  var counts = __USGS_SW_COUNTS__;
  var records = __USGS_SW_RECORDS__ || [];
  var recordsById = {};

  records.forEach(function(r) {
    if (r && r.site_no != null) recordsById[String(r.site_no)] = r;
  });

  var filters = {
    status: 'all',
    currentOnly: false,
    minPor: null,
    startMax: null,
    endMin: null,
    blmMode: 'any',
    blmMax: null,
    elevMin: null,
    elevMax: null
  };

  // BRIM_USGS_SW_042J:
  //   Filters must redraw the MarkerClusterGroup, not merely hide SVG paths.
  //   Hiding child circle markers leaves cluster counts/centroids unchanged until
  //   zoomed far enough to decluster.  Keep a stable marker index and add/remove
  //   markers from their cluster group so the visible map responds immediately.
  var markerById = {};
  var markerClusterById = {};
  var indexedClusterGroups = {};

  function fmt(n) {
    if (n == null || isNaN(Number(n))) return '—';
    return Number(n).toLocaleString();
  }

  function esc(s) {
    return String(s == null ? '' : s)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
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

  function isUsgsStreamText(s) {
    var g = norm(s);
    return g.indexOf('usgs streamgages') >= 0;
  }

  function num(v) {
    if (v == null || v === '') return null;
    var n = Number(v);
    return isNaN(n) ? null : n;
  }

  function getSiteId(layer) {
    if (!layer) return null;
    if (layer.__ptUsgsSwSiteNo != null) return layer.__ptUsgsSwSiteNo;
    if (layer.options) {
      var candidates = [layer.options.layerId, layer.options.id, layer.options.featureId];
      for (var i = 0; i < candidates.length; i++) {
        if (candidates[i] != null && recordsById[String(candidates[i])]) {
          layer.__ptUsgsSwSiteNo = String(candidates[i]);
          return layer.__ptUsgsSwSiteNo;
        }
      }
    }
    if (layer.feature && layer.feature.properties) {
      var p = layer.feature.properties;
      var propIds = [p.site_no, p.id, p.layerId];
      for (var j = 0; j < propIds.length; j++) {
        if (propIds[j] != null && recordsById[String(propIds[j])]) {
          layer.__ptUsgsSwSiteNo = String(propIds[j]);
          return layer.__ptUsgsSwSiteNo;
        }
      }
    }
    return null;
  }

  function isTargetLayer(layer) {
    if (!layer || !layer.setStyle) return false;
    return getSiteId(layer) != null || (layer.options && layer.options.group === groupName);
  }

  function collectTargetLayersFrom(layer, out, idHint) {
    if (!layer) return;
    if (idHint != null && recordsById[String(idHint)]) layer.__ptUsgsSwSiteNo = String(idHint);
    if (isTargetLayer(layer)) out.push(layer);
    if (layer.eachLayer) layer.eachLayer(function(child) { collectTargetLayersFrom(child, out, idHint); });
  }

  function collectFromLayerManager(out) {
    if (!(map && map.layerManager && map.layerManager._byGroup && map.layerManager._byGroup[groupName])) return;
    var tbl = map.layerManager._byGroup[groupName];
    Object.keys(tbl).forEach(function(k) { collectTargetLayersFrom(tbl[k], out, recordsById[String(k)] ? k : null); });
  }

  function targetLayers() {
    var out = [];
    collectFromLayerManager(out);
    if (map && map.eachLayer) map.eachLayer(function(layer) { collectTargetLayersFrom(layer, out, null); });
    var seen = {};
    return out.filter(function(layer) {
      var id = L.stamp(layer);
      if (seen[id]) return false;
      seen[id] = true;
      return !!(layer && layer.setStyle && getSiteId(layer) != null);
    });
  }

  function safeControlScan() {
    if (typeof document === 'undefined' || !document.querySelectorAll) return null;
    var labels = document.querySelectorAll('.leaflet-control-layers-overlays label');
    for (var i = 0; i < labels.length; i++) {
      var label = labels[i];
      var text = label.textContent || label.innerText || '';
      if (!isUsgsStreamText(text)) continue;
      var input = label.querySelector ? label.querySelector('input[type="checkbox"]') : null;
      if (input) return !!input.checked;
    }
    return null;
  }

  function eventMatches(evt) {
    if (!evt) return false;
    if (isUsgsStreamText(evt.name)) return true;
    if (evt.layer && evt.layer.options) {
      var bits = [evt.layer.options.group, evt.layer.options.name, evt.layer.options.layerId].join(' ');
      return isUsgsStreamText(bits);
    }
    return false;
  }

  var active = false;

  // Closeout for the USGS streamgage Local catalog legend.  This hides only
  // the legend and preserves filters/drawn state.  Re-open only when the USGS
  // Streamgages source layer itself is toggled off/on.
  var legendUserHidden = false;

  function symbolDot(fill, stroke) {
    return '<span style="width:10px;height:10px;border-radius:50%;background:' + fill + ';border:1.4px solid ' + stroke + ';box-sizing:border-box;display:inline-block;"></span>';
  }

  function symbolRing(stroke) {
    return '<span style="width:12px;height:12px;border:2.4px solid ' + stroke + ';border-radius:50%;box-sizing:border-box;display:inline-block;background:transparent;"></span>';
  }

  function row(symbol, label, count) {
    return '<div style="display:grid;grid-template-columns:14px minmax(0,1fr) auto;align-items:center;column-gap:6px;margin:2px 0;">' +
      symbol +
      '<span style="white-space:nowrap;overflow:hidden;text-overflow:ellipsis;">' + label + '</span>' +
      '<span style="font-variant-numeric:tabular-nums;color:#333;font-weight:600;">' + fmt(count) + '</span>' +
      '</div>';
  }

  function btn(label, action, selected) {
    return '<button type="button" data-pt-action="' + action + '" style="font-size:10.5px;line-height:1.05;padding:2px 5px;margin:1px 2px 1px 0;border-radius:4px;border:1px solid #9b9278;background:' + (selected ? '#d7c592' : '#fbf8ec') + ';cursor:pointer;">' + label + '</button>';
  }

  function input(id, value, placeholder, width) {
    return '<input id="' + id + '" type="number" value="' + (value == null ? '' : esc(value)) + '" placeholder="' + esc(placeholder) + '" style="width:' + width + 'px;font-size:10.5px;padding:1px 3px;margin:1px 2px;border:1px solid #aaa;border-radius:3px;">';
  }

  var old = document.querySelector('.pt-usgs-sw-local-legend');
  if (old && old.parentNode) old.parentNode.removeChild(old);

  var legend = L.control({position: 'bottomleft'});
  legend.onAdd = function(map) {
    var div = L.DomUtil.create('div', 'leaflet-control pt-usgs-sw-local-legend');
    div.style.display = 'none';
    div.style.background = 'rgba(246, 239, 222, 0.96)';
    div.style.border = '1px solid rgba(112, 103, 83, 0.55)';
    div.style.borderRadius = '6px';
    div.style.boxShadow = '0 1px 5px rgba(0,0,0,0.25)';
    div.style.padding = '7px 9px 8px 9px';
    div.style.maxWidth = '300px';
    div.style.fontFamily = 'Arial, sans-serif';
    div.style.fontSize = '11.5px';
    div.style.lineHeight = '1.22';
    div.style.color = '#222';
    div.style.marginBottom = '74px';
    div.style.position = 'relative';

    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);
    return div;
  };
  legend.addTo(map);

  function recordPasses(r) {
    if (!r) return false;
    if (filters.status === 'active' && !r.active) return false;
    if (filters.status === 'inactive' && !r.inactive) return false;
    if (filters.currentOnly && !r.current_data) return false;
    if (filters.minPor != null && (r.por_years == null || Number(r.por_years) < filters.minPor)) return false;
    if (filters.startMax != null && (r.start_year == null || Number(r.start_year) > filters.startMax)) return false;
    if (filters.endMin != null && (r.end_year == null || Number(r.end_year) < filters.endMin)) return false;
    if (filters.blmMode === 'on' && !r.on_blm) return false;
    if (filters.blmMode === 'distance') {
      if (r.dist_mi == null || isNaN(Number(r.dist_mi)) || Number(r.dist_mi) > filters.blmMax) return false;
    }
    if (filters.elevMin != null && (r.elev_ft == null || isNaN(Number(r.elev_ft)) || Number(r.elev_ft) < filters.elevMin)) return false;
    if (filters.elevMax != null && (r.elev_ft == null || isNaN(Number(r.elev_ft)) || Number(r.elev_ft) > filters.elevMax)) return false;
    return true;
  }

  function filteredCount() {
    var n = 0;
    records.forEach(function(r) { if (recordPasses(r)) n++; });
    return n;
  }

  function rememberOriginal(layer) {
    if (layer.__ptUsgsSwOrig) return;
    layer.__ptUsgsSwOrig = {
      radius: layer.getRadius ? layer.getRadius() : null,
      options: {
        color: layer.options && layer.options.color,
        weight: layer.options && layer.options.weight,
        opacity: layer.options && layer.options.opacity,
        fillColor: layer.options && layer.options.fillColor,
        fillOpacity: layer.options && layer.options.fillOpacity
      }
    };
  }

  function targetClusterGroups() {
    var out = [];
    var seen = {};
    if (!(map && map.eachLayer)) return out;
    map.eachLayer(function(layer) {
      if (!layer || typeof layer.getAllChildMarkers !== 'function') return;
      var markers = [];
      try { markers = layer.getAllChildMarkers() || []; } catch(e) { markers = []; }
      var hasTarget = false;
      for (var i = 0; i < markers.length; i++) {
        if (getSiteId(markers[i]) != null) { hasTarget = true; break; }
      }
      if (hasTarget) {
        var id = L.stamp(layer);
        if (!seen[id]) { seen[id] = true; out.push(layer); }
      }
    });
    return out;
  }

  function indexClusterMarkers() {
    targetClusterGroups().forEach(function(group) {
      var gid = L.stamp(group);
      if (indexedClusterGroups[gid]) return;
      indexedClusterGroups[gid] = true;
      var markers = [];
      try { markers = group.getAllChildMarkers() || []; } catch(e) { markers = []; }
      markers.forEach(function(marker) {
        var sid = getSiteId(marker);
        if (sid == null) return;
        sid = String(sid);
        rememberOriginal(marker);
        markerById[sid] = marker;
        markerClusterById[sid] = group;
      });
    });
  }

  function restoreMarkerStyle(marker) {
    if (!marker) return;
    var o = marker.__ptUsgsSwOrig || {};
    if (marker.setStyle && o.options) marker.setStyle(o.options);
    if (marker.setRadius && o.radius != null) marker.setRadius(o.radius);
    if (marker._path) marker._path.style.pointerEvents = '';
  }

  function applyFiltersToLayers() {
    // BRIM_USGS_SW_042K:
    //   Prefer the browser-built USGS streamgage layer.  Rebuilding that layer
    //   from the filtered record set mirrors the Ops Live streamflow/GW pattern
    //   and avoids the MarkerCluster problem where hidden child SVG markers keep
    //   stale cluster counts until deep zoom.
    if (window.BRIM_USGS_SW_LOCAL && typeof window.BRIM_USGS_SW_LOCAL.applyFilters === 'function') {
      try {
        window.BRIM_USGS_SW_LOCAL.applyFilters(filters);
        return;
      } catch(e) {
        if (window.console && console.warn) console.warn('BRIM USGS streamgage JS-layer filter failed; falling back.', e);
      }
    }

    // Fallback for older builds that still draw this layer through R leaflet.
    targetLayers().forEach(function(layer) {
      rememberOriginal(layer);
      var sid = getSiteId(layer);
      var r = recordsById[String(sid)];
      var show = recordPasses(r);
      if (show) {
        restoreMarkerStyle(layer);
      } else {
        if (layer.setStyle) layer.setStyle({opacity: 0, fillOpacity: 0, weight: 0});
        if (layer.setRadius) layer.setRadius(0.001);
        if (layer._path) layer._path.style.pointerEvents = 'none';
      }
    });
  }

  function filterSummaryText() {
    var bits = [];
    if (filters.status === 'active') bits.push('active');
    if (filters.status === 'inactive') bits.push('inactive');
    if (filters.currentOnly) bits.push('Ops Live data');
    if (filters.minPor != null) bits.push('POR≥' + filters.minPor + 'y');
    if (filters.startMax != null && filters.endMin != null) bits.push('covers WY' + filters.startMax + '–' + filters.endMin);
    else if (filters.startMax != null) bits.push('starts by WY' + filters.startMax);
    else if (filters.endMin != null) bits.push('ends after WY' + filters.endMin);
    if (filters.blmMode === 'on') bits.push('on BLM');
    if (filters.blmMode === 'distance') bits.push('≤' + filters.blmMax + ' mi BLM');
    if (filters.elevMin != null && filters.elevMax != null) bits.push(filters.elevMin + '–' + filters.elevMax + ' ft');
    else if (filters.elevMin != null) bits.push('elev≥' + filters.elevMin + ' ft');
    else if (filters.elevMax != null) bits.push('elev≤' + filters.elevMax + ' ft');
    return bits.length ? bits.join(' · ') : 'none';
  }

  function setInputValues(div) {
    var minPor = div.querySelector('#pt-usgs-sw-min-por');
    var startMax = div.querySelector('#pt-usgs-sw-startmax');
    var endMin = div.querySelector('#pt-usgs-sw-endmin');
    var blmMax = div.querySelector('#pt-usgs-sw-blmmax');
    var elevMin = div.querySelector('#pt-usgs-sw-elevmin');
    var elevMax = div.querySelector('#pt-usgs-sw-elevmax');
    if (minPor) minPor.value = filters.minPor == null ? '' : filters.minPor;
    if (startMax) startMax.value = filters.startMax == null ? '' : filters.startMax;
    if (endMin) endMin.value = filters.endMin == null ? '' : filters.endMin;
    if (blmMax) blmMax.value = filters.blmMode === 'distance' ? filters.blmMax : '';
    if (elevMin) elevMin.value = filters.elevMin == null ? '' : filters.elevMin;
    if (elevMax) elevMax.value = filters.elevMax == null ? '' : filters.elevMax;
  }

  function readTextFilters(div) {
    var minPor = num((div.querySelector('#pt-usgs-sw-min-por') || {}).value);
    var startMax = num((div.querySelector('#pt-usgs-sw-startmax') || {}).value);
    var endMin = num((div.querySelector('#pt-usgs-sw-endmin') || {}).value);
    var blmMax = num((div.querySelector('#pt-usgs-sw-blmmax') || {}).value);
    var elevMin = num((div.querySelector('#pt-usgs-sw-elevmin') || {}).value);
    var elevMax = num((div.querySelector('#pt-usgs-sw-elevmax') || {}).value);
    filters.minPor = minPor;
    filters.startMax = startMax;
    filters.endMin = endMin;
    filters.elevMin = elevMin;
    filters.elevMax = elevMax;
    if (blmMax != null) {
      filters.blmMode = 'distance';
      filters.blmMax = blmMax;
    } else if (filters.blmMode === 'distance') {
      filters.blmMode = 'any';
      filters.blmMax = null;
    }
  }

  function attachEvents(div) {
    if (window.BRIM && window.BRIM.legendCloseout) {
      window.BRIM.legendCloseout.wire(div, '.pt-usgs-sw-local-close', function(){
        legendUserHidden = true;
      });
    } else {
      var closeBtn = div.querySelector('.pt-usgs-sw-local-close');
      if (closeBtn) {
        closeBtn.addEventListener('click', function(e) {
          if (e) { e.preventDefault(); e.stopPropagation(); }
          legendUserHidden = true;
          div.style.display = 'none';
        }, false);
      }
    }

    var buttons = div.querySelectorAll('[data-pt-action]');
    buttons.forEach(function(b) {
      b.addEventListener('click', function(e) {
        if (e) { e.preventDefault(); e.stopPropagation(); }
        var a = b.getAttribute('data-pt-action');
        if (a === 'status-all') filters.status = 'all';
        if (a === 'status-active') filters.status = 'active';
        if (a === 'status-inactive') filters.status = 'inactive';
        if (a === 'current-toggle') filters.currentOnly = !filters.currentOnly;
        if (a === 'por20') filters.minPor = 20;
        if (a === 'start1995') { filters.startMax = 1995; filters.endMin = 1995; }
        if (a === 'end1y') filters.endMin = new Date().getFullYear() - 1;
        if (a === 'blm-any') { filters.blmMode = 'any'; filters.blmMax = null; }
        if (a === 'blm-on') { filters.blmMode = 'on'; filters.blmMax = null; }
        if (a === 'blm1') { filters.blmMode = 'distance'; filters.blmMax = 1; }
        if (a === 'blm5') { filters.blmMode = 'distance'; filters.blmMax = 5; }
        if (a === 'apply') readTextFilters(div);
        if (a === 'reset') {
          filters = {status:'all', currentOnly:false, minPor:null, startMax:null, endMin:null, blmMode:'any', blmMax:null, elevMin:null, elevMax:null};
        }
        updateLegend();
      }, false);
    });
  }

  function filtersHtml() {
    var h = '';
    h += '<div style="margin-top:6px;border-top:1px solid rgba(120,110,90,0.28);padding-top:5px;">';
    h += '<div style="font-weight:700;font-size:11px;margin-bottom:2px;">Filters</div>';
    h += '<div>Status: ' + btn('all','status-all',filters.status==='all') + btn('active','status-active',filters.status==='active') + btn('inactive','status-inactive',filters.status==='inactive') + '</div>';
    h += '<div>Data: ' + btn('Ops Live data','current-toggle',filters.currentOnly) + '</div>';
    h += '<div>POR: yrs≥' + input('pt-usgs-sw-min-por', filters.minPor, 'yrs', 37) + ' covers WY ' + input('pt-usgs-sw-startmax', filters.startMax, 'from', 47) + '–' + input('pt-usgs-sw-endmin', filters.endMin, 'to', 47) + '</div>';
    h += '<div>' + btn('POR≥20','por20',filters.minPor===20) + btn('covers WY1995','start1995',filters.startMax===1995 && filters.endMin===1995) + btn('recent WY','end1y',filters.endMin === (new Date().getFullYear()-1)) + '</div>';
    h += '<div>BLM max mi: ' + input('pt-usgs-sw-blmmax', filters.blmMode==='distance' ? filters.blmMax : null, 'mi', 43) + '</div>';
    h += '<div>' + btn('any','blm-any',filters.blmMode==='any') + btn('on BLM','blm-on',filters.blmMode==='on') + btn('≤1 mi','blm1',filters.blmMode==='distance' && filters.blmMax===1) + btn('≤5 mi','blm5',filters.blmMode==='distance' && filters.blmMax===5) + '</div>';
    h += '<div>Elev ft: ' + input('pt-usgs-sw-elevmin', filters.elevMin, 'min', 48) + '–' + input('pt-usgs-sw-elevmax', filters.elevMax, 'max', 48) + '</div>';
    h += '<div style="margin-top:2px;">' + btn('Apply','apply',false) + btn('Reset','reset',false) + ' <span style="font-size:10.5px;color:#4d4d4d;">USGS site ID labels (lbl) display at zoom 9+.</span></div>';
    h += '</div>';
    return h;
  }

  function updateLegend() {
    var checkboxActive = safeControlScan();
    var show = checkboxActive === null ? active : checkboxActive;
    var div = document.querySelector('.pt-usgs-sw-local-legend');
    if (!div) return;
    if (!show) {
      div.style.display = 'none';
      legendUserHidden = false;
      return;
    }

    applyFiltersToLayers();

    var html = '';
    html += '<button type="button" class="pt-usgs-sw-local-close" title="Hide legend" style="position:absolute;top:3px;right:5px;border:0;background:transparent;color:#776f61;font-weight:700;font-size:14px;line-height:1;padding:0 2px;cursor:pointer;">&times;</button>';
    html += '<div style="font-weight:700;font-size:12.5px;margin:0 0 2px 0;padding-right:18px;">USGS streamgage catalog</div>';
    html += '<div style="font-size:10.5px;color:#4d4d4d;margin:0 0 5px 0;">Use Ops Live for current values.</div>';
    html += '<div style="font-weight:700;font-size:11px;margin:3px 0 1px 0;">USGS site status</div>';
    html += row(symbolDot('#33A02C', '#1B7837'), 'Active', counts.active);
    html += row(symbolDot('#E31A1C', '#99000D'), 'Inactive / historical', counts.inactive);
    if (Number(counts.unknown || 0) > 0) html += row(symbolDot('#8C8C8C', '#4D4D4D'), 'Unknown', counts.unknown);
    html += '<div style="height:4px;"></div>';
    html += row(symbolRing('#ff00a8'), 'Ops Live data', counts.current_ops_data);
    html += '<div style="font-size:10.5px;color:#4d4d4d;margin-top:5px;">Showing ' + fmt(filteredCount()) + ' / ' + fmt(counts.total) + ' sites</div>';
    html += '<div style="font-size:10.5px;color:#4d4d4d;">Filters: ' + esc(filterSummaryText()) + '</div>';
    html += filtersHtml();

    div.innerHTML = html;
    div.style.display = legendUserHidden ? 'none' : 'block';
    setInputValues(div);
    attachEvents(div);
  }

  map.on('overlayadd', function(evt) { if (eventMatches(evt)) { active = true; legendUserHidden = false; } setTimeout(updateLegend, 0); });
  map.on('overlayremove', function(evt) { if (eventMatches(evt)) { active = false; legendUserHidden = false; } setTimeout(updateLegend, 0); });
  map.on('zoomend', updateLegend);

  if (typeof document !== 'undefined' && document.addEventListener) {
    document.addEventListener('change', function(evt) {
      var t = evt && evt.target;
      if (t && t.matches && t.matches('.leaflet-control-layers-overlays input[type="checkbox"]')) {
        var label = t.closest ? t.closest('label') : null;
        var text = label ? (label.textContent || label.innerText || '') : '';
        if (isUsgsStreamText(text)) {
          legendUserHidden = false;
          setTimeout(updateLegend, 0);
          setTimeout(updateLegend, 100);
        }
      }
    }, true);
  }

  setTimeout(updateLegend, 0);
  setTimeout(updateLegend, 500);
  setTimeout(updateLegend, 1500);
}
)---"

  js <- gsub("__USGS_SW_GROUP__", pt_jsq(group_name), js, fixed = TRUE)
  js <- gsub("__USGS_SW_COUNTS__", pt_jsq(counts), js, fixed = TRUE)
  js <- gsub("__USGS_SW_RECORDS__", pt_jsq(records), js, fixed = TRUE)

  htmlwidgets::onRender(m, js)
}


# ==== 2C. Browser-built USGS streamgage Local layer ==========================
##
## PURPOSE:
##   Draw the Local USGS streamgage catalog with the same filter architecture
##   used by Ops Live layers: keep the full record table in the browser, then
##   rebuild the MarkerClusterGroup from the filtered rows.  This is more
##   reliable than trying to hide child SVG markers created by R leaflet after a
##   MarkerClusterGroup has already indexed the full layer.

pt_add_usgs_streamgage_browser_layer <- function(m, usgs_sw = NULL, group_name = pt_layer_group_name("USGS streamgages")) {

  if (!inherits(usgs_sw, "sf") && !is.data.frame(usgs_sw)) return(m)
  if (nrow(usgs_sw) == 0 || !"site_no" %in% names(usgs_sw)) return(m)

  x <- usgs_sw

  ## Coordinates for browser-side L.circleMarker.  The Local point caches are
  ## normally WGS84 already, but explicitly transform when CRS metadata is set.
  coords <- NULL
  if (inherits(x, "sf")) {
    x_ll <- try({
      crs <- sf::st_crs(x)
      if (!is.na(crs)) sf::st_transform(x, 4326) else x
    }, silent = TRUE)
    if (inherits(x_ll, "try-error")) x_ll <- x
    coords <- suppressWarnings(sf::st_coordinates(sf::st_geometry(x_ll)))
  }

  if (is.null(coords) || nrow(coords) != nrow(x)) {
    warning("USGS Streamgages browser layer skipped: could not extract point coordinates.")
    return(m)
  }

  sx <- if (inherits(x, "sf")) sf::st_drop_geometry(x) else as.data.frame(x, stringsAsFactors = FALSE)
  sx$pt_lng <- as.numeric(coords[, "X"])
  sx$pt_lat <- as.numeric(coords[, "Y"])

  ## Keep the embedded table compact but self-sufficient for popup, style, and
  ## filters.  Include both newer compact fields and older popup_html fallback.
  keep <- c(
    "site_no",
    "pt_lat", "pt_lng",
    "fill_col", "stroke_col", "stroke_weight", "marker_radius",
    "hover_text", "popup_html",
    "status", "usgsswpop_status", "usgsswpop_name", "usgsswpop_site_type",
    "usgsswpop_elev_ft", "usgsswpop_count_nu", "usgsswpop_start_date",
    "usgsswpop_end_date", "usgsswpop_common_data",
    "count_nu", "measurements", "start_date", "end_date", "begin_date",
    "current_ops_data_value_available", "current_ops_data_class", "current_ops_data_age_hours",
    "current_ops_discharge_value_available", "current_ops_discharge_class",
    "on_blm_ca", "on_blm", "dist_to_blm_mi", "distance_to_blm_mi", "dist_to_blm_ft"
  )

  rec <- sx[, intersect(keep, names(sx)), drop = FALSE]
  rec$site_no <- as.character(rec$site_no)
  rec <- rec[!is.na(rec$site_no) & rec$site_no != "" & !is.na(rec$pt_lat) & !is.na(rec$pt_lng), , drop = FALSE]
  if (nrow(rec) == 0) return(m)

  js <- r"---(
function(el, x, data) {
  var map = this;
  var groupName = data && data.groupName ? String(data.groupName) : 'Points – USGS streamgages';
  var labelGroupName = data && data.labelGroupName ? String(data.labelGroupName) : 'Labels – USGS streamgages';

  function rowsToArray(rows) {
    if (!rows) return [];
    if (Array.isArray(rows)) return rows;
    if (typeof rows === 'object') {
      var keys = Object.keys(rows);
      var n = 0;
      for (var k = 0; k < keys.length; k++) {
        if (Array.isArray(rows[keys[k]])) { n = rows[keys[k]].length; break; }
      }
      var out = [];
      for (var i = 0; i < n; i++) {
        var rec = {};
        for (var j = 0; j < keys.length; j++) {
          var key = keys[j];
          rec[key] = Array.isArray(rows[key]) ? rows[key][i] : rows[key];
        }
        out.push(rec);
      }
      return out;
    }
    return [];
  }

  var records = rowsToArray(data && data.records).filter(function(r) {
    return r && r.site_no != null && r.pt_lat != null && r.pt_lng != null;
  });

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

  function num(v) {
    if (v === null || v === undefined || v === '') return null;
    var n = Number(v);
    return isNaN(n) ? null : n;
  }

  function bool(v) {
    if (v === true) return true;
    if (v === false || v === null || v === undefined) return false;
    var s = String(v).trim().toLowerCase();
    return ['true','t','1','yes','y'].indexOf(s) >= 0;
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

  function isUsgsStreamText(s) {
    return norm(s).indexOf('usgs streamgages') >= 0;
  }

  function isUsgsStreamLabelText(s) {
    var raw = String(s == null ? '' : s);
    return /^\s*Labels\s*[–-]\s*/i.test(raw) && norm(raw).indexOf('usgs streamgages') >= 0;
  }

  function safeLabelControlScan() {
    if (typeof document === 'undefined' || !document.querySelectorAll) return null;
    var labels = document.querySelectorAll('.leaflet-control-layers-overlays label');
    for (var i = 0; i < labels.length; i++) {
      var label = labels[i];
      var full = label.getAttribute ? (label.getAttribute('data-pt-layer-full-name') || '') : '';
      var text = label.textContent || label.innerText || '';
      if (!isUsgsStreamLabelText(full || text)) continue;
      var input = label.querySelector ? label.querySelector('input[type="checkbox"]') : null;
      if (input) return !!input.checked;
    }
    return null;
  }

  function safeControlScan() {
    if (typeof document === 'undefined' || !document.querySelectorAll) return null;
    var labels = document.querySelectorAll('.leaflet-control-layers-overlays label');
    for (var i = 0; i < labels.length; i++) {
      var label = labels[i];
      var text = label.textContent || label.innerText || '';
      if (!isUsgsStreamText(text)) continue;
      var input = label.querySelector ? label.querySelector('input[type="checkbox"]') : null;
      if (input) return !!input.checked;
    }
    return null;
  }

  function eventMatches(evt) {
    if (!evt) return false;
    if (isUsgsStreamText(evt.name)) return true;
    if (evt.layer && evt.layer.options) {
      var bits = [evt.layer.options.group, evt.layer.options.name, evt.layer.options.layerId].join(' ');
      return isUsgsStreamText(bits);
    }
    return false;
  }

  function labelEventMatches(evt) {
    if (!evt) return false;
    if (isUsgsStreamLabelText(evt.name)) return true;
    if (evt.layer && evt.layer.options) {
      var bits = [evt.layer.options.group, evt.layer.options.name, evt.layer.options.layerId].join(' ');
      return isUsgsStreamLabelText(bits);
    }
    return false;
  }

  function getStatus(r) {
    var raw = has(r.status) ? r.status : r.usgsswpop_status;
    return String(raw == null ? '' : raw).trim().toLowerCase();
  }

  function isActive(r) {
    var s = getStatus(r);
    return /active/.test(s) && !/inactive/.test(s);
  }

  function isInactive(r) {
    return /inactive|discontinued/.test(getStatus(r));
  }

  function firstNum(r, fields) {
    for (var i = 0; i < fields.length; i++) {
      if (r[fields[i]] !== undefined) {
        var n = num(r[fields[i]]);
        if (n !== null) return n;
      }
    }
    return null;
  }

  function firstYear(r, fields) {
    // Interpret date-like values as water years: Oct-Dec belong to the next WY.
    // Year-only values are left as entered.
    for (var i = 0; i < fields.length; i++) {
      if (r[fields[i]] !== undefined && r[fields[i]] !== null) {
        var s = String(r[fields[i]]).trim();
        var m = s.match(/((18|19|20)\d{2})(?:[-\/](\d{1,2}))?/);
        if (m) {
          var y = Number(m[1]);
          var mo = m[3] == null ? null : Number(m[3]);
          if (mo != null && !isNaN(mo) && mo >= 10) return y + 1;
          return y;
        }
      }
    }
    return null;
  }

  function porYears(r) {
    var sy = firstYear(r, ['usgsswpop_start_date','start_date','begin_date']);
    var ey = firstYear(r, ['usgsswpop_end_date','end_date']);
    if (sy === null || ey === null) return null;
    return ey - sy + 1;
  }

  function currentOpsData(r) {
    return bool(r.current_ops_data_value_available) || bool(r.current_ops_discharge_value_available);
  }

  function elevFt(r) {
    return firstNum(r, ['usgsswpop_elev_ft','elev_ft','elevation_ft']);
  }

  function onBlm(r) {
    return bool(r.on_blm_ca) || bool(r.on_blm);
  }

  function distMi(r) {
    return firstNum(r, ['dist_to_blm_mi','distance_to_blm_mi']);
  }

  function recordPasses(r, f) {
    f = f || {};
    if (f.status === 'active' && !isActive(r)) return false;
    if (f.status === 'inactive' && !isInactive(r)) return false;
    if (f.currentOnly && !currentOpsData(r)) return false;

    var py = porYears(r);
    var sy = firstYear(r, ['usgsswpop_start_date','start_date','begin_date']);
    var ey = firstYear(r, ['usgsswpop_end_date','end_date']);
    var dm = distMi(r);
    var elev = elevFt(r);

    if (f.minPor != null && (py === null || py < Number(f.minPor))) return false;
    if (f.startMax != null && (sy === null || sy > Number(f.startMax))) return false;
    if (f.endMin != null && (ey === null || ey < Number(f.endMin))) return false;
    if (f.blmMode === 'on' && !onBlm(r)) return false;
    if (f.blmMode === 'distance' && (dm === null || dm > Number(f.blmMax))) return false;
    if (f.elevMin != null && (elev === null || elev < Number(f.elevMin))) return false;
    if (f.elevMax != null && (elev === null || elev > Number(f.elevMax))) return false;
    return true;
  }

  function makePopup(r) {
    if (has(r.popup_html)) return String(r.popup_html);
    var sid = has(r.site_no) ? r.site_no : '';
    var parts = [];
    parts.push('<b>' + esc(sid) + '</b> – ' + esc(r.usgsswpop_name));
    parts.push('<b>Elevation:</b> ' + esc(r.usgsswpop_elev_ft) + ' ft');
    parts.push('<b>Type:</b> ' + esc(r.usgsswpop_site_type));
    parts.push('<b>Status:</b> ' + esc(r.usgsswpop_status || r.status));
    parts.push('<b>Record count:</b> ' + esc(r.usgsswpop_count_nu || r.count_nu || r.measurements));
    parts.push('<b>Period:</b> ' + esc(r.usgsswpop_start_date || r.start_date || r.begin_date) + ' to ' + esc(r.usgsswpop_end_date || r.end_date));
    if (has(r.usgsswpop_common_data)) parts.push('<b>Common data:</b> ' + esc(r.usgsswpop_common_data));
    parts.push('<span style="font-size:11px; color:#555;">Additional parameters may be available from USGS.</span>');
    if (has(sid)) parts.push('<a href="https://waterdata.usgs.gov/monitoring-location/' + encodeURIComponent(String(sid)) + '" target="_blank">USGS Site</a>');
    parts.push('<a href="https://dashboard.waterdata.usgs.gov/app/nwd/en/" target="_blank">usgs dash</a>');
    parts.push('<a href="https://water.noaa.gov/" target="_blank">noaa nwm</a>');
    return parts.join('<br/>');
  }

  function fmtElev(v) {
    var e = num(v);
    if (e === null) return 'not available';
    return Math.round(e).toLocaleString() + ' ft';
  }
  function makeTooltip(r) {
    var sid = has(r.site_no) ? r.site_no : '';
    var nm = has(r.usgsswpop_name) ? r.usgsswpop_name : 'USGS streamgage';
    return esc(sid) + '<br/>' + esc(nm) + '<br/>Elevation: ' + esc(fmtElev(r.usgsswpop_elev_ft || r.elev_ft || r.elevation_ft));
  }

  function streamLabelText(r) {
    var nm = has(r.usgsswpop_name) ? String(r.usgsswpop_name).trim() : '';
    if (!has(nm)) nm = has(r.name) ? String(r.name).trim() : '';
    if (!has(nm)) nm = has(r.site_no) ? String(r.site_no).trim() : '';
    return nm;
  }

  function makeStreamLabelMarker(r) {
    var lat = Number(r.pt_lat), lng = Number(r.pt_lng);
    if (isNaN(lat) || isNaN(lng)) return null;
    var txt = streamLabelText(r);
    if (!has(txt)) return null;
    return L.marker([lat, lng], {
      interactive: false,
      keyboard: false,
      pane: 'pane_labels_pts',
      icon: L.divIcon({
        className: 'pt-usgs-sw-local-label-divicon',
        html: '<span>' + esc(txt) + '</span>',
        iconSize: L.point(1, 1),
        iconAnchor: L.point(0, 0)
      })
    });
  }

  function springLabelText(r) {
    var nm = has(r.spring_name_display) ? String(r.spring_name_display).trim() : '';
    if (!has(nm)) return '';
    // Avoid thousands of visually unhelpful duplicate labels when the source
    // only says the feature is unnamed. The hover/popup still displays those
    // records normally.
    if (/^unnamed\s+spring$/i.test(nm)) return '';
    return nm;
  }

  function makeSpringLabelMarker(r) {
    var lat = Number(r.pt_lat), lng = Number(r.pt_lng);
    if (isNaN(lat) || isNaN(lng)) return null;
    var txt = springLabelText(r);
    if (!has(txt)) return null;
    return L.marker([lat, lng], {
      interactive: false,
      keyboard: false,
      pane: 'pane_labels_pts',
      icon: L.divIcon({
        className: 'pt-springs-local-label-divicon',
        html: '<span>' + esc(txt) + '</span>',
        iconSize: L.point(1, 1),
        iconAnchor: L.point(0, 0)
      })
    });
  }

  function makeMarker(r) {
    var lat = Number(r.pt_lat), lng = Number(r.pt_lng);
    if (isNaN(lat) || isNaN(lng)) return null;
    var marker = L.circleMarker([lat, lng], {
      radius: num(r.marker_radius) || (currentOpsData(r) ? 4.8 : 4.0),
      color: has(r.stroke_col) ? String(r.stroke_col) : (currentOpsData(r) ? '#ff00a8' : (isActive(r) ? '#1B7837' : (isInactive(r) ? '#99000D' : '#4D4D4D'))),
      weight: num(r.stroke_weight) || (currentOpsData(r) ? 2.4 : 0.8),
      opacity: 0.92,
      fillColor: has(r.fill_col) ? String(r.fill_col) : (isActive(r) ? '#33A02C' : (isInactive(r) ? '#E31A1C' : '#8C8C8C')),
      fillOpacity: 0.80,
      pane: 'pane_points'
    });
    marker.bindTooltip(makeTooltip(r), {
      direction: 'auto',
      opacity: 0.9,
      sticky: true,
      className: 'pt-usgs-sw-local-tooltip'
    });
    marker.bindPopup(makePopup(r), {
      maxWidth: 420,
      maxHeight: 360,
      autoPanPadding: L.point(20, 20)
    });
    return marker;
  }

  if (!document.getElementById('pt-usgs-sw-local-label-style')) {
    var style = document.createElement('style');
    style.id = 'pt-usgs-sw-local-label-style';
    style.textContent =
      '.pt-usgs-sw-local-label-divicon{background:transparent;border:0;white-space:nowrap;pointer-events:none;}' +
      '.pt-usgs-sw-local-label-divicon span{display:inline-block;transform:translate(-50%,-13px);font:700 11px/1.1 Arial,sans-serif;color:#0f3b5f;background:rgba(255,255,255,0.64);border:1px solid rgba(15,59,95,0.20);border-radius:3px;padding:1px 3px;text-shadow:0 1px 2px #fff,1px 0 2px #fff,-1px 0 2px #fff,0 -1px 2px #fff;box-shadow:0 1px 2px rgba(0,0,0,0.10);max-width:190px;overflow:hidden;text-overflow:ellipsis;}';
    document.head.appendChild(style);
  }

  var labelLayer = L.layerGroup();
  var labelsActive = false;
  var lastFilteredRecords = [];
  var labelMinZoom = 9;

  var markers = (typeof L.markerClusterGroup === 'function') ? L.markerClusterGroup({
    disableClusteringAtZoom: 12,
    maxClusterRadius: function(z) {
      if (z <= 6) return 125;
      if (z <= 8) return 115;
      if (z <= 9) return 105;
      if (z <= 10) return 95;
      if (z <= 11) return 80;
      return 55;
    },
    spiderfyOnMaxZoom: true,
    showCoverageOnHover: false,
    animate: false,
    removeOutsideVisibleBounds: true,
    chunkedLoading: true,
    chunkInterval: 120,
    chunkDelay: 15
  }) : L.layerGroup();

  var currentFilters = {status:'all', currentOnly:false, minPor:null, startMax:null, endMin:null, blmMode:'any', blmMax:null, elevMin:null, elevMax:null};
  var layerActive = false;
  var lastFiltered = records.length;
  var builtOnce = false;

  function syncLabels() {
    var checked = safeLabelControlScan();
    labelsActive = checked === null ? labelsActive : checked;
    labelLayer.clearLayers();
    if (!labelsActive || !layerActive || (map.getZoom && map.getZoom() < labelMinZoom)) {
      if (map.hasLayer(labelLayer)) map.removeLayer(labelLayer);
      return;
    }
    lastFilteredRecords.forEach(function(r) {
      var lm = makeStreamLabelMarker(r);
      if (lm) labelLayer.addLayer(lm);
    });
    if (!map.hasLayer(labelLayer)) labelLayer.addTo(map);
  }

  function rebuild(filters) {
    currentFilters = filters || currentFilters || {};
    markers.clearLayers();
    lastFilteredRecords = [];
    var n = 0;
    records.forEach(function(r) {
      if (!recordPasses(r, currentFilters)) return;
      lastFilteredRecords.push(r);
      var marker = makeMarker(r);
      if (!marker) return;
      markers.addLayer(marker);
      n += 1;
    });
    lastFiltered = n;
    builtOnce = true;
    if (layerActive && !map.hasLayer(markers)) markers.addTo(map);
    syncLabels();
    return {filtered: n, total: records.length, drawn: n};
  }

  function syncActive() {
    var checked = safeControlScan();
    layerActive = checked === null ? layerActive : checked;
    if (layerActive) {
      if (!builtOnce) rebuild(currentFilters);
      if (!map.hasLayer(markers)) markers.addTo(map);
    } else {
      if (map.hasLayer(markers)) map.removeLayer(markers);
      if (map.hasLayer(labelLayer)) map.removeLayer(labelLayer);
    }
    syncLabels();
  }

  window.BRIM_USGS_SW_LOCAL = {
    applyFilters: function(filters) {
      currentFilters = filters || currentFilters;
      return rebuild(currentFilters);
    },
    setActive: function(active) {
      layerActive = !!active;
      syncActive();
    },
    stats: function() { return {filtered: lastFiltered, total: records.length, active: layerActive, labels: labelsActive}; }
  };

  map.on('overlayadd', function(evt) {
    if (eventMatches(evt)) { layerActive = true; syncActive(); }
    if (labelEventMatches(evt)) { labelsActive = true; syncLabels(); }
  });
  map.on('overlayremove', function(evt) {
    if (eventMatches(evt)) { layerActive = false; if (map.hasLayer(markers)) map.removeLayer(markers); if (map.hasLayer(labelLayer)) map.removeLayer(labelLayer); }
    if (labelEventMatches(evt)) { labelsActive = false; syncLabels(); }
  });
  map.on('zoomend', function() { syncLabels(); });

  if (typeof document !== 'undefined' && document.addEventListener) {
    document.addEventListener('change', function(evt) {
      var t = evt && evt.target;
      if (t && t.matches && t.matches('.leaflet-control-layers-overlays input[type="checkbox"]')) {
        setTimeout(syncActive, 0);
        setTimeout(syncLabels, 0);
      }
    }, true);
  }

  setTimeout(syncActive, 0);
  setTimeout(syncLabels, 0);
  setTimeout(syncActive, 500);
  setTimeout(syncLabels, 500);
}
)---"

  htmlwidgets::onRender(m, js, data = list(
    groupName = group_name,
    labelGroupName = pt_layer_group_name("Labels: USGS streamgages"),
    records = rec
  ))
}
# ==== 2E. Viewport-virtualized USGS groundwater Local layer ==================
##
## PURPOSE:
##   Preserve every Local USGS groundwater site record in the standalone HTML
##   while bounding live Leaflet objects to the current viewport. Low and
##   intermediate zooms use deterministic screen-grid aggregates; zoom 11+
##   uses exact mapped locations rendered through a dedicated Canvas renderer.
##
## DATA INTEGRITY:
##   - Site rows are never sampled or deduplicated.
##   - Exact point coordinates remain in the browser record table.
##   - The separate location table only indexes contiguous site-row ranges.
##   - Co-located/nested groups follow the existing seven-decimal coordinate
##     grouping used by the browser layer this replaces.
##
## IMPLEMENTATION:
##   JavaScript is kept in a separately syntax-checkable source file and is
##   embedded by htmlwidgets, so the generated BRIM product remains one HTML.

pt_add_usgs_well_virtualized_browser_layer <- function(
  m,
  usgs_gw = NULL,
  group_name = pt_layer_group_name("USGS monitoring wells")
) {

  if (!inherits(usgs_gw, "sf") && !is.data.frame(usgs_gw)) return(m)
  if (nrow(usgs_gw) == 0 || !"site_no" %in% names(usgs_gw)) return(m)

  x <- usgs_gw
  coords <- NULL

  if (inherits(x, "sf")) {
    x_ll <- try({
      crs <- sf::st_crs(x)
      if (!is.na(crs)) sf::st_transform(x, 4326) else x
    }, silent = TRUE)
    if (inherits(x_ll, "try-error")) x_ll <- x
    coords <- suppressWarnings(sf::st_coordinates(sf::st_geometry(x_ll)))
  }

  if (is.null(coords) || nrow(coords) != nrow(x)) {
    warning("USGS Wells virtualized browser layer skipped: could not extract point coordinates.")
    return(m)
  }

  gx <- if (inherits(x, "sf")) {
    sf::st_drop_geometry(x)
  } else {
    as.data.frame(x, stringsAsFactors = FALSE)
  }

  gx$pt_lng <- as.numeric(coords[, "X"])
  gx$pt_lat <- as.numeric(coords[, "Y"])

  keep <- c(
    "site_no", "pt_lat", "pt_lng",
    "well_radius", "well_fill_col", "well_stroke_col",
    "well_stroke_col_display", "well_stroke_weight_display",
    "well_recent_feed_ring", "well_is_nested", "hover_text",
    names(gx)[grepl("^gwpop_", names(gx))],
    "on_blm_ca", "on_blm", "dist_to_blm_mi", "distance_to_blm_mi",
    "dist_to_blm_ft", "gw_on_blm_ca", "gw_dist_to_blm_mi",
    "gw_dist_to_blm_ft"
  )

  rec <- gx[, intersect(unique(keep), names(gx)), drop = FALSE]
  rec$site_no <- as.character(rec$site_no)
  rec <- rec[
    !is.na(rec$site_no) &
      rec$site_no != "" &
      is.finite(rec$pt_lat) &
      is.finite(rec$pt_lng),
    ,
    drop = FALSE
  ]
  if (nrow(rec) == 0) return(m)

  if ("hover_text" %in% names(rec)) {
    rec$hover_text <- sub("^MR WL:\\s*", "", as.character(rec$hover_text))
    rec$hover_text <- gsub(
      "No cached MR WL",
      "No cached water level",
      rec$hover_text,
      fixed = TRUE
    )
  }

  ## Keep the prior browser layer's nested-location interpretation exactly:
  ## coordinates matching after seven decimal places share one display
  ## location. Sorting makes each location's full site records contiguous.
  coord_key <- paste0(
    sprintf("%.7f", rec$pt_lng),
    "_",
    sprintf("%.7f", rec$pt_lat)
  )
  ord <- order(coord_key, rec$site_no, na.last = TRUE)
  rec <- rec[ord, , drop = FALSE]
  coord_key <- coord_key[ord]

  coord_runs <- rle(coord_key)
  loc_start_one <- cumsum(c(1L, head(coord_runs$lengths, -1L)))
  loc_count <- as.integer(coord_runs$lengths)

  locations <- data.frame(
    site_start = as.integer(loc_start_one - 1L),
    site_count = loc_count,
    stringsAsFactors = FALSE
  )

  nested_sizes <- loc_count[loc_count > 1L]
  metadata <- list(
    schemaVersion = 2L,
    siteCount = nrow(rec),
    uniqueCoordinateCount = nrow(locations),
    nestedLocationCount = length(nested_sizes),
    nestedSiteRecordCount = sum(nested_sizes),
    maxNestedLocationSize = if (length(nested_sizes) > 0) max(nested_sizes) else 1L,
    exactCoordinatePrecision = "source coordinates retained; display grouping matches prior 7-decimal key",
    clusterToExactZoom = 11L
  )

  js_path <- file.path(
    "03_functions",
    "js",
    "leaflet_usgs_groundwater_local_virtualized.js"
  )
  if (!file.exists(js_path)) {
    stop("Missing virtualized USGS groundwater browser helper: ", js_path)
  }

  js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")

  message(
    "USGS monitoring wells virtualized payload: ",
    format(metadata$siteCount, big.mark = ","),
    " individual sites; ",
    format(metadata$uniqueCoordinateCount, big.mark = ","),
    " mapped locations; ",
    format(metadata$nestedLocationCount, big.mark = ","),
    " nested/co-located locations."
  )

  htmlwidgets::onRender(
    m,
    js,
    data = list(
      groupName = group_name,
      records = rec,
      locations = locations,
      metadata = metadata
    )
  )
}


# ==== 3A. CDEC reservoir-station local layer =================================
##
## PURPOSE:
##   Draw the local CDEC reservoir-station metadata/index layer.  This is not a
##   current-storage layer; storage values will be handled later by a separate
##   live/static GeoJSON feed.

pt_add_cdec_reservoir_station_layer <- function(m, cdec_reservoir_stations, map_display) {

  if (!isTRUE(map_display$add_cdec_reservoir_stations)) {
    return(m)
  }

  if (is.null(cdec_reservoir_stations) || nrow(cdec_reservoir_stations) == 0) {
    message("CDEC Reservoir Stations layer is empty; no markers added.")
    return(m)
  }

  message("Adding CDEC Reservoir Stations: ", nrow(cdec_reservoir_stations))

  m |>
    leaflet::addCircleMarkers(
      data = cdec_reservoir_stations,
      group = pt_layer_group_name("CDEC Reservoir Stations"),
      radius = ~cdec_radius,
      stroke = TRUE,
      weight = 0.9,
      color = ~cdec_stroke_col,
      fillColor = ~cdec_fill_col,
      fillOpacity = 0.78,
      popup = ~popup_html,
      label = ~hover_text,
      labelOptions = leaflet::labelOptions(
        direction = "auto",
        opacity = 0.9,
        textsize = "12px",
        style = list("white-space" = "pre", "max-width" = "none")
      ),
      options = leaflet::pathOptions(pane = "pane_points"),
      clusterOptions = leaflet::markerClusterOptions()
    )
}

# ==== 4. USGS point layers ===================================================

pt_add_usgs_layers <- function(m, usgs_sw, usgs_gw, map_display) {
  
  # ---- 4.1 USGS streamgages -------------------------------------------------
  ##
  ## Status colors are computed at draw time to avoid storing extra style
  ## columns in the cached RDS.
  ##
  ##   active   = green
  ##   inactive = red
  ##   other/NA = gray
  
  if (isTRUE(map_display$add_usgs_streamgages)) {

    ## Newer caches carry compact usgsswpop_* values and use browser-side
    ## popup templating. Older caches keep working through popup_html fallback.
    if (!"fill_col" %in% names(usgs_sw)) {
      usgs_sw$fill_col <- dplyr::case_when(
        tolower(as.character(usgs_sw$status)) == "active"   ~ "#33A02C",
        tolower(as.character(usgs_sw$status)) == "inactive" ~ "#E31A1C",
        TRUE                                                ~ "#8C8C8C"
      )
    }

    if (!"stroke_col" %in% names(usgs_sw)) {
      usgs_sw$stroke_col <- dplyr::case_when(
        tolower(as.character(usgs_sw$status)) == "active"   ~ "#1B7837",
        tolower(as.character(usgs_sw$status)) == "inactive" ~ "#99000D",
        TRUE                                                ~ "#4D4D4D"
      )
    }

    current_ops_data <- if ("current_ops_data_value_available" %in% names(usgs_sw)) {
      raw <- tolower(trimws(as.character(usgs_sw$current_ops_data_value_available)))
      raw %in% c("true", "t", "1", "yes", "y")
    } else if ("current_ops_discharge_value_available" %in% names(usgs_sw)) {
      raw <- tolower(trimws(as.character(usgs_sw$current_ops_discharge_value_available)))
      raw %in% c("true", "t", "1", "yes", "y")
    } else {
      rep(FALSE, nrow(usgs_sw))
    }

    ## BRIM_USGS_SW_042K:
    ##   Keep Local streamgage fill color as USGS site status.  Pink outline
    ##   means current/recent Ops Live data are available (discharge or stage).
    ##   The actual Local layer is rebuilt in the browser from filtered records,
    ##   matching the Ops Live streamflow/GW approach and avoiding stale cluster
    ##   counts after filters.
    usgs_sw$stroke_col <- ifelse(current_ops_data, "#ff00a8", usgs_sw$stroke_col)
    usgs_sw$stroke_weight <- ifelse(current_ops_data, 2.4, 0.8)
    usgs_sw$marker_radius <- ifelse(current_ops_data, 4.8, 4.0)

    if (!"hover_text" %in% names(usgs_sw)) {
      site_txt <- if ("site_no" %in% names(usgs_sw)) as.character(usgs_sw$site_no) else rep("streamgage", nrow(usgs_sw))
      name_txt <- if ("name" %in% names(usgs_sw)) as.character(usgs_sw$name) else rep("Name not available", nrow(usgs_sw))
      status_txt <- if ("status" %in% names(usgs_sw)) as.character(usgs_sw$status) else rep("unknown", nrow(usgs_sw))
      usgs_sw$hover_text <- paste0("USGS ", site_txt, "\n", name_txt, "\nStatus: ", status_txt)
    }

    ## Register the overlay-group checkbox with a single invisible dummy marker.
    ## The visible streamgage markers are created by pt_add_usgs_streamgage_browser_layer().
    dummy <- data.frame(lng = -170, lat = 10)
    m <- m |>
      leaflet::addCircleMarkers(
        data = dummy,
        lng = ~lng,
        lat = ~lat,
        group = pt_layer_group_name("USGS streamgages"),
        layerId = "pt_usgs_streamgages_dummy",
        radius = 0.001,
        stroke = FALSE,
        opacity = 0,
        fillOpacity = 0,
        options = leaflet::pathOptions(pane = "pane_points", interactive = FALSE)
      )

    ## RF043c:
    ##   Register a companion label overlay row for the inline lbl checkbox.
    ##   Actual streamgage labels are drawn/filtered in the browser-side
    ##   streamgage helper so labels follow the current Local streamgage filters.
    if (isTRUE(map_display$add_labels)) {
      m <- m |>
        leaflet::addCircleMarkers(
          data = dummy,
          lng = ~lng,
          lat = ~lat,
          group = pt_layer_group_name("Labels: USGS streamgages"),
          layerId = "pt_usgs_streamgages_label_dummy",
          radius = 0.001,
          stroke = FALSE,
          opacity = 0,
          fillOpacity = 0,
          options = leaflet::pathOptions(pane = "pane_labels_pts", interactive = FALSE)
        ) |>
        leaflet::hideGroup(pt_layer_group_name("Labels: USGS streamgages"))
    }

    m <- pt_add_usgs_streamgage_browser_layer(
      m = m,
      usgs_sw = usgs_sw,
      group_name = pt_layer_group_name("USGS streamgages")
    )
  }

  # ---- 4.2 USGS groundwater wells ------------------------------------------
  
  if (isTRUE(map_display$add_usgs_wells)) {
    
    ## Fallbacks allow the map to build even if the core cache was not rebuilt.
    if (!"well_fill_col" %in% names(usgs_gw)) {
      usgs_gw$well_fill_col <- "#BDBDBD"
    }
    
    if (!"well_stroke_col" %in% names(usgs_gw)) {
      usgs_gw$well_stroke_col <- "#4D4D4D"
    }
    
    if (!"well_radius" %in% names(usgs_gw)) {
      usgs_gw$well_radius <- 3
    }
    
    if (!"hover_text" %in% names(usgs_gw)) {
      usgs_gw$hover_text <- paste0("USGS ", usgs_gw$site_no)
    }

    if (!"well_is_nested" %in% names(usgs_gw)) {
      usgs_gw$well_is_nested <- FALSE
    }

    if (!"well_nested_dash_array" %in% names(usgs_gw)) {
      usgs_gw$well_nested_dash_array <- NA_character_
    }
    
    ## Recent-feed ring fallback logic.
    ##
    ## Preferred field:
    ## - well_recent_feed_ring, created in the core cache.
    ##
    ## If the cache has not yet been rebuilt, fall back to the older
    ## well_is_active field if present. Only as a last resort do we infer from
    ## source status, because source active/inactive status can be stale and is
    ## no longer the intended green-ring basis.
    if (!"well_recent_feed_ring" %in% names(usgs_gw)) {
      if ("well_is_active" %in% names(usgs_gw)) {
        usgs_gw$well_recent_feed_ring <- dplyr::coalesce(as.logical(usgs_gw$well_is_active), FALSE)
      } else {
        status_candidates <- c(
          "site_status",
          "siteStatus",
          "site_status_cd",
          "status",
          "status_cd",
          "station_status"
        )

        status_field <- status_candidates[status_candidates %in% names(usgs_gw)][1]

        if (is.na(status_field)) {
          usgs_gw$well_recent_feed_ring <- FALSE
        } else {
          status_lower <- tolower(trimws(as.character(usgs_gw[[status_field]])))
          usgs_gw$well_recent_feed_ring <- dplyr::case_when(
            status_lower %in% c("active", "a") ~ TRUE,
            grepl("\\bactive\\b", status_lower) &
              !grepl("\\binactive\\b|\\bdiscontinued\\b", status_lower) ~ TRUE,
            TRUE ~ FALSE
          )
        }
      }
    }

    ## Recent-feed ring styling.
    ##
    ## Design decision:
    ## - Fill color remains dedicated to cached/latest groundwater-level depth.
    ## - Thin pink outline means the well is included in the Ops Live
    ##   groundwater layer/recent-feed candidate list.  This matches the newer
    ##   Local catalog convention used by streamgages and CNRFC catalog layers.
    ## - Nested/co-located wells are handled below as compact grouped symbols,
    ##   so the individual static well markers no longer need a competing
    ##   dashed outline.
    ## Match the Local groundwater point fill/stroke bins to the Ops Live
    ## groundwater depth-to-water legend.  This is intentionally done at draw
    ## time so build_final_map_only() can pick up the change without requiring
    ## a core-cache rebuild.
    pt_usgs_gw_latest_wl_for_style <- if ("gwpop_latest_wl" %in% names(usgs_gw)) {
      suppressWarnings(as.numeric(usgs_gw$gwpop_latest_wl))
    } else if ("latest_wl_ft_bgs" %in% names(usgs_gw)) {
      suppressWarnings(as.numeric(usgs_gw$latest_wl_ft_bgs))
    } else {
      rep(NA_real_, nrow(usgs_gw))
    }

    usgs_gw$well_fill_col <- dplyr::case_when(
      !is.na(pt_usgs_gw_latest_wl_for_style) & pt_usgs_gw_latest_wl_for_style < 0 ~ "#756BB1",
      !is.na(pt_usgs_gw_latest_wl_for_style) & pt_usgs_gw_latest_wl_for_style >= 0 & pt_usgs_gw_latest_wl_for_style < 25 ~ "#2B8CBE",
      !is.na(pt_usgs_gw_latest_wl_for_style) & pt_usgs_gw_latest_wl_for_style >= 25 & pt_usgs_gw_latest_wl_for_style < 100 ~ "#7BCCC4",
      !is.na(pt_usgs_gw_latest_wl_for_style) & pt_usgs_gw_latest_wl_for_style >= 100 & pt_usgs_gw_latest_wl_for_style < 250 ~ "#A1D99B",
      !is.na(pt_usgs_gw_latest_wl_for_style) & pt_usgs_gw_latest_wl_for_style >= 250 & pt_usgs_gw_latest_wl_for_style < 500 ~ "#FEE391",
      !is.na(pt_usgs_gw_latest_wl_for_style) & pt_usgs_gw_latest_wl_for_style >= 500 & pt_usgs_gw_latest_wl_for_style < 1000 ~ "#FEC44F",
      !is.na(pt_usgs_gw_latest_wl_for_style) & pt_usgs_gw_latest_wl_for_style >= 1000 ~ "#BD0026",
      TRUE ~ "#F2F2F2"
    )

    usgs_gw$well_stroke_col <- dplyr::case_when(
      !is.na(pt_usgs_gw_latest_wl_for_style) & pt_usgs_gw_latest_wl_for_style < 0 ~ "#4A1486",
      !is.na(pt_usgs_gw_latest_wl_for_style) & pt_usgs_gw_latest_wl_for_style >= 0 & pt_usgs_gw_latest_wl_for_style < 25 ~ "#08589E",
      !is.na(pt_usgs_gw_latest_wl_for_style) & pt_usgs_gw_latest_wl_for_style >= 25 & pt_usgs_gw_latest_wl_for_style < 100 ~ "#08589E",
      !is.na(pt_usgs_gw_latest_wl_for_style) & pt_usgs_gw_latest_wl_for_style >= 100 & pt_usgs_gw_latest_wl_for_style < 250 ~ "#238B45",
      !is.na(pt_usgs_gw_latest_wl_for_style) & pt_usgs_gw_latest_wl_for_style >= 250 & pt_usgs_gw_latest_wl_for_style < 500 ~ "#B8860B",
      !is.na(pt_usgs_gw_latest_wl_for_style) & pt_usgs_gw_latest_wl_for_style >= 500 & pt_usgs_gw_latest_wl_for_style < 1000 ~ "#A63603",
      !is.na(pt_usgs_gw_latest_wl_for_style) & pt_usgs_gw_latest_wl_for_style >= 1000 ~ "#7F0000",
      TRUE ~ "#BDBDBD"
    )

    usgs_gw <- usgs_gw %>%
      dplyr::mutate(
        well_recent_feed_ring = dplyr::coalesce(
          as.logical(.data$well_recent_feed_ring),
          FALSE
        ),
        well_stroke_col_display = dplyr::if_else(
          .data$well_recent_feed_ring,
          "#ff00cc",       # Ops Live subset halo
          .data$well_stroke_col
        ),
        well_stroke_weight_display = dplyr::if_else(
          .data$well_recent_feed_ring,
          1.55,
          0.95
        ),
        well_dash_array_display = NA_character_
      )

    ## Register the overlay-group checkbox with a single invisible dummy marker.
    ## The visible dense well catalog is created by the viewport-virtualized
    ## browser layer. The dummy remains only to register the standard Leaflet
    ## overlay checkbox; it is not part of the visible well count.
    dummy <- data.frame(lng = -170, lat = 10)
    m <- m |>
      leaflet::addCircleMarkers(
        data = dummy,
        lng = ~lng,
        lat = ~lat,
        group = pt_layer_group_name("USGS monitoring wells"),
        layerId = "pt_usgs_wells_dummy",
        radius = 0.001,
        stroke = FALSE,
        opacity = 0,
        fillOpacity = 0,
        options = leaflet::pathOptions(pane = "pane_points", interactive = FALSE)
      )

    m <- pt_add_usgs_well_virtualized_browser_layer(
      m = m,
      usgs_gw = usgs_gw,
      group_name = pt_layer_group_name("USGS monitoring wells")
    )
  }
  
  m
}
