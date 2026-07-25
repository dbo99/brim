# ==== leaflet_layer_local_well_spring_helpers.r ================================================
##
## PURPOSE:
##   BLM groundwater well inventory and springs browser-managed local layers.
##
## NOTE:
##   Extracted from leaflet_layer_helpers.r as a maintainability-only split.
##   Function names and behavior are intentionally unchanged.

# ==== 4.4 Springs layer =======================================================
##
## Browser-managed dense point layer.  The Leaflet layer-control row is
## registered with a hidden dummy marker; a separate viewport controller keeps
## compact analytical records columnar, aggregates below zoom 11, and mounts
## only buffered-view exact points at higher zoom.  This avoids a global
## 27k-marker/MarkerCluster population while retaining every spring record.


# ==== 4.w BLM groundwater-well inventory local layers =======================
##
## PURPOSE:
##   Add the two small BLM groundwater-well inventory layers normalized by 18_
##   and distance-enriched by 63_:
##
##     - BLM-drilled wells | NOC
##     - GW wells | 2025 Mojave-BLM limited field check
##
## DESIGN FOR 048c:
##   This first visible-layer patch intentionally adds only the browser-managed
##   point display, clustering, hover, and popup.  Dynamic tan legends, BLM
##   filters, and inline lbl companion layers are added in the next patch so we
##   can test the source/cache/display path before layering on controller state.
##
##   Use L.marker + divIcon + MarkerClusterGroup rather than addCircleMarkers.
##   This mirrors the stable dense Local point scaffolds and avoids the sluggish
##   SVG/native-circle path that caused trouble for Springs.

pt_add_blm_gw_well_inventory_browser_layer <- function(m,
                                                       well_records = NULL,
                                                       group_name,
                                                       label_group_name,
                                                       layer_kind = c("noc", "mojave_2025")) {

  layer_kind <- match.arg(layer_kind)

  if (!is.data.frame(well_records) || nrow(well_records) == 0) return(m)

  rec <- well_records[
    !is.na(well_records$pt_lat) &
      !is.na(well_records$pt_lng) &
      abs(suppressWarnings(as.numeric(well_records$pt_lat))) <= 90 &
      abs(suppressWarnings(as.numeric(well_records$pt_lng))) <= 180,
    ,
    drop = FALSE
  ]

  if (nrow(rec) == 0) return(m)

  legend_title <- if (identical(layer_kind, "noc")) {
    "BLM-drilled wells | NOC"
  } else {
    "2025 Mojave-BLM limited field check"
  }

  js <- r"---(
function(el, x, data) {
  var map = this;
  var groupName = data && data.groupName ? String(data.groupName) : '';
  var labelGroupName = data && data.labelGroupName ? String(data.labelGroupName) : '';
  var layerKind = data && data.layerKind ? String(data.layerKind) : 'noc';
  var legendTitle = data && data.legendTitle ? String(data.legendTitle) : groupName;
  var labelMinZoom = layerKind === 'noc' ? 9 : 10;

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
          r[keys[j]] = Array.isArray(rows[keys[j]]) ? rows[keys[j]][i] : rows[keys[j]];
        }
        out.push(r);
      }
      return out;
    }
    return [];
  }

  var records = rowsToArray(data && data.records).filter(function(r) {
    return r && r.pt_lat != null && r.pt_lng != null;
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

  function escLoose(v) {
    return String(v == null ? '' : v)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  function fmt(n) {
    if (n === null || n === undefined || isNaN(Number(n))) return '0';
    return Number(n).toLocaleString();
  }

  function boolVal(v) {
    if (v === true) return true;
    if (v === false) return false;
    var s = String(v == null ? '' : v).trim().toLowerCase();
    if (['true','t','1','yes','y'].indexOf(s) >= 0) return true;
    if (['false','f','0','no','n'].indexOf(s) >= 0) return false;
    return null;
  }

  function cleanNumberText(v) {
    var s = String(v == null ? '' : v).trim();
    if (!s) return '';
    s = s.replace(/,/g, '');
    var n = Number(s);
    if (isNaN(n) || n < 0) return '';
    return String(n);
  }

  function cleanWyText(v) {
    var s = String(v == null ? '' : v).trim();
    if (!s) return '';
    s = s.replace(/[^0-9]/g, '');
    if (!s) return '';
    var n = parseInt(s, 10);
    if (isNaN(n) || n < 1800 || n > 2200) return '';
    return String(n);
  }

  function waterYearFromDate(v) {
    if (!has(v)) return null;
    var s = String(v).trim();

    // Prefer an explicit yyyy-mm-dd / yyyy/mm/dd value when available.
    var m = s.match(/(\d{4})[-\/](\d{1,2})[-\/](\d{1,2})/);
    if (m) {
      var yr = parseInt(m[1], 10);
      var mo = parseInt(m[2], 10);
      if (!isNaN(yr) && !isNaN(mo) && yr >= 1800 && yr <= 2200 && mo >= 1 && mo <= 12) {
        return mo >= 10 ? yr + 1 : yr;
      }
    }

    // NOC completion dates have appeared in multiple exported formats, including
    // m/d/yyyy.  Treat Oct-Dec completions as the following water year.
    m = s.match(/(\d{1,2})[-\/](\d{1,2})[-\/](\d{2,4})/);
    if (m) {
      var mo2 = parseInt(m[1], 10);
      var yr2 = parseInt(m[3], 10);
      if (!isNaN(yr2) && yr2 < 100) yr2 += (yr2 >= 70 ? 1900 : 2000);
      if (!isNaN(yr2) && !isNaN(mo2) && yr2 >= 1800 && yr2 <= 2200 && mo2 >= 1 && mo2 <= 12) {
        return mo2 >= 10 ? yr2 + 1 : yr2;
      }
    }

    // Some NOC records only carry a completion year or a label containing one.
    // Use that year directly so broad ranges such as 1900-2026 do not filter
    // every dated record out.
    m = s.match(/\b(18\d{2}|19\d{2}|20\d{2}|21\d{2}|2200)\b/);
    if (m) {
      var yr3 = parseInt(m[1], 10);
      if (!isNaN(yr3)) return yr3;
    }

    return null;
  }

  function currentWaterYear() {
    var d = new Date();
    var y = d.getFullYear();
    // JavaScript months are zero-based: 9 = October.  The current water year
    // starts on Oct. 1 and is named for the ending calendar year.
    return d.getMonth() >= 9 ? y + 1 : y;
  }

  function cleanWyRangeTexts(fromValue, toValue) {
    var fromRaw = String(fromValue == null ? '' : fromValue);
    var toRaw = String(toValue == null ? '' : toValue);

    // Allow users to paste/type a full range such as "1900-2026" in either WY box.
    var combo = (fromRaw + ' ' + toRaw).trim();
    var years = combo.match(/\b(18\d{2}|19\d{2}|20\d{2}|21\d{2}|2200)\b/g) || [];
    if (years.length >= 2) {
      var a = parseInt(years[0], 10);
      var b = parseInt(years[1], 10);
      if (!isNaN(a) && !isNaN(b)) {
        if (a > b) { var tmp = a; a = b; b = tmp; }
        return {from: String(a), to: String(b)};
      }
    }

    var from = cleanWyText(fromRaw);
    var to = cleanWyText(toRaw);

    // Convenience: typing only a starting WY means "from that water year
    // through the current water year".  Leave both boxes blank as unfiltered.
    if (from && !to && !cleanWyText(toRaw)) to = String(currentWaterYear());

    if (from && to && Number(from) > Number(to)) {
      var t = from; from = to; to = t;
    }
    return {from: from, to: to};
  }

  function norm(s) {
    return String(s == null ? '' : s)
      .replace(/&amp;/g, '&')
      .replace(/[–—]/g, '-')
      .toLowerCase()
      .replace(/points\s*-\s*/g, '')
      // Do NOT strip a leading Labels prefix here. The source layer and its
      // companion label layer often share the same base name, and stripping
      // Labels made source overlayadd events look like label overlayadd events.
      // That caused BLM GW well labels to draw whenever the source layer was
      // turned on, even when the inline lbl checkbox was unchecked.
      .replace(/monitoring sites\s*\/\s*records\s*-\s*/g, '')
      .replace(/\s*\([^)]*\)\s*$/g, '')
      .replace(/\s+/g, ' ')
      .trim();
  }

  var targetNorm = norm(groupName);
  var labelTargetNorm = norm(labelGroupName);

  function eventMatches(evt) {
    if (!evt) return false;
    if (norm(evt.name) === targetNorm) return true;
    if (evt.layer && evt.layer.options) {
      var bits = [evt.layer.options.group, evt.layer.options.name, evt.layer.options.layerId].join(' ');
      return norm(bits) === targetNorm;
    }
    return false;
  }

  function labelEventMatches(evt) {
    if (!evt) return false;
    if (norm(evt.name) === labelTargetNorm) return true;
    if (evt.layer && evt.layer.options) {
      var bits = [evt.layer.options.group, evt.layer.options.name, evt.layer.options.layerId].join(' ');
      return norm(bits) === labelTargetNorm;
    }
    return false;
  }

  function scanControl(target) {
    if (typeof document === 'undefined' || !document.querySelectorAll) return null;
    var labels = document.querySelectorAll('.leaflet-control-layers-overlays label');
    for (var i = 0; i < labels.length; i++) {
      var label = labels[i];
      var full = label.getAttribute ? (label.getAttribute('data-pt-layer-full-name') || '') : '';
      var text = label.textContent || label.innerText || '';
      if (norm(full || text) !== target && norm(text) !== target) continue;
      var input = label.querySelector ? label.querySelector('input[type="checkbox"]') : null;
      if (input) return !!input.checked;
    }
    return null;
  }

  function makeTooltip(r) {
    var nm = has(r.well_name_display) ? r.well_name_display : 'Unnamed well';
    var out = esc(nm);
    if (has(r.hover_line2)) out += '<br/>' + esc(r.hover_line2);
    return out;
  }

  function splitTrailingQaRows(html) {
    var body = has(html) ? String(html) : '';
    var qaRows = [];

    // Keep machine/QC identifiers at the very bottom of the popup, matching
    // the SWRCB POD popup pattern.  The source popup_html usually already
    // places this near the bottom, but appendBlmDistancePopup() adds BLM
    // distance after it; move the QA row below any appended screening rows.
    var patterns = [
      // Most NOC popup rows are prebuilt upstream and may not carry the
      // standard pt2-popup-row class.  Match the full enclosing div by label
      // rather than by class so the QA row can be moved below any appended
      // screening rows such as BLM distance.
      /<div\b[^>]*>\s*<b>\s*NOC\s+QA\s+IDs:\s*<\/b>[\s\S]*?<\/div>/ig,
      /<div\b[^>]*>\s*<b>\s*GIS\s+QA\/QC[^<]*:<\/b>[\s\S]*?<\/div>/ig,
      /<div\b[^>]*>\s*<b>\s*GIS\s+OBJECTID[^<]*:<\/b>[\s\S]*?<\/div>/ig
    ];

    patterns.forEach(function(re) {
      body = body.replace(re, function(match) {
        qaRows.push(match);
        return '';
      });
    });

    return {body: body, qa: qaRows.join('')};
  }

  function appendBlmDistancePopup(html, r) {
    var pieces = splitTrailingQaRows(html);
    var out = pieces.body || '';
    var dist = has(r.blm_distance_popup) ? r.blm_distance_popup : (has(r.blm_distance_label) ? r.blm_distance_label : null);
    if (has(dist)) {
      out += '<div class="pt2-popup-row"><b>BLM distance:</b> ' + esc(dist) + '</div>';
    }
    out += pieces.qa || '';
    return out;
  }

  function makePopup(r) {
    return appendBlmDistancePopup(r.popup_html, r);
  }

  function makeIcon(r) {
    var html, size = 14, cls = 'pt-blm-gw-well-marker';
    if (layerKind === 'noc') {
      size = 12;
      html = '<span class="pt-blm-noc-well-dot" style="width:' + size + 'px;height:' + size + 'px;background:#F4A3C4;border:2px solid #2B6CB0;border-radius:50%;display:block;box-sizing:border-box;box-shadow:0 0 0 1px rgba(255,255,255,0.75);"></span>';
    } else {
      var status = has(r.well_present_key) ? String(r.well_present_key) : 'unknown';
      if (status === 'present') {
        size = 13;
        html = '<span class="pt-mojave-well-present" style="width:' + size + 'px;height:' + size + 'px;background:#31A354;border:2px solid #FFFFFF;border-radius:50%;display:block;box-sizing:border-box;box-shadow:0 0 0 1px rgba(0,0,0,0.45);"></span>';
      } else if (status === 'not_found') {
        size = 16;
        html = '<span class="pt-mojave-well-not-found" style="width:' + size + 'px;height:' + size + 'px;display:block;text-align:center;line-height:' + size + 'px;color:#CB181D;font-size:18px;font-weight:900;text-shadow:-1px -1px 0 #fff,1px -1px 0 #fff,-1px 1px 0 #fff,1px 1px 0 #fff;">×</span>';
      } else {
        size = 16;
        html = '<span class="pt-mojave-well-unknown" style="width:' + size + 'px;height:' + size + 'px;display:block;text-align:center;line-height:' + size + 'px;color:#6B4E16;background:#FEE391;border:1px solid #8C6D31;border-radius:50%;font-size:12px;font-weight:800;box-sizing:border-box;">?</span>';
      }
    }
    return L.divIcon({className: cls, html: html, iconSize: L.point(size, size), iconAnchor: L.point(size / 2, size / 2), popupAnchor: L.point(0, -Math.max(6, size / 2))});
  }

  function makeMarker(r) {
    var lat = Number(r.pt_lat), lng = Number(r.pt_lng);
    if (isNaN(lat) || isNaN(lng)) return null;
    var marker = L.marker([lat, lng], {icon: makeIcon(r), keyboard: false});
    marker.bindTooltip(makeTooltip(r), {direction: 'auto', opacity: 0.92, sticky: true, className: 'pt-local-hover-tooltip'});
    marker.bindPopup(makePopup(r), {maxWidth: 420, maxHeight: 360, autoPanPadding: L.point(20, 20)});
    return marker;
  }

  function labelText(r) {
    var s = has(r.well_name_display) ? String(r.well_name_display).trim() : '';
    if (!s || /^unnamed\s+well$/i.test(s)) return '';
    return s;
  }

  function makeLabelMarker(r) {
    if (!labelsActive || map.getZoom() < labelMinZoom) return null;
    var txt = labelText(r);
    if (!txt) return null;
    var lat = Number(r.pt_lat), lng = Number(r.pt_lng);
    if (isNaN(lat) || isNaN(lng)) return null;
    return L.marker([lat, lng], {
      icon: L.divIcon({
        className: 'pt-blm-gw-well-label-divicon',
        html: '<span>' + escLoose(txt) + '</span>',
        iconSize: null,
        iconAnchor: [0, 0]
      }),
      interactive: false,
      keyboard: false
    });
  }

  function makeMarkerContainer() {
    return (L.markerClusterGroup ? L.markerClusterGroup({
      disableClusteringAtZoom: 9,
      spiderfyOnMaxZoom: true,
      showCoverageOnHover: false,
      chunkedLoading: false,
      animateAddingMarkers: false,
      maxClusterRadius: function(z) { if (z <= 5) return 45; if (z <= 7) return 32; if (z <= 9) return 22; return 14; }
    }) : L.layerGroup());
  }

  var markers = makeMarkerContainer();
  var labelLayer = L.layerGroup();

  var filters = {blm: 'any', blmManual: '', status: 'all', wyFrom: '', wyTo: ''};
  var layerActive = false;
  var labelsActive = false;
  var builtOnce = false;
  var legendUserHidden = false;
  var lastDrawn = 0;
  var lastFilteredRecords = [];

  function passBlm(r) {
    if (filters.blm === 'any') return true;
    var on = boolVal(r.on_blm_ca);
    var d = r.dist_to_blm_mi == null || r.dist_to_blm_mi === '' ? null : Number(r.dist_to_blm_mi);
    if (filters.blm === 'on') return on === true;
    if (filters.blm === 'off') return on === false;
    if (filters.blm === '1') return (on === true) || (!isNaN(d) && d <= 1);
    if (filters.blm === '5') return (on === true) || (!isNaN(d) && d <= 5);
    if (filters.blm === 'manual') {
      var maxMi = Number(filters.blmManual);
      if (isNaN(maxMi)) return true;
      return (on === true) || (!isNaN(d) && d <= maxMi);
    }
    return true;
  }

  function passStatus(r) {
    if (layerKind !== 'mojave_2025' || filters.status === 'all') return true;
    var s = has(r.well_present_key) ? String(r.well_present_key) : 'unknown';
    return s === filters.status;
  }

  function passWaterYear(r) {
    if (layerKind !== 'noc') return true;
    var from = filters.wyFrom ? Number(filters.wyFrom) : null;
    var to = filters.wyTo ? Number(filters.wyTo) : null;
    if ((from === null || isNaN(from)) && (to === null || isNaN(to))) return true;
    var wy = waterYearFromDate(r.well_completion_wy || r.install_wy || r.well_completion_date || r.hover_line2);
    if (wy === null || isNaN(wy)) return false;
    if (from !== null && !isNaN(from) && wy < from) return false;
    if (to !== null && !isNaN(to) && wy > to) return false;
    return true;
  }

  function filteredRecords() {
    return records.filter(function(r) { return passBlm(r) && passStatus(r) && passWaterYear(r); });
  }

  function hardClearMarkers() {
    try { if (map.hasLayer(markers)) map.removeLayer(markers); } catch(e) {}
    try { if (markers.clearLayers) markers.clearLayers(); } catch(e) {}
    ['_featureGroup', '_nonPointGroup'].forEach(function(k) { try { if (markers[k] && markers[k].clearLayers) markers[k].clearLayers(); } catch(e) {} });
  }

  function clearLabels() {
    try { if (map.hasLayer(labelLayer)) map.removeLayer(labelLayer); } catch(e) {}
    try { labelLayer.clearLayers(); } catch(e) {}
  }

  function rebuildLabels() {
    clearLabels();
    if (!layerActive || !labelsActive || map.getZoom() < labelMinZoom) return;
    var layersToAdd = [];
    lastFilteredRecords.forEach(function(r) { var lm = makeLabelMarker(r); if (lm) layersToAdd.push(lm); });
    layersToAdd.forEach(function(lm) { labelLayer.addLayer(lm); });
    if (layersToAdd.length > 0 && !map.hasLayer(labelLayer)) labelLayer.addTo(map);
  }

  function rebuild() {
    // Recreate the MarkerClusterGroup on every filtered redraw instead of only
    // clearing it.  Leaflet.markercluster can occasionally leave a stale cluster
    // icon behind after rapid filter changes or repeated add/remove cycles; a
    // fresh container keeps cluster counts tied to the active filter state.
    hardClearMarkers();
    markers = makeMarkerContainer();
    var rows = filteredRecords();
    lastFilteredRecords = rows;
    var layersToAdd = [];
    rows.forEach(function(r) { var marker = makeMarker(r); if (marker) layersToAdd.push(marker); });
    builtOnce = true;
    lastDrawn = layersToAdd.length;
    if (typeof markers.addLayers === 'function') markers.addLayers(layersToAdd);
    else layersToAdd.forEach(function(marker) { markers.addLayer(marker); });
    if (layerActive && !map.hasLayer(markers)) markers.addTo(map);
    rebuildLabels();
    updateLegend();
    return {drawn: layersToAdd.length, total: records.length, active: layerActive};
  }

  function resetFilters() { filters = {blm: 'any', blmManual: '', status: 'all', wyFrom: '', wyTo: ''}; }

  function syncActive() {
    var checked = scanControl(targetNorm);
    layerActive = checked === null ? layerActive : checked;
    if (layerActive) {
      if (!builtOnce) rebuild();
      else { if (!map.hasLayer(markers)) markers.addTo(map); rebuildLabels(); updateLegend(); }
    } else {
      hardClearMarkers(); clearLabels(); builtOnce = false; lastDrawn = 0; lastFilteredRecords = [];
      resetFilters(); legendUserHidden = false; updateLegend();
    }
  }

  function syncLabels() {
    var checked = scanControl(labelTargetNorm);
    labelsActive = checked === null ? labelsActive : checked;
    rebuildLabels();
  }

  function sourceCount(key) {
    if (layerKind === 'noc') return records.length;
    return records.filter(function(r) { return String(r.well_present_key || 'unknown') === key; }).length;
  }

  function buttonHtml(kind, value, label) {
    var active = filters[kind] === value;
    return '<button type="button" class="pt-blm-gw-filter-btn' + (active ? ' active' : '') + '" data-kind="' + esc(kind) + '" data-value="' + esc(value) + '">' + esc(label) + '</button>';
  }

  function symRow(sym, label, count) {
    return '<div class="pt-blm-gw-legend-row">' + sym + '<span class="pt-blm-gw-legend-label">' + esc(label) + '</span><span class="pt-blm-gw-legend-count">' + fmt(count) + '</span></div>';
  }

  function legendHtml() {
    var rows = filteredRecords();
    var html = '';
    html += '<button type="button" class="pt-blm-gw-legend-close" title="Hide legend">&times;</button>';
    html += '<div class="pt-blm-gw-legend-title">' + esc(legendTitle) + '</div>';
    if (layerKind === 'noc') {
      html += symRow('<span class="pt-blm-gw-noc-symbol"></span>', 'NOC BLM-drilled well', records.length);
    } else {
      html += symRow('<span class="pt-blm-gw-present-symbol"></span>', 'Well present', sourceCount('present'));
      html += symRow('<span class="pt-blm-gw-notfound-symbol">×</span>', 'Well not found', sourceCount('not_found'));
      html += symRow('<span class="pt-blm-gw-unknown-symbol">?</span>', 'Unknown / not verified', sourceCount('unknown'));
    }
    html += '<div class="pt-blm-gw-legend-showing">Showing ' + fmt(rows.length) + ' / ' + fmt(records.length) + ' well record(s).</div>';
    html += '<div class="pt-blm-gw-filter-box">';
    if (layerKind === 'mojave_2025') {
      html += '<div class="pt-blm-gw-filter-line">Status: ' + buttonHtml('status','all','all') + buttonHtml('status','present','found') + buttonHtml('status','not_found','not found') + buttonHtml('status','unknown','unknown') + '</div>';
    }
    html += '<div class="pt-blm-gw-filter-line">BLM max mi: ' +
      '<input type="text" inputmode="decimal" class="pt-blm-gw-filter-input pt-blm-gw-blm-manual" placeholder="mi" value="' + escLoose(filters.blmManual) + '"> ' +
      buttonHtml('blm','any','any') + buttonHtml('blm','on','on') + buttonHtml('blm','off','off') + buttonHtml('blm','1','≤1') + buttonHtml('blm','5','≤5') + '</div>';
    if (layerKind === 'noc') {
      html += '<div class="pt-blm-gw-filter-line">Install WY: ' +
        '<input type="text" inputmode="numeric" class="pt-blm-gw-filter-input pt-blm-gw-wy-from" placeholder="from" value="' + escLoose(filters.wyFrom) + '">–' +
        '<input type="text" inputmode="numeric" class="pt-blm-gw-filter-input pt-blm-gw-wy-to" placeholder="current" value="' + escLoose(filters.wyTo) + '"></div>';
      html += '<div class="pt-blm-gw-filter-note">Install-date coverage is limited; blank to-WY means current WY.</div>';
    }
    html += '<div class="pt-blm-gw-filter-actions"><button type="button" class="pt-blm-gw-apply-btn">Apply</button><button type="button" class="pt-blm-gw-reset-btn">Reset</button></div>';
    html += '<div class="pt-blm-gw-legend-note">BLM distance is screening-only; verify points near boundaries. Well name labels (lbl) display at zoom ' + labelMinZoom + '+.</div>';
    html += '</div>';
    return html;
  }

  var legendControl = null;
  function ensureLegendControl() {
    if (legendControl) return legendControl;
    legendControl = L.control({position: 'bottomleft'});
    legendControl.onAdd = function() {
      var div = L.DomUtil.create('div', 'leaflet-control pt-blm-gw-legend');

      // Keep the new BLM groundwater-well legends aligned with the established
      // Local monitoring legends (USGS streamgages, SWRCB PODs, Springs).
      // A CSS-only `margin-bottom` is not reliable here because Leaflet's
      // `.leaflet-bottom .leaflet-control` selector can override a single-class
      // legend rule. Set the offset inline on the actual control container so
      // the lower edge clears the Local GIS Uploads panel consistently.
      div.style.marginBottom = '74px';
      div.style.marginLeft = '10px';

      L.DomEvent.disableClickPropagation(div);
      L.DomEvent.disableScrollPropagation(div);
      return div;
    };
    legendControl.addTo(map);
    return legendControl;
  }

  function legendDiv() {
    if (!legendControl || !legendControl.getContainer) return null;
    return legendControl.getContainer();
  }

  function updateLegend() {
    ensureLegendControl();
    var div = legendDiv();
    if (!div) return;
    if (!layerActive || legendUserHidden) { div.style.display = 'none'; return; }
    div.style.display = '';
    div.innerHTML = legendHtml();
    if (window.BRIM && window.BRIM.legendCloseout) {
      window.BRIM.legendCloseout.wire(div, '.pt-blm-gw-legend-close', function(){
        legendUserHidden = true;
        updateLegend();
      }, {hide: false});
    } else {
      var close = div.querySelector('.pt-blm-gw-legend-close');
      if (close) close.addEventListener('click', function(e) { if (e) { e.preventDefault(); e.stopPropagation(); } legendUserHidden = true; updateLegend(); });
    }
    var btns = div.querySelectorAll('.pt-blm-gw-filter-btn');
    for (var i = 0; i < btns.length; i++) {
      btns[i].addEventListener('click', function(e) {
        if (e) { e.preventDefault(); e.stopPropagation(); }
        var kind = this.getAttribute('data-kind');
        var value = this.getAttribute('data-value');
        if (kind && value) {
          filters[kind] = value;
          if (kind === 'blm') filters.blmManual = '';
          rebuild();
        }
      });
    }

    function readTextFiltersFromLegend() {
      var blmInput = div.querySelector('.pt-blm-gw-blm-manual');
      var cleanedBlm = blmInput ? cleanNumberText(blmInput.value) : '';
      filters.blmManual = cleanedBlm;
      filters.blm = cleanedBlm ? 'manual' : (filters.blm === 'manual' ? 'any' : filters.blm);

      var wyFrom = div.querySelector('.pt-blm-gw-wy-from');
      var wyTo = div.querySelector('.pt-blm-gw-wy-to');
      if (wyFrom || wyTo) {
        var wyRange = cleanWyRangeTexts(wyFrom ? wyFrom.value : '', wyTo ? wyTo.value : '');
        filters.wyFrom = wyRange.from;
        filters.wyTo = wyRange.to;
      }
    }

    var applyBtn = div.querySelector('.pt-blm-gw-apply-btn');
    if (applyBtn) {
      applyBtn.addEventListener('click', function(e) {
        if (e) { e.preventDefault(); e.stopPropagation(); }
        readTextFiltersFromLegend();
        rebuild();
      });
    }

    var resetBtn = div.querySelector('.pt-blm-gw-reset-btn');
    if (resetBtn) {
      resetBtn.addEventListener('click', function(e) {
        if (e) { e.preventDefault(); e.stopPropagation(); }
        resetFilters();
        rebuild();
      });
    }

    function bindEnterApply(selector) {
      var input = div.querySelector(selector);
      if (!input) return;
      input.addEventListener('keydown', function(e) {
        if (e && e.key === 'Enter') {
          e.preventDefault();
          e.stopPropagation();
          readTextFiltersFromLegend();
          rebuild();
        }
      });
    }

    bindEnterApply('.pt-blm-gw-blm-manual');
    bindEnterApply('.pt-blm-gw-wy-from');
    bindEnterApply('.pt-blm-gw-wy-to');
  }

  function installStyle() {
    if (typeof document === 'undefined' || !document.head || document.getElementById('pt-blm-gw-local-style')) return;
    var style = document.createElement('style');
    style.id = 'pt-blm-gw-local-style';
    style.textContent =
      '.pt-blm-gw-well-marker{background:transparent;border:0;}' +
      '.pt-blm-gw-well-label-divicon{background:transparent;border:0;white-space:nowrap;pointer-events:none;}' +
      '.pt-blm-gw-well-label-divicon span{display:inline-block;transform:translate(-50%,-13px);font:700 11px/1.1 Arial,sans-serif;color:#5a3a0f;background:rgba(255,255,255,0.64);border:1px solid rgba(90,58,15,0.22);border-radius:3px;padding:1px 3px;text-shadow:0 1px 2px #fff,1px 0 2px #fff,-1px 0 2px #fff,0 -1px 2px #fff;box-shadow:0 1px 2px rgba(0,0,0,0.10);max-width:190px;overflow:hidden;text-overflow:ellipsis;}' +
      '.pt-blm-gw-legend{background:rgba(246,239,222,0.96);border:1px solid rgba(112,103,83,0.55);border-radius:6px;box-shadow:0 1px 5px rgba(0,0,0,0.25);padding:7px 9px 8px 9px;width:258px;max-width:258px;font:11.5px/1.22 Arial,sans-serif;color:#222;position:relative;margin-bottom:74px;}' +
      '.pt-blm-gw-legend-title{font-weight:700;font-size:12px;margin:0 18px 4px 0;}' +
      '.pt-blm-gw-legend-close{position:absolute;top:3px;right:5px;border:0;background:transparent;color:#555;font-size:16px;line-height:16px;cursor:pointer;padding:0 2px;}' +
      '.pt-blm-gw-legend-row{display:flex;align-items:center;gap:5px;margin:2px 0;}' +
      '.pt-blm-gw-legend-label{flex:1;min-width:0;}' +
      '.pt-blm-gw-legend-count{font-variant-numeric:tabular-nums;color:#555;}' +
      '.pt-blm-gw-noc-symbol{display:inline-block;width:11px;height:11px;background:#F4A3C4;border:2px solid #2B6CB0;border-radius:50%;box-sizing:border-box;}' +
      '.pt-blm-gw-present-symbol{display:inline-block;width:11px;height:11px;background:#31A354;border:1px solid #fff;border-radius:50%;box-shadow:0 0 0 1px rgba(0,0,0,0.35);box-sizing:border-box;}' +
      '.pt-blm-gw-notfound-symbol{display:inline-block;width:13px;height:13px;text-align:center;line-height:13px;color:#CB181D;font-size:16px;font-weight:900;text-shadow:-1px -1px 0 #fff,1px -1px 0 #fff,-1px 1px 0 #fff,1px 1px 0 #fff;}' +
      '.pt-blm-gw-unknown-symbol{display:inline-block;width:13px;height:13px;text-align:center;line-height:12px;color:#6B4E16;background:#FEE391;border:1px solid #8C6D31;border-radius:50%;font-size:10px;font-weight:800;box-sizing:border-box;}' +
      '.pt-blm-gw-legend-showing{margin-top:5px;color:#444;font-size:10.5px;}' +
      '.pt-blm-gw-filter-box{border-top:1px solid rgba(112,103,83,0.25);margin-top:5px;padding-top:5px;}' +
      '.pt-blm-gw-filter-line{margin:3px 0;}' +
      '.pt-blm-gw-filter-btn,.pt-blm-gw-apply-btn,.pt-blm-gw-reset-btn{border:1px solid rgba(112,103,83,0.65);background:rgba(255,255,255,0.92);border-radius:5px;padding:2px 5px;margin:1px 2px 1px 0;font:11px/1.1 Arial,sans-serif;cursor:pointer;}' +
      '.pt-blm-gw-filter-btn.active{background:rgba(221,211,173,0.98);color:#222;border-color:rgba(112,103,83,0.75);font-weight:700;}' +
      '.pt-blm-gw-filter-actions{margin:4px 0 2px 0;}' +
      '.pt-blm-gw-filter-note{margin:2px 0 3px 0;color:#666;font-size:10px;line-height:1.15;}' +
      '.pt-blm-gw-filter-input{font-size:10.5px;line-height:1.05;padding:1px 3px;margin:0 2px;border:1px solid rgba(112,103,83,0.55);border-radius:3px;background:#fff;vertical-align:baseline;width:42px;box-sizing:border-box;}' +
      '.pt-blm-gw-filter-input:focus{outline:1px solid rgba(55,101,157,0.55);}' +
      '.pt-blm-gw-legend-note{margin-top:4px;color:#666;font-size:10px;}';
    document.head.appendChild(style);
  }

  installStyle();
  ensureLegendControl();
  updateLegend();

  var apiName = layerKind === 'noc' ? 'BRIM_BLM_NOC_WELLS_LOCAL' : 'BRIM_MOJAVE_2025_WELLS_LOCAL';
  window[apiName] = {
    rebuild: rebuild,
    clear: function() { hardClearMarkers(); clearLabels(); builtOnce = false; resetFilters(); updateLegend(); },
    resetFilters: function() { resetFilters(); rebuild(); updateLegend(); },
    stats: function() { return {total: records.length, filtered: lastFilteredRecords.length, drawn: lastDrawn, active: layerActive, labels: labelsActive, filters: filters}; }
  };

  map.on('overlayadd', function(evt) {
    if (eventMatches(evt)) { resetFilters(); legendUserHidden = false; layerActive = true; syncActive(); }
    if (labelEventMatches(evt)) { labelsActive = true; syncLabels(); }
  });

  map.on('overlayremove', function(evt) {
    if (eventMatches(evt)) { layerActive = false; hardClearMarkers(); clearLabels(); builtOnce = false; lastDrawn = 0; lastFilteredRecords = []; resetFilters(); legendUserHidden = false; updateLegend(); }
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
  setTimeout(syncActive, 500);
  setTimeout(syncLabels, 500);
}
)---"

  htmlwidgets::onRender(
    m,
    js,
    data = list(
      groupName = group_name,
      labelGroupName = label_group_name,
      layerKind = layer_kind,
      legendTitle = legend_title,
      records = rec
    )
  )
}

pt_prepare_blm_gw_well_inventory_records <- function(wells) {

  if ((!inherits(wells, "sf") && !is.data.frame(wells)) || nrow(wells) == 0) {
    return(data.frame())
  }

  x <- wells
  coords <- NULL

  if (inherits(x, "sf")) {
    x_ll <- try({
      crs <- sf::st_crs(x)
      if (!is.na(crs)) sf::st_transform(x, 4326) else x
    }, silent = TRUE)
    if (inherits(x_ll, "try-error")) x_ll <- x
    coords <- suppressWarnings(sf::st_coordinates(sf::st_geometry(x_ll)))
  } else if (all(c("longitude", "latitude") %in% names(x))) {
    coords <- cbind(
      X = suppressWarnings(as.numeric(x$longitude)),
      Y = suppressWarnings(as.numeric(x$latitude))
    )
  }

  if (is.null(coords) || nrow(coords) != nrow(x)) {
    return(data.frame())
  }

  sx <- if (inherits(x, "sf")) sf::st_drop_geometry(x) else as.data.frame(x, stringsAsFactors = FALSE)
  sx$pt_lng <- as.numeric(coords[, "X"])
  sx$pt_lat <- as.numeric(coords[, "Y"])

  n <- nrow(sx)

  chr_col <- function(nm, default = NA_character_) {
    if (nm %in% names(sx)) {
      out <- as.character(sx[[nm]])
    } else if (length(default) == n) {
      out <- as.character(default)
    } else {
      out <- rep(as.character(default)[1], n)
    }
    out <- trimws(out)
    out[out %in% c("", "NA", "NaN", "null", "undefined")] <- NA_character_
    out
  }

  num_col <- function(nm, default = NA_real_) {
    if (nm %in% names(sx)) {
      suppressWarnings(as.numeric(gsub(",", "", as.character(sx[[nm]]))))
    } else if (length(default) == n) {
      suppressWarnings(as.numeric(default))
    } else {
      rep(as.numeric(default)[1], n)
    }
  }

  record_uid <- chr_col("record_uid")
  record_uid[is.na(record_uid)] <- paste0("blm_gw_well_", seq_len(n))[is.na(record_uid)]

  well_name <- chr_col("well_name_display", "Unnamed well")
  well_name[is.na(well_name)] <- "Unnamed well"

  hover_line1 <- chr_col("hover_line1", well_name)
  hover_line1[is.na(hover_line1)] <- well_name[is.na(hover_line1)]

  data.frame(
    record_uid = record_uid,
    pt_lat = sx$pt_lat,
    pt_lng = sx$pt_lng,
    source_key = chr_col("source_key"),
    source_display = chr_col("source_display"),
    source_short = chr_col("source_short"),
    layer_name = chr_col("layer_name"),
    source_record_id = chr_col("source_record_id"),
    well_name_display = well_name,
    hover_line1 = hover_line1,
    hover_line2 = chr_col("hover_line2"),
    well_completion_date = chr_col("well_completion_date"),
    well_present_key = chr_col("well_present_key"),
    well_present_display = chr_col("well_present_display"),
    well_monitored_key = chr_col("well_monitored_key"),
    well_monitored_display = chr_col("well_monitored_display"),
    depth_to_water_display = chr_col("depth_to_water_display"),
    popup_html = chr_col("popup_html", paste0("<b>", well_name, "</b>")),
    on_blm_ca = chr_col("on_blm_ca"),
    dist_to_blm_mi = num_col("dist_to_blm_mi"),
    dist_to_blm_ft = num_col("dist_to_blm_ft"),
    blm_distance_label = chr_col("blm_distance_label"),
    blm_distance_popup = chr_col("blm_distance_popup"),
    stringsAsFactors = FALSE
  ) |>
    dplyr::filter(!is.na(.data$pt_lat), !is.na(.data$pt_lng))
}

pt_add_blm_gw_well_inventory_single_layer <- function(m,
                                                      wells,
                                                      group_name,
                                                      layer_kind = c("noc", "mojave_2025"),
                                                      map_display = NULL) {

  layer_kind <- match.arg(layer_kind)
  if (is.null(map_display)) map_display <- list()

  rec <- pt_prepare_blm_gw_well_inventory_records(wells)
  if (!is.data.frame(rec) || nrow(rec) == 0) {
    message(group_name, " layer is empty; no points added.")
    return(m)
  }

  message(group_name, " Local display: browser-managed MarkerClusterGroup for ", nrow(rec), " well record(s).")

  dummy <- data.frame(lng = -170, lat = 10)
  label_group_name <- paste0("Labels: ", group_name)

  m <- m |>
    leaflet::addCircleMarkers(
      data = dummy,
      lng = ~lng,
      lat = ~lat,
      group = pt_layer_group_name(group_name),
      layerId = paste0("pt_blm_gw_well_inventory_dummy_", layer_kind),
      radius = 0.001,
      stroke = FALSE,
      opacity = 0,
      fillOpacity = 0,
      options = leaflet::pathOptions(pane = "pane_points", interactive = FALSE)
    )

  ## Register a companion Labels overlay row for the existing BRIM inline `lbl`
  ## checkbox machinery.  Visible labels are drawn by the browser controller so
  ## they follow BLM/status filters and the zoom threshold, rather than using a
  ## one-off checkbox or native label layer.
  if (isTRUE(map_display$add_labels)) {
    m <- m |>
      leaflet::addCircleMarkers(
        data = dummy,
        lng = ~lng,
        lat = ~lat,
        group = pt_layer_group_name(label_group_name),
        layerId = paste0("pt_blm_gw_well_inventory_label_dummy_", layer_kind),
        radius = 0.001,
        stroke = FALSE,
        opacity = 0,
        fillOpacity = 0,
        options = leaflet::pathOptions(pane = "pane_labels_pts", interactive = FALSE)
      ) |>
      leaflet::hideGroup(pt_layer_group_name(label_group_name))
  }

  pt_add_blm_gw_well_inventory_browser_layer(
    m = m,
    well_records = rec,
    group_name = pt_layer_group_name(group_name),
    label_group_name = pt_layer_group_name(label_group_name),
    layer_kind = layer_kind
  )
}

pt_add_blm_gw_well_inventory_layers <- function(m, noc_wells, mojave_wells, map_display) {

  if (isTRUE(map_display$add_blm_noc_drilled_wells)) {
    m <- pt_add_blm_gw_well_inventory_single_layer(
      m = m,
      wells = noc_wells,
      group_name = "BLM-drilled wells | NOC",
      layer_kind = "noc",
      map_display = map_display
    )
  }

  if (isTRUE(map_display$add_mojave_2025_gw_well_inventory)) {
    m <- pt_add_blm_gw_well_inventory_single_layer(
      m = m,
      wells = mojave_wells,
      group_name = "GW wells | 2025 Mojave-BLM limited field check",
      layer_kind = "mojave_2025",
      map_display = map_display
    )
  }

  m
}

pt_prepare_springs_virtualized_payload <- function(spring_records) {

  if (!is.data.frame(spring_records) || nrow(spring_records) == 0) {
    stop("Springs virtualized payload requires at least one record.", call. = FALSE)
  }

  required <- c("spring_id", "pt_lat", "pt_lng")
  missing_required <- setdiff(required, names(spring_records))
  if (length(missing_required) > 0) {
    stop(
      "Springs virtualized payload is missing required field(s): ",
      paste(missing_required, collapse = ", "),
      call. = FALSE
    )
  }

  rec <- as.data.frame(spring_records, stringsAsFactors = FALSE)
  rec$spring_id <- trimws(as.character(rec$spring_id))
  rec$pt_lat <- suppressWarnings(as.numeric(rec$pt_lat))
  rec$pt_lng <- suppressWarnings(as.numeric(rec$pt_lng))

  bad_id <- is.na(rec$spring_id) | rec$spring_id == ""
  if (any(bad_id)) {
    stop(
      "Springs virtualized payload found ",
      sum(bad_id),
      " record(s) without a source record identifier.",
      call. = FALSE
    )
  }
  duplicate_ids <- unique(rec$spring_id[duplicated(rec$spring_id)])
  if (length(duplicate_ids) > 0) {
    stop(
      "Springs virtualized payload found duplicate spring_id value(s): ",
      paste(utils::head(duplicate_ids, 8L), collapse = ", "),
      call. = FALSE
    )
  }

  valid_coordinates <- is.finite(rec$pt_lat) &
    is.finite(rec$pt_lng) &
    abs(rec$pt_lat) <= 90 &
    abs(rec$pt_lng) <= 180
  if (!all(valid_coordinates)) {
    stop(
      "Springs virtualized payload found ",
      sum(!valid_coordinates),
      " record(s) without a valid longitude/latitude; refusing to drop records.",
      call. = FALSE
    )
  }

  ## Use each double's exact hexadecimal representation as the grouping key.
  ## This groups only records whose retained numeric coordinates are identical;
  ## the original coordinate columns remain unchanged in the embedded payload.
  coordinate_key <- paste0(
    sprintf("%a", rec$pt_lng),
    "_",
    sprintf("%a", rec$pt_lat)
  )
  source_key <- if ("spring_source_key" %in% names(rec)) {
    as.character(rec$spring_source_key)
  } else {
    rep("", nrow(rec))
  }
  ord <- order(coordinate_key, source_key, rec$spring_id, na.last = TRUE)
  rec <- rec[ord, , drop = FALSE]
  coordinate_key <- coordinate_key[ord]

  coordinate_runs <- rle(coordinate_key)
  location_start_one <- cumsum(c(1L, head(coordinate_runs$lengths, -1L)))
  location_record_count <- as.integer(coordinate_runs$lengths)
  locations <- data.frame(
    record_start = as.integer(location_start_one - 1L),
    record_count = location_record_count,
    stringsAsFactors = FALSE
  )

  duplicate_sizes <- location_record_count[location_record_count > 1L]
  source_counts_table <- if ("spring_source_key" %in% names(rec)) {
    table(as.character(rec$spring_source_key), useNA = "ifany")
  } else {
    integer(0)
  }
  source_counts <- as.list(as.integer(source_counts_table))
  names(source_counts) <- names(source_counts_table)

  metadata <- list(
    schemaVersion = 1L,
    analyticalRecordCount = nrow(spring_records),
    validCoordinateCount = nrow(rec),
    recordCount = nrow(rec),
    uniqueCoordinateCount = nrow(locations),
    duplicateLocationCount = length(duplicate_sizes),
    recordsAtDuplicateLocations = sum(duplicate_sizes),
    maxRecordsAtLocation = if (length(duplicate_sizes) > 0) {
      max(duplicate_sizes)
    } else {
      1L
    },
    sourceCounts = source_counts,
    clusterToExactZoom = 11L,
    spatialCellDegrees = 0.25,
    coordinateGrouping = "exact retained numeric longitude/latitude pairs"
  )

  if (requireNamespace("jsonlite", quietly = TRUE)) {
    payload_json <- jsonlite::toJSON(
      list(records = rec, locations = locations, metadata = metadata),
      dataframe = "columns",
      na = "null",
      auto_unbox = TRUE,
      POSIXt = "ISO8601"
    )
    metadata$approximatePayloadBytes <- nchar(
      as.character(payload_json),
      type = "bytes"
    )
  } else {
    metadata$approximatePayloadBytes <- NA_real_
  }

  list(records = rec, locations = locations, metadata = metadata)
}


pt_add_springs_browser_layer <- function(
  m,
  spring_records = NULL,
  group_name = pt_layer_group_name("Springs"),
  label_group_name = pt_layer_group_name("Labels: Springs")
) {

  if (!is.data.frame(spring_records) || nrow(spring_records) == 0) return(m)

  payload <- pt_prepare_springs_virtualized_payload(spring_records)
  js_path <- file.path(
    "03_functions",
    "js",
    "leaflet_springs_local_virtualized.js"
  )
  if (!file.exists(js_path)) {
    stop("Missing virtualized Springs browser helper: ", js_path)
  }

  js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")
  payload$groupName <- group_name
  payload$labelGroupName <- label_group_name

  payload_mb <- suppressWarnings(
    as.numeric(payload$metadata$approximatePayloadBytes) / 1024^2
  )
  message(
    "Springs virtualized payload: ",
    format(payload$metadata$recordCount, big.mark = ","),
    " analytical records; ",
    format(payload$metadata$uniqueCoordinateCount, big.mark = ","),
    " exact mapped locations; ",
    format(payload$metadata$duplicateLocationCount, big.mark = ","),
    " multi-record locations; approximately ",
    if (is.finite(payload_mb)) format(round(payload_mb, 2), nsmall = 2) else "NA",
    " MiB before htmlwidgets wrapping."
  )

  htmlwidgets::onRender(m, js, data = payload)
}

pt_add_springs_layer <- function(m, springs, map_display) {

  if (!isTRUE(map_display$add_springs)) {
    return(m)
  }

  if ((!inherits(springs, "sf") && !is.data.frame(springs)) || nrow(springs) == 0) {
    message("Springs layer is empty; no points added.")
    return(m)
  }

  x <- springs

  coords <- NULL
  if (inherits(x, "sf")) {
    x_ll <- try({
      crs <- sf::st_crs(x)
      if (!is.na(crs)) sf::st_transform(x, 4326) else x
    }, silent = TRUE)
    if (inherits(x_ll, "try-error")) x_ll <- x
    coords <- suppressWarnings(sf::st_coordinates(sf::st_geometry(x_ll)))
  } else if (all(c("lon", "lat") %in% names(x))) {
    coords <- cbind(
      X = suppressWarnings(as.numeric(x$lon)),
      Y = suppressWarnings(as.numeric(x$lat))
    )
  } else if (all(c("longitude", "latitude") %in% names(x))) {
    coords <- cbind(
      X = suppressWarnings(as.numeric(x$longitude)),
      Y = suppressWarnings(as.numeric(x$latitude))
    )
  }

  if (is.null(coords) || nrow(coords) != nrow(x)) {
    stop(
      "Springs browser layer requires one point coordinate per analytical ",
      "record; refusing to skip or partially display the layer.",
      call. = FALSE
    )
  }

  sx <- if (inherits(x, "sf")) sf::st_drop_geometry(x) else as.data.frame(x, stringsAsFactors = FALSE)
  sx$pt_lng <- as.numeric(coords[, "X"])
  sx$pt_lat <- as.numeric(coords[, "Y"])

  n <- nrow(sx)

  chr_col <- function(nm, default = NA_character_) {
    if (nm %in% names(sx)) {
      out <- as.character(sx[[nm]])
    } else if (length(default) == n) {
      out <- as.character(default)
    } else {
      out <- rep(as.character(default)[1], n)
    }
    out <- trimws(out)
    out[out %in% c("", "NA", "NaN", "null", "undefined")] <- NA_character_
    out
  }

  num_col <- function(nm, default = NA_real_) {
    if (nm %in% names(sx)) {
      suppressWarnings(as.numeric(gsub(",", "", as.character(sx[[nm]]))))
    } else if (length(default) == n) {
      suppressWarnings(as.numeric(default))
    } else {
      rep(as.numeric(default)[1], n)
    }
  }

  spring_id <- chr_col("spring_id")
  if (any(is.na(spring_id))) {
    stop(
      "Springs layer contains records without spring_id; refusing to invent ",
      "browser identifiers or silently drop analytical records.",
      call. = FALSE
    )
  }
  if (anyDuplicated(spring_id)) {
    duplicate_ids <- unique(spring_id[duplicated(spring_id)])
    stop(
      "Springs layer contains duplicate spring_id value(s): ",
      paste(utils::head(duplicate_ids, 8L), collapse = ", "),
      call. = FALSE
    )
  }

  spring_name <- chr_col("spring_name_display", "Unnamed spring")
  spring_name[is.na(spring_name)] <- "Unnamed spring"

  source_raw <- chr_col("spring_source", "Unknown source")
  source_raw[is.na(source_raw)] <- "Unknown source"
  source_key_raw <- chr_col("spring_source_key")
  source_display_raw <- chr_col("spring_source_display")
  symbol_raw <- chr_col("spring_symbol_group", source_raw)
  symbol_raw[is.na(symbol_raw)] <- source_raw[is.na(symbol_raw)]

  source_combo <- tolower(paste(source_key_raw, source_raw, symbol_raw))
  is_nhd <- source_key_raw == "nhd" |
    toupper(source_raw) == "NHD" |
    toupper(symbol_raw) == "NHD"
  is_survey <- grepl("zdon|2015.?16|2015|2016|mojave", source_combo) & !is_nhd

  source_key <- dplyr::case_when(
    is_survey ~ "survey_2015_16",
    is_nhd    ~ "nhd",
    !is.na(source_key_raw) ~ source_key_raw,
    TRUE      ~ "other"
  )

  source_display <- dplyr::case_when(
    is_survey ~ "2015–16 Mojave survey",
    is_nhd    ~ "NHD",
    !is.na(source_display_raw) ~ source_display_raw,
    TRUE      ~ source_raw
  )

  survey_report_url <- dplyr::if_else(
    is_survey,
    "https://www.scienceforconservation.org/products/mojave-desert-spring-survey",
    NA_character_
  )
  source_report_url <- chr_col("source_report_url")
  legacy_report_url <- chr_col("survey_report_url")
  missing_report <- is.na(source_report_url)
  source_report_url[missing_report] <- legacy_report_url[missing_report]
  missing_report <- is.na(source_report_url)
  source_report_url[missing_report] <- survey_report_url[missing_report]

  source_report_label <- chr_col("source_report_label")
  missing_report_label <- is.na(source_report_label)
  source_report_label[missing_report_label] <- ifelse(
    is_survey[missing_report_label],
    "Survey report",
    "Source report"
  )

  on_blm_ca <- chr_col("on_blm_ca")
  dist_to_blm_mi <- num_col("dist_to_blm_mi")
  dist_to_blm_ft <- num_col("dist_to_blm_ft")

  rec <- data.frame(
    spring_id = spring_id,
    pt_lat = sx$pt_lat,
    pt_lng = sx$pt_lng,
    spring_name_display = spring_name,
    spring_label_text = chr_col("spring_label_text"),
    spring_source_key = source_key,
    spring_source_display = source_display,
    spring_source_short = chr_col("spring_source_short", source_display),
    gnis_id = chr_col("gnis_id"),
    elevation_ft = num_col("elevation_ft"),
    elevation_display = chr_col("elevation_display"),
    google_search_url = chr_col("google_search_url"),
    source_report_url = source_report_url,
    source_report_label = source_report_label,
    on_blm_ca = on_blm_ca,
    dist_to_blm_mi = dist_to_blm_mi,
    dist_to_blm_ft = dist_to_blm_ft,
    stringsAsFactors = FALSE
  )

  valid_coordinates <- is.finite(rec$pt_lat) &
    is.finite(rec$pt_lng) &
    abs(rec$pt_lat) <= 90 &
    abs(rec$pt_lng) <= 180
  if (!all(valid_coordinates)) {
    stop(
      "Springs layer contains ",
      sum(!valid_coordinates),
      " record(s) without valid coordinates; refusing to omit analytical records.",
      call. = FALSE
    )
  }

  ## Carry known scientific/detail fields if a future retained Springs product
  ## supplies them. The current cache has no flow/condition/classification
  ## fields, so this does not invent or reinterpret scientific attributes.
  optional_semantic_fields <- intersect(
    c(
      "spring_type",
      "spring_classification",
      "spring_condition",
      "spring_status",
      "flow_condition",
      "flow_rate",
      "flow_unit",
      "provider"
    ),
    names(sx)
  )
  for (field_name in optional_semantic_fields) {
    rec[[field_name]] <- sx[[field_name]]
  }

  message(
    "Springs Local display: viewport-virtualized Canvas/aggregate controller for ",
    nrow(rec),
    " analytical record(s)."
  )

  ## Register the overlay-group checkbox with one invisible dummy marker.  The
  ## real Springs points are created by pt_add_springs_browser_layer().
  dummy <- data.frame(lng = -170, lat = 10)
  m <- m |>
    leaflet::addCircleMarkers(
      data = dummy,
      lng = ~lng,
      lat = ~lat,
      group = pt_layer_group_name("Springs"),
      layerId = "pt_springs_dummy",
      radius = 0.001,
      stroke = FALSE,
      opacity = 0,
      fillOpacity = 0,
      options = leaflet::pathOptions(pane = "pane_points", interactive = FALSE)
    )

  ## Register a companion Labels overlay row for the existing BRIM inline
  ## `lbl` checkbox machinery.  Visible label text is drawn in the browser-side
  ## Springs controller so labels follow the active source/BLM filters and the
  ## zoom threshold, matching the USGS Streamgages/SWRCB pattern rather than
  ## adding a one-off panel checkbox.
  if (isTRUE(map_display$add_labels)) {
    m <- m |>
      leaflet::addCircleMarkers(
        data = dummy,
        lng = ~lng,
        lat = ~lat,
        group = pt_layer_group_name("Labels: Springs"),
        layerId = "pt_springs_label_dummy",
        radius = 0.001,
        stroke = FALSE,
        opacity = 0,
        fillOpacity = 0,
        options = leaflet::pathOptions(pane = "pane_labels_pts", interactive = FALSE)
      ) |>
      leaflet::hideGroup(pt_layer_group_name("Labels: Springs"))
  }

  pt_add_springs_browser_layer(
    m = m,
    spring_records = rec,
    group_name = pt_layer_group_name("Springs"),
    label_group_name = pt_layer_group_name("Labels: Springs")
  )
}
