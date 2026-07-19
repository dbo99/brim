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
## Browser-managed dense point layer, matching the stable Local USGS/CNRFC/SWRCB
## pattern.  The Leaflet layer-control row is registered with a hidden dummy
## marker; visible spring markers are rebuilt in the browser from a compact
## record table.  This avoids stale native marker/cluster interaction issues on
## the ~27k-record springs layer.


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

pt_add_springs_browser_layer <- function(m,
                                         spring_records = NULL,
                                         group_name = pt_layer_group_name("Springs"),
                                         label_group_name = pt_layer_group_name("Labels: Springs")) {

  if (!is.data.frame(spring_records) || nrow(spring_records) == 0) return(m)

  rec <- spring_records[
    !is.na(spring_records$pt_lat) &
      !is.na(spring_records$pt_lng) &
      abs(suppressWarnings(as.numeric(spring_records$pt_lat))) <= 90 &
      abs(suppressWarnings(as.numeric(spring_records$pt_lng))) <= 180,
    ,
    drop = FALSE
  ]

  if (nrow(rec) == 0) return(m)

  js <- r"---(
function(el, x, data) {
  var map = this;
  var groupName = data && data.groupName ? String(data.groupName) : 'Points – Springs';
  var labelGroupName = data && data.labelGroupName ? String(data.labelGroupName) : 'Labels – Springs';

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

  function num(v) {
    if (v === null || v === undefined || v === '') return null;
    var n = Number(v);
    return isNaN(n) ? null : n;
  }

  function fmt(n) {
    if (n === null || n === undefined || isNaN(Number(n))) return '—';
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
      .replace(/monitoring sites\s*\/\s*records\s*-\s*/g, '')
      .replace(/\s*\([^)]*\)\s*$/g, '')
      .replace(/\s+/g, ' ')
      .trim();
  }

  function isSpringsText(s) {
    return norm(s) === 'springs' || norm(s).indexOf('springs') >= 0;
  }

  function isSpringsLabelText(s) {
    var raw = String(s == null ? '' : s);
    return /^\s*Labels\s*[–-]\s*/i.test(raw) && isSpringsText(raw);
  }

  function safeLabelControlScan() {
    if (typeof document === 'undefined' || !document.querySelectorAll) return null;
    var labels = document.querySelectorAll('.leaflet-control-layers-overlays label');
    for (var i = 0; i < labels.length; i++) {
      var label = labels[i];
      var full = label.getAttribute ? (label.getAttribute('data-pt-layer-full-name') || '') : '';
      var text = label.textContent || label.innerText || '';
      if (!isSpringsLabelText(full || text)) continue;
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
      var full = label.getAttribute ? (label.getAttribute('data-pt-layer-full-name') || '') : '';
      var text = label.textContent || label.innerText || '';
      if (isSpringsLabelText(full || text)) continue;
      if (!isSpringsText(text)) continue;
      var input = label.querySelector ? label.querySelector('input[type="checkbox"]') : null;
      if (input) return !!input.checked;
    }
    return null;
  }

  function eventMatches(evt) {
    if (!evt || labelEventMatches(evt)) return false;
    if (isSpringsText(evt.name)) return true;
    if (evt.layer && evt.layer.options) {
      var bits = [evt.layer.options.group, evt.layer.options.name, evt.layer.options.layerId].join(' ');
      return isSpringsText(bits);
    }
    return false;
  }

  function labelEventMatches(evt) {
    if (!evt) return false;
    if (isSpringsLabelText(evt.name)) return true;
    if (evt.layer && evt.layer.options) {
      var bits = [evt.layer.options.group, evt.layer.options.name, evt.layer.options.layerId].join(' ');
      return isSpringsLabelText(bits);
    }
    return false;
  }

  function makeTooltip(r) {
    var nm = has(r.spring_name_display) ? r.spring_name_display : 'Unnamed spring';
    var src = has(r.spring_source_display) ? r.spring_source_display : 'Unknown source';
    return esc(nm) + '<br/>Source: ' + esc(src);
  }

  function makePopup(r) {
    var nm = has(r.spring_name_display) ? r.spring_name_display : 'Unnamed spring';
    var src = has(r.spring_source_display) ? r.spring_source_display : 'Unknown source';
    var parts = [];
    parts.push('<div style="font:12px/1.35 Arial, Helvetica, sans-serif;max-width:380px;">');
    parts.push('<div style="font-weight:700;font-size:13px;margin-bottom:3px;">Spring: ' + esc(nm) + '</div>');
    parts.push('<div><b>Source:</b> ' + esc(src) + '</div>');
    var blmDist = num(r.dist_to_blm_mi);
    var onBlm = boolVal(r.on_blm_ca);
    if (onBlm === true) {
      parts.push('<div><b>BLM distance:</b> on BLM</div>');
    } else if (blmDist !== null) {
      parts.push('<div><b>BLM distance:</b> ' + esc(blmDist.toFixed(blmDist < 10 ? 2 : 1).replace(/\.00$/, '')) + ' mi</div>');
    }
    if (has(r.elevation_display)) parts.push('<div><b>Elevation:</b> ' + esc(r.elevation_display) + '</div>');
    if (has(r.gnis_id)) parts.push('<div><b>GNIS ID:</b> ' + esc(r.gnis_id) + '</div>');
    var links = [];
    if (has(r.google_search_url)) links.push('<a href="' + esc(r.google_search_url) + '" target="_blank">Google search</a>');
    if (has(r.survey_report_url)) links.push('<a href="' + esc(r.survey_report_url) + '" target="_blank">Survey report</a>');
    if (links.length) parts.push('<div style="margin-top:5px;">' + links.join(' · ') + '</div>');
    parts.push('</div>');
    return parts.join('');
  }

  function cssColor(v, fallback) {
    if (!has(v)) return fallback;
    var s = String(v).trim();
    if (/^#[0-9a-fA-F]{3,8}$/.test(s) || /^rgba?\([0-9.,\s]+\)$/.test(s) || s === 'transparent') return s;
    return fallback;
  }

  function markerOpacity(r) {
    var key = has(r.spring_source_key) ? String(r.spring_source_key) : '';
    if (key === 'survey_2015_16') return 0.96;
    return 0.88;
  }

  function makeSpringIcon(r) {
    var radius = num(r.spring_radius) || 3.5;
    var size = Math.max(7, Math.min(17, Math.round(radius * 2.35)));
    var weight = num(r.spring_stroke_weight);
    if (weight === null) weight = 1.0;
    weight = Math.max(0.7, Math.min(2.4, weight));
    var fillOpacity = num(r.spring_fill_opacity);
    if (fillOpacity === null) fillOpacity = 0.70;
    var fill = fillOpacity <= 0.05 ? 'transparent' : cssColor(r.spring_fill_col, '#777777');
    var stroke = cssColor(r.spring_stroke_col, '#333333');
    var z = has(r.spring_source_key) && String(r.spring_source_key) === 'survey_2015_16' ? ' pt-springs-local-survey-marker' : '';
    return L.divIcon({
      className: 'pt-springs-local-divicon',
      html: '<span class="pt-springs-local-marker' + z + '" style="width:' + size + 'px;height:' + size + 'px;background:' + fill + ';border-color:' + stroke + ';border-width:' + weight + 'px;opacity:' + markerOpacity(r) + ';"></span>',
      iconSize: L.point(size, size),
      iconAnchor: L.point(size / 2, size / 2),
      popupAnchor: L.point(0, -Math.max(6, size / 2))
    });
  }

  function defaultFilters() {
    return {source: 'all', blmMode: 'any', blmMax: null};
  }

  var filters = defaultFilters();
  var legendUserHidden = false;

  function resetFilters() {
    filters = defaultFilters();
  }

  function isNhdRecord(r) {
    return has(r.spring_source_key) && String(r.spring_source_key) === 'nhd';
  }

  function isSurveyRecord(r) {
    return has(r.spring_source_key) && String(r.spring_source_key) === 'survey_2015_16';
  }

  function recordPassesFilters(r) {
    if (!r) return false;
    if (filters.source === 'nhd' && !isNhdRecord(r)) return false;
    if (filters.source === 'survey' && !isSurveyRecord(r)) return false;

    var onBlm = boolVal(r.on_blm_ca);
    var distMi = num(r.dist_to_blm_mi);

    if (filters.blmMode === 'on') return onBlm === true;
    if (filters.blmMode === 'off') return onBlm === false;
    if (filters.blmMode === 'distance') {
      return distMi !== null && filters.blmMax !== null && distMi <= Number(filters.blmMax);
    }
    return true;
  }

  function filteredRecords() {
    return records.filter(recordPassesFilters);
  }

  function sourceCount(key) {
    return records.filter(function(r) {
      if (key === 'nhd') return isNhdRecord(r);
      if (key === 'survey') return isSurveyRecord(r);
      return !isNhdRecord(r) && !isSurveyRecord(r);
    }).length;
  }

  function springLabelText(r) {
    var nm = has(r.spring_name_display) ? String(r.spring_name_display).trim() : '';
    if (!has(nm)) return '';
    // Avoid thousands of visually unhelpful duplicate labels when the source
    // only says the feature is unnamed. The hover/popup still displays those
    // records normally.
    if (/^unnamed\s+(?:nhd\s+)?spring$/i.test(nm)) return '';
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
    var isSurvey = has(r.spring_source_key) && String(r.spring_source_key) === 'survey_2015_16';
    var marker = L.marker([lat, lng], {
      icon: makeSpringIcon(r),
      pane: 'pane_points',
      keyboard: false,
      riseOnHover: true,
      zIndexOffset: isSurvey ? 90 : 0
    });
    marker.bindTooltip(makeTooltip(r), {
      direction: 'auto',
      opacity: 0.9,
      sticky: true,
      className: 'pt-springs-local-tooltip'
    });
    marker.bindPopup(function() { return makePopup(r); }, {maxWidth: 420, maxHeight: 520});
    return marker;
  }

  if (typeof document !== 'undefined' && document.head && !document.getElementById('pt-springs-local-browser-style')) {
    var style = document.createElement('style');
    style.id = 'pt-springs-local-browser-style';
    style.textContent =
      '.leaflet-tooltip.pt-springs-local-tooltip{font:12px/1.25 Arial,sans-serif;white-space:normal;min-width:150px;max-width:340px;}' +
      '.pt-springs-local-divicon{background:transparent;border:0;}' +
      '.pt-springs-local-marker{display:block;border-style:solid;border-radius:50%;box-sizing:border-box;box-shadow:0 0 0 0.2px rgba(0,0,0,0.15);}' +
      '.pt-springs-local-survey-marker{box-shadow:0 0 0 1px rgba(255,255,255,0.85),0 1px 3px rgba(0,0,0,0.25);}' +
      '.pt-springs-local-label-divicon{background:transparent;border:0;white-space:nowrap;pointer-events:none;}' +
      '.pt-springs-local-label-divicon span{display:inline-block;transform:translate(-50%,-13px);font:700 11px/1.1 Arial,sans-serif;color:#5a3a0f;background:rgba(255,255,255,0.64);border:1px solid rgba(90,58,15,0.22);border-radius:3px;padding:1px 3px;text-shadow:0 1px 2px #fff,1px 0 2px #fff,-1px 0 2px #fff,0 -1px 2px #fff;box-shadow:0 1px 2px rgba(0,0,0,0.10);max-width:190px;overflow:hidden;text-overflow:ellipsis;}' +
      '.pt-springs-local-legend{background:rgba(246,239,222,0.96);border:1px solid rgba(112,103,83,0.55);border-radius:6px;box-shadow:0 1px 5px rgba(0,0,0,0.25);padding:7px 9px 8px 9px;width:255px;max-width:255px;font:11.5px/1.22 Arial,sans-serif;color:#222;position:relative;margin-bottom:74px;}' +
      '.pt-springs-local-title{font-weight:700;font-size:12px;margin:0 18px 4px 0;}' +
      '.pt-springs-local-close{position:absolute;top:3px;right:5px;border:0;background:transparent;color:#555;font-size:16px;line-height:16px;cursor:pointer;padding:0 2px;}' +
      '.pt-springs-local-row{display:flex;align-items:center;gap:5px;margin:2px 0;}' +
      '.pt-springs-local-label{flex:1;min-width:0;}' +
      '.pt-springs-local-count{font-variant-numeric:tabular-nums;color:#555;}' +
      '.pt-springs-local-dot{display:inline-block;width:9px;height:9px;border-radius:50%;border:1px solid #fff;background:#2B6CB0;box-sizing:border-box;}' +
      '.pt-springs-local-ring{display:inline-block;width:12px;height:12px;border-radius:50%;border:2px solid #E6550D;background:transparent;box-sizing:border-box;}' +
      '.pt-springs-filter-box{border-top:1px solid rgba(112,103,83,0.25);margin-top:5px;padding-top:5px;}' +
      '.pt-springs-filter-line{margin:3px 0;}' +
      '.pt-springs-filter-btn{border:1px solid rgba(112,103,83,0.65);background:rgba(255,255,255,0.92);border-radius:5px;padding:2px 5px;margin:1px 2px 1px 0;font:11px/1.1 Arial,sans-serif;cursor:pointer;}' +
      '.pt-springs-filter-btn.active{background:rgba(221,211,173,0.98);color:#222;border-color:rgba(112,103,83,0.75);font-weight:700;}' +
      '.pt-springs-local-showing{margin-top:5px;color:#444;font-size:10.5px;}' +
      '.pt-springs-local-note{margin-top:4px;color:#666;font-size:10px;}';
    document.head.appendChild(style);
  }

  var markers = (typeof L.markerClusterGroup === 'function') ? L.markerClusterGroup({
    // Mirror the stable Local USGS groundwater scaffold: batch-built divIcon
    // markers, low-zoom clustering, no SVG circleMarker swarm, no chunked
    // addLayer loop competing with basemap tile repaint.
    disableClusteringAtZoom: 11,
    spiderfyOnMaxZoom: true,
    showCoverageOnHover: false,
    zoomToBoundsOnClick: true,
    animate: false,
    animateAddingMarkers: false,
    chunkedLoading: false,
    removeOutsideVisibleBounds: true,
    maxClusterRadius: function(z) {
      if (z <= 6) return 175;
      if (z <= 8) return 145;
      if (z <= 9) return 115;
      if (z <= 10) return 75;
      return 35;
    }
  }) : L.layerGroup();

  var labelLayer = L.layerGroup();
  var labelsActive = false;
  var labelMinZoom = 12;
  var lastFilteredRecords = [];

  var layerActive = false;
  var builtOnce = false;
  var lastDrawn = 0;
  var homeCenter = (map && map.getCenter) ? map.getCenter() : null;
  var homeZoom = (map && map.getZoom) ? map.getZoom() : null;
  var restoreTimer = null;

  function clearLabels() {
    try { labelLayer.clearLayers(); } catch(e) {}
    try { if (map.hasLayer(labelLayer)) map.removeLayer(labelLayer); } catch(e) {}
  }

  function hardClearMarkers() {
    if (restoreTimer) { clearTimeout(restoreTimer); restoreTimer = null; }
    if (map.hasLayer(markers)) map.removeLayer(markers);
    if (markers.clearLayers) markers.clearLayers();
    ['_featureGroup', '_nonPointGroup'].forEach(function(k) {
      try { if (markers[k] && markers[k].clearLayers) markers[k].clearLayers(); } catch(e) {}
    });
    clearLabels();
  }

  function syncLabels() {
    var checked = safeLabelControlScan();
    labelsActive = checked === null ? labelsActive : checked;
    labelLayer.clearLayers();
    if (!labelsActive || !layerActive || (map.getZoom && map.getZoom() < labelMinZoom)) {
      if (map.hasLayer(labelLayer)) map.removeLayer(labelLayer);
      return;
    }

    // Springs has ~27k records.  Never build labels for every filtered record
    // synchronously; doing so can freeze Chrome before the inline lbl checkbox
    // visually changes.  Labels are a close-zoom aid, so only label records in
    // the current map view, with a generous pad and a hard safety cap.
    var b = null;
    try { b = map.getBounds ? map.getBounds().pad(0.12) : null; } catch(e) { b = null; }
    var labelRecords = [];
    var maxLabels = 700;

    for (var i = 0; i < lastFilteredRecords.length; i++) {
      var r = lastFilteredRecords[i];
      var lat = Number(r.pt_lat), lng = Number(r.pt_lng);
      if (isNaN(lat) || isNaN(lng)) continue;
      if (b && !b.contains([lat, lng])) continue;
      labelRecords.push(r);
      if (labelRecords.length >= maxLabels) break;
    }

    labelRecords.forEach(function(r) {
      var lm = makeSpringLabelMarker(r);
      if (lm) labelLayer.addLayer(lm);
    });
    if (labelRecords.length > 0 && !map.hasLayer(labelLayer)) labelLayer.addTo(map);
    if (labelRecords.length === 0 && map.hasLayer(labelLayer)) map.removeLayer(labelLayer);
  }

  function rebuild() {
    hardClearMarkers();
    var shown = filteredRecords();
    lastFilteredRecords = shown;
    var layersToAdd = [];
    shown.forEach(function(r) {
      var marker = makeMarker(r);
      if (marker) layersToAdd.push(marker);
    });
    lastDrawn = layersToAdd.length;
    builtOnce = true;
    if (typeof markers.addLayers === 'function') markers.addLayers(layersToAdd);
    else layersToAdd.forEach(function(marker) { markers.addLayer(marker); });
    if (layerActive && !map.hasLayer(markers)) markers.addTo(map);
    syncLabels();
    return {filtered: shown.length, total: records.length, drawn: lastDrawn};
  }

  function buttonHtml(kind, value, label) {
    var active = false;
    if (kind === 'source') active = String(filters.source) === String(value);
    if (kind === 'blm') {
      if (value === 'any') active = filters.blmMode === 'any';
      else if (value === 'on') active = filters.blmMode === 'on';
      else if (value === 'off') active = filters.blmMode === 'off';
      else active = filters.blmMode === 'distance' && String(filters.blmMax) === String(value);
    }
    return '<button type="button" class="pt-springs-filter-btn' + (active ? ' active' : '') + '" data-kind="' + esc(kind) + '" data-value="' + esc(value) + '">' + esc(label) + '</button>';
  }

  function symRow(sym, label, count) {
    return '<div class="pt-springs-local-row">' + sym + '<span class="pt-springs-local-label">' + esc(label) + '</span><span class="pt-springs-local-count">' + fmt(count) + '</span></div>';
  }

  function buildLegendHtml() {
    var shown = filteredRecords().length;
    var html = '';
    html += '<button type="button" class="pt-springs-local-close" title="Hide legend">&times;</button>';
    html += '<div class="pt-springs-local-title">Springs</div>';
    html += symRow('<span class="pt-springs-local-dot"></span>', 'NHD spring point', sourceCount('nhd'));
    html += symRow('<span class="pt-springs-local-ring"></span>', '2015–16 Mojave survey', sourceCount('survey'));
    var otherN = sourceCount('other');
    if (otherN > 0) html += symRow('<span class="pt-springs-local-dot" style="background:#777;border-color:#333;"></span>', 'Other source', otherN);
    html += '<div class="pt-springs-local-showing">Showing ' + fmt(shown) + ' / ' + fmt(records.length) + ' spring record(s).</div>';
    html += '<div class="pt-springs-filter-box">';
    html += '<div class="pt-springs-filter-line">Source: ' + buttonHtml('source','all','all') + buttonHtml('source','nhd','NHD') + buttonHtml('source','survey','2015–16') + '</div>';
    html += '<div class="pt-springs-filter-line">BLM max mi: ' + buttonHtml('blm','any','any') + buttonHtml('blm','on','on') + buttonHtml('blm','off','off') + buttonHtml('blm','1','≤1') + buttonHtml('blm','5','≤5') + '</div>';
    html += '<div class="pt-springs-local-note">BLM distance is screening-only; verify points near boundaries. Spring name labels (lbl) display at zoom 12+.</div>';
    html += '</div>';
    return html;
  }

  var legend = L.control({position: 'bottomleft'});
  legend.onAdd = function() {
    var div = L.DomUtil.create('div', 'leaflet-control pt-springs-local-legend');
    div.style.display = 'none';
    // Use an inline bottom margin, not only CSS. Leaflet's default selector
    // `.leaflet-bottom .leaflet-control { margin-bottom: ... }` is more
    // specific than a single legend class, so a CSS-only margin can be
    // ignored in some builds/browsers. The other Local legends use this
    // inline offset pattern, which keeps their lower edge aligned above the
    // Local GIS Uploads panel.
    div.style.marginBottom = '74px';
    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);
    div.innerHTML = buildLegendHtml();
    attachLegendEvents(div);
    return div;
  };
  legend.addTo(map);

  function legendDiv() {
    var c = map && map.getContainer ? map.getContainer() : el;
    var div = c && c.querySelector ? c.querySelector('.pt-springs-local-legend') : null;
    if (!div && el && el.querySelector) div = el.querySelector('.pt-springs-local-legend');
    return div;
  }

  function attachLegendEvents(div) {
    if (!div || !div.querySelectorAll) return;
    if (window.BRIM && window.BRIM.legendCloseout) {
      window.BRIM.legendCloseout.wire(div, '.pt-springs-local-close', function(){
        legendUserHidden = true;
      });
    } else {
      var close = div.querySelector('.pt-springs-local-close');
      if (close) close.onclick = function(e) {
        if (e && e.preventDefault) e.preventDefault();
        if (e && e.stopPropagation) e.stopPropagation();
        legendUserHidden = true;
        div.style.display = 'none';
      };
    }
    var btns = div.querySelectorAll('.pt-springs-filter-btn');
    for (var i = 0; i < btns.length; i++) {
      btns[i].onclick = function(e) {
        if (e && e.preventDefault) e.preventDefault();
        if (e && e.stopPropagation) e.stopPropagation();
        var kind = this.getAttribute('data-kind');
        var value = this.getAttribute('data-value');
        if (kind === 'source') {
          filters.source = value || 'all';
        } else if (kind === 'blm') {
          if (value === 'any') { filters.blmMode = 'any'; filters.blmMax = null; }
          else if (value === 'on') { filters.blmMode = 'on'; filters.blmMax = null; }
          else if (value === 'off') { filters.blmMode = 'off'; filters.blmMax = null; }
          else { filters.blmMode = 'distance'; filters.blmMax = Number(value); }
        }
        rebuild();
        updateLegend();
      };
    }
  }

  function updateLegend() {
    var div = legendDiv();
    if (!div) return;
    if (!layerActive) {
      div.style.display = 'none';
      legendUserHidden = false;
      return;
    }
    div.innerHTML = buildLegendHtml();
    div.style.display = legendUserHidden ? 'none' : 'block';
    attachLegendEvents(div);
  }

  function syncActive() {
    var checked = safeControlScan();
    layerActive = checked === null ? layerActive : checked;
    if (layerActive) {
      if (!builtOnce) rebuild();
      else if (!map.hasLayer(markers)) markers.addTo(map);
    } else {
      hardClearMarkers();
      builtOnce = false;
      lastDrawn = 0;
      lastFilteredRecords = [];
    }
    syncLabels();
    updateLegend();
  }

  function restoreAfterDelay(delayMs) {
    if (restoreTimer) clearTimeout(restoreTimer);
    restoreTimer = setTimeout(function() {
      restoreTimer = null;
      try {
        if (layerActive && builtOnce && !map.hasLayer(markers)) {
          markers.addTo(map);
          if (markers.refreshClusters) markers.refreshClusters();
        }
      } catch(e) {}
    }, delayMs == null ? 900 : delayMs);
  }

  function installSafeHomeJumpGuard() {
    try {
      if (typeof document === 'undefined' || !document.getElementById) return;
      var resetBtn = document.getElementById('pt-reset-zoom-btn');
      if (!resetBtn || resetBtn.__ptSpringsSafeHomeGuard) return;
      resetBtn.__ptSpringsSafeHomeGuard = true;
      resetBtn.addEventListener('click', function(e) {
        try {
          var fromZoom = (map && map.getZoom) ? map.getZoom() : null;
          var toZoom = homeZoom;
          var heavyHomeJump = layerActive && builtOnce && fromZoom !== null && toZoom !== null && fromZoom >= 11 && toZoom <= 8 && homeCenter && map && map.setView;
          if (!heavyHomeJump) return;
          if (e && e.preventDefault) e.preventDefault();
          if (e && e.stopPropagation) e.stopPropagation();
          if (e && e.stopImmediatePropagation) e.stopImmediatePropagation();
          try { if (map.hasLayer(markers)) map.removeLayer(markers); } catch(removeErr) {}
          setTimeout(function() {
            try { map.setView(homeCenter, homeZoom, {animate:false}); } catch(viewErr) {}
            restoreAfterDelay(700);
          }, 0);
        } catch(clickErr) {}
      }, true);
    } catch(installErr) {}
  }

  function suspendForMarqueeZoom(evt) {
    try {
      if (!layerActive || !builtOnce || !map.hasLayer(markers)) return;
      var fromZoom = (map && map.getZoom) ? map.getZoom() : null;
      var targetZoom = evt && evt.targetZoom != null ? Number(evt.targetZoom) : null;
      var bigMarqueeZoom = false;
      if (fromZoom !== null && targetZoom !== null && !isNaN(targetZoom)) {
        bigMarqueeZoom = (fromZoom <= 9 && targetZoom >= 10) || ((targetZoom - fromZoom) >= 2);
      } else if (fromZoom !== null) {
        bigMarqueeZoom = fromZoom <= 9;
      }
      if (!bigMarqueeZoom) return;
      try { map.removeLayer(markers); } catch(removeErr) {}
      try { map.once('moveend', function() { restoreAfterDelay(1400); }); } catch(moveErr) {}
    } catch(suspendErr) {}
  }

  function installMarqueeZoomGuard() {
    try {
      if (map.__ptSpringsMarqueeGuard) return;
      map.__ptSpringsMarqueeGuard = true;
      map.on('pt:marqueezoomstart', suspendForMarqueeZoom);
      map.on('pt:marqueezoomend', function() { restoreAfterDelay(1200); });
    } catch(installErr) {}
  }

  window.BRIM_SPRINGS_LOCAL = {
    rebuild: function() { var out = rebuild(); updateLegend(); return out; },
    setActive: function(active) { layerActive = !!active; syncActive(); },
    stats: function() { return {filtered: filteredRecords().length, total: records.length, drawn: lastDrawn, active: layerActive, labels: labelsActive, filters: filters}; },
    resetFilters: function() { resetFilters(); rebuild(); updateLegend(); },
    clear: function() { hardClearMarkers(); builtOnce = false; lastDrawn = 0; resetFilters(); updateLegend(); }
  };

  map.on('overlayadd', function(evt) {
    if (eventMatches(evt)) {
      resetFilters();
      legendUserHidden = false;
      layerActive = true;
      syncActive();
    }
    if (labelEventMatches(evt)) {
      labelsActive = true;
      syncLabels();
    }
  });
  map.on('overlayremove', function(evt) {
    if (eventMatches(evt)) {
      layerActive = false;
      hardClearMarkers();
      builtOnce = false;
      lastDrawn = 0;
      lastFilteredRecords = [];
      resetFilters();
      legendUserHidden = false;
      updateLegend();
    }
    if (labelEventMatches(evt)) {
      labelsActive = false;
      syncLabels();
    }
  });

  map.on('zoomend moveend', function() { syncLabels(); updateLegend(); });

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
  setTimeout(updateLegend, 800);
  setTimeout(installSafeHomeJumpGuard, 0);
  setTimeout(installSafeHomeJumpGuard, 800);
  setTimeout(installSafeHomeJumpGuard, 1800);
  setTimeout(installMarqueeZoomGuard, 0);
  setTimeout(installMarqueeZoomGuard, 800);
}
)---"

  htmlwidgets::onRender(
    m,
    js,
    data = list(
      groupName = group_name,
      labelGroupName = label_group_name,
      records = rec
    )
  )
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
    warning("Springs browser layer skipped: coordinates unavailable.")
    return(m)
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
  spring_id[is.na(spring_id)] <- paste0("spring_", seq_len(n))[is.na(spring_id)]

  spring_name <- chr_col("spring_name_display", "Unnamed spring")
  spring_name[is.na(spring_name)] <- "Unnamed spring"

  source_raw <- chr_col("spring_source", "Unknown source")
  source_raw[is.na(source_raw)] <- "Unknown source"
  symbol_raw <- chr_col("spring_symbol_group", source_raw)
  symbol_raw[is.na(symbol_raw)] <- source_raw[is.na(symbol_raw)]

  source_combo <- tolower(paste(source_raw, symbol_raw))
  is_nhd <- toupper(source_raw) == "NHD" | toupper(symbol_raw) == "NHD"
  is_survey <- grepl("zdon|2015.?16|2015|2016|mojave", source_combo) & !is_nhd

  source_key <- dplyr::case_when(
    is_survey ~ "survey_2015_16",
    is_nhd    ~ "nhd",
    TRUE      ~ "other"
  )

  source_display <- dplyr::case_when(
    is_survey ~ "2015–16 Mojave survey",
    is_nhd    ~ "NHD",
    TRUE      ~ source_raw
  )

  survey_report_url <- dplyr::if_else(
    is_survey,
    "https://www.scienceforconservation.org/products/mojave-desert-spring-survey",
    NA_character_
  )

  ## Keep the same visual intent as the older native layer: NHD as small blue
  ## dots, 2015–16 Mojave survey records as red/orange open rings on top.
  spring_radius <- dplyr::case_when(
    source_key == "survey_2015_16" ~ 6.0,
    source_key == "nhd"            ~ 3.2,
    TRUE                           ~ 3.5
  )
  spring_fill_col <- dplyr::case_when(
    source_key == "survey_2015_16" ~ "#FFFFFF",
    source_key == "nhd"            ~ "#2B6CB0",
    TRUE                           ~ "#777777"
  )
  spring_fill_opacity <- dplyr::case_when(
    source_key == "survey_2015_16" ~ 0.0,
    source_key == "nhd"            ~ 0.80,
    TRUE                           ~ 0.70
  )
  spring_stroke_col <- dplyr::case_when(
    source_key == "survey_2015_16" ~ "#E6550D",
    source_key == "nhd"            ~ "#FFFFFF",
    TRUE                           ~ "#333333"
  )
  spring_stroke_weight <- dplyr::case_when(
    source_key == "survey_2015_16" ~ 2.0,
    source_key == "nhd"            ~ 0.7,
    TRUE                           ~ 1.0
  )

  on_blm_ca <- chr_col("on_blm_ca")
  dist_to_blm_mi <- num_col("dist_to_blm_mi")
  dist_to_blm_ft <- num_col("dist_to_blm_ft")

  rec <- data.frame(
    spring_id = spring_id,
    pt_lat = sx$pt_lat,
    pt_lng = sx$pt_lng,
    spring_name_display = spring_name,
    spring_source_key = source_key,
    spring_source_display = source_display,
    gnis_id = chr_col("gnis_id"),
    elevation_display = chr_col("elevation_display"),
    google_search_url = chr_col("google_search_url"),
    survey_report_url = survey_report_url,
    on_blm_ca = on_blm_ca,
    dist_to_blm_mi = dist_to_blm_mi,
    dist_to_blm_ft = dist_to_blm_ft,
    spring_radius = spring_radius,
    spring_fill_col = spring_fill_col,
    spring_fill_opacity = spring_fill_opacity,
    spring_stroke_col = spring_stroke_col,
    spring_stroke_weight = spring_stroke_weight,
    stringsAsFactors = FALSE
  )

  rec <- rec[
    !is.na(rec$pt_lat) & !is.na(rec$pt_lng) &
      abs(rec$pt_lat) <= 90 & abs(rec$pt_lng) <= 180,
    ,
    drop = FALSE
  ]

  if (nrow(rec) == 0) {
    message("Springs layer has no usable coordinates; no points added.")
    return(m)
  }

  rec$pt_draw_order <- match(
    rec$spring_source_key,
    c("nhd", "other", "survey_2015_16")
  )
  rec$pt_draw_order[is.na(rec$pt_draw_order)] <- 2L
  rec <- rec[order(rec$pt_draw_order, rec$spring_name_display), , drop = FALSE]
  rec$pt_draw_order <- NULL

  message(
    "Springs Local display: browser-managed MarkerClusterGroup for ",
    nrow(rec),
    " point record(s)."
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

