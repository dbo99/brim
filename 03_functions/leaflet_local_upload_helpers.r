# ==== leaflet_local_upload_helpers.r =========================================
##
## PURPOSE:
##   Add a separate lower-left PortaTreasure2 local GIS upload panel.
##
## DESIGN:
##   This helper is intentionally browser-side only.  It does not read, write,
##   preprocess, cache, or permanently store user-uploaded GIS data.
##
##   Uploaded layers are temporary browser-session overlays.  They are intended
##   for quick screening and visual comparison against the PT2 hydroportal
##   layers.
##
## INITIAL SUPPORT:
##   - GeoJSON / JSON files using native browser JSON parsing.
##   - Zipped shapefiles using the browser-side shpjs library loaded on demand.
##
## IMPORTANT LIMITATIONS:
##   - WGS84 / EPSG:4326-style lon/lat coordinates are expected.
##   - If a layer draws in the wrong location, reproject it to WGS84 before
##     upload.
##   - The shapefile ZIP reader requires browser access to shpjs from the CDN
##     URL below unless the script is later embedded locally.
##   - This is a viewing/screening tool, not a data-validation workflow.
## ============================================================================

pt_add_local_upload_panel <- function(m, map_display = NULL) {
  
  ## If a future config flag is added, allow the panel to be disabled without
  ## changing this helper.  If the flag is absent, default to showing the panel.
  if (!is.null(map_display) &&
      !is.null(map_display$add_local_upload_panel) &&
      !isTRUE(map_display$add_local_upload_panel)) {
    return(m)
  }

  attribute_helper_js_path <- file.path(
    "03_functions", "js", "brim_local_upload_attribute_helpers.js"
  )
  if (!file.exists(attribute_helper_js_path)) {
    stop("Missing local-upload attribute helper: ", attribute_helper_js_path)
  }
  m <- htmlwidgets::onRender(
    m,
    paste(readLines(attribute_helper_js_path, warn = FALSE), collapse = "\n")
  )
  
  js <- r"---(
function(el, x) {

  var map = this;
  var ptAttr = window.BRIM && window.BRIM.localUploadAttribute;

  if (!ptAttr) {
    console.error('BRIM local-upload attribute helper was not initialized.');
    return;
  }

  // ------------------------------------------------------------------------
  // Configuration
  // ------------------------------------------------------------------------

  var PT2_LOCAL_MAX_LAYERS = 3;
  var PT2_LOCAL_MAX_FILE_MB = 50;
  var PT2_LOCAL_WARN_FEATURES = 5000;
  var PT2_LOCAL_MAX_FEATURES = 25000;

  // Browser-side shapefile reader.  This is loaded only when a zipped
  // shapefile is selected.  GeoJSON upload does not need this dependency.
  var PT2_SHPJS_URL = 'https://unpkg.com/shpjs@6.1.0/dist/shp.min.js';

  var PT2_LOCAL_DEFAULT_COLORS = [
    '#7B3294',
    '#D95F02',
    '#008837'
  ];

  // ------------------------------------------------------------------------
  // Basic utilities
  // ------------------------------------------------------------------------

  function ptEscapeHtml(value) {
    if (value === null || value === undefined) return '';
    return String(value)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#039;');
  }

  function ptCleanText(value) {
    if (value === null || value === undefined) return '';
    return String(value).trim();
  }

  function ptHasValue(value) {
    return value !== null &&
      value !== undefined &&
      String(value) !== '' &&
      String(value).toLowerCase() !== 'null' &&
      String(value).toLowerCase() !== 'na' &&
      String(value).toLowerCase() !== 'nan';
  }

  function ptDisplayValue(value) {
    return ptAttr.displayValue(value);
  }

  function ptShortName(value, maxLen) {
    value = String(value || '');
    maxLen = maxLen || 36;
    if (value.length <= maxLen) return value;
    return value.substring(0, maxLen - 3) + '...';
  }

  function ptSetLocalStatus(msg, isError) {
    var div = document.getElementById('pt-local-upload-status');
    if (!div) return;

    div.textContent = msg || '';
    div.style.color = isError ? '#8B0000' : '#244C1E';
  }

  function ptFormatBytes(bytes) {
    var mb = bytes / (1024 * 1024);
    if (mb >= 1) return mb.toFixed(1) + ' MB';
    var kb = bytes / 1024;
    return kb.toFixed(0) + ' KB';
  }

  function ptBaseGeomType(type) {
    type = String(type || '');

    if (type.indexOf('Point') >= 0) return 'Point';
    if (type.indexOf('LineString') >= 0) return 'Line';
    if (type.indexOf('Polygon') >= 0) return 'Polygon';
    if (type === 'GeometryCollection') return 'Mixed';

    return type || 'Unknown';
  }

  function ptGeomTypeLabel(featureCollection) {
    var features = featureCollection && Array.isArray(featureCollection.features) ?
      featureCollection.features : [];
    var seen = Object.create(null);

    features.forEach(function(f) {
      var g = f && f.geometry ? f.geometry : null;
      if (!g) return;
      seen[ptBaseGeomType(g.type)] = true;
    });

    var keys = Object.keys(seen);
    if (keys.length === 0) return 'Unknown';
    if (keys.length === 1) return keys[0];
    return 'Mixed';
  }

  function ptCollectFields(featureCollection) {
    var features = featureCollection && Array.isArray(featureCollection.features) ?
      featureCollection.features : [];
    var seen = Object.create(null);
    var out = [];

    function usefulField(name) {
      var clean = ptCleanText(name);
      var lower = clean.toLowerCase();
      if (!clean || lower.indexOf('_pt2_') === 0) return false;
      return ['geometry', 'geom', 'the_geom', 'wkt'].indexOf(lower) < 0;
    }

    features.forEach(function(f) {
      var props = f && f.properties ? f.properties : {};
      Object.keys(props).forEach(function(k) {
        if (usefulField(k) && !seen[k]) {
          seen[k] = true;
          out.push(k);
        }
      });
    });

    return out.sort(function(a, b) {
      return String(a).localeCompare(String(b));
    });
  }

  function ptValueForField(props, fieldName) {
    props = props || {};
    fieldName = ptCleanText(fieldName);
    if (!fieldName) return '';

    if (Object.prototype.hasOwnProperty.call(props, fieldName)) {
      return props[fieldName];
    }

    var target = fieldName.toLowerCase();
    var keys = Object.keys(props);

    for (var i = 0; i < keys.length; i++) {
      if (String(keys[i]).toLowerCase() === target) {
        return props[keys[i]];
      }
    }

    return '';
  }

  // ------------------------------------------------------------------------
  // Coordinate sanity checks
  // ------------------------------------------------------------------------

  function ptWalkCoords(coords, callback) {
    if (!Array.isArray(coords)) return;

    if (coords.length >= 2 && typeof coords[0] === 'number' && typeof coords[1] === 'number') {
      callback(coords[0], coords[1]);
      return;
    }

    coords.forEach(function(child) {
      ptWalkCoords(child, callback);
    });
  }

  function ptCoordinateWarning(featureCollection) {
    var features = featureCollection && Array.isArray(featureCollection.features) ?
      featureCollection.features : [];
    var checked = 0;
    var bad = 0;

    features.slice(0, 500).forEach(function(f) {
      var g = f && f.geometry ? f.geometry : null;
      if (!g) return;

      ptWalkCoords(g.coordinates, function(x, y) {
        if (checked >= 2000) return;
        checked += 1;

        if (Math.abs(x) > 180 || Math.abs(y) > 90) {
          bad += 1;
        }
      });
    });

    if (checked === 0) return 'No coordinates were found.';

    if (bad > 0) {
      return 'Coordinate values do not look like lon/lat WGS84. Reproject to EPSG:4326 if the layer draws in the wrong place.';
    }

    return '';
  }

  // ------------------------------------------------------------------------
  // Panes
  // ------------------------------------------------------------------------

  function ptEnsurePane(name, zIndex, pointerEvents) {
    if (!map.getPane(name)) {
      map.createPane(name);
    }

    var pane = map.getPane(name);
    pane.style.zIndex = String(zIndex);

    if (pointerEvents !== undefined && pointerEvents !== null) {
      pane.style.pointerEvents = pointerEvents;
    }

    return pane;
  }

  ptEnsurePane('pane_pt_local_polygon', 585, 'auto');
  ptEnsurePane('pane_pt_local_line', 595, 'auto');
  ptEnsurePane('pane_pt_local_point', 605, 'auto');

  // ------------------------------------------------------------------------
  // Local layer state
  // ------------------------------------------------------------------------

  var ptLocalLayers = [];
  var ptLocalSeq = 0;

  function ptLocalLayerById(id) {
    for (var i = 0; i < ptLocalLayers.length; i++) {
      if (ptLocalLayers[i].id === id) return ptLocalLayers[i];
    }
    return null;
  }

  function ptDefaultStyle() {
    var color = PT2_LOCAL_DEFAULT_COLORS[ptLocalLayers.length % PT2_LOCAL_DEFAULT_COLORS.length];

    return {
      strokeColor: color,
      fillColor: color,
      strokeOpacity: 0.95,
      fillOpacity: 0.18,
      weight: 2,
      pointRadius: 5
    };
  }

  function ptActiveStyleField(rec) {
    if (!rec || !rec.symbology) return '';
    if (rec.symbology.mode === 'numeric') return rec.symbology.numericField || '';
    if (rec.symbology.mode === 'categories') return rec.symbology.categoryField || '';
    return '';
  }

  function ptRebuildSymbology(rec) {
    if (!rec || !rec.symbology) return;
    var features = rec.featureCollection && Array.isArray(rec.featureCollection.features) ?
      rec.featureCollection.features : [];
    var mode = rec.symbology.mode;
    var field = ptActiveStyleField(rec);

    rec.symbology.result = null;
    if (!field || mode === 'single') return;

    if (mode === 'numeric') {
      var numericField = rec.numericFields.some(function(info) {
        return info.field === field && info.numeric;
      });
      if (!numericField) return;
      rec.symbology.result = ptAttr.numericClassification(features, field, {
        classCount: rec.symbology.classCount,
        method: rec.symbology.method,
        palette: rec.symbology.palette,
        reverse: rec.symbology.reverse
      });
    } else if (mode === 'categories') {
      rec.symbology.result = ptAttr.categoryClassification(features, field);
    }
  }

  function ptFeatureClassColor(rec, feature) {
    var symbology = rec && rec.symbology ? rec.symbology : null;
    var result = symbology ? symbology.result : null;
    var field = ptActiveStyleField(rec);
    if (!result || !field || symbology.mode === 'single') return '';

    var props = feature && feature.properties ? feature.properties : {};
    var raw = Object.prototype.hasOwnProperty.call(props, field) ? props[field] : undefined;
    var index = result.classIndex(raw);
    if (index < 0) return result.noDataColor || ptAttr.noDataColor;
    if (symbology.mode === 'numeric') {
      return result.bins[index] ? result.bins[index].color : result.noDataColor;
    }
    return result.categories[index] ? result.categories[index].color : result.noDataColor;
  }

  function ptLayerSwatchColor(rec) {
    var result = rec && rec.symbology ? rec.symbology.result : null;
    if (result && rec.symbology.mode === 'numeric' && result.bins.length) {
      return result.bins[Math.floor(result.bins.length / 2)].color;
    }
    if (result && rec.symbology.mode === 'categories' && result.categories.length) {
      return result.categories[0].color;
    }
    if (result && result.noDataCount > 0 && result.noDataColor) {
      return result.noDataColor;
    }
    return rec.style.fillColor;
  }

  function ptPathStyleForFeature(rec, feature) {
    rec = rec || {};
    var style = rec.style || ptDefaultStyle();
    var geomType = feature && feature.geometry ? ptBaseGeomType(feature.geometry.type) : 'Unknown';

    var pane = 'pane_pt_local_polygon';
    if (geomType === 'Line') pane = 'pane_pt_local_line';

    var classColor = ptFeatureClassColor(rec, feature);
    var attributeMode = !!classColor;

    return {
      pane: pane,
      color: attributeMode ? classColor : style.strokeColor,
      weight: Number(style.weight) || 2,
      opacity: Number(style.strokeOpacity),
      fillColor: attributeMode ? classColor : style.fillColor,
      fillOpacity: geomType === 'Polygon' ? Number(style.fillOpacity) : 0,
      interactive: true
    };
  }

  function ptPointStyle(rec, feature) {
    rec = rec || {};
    var style = rec.style || ptDefaultStyle();
    var classColor = ptFeatureClassColor(rec, feature);
    var attributeMode = !!classColor;

    return {
      pane: 'pane_pt_local_point',
      radius: Number(style.pointRadius) || 5,
      color: attributeMode ? classColor : style.strokeColor,
      weight: Number(style.weight) || 2,
      opacity: Number(style.strokeOpacity),
      fillColor: attributeMode ? classColor : style.fillColor,
      fillOpacity: Math.max(0.25, Number(style.fillOpacity)),
      interactive: true
    };
  }

  function ptFeatureTooltipHtml(feature, rec) {
    var props = feature && feature.properties ? feature.properties : {};
    var hoverField = ptCleanText(rec.hoverField);
    var hoverValue = hoverField ? ptValueForField(props, hoverField) : null;

    return '<div class="pt-local-upload-tooltip">' +
      ptEscapeHtml(ptDisplayValue(hoverValue)) + '</div>';
  }

  function ptFeaturePopupHtml(feature, rec) {
    var props = feature && feature.properties ? feature.properties : {};
    var keys = Object.keys(props).filter(function(k) {
      return ptHasValue(props[k]);
    });

    var html = '<b>' + ptEscapeHtml(rec.name) + '</b>' +
      '<br/><span style="font-size:11px;color:#555;">Local uploaded ' +
      ptEscapeHtml(rec.sourceType) + ' layer</span>';

    if (keys.length === 0) {
      html += '<br/>No attributes found.';
      return html;
    }

    var maxRows = Math.min(keys.length, 12);
    html += '<table style="border-collapse:collapse;margin-top:4px;">';

    for (var i = 0; i < maxRows; i++) {
      var k = keys[i];
      html += '<tr>' +
        '<td style="vertical-align:top;padding:1px 6px 1px 0;"><b>' + ptEscapeHtml(k) + '</b></td>' +
        '<td style="vertical-align:top;padding:1px 0;">' + ptEscapeHtml(props[k]) + '</td>' +
        '</tr>';
    }

    html += '</table>';

    if (keys.length > maxRows) {
      html += '<div style="font-size:11px;color:#555;margin-top:3px;">' +
        (keys.length - maxRows) + ' additional attribute(s) omitted for readability.' +
        '</div>';
    }

    return html;
  }

  function ptBindLocalInteractions(layer, rec, inheritedFeature) {
    if (!layer) return;

    var feature = layer.feature || inheritedFeature || null;
    var isFeatureGroup = layer.getLayers && layer.eachLayer &&
      !layer.getLatLng && !layer.getLatLngs;

    layer.unbindTooltip && layer.unbindTooltip();
    layer.unbindPopup && layer.unbindPopup();

    if (isFeatureGroup) {
      layer.eachLayer(function(child) {
        ptBindLocalInteractions(child, rec, feature);
      });
      return;
    }

    if (!feature) return;

    if (rec && rec.hoverEnabled === true && ptCleanText(rec.hoverField)) {
      layer.bindTooltip(ptFeatureTooltipHtml(feature, rec), {
        sticky: true,
        direction: 'auto',
        opacity: 0.95,
        className: 'pt-local-upload-tooltip-wrap'
      });
    }

    if (rec && rec.popupEnabled !== false) {
      layer.bindPopup(ptFeaturePopupHtml(feature, rec), {
        maxWidth: 360
      });
    }
  }

  function ptRefreshLocalInteractions(rec) {
    if (!rec || !rec.layer) return;
    ptBindLocalInteractions(rec.layer, rec, null);
  }

  function ptRestyleFeatureLayer(layer, inheritedFeature, rec) {
    if (!layer) return;
    var feature = layer.feature || inheritedFeature || null;
    var isFeatureGroup = layer.getLayers && layer.eachLayer &&
      !layer.getLatLng && !layer.getLatLngs;

    if (isFeatureGroup) {
      layer.eachLayer(function(child) {
        ptRestyleFeatureLayer(child, feature, rec);
      });
      return;
    }

    var geomType = feature && feature.geometry ? ptBaseGeomType(feature.geometry.type) : 'Unknown';
    if (layer.setRadius) geomType = 'Point';
    else if (window.L && L.Polygon && layer instanceof L.Polygon) geomType = 'Polygon';
    else if (window.L && L.Polyline && layer instanceof L.Polyline) geomType = 'Line';

    var styleFeature = feature;
    if (!styleFeature || !styleFeature.geometry || ptBaseGeomType(styleFeature.geometry.type) !== geomType) {
      styleFeature = {
        type: 'Feature',
        properties: feature && feature.properties ? feature.properties : {},
        geometry: {type: geomType}
      };
    }

    if (geomType === 'Point' && layer.setRadius) {
      layer.setRadius(Number(rec.style.pointRadius) || 5);
      layer.setStyle(ptPointStyle(rec, styleFeature));
    } else if (layer.setStyle) {
      layer.setStyle(ptPathStyleForFeature(rec, styleFeature));
    }
  }

  function ptRestyleLocalLayer(rec) {
    if (!rec || !rec.layer || !rec.layer.eachLayer) return;

    rec.layer.eachLayer(function(layer) {
      ptRestyleFeatureLayer(layer, null, rec);
    });

    ptRenderLocalLayerList();
    ptUpdateLocalLegend(rec);
  }

  function ptCreateLeafletLayer(featureCollection, rec) {
    return L.geoJSON(featureCollection, {
      style: function(feature) {
        return ptPathStyleForFeature(rec, feature);
      },
      pointToLayer: function(feature, latlng) {
        return L.circleMarker(latlng, ptPointStyle(rec, feature));
      }
    });
  }

  // ------------------------------------------------------------------------
  // Per-upload attribute legends
  // ------------------------------------------------------------------------

  function ptLegendActionsHtml() {
    if (window.BRIM && window.BRIM.legendCloseout && window.BRIM.legendCloseout.actionsHtml) {
      return window.BRIM.legendCloseout.actionsHtml(
        'pt-local-upload-legend-dock',
        'pt-local-upload-legend-close',
        'upload attribute legend'
      );
    }
    return '<span class="pt-map-card-actions">' +
      '<button type="button" class="pt-map-card-dock pt-local-upload-legend-dock" aria-label="Undock upload attribute legend" title="Undock upload attribute legend">&#x2197;</button>' +
      '<button type="button" class="pt-map-legend-close pt-local-upload-legend-close" aria-label="Hide upload attribute legend" title="Hide upload attribute legend">&times;</button>' +
      '</span>';
  }

  function ptEnsureLocalLegend(rec) {
    if (!rec || rec.legendControl) return;

    var control = L.control({position: 'topright'});
    control.onAdd = function() {
      var div = L.DomUtil.create('div', 'pt-local-upload-legend leaflet-control pt-map-legend-card');
      div.id = 'pt-local-upload-legend-' + rec.id;
      div.innerHTML =
        '<div class="pt-local-upload-legend-head pt-map-card-handle">' +
          '<span class="pt-local-upload-legend-title"></span>' +
          ptLegendActionsHtml() +
        '</div>' +
        '<div class="pt-local-upload-legend-field"></div>' +
        '<div class="pt-local-upload-legend-body"></div>';
      L.DomEvent.disableClickPropagation(div);
      L.DomEvent.disableScrollPropagation(div);
      return div;
    };
    control.addTo(map);

    rec.legendControl = control;
    rec.legendDiv = control.getContainer ? control.getContainer() : null;
    if (!rec.legendDiv) return;

    if (window.BRIM && window.BRIM.legendCloseout) {
      window.BRIM.legendCloseout.wire(
        rec.legendDiv,
        '.pt-local-upload-legend-close',
        function() { rec.legendHidden = true; }
      );
      window.BRIM.legendCloseout.makeDetachable({
        card: rec.legendDiv,
        map: map,
        handleSelector: '.pt-local-upload-legend-head',
        dockSelector: '.pt-local-upload-legend-dock',
        label: 'upload attribute legend'
      });
    }
  }

  function ptDestroyLocalLegend(rec) {
    if (!rec) return;
    var div = rec.legendDiv;
    if (div && div.__brimDetachableState && div.__brimDetachableState.destroy) {
      div.__brimDetachableState.destroy(false);
    }
    if (rec.legendControl) {
      try { map.removeControl(rec.legendControl); } catch (err) {}
    } else if (div && div.parentNode) {
      div.parentNode.removeChild(div);
    }
    rec.legendControl = null;
    rec.legendDiv = null;
    rec.legendHidden = false;
  }

  function ptLegendRow(color, label, count) {
    return '<div class="pt-local-upload-legend-row">' +
      '<span class="pt-local-upload-legend-swatch" style="background:' + ptEscapeHtml(color) + ';"></span>' +
      '<span class="pt-local-upload-legend-label">' + ptEscapeHtml(label) + '</span>' +
      '<span class="pt-local-upload-legend-count">' + Number(count || 0).toLocaleString() + '</span>' +
      '</div>';
  }

  function ptUpdateLocalLegend(rec) {
    if (!rec || !rec.symbology) return;
    var mode = rec.symbology.mode;
    var result = rec.symbology.result;
    var field = ptActiveStyleField(rec);
    var hasClasses = result && (
      (mode === 'numeric' && result.bins && result.bins.length) ||
      (mode === 'categories' && (
        (result.categories && result.categories.length) || result.noDataCount > 0
      ))
    );

    if (mode === 'single' || !field || !hasClasses) {
      ptDestroyLocalLegend(rec);
      return;
    }

    ptEnsureLocalLegend(rec);
    var div = rec.legendDiv;
    if (!div) return;

    var title = div.querySelector('.pt-local-upload-legend-title');
    var fieldDiv = div.querySelector('.pt-local-upload-legend-field');
    var body = div.querySelector('.pt-local-upload-legend-body');
    if (title) title.textContent = ptShortName(rec.name, 38);
    if (fieldDiv) fieldDiv.textContent = field;

    var html = '';
    if (mode === 'numeric') {
      result.bins.forEach(function(bin) {
        html += ptLegendRow(bin.color, bin.label, bin.count);
      });
      if (result.noDataCount > 0) {
        html += ptLegendRow(result.noDataColor, 'No data', result.noDataCount);
      }
      html += '<div class="pt-local-upload-legend-note">' +
        (rec.symbology.method === 'equal' ? 'Equal interval' : 'Quantile') +
        '; ' + result.effectiveCount + ' effective class' + (result.effectiveCount === 1 ? '' : 'es') +
        (result.effectiveCount < result.requestedCount ? ' (target ' + result.requestedCount + ')' : '') +
        '.</div>';
    } else {
      result.categories.forEach(function(category) {
        html += ptLegendRow(category.color, category.label, category.count);
      });
      if (result.noDataCount > 0) {
        html += ptLegendRow(result.noDataColor, 'No data', result.noDataCount);
      }
      if (result.highCardinality) {
        html += '<div class="pt-local-upload-legend-warning">High cardinality: ' +
          result.categories.length.toLocaleString() + ' categories.</div>';
      }
    }
    if (body) body.innerHTML = html;
    div.style.display = rec.visible && !rec.legendHidden ? 'block' : 'none';
  }

  // ------------------------------------------------------------------------
  // GeoJSON normalization
  // ------------------------------------------------------------------------

  function ptAsFeatureCollection(obj, sourceName) {
    var features = [];

    function addFeatureCollection(fc, sourceLayerName) {
      if (!fc || !Array.isArray(fc.features)) return;

      fc.features.forEach(function(f) {
        if (!f || !f.geometry) return;

        f.properties = f.properties || {};

        if (sourceLayerName && !Object.prototype.hasOwnProperty.call(f.properties, '_pt2_source_layer')) {
          f.properties._pt2_source_layer = sourceLayerName;
        }

        features.push(f);
      });
    }

    if (!obj) {
      return {type: 'FeatureCollection', features: []};
    }

    if (obj.type === 'FeatureCollection') {
      addFeatureCollection(obj, '');
    } else if (obj.type === 'Feature') {
      features.push(obj);
    } else if (Array.isArray(obj)) {
      obj.forEach(function(item, idx) {
        if (item && item.type === 'FeatureCollection') {
          addFeatureCollection(item, sourceName + '_' + (idx + 1));
        } else if (item && item.type === 'Feature') {
          features.push(item);
        }
      });
    } else if (typeof obj === 'object') {
      Object.keys(obj).forEach(function(key) {
        var item = obj[key];
        if (item && item.type === 'FeatureCollection') {
          addFeatureCollection(item, key);
        } else if (item && item.type === 'Feature') {
          item.properties = item.properties || {};
          if (!Object.prototype.hasOwnProperty.call(item.properties, '_pt2_source_layer')) {
            item.properties._pt2_source_layer = key;
          }
          features.push(item);
        }
      });
    }

    return {
      type: 'FeatureCollection',
      features: features
    };
  }

  // ------------------------------------------------------------------------
  // Layer add/remove/toggle/zoom
  // ------------------------------------------------------------------------

  function ptAddLocalFeatureCollection(featureCollection, file, sourceType) {
    featureCollection = ptAsFeatureCollection(featureCollection, file ? file.name : 'local_upload');

    var features = featureCollection.features || [];

    if (features.length === 0) {
      ptSetLocalStatus('No features were found in the uploaded file.', true);
      return;
    }

    if (features.length > PT2_LOCAL_MAX_FEATURES) {
      ptSetLocalStatus(
        'Layer has ' + features.length.toLocaleString() +
        ' features. This exceeds the current browser-session limit of ' +
        PT2_LOCAL_MAX_FEATURES.toLocaleString() + '.',
        true
      );
      return;
    }

    if (ptLocalLayers.length >= PT2_LOCAL_MAX_LAYERS) {
      ptSetLocalStatus('Limit reached: remove a local layer before adding another.', true);
      return;
    }

    ptLocalSeq += 1;

    var fields = ptCollectFields(featureCollection);
    var numericFields = ptAttr.detectNumericFields(features, fields);
    var geomType = ptGeomTypeLabel(featureCollection);
    var rec = {
      id: ptAttr.uploadId(ptLocalSeq),
      name: file && file.name ? file.name : ('Local layer ' + ptLocalSeq),
      sourceType: sourceType,
      fileSize: file && file.size ? file.size : 0,
      featureCount: features.length,
      geomType: geomType,
      fields: fields,
      numericFields: numericFields,
      featureCollection: featureCollection,
      hoverEnabled: false,
      hoverField: '',
      popupEnabled: true,
      visible: true,
      style: ptDefaultStyle(),
      symbology: {
        mode: 'single',
        numericField: numericFields.length ? numericFields[0].field : '',
        categoryField: fields.length ? fields[0] : '',
        classCount: 7,
        method: 'quantile',
        palette: 'Viridis',
        reverse: false,
        result: null
      },
      legendHidden: false,
      legendControl: null,
      legendDiv: null,
      layer: null
    };

    rec.layer = ptCreateLeafletLayer(featureCollection, rec);
    ptLocalLayers.push(rec);
    ptRefreshLocalInteractions(rec);
    rec.layer.addTo(map);
    ptRenderLocalLayerList();
    ptRenderLocalStylePanel(rec.id);

    var warning = ptCoordinateWarning(featureCollection);
    var msg = 'Loaded ' + features.length.toLocaleString() + ' feature(s) from ' + rec.name + '.';

    if (features.length >= PT2_LOCAL_WARN_FEATURES) {
      msg += ' Large layer: browser performance may be slower.';
    }

    if (warning) {
      msg += ' Warning: ' + warning;
    }

    ptSetLocalStatus(msg, !!warning);
  }

  function ptRemoveLocalLayer(id) {
    var keep = [];

    ptLocalLayers.forEach(function(rec) {
      if (rec.id === id) {
        ptDestroyLocalLegend(rec);
        if (rec.layer && map.hasLayer(rec.layer)) {
          map.removeLayer(rec.layer);
        }
        if (rec.layer && rec.layer.eachLayer) {
          rec.layer.eachLayer(function(layer) {
            if (layer.unbindTooltip) layer.unbindTooltip();
            if (layer.unbindPopup) layer.unbindPopup();
            if (layer.off) layer.off();
          });
        }
      } else {
        keep.push(rec);
      }
    });

    ptLocalLayers = keep;
    ptRenderLocalLayerList();
    ptRenderLocalStylePanel('');
    ptSetLocalStatus('Local layer removed.', false);
  }

  function ptToggleLocalLayer(id, visible) {
    var rec = ptLocalLayerById(id);
    if (!rec) return;

    rec.visible = visible;

    if (visible) {
      if (rec.layer && !map.hasLayer(rec.layer)) {
        rec.layer.addTo(map);
      }
    } else {
      if (rec.layer && map.hasLayer(rec.layer)) {
        map.removeLayer(rec.layer);
      }
    }
    ptUpdateLocalLegend(rec);
  }

  function ptZoomToLocalLayer(id) {
    var rec = ptLocalLayerById(id);
    if (!rec || !rec.layer || !rec.layer.getBounds) {
      ptSetLocalStatus('Could not zoom to this layer. No valid geometry is available.', true);
      return;
    }

    var bounds = rec.layer.getBounds();

    if (bounds && bounds.isValid && bounds.isValid()) {
      var northEast = bounds.getNorthEast();
      var southWest = bounds.getSouthWest();
      var isSingleLocation = northEast && southWest &&
        northEast.lat === southWest.lat && northEast.lng === southWest.lng;

      if (isSingleLocation) {
        var maxZoom = map.getMaxZoom ? map.getMaxZoom() : 18;
        if (!isFinite(maxZoom)) maxZoom = 18;
        map.setView(bounds.getCenter(), Math.min(maxZoom, 16));
      } else {
        var mapSize = map.getSize ? map.getSize() : null;
        var leftPadding = mapSize && mapSize.x >= 850 ? 375 : 28;
        map.fitBounds(bounds.pad(0.06), {
          paddingTopLeft: L.point(leftPadding, 42),
          paddingBottomRight: L.point(32, 64),
          maxZoom: 16
        });
      }
      ptSetLocalStatus('Zoomed to ' + rec.name + '.', false);
    } else {
      ptSetLocalStatus('Could not zoom to this layer. The layer may not have valid bounds.', true);
    }
  }

  function ptClearLocalLayers() {
    ptLocalLayers.forEach(function(rec) {
      ptDestroyLocalLegend(rec);
      if (rec.layer && map.hasLayer(rec.layer)) {
        map.removeLayer(rec.layer);
      }
      if (rec.layer && rec.layer.eachLayer) {
        rec.layer.eachLayer(function(layer) {
          if (layer.unbindTooltip) layer.unbindTooltip();
          if (layer.unbindPopup) layer.unbindPopup();
          if (layer.off) layer.off();
        });
      }
    });

    ptLocalLayers = [];
    ptRenderLocalLayerList();
    ptRenderLocalStylePanel('');
    ptSetLocalStatus('All local uploaded layers cleared.', false);
  }

  // ------------------------------------------------------------------------
  // File readers
  // ------------------------------------------------------------------------

  function ptEnsureShpJs(callback) {
    if (window.shp && typeof window.shp === 'function') {
      callback(true);
      return;
    }

    if (window.pt2ShpJsLoading) {
      window.pt2ShpJsCallbacks = window.pt2ShpJsCallbacks || [];
      window.pt2ShpJsCallbacks.push(callback);
      return;
    }

    window.pt2ShpJsLoading = true;
    window.pt2ShpJsCallbacks = [callback];

    ptSetLocalStatus('Loading browser-side shapefile reader...', false);

    var script = document.createElement('script');
    script.src = PT2_SHPJS_URL;
    script.async = true;

    script.onload = function() {
      window.pt2ShpJsLoading = false;
      var ok = !!(window.shp && typeof window.shp === 'function');
      var callbacks = window.pt2ShpJsCallbacks || [];
      window.pt2ShpJsCallbacks = [];

      callbacks.forEach(function(cb) {
        try { cb(ok); } catch (e) { console.error(e); }
      });
    };

    script.onerror = function() {
      window.pt2ShpJsLoading = false;
      var callbacks = window.pt2ShpJsCallbacks || [];
      window.pt2ShpJsCallbacks = [];

      callbacks.forEach(function(cb) {
        try { cb(false); } catch (e) { console.error(e); }
      });
    };

    document.head.appendChild(script);
  }

  function ptReadGeoJsonFile(file) {
    var reader = new FileReader();

    reader.onload = function(e) {
      try {
        var parsed = JSON.parse(e.target.result);
        ptAddLocalFeatureCollection(parsed, file, 'GeoJSON');
      } catch (err) {
        console.error(err);
        ptSetLocalStatus('Could not parse GeoJSON. Check that the file is valid .geojson or .json.', true);
      }
    };

    reader.onerror = function() {
      ptSetLocalStatus('Browser could not read the GeoJSON file.', true);
    };

    ptSetLocalStatus('Reading GeoJSON file...', false);
    reader.readAsText(file);
  }

  function ptReadShapefileZip(file) {
    ptEnsureShpJs(function(ok) {
      if (!ok) {
        ptSetLocalStatus('Could not load the shapefile reader. Check internet/CDN access or use GeoJSON.', true);
        return;
      }

      var reader = new FileReader();

      reader.onload = function(e) {
        try {
          ptSetLocalStatus('Parsing zipped shapefile...', false);

          window.shp(e.target.result)
            .then(function(geojson) {
              ptAddLocalFeatureCollection(geojson, file, 'zipped shapefile');
            })
            .catch(function(err) {
              console.error(err);
              ptSetLocalStatus('Could not parse zipped shapefile. Confirm the ZIP includes .shp, .shx, .dbf, and preferably .prj.', true);
            });

        } catch (err) {
          console.error(err);
          ptSetLocalStatus('Could not parse zipped shapefile.', true);
        }
      };

      reader.onerror = function() {
        ptSetLocalStatus('Browser could not read the zipped shapefile.', true);
      };

      ptSetLocalStatus('Reading zipped shapefile...', false);
      reader.readAsArrayBuffer(file);
    });
  }

  function ptHandleLocalFile(file) {
    if (!file) return;

    if (ptLocalLayers.length >= PT2_LOCAL_MAX_LAYERS) {
      ptSetLocalStatus('Limit reached: remove a local layer before adding another.', true);
      return;
    }

    var fileMb = file.size / (1024 * 1024);
    if (fileMb > PT2_LOCAL_MAX_FILE_MB) {
      ptSetLocalStatus(
        'File is ' + ptFormatBytes(file.size) +
        '. Current upload limit is ' + PT2_LOCAL_MAX_FILE_MB + ' MB.',
        true
      );
      return;
    }

    var lower = String(file.name || '').toLowerCase();

    if (lower.match(/\.geojson$/) || lower.match(/\.json$/)) {
      ptReadGeoJsonFile(file);
      return;
    }

    if (lower.match(/\.zip$/)) {
      ptReadShapefileZip(file);
      return;
    }

    ptSetLocalStatus('Unsupported file type. Use .zip shapefile, .geojson, or .json.', true);
  }

  // ------------------------------------------------------------------------
  // Style UI
  // ------------------------------------------------------------------------

  function ptRenderLocalStylePanel(selectedId) {
    var targetSelect = document.getElementById('pt-local-style-target');
    var styleBlock = document.getElementById('pt-local-style-controls');

    if (!targetSelect || !styleBlock) return;

    var html = '<option value="">Choose active layer...</option>';
    var nameTotals = Object.create(null);
    var nameSeen = Object.create(null);
    ptLocalLayers.forEach(function(rec) {
      nameTotals[rec.name] = (nameTotals[rec.name] || 0) + 1;
    });
    ptLocalLayers.forEach(function(rec) {
      nameSeen[rec.name] = (nameSeen[rec.name] || 0) + 1;
      var duplicateSuffix = nameTotals[rec.name] > 1 ? ' [' + nameSeen[rec.name] + ']' : '';
      html += '<option value="' + ptEscapeHtml(rec.id) + '">' +
        ptEscapeHtml(ptShortName(rec.name, 40) + duplicateSuffix) + '</option>';
    });

    targetSelect.innerHTML = html;

    if (selectedId && ptLocalLayerById(selectedId)) {
      targetSelect.value = selectedId;
    }

    var rec = ptLocalLayerById(targetSelect.value);

    if (!rec) {
      styleBlock.style.display = 'none';
      return;
    }

    styleBlock.style.display = 'block';
    var styleMode = document.getElementById('pt-local-style-mode');
    var styleField = document.getElementById('pt-local-style-field');
    var attributeBlock = document.getElementById('pt-local-attribute-controls');
    var numericBlock = document.getElementById('pt-local-numeric-controls');
    var singleColorBlock = document.getElementById('pt-local-single-color-controls');
    var classCount = document.getElementById('pt-local-class-count');
    var classMethod = document.getElementById('pt-local-class-method');
    var palette = document.getElementById('pt-local-palette');
    var reverse = document.getElementById('pt-local-reverse-palette');
    var styleNote = document.getElementById('pt-local-style-note');
    var strokeColor = document.getElementById('pt-local-stroke-color');
    var fillColor = document.getElementById('pt-local-fill-color');
    var fillOpacity = document.getElementById('pt-local-fill-opacity');
    var strokeOpacity = document.getElementById('pt-local-stroke-opacity');
    var weight = document.getElementById('pt-local-weight');
    var radius = document.getElementById('pt-local-point-radius');

    if (styleMode) styleMode.value = rec.symbology.mode;
    if (singleColorBlock) singleColorBlock.style.display = rec.symbology.mode === 'single' ? 'grid' : 'none';
    if (attributeBlock) attributeBlock.style.display = rec.symbology.mode === 'single' ? 'none' : 'block';
    if (numericBlock) numericBlock.style.display = rec.symbology.mode === 'numeric' ? 'block' : 'none';

    if (styleField) {
      var styleFieldHtml = '<option value="">Choose attribute...</option>';
      if (rec.symbology.mode === 'numeric') {
        rec.numericFields.forEach(function(info) {
          styleFieldHtml += '<option value="' + ptEscapeHtml(info.field) + '">' +
            ptEscapeHtml(info.field + (info.convertedText ? ' (numeric text)' : '')) + '</option>';
        });
        if (!rec.numericFields.length) styleFieldHtml = '<option value="">No validated numeric fields</option>';
      } else {
        rec.fields.forEach(function(field) {
          styleFieldHtml += '<option value="' + ptEscapeHtml(field) + '">' + ptEscapeHtml(field) + '</option>';
        });
        if (!rec.fields.length) styleFieldHtml = '<option value="">No attribute fields</option>';
      }
      styleField.innerHTML = styleFieldHtml;
      styleField.value = ptActiveStyleField(rec);
      styleField.disabled = rec.symbology.mode === 'numeric' ? !rec.numericFields.length : !rec.fields.length;
    }

    if (classCount) classCount.value = String(rec.symbology.classCount);
    if (classMethod) classMethod.value = rec.symbology.method;
    if (palette) {
      palette.innerHTML = ptAttr.paletteNames.map(function(name) {
        return '<option value="' + ptEscapeHtml(name) + '">' + ptEscapeHtml(name) + '</option>';
      }).join('');
      palette.value = rec.symbology.palette;
    }
    if (reverse) reverse.checked = !!rec.symbology.reverse;

    var note = '';
    var result = rec.symbology.result;
    if (rec.symbology.mode === 'numeric') {
      var selectedNumeric = rec.numericFields.filter(function(info) {
        return info.field === rec.symbology.numericField;
      })[0];
      if (!rec.numericFields.length) {
        note = 'No field passed strict numeric validation.';
      } else if (selectedNumeric && selectedNumeric.convertedText) {
        note = 'Validated numeric text is converted for styling only; source values are unchanged.';
      }
      if (result && result.effectiveCount < result.requestedCount) {
        note += (note ? ' ' : '') + 'Using ' + result.effectiveCount +
          ' nonempty class' + (result.effectiveCount === 1 ? '' : 'es') +
          ' for the requested ' + result.requestedCount + '.';
      }
    } else if (rec.symbology.mode === 'categories' && result && result.highCardinality) {
      note = 'High cardinality: ' + result.categories.length.toLocaleString() +
        ' categories may make the legend and rendering difficult to use.';
    }
    if (styleNote) {
      styleNote.textContent = note;
      styleNote.className = 'pt-local-muted' +
        (rec.symbology.mode === 'categories' && result && result.highCardinality ? ' pt-local-warning' : '');
    }

    if (strokeColor) strokeColor.value = rec.style.strokeColor;
    if (fillColor) fillColor.value = rec.style.fillColor;
    if (fillOpacity) fillOpacity.value = rec.style.fillOpacity;
    if (strokeOpacity) strokeOpacity.value = rec.style.strokeOpacity;
    if (weight) weight.value = rec.style.weight;
    if (radius) radius.value = rec.style.pointRadius;
  }

  function ptApplyLocalStyleFromControls(event) {
    var targetSelect = document.getElementById('pt-local-style-target');
    if (!targetSelect || !targetSelect.value) return;

    var rec = ptLocalLayerById(targetSelect.value);
    if (!rec) return;

    var styleMode = document.getElementById('pt-local-style-mode');
    var styleField = document.getElementById('pt-local-style-field');
    var classCount = document.getElementById('pt-local-class-count');
    var classMethod = document.getElementById('pt-local-class-method');
    var palette = document.getElementById('pt-local-palette');
    var reverse = document.getElementById('pt-local-reverse-palette');
    var strokeColor = document.getElementById('pt-local-stroke-color');
    var fillColor = document.getElementById('pt-local-fill-color');
    var fillOpacity = document.getElementById('pt-local-fill-opacity');
    var strokeOpacity = document.getElementById('pt-local-stroke-opacity');
    var weight = document.getElementById('pt-local-weight');
    var radius = document.getElementById('pt-local-point-radius');

    var previousMode = rec.symbology.mode;
    if (styleMode) rec.symbology.mode = styleMode.value;

    if (previousMode !== rec.symbology.mode) {
      rec.legendHidden = false;
    } else if (styleField) {
      if (rec.symbology.mode === 'numeric') rec.symbology.numericField = styleField.value;
      if (rec.symbology.mode === 'categories') rec.symbology.categoryField = styleField.value;
    }

    if (classCount) rec.symbology.classCount = Number(classCount.value) || 7;
    if (classMethod) rec.symbology.method = classMethod.value;
    if (palette) rec.symbology.palette = palette.value;
    if (reverse) rec.symbology.reverse = !!reverse.checked;
    if (strokeColor) rec.style.strokeColor = strokeColor.value;
    if (fillColor) rec.style.fillColor = fillColor.value;
    if (fillOpacity) rec.style.fillOpacity = Number(fillOpacity.value);
    if (strokeOpacity) rec.style.strokeOpacity = Number(strokeOpacity.value);
    if (weight) rec.style.weight = Number(weight.value);
    if (radius) rec.style.pointRadius = Number(radius.value);

    ptRebuildSymbology(rec);
    ptRestyleLocalLayer(rec);
    ptRefreshLocalInteractions(rec);
    ptRenderLocalStylePanel(rec.id);

  }

  function ptSyncLocalInteractionControls(rec) {
    var list = document.getElementById('pt-local-layer-list');
    if (!list || !rec) return;

    var hoverToggle = list.querySelector('[data-pt-local-hover="' + rec.id + '"]');
    var popupToggle = list.querySelector('[data-pt-local-popup="' + rec.id + '"]');
    var hoverState = list.querySelector('[data-pt-local-hover-state="' + rec.id + '"]');
    var popupState = list.querySelector('[data-pt-local-popup-state="' + rec.id + '"]');
    var hoverFieldBlock = list.querySelector('[data-pt-local-hover-field-block="' + rec.id + '"]');
    var hoverField = list.querySelector('[data-pt-local-hover-field="' + rec.id + '"]');

    if (hoverToggle) hoverToggle.checked = !!rec.hoverEnabled;
    if (popupToggle) popupToggle.checked = rec.popupEnabled !== false;
    if (hoverState) hoverState.textContent = rec.hoverEnabled ? 'On' : 'Off';
    if (popupState) popupState.textContent = rec.popupEnabled !== false ? 'On' : 'Off';
    if (hoverFieldBlock) hoverFieldBlock.hidden = !rec.hoverEnabled;
    if (hoverField) {
      hoverField.value = rec.hoverField || '';
      hoverField.disabled = !rec.hoverEnabled || !rec.fields.length;
    }
  }

  function ptSetLocalHover(id, enabled) {
    var rec = ptLocalLayerById(id);
    if (!rec) return;

    rec.hoverEnabled = !!enabled;
    ptRefreshLocalInteractions(rec);
    ptSyncLocalInteractionControls(rec);

    if (rec.hoverEnabled && !rec.hoverField) {
      ptSetLocalStatus('Hover is On for ' + rec.name + '. Choose a Hover field.', false);
    }
  }

  function ptSetLocalHoverField(id, fieldName) {
    var rec = ptLocalLayerById(id);
    if (!rec) return;

    var nextField = fieldName || '';
    if (rec.hoverField === nextField) return;
    rec.hoverField = nextField;
    ptRefreshLocalInteractions(rec);
  }

  function ptSetLocalPopup(id, enabled) {
    var rec = ptLocalLayerById(id);
    if (!rec) return;

    rec.popupEnabled = !!enabled;
    ptRefreshLocalInteractions(rec);
    ptSyncLocalInteractionControls(rec);
  }

  // ------------------------------------------------------------------------
  // Render active local layer list
  // ------------------------------------------------------------------------

  function ptLocalLayerHasValidBounds(rec) {
    if (!rec || !rec.layer || !rec.layer.getBounds) return false;

    try {
      var bounds = rec.layer.getBounds();
      return !!(bounds && bounds.isValid && bounds.isValid());
    } catch (err) {
      return false;
    }
  }

  function ptRenderLocalLayerList() {
    var list = document.getElementById('pt-local-layer-list');
    if (!list) return;

    if (ptLocalLayers.length === 0) {
      list.innerHTML = '<div class="pt-local-muted">No local files uploaded.</div>';
      return;
    }

    var html = '';

    ptLocalLayers.forEach(function(rec) {
      var swatchColor = ptLayerSwatchColor(rec);
      var styleSummary = rec.symbology.mode === 'single' ? 'single color' :
        (rec.symbology.mode === 'numeric' ? 'numeric: ' : 'categories: ') +
        (ptActiveStyleField(rec) || 'choose field');
      var safeId = ptEscapeHtml(rec.id);
      var hoverToggleId = 'pt-local-hover-' + safeId;
      var popupToggleId = 'pt-local-popup-' + safeId;
      var hoverFieldId = 'pt-local-hover-field-' + safeId;
      var hoverOptions = '<option value="">Choose attribute...</option>';
      rec.fields.forEach(function(field) {
        hoverOptions += '<option value="' + ptEscapeHtml(field) + '" ' +
          (field === rec.hoverField ? 'selected' : '') + '>' + ptEscapeHtml(field) + '</option>';
      });
      if (!rec.fields.length) hoverOptions = '<option value="">No attribute fields</option>';
      var hasValidBounds = ptLocalLayerHasValidBounds(rec);

      html +=
        '<div class="pt-local-layer-row">' +
          '<div class="pt-local-layer-head">' +
            '<label class="pt-local-visibility" title="Show or hide ' + ptEscapeHtml(rec.name) + '">' +
              '<input type="checkbox" data-pt-local-toggle="' + safeId + '" aria-label="Show or hide ' + ptEscapeHtml(rec.name) + '" ' +
                (rec.visible ? 'checked' : '') + '/>' +
              '<span class="pt-local-swatch" aria-hidden="true" style="background:' + ptEscapeHtml(swatchColor) + ';border-color:' + ptEscapeHtml(rec.style.strokeColor) + ';"></span>' +
            '</label>' +
            '<b class="pt-local-layer-name" title="' + ptEscapeHtml(rec.name) + '">' +
              ptEscapeHtml(ptShortName(rec.name, 30)) +
            '</b>' +
            '<div class="pt-local-layer-actions">' +
              '<button type="button" class="pt-local-icon-btn pt-local-style-icon" data-pt-local-style="' + safeId + '" aria-label="Style layer" title="Style layer">&#9881;&#65038;</button>' +
              '<button type="button" class="pt-local-icon-btn" data-pt-local-zoom="' + safeId + '" aria-label="Zoom to layer" title="' +
                (hasValidBounds ? 'Zoom to layer' : 'No valid geometry to zoom to') + '" ' + (hasValidBounds ? '' : 'disabled') + '>&#8982;</button>' +
              '<button type="button" class="pt-local-icon-btn pt-local-remove-icon" data-pt-local-remove="' + safeId + '" aria-label="Remove layer" title="Remove layer">&times;</button>' +
            '</div>' +
          '</div>' +
          '<div class="pt-local-muted">' +
            ptEscapeHtml(rec.geomType) + ' | ' +
            rec.featureCount.toLocaleString() + ' feature(s) | ' +
            ptEscapeHtml(ptFormatBytes(rec.fileSize)) + '<br/>' +
            ptEscapeHtml(styleSummary) +
          '</div>' +
          '<div class="pt-local-interaction-row" aria-label="Layer interactions">' +
            '<label class="pt-local-switch" for="' + hoverToggleId + '">' +
              '<span>Hover</span>' +
              '<input type="checkbox" role="switch" id="' + hoverToggleId + '" data-pt-local-hover="' + safeId + '" ' +
                (rec.hoverEnabled ? 'checked' : '') + '/>' +
              '<span class="pt-local-switch-track" aria-hidden="true"></span>' +
              '<span class="pt-local-switch-state" data-pt-local-hover-state="' + safeId + '" aria-hidden="true">' +
                (rec.hoverEnabled ? 'On' : 'Off') + '</span>' +
            '</label>' +
            '<label class="pt-local-switch" for="' + popupToggleId + '">' +
              '<span>Popup</span>' +
              '<input type="checkbox" role="switch" id="' + popupToggleId + '" data-pt-local-popup="' + safeId + '" ' +
                (rec.popupEnabled !== false ? 'checked' : '') + '/>' +
              '<span class="pt-local-switch-track" aria-hidden="true"></span>' +
              '<span class="pt-local-switch-state" data-pt-local-popup-state="' + safeId + '" aria-hidden="true">' +
                (rec.popupEnabled !== false ? 'On' : 'Off') + '</span>' +
            '</label>' +
            '<div class="pt-local-hover-field" data-pt-local-hover-field-block="' + safeId + '" ' +
              (rec.hoverEnabled ? '' : 'hidden') + '>' +
              '<label for="' + hoverFieldId + '">Field</label>' +
              '<select id="' + hoverFieldId + '" data-pt-local-hover-field="' + safeId + '" ' +
                (!rec.hoverEnabled || !rec.fields.length ? 'disabled' : '') + '>' + hoverOptions + '</select>' +
            '</div>' +
          '</div>' +
        '</div>';
    });

    list.innerHTML = html;
  }

  // ------------------------------------------------------------------------
  // Build UI
  // ------------------------------------------------------------------------

  function buildLocalUploadPanel() {
    var mapContainer = map.getContainer ? map.getContainer() : el.querySelector('.leaflet-container');
    if (!mapContainer) return;

    if (document.getElementById('pt-local-upload-wrap')) {
      return;
    }

    if (!document.getElementById('pt-local-upload-style')) {
      var style = document.createElement('style');
      style.id = 'pt-local-upload-style';
      style.innerHTML = `
        .pt-local-upload-wrap,
        .pt-local-upload-legend {
          --pt-local-upload-background: rgba(255, 248, 218, 0.97);
        }

        .pt-local-upload-wrap {
          position: absolute;
          left: 8px;
          bottom: 18px;
          width: 350px;
          max-width: calc(100vw - 24px);
          z-index: 9965;
          font-family: Arial, Helvetica, sans-serif;
          font-size: 12px;
          color: #222;
          pointer-events: auto;
        }

        .pt-local-upload-tab {
          display: inline-flex;
          align-items: center;
          gap: 8px;
          background: var(--pt-local-upload-background);
          border: 1px solid rgba(151, 126, 58, 0.72);
          border-radius: 6px;
          box-shadow: 0 1px 5px rgba(0,0,0,0.30);
          font-weight: 700;
          padding: 7px 8px 7px 10px;
          cursor: pointer;
          user-select: none;
        }

        .pt-local-upload-title {
          display: inline-flex;
          align-items: center;
          gap: 4px;
          white-space: nowrap;
        }

        .pt-local-ribbon-clear {
          border: 1px solid rgba(98, 117, 74, 0.80);
          border-radius: 4px;
          background: rgba(255, 255, 255, 0.92);
          cursor: pointer;
          font: 11px/1.15 Arial, Helvetica, sans-serif;
          font-weight: 400;
          padding: 3px 7px;
          color: #222;
          appearance: none;
          -webkit-appearance: none;
          box-shadow: 0 1px 3px rgba(0,0,0,0.18);
          transition: background-color 0.10s ease, border-color 0.10s ease, box-shadow 0.10s ease, transform 0.05s ease;
        }

        .pt-local-ribbon-clear:hover,
        .pt-local-ribbon-clear:focus {
          background: rgba(221, 238, 204, 0.98);
          border-color: rgba(66, 102, 47, 0.95);
          box-shadow: 0 1px 5px rgba(0,0,0,0.28);
          outline: none;
        }

        .pt-local-ribbon-clear:active {
          background: rgba(199, 223, 181, 0.98);
          box-shadow: inset 0 1px 3px rgba(0,0,0,0.28);
          transform: translateY(1px);
        }

        .pt-local-upload-body {
          margin-top: 5px;
          background: var(--pt-local-upload-background);
          border: 1px solid rgba(151, 126, 58, 0.72);
          border-radius: 7px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.30);
          padding: 9px;
          max-height: 56vh;
          overflow-y: auto;
          box-sizing: border-box;
        }

        .pt-local-collapsed .pt-local-upload-body {
          display: none;
        }

        .pt-local-section {
          border-top: 1px solid rgba(151, 126, 58, 0.24);
          padding-top: 7px;
          margin-top: 7px;
        }

        .pt-local-section:first-child {
          border-top: none;
          margin-top: 0;
          padding-top: 0;
        }

        .pt-local-subsection {
          border-top: 1px solid rgba(151, 126, 58, 0.24);
          padding-top: 6px;
          margin-top: 6px;
        }

        .pt-local-heading {
          font-weight: 700;
          margin-bottom: 5px;
        }

        .pt-local-muted {
          color: #555;
          font-size: 11px;
          line-height: 1.25;
        }

        .pt-local-status {
          font-size: 11px;
          line-height: 1.25;
          min-height: 14px;
          margin-top: 5px;
        }

        .pt-local-guidance {
          margin-top: 4px;
          font-size: 10.5px;
          line-height: 1.25;
        }

        .pt-local-guidance-rule {
          margin-top: 2px;
          color: #666;
          font-size: 10px;
          line-height: 1.2;
        }

        .pt-local-row {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 5px;
          margin: 4px 0;
        }

        .pt-local-grid-2 {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 5px 8px;
          align-items: center;
        }

        .pt-local-small-label {
          display: block;
          font-weight: 700;
          font-size: 11px;
          margin-top: 4px;
        }

        .pt-local-input,
        .pt-local-select {
          width: 100%;
          box-sizing: border-box;
          font-size: 12px;
          margin: 2px 0 5px 0;
          border: 1px solid #aaa;
          border-radius: 3px;
          padding: 4px;
          font-family: Arial, Helvetica, sans-serif;
        }

        .pt-local-compact-select {
          margin-bottom: 2px;
        }

        .pt-local-warning,
        .pt-local-upload-legend-warning {
          color: #8B3E00;
          font-weight: 700;
        }

        .pt-local-range {
          width: 100%;
        }

        .pt-local-btn,
        .pt-local-mini-btn {
          border: 1px solid rgba(98, 117, 74, 0.80);
          background: rgba(255, 255, 255, 0.92);
          border-radius: 4px;
          cursor: pointer;
          font-size: 12px;
          appearance: none;
          -webkit-appearance: none;
          box-shadow: 0 1px 2px rgba(0,0,0,0.12);
          transition: background-color 0.10s ease, border-color 0.10s ease, box-shadow 0.10s ease, transform 0.05s ease;
        }

        .pt-local-btn {
          padding: 4px 7px;
        }

        .pt-local-mini-btn {
          padding: 2px 6px;
        }

        .pt-local-btn:hover,
        .pt-local-mini-btn:hover,
        .pt-local-btn:focus,
        .pt-local-mini-btn:focus {
          background: rgba(221, 238, 204, 0.98);
          border-color: rgba(66, 102, 47, 0.95);
          box-shadow: 0 1px 5px rgba(0,0,0,0.24);
          outline: none;
        }

        .pt-local-btn:active,
        .pt-local-mini-btn:active {
          background: rgba(199, 223, 181, 0.98);
          box-shadow: inset 0 1px 3px rgba(0,0,0,0.22);
          transform: translateY(1px);
        }

        .pt-local-layer-row {
          border-top: 1px solid rgba(151, 126, 58, 0.20);
          padding-top: 6px;
          margin-top: 6px;
        }

        .pt-local-layer-row:first-child {
          border-top: none;
          margin-top: 0;
          padding-top: 0;
        }

        .pt-local-layer-head {
          display: grid;
          grid-template-columns: auto minmax(0, 1fr) auto;
          align-items: center;
          gap: 5px;
        }

        .pt-local-layer-name {
          min-width: 0;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }

        .pt-local-visibility {
          display: inline-flex;
          align-items: center;
          cursor: pointer;
        }

        .pt-local-visibility input {
          margin: 0 2px 0 0;
        }

        .pt-local-layer-actions {
          display: inline-flex;
          align-items: center;
          gap: 3px;
        }

        .pt-local-icon-btn {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          width: 24px;
          height: 23px;
          padding: 0;
          border: 1px solid rgba(98, 117, 74, 0.80);
          border-radius: 4px;
          background: rgba(255, 255, 255, 0.92);
          color: #222;
          cursor: pointer;
          font: 16px/1 Arial, Helvetica, sans-serif;
        }

        .pt-local-icon-btn:hover,
        .pt-local-icon-btn:focus-visible {
          background: rgba(221, 238, 204, 0.98);
          border-color: rgba(66, 102, 47, 0.95);
          outline: 2px solid #255E9B;
          outline-offset: 1px;
        }

        .pt-local-icon-btn:disabled {
          cursor: not-allowed;
          opacity: 0.42;
        }

        .pt-local-style-icon {
          color: #222;
          font-weight: 700;
          opacity: 1;
        }

        .pt-local-remove-icon {
          color: #7A1F1F;
          font-size: 18px;
        }

        .pt-local-upload-body .pt-local-heading,
        .pt-local-upload-body .pt-local-muted,
        .pt-local-upload-body .pt-local-guidance,
        .pt-local-upload-body .pt-local-guidance-rule,
        .pt-local-upload-body .pt-local-status,
        .pt-local-upload-body .pt-local-layer-name {
          cursor: text;
          user-select: text;
          -webkit-user-select: text;
        }

        .pt-local-interaction-row {
          display: flex;
          flex-wrap: wrap;
          align-items: center;
          gap: 4px 9px;
          margin-top: 5px;
        }

        .pt-local-switch {
          display: inline-flex;
          align-items: center;
          gap: 4px;
          font-size: 11px;
          font-weight: 700;
          cursor: pointer;
          user-select: none;
        }

        .pt-local-switch input {
          position: absolute;
          width: 1px;
          height: 1px;
          overflow: hidden;
          clip: rect(0 0 0 0);
          clip-path: inset(50%);
          white-space: nowrap;
        }

        .pt-local-switch-track {
          position: relative;
          width: 26px;
          height: 14px;
          box-sizing: border-box;
          border: 1px solid #777;
          border-radius: 999px;
          background: #ddd;
          transition: background-color 0.12s ease, border-color 0.12s ease;
        }

        .pt-local-switch-track::after {
          content: '';
          position: absolute;
          top: 1px;
          left: 1px;
          width: 10px;
          height: 10px;
          border-radius: 50%;
          background: #fff;
          box-shadow: 0 1px 2px rgba(0,0,0,0.35);
          transition: transform 0.12s ease;
        }

        .pt-local-switch input:checked + .pt-local-switch-track {
          border-color: #356629;
          background: #4D8C3D;
        }

        .pt-local-switch input:checked + .pt-local-switch-track::after {
          transform: translateX(12px);
        }

        .pt-local-switch input:focus-visible + .pt-local-switch-track {
          outline: 2px solid #255E9B;
          outline-offset: 2px;
        }

        .pt-local-switch-state {
          min-width: 18px;
          color: #555;
          font-size: 10px;
          font-weight: 400;
        }

        .pt-local-hover-field {
          display: inline-flex;
          align-items: center;
          gap: 4px;
          min-width: 0;
          flex: 1 1 145px;
          font-size: 11px;
          font-weight: 700;
        }

        .pt-local-hover-field[hidden] {
          display: none;
        }

        .pt-local-hover-field select {
          min-width: 0;
          width: 100%;
          box-sizing: border-box;
          border: 1px solid #aaa;
          border-radius: 3px;
          padding: 2px 3px;
          background: #fff;
          font: 11px Arial, Helvetica, sans-serif;
        }

        .pt-local-hover-field select:focus-visible,
        .pt-local-visibility input:focus-visible {
          outline: 2px solid #255E9B;
          outline-offset: 1px;
        }

        .pt-local-swatch {
          display: inline-block;
          width: 11px;
          height: 11px;
          margin: 0 4px 0 2px;
          border: 2px solid #333;
          vertical-align: -1px;
        }

        .pt-local-upload-tooltip-wrap {
          background: rgba(255,255,255,0.96);
          border: 1px solid rgba(0,0,0,0.35);
          border-radius: 4px;
          box-shadow: 0 1px 5px rgba(0,0,0,0.22);
          color: #222;
          font: 12px/1.25 Arial, sans-serif;
          padding: 4px 6px;
        }

        .pt-local-upload-tooltip span {
          color: #555;
        }

        .pt-local-upload-legend {
          width: 280px;
          max-width: calc(100vw - 24px);
          box-sizing: border-box;
          padding: 7px 8px;
          background: var(--pt-local-upload-background);
          border: 1px solid rgba(0,0,0,0.28);
          border-radius: 5px;
          box-shadow: 0 1px 6px rgba(0,0,0,0.24);
          color: #222;
          font: 11px/1.25 Arial, Helvetica, sans-serif;
          pointer-events: auto;
        }

        .pt-local-upload-legend-head {
          display: flex;
          align-items: flex-start;
          justify-content: space-between;
          gap: 7px;
          margin-bottom: 2px;
        }

        .pt-local-upload-legend-title {
          min-width: 0;
          overflow-wrap: anywhere;
          font-weight: 700;
        }

        .pt-local-upload-legend-field {
          color: #444;
          font-weight: 700;
          overflow-wrap: anywhere;
          margin-bottom: 4px;
        }

        .pt-local-upload-legend-body {
          max-height: 42vh;
          overflow-y: auto;
          overscroll-behavior: contain;
          padding-right: 2px;
        }

        .pt-local-upload-legend-row {
          display: grid;
          grid-template-columns: 13px minmax(0, 1fr) auto;
          align-items: center;
          gap: 5px;
          margin: 2px 0;
        }

        .pt-local-upload-legend-swatch {
          display: inline-block;
          width: 12px;
          height: 12px;
          border: 1px solid rgba(0,0,0,0.38);
          box-sizing: border-box;
        }

        .pt-local-upload-legend-label {
          min-width: 0;
          overflow-wrap: anywhere;
        }

        .pt-local-upload-legend-count {
          color: #666;
          padding-left: 3px;
        }

        .pt-local-upload-legend-note,
        .pt-local-upload-legend-warning {
          border-top: 1px solid #ddd;
          margin-top: 4px;
          padding-top: 4px;
        }

        @media (max-width: 900px) {
          .pt-local-upload-wrap {
            bottom: 56px;
            width: 330px;
          }
        }
      `;
      document.head.appendChild(style);
    }

    var wrap = document.createElement('div');
    wrap.id = 'pt-local-upload-wrap';
    wrap.className = 'pt-local-upload-wrap pt-local-collapsed';

    wrap.innerHTML =
      '<div class="pt-local-upload-tab" id="pt-local-upload-tab">' +
        '<span class="pt-local-upload-title">Local GIS Uploads <span id="pt-local-upload-caret">▸</span></span>' +
        '<button type="button" id="pt-local-clear-ribbon-btn" class="pt-local-ribbon-clear" title="Clear uploaded local GIS files">Clear uploads</button>' +
      '</div>' +
      '<div class="pt-local-upload-body">' +
        '<div class="pt-local-section">' +
          '<div class="pt-local-heading">Upload local GIS file</div>' +
          '<input type="file" id="pt-local-file-input" class="pt-local-input" accept=".zip,.geojson,.json"/>' +
          '<div class="pt-local-muted">Supports zipped shapefiles and GeoJSON. Local uploads are temporary and are not saved into BRIM.</div>' +
          '<div class="pt-local-muted pt-local-guidance"><b>Upload guidance:</b> Use WGS 84 (EPSG:4326); shapefile ZIPs should include a valid .prj. BRIM cannot independently guarantee source CRS correctness or alignment. Missing or incorrect CRS information may produce plausible-looking but misaligned data. Large or highly detailed layers may slow the map.</div>' +
          '<div class="pt-local-guidance-rule">NAD83–WGS84 differences are commonly about 1–2 m; NAD27 shifts are often 10–100 m or more. Always verify alignment against known features.</div>' +
          '<div class="pt-local-muted">Limit: up to 3 active local layers; current file-size limit ' + PT2_LOCAL_MAX_FILE_MB + ' MB.</div>' +
          '<div id="pt-local-upload-status" class="pt-local-status"></div>' +
        '</div>' +

        '<div class="pt-local-section">' +
          '<div class="pt-local-heading">Active local files</div>' +
          '<div id="pt-local-layer-list"></div>' +
          '<div class="pt-local-muted">Use Clear uploads in the ribbon to remove all uploaded files.</div>' +
        '</div>' +

        '<div class="pt-local-section">' +
          '<div class="pt-local-heading">Style selected local layer</div>' +
          '<label class="pt-local-small-label">Layer</label>' +
          '<select id="pt-local-style-target" class="pt-local-select"></select>' +
          '<div id="pt-local-style-controls" style="display:none;">' +
            '<label class="pt-local-small-label" for="pt-local-style-mode">Style by</label>' +
            '<select id="pt-local-style-mode" class="pt-local-select">' +
              '<option value="single">Single color</option>' +
              '<option value="numeric">Numeric bins</option>' +
              '<option value="categories">Categories</option>' +
            '</select>' +
            '<div id="pt-local-attribute-controls" style="display:none;">' +
              '<label class="pt-local-small-label" for="pt-local-style-field">Style field</label>' +
              '<select id="pt-local-style-field" class="pt-local-select"></select>' +
              '<div id="pt-local-numeric-controls" style="display:none;">' +
                '<div class="pt-local-grid-2">' +
                  '<label>Classes<select id="pt-local-class-count" class="pt-local-select pt-local-compact-select">' +
                    '<option value="3">3</option><option value="4">4</option><option value="5">5</option><option value="6">6</option>' +
                    '<option value="7">7</option><option value="8">8</option><option value="9">9</option><option value="10">10</option>' +
                  '</select></label>' +
                  '<label>Method<select id="pt-local-class-method" class="pt-local-select pt-local-compact-select">' +
                    '<option value="quantile">Quantile</option>' +
                    '<option value="equal">Equal interval</option>' +
                  '</select></label>' +
                '</div>' +
                '<label class="pt-local-small-label" for="pt-local-palette">Color ramp</label>' +
                '<select id="pt-local-palette" class="pt-local-select"></select>' +
                '<label class="pt-local-check"><input type="checkbox" id="pt-local-reverse-palette"/> reverse ramp</label>' +
              '</div>' +
              '<div id="pt-local-style-note" class="pt-local-muted"></div>' +
            '</div>' +
            '<div id="pt-local-single-color-controls" class="pt-local-grid-2">' +
              '<label>Outline<br/><input type="color" id="pt-local-stroke-color" value="#7B3294"/></label>' +
              '<label>Fill<br/><input type="color" id="pt-local-fill-color" value="#7B3294"/></label>' +
            '</div>' +
            '<div class="pt-local-grid-2">' +
              '<label>Fill alpha<br/><input type="range" id="pt-local-fill-opacity" class="pt-local-range" min="0" max="0.9" step="0.05" value="0.18"/></label>' +
              '<label>Line alpha<br/><input type="range" id="pt-local-stroke-opacity" class="pt-local-range" min="0.1" max="1" step="0.05" value="0.95"/></label>' +
              '<label>Line width<br/><input type="range" id="pt-local-weight" class="pt-local-range" min="1" max="8" step="0.5" value="2"/></label>' +
              '<label>Point size<br/><input type="range" id="pt-local-point-radius" class="pt-local-range" min="2" max="12" step="1" value="5"/></label>' +
            '</div>' +
          '</div>' +
        '</div>' +
      '</div>';

    mapContainer.appendChild(wrap);

    L.DomEvent.disableClickPropagation(wrap);
    L.DomEvent.disableScrollPropagation(wrap);

    var selectableTextSelector = [
      '.pt-local-heading',
      '.pt-local-muted',
      '.pt-local-guidance',
      '.pt-local-guidance-rule',
      '.pt-local-status',
      '.pt-local-layer-name'
    ].join(',');

    wrap.addEventListener('pointerdown', function(e) {
      if (e.target && e.target.closest && e.target.closest(selectableTextSelector)) {
        e.stopPropagation();
      }
    });

    var tab = document.getElementById('pt-local-upload-tab');
    var caret = document.getElementById('pt-local-upload-caret');

    if (tab) {
      tab.addEventListener('click', function(e) {
        e.preventDefault();
        e.stopPropagation();
        wrap.classList.toggle('pt-local-collapsed');
        if (caret) {
          caret.textContent = wrap.classList.contains('pt-local-collapsed') ? '▸' : '▾';
        }
      });
    }

    var clearRibbonBtn = document.getElementById('pt-local-clear-ribbon-btn');
    if (clearRibbonBtn) {
      clearRibbonBtn.addEventListener('click', function(e) {
        e.preventDefault();
        e.stopPropagation();
        ptClearLocalLayers();
      });
    }

    var fileInput = document.getElementById('pt-local-file-input');
    if (fileInput) {
      fileInput.addEventListener('change', function(e) {
        var file = e.target && e.target.files && e.target.files.length ? e.target.files[0] : null;
        ptHandleLocalFile(file);
        fileInput.value = '';
      });
    }

    var clearBtn = document.getElementById('pt-local-clear-btn');
    if (clearBtn) {
      clearBtn.addEventListener('click', ptClearLocalLayers);
    }

    var styleTarget = document.getElementById('pt-local-style-target');
    if (styleTarget) {
      styleTarget.addEventListener('change', function() {
        ptRenderLocalStylePanel(styleTarget.value);
      });
    }

    ['pt-local-style-mode', 'pt-local-style-field', 'pt-local-class-count',
     'pt-local-class-method', 'pt-local-palette', 'pt-local-reverse-palette',
     'pt-local-stroke-color', 'pt-local-fill-color', 'pt-local-fill-opacity',
     'pt-local-stroke-opacity', 'pt-local-weight', 'pt-local-point-radius'].forEach(function(id) {
      var input = document.getElementById(id);
      if (input) {
        input.addEventListener('input', ptApplyLocalStyleFromControls);
        input.addEventListener('change', ptApplyLocalStyleFromControls);
      }
    });

    wrap.addEventListener('change', function(e) {
      var target = e.target;
      if (!target) return;

      var hoverId = target.getAttribute('data-pt-local-hover');
      if (hoverId) {
        ptSetLocalHover(hoverId, target.checked);
        return;
      }

      var popupId = target.getAttribute('data-pt-local-popup');
      if (popupId) {
        ptSetLocalPopup(popupId, target.checked);
        return;
      }

      var hoverFieldId = target.getAttribute('data-pt-local-hover-field');
      if (hoverFieldId) {
        ptSetLocalHoverField(hoverFieldId, target.value);
      }
    });

    wrap.addEventListener('input', function(e) {
      var target = e.target;
      if (!target) return;

      var hoverFieldId = target.getAttribute('data-pt-local-hover-field');
      if (hoverFieldId) {
        ptSetLocalHoverField(hoverFieldId, target.value);
      }
    });

    wrap.addEventListener('click', function(e) {
      var target = e.target;
      if (!target) return;

      var toggleId = target.getAttribute('data-pt-local-toggle');
      if (toggleId) {
        ptToggleLocalLayer(toggleId, target.checked);
        return;
      }

      var styleId = target.getAttribute('data-pt-local-style');
      if (styleId) {
        ptRenderLocalStylePanel(styleId);
        var styleRec = ptLocalLayerById(styleId);
        var styleBlock = document.getElementById('pt-local-style-controls');
        if (styleBlock && styleBlock.scrollIntoView) {
          styleBlock.scrollIntoView({block: 'nearest'});
        }
        if (styleRec) {
          ptSetLocalStatus('Style controls shown for ' + styleRec.name + '.', false);
        }
        return;
      }

      var zoomId = target.getAttribute('data-pt-local-zoom');
      if (zoomId) {
        ptZoomToLocalLayer(zoomId);
        return;
      }

      var removeId = target.getAttribute('data-pt-local-remove');
      if (removeId) {
        ptRemoveLocalLayer(removeId);
        return;
      }
    });

    ptRenderLocalLayerList();
    ptRenderLocalStylePanel('');
  }

  buildLocalUploadPanel();
}
)---"
  
  htmlwidgets::onRender(m, js)
}
