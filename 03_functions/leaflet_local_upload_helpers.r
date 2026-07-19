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
  
  js <- r"---(
function(el, x) {

  var map = this;

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
    var seen = {};

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
    var seen = {};
    var out = [];

    features.slice(0, 500).forEach(function(f) {
      var props = f && f.properties ? f.properties : {};
      Object.keys(props).forEach(function(k) {
        if (!seen[k]) {
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

  function ptAutoLabelField(fields) {
    var preferred = [
      'name', 'Name', 'NAME',
      'site_name', 'SITE_NAME', 'station_nm', 'STATION_NM',
      'id', 'ID', 'objectid', 'OBJECTID', 'fid', 'FID'
    ];

    for (var i = 0; i < preferred.length; i++) {
      if (fields.indexOf(preferred[i]) >= 0) return preferred[i];
    }

    return fields.length ? fields[0] : '';
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

  function ptPathStyleForFeature(rec, feature) {
    rec = rec || {};
    var style = rec.style || ptDefaultStyle();
    var geomType = feature && feature.geometry ? ptBaseGeomType(feature.geometry.type) : 'Unknown';

    var pane = 'pane_pt_local_polygon';
    if (geomType === 'Line') pane = 'pane_pt_local_line';

    return {
      pane: pane,
      color: style.strokeColor,
      weight: Number(style.weight) || 2,
      opacity: Number(style.strokeOpacity),
      fillColor: style.fillColor,
      fillOpacity: geomType === 'Polygon' ? Number(style.fillOpacity) : 0,
      interactive: true
    };
  }

  function ptPointStyle(rec) {
    rec = rec || {};
    var style = rec.style || ptDefaultStyle();

    return {
      pane: 'pane_pt_local_point',
      radius: Number(style.pointRadius) || 5,
      color: style.strokeColor,
      weight: Number(style.weight) || 2,
      opacity: Number(style.strokeOpacity),
      fillColor: style.fillColor,
      fillOpacity: Math.max(0.25, Number(style.fillOpacity)),
      interactive: true
    };
  }

  function ptFeatureTooltipHtml(feature, rec) {
    var props = feature && feature.properties ? feature.properties : {};
    var labelField = ptCleanText(rec.labelField);
    var labelValue = labelField ? ptValueForField(props, labelField) : '';

    var html = '<div class="pt-local-upload-tooltip"><b>' + ptEscapeHtml(rec.name) + '</b>';

    if (labelField && ptHasValue(labelValue)) {
      html += '<br/><span>' + ptEscapeHtml(labelField) + ':</span> ' + ptEscapeHtml(labelValue);
    }

    html += '</div>';
    return html;
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

  function ptBindLocalInteractions(layer, rec) {
    var feature = layer && layer.feature ? layer.feature : null;

    if (!feature) return;

    layer.unbindTooltip && layer.unbindTooltip();
    layer.unbindPopup && layer.unbindPopup();

    if (!rec || rec.hoverEnabled !== false) {
      layer.bindTooltip(ptFeatureTooltipHtml(feature, rec), {
        sticky: true,
        direction: 'auto',
        opacity: 0.95,
        className: 'pt-local-upload-tooltip-wrap'
      });
    }

    layer.bindPopup(ptFeaturePopupHtml(feature, rec), {
      maxWidth: 360
    });
  }

  function ptRefreshLocalInteractions(rec) {
    if (!rec || !rec.layer || !rec.layer.eachLayer) return;

    rec.layer.eachLayer(function(layer) {
      ptBindLocalInteractions(layer, rec);
    });
  }

  function ptRestyleLocalLayer(rec) {
    if (!rec || !rec.layer || !rec.layer.eachLayer) return;

    rec.layer.eachLayer(function(layer) {
      var feature = layer.feature || null;
      var geomType = feature && feature.geometry ? ptBaseGeomType(feature.geometry.type) : 'Unknown';

      if (geomType === 'Point' && layer.setRadius) {
        layer.setRadius(Number(rec.style.pointRadius) || 5);
        layer.setStyle(ptPointStyle(rec));
      } else if (layer.setStyle) {
        layer.setStyle(ptPathStyleForFeature(rec, feature));
      }
    });

    ptRenderLocalLayerList();
  }

  function ptCreateLeafletLayer(featureCollection, rec) {
    return L.geoJSON(featureCollection, {
      style: function(feature) {
        return ptPathStyleForFeature(rec, feature);
      },
      pointToLayer: function(feature, latlng) {
        return L.circleMarker(latlng, ptPointStyle(rec));
      },
      onEachFeature: function(feature, layer) {
        ptBindLocalInteractions(layer, rec);
      }
    });
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
    var geomType = ptGeomTypeLabel(featureCollection);
    var rec = {
      id: 'pt_local_' + ptLocalSeq,
      name: file && file.name ? file.name : ('Local layer ' + ptLocalSeq),
      sourceType: sourceType,
      fileSize: file && file.size ? file.size : 0,
      featureCount: features.length,
      geomType: geomType,
      fields: fields,
      labelField: ptAutoLabelField(fields),
      hoverEnabled: true,
      visible: true,
      style: ptDefaultStyle(),
      layer: null
    };

    rec.layer = ptCreateLeafletLayer(featureCollection, rec);
    rec.layer.addTo(map);

    ptLocalLayers.push(rec);
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
        if (rec.layer && map.hasLayer(rec.layer)) {
          map.removeLayer(rec.layer);
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
  }

  function ptZoomToLocalLayer(id) {
    var rec = ptLocalLayerById(id);
    if (!rec || !rec.layer || !rec.layer.getBounds) return;

    var bounds = rec.layer.getBounds();

    if (bounds && bounds.isValid && bounds.isValid()) {
      map.fitBounds(bounds.pad(0.06));
    } else {
      ptSetLocalStatus('Could not zoom to this layer. The layer may not have valid bounds.', true);
    }
  }

  function ptClearLocalLayers() {
    ptLocalLayers.forEach(function(rec) {
      if (rec.layer && map.hasLayer(rec.layer)) {
        map.removeLayer(rec.layer);
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
    var labelFieldSelect = document.getElementById('pt-local-label-field');
    var styleBlock = document.getElementById('pt-local-style-controls');

    if (!targetSelect || !labelFieldSelect || !styleBlock) return;

    var html = '<option value="">Choose active layer...</option>';
    ptLocalLayers.forEach(function(rec) {
      html += '<option value="' + ptEscapeHtml(rec.id) + '">' + ptEscapeHtml(ptShortName(rec.name, 44)) + '</option>';
    });

    targetSelect.innerHTML = html;

    if (selectedId && ptLocalLayerById(selectedId)) {
      targetSelect.value = selectedId;
    }

    var rec = ptLocalLayerById(targetSelect.value);

    if (!rec) {
      styleBlock.style.display = 'none';
      labelFieldSelect.innerHTML = '<option value="">No active layer</option>';
      return;
    }

    styleBlock.style.display = 'block';

    var fieldHtml = '<option value="">Layer name only</option>';
    rec.fields.forEach(function(field) {
      fieldHtml += '<option value="' + ptEscapeHtml(field) + '">' + ptEscapeHtml(field) + '</option>';
    });
    labelFieldSelect.innerHTML = fieldHtml;
    labelFieldSelect.value = rec.labelField || '';

    var strokeColor = document.getElementById('pt-local-stroke-color');
    var fillColor = document.getElementById('pt-local-fill-color');
    var fillOpacity = document.getElementById('pt-local-fill-opacity');
    var strokeOpacity = document.getElementById('pt-local-stroke-opacity');
    var weight = document.getElementById('pt-local-weight');
    var radius = document.getElementById('pt-local-point-radius');
    var hoverEnabled = document.getElementById('pt-local-hover-enabled');

    if (strokeColor) strokeColor.value = rec.style.strokeColor;
    if (fillColor) fillColor.value = rec.style.fillColor;
    if (fillOpacity) fillOpacity.value = rec.style.fillOpacity;
    if (strokeOpacity) strokeOpacity.value = rec.style.strokeOpacity;
    if (weight) weight.value = rec.style.weight;
    if (radius) radius.value = rec.style.pointRadius;
    if (hoverEnabled) hoverEnabled.checked = rec.hoverEnabled !== false;
  }

  function ptApplyLocalStyleFromControls() {
    var targetSelect = document.getElementById('pt-local-style-target');
    if (!targetSelect || !targetSelect.value) return;

    var rec = ptLocalLayerById(targetSelect.value);
    if (!rec) return;

    var strokeColor = document.getElementById('pt-local-stroke-color');
    var fillColor = document.getElementById('pt-local-fill-color');
    var fillOpacity = document.getElementById('pt-local-fill-opacity');
    var strokeOpacity = document.getElementById('pt-local-stroke-opacity');
    var weight = document.getElementById('pt-local-weight');
    var radius = document.getElementById('pt-local-point-radius');
    var labelField = document.getElementById('pt-local-label-field');
    var hoverEnabled = document.getElementById('pt-local-hover-enabled');

    if (strokeColor) rec.style.strokeColor = strokeColor.value;
    if (fillColor) rec.style.fillColor = fillColor.value;
    if (fillOpacity) rec.style.fillOpacity = Number(fillOpacity.value);
    if (strokeOpacity) rec.style.strokeOpacity = Number(strokeOpacity.value);
    if (weight) rec.style.weight = Number(weight.value);
    if (radius) rec.style.pointRadius = Number(radius.value);
    if (labelField) rec.labelField = labelField.value;
    if (hoverEnabled) rec.hoverEnabled = !!hoverEnabled.checked;

    ptRestyleLocalLayer(rec);
    ptRefreshLocalInteractions(rec);
  }

  // ------------------------------------------------------------------------
  // Render active local layer list
  // ------------------------------------------------------------------------

  function ptRenderLocalLayerList() {
    var list = document.getElementById('pt-local-layer-list');
    if (!list) return;

    if (ptLocalLayers.length === 0) {
      list.innerHTML = '<div class="pt-local-muted">No local files uploaded.</div>';
      return;
    }

    var html = '';

    ptLocalLayers.forEach(function(rec) {
      html +=
        '<div class="pt-local-layer-row">' +
          '<label title="' + ptEscapeHtml(rec.name) + '">' +
            '<input type="checkbox" data-pt-local-toggle="' + rec.id + '" ' +
              (rec.visible ? 'checked' : '') + '/> ' +
            '<span class="pt-local-swatch" style="background:' + ptEscapeHtml(rec.style.fillColor) + ';border-color:' + ptEscapeHtml(rec.style.strokeColor) + ';"></span>' +
            '<b>' + ptEscapeHtml(ptShortName(rec.name, 34)) + '</b>' +
          '</label>' +
          '<div class="pt-local-muted">' +
            ptEscapeHtml(rec.geomType) + ' | ' +
            rec.featureCount.toLocaleString() + ' feature(s) | ' +
            ptEscapeHtml(ptFormatBytes(rec.fileSize)) +
            (rec.hoverEnabled === false ? ' | hover off' : '') +
          '</div>' +
          '<div class="pt-local-row">' +
            '<button type="button" class="pt-local-mini-btn" data-pt-local-style="' + rec.id + '">Style</button>' +
            '<button type="button" class="pt-local-mini-btn" data-pt-local-zoom="' + rec.id + '">Zoom</button>' +
            '<button type="button" class="pt-local-mini-btn" data-pt-local-remove="' + rec.id + '">Remove</button>' +
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
          background: rgba(255, 248, 218, 0.97);
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
          background: rgba(255, 248, 218, 0.97);
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
          '<div class="pt-local-muted">Supports zipped shapefiles and GeoJSON. Use WGS84 / EPSG:4326 lon/lat coordinates. Local uploads are temporary and are not saved into PT2.</div>' +
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
            '<label class="pt-local-small-label">Hover label field</label>' +
            '<select id="pt-local-label-field" class="pt-local-select"></select>' +
            '<label class="pt-local-check"><input type="checkbox" id="pt-local-hover-enabled" checked/> enable hover tooltip</label>' +
            '<div class="pt-local-grid-2">' +
              '<label>Outline<br/><input type="color" id="pt-local-stroke-color" value="#7B3294"/></label>' +
              '<label>Fill<br/><input type="color" id="pt-local-fill-color" value="#7B3294"/></label>' +
              '<label>Fill alpha<br/><input type="range" id="pt-local-fill-opacity" class="pt-local-range" min="0" max="0.9" step="0.05" value="0.18"/></label>' +
              '<label>Line alpha<br/><input type="range" id="pt-local-stroke-opacity" class="pt-local-range" min="0.1" max="1" step="0.05" value="0.95"/></label>' +
              '<label>Line width<br/><input type="range" id="pt-local-weight" class="pt-local-range" min="1" max="8" step="0.5" value="2"/></label>' +
              '<label>Point size<br/><input type="range" id="pt-local-point-radius" class="pt-local-range" min="2" max="12" step="1" value="5"/></label>' +
            '</div>' +
            '<div class="pt-local-muted">This first version uses constant layer styling. Attribute-based ramps/bins can be added next.</div>' +
          '</div>' +
        '</div>' +
      '</div>';

    mapContainer.appendChild(wrap);

    L.DomEvent.disableClickPropagation(wrap);
    L.DomEvent.disableScrollPropagation(wrap);

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

    ['pt-local-stroke-color', 'pt-local-fill-color', 'pt-local-fill-opacity',
     'pt-local-stroke-opacity', 'pt-local-weight', 'pt-local-point-radius',
     'pt-local-label-field', 'pt-local-hover-enabled'].forEach(function(id) {
      var input = document.getElementById(id);
      if (input) {
        input.addEventListener('input', ptApplyLocalStyleFromControls);
        input.addEventListener('change', ptApplyLocalStyleFromControls);
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
