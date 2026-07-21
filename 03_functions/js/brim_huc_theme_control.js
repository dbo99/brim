function(el, x, hucThemeData) {

  var map = this;
  var hucTheme = 'none';
  var visibleHucLayers = {};
  var hucLegendUserHidden = false;

  console.log('PT2 HUC theme control loading');

hucThemeData = hucThemeData || {};

/*
 * The R side passes HUC theme information as JSON-safe arrays of explicit
 * records rather than named vectors/lists. Rebuild the fast JavaScript lookup
 * objects here.
 */
var hucThemeLookup = {};
var hucThemeLookupRows = hucThemeData.lookup || [];

if (Array.isArray(hucThemeLookupRows)) {
  hucThemeLookupRows.forEach(function(row) {
    if (row && row.layer_id !== undefined && row.layer_id !== null) {
      hucThemeLookup[String(row.layer_id)] = row;
    }
  });
} else {
  /*
   * Backward-compatible fallback in case an older HTML/data payload is used.
   */
  hucThemeLookup = hucThemeLookupRows || {};
}

var hucThemeLegends = {};
var hucThemeLegendRows = hucThemeData.legends || [];

if (Array.isArray(hucThemeLegendRows)) {
  hucThemeLegendRows.forEach(function(row) {
    if (!row || !row.huc_layer || !row.theme) return;
    if (!hucThemeLegends[row.huc_layer]) {
      hucThemeLegends[row.huc_layer] = {};
    }
    hucThemeLegends[row.huc_layer][row.theme] = row.legend || null;
  });
} else {
  /*
   * Backward-compatible fallback in case an older HTML/data payload is used.
   */
  hucThemeLegends = hucThemeLegendRows || {};
}

console.log('PT2 HUC theme lookup keys:', Object.keys(hucThemeLookup).length);
console.log('PT2 HUC theme legend levels:', Object.keys(hucThemeLegends));

  function getMapContainerForHucControls() {
    return map.getContainer ? map.getContainer() : el.querySelector('.leaflet-container');
  }

  function addFloatingHucThemeControl() {

    /*
     * Use an absolute in-map control rather than the normal Leaflet top-left
     * control stack.  This gives PT2 stable placement for the HUC fill dropdown
     * after the HUC legend was moved out of the lower-left corner.
     */
    var mapContainer = getMapContainerForHucControls();

    if (!mapContainer) {
      console.warn('PT2 HUC fill control: map container not found.');
      return;
    }

    var oldControl = mapContainer.querySelector('.pt-huc-theme-control');
    if (oldControl) {
      oldControl.remove();
    }

    var div = document.createElement('div');
    div.className = 'pt-huc-theme-control leaflet-control';

    div.style.position = 'absolute';
    div.style.top = '198px';
    div.style.left = '8px';
    div.style.zIndex = '10010';
    div.style.background = 'rgba(255, 255, 255, 0.96)';
    div.style.padding = '6px';
    div.style.border = '1px solid #777';
    div.style.borderRadius = '4px';
    div.style.boxShadow = '0 1px 4px rgba(0,0,0,0.35)';
    div.style.font = '12px Arial, sans-serif';
    div.style.boxSizing = 'border-box';
    div.style.display = 'none';

    div.innerHTML =
      `<label style='font-weight:bold;display:block;margin-bottom:3px;'>HUC fill</label>` +
      `<select id='pt-huc-theme-select' style='font-size:12px;max-width:210px;'>` +
        `<option value='none'>None / boundaries only</option>` +
        `<option value='ppt_in'>PRISM precip - in/yr</option>` +
        `<option value='ppt_kaf'>PRISM precip - kaf/yr</option>` +
        `<option value='rech_in'>BCMv8 recharge - in/yr</option>` +
        `<option value='rech_kaf'>BCMv8 recharge - kaf/yr</option>` +
      `</select>` +
      `<div id='pt-huc-theme-status' aria-live='polite' ` +
        `style='display:none;margin-top:3px;font-size:10px;line-height:1.15;color:#24527a;'>` +
      `</div>`;

    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);

    mapContainer.appendChild(div);
  }

  function addFloatingHucLegend() {

    /*
     * The HUC legend is a floating information card, not a Leaflet corner
     * control.  It is tucked into the upper-left map pocket to free the lower
     * left for the Local GIS Uploads panel.  It is offset far enough right
     * to avoid the top-left teaching/markup controls without pushing the legend unnecessarily far into the map on cramped screens.
     */
    var mapContainer = getMapContainerForHucControls();

    if (!mapContainer) {
      console.warn('PT2 HUC legend: map container not found.');
      return;
    }

    var oldLegend = mapContainer.querySelector('.pt-huc-theme-legend');
    if (oldLegend) {
      oldLegend.remove();
    }

    var div = document.createElement('div');
    div.className = 'pt-huc-theme-legend leaflet-control';

    div.style.position = 'absolute';
    div.style.top = '12px';
    div.style.left = '126px';
    div.style.zIndex = '10010';
    div.style.background = 'rgba(255, 255, 255, 0.94)';
    div.style.padding = '6px';
    div.style.border = '1px solid #777';
    div.style.borderRadius = '4px';
    div.style.boxShadow = '0 1px 4px rgba(0,0,0,0.35)';
    div.style.font = '12px Arial, sans-serif';
    div.style.width = '300px';
    div.style.maxWidth = '300px';
    div.style.maxHeight = '220px';
    div.style.overflowY = 'auto';
    div.style.display = 'none';
    div.style.boxSizing = 'border-box';

    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);

    mapContainer.appendChild(div);
  }

  addFloatingHucThemeControl();
  addFloatingHucLegend();

  function getLayerId(layer) {
    if (!layer || !layer.options) return null;

    if (layer.options.layerId !== undefined && layer.options.layerId !== null) {
      return String(layer.options.layerId);
    }

    if (layer.options.layer_id !== undefined && layer.options.layer_id !== null) {
      return String(layer.options.layer_id);
    }

    return null;
  }

  function isHucLayer(layer) {
    var id = getLayerId(layer);
    return (
      id !== null &&
      Object.prototype.hasOwnProperty.call(hucThemeLookup, id) &&
      typeof layer.setStyle === 'function'
    );
  }

  function getFillColor(layer, theme) {
    var id = getLayerId(layer);
    var rec = hucThemeLookup[id];

    if (!rec) return '#FFFFFF';

    if (theme === 'ppt_in')   return rec.ppt_in   || '#FFFFFF';
    if (theme === 'ppt_kaf')  return rec.ppt_kaf  || '#FFFFFF';
    if (theme === 'rech_in')  return rec.rech_in  || '#FFFFFF';
    if (theme === 'rech_kaf') return rec.rech_kaf || '#FFFFFF';

    return '#FFFFFF';
  }

  function layerSortValue(layerName) {
    var n = parseInt(String(layerName).replace('huc', ''), 10);
    return isNaN(n) ? 999 : n;
  }

  function applyHucTheme() {

    var hucCount = 0;
    var polygonLikeCount = 0;
    var sampleIds = [];
    visibleHucLayers = {};

    function styleOneLayer(layer) {

      if (layer && typeof layer.setStyle === 'function') {
        polygonLikeCount = polygonLikeCount + 1;
        var sampleId = getLayerId(layer);
        if (sampleId !== null && sampleIds.length < 5) {
          sampleIds.push(sampleId);
        }
      }

      if (isHucLayer(layer)) {

        hucCount = hucCount + 1;

        var id = getLayerId(layer);
        var rec = hucThemeLookup[id];
        if (rec && rec.huc_layer) {
          visibleHucLayers[rec.huc_layer] = true;
        }

        if (hucTheme === 'none') {
          layer.setStyle({
            fillColor: '#FFFFFF',
            fillOpacity: 0
          });
        } else {
          layer.setStyle({
            fillColor: getFillColor(layer, hucTheme),
            fillOpacity: 0.58
          });
        }

        return;
      }

      if (layer && typeof layer.eachLayer === 'function') {
        layer.eachLayer(function(childLayer) {
          styleOneLayer(childLayer);
        });
      }
    }

    map.eachLayer(function(layer) {
      styleOneLayer(layer);
    });

    console.log(
      'PT2 HUC theme applied:',
      hucTheme,
      'HUC polygon layers styled:',
      hucCount,
      'visible HUC levels:',
      Object.keys(visibleHucLayers),
      'polygon-like layers seen:',
      polygonLikeCount,
      'sample layerIds:',
      sampleIds
    );

    updateLegend();
  }

  function legendRows(legend) {
    if (!legend) return '';

    var rows = legend.rows || [];
    var html = `<b>${legend.title || 'HUC legend'}</b><br/>`;

    if (rows.length === 0) {
      html += `<div>No mapped values for this HUC level.</div>`;
    }

    var useTwoCols = rows.length >= 9;

    if (useTwoCols) {
      html += `<div style='display:grid;grid-template-columns:1fr 1fr;column-gap:8px;row-gap:1px;margin-top:2px;'>`;
    }

    for (var i = 0; i < rows.length; i++) {
      html +=
        `<div style='white-space:nowrap;min-width:0;'>` +
        `<span style='display:inline-block;width:12px;height:12px;margin-right:4px;border:1px solid #777;background:${rows[i].color};vertical-align:-1px;'></span>` +
        `<span style='font-size:11px;'>${rows[i].label}</span>` +
        `</div>`;
    }

    if (useTwoCols) {
      html += `</div>`;
    }

    if (legend.note) {
      html += `<div style='font-size:10px;margin-top:3px;color:#444;'>${legend.note}</div>`;
    }

    return html;
  }

  function legendHtml(theme) {
    if (theme === 'none') return '';

    var levels = Object.keys(visibleHucLayers).sort(function(a, b) {
      return layerSortValue(a) - layerSortValue(b);
    });

    if (levels.length === 0) {
      return `<b>HUC fill</b><br/>Turn on a HUC layer to see the legend.`;
    }

    var html = '';

    if (levels.length > 1) {
      html += `<div style='font-size:10px;margin-bottom:4px;color:#444;'>` +
        `Multiple HUC levels are visible. Colors are scaled separately by HUC level.` +
        `</div>`;
    }

    for (var i = 0; i < levels.length; i++) {
      var lvl = levels[i];
      var legend = hucThemeLegends[lvl] ? hucThemeLegends[lvl][theme] : null;

      if (i > 0) {
        html += `<hr style='border:none;border-top:1px solid #ccc;margin:6px 0;'/>`;
      }

      html += legendRows(legend);
    }

    return html;
  }

  function updateHucThemeControlDisplay() {
    var ctl = document.querySelector('.pt-huc-theme-control');
    if (!ctl) return;

    var hasVisibleHuc = Object.keys(visibleHucLayers || {}).length > 0;
    ctl.style.display = hasVisibleHuc ? 'block' : 'none';
  }

  function updateLegend() {
    var div = document.querySelector('.pt-huc-theme-legend');

    updateHucThemeControlDisplay();

    if (!div) return;

    var hasVisibleHuc = Object.keys(visibleHucLayers || {}).length > 0;
    if (!hasVisibleHuc || hucTheme === 'none') hucLegendUserHidden = false;
    var html = legendHtml(hucTheme);
    div.style.display = (!hasVisibleHuc || hucTheme === 'none' || html === '' || hucLegendUserHidden) ? 'none' : 'block';
    div.innerHTML = '<div style="display:flex;justify-content:space-between;align-items:flex-start;gap:8px;margin-bottom:3px;"><span style="font-weight:700;">HUC thematic fill</span><button type="button" class="pt-huc-theme-legend-close" aria-label="Hide HUC thematic-fill legend" title="Hide HUC thematic-fill legend" style="border:0;background:transparent;color:#666;font-size:17px;line-height:1;cursor:pointer;padding:0 2px;">&times;</button></div>' + html;
    var close = div.querySelector('.pt-huc-theme-legend-close');
    if (close) close.addEventListener('click', function(e) {
      e.preventDefault();
      e.stopPropagation();
      hucLegendUserHidden = true;
      div.style.display = 'none';
    }, false);
  }

  var hucThemeStatusTimer = null;

  function setHucThemeStatus(message, autoHideMs) {
    var status = document.getElementById('pt-huc-theme-status');
    if (!status) return;

    if (hucThemeStatusTimer !== null) {
      window.clearTimeout(hucThemeStatusTimer);
      hucThemeStatusTimer = null;
    }

    if (!message) {
      status.textContent = '';
      status.style.display = 'none';
      return;
    }

    status.textContent = message;
    status.style.display = 'block';

    if (autoHideMs && autoHideMs > 0) {
      hucThemeStatusTimer = window.setTimeout(function() {
        status.textContent = '';
        status.style.display = 'none';
        hucThemeStatusTimer = null;
      }, autoHideMs);
    }
  }

  function deferHucThemeApply() {
    var runApply = function() {
      window.setTimeout(function() {
        applyHucTheme();
        setHucThemeStatus('HUC fill applied.', 900);
      }, 20);
    };

    if (window.requestAnimationFrame) {
      window.requestAnimationFrame(runApply);
    } else {
      runApply();
    }
  }

  var select = document.getElementById('pt-huc-theme-select');

  if (select) {
    select.addEventListener('mousedown', function() {
      setHucThemeStatus('Selecting HUC fill…');
    });

    select.addEventListener('focus', function() {
      setHucThemeStatus('Selecting HUC fill…');
    });

    select.addEventListener('keydown', function(e) {
      if (!e || e.key !== 'Escape') {
        setHucThemeStatus('Selecting HUC fill…');
      }
    });

    select.addEventListener('blur', function() {
      window.setTimeout(function() {
        var status = document.getElementById('pt-huc-theme-status');
        if (status && status.textContent === 'Selecting HUC fill…') {
          setHucThemeStatus('', 0);
        }
      }, 250);
    });

    select.addEventListener('change', function(e) {
      hucTheme = e.target.value;
      setHucThemeStatus(hucTheme === 'none' ? 'Returning to boundaries only…' : 'Applying HUC fill…');
      deferHucThemeApply();
    });
  } else {
    console.warn('PT2 HUC theme select was not found after control creation.');
  }

  function visibleHucCount() {
    return Object.keys(visibleHucLayers || {}).length;
  }

  function resetHucThemeToBoundariesOnly() {
    if (hucTheme !== 'none') {
      hucTheme = 'none';
    }

    var ctlSelect = document.getElementById('pt-huc-theme-select');
    if (ctlSelect && ctlSelect.value !== 'none') {
      ctlSelect.value = 'none';
    }
  }

  map.on('overlayadd', function(e) {
    var beforeCount = visibleHucCount();
    applyHucTheme();
    var afterCount = visibleHucCount();

    /*
     * If the user turns HUCs completely off and later opens a HUC layer again,
     * start that new HUC view as boundaries-only.  This prevents a previously
     * selected in/yr or kaf/yr fill from unexpectedly carrying into the next
     * HUC level the user opens.
     */
    if (beforeCount === 0 && afterCount > 0) {
      resetHucThemeToBoundariesOnly();
      applyHucTheme();
    }
  });

  map.on('overlayremove', function(e) {
    applyHucTheme();

    if (visibleHucCount() === 0) {
      resetHucThemeToBoundariesOnly();
      updateLegend();
    }
  });

  applyHucTheme();
  console.log('PT2 HUC theme control loaded');
}
