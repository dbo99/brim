# ==== leaflet_core_helpers.r =================================================
##
## PURPOSE:
##   Shared Leaflet helper functions for PortaTreasure2 map-building.
##
## DESIGN:
##   The final map runner should stay short. Repeated Leaflet setup tasks,
##   such as panes, basemaps, and basic layer-control handling, live here.
##

# ==== 1. Initialize base map =================================================

pt_init_map <- function(map_display) {
  
  leaflet::leaflet(
    options = leaflet::leafletOptions(
      preferCanvas         = TRUE,
      zoomSnap             = map_display$zoom_snap,
      zoomDelta            = map_display$zoom_delta,
      wheelPxPerZoomLevel  = map_display$wheel_px_per_zoom_level
    )
  ) |>
    leaflet::setView(
      lng  = map_display$default_lng,
      lat  = map_display$default_lat,
      zoom = map_display$default_zoom
    )
}

# ==== 2. Add map panes =======================================================
##
## Lower zIndex values are drawn underneath higher zIndex values.

pt_add_panes <- function(m) {
  
  m <- m |>
    leaflet::addMapPane("pane_blm",        zIndex = 300) |>
    leaflet::addMapPane("pane_blm_diff",   zIndex = 320) |>
    leaflet::addMapPane("pane_county",     zIndex = 380) |>
    leaflet::addMapPane("pane_gw",         zIndex = 390) |>
    leaflet::addMapPane("pane_huc",        zIndex = 430) |>
    leaflet::addMapPane("pane_cnrfc_basins", zIndex = 455) |>
    leaflet::addMapPane("pane_lines",       zIndex = 480) |>
    ## Browser-managed conveyance uses a dedicated canvas pane so it can be
    ## hidden during zoom/pan while basemap tiles paint.
    leaflet::addMapPane("pane_conveyance",  zIndex = 485) |>
    leaflet::addMapPane("pane_points",      zIndex = 520) |>
    ## Live operational raster/image overlays.
    ## Kept above normal polygons/points and below labels/offices.
    leaflet::addMapPane("pane_ops",         zIndex = 560) |>
    ## Delta Ops uses browser-side vector/canvas layers.  Give it dedicated
    ## panes so its renderer cannot leave an invisible high-z canvas over
    ## local polygon layers such as Bulletin 118 after the layer is toggled off.
    leaflet::addMapPane("pane_ops_delta_points", zIndex = 565) |>
    leaflet::addMapPane("pane_ops_delta_labels", zIndex = 570) |>
    leaflet::addMapPane("pane_labels_poly", zIndex = 610) |>
    leaflet::addMapPane("pane_labels_pts",  zIndex = 620) |>
    leaflet::addMapPane("pane_office_do",   zIndex = 770) |>
    leaflet::addMapPane("pane_office_caso", zIndex = 780) |>
    leaflet::addMapPane("pane_office_fo",   zIndex = 790)
  
  ## Cursor fix for popup links -------------------------------------------
  ##
  ## Some PT2 popup links were displaying a text/default cursor instead of
  ## the normal hand pointer. This small global CSS rule affects only links
  ## inside Leaflet popups and their child elements.
  m <- htmlwidgets::prependContent(
    m,
    htmltools::tags$style(
      htmltools::HTML("
        .leaflet-popup-pane a,
        .leaflet-popup-pane a *,
        .leaflet-popup-content a,
        .leaflet-popup-content a * {
          cursor: pointer !important;
        }

        /*
         * Blank basemap safety.
         *
         * The \"No Basemap\" option should render as a clean white canvas on all
         * platforms.  Some browsers/GPU paths can show Leaflet's default gray
         * tile background or tile-edge artifacts when a 1x1 data tile is used.
         * Keep all underlying map/tile containers white so blank tiles, tile
         * gaps, and outside-world extents do not show a gray checkerboard.
         */
        .leaflet-container,
        .leaflet-map-pane,
        .leaflet-tile-pane {
          background: #ffffff !important;
          background-color: #ffffff !important;
        }

        .pt-no-basemap-tile,
        .leaflet-tile.pt-no-basemap-tile {
          background: #ffffff !important;
          background-color: #ffffff !important;
        }
      " )
    )
  )
  
  ## Keep informational popups and tooltips above all PT2-drawn map content.
  ##
  ## WHY:
  ##   PT2 uses custom panes for polygons, lines, points, labels, uploaded GIS
  ##   files, external overlays, and live Ops layers. Those panes can have high
  ##   z-index values. A popup should behave like an information window, so no
  ##   mapped feature should draw on top of it.
  ##
  ## DESIGN:
  ##   - popupPane is raised above all map-feature panes.
  ##   - tooltipPane is also raised, but kept just below popupPane.
  ##   - Leaflet controls remain above these panes because controls are DOM
  ##     elements in the control stack, not normal map panes.
  m <- htmlwidgets::onRender(
    m,
    "
function(el, x) {
  var map = this;

  var tooltipPane = map.getPane('tooltipPane');
  if (tooltipPane) {
    tooltipPane.style.zIndex = '1190';
  }

  var popupPane = map.getPane('popupPane');
  if (popupPane) {
    popupPane.style.zIndex = '1200';
  }

  // Delta Ops vector/canvas panes should not intercept mouse events when the
  // Delta Ops layer is not active.  The Delta Ops helper temporarily turns
  // the point pane back on while that layer is visible.
  ['pane_ops_delta_points', 'pane_ops_delta_labels'].forEach(function(name) {
    var pane = map.getPane(name);
    if (pane) pane.style.pointerEvents = 'none';
  });
}
    "
  )
  
  m
}

# ==== 3a. Add basemaps ========================================================

pt_add_basemaps <- function(m) {
  
  m |>
    leaflet::addTiles(
      urlTemplate = "https://basemap.nationalmap.gov/arcgis/rest/services/USGSHydroCached/MapServer/tile/{z}/{y}/{x}",
      group       = "USGS Hydrography",
      options     = leaflet::tileOptions(maxZoom = 20, noWrap = TRUE),
      attribution = "USGS The National Map — Hydrography"
    ) |>
    leaflet::addTiles(
      ## Use a full 256x256 opaque white SVG tile instead of a stretched 1x1
      ## data tile.  Combined with the white container CSS above, this avoids
      ## gray checkerboard/tile-edge artifacts on Mac/Chrome/Safari while
      ## keeping the "No Basemap" base group as a normal Leaflet basemap option.
      urlTemplate = "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIyNTYiIGhlaWdodD0iMjU2Ij48cmVjdCB3aWR0aD0iMjU2IiBoZWlnaHQ9IjI1NiIgZmlsbD0iI2ZmZmZmZiIvPjwvc3ZnPg==",
      group       = "No Basemap",
      options     = leaflet::tileOptions(
        noWrap       = TRUE,
        opacity      = 1,
        tileSize     = 256,
        detectRetina = FALSE,
        className    = "pt-no-basemap-tile"
      ),
      attribution = ""
    ) |>
    leaflet::addProviderTiles("USGS.USTopo",        group = "USGS Topo") |>
    leaflet::addProviderTiles("USGS.USImagery",     group = "USGS National Map (Imagery)") |>
    leaflet::addProviderTiles("USGS.USImageryTopo", group = "USGS National Map (Imagery+Topo)") |>
    leaflet::addProviderTiles("Esri.WorldTopoMap",  group = "Esri World Topographic") |>
    leaflet::addProviderTiles("Esri.WorldStreetMap",group = "Esri World Street Map") |>
    leaflet::addProviderTiles("Esri.WorldImagery",  group = "Esri World Imagery") |>
    leaflet::addProviderTiles("CartoDB.Positron",   group = "CartoDB.Positron") |>
    leaflet::addProviderTiles("OpenStreetMap",      group = "OpenStreetMap")
}

pt_base_groups <- function() {
  c(
    "USGS Hydrography",
    "USGS Topo",
    "USGS National Map (Imagery)",
    "USGS National Map (Imagery+Topo)",
    "Esri World Topographic",
    "Esri World Street Map",
    "Esri World Imagery",
    "CartoDB.Positron",
    "OpenStreetMap",
    "No Basemap"
  )
}

# ==== 3b. Add radar ===========================================================

pt_add_radar <- function(m, map_display) {
  
  if (!isTRUE(map_display$add_radar)) {
    return(m)
  }
  
  m |>
    leaflet::addWMSTiles(
      baseUrl = "https://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0q.cgi?",
      layers  = "nexrad-n0q-900913",
      options = leaflet::WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        zIndex = 700
      ),
      attribution = "Weather radar © Iowa Environmental Mesonet",
      group = pt_layer_group_name("NEXRAD Radar")
    )
}

# ==== 3c. Add California geology overlay ====================================
##
## PURPOSE:
##   Add the California Geological Survey tiled geology layer as a visual-only
##   overlay.
##
## NOTE:
##   This is not a basemap. It is an optional ArcGIS tiled overlay intended
##   mainly for statewide geologic context.
##
## SOURCE:
##   Same service used in the older PortaTreasure main script.

pt_add_cgs_geology <- function(m, map_display) {
  
  if (!isTRUE(map_display$add_cgs_geology)) {
    return(m)
  }
  
  m |>
    leaflet::addTiles(
      urlTemplate = "https://gis.conservation.ca.gov/server/rest/services/CGS/Geologic_Map_of_California/MapServer/tile/{z}/{y}/{x}",
      group       = pt_layer_group_name("CA Geology (visual only)"),
      options     = leaflet::tileOptions(
        opacity = 0.8,
        maxZoom = 18,
        noWrap = TRUE
      ),
      attribution = "California Geological Survey"
    )
}

# ==== 4. Add layer control and default visibility ============================
##
## The actual Leaflet group names remain fully categorized, e.g.:
##
##   "Points – Springs"
##   "Basins – HUC8 [2]"
##
## This keeps toggling robust because overlayGroups and add*() group names
## still match exactly.
##
## The display-only JavaScript below makes the control easier to read by:
##   1. adding visual section headers,
##   2. shortening visible overlay labels by removing the category prefix.
##
## Example:
##   Internal group name:  "Points – Springs"
##   Visible control row: "Springs" under a "Points" header.

pt_add_layer_control_headers <- function(m) {
  
  ## Pre-hide the raw Leaflet layer control before the browser paints it.
  ##
  ## WHY:
  ##   Leaflet first creates a plain, flat layer-control list. The JavaScript
  ##   below then rewrites that list into the PortaTreasure2 grouped/header
  ##   table of contents. Without early CSS, users can briefly see the raw
  ##   ungrouped control before the enhancement runs.
  ##
  ## DESIGN:
  ##   - Hide only the layer-control box while it is unenhanced.
  ##   - Reveal it as soon as data-pt-layer-headers is set to "done".
  ##   - Also allow a "fallback" reveal state so the TOC never stays hidden
  ##     if a future JavaScript error prevents header enhancement.
  m <- htmlwidgets::prependContent(
    m,
    htmltools::tags$style(
      htmltools::HTML("
        .leaflet-control-layers:not([data-pt-layer-headers='done']):not([data-pt-layer-headers='fallback']) {
          opacity: 0 !important;
          pointer-events: none !important;
        }

        .leaflet-control-layers[data-pt-layer-headers='done'],
        .leaflet-control-layers[data-pt-layer-headers='fallback'] {
          opacity: 1 !important;
          pointer-events: auto !important;
          transition: opacity 0.08s ease-in;
        }
      " )
    )
  )
  
  js <- r"---(
function(el, x) {
  
  function enhanceLayerControl() {
    
    var root = el.querySelector('.leaflet-control-layers');
    
    if (!root) {
      return;
    }
    
    // Avoid applying this more than once.
    if (root.getAttribute('data-pt-layer-headers') === 'done') {
      return;
    }
    
    // ----------------------------------------------------------------------
    // CSS
    // ----------------------------------------------------------------------
    
    if (!document.getElementById('pt-layer-control-header-style')) {
      
      var style = document.createElement('style');
      style.id = 'pt-layer-control-header-style';
      
      style.innerHTML = `
        /*
         * Main layer-control layout:
         * - only the inner .leaflet-control-layers-list scrolls
         * - the outer Leaflet control does not scroll
         * This prevents the duplicate-scrollbar behavior.
         */
        .leaflet-control-layers,
        .leaflet-control-layers-expanded {
          max-height: 48vh !important;
          overflow: hidden !important;
          background: rgba(246, 239, 222, 0.96) !important;
          border-color: rgba(111, 89, 52, 0.46) !important;
          box-sizing: border-box;
          font-family: Arial, Helvetica, sans-serif;
          font-size: 12px;
        }

        .leaflet-control-layers-list {
          max-height: calc(48vh - 6px) !important;
          overflow-y: auto !important;
          overflow-x: hidden !important;
          box-sizing: border-box;
          padding: 0 4px 4px 4px;
          background: rgba(246, 239, 222, 0.96) !important;
        }

        .leaflet-top.leaflet-right {
          overflow: visible;
        }
        
        .leaflet-control-layers .pt-main-layer-title {
          display: flex;
          align-items: center;
          gap: 7px;
          font-weight: 700;
          font-size: 13px;
          line-height: 1.15;
          color: #111;
          background: rgba(236, 224, 197, 0.98);
          padding: 7px 9px;
          margin: 0 0 6px 0;
          border-bottom: 1px solid rgba(111, 89, 52, 0.38);
          box-shadow: 0 1px 3px rgba(0, 0, 0, 0.12);
          cursor: pointer;
          user-select: none;
          position: sticky;
          top: 0;
          z-index: 20;
          box-sizing: border-box;
          width: 100%;
        }

        .leaflet-control-layers .pt-main-layer-title-text {
          flex: 1 1 auto;
          min-width: 0;
          text-align: center;
          white-space: nowrap;
        }

        .leaflet-control-layers .pt-main-layer-clear-btn {
          flex: 0 0 auto;
          font-size: 10.5px;
          font-weight: 400 !important;
          line-height: 1.1;
          padding: 3px 6px;
          border: 1px solid rgba(98, 117, 74, 0.80);
          border-radius: 5px;
          background: rgba(255, 255, 255, 0.92);
          color: #222;
          cursor: pointer;
          appearance: none;
          -webkit-appearance: none;
          box-shadow: 0 1px 3px rgba(0,0,0,0.20);
          transition: background-color 0.10s ease, border-color 0.10s ease, box-shadow 0.10s ease, transform 0.05s ease;
        }

        .leaflet-control-layers .pt-main-layer-clear-btn:hover,
        .leaflet-control-layers .pt-main-layer-clear-btn:focus {
          background: rgba(221, 238, 204, 0.98);
          border-color: rgba(66, 102, 47, 0.95);
          box-shadow: 0 1px 5px rgba(0,0,0,0.28);
          outline: none;
        }

        .leaflet-control-layers .pt-main-layer-clear-btn:active {
          background: rgba(199, 223, 181, 0.98);
          box-shadow: inset 0 1px 3px rgba(0,0,0,0.28);
          transform: translateY(1px);
        }
        
        .leaflet-control-layers .pt-main-layer-title .pt-main-layer-caret {
          float: none !important;
          flex: 0 0 auto;
          font-size: 11px;
          line-height: 1;
        }
        
        .leaflet-control-layers.pt-main-layer-collapsed,
        .leaflet-control-layers-expanded.pt-main-layer-collapsed {
          max-height: 36px !important;
          min-height: 36px !important;
          height: 36px !important;
          overflow: hidden !important;
          min-width: 120px;
          width: auto !important;
          padding: 0 !important;
          border-radius: 8px !important;
        }
        
        .leaflet-control-layers.pt-main-layer-collapsed .leaflet-control-layers-base,
        .leaflet-control-layers.pt-main-layer-collapsed .leaflet-control-layers-separator,
        .leaflet-control-layers.pt-main-layer-collapsed .leaflet-control-layers-overlays {
          display: none !important;
        }

        .leaflet-control-layers.pt-main-layer-collapsed .leaflet-control-layers-list {
          max-height: 36px !important;
          height: 36px !important;
          overflow: hidden !important;
          padding: 0 !important;
        }

        .leaflet-control-layers.pt-main-layer-collapsed .pt-main-layer-title {
          min-height: 36px !important;
          height: 36px !important;
          margin: 0 !important;
          padding: 0 9px !important;
          border-bottom: none !important;
          box-shadow: none !important;
          position: static !important;
        }
        
        .leaflet-control-layers .pt-layer-section-header {
          font-weight: 700;
          font-size: 12px;
          line-height: 1.2;
          color: #222;
          margin: 8px 0 3px 0;
          padding: 4px 4px 3px 4px;
          border-top: 1px solid rgba(111, 89, 52, 0.28);
          background: rgba(232, 221, 196, 0.78);
          pointer-events: none;
        }
        
        .leaflet-control-layers .pt-layer-section-header:first-child {
          margin-top: 2px;
          border-top: none;
        }
        
        .leaflet-control-layers label {
          margin-left: 8px;
        }
        
        .leaflet-control-layers .pt-layer-control-note {
          font-size: 11px;
          color: #555;
          margin: 2px 0 6px 4px;
        }

        .leaflet-control-layers label.pt-has-inline-lbl {
          position: relative;
          padding-right: 35px;
          box-sizing: border-box;
        }

        .leaflet-control-layers label.pt-label-companion-hidden {
          display: none !important;
        }

        .leaflet-control-layers .pt-inline-lbl-toggle {
          position: absolute;
          right: 2px;
          top: 50%;
          transform: translateY(-50%);
          display: inline-flex;
          align-items: center;
          gap: 2px;
          font-size: 9px;
          font-weight: 400;
          line-height: 1;
          color: #555;
          background: rgba(246, 239, 222, 0.88);
          border-radius: 3px;
          padding: 0 1px;
          white-space: nowrap;
          cursor: pointer;
          user-select: none;
        }

        .leaflet-control-layers .pt-inline-lbl-toggle input {
          width: 10px;
          height: 10px;
          margin: 0;
          padding: 0;
          flex: 0 0 auto;
        }

        .leaflet-control-layers .pt-inline-lbl-toggle.pt-disabled {
          opacity: 0.45;
        }
      `;
      
      document.head.appendChild(style);
    }
    
    // ----------------------------------------------------------------------
    // Main collapsible title
    // ----------------------------------------------------------------------
    
    var list = root.querySelector('.leaflet-control-layers-list');
    
    if (list && !root.querySelector('.pt-main-layer-title')) {
      
      var title = document.createElement('div');
      title.className = 'pt-main-layer-title';
      title.innerHTML =
        '<span class="pt-main-layer-title-text">Basemaps / Local Layers</span>' +
        '<button type="button" class="pt-main-layer-clear-btn" title="Clear all local overlay layers">Clear local layers</button>' +
        '<span class="pt-main-layer-caret">▾</span>';

      var clearBtn = title.querySelector('.pt-main-layer-clear-btn');

      if (clearBtn) {
        clearBtn.addEventListener('click', function(e) {
          e.preventDefault();
          e.stopPropagation();

          var checkedOverlays = Array.prototype.slice.call(
            root.querySelectorAll('.leaflet-control-layers-overlays input[type="checkbox"]:checked')
          );

          checkedOverlays.forEach(function(input) {
            if (input && input.checked) input.click();
          });
        });
      }
      
      title.addEventListener('click', function(e) {
        e.preventDefault();
        e.stopPropagation();
        root.classList.toggle('pt-main-layer-collapsed');
        var collapsed = root.classList.contains('pt-main-layer-collapsed');
        var caret = title.querySelector('.pt-main-layer-caret');
        if (caret) {
          caret.textContent = collapsed ? '▸' : '▾';
        }
      });
      
      list.insertBefore(title, list.firstChild);

      // Start collapsed so the map opens with more viewing space. Users can
      // expand the Basemaps / Local Layers panel from the ribbon when needed.
      root.classList.add('pt-main-layer-collapsed');
      var initialCaret = title.querySelector('.pt-main-layer-caret');
      if (initialCaret) {
        initialCaret.textContent = '▸';
      }
    }
    
    // ----------------------------------------------------------------------
    // Basemap header
    // ----------------------------------------------------------------------
    
    var baseContainer = root.querySelector('.leaflet-control-layers-base');
    
    if (baseContainer && baseContainer.querySelector('label') && !baseContainer.querySelector('.pt-layer-section-header')) {
      
      var baseHeader = document.createElement('div');
      baseHeader.className = 'pt-layer-section-header';
      baseHeader.textContent = 'Basemaps';
      
      baseContainer.insertBefore(baseHeader, baseContainer.firstChild);
    }
    
    // ----------------------------------------------------------------------
    // Overlay category headers
    // ----------------------------------------------------------------------
    
    var overlayContainer = root.querySelector('.leaflet-control-layers-overlays');
    
    if (!overlayContainer) {
      root.setAttribute('data-pt-layer-headers', 'done');
      return;
    }
    
    var categoryPrefixes = [
      'Ops – ',
      'Core – ',
      'Basins – ',
      'Points – ',
      'Channels – ',
      'Reference – ',
      'Labels – '
    ];
    
    function findCategoryAndShortName(fullName) {
      
      for (var i = 0; i < categoryPrefixes.length; i++) {
        
        var prefix = categoryPrefixes[i];
        
        if (fullName.indexOf(prefix) === 0) {
          var categoryName = prefix.replace(' – ', '');
          if (categoryName === 'Points') {
            categoryName = 'Monitoring Sites / Records';
          }
          return {
            category: categoryName,
            shortName: fullName.substring(prefix.length)
          };
        }
      }
      
      return {
        category: 'Other',
        shortName: fullName
      };
    }
    
    function replaceVisibleText(label, oldText, newText) {
      
      function walk(node) {
        
        for (var i = 0; i < node.childNodes.length; i++) {
          
          var child = node.childNodes[i];
          
          if (child.nodeType === Node.TEXT_NODE) {
            child.nodeValue = child.nodeValue.replace(oldText, newText);
          } else if (
            child.nodeType === Node.ELEMENT_NODE &&
            child.tagName.toLowerCase() !== 'input'
          ) {
            walk(child);
          }
        }
      }
      
      walk(label);
    }
    
    function normLayerName(txt) {
      txt = String(txt == null ? '' : txt);
      txt = txt.replace(/\s+/g, ' ').trim();
      txt = txt.replace(/^(Ops|Core|Basins|Points|Channels|Reference|Labels)\s+–\s+/i, '');
      txt = txt.replace(/^Monitoring Sites\s*\/\s*Records\s+–\s+/i, '');
      txt = txt.replace(/\s*\([^)]*\)\s*$/g, '');
      txt = txt.replace(/\s+lbl\s*$/i, '');
      return txt.toLowerCase().trim();
    }

    function rowShortName(label) {
      if (!label) return '';
      return label.getAttribute('data-pt-layer-short-name') ||
        label.getAttribute('data-pt-layer-full-name') ||
        label.textContent || '';
    }

    function findLayerRow(rowLabels, desiredName, wantLabelRow) {
      var target = normLayerName(desiredName);
      for (var i = 0; i < rowLabels.length; i++) {
        var label = rowLabels[i];
        var full = label.getAttribute('data-pt-layer-full-name') || '';
        var isLabelRow = /^Labels\s+–\s+/i.test(full);
        if (wantLabelRow === true && !isLabelRow) continue;
        if (wantLabelRow === false && isLabelRow) continue;
        var vals = [
          rowShortName(label),
          full,
          label.textContent || ''
        ];
        for (var j = 0; j < vals.length; j++) {
          if (normLayerName(vals[j]) === target) return label;
        }
      }
      return null;
    }

    function refreshLabelsHeaderVisibility(container) {
      var headers = Array.prototype.slice.call(container.querySelectorAll('.pt-layer-section-header'));
      headers.forEach(function(header) {
        if ((header.textContent || '').replace(/\s+/g, ' ').trim() !== 'Labels') return;
        var node = header.nextSibling;
        var anyVisibleLabel = false;
        while (node) {
          if (node.nodeType === Node.ELEMENT_NODE && node.classList.contains('pt-layer-section-header')) break;
          if (node.nodeType === Node.ELEMENT_NODE && node.tagName && node.tagName.toLowerCase() === 'label') {
            if (!node.classList.contains('pt-label-companion-hidden')) {
              anyVisibleLabel = true;
              break;
            }
          }
          node = node.nextSibling;
        }
        header.style.display = anyVisibleLabel ? '' : 'none';
      });
    }

    function installInlineLabelToggles(container, rowLabels) {
      // Keep this registry deliberately tied to the actual companion rows
      // that exist in the current Labels group.  Do not add future/config-only
      // label candidates here until they are real visible label overlay rows.
      // Current expected count: 25 companion label rows.
      var pairs = [
        {main: 'BLM Field Office Boundaries', label: 'BLM Field Office (outer)'},
        {main: 'GW Basins, Bulletin 118', label: 'GW – Bull. 118'},
        {main: 'Counties', label: 'Counties'},
        {main: 'HUC2 – PRISM/BCMv8', label: 'HUC2'},
        {main: 'HUC4 – PRISM/BCMv8', label: 'HUC4'},
        {main: 'HUC6 – PRISM/BCMv8', label: 'HUC6'},
        {main: 'HUC8 – PRISM/BCMv8', label: 'HUC8'},
        {main: 'HUC10 – PRISM/BCMv8', label: 'HUC10'},
        {main: 'HUC12 – PRISM/BCMv8', label: 'HUC12'},
        {main: 'CNRFC Product Availability', label: 'CNRFC Product Availability'},
        {main: 'CNRFC weather station catalog', label: 'CNRFC Precip Gages'},
        {main: 'CNRFC river/reservoir catalog', label: 'CNRFC Stream Gages'},
        {main: 'USGS streamgages', label: 'USGS streamgages'},
        {main: 'BLM-drilled wells | NOC', label: 'BLM-drilled wells | NOC'},
        {main: 'GW wells | 2025 Mojave-BLM limited field check', label: 'GW wells | 2025 Mojave-BLM limited field check'},
        {main: 'Springs', label: 'Springs'},
        {main: 'Water rights POD | SWRCB 2026 BLM list', label: 'Water rights POD | SWRCB 2026 BLM list'},
        {main: 'Water rights POD | BRIM spatial BLM match', label: 'Water rights POD | BRIM spatial BLM match'},
        {main: 'Water rights POD | BRIM name/text BLM candidate', label: 'Water rights POD | BRIM name/text BLM candidate'},
        {main: 'CNRFC FNF Sha/Tri/west Sierra Basins', label: 'CNRFC FNF Sha/Tri/west Sierra Basins'},
        {main: 'Groundwater Sustainability Plan Areas', label: 'Groundwater Sustainability Plan Areas'},
        {main: 'Adjudicated Groundwater Basins', label: 'Adjudicated Groundwater Basins'},
        {main: 'ACECs', label: 'ACECs'},
        {main: 'Major Conveyance', label: 'Major Conveyance'},
        {main: 'Water conveyance | BRIM mapped', label: 'Water conveyance | BRIM mapped'},
        {main: 'Water Districts', label: 'Water Districts'},
        {main: 'RWQCB Regions', label: 'RWQCB Regions'}
      ];

      pairs.forEach(function(pair) {
        var mainRow = findLayerRow(rowLabels, pair.main, false);
        var labelRow = findLayerRow(rowLabels, pair.label, true);
        if (!mainRow || !labelRow || mainRow === labelRow) return;
        if (mainRow.querySelector('.pt-inline-lbl-toggle')) return;

        var mainInput = mainRow.querySelector('input[type="checkbox"]');
        var labelInput = labelRow.querySelector('input[type="checkbox"]');
        if (!mainInput || !labelInput) return;

        labelRow.classList.add('pt-label-companion-hidden');
        labelRow.setAttribute('data-pt-inline-companion-label', 'true');

        var toggle = document.createElement('span');
        toggle.className = 'pt-inline-lbl-toggle';
        toggle.title = 'Show/hide labels for this layer';
        toggle.innerHTML = '<input type="checkbox" aria-label="Show labels"><span>lbl</span>';

        var inlineInput = toggle.querySelector('input');
        mainRow.classList.add('pt-has-inline-lbl');
        mainRow.appendChild(toggle);

        function stop(e) {
          e.stopPropagation();
        }

        toggle.addEventListener('click', stop);
        toggle.addEventListener('mousedown', stop);
        toggle.addEventListener('dblclick', stop);

        inlineInput.addEventListener('click', stop);
        inlineInput.addEventListener('change', function(e) {
          e.stopPropagation();
          if (!mainInput.checked) {
            inlineInput.checked = false;
            return;
          }
          if (labelInput.checked !== inlineInput.checked) {
            labelInput.click();
          }
          syncInlineState();
        });

        function syncInlineState() {
          var mainOn = !!mainInput.checked;

          if (!mainOn && labelInput.checked) {
            labelInput.click();
          }

          inlineInput.disabled = !mainOn;
          inlineInput.checked = mainOn && !!labelInput.checked;
          toggle.classList.toggle('pt-disabled', !mainOn);
        }

        mainInput.addEventListener('change', function() {
          setTimeout(syncInlineState, 0);
        });
        labelInput.addEventListener('change', function() {
          setTimeout(syncInlineState, 0);
        });

        setTimeout(syncInlineState, 0);
      });

      refreshLabelsHeaderVisibility(container);
    }

    var labels = Array.prototype.slice.call(
      overlayContainer.querySelectorAll('label')
    );
    
    var currentCategory = null;
    
    labels.forEach(function(label) {
      
      var fullName = label.textContent.replace(/\\s+/g, ' ').trim();
      
      if (!fullName) {
        return;
      }
      
      var parsed = findCategoryAndShortName(fullName);
      
      label.setAttribute('data-pt-layer-full-name', fullName);
      label.setAttribute('data-pt-layer-short-name', parsed.shortName);
      
      if (parsed.category !== currentCategory) {
        
        var header = document.createElement('div');
        header.className = 'pt-layer-section-header';
        header.textContent = parsed.category;
        
        overlayContainer.insertBefore(header, label);
        currentCategory = parsed.category;
      }
      
      if (parsed.shortName !== fullName) {
        replaceVisibleText(label, fullName, parsed.shortName);
      }
    });
    
    installInlineLabelToggles(overlayContainer, labels);
    
    root.setAttribute('data-pt-layer-headers', 'done');
  }
  
  function revealLayerControlFallback() {

    var root = el.querySelector('.leaflet-control-layers');

    if (!root) {
      return;
    }

    if (root.getAttribute('data-pt-layer-headers') !== 'done') {
      root.setAttribute('data-pt-layer-headers', 'fallback');
    }
  }
  
  // The layer-control DOM usually exists immediately, but short retries make
  // this robust when the widget initializes slowly.
  setTimeout(enhanceLayerControl, 0);
  setTimeout(enhanceLayerControl, 250);
  setTimeout(enhanceLayerControl, 1000);

  // Failsafe: if a future JS issue prevents enhancement, reveal the control
  // rather than leaving users with no table of contents.
  setTimeout(revealLayerControlFallback, 2500);
  setTimeout(revealLayerControlFallback, 5000);
}
)---"
  
  htmlwidgets::onRender(m, js)
}


pt_add_layer_control <- function(m, overlay_groups, map_display) {
  
  ## Keep the main layer control open by default. The custom header above lets
  ## users collapse it to a compact "Layers" bar when Ops overlays need space.
  m <- m |>
    leaflet::addLayersControl(
      baseGroups = pt_base_groups(),
      overlayGroups = overlay_groups,
      options = leaflet::layersControlOptions(
        collapsed = FALSE
      )
    )
  
  hide_by_default <- setdiff(
    overlay_groups,
    map_display$default_visible_overlays
  )
  
  for (grp in hide_by_default) {
    m <- leaflet::hideGroup(m, grp)
  }
  
  m <- pt_add_layer_control_headers(m)
  
  m
}

# ==== 5. Add mouse-coordinate readout ========================================
##
## PURPOSE:
##   Display the current mouse position as latitude/longitude in the lower
##   right corner of the map.
##
## NOTES:
##   This is client-side JavaScript added through htmlwidgets::onRender().
##   It does not affect preprocessing or cached layers.

pt_add_mouse_coordinates <- function(m, digits = 5) {

  js <- sprintf(
    "
function(el, x) {

  var map = this;
  var container = map && map.getContainer ? map.getContainer() : null;

  if (!container) {
    return;
  }

  /*
   * PT2 bottom info row.
   *
   * Keep the Lat/Lon/Zoom readout horizontally aligned to the left of the
   * Leaflet scale bar, and keep both shifted left of the fixed Ops Live panel.
   */
  if (!document.getElementById('pt-bottom-info-style')) {
    var style = document.createElement('style');
    style.id = 'pt-bottom-info-style';
    style.innerHTML = `
      .pt-bottom-info-row {
        position: absolute;
        right: 390px;
        bottom: 10px;
        z-index: 10020;
        display: flex;
        flex-direction: row;
        align-items: flex-end;
        gap: 8px;
        pointer-events: none;
      }

      .pt-bottom-info-row .leaflet-control,
      .pt-bottom-info-row .pt-mouse-coords {
        margin: 0 !important;
        pointer-events: auto;
      }

      .pt-bottom-info-row .leaflet-control-scale {
        clear: none !important;
      }

      .pt-mouse-coords {
        background: rgba(255, 255, 255, 0.88);
        padding: 4px 7px;
        border: 1px solid #999;
        border-radius: 4px;
        font: 12px/1.2 Arial, sans-serif;
        color: #222;
        white-space: nowrap;
        box-sizing: border-box;
      }

      @media (max-width: 900px) {
        .pt-bottom-info-row {
          right: 12px;
          bottom: 10px;
          flex-direction: column;
          align-items: flex-end;
        }
      }
    `;
    document.head.appendChild(style);
  }

  var row = container.querySelector('.pt-bottom-info-row');
  if (!row) {
    row = document.createElement('div');
    row.className = 'pt-bottom-info-row leaflet-control';
    container.appendChild(row);
  }

  var coordDiv = row.querySelector('.pt-mouse-coords');
  if (!coordDiv) {
    coordDiv = document.createElement('div');
    coordDiv.className = 'pt-mouse-coords';
    coordDiv.innerHTML = 'Lat/Lon: --, -- | Zoom: ' + map.getZoom();
    row.appendChild(coordDiv);
    L.DomEvent.disableClickPropagation(coordDiv);
    L.DomEvent.disableScrollPropagation(coordDiv);
  }

  function attachScaleBarToBottomRow() {
    var scale = container.querySelector('.leaflet-control-scale');
    if (scale && scale.parentNode !== row) {
      row.appendChild(scale);
    }
  }

  attachScaleBarToBottomRow();
  window.setTimeout(attachScaleBarToBottomRow, 250);
  window.setTimeout(attachScaleBarToBottomRow, 1000);

  var lastLatLng = null;

  function coordHtml(latlng) {
    var zoomText = map.getZoom();

    if (!latlng) {
      return 'Lat/Lon: --, -- | Zoom: ' + zoomText;
    }

    return 'Lat/Lon: ' +
      latlng.lat.toFixed(%s) +
      ', ' +
      latlng.lng.toFixed(%s) +
      ' | Zoom: ' +
      zoomText;
  }

  function updateCoordDiv(latlng) {
    if (coordDiv) {
      coordDiv.innerHTML = coordHtml(latlng);
    }
  }

  map.on('mousemove', function(e) {
    lastLatLng = e.latlng;
    updateCoordDiv(lastLatLng);
  });

  map.on('mouseout', function(e) {
    lastLatLng = null;
    updateCoordDiv(null);
  });

  map.on('zoomend', function(e) {
    updateCoordDiv(lastLatLng);
    attachScaleBarToBottomRow();
  });

  updateCoordDiv(null);
}
",
    digits,
    digits
  )

  htmlwidgets::onRender(m, js)
}

# ==== 5b. Add map utility zoom controls ======================================
##
## PURPOSE:
##   Add two compact buttons next to the standard Leaflet +/- zoom control:
##   reset to initial extent next to +, and one-shot marquee/box zoom next to -.
##   This is intentionally lightweight custom JS, not a plugin.

pt_add_marquee_zoom_control <- function(m) {

  js <- "
function(el, x) {

  var map = this;
  var container = map && map.getContainer ? map.getContainer() : null;
  if (!container || document.getElementById('pt-map-zoom-utility-control')) return;

  var initialCenter = map.getCenter ? map.getCenter() : null;
  var initialZoom = map.getZoom ? map.getZoom() : null;

  if (!document.getElementById('pt-marquee-zoom-style')) {
    var style = document.createElement('style');
    style.id = 'pt-marquee-zoom-style';
    style.innerHTML = `
      .pt-map-zoom-utility-control {
        position: absolute;
        top: 10px;
        left: 46px;
        z-index: 10030;
        pointer-events: auto;
        display: flex;
        flex-direction: column;
        gap: 0;
      }

      .pt-map-zoom-utility-btn {
        width: 34px;
        height: 34px;
        box-sizing: border-box;
        border: 2px solid rgba(0,0,0,0.26);
        border-radius: 0;
        background: rgba(255,255,255,0.96);
        box-shadow: 0 1px 4px rgba(0,0,0,0.28);
        display: flex;
        align-items: center;
        justify-content: center;
        padding: 0;
        font-family: Arial, Helvetica, sans-serif;
        color: #245f9c;
      }

      .pt-map-zoom-reset-btn {
        border-top-left-radius: 3px;
        border-top-right-radius: 3px;
        border-bottom-width: 1px;
      }

      .pt-marquee-zoom-btn {
        border-bottom-left-radius: 3px;
        border-bottom-right-radius: 3px;
        border-top-width: 1px;
      }

      .pt-map-zoom-reset-btn {
        cursor: pointer;
        font-size: 20px;
        font-weight: 700;
        line-height: 1;
      }

      .pt-marquee-zoom-btn {
        cursor: zoom-in;
      }

      .pt-map-zoom-utility-btn:hover,
      .pt-marquee-zoom-active .pt-marquee-zoom-btn {
        background: rgba(235,247,255,0.98);
        border-color: rgba(25,101,176,0.82);
      }

      .pt-marquee-zoom-icon {
        width: 17px;
        height: 13px;
        border: 2px dashed #245f9c;
        border-radius: 2px;
        box-sizing: border-box;
        position: relative;
      }

      .pt-marquee-zoom-icon::after {
        content: '';
        position: absolute;
        right: -6px;
        bottom: -6px;
        width: 7px;
        height: 2px;
        background: #245f9c;
        transform: rotate(45deg);
        transform-origin: left center;
      }

      .leaflet-container.pt-marquee-zoom-mode,
      .leaflet-container.pt-marquee-zoom-mode .leaflet-interactive {
        cursor: crosshair !important;
      }

      .pt-marquee-zoom-rect {
        position: absolute;
        border: 2px dashed #245f9c;
        background: rgba(43,108,176,0.12);
        box-sizing: border-box;
        z-index: 10025;
        pointer-events: none;
      }
    `;
    document.head.appendChild(style);
  }

  var wrap = document.createElement('div');
  wrap.id = 'pt-map-zoom-utility-control';
  wrap.className = 'pt-map-zoom-utility-control leaflet-control';
  wrap.innerHTML =
    '<button type=\"button\" id=\"pt-reset-zoom-btn\" class=\"pt-map-zoom-utility-btn pt-map-zoom-reset-btn\" title=\"Reset to initial map extent\" aria-label=\"Reset map extent\">⌂</button>' +
    '<button type=\"button\" id=\"pt-marquee-zoom-btn\" class=\"pt-map-zoom-utility-btn pt-marquee-zoom-btn\" title=\"Marquee zoom: drag a box to zoom\" aria-label=\"Marquee zoom\"><span class=\"pt-marquee-zoom-icon\" aria-hidden=\"true\"></span></button>';
  container.appendChild(wrap);

  L.DomEvent.disableClickPropagation(wrap);
  L.DomEvent.disableScrollPropagation(wrap);

  var resetBtn = document.getElementById('pt-reset-zoom-btn');
  var btn = document.getElementById('pt-marquee-zoom-btn');
  var active = false;
  var drawing = false;
  var start = null;
  var rect = null;
  var wasDragging = true;

  function getPoint(evt) {
    var r = container.getBoundingClientRect();
    return L.point(evt.clientX - r.left, evt.clientY - r.top);
  }

  function setActive(on) {
    active = !!on;
    drawing = false;
    start = null;
    if (rect) { rect.remove(); rect = null; }

    wrap.classList.toggle('pt-marquee-zoom-active', active);
    container.classList.toggle('pt-marquee-zoom-mode', active);

    if (active) {
      wasDragging = map.dragging && map.dragging.enabled ? map.dragging.enabled() : true;
      if (map.dragging) map.dragging.disable();
      if (btn) btn.setAttribute('aria-pressed', 'true');
    } else {
      if (map.dragging && wasDragging) map.dragging.enable();
      if (btn) btn.setAttribute('aria-pressed', 'false');
    }
  }

  function isControlTarget(target) {
    return target && target.closest && target.closest(
      '.leaflet-control, .pt-tools-adddata-wrap, .pt-local-upload-wrap, ' +
      '.pt-ops-live-panel, .pt-huc-theme-control, .pt-huc-theme-legend'
    );
  }

  resetBtn.addEventListener('click', function(e) {
    e.preventDefault();
    e.stopPropagation();
    setActive(false);
    if (initialCenter && initialZoom !== null && initialZoom !== undefined && map.setView) {
      map.setView(initialCenter, initialZoom);
    }
  });

  btn.addEventListener('click', function(e) {
    e.preventDefault();
    e.stopPropagation();
    setActive(!active);
  });

  container.addEventListener('pointerdown', function(e) {
    if (!active || isControlTarget(e.target)) return;
    if (e.button !== undefined && e.button !== 0) return;

    e.preventDefault();
    e.stopPropagation();

    drawing = true;
    start = getPoint(e);
    rect = document.createElement('div');
    rect.className = 'pt-marquee-zoom-rect';
    rect.style.left = start.x + 'px';
    rect.style.top = start.y + 'px';
    rect.style.width = '0px';
    rect.style.height = '0px';
    container.appendChild(rect);
  }, true);

  container.addEventListener('pointermove', function(e) {
    if (!active || !drawing || !start || !rect) return;
    e.preventDefault();
    e.stopPropagation();

    var p = getPoint(e);
    var x = Math.min(start.x, p.x);
    var y = Math.min(start.y, p.y);
    var w = Math.abs(p.x - start.x);
    var h = Math.abs(p.y - start.y);

    rect.style.left = x + 'px';
    rect.style.top = y + 'px';
    rect.style.width = w + 'px';
    rect.style.height = h + 'px';
  }, true);

  window.addEventListener('pointerup', function(e) {
    if (!active || !drawing || !start) return;
    e.preventDefault();
    e.stopPropagation();

    var p = getPoint(e);
    var w = Math.abs(p.x - start.x);
    var h = Math.abs(p.y - start.y);

    if (rect) { rect.remove(); rect = null; }
    drawing = false;

    if (w > 8 && h > 8) {
      var sw = map.containerPointToLatLng(L.point(Math.min(start.x, p.x), Math.max(start.y, p.y)));
      var ne = map.containerPointToLatLng(L.point(Math.max(start.x, p.x), Math.min(start.y, p.y)));
      var bounds = L.latLngBounds(sw, ne);
      var targetZoom = null;
      try { targetZoom = map.getBoundsZoom(bounds, false, L.point(8, 8)); } catch(errZoom) {}

      // Let heavy browser-built layers temporarily get out of the way before
      // a one-shot marquee zoom.  This keeps basemap tiles from waiting behind
      // expensive marker/cluster recalculation.  Listeners are optional.
      try {
        map.fire('pt:marqueezoomstart', {
          bounds: bounds,
          fromZoom: map.getZoom ? map.getZoom() : null,
          targetZoom: targetZoom
        });
      } catch(errStart) {}

      try {
        map.once('moveend', function() {
          setTimeout(function() {
            try {
              map.fire('pt:marqueezoomend', {
                bounds: bounds,
                targetZoom: targetZoom
              });
            } catch(errEnd) {}
          }, 0);
        });
      } catch(errOnce) {}

      map.fitBounds(bounds, {padding: [8, 8], animate: false});
    }

    setActive(false);
  }, true);

  window.addEventListener('keydown', function(e) {
    if (active && e.key === 'Escape') {
      setActive(false);
    }
  });
}
"

  htmlwidgets::onRender(m, js)
}
