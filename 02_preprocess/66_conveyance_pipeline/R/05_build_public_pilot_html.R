# ---- Standalone pilot HTML ---------------------------------------------------

segments_geojson_path <- tempfile(fileext = ".geojson")
labels_geojson_path <- tempfile(fileext = ".geojson")

sf::st_write(
  segments_out,
  segments_geojson_path,
  driver = "GeoJSON",
  delete_dsn = TRUE,
  quiet = TRUE
)

sf::st_write(
  labels_out,
  labels_geojson_path,
  driver = "GeoJSON",
  delete_dsn = TRUE,
  quiet = TRUE
)

segments_geojson <- jsonlite::fromJSON(
  paste(readLines(segments_geojson_path, warn = FALSE), collapse = "\n"),
  simplifyVector = FALSE
)

labels_geojson <- jsonlite::fromJSON(
  paste(readLines(labels_geojson_path, warn = FALSE), collapse = "\n"),
  simplifyVector = FALSE
)

checkbox_group_html <- function(title, values, filter_name, default_off = character()) {
  items <- vapply(
    values,
    function(value) {
      checked <- if (value %in% default_off) "" else " checked"
      paste0(
        "<label class='brim-filter-item'>",
        "<input type='checkbox' data-filter='",
        htmltools::htmlEscape(filter_name),
        "' value='",
        htmltools::htmlEscape(value),
        "'",
        checked,
        "> ",
        htmltools::htmlEscape(value),
        "</label>"
      )
    },
    character(1)
  )

  paste0(
    "<details open>",
    "<summary>", htmltools::htmlEscape(title), "</summary>",
    paste(items, collapse = ""),
    "</details>"
  )
}

ownership_values <- sort(unique(segments_out$ownership_class))
project_family_values <- sort(
  unique(unlist(strsplit(segments_out$project_family, ";\\s*")))
)
project_family_values <- project_family_values[
  !is.na(project_family_values) &
    project_family_values != ""
]
facility_group_values <- sort(unique(segments_out$facility_group))
rank_values <- c(
  "statewide_major",
  "regional_major",
  "medium",
  "local_supporting"
)
rank_values <- rank_values[rank_values %in% segments_out$display_rank]

project_system_values <- sort(
  unique(unlist(strsplit(segments_out$project_name, ";\\s*")))
)
project_system_values <- project_system_values[
  !is.na(project_system_values) &
    project_system_values != ""
]

project_options <- paste0(
  "<option value='__ALL__'>All project systems</option>",
  paste0(
    "<option value='",
    htmltools::htmlEscape(project_system_values),
    "'>",
    htmltools::htmlEscape(project_system_values),
    "</option>",
    collapse = ""
  )
)

filter_control <- paste0(
  "<div id='brim-conveyance-panel'>",
  "<div class='brim-title'>BRIM conveyance sandbox pilot</div>",
  "<div class='brim-subtitle'>",
  "Facility + segment + label-point architecture",
  "</div>",
  "<input id='brim-search' type='search' ",
  "placeholder='Search names, aliases, projects, owner…'>",
  "<div class='brim-row'>",
  "<label><input id='brim-label-toggle' type='checkbox'> lbl</label>",
  "<label><input id='brim-low-toggle' type='checkbox' checked> include low confidence</label>",
  "</div>",
  checkbox_group_html(
    "Ownership",
    ownership_values,
    "ownership"
  ),
  checkbox_group_html(
    "Project family",
    project_family_values,
    "project_family"
  ),
  "<details open><summary>Named project/system</summary>",
  "<select id='brim-project-select'>",
  project_options,
  "</select></details>",
  checkbox_group_html(
    "Facility group",
    facility_group_values,
    "facility_group"
  ),
  checkbox_group_html(
    "Display rank",
    rank_values,
    "display_rank",
    default_off = "local_supporting"
  ),
  "<details><summary>BLM proximity</summary>",
  "<div class='brim-disabled'>",
  "<label><input type='checkbox' disabled> crosses BLM</label>",
  "<div>Distance range: <input type='number' disabled value='0'> to ",
  "<input type='number' disabled value='10'> mi</div>",
  "<div>Reserved fields are present; phase-2 BLM preprocessing has not run.</div>",
  "</div></details>",
  "<div id='brim-count'></div>",
  "<div class='brim-help'>",
  "Click a line for canonical name, aliases, hierarchy, type, ",
  "project membership, source lineage, and QA confidence.",
  "</div>",
  "</div>"
)

legend_control <- paste0(
  "<div class='brim-type-legend'>",
  "<b>Line color = facility group</b>",
  "<div><span style='background:#2166AC'></span> Open conveyance</div>",
  "<div><span style='background:#6A51A3'></span> Closed conveyance</div>",
  "<div><span style='background:#CB181D'></span> Power conveyance</div>",
  "<div><span style='background:#8C510A'></span> Drainage</div>",
  "<div><span style='background:#238B8B'></span> Flood conveyance</div>",
  "<div><span style='background:#636363'></span> Distribution/local</div>",
  "<div><span style='background:#252525'></span> Other/unknown</div>",
  "</div>"
)

panel_css <- tags$style(HTML(
  "
  #brim-conveyance-panel {
    width: 350px;
    max-height: 72vh;
    overflow-y: auto;
    background: rgba(255,255,255,.97);
    padding: 10px 12px;
    border-radius: 5px;
    box-shadow: 0 1px 8px rgba(0,0,0,.38);
    font: 12px/1.35 Arial, sans-serif;
    color: #222;
  }
  .brim-title {
    font-size: 16px;
    font-weight: 700;
  }
  .brim-subtitle {
    color: #555;
    margin: 2px 0 8px;
  }
  #brim-search,
  #brim-project-select {
    width: 100%;
    box-sizing: border-box;
    padding: 5px;
    margin: 3px 0 7px;
  }
  .brim-row {
    display: flex;
    gap: 14px;
    margin-bottom: 6px;
  }
  #brim-conveyance-panel details {
    border-top: 1px solid #ddd;
    padding: 5px 0 3px;
  }
  #brim-conveyance-panel summary {
    font-weight: 700;
    cursor: pointer;
    margin-bottom: 3px;
  }
  .brim-filter-item {
    display: block;
    margin: 2px 0;
  }
  .brim-disabled {
    opacity: .65;
  }
  .brim-disabled input[type='number'] {
    width: 52px;
  }
  #brim-count {
    margin-top: 8px;
    padding-top: 7px;
    border-top: 1px solid #ccc;
    font-weight: 700;
  }
  .brim-help {
    margin-top: 5px;
    color: #555;
  }
  .brim-type-legend {
    background: rgba(255,255,255,.95);
    padding: 8px 10px;
    border-radius: 4px;
    box-shadow: 0 1px 6px rgba(0,0,0,.3);
    font: 11px/1.4 Arial, sans-serif;
  }
  .brim-type-legend span {
    display: inline-block;
    width: 22px;
    height: 4px;
    vertical-align: middle;
    margin-right: 5px;
  }
  .brim-conveyance-label-tooltip {
    background: transparent;
    border: 0;
    box-shadow: none;
    padding: 0;
    font: 600 11px/1.1 Arial, sans-serif;
    color: #202020;
    white-space: nowrap;
    text-shadow:
      -1px -1px 0 #fff,
       1px -1px 0 #fff,
      -1px  1px 0 #fff,
       1px  1px 0 #fff;
    pointer-events: none;
  }
  .brim-conveyance-label-tooltip:before {
    display: none;
  }
  .leaflet-popup-content {
    margin: 10px 12px;
  }
  "
))

bbox <- sf::st_bbox(segments_out)

m <- leaflet(
  options = leafletOptions(
    preferCanvas = FALSE,
    zoomControl = TRUE
  )
) |>
  addProviderTiles(
    providers$CartoDB.Positron,
    group = "Light basemap"
  ) |>
  addProviderTiles(
    providers$Esri.WorldImagery,
    group = "Satellite"
  ) |>
  addProviderTiles(
    providers$CartoDB.PositronNoLabels,
    group = "USGS Hydrography"
  ) |>
  addTiles(
    urlTemplate = USGS_HYDRO_TILES,
    group = "USGS Hydrography",
    options = tileOptions(
      minZoom = 0,
      maxZoom = 20,
      maxNativeZoom = 16,
      opacity = 1
    ),
    attribution = "USGS The National Map: National Hydrography Dataset"
  ) |>
  addLayersControl(
    baseGroups = c(
      "Light basemap",
      "Satellite",
      "USGS Hydrography"
    ),
    options = layersControlOptions(
      collapsed = FALSE
    )
  ) |>
  addScaleBar(position = "bottomleft") |>
  addControl(
    filter_control,
    position = "topleft"
  ) |>
  addControl(
    legend_control,
    position = "bottomright"
  ) |>
  fitBounds(
    lng1 = unname(bbox["xmin"]),
    lat1 = unname(bbox["ymin"]),
    lng2 = unname(bbox["xmax"]),
    lat2 = unname(bbox["ymax"])
  ) |>
  htmlwidgets::prependContent(panel_css)

widget_data <- list(
  segments = segments_geojson,
  labels = labels_geojson
)

js_code <- "
function(el, x, data) {
  var map = this;
  var segmentsData = data.segments;
  var labelsData = data.labels;

  var visibleSegments = L.layerGroup().addTo(map);
  var visibleLabels = L.layerGroup().addTo(map);

  var allSegments = [];
  var allLabels = [];

  function esc(value) {
    return String(value == null ? '' : value)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/\"/g, '&quot;')
      .replace(/'/g, '&#039;');
  }

  function tokens(value) {
    return String(value || '')
      .split(';')
      .map(function(v) { return v.trim(); })
      .filter(function(v) { return v.length > 0; });
  }

  function colorFor(group) {
    var colors = {
      'Open conveyance': '#2166AC',
      'Closed conveyance': '#6A51A3',
      'Power conveyance': '#CB181D',
      'Drainage': '#8C510A',
      'Flood conveyance': '#238B8B',
      'Distribution/local': '#636363',
      'Other/unknown': '#252525'
    };
    return colors[group] || '#252525';
  }

  function styleFor(p) {
    var weights = {
      'statewide_major': 6,
      'regional_major': 5,
      'medium': 3.5,
      'local_supporting': 2
    };

    var dash = null;
    if (p.facility_type === 'Pipeline' || p.facility_type === 'Tunnel' ||
        p.facility_type === 'Conduit' || p.facility_type === 'Siphon') {
      dash = '9,5';
    } else if (p.facility_group === 'Drainage') {
      dash = '4,4';
    } else if (p.geometry_confidence === 'low') {
      dash = '2,5';
    }

    return {
      color: colorFor(p.facility_group),
      weight: weights[p.display_rank] || 2.5,
      opacity: p.geometry_confidence === 'low' ? 0.58 : 0.82,
      dashArray: dash,
      lineCap: 'round',
      lineJoin: 'round'
    };
  }

  function popupHtml(p) {
    var projectText = [
      p.project_family,
      p.project_name,
      p.project_division,
      p.project_unit,
      p.project_subunit
    ]
      .filter(function(v) { return v && String(v).length > 0; })
      .join(' — ');

    return (
      '<div style=\"min-width:310px;line-height:1.38\">' +
      '<b style=\"font-size:14px\">' + esc(p.canonical_name) + '</b><br>' +
      '<b>Map label:</b> ' + esc(p.lbl) + '<br>' +
      '<b>Aliases:</b> ' + esc(p.aliases || '(none recorded)') + '<br>' +
      '<b>Parent system:</b> ' + esc(p.parent_system || '(unassigned)') + '<br>' +
      '<b>Project:</b> ' + esc(projectText || '(unassigned)') + '<br>' +
      '<b>Ownership:</b> ' + esc(p.ownership_class) + '<br>' +
      '<b>Owner:</b> ' + esc(p.owner_agency || '(not attributed)') + '<br>' +
      '<b>Operator:</b> ' + esc(p.operator_agency || '(not attributed)') + '<br>' +
      '<b>Type / role:</b> ' + esc(p.facility_type) + ' / ' + esc(p.network_role) + '<br>' +
      '<b>Display rank:</b> ' + esc(p.display_rank) + '<br>' +
      '<b>Segment length:</b> ' + Number(p.length_mi || 0).toFixed(2) + ' mi' +
      '<hr style=\"margin:7px 0 5px;border:0;border-top:1px solid #ddd\">' +
      '<span style=\"color:#555\"><b>QA/QC:</b> Derived from ' +
      esc(p.geometry_source || 'source conveyance data') +
      '; geometry handling: ' + esc(p.geometry_decision || 'source retained') +
      '; confidence: ' + esc(p.geometry_confidence || 'not rated') +
      '.</span>' +
      '</div>'
    );
  }

  L.geoJSON(segmentsData, {
    style: function(feature) {
      return styleFor(feature.properties || {});
    },
    onEachFeature: function(feature, layer) {
      var p = feature.properties || {};
      layer.bindPopup(popupHtml(p));
      layer.bindTooltip(esc(p.canonical_name), {
        sticky: true,
        opacity: 0.92
      });
      allSegments.push({layer: layer, properties: p});
    }
  });

  L.geoJSON(labelsData, {
    pointToLayer: function(feature, latlng) {
      var p = feature.properties || {};

      var marker = L.circleMarker(latlng, {
        radius: 1,
        stroke: false,
        fill: false,
        opacity: 0,
        fillOpacity: 0,
        interactive: false
      });

      marker.bindTooltip(esc(p.lbl), {
        permanent: true,
        direction: 'center',
        className: 'brim-conveyance-label-tooltip',
        opacity: 1,
        interactive: false
      });

      allLabels.push({layer: marker, properties: p});
      return marker;
    }
  });

  function selectedValues(filterName) {
    return Array.prototype.slice.call(
      document.querySelectorAll(
        '#brim-conveyance-panel input[data-filter=\"' + filterName + '\"]:checked'
      )
    ).map(function(input) {
      return input.value;
    });
  }

  function tokenIntersects(value, selected) {
    if (selected.length === 0) return false;
    var valueTokens = tokens(value);
    return valueTokens.some(function(v) {
      return selected.indexOf(v) >= 0;
    });
  }

  function matches(p) {
    var ownership = selectedValues('ownership');
    var projectFamilies = selectedValues('project_family');
    var facilityGroups = selectedValues('facility_group');
    var ranks = selectedValues('display_rank');

    var projectSelect = document.getElementById('brim-project-select');
    var projectValue = projectSelect ? projectSelect.value : '__ALL__';

    var search = document.getElementById('brim-search');
    var searchValue = search ? search.value.trim().toLowerCase() : '';

    var lowToggle = document.getElementById('brim-low-toggle');
    var includeLow = lowToggle ? lowToggle.checked : true;

    if (!tokenIntersects(p.ownership_class, ownership)) return false;
    if (!tokenIntersects(p.project_family, projectFamilies)) return false;
    if (!tokenIntersects(p.facility_group, facilityGroups)) return false;
    if (!tokenIntersects(p.display_rank, ranks)) return false;

    if (
      projectValue !== '__ALL__' &&
      tokens(p.project_name).indexOf(projectValue) < 0
    ) {
      return false;
    }

    if (!includeLow && p.geometry_confidence === 'low') return false;

    if (
      searchValue.length > 0 &&
      String(p.search_text || '').indexOf(searchValue) < 0
    ) {
      return false;
    }

    return true;
  }

  function refresh() {
    visibleSegments.clearLayers();
    visibleLabels.clearLayers();

    var visibleCount = 0;
    var visibleFacilityIds = {};

    allSegments.forEach(function(item) {
      if (matches(item.properties)) {
        item.layer.setStyle(styleFor(item.properties));
        visibleSegments.addLayer(item.layer);
        visibleCount += 1;
        visibleFacilityIds[item.properties.facility_id] = true;
      }
    });

    var labelToggle = document.getElementById('brim-label-toggle');
    var labelsOn = labelToggle ? labelToggle.checked : false;
    var currentZoom = map.getZoom();

    var visibleLabelCount = 0;

    if (labelsOn) {
      allLabels
        .slice()
        .sort(function(a, b) {
          return Number(b.properties.lbl_priority || 0) -
                 Number(a.properties.lbl_priority || 0);
        })
        .forEach(function(item) {
          var p = item.properties;
          if (
            visibleFacilityIds[p.facility_id] &&
            matches(p) &&
            p.show_default !== false &&
            String(p.show_default).toLowerCase() !== 'false' &&
            currentZoom >= Number(p.lbl_min_zoom || 0) &&
            currentZoom <= Number(p.lbl_max_zoom || 20)
          ) {
            visibleLabels.addLayer(item.layer);
            visibleLabelCount += 1;
          }
        });
    }

    var count = document.getElementById('brim-count');
    if (count) {
      count.textContent =
        visibleCount + ' visible segments / ' +
        Object.keys(visibleFacilityIds).length + ' facilities; ' +
        visibleLabelCount + ' labels at zoom ' + currentZoom;
    }
  }

  Array.prototype.slice.call(
    document.querySelectorAll('#brim-conveyance-panel input')
  ).forEach(function(input) {
    input.addEventListener('change', refresh);
  });

  var projectSelect = document.getElementById('brim-project-select');
  if (projectSelect) projectSelect.addEventListener('change', refresh);

  var search = document.getElementById('brim-search');
  if (search) search.addEventListener('input', refresh);

  map.on('zoomend', refresh);

  setTimeout(refresh, 0);
}
"

m <- htmlwidgets::onRender(
  m,
  js_code,
  data = widget_data
)

htmlwidgets::saveWidget(
  m,
  file = HTML_PATH,
  selfcontained = TRUE,
  title = "BRIM conveyance pipeline pilot"
)

