function(el, x, data) {
  var map = this;
  window.BRIM = window.BRIM || {};

  function clean(value) {
    value = String(value == null ? '' : value).replace(/\s+/g, ' ').trim();
    return value && !/^(na|nan|null|undefined)$/i.test(value) ? value : '';
  }

  function esc(value) {
    return clean(value)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  function norm(value) {
    return clean(value).toLowerCase().replace(/[^a-z0-9]+/g, ' ').trim();
  }

  function firstValue(object, fields) {
    object = object || {};
    fields = Array.isArray(fields) ? fields : [fields];
    for (var i = 0; i < fields.length; i += 1) {
      if (clean(object[fields[i]])) return object[fields[i]];
    }
    return '';
  }

  function splitValues(value) {
    value = clean(value);
    if (!value) return [];
    return value.split(/\s*;\s*/).map(clean).filter(function(item, index, all) {
      return item && all.indexOf(item) === index;
    });
  }

  function hashString(value) {
    var string = String(value || '');
    var hash = 2166136261;
    for (var i = 0; i < string.length; i += 1) {
      hash ^= string.charCodeAt(i);
      hash += (hash << 1) + (hash << 4) + (hash << 7) +
        (hash << 8) + (hash << 24);
    }
    return hash >>> 0;
  }

  var californiaCounties = [
    'Alameda', 'Alpine', 'Amador', 'Butte', 'Calaveras', 'Colusa',
    'Contra Costa', 'Del Norte', 'El Dorado', 'Fresno', 'Glenn', 'Humboldt',
    'Imperial', 'Inyo', 'Kern', 'Kings', 'Lake', 'Lassen', 'Los Angeles',
    'Madera', 'Marin', 'Mariposa', 'Mendocino', 'Merced', 'Modoc', 'Mono',
    'Monterey', 'Napa', 'Nevada', 'Orange', 'Placer', 'Plumas', 'Riverside',
    'Sacramento', 'San Benito', 'San Bernardino', 'San Diego',
    'San Francisco', 'San Joaquin', 'San Luis Obispo', 'San Mateo',
    'Santa Barbara', 'Santa Clara', 'Santa Cruz', 'Shasta', 'Sierra',
    'Siskiyou', 'Solano', 'Sonoma', 'Stanislaus', 'Sutter', 'Tehama',
    'Trinity', 'Tulare', 'Tuolumne', 'Ventura', 'Yolo', 'Yuba'
  ];
  var countyLookup = {};
  californiaCounties.forEach(function(county) {
    countyLookup[county.toLowerCase()] = county;
  });

  function normalizeCounties(value) {
    value = clean(value);
    if (!value) return [];
    var parts = value
      .replace(/\bcount(?:y|ies)\b/gi, '')
      .replace(/\s+(?:and|&)\s+/gi, ';')
      .replace(/[,;/|]+/g, ';')
      .split(';')
      .map(clean)
      .filter(Boolean);
    var matched = parts.map(function(part) {
      return countyLookup[part.toLowerCase()] || '';
    });
    if (!parts.length || matched.some(function(county) { return !county; })) {
      return [];
    }
    return matched.filter(function(county, index, all) {
      return all.indexOf(county) === index;
    });
  }

  function yearValue(value) {
    if (value === null || value === undefined || value === '') return null;
    var numeric = Number(value);
    if (isFinite(numeric) && Math.abs(numeric) > 10000000000) {
      var epochYear = new Date(numeric).getUTCFullYear();
      return isFinite(epochYear) ? epochYear : null;
    }
    var match = clean(value).match(/(18|19|20)\d{2}/);
    if (match) return parseInt(match[0], 10);
    var parsed = Date.parse(value);
    return isNaN(parsed) ? null : new Date(parsed).getUTCFullYear();
  }

  function pacificTime(value) {
    value = clean(value);
    if (!value) return 'time not recorded';
    var date = new Date(value);
    if (isNaN(date.getTime())) return value;
    try {
      return new Intl.DateTimeFormat('en-US', {
        timeZone: 'America/Los_Angeles',
        month: 'short',
        day: 'numeric',
        hour: 'numeric',
        minute: '2-digit'
      }).format(date) + ' PT';
    } catch (ignoreIntl) {
      return value.replace('T', ' ').replace(/Z$/, ' UTC');
    }
  }

  /*
   * UIC production styling is centralized here. The External Layers renderer
   * calls the public featureStyle(), pointStyle(), and catalogColor() methods
   * below, while this explorer uses the same definitions for swatches,
   * filtering, label restoration, and the overlap-distinction mode.
   */
  var definitions = [
    {
      catalog_id: 'UIC_EPA_LIVE',
      source_key: 'uic_epa_live',
      display_name: 'EPA mapped boundaries',
      formal_name: 'EPA Aquifer Exemption Data mapped boundaries',
      geometry_type: 'polygon',
      type_label: 'Polygon boundaries',
      color: '#2166ac',
      fill_color: '#67a9cf',
      weight: 1.6,
      fill_opacity: 0.026,
      supports_labels: true,
      min_label_zoom: 8,
      status: 'authoritative EPA FeatureServer'
    },
    {
      catalog_id: 'UIC_CALGEM_POST_LIVE',
      source_key: 'uic_calgem_post_live',
      display_name: 'CalGEM post-primacy approvals',
      formal_name: 'CalGEM Post-Primacy Aquifer Exemptions',
      geometry_type: 'polygon',
      type_label: 'Polygon boundaries',
      color: '#b35806',
      fill_color: '#fdb863',
      weight: 1.8,
      fill_opacity: 0.024,
      supports_labels: true,
      min_label_zoom: 8,
      status: 'authoritative CalGEM FeatureServer'
    },
    {
      catalog_id: 'UIC_CALGEM_PRIMACY_LIVE',
      source_key: 'uic_calgem_primacy_live',
      display_name: 'CalGEM 1983 historic subset',
      formal_name: 'CalGEM 1983 Primacy Aquifer Exemptions shaded subset',
      geometry_type: 'polygon',
      type_label: 'Historic polygon subset',
      color: '#762a83',
      fill_color: '#c2a5cf',
      weight: 1.7,
      fill_opacity: 0.020,
      supports_labels: true,
      min_label_zoom: 8,
      status: 'authoritative CalGEM FeatureServer · partial historic subset'
    },
    {
      catalog_id: 'UIC_EPA_REFERENCE_POINTS',
      source_key: 'uic_epa_reference_points',
      display_name: 'EPA exemption reference points',
      formal_name: 'EPA Aquifer Exemption Data centroids',
      geometry_type: 'point',
      type_label: 'Reference points · centroid/locator · not wells',
      color: '#0b5a7a',
      fill_color: '#56b4e9',
      weight: 1.3,
      fill_opacity: 0.82,
      radius: 4.4,
      supports_labels: false,
      status: 'authoritative EPA FeatureServer · overview locators'
    }
  ];

  var definitionById = {};
  definitions.forEach(function(definition) {
    definition.records = [];
    definition.recordById = {};
    definition.active = false;
    definition.loading = false;
    definition.error = '';
    definition.labelsOn = false;
    definitionById[definition.catalog_id] = definition;
    definitionById[definition.source_key] = definition;
  });

  var distinctionPalette = [
    '#0072b2', '#e69f00', '#009e73',
    '#cc79a7', '#56b4e9', '#d55e00'
  ];

  function definitionFor(id) {
    return definitionById[clean(id)] ||
      definitionById[clean(id).toUpperCase()] || null;
  }

  function sourceId(definition, props, fallback) {
    props = props || {};
    if (definition.source_key === 'uic_epa_live') {
      return clean(firstValue(props, ['ID_1', 'OBJECTID', 'FID'])) || fallback;
    }
    if (definition.source_key === 'uic_epa_reference_points') {
      return clean(firstValue(props, ['ID', 'OBJECTID', 'FID'])) || fallback;
    }
    if (definition.source_key === 'uic_calgem_post_live') {
      return clean(firstValue(props, ['ID', 'OBJECTID', 'FID'])) || fallback;
    }
    var historicId = clean(firstValue(
      props,
      ['OBJECTID_1', 'OBJECTID', 'FID']
    )) || fallback;
    return 'CalGEM_PRIMACY_' + historicId;
  }

  function baselineFilters() {
    return {
      fields: [],
      counties: [],
      formations: [],
      yearFrom: '',
      yearTo: ''
    };
  }

  function baselineOpenState() {
    return {
      advanced: false,
      field: false,
      county: false,
      year: false,
      formation: false
    };
  }

  var state = {
    filters: baselineFilters(),
    draft: null,
    auto: true,
    dirty: false,
    distinguish: false,
    timer: null,
    legend: null,
    div: null,
    initialized: false,
    hiddenByClose: false,
    noticeDismissed: false,
    open: baselineOpenState(),
    findQuery: '',
    findMessage: ''
  };

  function cloneFilters(filters) {
    return {
      fields: (filters.fields || []).slice(),
      counties: (filters.counties || []).slice(),
      formations: (filters.formations || []).slice(),
      yearFrom: clean(filters.yearFrom),
      yearTo: clean(filters.yearTo)
    };
  }
  state.draft = cloneFilters(state.filters);

  function sameFilters(left, right) {
    function sameArray(a, b) {
      a = (a || []).map(norm).sort();
      b = (b || []).map(norm).sort();
      return a.length === b.length && a.every(function(item, index) {
        return item === b[index];
      });
    }
    return sameArray(left.fields, right.fields) &&
      sameArray(left.counties, right.counties) &&
      sameArray(left.formations, right.formations) &&
      clean(left.yearFrom) === clean(right.yearFrom) &&
      clean(left.yearTo) === clean(right.yearTo);
  }

  function polygonStyle(definition, recordId, distinguished) {
    var useDistinction = distinguished === undefined ?
      state.distinguish : !!distinguished;
    var fill = definition.fill_color;
    var fillOpacity = Number(definition.fill_opacity || 0.02);
    if (useDistinction) {
      fill = distinctionPalette[
        hashString(definition.source_key + '|' + clean(recordId)) %
          distinctionPalette.length
      ];
      fillOpacity = 0.27;
    }
    return {
      pane: 'pane_pt_custom_polygon',
      color: definition.color,
      weight: Number(definition.weight || 1.6),
      opacity: 0.96,
      dashArray: null,
      fillColor: fill,
      fillOpacity: fillOpacity
    };
  }

  function referencePointStyle(definition) {
    return {
      pane: 'pane_pt_custom_point',
      radius: Number(definition.radius || 4.4),
      color: definition.color,
      weight: Number(definition.weight || 1.3),
      opacity: 0.96,
      fillColor: definition.fill_color,
      fillOpacity: Number(definition.fill_opacity || 0.82)
    };
  }

  function featureStyle(options, feature) {
    options = options || {};
    var definition = definitionFor(
      options.catalogExtId || options.external_layer_id ||
      options.defaultStyleMethod
    );
    if (!definition) return null;
    if (definition.geometry_type === 'point') {
      return referencePointStyle(definition);
    }
    var props = feature && feature.properties ? feature.properties : {};
    return polygonStyle(definition, sourceId(definition, props, ''));
  }

  function pointStyle(options) {
    options = options || {};
    var definition = definitionFor(
      options.catalogExtId || options.external_layer_id ||
      options.defaultStyleMethod
    );
    return definition && definition.geometry_type === 'point' ?
      referencePointStyle(definition) : null;
  }

  function hoverStyle(options, feature) {
    var style = featureStyle(options, feature);
    if (!style) return null;
    var definition = definitionFor(
      options.catalogExtId || options.external_layer_id ||
      options.defaultStyleMethod
    );
    if (!definition) return style;
    style.weight = Number(style.weight || definition.weight || 1.3) +
      (definition.geometry_type === 'point' ? 1.0 : 1.4);
    style.opacity = 1;
    style.fillOpacity = definition.geometry_type === 'point' ?
      Math.max(Number(style.fillOpacity || 0), 0.92) :
      Math.max(Number(style.fillOpacity || 0), 0.14);
    if (definition.geometry_type === 'point') {
      style.radius = Number(style.radius || definition.radius || 4.4) + 1.6;
    }
    return style;
  }

  function recordBaseStyle(record) {
    return record._dataset.geometry_type === 'point' ?
      referencePointStyle(record._dataset) :
      polygonStyle(record._dataset, record.source_id);
  }

  function applyRecordStyle(record) {
    var layer = record && record._layer;
    if (!layer || !layer.setStyle) return;
    var collection = record._dataset && record._dataset.layer;
    if (record._filteredOut) {
      if (collection && collection.hasLayer && collection.removeLayer &&
          collection.hasLayer(layer)) {
        collection.removeLayer(layer);
      }
      return;
    }
    if (collection && collection.hasLayer && collection.addLayer &&
        !collection.hasLayer(layer)) {
      collection.addLayer(layer);
    }
    layer.setStyle(recordBaseStyle(record));
  }

  function ensureCss() {
    if (document.getElementById('brim-uic-external-explorer-css')) return;
    var style = document.createElement('style');
    style.id = 'brim-uic-external-explorer-css';
    style.textContent =
      '.pt-uic-explorer{width:370px;max-width:calc(100vw - 20px);box-sizing:border-box;overflow-x:clip;border:1px solid rgba(72,117,145,.76);border-radius:6px;box-shadow:0 2px 10px rgba(0,0,0,.30);padding:0;background:rgba(225,240,251,.97);color:#18384c;font:11px/1.22 Arial,Helvetica,sans-serif}' +
      '.pt-uic-explorer.pt-map-card-undocked{max-height:calc(100vh - 16px);overflow-y:auto;overscroll-behavior:contain}.pt-uic-head{display:flex;align-items:flex-start;justify-content:space-between;gap:8px;padding:6px 8px 5px;border-bottom:1px solid rgba(72,117,145,.30);background:rgba(204,226,238,.74)}.pt-uic-title{font-size:12.5px;font-weight:700}.pt-uic-subtitle{font-size:9.5px;color:#486779;font-weight:400}' +
      '.pt-uic-body{padding:5px 7px 7px;background:rgba(225,240,251,.94)}.pt-uic-warning{display:flex;align-items:flex-start;gap:5px;padding:4px 5px;margin:0 0 4px;border:1px solid #d2b958;border-radius:4px;background:rgba(255,247,210,.96);color:#4c421f;font-size:9.7px}.pt-uic-warning span{flex:1;min-width:0}.pt-uic-warning button{flex:0 0 auto;border:0;background:transparent;color:#746520;padding:0 1px;cursor:pointer;font:bold 13px/1 Arial,sans-serif}' +
      '.pt-uic-source{display:grid;grid-template-columns:27px minmax(0,1fr) auto;gap:2px 5px;align-items:center;margin:3px 0;padding:4px 5px;border:1px solid rgba(72,117,145,.20);border-radius:4px;background:rgba(255,255,255,.60)}.pt-uic-source-off{opacity:.58}.pt-uic-source-name{min-width:0;font-weight:700;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}.pt-uic-source-type{font-size:9px;color:#587487;font-weight:400;overflow:hidden;text-overflow:ellipsis;white-space:nowrap}.pt-uic-source-count{font-size:9.5px;font-variant-numeric:tabular-nums;white-space:nowrap}.pt-uic-source-actions{grid-column:2 / 4;display:flex;gap:3px;align-items:center;min-width:0;white-space:nowrap;color:#526d7e;font-size:9.2px}.pt-uic-source-retrieved{margin-right:auto;overflow:hidden;text-overflow:ellipsis}' +
      '.pt-uic-line-swatch{display:inline-block;width:24px;height:9px;border-top-style:solid;box-sizing:border-box}.pt-uic-point-swatch{display:inline-block;width:9px;height:9px;margin-left:7px;border-radius:50%;box-sizing:border-box}' +
      '.pt-uic-source-actions button,.pt-uic-tools button,.pt-uic-any,.pt-uic-find button{appearance:none;-webkit-appearance:none;padding:2px 5px;border:1px solid rgba(72,117,145,.72);border-radius:4px;box-shadow:0 1px 2px rgba(0,0,0,.10);background:rgba(255,255,255,.94);color:#214b64;cursor:pointer;font:9.7px/1.1 Arial,Helvetica,sans-serif}.pt-uic-source-actions button:hover,.pt-uic-tools button:hover,.pt-uic-any:hover,.pt-uic-find button:hover{background:rgba(218,236,245,.98);border-color:#365f78}.pt-uic-source-actions button:focus-visible,.pt-uic-tools button:focus-visible,.pt-uic-any:focus-visible,.pt-uic-find button:focus-visible{outline:2px solid #255e9b;outline-offset:1px}' +
      '.pt-uic-combined{padding:3px 1px 1px;color:#4d6879;font-size:9.5px;font-variant-numeric:tabular-nums}.pt-uic-section{margin-top:5px;padding-top:4px;border-top:1px solid rgba(45,99,130,.20)}.pt-uic-section-title{display:flex;justify-content:space-between;align-items:center;margin-bottom:3px;font-weight:700}' +
      '.pt-uic-find{display:grid;grid-template-columns:minmax(0,1fr) auto;gap:4px}.pt-uic-find input,.pt-uic-facet input,.pt-uic-facet select,.pt-uic-years input{width:100%;min-width:0;box-sizing:border-box;padding:3px;border:1px solid #91a9b8;border-radius:3px;background:#fff;font:10.3px Arial,Helvetica,sans-serif}.pt-uic-find-note{min-height:11px;margin-top:2px;color:#567181;font-size:9.3px;overflow-wrap:anywhere}' +
      '.pt-uic-chips{display:flex;gap:3px;flex-wrap:wrap;margin:2px 0 4px}.pt-uic-chip{display:inline-flex;gap:3px;align-items:center;max-width:100%;padding:2px 3px 2px 6px;border:1px solid #7694a7;border-radius:10px;background:#fff;font-size:9.3px}.pt-uic-chip span{overflow:hidden;text-overflow:ellipsis;white-space:nowrap}.pt-uic-chip button{border:0;background:transparent;padding:0 2px;color:#31566c;cursor:pointer}.pt-uic-no-chips{color:#718592;font-size:9px}' +
      '.pt-uic-facet{margin-top:4px;border:1px solid #aac2d1;border-radius:4px;background:rgba(255,255,255,.52)}.pt-uic-facet>summary{display:flex;gap:6px;padding:4px 5px;cursor:pointer;list-style:none;font-size:10px;font-weight:700}.pt-uic-facet>summary::-webkit-details-marker{display:none}.pt-uic-facet>summary:after{content:"▾";margin-left:auto}.pt-uic-facet[open]>summary:after{content:"▴"}.pt-uic-facet-body{padding:0 4px 4px}.pt-uic-facet select{height:88px}.pt-uic-facet-search{margin-bottom:2px}.pt-uic-facet-count{color:#647e8d;font-weight:400}.pt-uic-advanced>summary{margin-top:5px;cursor:pointer;font-weight:700}.pt-uic-years{display:grid;grid-template-columns:1fr 1fr;gap:4px}.pt-uic-year-wrap{padding:0 4px 4px}' +
      '.pt-uic-tools{display:flex;gap:4px;align-items:center;flex-wrap:nowrap;margin-top:6px}.pt-uic-tools button{white-space:nowrap}.pt-uic-tools button[data-uic-action="zoom"]{margin-left:auto}.pt-uic-auto{display:inline-flex;gap:2px;align-items:center;font-size:9.7px}.pt-uic-auto input{margin:0}.pt-uic-help{margin-top:3px;color:#637c8b;font-size:9px}' +
      '.pt-uic-display-row{display:flex;align-items:center;gap:7px;min-width:0}.pt-uic-distinguish{display:inline-flex;gap:4px;align-items:center;white-space:nowrap}.pt-uic-distinguish input{margin:0}.pt-uic-distinguish-key{display:inline-flex;gap:2px;align-items:center;flex-wrap:nowrap}.pt-uic-distinguish-key i{display:inline-block;width:13px;height:7px;border:1px solid rgba(0,0,0,.22)}.pt-uic-distinguish-note{margin-top:2px;color:#526d7e;font-size:9px}' +
      '.pt-uic-map-label{padding:1px 3px!important;border:0!important;box-shadow:none!important;background:rgba(255,255,255,.75)!important;color:#18384c!important;font:9px Arial,sans-serif!important;white-space:nowrap!important}.pt-uic-overlap{max-height:330px;overflow:auto}.pt-uic-overlap-row{padding:5px 0;border-bottom:1px solid #ddd}.pt-uic-overlap-row button{border:0;background:none;color:#165a9b;text-decoration:underline;padding:0;cursor:pointer;text-align:left}' +
      '@media(max-width:700px){.pt-uic-explorer{width:min(370px,calc(100vw - 16px))}.pt-uic-explorer.pt-map-card-undocked{max-height:calc(100vh - 12px)}}' +
      '@media(max-height:720px){.pt-uic-body{padding-top:4px;padding-bottom:5px}.pt-uic-source{margin:2px 0;padding-top:3px;padding-bottom:3px}.pt-uic-section{margin-top:4px;padding-top:3px}}';
    document.head.appendChild(style);
  }

  // This asset is the production owner of External UIC explorer styling.
  // Inject it once when the onRender callback runs, before any source can
  // create or reveal the explorer.
  ensureCss();

  function prepareRecord(definition, layer, props, index) {
    props = props || {};
    var historic = definition.source_key === 'uic_calgem_primacy_live';
    var epa = definition.source_key === 'uic_epa_live' ||
      definition.source_key === 'uic_epa_reference_points';
    var post = definition.source_key === 'uic_calgem_post_live';
    var id = sourceId(definition, props, String(index));
    var field = epa ? props.Injection_Well_ID :
      (post ? firstValue(props, ['Field_Labe', 'Name']) : props.FieldName);
    var formation = post ? props.Formation : '';
    var zone = epa ? props.Injection_Zone :
      (post ? firstValue(props, ['Zone', 'Inj_Zone', 'Zone_Label']) :
        Array.from({length: 18}, function(_, position) {
          return props['FormZone' + (position + 1)];
        }).map(clean).filter(Boolean).join('; '));
    var date = historic ? '' : (epa ? props.Decision_Date : props.Approve);
    var countyValues = historic ? [] : normalizeCounties(props.County);
    var bounds = layer && layer.getBounds ? layer.getBounds() : null;
    if (!bounds && layer && layer.getLatLng) {
      var latlng = layer.getLatLng();
      bounds = L.latLngBounds(latlng, latlng);
    }
    var formationDisplay = clean([formation, zone].map(clean).filter(Boolean).join(' — '));
    var record = {
      layer_id: 'pt-uic-external::' + definition.source_key + '::' + id,
      source_id: id,
      source_family: epa ? 'EPA' : 'CalGEM',
      field_project: clean(field),
      county: countyValues,
      formation: splitValues(formationDisplay),
      decision_year: yearValue(date),
      formation_display: formationDisplay,
      label_text: clean(field || zone || id),
      historic: historic,
      _dataset: definition,
      _layer: layer,
      _properties: props,
      _bounds: bounds && bounds.isValid && bounds.isValid() ? bounds : null,
      _filteredOut: false
    };
    record.field = splitValues(record.field_project);
    record.search = {
      field: norm(record.field_project),
      county: norm(record.county.join(' ')),
      formation: norm(record.formation.join(' ')),
      id: norm(record.source_id)
    };
    record.search.all = [
      record.search.field,
      record.search.county,
      record.search.formation,
      record.search.id
    ].join(' ');
    layer._ptUicRecord = record;
    if (definition.geometry_type === 'polygon' && layer.on &&
        !layer._ptUicOverlapClickBound) {
      layer._ptUicOverlapClickBound = true;
      layer.on('click', showOverlap);
    }
    return record;
  }

  function activeDefinitions() {
    return definitions.filter(function(definition) {
      return definition.active;
    });
  }

  function allActiveRecords() {
    var records = [];
    activeDefinitions().forEach(function(definition) {
      records = records.concat(definition.records || []);
    });
    return records;
  }

  function selectedMatch(selected, values) {
    if (!selected.length) return true;
    var lookup = {};
    selected.forEach(function(value) { lookup[norm(value)] = true; });
    return (values || []).some(function(value) { return lookup[norm(value)]; });
  }

  function recordMatches(record, filters, skip) {
    if (skip !== 'field' && !selectedMatch(filters.fields, record.field)) {
      return false;
    }
    if (skip !== 'county' && !selectedMatch(filters.counties, record.county)) {
      return false;
    }
    if (skip !== 'formation' &&
        !selectedMatch(filters.formations, record.formation)) {
      return false;
    }
    if (filters.yearFrom &&
        (!isFinite(record.decision_year) ||
          record.decision_year < parseInt(filters.yearFrom, 10))) {
      return false;
    }
    if (filters.yearTo &&
        (!isFinite(record.decision_year) ||
          record.decision_year > parseInt(filters.yearTo, 10))) {
      return false;
    }
    return true;
  }

  function applyFilters(options) {
    options = options || {};
    var matches = [];
    allActiveRecords().forEach(function(record) {
      var match = recordMatches(record, state.filters, '');
      record._filteredOut = !match;
      if (record._layer && record._layer.options) {
        record._layer.options.interactive = match;
      }
      if (record._layer && record._layer._path) {
        record._layer._path.style.pointerEvents = match ? '' : 'none';
      }
      if (!match && record._layer && record._layer.closePopup) {
        record._layer.closePopup();
      }
      applyRecordStyle(record);
      if (match) matches.push(record);
    });
    updateLabels();
    render();
    if (options.fit && matches.length) fitRecords(matches);
  }

  function connectedOptions(field) {
    var values = {};
    allActiveRecords().forEach(function(record) {
      if (!recordMatches(record, state.draft, field)) return;
      (record[field] || []).forEach(function(value) {
        var key = norm(value);
        if (!key) return;
        if (!values[key]) values[key] = {value: value, count: 0};
        values[key].count += 1;
      });
    });
    var selected = field === 'field' ? state.draft.fields :
      (field === 'county' ? state.draft.counties : state.draft.formations);
    selected.forEach(function(value) {
      var key = norm(value);
      if (!values[key]) values[key] = {value: value, count: 0};
    });
    return Object.keys(values).map(function(key) {
      return values[key];
    }).sort(function(left, right) {
      return left.value.localeCompare(right.value, undefined, {
        numeric: true,
        sensitivity: 'base'
      });
    });
  }

  function fitRecords(records) {
    var bounds = null;
    (records || []).forEach(function(record) {
      if (!record._bounds) return;
      if (!bounds) bounds = L.latLngBounds(record._bounds);
      else bounds.extend(record._bounds);
    });
    if (bounds && bounds.isValid()) {
      map.fitBounds(bounds, {padding: [28, 28], maxZoom: 13, animate: false});
    }
  }

  function commitDraft(fit) {
    state.filters = cloneFilters(state.draft);
    state.dirty = false;
    applyFilters({fit: !!fit});
  }

  function scheduleApply() {
    state.dirty = !sameFilters(state.filters, state.draft);
    clearTimeout(state.timer);
    if (!state.auto || !state.dirty) {
      render();
      return;
    }
    state.timer = setTimeout(function() {
      commitDraft(false);
    }, 180);
  }

  function resetFilters() {
    state.filters = baselineFilters();
    state.draft = cloneFilters(state.filters);
    state.dirty = false;
    applyFilters({fit: false});
  }

  function sourceSwatch(definition) {
    if (definition.geometry_type === 'point') {
      return '<span class="pt-uic-point-swatch" style="background:' +
        esc(definition.fill_color) + ';border:' +
        esc(definition.weight) + 'px solid ' + esc(definition.color) + '"></span>';
    }
    return '<span class="pt-uic-line-swatch" style="border-top-width:' +
      esc(definition.weight) + 'px;border-top-color:' +
      esc(definition.color) + ';background:' +
      esc(definition.fill_color) + '"></span>';
  }

  function sourceRowsHtml() {
    return definitions.map(function(definition) {
      var total = (definition.records || []).length ||
        Number(definition.feature_count || 0);
      var shown = definition.active ? (definition.records || []).filter(
        function(record) { return !record._filteredOut; }
      ).length : 0;
      var count = definition.active ?
        shown + ' / ' + total :
        'Off';
      var countTitle = '';
      if (definition.loading) {
        count = 'Loading…';
        countTitle = clean(definition.loadingMessage);
      }
      if (definition.error) {
        count = 'Error';
        countTitle = clean(definition.error);
      }
      var actions = '';
      if (definition.active) {
        actions = '<div class="pt-uic-source-actions"><span class="pt-uic-source-retrieved">retrieved ' +
          esc(pacificTime(definition.retrieval_utc)) + '</span>';
        if (definition.supports_labels) {
          actions += '<button type="button" data-uic-action="labels" data-source="' +
            esc(definition.source_key) + '">' +
            (definition.labelsOn ? 'lbl on' : 'lbl') + '</button>';
        }
        actions += '<button type="button" data-uic-action="refresh" data-source="' +
          esc(definition.source_key) + '">Refresh</button>' +
          '<button type="button" data-uic-action="clear-source" data-source="' +
          esc(definition.source_key) + '">Clear</button></div>';
      }
      return '<div class="pt-uic-source ' +
        (definition.active || definition.loading ? '' : 'pt-uic-source-off') +
        '">' + sourceSwatch(definition) +
        '<div class="pt-uic-source-name" title="' +
        esc(definition.formal_name) + '">' + esc(definition.display_name) +
        '<div class="pt-uic-source-type">' + esc(definition.type_label) +
        '</div></div><div class="pt-uic-source-count"' +
        (countTitle ? ' title="' + esc(countTitle) + '"' : '') + '>' + esc(count) +
        '</div>' + actions + '</div>';
    }).join('');
  }

  function facetHtml(field, label, open) {
    return '<details class="pt-uic-facet" data-uic-facet="' + field +
      '"' + (open ? ' open' : '') + '><summary><span>' + esc(label) +
      '</span><span class="pt-uic-facet-count" data-facet-count="' + field +
      '"></span></summary><div class="pt-uic-facet-body">' +
      '<input class="pt-uic-facet-search" type="search" data-option-search="' +
      field + '" placeholder="Search options…">' +
      '<select multiple data-filter="' + field + '" aria-label="' +
      esc(label) + ' filter"></select>' +
      '<button type="button" class="pt-uic-any" data-any="' + field +
      '">Any</button></div></details>';
  }

  function chipsHtml() {
    var chips = [];
    function add(field, label, values) {
      (values || []).forEach(function(value) {
        chips.push('<span class="pt-uic-chip"><span>' + esc(label) + ': ' +
          esc(value) + '</span><button type="button" data-chip-field="' +
          field + '" data-chip-value="' + esc(value) +
          '" aria-label="Remove filter">×</button></span>');
      });
    }
    add('field', 'Field', state.draft.fields);
    add('county', 'County', state.draft.counties);
    add('formation', 'Formation', state.draft.formations);
    if (state.draft.yearFrom || state.draft.yearTo) {
      chips.push('<span class="pt-uic-chip"><span>Year: ' +
        esc(state.draft.yearFrom || 'Any') + '–' +
        esc(state.draft.yearTo || 'Any') +
        '</span><button type="button" data-chip-field="year" ' +
        'data-chip-value="year" aria-label="Remove year filter">×</button></span>');
    }
    return chips.length ? chips.join('') :
      '<span class="pt-uic-no-chips">No active filters</span>';
  }

  function distinctionKeyHtml() {
    if (!state.distinguish) return '';
    return '<div class="pt-uic-distinguish-key">' +
      distinctionPalette.map(function(color) {
        return '<i style="background:' + esc(color) + '"></i>';
      }).join('') + '</div>';
  }

  function panelHtml() {
    var actions = window.BRIM.legendCloseout &&
      window.BRIM.legendCloseout.actionsHtml ?
      window.BRIM.legendCloseout.actionsHtml(
        'pt-uic-external-dock',
        'pt-uic-external-close',
        'External UIC Explorer'
      ) :
      '<button type="button" data-uic-action="close">×</button>';
    var active = activeDefinitions();
    var shown = allActiveRecords().filter(function(record) {
      return !record._filteredOut;
    }).length;
    var total = allActiveRecords().length;
    return '<div class="pt-uic-head pt-map-card-handle"><div>' +
      '<div class="pt-uic-title">External UIC Explorer</div>' +
      '<div class="pt-uic-subtitle">Authoritative services · fetched on demand</div>' +
      '</div>' + actions + '</div><div class="pt-uic-body">' +
      (state.noticeDismissed ? '' :
        '<div class="pt-uic-warning"><span>Surface footprints only; limits may ' +
        'vary by formation, zone, depth, or structure.</span><button type="button" ' +
        'data-uic-action="dismiss-notice" aria-label="Dismiss interpretation ' +
        'notice" title="Dismiss for this page session">×</button></div>') +
      '<div data-source-rows>' + sourceRowsHtml() + '</div>' +
      '<div class="pt-uic-combined">' + shown + ' / ' + total +
      ' shown · ' + active.length + ' active source row' +
      (active.length === 1 ? '' : 's') + '</div>' +
      '<div class="pt-uic-section"><div class="pt-uic-section-title">Find records</div>' +
      '<div class="pt-uic-find"><input type="search" data-find ' +
      'placeholder="Search field, formation, county, or ID…">' +
      '<button type="button" data-uic-action="find">Find</button></div>' +
      '<div class="pt-uic-find-note" data-find-note>' +
      esc(state.findMessage) + '</div></div>' +
      '<div class="pt-uic-section"><div class="pt-uic-section-title"><span>Filters</span>' +
      '<label class="pt-uic-auto"><input type="checkbox" data-auto ' +
      (state.auto ? 'checked' : '') + '> Auto</label></div>' +
      '<div class="pt-uic-chips" data-chips>' + chipsHtml() + '</div>' +
      facetHtml('field', 'Field / project', state.open.field) +
      '<details class="pt-uic-advanced"' +
      (state.open.advanced ? ' open' : '') + '><summary>Advanced</summary>' +
      facetHtml('county', 'County', state.open.county) +
      '<details class="pt-uic-facet" data-uic-facet="year"' +
      (state.open.year ? ' open' : '') + '><summary>' +
      '<span>Decision year</span></summary><div class="pt-uic-year-wrap">' +
      '<div class="pt-uic-years"><input type="number" data-year="from" ' +
      'placeholder="From" value="' + esc(state.draft.yearFrom) + '">' +
      '<input type="number" data-year="to" placeholder="To" value="' +
      esc(state.draft.yearTo) + '"></div><button type="button" ' +
      'class="pt-uic-any" data-any="year">Any</button></div></details>' +
      facetHtml('formation', 'Formation / zone', state.open.formation) +
      '</details><div class="pt-uic-tools">' +
      '<button type="button" data-uic-action="apply">Apply</button>' +
      '<button type="button" data-uic-action="reset">Reset</button>' +
      '<button type="button" data-uic-action="zoom">Zoom to results</button>' +
      '</div>' +
      '<div class="pt-uic-help">Empty = Any · OR within a filter · AND across filters.' +
      (state.dirty ? ' Staged changes are not yet applied.' : '') + '</div></div>' +
      '<div class="pt-uic-section"><div class="pt-uic-section-title">Map display</div>' +
      '<div class="pt-uic-display-row"><label class="pt-uic-distinguish">' +
      '<input type="checkbox" data-distinguish ' +
      (state.distinguish ? 'checked' : '') + '><span>Distinguish overlaps</span></label>' +
      distinctionKeyHtml() + '</div>' +
      (state.distinguish ?
        '<div class="pt-uic-distinguish-note">Colors repeat; no category meaning.</div>' :
        '') + '</div></div>';
  }

  function selectedValues(field) {
    return field === 'field' ? state.draft.fields :
      (field === 'county' ? state.draft.counties : state.draft.formations);
  }

  function setSelectedValues(field, values) {
    if (field === 'field') state.draft.fields = values;
    if (field === 'county') state.draft.counties = values;
    if (field === 'formation') state.draft.formations = values;
  }

  function renderFacet(field) {
    if (!state.div) return;
    var details = state.div.querySelector('[data-uic-facet="' + field + '"]');
    if (!details) return;
    var select = details.querySelector('[data-filter="' + field + '"]');
    if (!select) return;
    var search = details.querySelector('[data-option-search="' + field + '"]');
    var query = norm(search && search.value);
    var selected = selectedValues(field).map(norm);
    var options = connectedOptions(field).filter(function(option) {
      return !query || norm(option.value).indexOf(query) >= 0 ||
        selected.indexOf(norm(option.value)) >= 0;
    });
    select.innerHTML = options.map(function(option) {
      return '<option value="' + esc(option.value) + '"' +
        (selected.indexOf(norm(option.value)) >= 0 ? ' selected' : '') +
        '>' + esc(option.value) + ' (' + option.count + ')</option>';
    }).join('');
    var count = details.querySelector('[data-facet-count="' + field + '"]');
    if (count) count.textContent = options.length + ' options';
  }

  function anyEligible() {
    return definitions.some(function(definition) {
      return definition.active || definition.loading || definition.error;
    });
  }

  function restoreCachedRecordsToBaseline() {
    definitions.forEach(function(definition) {
      (definition.records || []).forEach(function(record) {
        record._filteredOut = false;
        if (record._layer && record._layer.options) {
          record._layer.options.interactive = true;
        }
        if (record._layer && record._layer._path) {
          record._layer._path.style.pointerEvents = '';
        }
        applyRecordStyle(record);
      });
    });
    updateLabels();
  }

  function resetTransientExplorerState() {
    clearTimeout(state.timer);
    state.timer = null;
    state.filters = baselineFilters();
    state.draft = cloneFilters(state.filters);
    state.auto = true;
    state.dirty = false;
    state.distinguish = false;
    state.hiddenByClose = false;
    state.open = baselineOpenState();
    state.findQuery = '';
    state.findMessage = '';
    restoreCachedRecordsToBaseline();
  }

  function destroyExplorerControl() {
    if (!state.initialized) return;
    if (state.div && state.div.__brimDetachableState &&
        state.div.__brimDetachableState.destroy) {
      state.div.__brimDetachableState.destroy(false);
    }
    if (state.legend && map.removeControl) {
      map.removeControl(state.legend);
    } else if (state.legend && state.legend.remove) {
      state.legend.remove();
    }
    state.legend = null;
    state.div = null;
    state.initialized = false;
  }

  function endExplorerLifecycleIfIdle() {
    if (anyEligible()) return false;
    resetTransientExplorerState();
    destroyExplorerControl();
    return true;
  }

  function captureOpenState() {
    if (!state.div) return;
    var advanced = state.div.querySelector('.pt-uic-advanced');
    if (advanced) state.open.advanced = !!advanced.open;
    ['field', 'county', 'year', 'formation'].forEach(function(field) {
      var details = state.div.querySelector(
        '[data-uic-facet="' + field + '"]'
      );
      if (details) state.open[field] = !!details.open;
    });
  }

  function render() {
    if (!state.initialized || !state.div) return;
    var shouldShow = anyEligible() && !state.hiddenByClose;
    state.div.style.display = shouldShow ? '' : 'none';
    if (!shouldShow) return;
    captureOpenState();
    state.div.innerHTML = panelHtml();
    ['field', 'county', 'formation'].forEach(function(field) {
      var details = state.div.querySelector(
        '[data-uic-facet="' + field + '"]'
      );
      if (details && details.open) renderFacet(field);
    });
    var findInput = state.div.querySelector('[data-find]');
    if (findInput) findInput.value = state.findQuery;
  }

  function initialize() {
    if (state.initialized) return;
    state.initialized = true;
    state.legend = L.control({position: 'bottomleft'});
    state.legend.onAdd = function() {
      state.div = L.DomUtil.create(
        'div',
        'leaflet-control pt-uic-explorer pt-map-legend-card pt-map-legend-external'
      );
      state.div.style.display = 'none';
      L.DomEvent.disableClickPropagation(state.div);
      L.DomEvent.disableScrollPropagation(state.div);
      return state.div;
    };
    state.legend.addTo(map);
    bindUi();
    render();
  }

  function updateAllStyles() {
    definitions.forEach(function(definition) {
      (definition.records || []).forEach(applyRecordStyle);
    });
  }

  function toggleLabels(sourceKey) {
    var definition = definitionFor(sourceKey);
    if (!definition || !definition.supports_labels || !definition.layer) return;
    definition.labelsOn = !definition.labelsOn;
    updateLabels();
    render();
  }

  function updateLabels() {
    var zoom = map.getZoom();
    definitions.forEach(function(definition) {
      if (!definition.supports_labels) return;
      if (!definition.labelLayer) definition.labelLayer = L.layerGroup();
      definition.labelLayer.clearLayers();
      if (!definition.active || !definition.labelsOn ||
          zoom < Number(definition.min_label_zoom || 8)) {
        if (map.hasLayer(definition.labelLayer)) {
          map.removeLayer(definition.labelLayer);
        }
        return;
      }
      definition.records.forEach(function(record) {
        if (record._filteredOut || !record._bounds || !record.label_text) return;
        var marker = L.marker(record._bounds.getCenter(), {
          interactive: false,
          icon: L.divIcon({
            className: 'pt-uic-map-label',
            html: esc(record.label_text),
            iconSize: null
          })
        });
        definition.labelLayer.addLayer(marker);
      });
      if (!map.hasLayer(definition.labelLayer)) definition.labelLayer.addTo(map);
    });
  }

  function findRecords() {
    var input = state.div && state.div.querySelector('[data-find]');
    state.findQuery = clean(input && input.value);
    var query = norm(state.findQuery);
    if (!query) {
      state.findMessage = 'Enter a field, formation, county, or source ID.';
      render();
      return;
    }
    var hits = allActiveRecords().filter(function(record) {
      return record.search.all.indexOf(query) >= 0;
    });
    if (!hits.length) {
      state.findMessage = 'No active UIC source rows match “' +
        state.findQuery + '”.';
      render();
      return;
    }
    var categoryCounts = [
      ['Field/project', 'field'],
      ['County', 'county'],
      ['Formation/zone', 'formation'],
      ['Source ID', 'id']
    ].map(function(category) {
      return [
        category[0],
        hits.filter(function(record) {
          return record.search[category[1]].indexOf(query) >= 0;
        }).length
      ];
    }).filter(function(category) {
      return category[1] > 0;
    });
    state.findMessage = hits.length + ' match' + (hits.length === 1 ? '' : 'es') +
      ' · ' + categoryCounts.map(function(category) {
        return category[0] + ' ' + category[1];
      }).join(' · ') + ' · zoomed without changing filters.';
    fitRecords(hits);
    render();
  }

  function removeChip(field, value) {
    if (field === 'year') {
      state.draft.yearFrom = '';
      state.draft.yearTo = '';
    } else {
      var values = selectedValues(field).filter(function(item) {
        return norm(item) !== norm(value);
      });
      setSelectedValues(field, values);
    }
    scheduleApply();
  }

  function bridgeAction(action, sourceKey) {
    var bridge = window.BRIM.uicExternalBridge;
    if (!bridge) return;
    if (action === 'refresh' && bridge.refreshBySourceKey) {
      bridge.refreshBySourceKey(sourceKey);
    }
    if (action === 'clear-source' && bridge.removeBySourceKey) {
      bridge.removeBySourceKey(sourceKey);
    }
  }

  function bindUi() {
    if (!state.div) return;
    state.div.addEventListener('click', function(event) {
      var actionNode = event.target.closest(
        '[data-uic-action],.pt-uic-external-close'
      );
      var anyNode = event.target.closest('[data-any]');
      var chipNode = event.target.closest('[data-chip-field]');
      if (chipNode) {
        event.preventDefault();
        removeChip(
          chipNode.getAttribute('data-chip-field'),
          chipNode.getAttribute('data-chip-value')
        );
        return;
      }
      if (anyNode) {
        event.preventDefault();
        var field = anyNode.getAttribute('data-any');
        if (field === 'year') {
          state.draft.yearFrom = '';
          state.draft.yearTo = '';
        } else {
          setSelectedValues(field, []);
        }
        scheduleApply();
        return;
      }
      if (!actionNode) return;
      event.preventDefault();
      var action = actionNode.classList.contains('pt-uic-external-close') ?
        'close' : actionNode.getAttribute('data-uic-action');
      var sourceKey = actionNode.getAttribute('data-source');
      if (action === 'apply') commitDraft(true);
      if (action === 'reset') resetFilters();
      if (action === 'zoom') {
        fitRecords(allActiveRecords().filter(function(record) {
          return !record._filteredOut;
        }));
      }
      if (action === 'find') findRecords();
      if (action === 'labels') toggleLabels(sourceKey);
      if (action === 'refresh' || action === 'clear-source') {
        bridgeAction(action, sourceKey);
      }
      if (action === 'close') {
        state.hiddenByClose = true;
        render();
      }
      if (action === 'dismiss-notice') {
        state.noticeDismissed = true;
        render();
      }
    });
    state.div.addEventListener('change', function(event) {
      var select = event.target.closest('[data-filter]');
      if (select) {
        var values = Array.prototype.slice.call(select.options)
          .filter(function(option) { return option.selected; })
          .map(function(option) { return option.value; });
        setSelectedValues(select.getAttribute('data-filter'), values);
        scheduleApply();
        return;
      }
      if (event.target.matches('[data-year]')) {
        var from = state.div.querySelector('[data-year="from"]');
        var to = state.div.querySelector('[data-year="to"]');
        state.draft.yearFrom = clean(from && from.value);
        state.draft.yearTo = clean(to && to.value);
        scheduleApply();
        return;
      }
      if (event.target.matches('[data-auto]')) {
        state.auto = !!event.target.checked;
        scheduleApply();
        return;
      }
      if (event.target.matches('[data-distinguish]')) {
        state.distinguish = !!event.target.checked;
        updateAllStyles();
        render();
      }
    });
    state.div.addEventListener('input', function(event) {
      if (!event.target.matches('[data-option-search]')) return;
      renderFacet(event.target.getAttribute('data-option-search'));
    });
    state.div.addEventListener('toggle', function(event) {
      var details = event.target;
      if (!details || details.tagName !== 'DETAILS') return;
      if (details.matches('.pt-uic-advanced')) {
        state.open.advanced = !!details.open;
        return;
      }
      var field = details.getAttribute('data-uic-facet');
      if (!field) return;
      state.open[field] = !!details.open;
      if (details.open && field !== 'year') renderFacet(field);
    }, true);
    state.div.addEventListener('keydown', function(event) {
      if (!event.target.matches('[data-find]') || event.key !== 'Enter') return;
      event.preventDefault();
      findRecords();
    });
  }

  function buildOverlapIndex(definition) {
    if (definition.geometry_type !== 'polygon') return;
    var grid = {};
    var fallback = [];
    definition.records.forEach(function(record) {
      var bounds = record._bounds;
      if (!bounds || !bounds.isValid()) {
        fallback.push(record);
        return;
      }
      var west = Math.floor(bounds.getWest());
      var east = Math.floor(bounds.getEast());
      var south = Math.floor(bounds.getSouth());
      var north = Math.floor(bounds.getNorth());
      for (var lng = west; lng <= east; lng += 1) {
        for (var lat = south; lat <= north; lat += 1) {
          var key = lng + ':' + lat;
          if (!grid[key]) grid[key] = [];
          grid[key].push(record);
        }
      }
    });
    definition.overlapCandidates = function(latlng) {
      return (grid[
        Math.floor(latlng.lng) + ':' + Math.floor(latlng.lat)
      ] || []).concat(fallback);
    };
  }

  function overlapHits(latlng, originalEvent) {
    var point = map.latLngToLayerPoint(latlng);
    var paintedPaths = [];
    if (originalEvent && document.elementsFromPoint &&
        isFinite(originalEvent.clientX) && isFinite(originalEvent.clientY)) {
      paintedPaths = document.elementsFromPoint(
        originalEvent.clientX,
        originalEvent.clientY
      ).filter(function(node) {
        return node && node.matches &&
          node.matches('.leaflet-pane_pt_custom_polygon-pane path');
      });
    }
    var hits = [];
    activeDefinitions().filter(function(definition) {
      return definition.geometry_type === 'polygon';
    }).forEach(function(definition) {
      var records = definition.overlapCandidates ?
        definition.overlapCandidates(latlng) : definition.records;
      records.forEach(function(record) {
        if (record._filteredOut || !record._layer) return;
        var collection = record._dataset && record._dataset.layer;
        var recordIsVisible = collection && collection.hasLayer ?
          collection.hasLayer(record._layer) : map.hasLayer(record._layer);
        if (!recordIsVisible) return;
        var hit = !!(
          record._layer._path &&
          paintedPaths.indexOf(record._layer._path) >= 0
        );
        if (typeof record._layer._containsPoint === 'function') {
          try {
            hit = hit || record._layer._containsPoint(point);
          } catch (ignoreContainsPoint) {}
        }
        if (!hit && record._bounds) hit = record._bounds.contains(latlng);
        if (hit) hits.push(record);
      });
    });
    return hits;
  }

  function showOverlap(event) {
    if (!state.distinguish || map._ptMeasureInteractionActive ||
        map._ptDrawInteractionActive) return;
    var originalEvent = event && event.originalEvent;
    if (originalEvent && originalEvent._ptUicOverlapHandled) return;
    var hits = overlapHits(event.latlng, originalEvent);
    if (hits.length < 2) return;
    if (originalEvent) originalEvent._ptUicOverlapHandled = true;
    hits.sort(function(left, right) {
      return (left._dataset.display_name + left.source_id).localeCompare(
        right._dataset.display_name + right.source_id
      );
    });
    var rows = hits.map(function(record) {
      return '<div class="pt-uic-overlap-row"><b>' +
        esc(record._dataset.display_name) + '</b><br>' +
        '<button type="button" data-open-record="' + esc(record.layer_id) + '">' +
        esc(record.field_project || record.formation_display ||
          record.source_id || 'UIC record') + '</button>' +
        '<div style="font-size:10px;color:#555">' +
        esc(record.formation_display || record.source_id) + '</div></div>';
    }).join('');
    var popup = L.popup({maxWidth: 440})
      .setLatLng(event.latlng)
      .setContent('<div class="pt-uic-overlap"><strong>' + hits.length +
        ' polygon records at this location</strong><div style="font-size:10px;' +
        'margin:2px 0 5px">Record fills repeat and have no category meaning. ' +
        'Choose a record for its source popup.</div>' + rows + '</div>')
      .openOn(map);
    setTimeout(function() {
      var node = popup.getElement ? popup.getElement() : null;
      if (!node) return;
      node.querySelectorAll('[data-open-record]').forEach(function(button) {
        button.addEventListener('click', function() {
          openRecord(button.getAttribute('data-open-record'));
        });
      });
    }, 0);
  }

  function openRecord(layerId) {
    definitions.some(function(definition) {
      var record = definition.recordById[layerId];
      if (!record || !record._layer) return false;
      if (record._bounds) {
        map.fitBounds(record._bounds, {
          padding: [28, 28],
          maxZoom: 13,
          animate: false
        });
      }
      if (record._layer.openPopup) record._layer.openPopup();
      return true;
    });
  }

  window.BRIM.uicExplorer = {
    definitions: definitions,
    isUicCatalogId: function(id) {
      return !!definitionFor(id);
    },
    catalogColor: function(id) {
      var definition = definitionFor(id);
      return definition ? definition.color : '';
    },
    featureStyle: featureStyle,
    pointStyle: pointStyle,
    hoverStyle: hoverStyle,
    setExternalLoading: function(id, loading, message) {
      var definition = definitionFor(id);
      if (!definition) return;
      definition.loading = !!loading;
      definition.loadingMessage = clean(message);
      if (loading) {
        definition.error = '';
        state.hiddenByClose = false;
      }
      initialize();
      render();
    },
    setExternalError: function(id, message) {
      var definition = definitionFor(id);
      if (!definition) return;
      definition.loading = false;
      definition.error = clean(message) || 'Live retrieval failed.';
      initialize();
      render();
    },
    upsertExternal: function(record) {
      record = record || {};
      var id = record.catalogExtId || record.catalog_id ||
        (record.options && record.options.catalogExtId);
      var definition = definitionFor(id);
      if (!definition) return;
      if (definition.labelLayer && map.hasLayer(definition.labelLayer)) {
        map.removeLayer(definition.labelLayer);
      }
      definition.layer = record.layer;
      definition.recordId = record.id;
      definition.active = record.visible !== false;
      definition.loading = false;
      definition.error = '';
      definition.retrieval_utc = record.retrievalUtc ||
        (record.options && record.options.retrievalUtc) ||
        new Date().toISOString();
      definition.records = [];
      definition.recordById = {};
      definition.labelsOn = false;
      if (record.layer && record.layer.eachLayer) {
        var index = 0;
        record.layer.eachLayer(function(layer) {
          if (!layer || !layer.feature) return;
          index += 1;
          var normalized = prepareRecord(
            definition,
            layer,
            layer.feature.properties || {},
            index
          );
          definition.records.push(normalized);
          definition.recordById[normalized.layer_id] = normalized;
        });
      }
      definition.feature_count = definition.records.length;
      buildOverlapIndex(definition);
      state.hiddenByClose = false;
      if (!definition.active && endExplorerLifecycleIfIdle()) return;
      initialize();
      applyFilters({fit: false});
    },
    removeExternal: function(id) {
      var definition = definitionFor(id);
      if (!definition) {
        definitions.some(function(item) {
          if (item.recordId === id) {
            definition = item;
            return true;
          }
          return false;
        });
      }
      if (!definition) return;
      if (definition.labelLayer && map.hasLayer(definition.labelLayer)) {
        map.removeLayer(definition.labelLayer);
      }
      definition.layer = null;
      definition.recordId = null;
      definition.records = [];
      definition.recordById = {};
      definition.feature_count = 0;
      definition.active = false;
      definition.loading = false;
      definition.error = '';
      definition.labelsOn = false;
      if (!endExplorerLifecycleIfIdle()) render();
    },
    setExternalVisible: function(id, visible) {
      var definition = definitionFor(id);
      if (!definition) {
        definitions.some(function(item) {
          if (item.recordId === id) {
            definition = item;
            return true;
          }
          return false;
        });
      }
      if (!definition) return;
      definition.active = !!visible;
      if (!visible && endExplorerLifecycleIfIdle()) return;
      if (visible) {
        state.hiddenByClose = false;
        initialize();
      }
      applyFilters({fit: false});
    },
    toggleExternalLabels: function(id) {
      var definition = definitionFor(id);
      if (definition) toggleLabels(definition.source_key);
    },
    openRecord: openRecord,
    snapshot: function() {
      return {
        production_mode: 'external_only',
        initialized: state.initialized,
        auto: state.auto,
        filters: cloneFilters(state.filters),
        draft: cloneFilters(state.draft),
        dirty: state.dirty,
        distinguish_overlaps: state.distinguish,
        find_query: state.findQuery,
        find_message: state.findMessage,
        open: {
          advanced: state.open.advanced,
          field: state.open.field,
          county: state.open.county,
          year: state.open.year,
          formation: state.open.formation
        },
        notice_dismissed: state.noticeDismissed,
        sources: definitions.map(function(definition) {
          return {
            source_key: definition.source_key,
            geometry_type: definition.geometry_type,
            active: definition.active,
            records: definition.records.length,
            labels_on: definition.labelsOn
          };
        })
      };
    }
  };

  map.on('click', showOverlap);
  map.on('zoomend', updateLabels);
}
