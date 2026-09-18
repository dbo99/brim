# ==== leaflet_ops_live_legend_helpers.r ====================================
##
## PURPOSE:
##   Active Ops Live legend/overlay-note rendering helper.
##
## DESIGN:
##   This file is sourced by `leaflet_ops_live_helpers.r`.
##   It returns browser-side JavaScript as text for injection into the Ops Live
##   htmlwidgets/onRender function. Keep edits narrow and feature-specific.
## ============================================================================

pt_ops_live_legend_helpers_js <- function() {

  r"---(
  var ptOpsMapLegendControl = null;
  var ptOpsMapLegendDiv = null;
  var ptOpsMapLegendHiddenTypes = {};

  // Published RFC legend presentation, reviewed 2026-09-11 (32/56) and 2026-09-14 (40).
  // Source: https://mapservices.weather.noaa.gov/raster/rest/services/obs/rfc_qpe/MapServer/legend?f=pjson
  // Original label/PNG hashes and independent decoded RGBA evidence live in
  // qa/test_ops_radar_qpe_contracts.js. Preserve each product's own scale.
  // Solid 20px PNGs contain a 16px opaque interior with a 2px transparent margin;
  // the lowest class is wholly transparent. This describes legend PNGs only.
  var ptOpsRfcQpeLegends = {
    'ops_qpe_rfc_1day': {
      title: 'RFC mosaic · Daily QPE',
      period: 'Daily analysis: one 24-hour accumulation, not a rolling last-24-hour product.',
      rows: [
        ["Greater than or equal to 10", '#dcdcdc'],
        ["8 to 10", '#7d4be1'],
        ["6 to 8", '#fa00fa'],
        ["5 to 6", '#7d0000'],
        ["4 to 5", '#af0000'],
        ["3 to 4", '#fa0000'],
        ["2.5 to 3", '#fa9600'],
        ["2 to 2.5", '#ffd966'],
        ["1.5 to 2", '#fafa00'],
        ["1 to 1.5", '#00640a'],
        ["0.75 to 1", '#00a00f'],
        ["0.5 to 0.75", '#00fa14'],
        ["0.25 to 0.5", '#001432'],
        ["0.1  to  0.25", '#3d85c6'],
        ["0.01 to 0.1", '#14c8fa'],
        ["Less than 0.01", 'transparent'],
        ["Missing data", '#7d7d7d']
      ]
    },
    'ops_qpe_rfc_3day': {
      title: 'RFC mosaic · 3-day QPE',
      period: 'Three-day accumulation: sum of three daily 24-hour analyses.',
      rows: [
        ["Greater than or equal to 20", '#dbdbdb'],
        ["15 to 20", '#7c4ae0'],
        ["10 to 15", '#fa00fa'],
        ["8 to 10", '#7d0000'],
        ["6 to 8", '#b00000'],
        ["5 to 6", '#fa0000'],
        ["4 to 5", '#fa9600'],
        ["3 to 4", '#ffd966'],
        ["2 to 3", '#fafa00'],
        ["1.5 to 2", '#00630a'],
        ["1 to 1.5", '#00a110'],
        ["0.5 to 1", '#00fa15'],
        ["0.25 to 0.5", '#001496'],
        ["0.1 to 0.25", '#3e87c7'],
        ["0.01 to 0.1", '#14c8fa'],
        ["Less than 0.01", 'transparent'],
        ["Missing Data", '#7d7d7d']
      ]
    },
    'ops_qpe_rfc_7day': {
      title: 'RFC mosaic · 7-day QPE',
      period: 'Seven-day accumulation: sum of seven daily 24-hour analyses.',
      rows: [
        ["Greater than or equal to 20", '#dbdbdb'],
        ["15 to 20", '#7c4ae0'],
        ["10 to 15", '#fa00fa'],
        ["8 to 10", '#7d0000'],
        ["6 to 8", '#b00000'],
        ["5 to 6", '#fa0000'],
        ["4 to 5", '#fa9600'],
        ["3 to 4", '#ffd966'],
        ["2 to 3", '#fafa00'],
        ["1.5 to 2", '#00630a'],
        ["1 to 1.5", '#00a110'],
        ["0.5 to 1", '#00fa15'],
        ["0.25 to 0.5", '#001496'],
        ["0.1 to 0.25", '#3e87c7'],
        ["0.01 to 0.1", '#14c8fa'],
        ["Less than 0.01", 'transparent'],
        ["Missing Data", '#7d7d7d']
      ]
    }
  };

  // Views belong to activeLegendDefs registration objects, not freshness/status.
  // Retaining the node retains its current activation's hidden and docking state.
  var ptOpsRfcQpeCards = {};

  function ptOpsRfcQpeRowHtml(row, label) {
    // Authored scalars only; retain the exact provider wording separately.
    var color = row[1];
    if (!/^(#[0-9a-f]{6}|transparent)$/.test(color)) throw new Error('Invalid authored RFC legend color');
    return '<div class="pt-ops-rfc-qpe-row" title="' + escapeHtml(row[0]) + '">' +
      '<span class="pt-ops-rfc-qpe-swatch" aria-hidden="true"><span class="pt-ops-rfc-qpe-color" style="background:' +
      color + '"></span></span><span class="pt-ops-rfc-qpe-label" aria-label="' + escapeHtml(row[0]) + '">' +
      escapeHtml(label) + '</span></div>';
  }

  function ptOpsRfcQpeCardHtml(def) {
    var metadata = ptOpsRfcQpeMetadata(def);
    var numeric = [], missing = [];
    def.rows.forEach(function(row) {
      var label = row[0].replace(/\s+/g, ' ').trim(), match;
      // Omit only the published transparent threshold from the primary key.
      // It remains in the authored provider classification; no raster changes.
      if (label === 'Less than 0.01') return;
      if (/^missing data$/i.test(label)) { missing.push(row); return; }
      match = /^(\d+(?:\.\d+)?) to (\d+(?:\.\d+)?)$/.exec(label);
      if (match) {
        numeric.push({row: row, lower: Number(match[1]), label: match[1] + '–' + match[2]});
        return;
      }
      match = /^Greater than or equal to (\d+(?:\.\d+)?)$/.exec(label);
      if (!match) throw new Error('Unrecognized authored RFC legend class');
      numeric.push({row: row, lower: Number(match[1]), label: '≥' + match[1]});
    });
    numeric.sort(function(a, b) { return a.lower - b.lower; });
    var html = '<div class="pt-map-card-handle pt-ops-rfc-qpe-head"><h4>' +
      escapeHtml(def.title) + '</h4>' +
      window.BRIM.legendCloseout.actionsHtml('pt-ops-rfc-qpe-dock', 'pt-ops-rfc-qpe-close', escapeHtml(def.title)) +
      '</div><div class="pt-ops-rfc-qpe-interval">Displayed accumulation interval: ' + escapeHtml(metadata.interval) + '</div>' +
      '<div class="pt-ops-rfc-qpe-units">Precipitation (inches)</div><div class="pt-ops-rfc-qpe-scale">';
    numeric.forEach(function(item) { html += ptOpsRfcQpeRowHtml(item.row, item.label); });
    html += '</div><div class="pt-ops-rfc-qpe-missing">';
    missing.forEach(function(row) { html += ptOpsRfcQpeRowHtml(row, 'Missing data'); });
    return html + '</div><div class="pt-ops-rfc-qpe-note">Below 0.01 in omitted from this key.</div>' +
      '<details class="pt-ops-rfc-qpe-method"><summary>Source, method &amp; timing</summary>' +
      metadata.fields.map(function(field) {
        return '<div><b>' + escapeHtml(field.label) + ':</b> <span data-rfc-qpe-metadata="' +
          escapeHtml(field.key) + '">' + escapeHtml(field.value) + '</span></div>';
      }).join('') + '</details>';
  }

  function ptOpsUpdateRfcQpeMetadata() {
    // Update text in mounted cards only: never remount, reopen or rebind them.
    Object.keys(ptOpsRfcQpeCards).forEach(function(id) {
      var card = ptOpsRfcQpeCards[id].card;
      if (!card || !card.parentNode) return;
      ptOpsRfcQpeMetadata(ptOpsRfcQpeLegends[id]).fields.forEach(function(field) {
        var value = card.querySelector('[data-rfc-qpe-metadata="' + field.key + '"]');
        if (value) value.textContent = field.value;
      });
    });
  }


  function ptOpsSyncRfcQpeLegends() {
    var wanted = {};
    Object.keys(activeLegendDefs).forEach(function(name) {
      var owner = activeLegendDefs[name], id = owner.rfcQpeProductId;
      if (Object.prototype.hasOwnProperty.call(ptOpsRfcQpeLegends, id)) wanted[id] = owner;
    });
    ptOpsSyncLegendCards(ptOpsRfcQpeCards, wanted, {
      title: function(id) { return ptOpsRfcQpeLegends[id].title; },
      html: function(id) { return ptOpsRfcQpeCardHtml(ptOpsRfcQpeLegends[id]); },
      className: 'pt-ops-rfc-qpe-card pt-map-legend-card',
      attribute: 'data-rfc-qpe-product-id',
      dockSelector: '.pt-ops-rfc-qpe-dock', closeSelector: '.pt-ops-rfc-qpe-close'
    });
  }

  // One existing registration-owned lifecycle serves both presentation families.
  function ptOpsSyncLegendCards(views, wanted, presentation) {
    Object.keys(views).forEach(function(id) {
      var view = views[id];
      if (wanted[id] === view.owner) return;
      view.detachable.destroy(false);
      view.control.remove();
      delete views[id];
    });
    Object.keys(wanted).forEach(function(id) {
      if (views[id]) return;
      var title = presentation.title(id);
      var control = L.control({position: 'bottomleft'});
      var card;
      control.onAdd = function() {
        card = L.DomUtil.create('div', presentation.className);
        card.setAttribute(presentation.attribute, id);
        card.setAttribute('role', 'group');
        card.setAttribute('aria-label', title);
        card.innerHTML = presentation.html(id);
        L.DomEvent.disableClickPropagation(card);
        L.DomEvent.disableScrollPropagation(card);
        return card;
      };
      control.addTo(map);
      var detachable = window.BRIM.legendCloseout.makeDetachable({
        card: card, map: map, handleSelector: '.pt-map-card-handle',
        dockSelector: presentation.dockSelector, label: title
      });
      window.BRIM.legendCloseout.wire(card, presentation.closeSelector, function() {
        // Return a floating card before hiding it; off/clear can then remove
        // its Leaflet control and destroy its shared drag/resize listeners.
        detachable.dock();
        window.BRIM.legendCloseout.scheduleLayout();
      });
      views[id] = {owner: wanted[id], control: control, detachable: detachable, card: card};
      window.BRIM.legendCloseout.scheduleLayout(card);
    });
  }


  var ptOpsForecastCards = {};
  // Source-owned scale derived from authenticated NOAA E37/E37R1 raw renderer + legend.
  // QPF selectors 1/2/3/9/11; ERO 0/1/2. Not a freshly checked live renderer claim.
  var ptWpcQpfScale = [{"value":0.0,"label":"0","rgba":[255,255,255,255]},{"value":0.01,"label":"0.01","rgba":[127,255,0,255]},{"value":0.1,"label":"0.10","rgba":[0,255,0,255]},{"value":0.25,"label":"0.25","rgba":[8,139,0,255]},{"value":0.5,"label":"0.50","rgba":[16,78,139,255]},{"value":0.75,"label":"0.75","rgba":[30,144,255,255]},{"value":1.0,"label":"1.00","rgba":[0,178,238,255]},{"value":1.25,"label":"1.25","rgba":[0,238,238,255]},{"value":1.5,"label":"1.50","rgba":[137,104,205,255]},{"value":1.75,"label":"1.75","rgba":[145,44,238,255]},{"value":2.0,"label":"2.00","rgba":[139,0,139,255]},{"value":2.5,"label":"2.50","rgba":[139,0,0,255]},{"value":3.0,"label":"3.00","rgba":[255,0,0,255]},{"value":4.0,"label":"4.00","rgba":[238,64,0,255]},{"value":5.0,"label":"5.00","rgba":[255,127,0,255]},{"value":7.0,"label":"7.00","rgba":[206,133,0,255]},{"value":10.0,"label":"10.00","rgba":[255,215,0,255]},{"value":15.0,"label":"15.00","rgba":[255,255,0,255]},{"value":20.0,"label":"20.00 (inches)","rgba":[255,192,183,255]}];
  var ptWpcEroScale = [{"value":1.0,"label":"Marginal (At Least 5%)","rgba":[56,168,0,255]},{"value":2.0,"label":"Slight (At Least 15%)","rgba":[255,254,0,255]},{"value":3.0,"label":"Moderate (At Least 40%)","rgba":[245,0,0,255]},{"value":4.0,"label":"High (At Least 70%)","rgba":[255,105,197,255]}];

  function ptOpsForecastFamily(id) {
    if (['ops_wpc_qpf_day_1','ops_wpc_qpf_day_2','ops_wpc_qpf_day_3','ops_wpc_qpf_3day','ops_wpc_qpf_7day'].indexOf(id)>=0) return 'qpf';
    if (['ops_wpc_ero_day_1','ops_wpc_ero_day_2','ops_wpc_ero_day_3'].indexOf(id)>=0) return 'ero';
    if (['ops_cpc_6_10_temperature','ops_cpc_6_10_precipitation','ops_cpc_8_14_temperature','ops_cpc_8_14_precipitation'].indexOf(id)>=0) return 'cpc';
    return null;
  }

  function ptOpsForecastScaleHtml(def) {
    var family = ptOpsForecastFamily(def.forecastProductId);
    var rows = family === 'qpf' ? ptWpcQpfScale : family === 'ero' ? ptWpcEroScale : def.forecastScale && def.forecastScale.entries;
    if (!rows) return 'BRIM display colors unavailable — Unverified';
    if (family === 'qpf') {
      function tick(r) {
        var label = r.value >= 2 && Number.isInteger(r.value) ? String(r.value) : r.label.replace(' (inches)','');
        return '<span class="pt-qpf-boundary-tick" aria-hidden="true">'+escapeHtml(label)+'</span>';
      }
      function boundary(r, kind) {
        var color = 'rgba('+r.rgba.slice(0,3).join(',')+','+(r.rgba[3]/255)+')';
        return '<span class="pt-qpf-contour-'+kind+'" data-qpf-boundary="'+r.value+'" role="listitem" aria-label="'+
          escapeHtml(r.value+' inches contour boundary')+'">'+
          '<span class="pt-ops-swatch" aria-hidden="true" style="background:'+color+'"></span>'+
          tick(r)+'</span>';
      }
      // Adjacent ticks communicate contour intervals, not endpoint inclusion.
      // Repeat the join tick visually; the ordered accessible list keeps each value once.
      // The final captured color is a boundary cap, not an asserted open-ended band.
      function strip(start, end, last) {
        return '<div class="pt-qpf-contour-strip" role="presentation">'+rows.slice(start,end).map(function(r) {
          return boundary(r,'band');
        }).join('')+(last ? boundary(rows[end],'cap') : '<span class="pt-qpf-contour-end" aria-hidden="true">'+tick(rows[end])+'</span>')+'</div>';
      }
      return '<div class="pt-qpf-contour-scale"><div role="list" aria-label="QPF contour boundaries in inches, low to high">'+
        '<div class="pt-qpf-contour-strips" role="presentation">'+strip(1,10,false)+strip(10,18,true)+'</div>'+
        '</div><div class="pt-qpf-contour-note">No color = no forecast precipitation</div></div>';
    }
    function row(r, label) {
      var color = r.color || 'rgba('+r.rgba.slice(0,3).join(',')+','+(r.rgba[3]/255)+')';
      return '<div class="pt-ops-legend-line" aria-label="'+escapeHtml(r.label)+'"><span class="pt-ops-swatch" aria-hidden="true" style="background:'+escapeHtml(color)+'"></span><span>'+escapeHtml(label || r.label)+'</span></div>';
    }
    if (family === 'cpc') {
      function group(title, predicate, short) {
        return '<section class="pt-forecast-cpc-group"><h5>'+title+'</h5>'+rows.filter(predicate).map(function(r) { return row(r,short ? r.label.replace(/^(Below|Above) normal /,'') : r.label); }).join('')+'</section>';
      }
      return '<div class="pt-forecast-cpc-groups">'+
        group('Below normal',function(r) { return /^Below normal /.test(r.label); },true)+
        group('Near normal',function(r) { return r.label === 'Near Normal'; },false)+
        group('Above normal',function(r) { return /^Above normal /.test(r.label); },true)+
        '</div><div class="pt-forecast-cpc-status">'+rows.filter(function(r) { return r.label === 'Equal Chances' || r.label === 'Unknown'; }).map(function(r) { return row(r); }).join('')+'</div>';
    }
    return '<div class="pt-forecast-risks">'+rows.map(function(r) {
      return row(r,r.label);
    }).join('')+'</div>';
  }

  function ptOpsForecastCardHtml(name, def) {
    var family = ptOpsForecastFamily(def.forecastProductId);
    var label = family === 'qpf' ? 'QPF contours (inches)' : family === 'ero' ? 'Excessive-rainfall risk' : 'Category probability (%)';
    var method = family === 'qpf' ? 'Captured NOAA contour values and colors; tick spacing is schematic. The unique-value renderer does not separately establish top-bin semantics. Zero is real zero; missing data is separate. The final color marks the captured 20 boundary. Polygon/contour attributes are not interpolated point forecasts.' :
      family === 'ero' ? 'Captured provider risk labels and BRIM mapped fills. Slight remains #fffe00; the captured provider legend image uses #ffff00. Unknown/missing is not confirmed Marginal; the existing map may use its green fallback for unknown values.' :
      'Percent is category probability, not percent-normal, amount, degrees, or probability of any rain. Keys match BRIM display colors, not a substituted provider palette. Near Normal, Equal Chances and Unknown are distinct even where BRIM uses the same gray. Unknown is not a valid forecast.';
    return '<div class="pt-map-card-handle pt-forecast-head"><h4>'+escapeHtml(name)+'</h4>'+window.BRIM.legendCloseout.actionsHtml('pt-forecast-dock','pt-forecast-close',escapeHtml(name))+'</div>'+
      '<div class="pt-forecast-valid"><b>Valid:</b> <span data-forecast-field="valid"></span></div>'+
      '<div class="pt-forecast-issued"><b>'+(family === 'cpc' ? 'Forecast date' : 'Issued')+':</b> <span data-forecast-field="issued"></span></div>'+
      '<b class="pt-forecast-scale-title">'+escapeHtml(label)+'</b><div class="pt-forecast-scale"></div>'+
      '<div class="pt-forecast-qualification">'+(family === 'cpc' ? 'BRIM display colors. ' : '')+'<span data-forecast-field="scope"></span></div>'+
      '<details><summary>Source, method &amp; timing</summary><div>'+escapeHtml(method)+'</div>'+
      (family === 'qpf' ? '<div>Legend uses provider boundary values; endpoint inclusion is not separately specified</div>'+
        '<div>Image and metadata cycle alignment is not independently confirmed</div>' : '')+
      '<div data-forecast-field="fullscope"></div><div>Issued / forecast date: <span data-forecast-field="rawissued"></span></div>'+
      '<div>Full valid interval: <span data-forecast-field="rawvalid"></span></div><div data-forecast-field="utc"></div>'+activeOverlayLinksHtml(def)+'</details>';
  }

  function ptOpsSizeForecastCards() {
    var size = map.getSize();
    Object.keys(ptOpsForecastCards).forEach(function(id) {
      var card = ptOpsForecastCards[id].card;
      card.style.maxWidth = Math.max(1,Math.min(360,size.x-24))+'px';
      card.style.setProperty('--pt-forecast-map-height',Math.max(1,size.y-24)+'px');
      card.classList.toggle('pt-forecast-narrow',size.x < 330);
    });
  }
  var ptForecastSizingBound = false;

  function ptOpsSyncForecastLegends() {
    var wanted = {}, names = {};
    Object.keys(activeLegendDefs).forEach(function(name) {
      var def = activeLegendDefs[name], id = def.forecastProductId;
      if (ptOpsForecastFamily(id)) { wanted[id] = def; names[id] = name; }
    });
    ptOpsSyncLegendCards(ptOpsForecastCards,wanted,{
      title:function(id) { return names[id]; }, html:function(id) { return ptOpsForecastCardHtml(names[id],wanted[id]); },
      className:'pt-map-legend-card pt-map-legend-external pt-ops-forecast-card', attribute:'data-forecast-product',
      dockSelector:'.pt-forecast-dock', closeSelector:'.pt-forecast-close'
    });
    Object.keys(ptOpsForecastCards).forEach(function(id) {
      var view = ptOpsForecastCards[id], def = view.owner;
      var metadata = def.forecastMetadata || ptForecastMetadata(ptOpsForecastFamily(id),[],false);
      var fields = ptForecastPresentation(ptOpsForecastFamily(id),metadata);
      fields.fullscope = ptOpsForecastFamily(id) === 'qpf' ?
        metadata.scope.split(';').filter(function(text) { return !/^\s*image-cycle match unverified\s*$/i.test(text); }).join(';') : metadata.scope;
      fields.rawissued = metadata.issued; fields.rawvalid = metadata.valid;
      Object.keys(fields).forEach(function(key) {
        var el = view.card.querySelector('[data-forecast-field="'+key+'"]');
        if (el && el.textContent !== fields[key]) el.textContent = fields[key];
      });
      var scale = ptOpsForecastScaleHtml(def);
      if (view.scale !== scale) { view.card.querySelector('.pt-forecast-scale').innerHTML = scale; view.scale = scale; }
    });
    var hasCards = Object.keys(ptOpsForecastCards).length > 0;
    if (hasCards !== ptForecastSizingBound) {
      map[hasCards ? 'on' : 'off']('resize',ptOpsSizeForecastCards);
      ptForecastSizingBound = hasCards;
    }
    ptOpsSizeForecastCards();
  }

  var ptOpsRadarMrmsCards = {};

  // BRIM semantic guide only; these colors do not encode provider thresholds.
  var ptOpsRadarGuide = {
    entries: [
      {label: 'Light', color: '#31d843'},
      {label: 'Moderate', color: '#fff04a'},
      {label: 'Heavy', color: '#ff9a26'},
      {label: 'Very heavy', color: '#e31a1c'},
      {label: 'Strongest echoes', color: '#d900ff'}
    ],
    note: 'General radar reflectivity guide — approximate. Provider palettes vary; this is not an exact dBZ or precipitation-rate scale.'
  };

  function ptOpsUsesRadarGuide(def) {
    return def.legendType === 'radar_iem' || def.legendType === 'radar_noaa';
  }

  function ptOpsRadarGuideHtml() {
    return '<div class="pt-ops-section pt-ops-radar-guide"><b>Radar reflectivity guide</b>' +
      ptOpsRadarGuide.entries.map(function(entry) {
        return '<div class="pt-ops-legend-line"><span class="pt-ops-swatch" aria-hidden="true" style="background:' +
          escapeHtml(entry.color) + '"></span>' + escapeHtml(entry.label) + '</div>';
      }).join('') + '<div class="pt-ops-muted">' + escapeHtml(ptOpsRadarGuide.note) + '</div></div>';
  }

  function ptOpsRadarMrmsMetadataHtml(def) {
    return ptOpsRadarMrmsMetadata(def).filter(function(field) {
      // IEM's former provider-image visual-key row no longer describes this UI.
      return !(def.legendType === 'radar_iem' && field.key === 'scale');
    }).map(function(field) {
      return '<div><b>' + escapeHtml(field.label) + ':</b> <span data-radar-mrms-field="' +
        field.key + '">' + escapeHtml(field.value) + '</span></div>';
    }).join('');
  }

  function ptOpsRadarMrmsCardHtml(name, def) {
    var html = '<div class="pt-map-card-handle pt-ops-radar-mrms-head"><h4>' + escapeHtml(name) + '</h4>' +
      window.BRIM.legendCloseout.actionsHtml('pt-ops-radar-mrms-dock', 'pt-ops-radar-mrms-close', escapeHtml(name)) + '</div>';
    if (ptOpsUsesRadarGuide(def)) html += ptOpsRadarGuideHtml();
    if (def.legendType === 'mrms_qpe') html += qpeLegendHtml('MRMS QPE approximate color guide');
    html += ptOpsRadarMrmsMetadataHtml(def);
    html += '<details><summary>Provider links</summary>';
    if (!ptOpsUsesRadarGuide(def)) {
      html += '<a href="' + escapeHtml(def.legendUrl) + '" target="_blank" rel="noopener noreferrer">' +
        'Provider whole-service legend — not a verified selected-product scale</a><br>';
    }
    return html + '<a href="' + escapeHtml(def.sourceUrl) +
      '" target="_blank" rel="noopener noreferrer">Provider source</a></details>';
  }

  function ptOpsUpdateRadarMrmsMetadata() {
    function update(node, def) {
      if (!node || !def) return;
      var fields = ptOpsRadarMrmsMetadata(def);
      if (!fields) return;
      fields.forEach(function(field) {
        var span = node.querySelector('[data-radar-mrms-field="' + field.key + '"]');
        if (span) span.textContent = field.value;
      });
    }
    Object.keys(ptOpsRadarMrmsCards).forEach(function(name) {
      var view = ptOpsRadarMrmsCards[name];
      if (activeLegendDefs[name] === view.owner) update(view.card, view.owner);
    });
    if (legendDiv) Array.prototype.forEach.call(legendDiv.querySelectorAll('[data-radar-mrms-name]'), function(node) {
      update(node, activeLegendDefs[node.getAttribute('data-radar-mrms-name')]);
    });
  }

  function ptOpsSyncRadarMrmsLegends() {
    var wanted = {};
    Object.keys(activeLegendDefs).forEach(function(name) {
      var owner = activeLegendDefs[name];
      if (ptOpsRadarMrmsMetadata(owner)) wanted[name] = owner;
    });
    ptOpsSyncLegendCards(ptOpsRadarMrmsCards, wanted, {
      title: function(name) { return name; },
      html: function(name) { return ptOpsRadarMrmsCardHtml(name, wanted[name]); },
      className: 'pt-ops-radar-mrms-card pt-map-legend-card',
      attribute: 'data-radar-mrms-name',
      dockSelector: '.pt-ops-radar-mrms-dock', closeSelector: '.pt-ops-radar-mrms-close'
    });
  }


  function ptOpsMapLegendCloseButtonHtml(type) {
    return '<button type="button" class="pt-ops-map-legend-close" data-pt-ops-map-legend-close="' + escapeHtml(type || '') + '" title="Hide this legend">&times;</button>';
  }

  function ptOpsMapLegendTitleHtml(title, type) {
    return '<div class="pt-ops-map-legend-titlebar"><h4>' + escapeHtml(title || '') + '</h4>' + ptOpsMapLegendCloseButtonHtml(type) + '</div>';
  }

  function ptOpsMapLegendEnsureClose(html, type) {
    html = String(html || '');
    type = String(type || '');
    if (!html || !type || html.indexOf('data-pt-ops-map-legend-close') >= 0) return html;

    return html.replace(/<h4([^>]*)>([\s\S]*?)<\/h4>/i, function(full, attrs, titleHtml) {
      return '<div class="pt-ops-map-legend-titlebar"><h4' + (attrs || '') + '>' + titleHtml + '</h4>' + ptOpsMapLegendCloseButtonHtml(type) + '</div>';
    });
  }

  function ptOpsEnsureMapLegendControl() {
    if (ptOpsMapLegendDiv && document.documentElement.contains(ptOpsMapLegendDiv)) {
      return ptOpsMapLegendDiv;
    }
    if (typeof L === 'undefined' || !map || !map.getContainer) return null;

    var mapContainer = map.getContainer();
    if (!mapContainer) return null;

    // Mount directly under the map container rather than inside Leaflet's
    // top-left control corner. The External Layers ribbon is also absolutely
    // positioned under the map container; keeping both in the same stacking
    // context lets this legend's z-index reliably place it above that ribbon.
    var div = mapContainer.querySelector('.pt-ops-map-legend');
    if (!div) {
      div = L.DomUtil.create('div', 'pt-ops-map-legend leaflet-control');
      mapContainer.appendChild(div);
    }
    div.style.display = 'none';
    div.style.background = 'rgba(226, 238, 235, 0.98)';
    div.style.borderColor = 'rgba(90, 120, 116, 0.55)';
    div.style.zIndex = '10010';
    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);

    if (!document.getElementById('pt-ops-map-legend-close-style')) {
      var style = document.createElement('style');
      style.id = 'pt-ops-map-legend-close-style';
      style.textContent = '.pt-ops-map-legend{background:rgba(226,238,235,0.98)!important;border-color:rgba(90,120,116,0.55)!important;z-index:10010!important;} .pt-ops-map-legend-titlebar{display:flex;align-items:flex-start;justify-content:space-between;gap:8px;margin-bottom:3px;} .pt-ops-map-legend-titlebar h4{margin:0;} .pt-ops-map-legend-close{border:0;background:transparent;color:#777;font:bold 16px/1 Arial,Helvetica,sans-serif;padding:0 2px;cursor:pointer;} .pt-ops-map-legend-close:hover{color:#222;} .pt-ops-wind-lead-controls{display:flex;align-items:center;gap:4px;flex-wrap:wrap;margin:3px 0 5px 0;} .pt-ops-wind-lead-label{font-size:11px;font-weight:700;color:#365b58;margin-right:2px;} .pt-ops-wind-lead-btn{border:1px solid #78918e;background:#f5fbfa;color:#244744;border-radius:3px;padding:2px 6px;font:600 11px/1.25 Arial,Helvetica,sans-serif;cursor:pointer;} .pt-ops-wind-lead-btn:hover{background:#d8ebe7;} .pt-ops-wind-lead-btn.is-active{background:#4f7d78;color:#fff;border-color:#3f6763;} .pt-ops-wind-lead-btn:disabled{opacity:.55;cursor:wait;} .pt-ops-wind-scale-title{font-size:10px;color:#4b6260;margin-top:2px;} .pt-ops-wind-scale-ticks{display:flex;justify-content:space-between;gap:2px;font-size:9px;line-height:1.1;color:#3f5553;margin-top:2px;} .pt-ops-wind-scale-ticks span{white-space:nowrap;}';
      document.head.appendChild(style);
    }
    if (!div._ptOpsLegendCloseBound) {
      div.addEventListener('click', function(e) {
        var leadBtn = e.target && e.target.closest ?
          e.target.closest('[data-pt-ops-wind-lead]') : null;
        if (leadBtn) {
          if (typeof L !== 'undefined' && L.DomEvent) L.DomEvent.stop(e);
          else if (e && e.preventDefault) { e.preventDefault(); e.stopPropagation(); }
          if (leadBtn.disabled) return;
          var modelKey = leadBtn.getAttribute('data-pt-ops-wind-model') || '';
          var leadHours = Number(leadBtn.getAttribute('data-pt-ops-wind-lead'));
          if (typeof ptOpsWindSetLead === 'function') {
            ptOpsWindSetLead(modelKey, leadHours);
          }
          return;
        }

        var btn = e.target && e.target.closest ? e.target.closest('[data-pt-ops-map-legend-close]') : null;
        if (!btn) return;
        if (typeof L !== 'undefined' && L.DomEvent) L.DomEvent.stop(e);
        else if (e && e.preventDefault) { e.preventDefault(); e.stopPropagation(); }
        var type = btn.getAttribute('data-pt-ops-map-legend-close') || '';
        if (type) ptOpsMapLegendHiddenTypes[type] = true;
        redrawLegend();
      });
      div._ptOpsLegendCloseBound = true;
    }

    ptOpsMapLegendControl = null;
    ptOpsMapLegendDiv = div;
    return ptOpsMapLegendDiv;
  }

  function opsReservoirCapacityLegendHtml() {
    return '<div class="pt-ops-map-legend-section">' +
      '<h4>Reservoir observed / forecast data</h4>' +
      '<div class="pt-ops-legend-row">' +
        '<div class="pt-ops-legend-main">' +
          '<div class="pt-ops-legend-gradient pt-ops-legend-gradient-reservoir"></div>' +
          '<div class="pt-ops-legend-scale"><span>0%</span><span>50%</span><span>100%+</span></div>' +
          '<div class="pt-ops-legend-small">Fill color = latest storage as percent of listed capacity.</div>' +
        '</div>' +
        '<div class="pt-ops-legend-side">' +
          '<div class="pt-ops-legend-textline"><span class="pt-ops-legend-circle" style="background:#F7F7F7;border-color:#777;border-style:dashed;"></span>No current storage value / link-only point</div>' +
          '<div class="pt-ops-legend-textline"><span class="pt-ops-legend-circle" style="background:#6BAED6;border-color:#08306B;"></span>Circle size scales with storage</div>' +
          '<div class="pt-ops-legend-small">Orange/red outline = stale observation.</div>' +
        '</div>' +
      '</div>' +
      '</div>';
  }

  function opsGroundwaterLegendBodyHtml() {
    return '' +
      '<div class="pt-ops-legend-small">Fill color = latest depth to water (ft bgs).</div>' +
      '<div class="pt-ops-legend-grid-2">' +
        '<div class="pt-ops-legend-textline"><span class="pt-ops-legend-circle" style="background:#756BB1;border-color:#4A1486;"></span>Reported artesian</div>' +
        '<div class="pt-ops-legend-textline"><span class="pt-ops-legend-circle" style="background:#2B8CBE;border-color:#08589E;"></span>0–25 ft bgs</div>' +
        '<div class="pt-ops-legend-textline"><span class="pt-ops-legend-circle" style="background:#7BCCC4;border-color:#08589E;"></span>25–100 ft bgs</div>' +
        '<div class="pt-ops-legend-textline"><span class="pt-ops-legend-circle" style="background:#A1D99B;border-color:#238B45;"></span>100–250 ft bgs</div>' +
        '<div class="pt-ops-legend-textline"><span class="pt-ops-legend-circle" style="background:#FEE391;border-color:#B8860B;"></span>250–500 ft bgs</div>' +
        '<div class="pt-ops-legend-textline"><span class="pt-ops-legend-circle" style="background:#FEC44F;border-color:#A63603;"></span>500–1,000 ft bgs</div>' +
        '<div class="pt-ops-legend-textline"><span class="pt-ops-legend-circle" style="background:#BD0026;border-color:#7F0000;"></span>&gt;1,000 ft bgs</div>' +
      '</div>' +
      '<div class="pt-ops-legend-row pt-ops-legend-footnotes">' +
        '<div class="pt-ops-legend-small"><span class="pt-ops-legend-circle pt-ops-legend-stale" style="background:#FEE391;border-color:#333333;border-style:solid;"></span>Dark outline = older/stale measurement.</div>' +
        '<div class="pt-ops-legend-small"><span class="pt-ops-legend-multipoint"></span>Multipoint symbol = nested/co-located USGS wells.</div>' +
      '</div>';
  }

  function opsGroundwaterLegendHtml() {
    return '<div class="pt-ops-map-legend-section">' +
      ptOpsMapLegendTitleHtml('USGS GW monitoring wells measured in last 800 days', 'usgs_groundwater') +
      opsGroundwaterLegendBodyHtml() +
      '</div>';
  }

  function opsStreamflowLegendDot(color, sizePx, label) {
    return '<div class="pt-ops-legend-textline"><span class="pt-ops-legend-circle" style="width:' + sizePx + 'px;height:' + sizePx + 'px;background:' + color + ';border-color:#555;vertical-align:-2px;"></span>' + label + '</div>';
  }

  function opsAirNowAqiLegendHtml() {
    function aqiDot(color, label) {
      return '<div class="pt-ops-legend-textline"><span class="pt-ops-legend-circle" style="background:' + color + ';border-color:#222;"></span>' + label + '</div>';
    }

    return '<div class="pt-ops-map-legend-section">' +
      '<h4>AirNow current AQI | Ops Live</h4>' +
      '<div class="pt-ops-legend-small">Colors follow the U.S. EPA / AirNow AQI categories. Monitor layer uses highest current Ozone/PM AQI where available; PM2.5 contour layer uses AirNow gridcode categories.</div>' +
      '<div class="pt-ops-legend-grid-2">' +
        aqiDot('#00e400', 'Good: 0–50') +
        aqiDot('#ffff00', 'Moderate: 51–100') +
        aqiDot('#ff7e00', 'USG: 101–150') +
        aqiDot('#ff0000', 'Unhealthy: 151–200') +
        aqiDot('#8f3f97', 'Very unhealthy: 201–300') +
        aqiDot('#7e0023', 'Hazardous: 301+') +
        aqiDot('#d9d9d9', 'No data') +
      '</div>' +
      '<div class="pt-ops-legend-small" style="margin-top:3px;">AirNow values are preliminary/current-hour screening data. Check timestamps; contours can lag monitor points.</div>' +
      '</div>';
  }

  function ptOpsWindLeadControlsHtml(def) {
    def = def || {};
    var options = Array.isArray(def.windLeadOptions) ? def.windLeadOptions : [];
    if (!options.length || !def.windModelKey) return '';

    var active = Number(def.windLeadHours || 0);
    var loading = def.windLeadLoading === true;
    var buttons = options.map(function(opt) {
      opt = opt || {};
      var hours = Number(opt.hours || 0);
      var label = opt.label != null ? String(opt.label) :
        (Math.abs(hours) < 0.01 ? 'Current' : '+' + Math.round(hours) + ' hr');
      var isActive = Math.abs(hours - active) < 0.01;
      return '<button type="button" class="pt-ops-wind-lead-btn' +
        (isActive ? ' is-active' : '') +
        '" data-pt-ops-wind-model="' + escapeHtml(def.windModelKey) +
        '" data-pt-ops-wind-lead="' + escapeHtml(String(hours)) +
        '" aria-pressed="' + (isActive ? 'true' : 'false') + '"' +
        (loading ? ' disabled' : '') + '>' + escapeHtml(label) + '</button>';
    }).join('');

    return '<div class="pt-ops-wind-lead-controls"><span class="pt-ops-wind-lead-label">View</span>' +
      buttons + '</div>';
  }

  function ptOpsWindScaleTicksHtml(def) {
    def = def || {};
    var summary = def.windSummary || {};
    var scaleMph = null;

    if (typeof ptOpsWindFirstNumber === 'function') {
      scaleMph = ptOpsWindFirstNumber([
        summary.recommended_velocity_scale_mph,
        summary.max_velocity_mph
      ]);
      if (scaleMph == null) {
        var scaleMs = ptOpsWindFirstNumber([
          summary.recommended_velocity_scale_ms,
          summary.max_velocity_ms
        ]);
        if (scaleMs != null) scaleMph = scaleMs * 2.2369362920544;
      }
      if (scaleMph == null && typeof ptOpsWindSpeedMph === 'function') {
        scaleMph = ptOpsWindSpeedMph(summary, 'p95');
      }
    }

    scaleMph = Number(scaleMph);
    if (!isFinite(scaleMph) || scaleMph <= 0) {
      return '<div class="pt-ops-legend-scale"><span>lighter/slower</span><span>stronger</span></div>';
    }

    var domainMax = null;
    if (typeof ptOpsWindSpeedMph === 'function') {
      domainMax = ptOpsWindSpeedMph(summary, 'max');
    }
    domainMax = Number(domainMax);

    var labels = [];
    for (var i = 0; i <= 5; i += 1) {
      var value = Math.round(scaleMph * i / 5);
      if (i === 0) value = 0;
      var label = String(value);
      if (i === 5) {
        label = (isFinite(domainMax) && domainMax > scaleMph * 1.02 ? '≥' : '') +
          label + ' mph';
      }
      labels.push('<span>' + escapeHtml(label) + '</span>');
    }

    return '<div class="pt-ops-wind-scale-title">Particle color scale</div>' +
      '<div class="pt-ops-wind-scale-ticks">' + labels.join('') + '</div>';
  }

  function opsWindFlowGfsLegendHtml(def) {
    def = def || {};
    return '<div class="pt-ops-map-legend-section">' +
      ptOpsMapLegendTitleHtml('NOAA GFS 10-m model wind', 'wind_flow_gfs') +
      ptOpsWindLeadControlsHtml(def) +
      ptOpsLegendMetricHtml(def) +
      '<div class="pt-ops-legend-small">Animated particles show modeled 10-m wind flow from NOAA/NCEP GFS U/V components. Each field is an instantaneous hourly model snapshot valid at the time shown, not an hourly average. Use for broad pattern context, not observed station wind.</div>' +
      '<div class="pt-ops-legend-row" style="gap:10px;margin-top:4px;">' +
        '<div class="pt-ops-legend-main">' +
          '<div class="pt-ops-legend-gradient" style="height:8px;background:linear-gradient(to right,#b2e2e2,#66c2a4,#2ca25f,#238b45,#2b8cbe,#0868ac,#253494,#fed976,#feb24c,#fd8d3c,#e31a1c);"></div>' +
          ptOpsWindScaleTicksHtml(def) +
        '</div>' +
      '</div>' +
      '<div class="pt-ops-legend-small" style="margin-top:3px;">Check NWS products for official forecasts, warnings, and fire-weather decisions.</div>' +
      '</div>';
  }

  function opsWindFlowHrrrLegendHtml(def) {
    def = def || {};
    return '<div class="pt-ops-map-legend-section">' +
      ptOpsMapLegendTitleHtml('NOAA HRRR 10-m model wind', 'wind_flow_hrrr') +
      ptOpsWindLeadControlsHtml(def) +
      ptOpsLegendMetricHtml(def) +
      '<div class="pt-ops-legend-small">Animated particles show higher-resolution NOAA/NCEP HRRR 10-m wind over Hydrologic California + adjacent basins. Wind vectors are corrected for native model-grid orientation before regridding to a regular 0.05° display grid. Each field is an instantaneous hourly model snapshot, not an hourly average.</div>' +
      '<div class="pt-ops-legend-row" style="gap:10px;margin-top:4px;">' +
        '<div class="pt-ops-legend-main">' +
          '<div class="pt-ops-legend-gradient" style="height:8px;background:linear-gradient(to right,#7bb6ff,#b7dcff,#d8f5e0,#fff0a6,#ffc16e,#ff7d62,#d83b62);"></div>' +
          ptOpsWindScaleTicksHtml(def) +
        '</div>' +
      '</div>' +
      '<div class="pt-ops-legend-small" style="margin-top:3px;">Use for regional flow and terrain-detail context. Check NWS products for official forecasts, warnings, and fire-weather decisions.</div>' +
      '</div>';
  }

  function ptOpsNbmScaleTicksHtml(def) {
    def = def || {};
    var summary = def.nbmSummary || def.windSummary || {};
    var scale = Number(summary.recommended_gust_scale_mph);
    if (!isFinite(scale) || scale <= 0) {
      scale = summary.gust_p50_mph ? Number(summary.gust_p50_mph.p95) : 45;
    }
    if (!isFinite(scale) || scale <= 0) scale = 45;
    var domainMax = summary.gust_p50_mph ? Number(summary.gust_p50_mph.max) : null;
    var labels = [];
    for (var i = 0; i <= 5; i += 1) {
      var value = Math.round(scale * i / 5);
      if (i === 0) value = 0;
      var label = String(value);
      if (i === 5) label = (isFinite(domainMax) && domainMax > scale * 1.02 ? '≥' : '') + label + ' mph';
      labels.push('<span>' + escapeHtml(label) + '</span>');
    }
    return '<div class="pt-ops-wind-scale-title">Median gust color scale</div>' +
      '<div class="pt-ops-wind-scale-ticks">' + labels.join('') + '</div>';
  }

  function opsNbmWindGuidanceLegendHtml(def) {
    def = def || {};
    return '<div class="pt-ops-map-legend-section">' +
      ptOpsMapLegendTitleHtml('NOAA NBM wind guidance', 'wind_guidance_nbm') +
      ptOpsWindLeadControlsHtml(def) +
      ptOpsLegendMetricHtml(def) +
      '<div class="pt-ops-legend-small">NBM is calibrated multi-model guidance. Barb speed shows median sustained wind; orientation shows NBM central wind direction; circle color shows median gust. Hover or click for the NBM 10th–90th percentile sustained and gust ranges.</div>' +
      '<div class="pt-ops-legend-row" style="gap:10px;margin-top:4px;">' +
        '<div class="pt-ops-legend-main">' +
          '<div class="pt-ops-legend-gradient" style="height:8px;background:linear-gradient(to right,#ffffcc,#c2e699,#78c679,#31a354,#2b8cbe,#fdae61,#e31a1c,#7a0177);"></div>' +
          ptOpsNbmScaleTicksHtml(def) +
        '</div>' +
      '</div>' +
      '<div class="pt-ops-legend-small" style="margin-top:3px;">Percentile ranges are guidance distributions, not formal confidence limits. Check official NWS forecasts and warnings for decisions.</div>' +
      '</div>';
  }

  function opsObservedWindMetarLegendHtml(def) {
    def = def || {};
    return '<div class="pt-ops-map-legend-section">' +
      ptOpsMapLegendTitleHtml('Observed wind | METAR/ASOS', 'observed_wind_metar') +
      ptOpsLegendMetricHtml(def) +
      '<div class="pt-ops-legend-small">Wind barbs use sustained wind in knots. Staff points toward the direction the wind is coming from; half barb = 5 kt, full barb = 10 kt, pennant = 50 kt.</div>' +
      '<div class="pt-ops-legend-grid-2" style="margin-top:4px;">' +
        '<div class="pt-ops-legend-textline"><span style="display:inline-block;width:18px;height:18px;margin-right:5px;vertical-align:-5px;"><svg viewBox="0 0 18 18" width="18" height="18" aria-hidden="true"><line x1="9" y1="16" x2="9" y2="2" stroke="#fff" stroke-width="4"/><line x1="9" y1="16" x2="9" y2="2" stroke="#111" stroke-width="1.8"/><line x1="9" y1="4" x2="16" y2="8" stroke="#fff" stroke-width="4"/><line x1="9" y1="4" x2="16" y2="8" stroke="#111" stroke-width="1.8"/></svg></span>Example: 10-kt full barb</div>' +
        '<div class="pt-ops-legend-textline"><span class="pt-ops-legend-circle" style="background:transparent;border:2px solid #d95f0e;"></span>Orange station ring = observation older than 2 hours</div>' +
      '</div>' +
      '<div class="pt-ops-legend-small" style="margin-top:3px;">Optional <b>lbl</b> labels appear at zoom 7+ and show the familiar three-letter station ID where applicable plus sustained/gust mph. <b>G–</b> means gust not reported, not zero.</div>' +
      '<div class="pt-ops-legend-small" style="margin-top:3px;">Airport observations are point measurements. Terrain, exposure, and distance from a station matter; use NWS products for official forecasts and warnings.</div>' +
      '</div>';
  }

  function ptOpsLegendMetricHtml(def) {
    def = def || {};
    var lines = Array.isArray(def.metricLines) ? def.metricLines : [];
    lines = lines.map(function(x) { return String(x == null ? '' : x); })
      .filter(function(x) { return x !== ''; });

    if (lines.length) {
      return '<div class="pt-ops-legend-small" style="margin:2px 0 4px 0;font-weight:700;color:#264653;">' +
        lines.map(function(x) { return escapeHtml(x); }).join('<br>') +
        '</div>';
    }

    var txt = def.metricText ? String(def.metricText) : '';
    if (!txt) return '';
    return '<div class="pt-ops-legend-small" style="margin:2px 0 4px 0;font-weight:700;color:#264653;">' + escapeHtml(txt) + '</div>';
  }

  function ptOpsSetLegendMetric(name, metricText) {
    name = String(name || '');
    if (!name || !activeLegendDefs || !activeLegendDefs[name]) return;
    activeLegendDefs[name].metricText = metricText || '';
    redrawLegend();
  }

  function opsUsgsStreamflowLegendBodyHtml() {
    return '' +
      '<div class="pt-ops-legend-small">Circle size and fill color = latest discharge magnitude (raw cfs, not percentile/normal condition).</div>' +
      '<div class="pt-ops-legend-grid-2">' +
        opsStreamflowLegendDot('#F7F7F7', 8, '0 cfs') +
        opsStreamflowLegendDot('#DEEBF7', 9, '&lt;1 cfs') +
        opsStreamflowLegendDot('#9ECAE1', 10, '1–10 cfs') +
        opsStreamflowLegendDot('#4292C6', 11, '10–100 cfs') +
        opsStreamflowLegendDot('#08519C', 12, '100–1k cfs') +
        opsStreamflowLegendDot('#31A354', 13, '1k–10k cfs') +
        opsStreamflowLegendDot('#FDAE61', 14, '10k–50k cfs') +
        opsStreamflowLegendDot('#D73027', 15, '&gt;50k cfs') +
      '</div>' +
      '<div class="pt-ops-legend-small" style="margin-top:3px;"><span class="pt-ops-legend-circle" style="background:#D9EAF7;border-color:#3182BD;border-style:dashed;"></span>Stage-only site. This Ops layer is not a flood-stage renderer.</div>';
  }

  function opsUsgsStreamflowLegendHtml(def) {
    return '<div class="pt-ops-map-legend-section">' +
      ptOpsMapLegendTitleHtml('USGS streamflow | California | Ops Live', 'usgs_streamflow') +
      ptOpsLegendMetricHtml(def) +
      opsUsgsStreamflowLegendBodyHtml() +
      '</div>';
  }

  function opsMultiAgencyStreamflowLegendHtml(def) {
    return '<div class="pt-ops-map-legend-section">' +
      ptOpsMapLegendTitleHtml('Multi-agency streamflow | National | Ops Live', 'multi_agency_streamflow') +
      ptOpsLegendMetricHtml(def) +
      '<div class="pt-ops-legend-small">Stream/canal gage circles use raw source flow magnitude; bins extend higher for national-scale rivers.</div>' +
      '<div class="pt-ops-legend-grid-2">' +
        opsStreamflowLegendDot('#F7F7F7', 8, '0 cfs') +
        opsStreamflowLegendDot('#DADAEB', 9, '&lt;1 cfs') +
        opsStreamflowLegendDot('#9E9AC8', 10, '1–10 cfs') +
        opsStreamflowLegendDot('#4292C6', 11, '10–100 cfs') +
        opsStreamflowLegendDot('#41B6C4', 12, '100–1k cfs') +
        opsStreamflowLegendDot('#31A354', 13, '1k–10k cfs') +
        opsStreamflowLegendDot('#FDAE61', 14, '10k–100k cfs') +
        opsStreamflowLegendDot('#D73027', 15, '100k–500k cfs') +
        opsStreamflowLegendDot('#7A0177', 16, '&gt;500k cfs') +
      '</div>' +
      '<div class="pt-ops-legend-row pt-ops-legend-footnotes">' +
        '<div class="pt-ops-legend-small"><span class="pt-ops-legend-circle" style="border-radius:0;background:#4292c6;border:2px solid #08306b;"></span>Likely reservoir/lake pool record; source flow_cfs is not treated as current discharge.</div>' +
        '<div class="pt-ops-legend-small"><span class="pt-ops-legend-circle" style="border-radius:0;background:#fff;border:2px dashed #2b8cbe;"></span>Reservoir/lake/dam-adjacent or questionable record; verify source graph.</div>' +
      '</div>' +
      '</div>';
  }

  function opsFireYearLegendHtml() {
    var currentYear = new Date().getFullYear();
    var colors = ['#7f0000', '#b30000', '#e34a33', '#fc8d59', '#fdbb84', '#fdd49e', '#fee8c8'];
    var labels = [
      String(currentYear),
      String(currentYear - 1),
      String(currentYear - 2),
      String(currentYear - 3),
      String(currentYear - 4),
      String(currentYear - 5),
      String(currentYear - 6) + ' or older'
    ];

    var html = '<div class="pt-ops-section"><b>Fire year</b><br>';
    labels.forEach(function(label, idx) {
      html += '<div class="pt-ops-legend-line"><span class="pt-ops-swatch" style="background:' + colors[idx] + '"></span>' + escapeHtml(label) + '</div>';
    });
    html += '<div class="pt-ops-muted">Recent years draw darker/redder; older perimeters draw lighter.</div></div>';
    return html;
  }

  function opsCnrfcForecastPointsLegendHtml() {
    if (typeof ptCnrfcFpLegendHtml === 'function') {
      return ptCnrfcFpLegendHtml();
    }
    return '<div class="pt-ops-map-legend-section"><h4>CNRFC forecast points | river/reservoir</h4><div class="pt-ops-legend-small">Active CNRFC river/reservoir forecast points.</div></div>';
  }

  function opsCnrfcPrecipWeatherLegendHtml() {
    if (typeof ptCnrfcPwLegendHtml === 'function') {
      return ptCnrfcPwLegendHtml();
    }
    return '<div class="pt-ops-map-legend-section"><h4>CNRFC stations | NWS/WRH time series</h4><div class="pt-ops-legend-small">CNRFC precip/weather stations with NWS/WRH station time-series links.</div></div>';
  }

  function ptOpsMapLegendHtml(keys) {
    keys = keys || [];
    var activeTypes = {};
    keys.forEach(function(k) {
      var def = activeLegendDefs[k] || {};
      if (def.unifiedCard === true) return;
      if (def.legendType) activeTypes[def.legendType] = true;
    });
    Object.keys(ptOpsMapLegendHiddenTypes).forEach(function(type) {
      if (!activeTypes[type]) delete ptOpsMapLegendHiddenTypes[type];
    });

    var seen = {};
    var html = '';
    keys.forEach(function(k) {
      var def = activeLegendDefs[k] || {};
      if (def.unifiedCard === true) return;
      if (def.legendType && ptOpsMapLegendHiddenTypes[def.legendType]) return;
      if (def.legendType === 'airnow_aqi' && !seen.airnow_aqi) {
        html += ptOpsMapLegendEnsureClose(opsAirNowAqiLegendHtml(), 'airnow_aqi');
        seen.airnow_aqi = true;
      }
      if (def.legendType === 'reservoir_capacity' && !seen.reservoir_capacity) {
        html += ptOpsMapLegendEnsureClose(opsReservoirCapacityLegendHtml(), 'reservoir_capacity');
        seen.reservoir_capacity = true;
      }
      if (def.legendType === 'usgs_groundwater' && !seen.usgs_groundwater) {
        html += ptOpsMapLegendEnsureClose(opsGroundwaterLegendHtml(), 'usgs_groundwater');
        seen.usgs_groundwater = true;
      }
      if (def.legendType === 'usgs_streamflow' && !seen.usgs_streamflow) {
        html += ptOpsMapLegendEnsureClose(opsUsgsStreamflowLegendHtml(def), 'usgs_streamflow');
        seen.usgs_streamflow = true;
      }
      if (def.legendType === 'multi_agency_streamflow' && !seen.multi_agency_streamflow) {
        html += ptOpsMapLegendEnsureClose(opsMultiAgencyStreamflowLegendHtml(def), 'multi_agency_streamflow');
        seen.multi_agency_streamflow = true;
      }
      if (def.legendType === 'cnrfc_forecast_points' && !seen.cnrfc_forecast_points) {
        html += ptOpsMapLegendEnsureClose(opsCnrfcForecastPointsLegendHtml(def), 'cnrfc_forecast_points');
        seen.cnrfc_forecast_points = true;
      }
      if (def.legendType === 'cnrfc_precip_weather_stations' && !seen.cnrfc_precip_weather_stations) {
        html += ptOpsMapLegendEnsureClose(opsCnrfcPrecipWeatherLegendHtml(def), 'cnrfc_precip_weather_stations');
        seen.cnrfc_precip_weather_stations = true;
      }
      if (def.legendType === 'wind_flow_gfs' && !seen.wind_flow_gfs) {
        html += ptOpsMapLegendEnsureClose(opsWindFlowGfsLegendHtml(def), 'wind_flow_gfs');
        seen.wind_flow_gfs = true;
      }
      if (def.legendType === 'wind_flow_hrrr' && !seen.wind_flow_hrrr) {
        html += ptOpsMapLegendEnsureClose(opsWindFlowHrrrLegendHtml(def), 'wind_flow_hrrr');
        seen.wind_flow_hrrr = true;
      }
      if (def.legendType === 'wind_guidance_nbm' && !seen.wind_guidance_nbm) {
        html += ptOpsMapLegendEnsureClose(opsNbmWindGuidanceLegendHtml(def), 'wind_guidance_nbm');
        seen.wind_guidance_nbm = true;
      }
      if (def.legendType === 'observed_wind_metar' && !seen.observed_wind_metar) {
        html += ptOpsMapLegendEnsureClose(opsObservedWindMetarLegendHtml(def), 'observed_wind_metar');
        seen.observed_wind_metar = true;
      }
    });
    return html;
  }

  function redrawOpsMapLegend(keys) {
    var div = ptOpsEnsureMapLegendControl();
    if (!div) return;

    var html = ptOpsMapLegendHtml(keys || Object.keys(activeLegendDefs).sort());
    if (html) {
      div.innerHTML = html;
      div.style.display = 'block';
    } else {
      div.innerHTML = '';
      div.style.display = 'none';
    }
  }

  function redrawLegend() {
    ptOpsSyncRfcQpeLegends();
    ptOpsSyncRadarMrmsLegends();
    ptOpsSyncForecastLegends();
    if (!legendDiv) {
      redrawOpsMapLegend(Object.keys(activeLegendDefs).sort());
      return;
    }
    var keys = Object.keys(activeLegendDefs).sort();
    redrawOpsMapLegend(keys);
    if (!keys.length) {
      legendDiv.innerHTML = '<h4>Active overlay notes</h4><div class="pt-ops-muted">Turn on an Ops overlay to show notes here.</div>';
      return;
    }
    var seen = {};
    var html = '<h4>Active overlay notes</h4>';
    keys.forEach(function(k) {
      var def = activeLegendDefs[k];
      // Hide the same misleading radar legend links in the textual Ops notes.
      var linkDef = ptOpsUsesRadarGuide(def) ? Object.assign({}, def, {legendUrl: null}) : def;
      html += '<div class="pt-ops-section"><b>' + escapeHtml(k) + '</b><br><span class="pt-ops-muted">' + escapeHtml(def.note || '') + '</span>' + activeOverlayLinksHtml(linkDef) + '</div>';
      if (ptOpsRadarMrmsMetadata(def)) {
        html += '<div class="pt-ops-section" data-radar-mrms-name="' + escapeHtml(k) + '">' +
          ptOpsRadarMrmsMetadataHtml(def) + '</div>';
      }
      if (def.legendType && !seen[def.legendType]) {
        // Forecast scales belong to their product-owned map cards.
        if (def.legendType === 'airnow_aqi') html += '<div class="pt-ops-section"><b>AirNow AQI legend</b><br><span class="pt-ops-muted">A compact AirNow AQI category legend is shown on the map while this layer is active.</span></div>';

        if (def.legendType === 'fire_year') html += opsFireYearLegendHtml();
        if (def.legendType === 'reservoir_capacity') html += '<div class="pt-ops-section"><b>Reservoir legend</b><br><span class="pt-ops-muted">A compact reservoir legend is shown on the map while this layer is active.</span></div>';
        if (def.legendType === 'usgs_groundwater') html += '<div class="pt-ops-section"><b>Groundwater legend</b><br><span class="pt-ops-muted">A compact groundwater legend is shown on the map while this layer is active.</span></div>';
        if (def.legendType === 'usgs_streamflow') html += '<div class="pt-ops-section"><b>USGS streamflow legend</b><br><span class="pt-ops-muted">A compact USGS streamflow legend is shown on the map while this layer is active.</span></div>';
        if (def.legendType === 'multi_agency_streamflow') html += '<div class="pt-ops-section"><b>Multi-agency streamflow legend</b><br><span class="pt-ops-muted">A compact multi-agency streamflow/reservoir flag legend is shown on the map while this layer is active.</span></div>';
        if (def.legendType === 'cnrfc_forecast_points') html += '<div class="pt-ops-section"><b>CNRFC forecast-point legend</b><br><span class="pt-ops-muted">A compact CNRFC river/reservoir forecast-point legend is shown on the map while this layer is active.</span></div>';
        if (def.legendType === 'cnrfc_precip_weather_stations') html += '<div class="pt-ops-section"><b>CNRFC NWS/WRH station time-series legend</b><br><span class="pt-ops-muted">A compact station legend is shown on the map while this layer is active.</span></div>';
        if (def.legendType === 'wind_flow_gfs') html += '<div class="pt-ops-section"><b>GFS wind-flow legend</b><br><span class="pt-ops-muted">A compact wind-flow legend is shown on the map while this layer is active.</span></div>';
        if (def.legendType === 'wind_flow_hrrr') html += '<div class="pt-ops-section"><b>HRRR wind-flow legend</b><br><span class="pt-ops-muted">A compact regional HRRR wind-flow legend is shown on the map while this layer is active.</span></div>';
        if (def.legendType === 'wind_guidance_nbm') html += '<div class="pt-ops-section"><b>NBM wind-guidance legend</b><br><span class="pt-ops-muted">A compact NBM median-and-uncertainty legend is shown on the map while this layer is active.</span></div>';
        if (def.legendType === 'observed_wind_metar') html += '<div class="pt-ops-section"><b>Observed-wind legend</b><br><span class="pt-ops-muted">A compact METAR/ASOS wind-barb legend is shown on the map while this layer is active.</span></div>';
        seen[def.legendType] = true;
      }
    });
    legendDiv.innerHTML = html;
  }


)---"
}
