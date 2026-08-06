function(el, x, data) {
  'use strict';
  var map = this;
  var payloads = Array.isArray(data) ? data : [];
  window.BRIM = window.BRIM || {};

  if (!window.BRIM.localReferenceFilterEngine) {
    throw new Error('BRIM Local Reference filter engine was not installed before its controller.');
  }
  if (window.BRIM.localReferenceController &&
      typeof window.BRIM.localReferenceController.destroy === 'function') {
    window.BRIM.localReferenceController.destroy();
  }

  var listenerRecords = [];
  var domListenerRecords = [];
  var controllers = [];
  var destroyed = false;
  var tabbedPopupLayoutState = null;

  function listen(target, names, handler) {
    target.on(names, handler);
    listenerRecords.push({target: target, names: names, handler: handler});
  }

  function listenDom(target, name, handler, options) {
    target.addEventListener(name, handler, options);
    domListenerRecords.push({
      target: target,
      name: name,
      handler: handler,
      options: options
    });
  }

  function escapeHtml(value) {
    return String(value === undefined || value === null ? '' : value)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#039;');
  }

  function cleanText(value) {
    var text = String(value === undefined || value === null ? '' : value).trim();
    return /^(?:NA|N\/A|null|undefined)$/i.test(text) ? '' : text;
  }

  function formatNumber(value, digits) {
    var number = Number(value);
    if (!isFinite(number)) return '';
    return number.toLocaleString('en-US', {
      minimumFractionDigits: digits,
      maximumFractionDigits: digits
    });
  }

  function popupRow(label, value) {
    var text = cleanText(value);
    if (!text) return '';
    return '<div class="pt-lr-popup-row"><span class="pt-lr-popup-label">' +
      escapeHtml(label) + ':</span> ' + escapeHtml(text) + '</div>';
  }

  function popupLink(url, label) {
    var href = cleanText(url);
    var text = cleanText(label);
    if (!href || !text) return '';
    return '<a href="' + escapeHtml(href) +
      '" target="_blank" rel="noopener noreferrer">' + escapeHtml(text) + '</a>';
  }

  function resourceList(items) {
    var links = (items || []).map(function(item) {
      return popupLink(item && item.url, item && item.label);
    }).filter(Boolean);
    return links.length ? '<ul class="pt-lr-popup-resource-list"><li>' +
      links.join('</li><li>') + '</li></ul>' : '';
  }

  function popupSection(heading, content, className) {
    var html = cleanText(content);
    if (!html) return '';
    return '<section class="pt-lr-popup-section' +
      (cleanText(className) ? ' ' + escapeHtml(className) : '') + '"><h3>' +
      escapeHtml(heading) + '</h3>' + html + '</section>';
  }

  function publicLawToken(value) {
    var match = cleanText(value).match(/Public Law\s+(\d+)-(\d+)/i);
    return match ? 'Public Law ' + match[1] + '-' + match[2] : '';
  }

  function publicLawUrl(template, value) {
    var match = publicLawToken(value).match(/Public Law\s+(\d+)-(\d+)/i);
    if (!match) return '';
    return cleanText(template)
      .replace(/\{congress\}/g, match[1])
      .replace(/\{number\}/g, match[2]);
  }

  function tabbedPopup(key, title, tabs) {
    var safeKey = cleanText(key).toLowerCase().replace(/[^A-Za-z0-9_-]+/g, '-');
    var buttons = tabs.map(function(tab, index) {
      var tabId = 'pt-lr-tab-' + safeKey + '-' + tab.key;
      var panelId = 'pt-lr-panel-' + safeKey + '-' + tab.key;
      return '<button type="button" id="' + tabId +
        '" class="pt-lr-popup-tab" role="tab" data-pt-lr-popup-tab="' +
        escapeHtml(tab.key) + '" aria-controls="' + panelId +
        '" aria-selected="' + (index === 0 ? 'true' : 'false') +
        '" tabindex="' + (index === 0 ? '0' : '-1') + '">' +
        escapeHtml(tab.label) + '</button>';
    }).join('');
    var panels = tabs.map(function(tab, index) {
      var tabId = 'pt-lr-tab-' + safeKey + '-' + tab.key;
      var panelId = 'pt-lr-panel-' + safeKey + '-' + tab.key;
      return '<section id="' + panelId +
        '" class="pt-lr-popup-panel" role="tabpanel" aria-labelledby="' +
        tabId + '" data-pt-lr-popup-panel="' + escapeHtml(tab.key) + '"' +
        (index === 0 ? '' : ' hidden') + '>' + tab.html + '</section>';
    }).join('');
    return '<article class="pt-popup pt-local-reference-popup ' +
      'pt-local-reference-tabbed-popup-card pt-fw-popup" ' +
      'data-pt-lr-tabbed-popup data-pt-lr-popup-key="' + escapeHtml(safeKey) + '">' +
      '<div class="pt-lr-popup-sticky"><header class="pt-lr-popup-header">' +
      '<div class="pt-lr-popup-title" role="heading" aria-level="2">' +
      escapeHtml(title) + '</div><span class="pt-lr-popup-badge">Federal Wilderness</span>' +
      '</header><div class="pt-lr-popup-tabs" role="tablist" ' +
      'aria-label="Federal Wilderness details">' + buttons + '</div></div>' +
      '<div class="pt-lr-popup-panel-scroll">' + panels + '</div></article>';
  }

  function buildFederalWildernessPopup(record, data, lookup) {
    var semantic = lookup.semantic[String(record.semantic_feature_key)] || {};
    var component = lookup.component[String(record.geometry_key)] || {};
    var agency = lookup.agency[String(component.agency_key)] || {};
    var office = lookup.office[String(component.office_key)] || {};
    var policy = lookup.policy;
    var docs = (data.documents || []).filter(function(document) {
      return String(document.wilderness_id || '') === String(semantic.wilderness_id || '') ||
        String(document.wilderness_id || '') === 'ALL';
    });
    var officialDocs = docs.filter(function(document) {
      return /^(?:3|5)\s*-/.test(cleanText(document.authority_level));
    });
    var secondaryDocs = docs.filter(function(document) {
      return /^(?:9|11)\s*-/.test(cleanText(document.authority_level));
    });
    var lawTemplate = data.templates && data.templates.govinfo_public_law;
    var laterLaws = cleanText(semantic.subsequent_public_laws).split(';')
      .map(cleanText).filter(Boolean);
    var lawItems = [semantic.original_public_law].concat(laterLaws).map(function(law, index) {
      var token = publicLawToken(law);
      var document = docs.find(function(item) {
        return cleanText(item.type) === 'public law' && publicLawToken(item.title) === token;
      });
      var title = document ? cleanText(document.title) : token;
      var date = document ? cleanText(document.publication_date) : '';
      return {
        url: publicLawUrl(lawTemplate, token),
        label: (index === 0 ? 'Original designation law — ' : 'Subsequent public law — ') +
          title + (date ? ' (' + date + ')' : '')
      };
    });
    var officialLinks = officialDocs.map(function(document) {
      return {url: document.url, label: document.title};
    });
    var secondaryLinks = secondaryDocs.map(function(document) {
      return {url: document.url, label: cleanText(document.title) + ' — secondary research'};
    });
    var generalSourceTypes = /^(?:Official GIS service|Official GIS download|Official data catalog|Interagency GIS service|Interagency methodology and downloads|Law cross-reference|Official statutory codification|Official agency program page)$/;
    var sourceLinks = (data.sources || []).filter(function(source) {
      return generalSourceTypes.test(cleanText(source.type));
    }).map(function(source) {
      return {url: source.url, label: cleanText(source.title) +
        (cleanText(source.agency) ? ' — ' + cleanText(source.agency) : '')};
    });
    var officeLinks = [
      {url: office.local_unit_url, label: 'Selected component managing unit'},
      {url: office.blm_office_url, label: 'Selected BLM office'},
      {url: semantic.direct_official_agency_page_url, label: 'Official managing-agency page'},
      {url: semantic.official_page_url, label: 'Official wilderness page'},
      {url: semantic.official_map_url, label: 'Official map'},
      {url: semantic.primary_management_plan_url,
        label: cleanText(semantic.primary_management_plan_title) || 'Management plan'},
      {url: semantic.wilderness_connect_url, label: 'Wilderness Connect profile'}
    ];
    var publishedAcres = formatNumber(semantic.official_reference_acres, 0);
    var sourceAcres = formatNumber(component.source_gis_acres, 1);
    var calculatedAcres = formatNumber(component.calculated_acres, 1);
    var overview = '';
    if (cleanText(semantic.summary_short)) {
      overview += '<p class="pt-lr-popup-summary">' +
        escapeHtml(cleanText(semantic.summary_short)) + '</p>';
    }
    overview += popupRow('Original designation', [
      cleanText(semantic.designation_date), cleanText(semantic.original_public_law)
    ].filter(Boolean).join(' · '));
    overview += popupRow('States', semantic.states);
    overview += popupRow('Selected mapped-component state',
      cleanText(component.geographic_state) === 'NV' ? 'Nevada' : 'California');
    overview += popupRow('Selected mapped-component agency', agency.name);
    overview += popupRow('Named-wilderness managers', semantic.managing_agencies);
    overview += popupRow('Relevant managing unit', office.local_managing_unit);
    overview += popupRow('Relevant BLM office', office.blm_office);
    overview += popupRow('Official published wilderness acreage',
      publishedAcres ? publishedAcres + ' acres' : '');
    overview += popupRow('Selected mapped-component source GIS acreage',
      sourceAcres ? sourceAcres + ' acres' : '');
    overview += popupRow('Selected mapped-component calculated acreage',
      calculatedAcres ? calculatedAcres + ' acres' : '');
    overview += popupRow('Mapped representation',
      cleanText(semantic.source_component_count) + ' component(s) · ' +
      cleanText(semantic.managing_agencies));
    if (semantic.shared_management === true) {
      overview += '<div class="pt-fw-shared-cue">' +
        escapeHtml(cleanText(policy['Management identity'])) + '</div>';
    }
    if (cleanText(component.geographic_state) === 'NV') {
      overview += '<div class="pt-fw-nevada-cue">State: Nevada' +
        (cleanText(agency.name) ? ' · Managing agency: ' + escapeHtml(agency.name) : '') +
        (cleanText(office.blm_office) ? ' · Managing office: ' + escapeHtml(office.blm_office) : '') +
        ' · BRIM inclusion: Western Nevada context.</div>';
    }
    overview += '<div class="pt-lr-popup-note"><strong>Acreage context:</strong> ' +
      escapeHtml(cleanText(policy.Acreage)) +
      (cleanText(semantic.acreage_source) ? ' ' + escapeHtml(semantic.acreage_source) : '') +
      '</div><div class="pt-fw-caution">' +
      escapeHtml(cleanText(policy['Boundary meaning'])) + '</div>';

    var useAccess = '';
    if (cleanText(semantic.management_access_summary)) {
      useAccess += popupSection(
        'Area-specific management and access context',
        '<p>' + escapeHtml(cleanText(semantic.management_access_summary)) + '</p>'
      );
    }
    useAccess += popupSection('What Wilderness designation generally means', [
      policy.Designation, policy['Motorized/mechanized use'], policy.Access,
      policy.Rules, policy['Boundary meaning'], policy['Current conditions']
    ].map(function(value) {
      return cleanText(value) ? '<p>' + escapeHtml(cleanText(value)) + '</p>' : '';
    }).join(''));
    useAccess += popupSection('Official pages and current information', resourceList(officeLinks));

    var researchLinks = secondaryLinks.concat([
      {url: semantic.congress_search_url, label: 'Congress search — secondary research'},
      {url: semantic.nepa_search_url, label: 'NEPA search — secondary research'},
      {url: semantic.courtlistener_search_url, label: 'CourtListener search — discovery only'},
      {url: semantic.google_scholar_case_search_url, label: 'Google Scholar case search — discovery only'},
      {url: semantic.web_search_url, label: 'Web search — discovery only'}
    ]);
    var laws = popupSection('Area-specific public laws', resourceList(lawItems)) +
      popupSection('Official pages and management documents', resourceList(officialLinks)) +
      popupSection('General authorities and source references', resourceList(sourceLinks));
    if (resourceList(researchLinks)) {
      laws += '<details class="pt-popup-technical"><summary>Additional research references</summary>' +
        resourceList(researchLinks) + '<div class="pt-lr-popup-note">' +
        escapeHtml(cleanText(policy.Litigation)) + '</div></details>';
    }
    laws += '<details class="pt-popup-technical"><summary>Technical details</summary>' +
      popupRow('Semantic wilderness ID', semantic.wilderness_id) +
      popupRow('Mapped component ID', component.component_id) +
      popupRow('Designation validation', semantic.validation_status) +
      popupRow('Validation evidence', semantic.evidence_source) +
      (cleanText(semantic.explanatory_note) ? '<div class="pt-lr-popup-note">' +
        escapeHtml(cleanText(semantic.explanatory_note)) + '</div>' : '') +
      resourceList([{url: semantic.evidence_url, label: 'Primary designation-law evidence'}]) +
      popupRow('Component context', component.component_description) +
      popupRow('Geometry limitation', component.geometry_caveat) +
      '<div class="pt-lr-popup-note">' +
      escapeHtml(cleanText(policy['Management identity'])) + ' ' +
      escapeHtml(cleanText(policy['BLM stewardship'])) + '</div></details>';
    return tabbedPopup(component.component_id, semantic.official_name, [
      {key: 'overview', label: 'Overview', html: overview},
      {key: 'use-access', label: 'Use & access', html: useAccess},
      {key: 'laws-documents', label: 'Laws & official documents', html: laws}
    ]);
  }

  function layerId(layer) {
    if (!layer || !layer.options) return '';
    if (layer.options.layerId !== undefined && layer.options.layerId !== null) {
      return String(layer.options.layerId);
    }
    if (layer.options.layer_id !== undefined && layer.options.layer_id !== null) {
      return String(layer.options.layer_id);
    }
    return '';
  }

  function swatch(category) {
    var fill = category.fill_color === 'transparent' ? 'transparent' : category.fill_color;
    var dash = String(category.dash_array || '');
    var borderStyle = dash ? 'dashed' : 'solid';
    if (category.legend_swatch_style === 'line') {
      return '<span class="pt-lr-swatch pt-lr-swatch-line" style="border-top:' +
        Number(category.stroke_weight || 2) + 'px ' + borderStyle + ' ' +
        escapeHtml(category.stroke_color) + '"></span>';
    }
    return '<span class="pt-lr-swatch" style="background:' + escapeHtml(fill) +
      ';border:' + Number(category.stroke_weight || 1) + 'px ' + borderStyle + ' ' +
      escapeHtml(category.stroke_color) + '"></span>';
  }

  function primaryCount(counts, mode) {
    if (mode === 'record') return Number(counts.record_count || 0);
    if (mode === 'geometry_component') return Number(counts.geometry_component_count || 0);
    return Number(counts.semantic_feature_count || 0);
  }

  function installCss() {
    if (document.getElementById('pt-local-reference-controller-css')) return;
    var style = document.createElement('style');
    style.id = 'pt-local-reference-controller-css';
    style.textContent =
      '.pt-local-reference-card{box-sizing:border-box;width:330px;max-width:calc(100vw - 28px);max-height:min(68vh,610px);overflow:auto;padding:9px 10px;background:rgba(246,239,222,.97);color:#262626;border:1px solid rgba(82,72,45,.38);border-radius:6px;box-shadow:0 1px 5px rgba(0,0,0,.32);font:12px/1.35 Arial,sans-serif;touch-action:pan-y}' +
      '.pt-local-reference-card .pt-lr-head{display:flex;align-items:flex-start;justify-content:space-between;gap:8px;margin-bottom:7px}.pt-local-reference-card .pt-lr-title{font-size:14px;font-weight:700}' +
      '.pt-lr-feature-picker{max-width:100%;margin:3px 0 7px}.pt-lr-feature-label{display:block;margin-bottom:3px;color:#4f493e;font-size:11px;font-weight:700}' +
      '.pt-local-reference-card .pt-lr-search{box-sizing:border-box;width:100%;min-height:32px;margin:0;padding:5px 7px;border:1px solid #8c887e;border-radius:4px;font:12px Arial,sans-serif}' +
      '.pt-lr-suggestions{box-sizing:border-box;max-height:150px;margin:2px 0 0;padding:0;overflow:auto;border:1px solid #8c887e;border-radius:4px;background:#fff;list-style:none}.pt-lr-suggestions[hidden]{display:none}' +
      '.pt-lr-option{display:block;padding:5px 7px;cursor:pointer;white-space:normal;overflow-wrap:break-word;word-break:normal}.pt-lr-option[aria-selected=true],.pt-lr-option:hover{background:#e9e0c9;outline:none}' +
      '.pt-lr-search-status{min-height:0;color:#59554d;font-size:11px}.pt-lr-chips{display:flex;max-width:100%;flex-wrap:wrap;gap:4px;margin-top:5px}.pt-lr-chips:empty{display:none}' +
      '.pt-lr-chip{display:inline-flex;max-width:100%;min-width:0;align-items:center;gap:4px;padding:2px 3px 2px 7px;border:1px solid #81745d;border-radius:13px;background:#fffaf0}.pt-lr-chip-name{min-width:0;white-space:normal;overflow-wrap:break-word;word-break:normal}' +
      '.pt-lr-chip-remove{flex:0 0 auto;min-width:23px;min-height:23px;padding:0;border:0;border-radius:50%;background:transparent;color:#4d4030;font:bold 16px/1 Arial,sans-serif;cursor:pointer}.pt-lr-chip-remove:hover,.pt-lr-chip-remove:focus{background:#eadfc7;outline:2px solid #6f624c;outline-offset:1px}' +
      '.pt-lr-toolbar,.pt-lr-actions{display:flex;align-items:center;gap:5px;flex-wrap:wrap;margin:3px 0 6px}.pt-lr-toolbar button,.pt-lr-actions button{min-height:28px;padding:3px 8px;border:1px solid #817b6e;border-radius:4px;background:#fffdf8;color:#292929;cursor:pointer}.pt-lr-toolbar button:disabled,.pt-lr-actions button:disabled{cursor:default;opacity:.55}' +
      '.pt-lr-toolbar .pt-lr-toggle{display:inline-flex;align-items:center;gap:4px;margin-left:0}.pt-lr-toolbar .pt-lr-auto-toggle{margin-left:auto}' +
      '.pt-lr-category-heading,.pt-lr-facet-heading{max-width:100%;margin:6px 0 3px;color:#544c3e;font-size:11px;font-weight:700;line-height:1.25;white-space:normal;overflow-wrap:break-word;word-break:normal}.pt-lr-categories,.pt-lr-facet-values{border-top:1px solid rgba(82,72,45,.23)}' +
      '.pt-lr-category{display:grid;grid-template-columns:18px 25px minmax(0,1fr) auto;align-items:center;gap:5px;padding:5px 0;border-bottom:1px solid rgba(82,72,45,.14)}' +
      '.pt-lr-facet{margin-top:7px}.pt-lr-facet-head{display:flex;align-items:center;justify-content:space-between;gap:6px}.pt-lr-facet-toolbar{display:flex;gap:3px}.pt-lr-facet-toolbar button{min-height:25px;padding:2px 6px;border:1px solid #817b6e;border-radius:4px;background:#fffdf8;color:#292929;cursor:pointer;font-size:10.5px}.pt-lr-facet-row{display:grid;grid-template-columns:18px minmax(0,1fr) auto;align-items:center;gap:5px;padding:4px 0;border-bottom:1px solid rgba(82,72,45,.12)}.pt-lr-facet-count{color:#555;font-variant-numeric:tabular-nums;white-space:nowrap}.pt-lr-distinguish-note{margin:-2px 0 5px;color:#5a5144;font-size:10.5px}' +
      '.pt-lr-swatch{display:inline-block;width:19px;height:13px;box-sizing:border-box}.pt-lr-swatch-line{height:0;border-left:0!important;border-right:0!important;border-bottom:0!important}' +
      '.pt-lr-category-count{color:#555;font-variant-numeric:tabular-nums;white-space:nowrap}.pt-lr-summary{margin:6px 0;color:#3d3a35}.pt-lr-pending{font-weight:700;color:#8a4d00}' +
      '.pt-lr-caution{margin-top:7px;padding-top:6px;border-top:1px solid rgba(82,72,45,.26);color:#5a4634;font-size:11px}' +
      '.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"]{width:330px;max-height:none;overflow:visible;padding:5px 7px;font-size:10.5px;line-height:1.2}' +
      '.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-head{margin-bottom:2px}.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-title{font-size:13px}' +
      '.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-feature-picker{margin:1px 0 3px}.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-feature-label{position:absolute!important;width:1px!important;height:1px!important;padding:0!important;margin:-1px!important;overflow:hidden!important;clip:rect(0,0,0,0)!important;white-space:nowrap!important;border:0!important}.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-search{min-height:25px;padding:3px 5px;font-size:11px}' +
      '.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-toolbar,.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-actions{gap:4px;margin:1px 0 3px}.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-toolbar button,.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-actions button{min-height:22px;padding:1px 6px;font-size:10.5px}' +
      '.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-category-heading,.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-facet-heading{margin:2px 0 1px;font-size:10px}.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-categories{display:block}.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-category{grid-template-columns:15px 20px minmax(0,1fr) auto;gap:3px;min-height:17px;padding:1px 0}.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-swatch{width:17px;height:11px}' +
      '.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-distinguish-row{margin-top:2px}.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-distinguish-note{margin:0;color:#5a5144;font-size:9.5px}' +
      '.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-facet{margin-top:2px}.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-facet-toolbar{gap:2px}.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-facet-toolbar button{min-height:18px;padding:0 4px;font-size:9px}.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-facet-values{display:block}.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-facet-row{grid-template-columns:15px minmax(0,1fr) auto;gap:3px;min-height:17px;padding:1px 0}' +
      '.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-summary{margin:2px 0}.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-map-details,.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-caution{margin:2px 0;padding:0;font-size:9.5px}.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"] .pt-lr-caution>div{margin-top:2px;padding-top:3px;border-top:1px solid rgba(82,72,45,.2)}' +
      '.pt-local-reference-card[data-pt-local-reference-layer="federal_wilderness"].pt-map-card-undocked{max-height:calc(100vh - 8px);overflow-y:auto;overflow-x:hidden;overscroll-behavior:contain}' +
      '.pt-lr-visually-hidden{position:absolute!important;width:1px!important;height:1px!important;padding:0!important;margin:-1px!important;overflow:hidden!important;clip:rect(0,0,0,0)!important;white-space:nowrap!important;border:0!important}' +
      '.leaflet-tooltip.pt-wsa-hover-tooltip,.leaflet-tooltip.pt-trails-hover-tooltip,.leaflet-tooltip.pt-fw-hover-tooltip{white-space:normal!important;width:fit-content!important;min-width:min(220px,calc(100vw - 32px))!important;max-width:min(320px,calc(100vw - 32px))!important;overflow-wrap:break-word!important;word-break:normal!important;line-height:1.3!important;box-sizing:border-box}' +
      '.pt-wsa-hover-lines{display:block;max-width:100%}.pt-wsa-hover-line{display:block;white-space:normal}.pt-wsa-hover-name{font-weight:600}' +
      '.pt-trails-hover-lines{display:block;max-width:100%}.pt-trails-hover-line{display:block;white-space:normal}.pt-trails-hover-name{font-weight:600}' +
      '.pt-fw-hover-lines{display:block;max-width:100%}.pt-fw-hover-line{display:block;white-space:normal}.pt-fw-hover-title{font-weight:700}.pt-fw-shared-cue,.pt-fw-nevada-cue,.pt-fw-caution{margin-top:8px;padding:6px;background:#fff3cf;border-left:3px solid #a86f00}' +
      '.pt-wsa-popup .pt-popup-subtitle{margin-top:2px;color:#555;font-size:12px}.pt-wsa-popup .pt-popup-section{margin-top:7px}.pt-wsa-popup .pt-wsa-caution{margin-top:8px;padding:6px;background:#fff3cf;border-left:3px solid #a86f00}.pt-wsa-source-anomaly{color:#8a2f1c}.pt-popup-technical{margin-top:7px;font-size:11px}' +
      '.leaflet-popup.pt-local-reference-tabbed-popup .leaflet-popup-content-wrapper{padding:0;overflow:hidden}.leaflet-popup.pt-local-reference-tabbed-popup .leaflet-popup-content{box-sizing:border-box;width:min(430px,calc(100vw - 72px))!important;min-width:min(400px,calc(100vw - 72px))!important;max-width:min(460px,calc(100vw - 72px))!important;margin:10px 12px 12px}' +
      '.leaflet-container.pt-lr-tabbed-popup-open .leaflet-popup-pane{z-index:1100}' +
      '.pt-local-reference-tabbed-popup-card{display:flex;max-height:min(72vh,620px);min-height:0;flex-direction:column;overflow:hidden;color:#272727;font:12px/1.4 Arial,sans-serif}.pt-lr-popup-sticky{position:sticky;top:0;z-index:2;flex:0 0 auto;background:#fff}.pt-lr-popup-header{display:flex;align-items:flex-start;justify-content:space-between;gap:8px;padding:2px 1px 9px}.pt-lr-popup-title{font-size:15px;font-weight:700;line-height:1.2}.pt-lr-popup-badge{flex:0 0 auto;padding:2px 6px;border:1px solid #8d8370;border-radius:10px;background:#f4eee1;color:#493f31;font-size:10px;line-height:1.25;white-space:nowrap}' +
      '.pt-lr-popup-tabs{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:2px;border-bottom:1px solid #8f8778}.pt-lr-popup-tab{min-width:0;padding:6px 4px;border:1px solid transparent;border-bottom:0;border-radius:4px 4px 0 0;background:#eee8dc;color:#3d3933;font:600 11px/1.2 Arial,sans-serif;white-space:normal;cursor:pointer}.pt-lr-popup-tab[aria-selected=true]{border-color:#8f8778;background:#fff;color:#171717}.pt-lr-popup-tab:focus-visible{outline:3px solid #1d6fa5;outline-offset:-2px}' +
      '.pt-fw-popup .pt-lr-popup-tabs{grid-template-columns:repeat(3,minmax(0,1fr))}' +
      '.pt-local-reference-tabbed-popup-card button:enabled,.pt-local-reference-tabbed-popup-card summary{cursor:pointer}.pt-local-reference-tabbed-popup-card button:disabled{cursor:not-allowed}' +
      '.pt-lr-popup-panel-scroll{height:var(--pt-lr-popup-panel-height,auto);min-height:0;max-height:min(54vh,450px);overflow-y:auto;overflow-x:hidden;overscroll-behavior:contain}.pt-lr-popup-panel{padding:9px 2px 4px}.pt-lr-popup-panel[hidden]{display:none!important}.pt-lr-popup-summary,.pt-lr-popup-panel p{margin:0 0 8px}.pt-lr-popup-row{margin:3px 0}.pt-lr-popup-label,.pt-lr-popup-evidence>span,.pt-trails-caution>span{font-weight:700}.pt-lr-popup-section{margin-top:10px}.pt-lr-popup-section h3{margin:0 0 4px;color:#3e392f;font-size:12px;line-height:1.25}.pt-lr-popup-narrative{padding-top:2px;border-top:1px solid rgba(82,72,45,.18)}.pt-lr-narrative-source{margin-top:3px;color:#5c574f;font-size:10.5px}.pt-lr-narrative-source span{font-weight:700}.pt-lr-popup-evidence{margin-top:7px}.pt-lr-popup-resource-list{margin:0;padding-left:19px}.pt-lr-popup-resource-list li{margin:4px 0}.pt-lr-popup-note{margin:1px 0 4px;color:#5b5650;font-size:10.5px}.pt-trails-popup .pt-trails-caution{margin-top:9px;padding:6px;background:#fff3cf;border-left:3px solid #a86f00}.pt-trails-popup .pt-popup-technical{margin-top:10px;padding-top:6px;border-top:1px solid rgba(82,72,45,.2)}' +
      '.pt-local-reference-tabbed-popup-card.pt-lr-popup-measuring{visibility:hidden!important}.pt-lr-popup-measuring .pt-lr-popup-panel-scroll{height:auto!important;min-height:0!important;max-height:none!important;overflow:visible!important}' +
      '@media (max-width:520px){.leaflet-container.pt-lr-tabbed-popup-open .leaflet-control-container{visibility:hidden}.leaflet-popup.pt-local-reference-tabbed-popup .leaflet-popup-content{width:calc(100vw - 56px)!important;min-width:0!important;max-width:calc(100vw - 56px)!important;margin:9px 10px 11px}.pt-lr-popup-tabs{grid-template-columns:repeat(2,minmax(0,1fr))}.pt-lr-popup-badge{max-width:42%;white-space:normal;text-align:center}.pt-lr-popup-panel-scroll{max-height:min(50vh,390px)}}' +
      '@media (max-width:420px){.pt-local-reference-card{width:calc(100vw - 28px)}.pt-lr-toolbar .pt-lr-auto-toggle{margin-left:0}.pt-lr-chip{width:100%;box-sizing:border-box}.pt-lr-chip-remove{margin-left:auto}}' +
      '@media (pointer:coarse){.leaflet-tooltip.pt-wsa-hover-tooltip,.leaflet-tooltip.pt-trails-hover-tooltip{display:none!important}.leaflet-tooltip.pt-fw-hover-tooltip{display:none!important}.pt-local-reference-card button,.pt-local-reference-card input{min-height:38px}.pt-lr-category{min-height:34px}.pt-local-reference-card{max-height:58vh}.pt-lr-chip-remove{min-width:38px}}';
    document.head.appendChild(style);
  }

  function popupRoot(node) {
    return node && node.closest ? node.closest('[data-pt-lr-tabbed-popup]') : null;
  }

  function cleanupTabbedPopupLayout() {
    var state = tabbedPopupLayoutState;
    tabbedPopupLayoutState = null;
    if (!state) return;
    if (state.resizeTimer !== null) window.clearTimeout(state.resizeTimer);
    if (state.settleTimer !== null) window.clearTimeout(state.settleTimer);
    if (state.frameOne !== null) window.cancelAnimationFrame(state.frameOne);
    if (state.frameTwo !== null) window.cancelAnimationFrame(state.frameTwo);
    if (state.resizeObserver) state.resizeObserver.disconnect();
  }

  function tallestNaturalPanelHeight(root, scroller) {
    if (!scroller) return 0;
    var panels = Array.prototype.slice.call(
      scroller.querySelectorAll('[role="tabpanel"][data-pt-lr-popup-panel]')
    );
    if (!panels.length) return 0;
    var panelStates = panels.map(function(panel) { return panel.hidden; });
    var wasMeasuring = root.classList.contains('pt-lr-popup-measuring');
    var tallest = 0;
    try {
      // All state changes are synchronous within one frame. Hiding the live
      // card prevents a panel flash while each populated tab is measured at
      // its natural height; no Leaflet pan/zoom event is involved.
      root.classList.add('pt-lr-popup-measuring');
      panels.forEach(function(panel) { panel.hidden = true; });
      panels.forEach(function(panel) {
        panel.hidden = false;
        tallest = Math.max(
          tallest,
          panel.scrollHeight,
          panel.getBoundingClientRect().height
        );
        panel.hidden = true;
      });
    } finally {
      panels.forEach(function(panel, index) { panel.hidden = panelStates[index]; });
      if (!wasMeasuring) root.classList.remove('pt-lr-popup-measuring');
    }
    return Math.ceil(tallest);
  }

  function applyTabbedPopupLayout(state) {
    if (!state || state !== tabbedPopupLayoutState || !state.root.isConnected) return false;
    var root = state.root;
    var scroller = root.querySelector('.pt-lr-popup-panel-scroll');
    if (!scroller) return false;
    var width = scroller.getBoundingClientRect().width;
    if (!(width > 0)) return false;
    var naturalHeight = tallestNaturalPanelHeight(root, scroller);
    if (!(naturalHeight > 0)) return false;
    var cap = parseFloat(window.getComputedStyle(scroller).maxHeight);
    if (!(cap > 0)) cap = 450;
    var floor = Math.min(112, cap);
    var target = Math.ceil(Math.max(floor, Math.min(naturalHeight + 1, cap)));
    root.style.setProperty('--pt-lr-popup-panel-height', target + 'px');
    root.setAttribute('data-pt-lr-popup-layout-ready', 'true');
    root.setAttribute('data-pt-lr-popup-natural-height', String(naturalHeight));
    root.setAttribute('data-pt-lr-popup-target-height', String(target));
    root.setAttribute('data-pt-lr-popup-height-cap', String(Math.round(cap * 1000) / 1000));
    state.lastWidth = width;
    return true;
  }

  function applyInitialTabbedPopupLayout(state) {
    var applied = applyTabbedPopupLayout(state);
    if (applied && !state.initialPositionUpdated && state.popup) {
      state.initialPositionUpdated = true;
      // Leaflet initially positions the popup before the content-driven panel
      // height is known. Refresh layout and position without update(), which
      // would rebuild string-backed popup content and discard the measured
      // card, then apply Leaflet's existing open-time auto-pan rules once.
      if (state.popup._updateLayout) state.popup._updateLayout();
      if (state.popup._updatePosition) state.popup._updatePosition();
      if (state.popup.options.autoPan !== false && state.popup._adjustPan) {
        state.popup._adjustPan();
      }
    }
    return applied;
  }

  function scheduleTabbedPopupLayout(state, delay) {
    if (!state || state !== tabbedPopupLayoutState) return;
    if (state.resizeTimer !== null) window.clearTimeout(state.resizeTimer);
    state.resizeTimer = window.setTimeout(function() {
      state.resizeTimer = null;
      state.frameOne = window.requestAnimationFrame(function() {
        state.frameOne = null;
        state.frameTwo = window.requestAnimationFrame(function() {
          state.frameTwo = null;
          applyInitialTabbedPopupLayout(state);
        });
      });
    }, Math.max(0, Number(delay) || 0));
  }

  function startTabbedPopupLayout(root, popup) {
    cleanupTabbedPopupLayout();
    if (!root) return;
    var state = {
      root: root,
      lastWidth: 0,
      resizeTimer: null,
      settleTimer: null,
      frameOne: null,
      frameTwo: null,
      resizeObserver: null,
      popup: popup,
      initialPositionUpdated: false
    };
    tabbedPopupLayoutState = state;
    if (!applyInitialTabbedPopupLayout(state)) scheduleTabbedPopupLayout(state, 0);
    state.settleTimer = window.setTimeout(function() {
      state.settleTimer = null;
      scheduleTabbedPopupLayout(state, 0);
    }, 120);
    if (document.fonts && document.fonts.ready && document.fonts.ready.then) {
      document.fonts.ready.then(function() {
        if (state === tabbedPopupLayoutState) scheduleTabbedPopupLayout(state, 0);
      });
    }
    if (window.ResizeObserver) {
      state.resizeObserver = new window.ResizeObserver(function(entries) {
        if (state !== tabbedPopupLayoutState || !entries.length) return;
        var width = entries[0].contentRect.width;
        if (Math.abs(width - state.lastWidth) > 1) scheduleTabbedPopupLayout(state, 60);
      });
      state.resizeObserver.observe(root.querySelector('.pt-lr-popup-panel-scroll'));
    }
  }

  function onTabbedPopupViewportResize() {
    var state = tabbedPopupLayoutState;
    if (!state || !state.root.isConnected) return;
    scheduleTabbedPopupLayout(state, 80);
  }

  function onTabbedPopupDetailsToggle(event) {
    var details = event && event.target;
    if (!details || String(details.tagName || '').toLowerCase() !== 'details') return;
    var root = popupRoot(details);
    var state = tabbedPopupLayoutState;
    if (!root || !state || state.root !== root) return;
    // Native details toggles update their open state before this event. Two
    // animation frames let the collapsed/expanded layout settle before all
    // populated panels are measured again at the current popup width.
    scheduleTabbedPopupLayout(state, 0);
  }

  function activatePopupTab(root, tab, moveFocus) {
    if (!root || !tab) return false;
    var key = String(tab.getAttribute('data-pt-lr-popup-tab') || '');
    var tabs = Array.prototype.slice.call(
      root.querySelectorAll('[role="tab"][data-pt-lr-popup-tab]')
    );
    var panels = Array.prototype.slice.call(
      root.querySelectorAll('[role="tabpanel"][data-pt-lr-popup-panel]')
    );
    tabs.forEach(function(candidate) {
      var selected = candidate === tab;
      candidate.setAttribute('aria-selected', selected ? 'true' : 'false');
      candidate.setAttribute('tabindex', selected ? '0' : '-1');
    });
    panels.forEach(function(panel) {
      panel.hidden = String(panel.getAttribute('data-pt-lr-popup-panel') || '') !== key;
    });
    if (moveFocus && tab.focus) {
      try { tab.focus({preventScroll: true}); } catch (error) { tab.focus(); }
    }
    return true;
  }

  function resetTabbedPopup(container) {
    var root = container && container.querySelector ?
      container.querySelector('[data-pt-lr-tabbed-popup]') : null;
    if (!root) return false;
    var first = root.querySelector('[role="tab"][data-pt-lr-popup-tab]');
    return activatePopupTab(root, first, false);
  }

  function onTabbedPopupClick(event) {
    var tab = event.target.closest && event.target.closest('[data-pt-lr-popup-tab]');
    var root = popupRoot(tab);
    if (!root) return;
    event.preventDefault();
    activatePopupTab(root, tab, false);
  }

  function onTabbedPopupKeydown(event) {
    var tab = event.target.closest && event.target.closest('[data-pt-lr-popup-tab]');
    var root = popupRoot(tab);
    if (!root) return;
    var tabs = Array.prototype.slice.call(
      root.querySelectorAll('[role="tab"][data-pt-lr-popup-tab]')
    );
    var index = tabs.indexOf(tab);
    var nextIndex = index;
    if (event.key === 'ArrowRight' || event.key === 'ArrowDown') {
      nextIndex = (index + 1) % tabs.length;
    } else if (event.key === 'ArrowLeft' || event.key === 'ArrowUp') {
      nextIndex = (index - 1 + tabs.length) % tabs.length;
    } else if (event.key === 'Home') {
      nextIndex = 0;
    } else if (event.key === 'End') {
      nextIndex = tabs.length - 1;
    } else if (event.key === 'Enter' || event.key === ' ') {
      event.preventDefault();
      activatePopupTab(root, tab, true);
      return;
    } else {
      return;
    }
    event.preventDefault();
    activatePopupTab(root, tabs[nextIndex], true);
  }

  function onAnyPopupOpen(event) {
    var container = event && event.popup ? event.popup._container : null;
    var tabbed = resetTabbedPopup(container);
    if (el && el.classList) {
      if (tabbed) el.classList.add('pt-lr-tabbed-popup-open');
      else el.classList.remove('pt-lr-tabbed-popup-open');
    }
    if (tabbed) {
      startTabbedPopupLayout(
        container.querySelector('[data-pt-lr-tabbed-popup]'),
        event.popup
      );
    }
    else cleanupTabbedPopupLayout();
  }

  function onAnyPopupClose() {
    cleanupTabbedPopupLayout();
    if (el && el.classList) el.classList.remove('pt-lr-tabbed-popup-open');
  }

  function createLayerController(layerData) {
    var engine = window.BRIM.localReferenceFilterEngine.create(layerData);
    var groupName = String(layerData.group_name || '');
    var layerManager = map.layerManager || {};
    var groupTable = (layerManager._byGroup || {})[groupName] || {};
    var groupRoot = layerManager._groupContainers ? layerManager._groupContainers[groupName] : null;
    var recordByGeometry = Object.create(null);
    var layerByGeometry = Object.create(null);
    var card = null;
    var control = null;
    var detachable = null;
    var searchInput = null;
    var suggestionList = null;
    var searchStatus = null;
    var chipList = null;
    var suggestions = [];
    var activeSuggestionIndex = -1;
    var suggestionCloseTimer = null;
    var hiddenByClose = false;
    var active = !!(groupRoot && map.hasLayer && map.hasLayer(groupRoot));
    var primaryCountMode = String(layerData.primary_count_mode || 'semantic_feature');
    var categoryCountMode = String(layerData.category_count_mode || primaryCountMode);
    var primaryCountLabel = String(layerData.primary_count_label || layerData.display_name || 'features');
    var namedFeatureLabel = /^named\s+/i.test(primaryCountLabel) ?
      primaryCountLabel : 'named ' + primaryCountLabel;
    var componentCountLabel = String(layerData.component_count_label || 'mapped components');
    var featureSelectionSupported = layerData.feature_selection_supported === true;
    var autoZoomSupported = layerData.auto_zoom_supported === true;
    var zoomPadding = Math.max(0, Number(layerData.zoom_padding) || 0);
    var zoomMax = Math.max(1, Number(layerData.zoom_max) || 12);
    var preserveViewOnReset = layerData.preserve_view_on_reset !== false;
    var distinguishUnitsSupported = layerData.distinguish_units_supported === true;
    var distinguishUnits = false;
    var categoryByKey = Object.create(null);
    var safeLayerId = String(layerData.layer_id || 'layer').replace(/[^A-Za-z0-9_-]/g, '-');
    var searchId = 'pt-lr-search-' + safeLayerId;
    var listboxId = 'pt-lr-listbox-' + safeLayerId;
    var diagnosticPrefix = 'data-pt-lr-' + safeLayerId + '-';
    var teardownCount = 0;
    var federalData = layerData.federal_wilderness || null;
    var federalLookup = {
      semantic: Object.create(null),
      component: Object.create(null),
      agency: Object.create(null),
      office: Object.create(null),
      policy: Object.create(null)
    };

    if (federalData) {
      (federalData.semantics || []).forEach(function(row) {
        federalLookup.semantic[String(row.wilderness_id)] = row;
      });
      (federalData.components || []).forEach(function(row) {
        federalLookup.component[String(row.component_id)] = row;
      });
      (federalData.agencies || []).forEach(function(row) {
        federalLookup.agency[String(row.agency_key)] = row;
      });
      (federalData.offices || []).forEach(function(row) {
        federalLookup.office[String(row.office_key)] = row;
      });
      (federalData.policy || []).forEach(function(row) {
        federalLookup.policy[String(row.topic)] = row.language;
      });
    }

    layerData.categories.forEach(function(category) {
      categoryByKey[String(category.category_key)] = category;
    });

    layerData.records.forEach(function(record) {
      recordByGeometry[String(record.geometry_key)] = record;
    });
    Object.keys(groupTable).forEach(function(stamp) {
      var layer = groupTable[stamp];
      var id = layerId(layer);
      if (recordByGeometry[id]) layerByGeometry[id] = layer;
    });
    if (federalData) {
      Object.keys(layerByGeometry).forEach(function(key) {
        var layer = layerByGeometry[key];
        listen(layer, 'click', function(event) {
          var record = recordByGeometry[key];
          if (!record) return;
          if (layer.closeTooltip) layer.closeTooltip();
          layer.bindPopup(
            buildFederalWildernessPopup(record, federalData, federalLookup),
            {
              maxWidth: 460,
              minWidth: 400,
              autoPan: true,
              keepInView: true,
              autoPanPaddingTopLeft: L.point(16, 84),
              autoPanPaddingBottomRight: L.point(16, 24),
              className: 'pt-local-reference-tabbed-popup'
            }
          );
          layer.openPopup(event && event.latlng ? event.latlng : undefined);
        });
      });
    }

    function rootHas(layer) {
      return !!(groupRoot && groupRoot.hasLayer && groupRoot.hasLayer(layer));
    }

    function ownedLayers() {
      return Object.keys(layerByGeometry).map(function(key) {
        return layerByGeometry[key];
      });
    }

    function groupMemberCount() {
      return groupRoot && typeof groupRoot.getLayers === 'function' ?
        groupRoot.getLayers().length : ownedLayers().filter(rootHas).length;
    }

    function attachedOwnedLayerCount() {
      return ownedLayers().filter(function(layer) {
        return !!(map.hasLayer && map.hasLayer(layer));
      }).length;
    }

    function writeDiagnostics(snapshot) {
      if (!el || !el.setAttribute) return;
      var showing = snapshot && snapshot.counts ? snapshot.counts.currently_showing : null;
      el.setAttribute(diagnosticPrefix + 'active', active ? 'true' : 'false');
      el.setAttribute(
        diagnosticPrefix + 'semantic-count',
        String(active && showing ? Number(showing.semantic_feature_count || 0) : 0)
      );
      el.setAttribute(
        diagnosticPrefix + 'component-count',
        String(active && showing ? Number(showing.geometry_component_count || 0) : 0)
      );
      el.setAttribute(diagnosticPrefix + 'attached-layer-count', String(attachedOwnedLayerCount()));
      el.setAttribute(diagnosticPrefix + 'group-member-count', String(groupMemberCount()));
      el.setAttribute(diagnosticPrefix + 'card-count', card ? '1' : '0');
      el.setAttribute(
        diagnosticPrefix + 'distinguish-units', distinguishUnits ? 'true' : 'false'
      );
      el.setAttribute(
        diagnosticPrefix + 'pending-callback-count',
        suggestionCloseTimer === null ? '0' : '1'
      );
    }

    function clearDiagnostics() {
      if (!el || !el.removeAttribute) return;
      [
        'active', 'semantic-count', 'component-count', 'attached-layer-count',
        'group-member-count', 'card-count', 'distinguish-units',
        'pending-callback-count'
      ].forEach(function(name) {
        el.removeAttribute(diagnosticPrefix + name);
      });
    }

    function reconcileLayers(snapshot) {
      if (!active || !groupRoot) return;
      var visible = Object.create(null);
      snapshot.visible_geometry_keys.forEach(function(key) {
        visible[String(key)] = true;
      });
      Object.keys(layerByGeometry).forEach(function(key) {
        var layer = layerByGeometry[key];
        if (visible[key] && !rootHas(layer)) groupRoot.addLayer(layer);
        if (!visible[key] && rootHas(layer)) groupRoot.removeLayer(layer);
      });
      applyUnitStyles();
    }

    function semanticFillColor(semanticKey) {
      var hash = 2166136261;
      String(semanticKey || '').split('').forEach(function(character) {
        hash ^= character.charCodeAt(0);
        hash = Math.imul(hash, 16777619);
      });
      var hue = Math.abs(hash >>> 0) % 360;
      return 'hsl(' + hue + ',58%,52%)';
    }

    function styleForRecord(record) {
      var category = categoryByKey[String(record.category_key)] || {};
      return {
        fillColor: distinguishUnits ?
          semanticFillColor(record.semantic_feature_key) : category.fill_color,
        color: category.stroke_color,
        fillOpacity: Number(category.fill_opacity || 0),
        weight: Number(category.stroke_weight || 1),
        dashArray: String(category.dash_array || '')
      };
    }

    function applyUnitStyles() {
      if (!distinguishUnitsSupported) return;
      Object.keys(layerByGeometry).forEach(function(key) {
        var layer = layerByGeometry[key];
        var record = recordByGeometry[key];
        if (layer && record && typeof layer.setStyle === 'function') {
          layer.setStyle(styleForRecord(record));
        }
      });
    }

    function setDistinguishUnits(value, snapshot) {
      var eligible = distinguishUnitsSupported && snapshot &&
        snapshot.draft_selected.length === 1;
      distinguishUnits = eligible && value === true;
      applyUnitStyles();
    }

    function eventMatches(event) {
      return !!event && String(event.name || '') === groupName;
    }

    function setCardVisible() {
      if (!card) return;
      card.style.display = active && !hiddenByClose ? '' : 'none';
    }

    function hasUsableBounds(snapshot) {
      var bounds = snapshot.visible_semantic_feature_bounds;
      return Array.isArray(bounds) && bounds.length === 4 &&
        bounds.every(function(value) { return isFinite(Number(value)); });
    }

    function isWholeLayerView(snapshot) {
      var facetsWhole = (layerData.facets || []).every(function(facet) {
        return snapshot.applied_facets[String(facet.facet_key)].length ===
          (facet.values || []).length;
      });
      return !snapshot.applied_feature_keys.length && facetsWhole &&
        snapshot.applied_selected.length === layerData.categories.length;
    }

    function fitSnapshot(snapshot, explicit) {
      if (!hasUsableBounds(snapshot) ||
          primaryCount(snapshot.counts.currently_showing, primaryCountMode) === 0) {
        return false;
      }
      if (!explicit && (!autoZoomSupported || !snapshot.auto_zoom || isWholeLayerView(snapshot))) {
        return false;
      }
      var bounds = snapshot.visible_semantic_feature_bounds.map(Number);
      map.fitBounds(
        [[bounds[0], bounds[1]], [bounds[2], bounds[3]]],
        {
          padding: [zoomPadding, zoomPadding],
          maxZoom: zoomMax,
          animate: false
        }
      );
      return true;
    }

    function maybeAutoZoom(snapshot, action) {
      if (action === 'typing' || action === 'none' || action === 'all') {
        return false;
      }
      if (action === 'reset') {
        return preserveViewOnReset ? false : fitSnapshot(snapshot, true);
      }
      return fitSnapshot(snapshot, false);
    }

    function closeSuggestions() {
      if (suggestionCloseTimer !== null) {
        window.clearTimeout(suggestionCloseTimer);
        suggestionCloseTimer = null;
      }
      suggestions = [];
      activeSuggestionIndex = -1;
      if (suggestionList) {
        suggestionList.innerHTML = '';
        suggestionList.hidden = true;
      }
      if (searchInput) {
        searchInput.setAttribute('aria-expanded', 'false');
        searchInput.removeAttribute('aria-activedescendant');
      }
      if (searchStatus) searchStatus.textContent = '';
    }

    function clearFeaturePicker() {
      if (searchInput) searchInput.value = '';
      closeSuggestions();
    }

    function renderSuggestions(value, requestedIndex) {
      if (!featureSelectionSupported || !suggestionList || !searchInput) return;
      suggestions = engine.featureSearch(value, 8);
      activeSuggestionIndex = suggestions.length
        ? Math.min(Math.max(Number.isInteger(requestedIndex) ? requestedIndex : 0, 0), suggestions.length - 1)
        : -1;
      suggestionList.innerHTML = suggestions.map(function(feature, index) {
        var optionId = listboxId + '-option-' + index;
        return '<li id="' + optionId + '" class="pt-lr-option" role="option" tabindex="-1" ' +
          'aria-selected="' + (index === activeSuggestionIndex ? 'true' : 'false') + '" ' +
          'data-pt-lr-feature-option="' + escapeHtml(feature.semantic_feature_key) + '">' +
          escapeHtml(feature.display_name) + '</li>';
      }).join('');
      suggestionList.hidden = !suggestions.length;
      searchInput.setAttribute('aria-expanded', suggestions.length ? 'true' : 'false');
      if (activeSuggestionIndex >= 0) {
        searchInput.setAttribute(
          'aria-activedescendant',
          listboxId + '-option-' + activeSuggestionIndex
        );
      } else {
        searchInput.removeAttribute('aria-activedescendant');
      }
      if (searchStatus) {
        searchStatus.textContent = suggestions.length ?
          suggestions.length + ' matching ' + primaryCountLabel + ' available.' :
          (String(value || '').trim() ? 'No matching ' + primaryCountLabel + '.' : '');
      }
    }

    function setActiveSuggestion(nextIndex) {
      if (!suggestions.length) return;
      renderSuggestions(
        searchInput.value,
        (nextIndex + suggestions.length) % suggestions.length
      );
      searchInput.setAttribute(
        'aria-activedescendant',
        listboxId + '-option-' + activeSuggestionIndex
      );
      if (suggestionList && suggestionList.children) {
        Array.prototype.forEach.call(suggestionList.children, function(option, index) {
          option.setAttribute('aria-selected', index === activeSuggestionIndex ? 'true' : 'false');
          if (index === activeSuggestionIndex && option.scrollIntoView) {
            option.scrollIntoView({block: 'nearest'});
          }
        });
      }
    }

    function selectFeature(semanticKey) {
      var next = engine.addFeature(semanticKey);
      clearFeaturePicker();
      render(next, next.auto, next.auto ? 'feature' : '');
    }

    function renderChips(snapshot) {
      if (!chipList) return;
      chipList.innerHTML = snapshot.draft_features.map(function(feature) {
        var removeLabel = 'Remove ' + feature.display_name + ' from selected ' + primaryCountLabel;
        return '<span class="pt-lr-chip" role="listitem">' +
          '<span class="pt-lr-chip-name">' + escapeHtml(feature.display_name) + '</span>' +
          '<button type="button" class="pt-lr-chip-remove" ' +
          'data-pt-lr-remove-feature="' + escapeHtml(feature.semantic_feature_key) + '" ' +
          'aria-label="' + escapeHtml(removeLabel) + '" title="' + escapeHtml(removeLabel) + '">' +
          '<span aria-hidden="true">&times;</span></button></span>';
      }).join('');
      chipList.setAttribute(
        'aria-label',
        snapshot.draft_features.length ?
          'Selected ' + primaryCountLabel + ', ' + snapshot.draft_features.length :
          'No selected ' + primaryCountLabel
      );
    }

    function render(snapshot, reconcile, zoomAction) {
      if (!card) return;
      if (distinguishUnits && snapshot.draft_selected.length !== 1) {
        setDistinguishUnits(false, snapshot);
      }
      if (reconcile) reconcileLayers(snapshot);
      var selected = Object.create(null);
      snapshot.draft_selected.forEach(function(key) { selected[key] = true; });
      layerData.categories.forEach(function(category) {
        var key = String(category.category_key);
        var input = card.querySelector('[data-pt-lr-category="' + key + '"]');
        if (input) input.checked = !!selected[key];
        var count = snapshot.category_counts[key];
        var countNode = card.querySelector('[data-pt-lr-count="' + key + '"]');
        if (countNode && count) {
          var currentCount = primaryCount(count.currently_showing, categoryCountMode);
          var totalCount = primaryCount(count.total, categoryCountMode);
          var accessibleCount = currentCount + ' of ' + totalCount +
            (categoryCountMode === 'geometry_component' ? ' mapped components' : ' ' + primaryCountLabel);
          countNode.textContent = currentCount === totalCount ? String(totalCount) :
            currentCount + ' of ' + totalCount;
          countNode.setAttribute('aria-label', accessibleCount);
          countNode.setAttribute('title', accessibleCount);
          if (input) input.setAttribute(
            'aria-label',
            String(category.label) + ': ' + accessibleCount
          );
        }
      });
      (layerData.facets || []).forEach(function(facet) {
        var facetKey = String(facet.facet_key);
        var selectedValues = snapshot.draft_facets[facetKey] || [];
        (facet.values || []).forEach(function(value) {
          var valueKey = String(value.value_key);
          var input = card.querySelector(
            '[data-pt-lr-facet="' + facetKey + '"][data-pt-lr-facet-value="' + valueKey + '"]'
          );
          if (input) input.checked = selectedValues.indexOf(valueKey) !== -1;
          var countNode = card.querySelector(
            '[data-pt-lr-facet-count="' + facetKey + '"][data-pt-lr-facet-count-value="' + valueKey + '"]'
          );
          var count = snapshot.facet_counts[facetKey] && snapshot.facet_counts[facetKey][valueKey];
          if (countNode && count) {
            var mode = String(facet.count_mode || primaryCountMode);
            var currentCount = primaryCount(count.currently_showing, mode);
            var totalCount = primaryCount(count.total, mode);
            countNode.textContent = currentCount === totalCount ? String(totalCount) :
              currentCount + ' of ' + totalCount;
            countNode.setAttribute(
              'aria-label',
              currentCount + ' of ' + totalCount + ' matching ' + String(value.label)
            );
          }
        });
      });
      renderChips(snapshot);
      var auto = card.querySelector('.pt-lr-auto');
      if (auto) auto.checked = snapshot.auto;
      var autoZoom = card.querySelector('.pt-lr-auto-zoom');
      if (autoZoom) autoZoom.checked = snapshot.auto_zoom;
      var distinguish = card.querySelector('.pt-lr-distinguish');
      if (distinguish) {
        var distinguishEligible = snapshot.draft_selected.length === 1;
        distinguish.disabled = !distinguishEligible;
        distinguish.checked = distinguishUnits;
        distinguish.setAttribute(
          'aria-label',
          distinguishEligible ?
            'Distinguish named wildernesses within the selected agency' :
            'Select exactly one managing agency before distinguishing named wildernesses'
        );
      }
      var apply = card.querySelector('.pt-lr-apply');
      if (apply) apply.disabled = !snapshot.has_pending_changes;
      var zoomButton = card.querySelector('.pt-lr-zoom-results');
      if (zoomButton) {
        zoomButton.disabled =
          primaryCount(snapshot.counts.currently_showing, primaryCountMode) === 0 ||
          !hasUsableBounds(snapshot);
      }
      var pending = card.querySelector('.pt-lr-pending');
      if (pending) pending.textContent = snapshot.has_pending_changes ? 'Changes pending Apply.' : '';
      var summary = card.querySelector('.pt-lr-summary');
      if (summary) {
        var currentPrimary = primaryCount(snapshot.counts.currently_showing, primaryCountMode);
        var totalPrimary = primaryCount(snapshot.counts.total, primaryCountMode);
        var semanticSummary = currentPrimary + ' of ' + totalPrimary +
          ' ' + primaryCountLabel;
        var fullSummary = layerData.show_component_count ?
          'Showing ' + semanticSummary + ' · ' +
            snapshot.counts.currently_showing.geometry_component_count + ' of ' +
            snapshot.counts.total.geometry_component_count + ' ' + componentCountLabel :
          'Showing ' + semanticSummary;
        summary.textContent = federalData ?
          currentPrimary + '/' + totalPrimary + ' wildernesses · ' +
            snapshot.counts.currently_showing.geometry_component_count + '/' +
            snapshot.counts.total.geometry_component_count + ' components' :
          fullSummary;
        summary.setAttribute('aria-label', fullSummary);
        summary.setAttribute('title', fullSummary);
      }
      var mapDetails = layerData.show_component_count ?
        card.querySelector('.pt-lr-map-details-text') : null;
      if (mapDetails) {
        mapDetails.textContent =
          primaryCount(snapshot.counts.currently_showing, primaryCountMode) + ' ' +
          primaryCountLabel + ' represented by ' +
          snapshot.counts.currently_showing.geometry_component_count + ' ' +
          componentCountLabel + '.';
      }
      writeDiagnostics(snapshot);
      if (zoomAction) maybeAutoZoom(snapshot, zoomAction);
    }

    function featurePickerHtml() {
      if (!featureSelectionSupported) return '';
      return '<div class="pt-lr-feature-picker">' +
        '<label class="pt-lr-feature-label" for="' + searchId + '">Select ' +
        escapeHtml(namedFeatureLabel) + '</label>' +
        '<input id="' + searchId + '" class="pt-lr-search" type="search" role="combobox" ' +
        'aria-autocomplete="list" aria-expanded="false" aria-controls="' + listboxId + '" ' +
        'autocomplete="off" placeholder="Type a name or identifier">' +
        '<ul id="' + listboxId + '" class="pt-lr-suggestions" role="listbox" hidden></ul>' +
        '<div class="pt-lr-search-status pt-lr-visually-hidden" aria-live="polite"></div>' +
        '<div class="pt-lr-chips" role="list" aria-label="No selected ' +
        escapeHtml(primaryCountLabel) + '"></div></div>';
    }

    function wireFeaturePicker() {
      if (!featureSelectionSupported) return;
      searchInput = card.querySelector('.pt-lr-search');
      suggestionList = card.querySelector('.pt-lr-suggestions');
      searchStatus = card.querySelector('.pt-lr-search-status');
      chipList = card.querySelector('.pt-lr-chips');

      searchInput.addEventListener('input', function(event) {
        renderSuggestions(event.target.value);
      });
      searchInput.addEventListener('keydown', function(event) {
        if (event.key === 'ArrowDown') {
          event.preventDefault();
          setActiveSuggestion(activeSuggestionIndex + 1);
        } else if (event.key === 'ArrowUp') {
          event.preventDefault();
          setActiveSuggestion(activeSuggestionIndex - 1);
        } else if (event.key === 'Enter' && activeSuggestionIndex >= 0) {
          event.preventDefault();
          selectFeature(suggestions[activeSuggestionIndex].semantic_feature_key);
        } else if (event.key === 'Escape') {
          event.preventDefault();
          closeSuggestions();
        }
      });
      searchInput.addEventListener('blur', function() {
        if (suggestionCloseTimer !== null) window.clearTimeout(suggestionCloseTimer);
        suggestionCloseTimer = window.setTimeout(function() {
          suggestionCloseTimer = null;
          closeSuggestions();
        }, 120);
      });
      suggestionList.addEventListener('mousedown', function(event) {
        event.preventDefault();
      });
      suggestionList.addEventListener('click', function(event) {
        var option = event.target.closest && event.target.closest('[data-pt-lr-feature-option]');
        if (!option) return;
        selectFeature(option.getAttribute('data-pt-lr-feature-option'));
      });
      chipList.addEventListener('click', function(event) {
        var button = event.target.closest && event.target.closest('[data-pt-lr-remove-feature]');
        if (!button) return;
        var next = engine.removeFeature(button.getAttribute('data-pt-lr-remove-feature'));
        render(next, next.auto, next.auto ? 'feature' : '');
      });
    }

    function facetsHtml() {
      return (layerData.facets || []).map(function(facet) {
        var facetKey = escapeHtml(facet.facet_key);
        var values = (facet.values || []).map(function(value) {
          var valueKey = escapeHtml(value.value_key);
          return '<label class="pt-lr-facet-row"><input type="checkbox" ' +
            'data-pt-lr-facet="' + facetKey + '" data-pt-lr-facet-value="' + valueKey + '" checked>' +
            '<span>' + escapeHtml(value.label) + '</span>' +
            '<span class="pt-lr-facet-count" data-pt-lr-facet-count="' + facetKey +
            '" data-pt-lr-facet-count-value="' + valueKey + '"></span></label>';
        }).join('');
        return '<section class="pt-lr-facet" data-pt-lr-facet-section="' + facetKey + '">' +
          '<div class="pt-lr-facet-head"><div class="pt-lr-facet-heading" role="heading" aria-level="3">' +
          escapeHtml(facet.label) + '</div><div class="pt-lr-facet-toolbar">' +
          '<button type="button" data-pt-lr-facet-all="' + facetKey + '">All</button>' +
          '<button type="button" data-pt-lr-facet-none="' + facetKey + '">None</button>' +
          '</div></div><div class="pt-lr-facet-values">' + values + '</div></section>';
      }).join('');
    }

    function createCard() {
      if (card) return;
      if (!control) control = L.control({position: 'bottomleft'});
      control.onAdd = function() {
        card = L.DomUtil.create(
          'div',
          'leaflet-control pt-map-legend-card pt-map-legend-local pt-local-reference-card'
        );
        card.setAttribute('data-pt-local-reference-layer', layerData.layer_id);
        var categoryRows = layerData.categories.map(function(category) {
          var key = escapeHtml(category.category_key);
          return '<label class="pt-lr-category"><input type="checkbox" data-pt-lr-category="' + key + '" checked>' +
            swatch(category) + '<span>' + escapeHtml(category.label) + '</span>' +
            (layerData.show_category_count ?
              '<span class="pt-lr-category-count" data-pt-lr-count="' + key + '"></span>' : '') +
            '</label>';
        }).join('');
        var closeHtml = window.BRIM.legendCloseout ?
          window.BRIM.legendCloseout.actionsHtml(
            'pt-lr-dock', 'pt-lr-close', 'Local Reference filter'
          ) :
          '<button type="button" class="pt-lr-close" aria-label="Hide Local Reference filter">&times;</button>';
        var categoryHeadingHtml = String(layerData.category_heading || '') ?
          '<div class="pt-lr-category-heading" role="heading" aria-level="3">' +
          escapeHtml(layerData.category_heading) + '</div>' : '';
        var subtitleHtml = federalData ? '' : '<div>' +
          (featureSelectionSupported ?
            'Filter by category or select named features' : 'Filter by category') +
          '</div>';
        var cautionHtml = federalData ? '' :
          '<div class="pt-lr-caution">' + escapeHtml(layerData.caution) + '</div>';
        var mapDetailsHtml = layerData.show_component_count ?
          (federalData ?
            '<details class="pt-lr-map-details"><summary>Map / layer note</summary>' +
            '<div class="pt-lr-map-details-text"></div><div class="pt-lr-caution">' +
            escapeHtml(layerData.caution) + '</div></details>' :
            '<details class="pt-lr-map-details"><summary>Map details</summary>' +
            '<div class="pt-lr-map-details-text"></div></details>') : '';
        card.innerHTML =
          '<div class="pt-lr-head pt-map-card-handle"><div><div class="pt-lr-title">' +
          escapeHtml(layerData.display_name) + '</div>' + subtitleHtml +
          '</div>' + closeHtml + '</div>' +
          featurePickerHtml() +
          '<div class="pt-lr-toolbar"><button type="button" class="pt-lr-all">All</button>' +
          '<button type="button" class="pt-lr-none">None</button>' +
          (layerData.auto_supported ?
            '<label class="pt-lr-toggle pt-lr-auto-toggle"><input type="checkbox" class="pt-lr-auto"> Auto</label>' : '') +
          (autoZoomSupported ?
            '<label class="pt-lr-toggle"><input type="checkbox" class="pt-lr-auto-zoom"> Auto-zoom</label>' : '') +
          '</div>' + categoryHeadingHtml +
          '<div class="pt-lr-categories">' + categoryRows + '</div>' +
          (distinguishUnitsSupported ?
            '<div class="pt-lr-toolbar pt-lr-distinguish-row"><label class="pt-lr-toggle"><input type="checkbox" class="pt-lr-distinguish"> Distinguish named units</label>' +
            '<span class="pt-lr-distinguish-note" title="Available when exactly one managing agency is selected.">one agency only</span></div>' : '') +
          facetsHtml() +
          '<div class="pt-lr-actions"><button type="button" class="pt-lr-apply">Apply</button>' +
          '<button type="button" class="pt-lr-reset">Reset</button>' +
          (autoZoomSupported ?
            '<button type="button" class="pt-lr-zoom-results">Zoom to results</button>' : '') +
          '<span class="pt-lr-pending"></span></div>' +
          '<div class="pt-lr-summary" aria-live="polite"></div>' +
          mapDetailsHtml + cautionHtml;
        L.DomEvent.disableClickPropagation(card);
        L.DomEvent.disableScrollPropagation(card);
        wireFeaturePicker();

        card.addEventListener('change', function(event) {
          var categoryKey = event.target.getAttribute &&
            event.target.getAttribute('data-pt-lr-category');
          if (categoryKey) {
            var categoryState = engine.setCategory(categoryKey, event.target.checked);
            render(categoryState, categoryState.auto, categoryState.auto ? 'category' : '');
          }
          var facetKey = event.target.getAttribute &&
            event.target.getAttribute('data-pt-lr-facet');
          var facetValue = event.target.getAttribute &&
            event.target.getAttribute('data-pt-lr-facet-value');
          if (facetKey && facetValue) {
            var facetState = engine.setFacetValue(
              facetKey, facetValue, event.target.checked
            );
            render(facetState, facetState.auto, facetState.auto ? 'facet' : '');
          }
          if (event.target.classList.contains('pt-lr-auto')) {
            var autoState = engine.setAuto(event.target.checked);
            render(autoState, event.target.checked, event.target.checked ? 'apply' : '');
          }
          if (event.target.classList.contains('pt-lr-auto-zoom')) {
            render(engine.setAutoZoom(event.target.checked), false, '');
          }
          if (event.target.classList.contains('pt-lr-distinguish')) {
            var distinguishSnapshot = engine.snapshot();
            setDistinguishUnits(event.target.checked, distinguishSnapshot);
            render(distinguishSnapshot, false, '');
          }
        });
        if ((layerData.facets || []).length && card.querySelectorAll) {
          Array.prototype.forEach.call(
            card.querySelectorAll('[data-pt-lr-facet-all]'),
            function(button) {
              button.addEventListener('click', function() {
                var next = engine.facetAll(button.getAttribute('data-pt-lr-facet-all'));
                render(next, next.auto, next.auto ? 'facet' : '');
              });
            }
          );
          Array.prototype.forEach.call(
            card.querySelectorAll('[data-pt-lr-facet-none]'),
            function(button) {
              button.addEventListener('click', function() {
                var next = engine.facetNone(button.getAttribute('data-pt-lr-facet-none'));
                render(next, next.auto, next.auto ? 'facet' : '');
              });
            }
          );
        }
        card.querySelector('.pt-lr-all').addEventListener('click', function() {
          var next = engine.all();
          render(next, next.auto, 'all');
        });
        card.querySelector('.pt-lr-none').addEventListener('click', function() {
          var next = engine.none();
          render(next, next.auto, 'none');
        });
        card.querySelector('.pt-lr-apply').addEventListener('click', function() {
          var next = engine.apply();
          render(next, true, 'apply');
        });
        card.querySelector('.pt-lr-reset').addEventListener('click', function() {
          clearFeaturePicker();
          var resetSnapshot = engine.reset();
          setDistinguishUnits(false, resetSnapshot);
          render(resetSnapshot, true, 'reset');
        });
        var zoomButton = card.querySelector('.pt-lr-zoom-results');
        if (zoomButton) {
          zoomButton.addEventListener('click', function() {
            fitSnapshot(engine.snapshot(), true);
          });
        }
        if (window.BRIM.legendCloseout) {
          window.BRIM.legendCloseout.wire(card, '.pt-lr-close', function() {
            hiddenByClose = true;
          });
          detachable = window.BRIM.legendCloseout.makeDetachable({
            card: card,
            map: map,
            label: 'Local Reference filter',
            dockSelector: '.pt-lr-dock',
            handleSelector: '.pt-map-card-handle'
          });
        } else {
          card.querySelector('.pt-lr-close').addEventListener('click', function() {
            hiddenByClose = true;
            setCardVisible();
          });
        }
        render(engine.snapshot(), true, '');
        setCardVisible();
        return card;
      };
      control.addTo(map);
    }

    function removeCard() {
      if (detachable && typeof detachable.destroy === 'function') {
        detachable.destroy(true, true);
      }
      detachable = null;
      if (control && typeof control.remove === 'function') control.remove();
      card = null;
      searchInput = null;
      suggestionList = null;
      searchStatus = null;
      chipList = null;
    }

    function closeLayerPopup() {
      if (String(layerData.popup_layout || '') !== 'tabbed_card') return false;
      var popup = map._popup;
      var source = popup && popup._source;
      if (!source || !recordByGeometry[layerId(source)]) return false;
      map.closePopup(popup);
      return true;
    }

    function closeOwnedPresentation() {
      var popupClosed = closeLayerPopup();
      Object.keys(layerByGeometry).forEach(function(key) {
        var layer = layerByGeometry[key];
        if (layer && typeof layer.closeTooltip === 'function') {
          try { layer.closeTooltip(); } catch (tooltipError) {}
        }
        if (layer && typeof layer.closePopup === 'function') {
          try { layer.closePopup(); } catch (popupError) {}
        }
      });
      if (popupClosed) cleanupTabbedPopupLayout();
    }

    function detachOwnedGeometry() {
      var groupLayers = groupRoot && typeof groupRoot.getLayers === 'function' ?
        groupRoot.getLayers().slice() : [];
      Object.keys(layerByGeometry).forEach(function(key) {
        var layer = layerByGeometry[key];
        if (rootHas(layer) && groupRoot.removeLayer) groupRoot.removeLayer(layer);
        if (map.hasLayer && map.hasLayer(layer) && map.removeLayer) map.removeLayer(layer);
      });
      // The Local Reference group is an exclusive owner for one user-facing
      // registry row. Clear any unmatched residue as well as registered
      // components so no parallel or stale geometry can survive layer-off.
      groupLayers.forEach(function(layer) {
        if (groupRoot && groupRoot.hasLayer && groupRoot.hasLayer(layer) && groupRoot.removeLayer) {
          groupRoot.removeLayer(layer);
        }
        if (map.hasLayer && map.hasLayer(layer) && map.removeLayer) map.removeLayer(layer);
      });
      if (groupRoot && typeof groupRoot.clearLayers === 'function') groupRoot.clearLayers();
      if (groupRoot && map.hasLayer && map.hasLayer(groupRoot) && map.removeLayer) {
        map.removeLayer(groupRoot);
      }
    }

    function teardownInactiveLayer() {
      active = false;
      hiddenByClose = false;
      closeSuggestions();
      closeOwnedPresentation();
      detachOwnedGeometry();
      var resetSnapshot = engine.reset();
      setDistinguishUnits(false, resetSnapshot);
      removeCard();
      teardownCount += 1;
      writeDiagnostics(resetSnapshot);
    }

    function resetController() {
      if (!active) {
        teardownInactiveLayer();
        return;
      }
      closeOwnedPresentation();
      clearFeaturePicker();
      var resetSnapshot = engine.reset();
      setDistinguishUnits(false, resetSnapshot);
      render(resetSnapshot, true, 'reset');
    }

    function onOverlayAdd(event) {
      if (!eventMatches(event)) return;
      active = true;
      hiddenByClose = false;
      closeSuggestions();
      var resetSnapshot = engine.reset();
      setDistinguishUnits(false, resetSnapshot);
      createCard();
      render(engine.snapshot(), true, '');
      setCardVisible();
    }

    function onOverlayRemove(event) {
      if (!eventMatches(event)) return;
      teardownInactiveLayer();
    }

    function onPopupOpen(event) {
      var source = event && event.popup && event.popup._source;
      if (!source) return;
      var id = layerId(source);
      if (!recordByGeometry[id]) return;
      if (source.closeTooltip) source.closeTooltip();
    }

    function destroy() {
      teardownInactiveLayer();
      clearDiagnostics();
      control = null;
      recordByGeometry = Object.create(null);
      layerByGeometry = Object.create(null);
    }

    if (active) createCard();
    else teardownInactiveLayer();
    return {
      layerId: layerData.layer_id,
      reset: resetController,
      snapshot: engine.snapshot,
      destroy: destroy,
      onOverlayAdd: onOverlayAdd,
      onOverlayRemove: onOverlayRemove,
      onPopupOpen: onPopupOpen,
      resolvedLayerCount: function() { return Object.keys(layerByGeometry).length; },
      expectedLayerCount: function() { return layerData.records.length; },
      diagnostics: function() {
        var owned = ownedLayers();
        return {
          active: active,
          group_root_attached: !!(groupRoot && map.hasLayer && map.hasLayer(groupRoot)),
          group_member_layer_count: groupMemberCount(),
          attached_owned_layer_count: attachedOwnedLayerCount(),
          open_owned_tooltip_count: owned.filter(function(layer) {
            return !!(layer && layer._tooltip &&
              typeof layer.isTooltipOpen === 'function' && layer.isTooltipOpen());
          }).length,
          card_count: card ? 1 : 0,
          pending_controller_callback_count: suggestionCloseTimer === null ? 0 : 1,
          controller_created_group_count: 0,
          distinguish_units: distinguishUnits,
          teardown_count: teardownCount
        };
      }
    };
  }

  installCss();
  listenDom(el, 'click', onTabbedPopupClick);
  listenDom(el, 'keydown', onTabbedPopupKeydown);
  // The native details toggle event does not bubble consistently. Capture it
  // once at the shared map root so Trails and WSA use the same layout path.
  listenDom(el, 'toggle', onTabbedPopupDetailsToggle, true);
  if (window.addEventListener) {
    listenDom(window, 'resize', onTabbedPopupViewportResize);
  }
  listen(map, 'popupclose', onAnyPopupClose);
  payloads.forEach(function(payload) {
    controllers.push(createLayerController(payload));
  });
  listen(map, 'overlayadd', function(event) {
    controllers.forEach(function(controller) { controller.onOverlayAdd(event); });
  });
  listen(map, 'overlayremove', function(event) {
    controllers.forEach(function(controller) { controller.onOverlayRemove(event); });
  });
  listen(map, 'popupopen', function(event) {
    onAnyPopupOpen(event);
    controllers.forEach(function(controller) { controller.onPopupOpen(event); });
  });

  function destroy() {
    if (destroyed) return;
    destroyed = true;
    cleanupTabbedPopupLayout();
    listenerRecords.forEach(function(record) {
      try { record.target.off(record.names, record.handler); } catch (error) {}
    });
    listenerRecords = [];
    domListenerRecords.forEach(function(record) {
      try {
        record.target.removeEventListener(record.name, record.handler, record.options);
      } catch (error) {}
    });
    domListenerRecords = [];
    controllers.forEach(function(controller) { controller.destroy(); });
    controllers = [];
    if (el && el.classList) el.classList.remove('pt-lr-tabbed-popup-open');
  }

  listen(map, 'unload', destroy);
  window.BRIM.localReferenceController = {
    reset: function() {
      controllers.forEach(function(controller) { controller.reset(); });
    },
    stats: function() {
      return controllers.map(function(controller) {
        var snapshot = controller.snapshot();
        snapshot.layer_id = controller.layerId;
        snapshot.resolved_leaflet_layers = controller.resolvedLayerCount();
        snapshot.expected_leaflet_layers = controller.expectedLayerCount();
        var diagnostics = controller.diagnostics();
        Object.keys(diagnostics).forEach(function(key) {
          snapshot[key] = diagnostics[key];
        });
        return snapshot;
      });
    },
    destroy: destroy
  };
}
