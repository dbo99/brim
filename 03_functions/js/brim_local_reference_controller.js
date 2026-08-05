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

  function listen(target, names, handler) {
    target.on(names, handler);
    listenerRecords.push({target: target, names: names, handler: handler});
  }

  function listenDom(target, name, handler) {
    target.addEventListener(name, handler);
    domListenerRecords.push({target: target, name: name, handler: handler});
  }

  function escapeHtml(value) {
    return String(value === undefined || value === null ? '' : value)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#039;');
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
      '.pt-lr-category-heading{max-width:100%;margin:6px 0 3px;color:#544c3e;font-size:11px;font-weight:700;line-height:1.25;white-space:normal;overflow-wrap:break-word;word-break:normal}.pt-lr-categories{border-top:1px solid rgba(82,72,45,.23)}' +
      '.pt-lr-category{display:grid;grid-template-columns:18px 25px minmax(0,1fr) auto;align-items:center;gap:5px;padding:5px 0;border-bottom:1px solid rgba(82,72,45,.14)}' +
      '.pt-lr-swatch{display:inline-block;width:19px;height:13px;box-sizing:border-box}.pt-lr-swatch-line{height:0;border-left:0!important;border-right:0!important;border-bottom:0!important}' +
      '.pt-lr-category-count{color:#555;font-variant-numeric:tabular-nums;white-space:nowrap}.pt-lr-summary{margin:6px 0;color:#3d3a35}.pt-lr-pending{font-weight:700;color:#8a4d00}' +
      '.pt-lr-caution{margin-top:7px;padding-top:6px;border-top:1px solid rgba(82,72,45,.26);color:#5a4634;font-size:11px}' +
      '.pt-lr-visually-hidden{position:absolute!important;width:1px!important;height:1px!important;padding:0!important;margin:-1px!important;overflow:hidden!important;clip:rect(0,0,0,0)!important;white-space:nowrap!important;border:0!important}' +
      '.leaflet-tooltip.pt-wsa-hover-tooltip,.leaflet-tooltip.pt-trails-hover-tooltip{white-space:normal!important;width:fit-content!important;min-width:min(220px,calc(100vw - 32px))!important;max-width:min(320px,calc(100vw - 32px))!important;overflow-wrap:break-word!important;word-break:normal!important;line-height:1.3!important;box-sizing:border-box}' +
      '.pt-wsa-hover-lines{display:block;max-width:100%}.pt-wsa-hover-line{display:block;white-space:normal}.pt-wsa-hover-name{font-weight:600}' +
      '.pt-trails-hover-lines{display:block;max-width:100%}.pt-trails-hover-line{display:block;white-space:normal}.pt-trails-hover-name{font-weight:600}' +
      '.pt-wsa-popup .pt-popup-subtitle{margin-top:2px;color:#555;font-size:12px}.pt-wsa-popup .pt-popup-section{margin-top:7px}.pt-wsa-popup .pt-wsa-caution{margin-top:8px;padding:6px;background:#fff3cf;border-left:3px solid #a86f00}.pt-wsa-source-anomaly{color:#8a2f1c}.pt-popup-technical{margin-top:7px;font-size:11px}' +
      '.leaflet-popup.pt-local-reference-tabbed-popup .leaflet-popup-content-wrapper{padding:0;overflow:hidden}.leaflet-popup.pt-local-reference-tabbed-popup .leaflet-popup-content{box-sizing:border-box;width:min(430px,calc(100vw - 72px))!important;min-width:min(400px,calc(100vw - 72px))!important;max-width:min(460px,calc(100vw - 72px))!important;margin:10px 12px 12px}' +
      '.leaflet-container.pt-lr-tabbed-popup-open .leaflet-popup-pane{z-index:1100}' +
      '.pt-local-reference-tabbed-popup-card{display:flex;max-height:min(72vh,620px);min-height:0;flex-direction:column;overflow:hidden;color:#272727;font:12px/1.4 Arial,sans-serif}.pt-lr-popup-sticky{position:sticky;top:0;z-index:2;flex:0 0 auto;background:#fff}.pt-lr-popup-header{display:flex;align-items:flex-start;justify-content:space-between;gap:8px;padding:2px 1px 9px}.pt-lr-popup-title{font-size:15px;font-weight:700;line-height:1.2}.pt-lr-popup-badge{flex:0 0 auto;padding:2px 6px;border:1px solid #8d8370;border-radius:10px;background:#f4eee1;color:#493f31;font-size:10px;line-height:1.25;white-space:nowrap}' +
      '.pt-lr-popup-tabs{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:2px;border-bottom:1px solid #8f8778}.pt-lr-popup-tab{min-width:0;padding:6px 4px;border:1px solid transparent;border-bottom:0;border-radius:4px 4px 0 0;background:#eee8dc;color:#3d3933;font:600 11px/1.2 Arial,sans-serif;white-space:normal;cursor:pointer}.pt-lr-popup-tab[aria-selected=true]{border-color:#8f8778;background:#fff;color:#171717}.pt-lr-popup-tab:focus-visible{outline:3px solid #1d6fa5;outline-offset:-2px}' +
      '.pt-local-reference-tabbed-popup-card button:enabled,.pt-local-reference-tabbed-popup-card summary{cursor:pointer}.pt-local-reference-tabbed-popup-card button:disabled{cursor:not-allowed}' +
      '.pt-lr-popup-panel-scroll{height:min(54vh,450px);min-height:0;overflow-y:auto;overflow-x:hidden;overscroll-behavior:contain}.pt-lr-popup-panel{padding:9px 2px 4px}.pt-lr-popup-panel[hidden]{display:none!important}.pt-lr-popup-summary,.pt-lr-popup-panel p{margin:0 0 8px}.pt-lr-popup-row{margin:3px 0}.pt-lr-popup-label,.pt-lr-popup-evidence>span,.pt-trails-caution>span{font-weight:700}.pt-lr-popup-section{margin-top:10px}.pt-lr-popup-section h3{margin:0 0 4px;color:#3e392f;font-size:12px;line-height:1.25}.pt-lr-popup-narrative{padding-top:2px;border-top:1px solid rgba(82,72,45,.18)}.pt-lr-narrative-source{margin-top:3px;color:#5c574f;font-size:10.5px}.pt-lr-narrative-source span{font-weight:700}.pt-lr-popup-evidence{margin-top:7px}.pt-lr-popup-resource-list{margin:0;padding-left:19px}.pt-lr-popup-resource-list li{margin:4px 0}.pt-lr-popup-note{margin:1px 0 4px;color:#5b5650;font-size:10.5px}.pt-trails-popup .pt-trails-caution{margin-top:9px;padding:6px;background:#fff3cf;border-left:3px solid #a86f00}.pt-trails-popup .pt-popup-technical{margin-top:10px;padding-top:6px;border-top:1px solid rgba(82,72,45,.2)}' +
      '@media (max-width:520px){.leaflet-container.pt-lr-tabbed-popup-open .leaflet-control-container{visibility:hidden}.leaflet-popup.pt-local-reference-tabbed-popup .leaflet-popup-content{width:calc(100vw - 56px)!important;min-width:0!important;max-width:calc(100vw - 56px)!important;margin:9px 10px 11px}.pt-lr-popup-tabs{grid-template-columns:repeat(2,minmax(0,1fr))}.pt-lr-popup-badge{max-width:42%;white-space:normal;text-align:center}.pt-lr-popup-panel-scroll{height:min(50vh,390px)}}' +
      '@media (max-width:420px){.pt-local-reference-card{width:calc(100vw - 28px)}.pt-lr-toolbar .pt-lr-auto-toggle{margin-left:0}.pt-lr-chip{width:100%;box-sizing:border-box}.pt-lr-chip-remove{margin-left:auto}}' +
      '@media (pointer:coarse){.leaflet-tooltip.pt-wsa-hover-tooltip,.leaflet-tooltip.pt-trails-hover-tooltip{display:none!important}.pt-local-reference-card button,.pt-local-reference-card input{min-height:38px}.pt-lr-category{min-height:34px}.pt-local-reference-card{max-height:58vh}.pt-lr-chip-remove{min-width:38px}}';
    document.head.appendChild(style);
  }

  function popupRoot(node) {
    return node && node.closest ? node.closest('[data-pt-lr-tabbed-popup]') : null;
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
  }

  function onAnyPopupClose() {
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
    var hiddenByClose = false;
    var active = !!(groupRoot && map.hasLayer && map.hasLayer(groupRoot));
    var primaryCountMode = String(layerData.primary_count_mode || 'semantic_feature');
    var primaryCountLabel = String(layerData.primary_count_label || layerData.display_name || 'features');
    var componentCountLabel = String(layerData.component_count_label || 'mapped components');
    var featureSelectionSupported = layerData.feature_selection_supported === true;
    var autoZoomSupported = layerData.auto_zoom_supported === true;
    var zoomPadding = Math.max(0, Number(layerData.zoom_padding) || 0);
    var zoomMax = Math.max(1, Number(layerData.zoom_max) || 12);
    var preserveViewOnReset = layerData.preserve_view_on_reset !== false;
    var safeLayerId = String(layerData.layer_id || 'layer').replace(/[^A-Za-z0-9_-]/g, '-');
    var searchId = 'pt-lr-search-' + safeLayerId;
    var listboxId = 'pt-lr-listbox-' + safeLayerId;

    layerData.records.forEach(function(record) {
      recordByGeometry[String(record.geometry_key)] = record;
    });
    Object.keys(groupTable).forEach(function(stamp) {
      var layer = groupTable[stamp];
      var id = layerId(layer);
      if (recordByGeometry[id]) layerByGeometry[id] = layer;
    });

    function rootHas(layer) {
      return !!(groupRoot && groupRoot.hasLayer && groupRoot.hasLayer(layer));
    }

    function reconcileLayers(snapshot) {
      if (!groupRoot) return;
      var visible = Object.create(null);
      snapshot.visible_geometry_keys.forEach(function(key) {
        visible[String(key)] = true;
      });
      Object.keys(layerByGeometry).forEach(function(key) {
        var layer = layerByGeometry[key];
        if (visible[key] && !rootHas(layer)) groupRoot.addLayer(layer);
        if (!visible[key] && rootHas(layer)) groupRoot.removeLayer(layer);
      });
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
      return !snapshot.applied_feature_keys.length &&
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
          var currentCount = primaryCount(count.currently_showing, primaryCountMode);
          var totalCount = primaryCount(count.total, primaryCountMode);
          var accessibleCount = currentCount + ' of ' + totalCount + ' ' + primaryCountLabel;
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
      renderChips(snapshot);
      var auto = card.querySelector('.pt-lr-auto');
      if (auto) auto.checked = snapshot.auto;
      var autoZoom = card.querySelector('.pt-lr-auto-zoom');
      if (autoZoom) autoZoom.checked = snapshot.auto_zoom;
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
        summary.textContent = 'Showing ' +
          primaryCount(snapshot.counts.currently_showing, primaryCountMode) + ' of ' +
          primaryCount(snapshot.counts.total, primaryCountMode) + ' ' +
          primaryCountLabel;
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
      if (zoomAction) maybeAutoZoom(snapshot, zoomAction);
    }

    function featurePickerHtml() {
      if (!featureSelectionSupported) return '';
      return '<div class="pt-lr-feature-picker">' +
        '<label class="pt-lr-feature-label" for="' + searchId + '">Select named ' +
        escapeHtml(primaryCountLabel) + '</label>' +
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
        setTimeout(closeSuggestions, 120);
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

    function createCard() {
      control = L.control({position: 'bottomleft'});
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
        var mapDetailsHtml = layerData.show_component_count ?
          '<details class="pt-lr-map-details"><summary>Map details</summary><div class="pt-lr-map-details-text"></div></details>' : '';
        var categoryHeadingHtml = String(layerData.category_heading || '') ?
          '<div class="pt-lr-category-heading" role="heading" aria-level="3">' +
          escapeHtml(layerData.category_heading) + '</div>' : '';
        card.innerHTML =
          '<div class="pt-lr-head pt-map-card-handle"><div><div class="pt-lr-title">' +
          escapeHtml(layerData.display_name) + '</div><div>' +
          (featureSelectionSupported ?
            'Filter by category or select named features' : 'Filter by category') +
          '</div></div>' + closeHtml + '</div>' +
          featurePickerHtml() +
          '<div class="pt-lr-toolbar"><button type="button" class="pt-lr-all">All</button>' +
          '<button type="button" class="pt-lr-none">None</button>' +
          (layerData.auto_supported ?
            '<label class="pt-lr-toggle pt-lr-auto-toggle"><input type="checkbox" class="pt-lr-auto"> Auto</label>' : '') +
          (autoZoomSupported ?
            '<label class="pt-lr-toggle"><input type="checkbox" class="pt-lr-auto-zoom"> Auto-zoom</label>' : '') +
          '</div>' + categoryHeadingHtml +
          '<div class="pt-lr-categories">' + categoryRows + '</div>' +
          '<div class="pt-lr-actions"><button type="button" class="pt-lr-apply">Apply</button>' +
          '<button type="button" class="pt-lr-reset">Reset</button>' +
          (autoZoomSupported ?
            '<button type="button" class="pt-lr-zoom-results">Zoom to results</button>' : '') +
          '<span class="pt-lr-pending"></span></div>' +
          '<div class="pt-lr-summary" aria-live="polite"></div>' +
          mapDetailsHtml + '<div class="pt-lr-caution">' +
          escapeHtml(layerData.caution) + '</div>';
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
          if (event.target.classList.contains('pt-lr-auto')) {
            var autoState = engine.setAuto(event.target.checked);
            render(autoState, event.target.checked, event.target.checked ? 'apply' : '');
          }
          if (event.target.classList.contains('pt-lr-auto-zoom')) {
            render(engine.setAutoZoom(event.target.checked), false, '');
          }
        });
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
          render(engine.reset(), true, 'reset');
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

    function closeLayerPopup() {
      if (String(layerData.popup_layout || '') !== 'tabbed_card') return false;
      var popup = map._popup;
      var source = popup && popup._source;
      if (!source || !recordByGeometry[layerId(source)]) return false;
      map.closePopup(popup);
      return true;
    }

    function resetController() {
      closeLayerPopup();
      clearFeaturePicker();
      render(engine.reset(), true, 'reset');
    }

    function onOverlayAdd(event) {
      if (!eventMatches(event)) return;
      active = true;
      hiddenByClose = false;
      render(engine.snapshot(), true, '');
      setCardVisible();
    }

    function onOverlayRemove(event) {
      if (!eventMatches(event)) return;
      active = false;
      hiddenByClose = false;
      resetController();
      setCardVisible();
    }

    function onPopupOpen(event) {
      var source = event && event.popup && event.popup._source;
      if (!source) return;
      var id = layerId(source);
      if (!recordByGeometry[id]) return;
      if (source.closeTooltip) source.closeTooltip();
    }

    function destroy() {
      closeLayerPopup();
      if (detachable && typeof detachable.destroy === 'function') {
        detachable.destroy(true, true);
      }
      detachable = null;
      if (control && typeof control.remove === 'function') control.remove();
      card = null;
      control = null;
      searchInput = null;
      suggestionList = null;
      searchStatus = null;
      chipList = null;
    }

    listen(map, 'overlayadd', onOverlayAdd);
    listen(map, 'overlayremove', onOverlayRemove);
    listen(map, 'popupopen', onPopupOpen);
    createCard();
    return {
      layerId: layerData.layer_id,
      reset: resetController,
      snapshot: engine.snapshot,
      destroy: destroy,
      resolvedLayerCount: function() { return Object.keys(layerByGeometry).length; },
      expectedLayerCount: function() { return layerData.records.length; }
    };
  }

  installCss();
  listenDom(el, 'click', onTabbedPopupClick);
  listenDom(el, 'keydown', onTabbedPopupKeydown);
  listen(map, 'popupopen', onAnyPopupOpen);
  listen(map, 'popupclose', onAnyPopupClose);
  payloads.forEach(function(payload) {
    controllers.push(createLayerController(payload));
  });

  function destroy() {
    if (destroyed) return;
    destroyed = true;
    listenerRecords.forEach(function(record) {
      try { record.target.off(record.names, record.handler); } catch (error) {}
    });
    listenerRecords = [];
    domListenerRecords.forEach(function(record) {
      try {
        record.target.removeEventListener(record.name, record.handler);
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
        return snapshot;
      });
    },
    destroy: destroy
  };
}
