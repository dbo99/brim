function brimBulletin118ThemeController(el, x, bulletinData) {
  if (
    window.BRIM_BULLETIN118_LOCAL &&
    typeof window.BRIM_BULLETIN118_LOCAL.destroy === 'function'
  ) {
    try {
      window.BRIM_BULLETIN118_LOCAL.destroy();
    } catch (oldControllerError) {
      console.warn(
        'BRIM Bulletin 118 controller: prior cleanup failed.',
        oldControllerError
      );
    }
  }

  var map = this;
  var destroyed = false;
  var active = false;
  var currentTheme = 'basins';
  var appliedTheme = null;
  var cardUserHidden = false;
  var card = null;
  var cardControl = null;
  var detachableState = null;
  var select = null;
  var legendBody = null;
  var statusNode = null;
  var minimumSlider = null;
  var minimumValueNode = null;
  var visibleCountNode = null;
  var searchInput = null;
  var searchClear = null;
  var searchResults = null;
  var searchMatches = [];
  var searchActiveIndex = -1;
  var currentTooltip = null;
  var currentTooltipLayer = null;
  var currentPopup = null;
  var currentMinimumBlm = 0;
  var visibleBasinCount = 0;
  var styleGeneration = 0;
  var scheduledStyleFrame = null;
  var filterGeneration = 0;
  var scheduledFilterFrame = null;
  var listenerRecords = [];
  var domListenerRecords = [];

  bulletinData = bulletinData || {};
  currentTheme = bulletinData.default_theme || 'basins';

  var allowedThemes = {
    basins: true,
    sgma_2019: true,
    blm_pct: true
  };
  var records = bulletinData.records || [];
  var recordById = {};
  var layerById = {};
  var searchIndex = [];
  records.forEach(function(record) {
    if (!record || record.layer_id === undefined || record.layer_id === null) {
      return;
    }
    recordById[String(record.layer_id)] = record;
  });

  var groupName = String(bulletinData.group_name || '');
  var layerManager = map.layerManager || {};
  var byGroup = layerManager._byGroup || {};
  var groupTable = byGroup[groupName] || {};
  var groupRoot = layerManager._groupContainers ?
    layerManager._groupContainers[groupName] :
    null;
  var layers = [];
  var renderer = null;

  function layerId(layer) {
    if (!layer || !layer.options) return null;
    if (layer.options.layerId !== undefined && layer.options.layerId !== null) {
      return String(layer.options.layerId);
    }
    if (layer.options.layer_id !== undefined && layer.options.layer_id !== null) {
      return String(layer.options.layer_id);
    }
    return null;
  }

  Object.keys(groupTable).forEach(function(stamp) {
    var layer = groupTable[stamp];
    var id = layerId(layer);
    if (!layer || typeof layer.setStyle !== 'function' || !recordById[id]) {
      return;
    }
    layers.push(layer);
    layerById[id] = layer;
    if (!renderer && layer.options) {
      renderer = layer.options.renderer || layer._renderer || null;
    }
  });

  records.forEach(function(record) {
    if (!record || record.layer_id === undefined || record.layer_id === null) {
      return;
    }
    var fields = [
      record.display_label,
      record.basin_name,
      record.subbasin_name,
      record.subbasin_num,
      record.basin_num
    ].filter(function(value) {
      return value !== undefined && value !== null && String(value).trim() !== '';
    }).map(function(value) {
      return String(value).trim();
    });
    var displayLabel = String(
      record.display_label ||
      record.subbasin_name ||
      record.basin_name ||
      record.subbasin_num ||
      record.layer_id
    );
    searchIndex.push({
      layer_id: String(record.layer_id),
      label: displayLabel,
      code: String(record.subbasin_num || record.basin_num || ''),
      haystack: fields.join('\u0000').toLowerCase()
    });
  });

  visibleBasinCount = records.length;

  if (groupRoot && map.hasLayer) active = !!map.hasLayer(groupRoot);

  var diagnostics = {
    installedAt: Date.now(),
    mapScanCount: 0,
    styleJobCount: 0,
    styleOperationCount: 0,
    optionPrimeCount: 0,
    canceledStyleJobs: 0,
    sameThemeNoops: 0,
    activationCount: 0,
    deactivationCount: 0,
    legendUpdateCount: 0,
    filterReconciliationCount: 0,
    filterOperationCount: 0,
    canceledFilterJobs: 0,
    sameThresholdNoops: 0,
    searchQueryCount: 0,
    searchSelectionCount: 0,
    lastApplyMs: null,
    lastApplyReason: null,
    lastFilterReason: null
  };

  function nowMs() {
    return (
      window.performance &&
      typeof window.performance.now === 'function'
    ) ? window.performance.now() : Date.now();
  }

  function listen(target, eventNames, handler) {
    if (!target || typeof target.on !== 'function') return;
    target.on(eventNames, handler);
    listenerRecords.push({
      target: target,
      eventNames: eventNames,
      handler: handler
    });
  }

  function listenDom(target, eventName, handler) {
    if (!target || typeof target.addEventListener !== 'function') return;
    target.addEventListener(eventName, handler, false);
    domListenerRecords.push({
      target: target,
      eventName: eventName,
      handler: handler
    });
  }

  function escapeHtml(value) {
    return String(value === undefined || value === null ? '' : value)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#39;');
  }

  function addCardCss() {
    if (document.getElementById('pt-bulletin118-theme-card-css')) return;
    var style = document.createElement('style');
    style.id = 'pt-bulletin118-theme-card-css';
    style.textContent =
      '.pt-bulletin118-theme-card{' +
        'width:300px;max-width:calc(100vw - 24px);box-sizing:border-box;' +
        'padding:8px;border:1px solid rgba(111,89,52,.58);border-radius:6px;' +
        'box-shadow:0 1px 5px rgba(0,0,0,.30);font:12px/1.25 Arial,sans-serif;' +
        'color:#222;}' +
      '.pt-bulletin118-theme-head{display:flex;align-items:flex-start;' +
        'justify-content:space-between;gap:8px;margin-bottom:7px;}' +
      '.pt-bulletin118-theme-title{font-weight:700;font-size:13px;' +
        'line-height:1.2;color:#3f2918;}' +
      '.pt-bulletin118-theme-display{display:grid;grid-template-columns:auto 1fr;' +
        'align-items:center;gap:5px;margin-bottom:7px;}' +
      '.pt-bulletin118-theme-display label{font-weight:700;}' +
      '.pt-bulletin118-theme-select{min-width:0;width:100%;font-size:12px;}' +
      '.pt-bulletin118-filter{border-top:1px solid rgba(111,89,52,.23);' +
        'padding-top:6px;margin-top:2px;}' +
      '.pt-bulletin118-filter-head{display:flex;align-items:baseline;' +
        'justify-content:space-between;gap:6px;font-weight:700;}' +
      '.pt-bulletin118-filter-value{font-variant-numeric:tabular-nums;' +
        'color:#3f2918;}' +
      '.pt-bulletin118-minimum{display:block;width:100%;margin:3px 0 1px;}' +
      '.pt-bulletin118-visible-count{font-size:10.5px;color:#555;' +
        'font-variant-numeric:tabular-nums;}' +
      '.pt-bulletin118-search{border-top:1px solid rgba(111,89,52,.23);' +
        'padding-top:6px;margin-top:6px;}' +
      '.pt-bulletin118-search-row{display:grid;' +
        'grid-template-columns:minmax(0,1fr) auto;gap:4px;}' +
      '.pt-bulletin118-search-input{width:100%;min-width:0;box-sizing:border-box;' +
        'border:1px solid #b8ad95;border-radius:3px;padding:3px 5px;' +
        'font:11px/1.2 Arial,sans-serif;}' +
      '.pt-bulletin118-search-clear{border:1px solid #b8ad95;border-radius:3px;' +
        'background:#f8f5ec;color:#555;padding:1px 7px;cursor:pointer;}' +
      '.pt-bulletin118-search-results{display:grid;gap:2px;max-height:150px;' +
        'overflow-y:auto;overscroll-behavior:contain;margin-top:3px;}' +
      '.pt-bulletin118-search-results[hidden]{display:none;}' +
      '.pt-bulletin118-search-result{display:grid;width:100%;box-sizing:border-box;' +
        'grid-template-columns:minmax(0,1fr) auto;gap:5px;text-align:left;' +
        'border:1px solid #c8bfae;border-radius:3px;background:#fffdf8;' +
        'padding:3px 5px;color:#222;cursor:pointer;font:10.5px/1.15 Arial,sans-serif;}' +
      '.pt-bulletin118-search-result.is-active,' +
        '.pt-bulletin118-search-result:hover{background:#efe4c9;' +
        'border-color:#8b6b3e;}' +
      '.pt-bulletin118-search-result-label{min-width:0;overflow:hidden;' +
        'text-overflow:ellipsis;white-space:nowrap;}' +
      '.pt-bulletin118-search-result-code{color:#655b4c;white-space:nowrap;' +
        'font-variant-numeric:tabular-nums;}' +
      '.pt-bulletin118-search-empty{font-size:10.5px;color:#666;padding:3px 1px;}' +
      '.pt-bulletin118-theme-legend-title{font-weight:700;margin-bottom:4px;}' +
      '.pt-bulletin118-theme-legend{border-top:1px solid rgba(111,89,52,.23);' +
        'padding-top:6px;margin-top:6px;}' +
      '.pt-bulletin118-theme-row{display:flex;align-items:center;gap:5px;' +
        'margin:2px 0;min-width:0;}' +
      '.pt-bulletin118-theme-swatch{width:13px;height:13px;flex:0 0 13px;' +
        'border:1px solid #777;box-sizing:border-box;}' +
      '.pt-bulletin118-theme-row-label{flex:1 1 auto;min-width:0;}' +
      '.pt-bulletin118-theme-row-count{flex:0 0 auto;color:#555;' +
        'font-variant-numeric:tabular-nums;}' +
      '.pt-bulletin118-theme-note{margin-top:5px;color:#555;font-size:10.5px;}' +
      '.pt-bulletin118-theme-note a{color:#5a381e;}' +
      '.pt-bulletin118-theme-status{display:none;margin-top:5px;color:#24527a;' +
        'font-size:10.5px;}';
    document.head.appendChild(style);
  }

  function actionsHtml() {
    if (
      window.BRIM &&
      window.BRIM.legendCloseout &&
      typeof window.BRIM.legendCloseout.actionsHtml === 'function'
    ) {
      return window.BRIM.legendCloseout.actionsHtml(
        'pt-bulletin118-theme-dock',
        'pt-bulletin118-theme-close',
        'Bulletin 118 thematic card'
      );
    }
    return '<span class="pt-map-card-actions">' +
      '<button type="button" class="pt-map-card-dock ' +
        'pt-bulletin118-theme-dock" title="Undock Bulletin 118 thematic card">' +
        '&#x2197;</button>' +
      '<button type="button" class="pt-map-legend-close ' +
        'pt-bulletin118-theme-close" title="Hide Bulletin 118 thematic card">' +
        '&times;</button></span>';
  }

  function scheduleSharedLayout() {
    if (
      window.BRIM &&
      window.BRIM.legendCloseout &&
      typeof window.BRIM.legendCloseout.scheduleLayout === 'function'
    ) {
      window.BRIM.legendCloseout.scheduleLayout(card);
    }
  }

  function hideCard() {
    if (!card) return;
    if (detachableState && detachableState.floating && detachableState.dock) {
      detachableState.dock();
    }
    if (card.style.display !== 'none') card.style.display = 'none';
    scheduleSharedLayout();
  }

  function showCard() {
    if (!card || !active || cardUserHidden) return;
    if (card.style.display !== 'block') card.style.display = 'block';
    scheduleSharedLayout();
  }

  function setStatus(message) {
    if (!statusNode) return;
    var text = message || '';
    var display = message ? 'block' : 'none';
    if (statusNode.textContent !== text) statusNode.textContent = text;
    if (statusNode.style.display !== display) {
      statusNode.style.display = display;
    }
  }

  function recordIsEligible(record, minimum) {
    if (minimum <= 0) return true;
    if (
      !record ||
      record.percent_blm === undefined ||
      record.percent_blm === null ||
      record.percent_blm === ''
    ) {
      return false;
    }
    var value = Number(record.percent_blm);
    return isFinite(value) && value >= minimum;
  }

  function countEligible(minimum) {
    var count = 0;
    records.forEach(function(record) {
      if (recordIsEligible(record, minimum)) count += 1;
    });
    return count;
  }

  function updateFilterUi() {
    if (minimumSlider && Number(minimumSlider.value) !== currentMinimumBlm) {
      minimumSlider.value = String(currentMinimumBlm);
    }
    if (minimumValueNode) {
      var minimumText = currentMinimumBlm + '%';
      if (minimumValueNode.textContent !== minimumText) {
        minimumValueNode.textContent = minimumText;
      }
    }
    if (visibleCountNode) {
      var countText =
        visibleBasinCount + ' of ' + records.length + ' basins shown';
      if (visibleCountNode.textContent !== countText) {
        visibleCountNode.textContent = countText;
      }
    }
  }

  function groupHasLayer(layer) {
    return !!(
      groupRoot &&
      typeof groupRoot.hasLayer === 'function' &&
      groupRoot.hasLayer(layer)
    );
  }

  function closeInteractionForLayer(layer) {
    if (!layer) return;
    if (
      currentTooltipLayer === layer ||
      (currentTooltip && currentTooltip._source === layer)
    ) {
      if (typeof layer.closeTooltip === 'function') {
        try { layer.closeTooltip(); } catch (tooltipError) {}
      } else if (map.closeTooltip) {
        try { map.closeTooltip(currentTooltip); } catch (mapTooltipError) {}
      }
      currentTooltip = null;
      currentTooltipLayer = null;
    }
    if (
      currentPopup &&
      currentPopup._source === layer &&
      map.closePopup
    ) {
      try { map.closePopup(currentPopup); } catch (popupError) {}
      currentPopup = null;
    }
  }

  function cancelFilterWork(reason) {
    filterGeneration += 1;
    if (scheduledFilterFrame !== null) {
      if (window.cancelAnimationFrame) {
        window.cancelAnimationFrame(scheduledFilterFrame);
      } else {
        window.clearTimeout(scheduledFilterFrame);
      }
      scheduledFilterFrame = null;
      diagnostics.canceledFilterJobs += 1;
      diagnostics.lastFilterCancelReason = reason || 'unspecified';
    }
  }

  function reconcileFilterNow(reason) {
    if (!active || !groupRoot) return;
    var operationCount = 0;
    var visibleCount = 0;

    layers.forEach(function(layer) {
      var record = recordById[layerId(layer)];
      var eligible = recordIsEligible(record, currentMinimumBlm);
      var present = groupHasLayer(layer);
      if (eligible) {
        visibleCount += 1;
        if (!present && typeof groupRoot.addLayer === 'function') {
          var style = styleFor(layer, currentTheme);
          layer.options.fillColor = style.fillColor;
          layer.options.fillOpacity = style.fillOpacity;
          groupRoot.addLayer(layer);
          operationCount += 1;
        }
      } else if (present && typeof groupRoot.removeLayer === 'function') {
        closeInteractionForLayer(layer);
        groupRoot.removeLayer(layer);
        operationCount += 1;
      }
    });

    visibleBasinCount = visibleCount;
    diagnostics.filterReconciliationCount += 1;
    diagnostics.filterOperationCount += operationCount;
    diagnostics.lastFilterReason = reason || 'filter reconciliation';
    updateFilterUi();
    setStatus('');
  }

  function requestFilterReconcile(reason) {
    if (!active) return;
    cancelFilterWork('superseded by ' + (reason || 'threshold change'));
    var generation = filterGeneration;
    diagnostics.filterJobCount =
      (diagnostics.filterJobCount || 0) + 1;
    setStatus('Filtering basins\u2026');
    var callback = function() {
      scheduledFilterFrame = null;
      if (destroyed || !active || generation !== filterGeneration) return;
      reconcileFilterNow(reason || 'threshold change');
    };
    scheduledFilterFrame = window.requestAnimationFrame ?
      window.requestAnimationFrame(callback) :
      window.setTimeout(callback, 0);
  }

  function setMinimumBlm(value, reason, immediate) {
    value = Math.max(0, Math.min(100, Math.round(Number(value) || 0)));
    if (value === currentMinimumBlm) {
      diagnostics.sameThresholdNoops += 1;
      return false;
    }
    currentMinimumBlm = value;
    visibleBasinCount = countEligible(currentMinimumBlm);
    updateFilterUi();
    if (active) {
      if (immediate) {
        cancelFilterWork('immediate ' + (reason || 'threshold change'));
        reconcileFilterNow(reason || 'threshold change');
      } else {
        requestFilterReconcile(reason || 'threshold change');
      }
    }
    return true;
  }

  function clearSearch(clearInput) {
    searchMatches = [];
    searchActiveIndex = -1;
    if (searchResults) {
      searchResults.innerHTML = '';
      searchResults.hidden = true;
    }
    if (clearInput && searchInput) searchInput.value = '';
  }

  function matchingSearchRows(query) {
    query = String(query || '').trim().toLowerCase();
    if (!query) return [];
    diagnostics.searchQueryCount += 1;
    return searchIndex.filter(function(row) {
      return row.haystack.indexOf(query) >= 0;
    }).slice(0, 8);
  }

  function setActiveSearchResult(index) {
    if (!searchResults || !searchMatches.length) {
      searchActiveIndex = -1;
      return;
    }
    var resultButtons = searchResults.querySelectorAll(
      '.pt-bulletin118-search-result'
    );
    searchActiveIndex =
      (index + searchMatches.length) % searchMatches.length;
    Array.prototype.forEach.call(resultButtons, function(button, i) {
      if (button.classList) {
        if (i === searchActiveIndex) button.classList.add('is-active');
        else button.classList.remove('is-active');
      }
      button.setAttribute(
        'aria-selected',
        i === searchActiveIndex ? 'true' : 'false'
      );
    });
    var activeButton = resultButtons[searchActiveIndex];
    if (activeButton && typeof activeButton.scrollIntoView === 'function') {
      activeButton.scrollIntoView({block: 'nearest'});
    }
  }

  function renderSearchResults(query) {
    if (!searchResults) return [];
    searchMatches = matchingSearchRows(query);
    searchActiveIndex = -1;
    searchResults.innerHTML = '';
    if (!String(query || '').trim()) {
      searchResults.hidden = true;
      return searchMatches;
    }
    searchResults.hidden = false;
    if (!searchMatches.length) {
      var empty = document.createElement('div');
      empty.className = 'pt-bulletin118-search-empty';
      empty.textContent = 'No matching basin.';
      searchResults.appendChild(empty);
      return searchMatches;
    }
    searchMatches.forEach(function(row) {
      var button = document.createElement('button');
      button.type = 'button';
      button.className = 'pt-bulletin118-search-result';
      button.setAttribute('role', 'option');
      button.setAttribute('aria-selected', 'false');
      button.setAttribute('data-layer-id', row.layer_id);
      button.innerHTML =
        '<span class="pt-bulletin118-search-result-label">' +
          escapeHtml(row.label) +
        '</span><span class="pt-bulletin118-search-result-code">' +
          escapeHtml(row.code) +
        '</span>';
      searchResults.appendChild(button);
    });
    return searchMatches;
  }

  function selectSearchResult(layerIdValue) {
    var id = String(layerIdValue || '');
    var layer = layerById[id];
    var record = recordById[id];
    if (!layer || !record || !active) return false;

    if (!recordIsEligible(record, currentMinimumBlm)) {
      setMinimumBlm(0, 'search result reveal', true);
    } else if (!groupHasLayer(layer) && groupRoot) {
      var style = styleFor(layer, currentTheme);
      layer.options.fillColor = style.fillColor;
      layer.options.fillOpacity = style.fillOpacity;
      groupRoot.addLayer(layer);
      visibleBasinCount = countEligible(currentMinimumBlm);
      updateFilterUi();
    }

    clearSearch(false);
    if (searchInput) searchInput.value = String(
      record.display_label ||
      record.subbasin_name ||
      record.basin_name ||
      record.subbasin_num ||
      ''
    );
    if (typeof layer.getBounds === 'function' && map.fitBounds) {
      var bounds = layer.getBounds();
      if (bounds && (!bounds.isValid || bounds.isValid())) {
        map.fitBounds(bounds, {
          padding: [24, 24],
          maxZoom: 11
        });
      }
    }
    if (typeof layer.openPopup === 'function') {
      try { layer.openPopup(); } catch (popupOpenError) {}
    }
    diagnostics.searchSelectionCount += 1;
    return true;
  }

  function renderLegend() {
    if (!legendBody) return;
    var legend = bulletinData.themes ?
      bulletinData.themes[currentTheme] :
      null;
    if (!legend) {
      legendBody.innerHTML =
        '<div class="pt-bulletin118-theme-note">Legend unavailable.</div>';
      return;
    }

    var html =
      '<div class="pt-bulletin118-theme-legend-title">' +
      escapeHtml(legend.title || '') +
      '</div>';
    (legend.rows || []).forEach(function(row) {
      html +=
        '<div class="pt-bulletin118-theme-row">' +
          '<span class="pt-bulletin118-theme-swatch" style="background:' +
            escapeHtml(row.color || '#9E9E9E') + ';"></span>' +
          '<span class="pt-bulletin118-theme-row-label">' +
            escapeHtml(row.label || '') + '</span>' +
          '<span class="pt-bulletin118-theme-row-count">' +
            escapeHtml(row.count === undefined ? '' : row.count) + '</span>' +
        '</div>';
    });
    if (legend.note) {
      html +=
        '<div class="pt-bulletin118-theme-note">' +
        escapeHtml(legend.note) +
        (
          currentTheme === 'sgma_2019' && bulletinData.source_url ?
            ' <a href="' + escapeHtml(bulletinData.source_url) +
            '" target="_blank" rel="noopener">DWR source</a>' :
            ''
        ) +
        '</div>';
    }
    legendBody.innerHTML = html;
    diagnostics.legendUpdateCount += 1;
    scheduleSharedLayout();
  }

  function createCard() {
    addCardCss();
    cardControl = L.control({position: 'bottomleft'});
    cardControl.onAdd = function() {
      var div = L.DomUtil.create(
        'div',
        'leaflet-control pt-map-legend-card pt-map-legend-local ' +
        'pt-bulletin118-theme-card'
      );
      div.innerHTML =
        '<div class="pt-bulletin118-theme-head pt-map-card-handle">' +
          '<span class="pt-bulletin118-theme-title">' +
            'Bulletin 118 Groundwater Basins</span>' +
          actionsHtml() +
        '</div>' +
        '<div class="pt-bulletin118-theme-display">' +
          '<label for="pt-bulletin118-theme-select">Display</label>' +
          '<select id="pt-bulletin118-theme-select" ' +
            'class="pt-bulletin118-theme-select">' +
            '<option value="basins">Basins only</option>' +
            '<option value="sgma_2019">' +
              'DWR SGMA 2019 Basin Prioritization</option>' +
            '<option value="blm_pct">BLM-managed land \u2014 %</option>' +
          '</select>' +
        '</div>' +
        '<div class="pt-bulletin118-filter">' +
          '<div class="pt-bulletin118-filter-head">' +
            '<label for="pt-bulletin118-minimum">' +
              'Minimum BLM-managed land</label>' +
            '<span class="pt-bulletin118-filter-value">0%</span>' +
          '</div>' +
          '<input id="pt-bulletin118-minimum" ' +
            'class="pt-bulletin118-minimum" type="range" ' +
            'min="0" max="100" step="1" value="0">' +
          '<div class="pt-bulletin118-visible-count">' +
            records.length + ' of ' + records.length + ' basins shown</div>' +
        '</div>' +
        '<div class="pt-bulletin118-search">' +
          '<div class="pt-bulletin118-search-row">' +
            '<input class="pt-bulletin118-search-input" type="search" ' +
              'autocomplete="off" placeholder="Find basin or subbasin\u2026" ' +
              'aria-label="Find Bulletin 118 basin or subbasin" ' +
              'aria-controls="pt-bulletin118-search-results">' +
            '<button type="button" class="pt-bulletin118-search-clear" ' +
              'aria-label="Clear basin search" title="Clear basin search">' +
              '&times;</button>' +
          '</div>' +
          '<div id="pt-bulletin118-search-results" ' +
            'class="pt-bulletin118-search-results" role="listbox" hidden>' +
          '</div>' +
        '</div>' +
        '<div class="pt-bulletin118-theme-legend"></div>' +
        '<div class="pt-bulletin118-theme-status" aria-live="polite"></div>';
      L.DomEvent.disableClickPropagation(div);
      L.DomEvent.disableScrollPropagation(div);
      return div;
    };
    cardControl.addTo(map);
    card = map.getContainer().querySelector(
      '.pt-bulletin118-theme-card'
    );
    if (!card) return;

    select = card.querySelector('.pt-bulletin118-theme-select');
    legendBody = card.querySelector('.pt-bulletin118-theme-legend');
    statusNode = card.querySelector('.pt-bulletin118-theme-status');
    minimumSlider = card.querySelector('.pt-bulletin118-minimum');
    minimumValueNode = card.querySelector('.pt-bulletin118-filter-value');
    visibleCountNode = card.querySelector('.pt-bulletin118-visible-count');
    searchInput = card.querySelector('.pt-bulletin118-search-input');
    searchClear = card.querySelector('.pt-bulletin118-search-clear');
    searchResults = card.querySelector('.pt-bulletin118-search-results');
    if (select) {
      select.value = currentTheme;
      listenDom(select, 'change', function(event) {
        setTheme(event.target.value, 'dropdown change');
      });
    }
    if (minimumSlider) {
      minimumSlider.value = String(currentMinimumBlm);
      listenDom(minimumSlider, 'input', function(event) {
        setMinimumBlm(event.target.value, 'slider input', false);
      });
    }
    if (searchInput) {
      listenDom(searchInput, 'input', function(event) {
        renderSearchResults(event.target.value);
      });
      listenDom(searchInput, 'keydown', function(event) {
        var key = event && event.key ? event.key : '';
        if (key === 'ArrowDown' && searchMatches.length) {
          if (event.preventDefault) event.preventDefault();
          setActiveSearchResult(searchActiveIndex + 1);
        } else if (key === 'ArrowUp' && searchMatches.length) {
          if (event.preventDefault) event.preventDefault();
          setActiveSearchResult(
            searchActiveIndex < 0 ?
              searchMatches.length - 1 :
              searchActiveIndex - 1
          );
        } else if (key === 'Enter' && searchMatches.length) {
          if (event.preventDefault) event.preventDefault();
          var index = searchActiveIndex < 0 ? 0 : searchActiveIndex;
          selectSearchResult(searchMatches[index].layer_id);
        } else if (key === 'Escape') {
          if (event.preventDefault) event.preventDefault();
          clearSearch(false);
        }
      });
    }
    if (searchClear) {
      listenDom(searchClear, 'click', function(event) {
        if (event.preventDefault) event.preventDefault();
        if (event.stopPropagation) event.stopPropagation();
        clearSearch(true);
        if (searchInput && searchInput.focus) searchInput.focus();
      });
    }
    if (searchResults) {
      listenDom(searchResults, 'click', function(event) {
        var button = event && event.target && event.target.closest ?
          event.target.closest('.pt-bulletin118-search-result') :
          null;
        if (!button || !searchResults.contains(button)) return;
        if (event.preventDefault) event.preventDefault();
        if (event.stopPropagation) event.stopPropagation();
        selectSearchResult(button.getAttribute('data-layer-id'));
      });
    }

    var close = card.querySelector('.pt-bulletin118-theme-close');
    if (close) {
      listenDom(close, 'click', function(event) {
        event.preventDefault();
        event.stopPropagation();
        cardUserHidden = true;
        hideCard();
      });
    }

    if (
      window.BRIM &&
      window.BRIM.legendCloseout &&
      typeof window.BRIM.legendCloseout.makeDetachable === 'function'
    ) {
      detachableState = window.BRIM.legendCloseout.makeDetachable({
        card: card,
        map: map,
        handleSelector: '.pt-bulletin118-theme-head',
        dockSelector: '.pt-bulletin118-theme-dock',
        label: 'Bulletin 118 thematic card'
      });
    }

    renderLegend();
    updateFilterUi();
    if (active) showCard();
    else hideCard();
  }

  function styleFor(layer, theme) {
    var record = recordById[layerId(layer)] || {};
    var styles = bulletinData.styles || {};
    if (theme === 'sgma_2019') {
      return {
        fillColor: record.sgma_color || '#9E9E9E',
        fillOpacity: styles.sgma_2019 ?
          Number(styles.sgma_2019.fill_opacity) :
          0.48
      };
    }
    if (theme === 'blm_pct') {
      return {
        fillColor: record.blm_color || '#9E9E9E',
        fillOpacity: styles.blm_pct ?
          Number(styles.blm_pct.fill_opacity) :
          0.58
      };
    }
    return {
      fillColor: styles.basins && styles.basins.fill_color ?
        styles.basins.fill_color :
        '#8B5A2B',
      fillOpacity: styles.basins ?
        Number(styles.basins.fill_opacity) :
        0.20
    };
  }

  function cancelStyleWork(reason) {
    styleGeneration += 1;
    if (scheduledStyleFrame !== null) {
      if (window.cancelAnimationFrame) {
        window.cancelAnimationFrame(scheduledStyleFrame);
      } else {
        window.clearTimeout(scheduledStyleFrame);
      }
      scheduledStyleFrame = null;
      diagnostics.canceledStyleJobs += 1;
      diagnostics.lastCancelReason = reason || 'unspecified';
    }
  }

  function primeOptions(theme) {
    layers.forEach(function(layer) {
      var style = styleFor(layer, theme);
      layer.options.fillColor = style.fillColor;
      layer.options.fillOpacity = style.fillOpacity;
    });
    diagnostics.optionPrimeCount += layers.length;
  }

  function requestStyleApply(reason) {
    if (!active) return;
    cancelStyleWork('superseded by ' + (reason || 'theme change'));
    var generation = styleGeneration;
    var targetTheme = currentTheme;
    diagnostics.styleJobCount += 1;
    setStatus('Applying display\u2026');

    var callback = function() {
      scheduledStyleFrame = null;
      if (destroyed || !active || generation !== styleGeneration) return;
      var started = nowMs();
      layers.forEach(function(layer) {
        var style = styleFor(layer, targetTheme);
        layer.options.fillColor = style.fillColor;
        layer.options.fillOpacity = style.fillOpacity;
        if (groupHasLayer(layer)) {
          layer.setStyle(style);
          diagnostics.styleOperationCount += 1;
        }
      });
      appliedTheme = targetTheme;
      diagnostics.lastApplyMs = nowMs() - started;
      diagnostics.lastApplyReason = reason || 'theme change';
      renderLegend();
      setStatus('');
    };

    scheduledStyleFrame = window.requestAnimationFrame ?
      window.requestAnimationFrame(callback) :
      window.setTimeout(callback, 0);
  }

  function setTheme(nextTheme, reason) {
    nextTheme = String(nextTheme || '');
    if (!allowedThemes[nextTheme]) return false;
    if (nextTheme === currentTheme) {
      diagnostics.sameThemeNoops += 1;
      return false;
    }

    currentTheme = nextTheme;
    if (select && select.value !== nextTheme) select.value = nextTheme;
    if (active) requestStyleApply(reason || 'theme change');
    return true;
  }

  function isBulletinLayer(layer) {
    var id = layerId(layer);
    return id !== null && !!recordById[id];
  }

  function closeTransientInteraction() {
    var tooltip = currentTooltip;
    var tooltipLayer = currentTooltipLayer;
    currentTooltip = null;
    currentTooltipLayer = null;
    if (tooltipLayer && typeof tooltipLayer.closeTooltip === 'function') {
      try { tooltipLayer.closeTooltip(); } catch (tooltipError) {}
    } else if (tooltip && map.closeTooltip) {
      try { map.closeTooltip(tooltip); } catch (mapTooltipError) {}
    }
    if (currentPopup && map.closePopup) {
      try { map.closePopup(currentPopup); } catch (popupError) {}
    }
    currentPopup = null;
  }

  function eventIsBulletin(event) {
    if (event && event.name && String(event.name) === groupName) return true;
    return !!(event && event.layer && event.layer === groupRoot);
  }

  listen(map, 'tooltipopen', function(event) {
    var tooltip = event && event.tooltip;
    var source = tooltip && tooltip._source;
    if (!isBulletinLayer(source)) return;
    if (currentTooltip && currentTooltip !== tooltip) {
      closeTransientInteraction();
    }
    currentTooltip = tooltip;
    currentTooltipLayer = source;
  });
  listen(map, 'tooltipclose', function(event) {
    if (event && event.tooltip === currentTooltip) {
      currentTooltip = null;
      currentTooltipLayer = null;
    }
  });
  listen(map, 'popupopen', function(event) {
    var popup = event && event.popup;
    if (!popup || !isBulletinLayer(popup._source)) return;
    if (currentTooltipLayer && currentTooltipLayer.closeTooltip) {
      try { currentTooltipLayer.closeTooltip(); } catch (tooltipError) {}
    }
    currentTooltip = null;
    currentTooltipLayer = null;
    currentPopup = popup;
  });
  listen(map, 'popupclose', function(event) {
    if (!event || event.popup === currentPopup) currentPopup = null;
  });
  listen(map, 'movestart zoomstart', function() {
    if (currentTooltip || currentTooltipLayer) closeTransientInteraction();
  });
  listen(map, 'pt:measureinteractionchange', function(event) {
    if (event && event.active) closeTransientInteraction();
  });

  listen(map, 'overlayadd', function(event) {
    if (!eventIsBulletin(event)) return;
    active = true;
    diagnostics.activationCount += 1;
    cardUserHidden = false;
    visibleBasinCount = countEligible(currentMinimumBlm);
    updateFilterUi();
    primeOptions(currentTheme);
    requestFilterReconcile('Bulletin 118 activation');
    requestStyleApply('Bulletin 118 activation');
    showCard();
  });

  listen(map, 'overlayremove', function(event) {
    if (!eventIsBulletin(event)) return;
    cancelStyleWork('Bulletin 118 deactivation');
    cancelFilterWork('Bulletin 118 deactivation');
    active = false;
    diagnostics.deactivationCount += 1;
    closeTransientInteraction();
    clearSearch(true);
    hideCard();
    if (
      renderer &&
      map.hasLayer &&
      map.hasLayer(renderer) &&
      map.removeLayer
    ) {
      map.removeLayer(renderer);
    }
  });

  function stats() {
    return {
      active: active,
      currentTheme: currentTheme,
      appliedTheme: appliedTheme,
      expectedCount: Number(bulletinData.expected_count) || 0,
      registeredCount: layers.length,
      retainedRecordCount: Object.keys(recordById).length,
      currentMinimumBlm: currentMinimumBlm,
      visibleBasinCount: visibleBasinCount,
      searchResultCount: searchMatches.length,
      cardVisible: !!(
        card &&
        card.style.display !== 'none' &&
        !cardUserHidden
      ),
      cardUserHidden: cardUserHidden,
      stylePending: scheduledStyleFrame !== null,
      filterPending: scheduledFilterFrame !== null,
      diagnostics: {
        mapScanCount: diagnostics.mapScanCount,
        styleJobCount: diagnostics.styleJobCount,
        styleOperationCount: diagnostics.styleOperationCount,
        optionPrimeCount: diagnostics.optionPrimeCount,
        canceledStyleJobs: diagnostics.canceledStyleJobs,
        sameThemeNoops: diagnostics.sameThemeNoops,
        activationCount: diagnostics.activationCount,
        deactivationCount: diagnostics.deactivationCount,
        legendUpdateCount: diagnostics.legendUpdateCount,
        filterJobCount: diagnostics.filterJobCount || 0,
        filterReconciliationCount: diagnostics.filterReconciliationCount,
        filterOperationCount: diagnostics.filterOperationCount,
        canceledFilterJobs: diagnostics.canceledFilterJobs,
        sameThresholdNoops: diagnostics.sameThresholdNoops,
        searchQueryCount: diagnostics.searchQueryCount,
        searchSelectionCount: diagnostics.searchSelectionCount,
        lastApplyMs: diagnostics.lastApplyMs,
        lastApplyReason: diagnostics.lastApplyReason,
        lastFilterReason: diagnostics.lastFilterReason,
        listenerCount: listenerRecords.length,
        domListenerCount: domListenerRecords.length,
        destroyed: destroyed
      }
    };
  }

  function destroy() {
    if (destroyed) return;
    destroyed = true;
    cancelStyleWork('controller destruction');
    cancelFilterWork('controller destruction');
    closeTransientInteraction();
    clearSearch(true);
    listenerRecords.forEach(function(record) {
      try {
        record.target.off(
          record.eventNames,
          record.handler
        );
      } catch (listenerError) {}
    });
    listenerRecords = [];
    domListenerRecords.forEach(function(record) {
      try {
        record.target.removeEventListener(
          record.eventName,
          record.handler,
          false
        );
      } catch (domListenerError) {}
    });
    domListenerRecords = [];

    if (detachableState && typeof detachableState.destroy === 'function') {
      detachableState.destroy(true, true);
      detachableState = null;
    } else if (card && card.parentNode) {
      card.parentNode.removeChild(card);
    }
    if (cardControl && typeof cardControl.remove === 'function') {
      try { cardControl.remove(); } catch (controlError) {}
    }
    card = null;
  }

  createCard();
  if (active) {
    visibleBasinCount = countEligible(currentMinimumBlm);
    updateFilterUi();
    primeOptions(currentTheme);
    requestFilterReconcile('active controller installation');
    requestStyleApply('active controller installation');
    showCard();
  }

  listen(map, 'unload', destroy);
  window.BRIM_BULLETIN118_LOCAL = {
    setTheme: function(theme) {
      return setTheme(theme, 'console');
    },
    setMinimumBlm: function(value) {
      return setMinimumBlm(value, 'console', false);
    },
    search: function(query) {
      return renderSearchResults(query);
    },
    selectBasin: selectSearchResult,
    clearSearch: function() {
      clearSearch(true);
    },
    stats: stats,
    destroy: destroy
  };

  console.log(
    'BRIM Bulletin 118 controller loaded:',
    layers.length,
    'direct polygon references'
  );
}
