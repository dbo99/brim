function(el, x, toolsData) {

  var map = this;

  // --------------------------------------------------------------------------
  // Configuration
  // --------------------------------------------------------------------------

  toolsData = toolsData || {};
  var PT2_ENABLE_BLM_SMA = !!toolsData.enable_blm_sma;
  var PT2_CATALOG = Array.isArray(toolsData.catalog) ? toolsData.catalog : [];
  var PT2_CAPABILITY_DEFINITIONS = toolsData.capability_definitions || {};
  var PT2_CAPABILITY_COVERAGE = Array.isArray(toolsData.capability_coverage) ?
    toolsData.capability_coverage : [];
  var PT2_LAYER_EXPLORER = toolsData.layer_explorer || {};

  // --------------------------------------------------------------------------
  // Layer Explorer read-only model
  // --------------------------------------------------------------------------

  function ptCreateLayerExplorerReadOnlyModel(payload) {
    payload = payload || {};

    var sourceRecords = Array.isArray(payload.records) ? payload.records : [];
    var records = sourceRecords.map(function(record) {
      record = record || {};
      return Object.freeze({
        stableId: String(record.stableId || ''),
        displayName: String(record.displayName || ''),
        architecture: String(record.architecture || 'UNKNOWN'),
        catalogPath: String(record.catalogPath || 'UNKNOWN'),
        implementation: String(record.implementation || 'UNKNOWN'),
        renderer: String(record.renderer || 'UNKNOWN'),
        domainReview: String(record.domainReview || 'UNKNOWN'),
        confidence: String(record.confidence || 'UNKNOWN')
      });
    });

    var architectures = [];
    records.forEach(function(record) {
      if (architectures.indexOf(record.architecture) === -1) {
        architectures.push(record.architecture);
      }
    });

    function filterRecords(query, architecture) {
      var needle = String(query || '').trim().toLowerCase();
      var family = String(architecture || 'ALL');

      return records.filter(function(record) {
        var architectureMatch = family === 'ALL' || record.architecture === family;
        var searchText = (record.displayName + ' ' + record.stableId).toLowerCase();
        return architectureMatch && (!needle || searchText.indexOf(needle) !== -1);
      });
    }

    return Object.freeze({
      coverage: String(payload.coverage || 'UNKNOWN'),
      catalogAuthority: String(payload.catalogAuthority || 'UNKNOWN'),
      uiConsumption: String(payload.uiConsumption || 'UNKNOWN'),
      runtimeControl: String(payload.runtimeControl || 'UNKNOWN'),
      runtimeAuthority: String(payload.runtimeAuthority || 'UNKNOWN'),
      records: Object.freeze(records.slice()),
      architectures: Object.freeze(architectures.slice()),
      filterRecords: filterRecords
    });
  }

  var ptLayerExplorerModel = ptCreateLayerExplorerReadOnlyModel(PT2_LAYER_EXPLORER);

  // Keep a stable browser-side index for quick-add catalog rows.
  PT2_CATALOG.forEach(function(rec, idx) {
    if (rec) rec.__pt2Index = idx;
  });

  var PT2_ESRI_LEAFLET_URL =
    'https://unpkg.com/esri-leaflet@3.0.15/dist/esri-leaflet.js';

  var PT2_BLM_SMA_URL =
    'https://gis.blm.gov/caarcgis/rest/services/lands/BLM_CA_LandStatus_SurfaceManagementAgency/MapServer';

  var PT2_MAX_CUSTOM_LAYERS = 3;

  var PT2_CUSTOM_COLORS = [
    '#7B3294',
    '#008837',
    '#D95F02'
  ];

  // Quick-add catalog loading state.  This is intentionally UI-only: it helps
  // users understand that browser-side service queries can take several
  // seconds, especially current-view FeatureServer requests.
  var ptCatalogLoadingIdx = null;
  var ptCatalogLoadingStartedAt = 0;
  var ptCatalogClearTimer = null;
  var PT2_CATALOG_MIN_SPINNER_MS = 650;
  var ptPendingCatalogIdx = null;
  var ptCatalogLoadingToken = null;
  var ptCatalogActiveLoadTokens = {};
  var ptCatalogCancelledLoadTokens = {};

  // Manual advanced-URL add loading state. The manual form can be long and
  // scrollable, so status must be visible near the clicked Add button as well
  // as in the shared External Layers status area.
  var ptManualAddInProgress = false;
  var ptManualAddStartedAt = 0;
  var ptManualAddClearTimer = null;
  var PT2_MANUAL_ADD_MIN_SPINNER_MS = 650;

  // External catalog group expansion state.  Groups are collapsed by default
  // to keep the quick-add browser compact, but active groups and search
  // matches are expanded automatically for visibility.
  var ptCatalogExpandedGroups = {};
  // Theme/subgroup expansion is separate from parent-group expansion.
  // Default is collapsed to keep open parent groups compact. Search matches
  // and active layers are still expanded automatically for visibility.
  var ptCatalogExpandedSubgroups = {};
  var ptCatalogVisibleGroupNames = [];

  // Compact/collapsible top summary for active External Layers. The catalog
  // rows now include their own Remove/Refresh buttons, so the top active-list
  // should preserve useful technical details without taking over the panel.
  var ptActiveExternalExpanded = false;

  // Baseline map attribution before user-added external overlays. Some public
  // ArcGIS services add long attribution text that can persist in the Leaflet
  // attribution control after the overlay is removed. Store a clean baseline
  // so Clear external can restore the footer to the normal PT2/base-layer text.
  var ptExternalBaseAttributionHtml = null;

  function ptCaptureExternalBaseAttribution() {
    var div = document.querySelector('.leaflet-control-attribution');
    if (div && ptExternalBaseAttributionHtml === null) {
      ptExternalBaseAttributionHtml = div.innerHTML;
    }
  }

  function ptRestoreExternalBaseAttribution() {
    var div = document.querySelector('.leaflet-control-attribution');
    if (div && ptExternalBaseAttributionHtml !== null) {
      div.innerHTML = ptExternalBaseAttributionHtml;
    }
  }

  // --------------------------------------------------------------------------
  // Utility helpers
  // --------------------------------------------------------------------------

  function ptSetStatus(msg, isError) {
    var div = document.getElementById('pt-tools-status');
    if (!div) return;

    div.textContent = msg || '';
    div.style.display = msg ? 'block' : 'none';
    div.classList.toggle('pt-tools-status-error', !!isError);
    div.classList.toggle('pt-tools-status-ok', !isError && !!msg);
    div.style.color = isError ? '#8B0000' : '#244C1E';

    // If a quick-add row is showing a loading spinner, mirror final status
    // messages into that row before clearing the spinner. This keeps min-zoom,
    // error, and success messages near the layer the user clicked.
    if (ptCatalogLoadingIdx !== null && !ptStatusKeepsCatalogSpinner(msg)) {
      if (msg) {
        ptSetCatalogRowNote(ptCatalogLoadingIdx, msg, isError);
      }
      ptClearCatalogLoading();
    }

    // When a manual add is in progress, repeat the shared status message
    // immediately below the manual Add button and clear the working state once
    // a terminal success/error message is reached.
    ptMirrorManualStatusAndMaybeFinish(msg, isError);
  }

  function ptNotifyOpsPromotedStatus(options, msg, isError) {
    options = options || {};

    if (!ptIsOpsPromotedRecord(options)) return;

    var opsName = ptCleanText(options.opsPromotedDisplayName || options.layerName || options.name);
    var opsKey = ptCleanText(options.opsPromotedKey);

    if (!opsName && !opsKey) return;

    // Reuse the catalog spinner heuristic: querying/loading/requested messages
    // are non-terminal; added/failed/no-feature messages should end the Ops row
    // loading indicator.
    var terminal = !ptStatusKeepsCatalogSpinner(msg);

    try {
      window.dispatchEvent(new CustomEvent('ptOpsCatalogLayerStatus', {
        detail: {
          opsDisplayName: opsName,
          opsKey: opsKey,
          msg: msg || '',
          isError: !!isError,
          terminal: terminal
        }
      }));
    } catch(e) {}
  }


  function ptUpdateOpsPromotedStreamflowMetric(options, featureCount, suffix) {
    options = options || {};
    if (!ptIsOpsPromotedRecord(options) || !ptIsStreamGaugeFlowStyle(options)) return;
    if (typeof ptOpsSetLegendMetric !== 'function') return;

    var opsName = ptCleanText(options.opsPromotedDisplayName || options.layerName || options.name);
    if (!opsName) return;

    var n = Number(featureCount || 0);
    var txt = 'Loaded current-view snapshot: ' + n.toLocaleString() + ' record' + (n === 1 ? '' : 's') + '.';
    if (suffix) txt += ' ' + suffix;
    txt += ' Refresh after panning/zooming.';
    ptOpsSetLegendMetric(opsName, txt);
  }

  function ptSetExternalStatus(msg, isError, options) {
    // Ops Live-promoted catalog layers reuse the External Layers loader and
    // styling logic, but they should not update the External Layers status box.
    // Their user-facing status is reported in the Ops panel instead.
    if (options && ptIsOpsPromotedRecord(options)) {
      ptNotifyOpsPromotedStatus(options, msg, isError);
      return;
    }
    ptSetStatus(msg, isError);
  }

  function ptStatusKeepsCatalogSpinner(msg) {
    var text = String(msg || '').toLowerCase();

    return (
      text.indexOf('loading') >= 0 ||
      text.indexOf('querying') >= 0 ||
      text.indexOf('adding') >= 0 ||
      text.indexOf('auto-detected') >= 0 ||
      text.indexOf('auto-selected') >= 0 ||
      text.indexOf('requested') >= 0 ||
      text.indexOf('waiting') >= 0 ||
      text.indexOf('checking') >= 0 ||
      text.indexOf('resolving') >= 0 ||
      text.indexOf('metadata') >= 0 ||
      text.indexOf('this may take') >= 0 ||
      text.indexOf('drawing') >= 0 ||
      text.indexOf('rendering') >= 0 ||
      text.indexOf('painting') >= 0 ||
      text.indexOf('image response') >= 0
    );
  }

  function ptSetCatalogRowNote(idx, msg, isError) {
    var note = document.getElementById('pt-catalog-row-note-' + Number(idx));
    if (!note) return;

    note.textContent = msg || '';
    note.style.display = msg ? 'block' : 'none';
    note.style.color = isError ? '#8B0000' : '#244C1E';
  }

  function ptClearCatalogRowNotes() {
    var notes = document.querySelectorAll('.pt-catalog-row-note');
    Array.prototype.forEach.call(notes, function(note) {
      note.textContent = '';
      note.style.display = 'none';
    });
  }

  function ptNewCatalogLoadToken(idx) {
    return String(idx) + ':' + String(Date.now()) + ':' + String(Math.random()).slice(2);
  }

  function ptMarkCatalogLoadingCancelled(idx) {
    idx = Number(idx);
    if (isNaN(idx)) return;

    var token = ptCatalogActiveLoadTokens[idx] || '';
    if (token) {
      ptCatalogCancelledLoadTokens[idx] = token;
    }
  }

  function ptCatalogLoadWasCancelled(idx, token) {
    idx = Number(idx);
    token = ptCleanText(token);
    if (isNaN(idx) || !token) return false;

    return ptCatalogCancelledLoadTokens[idx] === token;
  }

  function ptCatalogRecordIsRefreshableCurrentView(rec) {
    if (!rec) return false;

    var mode = ptCatalogField(rec, 'default_load_mode').toLowerCase();
    var src = ptCatalogField(rec, 'service_type').toLowerCase();

    return (mode === 'current_view' &&
      (src.indexOf('feature') >= 0 || src.indexOf('map') >= 0)) ||
      (mode === 'visual' && (src.indexOf('image') >= 0 || src.indexOf('map') >= 0)) ||
      (mode === 'live' && src.indexOf('geojson') >= 0) ||
      (mode === 'live_snapshot' && src.indexOf('feature') >= 0);
  }

  function ptSetCatalogLoading(idx, msg) {
    idx = Number(idx);

    if (ptCatalogLoadingIdx !== null && Number(ptCatalogLoadingIdx) !== idx) {
      var previousIdx = Number(ptCatalogLoadingIdx);
      ptMarkCatalogLoadingCancelled(ptCatalogLoadingIdx);
      if (PT2_CATALOG[previousIdx] && window.BRIM && window.BRIM.uicExplorer) {
        var previousUicId = ptCatalogField(
          PT2_CATALOG[previousIdx],
          'external_layer_id'
        );
        if (window.BRIM.uicExplorer.isUicCatalogId(previousUicId)) {
          window.BRIM.uicExplorer.removeExternal(previousUicId);
        }
      }
    }

    // Clear any prior loading row without an intermediate render; this new
    // loading row is rendered immediately below.
    ptClearCatalogLoading(true, true);
    ptCatalogLoadingIdx = idx;
    ptCatalogLoadingStartedAt = Date.now();
    ptCatalogLoadingToken = ptNewCatalogLoadToken(idx);
    ptCatalogActiveLoadTokens[idx] = ptCatalogLoadingToken;

    // Re-render immediately so row-level Cancel/Refresh controls exist
    // as soon as the user clicks the plus button, not only after a later
    // status/render event. This is especially important for slow public
    // FeatureServer/MapServer requests.
    ptRenderQuickCatalog();

    if (ptCatalogClearTimer) {
      window.clearTimeout(ptCatalogClearTimer);
      ptCatalogClearTimer = null;
    }

    var row = document.querySelector('[data-pt-catalog-row="' + idx + '"]');
    if (row) {
      row.classList.add('pt-catalog-loading');
      row.setAttribute('aria-busy', 'true');
    }

    var buttons = document.querySelectorAll('[data-pt-catalog-quick-add="' + idx + '"]');
    Array.prototype.forEach.call(buttons, function(btn) {
      btn.disabled = true;
      btn.setAttribute('aria-disabled', 'true');
      btn.title = 'Loading overlay...';
    });

    var message = msg || 'Adding overlay... public GIS layers may take several seconds to a minute, depending on extent and provider response.';
    ptSetCatalogRowNote(idx, message, false);
    ptSetInlineNote('pt-catalog-action-note', message, false);
    ptSetStatus(message, false);
    if (PT2_CATALOG[idx] && window.BRIM && window.BRIM.uicExplorer) {
      var loadingUicId = ptCatalogField(PT2_CATALOG[idx], 'external_layer_id');
      if (window.BRIM.uicExplorer.isUicCatalogId(loadingUicId)) {
        window.BRIM.uicExplorer.setExternalLoading(loadingUicId, true, message);
      }
    }

    window.setTimeout(function() {
      if (ptCatalogLoadingIdx === idx) {
        ptSetCatalogRowNote(
          idx,
          'Still loading. Large public GIS layers can take 10–60 seconds, especially broad current-view snapshots or provider-rendered image overlays.',
          false
        );
        ptSetStatus(
          'Still loading external overlay. Large public GIS layers can take 10–60 seconds.',
          false
        );
      }
    }, 12000);

    window.setTimeout(function() {
      if (ptCatalogLoadingIdx === idx) {
        ptSetCatalogRowNote(
          idx,
          'Still waiting after about 45 seconds. Public GIS services can be slow or intermittent; the overlay may still draw, or you can Cancel/Clear and retry.',
          false
        );
      }
    }, 45000);

    window.setTimeout(function() {
      if (ptCatalogLoadingIdx === idx) {
        ptSetCatalogRowNote(
          idx,
          'Request is still slow or blank. The layer may already be active with no visible features in this view, or the public service may be throttled. Clear/retry or check the source page.',
          false
        );
        ptClearCatalogLoading();
      }
    }, 90000);
  }

  function ptClearCatalogLoading(force, skipRender) {
    if (ptCatalogLoadingIdx === null) return;

    // Fast validation failures and fast visual-overlay requests can complete
    // before the browser paints the quick-add button.  Keep the row-level
    // spinner visible briefly so users get clear feedback that their click was
    // accepted.  Force is used for explicit resets such as Clear external.
    if (!force) {
      var elapsed = Date.now() - ptCatalogLoadingStartedAt;
      if (elapsed >= 0 && elapsed < PT2_CATALOG_MIN_SPINNER_MS) {
        if (ptCatalogClearTimer) {
          window.clearTimeout(ptCatalogClearTimer);
        }
        ptCatalogClearTimer = window.setTimeout(function() {
          ptClearCatalogLoading(true);
        }, PT2_CATALOG_MIN_SPINNER_MS - elapsed);
        return;
      }
    }

    if (ptCatalogClearTimer) {
      window.clearTimeout(ptCatalogClearTimer);
      ptCatalogClearTimer = null;
    }

    var idx = ptCatalogLoadingIdx;
    var row = document.querySelector('[data-pt-catalog-row="' + idx + '"]');
    if (row) {
      row.classList.remove('pt-catalog-loading');
      row.removeAttribute('aria-busy');
    }

    var buttons = document.querySelectorAll('[data-pt-catalog-quick-add="' + idx + '"]');
    Array.prototype.forEach.call(buttons, function(btn) {
      btn.disabled = false;
      btn.removeAttribute('aria-disabled');
      btn.title = 'Add this overlay';
    });

    ptCatalogLoadingIdx = null;
    ptCatalogLoadingStartedAt = 0;
    ptCatalogLoadingToken = null;

    // The row action buttons are generated by ptRenderQuickCatalog(), not by
    // simple class toggles.  Re-render after a terminal load/error/cancel state
    // so current-view FeatureServer rows consistently transition from:
    //   loading: Cancel + Refresh current view
    // to:
    //   active:  Remove + Refresh current view
    // This avoids the confusing mixed Cancel/Remove state and fixes rows that
    // did not show Remove until some later unrelated panel interaction.
    if (!skipRender) {
      ptRenderQuickCatalog();
    }
  }

  function ptCancelCatalogLoading(idx, msg) {
    idx = Number(idx);
    if (ptCatalogLoadingIdx !== idx) return;

    ptMarkCatalogLoadingCancelled(idx);

    // A browser request may still finish later, but this gives the user a
    // local escape hatch from a stuck-looking row and removes any layer record
    // that was already registered for this catalog item.
    if (ptPendingCatalogIdx === idx) {
      ptPendingCatalogIdx = null;
    }

    var activeRec = ptFindCustomLayerByCatalogIndex(idx);
    if (activeRec && activeRec.id) {
      ptRemoveCustomLayer(activeRec.id);
    }
    if (PT2_CATALOG[idx] && window.BRIM && window.BRIM.uicExplorer) {
      window.BRIM.uicExplorer.removeExternal(
        ptCatalogField(PT2_CATALOG[idx], 'external_layer_id')
      );
    }

    ptClearCatalogLoading(true);
    ptRenderQuickCatalog();
    ptSetCatalogRowNote(idx, msg || 'Load cancelled. Zoom or pan if needed, then try again.', false);
    ptSetStatus('External overlay load cancelled.', false);
  }

  function ptRetryCatalogLoading(idx) {
    idx = Number(idx);
    ptCancelCatalogLoading(idx, 'Retrying external overlay for the current map view...');
    window.setTimeout(function() {
      ptAddCatalogQuickIndex(idx);
    }, 0);
  }

  function ptSetInlineNote(id, msg, isError) {
    var div = document.getElementById(id);
    if (!div) return;

    div.textContent = msg || '';
    div.style.display = msg ? 'block' : 'none';
    div.style.color = isError ? '#8B0000' : '#244C1E';
  }

  function ptManualSelectedLoadMode(currentViewOnly) {
    if (ptPendingCatalogIdx !== null && PT2_CATALOG[ptPendingCatalogIdx]) {
      var catalogMode = ptCatalogField(
        PT2_CATALOG[ptPendingCatalogIdx],
        'default_load_mode'
      ).toLowerCase();
      if (catalogMode) return catalogMode;
    }
    return currentViewOnly ? 'current_view' : 'live';
  }

  function ptManualSqlState(url, selectedType, currentViewOnly, loadMode, whereClause) {
    var detectedType = ptDetectServiceType(url, selectedType);
    var normalizedLoadMode = ptCleanText(loadMode).toLowerCase();
    var cleanedWhere = ptCleanText(whereClause);
    var isFeatureServer = detectedType === 'feature';
    var isMapServer = detectedType === 'map';
    var isTiled = normalizedLoadMode === 'tiled';
    var parentVisualMapServer = isMapServer && !isTiled && !currentViewOnly &&
      ptIsParentMapServerUrl(url);
    var supported = isFeatureServer ||
      (isMapServer && !isTiled && !parentVisualMapServer);

    return {
      detectedType: detectedType,
      supported: supported,
      allowsInput: (isFeatureServer || isMapServer) && !isTiled,
      appliedWhereClause: supported ? cleanedWhere : '',
      validationWhereClause: parentVisualMapServer ? cleanedWhere : '',
      actionLabel: supported && cleanedWhere ?
        'Add filtered overlay' : 'Add configured overlay'
    };
  }

  function ptCurrentManualSqlState() {
    var urlInput = document.getElementById('pt-custom-url');
    var typeInput = document.getElementById('pt-custom-type');
    var currentViewInput = document.getElementById('pt-custom-current-view');
    var whereInput = document.getElementById('pt-custom-where');
    var selectedType = typeInput ? typeInput.value : 'auto';
    var currentViewOnly = currentViewInput ? currentViewInput.checked : false;
    var loadMode = ptManualSelectedLoadMode(currentViewOnly);

    return ptManualSqlState(
      urlInput ? urlInput.value : '',
      selectedType,
      currentViewOnly,
      loadMode,
      whereInput ? whereInput.value : ''
    );
  }

  function ptUpdateManualAddActionLabel() {
    var btn = document.getElementById('pt-custom-add-btn');
    var state = ptCurrentManualSqlState();
    if (btn && !ptManualAddInProgress) btn.textContent = state.actionLabel;
    return state.actionLabel;
  }

  function ptSetManualAddLoading(isLoading, msg) {
    var btn = document.getElementById('pt-custom-add-btn');
    var spinner = document.getElementById('pt-custom-add-spinner');

    if (isLoading) {
      if (ptManualAddClearTimer) {
        window.clearTimeout(ptManualAddClearTimer);
        ptManualAddClearTimer = null;
      }
      ptManualAddStartedAt = Date.now();
      ptManualAddInProgress = true;
    } else {
      // A number of manual-add validation errors are detected synchronously.
      // Without a small minimum display time, the button can switch to
      // "Adding…" and back before the browser has a chance to paint, making it
      // look like nothing happened. Keep the spinner visible briefly even for
      // immediate validation failures.
      var elapsed = Date.now() - ptManualAddStartedAt;
      if (ptManualAddInProgress && elapsed >= 0 && elapsed < PT2_MANUAL_ADD_MIN_SPINNER_MS) {
        if (ptManualAddClearTimer) {
          window.clearTimeout(ptManualAddClearTimer);
        }
        ptManualAddClearTimer = window.setTimeout(function() {
          ptSetManualAddLoading(false);
        }, PT2_MANUAL_ADD_MIN_SPINNER_MS - elapsed);
        return;
      }
      ptManualAddInProgress = false;
      ptManualAddClearTimer = null;
    }

    if (btn) {
      if (isLoading) {
        btn.disabled = true;
        btn.setAttribute('aria-disabled', 'true');
        btn.textContent = 'Adding…';
        btn.classList.add('pt-tools-btn-working');
      } else {
        btn.disabled = false;
        btn.removeAttribute('aria-disabled');
        btn.classList.remove('pt-tools-btn-working');
        ptUpdateManualAddActionLabel();
      }
    }

    if (spinner) {
      spinner.style.display = isLoading ? 'inline-block' : 'none';
    }

    if (isLoading && msg) {
      ptSetInlineNote('pt-manual-action-note', msg, false);
    }
  }

  function ptResetManualExternalAddState() {
    // Manual URL-added overlays are normal ptCustomLayers records and are
    // removed by ptClearCustomLayers().  This helper resets the surrounding
    // manual-add UI so Clear external / clear all leaves no stale spinner,
    // action note, or current-view refresh control behind.
    if (ptManualAddClearTimer) {
      window.clearTimeout(ptManualAddClearTimer);
      ptManualAddClearTimer = null;
    }
    ptManualAddInProgress = false;
    ptManualAddStartedAt = 0;
    ptSetManualAddLoading(false);
    ptSetInlineNote('pt-manual-action-note', '', false);
    ptLastManualCurrentViewLayerId = null;
    ptUpdateManualRefreshControl();
  }

  function ptMirrorManualStatusAndMaybeFinish(msg, isError) {
    if (!ptManualAddInProgress) return;

    if (msg) {
      ptSetInlineNote('pt-manual-action-note', msg, isError);
    }

    if (!ptStatusKeepsCatalogSpinner(msg)) {
      ptSetManualAddLoading(false);
    }
  }

  function ptClearActionNotes() {
    ptSetInlineNote('pt-catalog-action-note', '', false);
    ptSetInlineNote('pt-manual-action-note', '', false);
  }

  function ptSetMeasureStatus(msg, isError) {
    ptSetInlineNote('pt-measure-status', msg, isError);
  }

  function ptEscapeHtml(value) {
    if (value === null || value === undefined) return '';
    return String(value)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#039;');
  }

  function ptNormalizeUrl(url) {
    return String(url || '').trim().replace(/\/+$/, '');
  }

  function ptShortUrl(url) {
    url = String(url || '');
    if (url.length <= 58) return url;
    return url.substring(0, 55) + '...';
  }

  function ptCleanText(value) {
    if (value === null || value === undefined) return '';
    return String(value).trim();
  }

  function ptTruth(value) {
    var v = ptCleanText(value).toLowerCase();
    return v === 'true' || v === 'yes' || v === '1' || v === 'y';
  }

  function ptLooksLikeArcgisHubPage(url) {
    url = String(url || '').toLowerCase();

    return (
      url.indexOf('/datasets/') >= 0 ||
      url.indexOf('hub.arcgis.com') >= 0 ||
      url.indexOf('arcgis.com/home/item.html') >= 0 ||
      url.indexOf('arcgis.com/apps/mapviewer') >= 0
    );
  }

  function ptDetectServiceType(url, selectedType) {
    url = ptNormalizeUrl(url);
    selectedType = selectedType || 'auto';

    // Manual selections always win.
    if (selectedType !== 'auto') {
      return selectedType;
    }

    var lower = url.toLowerCase();

    if (
      lower.indexOf('f=geojson') >= 0 ||
      lower.indexOf('format=geojson') >= 0 ||
      lower.match(/\.geojson($|\?)/)
    ) {
      return 'geojson';
    }

    if (lower.match(/\/featureserver(\/\d+)?($|\?)/)) {
      return 'feature';
    }

    if (lower.match(/\/imageserver($|\?)/)) {
      return 'image';
    }

    if (lower.match(/\/mapserver(\/\d+)?($|\?)/)) {
      return 'map';
    }

    if (ptLooksLikeArcgisHubPage(url)) {
      return 'hub';
    }

    return 'unknown';
  }

  function ptIsParentMapServerUrl(url) {
    return /\/MapServer$/i.test(ptNormalizeUrl(url));
  }

  function ptIsArcgisRestLayerOrServiceUrl(url) {
    return /\/(MapServer|FeatureServer)(\/\d+)?$|\/ImageServer$/i.test(ptNormalizeUrl(url));
  }

  function ptArcgisParentServiceUrl(url) {
    var m = ptNormalizeUrl(url).match(/^(.*\/(?:MapServer|FeatureServer))(?:\/\d+)?$/i);
    return m && m[1] ? m[1] : ptNormalizeUrl(url);
  }

  function ptIsParentArcgisServiceUrl(url) {
    return /\/(MapServer|FeatureServer)$/i.test(ptNormalizeUrl(url));
  }

  function ptArcgisServerWord(url) {
    var m = ptNormalizeUrl(url).match(/\/(MapServer|FeatureServer)(?:\/\d+)?$/i);
    return m && m[1] ? m[1] : '';
  }

  function ptArcgisJsonUrl(url) {
    url = ptNormalizeUrl(url);
    return url + (url.indexOf('?') >= 0 ? '&' : '?') + 'f=json';
  }

  function ptFetchArcgisJson(url) {
    return fetch(ptArcgisJsonUrl(url), { cache: 'no-store' })
      .then(function(resp) {
        if (!resp.ok) {
          throw new Error('HTTP ' + resp.status + ' ' + resp.statusText);
        }
        return resp.json();
      });
  }

  function ptLayerUrlFromParent(parentUrl, layerId) {
    return ptNormalizeUrl(parentUrl) + '/' + String(layerId);
  }

  function ptLayerChoicesFromServiceMetadata(meta) {
    meta = meta || {};

    return (Array.isArray(meta.layers) ? meta.layers : [])
      .filter(function(layer) {
        return layer && layer.id !== null && layer.id !== undefined;
      })
      .map(function(layer) {
        return {
          id: layer.id,
          name: ptCleanText(layer.name) || ('Layer ' + layer.id),
          type: ptCleanText(layer.type)
        };
      });
  }

  function ptHideSublayerChooser() {
    var chooser = document.getElementById('pt-custom-sublayer-chooser');
    if (chooser) chooser.style.display = 'none';
  }

  function ptPopulateSublayerChooser(parentUrl, choices) {
    var chooser = document.getElementById('pt-custom-sublayer-chooser');
    var select = document.getElementById('pt-custom-sublayer-select');

    if (!chooser || !select) return;

    chooser.style.display = choices && choices.length > 1 ? 'block' : 'none';
    chooser.setAttribute('data-pt-parent-url', ptNormalizeUrl(parentUrl));
    select.setAttribute('data-pt-parent-url', ptNormalizeUrl(parentUrl));

    select.innerHTML = '';

    var blank = document.createElement('option');
    blank.value = '';
    blank.textContent = 'Choose a layer...';
    select.appendChild(blank);

    (choices || []).forEach(function(choice) {
      var opt = document.createElement('option');
      opt.value = ptLayerUrlFromParent(parentUrl, choice.id);
      opt.textContent = choice.name + ' (' + choice.id + ')';
      select.appendChild(opt);
    });
  }

  function ptSelectedSublayerUrlForParent(parentUrl) {
    var select = document.getElementById('pt-custom-sublayer-select');
    if (!select) return '';

    var expectedParent = ptNormalizeUrl(parentUrl);
    var selectParent = ptNormalizeUrl(select.getAttribute('data-pt-parent-url') || '');

    if (expectedParent && selectParent === expectedParent && select.value) {
      return ptNormalizeUrl(select.value);
    }

    return '';
  }

  function ptUseSelectedSublayerUrl() {
    var select = document.getElementById('pt-custom-sublayer-select');
    var urlInput = document.getElementById('pt-custom-url');

    if (!select || !urlInput || !select.value) return false;

    urlInput.value = ptNormalizeUrl(select.value);
    ptHideSublayerChooser();
    ptUpdatePopupSupportUI();
    ptUpdateLoadModeUI();
    ptSetInlineNote(
      'pt-manual-action-note',
      'Updated URL to selected service layer. Click ' + ptUpdateManualAddActionLabel() + ' to load it.',
      false
    );

    return true;
  }

  function ptResolveParentArcgisUrlForCurrentView(url, detectedType, actionNoteId) {
    var urlInput = document.getElementById('pt-custom-url');
    var parentUrl = ptArcgisParentServiceUrl(url);
    var selectedLayerUrl = ptSelectedSublayerUrlForParent(parentUrl);

    if (selectedLayerUrl) {
      if (urlInput) urlInput.value = selectedLayerUrl;
      ptHideSublayerChooser();
      ptUpdatePopupSupportUI();
      ptUpdateLoadModeUI();
      window.setTimeout(function() {
        ptHandleAddCustomLayer(actionNoteId);
      }, 25);
      return;
    }

    ptSetStatus('Checking ArcGIS service metadata for layer IDs...', false);

    ptFetchArcgisJson(parentUrl)
      .then(function(meta) {
        var choices = ptLayerChoicesFromServiceMetadata(meta);

        if (choices.length === 0) {
          ptSetStatus(
            'Could not find mappable layers in this parent service. Open service metadata and paste a specific layer URL ending in /' +
              ptArcgisServerWord(parentUrl) + '/0, /' + ptArcgisServerWord(parentUrl) + '/1, etc.',
            true
          );
          return;
        }

        if (choices.length === 1) {
          var resolvedUrl = ptLayerUrlFromParent(parentUrl, choices[0].id);
          if (urlInput) urlInput.value = resolvedUrl;
          ptHideSublayerChooser();
          ptUpdatePopupSupportUI();
          ptUpdateLoadModeUI();
          ptSetStatus(
            'Auto-selected service layer ' + choices[0].name + ' (' + choices[0].id + '). Querying current map view...',
            false
          );
          window.setTimeout(function() {
            ptHandleAddCustomLayer(actionNoteId);
          }, 25);
          return;
        }

        ptPopulateSublayerChooser(parentUrl, choices);
        ptSetStatus(
          'This parent service has multiple layers. Choose one in the layer selector near the URL box, then click ' +
            ptUpdateManualAddActionLabel() + ' again.',
          true
        );
      })
      .catch(function(err) {
        console.error(err);
        ptSetStatus(
          'Could not read the ArcGIS service metadata. Open service metadata and paste a specific layer URL ending in /' +
            ptArcgisServerWord(parentUrl) + '/0, /' + ptArcgisServerWord(parentUrl) + '/1, etc.',
          true
        );
      });
  }

  function ptMapServerParentUrlMessage() {
    return 'Current-view MapServer loading needs a specific layer URL ending in /MapServer/0, /MapServer/1, etc. This URL ends at the parent /MapServer service. Open the service metadata and use the number listed under Layers. For single-layer services, /MapServer/0 is often correct; use Try /0 to update the URL.';
  }

  function ptServiceTypeLabel(type) {
    if (type === 'feature') return 'ArcGIS FeatureServer';
    if (type === 'map') return 'ArcGIS MapServer';
    if (type === 'image') return 'ArcGIS ImageServer';
    if (type === 'geojson') return 'GeoJSON';
    if (type === 'hub') return 'ArcGIS Hub / portal page';
    return 'Unknown';
  }

  function ptNumberOrNull(value) {
    var txt = ptCleanText(value);
    if (!txt) return null;
    var out = Number(txt);
    if (!isFinite(out)) return null;
    return out;
  }

  function ptDeriveLegendUrl(url, serviceType) {
    url = ptNormalizeUrl(url);
    serviceType = serviceType || '';
    var lower = url.toLowerCase();
    if (serviceType === 'map' || lower.indexOf('/mapserver') >= 0) {
      var m = url.match(/^(.*\/MapServer)(\/\d+)?$/i);
      if (m && m[1]) return m[1] + '/legend';
    }
    return '';
  }

  function ptZoomCheck(minZoomValue, label, noteId) {
    var minZoom = ptNumberOrNull(minZoomValue);
    if (minZoom === null) return true;

    var zoom = map && typeof map.getZoom === 'function' ? map.getZoom() : null;
    if (zoom === null) return true;

    if (zoom < minZoom) {
      var msg =
        label + ' is best loaded at zoom ' + minZoom +
        ' or closer. Current zoom: ' + zoom +
        '. Zoom in and try again.';

      if (noteId) {
        ptSetInlineNote(noteId, msg, true);
      }
      ptSetStatus(msg, true);

      return false;
    }

    return true;
  }

  function ptExplainUrlProblem(url, detectedType) {
    if (detectedType === 'hub') {
      return 'This looks like an ArcGIS Hub or portal dataset page, not a direct service endpoint. Open the dataset API / REST / ArcGIS GeoServices link and paste a URL ending in /FeatureServer/0 or /MapServer/0.';
    }

    if (detectedType === 'unknown') {
      return 'Could not auto-detect the service type. Try a direct URL containing /FeatureServer/0, /MapServer/0, or a GeoJSON URL.';
    }

    return '';
  }

  function ptSplitFieldList(value) {
    value = ptCleanText(value);

    if (!value) return [];

    return value
      .split(/[;,]/)
      .map(function(v) { return ptCleanText(v); })
      .filter(function(v, idx, arr) {
        return v && arr.indexOf(v) === idx;
      });
  }

  function ptFieldLookupAliases(fieldName) {
    // ArcGIS identify/query responses are not consistent about whether the
    // returned attribute keys are native field names or service aliases.  This
    // is especially obvious in NOAA/NWS/SPC MapServers: Day 1 fire weather can
    // return alias keys like "Outlook" and "Valid Date Time", while Day 3
    // layers return native/lower-case keys such as dn, valid, and expire.
    // Keep this lookup small and conservative so general External layers are
    // not accidentally remapped.
    var f = String(fieldName || '').toLowerCase();

    if (f === 'dn') return ['dn', 'DN', 'Outlook'];
    if (f === 'valid') return ['valid', 'VALID', 'Valid Date Time'];
    if (f === 'expire') return ['expire', 'EXPIRE', 'Expiration Date'];
    if (f === 'issue') return ['issue', 'ISSUE', 'Issued', 'Issue'];
    if (f === 'idp_filedate') return ['idp_filedate', 'IDP file date', 'GIS FIle Date', 'GIS File Date'];
    if (f === 'idp_ingestdate') return ['idp_ingestdate', 'IDP ingest date', 'GIS Ingest Date'];
    if (f === 'idp_source') return ['idp_source', 'IDP source', 'GIS Source'];
    if (f === 'label') return ['label', 'LABEL', 'Risk label'];
    if (f === 'label2') return ['label2', 'LABEL2', 'Risk category'];
    if (f === 'name') return ['name', 'NAME', 'Discussion'];

    return [fieldName];
  }

  function ptFieldValue(props, fieldName) {
    props = props || {};

    if (!fieldName) return '';

    var candidates = ptFieldLookupAliases(fieldName);

    // Exact/native-or-alias match first.
    for (var c = 0; c < candidates.length; c++) {
      if (Object.prototype.hasOwnProperty.call(props, candidates[c])) {
        return props[candidates[c]];
      }
    }

    // Case-insensitive fallback because public service field casing can vary.
    var keys = Object.keys(props);

    for (var k = 0; k < keys.length; k++) {
      for (var i = 0; i < candidates.length; i++) {
        if (String(keys[k]).toLowerCase() === String(candidates[i]).toLowerCase()) {
          return props[keys[k]];
        }
      }
    }

    return '';
  }

  function ptHasValue(value) {
    return value !== null &&
      value !== undefined &&
      String(value) !== '' &&
      String(value).toLowerCase() !== 'null' &&
      String(value).toLowerCase() !== 'na' &&
      String(value).toLowerCase() !== 'nan';
  }

  function ptUicFirstValue(props, fieldNames) {
    var names = Array.isArray(fieldNames) ? fieldNames : [fieldNames];
    for (var i = 0; i < names.length; i += 1) {
      var value = ptFieldValue(props, names[i]);
      if (ptHasValue(value)) return value;
    }
    return '';
  }

  function ptIsExternalNoiseField(fieldName) {
    // Public ArcGIS services often return GIS bookkeeping fields that are
    // useful for QA but distracting in field-staff popups.  Hide these from
    // normal External popup tables.  Curated popup_fields can still name a
    // field explicitly if a future layer truly needs it.
    var f = String(fieldName || '').trim();
    if (!f) return true;

    var lower = f.toLowerCase();
    var compact = lower.replace(/[\s_\.\-]+/g, '');

    if (compact === 'objectid' || compact === 'fid' || compact === 'oid' ||
        compact === 'globalid' || compact === 'shape' || compact === 'geometry' ||
        compact === 'shapearea' || compact === 'shapelength' || compact === 'shapelen') {
      return true;
    }

    if (/^objectid_?\d*$/.test(lower)) return true;
    if (/^fid_?\d*$/.test(lower)) return true;
    if (/^shape[_\.]/i.test(f)) return true;
    if (/^shape$/i.test(f)) return true;

    return false;
  }

  function ptExternalAttributeKeys(props, options, excludeFields) {
    props = props || {};
    options = options || {};
    excludeFields = excludeFields || [];

    var excludeLookup = {};
    excludeFields.forEach(function(fieldName) {
      var f = ptCleanText(fieldName).toLowerCase();
      if (f) excludeLookup[f] = true;
    });

    return Object.keys(props).filter(function(k) {
      if (!ptHasValue(props[k])) return false;
      if (ptIsExternalNoiseField(k)) return false;
      if (excludeLookup[String(k).toLowerCase()]) return false;
      return true;
    });
  }

  function ptHasMeaningfulPopupProperties(props, options) {
    return ptExternalAttributeKeys(props || {}, options || {}).length > 0;
  }

  function ptStreamGaugeRecordClass(props) {
    props = props || {};

    var name = ptCleanText(ptFieldValue(props, 'name')).toLowerCase();
    var governing = ptCleanText(ptFieldValue(props, 'governing_location')).toLowerCase();
    var text = (name + ' ' + governing).trim();
    var stage = Number(ptFieldValue(props, 'stage_ft'));

    if (!text) return 'stream';

    // The Living Atlas live gage feed mixes ordinary stream/canal gages with
    // reservoir/lake pool records.  Do not use high flow_cfs alone as a
    // classifier: large rivers and tidal/backwater reaches can have very high
    // discharge, while some reservoir pool records reuse the same flow_cfs
    // service field for a source-graph value.  Classify first from station
    // name/context, using stage/elevation only as supporting evidence.
    var hasReservoirWord = /\b(reservoir|reservior|storage|forebay|afterbay)\b/.test(text);
    var hasLakeWord = /\blake\b/.test(text);
    var hasDamWord = /\bdam\b/.test(text);
    var watercoursePattern = '\\b(creek|cr|c|river|rvr|rv|r|canal|cn|channel|ch|wash|slough|fork|run|tributary|aqueduct|drain|ditch)\\b';
    var flowStructurePattern = '\\b(outlet|inlet|release|spill|spillway|spilway|weir|diversion|div|outflow|inflow)\\b';
    var relativePositionPattern = '\\b(above|below|ab|bl|downstream|upstream|near|nr|at)\\b';
    var hasWatercourseWord = new RegExp(watercoursePattern).test(text);
    var hasFlowStructureWord = new RegExp(flowStructurePattern).test(text);
    var hasRelativePositionWord = new RegExp(relativePositionPattern).test(text);
    var hasWatercourseAtPoolWord = new RegExp(watercoursePattern + '.*' + relativePositionPattern + '.*\\b(reservoir|reservior|forebay|afterbay|lake)\\b').test(text);
    var hasPoolStage = isFinite(stage) && Math.abs(stage) >= 100;

    // Clear reservoir/lake pool records.  Stage/elevation >= 100 ft is strong
    // evidence when paired with reservoir/lake wording, but not required for
    // direct pool-name records such as Clear Lake at Lakeport.
    if ((hasReservoirWord || hasLakeWord) && hasPoolStage) return 'reservoir';

    if (/\b(clear lake|lake havasu|lake mohave|lake sonoma|lake mendocino|san andreas lake|walker lake)\b/.test(text)) {
      return 'reservoir';
    }

    if (hasReservoirWord && !hasFlowStructureWord && !hasWatercourseAtPoolWord) {
      return 'reservoir';
    }

    if (hasLakeWord && !hasWatercourseWord && !hasFlowStructureWord && !hasRelativePositionWord) {
      return 'reservoir';
    }

    // Reservoir/lake-adjacent records are retained, but flagged rather than
    // forced into either the plain streamflow class or the likely pool-record
    // class.  These often represent streams, canals, outlets, releases, or
    // downstream gages near reservoirs/lakes.  Dam-only names generally stay
    // as stream/canal gages unless paired with reservoir/lake/pool wording.
    if (hasReservoirWord || hasLakeWord) {
      return 'questionable';
    }

    if (hasDamWord && /\b(pool|lake|reservoir|reservior|forebay|afterbay)\b/.test(text)) {
      return 'questionable';
    }

    return 'stream';
  }

  function ptIsReservoirGaugeFeature(props) {
    return ptStreamGaugeRecordClass(props) === 'reservoir';
  }

  function ptIsQuestionableReservoirGaugeFeature(props) {
    return ptStreamGaugeRecordClass(props) === 'questionable';
  }

  function ptFormatStorageAf(value) {
    var af = Number(value);

    if (!isFinite(af)) return '';

    // Storage values are shown consistently as thousand acre-feet.  This keeps
    // reservoir hover cards compact and avoids switching between AF and KAF.
    return (af / 1000).toLocaleString(undefined, {
      maximumFractionDigits: Math.abs(af) >= 100000 ? 0 : 1
    }) + ' KAF';
  }

  function ptShortNativeFieldName(fieldName) {
    var f = String(fieldName || '');
    if (f.indexOf('.') < 0) return f;

    var parts = f.split('.').filter(function(part) {
      return ptCleanText(part) !== '';
    });

    return parts.length ? parts[parts.length - 1] : f;
  }

  function ptDisplayFieldName(fieldName, aliasMap) {
    aliasMap = aliasMap || {};

    if (aliasMap[fieldName]) {
      return aliasMap[fieldName];
    }

    // Many ArcGIS services return joined/database-qualified field names such
    // as SCHEMA.VIEW.FIELD. For display only, strip unhelpful prefixes. The
    // native attribute keys remain unchanged internally for querying and lookup.
    return ptShortNativeFieldName(fieldName);
  }

  function ptDisplayFieldNameForKey(fieldName, allKeys, aliasMap) {
    aliasMap = aliasMap || {};
    allKeys = allKeys || [];

    if (aliasMap[fieldName]) {
      return aliasMap[fieldName];
    }

    var native = String(fieldName || '');

    if (native.indexOf('.') < 0) {
      return native;
    }

    var parts = native.split('.').filter(function(part) {
      return ptCleanText(part) !== '';
    });

    if (parts.length === 0) {
      return native;
    }

    function suffixFor(key, nParts) {
      var p = String(key || '').split('.').filter(function(part) {
        return ptCleanText(part) !== '';
      });
      return p.slice(Math.max(0, p.length - nParts)).join('.');
    }

    // Prefer the final token, but if multiple returned fields would collapse
    // to the same label, keep enough suffix to make the popup labels unique.
    for (var n = 1; n <= parts.length; n++) {
      var candidate = suffixFor(native, n);
      var collisions = allKeys.filter(function(k) {
        return suffixFor(k, n) === candidate;
      });

      if (collisions.length <= 1) {
        return candidate;
      }
    }

    return native;
  }

  function ptDisplayFieldLabelHtml(fieldName, aliasMap, options) {
    options = options || {};
    aliasMap = aliasMap || {};

    var alias = ptDisplayFieldName(fieldName, aliasMap);
    var showNative = ptCleanText(options.showNativeFieldNames).toLowerCase() === 'true';

    if (showNative && alias !== fieldName) {
      return ptEscapeHtml(alias) +
        ' <span class="pt-native-field-name">(' +
        ptEscapeHtml(fieldName) +
        ')</span>';
    }

    return ptEscapeHtml(alias);
  }

  function ptDisplayFieldLabelHtmlForProps(fieldName, aliasMap, options, props) {
    options = options || {};
    props = props || {};

    var lowerField = String(fieldName || '').toLowerCase();

    // Live Stream Gages / Flow uses the service field flow_cfs for ordinary
    // stream discharge, while reservoir/lake records in the same feed can use
    // that numeric field as a storage-style value.  Keep the data field intact
    // but make the display label contextual so users do not see a confusing
    // combined "flow/storage" row.
    if (ptIsStreamGaugeFlowStyle(options)) {
      var gaugeClass = ptStreamGaugeRecordClass(props);

      if (lowerField === 'flow_cfs') {
        if (gaugeClass === 'reservoir') return 'Provider flow_cfs';
        if (gaugeClass === 'questionable') return 'Flow / source value';
        return 'Flow';
      }
      if (lowerField === 'stage_ft') {
        if (gaugeClass === 'reservoir') return 'Pool elevation / stage';
        if (gaugeClass === 'questionable') return 'Stage / source value';
        return 'Stage';
      }
    }

    return ptDisplayFieldLabelHtml(fieldName, aliasMap, options);
  }

  function ptLooksLikeDateField(fieldName) {
    var f = String(fieldName || '').toLowerCase();

    return f.indexOf('date') >= 0 ||
      f.indexOf('_dt') >= 0 ||
      f.indexOf('time') >= 0 ||
      f.indexOf('msmt') >= 0 && f.indexOf('date') >= 0;
  }

  function ptFormatDateMillis(value) {
    var n = Number(value);

    if (!isFinite(n)) return '';

    // ArcGIS date fields are commonly milliseconds since Unix epoch.  Use a
    // broad but conservative threshold so small numeric water-level values are
    // not accidentally treated as dates.
    if (Math.abs(n) < 100000000000) return '';

    var d = new Date(n);

    if (isNaN(d.getTime())) return '';

    var yyyy = d.getUTCFullYear();
    var mm = String(d.getUTCMonth() + 1).padStart(2, '0');
    var dd = String(d.getUTCDate()).padStart(2, '0');

    return yyyy + '-' + mm + '-' + dd;
  }

  function ptFormatDateTimeMillis(value) {
    var n = Number(value);

    if (!isFinite(n)) return '';
    if (Math.abs(n) < 100000000000) return '';

    var d = new Date(n);

    if (isNaN(d.getTime())) return '';

    try {
      return d.toLocaleString(undefined, {
        year: 'numeric',
        month: '2-digit',
        day: '2-digit',
        hour: '2-digit',
        minute: '2-digit',
        timeZoneName: 'short'
      });
    } catch (e) {
      return d.toString();
    }
  }

  function ptParseCompactYmdHmUtc(value) {
    // NOAA/SPC valid/expire fields are compact UTC/Z-style operational tokens
    // such as YYYYMMDDHHMM.  Parse them as UTC so BRIM can display a clear
    // California/Pacific operational time in hover cards while also preserving
    // provider/UTC time in popups.
    var s = String(value == null ? '' : value).trim();
    var m = s.match(/^(\d{4})(\d{2})(\d{2})(?:(\d{2})(\d{2}))?$/);
    if (!m) return null;

    var yyyy = Number(m[1]);
    var mm = Number(m[2]);
    var dd = Number(m[3]);
    var hh = m[4] == null ? 0 : Number(m[4]);
    var min = m[5] == null ? 0 : Number(m[5]);

    if (yyyy < 1900 || yyyy > 2100 || mm < 1 || mm > 12 || dd < 1 || dd > 31) return null;
    if (hh < 0 || hh > 23 || min < 0 || min > 59) return null;

    var d = new Date(Date.UTC(yyyy, mm - 1, dd, hh, min, 0));
    return isNaN(d.getTime()) ? null : d;
  }

  function ptFormatSpcUtcTime(value) {
    var d = ptParseCompactYmdHmUtc(value);
    if (!d) return '';

    var yyyy = d.getUTCFullYear();
    var mm = String(d.getUTCMonth() + 1).padStart(2, '0');
    var dd = String(d.getUTCDate()).padStart(2, '0');
    var hh = String(d.getUTCHours()).padStart(2, '0');
    var min = String(d.getUTCMinutes()).padStart(2, '0');

    return yyyy + '-' + mm + '-' + dd + ' ' + hh + ':' + min + ' UTC';
  }

  function ptFormatSpcPacificTime(value) {
    var d = ptParseCompactYmdHmUtc(value);
    if (!d) return '';

    try {
      return new Intl.DateTimeFormat('en-US', {
        timeZone: 'America/Los_Angeles',
        month: 'short',
        day: 'numeric',
        hour: 'numeric',
        minute: '2-digit',
        timeZoneName: 'short'
      }).format(d);
    } catch (e) {
      return ptFormatSpcUtcTime(value);
    }
  }

  function ptSpcTimeValue(props, sourceField, mode) {
    props = props || {};
    var raw = ptFieldValue(props, sourceField);
    if (!ptHasValue(raw)) return '';

    if (mode === 'pacific') return ptFormatSpcPacificTime(raw);
    if (mode === 'utc') return ptFormatSpcUtcTime(raw);

    return '';
  }


  function ptFormatCompactYmdHm(value) {
    // Some NOAA/NWS/SPC MapServer fields named valid/expire are strings such
    // as YYYYMMDDHHMM, not ArcGIS epoch milliseconds.  If BRIM treats those
    // values as epoch milliseconds, 202606020000 renders as a bogus 1976 date.
    // Format only strict 8- or 12-digit tokens here; leave all other values to
    // the normal ArcGIS date/numeric/text handling below.
    var s = String(value == null ? '' : value).trim();
    var m = s.match(/^(\d{4})(\d{2})(\d{2})(?:(\d{2})(\d{2}))?$/);
    if (!m) return '';

    var yyyy = Number(m[1]);
    var mm = Number(m[2]);
    var dd = Number(m[3]);
    var hh = m[4] == null ? null : Number(m[4]);
    var min = m[5] == null ? null : Number(m[5]);

    if (yyyy < 1900 || yyyy > 2100 || mm < 1 || mm > 12 || dd < 1 || dd > 31) return '';
    if (hh != null && (hh < 0 || hh > 23 || min < 0 || min > 59)) return '';

    var out = m[1] + '-' + m[2] + '-' + m[3];
    if (hh != null) out += ' ' + m[4] + ':' + m[5] + ' UTC';
    return out;
  }


  function ptSpcDnLabel(value, props, options) {
    // SPC MapServer services commonly expose the drawn category as a numeric
    // `dn` value.  The official renderer maps those values to the risk class,
    // but the identify/query attributes only return the number.  Convert the
    // numeric code to the same plain-language label for BRIM hover cards and
    // curated popup summaries.
    options = options || {};
    var n = Number(value);
    if (!isFinite(n)) return '';

    var layerName = ptCleanText(options.layerName || '').toLowerCase();

    if (layerName.indexOf('spc fire wx') >= 0 || layerName.indexOf('spc fire weather') >= 0) {
      if (layerName.indexOf('dry thunder') >= 0) {
        if (n === 5) return 'Isolated dry thunder';
        if (n === 8) return 'Scattered dry thunder';
      }
      if (n === 5) return 'Elevated';
      if (n === 8) return 'Critical';
      if (n === 10) return 'Extreme';
    }

    if (layerName.indexOf('spc convective') >= 0) {
      if (n === 2) return 'Thunderstorm';
      if (n === 3) return 'Marginal Risk';
      if (n === 4) return 'Slight Risk';
      if (n === 5) return 'Enhanced Risk';
      if (n === 6) return 'Moderate Risk';
      if (n === 8) return 'High Risk';
    }

    return '';
  }


  function ptLooksNumericText(value) {
    var s = String(value == null ? '' : value).trim();
    return /^-?\d+(?:\.\d+)?$/.test(s);
  }

  function ptSpcRendererLabelFromNumeric(value, props, options) {
    // Day 3+ SPC fire-weather products sometimes use a renderer field named
    // `label` whose value is a probability token such as 0.40 or 0.70 rather
    // than a plain category string.  Convert those tokens to the same labels
    // shown in the official renderer so hover cards do not show cryptic codes.
    options = options || {};
    var layerName = ptCleanText(options.layerName || '').toLowerCase();
    var n = Number(value);
    if (!isFinite(n)) return '';

    if (layerName.indexOf('spc fire wx') >= 0 || layerName.indexOf('spc fire weather') >= 0) {
      if (layerName.indexOf('dry thunder') >= 0) {
        if (Math.abs(n - 0.10) < 0.00001) return 'Marginal (10%)';
        if (Math.abs(n - 0.40) < 0.00001) return 'Critical (40%)';
      } else {
        if (Math.abs(n - 0.40) < 0.00001) return 'Marginal (40%)';
        if (Math.abs(n - 0.70) < 0.00001) return 'Critical (70%)';
      }
    }

    return '';
  }

  function ptSpcCategoryValue(props, options) {
    // Pseudo-field used by BRIM catalog rows.  It consolidates the best SPC
    // category text across several NOAA/NWS field patterns:
    //   * convective outlooks: label2 is usually the readable risk label;
    //   * fire Day 1/2: dn/Outlook numeric code maps to Elevated/Critical/etc.;
    //   * fire Day 3+: label/label2 may hold probability/category labels.
    props = props || {};
    options = options || {};

    var label2 = ptFieldValue(props, 'label2');
    if (ptHasValue(label2) && !ptLooksNumericText(label2)) return String(label2);

    var label = ptFieldValue(props, 'label');
    if (ptHasValue(label)) {
      var mappedLabel = ptSpcRendererLabelFromNumeric(label, props, options);
      if (mappedLabel) return mappedLabel;
      if (!ptLooksNumericText(label)) return String(label);
    }

    var dn = ptFieldValue(props, 'dn');
    if (ptHasValue(dn)) {
      var dnLabel = ptSpcDnLabel(dn, props, options);
      if (dnLabel) return dnLabel;
    }

    if (ptHasValue(label2)) return String(label2);
    if (ptHasValue(label)) return String(label);
    if (ptHasValue(dn)) return String(dn);

    return '';
  }


  function ptSpcCategoryInterpretationValue(props, options) {
    // BRIM pseudo-field for short, user-facing explanation of SPC categories.
    // This keeps official SPC colors/rendering intact while explaining why,
    // for example, General Thunderstorms may be lighter than Marginal Risk:
    // General Thunderstorms is below the organized severe-risk categories.
    props = props || {};
    options = options || {};

    var category = ptCleanText(ptSpcCategoryValue(props, options));
    if (!category) return '';

    var lowerCategory = category.toLowerCase();
    var lowerLayer = ptCleanText(options.layerName || '').toLowerCase();

    if (lowerLayer.indexOf('spc convective') >= 0) {
      if (lowerCategory.indexOf('general thunderstorm') >= 0 || lowerCategory === 'thunderstorm') {
        return 'Thunderstorms possible; below Marginal/Slight/Enhanced severe-risk categories.';
      }
      if (lowerCategory.indexOf('marginal') >= 0) {
        return 'Lowest SPC severe-thunderstorm risk category.';
      }
      if (lowerCategory.indexOf('slight') >= 0) {
        return 'Second SPC severe-thunderstorm risk category.';
      }
      if (lowerCategory.indexOf('enhanced') >= 0) {
        return 'Middle SPC severe-thunderstorm risk category.';
      }
      if (lowerCategory.indexOf('moderate') >= 0) {
        return 'High-end SPC severe-thunderstorm risk category.';
      }
      if (lowerCategory.indexOf('high') >= 0) {
        return 'Highest SPC severe-thunderstorm risk category.';
      }
    }

    if (lowerLayer.indexOf('spc fire wx') >= 0 || lowerLayer.indexOf('spc fire weather') >= 0) {
      if (lowerCategory.indexOf('isolated dry thunder') >= 0 || lowerCategory.indexOf('iso dryt') >= 0) {
        return 'Isolated dry thunderstorms possible.';
      }
      if (lowerCategory.indexOf('scattered dry thunder') >= 0 || lowerCategory.indexOf('scattered dryt') >= 0) {
        return 'Scattered dry thunderstorms possible.';
      }
      if (lowerCategory.indexOf('marginal') >= 0 && lowerLayer.indexOf('dry thunder') >= 0) {
        return 'Lower-probability dry-thunder outlook area.';
      }
      if (lowerCategory.indexOf('critical') >= 0 && lowerLayer.indexOf('dry thunder') >= 0) {
        return 'Higher-probability dry-thunder outlook area.';
      }
      if (lowerCategory.indexOf('elevated') >= 0) {
        return 'Fire-weather conditions may support increased fire danger; below Critical/Extreme.';
      }
      if (lowerCategory.indexOf('marginal') >= 0) {
        return 'Lower-probability SPC fire-weather outlook area.';
      }
      if (lowerCategory.indexOf('critical') >= 0) {
        return 'Critical fire-weather conditions possible.';
      }
      if (lowerCategory.indexOf('extreme') >= 0) {
        return 'Extreme fire-weather conditions possible.';
      }
    }

    return '';
  }

  function ptFormatFieldValue(fieldName, value, props, options) {
    props = props || {};
    options = options || {};
    if (!ptHasValue(value)) return '';

    var lowerField = String(fieldName || '').toLowerCase();

    if (lowerField === 'gridcode' && ptIsAirNowAqiStyle(options)) {
      return ptAirNowAqiCategory(value, props, options);
    }

    if ((lowerField === 'validtime' || lowerField === 'timestamp') && ptIsAirNowAqiStyle(options)) {
      var airNowTimeText = ptFormatDateTimeMillis(value);
      if (airNowTimeText) return airNowTimeText;
    }

    if (lowerField === 'wcr_name' || lowerField === 'wcr_screen_interval') {
      return String(value);
    }

    if ([
      'totalcompleteddepth',
      'totaldrilldepth',
      'topofperforatedinterval',
      'bottomofperforatedinterval',
      'staticwaterlevel'
    ].indexOf(lowerField) >= 0) {
      var wcrDepth = Number(value);
      if (isFinite(wcrDepth)) {
        return Math.round(wcrDepth).toLocaleString() + ' ft';
      }
    }

    if (lowerField === 'wellyield') {
      var y = Number(value);
      if (isFinite(y)) {
        return y.toLocaleString(undefined, { maximumFractionDigits: 1 });
      }
    }

    if (lowerField === 'spc_category' || lowerField === 'spc_valid_pacific' ||
        lowerField === 'spc_expire_pacific' || lowerField === 'spc_valid_utc' ||
        lowerField === 'spc_expire_utc' || lowerField === 'spc_interpretation') {
      return String(value);
    }

    if (lowerField === 'dn') {
      var spcDnLabel = ptSpcDnLabel(value, props, options);
      if (spcDnLabel) return spcDnLabel;
    }

    if (ptIsUsgsEarthquakeStyle(options) && (lowerField === 'time' || lowerField === 'updated')) {
      var eqTimeText = ptFormatDateTimeMillis(value);
      if (eqTimeText) return eqTimeText;
    }

    if (lowerField === 'valid' || lowerField === 'expire' || ptLooksLikeDateField(lowerField)) {
      var compactYmdHmText = ptFormatCompactYmdHm(value);
      if (compactYmdHmText) return compactYmdHmText;
    }

    // Real-time/live layers need sub-daily timestamps.  ArcGIS Date fields
    // are milliseconds since epoch; show live gage update fields with time,
    // while keeping most map/product dates as simple YYYY-MM-DD values.
    if (lowerField === 'lastupdate' || lowerField === 'last_update' ||
        lowerField === 'last_updated' || lowerField.indexOf('datetime') >= 0) {
      var dateTimeText = ptFormatDateTimeMillis(value);
      if (dateTimeText) return dateTimeText;
    }

    var dateText = ptFormatDateMillis(value);

    // ArcGIS Date fields commonly arrive as epoch milliseconds. If the value
    // is clearly an epoch-millisecond date, format it even if field-name
    // heuristics miss.
    if (dateText) return dateText;

    // U.S. Drought Monitor uses integer DM classes in its current
    // FeatureServer/MapServer identify responses.  Display the human-readable
    // D0-D4 category while preserving the native numeric attribute internally.
    if (lowerField === 'dm') {
      var dmCode = Number(value);
      if (dmCode === 0) return 'D0 (Abnormally Dry)';
      if (dmCode === 1) return 'D1 (Moderate Drought)';
      if (dmCode === 2) return 'D2 (Severe Drought)';
      if (dmCode === 3) return 'D3 (Extreme Drought)';
      if (dmCode === 4) return 'D4 (Exceptional Drought)';
    }

    // CPC outlook probabilities are served as numeric categories.  Appending
    // the percent sign makes hover cards easier to read without changing the
    // underlying returned attribute.
    if (lowerField === 'prob') {
      var prob = Number(value);
      if (isFinite(prob)) return String(prob) + '%';
    }

    // CPC outlook categories are short operational categories in the service.
    // Display them as plain-language labels in PT2 hover cards and popups.
    if (lowerField === 'cat') {
      return ptCpcCategoryLabel(value);
    }

    // ALERTCalifornia camera fields are short operational values.  Format
    // them as plain language so hover cards can be read quickly.
    if (lowerField === 'isonline') {
      return ptAlertCameraOnlineLabel(value);
    }

    if (lowerField === 'positionpan') {
      var pan = Number(value);
      if (isFinite(pan)) return Math.round(pan) + '°';
    }

    if (lowerField === 'positiontilt') {
      var tilt = Number(value);
      if (isFinite(tilt)) return Math.round(tilt) + '°';
    }

    // Live stream gauge feeds commonly expose instantaneous discharge as
    // flow_cfs and stage as stage_ft. Format those fields with units so hover
    // cards and curated popups can be read quickly in the field.
    if (lowerField === 'flow_cfs') {
      var flow = Number(value);
      if (isFinite(flow)) {
        if (ptIsStreamGaugeFlowStyle(options)) {
          var gaugeClass = ptStreamGaugeRecordClass(props);
          if (gaugeClass === 'reservoir') {
            return Math.round(flow).toLocaleString() + ' (provider flow_cfs; verify source graph)';
          }
          if (gaugeClass === 'questionable') {
            return Math.round(flow).toLocaleString() + ' cfs? (verify source graph)';
          }
        }
        return Math.round(flow).toLocaleString() + ' cfs';
      }
    }

    if (lowerField === 'stage_ft') {
      var stage = Number(value);
      if (isFinite(stage)) {
        return stage.toFixed(2) + ' ft';
      }
    }

    if (lowerField === 'lastupdate_age') {
      var age = Number(value);
      if (isFinite(age)) {
        return age.toLocaleString() + ' hr';
      }
    }

    return value;
  }

  function ptAliasMapFromString(value) {
    var out = {};
    var parts = ptSplitFieldList(value);

    parts.forEach(function(part) {
      var m = String(part).split('=');

      if (m.length >= 2) {
        var key = ptCleanText(m.shift());
        var val = ptCleanText(m.join('='));

        if (key && val) {
          out[key] = val;
        }
      }
    });

    return out;
  }

  function ptPropertiesForFields(props, fields, options) {
    props = props || {};
    fields = fields || [];
    options = options || {};

    var out = [];

    fields.forEach(function(fieldName) {
      var lowerField = String(fieldName || '').toLowerCase();
      var v;

      if (lowerField === 'spc_category') {
        v = ptSpcCategoryValue(props, options);
      } else if (lowerField === 'spc_interpretation') {
        v = ptSpcCategoryInterpretationValue(props, options);
      } else if (lowerField === 'spc_valid_pacific') {
        v = ptSpcTimeValue(props, 'valid', 'pacific');
      } else if (lowerField === 'spc_expire_pacific') {
        v = ptSpcTimeValue(props, 'expire', 'pacific');
      } else if (lowerField === 'spc_valid_utc') {
        v = ptSpcTimeValue(props, 'valid', 'utc');
      } else if (lowerField === 'spc_expire_utc') {
        v = ptSpcTimeValue(props, 'expire', 'utc');
      } else if (lowerField === 'wcr_name') {
        v = ptWcrNameValue(props);
      } else if (lowerField === 'wcr_screen_interval') {
        v = ptWcrScreenIntervalValue(props);
      } else {
        v = ptFieldValue(props, fieldName);
      }

      if (ptHasValue(v)) {
        out.push({
          field: fieldName,
          value: v
        });
      }
    });

    return out;
  }

  function ptExternalAttributeRowsHtml(props, options, suppliedKeys) {
    props = props || {};
    options = options || {};

    var keys = Array.isArray(suppliedKeys) ? suppliedKeys : ptExternalAttributeKeys(props, options);

    if (keys.length === 0) {
      return '<div style="font-size:11px;color:#555;margin-top:4px;">No non-system attributes returned.</div>';
    }

    var aliasMap = ptAliasMapFromString(options.popupAliases);
    var showNative = ptCleanText(options.showNativeFieldNames).toLowerCase() === 'true';
    var html =
      '<div class="pt-external-popup-table-wrap" style="max-height:300px;overflow:auto;margin-top:5px;border:1px solid #ddd;border-radius:4px;">' +
      '<table style="border-collapse:collapse;width:100%;font-size:11px;">';

    for (var i = 0; i < keys.length; i++) {
      var k = keys[i];
      var label = ptDisplayFieldNameForKey(k, keys, aliasMap);
      var nativeNote = showNative && label !== k ?
        ' <span class="pt-native-field-name">(' + ptEscapeHtml(k) + ')</span>' :
        '';

      html +=
        '<tr>' +
          '<td style="vertical-align:top;padding:2px 6px;border-bottom:1px solid #eee;background:#fafafa;white-space:nowrap;" title="' + ptEscapeHtml(k) + '"><b>' +
            ptEscapeHtml(label) + nativeNote +
          '</b></td>' +
          '<td style="vertical-align:top;padding:2px 6px;border-bottom:1px solid #eee;">' +
            ptEscapeHtml(ptFormatFieldValue(k, props[k], props, options)) +
          '</td>' +
        '</tr>';
    }

    html += '</table></div>';

    if (keys.length > 50) {
      html += '<div style="font-size:11px;color:#555;margin-top:3px;">' +
        'Showing all ' + keys.length + ' returned non-system attributes. Field labels may be shortened for readability; hover a label for the native field name.' +
        '</div>';
    }

    return html;
  }

  function ptTemplateUrlFromProperties(template, props) {
    template = ptCleanText(template);
    props = props || {};

    if (!template) return '';

    var missingToken = false;

    var out = template.replace(/\{([^{}]+)\}/g, function(match, token) {
      token = ptCleanText(token);

      var transform = '';
      var fieldName = token;

      if (/_lower$/i.test(fieldName)) {
        transform = 'lower';
        fieldName = fieldName.replace(/_lower$/i, '');
      } else if (/_upper$/i.test(fieldName)) {
        transform = 'upper';
        fieldName = fieldName.replace(/_upper$/i, '');
      } else if (/_raw$/i.test(fieldName)) {
        transform = 'raw';
        fieldName = fieldName.replace(/_raw$/i, '');
      }

      var value = ptFieldValue(props, fieldName);

      if (!ptHasValue(value)) {
        missingToken = true;
        return '';
      }

      value = String(value).trim();

      if (transform === 'lower') {
        value = value.toLowerCase();
      } else if (transform === 'upper') {
        value = value.toUpperCase();
      } else if (transform === 'raw') {
        return value;
      }

      return encodeURIComponent(value);
    });

    return missingToken ? '' : out;
  }

  function ptPopupLinkHtml(props, options) {
    props = props || {};
    options = options || {};

    var templateText = ptCleanText(options.popupLinkTemplate);
    if (!templateText) return '';

    // Multiple curated popup links can be supplied with || separators in the
    // catalog/override fields.  This keeps simple one-link rows unchanged,
    // while allowing layers such as U.S. Drought Monitor to offer both the
    // source page and a related BRIM/BLM report page.
    var templates = templateText.split('||').map(ptCleanText).filter(function(x) { return !!x; });
    var labels = ptCleanText(options.popupLinkLabel).split('||').map(ptCleanText);
    var linkHtml = '';

    templates.forEach(function(template, idx) {
      var url = ptTemplateUrlFromProperties(template, props);
      if (!url || !/^https?:\/\//i.test(url)) return;

      var label = labels[idx] || labels[0] || 'Open related page';

      linkHtml += '<div style="margin-top:7px;">' +
        '<a href="' + ptEscapeHtml(url) + '" target="_blank" rel="noopener noreferrer">' +
          ptEscapeHtml(label) +
        '</a>' +
      '</div>';
    });

    return linkHtml;
  }

  function ptStreamGaugeClassificationNoteHtml(props, options) {
    props = props || {};
    options = options || {};

    if (!ptIsStreamGaugeFlowStyle(options)) return '';

    var gaugeClass = ptStreamGaugeRecordClass(props);

    if (gaugeClass === 'reservoir') {
      return '<div style="margin-top:7px;padding:5px 6px;border-left:3px solid #2b8cbe;background:#eef7fb;font-size:11px;color:#24465f;">' +
        '<b>BRIM note:</b> Likely reservoir/lake pool record. The provider flow_cfs value is retained from the source service, but should not be interpreted as current stream discharge. Use the linked source graph for context.' +
        '</div>';
    }

    if (gaugeClass === 'questionable') {
      return '<div style="margin-top:7px;padding:5px 6px;border-left:3px solid #7b8da8;background:#f7f9fc;font-size:11px;color:#3d4655;">' +
        '<b>BRIM note:</b> Reservoir/lake/dam wording appears in the site name or context, but the record is not a clear pool record. Treat flow/stage as reservoir-adjacent or source-specific and verify with the linked graph.' +
        '</div>';
    }

    return '';
  }



  function ptShowExternalExtraAttributesInline(extraAttributeKeys, options) {
    extraAttributeKeys = extraAttributeKeys || [];
    options = options || {};

    // AML source rows have a manageable number of useful operational/source
    // fields. Showing them directly is faster for review than hiding them
    // behind a second "show attributes" click.
    if (ptIsAmlFeatureStyle(options)) return true;

    return extraAttributeKeys.length <= 20;
  }

  function ptIsUicAquiferExemptionStyle(options) {
    options = options || {};
    var method = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var id = ptCleanText(options.catalogExtId || options.external_layer_id).toUpperCase();
    return method.indexOf('uic_') === 0 || id.indexOf('UIC_') === 0;
  }

  function ptIsUicReferencePointStyle(options) {
    options = options || {};
    var method = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var id = ptCleanText(
      options.catalogExtId || options.external_layer_id
    ).toUpperCase();
    return method.indexOf('reference_point') >= 0 ||
      id === 'UIC_EPA_REFERENCE_POINTS';
  }

  function ptSetUicPreflightError(options, message) {
    if (!ptIsUicAquiferExemptionStyle(options) ||
        !window.BRIM || !window.BRIM.uicExplorer ||
        !window.BRIM.uicExplorer.setExternalError) {
      return;
    }
    window.BRIM.uicExplorer.setExternalError(
      options.catalogExtId || options.external_layer_id,
      message
    );
  }

  function ptUicSourceKey(options, props) {
    options = options || {};
    props = props || {};
    var method = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var id = ptCleanText(options.catalogExtId).toLowerCase();
    if (method.indexOf('epa') >= 0 || id.indexOf('epa') >= 0 ||
        ptHasValue(ptFieldValue(props, 'ID_1'))) return 'epa';
    if (method.indexOf('post') >= 0 || id.indexOf('post') >= 0 ||
        ptHasValue(ptFieldValue(props, 'Field_Labe'))) return 'calgem_post';
    return 'calgem_primacy';
  }

  function ptUicPopupRow(label, value) {
    value = ptCleanText(value);
    if (!value) return '';
    return '<tr><th style="text-align:left;vertical-align:top;padding:2px 8px 2px 0;white-space:nowrap;">' +
      ptEscapeHtml(label) + '</th><td style="vertical-align:top;padding:2px 0;">' +
      ptEscapeHtml(value) + '</td></tr>';
  }

  function ptUicFormatDate(value) {
    if (!ptHasValue(value)) return '';
    var numeric = Number(value);
    var parsed = null;
    if (isFinite(numeric) && Math.abs(numeric) > 10000000000) {
      parsed = new Date(numeric);
    } else {
      var textValue = ptCleanText(value);
      if (/^\d{4}-\d{2}-\d{2}/.test(textValue)) return textValue.slice(0, 10);
      var timestamp = Date.parse(textValue);
      if (!isNaN(timestamp)) parsed = new Date(timestamp);
    }
    return parsed && !isNaN(parsed.getTime()) ?
      parsed.toISOString().slice(0, 10) :
      ptCleanText(value);
  }

  function ptUicLink(url, label) {
    url = ptCleanText(url);
    if (!/^(https?|ftp):\/\//i.test(url)) return '';
    var legacy = /^ftp:\/\//i.test(url) ? ' (legacy FTP; unverified)' : '';
    return '<a href="' + ptEscapeHtml(url) + '" target="_blank" rel="noopener">' +
      ptEscapeHtml(label + legacy) + '</a>';
  }

  function ptUicPopupFromProperties(props, options) {
    props = props || {};
    options = options || {};
    var sourceKey = ptUicSourceKey(options, props);
    var isEpa = sourceKey === 'epa';
    var isPost = sourceKey === 'calgem_post';
    var isHistoric = sourceKey === 'calgem_primacy';
    var isReferencePoint = ptIsUicReferencePointStyle(options);
    var sourceFamily = isEpa ? 'EPA' : 'CalGEM';
    var sourceId = isEpa ?
      ptUicFirstValue(
        props,
        isReferencePoint ? ['ID', 'OBJECTID'] : ['ID_1', 'OBJECTID']
      ) :
      (isPost ?
        ptUicFirstValue(props, ['ID', 'OBJECTID']) :
        ('CalGEM_PRIMACY_' + ptUicFirstValue(props, ['OBJECTID_1', 'OBJECTID'])));
    var field = isEpa ?
      ptFieldValue(props, 'Injection_Well_ID') :
      (isPost ?
        ptUicFirstValue(props, ['Field_Labe', 'Name']) :
        ptFieldValue(props, 'FieldName'));
    var county = isHistoric ? ptFieldValue(props, 'AreaName') : ptFieldValue(props, 'County');
    var formation = isPost ? ptFieldValue(props, 'Formation') : '';
    var zone = isEpa ? ptFieldValue(props, 'Injection_Zone') :
      (isPost ?
        ptUicFirstValue(props, ['Zone', 'Inj_Zone', 'Zone_Label']) :
        '');
    var formationZones = [];
    if (isHistoric) {
      for (var i = 1; i <= 18; i++) {
        var value = ptCleanText(ptFieldValue(props, 'FormZone' + i));
        if (value) formationZones.push(value);
      }
      zone = formationZones.join('; ');
    }
    var decision = isHistoric ? '' : ptUicFormatDate(
      isEpa ? ptFieldValue(props, 'Decision_Date') : ptFieldValue(props, 'Approve')
    );
    var documentUrl = isPost ? ptFieldValue(props, 'Documentat') : '';
    var rows = '';
    rows += ptUicPopupRow('Source record', sourceId);
    rows += ptUicPopupRow('Field / project', field);
    rows += ptUicPopupRow(isHistoric ? 'Area' : 'County', county);
    rows += ptUicPopupRow('Formation', formation);
    rows += ptUicPopupRow(isHistoric ? 'Formation / zone list' : 'Zone / member', zone);
    rows += ptUicPopupRow('Decision / approval date', decision);
    rows += ptUicPopupRow('Well class', ptFieldValue(props, 'Well_Class'));
    rows += ptUicPopupRow(
      'Injection activity',
      ptFieldValue(props, isEpa ? 'Injection_Activity' : 'Injectate')
    );
    rows += ptUicPopupRow('Exemption criterion', ptFieldValue(props, 'Exemption_'));
    rows += ptUicPopupRow('Pool', ptFieldValue(props, 'Pool'));
    rows += ptUicPopupRow('Reported depth', [
      ptUicFirstValue(props, ['DepthMin_Z', 'Depth']),
      ptFieldValue(props, 'DepthMax_Z'),
      ptFieldValue(props, 'Depth_Units')
    ].map(ptCleanText).filter(Boolean).join(' – '));
    rows += ptUicPopupRow('USDW values (as reported)', [
      ptFieldValue(props, 'USDW_MinDe'), ptFieldValue(props, 'USDW_MaxDe')
    ].map(ptCleanText).filter(Boolean).join(' – '));
    rows += ptUicPopupRow('TDS values (as reported)', [
      ptFieldValue(props, 'TDS_Min'), ptFieldValue(props, 'TDS_Max')
    ].map(ptCleanText).filter(Boolean).join(' – '));
    rows += ptUicPopupRow('Boron values (as reported)', [
      ptFieldValue(props, 'Boron_Min'), ptFieldValue(props, 'Boron_Max')
    ].map(ptCleanText).filter(Boolean).join(' – '));
    rows += ptUicPopupRow('Reported acreage', [
      ptFieldValue(
        props, isHistoric ? 'AcresTable' : (isPost ? 'Acreage' : 'AE_Area')
      ),
      isEpa ? ptFieldValue(props, 'AE_Area_Units') : ''
    ].map(ptCleanText).filter(Boolean).join(' '));
    rows += ptUicPopupRow('Calculated acreage', ptFieldValue(props, 'AcresCalc'));
    rows += ptUicPopupRow('Data quality', ptFieldValue(
      props, 'Data_Quality_Category'
    ));
    rows += ptUicPopupRow('Source volume', ptFieldValue(props, 'Doc_Source'));
    rows += ptUicPopupRow('Source comments', ptFieldValue(props, 'Comments'));
    rows += ptUicPopupRow('RMS error', ptFieldValue(props, 'RMS_Error'));
    rows += ptUicPopupRow('GIS comments', ptFieldValue(props, 'GIS_Comments'));
    var links = [
      ptUicLink(documentUrl, 'Open source documentation'),
      ptUicLink(options.sourcePage, 'Source information'),
      isEpa ? ptUicLink(
        'https://www.epa.gov/uic/california-uic-program-oversight-arods',
        'EPA California decisions'
      ) : ''
    ].filter(Boolean).join(' · ');
    var warning = 'Aquifer-exemption polygons show mapped surface footprints only. ' +
      'Exemptions may be limited to particular formations, zones, depths, elevations, ' +
      'or structural boundaries. A surface intersection does not establish that all ' +
      'underlying groundwater is exempt. Review the applicable EPA Record of Decision ' +
      'and supporting documents for project-level interpretation.';
    if (isHistoric) {
      warning += ' This CalGEM layer is a partial historic shaded subset and is not a ' +
        'complete map of all 1983 primacy exemptions.';
    }
    if (isReferencePoint) {
      warning = 'This symbol is an EPA exemption-area centroid/reference locator. ' +
        'It is not a well, injection well, or additional exemption area. Use it ' +
        'for overview and record location only; review the mapped boundary and ' +
        'applicable EPA Record of Decision for spatial interpretation.';
    }
    return '<div class="pt-uic-popup">' +
      '<div class="pt-uic-popup-badge">' + ptEscapeHtml(sourceFamily) +
      (isReferencePoint ? ' · Centroid / locator' : ' · Authoritative Live') +
      ' · retrieved ' +
      ptEscapeHtml(ptCleanText(options.retrievalUtc) || 'this session') + '</div>' +
      '<div class="pt-uic-popup-title">' +
      ptEscapeHtml(ptCleanText(field) || ptCleanText(sourceId) || 'UIC record') +
      '</div><table>' + rows + '</table>' +
      '<div class="pt-uic-popup-warning"><strong>Interpretation warning:</strong> ' +
      ptEscapeHtml(warning) + '</div>' +
      (links ? '<div class="pt-uic-popup-links">' + links + '</div>' : '') +
      '</div>';
  }

  function ptUicTooltipFromProperties(props, options) {
    props = props || {};
    var sourceKey = ptUicSourceKey(options, props);
    var field = sourceKey === 'epa' ?
      ptFieldValue(props, 'Injection_Well_ID') :
      (sourceKey === 'calgem_post' ?
        ptUicFirstValue(props, ['Field_Labe', 'Name']) :
        ptFieldValue(props, 'FieldName'));
    var zone = sourceKey === 'epa' ?
      ptFieldValue(props, 'Injection_Zone') :
      (sourceKey === 'calgem_post' ?
        ptUicFirstValue(props, ['Formation', 'Zone']) :
        ptUicFirstValue(props, ['AreaName', 'FormZone1']));
    var isReferencePoint = ptIsUicReferencePointStyle(options);
    var id = sourceKey === 'epa' ?
      ptFieldValue(props, isReferencePoint ? 'ID' : 'ID_1') :
      (sourceKey === 'calgem_post' ? ptFieldValue(props, 'ID') : ptFieldValue(props, 'OBJECTID_1'));
    return '<div class="pt-external-hover"><b>' +
      ptEscapeHtml(ptCleanText(field) || ptCleanText(id) || 'UIC record') +
      '</b>' + (ptCleanText(zone) ? '<div>' + ptEscapeHtml(zone) + '</div>' : '') +
      '<div>' + ptEscapeHtml(
        isReferencePoint ? 'EPA · centroid / locator' :
          (sourceKey === 'epa' ? 'EPA · Live' : 'CalGEM · Live')
      ) +
      '</div></div>';
  }

  function ptPopupFromProperties(props, options) {
    props = props || {};
    options = options || {};

    if (ptIsUicAquiferExemptionStyle(options)) {
      return ptUicPopupFromProperties(props, options);
    }

    if (ptIsAlertCameraStyle(options) && !ptIsAlertCameraViewshedStyle(options)) {
      return ptAlertCameraPopupHtml([props], options);
    }

    var layerName = options.layerName || 'External GIS feature';
    var popupFields = ptSplitFieldList(options.popupFields);
    var aliasMap = ptAliasMapFromString(options.popupAliases);
    var curatedRows = ptPropertiesForFields(props, popupFields, options);
    var linkHtml = ptPopupLinkHtml(props, options);
    var streamGaugeNoteHtml = ptStreamGaugeClassificationNoteHtml(props, options);
    var wcrNoteHtml = ptWcrInterpretationNoteHtml(props, options);
    var mlrsNoteHtml = ptMlrsInterpretationNoteHtml(props, options);
    var airNowNoteHtml = ptAirNowUsefulLinksHtml(props, options);
    var alertCameraImageHtml = ptAlertCameraPopupImageHtml(props, options);
    var amlSourceNoteHtml = ptAmlSourceNoteHtml(props, options);

    var html = '<b>' + ptEscapeHtml(layerName) + '</b>';

    if (curatedRows.length > 0) {
      html += '<div style="margin-top:4px;"><b>BRIM summary</b></div>';

      curatedRows.forEach(function(row) {
        html += '<br/><b>' +
          ptDisplayFieldLabelHtmlForProps(row.field, aliasMap, options, props) +
          ':</b> ' +
          ptEscapeHtml(ptFormatFieldValue(row.field, row.value, props, options));
      });

      if (linkHtml) {
        html += linkHtml;
      }

      if (streamGaugeNoteHtml) {
        html += streamGaugeNoteHtml;
      }

      if (wcrNoteHtml) {
        html += wcrNoteHtml;
      }

      if (mlrsNoteHtml) {
        html += mlrsNoteHtml;
      }

      if (airNowNoteHtml) {
        html += airNowNoteHtml;
      }

      if (alertCameraImageHtml) {
        html += alertCameraImageHtml;
      }

      if (amlSourceNoteHtml) {
        html += amlSourceNoteHtml;
      }

      html += '<br/><span style="font-size:11px;color:#666;"><i>' +
        'Summary labels are BRIM-friendly aliases for native service fields unless explicitly marked as BRIM-derived.' +
        '</i></span>';

      var extraAttributeKeys = ptExternalAttributeKeys(props, options, popupFields);

      if (extraAttributeKeys.length > 0) {
        if (ptShowExternalExtraAttributesInline(extraAttributeKeys, options)) {
          html += '<div style="margin-top:8px;font-size:11px;color:#555;"><b>Additional returned attributes</b></div>' +
            ptExternalAttributeRowsHtml(props, options, extraAttributeKeys);
        } else {
          html +=
            '<details style="margin-top:8px;">' +
              '<summary style="cursor:pointer;color:#006c9c;">Show additional returned attributes (' + extraAttributeKeys.length + ')</summary>' +
              ptExternalAttributeRowsHtml(props, options, extraAttributeKeys) +
            '</details>';
        }
      }

      return html;
    }

    html += '<div style="margin-top:4px;font-size:11px;color:#555;">Returned service attributes</div>';
    html += ptExternalAttributeRowsHtml(props, options);
    if (linkHtml) {
      html += linkHtml;
    }
    if (streamGaugeNoteHtml) {
      html += streamGaugeNoteHtml;
    }
    if (wcrNoteHtml) {
      html += wcrNoteHtml;
    }
    if (mlrsNoteHtml) {
      html += mlrsNoteHtml;
    }
    if (airNowNoteHtml) {
      html += airNowNoteHtml;
    }
    if (alertCameraImageHtml) {
      html += alertCameraImageHtml;
    }
    if (amlSourceNoteHtml) {
      html += amlSourceNoteHtml;
    }
    html += '<div style="font-size:11px;color:#555;margin-top:3px;">' +
      'External overlays are exploratory agency-service layers. Curated popup fields can be added later for a cleaner BRIM summary.' +
      '</div>';

    return html;
  }


  function ptMlrsCaseTitle(props) {
    props = props || {};

    var name = ptCleanText(ptFieldValue(props, 'CSE_NAME'));
    var nr = ptCleanText(ptFieldValue(props, 'CSE_NR'));
    var serial = ptCleanText(ptFieldValue(props, 'SERIAL_NR'));

    if (name && nr) return name + ' — ' + nr;
    if (name) return name;
    if (nr) return nr;
    if (serial) return serial;

    return 'MLRS record';
  }

  function ptMlrsShortCaseLabel(props, idx) {
    props = props || {};

    var nr = ptCleanText(ptFieldValue(props, 'CSE_NR'));
    var name = ptCleanText(ptFieldValue(props, 'CSE_NAME'));

    var label = nr || name || ('Record ' + String(idx + 1));
    if (label.length > 30) label = label.slice(0, 27) + '...';

    return String(idx + 1) + '. ' + label;
  }

  function ptInstallMlrsPopupHandlers() {
    if (window.ptMlrsPopupHandlersInstalled) return;
    window.ptMlrsPopupHandlersInstalled = true;

    document.addEventListener('click', function(e) {
      var btn = e.target && e.target.closest ? e.target.closest('.pt-mlrs-popup-tab') : null;
      if (!btn) return;

      e.preventDefault();
      e.stopPropagation();

      var popupId = btn.getAttribute('data-pt-mlrs-popup-id') || '';
      var idx = Number(btn.getAttribute('data-pt-mlrs-record-idx'));
      if (!popupId || !isFinite(idx)) return;

      var root = document.getElementById(popupId);
      if (!root) return;

      var tabs = root.querySelectorAll('.pt-mlrs-popup-tab');
      Array.prototype.forEach.call(tabs, function(tab) {
        var active = Number(tab.getAttribute('data-pt-mlrs-record-idx')) === idx;
        tab.classList.toggle('pt-mlrs-popup-tab-active', active);
      });

      var panels = root.querySelectorAll('.pt-mlrs-popup-record-panel');
      Array.prototype.forEach.call(panels, function(panel) {
        var active = Number(panel.getAttribute('data-pt-mlrs-record-idx')) === idx;
        panel.style.display = active ? 'block' : 'none';
      });
    }, true);
  }

  function ptMlrsRecordSortKey(hit) {
    hit = hit || {};
    var props = hit.props || {};
    return [
      ptCleanText(ptFieldValue(props, 'CSE_NAME')).toLowerCase(),
      ptCleanText(ptFieldValue(props, 'CSE_NR')).toLowerCase(),
      ptCleanText(hit.layerName).toLowerCase()
    ].join('|');
  }

  function ptMlrsHitLayerContainsLatLng(layer, latlng) {
    if (!layer || !latlng) return false;

    var point = null;

    try {
      point = map.latLngToLayerPoint(latlng);
    } catch (e) {
      point = null;
    }

    if (point && typeof layer._containsPoint === 'function') {
      try {
        if (layer._containsPoint(point)) return true;
      } catch (e) {}
    }

    // CircleMarker/Marker fallback. Most MLRS layers are polygons, but this
    // keeps the stacked-record popup usable if a future MLRS service returns
    // point features or centroids.
    if (typeof layer.getLatLng === 'function' && point) {
      try {
        var ll = layer.getLatLng();
        var p2 = map.latLngToLayerPoint(ll);
        var tol = typeof layer.getRadius === 'function' ? Math.max(8, Number(layer.getRadius()) + 5) : 12;
        if (p2 && p2.distanceTo && p2.distanceTo(point) <= tol) return true;
      } catch (e) {}
    }

    return false;
  }

  function ptCollectMlrsHitsAtLatLng(latlng, fallbackProps, fallbackOptions) {
    var hits = [];
    var seen = {};

    function addHit(layer, rec) {
      if (!layer || !layer.feature || !layer.feature.properties) return;

      var props = layer.feature.properties || {};
      var key = [
        ptCleanText(ptFieldValue(props, 'CSE_NR')),
        ptCleanText(ptFieldValue(props, 'CSE_NAME')),
        ptCleanText(ptFieldValue(props, 'CSE_DISP')),
        ptCleanText(rec && rec.name),
        L.Util && L.Util.stamp ? L.Util.stamp(layer) : String(Math.random())
      ].join('|');

      if (seen[key]) return;
      seen[key] = true;

      var hitOptions = Object.assign({}, rec || fallbackOptions || {});
      if (!ptCleanText(hitOptions.layerName)) {
        hitOptions.layerName = rec && rec.name ? rec.name : ptCleanText(fallbackOptions && (fallbackOptions.layerName || fallbackOptions.name));
      }
      if (!ptCleanText(hitOptions.name) && hitOptions.layerName) {
        hitOptions.name = hitOptions.layerName;
      }

      hits.push({
        props: props,
        options: hitOptions,
        layerName: hitOptions.layerName || ''
      });
    }

    function visit(layer, rec) {
      if (!layer) return;

      if (layer.feature && layer.feature.properties) {
        if (ptMlrsHitLayerContainsLatLng(layer, latlng)) {
          addHit(layer, rec);
        }
        return;
      }

      if (typeof layer.eachLayer === 'function') {
        layer.eachLayer(function(child) {
          visit(child, rec);
        });
      }
    }

    ptCustomLayers.forEach(function(rec) {
      if (!rec || !rec.layer || !ptIsMlrsMineralCaseStyle(rec)) return;
      if (!map.hasLayer(rec.layer)) return;
      visit(rec.layer, rec);
    });

    if (hits.length === 0 && fallbackProps) {
      var fallbackHitOptions = Object.assign({}, fallbackOptions || {});
      if (!ptCleanText(fallbackHitOptions.layerName)) {
        fallbackHitOptions.layerName = ptCleanText(fallbackOptions && (fallbackOptions.layerName || fallbackOptions.name));
      }
      if (!ptCleanText(fallbackHitOptions.name) && fallbackHitOptions.layerName) {
        fallbackHitOptions.name = fallbackHitOptions.layerName;
      }

      hits.push({
        props: fallbackProps,
        options: fallbackHitOptions,
        layerName: fallbackHitOptions.layerName || ''
      });
    }

    hits.sort(function(a, b) {
      return ptMlrsRecordSortKey(a).localeCompare(ptMlrsRecordSortKey(b));
    });

    return hits;
  }

  function ptMlrsAggregatePopupHtml(hits) {
    hits = Array.isArray(hits) ? hits : [];
    ptInstallMlrsPopupHandlers();

    var total = hits.length;
    var maxShown = 25;
    var shownHits = hits.slice(0, maxShown);
    var popupId = 'pt-mlrs-popup-' + String(Date.now()) + '-' + String(Math.random()).slice(2);

    if (total <= 1) {
      var only = shownHits[0] || { props: {}, options: {} };
      return '<div id="' + ptEscapeHtml(popupId) + '" class="pt-mlrs-popup-stack">' +
        ptPopupFromProperties(only.props || {}, only.options || {}) +
        '</div>';
    }

    var html = '<div id="' + ptEscapeHtml(popupId) + '" class="pt-mlrs-popup-stack">';
    html += '<div class="pt-mlrs-stack-header"><b>' + total.toLocaleString() + ' MLRS records at clicked location</b></div>';
    html += '<div class="pt-mlrs-stack-subnote">Darker map patches often reflect overlapping PLSS/legal-description-derived case geometries. Use the buttons below to inspect each returned record.</div>';

    if (total > maxShown) {
      html += '<div class="pt-mlrs-stack-limit-note">Showing the first ' + maxShown + ' records, sorted by case name/serial. Zoom in or add an SQL filter for a smaller result set.</div>';
    }

    html += '<div class="pt-mlrs-popup-tabs" role="tablist">';
    shownHits.forEach(function(hit, idx) {
      html += '<button type="button" class="pt-mlrs-popup-tab' + (idx === 0 ? ' pt-mlrs-popup-tab-active' : '') + '" ' +
        'data-pt-mlrs-popup-id="' + ptEscapeHtml(popupId) + '" ' +
        'data-pt-mlrs-record-idx="' + idx + '">' +
        ptEscapeHtml(ptMlrsShortCaseLabel(hit.props || {}, idx)) +
        '</button>';
    });
    html += '</div>';

    shownHits.forEach(function(hit, idx) {
      html += '<div class="pt-mlrs-popup-record-panel" data-pt-mlrs-record-idx="' + idx + '" style="display:' + (idx === 0 ? 'block' : 'none') + ';">';
      html += '<div class="pt-mlrs-record-count-note">Record ' + String(idx + 1) + ' of ' + total.toLocaleString() + '</div>';
      html += ptPopupFromProperties(hit.props || {}, hit.options || {});
      html += '</div>';
    });

    html += '</div>';
    return html;
  }

  function ptOpenMlrsAggregatePopup(latlng, fallbackProps, fallbackOptions) {
    var hits = ptCollectMlrsHitsAtLatLng(latlng, fallbackProps, fallbackOptions);
    var html = ptMlrsAggregatePopupHtml(hits);

    L.popup({
      maxWidth: 640,
      minWidth: 340,
      className: 'pt-mlrs-leaflet-popup pt-mlrs-stack-leaflet-popup'
    })
      .setLatLng(latlng)
      .setContent(html)
      .openOn(map);
  }

  function ptTooltipFromProperties(props, options) {
    props = props || {};
    options = options || {};

    if (ptIsUicAquiferExemptionStyle(options)) {
      return ptUicTooltipFromProperties(props, options);
    }

    var hoverFields = ptSplitFieldList(options.hoverFields);
    var hoverAliasMap = ptAliasMapFromString(options.hoverAliases || options.popupAliases);
    var boldFields = ptSplitFieldList(options.hoverBoldFields);
    var noLabelFields = ptSplitFieldList(options.hoverNoLabelFields);
    var roundFields = ptSplitFieldList(options.hoverRoundFields);

    // Tooltips default to friendly labels only. Native field names can still
    // be shown in popups for technical review.
    var hoverLabelOptions = {
      showNativeFieldNames: ptCleanText(options.hoverShowNativeFieldNames).toLowerCase() === 'true' ? 'true' : 'false',
      defaultStyleField: options.defaultStyleField,
      defaultStyleMethod: options.defaultStyleMethod,
      layerName: options.layerName,
      styleLegendTitle: options.styleLegendTitle,
      styleUnits: options.styleUnits
    };

    if (hoverFields.length === 0 && ptCleanText(options.defaultLabelField)) {
      hoverFields = [ptCleanText(options.defaultLabelField)];
    }

    var rows = ptPropertiesForFields(props, hoverFields, options);

    if (rows.length === 0) {
      return '';
    }

    var html = '<div class="pt-external-hover">';

    rows.forEach(function(row, idx) {
      var displayValue = ptFormatFieldValue(row.field, row.value, props, options);

      if (roundFields.indexOf(row.field) >= 0) {
        var n = Number(row.value);

        if (isFinite(n)) {
          displayValue = String(Math.round(n));
        }
      }

      var noLabel = noLabelFields.indexOf(row.field) >= 0 || idx === 0;
      var isBold = boldFields.indexOf(row.field) >= 0 || idx === 0;

      if (noLabel) {
        html += '<div>' +
          (isBold ? '<b>' : '') +
          ptEscapeHtml(displayValue) +
          (isBold ? '</b>' : '') +
          '</div>';
      } else {
        html += '<div><span>' +
          ptDisplayFieldLabelHtmlForProps(row.field, hoverAliasMap, hoverLabelOptions, props) +
          ':</span> ' +
          (isBold ? '<b>' : '') +
          ptEscapeHtml(displayValue) +
          (isBold ? '</b>' : '') +
          '</div>';
      }
    });

    html += '</div>';
    return html;
  }

  function ptOutFieldsArray(options) {
    options = options || {};

    /*
     * External overlays are exploratory by design.  Returning an empty field
     * list tells the FeatureServer query/layer code not to restrict attributes
     * to curated popup/hover/style fields.  This gives popups access to all
     * attributes returned by the service, while curated popup_fields can still
     * be used as a clean BRIM summary at the top of the popup.
     */
    return [];
  }

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

  ptEnsurePane('pane_pt_blm_sma_context', 285, 'auto');
  ptEnsurePane('pane_pt_custom_polygon', 500, 'auto');
  ptEnsurePane('pane_pt_custom_line', 545, 'auto');
  ptEnsurePane('pane_pt_custom_point', 575, 'auto');
  ptEnsurePane('pane_pt_measure', 900, 'auto');
  ptEnsurePane('pane_pt_teaching_markup', 925, 'none');
  ptEnsurePane('pane_pt_teaching_labels', 935, 'auto');

  function ptEnsureEsriLeaflet(callback) {

    if (window.L && L.esri) {
      callback(true);
      return;
    }

    if (window.pt2EsriLeafletLoading) {
      window.pt2EsriLeafletCallbacks = window.pt2EsriLeafletCallbacks || [];
      window.pt2EsriLeafletCallbacks.push(callback);
      return;
    }

    window.pt2EsriLeafletLoading = true;
    window.pt2EsriLeafletCallbacks = [callback];

    var script = document.createElement('script');
    script.src = PT2_ESRI_LEAFLET_URL;
    script.async = true;

    script.onload = function() {
      window.pt2EsriLeafletLoading = false;
      var ok = !!(window.L && L.esri);
      var callbacks = window.pt2EsriLeafletCallbacks || [];
      window.pt2EsriLeafletCallbacks = [];

      callbacks.forEach(function(cb) {
        try { cb(ok); } catch (e) { console.error(e); }
      });
    };

    script.onerror = function() {
      window.pt2EsriLeafletLoading = false;
      var callbacks = window.pt2EsriLeafletCallbacks || [];
      window.pt2EsriLeafletCallbacks = [];

      callbacks.forEach(function(cb) {
        try { cb(false); } catch (e) { console.error(e); }
      });
    };

    document.head.appendChild(script);
  }

  // --------------------------------------------------------------------------
  // Measurement tools
  // --------------------------------------------------------------------------

  var ptMeasureLayer = L.layerGroup().addTo(map);
  var ptMeasureMode = null;
  var ptMeasurePoints = [];
  var ptMeasureShape = null;
  var ptMeasureTempShape = null;
  var ptMeasureInteractionActive = false;
  var ptMeasureSuspendedPanes = [];
  var ptMeasurePaneObserver = null;

  // --------------------------------------------------------------------------
  // Teaching Markup / Annotate tools
  // --------------------------------------------------------------------------

  var ptTeachingMarkupLayer = L.layerGroup().addTo(map);
  var ptTeachingLabelLayer = L.layerGroup().addTo(map);
  var ptTeachingMarkupActive = false;
  var ptTeachingMarkupDrawing = false;
  var ptTeachingMarkupPoints = [];
  var ptTeachingMarkupCurrentStroke = null;
  var ptTeachingMarkupStrokes = [];
  var ptTeachingMarkupHistory = [];
  var ptTeachingLabels = [];
  var ptTeachingToolMode = 'draw';
  var ptTeachingLabelPlacementActive = false;
  var ptTeachingLabelStyle = 'point_text';
  var ptTeachingLabelSize = 'small';
  var ptTeachingLabelSeq = 0;
  var ptTeachingMarkupSuppressClickUntil = 0;
  var ptTeachingMarkupColor = '#3388ff';
  var ptTeachingMarkupWeight = 9;
  var ptTeachingMarkupOpacity = 0.45;
  var ptTeachingMarkupTempPanActive = false;
  var ptTeachingMarkupSpacePanReady = false;
  var ptTeachingMarkupPanLastPoint = null;
  var ptTeachingMarkupPanPointerId = null;
  var ptTeachingMarkupCursorEl = null;

  function ptCloseTransientFeatureUi() {
    try { map.closePopup(); } catch (popupErr) {}

    var transientTooltips = [];
    try {
      map.eachLayer(function(layer) {
        if (
          window.L &&
          L.Tooltip &&
          layer instanceof L.Tooltip &&
          !(layer.options && layer.options.permanent)
        ) {
          transientTooltips.push(layer);
        }
      });
    } catch (tooltipScanErr) {}

    transientTooltips.forEach(function(tooltip) {
      try { map.closeTooltip(tooltip); } catch (tooltipCloseErr) {}
    });

    if (typeof ptHideVisualIdentifyTooltip === 'function') {
      try { ptHideVisualIdentifyTooltip(); } catch (identifyTooltipErr) {}
    }
  }

  function ptClearCurrentFeatureHover() {
    var container = map && map.getContainer ? map.getContainer() : null;
    if (!container || !container.querySelectorAll) return;

    var interactiveNodes = container.querySelectorAll('.leaflet-interactive');
    Array.prototype.forEach.call(interactiveNodes, function(node) {
      try {
        if (!node.matches || !node.matches(':hover')) return;
        node.dispatchEvent(new MouseEvent('mouseout', {
          bubbles: true,
          cancelable: true,
          relatedTarget: container
        }));
      } catch (hoverErr) {}
    });
  }

  function ptMeasurePaneIsExempt(pane) {
    if (!pane) return true;
    return pane === map.getPane('mapPane') ||
      pane === map.getPane('tilePane') ||
      pane === map.getPane('pane_pt_measure') ||
      (pane === map.getPane('pane_pt_teaching_labels') && ptTeachingLabelPlacementActive);
  }

  function ptSuspendMeasurePane(pane) {
    if (ptMeasurePaneIsExempt(pane)) return;
    if (pane.getAttribute('data-pt-measure-suspended') === 'true') return;

    ptMeasureSuspendedPanes.push({
      pane: pane,
      pointerEvents: pane.style.pointerEvents
    });
    pane.setAttribute('data-pt-measure-suspended', 'true');
    pane.classList.add('pt-measure-pane-suspended');
    pane.style.pointerEvents = 'none';
  }

  function ptSuspendFeaturePanes() {
    var panes = map && map.getPanes ? map.getPanes() : null;
    if (panes) {
      Object.keys(panes).forEach(function(name) {
        ptSuspendMeasurePane(panes[name]);
      });
    }

    if (!ptMeasurePaneObserver && window.MutationObserver) {
      var mapPane = map.getPane('mapPane');
      if (mapPane) {
        ptMeasurePaneObserver = new MutationObserver(function(records) {
          if (!ptMeasureInteractionActive) return;
          records.forEach(function(record) {
            Array.prototype.forEach.call(record.addedNodes || [], function(node) {
              if (node && node.nodeType === 1 && node.classList.contains('leaflet-pane')) {
                ptSuspendMeasurePane(node);
              }
            });
          });
        });
        ptMeasurePaneObserver.observe(mapPane, { childList: true });
      }
    }
  }

  function ptRestoreFeaturePanes() {
    ptMeasureSuspendedPanes.forEach(function(state) {
      if (!state || !state.pane) return;
      state.pane.style.pointerEvents = state.pointerEvents;
      state.pane.classList.remove('pt-measure-pane-suspended');
      state.pane.removeAttribute('data-pt-measure-suspended');
    });
    ptMeasureSuspendedPanes = [];
  }

  function ptSetMeasureInteractionActive(active) {
    active = !!active;
    var container = map && map.getContainer ? map.getContainer() : null;

    if (active === ptMeasureInteractionActive) {
      if (container) container.classList.toggle('pt-measure-active', active);
      return;
    }

    if (active) {
      ptCloseTransientFeatureUi();
      ptClearCurrentFeatureHover();
      ptMeasureInteractionActive = true;
      map._ptMeasureInteractionActive = true;
      ptSuspendFeaturePanes();
    } else {
      ptCloseTransientFeatureUi();
      ptRestoreFeaturePanes();
      ptMeasureInteractionActive = false;
      map._ptMeasureInteractionActive = false;
    }

    if (container) container.classList.toggle('pt-measure-active', active);
    if (map && typeof map.fire === 'function') {
      map.fire('pt:measureinteractionchange', {active: active});
    }
  }

  function ptTeachingSetStatus(msg, isError) {
    ptSetInlineNote('pt-teaching-markup-status', msg || '', !!isError);
  }

  function ptTeachingLabelText() {
    var input = document.getElementById('pt-teaching-label-text');
    return input ? ptCleanText(input.value) : '';
  }

  function ptTeachingLabelSizePx() {
    if (ptTeachingLabelSize === 'large') return 20;
    if (ptTeachingLabelSize === 'medium') return 16;
    return 13;
  }

  function ptTeachingLabelIcon(text) {
    var sizePx = ptTeachingLabelSizePx();
    var safeText = ptEscapeHtml(text);
    var safeColor = ptEscapeHtml(ptTeachingMarkupColor || '#3388ff');
    var withPoint = ptTeachingLabelStyle === 'point_text';
    var pointHtml = withPoint ?
      '<span class="pt-teaching-label-point" style="background-color:' + safeColor + ';border-color:' + safeColor + ';"></span>' : '';
    var html = pointHtml +
      '<span class="pt-teaching-label-text" style="color:' + safeColor + ';font-size:' + sizePx + 'px;">' +
        safeText +
      '</span>';

    return L.divIcon({
      className: 'pt-teaching-label-icon ' +
        (withPoint ? 'pt-teaching-label-point-text' : 'pt-teaching-label-text-only'),
      html: html,
      iconSize: [1, 1],
      iconAnchor: withPoint ? [5, 5] : [0, Math.round(sizePx * 0.72)]
    });
  }

  function ptTeachingPlaceLabel(latlng) {
    var text = ptTeachingLabelText();
    if (!text) {
      ptTeachingSetStatus('Enter label text before clicking the map.', true);
      return;
    }

    ptTeachingLabelSeq += 1;
    var marker = L.marker(latlng, {
      pane: 'pane_pt_teaching_labels',
      icon: ptTeachingLabelIcon(text),
      draggable: true,
      keyboard: true,
      riseOnHover: true,
      bubblingMouseEvents: false,
      title: text,
      alt: 'Map label: ' + text
    }).addTo(ptTeachingLabelLayer);

    var rec = {
      id: 'pt_teaching_label_' + ptTeachingLabelSeq,
      layer: marker,
      text: text,
      style: ptTeachingLabelStyle,
      size: ptTeachingLabelSize,
      color: ptTeachingMarkupColor
    };

    ptTeachingLabels.push(rec);
    ptTeachingMarkupHistory.push({type: 'label', layer: marker, record: rec});

    var iconEl = marker.getElement ? marker.getElement() : null;
    if (iconEl) {
      iconEl.setAttribute('role', 'img');
      iconEl.setAttribute('aria-label', 'Map label: ' + text + '. Drag to reposition.');
    }

    marker.on('dragstart', function() {
      ptCloseTransientFeatureUi();
    });

    ptTeachingSetStatus('Placed label “' + text + '”. Drag it to reposition.', false);

    var input = document.getElementById('pt-teaching-label-text');
    if (input) {
      try { input.focus({preventScroll: true}); } catch (focusErr) { input.focus(); }
      input.select();
    }
  }

  function ptTeachingSyncLabelControls() {
    var style0 = document.getElementById('pt-teaching-label-style');
    var size0 = document.getElementById('pt-teaching-label-size');
    var color0 = document.getElementById('pt-teaching-label-color');

    if (style0) style0.value = ptTeachingLabelStyle;
    if (size0) size0.value = ptTeachingLabelSize;
    if (color0) color0.value = ptTeachingMarkupColor;
  }

  function ptTeachingSyncToolMode() {
    var drawTab = document.getElementById('pt-teaching-mode-draw');
    var labelTab = document.getElementById('pt-teaching-mode-label');
    var drawPanel = document.getElementById('pt-teaching-draw-panel');
    var labelPanel = document.getElementById('pt-teaching-label-panel');
    var isDraw = ptTeachingToolMode === 'draw';

    if (drawTab) {
      drawTab.setAttribute('aria-selected', isDraw ? 'true' : 'false');
      drawTab.tabIndex = isDraw ? 0 : -1;
    }
    if (labelTab) {
      labelTab.setAttribute('aria-selected', isDraw ? 'false' : 'true');
      labelTab.tabIndex = isDraw ? -1 : 0;
    }
    if (drawPanel) drawPanel.hidden = !isDraw;
    if (labelPanel) labelPanel.hidden = isDraw;

    ptTeachingSyncLabelControls();
  }

  function ptSetTeachingLabelPlacementActive(active) {
    active = !!active;
    ptTeachingLabelPlacementActive = active;

    var container = map && map.getContainer ? map.getContainer() : null;
    if (container) container.classList.toggle('pt-teaching-label-active', active);

    if (active) {
      ptSetTeachingMarkupMode(false);
      ptMeasureMode = null;
      ptResetMeasureShapeOnly();
      ptUpdateMeasureButtons();
      if (ptMeasureInteractionActive) ptSetMeasureInteractionActive(false);
      ptSetMeasureInteractionActive(true);
      ptTeachingSetStatus('Enter text, then click the map to place the label.', false);
    } else if (!ptMeasureMode) {
      ptSetMeasureInteractionActive(false);
    }
  }

  function ptSetTeachingToolMode(mode) {
    mode = mode === 'label' ? 'label' : 'draw';
    ptTeachingToolMode = mode;

    if (mode === 'label') {
      ptSetTeachingLabelPlacementActive(true);
    } else {
      ptSetTeachingLabelPlacementActive(false);
      ptTeachingSetStatus('Draw mode ready. Turn On to draw.', false);
    }

    ptTeachingSyncToolMode();
  }

  function ptTeachingMarkupStyle() {
    return {
      pane: 'pane_pt_teaching_markup',
      color: ptTeachingMarkupColor,
      weight: ptTeachingMarkupWeight,
      opacity: ptTeachingMarkupOpacity,
      interactive: false,
      bubblingMouseEvents: false,
      lineCap: 'round',
      lineJoin: 'round',
      smoothFactor: 1.25
    };
  }

  function ptTeachingEnsureBrushCursor() {
    var container = map && map.getContainer ? map.getContainer() : null;
    if (!container) return null;

    if (!ptTeachingMarkupCursorEl) {
      ptTeachingMarkupCursorEl = document.createElement('div');
      ptTeachingMarkupCursorEl.id = 'pt-teaching-markup-brush-cursor';
      ptTeachingMarkupCursorEl.className = 'pt-teaching-markup-brush-cursor';
      ptTeachingMarkupCursorEl.setAttribute('aria-hidden', 'true');
      container.appendChild(ptTeachingMarkupCursorEl);
    }

    return ptTeachingMarkupCursorEl;
  }

  function ptTeachingUpdateBrushCursorStyle() {
    var cursor = ptTeachingEnsureBrushCursor();
    if (!cursor) return;

    var diameter = Math.max(3, Number(ptTeachingMarkupWeight) || 9);
    var previewOpacity = Math.max(0.18, Math.min(0.55, (Number(ptTeachingMarkupOpacity) || 0.45) * 0.80));

    cursor.style.width = diameter + 'px';
    cursor.style.height = diameter + 'px';
    cursor.style.borderColor = ptTeachingMarkupColor || '#3388ff';
    cursor.style.backgroundColor = ptTeachingMarkupColor || '#3388ff';
    cursor.style.opacity = previewOpacity;
    cursor.style.boxShadow = '0 0 0 1px rgba(0,0,0,0.35), 0 1px 4px rgba(0,0,0,0.25)';
  }

  function ptTeachingHideBrushCursor() {
    if (ptTeachingMarkupCursorEl) {
      ptTeachingMarkupCursorEl.style.display = 'none';
    }
  }

  function ptTeachingMoveBrushCursor(evt) {
    if (!ptTeachingMarkupActive || !evt || ptTeachingMarkupTempPanActive || ptTeachingMarkupSpacePanReady) {
      ptTeachingHideBrushCursor();
      return;
    }

    if (ptTeachingIsControlTarget(evt.target)) {
      ptTeachingHideBrushCursor();
      return;
    }

    var container = map && map.getContainer ? map.getContainer() : null;
    var cursor = ptTeachingEnsureBrushCursor();
    if (!container || !cursor || !container.getBoundingClientRect) return;

    var rect = container.getBoundingClientRect();
    var x = (evt.clientX || 0) - rect.left;
    var y = (evt.clientY || 0) - rect.top;

    if (x < 0 || y < 0 || x > rect.width || y > rect.height) {
      ptTeachingHideBrushCursor();
      return;
    }

    ptTeachingUpdateBrushCursorStyle();

    var diameter = Math.max(3, Number(ptTeachingMarkupWeight) || 9);
    cursor.style.display = 'block';
    cursor.style.transform = 'translate(' + Math.round(x - diameter / 2) + 'px, ' + Math.round(y - diameter / 2) + 'px)';
  }

  function ptTeachingIsControlTarget(target) {
    if (!target || !target.closest) return false;

    return !!target.closest(
      '.leaflet-control, .leaflet-popup, .leaflet-tooltip, ' +
      '.pt-measure-wrap, .pt-clear-all-wrap, .pt-tools-adddata-wrap, ' +
      '.pt-local-upload-wrap, .pt-teaching-markup-wrap, .pt-ops-live-panel, ' +
      '.pt-source-links-overlay'
    );
  }

  function ptTeachingIsTypingTarget(target) {
    if (!target) return false;
    var tag = target.tagName ? String(target.tagName).toLowerCase() : '';
    if (tag === 'input' || tag === 'textarea' || tag === 'select' || tag === 'button') return true;
    if (target.isContentEditable) return true;
    return !!(target.closest && target.closest('input, textarea, select, button, [contenteditable="true"]'));
  }

  function ptTeachingUpdatePanCursor() {
    var container = map && map.getContainer ? map.getContainer() : null;
    if (!container) return;

    container.classList.toggle('pt-teaching-markup-pan-ready', !!(ptTeachingMarkupActive && ptTeachingMarkupSpacePanReady && !ptTeachingMarkupTempPanActive));
    container.classList.toggle('pt-teaching-markup-panning', !!(ptTeachingMarkupActive && ptTeachingMarkupTempPanActive));

    if (ptTeachingMarkupSpacePanReady || ptTeachingMarkupTempPanActive || !ptTeachingMarkupActive) {
      ptTeachingHideBrushCursor();
    }
  }

  function ptTeachingCancelCurrentStroke() {
    if (!ptTeachingMarkupDrawing) return;

    if (ptTeachingMarkupCurrentStroke) {
      try { ptTeachingMarkupLayer.removeLayer(ptTeachingMarkupCurrentStroke); } catch (err) {}
    }

    ptTeachingMarkupDrawing = false;
    ptTeachingMarkupCurrentStroke = null;
    ptTeachingMarkupPoints = [];
  }

  function ptTeachingBeginTemporaryPan(evt) {
    if (!ptTeachingMarkupActive || !evt) return;
    if (ptTeachingIsControlTarget(evt.target)) return;

    ptTeachingCancelCurrentStroke();

    ptTeachingMarkupTempPanActive = true;
    ptTeachingMarkupPanPointerId = evt.pointerId !== undefined ? evt.pointerId : null;
    ptTeachingMarkupPanLastPoint = { x: evt.clientX || 0, y: evt.clientY || 0 };
    ptTeachingMarkupSuppressClickUntil = Date.now() + 650;
    ptTeachingUpdatePanCursor();

    var container = map && map.getContainer ? map.getContainer() : null;
    if (container && container.setPointerCapture && evt.pointerId !== undefined) {
      try { container.setPointerCapture(evt.pointerId); } catch (err) {}
    }

    if (evt.preventDefault) evt.preventDefault();
    if (evt.stopPropagation) evt.stopPropagation();
    if (evt.stopImmediatePropagation) evt.stopImmediatePropagation();
  }

  function ptTeachingTemporaryPanMove(evt) {
    if (!ptTeachingMarkupActive || !ptTeachingMarkupTempPanActive || !ptTeachingMarkupPanLastPoint) return;
    if (ptTeachingMarkupPanPointerId !== null && evt.pointerId !== undefined && evt.pointerId !== ptTeachingMarkupPanPointerId) return;

    var x = evt.clientX || 0;
    var y = evt.clientY || 0;
    var dx = x - ptTeachingMarkupPanLastPoint.x;
    var dy = y - ptTeachingMarkupPanLastPoint.y;
    ptTeachingMarkupPanLastPoint = { x: x, y: y };

    if ((dx || dy) && map && map.panBy) {
      map.panBy([-dx, -dy], { animate: false });
    }

    if (evt.preventDefault) evt.preventDefault();
    if (evt.stopPropagation) evt.stopPropagation();
    if (evt.stopImmediatePropagation) evt.stopImmediatePropagation();
  }

  function ptTeachingEndTemporaryPan(evt) {
    if (!ptTeachingMarkupTempPanActive) return false;
    if (evt && ptTeachingMarkupPanPointerId !== null && evt.pointerId !== undefined && evt.pointerId !== ptTeachingMarkupPanPointerId) return false;

    var container = map && map.getContainer ? map.getContainer() : null;
    if (container && container.releasePointerCapture && evt && evt.pointerId !== undefined) {
      try { container.releasePointerCapture(evt.pointerId); } catch (err) {}
    }

    ptTeachingMarkupTempPanActive = false;
    ptTeachingMarkupPanLastPoint = null;
    ptTeachingMarkupPanPointerId = null;
    ptTeachingMarkupSuppressClickUntil = Date.now() + 650;
    ptTeachingUpdatePanCursor();

    if (evt && evt.preventDefault) evt.preventDefault();
    if (evt && evt.stopPropagation) evt.stopPropagation();
    if (evt && evt.stopImmediatePropagation) evt.stopImmediatePropagation();
    return true;
  }

  function ptTeachingKeyDown(evt) {
    if (!ptTeachingMarkupActive || !evt) return;
    if (!(evt.code === 'Space' || evt.key === ' ' || evt.key === 'Spacebar')) return;
    if (ptTeachingIsTypingTarget(evt.target)) return;

    ptTeachingMarkupSpacePanReady = true;
    ptTeachingUpdatePanCursor();

    if (evt.preventDefault) evt.preventDefault();
    if (evt.stopPropagation) evt.stopPropagation();
  }

  function ptTeachingKeyUp(evt) {
    if (!(evt && (evt.code === 'Space' || evt.key === ' ' || evt.key === 'Spacebar'))) return;

    ptTeachingMarkupSpacePanReady = false;
    ptTeachingEndTemporaryPan(evt);
    ptTeachingUpdatePanCursor();

    if (ptTeachingMarkupActive && evt.preventDefault) evt.preventDefault();
    if (ptTeachingMarkupActive && evt.stopPropagation) evt.stopPropagation();
  }

  function ptTeachingSyncControl() {
    var wrap0 = document.getElementById('pt-teaching-markup-wrap');
    var toggle0 = document.getElementById('pt-teaching-markup-toggle');
    var color0 = document.getElementById('pt-teaching-markup-color');
    var size0 = document.getElementById('pt-teaching-markup-size');
    var opacity0 = document.getElementById('pt-teaching-markup-opacity');

    if (wrap0) wrap0.classList.toggle('pt-teaching-markup-on', ptTeachingMarkupActive);
    if (toggle0) toggle0.checked = ptTeachingMarkupActive;
    if (color0) color0.value = ptTeachingMarkupColor;
    if (size0) size0.value = String(ptTeachingMarkupWeight);
    if (opacity0) opacity0.value = String(ptTeachingMarkupOpacity);
    ptTeachingSyncLabelControls();
  }

  function ptSetTeachingMarkupMode(active) {
    active = !!active;
    if (active && ptTeachingLabelPlacementActive) {
      ptSetTeachingLabelPlacementActive(false);
    }
    ptTeachingMarkupActive = active;
    map._ptDrawInteractionActive = active;
    ptTeachingMarkupDrawing = false;
    ptTeachingMarkupPoints = [];
    ptTeachingMarkupCurrentStroke = null;
    ptTeachingMarkupTempPanActive = false;
    ptTeachingMarkupSpacePanReady = false;
    ptTeachingMarkupPanLastPoint = null;
    ptTeachingMarkupPanPointerId = null;

    var container = map && map.getContainer ? map.getContainer() : null;
    if (container) {
      container.classList.toggle('pt-teaching-markup-active', active);
      container.classList.remove('pt-teaching-markup-pan-ready');
      container.classList.remove('pt-teaching-markup-panning');
    }

    if (active) {
      // Keep the tool modal: teaching markup and measurement should not both
      // be active at the same time.
      ptMeasureMode = null;
      ptResetMeasureShapeOnly();
      ptUpdateMeasureButtons();
      ptSetMeasureInteractionActive(false);

      if (map.dragging && map.dragging.disable) map.dragging.disable();
      if (map.doubleClickZoom && map.doubleClickZoom.disable) map.doubleClickZoom.disable();
      ptTeachingUpdateBrushCursorStyle();
      ptTeachingSetStatus('Draw mode on.', false);
    } else {
      if (map.dragging && map.dragging.enable) map.dragging.enable();
      if (map.doubleClickZoom && map.doubleClickZoom.enable) map.doubleClickZoom.enable();
      ptTeachingHideBrushCursor();
      ptTeachingSetStatus('Draw mode off.', false);
    }

    ptTeachingSyncControl();
  }

  function ptTeachingUpdateStyleFromControls() {
    var color0 = document.getElementById('pt-teaching-markup-color');
    var size0 = document.getElementById('pt-teaching-markup-size');
    var opacity0 = document.getElementById('pt-teaching-markup-opacity');

    if (color0 && color0.value) ptTeachingMarkupColor = color0.value;
    if (size0 && size0.value) ptTeachingMarkupWeight = Number(size0.value) || 9;
    if (opacity0 && opacity0.value) ptTeachingMarkupOpacity = Number(opacity0.value) || 0.45;

    ptTeachingUpdateBrushCursorStyle();
    ptTeachingSyncControl();
  }

  function ptTeachingSetSharedColor(color) {
    if (color) ptTeachingMarkupColor = color;
    ptTeachingUpdateBrushCursorStyle();
    ptTeachingSyncControl();
  }

  function ptTeachingUpdateLabelControls() {
    var style0 = document.getElementById('pt-teaching-label-style');
    var size0 = document.getElementById('pt-teaching-label-size');
    var color0 = document.getElementById('pt-teaching-label-color');

    if (style0 && style0.value) ptTeachingLabelStyle = style0.value;
    if (size0 && size0.value) ptTeachingLabelSize = size0.value;
    if (color0 && color0.value) ptTeachingSetSharedColor(color0.value);
    ptTeachingSyncLabelControls();
  }

  function ptTeachingFinishStroke() {
    if (!ptTeachingMarkupDrawing) return;

    ptTeachingMarkupDrawing = false;

    if (ptTeachingMarkupCurrentStroke && ptTeachingMarkupPoints.length >= 2) {
      ptTeachingMarkupStrokes.push(ptTeachingMarkupCurrentStroke);
      ptTeachingMarkupHistory.push({type: 'stroke', layer: ptTeachingMarkupCurrentStroke});
      ptTeachingSetStatus('Stroke added. Use Undo or Clear as needed.', false);
    } else if (ptTeachingMarkupCurrentStroke) {
      try { ptTeachingMarkupLayer.removeLayer(ptTeachingMarkupCurrentStroke); } catch (err) {}
    }

    ptTeachingMarkupCurrentStroke = null;
    ptTeachingMarkupPoints = [];
  }

  function ptTeachingUndoLastStroke() {
    var last = ptTeachingMarkupHistory.pop();
    if (last && last.layer) {
      if (last.type === 'label') {
        try { ptTeachingLabelLayer.removeLayer(last.layer); } catch (labelErr) {}
        ptTeachingLabels = ptTeachingLabels.filter(function(rec) {
          return rec.layer !== last.layer;
        });
        ptTeachingSetStatus('Removed last map label.', false);
      } else {
        try { ptTeachingMarkupLayer.removeLayer(last.layer); } catch (strokeErr) {}
        ptTeachingMarkupStrokes = ptTeachingMarkupStrokes.filter(function(layer) {
          return layer !== last.layer;
        });
        ptTeachingSetStatus('Removed last markup stroke.', false);
      }
    } else {
      ptTeachingSetStatus('No drawings or labels to undo.', false);
    }
  }

  function ptTeachingClearMarkup() {
    ptTeachingMarkupLayer.clearLayers();
    ptTeachingLabelLayer.clearLayers();
    ptTeachingMarkupCurrentStroke = null;
    ptTeachingMarkupPoints = [];
    ptTeachingMarkupStrokes = [];
    ptTeachingMarkupHistory = [];
    ptTeachingLabels = [];
    ptTeachingSetStatus('Drawings and labels cleared.', false);
  }

  function ptTeachingPointerLatLng(evt) {
    if (!map || !map.mouseEventToLatLng) return null;
    try {
      return map.mouseEventToLatLng(evt);
    } catch (err) {
      return null;
    }
  }

  function ptTeachingPointerDown(evt) {
    if (!ptTeachingMarkupActive) return;
    ptTeachingMoveBrushCursor(evt);
    if (ptTeachingIsControlTarget(evt.target)) return;

    var button = evt.button !== undefined ? evt.button : 0;
    var isMiddleButton = button === 1 || (evt.buttons !== undefined && (evt.buttons & 4));
    var isSpaceLeftPan = ptTeachingMarkupSpacePanReady && button === 0;

    if (isMiddleButton || isSpaceLeftPan) {
      ptTeachingBeginTemporaryPan(evt);
      return;
    }

    if (button !== 0) return;

    var latlng = ptTeachingPointerLatLng(evt);
    if (!latlng) return;

    evt.preventDefault();
    evt.stopPropagation();
    if (evt.stopImmediatePropagation) evt.stopImmediatePropagation();

    ptTeachingMarkupDrawing = true;
    ptTeachingMarkupPoints = [latlng];
    ptTeachingMarkupCurrentStroke = L.polyline(ptTeachingMarkupPoints, ptTeachingMarkupStyle()).addTo(ptTeachingMarkupLayer);
    ptTeachingMarkupSuppressClickUntil = Date.now() + 650;
  }

  function ptTeachingPointerMove(evt) {
    if (ptTeachingMarkupActive) ptTeachingMoveBrushCursor(evt);

    if (ptTeachingMarkupTempPanActive) {
      if (evt.preventDefault) evt.preventDefault();
      if (evt.stopPropagation) evt.stopPropagation();
      if (evt.stopImmediatePropagation) evt.stopImmediatePropagation();
      return;
    }

    if (!ptTeachingMarkupActive || !ptTeachingMarkupDrawing) return;

    var latlng = ptTeachingPointerLatLng(evt);
    if (!latlng) return;

    evt.preventDefault();
    evt.stopPropagation();
    if (evt.stopImmediatePropagation) evt.stopImmediatePropagation();

    var last = ptTeachingMarkupPoints[ptTeachingMarkupPoints.length - 1];
    if (last) {
      var p1 = map.latLngToLayerPoint(last);
      var p2 = map.latLngToLayerPoint(latlng);
      if (p1 && p2 && p1.distanceTo && p1.distanceTo(p2) < 3) return;
    }

    ptTeachingMarkupPoints.push(latlng);
    if (ptTeachingMarkupCurrentStroke) {
      ptTeachingMarkupCurrentStroke.setLatLngs(ptTeachingMarkupPoints);
    }
  }

  function ptTeachingPointerUp(evt) {
    if (ptTeachingEndTemporaryPan(evt)) return;
    if (!ptTeachingMarkupActive || !ptTeachingMarkupDrawing) return;

    evt.preventDefault();
    evt.stopPropagation();
    if (evt.stopImmediatePropagation) evt.stopImmediatePropagation();

    ptTeachingMarkupSuppressClickUntil = Date.now() + 650;
    ptTeachingFinishStroke();
  }

  function ptTeachingSuppressMapClick(evt) {
    if (!ptTeachingMarkupActive && Date.now() > ptTeachingMarkupSuppressClickUntil) return;
    if (ptTeachingIsControlTarget(evt.target)) return;

    evt.preventDefault();
    evt.stopPropagation();
    if (evt.stopImmediatePropagation) evt.stopImmediatePropagation();
  }

  function ptInstallTeachingMarkupPointerHandlers() {
    var container = map && map.getContainer ? map.getContainer() : null;
    if (!container || container._ptTeachingMarkupHandlersInstalled) return;

    container._ptTeachingMarkupHandlersInstalled = true;
    container.addEventListener('pointerdown', ptTeachingPointerDown, true);
    container.addEventListener('pointermove', ptTeachingPointerMove, true);
    container.addEventListener('pointerleave', ptTeachingHideBrushCursor, true);
    window.addEventListener('pointermove', ptTeachingTemporaryPanMove, true);
    window.addEventListener('pointerup', ptTeachingPointerUp, true);
    window.addEventListener('keydown', ptTeachingKeyDown, true);
    window.addEventListener('keyup', ptTeachingKeyUp, true);
    container.addEventListener('click', ptTeachingSuppressMapClick, true);
    container.addEventListener('dblclick', ptTeachingSuppressMapClick, true);
    container.addEventListener('auxclick', ptTeachingSuppressMapClick, true);
    container.addEventListener('contextmenu', ptTeachingSuppressMapClick, true);
  }

  function ptResetMeasureShapeOnly() {
    ptMeasurePoints = [];
    ptMeasureShape = null;
    ptMeasureTempShape = null;
  }

  function ptClearMeasurements() {
    ptMeasureLayer.clearLayers();
    ptMeasureMode = null;
    ptResetMeasureShapeOnly();
    ptSetMeasureStatus('Measurements cleared.', false);
    ptUpdateMeasureButtons();
    if (!ptTeachingLabelPlacementActive) {
      ptSetMeasureInteractionActive(false);
    }
  }

  function ptDistanceMeters(points) {
    var total = 0;

    for (var i = 1; i < points.length; i++) {
      total += map.distance(points[i - 1], points[i]);
    }

    return total;
  }

  function ptFormatDistance(meters) {
    if (!isFinite(meters)) return '0 ft';

    var feet = meters * 3.280839895;
    var miles = feet / 5280;

    if (miles >= 0.25) {
      return miles.toFixed(2) + ' mi';
    }

    return Math.round(feet).toLocaleString() + ' ft';
  }

  function ptGeodesicArea(latLngs) {
    // Approximate geodesic polygon area adapted from common Leaflet Draw logic.
    // Good enough for screening-level map measurement, not survey work.
    var pointsCount = latLngs.length;
    var area = 0.0;
    var d2r = Math.PI / 180;
    var earthRadius = 6378137.0;

    if (pointsCount < 3) {
      return 0;
    }

    for (var i = 0; i < pointsCount; i++) {
      var p1 = latLngs[i];
      var p2 = latLngs[(i + 1) % pointsCount];

      area += ((p2.lng - p1.lng) * d2r) *
        (2 + Math.sin(p1.lat * d2r) + Math.sin(p2.lat * d2r));
    }

    area = area * earthRadius * earthRadius / 2.0;
    return Math.abs(area);
  }

  function ptFormatArea(squareMeters) {
    if (!isFinite(squareMeters)) return '0 acres';

    var acres = squareMeters * 0.000247105381;
    var sqmi = acres / 640;

    if (sqmi >= 1) {
      return sqmi.toFixed(2) + ' sq mi';
    }

    return acres.toFixed(1) + ' acres';
  }

  function ptUpdateMeasureButtons() {
    var distBtn = document.getElementById('pt-measure-distance-btn');
    var areaBtn = document.getElementById('pt-measure-area-btn');
    var finishBtn = document.getElementById('pt-measure-finish-btn');

    if (distBtn) distBtn.classList.toggle('pt-tools-active-btn', ptMeasureMode === 'distance');
    if (areaBtn) areaBtn.classList.toggle('pt-tools-active-btn', ptMeasureMode === 'area');
    if (finishBtn) finishBtn.style.display = ptMeasureMode ? 'inline-block' : 'none';
  }

  function ptStartMeasure(mode) {
    ptSetTeachingLabelPlacementActive(false);
    ptSetTeachingMarkupMode(false);
    ptMeasureMode = mode;
    ptResetMeasureShapeOnly();
    ptUpdateMeasureButtons();
    ptSetMeasureInteractionActive(true);

    if (mode === 'distance') {
      ptSetMeasureStatus('Distance mode: click map points. Click Finish to stop.', false);
    } else {
      ptSetMeasureStatus('Area mode: click polygon vertices. Click Finish to close polygon.', false);
    }
  }

  function ptFinishMeasure() {

    if (!ptMeasureMode) {
      return;
    }

    var resultShape = null;
    var resultPopup = '';

    if (ptMeasureMode === 'area' && ptMeasurePoints.length >= 3) {
      var area = ptGeodesicArea(ptMeasurePoints);

      if (ptMeasureShape) {
        resultShape = ptMeasureShape;
        resultPopup = '<b>Measured area:</b> ' + ptFormatArea(area) +
          '<br/><span style="font-size:11px;color:#555;">Screening-level estimate.</span>';
      }

      ptSetMeasureStatus('Area: ' + ptFormatArea(area), false);
    } else if (ptMeasureMode === 'distance' && ptMeasurePoints.length >= 2) {
      var dist = ptDistanceMeters(ptMeasurePoints);

      if (ptMeasureShape) {
        resultShape = ptMeasureShape;
        resultPopup = '<b>Measured distance:</b> ' + ptFormatDistance(dist) +
          '<br/><span style="font-size:11px;color:#555;">Screening-level estimate.</span>';
      }

      ptSetMeasureStatus('Distance: ' + ptFormatDistance(dist), false);
    } else {
      ptSetMeasureStatus('Measurement stopped.', false);
    }

    ptMeasureMode = null;
    ptResetMeasureShapeOnly();
    ptUpdateMeasureButtons();
    ptSetMeasureInteractionActive(false);

    if (resultShape && resultPopup) {
      resultShape.bindPopup(resultPopup).openPopup();
    }
  }

  function ptDrawMeasure() {
    if (!ptMeasureMode) return;

    if (ptMeasureShape) {
      ptMeasureLayer.removeLayer(ptMeasureShape);
      ptMeasureShape = null;
    }

    if (ptMeasurePoints.length === 0) return;

    if (ptMeasureMode === 'distance') {
      ptMeasureShape = L.polyline(ptMeasurePoints, {
        pane: 'pane_pt_measure',
        color: '#222222',
        weight: 3,
        dashArray: '6,4',
        interactive: true
      }).addTo(ptMeasureLayer);

      var dist = ptDistanceMeters(ptMeasurePoints);
      ptSetMeasureStatus('Distance: ' + ptFormatDistance(dist), false);

    } else if (ptMeasureMode === 'area') {
      var style = {
        pane: 'pane_pt_measure',
        color: '#222222',
        weight: 2.5,
        fillColor: '#FEE08B',
        fillOpacity: 0.20,
        dashArray: '6,4',
        interactive: true
      };

      if (ptMeasurePoints.length >= 3) {
        ptMeasureShape = L.polygon(ptMeasurePoints, style).addTo(ptMeasureLayer);
        ptSetStatus('Area: ' + ptFormatArea(ptGeodesicArea(ptMeasurePoints)), false);
      } else {
        ptMeasureShape = L.polyline(ptMeasurePoints, style).addTo(ptMeasureLayer);
        ptSetMeasureStatus('Area mode: add at least 3 points.', false);
      }
    }

    var last = ptMeasurePoints[ptMeasurePoints.length - 1];
    L.circleMarker(last, {
      pane: 'pane_pt_measure',
      radius: 3,
      color: '#222222',
      weight: 1,
      fillColor: '#FFFFFF',
      fillOpacity: 1,
      interactive: false
    }).addTo(ptMeasureLayer);
  }

  function ptCaptureMeasureClick(evt) {
    if (!ptMeasureMode && !ptTeachingLabelPlacementActive) return;
    if (
      ptTeachingLabelPlacementActive &&
      evt.target && evt.target.closest &&
      evt.target.closest('.pt-teaching-label-icon')
    ) return;
    if (ptTeachingIsControlTarget(evt.target)) return;

    evt.preventDefault();
    evt.stopPropagation();
    if (evt.stopImmediatePropagation) evt.stopImmediatePropagation();

    if (map.dragging && map.dragging.moved && map.dragging.moved()) return;

    var latlng = null;
    try { latlng = map.mouseEventToLatLng(evt); } catch (latlngErr) {}
    if (!latlng) return;

    if (ptTeachingLabelPlacementActive) {
      ptTeachingPlaceLabel(latlng);
      return;
    }

    ptMeasurePoints.push(latlng);
    ptDrawMeasure();
  }

  function ptInstallMeasureClickHandler() {
    var container = map && map.getContainer ? map.getContainer() : null;
    if (!container || container._ptMeasureClickHandlerInstalled) return;
    container._ptMeasureClickHandlerInstalled = true;
    container.addEventListener('click', ptCaptureMeasureClick, true);
  }

  ptInstallMeasureClickHandler();

  // --------------------------------------------------------------------------
  // Built-in BLM CA SMA context overlay
  // --------------------------------------------------------------------------

  var ptBlmSmaLayer = null;

  function ptSetBlmSmaOpacity() {
    var slider = document.getElementById('pt-blm-sma-opacity');
    var opacity = slider ? Number(slider.value) : 0.58;

    if (ptBlmSmaLayer && ptBlmSmaLayer.setOpacity) {
      ptBlmSmaLayer.setOpacity(opacity);
    }
  }

  function ptClearBlmSma(silent) {
    var wasOn = !!(ptBlmSmaLayer && map.hasLayer(ptBlmSmaLayer));

    if (ptBlmSmaLayer && map.hasLayer(ptBlmSmaLayer)) {
      map.removeLayer(ptBlmSmaLayer);
    }

    var cb = document.getElementById('pt-blm-sma-toggle');
    if (cb) cb.checked = false;

    if (!silent && wasOn) {
      ptSetStatus('BLM land status / SMA overlay off.', false);
    }

    return wasOn;
  }

  function ptToggleBlmSma(checked) {

    if (!PT2_ENABLE_BLM_SMA) {
      ptSetStatus('BLM SMA overlay is disabled in map configuration.', true);
      return;
    }

    if (!checked) {
      ptClearBlmSma(false);
      return;
    }

    ptEnsureEsriLeaflet(function(ok) {

      if (!ok || !L.esri || !L.esri.dynamicMapLayer) {
        ptSetStatus(
          'Could not load Esri Leaflet. BLM SMA overlay requires browser access to the Esri Leaflet library.',
          true
        );
        var cb = document.getElementById('pt-blm-sma-toggle');
        if (cb) cb.checked = false;
        return;
      }

      // If the user clicked Clear external / clear all while Esri Leaflet was
      // still loading, do not let the late async callback re-add the SMA layer.
      var cbAfterLoad = document.getElementById('pt-blm-sma-toggle');
      if (!cbAfterLoad || !cbAfterLoad.checked) {
        return;
      }

      if (!ptBlmSmaLayer) {
        var slider = document.getElementById('pt-blm-sma-opacity');
        var opacity = slider ? Number(slider.value) : 0.58;

        ptBlmSmaLayer = L.esri.dynamicMapLayer({
          url: PT2_BLM_SMA_URL,
          layers: [0],
          opacity: opacity,
          pane: 'pane_pt_blm_sma_context'
        });

        ptBlmSmaLayer.on('requesterror', function(e) {
          ptSetStatus('BLM SMA overlay request failed. The service may be blocked or temporarily unavailable.', true);
          console.warn('PT2 BLM SMA request error:', e);
        });
      }

      if (!map.hasLayer(ptBlmSmaLayer)) {
        ptBlmSmaLayer.addTo(map);
      }

      ptSetStatus('BLM CA land status / SMA overlay on.', false);
    });
  }

  // --------------------------------------------------------------------------
  // External custom GIS layers
  // --------------------------------------------------------------------------

  var ptCustomLayers = [];
  var ptCustomLayerSeq = 0;
  var ptLastManualCurrentViewLayerId = null;

  // Keys for external-catalog layers that were launched from Ops Live.  This
  // lets Ops clear/remove only the promoted layer it owns without disturbing
  // ordinary user-added External Layers.  A cancel flag also prevents late
  // async current-view query responses from appearing after the Ops checkbox
  // has already been turned off.
  var ptOpsPromotedCancelled = {};
  var ptOpsPromotedGeneration = {};

  function ptIsOpsPromotedRecord(rec) {
    return !!(rec && ptCleanText(rec.opsPromotedKey));
  }

  function ptExternalPanelLayerRecords() {
    return ptCustomLayers.filter(function(rec) {
      return !ptIsOpsPromotedRecord(rec);
    });
  }

  function ptExternalPanelLayerCount() {
    return ptExternalPanelLayerRecords().length;
  }

  var ptWcrCompletedDepthMapLegendClosed = false;

  function ptEnsureWcrCompletedDepthMapLegend() {
    var mapContainer = map && map.getContainer ? map.getContainer() : null;
    if (!mapContainer) return null;

    var div = mapContainer.querySelector('.pt-wcr-completed-depth-map-legend');
    if (div) return div;

    div = document.createElement('div');
    div.className = 'pt-wcr-completed-depth-map-legend leaflet-control';
    div.style.position = 'absolute';
    div.style.top = '10px';
    div.style.left = '228px';
    div.style.zIndex = '10020';
    div.style.background = 'rgba(235, 249, 248, 0.92)';
    div.style.border = '1px solid rgba(90, 120, 122, 0.72)';
    div.style.borderRadius = '7px';
    div.style.boxShadow = '0 2px 8px rgba(0,0,0,0.22)';
    div.style.padding = '9px 11px';
    div.style.font = '12px Arial, sans-serif';
    div.style.color = '#202b2e';
    div.style.width = '370px';
    div.style.boxSizing = 'border-box';
    div.style.display = 'none';

    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);
    mapContainer.appendChild(div);
    return div;
  }

  function ptUpdateWcrCompletedDepthMapLegend() {
    var div = ptEnsureWcrCompletedDepthMapLegend();
    if (!div) return;

    var active = ptExternalPanelLayerRecords().some(function(rec) {
      return rec && ptIsWcrCompletedDepthStyle(rec) && rec.visible !== false && rec.layer && map.hasLayer(rec.layer);
    });

    if (!active) {
      ptWcrCompletedDepthMapLegendClosed = false;
      if (div.__brimDetachableState && div.__brimDetachableState.destroy) {
        div.__brimDetachableState.destroy(true);
      }
      div.style.display = 'none';
      div.innerHTML = '';
      return;
    }

    if (ptWcrCompletedDepthMapLegendClosed) {
      div.style.display = 'none';
      return;
    }

    div.innerHTML = '<div class="pt-wcr-completed-depth-map-legend-head pt-map-card-handle"><span style="font-weight:700;">Well Completion Reports | completed depth</span><span class="pt-map-card-actions"><button type="button" class="pt-map-card-dock pt-wcr-completed-depth-map-legend-dock" aria-label="Undock WCR completed-depth legend" title="Undock WCR completed-depth legend">&#x2197;</button><button type="button" class="pt-map-legend-close pt-wcr-completed-depth-map-legend-close" aria-label="Hide WCR completed-depth legend" title="Hide WCR completed-depth legend">&times;</button></span></div>' + ptWcrCompletedDepthMapLegendHtml();
    ptWireMapLegendCloseButton(div, '.pt-wcr-completed-depth-map-legend-close', function() {
      ptWcrCompletedDepthMapLegendClosed = true;
    });
    if (window.BRIM && window.BRIM.legendCloseout && window.BRIM.legendCloseout.makeDetachable) {
      window.BRIM.legendCloseout.makeDetachable({card: div, map: map, handleSelector: '.pt-wcr-completed-depth-map-legend-head', dockSelector: '.pt-wcr-completed-depth-map-legend-dock', label: 'WCR completed-depth legend'});
    }
    div.style.display = 'block';
  }


  var ptMlrsMineralCasesMapLegendClosed = false;
  var ptSgmaPrioritizationMapLegendClosed = false;
  var ptSubsidenceObservationMapLegendClosed = false;

  function ptWireMapLegendCloseButton(div, selector, onClose) {
    if (!div) return;
    var btn = div.querySelector(selector);
    if (!btn || btn.__ptMapLegendCloseWired) return;

    btn.__ptMapLegendCloseWired = true;
    btn.addEventListener('click', function(e) {
      if (e && e.preventDefault) e.preventDefault();
      if (e && e.stopPropagation) e.stopPropagation();
      onClose();
      div.style.display = 'none';
    });
  }

  function ptMapLegendCloseButtonHtml(extraClass, label) {
    return '<button type="button" class="pt-map-legend-close ' + ptEscapeHtml(extraClass) + '" aria-label="' + ptEscapeHtml(label || 'Close legend') + '" title="Close legend">×</button>';
  }

  function ptEnsureMlrsMineralCasesMapLegend() {
    var mapContainer = map && map.getContainer ? map.getContainer() : null;
    if (!mapContainer) return null;

    var div = mapContainer.querySelector('.pt-mlrs-mineral-cases-map-legend');
    if (div) return div;

    div = document.createElement('div');
    div.className = 'pt-mlrs-mineral-cases-map-legend leaflet-control';
    div.style.position = 'absolute';
    div.style.top = '10px';
    div.style.left = '92px';
    div.style.zIndex = '10021';
    div.style.background = 'rgba(235, 249, 248, 0.92)';
    div.style.border = '1px solid rgba(90, 120, 122, 0.72)';
    div.style.borderRadius = '7px';
    div.style.boxShadow = '0 2px 8px rgba(0,0,0,0.22)';
    div.style.padding = '9px 28px 11px 11px';
    div.style.font = '12px Arial, sans-serif';
    div.style.color = '#202b2e';
    div.style.width = '420px';
    div.style.boxSizing = 'border-box';
    div.style.display = 'none';

    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);
    mapContainer.appendChild(div);
    return div;
  }

  function ptUpdateMlrsMineralCasesMapLegend() {
    var div = ptEnsureMlrsMineralCasesMapLegend();
    if (!div) return;

    var active = ptExternalPanelLayerRecords().some(function(rec) {
      return rec && ptIsMlrsMineralCaseStyle(rec) && rec.visible !== false && rec.layer && map.hasLayer(rec.layer);
    });

    if (!active) {
      ptMlrsMineralCasesMapLegendClosed = false;
      div.style.display = 'none';
      div.innerHTML = '';
      return;
    }

    var wcrLegend = map.getContainer().querySelector('.pt-wcr-completed-depth-map-legend');
    if (wcrLegend && wcrLegend.style.display !== 'none' && wcrLegend.innerHTML) {
      div.style.top = '142px';
    } else {
      div.style.top = '10px';
    }

    if (ptMlrsMineralCasesMapLegendClosed) {
      div.style.display = 'none';
      return;
    }

    div.innerHTML =
      ptMapLegendCloseButtonHtml('pt-mlrs-map-legend-close', 'Close MLRS legend') +
      ptMlrsMineralCasesMapLegendHtml();
    ptWireMapLegendCloseButton(div, '.pt-mlrs-map-legend-close', function() {
      ptMlrsMineralCasesMapLegendClosed = true;
    });
    div.style.display = 'block';
  }


  function ptIsSgmaPrioritizationLayer(options) {
    options = options || {};

    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();
    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var styleLegendTitle = ptCleanText(options.styleLegendTitle).toLowerCase();
    var url = ptCleanText(options.url || options.serviceUrl || options.legendUrl).toLowerCase();

    return styleMethod.indexOf('sgma_basin_prioritization') >= 0 ||
      layerName.indexOf('sgma 2019 basin prioritization') >= 0 ||
      styleLegendTitle.indexOf('sgma 2019 basin prioritization') >= 0 ||
      url.indexOf('sgma_2019_basin_prioritization') >= 0;
  }

  function ptSgmaPrioritizationLegendHtml() {
    var items = [
      ['High', '#ff0000'],
      ['Medium', '#ffff00'],
      ['Low', '#55ff00'],
      ['Very Low', '#1472e6']
    ];

    var html = '<div class="pt-tools-muted"><b>DWR SGMA 2019 basin prioritization:</b></div>';
    html += '<div class="pt-sgma-legend-row">';
    html += items.map(function(item) {
      return '<span class="pt-sgma-legend-item"><span class="pt-sgma-legend-swatch" style="background:' + item[1] + ';"></span>' + ptEscapeHtml(item[0]) + '</span>';
    }).join('');
    html += '</div>';
    html += '<div class="pt-tools-muted pt-sgma-legend-note">Legend copied from the DWR source service for screening context.</div>';
    return html;
  }

  function ptEnsureSgmaPrioritizationMapLegend() {
    var mapContainer = map && map.getContainer ? map.getContainer() : null;
    if (!mapContainer) return null;

    var div = mapContainer.querySelector('.pt-sgma-prioritization-map-legend');
    if (div) return div;

    div = document.createElement('div');
    div.className = 'pt-sgma-prioritization-map-legend leaflet-control';
    div.style.position = 'absolute';
    div.style.top = '10px';
    div.style.left = '92px';
    div.style.zIndex = '10022';
    div.style.background = 'rgba(235, 249, 248, 0.92)';
    div.style.border = '1px solid rgba(90, 120, 122, 0.72)';
    div.style.borderRadius = '7px';
    div.style.boxShadow = '0 2px 8px rgba(0,0,0,0.22)';
    div.style.padding = '8px 28px 10px 10px';
    div.style.font = '12px Arial, sans-serif';
    div.style.color = '#202b2e';
    div.style.width = '390px';
    div.style.boxSizing = 'border-box';
    div.style.display = 'none';

    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);
    mapContainer.appendChild(div);
    return div;
  }

  function ptUpdateSgmaPrioritizationMapLegend() {
    var div = ptEnsureSgmaPrioritizationMapLegend();
    if (!div) return;

    var active = ptExternalPanelLayerRecords().some(function(rec) {
      return rec && ptIsSgmaPrioritizationLayer(rec) && rec.visible !== false && rec.layer && map.hasLayer(rec.layer);
    });

    if (!active) {
      ptSgmaPrioritizationMapLegendClosed = false;
      div.style.display = 'none';
      div.innerHTML = '';
      return;
    }

    if (ptSgmaPrioritizationMapLegendClosed) {
      div.style.display = 'none';
      return;
    }

    div.innerHTML =
      ptMapLegendCloseButtonHtml('pt-sgma-map-legend-close', 'Close SGMA prioritization legend') +
      '<div style="font-weight:700;margin-bottom:4px;">SGMA 2019 Basin Prioritization</div>' +
      ptSgmaPrioritizationLegendHtml();
    ptWireMapLegendCloseButton(div, '.pt-sgma-map-legend-close', function() {
      ptSgmaPrioritizationMapLegendClosed = true;
    });
    div.style.display = 'block';
  }


  function ptEnsureSubsidenceObservationMapLegend() {
    var mapContainer = map && map.getContainer ? map.getContainer() : null;
    if (!mapContainer) return null;

    var div = mapContainer.querySelector('.pt-subsidence-observation-map-legend');
    if (div) return div;

    div = document.createElement('div');
    div.className = 'pt-subsidence-observation-map-legend leaflet-control';
    div.style.position = 'absolute';
    div.style.top = '10px';
    div.style.left = '92px';
    div.style.zIndex = '10023';
    div.style.background = 'rgba(235, 249, 248, 0.93)';
    div.style.border = '1px solid rgba(90, 120, 122, 0.72)';
    div.style.borderRadius = '7px';
    div.style.boxShadow = '0 2px 8px rgba(0,0,0,0.22)';
    div.style.padding = '8px 28px 10px 10px';
    div.style.font = '12px Arial, sans-serif';
    div.style.color = '#202b2e';
    div.style.width = '405px';
    div.style.boxSizing = 'border-box';
    div.style.display = 'none';

    L.DomEvent.disableClickPropagation(div);
    L.DomEvent.disableScrollPropagation(div);
    mapContainer.appendChild(div);
    return div;
  }

  function ptUpdateSubsidenceObservationMapLegend() {
    var div = ptEnsureSubsidenceObservationMapLegend();
    if (!div) return;

    var active = ptExternalPanelLayerRecords().some(function(rec) {
      return rec && ptIsSubsidenceObservationStyle(rec) && rec.visible !== false && rec.layer && map.hasLayer(rec.layer);
    });

    if (!active) {
      ptSubsidenceObservationMapLegendClosed = false;
      div.style.display = 'none';
      div.innerHTML = '';
      return;
    }

    // Keep this compact legend from colliding with other External map legends
    // when a user has several custom styled External layers active.
    var offsetTop = 10;
    ['.pt-wcr-completed-depth-map-legend', '.pt-mlrs-mineral-cases-map-legend', '.pt-sgma-prioritization-map-legend'].forEach(function(sel) {
      var other = map.getContainer().querySelector(sel);
      if (other && other.style.display !== 'none' && other.innerHTML) {
        offsetTop += Math.max(0, other.offsetHeight || 0) + 8;
      }
    });
    div.style.top = offsetTop + 'px';

    if (ptSubsidenceObservationMapLegendClosed) {
      div.style.display = 'none';
      return;
    }

    div.innerHTML =
      ptMapLegendCloseButtonHtml('pt-subsidence-observation-map-legend-close', 'Close subsidence observation legend') +
      ptSubsidenceObservationMapLegendHtml(ptSubsidenceObservationActiveClassCounts());
    ptWireMapLegendCloseButton(div, '.pt-subsidence-observation-map-legend-close', function() {
      ptSubsidenceObservationMapLegendClosed = true;
    });
    div.style.display = 'block';
  }


  // Visual MapServer overlays preserve official agency cartography by drawing
  // server-rendered images/tiles.  For selected curated rows (CPC outlooks,
  // U.S. Drought Monitor), PT2 also performs a lightweight identify/query on
  // hover or click so users can read the category/probability without switching
  // to a current-view feature snapshot.
  var ptVisualIdentifyTooltip = null;
  var ptVisualIdentifyHoverTimer = null;
  var ptVisualIdentifySeq = 0;
  var PT2_VISUAL_IDENTIFY_HOVER_DELAY_MS = 420;

  function ptStyleForColor(color, fillOpacity) {
    return {
      pane: 'pane_pt_custom_polygon',
      color: color,
      weight: 2,
      opacity: 0.95,
      fillColor: color,
      fillOpacity: fillOpacity === undefined ? 0.16 : fillOpacity
    };
  }

  function ptDroughtMonitorColor(value) {
    var dm = Number(value);

    // Official USDM current FeatureServer renderer colors as of the NOAA /
    // NDMC / USDA ArcGIS service metadata.  These are kept local so the
    // drought layer does not depend on fragile tiled rendering or server-side
    // identify calls just to show D0-D4 classes consistently.
    if (dm === 0) return '#ffff00'; // D0 Abnormally Dry
    if (dm === 1) return '#fcd37f'; // D1 Moderate Drought
    if (dm === 2) return '#ffaa00'; // D2 Severe Drought
    if (dm === 3) return '#e60000'; // D3 Extreme Drought
    if (dm === 4) return '#730000'; // D4 Exceptional Drought

    return '#777777';
  }

  function ptIsDroughtMonitorStyle(options) {
    options = options || {};

    var styleField = ptCleanText(options.defaultStyleField).toLowerCase();
    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var layerName = ptCleanText(options.layerName).toLowerCase();

    return styleField === 'dm' ||
      styleMethod.indexOf('usdm') >= 0 ||
      layerName.indexOf('drought monitor') >= 0;
  }

  function ptDroughtMonitorDmCode(propsOrValue) {
    var value = propsOrValue;

    if (propsOrValue && typeof propsOrValue === 'object') {
      value = ptFieldValue(propsOrValue, 'DM');
    }

    var dm = Number(value);
    return isFinite(dm) ? dm : null;
  }

  function ptDroughtMonitorPaneName(dm) {
    dm = Number(dm);

    if (!isFinite(dm) || dm < 0 || dm > 4) {
      return 'pane_pt_usdm_other';
    }

    return 'pane_pt_usdm_dm' + String(Math.round(dm));
  }

  function ptEnsureDroughtMonitorPanes() {
    // USDM polygons can be cumulative/nested. Dedicated panes make draw order
    // deterministic even if the browser, renderer, or service response does
    // not preserve feature order exactly. D4 must always sit above D3, etc.
    ptEnsurePane('pane_pt_usdm_other', 500, 'auto');
    ptEnsurePane('pane_pt_usdm_dm0', 501, 'auto');
    ptEnsurePane('pane_pt_usdm_dm1', 502, 'auto');
    ptEnsurePane('pane_pt_usdm_dm2', 503, 'auto');
    ptEnsurePane('pane_pt_usdm_dm3', 504, 'auto');
    ptEnsurePane('pane_pt_usdm_dm4', 505, 'auto');
  }


  function ptCpcOutlookIntensityRank(props) {
    props = props || {};

    var prob = Number(ptFieldValue(props, 'prob'));
    if (!isFinite(prob)) prob = Number(ptFieldValue(props, 'PROB'));
    if (!isFinite(prob)) prob = 33;

    if (prob >= 90) return 7;
    if (prob >= 80) return 6;
    if (prob >= 70) return 5;
    if (prob >= 60) return 4;
    if (prob >= 50) return 3;
    if (prob >= 40) return 2;
    if (prob >= 33) return 1;
    return 0;
  }

  function ptCpcOutlookPaneName(rank) {
    rank = Number(rank);
    if (!isFinite(rank)) rank = 0;
    rank = Math.max(0, Math.min(7, Math.round(rank)));
    return 'pane_pt_cpc_prob_' + String(rank);
  }

  function ptEnsureCpcOutlookPanes() {
    // CPC outlook probability bands can overlap or nest by category/probability.
    // Separate panes make high-probability bands draw above lower-probability
    // bands, similar to the U.S. Drought Monitor fix.
    for (var i = 0; i <= 7; i++) {
      ptEnsurePane('pane_pt_cpc_prob_' + String(i), 520 + i, 'auto');
    }
  }

  function ptCpcOutlookLayerFromFeatureCollection(featureCollection, color, fillOpacity, options, clickable) {
    options = options || {};
    featureCollection = featureCollection || {
      type: 'FeatureCollection',
      features: []
    };

    ptEnsureCpcOutlookPanes();

    var features = Array.isArray(featureCollection.features) ? featureCollection.features : [];
    var group = L.layerGroup();

    for (var rank = 0; rank <= 7; rank++) {
      var subset = features.filter(function(feature) {
        var props = feature && feature.properties ? feature.properties : {};
        return ptCpcOutlookIntensityRank(props) === rank;
      });

      if (!subset.length) continue;

      var layer = L.geoJSON({
        type: 'FeatureCollection',
        features: subset
      }, {
        pane: ptCpcOutlookPaneName(rank),
        style: function(feature) {
          return ptStyleForExternalFeature(feature, color, fillOpacity, options);
        },
        pointToLayer: ptPointForColor(color, options),
        onEachFeature: ptOnEachFeature(clickable, options)
      });

      group.addLayer(layer);
    }

    return group;
  }

  function ptDroughtMonitorLayerFromFeatureCollection(featureCollection, color, fillOpacity, options, clickable) {
    options = options || {};
    featureCollection = featureCollection || {
      type: 'FeatureCollection',
      features: []
    };

    ptEnsureDroughtMonitorPanes();

    var features = Array.isArray(featureCollection.features) ? featureCollection.features : [];
    var group = L.layerGroup();
    var order = [-1, 0, 1, 2, 3, 4];

    order.forEach(function(dmCode) {
      var subset = features.filter(function(feature) {
        var props = feature && feature.properties ? feature.properties : {};
        var dm = ptDroughtMonitorDmCode(props);

        if (dmCode < 0) {
          return dm === null || dm < 0 || dm > 4;
        }

        return dm === dmCode;
      });

      if (!subset.length) return;

      var layer = L.geoJSON({
        type: 'FeatureCollection',
        features: subset
      }, {
        pane: ptDroughtMonitorPaneName(dmCode),
        style: function(feature) {
          return ptStyleForExternalFeature(feature, color, fillOpacity, options);
        },
        pointToLayer: ptPointForColor(color, options),
        onEachFeature: ptOnEachFeature(clickable, options)
      });

      group.addLayer(layer);
    });

    return group;
  }

  function ptDroughtMonitorLegendHtml() {
    var items = [
      ['D0', 'Abnormally Dry', '#ffff00'],
      ['D1', 'Moderate Drought', '#fcd37f'],
      ['D2', 'Severe Drought', '#ffaa00'],
      ['D3', 'Extreme Drought', '#e60000'],
      ['D4', 'Exceptional Drought', '#730000']
    ];

    var html = '<div class="pt-tools-muted"><b>Drought classes:</b> ';

    items.forEach(function(item, idx) {
      if (idx > 0) html += ' ';
      html += '<span title="' + ptEscapeHtml(item[1]) + '" style="display:inline-block;border:1px solid rgba(0,0,0,0.35);background:' + item[2] + ';color:#111;padding:0 4px;margin-right:2px;border-radius:3px;font-size:10.5px;line-height:1.4;">' + item[0] + '</span>';
    });

    html += '</div>';
    return html;
  }


  function ptIsFireYearStyle(options) {
    options = options || {};

    var styleField = ptCleanText(options.defaultStyleField).toLowerCase();
    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var styleUnits = ptCleanText(options.styleUnits).toLowerCase();
    var styleLegendTitle = ptCleanText(options.styleLegendTitle).toLowerCase();
    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();

    return (
      styleField === 'year_' ||
      styleField === 'year' ||
      styleLegendTitle.indexOf('fire year') >= 0 ||
      (styleUnits.indexOf('year') >= 0 && (layerName.indexOf('fire') >= 0 || layerName.indexOf('burn') >= 0 || layerName.indexOf('perimeter') >= 0)) ||
      (styleMethod.indexOf('fire_year') >= 0)
    );
  }

  function ptFireYearValue(props) {
    props = props || {};

    var direct = ptFieldValue(props, 'YEAR_');
    if (direct === null || direct === undefined || direct === '') direct = ptFieldValue(props, 'YEAR');
    if (direct === null || direct === undefined || direct === '') direct = ptFieldValue(props, 'FIRE_YEAR');
    if (direct === null || direct === undefined || direct === '') direct = ptFieldValue(props, 'FireYear');

    var yr = parseInt(String(direct || '').replace(/[^0-9]/g, '').slice(0, 4), 10);
    if (isFinite(yr) && yr >= 1800 && yr <= 2200) return yr;

    var alarm = ptCleanText(ptFieldValue(props, 'ALARM_DATE'));
    var m = alarm.match(/(19|20)\d{2}/);
    if (m) {
      yr = parseInt(m[0], 10);
      if (isFinite(yr)) return yr;
    }

    return null;
  }

  function ptFireYearColor(year) {
    var yr = Number(year);
    if (!isFinite(yr)) return '#bdbdbd';

    var currentYear = new Date().getFullYear();
    var age = currentYear - yr;

    // Recent-to-old ramp.  This intentionally favors quick visual distinction
    // among recent burn years, not exact agency cartography.
    if (age <= 0) return '#7f0000';
    if (age === 1) return '#b30000';
    if (age === 2) return '#e34a33';
    if (age === 3) return '#fc8d59';
    if (age === 4) return '#fdbb84';
    if (age === 5) return '#fdd49e';
    return '#fee8c8';
  }

  function ptFireYearLegendHtml() {
    var currentYear = new Date().getFullYear();
    var items = [
      [String(currentYear), ptFireYearColor(currentYear)],
      [String(currentYear - 1), ptFireYearColor(currentYear - 1)],
      [String(currentYear - 2), ptFireYearColor(currentYear - 2)],
      [String(currentYear - 3), ptFireYearColor(currentYear - 3)],
      [String(currentYear - 4), ptFireYearColor(currentYear - 4)],
      [String(currentYear - 5), ptFireYearColor(currentYear - 5)],
      [String(currentYear - 6) + ' or older', ptFireYearColor(currentYear - 6)]
    ];

    var html = '<div class="pt-tools-muted"><b>Fire year:</b> ';
    items.forEach(function(item, idx) {
      if (idx > 0) html += ' ';
      html += '<span style="display:inline-block;border:1px solid rgba(0,0,0,0.35);background:' + item[1] + ';color:#111;padding:0 4px;margin-right:2px;border-radius:3px;font-size:10.5px;line-height:1.4;">' + ptEscapeHtml(item[0]) + '</span>';
    });
    html += '</div>';
    return html;
  }




  function ptIsMlrsMineralCaseStyle(options) {
    options = options || {};

    var layerName = ptCleanText(options.layerName).toLowerCase();
    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var serviceUrl = ptCleanText(options.url || options.serviceUrl).toLowerCase();

    return styleMethod.indexOf('mlrs_case_status') >= 0 ||
      layerName.indexOf('blm mlrs') >= 0 ||
      serviceUrl.indexOf('mlrs') >= 0 ||
      serviceUrl.indexOf('miningclaims') >= 0;
  }

  function ptMlrsCaseDispositionValue(props) {
    props = props || {};
    return ptCleanText(ptFieldValue(props, 'CSE_DISP'));
  }

  function ptMlrsProductValue(props, options) {
    props = props || {};
    options = options || {};

    var layerName = ptCleanText(options.layerName).toLowerCase();
    var blmProd = ptCleanText(ptFieldValue(props, 'BLM_PROD')).toLowerCase();

    if (layerName.indexOf('oil') >= 0 && layerName.indexOf('gas') >= 0) return 'oil_gas';
    if (layerName.indexOf('geothermal') >= 0) return 'geothermal';
    if (layerName.indexOf('locatable') >= 0) return 'locatable';
    if (layerName.indexOf('mineral materials') >= 0 || layerName.indexOf('salable') >= 0) return 'mineral_materials';
    if (layerName.indexOf('mining claims') >= 0) return 'mining_claims';

    if (blmProd.indexOf('oil') >= 0 || blmProd.indexOf('gas') >= 0) return 'oil_gas';
    if (blmProd.indexOf('geothermal') >= 0) return 'geothermal';
    if (blmProd.indexOf('locatable') >= 0) return 'locatable';
    if (blmProd.indexOf('mineral material') >= 0 || blmProd.indexOf('salable') >= 0) return 'mineral_materials';
    if (blmProd.indexOf('mining claim') >= 0 || blmProd.indexOf('claim') >= 0) return 'mining_claims';

    return 'other';
  }

  function ptMlrsStatusClass(value, options) {
    var v = ptCleanText(value).toLowerCase();
    var layerName = ptCleanText(options && options.layerName).toLowerCase();

    if (!v) {
      if (layerName.indexOf('closed') >= 0 || layerName.indexOf('inactive') >= 0) return 'closed';
      if (layerName.indexOf('active') >= 0) return 'active';
      return 'unknown';
    }

    if (v.indexOf('closed') >= 0 || v.indexOf('cancel') >= 0 ||
        v.indexOf('expired') >= 0 || v.indexOf('terminated') >= 0 ||
        v.indexOf('rejected') >= 0 || v.indexOf('void') >= 0 ||
        v.indexOf('inactive') >= 0 || v.indexOf('abandoned') >= 0) {
      return 'closed';
    }

    if (v.indexOf('pending') >= 0 || v.indexOf('application') >= 0 || v.indexOf('proposed') >= 0) {
      return 'pending';
    }

    if (v.indexOf('interim') >= 0 || v.indexOf('suspended') >= 0) {
      return 'interim';
    }

    if (v.indexOf('authorized') >= 0 || v.indexOf('active') >= 0 ||
        v.indexOf('filed') >= 0 || v.indexOf('open') >= 0 ||
        v.indexOf('producing') >= 0 || v.indexOf('issued') >= 0) {
      return 'active';
    }

    return 'other';
  }

  function ptMlrsStatusColor(value, options) {
    var cls = ptMlrsStatusClass(value, options);

    if (cls === 'active') return '#31a354';       // filed / authorized / active
    if (cls === 'pending') return '#ffd92f';      // pending / application
    if (cls === 'interim') return '#fdae61';      // interim / suspended
    if (cls === 'closed') return '#e34a33';       // closed / cancelled / expired
    if (cls === 'other') return '#8c6bb1';        // known but not classified
    return '#bdbdbd';                             // missing / unknown
  }

  function ptMlrsOutlineColor(props, options) {
    var product = ptMlrsProductValue(props, options);

    if (product === 'mining_claims') return '#238b45';
    if (product === 'oil_gas') return '#8c510a';
    if (product === 'geothermal') return '#c51b7d';
    if (product === 'locatable') return '#54278f';
    if (product === 'mineral_materials') return '#b8860b';

    return '#555555';
  }

  function ptMlrsMineralCasesLegendHtml() {
    function swatch(fill, outline, label, dashed) {
      return '<span class="pt-mlrs-legend-item">' +
        '<span class="pt-mlrs-legend-swatch" style="border:' +
          (dashed ? '2px dashed ' : '2px solid ') + outline + ';background:' + fill +
        ';"></span>' +
        '<span>' + ptEscapeHtml(label) + '</span>' +
      '</span>';
    }

    var html = '<div class="pt-tools-muted"><b>MLRS symbology:</b> fill = case status/disposition; outline = mineral program / record type.</div>';
    html += '<div class="pt-tools-muted pt-mlrs-legend-row" style="margin-top:4px;">' +
      '<b>Fill:</b> ' +
      swatch('#31a354', '#555', 'Active / filed / authorized', false) +
      swatch('#ffd92f', '#555', 'Pending', false) +
      swatch('#fdae61', '#555', 'Interim / suspended', false) +
      swatch('#e34a33', '#555', 'Closed / inactive', false) +
      swatch('#8c6bb1', '#555', 'Other', false) +
      swatch('#bdbdbd', '#555', 'Unknown', false) +
      '</div>';
    html += '<div class="pt-tools-muted pt-mlrs-legend-row" style="margin-top:4px;">' +
      '<b>Outline:</b> ' +
      swatch('rgba(255,255,255,0.35)', '#238b45', 'Locatable: claims/sites', false) +
      swatch('rgba(255,255,255,0.35)', '#54278f', 'Locatable: notices/plans', false) +
      swatch('rgba(255,255,255,0.35)', '#8c510a', 'Leasable: oil & gas', false) +
      swatch('rgba(255,255,255,0.35)', '#c51b7d', 'Leasable: geothermal', false) +
      swatch('rgba(255,255,255,0.35)', '#b8860b', 'Saleable materials', false) +
      '</div>';
    html += '<div class="pt-tools-muted" style="margin-top:4px;line-height:1.22;">Locatable notices/plans are surface-management filings for locatable mineral operations, not mining claims. Saleable materials are common materials such as sand/gravel/rock.</div>';
    html += '<div class="pt-tools-muted" style="margin-top:3px;line-height:1.22;">Darker map patches usually mean overlapping case features, not a separate status class.</div>';

    return html;
  }

  function ptMlrsMineralCasesMapLegendHtml() {
    return '<div style="font-weight:bold;margin-bottom:4px;">BLM MLRS mineral case records</div>' +
      ptMlrsMineralCasesLegendHtml() +
      '<div style="margin-top:5px;font-size:11px;color:#42545a;line-height:1.25;">Screening-level case/claim/lease/notice geometry; not mineral estate or split-estate mapping.</div>';
  }

  function ptMlrsInterpretationNoteHtml(props, options) {
    if (!ptIsMlrsMineralCaseStyle(options)) return '';

    return '<div style="margin-top:7px;padding:5px 6px;border-left:3px solid #8c510a;background:#fff8ec;font-size:11px;color:#4b3a24;line-height:1.25;">' +
      '<b>BRIM note:</b> MLRS case/claim/lease/notice geometry is screening-level and may be PLSS/legal-description-derived. It is not mineral estate ownership or split-estate mapping.' +
      '</div>';
  }

  function ptIsCarbAirContextStyle(options) {
    options = options || {};

    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();
    var serviceUrl = ptCleanText(options.url || options.serviceUrl).toLowerCase();

    return layerName.indexOf('carb air districts') >= 0 ||
      layerName.indexOf('carb air basins') >= 0 ||
      serviceUrl.indexOf('california_air_districts') >= 0 ||
      serviceUrl.indexOf('california_air_basins') >= 0;
  }

  function ptIsAirNowAqiStyle(options) {
    options = options || {};

    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();
    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var serviceUrl = ptCleanText(options.url || options.serviceUrl).toLowerCase();

    return styleMethod.indexOf('airnow_aqi') >= 0 ||
      layerName.indexOf('airnow') >= 0 ||
      serviceUrl.indexOf('air%20now%20current%20monitor%20data%20public') >= 0 ||
      serviceUrl.indexOf('airnowlatestcontours') >= 0;
  }

  function ptAirNowAqiValue(props, options) {
    props = props || {};
    options = options || {};

    var fields = [
      ptCleanText(options.defaultStyleField),
      'OZONEPM_AQI_SORT',
      'OZONEPM_AQI',
      'PM_AQI_SORT',
      'PM_AQI',
      'PM25_AQI_SORT',
      'PM25_AQI',
      'PM10_AQI_SORT',
      'PM10_AQI',
      'OZONE_AQI_SORT',
      'OZONE_AQI',
      'gridcode'
    ];

    for (var i = 0; i < fields.length; i++) {
      var field = fields[i];
      if (!field) continue;
      var v = ptFieldValue(props, field);
      if (ptHasValue(v)) {
        var n = Number(v);
        if (isFinite(n)) return n;
      }
    }

    return null;
  }

  function ptAirNowAqiColor(value, props, options) {
    props = props || {};
    options = options || {};

    var n = Number(value);
    var styleField = ptCleanText(options.defaultStyleField).toLowerCase();
    var hasGridCode = ptHasValue(ptFieldValue(props, 'gridcode')) || styleField === 'gridcode';

    // AirNow contour services use gridcode classes 1-6 rather than numeric AQI.
    if (hasGridCode && isFinite(n) && n >= 1 && n <= 6) {
      if (n === 1) return '#00e400';
      if (n === 2) return '#ffff00';
      if (n === 3) return '#ff7e00';
      if (n === 4) return '#ff0000';
      if (n === 5) return '#99004c';
      if (n === 6) return '#7e0023';
    }

    if (!isFinite(n) || n < 0) return '#d9d9d9';
    if (n <= 50) return '#00e400';
    if (n <= 100) return '#ffff00';
    if (n <= 150) return '#ff7e00';
    if (n <= 200) return '#ff0000';
    if (n <= 300) return '#8f3f97';
    return '#7e0023';
  }

  function ptAirNowAqiCategory(value, props, options) {
    props = props || {};
    options = options || {};

    var labelFields = ['OZONEPM_AQI_LABEL', 'PM_AQI_LABEL', 'PM25_AQI_LABEL', 'PM10_AQI_LABEL', 'OZONE_AQI_LABEL'];
    for (var i = 0; i < labelFields.length; i++) {
      var label = ptCleanText(ptFieldValue(props, labelFields[i]));
      if (label && label.toLowerCase() !== 'no data') return label;
    }

    var n = Number(value);
    var styleField = ptCleanText(options.defaultStyleField).toLowerCase();
    var hasGridCode = ptHasValue(ptFieldValue(props, 'gridcode')) || styleField === 'gridcode';

    if (hasGridCode && isFinite(n) && n >= 1 && n <= 6) {
      if (n === 1) return 'Good';
      if (n === 2) return 'Moderate';
      if (n === 3) return 'Unhealthy for Sensitive Groups';
      if (n === 4) return 'Unhealthy';
      if (n === 5) return 'Very Unhealthy';
      if (n === 6) return 'Hazardous';
    }

    if (!isFinite(n) || n < 0) return 'No Data';
    if (n <= 50) return 'Good';
    if (n <= 100) return 'Moderate';
    if (n <= 150) return 'Unhealthy for Sensitive Groups';
    if (n <= 200) return 'Unhealthy';
    if (n <= 300) return 'Very Unhealthy';
    return 'Hazardous';
  }

  function ptAirNowUsefulLinksHtml(props, options) {
    if (!ptIsAirNowAqiStyle(options)) return '';

    return '<div style="margin-top:7px;padding:5px 6px;border-left:3px solid #2b8cbe;background:#eef7fb;font-size:11px;color:#24465f;line-height:1.25;">' +
      '<b>Useful air/smoke links:</b> ' +
      '<a href="https://gispub.epa.gov/airnow/" target="_blank" rel="noopener noreferrer">AirNow map</a> · ' +
      '<a href="https://fire.airnow.gov/" target="_blank" rel="noopener noreferrer">AirNow Fire &amp; Smoke</a> · ' +
      '<a href="https://portal.airfire.org/" target="_blank" rel="noopener noreferrer">AirFire portal</a> · ' +
      '<a href="https://map.purpleair.com/air-quality-standards-us-epa-aqi?opt=%2F1%2Flp%2Fa10%2Fp604800%2FcC0#5.38/36.993/-117.891" target="_blank" rel="noopener noreferrer">PurpleAir map</a>' +
      '<div style="margin-top:3px;">AirNow current-hour values are preliminary screening data. Use the timestamp and source portal during fast-changing smoke/pollution events.</div>' +
      '</div>';
  }


  function ptIsNifcCurrentFirePerimeterStyle(options) {
    options = options || {};

    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();
    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var serviceUrl = ptCleanText(options.url || options.serviceUrl).toLowerCase();

    return styleMethod.indexOf('nifc_current_fire') >= 0 ||
      layerName.indexOf('nifc current wildfire') >= 0 ||
      serviceUrl.indexOf('wfigs_interagency_perimeters_current') >= 0;
  }

  function ptNifcIncidentTypeValue(props) {
    props = props || {};
    var v = ptCleanText(ptFieldValue(props, 'attr_IncidentTypeCategory'));
    if (!v) v = ptCleanText(ptFieldValue(props, 'IncidentTypeCategory'));
    if (!v) v = ptCleanText(ptFieldValue(props, 'poly_IncidentTypeCategory'));
    return v.toUpperCase();
  }

  function ptNifcCurrentFireColor(props) {
    var t = ptNifcIncidentTypeValue(props);

    // WFIGS current perimeter categories commonly include WF = Wildfire,
    // RX = Prescribed Fire, and CX = Incident Complex.  This Ops row is
    // normally filtered to WF/CX, but keep RX/unknown handling defensive in
    // case the source schema changes or the layer is used manually.
    if (t === 'CX') return '#b10026';
    if (t === 'RX') return '#7b3294';
    if (t === 'WF' || !t) return '#ff1f1f';
    return '#ff4d2e';
  }

  function ptNifcCurrentFireLegendHtml() {
    return '<div class="pt-tools-muted"><b>NIFC current perimeter style:</b> bright red = wildfire/current incident perimeter. Dark red = incident complex if present. BRIM filters this Ops row to WF/CX where the service supports the SQL filter.</div>' +
      '<div class="pt-tools-muted" style="margin-top:3px;line-height:1.25;">No BRIM acreage filter is applied. Missing small fires usually reflects source availability/fall-off rules, perimeter availability, or service transfer limits in broad views.</div>';
  }



  function ptIsUsgsEarthquakeStyle(options) {
    options = options || {};

    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();
    var serviceUrl = ptCleanText(options.url || options.serviceUrl).toLowerCase();

    return styleMethod.indexOf('usgs_earthquake') >= 0 ||
      layerName.indexOf('earthquake') >= 0 ||
      serviceUrl.indexOf('earthquake.usgs.gov/earthquakes/feed') >= 0;
  }

  function ptUsgsEarthquakeMagnitude(props) {
    var mag = Number(ptFieldValue(props || {}, 'mag'));
    return isFinite(mag) ? mag : null;
  }

  function ptUsgsEarthquakeRadius(props) {
    var mag = ptUsgsEarthquakeMagnitude(props);
    if (mag === null) return 4.5;
    if (mag < 1) return 3.5;
    if (mag < 2) return 4.5;
    if (mag < 3) return 5.5;
    if (mag < 4) return 7;
    if (mag < 5) return 9;
    return 12;
  }

  function ptUsgsEarthquakeColor(props) {
    var mag = ptUsgsEarthquakeMagnitude(props);
    if (mag === null) return '#9e9e9e';
    if (mag < 2) return '#fee08b';
    if (mag < 3) return '#fdae61';
    if (mag < 4) return '#f46d43';
    if (mag < 5) return '#d73027';
    return '#7f0000';
  }

  function ptIsAmlFeatureStyle(options) {
    options = options || {};

    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();
    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var serviceUrl = ptCleanText(options.url || options.serviceUrl).toLowerCase();

    return styleMethod.indexOf('aml_status') >= 0 ||
      layerName.indexOf('abandoned mine') >= 0 ||
      layerName.indexOf('aml') >= 0 ||
      serviceUrl.indexOf('mapped_abandoned_mine_features') >= 0;
  }

  function ptAmlStatusValue(props) {
    props = props || {};

    var v = ptCleanText(ptFieldValue(props, 'Status'));
    if (!v) v = ptCleanText(ptFieldValue(props, 'status'));
    if (!v) v = ptCleanText(ptFieldValue(props, 'STATUS'));

    return v;
  }

  function ptAmlStatusColor(status) {
    var s = ptCleanText(status).toLowerCase();

    // AML points are small screening features and were too faint with the
    // generic categorical palette.  Keep some transparency, but make the three
    // common source Status classes visually distinct:
    //   Potential   = magenta/pink
    //   Inventoried = orange
    //   Remediated  = green
    // This is BRIM-brightened screening symbology, not the official DOC style.
    if (s.indexOf('potential') >= 0) return '#e7298a';
    if (s.indexOf('inventory') >= 0 || s.indexOf('inventoried') >= 0) return '#ff9f1a';
    if (s.indexOf('remediat') >= 0 || s.indexOf('reclaim') >= 0 || s.indexOf('closed') >= 0) return '#31a354';
    if (s.indexOf('hazard') >= 0 || s.indexOf('open') >= 0) return '#e31a1c';
    return '#fdbf6f';
  }

  function ptAmlStatusOutlineColor(status) {
    var s = ptCleanText(status).toLowerCase();
    if (s.indexOf('potential') >= 0) return '#7a0177';
    if (s.indexOf('inventory') >= 0 || s.indexOf('inventoried') >= 0) return '#994c00';
    if (s.indexOf('remediat') >= 0 || s.indexOf('reclaim') >= 0 || s.indexOf('closed') >= 0) return '#005a32';
    return '#4a1486';
  }

  function ptAmlStatusLegendSwatch(status, label) {
    var fill = ptAmlStatusColor(status);
    var outline = ptAmlStatusOutlineColor(status);
    return '<span style="display:inline-block;width:9px;height:9px;border-radius:50%;' +
      'border:1.4px solid ' + outline + ';background:' + fill + ';opacity:0.88;margin-right:4px;"></span>' +
      ptEscapeHtml(label);
  }

  function ptAmlStatusLegendHtml() {
    return '<div class="pt-tools-muted"><b>AML feature style:</b> ' +
      ptAmlStatusLegendSwatch('Potential', 'Potential') + ' &nbsp; ' +
      ptAmlStatusLegendSwatch('Inventoried', 'Inventoried') + ' &nbsp; ' +
      ptAmlStatusLegendSwatch('Remediated', 'Remediated') +
      '<br/>Colors are BRIM-brightened from the source Status field for visibility; not official DOC symbology.</div>';
  }

  function ptAmlSourceNoteHtml(props, options) {
    if (!ptIsAmlFeatureStyle(options)) return '';

    // The public DOC AML service intentionally exposes a sparse screening
    // layer: the useful returned field in normal popups is generally the
    // source Status class.  Avoid implying BRIM lost a hidden mine name/ID; add
    // a concise note where users will see it.
    return '<div style="margin-top:7px;padding:5px 6px;border-left:3px solid #7a0177;' +
      'background:#fff7fb;font-size:11px;color:#5a315d;line-height:1.25;">' +
      '<b>Source note:</b> Public DOC AML features are obfuscated/generalized screening points and usually expose only a status class in this layer. Verify details with the source agency before field use.' +
      '</div>';
  }

  function ptIsGenericCategoricalStyle(options) {
    options = options || {};

    var styleField = ptCleanText(options.defaultStyleField);
    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();

    if (!styleField) return false;

    // These have dedicated BRIM renderers/legends and should not fall through
    // to the generic categorical palette merely because their style method or
    // field happens to be categorical.
    if (ptIsDroughtMonitorStyle(options) || ptIsCpcOutlookStyle(options) ||
        ptIsFireYearStyle(options) || ptIsSpcForecastStyle(options) ||
        ptIsAlertCameraStyle(options) || ptIsWcrCompletedDepthStyle(options) ||
        ptIsMlrsMineralCaseStyle(options) || ptIsStreamGaugeFlowStyle(options) ||
        ptIsAirNowAqiStyle(options) || ptIsNifcCurrentFirePerimeterStyle(options) ||
        ptIsAmlFeatureStyle(options) || ptIsUsgsEarthquakeStyle(options) ||
        ptIsCalIpcRampStyle(options) || ptIsSwrcbIrListingStatusStyle(options) ||
        ptIsSubsidenceObservationStyle(options)) {
      return false;
    }

    return styleMethod.indexOf('categorical') >= 0 ||
      styleMethod.indexOf('category') >= 0 ||
      styleMethod.indexOf('distinct') >= 0;
  }

  function ptGenericCategoricalValue(props, options) {
    props = props || {};
    options = options || {};

    var field = ptCleanText(options.defaultStyleField);
    if (!field) return '';

    return ptCleanText(ptFieldValue(props, field));
  }

  function ptHashString(value) {
    var s = String(value || '');
    var h = 0;

    for (var i = 0; i < s.length; i++) {
      h = ((h << 5) - h) + s.charCodeAt(i);
      h = h | 0;
    }

    return Math.abs(h);
  }

  function ptGenericCategoricalColor(value) {
    var v = ptCleanText(value);
    if (!v) return '#bdbdbd';

    // Stable qualitative palette for External Layers where BRIM needs to
    // separate adjacent named features but should not imply numeric order.
    var palette = [
      '#1f78b4', '#33a02c', '#e31a1c', '#ff7f00', '#6a3d9a',
      '#b15928', '#a6cee3', '#b2df8a', '#fb9a99', '#fdbf6f',
      '#cab2d6', '#8dd3c7', '#ffff99', '#bebada', '#80b1d3',
      '#fdb462', '#b3de69', '#fccde5', '#bc80bd', '#ccebc5'
    ];

    return palette[ptHashString(v.toLowerCase()) % palette.length];
  }

  function ptGenericCategoricalLegendHtml(options) {
    options = options || {};

    if (!ptIsGenericCategoricalStyle(options)) return '';

    var field = ptCleanText(options.defaultStyleField);
    var title = ptCleanText(options.styleLegendTitle) || ('Categorical colors by ' + field);

    return '<div class="pt-tools-muted"><b>' + ptEscapeHtml(title) + ':</b> ' +
      'colors are assigned from the ' + ptEscapeHtml(field) +
      ' field to help separate adjacent features. Colors are BRIM-derived and not official source symbology.</div>';
  }

  function ptIsDwrTreInsarPointLocationStyle(options) {
    options = options || {};

    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();
    var styleLegendTitle = ptCleanText(options.styleLegendTitle).toLowerCase();
    var serviceUrl = ptCleanText(options.url || options.serviceUrl).toLowerCase();

    return styleMethod.indexOf('dwr_tre_insar_point_location') >= 0 ||
      styleMethod.indexOf('tre_insar_point_location') >= 0 ||
      styleLegendTitle.indexOf('dwr/tre 2026q1 insar point') >= 0 ||
      (layerName.indexOf('dwr/tre 2026q1') >= 0 && layerName.indexOf('insar point') >= 0) ||
      serviceUrl.indexOf('vertical_displacement_point_data_locations_2026q1') >= 0;
  }

  function ptDwrTreInsarPointValue(props, fieldName) {
    props = props || {};
    return ptCleanText(ptFieldValue(props, fieldName));
  }

  function ptDwrTreInsarPointLatLng(props) {
    props = props || {};

    var lon = Number(ptFieldValue(props, 'Longitude'));
    var lat = Number(ptFieldValue(props, 'Latitude'));

    if (!isFinite(lon)) lon = Number(ptFieldValue(props, 'X'));
    if (!isFinite(lat)) lat = Number(ptFieldValue(props, 'Y'));

    if (!isFinite(lat) || !isFinite(lon)) return null;
    if (lat < -90 || lat > 90 || lon < -180 || lon > 180) return null;

    return L.latLng(lat, lon);
  }

  function ptAnnotateDwrTreInsarPointFeature(feature) {
    if (!feature || !feature.properties) return feature;

    var props = feature.properties;
    var tableVal = ptDwrTreInsarPointValue(props, 'DataTable');
    var codeVal = ptDwrTreInsarPointValue(props, 'CODE');
    var ll = ptDwrTreInsarPointLatLng(props);

    props.BRIM_TRE_Point_Table_Label = tableVal ? ('DataTable ' + tableVal + (String(tableVal) === '34' ? ' (latest table in 2026Q1 service)' : '')) : '';
    props.BRIM_TRE_Point_Source_Note = 'DWR/TRE InSAR point/cell index. Use the DWR chart/CSV links for displacement time series; displacement values are not exposed as simple attributes in this map layer.';

    if (ll) {
      props.BRIM_TRE_Point_Coord = ll.lat.toFixed(5) + ', ' + ll.lng.toFixed(5);
      feature.geometry = {
        type: 'Point',
        coordinates: [ll.lng, ll.lat]
      };
    }

    if (codeVal) {
      props.BRIM_TRE_Point_Code_Label = codeVal;
    }

    return feature;
  }

  function ptDwrTreInsarPointFeatureCollection(featureCollection) {
    featureCollection = featureCollection || { type: 'FeatureCollection', features: [] };

    var out = {
      type: 'FeatureCollection',
      features: []
    };

    var features = Array.isArray(featureCollection.features) ? featureCollection.features : [];
    features.forEach(function(feature) {
      if (!feature || !feature.properties) return;
      feature = ptAnnotateDwrTreInsarPointFeature(feature);
      if (!feature.geometry || ptCleanText(feature.geometry.type).toLowerCase() !== 'point') return;
      out.features.push(feature);
    });

    return out;
  }

  function ptDwrTreInsarPointMarker(feature, latlng) {
    return L.circleMarker(latlng, {
      pane: 'pane_pt_custom_point',
      radius: 4.2,
      color: '#063b5c',
      weight: 1.15,
      opacity: 0.96,
      fillColor: '#26c6da',
      fillOpacity: 0.80
    });
  }

  function ptDwrTreInsarPointLayerFromFeatureCollection(featureCollection, color, options, clickable) {
    options = options || {};
    var pointCollection = ptDwrTreInsarPointFeatureCollection(featureCollection);

    return L.geoJSON(pointCollection, {
      pane: 'pane_pt_custom_point',
      pointToLayer: function(feature, latlng) {
        return ptDwrTreInsarPointMarker(feature, latlng);
      },
      onEachFeature: ptOnEachFeature(clickable, options)
    });
  }

  function ptDwrTreInsarPointLegendHtml() {
    return '<div class="pt-tools-muted"><b>DWR/TRE InSAR point links:</b> ' +
      '<span style="display:inline-block;width:10px;height:10px;border-radius:50%;background:#26c6da;border:1.2px solid #063b5c;margin:0 4px 0 3px;vertical-align:-1px;"></span>' +
      'latest-table DWR/TRE 2026Q1 InSAR point/cell location. BRIM loads <code>DataTable = 34</code> only in the current view and draws markers from the source Longitude/Latitude fields. Popup links open the DWR point chart and CSV by CODE; the map layer itself does not expose displacement magnitude fields.</div>';
  }

  function ptIsSubsidenceObservationStyle(options) {
    options = options || {};

    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();
    var styleLegendTitle = ptCleanText(options.styleLegendTitle).toLowerCase();
    var styleField = ptCleanText(options.defaultStyleField).toLowerCase();

    return styleMethod.indexOf('subsidence_observation') >= 0 ||
      styleMethod.indexOf('subsidence_obs') >= 0 ||
      (layerName.indexOf('land subsidence observations') >= 0 && styleField === 'data_source') ||
      styleLegendTitle.indexOf('subsidence observation') >= 0;
  }

  function ptSubsidenceObservationRawSource(props) {
    props = props || {};
    return ptCleanText(ptFieldValue(props, 'Data_Source')) ||
      ptCleanText(ptFieldValue(props, 'data_source')) ||
      ptCleanText(ptFieldValue(props, 'SOURCE')) ||
      ptCleanText(ptFieldValue(props, 'Source'));
  }

  function ptSubsidenceObservationCombinedText(props) {
    props = props || {};

    return [
      ptFieldValue(props, 'Name'),
      ptFieldValue(props, 'SubsidenceObs_ID'),
      ptFieldValue(props, 'Data_Source'),
      ptFieldValue(props, 'Comments')
    ]
      .map(ptCleanText)
      .filter(function(x) { return x !== ''; })
      .join(' | ');
  }

  function ptSubsidenceObservationPrefixCandidates(props) {
    props = props || {};
    return [
      { field: 'Name', value: ptCleanText(ptFieldValue(props, 'Name')) },
      { field: 'SubsidenceObs_ID', value: ptCleanText(ptFieldValue(props, 'SubsidenceObs_ID')) },
      { field: 'Observation ID', value: ptCleanText(ptFieldValue(props, 'Observation_ID')) },
      { field: 'Obs ID', value: ptCleanText(ptFieldValue(props, 'Obs_ID')) }
    ].filter(function(x) { return x.value !== ''; });
  }

  function ptSubsidenceObservationClassInfo(props) {
    props = props || {};

    function info(cls, basis, prefix) {
      return {
        className: cls,
        basis: basis,
        prefix: prefix || '',
        sourceText: ptSubsidenceObservationCombinedText(props)
      };
    }

    var candidates = ptSubsidenceObservationPrefixCandidates(props);

    if (!candidates.length) {
      return info('Unknown / no name or ID', 'No Name or observation ID returned for prefix classification', '');
    }

    // Prefix-first classification.  The broad Data_Source / Comments text can be
    // misleading for this DWR layer, so BRIM classifies from the observation
    // Name / ID prefix and leaves raw source text in the popup for verification.
    for (var i = 0; i < candidates.length; i++) {
      var field = candidates[i].field;
      var value = candidates[i].value;
      var v = value.toLowerCase();
      var basisPrefix = value.replace(/\s+/g, ' ').trim();

      if (/^insar\b|^insar[\s_\-]*\d+/i.test(value)) {
        return info('InSAR', field + ' prefix "InSAR"', 'InSAR');
      }

      if (/^exten\b|^exten[\s_\-]*\d+|^extensometer\b|^extn\b/i.test(value)) {
        return info('Extensometer', field + ' prefix "Exten" / "Extensometer"', 'Exten');
      }

      if (/^resjr\b|^resjr[\s_\-]*\d+|^restore[\s_\-]*sjr\b|^restoresjr\b/i.test(value)) {
        return info('RestoreSJR GPS survey', field + ' prefix "ReSJR" / "RestoreSJR"', 'ReSJR');
      }

      if (/^pbo\b|^pbo[\s_\-]*\d+|^pb[\s_\-]*\d+|^pb\b/i.test(value)) {
        return info('PBO / UNAVCO GPS', field + ' prefix "PB" / "PBO"', 'PB/PBO');
      }

      if (/^c2vsim\b|^c2vsim[\s_\-]*\d+|^c2v\b|^c2v[\s_\-]*\d+/i.test(value)) {
        return info('C2VSim / model result', field + ' prefix "C2VSim" / "C2V"', 'C2VSim');
      }

      if (/^gps\b|^gnss\b|^cgps\b/i.test(value)) {
        return info('GPS / GNSS', field + ' prefix "GPS" / "GNSS"', value.split(/[\s_\-]+/)[0]);
      }
    }

    return info(
      'Other / unclassified prefix',
      'No BRIM prefix rule matched Name/ID: ' + candidates.map(function(x) { return x.field + '=' + x.value; }).join('; '),
      candidates[0].value.split(/[\s_\-]+/)[0]
    );
  }

  function ptSubsidenceObservationClass(props) {
    return ptSubsidenceObservationClassInfo(props).className;
  }

  function ptSubsidenceObservationGeometryPointCount(feature) {
    feature = feature || {};
    var geom = feature.geometry || {};
    var type = ptCleanText(geom.type).toLowerCase();
    var coords = geom.coordinates;

    if (type === 'point') {
      return Array.isArray(coords) && coords.length >= 2 ? 1 : 0;
    }

    if (type === 'multipoint') {
      return Array.isArray(coords) ? coords.length : 0;
    }

    if (type === 'geometrycollection') {
      var geoms = Array.isArray(geom.geometries) ? geom.geometries : [];
      return geoms.reduce(function(total, childGeom) {
        return total + ptSubsidenceObservationGeometryPointCount({ geometry: childGeom });
      }, 0);
    }

    // This DWR layer should be point/multipoint.  If an unexpected geometry
    // slips through, count the feature as one mapped object so the legend does
    // not silently drop it.
    return 1;
  }

  function ptAnnotateSubsidenceObservationFeature(feature) {
    if (!feature || !feature.properties) return feature;

    var info = ptSubsidenceObservationClassInfo(feature.properties);
    var pointCount = ptSubsidenceObservationGeometryPointCount(feature);

    feature.properties.BRIM_SubObs_Class = info.className;
    feature.properties.BRIM_SubObs_Class_Basis = info.basis;
    feature.properties.BRIM_SubObs_Prefix = info.prefix;
    feature.properties.BRIM_SubObs_Source_Text = info.sourceText;
    feature.properties.BRIM_SubObs_Point_Count = pointCount;
    feature.properties.BRIM_SubObs_Point_Count_Label = pointCount > 1 ? String(pointCount) : '';
    return feature;
  }

  function ptAnnotateSubsidenceObservationFeatureCollection(featureCollection) {
    featureCollection = featureCollection || { type: 'FeatureCollection', features: [] };
    var features = Array.isArray(featureCollection.features) ? featureCollection.features : [];
    features.forEach(ptAnnotateSubsidenceObservationFeature);
    return featureCollection;
  }

  function ptSubsidenceObservationClassCountsFromFeatures(features) {
    var counts = {};
    (features || []).forEach(function(feature) {
      var props = feature && feature.properties ? feature.properties : {};
      var cls = ptCleanText(props.BRIM_SubObs_Class) || ptSubsidenceObservationClass(props);
      var pointCount = Number(props.BRIM_SubObs_Point_Count);

      if (!isFinite(pointCount) || pointCount <= 0) {
        pointCount = ptSubsidenceObservationGeometryPointCount(feature);
      }

      if (!isFinite(pointCount) || pointCount <= 0) pointCount = 1;
      counts[cls] = (counts[cls] || 0) + pointCount;
    });
    return counts;
  }

  function ptSubsidenceObservationCountLabel(cls, counts) {
    var label = ptCleanText(cls);
    if (!counts || counts[label] === undefined || counts[label] === null) return label;
    return label + ' (' + Number(counts[label]).toLocaleString() + ')';
  }

  function ptSubsidenceObservationClassSummary(counts) {
    counts = counts || {};
    var order = [
      'InSAR',
      'Extensometer',
      'RestoreSJR GPS survey',
      'PBO / UNAVCO GPS',
      'GPS / GNSS',
      'C2VSim / model result',
      'Other / unclassified prefix',
      'Unknown / no name or ID'
    ];

    var parts = [];
    order.forEach(function(cls) {
      if (counts[cls]) parts.push(cls + ': ' + Number(counts[cls]).toLocaleString());
    });

    Object.keys(counts).sort().forEach(function(cls) {
      if (order.indexOf(cls) < 0 && counts[cls]) {
        parts.push(cls + ': ' + Number(counts[cls]).toLocaleString());
      }
    });

    return parts.join('; ');
  }

  function ptSubsidenceObservationColor(cls) {
    var key = ptCleanText(cls).toLowerCase();
    if (key.indexOf('extensometer') >= 0) return '#d73027';
    if (key.indexOf('insar') >= 0) return '#7b3294';
    if (key.indexOf('restoresjr') >= 0) return '#f46d43';
    if (key.indexOf('pbo') >= 0 || key.indexOf('unavco') >= 0) return '#4575b4';
    if (key.indexOf('gps') >= 0 || key.indexOf('gnss') >= 0) return '#74add1';
    if (key.indexOf('c2vsim') >= 0 || key.indexOf('model') >= 0) return '#8c6d31';
    if (key.indexOf('other') >= 0) return '#fdae61';
    return '#bdbdbd';
  }

  function ptSubsidenceObservationMarker(feature, latlng) {
    var props = feature && feature.properties ? feature.properties : {};
    var cls = ptSubsidenceObservationClass(props);
    var fill = ptSubsidenceObservationColor(cls);
    var knownClass = cls !== 'Unknown / no name or ID' && cls !== 'Other / unclassified prefix';

    return L.circleMarker(latlng, {
      pane: 'pane_pt_custom_point',
      radius: knownClass ? 6.2 : 5.5,
      color: '#263238',
      weight: knownClass ? 1.25 : 1.05,
      opacity: 0.96,
      fillColor: fill,
      fillOpacity: knownClass ? 0.86 : 0.62,
      dashArray: knownClass ? null : '2,2'
    });
  }

  function ptSubsidenceObservationLegendRowsHtml(symbolSize, outlineColor, counts) {
    symbolSize = symbolSize || 11;
    outlineColor = outlineColor || '#263238';
    counts = counts || null;

    function dot(cls, label, borderStyle) {
      var display = counts ? ptSubsidenceObservationCountLabel(cls, counts) : label;
      return '<span style="display:inline-block;width:' + symbolSize + 'px;height:' + symbolSize + 'px;border-radius:50%;background:' +
        ptSubsidenceObservationColor(cls) + ';border:1.3px ' + (borderStyle || 'solid') + ' ' + outlineColor +
        ';margin-right:5px;vertical-align:-1px;"></span>' + ptEscapeHtml(display);
    }

    return '' +
      '<div>' + dot('InSAR', 'InSAR') + '</div>' +
      '<div>' + dot('Extensometer', 'Extensometer') + '</div>' +
      '<div>' + dot('RestoreSJR GPS survey', 'RestoreSJR GPS survey') + '</div>' +
      '<div>' + dot('PBO / UNAVCO GPS', 'PBO / UNAVCO GPS') + '</div>' +
      '<div>' + dot('GPS / GNSS', 'GPS / GNSS') + '</div>' +
      '<div>' + dot('C2VSim / model result', 'C2VSim / model result') + '</div>' +
      '<div>' + dot('Other / unclassified prefix', 'Other / unclassified prefix', 'dashed') + '</div>' +
      '<div>' + dot('Unknown / no name or ID', 'No name/ID', 'dashed') + '</div>';
  }

  function ptSubsidenceObservationLegendHtml() {
    return '<div class="pt-tools-muted"><b>Subsidence observation points:</b> colors are BRIM-screening classes derived from Name/ID prefixes, not broad source text.</div>' +
      '<div class="pt-tools-muted" style="display:grid;grid-template-columns:repeat(2,minmax(135px,1fr));gap:2px 10px;margin-top:3px;">' +
      ptSubsidenceObservationLegendRowsHtml(10, '#263238') +
      '</div>' +
      '<div class="pt-tools-muted" style="margin-top:4px;">Popup includes prefix, BRIM class, raw source text, and class basis. Source does not expose a true per-point POR URL in this layer.</div>';
  }

  function ptSubsidenceObservationActiveClassCounts() {
    var counts = {};

    ptCustomLayers.forEach(function(rec) {
      if (!rec || !ptIsSubsidenceObservationStyle(rec) || rec.visible === false || !rec.layer || !map.hasLayer(rec.layer)) return;
      var recCounts = rec.subsidenceObservationClassCounts || {};
      Object.keys(recCounts).forEach(function(cls) {
        counts[cls] = (counts[cls] || 0) + Number(recCounts[cls] || 0);
      });
    });

    return counts;
  }

  function ptSubsidenceObservationMapLegendHtml(counts) {
    counts = counts || null;
    return '' +
      '<div style="font-weight:700;margin-bottom:3px;">Subsidence observation points</div>' +
      '<div style="font-size:11px;color:#4f5659;margin-bottom:5px;">Fill color = BRIM Name/ID prefix class.</div>' +
      '<div style="display:grid;grid-template-columns:repeat(2,minmax(145px,1fr));gap:3px 10px;">' +
      ptSubsidenceObservationLegendRowsHtml(12, '#263238', counts) +
      '</div>' +
      '<div style="font-size:10.5px;color:#606060;margin-top:5px;line-height:1.25;">Counts are mapped point locations in the current-view snapshot. Some source records are MultiPoint features, especially InSAR-style observations. Prefix classes are for screening; verify source context before reporting.</div>';
  }





  function ptIsSwrcbIrListingStatusStyle(options) {
    options = options || {};
    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();
    var styleLegendTitle = ptCleanText(options.styleLegendTitle).toLowerCase();

    return styleMethod.indexOf('swrcb_ir_listing_status') >= 0 ||
      layerName.indexOf('2024 integrated report') >= 0 ||
      styleLegendTitle.indexOf('integrated report listing status') >= 0;
  }

  function ptSwrcbIrListingStatusValue(props) {
    props = props || {};
    return ptCleanText(ptFieldValue(props, 'listing_status')) || ptCleanText(ptFieldValue(props, 'Listing Status'));
  }

  function ptSwrcbIrListingStatusColor(status) {
    var s = ptCleanText(status).toLowerCase();

    if (s.indexOf('not') >= 0 && s.indexOf('listed') >= 0) return '#2ca25f';
    if (s.indexOf('listed') >= 0 || s.indexOf('impaired') >= 0) return '#de2d26';
    return '#9e9e9e';
  }

  function ptSwrcbIrListingStatusLegendHtml() {
    return '<div class="pt-tools-muted"><b>2024 Integrated Report style:</b> ' +
      '<span style="display:inline-block;width:11px;height:10px;border:1px solid rgba(0,0,0,0.35);background:#de2d26;margin:0 4px 0 3px;vertical-align:-1px;"></span>listed / impaired ' +
      '<span style="display:inline-block;width:11px;height:10px;border:1px solid rgba(0,0,0,0.35);background:#2ca25f;margin:0 4px 0 8px;vertical-align:-1px;"></span>not listed ' +
      '<span style="display:inline-block;width:11px;height:10px;border:1px solid rgba(0,0,0,0.35);background:#9e9e9e;margin:0 4px 0 8px;vertical-align:-1px;"></span>other/unknown. Colors are BRIM-derived from source listing_status.</div>';
  }

  function ptIsCalIpcSpeciesCountStyle(options) {
    options = options || {};
    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();
    return styleMethod.indexOf('calipc_species_count') >= 0 ||
      (layerName.indexOf('cal-ipc') >= 0 && layerName.indexOf('species count') >= 0);
  }

  function ptIsCalIpcThreatStyle(options) {
    options = options || {};
    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();
    return styleMethod.indexOf('calipc_invasive_threat') >= 0 ||
      (layerName.indexOf('cal-ipc') >= 0 && layerName.indexOf('invasion level') >= 0);
  }

  function ptIsCalIpcRampStyle(options) {
    return ptIsCalIpcSpeciesCountStyle(options) || ptIsCalIpcThreatStyle(options);
  }

  function ptCalIpcNumericValue(props, options) {
    props = props || {};
    options = options || {};
    var field = ptCleanText(options.defaultStyleField);
    var candidates = field ? [field] : [];

    if (ptIsCalIpcSpeciesCountStyle(options)) {
      candidates = candidates.concat(['COUNT_SPECIES', 'Count_Species', 'count_species', 'Species_Count']);
    } else if (ptIsCalIpcThreatStyle(options)) {
      candidates = candidates.concat(['Invasive_Threat', 'invasive_threat', 'INVASIVE_THREAT']);
    }

    for (var i = 0; i < candidates.length; i++) {
      var v = Number(ptFieldValue(props, candidates[i]));
      if (isFinite(v)) return v;
    }

    return null;
  }

  function ptCalIpcSpeciesCountColor(value) {
    var n = Number(value);
    if (!isFinite(n)) return '#d9d9d9';
    // Simple ordered ramp: low species counts green, high counts red.
    // Use stronger colors than the first pass so the quad grid is readable
    // over USGS Hydrography and BLM land-status fills.
    if (n <= 5) return '#d9f0d3';
    if (n <= 15) return '#74c476';
    if (n <= 30) return '#fee08b';
    if (n <= 60) return '#fdae61';
    if (n <= 100) return '#f46d43';
    return '#b30000';
  }

  function ptCalIpcThreatColor(value) {
    var n = Number(value);
    if (!isFinite(n)) return '#d9d9d9';
    // White/yellow/orange/red is easier to interpret for threat/intensity than
    // the earlier pale-green ramp, which looked like a categorical quad grid.
    if (n <= 0.05) return '#fff7bc';
    if (n <= 0.15) return '#fee391';
    if (n <= 0.30) return '#fec44f';
    if (n <= 0.50) return '#fe9929';
    if (n <= 0.75) return '#d95f0e';
    return '#7f0000';
  }

  function ptCalIpcRampLegendHtml(options) {
    options = options || {};
    if (ptIsCalIpcSpeciesCountStyle(options)) {
      return '<div class="pt-tools-muted"><b>Cal-IPC species-count ramp:</b> green = lower invasive-plant species count by quad; yellow/orange/red = higher species count. Colors are BRIM-derived from the source count field.</div>';
    }
    if (ptIsCalIpcThreatStyle(options)) {
      return '<div class="pt-tools-muted"><b>Cal-IPC invasion-level ramp:</b> light yellow = lower source invasion-threat index; orange/red = higher index. Colors are BRIM-derived from the source Invasive_Threat field.</div>';
    }
    return '';
  }

  function ptSortFeatureCollectionForDrawing(featureCollection, options) {
    options = options || {};

    if (!featureCollection || !Array.isArray(featureCollection.features)) {
      return featureCollection;
    }

    if (ptIsDroughtMonitorStyle(options)) {
      // The U.S. Drought Monitor polygons are cumulative/nested by category:
      // D0 can cover the broad dry area, with D1-D4 progressively nested inside.
      // Leaflet draws later features on top.  Sort low-to-high intensity so
      // D3/D4 are not hidden underneath broader D0-D2 polygons.
      featureCollection.features = featureCollection.features.slice().sort(function(a, b) {
        var aProps = a && a.properties ? a.properties : {};
        var bProps = b && b.properties ? b.properties : {};
        var aDm = Number(ptFieldValue(aProps, 'DM'));
        var bDm = Number(ptFieldValue(bProps, 'DM'));

        if (!isFinite(aDm)) aDm = -999;
        if (!isFinite(bDm)) bDm = -999;

        return aDm - bDm;
      });

      return featureCollection;
    }

    if (ptIsCpcOutlookStyle(options)) {
      // Draw lower-probability CPC outlook polygons first and higher
      // probability bands last.  Some CPC services return overlapping bands,
      // so relying on source response order can hide interior bands.
      featureCollection.features = featureCollection.features.slice().sort(function(a, b) {
        var aProps = a && a.properties ? a.properties : {};
        var bProps = b && b.properties ? b.properties : {};
        return ptCpcOutlookIntensityRank(aProps) - ptCpcOutlookIntensityRank(bProps);
      });

      return featureCollection;
    }

    if (ptIsSpcForecastStyle(options)) {
      // Draw lower-severity SPC outlook polygons first and higher categories
      // last.  Dedicated panes below make the draw order deterministic.
      featureCollection.features = featureCollection.features.slice().sort(function(a, b) {
        var aProps = a && a.properties ? a.properties : {};
        var bProps = b && b.properties ? b.properties : {};
        var aCat = ptSpcCategoryValue(aProps, options);
        var bCat = ptSpcCategoryValue(bProps, options);
        return ptSpcForecastSeverityRank(aCat, aProps, options) - ptSpcForecastSeverityRank(bCat, bProps, options);
      });

      return featureCollection;
    }

    if (ptIsFireYearStyle(options)) {
      // Draw older/unknown fire perimeters first and newer fire years last so
      // recent burn areas remain visible where perimeters overlap.
      featureCollection.features = featureCollection.features.slice().sort(function(a, b) {
        var aProps = a && a.properties ? a.properties : {};
        var bProps = b && b.properties ? b.properties : {};
        var aYr = ptFireYearValue(aProps);
        var bYr = ptFireYearValue(bProps);

        if (!isFinite(aYr)) aYr = -9999;
        if (!isFinite(bYr)) bYr = -9999;

        return aYr - bYr;
      });

      return featureCollection;
    }

    return featureCollection;
  }

  function ptCpcCategoryLabel(value) {
    var txt = ptCleanText(value);
    var low = txt.toLowerCase();

    if (low === 'a' || low === 'above' || low === 'above normal') return 'Above normal';
    if (low === 'b' || low === 'below' || low === 'below normal') return 'Below normal';
    if (low === 'n' || low === 'normal' || low === 'near normal') return 'Near normal';
    if (low === 'ec' || low === 'equal chances') return 'Equal chances';

    return txt;
  }

  function ptCpcCategoryKey(value) {
    var low = ptCleanText(value).toLowerCase();

    if (low === 'a' || low === 'above' || low === 'above normal') return 'above';
    if (low === 'b' || low === 'below' || low === 'below normal') return 'below';
    if (low === 'n' || low === 'normal' || low === 'near normal') return 'normal';
    if (low === 'ec' || low === 'equal chances') return 'normal';

    return low;
  }

  function ptIsCpcOutlookStyle(options) {
    options = options || {};

    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();
    var styleLegendTitle = ptCleanText(options.styleLegendTitle).toLowerCase();

    return styleMethod.indexOf('cpc') >= 0 ||
      layerName.indexOf('cpc ') >= 0 ||
      layerName.indexOf('climate prediction center') >= 0 ||
      styleLegendTitle.indexOf('cpc') >= 0;
  }

  function ptCpcOutlookKind(options) {
    options = options || {};

    var text = (
      ptCleanText(options.styleUnits) + ' ' +
      ptCleanText(options.layerName || options.name) + ' ' +
      ptCleanText(options.styleLegendTitle)
    ).toLowerCase();

    if (text.indexOf('precip') >= 0) return 'precipitation';
    return 'temperature';
  }

  function ptCpcOutlookColor(catValue, probValue, options) {
    var key = ptCpcCategoryKey(catValue);
    var kind = ptCpcOutlookKind(options);
    var prob = Number(probValue);

    if (!isFinite(prob)) prob = 33;

    var idx = 0;
    if (prob >= 90) idx = 6;
    else if (prob >= 80) idx = 5;
    else if (prob >= 70) idx = 4;
    else if (prob >= 60) idx = 3;
    else if (prob >= 50) idx = 2;
    else if (prob >= 40) idx = 1;

    var tempAbove = ['#fdd49e', '#fdbb84', '#fc8d59', '#ef6548', '#d7301f', '#b30000', '#7f0000'];
    var tempBelow = ['#deebf7', '#c6dbef', '#9ecae1', '#6baed6', '#3182bd', '#08519c', '#08306b'];
    var precipAbove = ['#c7e9c0', '#a1d99b', '#74c476', '#41ab5d', '#238b45', '#006d2c', '#00441b'];
    var precipBelow = ['#fee8c8', '#fdd49e', '#fdbb84', '#fc8d59', '#d95f0e', '#a63603', '#7f2704'];

    if (key === 'above') return kind === 'precipitation' ? precipAbove[idx] : tempAbove[idx];
    if (key === 'below') return kind === 'precipitation' ? precipBelow[idx] : tempBelow[idx];

    return '#eeeeee';
  }

  function ptCpcOutlookLegendHtml(options) {
    options = options || {};

    var kind = ptCpcOutlookKind(options);
    var aboveColor = ptCpcOutlookColor('Above', 60, options);
    var belowColor = ptCpcOutlookColor('Below', 60, options);

    var aboveLabel = kind === 'precipitation' ? 'Above precip' : 'Above temp';
    var belowLabel = kind === 'precipitation' ? 'Below precip' : 'Below temp';

    return '<div class="pt-tools-muted"><b>CPC style:</b> ' +
      '<span style="display:inline-block;border:1px solid rgba(0,0,0,0.35);background:' + aboveColor + ';padding:0 5px;margin-right:3px;border-radius:3px;font-size:10.5px;line-height:1.4;">' + aboveLabel + '</span>' +
      '<span style="display:inline-block;border:1px solid rgba(0,0,0,0.35);background:' + belowColor + ';padding:0 5px;margin-right:3px;border-radius:3px;font-size:10.5px;line-height:1.4;">' + belowLabel + '</span>' +
      '<span style="display:inline-block;border:1px solid rgba(0,0,0,0.35);background:#eeeeee;padding:0 5px;border-radius:3px;font-size:10.5px;line-height:1.4;">Near normal / EC</span>' +
      '</div>';
  }


  function ptIsAlertCameraStyle(options) {
    options = options || {};

    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();
    var styleLegendTitle = ptCleanText(options.styleLegendTitle).toLowerCase();

    return styleMethod.indexOf('alert_camera') >= 0 ||
      layerName.indexOf('alertcalifornia') >= 0 ||
      styleLegendTitle.indexOf('alertcalifornia') >= 0;
  }

  function ptIsAlertCameraViewshedStyle(options) {
    options = options || {};

    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();

    return styleMethod.indexOf('alert_camera_viewshed') >= 0 ||
      layerName.indexOf('viewshed') >= 0 && layerName.indexOf('alertcalifornia') >= 0;
  }

  function ptAlertCameraIsOnline(props) {
    props = props || {};

    var online = ptCleanText(ptFieldValue(props, 'isOnline')).toLowerCase();
    var active = ptCleanText(ptFieldValue(props, 'isActive')).toLowerCase();

    if (online === 'online' || online === 'true' || online === 'yes' || online === '1') return true;
    if (online === 'offline' || online === 'false' || online === 'no' || online === '0') return false;

    if (active === 'active' || active === 'true' || active === 'yes' || active === '1') return true;

    return false;
  }

  function ptAlertCameraOnlineLabel(value) {
    var v = ptCleanText(value).toLowerCase();

    if (v === 'online' || v === 'true' || v === 'yes' || v === '1') return 'Online';
    if (v === 'offline' || v === 'false' || v === 'no' || v === '0') return 'Offline';

    return ptCleanText(value);
  }

  function ptAlertCameraLegendHtml() {
    return '<div class="pt-tools-muted"><b>Camera style:</b> ' +
      '<span style="display:inline-block;width:0;height:0;border-left:5px solid transparent;border-right:5px solid transparent;border-bottom:13px solid #38bdf8;filter:drop-shadow(0 0 1px #111);margin:0 4px 0 3px;vertical-align:middle;"></span> online directional camera ' +
      '<span style="display:inline-block;width:0;height:0;border-left:5px solid transparent;border-right:5px solid transparent;border-bottom:13px solid #bdbdbd;filter:drop-shadow(0 0 1px #111);margin:0 4px 0 8px;vertical-align:middle;"></span> offline/unknown. ' +
      'Arrow direction uses the service pan/heading field when available. Open the popup for the ALERTCalifornia viewer and latest image thumbnail when available.</div>';
  }

  function ptAlertCameraViewshedLegendHtml() {
    return '<div class="pt-tools-muted"><b>Viewshed style:</b> translucent cyan polygons show ALERTCalifornia camera viewsheds from the source service. Use together with the camera arrows for direction and live viewer links.</div>';
  }

  function ptAlertCameraFieldAny(props, fields) {
    props = props || {};
    fields = fields || [];

    for (var i = 0; i < fields.length; i++) {
      var direct = ptCleanText(ptFieldValue(props, fields[i]));
      if (direct) return direct;
    }

    var keys = Object.keys(props);
    for (var k = 0; k < keys.length; k++) {
      var lk = String(keys[k]).toLowerCase();
      for (var j = 0; j < fields.length; j++) {
        if (lk === String(fields[j]).toLowerCase()) {
          var v = ptCleanText(props[keys[k]]);
          if (v) return v;
        }
      }
    }

    return '';
  }

  function ptAlertCameraImageUrl(props) {
    props = props || {};
    return ptAlertCameraFieldAny(props, [
      'imageURL', 'imageUrl', 'imgFullURL', 'imgFullUrl', 'thumbnailURL',
      'thumbnailUrl', 'latestImageURL', 'latestImageUrl'
    ]);
  }

  function ptAlertCameraViewerUrl(props) {
    props = props || {};

    var urlFields = [
      'cameraURL_raw', 'cameraURL', 'cameraUrl', 'Camera URL', 'camera_url',
      'viewerURL', 'viewerUrl', 'url'
    ];

    var bestUrl = '';
    for (var i = 0; i < urlFields.length; i++) {
      var candidate = ptCleanText(ptAlertCameraFieldAny(props, [urlFields[i]]));
      if (!candidate || !/^https?:\/\//i.test(candidate)) continue;

      // Prefer URLs that go directly to a camera id. Generic ALERTCalifornia
      // homepage/network URLs are less useful as thumbnail/card links.
      if (/cameras\.alertcalifornia\.org\/\?[^\s]*\bid=/i.test(candidate)) return candidate;
      if (!bestUrl) bestUrl = candidate;
    }

    var cameraId = ptAlertCameraFieldAny(props, [
      'cameraID', 'cameraId', 'camera_id', 'id', 'ID', 'axisID', 'axisId'
    ]);
    if (cameraId) {
      return 'https://cameras.alertcalifornia.org/?id=' + encodeURIComponent(cameraId);
    }

    var cameraName = ptCleanText(ptFieldValue(props, 'cameraName'));
    if (cameraName) {
      return 'https://cameras.alertcalifornia.org/?id=' + encodeURIComponent(cameraName);
    }

    return bestUrl || 'https://cameras.alertcalifornia.org/';
  }

  function ptAlertCameraPopupImageHtml(props, options) {
    props = props || {};
    options = options || {};

    if (!ptIsAlertCameraStyle(options)) return '';

    var imageUrl = ptAlertCameraImageUrl(props);
    if (!imageUrl || !/^https?:\/\//i.test(imageUrl)) return '';

    var viewerUrl = ptAlertCameraViewerUrl(props);
    var imageHtml = '<img src="' + ptEscapeHtml(imageUrl) + '" alt="ALERTCalifornia camera image" style="display:block;width:100%;height:auto;max-height:255px;object-fit:contain;background:#111;" loading="lazy" />';

    if (viewerUrl && /^https?:\/\//i.test(viewerUrl)) {
      imageHtml = '<a href="' + ptEscapeHtml(viewerUrl) + '" target="_blank" rel="noopener noreferrer" title="Open this camera in ALERTCalifornia">' + imageHtml + '</a>';
    }

    return '<div style="margin-top:7px;border:1px solid #d6d6d6;border-radius:7px;overflow:hidden;background:#111;max-width:360px;">' +
      imageHtml +
      '</div>';
  }

  function ptAlertCameraValueRowHtml(label, value) {
    if (!ptHasValue(value)) return '';
    return '<div><b>' + ptEscapeHtml(label) + ':</b> ' + ptEscapeHtml(value) + '</div>';
  }

  function ptAlertCameraCompassLabel(deg) {
    deg = Number(deg);
    if (!isFinite(deg)) return '';

    var dirs = ['N', 'NE', 'E', 'SE', 'S', 'SW', 'W', 'NW'];
    var norm = ((deg % 360) + 360) % 360;
    return dirs[Math.round(norm / 45) % 8];
  }

  function ptAlertCameraDirectionLabel(props) {
    props = props || {};

    var pan = Number(ptFieldValue(props, 'positionPan'));
    if (!isFinite(pan)) return '';

    var rounded = Math.round(((pan % 360) + 360) % 360);
    var compass = ptAlertCameraCompassLabel(rounded);
    return compass ? compass + ' (' + rounded + '°)' : rounded + '°';
  }

  function ptAlertCameraCompactLabel(props) {
    props = props || {};
    var nm = ptCleanText(ptFieldValue(props, 'cameraName')) || 'Camera';
    var dir = ptAlertCameraDirectionLabel(props);
    return nm + (dir ? ' — ' + dir : '');
  }

  function ptAlertCameraSwitchButtonHtml(props, idx, options) {
    options = options || {};
    props = props || {};
    var cameraName = ptCleanText(ptFieldValue(props, 'cameraName')) || 'ALERTCalifornia camera';
    var direction = ptAlertCameraDirectionLabel(props);
    var viewTime = ptFormatFieldValue('viewTime', ptFieldValue(props, 'viewTime'), props, options);
    var imageUrl = ptAlertCameraImageUrl(props) || '';
    var viewerUrl = ptAlertCameraViewerUrl(props) || '';
    var label = cameraName + (direction ? ' — ' + direction : '');
    var activeStyle = idx === 0 ? 'background:#dff3fb;border-color:#38bdf8;color:#075985;font-weight:700;' : 'background:#f7fbfc;border-color:#cbd5dc;color:#1f2937;font-weight:600;';

    return '<button type="button" class="pt-alert-camera-tab" ' +
      'data-pt-alert-camera-tab="' + String(idx) + '" ' +
      'data-pt-alert-camera-name="' + ptEscapeHtml(cameraName) + '" ' +
      'data-pt-alert-camera-label="' + ptEscapeHtml(label) + '" ' +
      'data-pt-alert-camera-direction="' + ptEscapeHtml(direction) + '" ' +
      'data-pt-alert-camera-time="' + ptEscapeHtml(viewTime || '') + '" ' +
      'data-pt-alert-camera-image="' + ptEscapeHtml(imageUrl || '') + '" ' +
      'data-pt-alert-camera-url="' + ptEscapeHtml(viewerUrl || '') + '" ' +
      'style="display:block;width:100%;box-sizing:border-box;text-align:left;margin:4px 0;padding:6px 8px;border:1px solid;border-radius:7px;' + activeStyle +
      'font-size:12.5px;line-height:1.2;cursor:pointer;">' + ptEscapeHtml(label) + '</button>';
  }

  function ptAlertCameraActiveViewerButtonHtml(viewerUrl) {
    if (!viewerUrl || !/^https?:\/\//i.test(viewerUrl)) return '';
    return '<a class="pt-alert-camera-open-link" href="' + ptEscapeHtml(viewerUrl) + '" target="_blank" rel="noopener noreferrer" ' +
      'style="display:inline-block;margin:6px 6px 0 0;padding:5px 8px;border:1px solid #9ab7c3;border-radius:6px;background:#eef8fb;color:#075985;text-decoration:none;font-size:12px;line-height:1.2;cursor:pointer;">Open camera page</a>';
  }

  function ptInstallAlertCameraPopupHandlers() {
    if (window.ptAlertCameraPopupHandlersInstalled) return;
    window.ptAlertCameraPopupHandlersInstalled = true;

    document.addEventListener('click', function(e) {
      var tab = e.target && e.target.closest ? e.target.closest('[data-pt-alert-camera-tab]') : null;
      if (tab) {
        e.preventDefault();
        e.stopPropagation();

        var root = tab.closest ? tab.closest('.pt-alert-camera-popup') : null;
        if (!root) return;

        var imageUrl = tab.getAttribute('data-pt-alert-camera-image') || '';
        var viewerUrl = tab.getAttribute('data-pt-alert-camera-url') || '';
        var cameraName = tab.getAttribute('data-pt-alert-camera-name') || '';
        var direction = tab.getAttribute('data-pt-alert-camera-direction') || '';
        var viewTime = tab.getAttribute('data-pt-alert-camera-time') || '';

        var tabs = root.querySelectorAll ? root.querySelectorAll('[data-pt-alert-camera-tab]') : [];
        for (var i = 0; i < tabs.length; i++) {
          tabs[i].style.background = '#f7fbfc';
          tabs[i].style.borderColor = '#cbd5dc';
          tabs[i].style.color = '#1f2937';
          tabs[i].style.fontWeight = '600';
        }
        tab.style.background = '#dff3fb';
        tab.style.borderColor = '#38bdf8';
        tab.style.color = '#075985';
        tab.style.fontWeight = '700';

        var img = root.querySelector ? root.querySelector('.pt-alert-camera-active-img') : null;
        var imgWrap = root.querySelector ? root.querySelector('.pt-alert-camera-active-img-link') : null;
        var openLink = root.querySelector ? root.querySelector('.pt-alert-camera-open-link') : null;
        var nameEl = root.querySelector ? root.querySelector('.pt-alert-camera-active-name') : null;
        var dirEl = root.querySelector ? root.querySelector('.pt-alert-camera-active-direction') : null;
        var timeEl = root.querySelector ? root.querySelector('.pt-alert-camera-active-time') : null;

        if (img && imageUrl) img.setAttribute('src', imageUrl);
        if (imgWrap && viewerUrl) imgWrap.setAttribute('href', viewerUrl);
        if (openLink && viewerUrl) openLink.setAttribute('href', viewerUrl);
        if (nameEl) nameEl.textContent = cameraName || 'ALERTCalifornia camera';
        if (dirEl) dirEl.textContent = direction ? 'Direction: ' + direction : 'Direction: n/a';
        if (timeEl) timeEl.textContent = viewTime ? 'Source view time: ' + viewTime : 'Source view time: n/a';
        return;
      }

      var btn = e.target && e.target.closest ? e.target.closest('[data-pt-alert-camera-viewsheds]') : null;
      if (!btn) return;

      e.preventDefault();
      e.stopPropagation();

      var viewshedLayerName = 'ALERTCalifornia Camera Viewsheds';
      var result = null;

      if (window.ptOpsToggleLayerByName) {
        result = window.ptOpsToggleLayerByName(viewshedLayerName);
      } else if (window.ptOpsActivateLayerByName) {
        result = window.ptOpsActivateLayerByName(viewshedLayerName);
        if (result && result.ok) result.active = true;
      }

      if (result && result.ok) {
        if (result.active) {
          btn.textContent = 'Turn viewsheds off';
          btn.style.background = '#d1fae5';
          btn.style.borderColor = '#34d399';
          btn.style.color = '#065f46';
        } else {
          btn.textContent = 'Turn viewsheds on';
          btn.style.background = '#f7fbfc';
          btn.style.borderColor = '#9ab7c3';
          btn.style.color = '#075985';
        }
      } else {
        btn.textContent = (result && result.message) ? result.message : 'Use Ops Viewsheds';
      }
    }, false);
  }

  function ptAlertCameraPopupHtml(cameraPropsList, options) {
    cameraPropsList = cameraPropsList || [];
    options = options || {};

    ptInstallAlertCameraPopupHandlers();

    if (!cameraPropsList.length) return '<b>ALERTCalifornia Cameras</b>';

    var first = cameraPropsList[0] || {};
    var firstName = ptCleanText(ptFieldValue(first, 'cameraName')) || 'ALERTCalifornia camera';
    var firstDirection = ptAlertCameraDirectionLabel(first);
    var firstViewTime = ptFormatFieldValue('viewTime', ptFieldValue(first, 'viewTime'), first, options);
    var firstImageUrl = ptAlertCameraImageUrl(first) || '';
    var firstViewerUrl = ptAlertCameraViewerUrl(first) || '';
    var total = cameraPropsList.length;

    var html = '<div class="pt-alert-camera-popup" style="max-width:370px;min-width:310px;line-height:1.25;">';

    if (total > 1) {
      html += '<div style="font-weight:700;font-size:13.5px;margin-bottom:5px;">Choose camera view</div>';
      html += '<div style="margin-bottom:7px;">';
      cameraPropsList.forEach(function(props, idx) {
        html += ptAlertCameraSwitchButtonHtml(props, idx, options);
      });
      html += '</div>';
    } else {
      html += '<div style="font-weight:700;font-size:14px;margin-bottom:5px;" class="pt-alert-camera-active-name">' + ptEscapeHtml(firstName) + '</div>';
    }

    var image = '';
    if (firstImageUrl && /^https?:\/\//i.test(firstImageUrl)) {
      image = '<img class="pt-alert-camera-active-img" src="' + ptEscapeHtml(firstImageUrl) + '" alt="ALERTCalifornia camera image" style="display:block;width:100%;height:auto;max-height:255px;object-fit:contain;background:#111;" loading="lazy" />';
      if (firstViewerUrl && /^https?:\/\//i.test(firstViewerUrl)) {
        image = '<a class="pt-alert-camera-active-img-link" href="' + ptEscapeHtml(firstViewerUrl) + '" target="_blank" rel="noopener noreferrer" title="Open this camera in ALERTCalifornia">' + image + '</a>';
      }
      html += '<div style="border:1px solid #d6d6d6;border-radius:7px;overflow:hidden;background:#111;">' + image + '</div>';
    } else {
      html += '<div style="border:1px solid #d6d6d6;border-radius:7px;background:#f7f7f7;padding:12px;color:#555;">No thumbnail URL returned for this camera.</div>';
    }

    if (total > 1) {
      html += '<div style="font-weight:700;font-size:13px;margin-top:7px;" class="pt-alert-camera-active-name">' + ptEscapeHtml(firstName) + '</div>';
    }
    html += '<div style="font-size:12px;color:#4b5563;margin-top:3px;">' +
      '<div class="pt-alert-camera-active-direction">' + ptEscapeHtml(firstDirection ? 'Direction: ' + firstDirection : 'Direction: n/a') + '</div>' +
      '<div class="pt-alert-camera-active-time">' + ptEscapeHtml(firstViewTime ? 'Source view time: ' + firstViewTime : 'Source view time: n/a') + '</div>' +
      '</div>';

    var viewshedsOn = !!(window.ptOpsIsLayerActiveByName && window.ptOpsIsLayerActiveByName('ALERTCalifornia Camera Viewsheds'));
    var viewshedBtnText = viewshedsOn ? 'Turn viewsheds off' : 'Turn viewsheds on';
    var viewshedBtnStyle = viewshedsOn ?
      'background:#d1fae5;border-color:#34d399;color:#065f46;' :
      'background:#f7fbfc;border-color:#9ab7c3;color:#075985;';

    html += '<div style="margin-top:2px;">' + ptAlertCameraActiveViewerButtonHtml(firstViewerUrl) +
      '<button type="button" data-pt-alert-camera-viewsheds="1" style="display:inline-block;margin:6px 0 0 0;padding:5px 8px;border:1px solid;border-radius:6px;' + viewshedBtnStyle + 'text-decoration:none;font-size:12px;line-height:1.2;cursor:pointer;">' + ptEscapeHtml(viewshedBtnText) + '</button>' +
      '</div>';

    html += '<div style="font-size:10.5px;color:#666;margin-top:6px;line-height:1.25;"><i>' +
      'Thumbnail and source view time may refresh on different schedules.' +
      '</i></div>';

    html += '</div>';
    return html;
  }

  function ptAlertCameraGroupTooltipHtml(cameraPropsList, options) {
    cameraPropsList = cameraPropsList || [];
    options = options || {};

    if (!cameraPropsList.length) return '';

    if (cameraPropsList.length === 1) {
      var props = cameraPropsList[0];
      var name = ptCleanText(ptFieldValue(props, 'cameraName')) || 'ALERTCalifornia camera';
      var dir = ptAlertCameraDirectionLabel(props);
      var viewTime = ptFormatFieldValue('viewTime', ptFieldValue(props, 'viewTime'), props, options);
      var rows = '<div style="min-width:285px;max-width:410px;white-space:normal;line-height:1.25;">' +
        '<div style="font-weight:700;font-size:13px;margin-bottom:2px;">' + ptEscapeHtml(name) + '</div>';
      if (dir) rows += '<div>Direction: ' + ptEscapeHtml(dir) + '</div>';
      if (viewTime) rows += '<div style="color:#555;">Source time: ' + ptEscapeHtml(viewTime) + '</div>';
      rows += '</div>';
      return rows;
    }

    var html = '<div style="min-width:340px;max-width:440px;white-space:normal;line-height:1.25;">' +
      '<div style="font-weight:700;font-size:13px;margin-bottom:4px;">' + cameraPropsList.length + ' co-located camera views</div>';

    cameraPropsList.slice(0, 6).forEach(function(props) {
      var nm = ptCleanText(ptFieldValue(props, 'cameraName')) || 'camera';
      var dir = ptAlertCameraDirectionLabel(props);
      var viewTime = ptFormatFieldValue('viewTime', ptFieldValue(props, 'viewTime'), props, options);
      html += '<div style="margin-top:3px;"><b>' + ptEscapeHtml(nm) + '</b>' +
        (dir ? ' — ' + ptEscapeHtml(dir) : '') +
        (viewTime ? '<span style="color:#555;"> · ' + ptEscapeHtml(viewTime) + '</span>' : '') +
        '</div>';
    });
    if (cameraPropsList.length > 6) html += '<div>…</div>';
    html += '</div>';
    return html;
  }

  function ptAlertCameraFeatureLatLng(feature) {
    var geom = feature && feature.geometry ? feature.geometry : null;
    var coords = geom && Array.isArray(geom.coordinates) ? geom.coordinates : null;
    if (!coords || coords.length < 2) return null;

    var lng = Number(coords[0]);
    var lat = Number(coords[1]);
    if (!isFinite(lat) || !isFinite(lng)) return null;

    return L.latLng(lat, lng);
  }

  function ptAlertCameraGroupKey(feature) {
    var ll = ptAlertCameraFeatureLatLng(feature);
    if (!ll) return '';

    // Round to ~1 meter. ALERTCalifornia colocated north/south views typically
    // have identical coordinates, but this small tolerance avoids brittle exact
    // floating-point grouping while not merging nearby camera sites.
    return ll.lat.toFixed(5) + ',' + ll.lng.toFixed(5);
  }

  function ptAlertCameraMultiSiteMarker(latlng, cameraPropsList) {
    cameraPropsList = cameraPropsList || [];

    var anyOnline = cameraPropsList.some(function(props) { return ptAlertCameraIsOnline(props); });
    var border = anyOnline ? '#075985' : '#777777';
    var count = cameraPropsList.length;

    var arrowSvg = '';
    cameraPropsList.slice(0, 6).forEach(function(props) {
      var pan = Number(ptFieldValue(props, 'positionPan'));
      var angle = isFinite(pan) ? pan : 0;
      var online = ptAlertCameraIsOnline(props);
      var fill = online ? '#38bdf8' : '#bdbdbd';
      var opacity = online ? '0.94' : '0.70';

      arrowSvg += '<polygon points="24,6 16,25 24,20 32,25" fill="' + fill + '" fill-opacity="' + opacity + '" stroke="#111827" stroke-width="1.25" stroke-linejoin="round" transform="rotate(' + angle + ' 24 24)" />';
    });

    var badge = '';
    if (count > 2) {
      badge = '<span style="position:absolute;right:-1px;bottom:-2px;display:flex;align-items:center;justify-content:center;width:14px;height:14px;border-radius:50%;background:#ffffff;border:1px solid ' + border + ';box-sizing:border-box;color:#111;font:700 9px/1 Arial,sans-serif;">' + String(count) + '</span>';
    }

    var html = '<span style="position:relative;display:block;width:44px;height:44px;">' +
      '<svg viewBox="0 0 48 48" width="44" height="44" aria-hidden="true" focusable="false" style="display:block;overflow:visible;filter:drop-shadow(0 1px 1px rgba(0,0,0,0.18));">' +
        '<circle cx="24" cy="24" r="18" fill="rgba(255,255,255,0.01)" stroke="' + border + '" stroke-opacity="0.55" stroke-width="1.25" stroke-dasharray="none" />' +
        arrowSvg +
      '</svg>' +
      badge +
      '</span>';

    return L.marker(latlng, {
      pane: 'pane_pt_custom_point',
      icon: L.divIcon({
        className: 'pt-alert-camera-multi-site-marker',
        iconSize: [44, 44],
        iconAnchor: [22, 22],
        html: html
      })
    });
  }

  function ptAlertCameraLayerFromFeatureCollection(featureCollection, color, options, clickable) {
    options = options || {};

    var features = featureCollection && Array.isArray(featureCollection.features) ? featureCollection.features : [];
    var groups = {};
    var order = [];

    features.forEach(function(feature) {
      if (!ptIsPointFeature(feature)) return;

      var key = ptAlertCameraGroupKey(feature);
      if (!key) return;

      if (!groups[key]) {
        groups[key] = {
          latlng: ptAlertCameraFeatureLatLng(feature),
          features: []
        };
        order.push(key);
      }

      groups[key].features.push(feature);
    });

    var markers = [];
    var singleMarkerFactory = ptPointForColor(color, options);

    order.forEach(function(key) {
      var group = groups[key];
      var groupFeatures = group.features || [];
      var propsList = groupFeatures.map(function(f) { return f && f.properties ? f.properties : {}; });
      var marker;

      if (groupFeatures.length === 1) {
        marker = singleMarkerFactory(groupFeatures[0], group.latlng);
      } else {
        // Sort by camera name for a stable popup order.
        propsList = propsList.slice().sort(function(a, b) {
          return ptCleanText(ptFieldValue(a, 'cameraName')).localeCompare(ptCleanText(ptFieldValue(b, 'cameraName')));
        });
        marker = ptAlertCameraMultiSiteMarker(group.latlng, propsList);
      }

      var tooltipHtml = ptAlertCameraGroupTooltipHtml(propsList, options);
      if (tooltipHtml) {
        marker.bindTooltip(tooltipHtml, {
          sticky: true,
          direction: 'auto',
          opacity: 0.95,
          className: 'pt-alert-camera-tooltip pt-external-overlay-tooltip'
        });
      }

      if (clickable) {
        marker.bindPopup(ptAlertCameraPopupHtml(propsList, options), {
          maxWidth: 360
        });
      }

      markers.push(marker);
    });

    var layer = L.layerGroup(markers);
    layer._ptFeatureCount = features.length;
    layer._ptCameraSiteCount = markers.length;
    layer._ptCameraGroupedRecordCount = features.length - markers.length;
    return layer;
  }

  function ptExternalFeatureCollectionLayer(featureCollection, color, fillOpacity, options, clickable) {
    options = options || {};

    if (ptIsDwrTreInsarPointLocationStyle(options)) {
      return ptDwrTreInsarPointLayerFromFeatureCollection(featureCollection, color, options, clickable);
    }

    if (ptIsSubsidenceObservationStyle(options)) {
      featureCollection = ptAnnotateSubsidenceObservationFeatureCollection(featureCollection);
    }

    if (ptIsDroughtMonitorStyle(options)) {
      return ptDroughtMonitorLayerFromFeatureCollection(featureCollection, color, fillOpacity, options, clickable);
    }

    if (ptIsCpcOutlookStyle(options)) {
      return ptCpcOutlookLayerFromFeatureCollection(featureCollection, color, fillOpacity, options, clickable);
    }

    if (ptIsSpcForecastStyle(options)) {
      return ptSpcForecastLayerFromFeatureCollection(featureCollection, color, fillOpacity, options, clickable);
    }

    if (ptIsAlertCameraStyle(options) && !ptIsAlertCameraViewshedStyle(options)) {
      return ptAlertCameraLayerFromFeatureCollection(featureCollection, color, options, clickable);
    }

    return L.geoJSON(featureCollection, {
      pane: 'pane_pt_custom_polygon',
      style: function(feature) {
        return ptStyleForExternalFeature(feature, color, fillOpacity, options);
      },
      pointToLayer: ptPointForColor(color, options),
      onEachFeature: ptOnEachFeature(clickable, options)
    });
  }

  function ptIsPointFeature(feature) {
    var geomType = ptCleanText(feature && feature.geometry ? feature.geometry.type : '').toLowerCase();
    return geomType === 'point' || geomType === 'multipoint';
  }

  function ptIsStreamGaugeFlowStyle(options) {
    options = options || {};

    var styleField = ptCleanText(options.defaultStyleField).toLowerCase();
    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();
    var styleLegendTitle = ptCleanText(options.styleLegendTitle).toLowerCase();

    return styleField === 'flow_cfs' ||
      styleMethod.indexOf('flow_cfs') >= 0 ||
      styleMethod.indexOf('streamflow') >= 0 ||
      layerName.indexOf('live stream') >= 0 ||
      styleLegendTitle.indexOf('flow') >= 0 && styleLegendTitle.indexOf('cfs') >= 0;
  }


  function ptIsWcrCompletedDepthStyle(options) {
    options = options || {};

    var styleField = ptCleanText(options.defaultStyleField).toLowerCase();
    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();
    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();
    var styleLegendTitle = ptCleanText(options.styleLegendTitle).toLowerCase();

    return (
      styleMethod.indexOf('wcr_completed_depth') >= 0 ||
      (styleField === 'totalcompleteddepth' && layerName.indexOf('well completion') >= 0) ||
      (layerName.indexOf('well completion reports') >= 0 && styleLegendTitle.indexOf('completed depth') >= 0)
    );
  }

  function ptWcrNameValue(props) {
    props = props || {};

    var ownerName = ptCleanText(ptFieldValue(props, 'OwnerAssignedWellNumber'));
    if (ownerName) return ownerName;

    var wcr = ptCleanText(ptFieldValue(props, 'WCRNumber'));
    if (wcr) return wcr;

    var legacy = ptCleanText(ptFieldValue(props, 'LegacyLogNumber'));
    if (legacy) return 'Legacy log ' + legacy;

    return 'Well completion report';
  }

  function ptWcrScreenIntervalValue(props) {
    props = props || {};

    var top = ptFieldValue(props, 'TopOfPerforatedInterval');
    var bottom = ptFieldValue(props, 'BottomofPerforatedInterval');

    function fmt(v) {
      var n = Number(v);
      if (isFinite(n)) return Math.round(n).toLocaleString();
      var s = ptCleanText(v);
      return s ? s : '';
    }

    var topText = fmt(top);
    var bottomText = fmt(bottom);

    if (topText && bottomText) return topText + ' / ' + bottomText + ' ft';
    if (topText) return topText + ' / n/a ft';
    if (bottomText) return 'n/a / ' + bottomText + ' ft';

    return '';
  }

  function ptWcrCompletedDepthValue(props) {
    props = props || {};

    var completed = Number(ptFieldValue(props, 'TotalCompletedDepth'));
    if (isFinite(completed)) return completed;

    var drilled = Number(ptFieldValue(props, 'TotalDrillDepth'));
    if (isFinite(drilled)) return drilled;

    return NaN;
  }

  function ptWcrCompletedDepthColor(value) {
    var depth = Number(value);

    // Use the USGS groundwater Ops-style ramp, but adapt the first bins for
    // WCR completed depth rather than depth-to-water.  WCR completed depth is
    // expected to be nonnegative; missing or nonsensical negative values are
    // shown with the no-depth symbol rather than an artesian/negative class.
    if (!isFinite(depth) || depth < 0) return '#f7f7f7';
    if (depth < 25) return '#7b5bbd';
    if (depth < 50) return '#2c7fb8';
    if (depth < 100) return '#66c2a4';
    if (depth < 250) return '#74c476';
    if (depth < 500) return '#fed976';
    if (depth < 1000) return '#fd8d3c';
    return '#de2d26';
  }

  function ptWcrCompletedDepthLegendRowsHtml(symbolSize, outlineColor) {
    symbolSize = symbolSize || 11;
    outlineColor = outlineColor || '#686868';

    function dot(color, borderStyle) {
      return '<span style="display:inline-block;width:' + symbolSize + 'px;height:' + symbolSize + 'px;border-radius:50%;background:' + color + ';border:1.5px ' + (borderStyle || 'solid') + ' ' + outlineColor + ';margin-right:5px;vertical-align:-1px;"></span>';
    }

    return '' +
      '<div>' + dot('#7b5bbd') + '0–25 ft</div>' +
      '<div>' + dot('#2c7fb8') + '25–50 ft</div>' +
      '<div>' + dot('#66c2a4') + '50–100 ft</div>' +
      '<div>' + dot('#74c476') + '100–250 ft</div>' +
      '<div>' + dot('#fed976') + '250–500 ft</div>' +
      '<div>' + dot('#fd8d3c') + '500–1,000 ft</div>' +
      '<div>' + dot('#de2d26') + '&gt;1,000 ft</div>' +
      '<div>' + dot('#f7f7f7', 'dashed') + 'No depth</div>';
  }

  function ptWcrCompletedDepthLegendHtml() {
    return '<div class="pt-tools-muted"><b>Completed-depth style:</b> points are colored by reported completed depth.</div>' +
      '<div class="pt-tools-muted" style="display:grid;grid-template-columns:repeat(2,minmax(92px,1fr));gap:2px 10px;margin-top:3px;">' +
      ptWcrCompletedDepthLegendRowsHtml(9, '#686868') +
      '</div>' +
      '<div class="pt-tools-muted" style="margin-top:4px;">WCR index locations and construction fields are screening-level; verify details against the original report.</div>';
  }

  function ptWcrCompletedDepthMapLegendHtml() {
    return '' +
      '<div style="font-weight:700;margin-bottom:2px;">DWR WCR completed depth</div>' +
      '<div style="font-size:11px;color:#4f5659;margin-bottom:5px;">Fill color = reported completed depth.</div>' +
      '<div style="display:grid;grid-template-columns:repeat(2,minmax(105px,1fr));gap:3px 12px;">' +
      ptWcrCompletedDepthLegendRowsHtml(13, '#686868') +
      '</div>' +
      '<div style="font-size:10.5px;color:#606060;margin-top:5px;line-height:1.25;">Screening-level WCR index; verify location and construction fields against the report.</div>';
  }

  function ptWcrInterpretationNoteHtml(props, options) {
    if (!ptIsWcrCompletedDepthStyle(options)) return '';

    return '<div style="margin-top:7px;padding:5px 6px;border-left:3px solid #7b3294;background:#fbf7ff;font-size:11px;color:#3f2a4f;">' +
      '<b>BRIM note:</b> WCR index points and completion fields are screening-level. Verify location, depth, and construction details against the original WCR report.' +
      '</div>';
  }

  function ptFlowCfsRadius(value) {
    var flow = Number(value);

    if (!isFinite(flow) || flow < 0) return 4.2;
    if (flow === 0) return 3.8;
    if (flow < 1) return 4.5;
    if (flow < 10) return 5.4;
    if (flow < 100) return 6.6;
    if (flow < 1000) return 8.2;
    if (flow < 10000) return 10.1;
    if (flow < 100000) return 12.4;
    if (flow < 500000) return 15.0;
    return 18.0;
  }

  function ptFlowCfsColor(value) {
    var flow = Number(value);

    if (!isFinite(flow) || flow < 0) return '#c7c7c7';
    if (flow === 0) return '#f7f7f7';
    if (flow < 1) return '#dadaeb';
    if (flow < 10) return '#9e9ac8';
    if (flow < 100) return '#4292c6';
    if (flow < 1000) return '#41b6c4';
    if (flow < 10000) return '#31a354';
    if (flow < 100000) return '#fdae61';
    if (flow < 500000) return '#d73027';
    return '#7a0177';
  }

  function ptStorageAfRadius(value) {
    var af = Number(value);

    if (!isFinite(af) || af < 0) return 5;
    if (af < 10000) return 5.5;
    if (af < 100000) return 7;
    if (af < 1000000) return 9.5;
    if (af < 5000000) return 12.5;
    return 15.5;
  }

  function ptStorageAfColor(value) {
    var af = Number(value);

    if (!isFinite(af) || af < 0) return '#c7c7c7';
    if (af < 10000) return '#deebf7';
    if (af < 100000) return '#9ecae1';
    if (af < 1000000) return '#4292c6';
    if (af < 5000000) return '#08519c';
    return '#08306b';
  }

  function ptStreamGaugeStatusOutlineColor(statusValue, fallbackColor) {
    var status = ptCleanText(statusValue).toLowerCase();

    if (status.indexOf('major') >= 0) return '#7f0000';
    if (status.indexOf('moderate') >= 0) return '#b50000';
    if (status.indexOf('minor') >= 0) return '#d95f0e';
    if (status.indexOf('action') >= 0) return '#b8860b';
    if (status.indexOf('low') >= 0) return '#6a3d9a';

    return fallbackColor || '#555555';
  }

  function ptStreamGaugeHasImportantStatus(statusValue) {
    var status = ptCleanText(statusValue).toLowerCase();

    return status.indexOf('major') >= 0 ||
      status.indexOf('moderate') >= 0 ||
      status.indexOf('minor') >= 0 ||
      status.indexOf('action') >= 0 ||
      status.indexOf('low') >= 0;
  }

  function ptIsNwmStreamAnalysisLayer(options, url) {
    options = options || {};

    var text = (
      ptCleanText(options.layerName || options.name) + ' ' +
      ptCleanText(options.styleLegendTitle) + ' ' +
      ptCleanText(options.fieldCurationNotes) + ' ' +
      ptCleanText(url)
    ).toLowerCase();

    return text.indexOf('nwm_stream_analysis') >= 0 ||
      text.indexOf('nwm streamflow') >= 0 ||
      text.indexOf('national water model') >= 0;
  }

  function ptNwmStreamAnalysisLayerIds(options) {
    options = options || {};

    var text = (
      ptCleanText(options.layerName || options.name) + ' ' +
      ptCleanText(options.styleLegendTitle) + ' ' +
      ptCleanText(options.fieldCurationNotes)
    ).toLowerCase();

    // The NWM Stream Analysis MapServer uses scale-dependent leaf sublayers.
    // Dynamic map export is much more reliable when PT2 requests the leaf
    // streamflow sublayers explicitly rather than relying on the parent group
    // layer to draw through Esri Leaflet.
    if (text.indexOf('anomaly') >= 0) {
      return [15, 16, 17, 18, 19, 20];
    }

    return [1, 2, 3, 4, 5, 6];
  }

  function ptStreamGaugeFlowLegendHtml() {
    return '<div class="pt-tools-muted"><b>Live gage style:</b> stream/canal gages are sized and colored by raw, unnormalized flow magnitude: ' +
      '<span style="display:inline-block;width:9px;height:9px;border-radius:50%;background:#f7f7f7;border:1px solid #555;margin-left:3px;"></span> 0 ' +
      '<span style="display:inline-block;width:9px;height:9px;border-radius:50%;background:#dadaeb;border:1px solid #555;margin-left:3px;"></span> &lt;1 ' +
      '<span style="display:inline-block;width:9px;height:9px;border-radius:50%;background:#9e9ac8;border:1px solid #555;margin-left:3px;"></span> 1–10 ' +
      '<span style="display:inline-block;width:9px;height:9px;border-radius:50%;background:#4292c6;border:1px solid #555;margin-left:3px;"></span> 10–100 ' +
      '<span style="display:inline-block;width:9px;height:9px;border-radius:50%;background:#41b6c4;border:1px solid #555;margin-left:3px;"></span> 100–1k ' +
      '<span style="display:inline-block;width:9px;height:9px;border-radius:50%;background:#31a354;border:1px solid #555;margin-left:3px;"></span> 1k–10k ' +
      '<span style="display:inline-block;width:9px;height:9px;border-radius:50%;background:#fdae61;border:1px solid #555;margin-left:3px;"></span> 10k–100k ' +
      '<span style="display:inline-block;width:9px;height:9px;border-radius:50%;background:#d73027;border:1px solid #555;margin-left:3px;"></span> 100k–500k ' +
      '<span style="display:inline-block;width:9px;height:9px;border-radius:50%;background:#7a0177;border:1px solid #555;margin-left:3px;"></span> 500k+ cfs. ' +
      'These colors show magnitude only, not percentile/normal condition. ' +
      '<span style="display:inline-block;width:10px;height:10px;background:#4292c6;border:2px solid #08306b;margin-left:3px;vertical-align:-1px;"></span> likely reservoir/lake pool record; provider flow_cfs is retained but not treated as current discharge. ' +
      '<span style="display:inline-block;width:10px;height:10px;background:#fff;border:2px dashed #2b8cbe;margin-left:3px;vertical-align:-1px;"></span> reservoir/lake/dam-adjacent or questionable record. ' +
      'Flood/action/low-flow status is emphasized by marker outline when available.</div>';
  }


  function ptIsSpcForecastStyle(options) {
    options = options || {};

    var layerName = ptCleanText(options.layerName || options.name).toLowerCase();
    var styleMethod = ptCleanText(options.defaultStyleMethod).toLowerCase();

    return styleMethod.indexOf('spc') >= 0 ||
      layerName.indexOf('spc fire wx') >= 0 ||
      layerName.indexOf('spc fire weather') >= 0 ||
      layerName.indexOf('spc convective') >= 0 ||
      layerName.indexOf('spc mesoscale') >= 0;
  }

  function ptSpcForecastColorFromCategory(category, props, options) {
    // Client-side style fallback for SPC layers when BRIM uses current-view
    // MapServer query mode instead of the more fragile server-rendered
    // exportImage/visual MapServer mode.  This is not intended to replace the
    // official SPC renderer exactly; it preserves the basic category hierarchy
    // and avoids blank overlays when NOAA ArcGIS image exports intermittently
    // return HTTP 400.
    var c = ptCleanText(category).toLowerCase();
    var layerName = ptCleanText((options || {}).layerName || (options || {}).name).toLowerCase();
    var dn = Number(ptFieldValue(props || {}, 'dn'));

    if (layerName.indexOf('convective') >= 0) {
      if (c.indexOf('general') >= 0 || c === 'tstm') return '#c7e9c0';
      if (c.indexOf('marginal') >= 0 || c === 'mrgl') return '#74c476';
      if (c.indexOf('slight') >= 0 || c === 'slgt') return '#f6e05e';
      if (c.indexOf('enhanced') >= 0 || c === 'enh') return '#f59e0b';
      if (c.indexOf('moderate') >= 0 || c === 'mdt') return '#ef4444';
      if (c.indexOf('high') >= 0 || c === 'high') return '#ec4899';
    }

    if (layerName.indexOf('dry thunder') >= 0) {
      if (c.indexOf('scattered') >= 0 || dn === 8) return '#f59e0b';
      if (c.indexOf('isolated') >= 0 || dn === 5) return '#facc15';
    }

    if (layerName.indexOf('fire wx') >= 0 || layerName.indexOf('fire weather') >= 0) {
      if (c.indexOf('extreme') >= 0 || dn === 10) return '#c026d3';
      if (c.indexOf('critical') >= 0 || dn === 8 || c.indexOf('70%') >= 0) return '#ef4444';
      if (c.indexOf('elevated') >= 0 || dn === 5) return '#d97706';
      if (c.indexOf('marginal') >= 0 || c.indexOf('40%') >= 0 || c.indexOf('10%') >= 0) return '#f59e0b';
    }

    return '#6b7280';
  }


  function ptSpcForecastSeverityRank(category, props, options) {
    var c = ptCleanText(category).toLowerCase();
    var layerName = ptCleanText((options || {}).layerName || (options || {}).name).toLowerCase();
    var dn = Number(ptFieldValue(props || {}, 'dn'));

    if (layerName.indexOf('convective') >= 0) {
      if (c.indexOf('high') >= 0) return 6;
      if (c.indexOf('moderate') >= 0 || c === 'mdt') return 5;
      if (c.indexOf('enhanced') >= 0 || c === 'enh') return 4;
      if (c.indexOf('slight') >= 0 || c === 'slgt') return 3;
      if (c.indexOf('marginal') >= 0 || c === 'mrgl') return 2;
      if (c.indexOf('general') >= 0 || c === 'tstm') return 1;
      return 0;
    }

    if (layerName.indexOf('dry thunder') >= 0) {
      if (c.indexOf('scattered') >= 0 || dn === 8) return 3;
      if (c.indexOf('isolated') >= 0 || dn === 5) return 2;
      return 0;
    }

    if (layerName.indexOf('fire wx') >= 0 || layerName.indexOf('fire weather') >= 0) {
      if (c.indexOf('extreme') >= 0 || dn === 10) return 5;
      if (c.indexOf('critical') >= 0 || dn === 8 || c.indexOf('70%') >= 0) return 4;
      if (c.indexOf('elevated') >= 0 || dn === 5) return 3;
      if (c.indexOf('marginal') >= 0 || c.indexOf('40%') >= 0 || c.indexOf('10%') >= 0) return 2;
      return 0;
    }

    if (isFinite(dn)) return dn;
    return 0;
  }

  function ptSpcForecastPaneName(rank) {
    rank = Number(rank);
    if (!isFinite(rank)) rank = 0;
    rank = Math.max(0, Math.min(10, Math.round(rank)));
    return 'pane_pt_spc_rank_' + String(rank);
  }

  function ptEnsureSpcForecastPanes() {
    // SPC polygons can overlap. Draw lower-severity areas first and higher
    // categories above them so critical/moderate/high areas do not disappear.
    for (var i = 0; i <= 10; i++) {
      ptEnsurePane('pane_pt_spc_rank_' + String(i), 540 + i, 'auto');
    }
  }

  function ptSpcForecastLayerFromFeatureCollection(featureCollection, color, fillOpacity, options, clickable) {
    options = options || {};
    featureCollection = featureCollection || {
      type: 'FeatureCollection',
      features: []
    };

    ptEnsureSpcForecastPanes();

    var features = Array.isArray(featureCollection.features) ? featureCollection.features : [];
    var group = L.layerGroup();

    for (var rank = 0; rank <= 10; rank++) {
      var subset = features.filter(function(feature) {
        var props = feature && feature.properties ? feature.properties : {};
        var category = ptSpcCategoryValue(props, options);
        return Math.round(ptSpcForecastSeverityRank(category, props, options)) === rank;
      });

      if (!subset.length) continue;

      var layer = L.geoJSON({
        type: 'FeatureCollection',
        features: subset
      }, {
        pane: ptSpcForecastPaneName(rank),
        style: function(feature) {
          return ptStyleForExternalFeature(feature, color, fillOpacity, options);
        },
        pointToLayer: ptPointForColor(color, options),
        onEachFeature: ptOnEachFeature(clickable, options)
      });

      group.addLayer(layer);
    }

    return group;
  }

  function ptSpcForecastStyle(feature, color, options) {
    var props = feature && feature.properties ? feature.properties : {};
    var category = ptSpcCategoryValue(props, options);
    var fill = ptSpcForecastColorFromCategory(category, props, options);

    return {
      pane: ptSpcForecastPaneName(ptSpcForecastSeverityRank(category, props, options)),
      color: fill,
      weight: 1.3,
      opacity: 0.85,
      fillColor: fill,
      fillOpacity: 0.28
    };
  }

  function ptStyleForExternalFeature(feature, color, fillOpacity, options) {
    options = options || {};

    if (ptIsUicAquiferExemptionStyle(options)) {
      if (window.BRIM && window.BRIM.uicExplorer &&
          window.BRIM.uicExplorer.featureStyle) {
        return window.BRIM.uicExplorer.featureStyle(options, feature) || {};
      }
      return {};
    }

    // For point layers whose markers are fully styled in pointToLayer(), do not
    // return a generic GeoJSON style here. Leaflet applies the GeoJSON style to
    // CircleMarkers after pointToLayer(), which would overwrite flow-bin
    // fillColor/radius choices with the external layer's default blue swatch.
    if (
      ptIsPointFeature(feature) &&
      (ptIsStreamGaugeFlowStyle(options) || ptIsWcrCompletedDepthStyle(options) || ptIsAirNowAqiStyle(options) || ptIsUsgsEarthquakeStyle(options) || ptIsSubsidenceObservationStyle(options))
    ) {
      return {};
    }

    if (ptIsSpcForecastStyle(options)) {
      return ptSpcForecastStyle(feature, color, options);
    }

    if (ptIsDroughtMonitorStyle(options)) {
      var props = feature && feature.properties ? feature.properties : {};
      var dm = ptFieldValue(props, 'DM');
      var dmColor = ptDroughtMonitorColor(dm);

      return {
        pane: ptDroughtMonitorPaneName(dm),
        color: '#555555',
        weight: 0.35,
        opacity: 0.30,
        fillColor: dmColor,
        // More transparent than the original 0.66 so basemap, BLM land,
        // roads, streams, and local overlays remain readable underneath.
        fillOpacity: 0.46
      };
    }

    if (ptIsCpcOutlookStyle(options)) {
      var cpcProps = feature && feature.properties ? feature.properties : {};
      var cpcColor = ptCpcOutlookColor(
        ptFieldValue(cpcProps, 'cat'),
        ptFieldValue(cpcProps, 'prob'),
        options
      );

      return {
        pane: ptCpcOutlookPaneName(ptCpcOutlookIntensityRank(cpcProps)),
        color: '#666666',
        weight: 0.5,
        opacity: 0.55,
        fillColor: cpcColor,
        fillOpacity: 0.64
      };
    }

    if (ptIsSwrcbIrListingStatusStyle(options)) {
      var irProps = feature && feature.properties ? feature.properties : {};
      var irColor = ptSwrcbIrListingStatusColor(ptSwrcbIrListingStatusValue(irProps));

      return {
        pane: 'pane_pt_custom_polygon',
        color: irColor,
        weight: 2.0,
        opacity: 0.92,
        fillColor: irColor,
        fillOpacity: 0.30
      };
    }

    if (ptIsNifcCurrentFirePerimeterStyle(options)) {
      var nifcProps = feature && feature.properties ? feature.properties : {};
      var nifcColor = ptNifcCurrentFireColor(nifcProps);

      return {
        pane: 'pane_pt_custom_polygon',
        color: nifcColor === '#7b3294' ? '#4a1486' : '#99000d',
        weight: 1.6,
        opacity: 0.98,
        fillColor: nifcColor,
        fillOpacity: 0.58
      };
    }

    if (ptIsFireYearStyle(options)) {
      var fireProps = feature && feature.properties ? feature.properties : {};
      var fireYear = ptFireYearValue(fireProps);
      var fireColor = ptFireYearColor(fireYear);

      return {
        pane: 'pane_pt_custom_polygon',
        color: '#4d2a1d',
        weight: 0.75,
        opacity: 0.78,
        fillColor: fireColor,
        fillOpacity: 0.50
      };
    }

    if (ptIsMlrsMineralCaseStyle(options)) {
      var mlrsProps = feature && feature.properties ? feature.properties : {};
      var mlrsDisp = ptMlrsCaseDispositionValue(mlrsProps);
      var mlrsFill = ptMlrsStatusColor(mlrsDisp, options);
      var mlrsOutline = ptMlrsOutlineColor(mlrsProps, options);
      var mlrsStatus = ptMlrsStatusClass(mlrsDisp, options);

      return {
        pane: 'pane_pt_custom_polygon',
        color: mlrsOutline,
        weight: mlrsStatus === 'closed' ? 1.05 : 1.25,
        opacity: 0.92,
        fillColor: mlrsFill,
        fillOpacity: 0.34,
        dashArray: null
      };
    }

    if (ptIsAirNowAqiStyle(options)) {
      var airProps = feature && feature.properties ? feature.properties : {};
      var airValue = ptAirNowAqiValue(airProps, options);
      var airColor = ptAirNowAqiColor(airValue, airProps, options);

      return {
        pane: 'pane_pt_custom_polygon',
        color: '#555555',
        weight: 0.25,
        opacity: 0.32,
        fillColor: airColor,
        fillOpacity: 0.38
      };
    }

    if (ptIsAmlFeatureStyle(options)) {
      var amlProps = feature && feature.properties ? feature.properties : {};
      var amlStatus = ptAmlStatusValue(amlProps);
      var amlColor = ptAmlStatusColor(amlStatus);
      var amlOutline = ptAmlStatusOutlineColor(amlStatus);

      return {
        pane: 'pane_pt_custom_polygon',
        color: amlOutline,
        weight: 1.25,
        opacity: 0.94,
        fillColor: amlColor,
        fillOpacity: 0.42
      };
    }

    if (ptIsCalIpcRampStyle(options)) {
      var calProps = feature && feature.properties ? feature.properties : {};
      var calValue = ptCalIpcNumericValue(calProps, options);
      var calColor = ptIsCalIpcSpeciesCountStyle(options) ?
        ptCalIpcSpeciesCountColor(calValue) :
        ptCalIpcThreatColor(calValue);

      return {
        pane: 'pane_pt_custom_polygon',
        color: '#4d4d4d',
        weight: 0.65,
        opacity: 0.62,
        fillColor: calColor,
        fillOpacity: 0.58
      };
    }

    if (ptIsGenericCategoricalStyle(options)) {
      var catProps = feature && feature.properties ? feature.properties : {};
      var catValue = ptGenericCategoricalValue(catProps, options);
      var catColor = ptGenericCategoricalColor(catValue);

      return {
        pane: 'pane_pt_custom_polygon',
        color: catColor,
        weight: 0.9,
        opacity: 0.86,
        fillColor: catColor,
        fillOpacity: 0.34
      };
    }

    if (ptIsAlertCameraViewshedStyle(options)) {
      return {
        pane: 'pane_pt_custom_polygon',
        color: '#0284c7',
        weight: 0.8,
        opacity: 0.55,
        fillColor: '#38bdf8',
        fillOpacity: 0.14
      };
    }

    return ptStyleForColor(color, fillOpacity);
  }

  function ptPointForColor(color, options) {
    options = options || {};

    return function(feature, latlng) {
      var props = feature && feature.properties ? feature.properties : {};

      if (ptIsUicReferencePointStyle(options) &&
          window.BRIM && window.BRIM.uicExplorer &&
          window.BRIM.uicExplorer.pointStyle) {
        var uicPointStyle = window.BRIM.uicExplorer.pointStyle(options);
        if (uicPointStyle) return L.circleMarker(latlng, uicPointStyle);
      }

      if (ptIsAlertCameraStyle(options) && !ptIsAlertCameraViewshedStyle(options)) {
        var pan = Number(ptFieldValue(props, 'positionPan'));
        var online = ptAlertCameraIsOnline(props);
        var arrowColor = online ? '#38bdf8' : '#bdbdbd';
        var outlineColor = online ? '#111827' : '#555555';
        var opacity = online ? 0.92 : 0.70;
        var angle = isFinite(pan) ? pan : 0;
        var size = online ? 18 : 15;
        var half = Math.round(size * 0.42);

        return L.marker(latlng, {
          pane: 'pane_pt_custom_point',
          icon: L.divIcon({
            className: 'pt-alert-camera-direction-marker',
            iconSize: [size + 8, size + 8],
            iconAnchor: [(size + 8) / 2, (size + 8) / 2],
            html: '<span style="display:block;width:0;height:0;border-left:' + half + 'px solid transparent;border-right:' + half + 'px solid transparent;border-bottom:' + size + 'px solid ' + arrowColor + ';filter:drop-shadow(0 0 1px ' + outlineColor + ');opacity:' + opacity + ';transform:rotate(' + angle + 'deg);transform-origin:50% 65%;"></span>'
          })
        });
      }

      if (ptIsWcrCompletedDepthStyle(options)) {
        var depth = ptWcrCompletedDepthValue(props);
        var fillColor = ptWcrCompletedDepthColor(depth);
        var hasDepth = isFinite(depth);
        var outlineColor = hasDepth ? '#5f676b' : '#777777';

        return L.circleMarker(latlng, {
          pane: 'pane_pt_custom_point',
          radius: 5.6,
          color: outlineColor,
          weight: hasDepth ? 0.9 : 1.1,
          opacity: 0.88,
          fillColor: fillColor,
          fillOpacity: hasDepth ? 0.82 : 0.42,
          dashArray: hasDepth ? null : '2,2'
        });
      }

      if (ptIsStreamGaugeFlowStyle(options)) {
        var flow = ptFieldValue(props, 'flow_cfs');
        var status = ptFieldValue(props, 'status');
        var importantStatus = ptStreamGaugeHasImportantStatus(status);

        var gaugeClass = ptStreamGaugeRecordClass(props);

        if (gaugeClass === 'reservoir') {
          var storageOutline = ptStreamGaugeStatusOutlineColor(status, '#08306b');
          var storageSize = 12;
          var storageBorder = importantStatus ? storageOutline : '#08306b';
          var storageBorderWidth = importantStatus ? 3 : 2;

          return L.marker(latlng, {
            pane: 'pane_pt_custom_point',
            icon: L.divIcon({
              className: 'pt-stream-reservoir-square-marker',
              iconSize: [storageSize + 2, storageSize + 2],
              iconAnchor: [(storageSize + 2) / 2, (storageSize + 2) / 2],
              html: '<span style="display:block;width:' + storageSize + 'px;height:' + storageSize + 'px;background:#4292c6;border:' + storageBorderWidth + 'px solid ' + storageBorder + ';box-sizing:border-box;opacity:0.88;"></span>'
            })
          });
        }

        if (gaugeClass === 'questionable') {
          var questionOutline = ptStreamGaugeStatusOutlineColor(status, '#2b8cbe');
          var questionSize = 11;
          var questionBorderWidth = importantStatus ? 3 : 2;

          return L.marker(latlng, {
            pane: 'pane_pt_custom_point',
            icon: L.divIcon({
              className: 'pt-stream-questionable-square-marker',
              iconSize: [questionSize + 3, questionSize + 3],
              iconAnchor: [(questionSize + 3) / 2, (questionSize + 3) / 2],
              html: '<span style="display:block;width:' + questionSize + 'px;height:' + questionSize + 'px;background:rgba(255,255,255,0.68);border:' + questionBorderWidth + 'px dashed ' + questionOutline + ';box-sizing:border-box;opacity:0.94;"></span>'
            })
          });
        }

        var fill = ptFlowCfsColor(flow);
        var outline = ptStreamGaugeStatusOutlineColor(status, importantStatus ? '#333333' : '#555555');

        return L.circleMarker(latlng, {
          pane: 'pane_pt_custom_point',
          radius: ptFlowCfsRadius(flow),
          color: outline,
          weight: importantStatus ? 2.6 : 1.2,
          opacity: 0.96,
          fillColor: fill,
          fillOpacity: 0.72
        });
      }

      if (ptIsUsgsEarthquakeStyle(options)) {
        var eqFill = ptUsgsEarthquakeColor(props);
        var eqRadius = ptUsgsEarthquakeRadius(props);

        return L.circleMarker(latlng, {
          pane: 'pane_pt_custom_point',
          radius: eqRadius,
          color: '#4a1f1f',
          weight: 1.1,
          opacity: 0.92,
          fillColor: eqFill,
          fillOpacity: 0.78
        });
      }

      if (ptIsAirNowAqiStyle(options)) {
        var airValue = ptAirNowAqiValue(props, options);
        var airFill = ptAirNowAqiColor(airValue, props, options);
        var noAirData = !isFinite(Number(airValue)) || Number(airValue) < 0;

        return L.circleMarker(latlng, {
          pane: 'pane_pt_custom_point',
          radius: noAirData ? 5.5 : 6.8,
          color: '#222222',
          weight: noAirData ? 0.9 : 1.2,
          opacity: 0.90,
          fillColor: airFill,
          fillOpacity: noAirData ? 0.55 : 0.86
        });
      }

      if (ptIsMlrsMineralCaseStyle(options)) {
        var mlrsDisp = ptMlrsCaseDispositionValue(props);
        var mlrsFill = ptMlrsStatusColor(mlrsDisp, options);
        var mlrsOutline = ptMlrsOutlineColor(props, options);
        var mlrsStatus = ptMlrsStatusClass(mlrsDisp, options);

        return L.circleMarker(latlng, {
          pane: 'pane_pt_custom_point',
          radius: 5.2,
          color: mlrsOutline,
          weight: 1.4,
          opacity: 0.93,
          fillColor: mlrsFill,
          fillOpacity: 0.80,
          dashArray: null
        });
      }

      if (ptIsAmlFeatureStyle(options)) {
        var amlStatus = ptAmlStatusValue(props);
        var amlFill = ptAmlStatusColor(amlStatus);
        var amlOutline = ptAmlStatusOutlineColor(amlStatus);

        return L.circleMarker(latlng, {
          pane: 'pane_pt_custom_point',
          radius: 6.5,
          color: amlOutline,
          weight: 1.8,
          opacity: 0.98,
          fillColor: amlFill,
          fillOpacity: 0.90
        });
      }

      if (ptIsCalIpcRampStyle(options)) {
        var calVal = ptCalIpcNumericValue(props, options);
        var calFill = ptIsCalIpcSpeciesCountStyle(options) ?
          ptCalIpcSpeciesCountColor(calVal) :
          ptCalIpcThreatColor(calVal);

        return L.circleMarker(latlng, {
          pane: 'pane_pt_custom_point',
          radius: 5,
          color: '#555555',
          weight: 1.0,
          opacity: 0.92,
          fillColor: calFill,
          fillOpacity: 0.82
        });
      }

      if (ptIsSubsidenceObservationStyle(options)) {
        return ptSubsidenceObservationMarker(feature, latlng);
      }

      if (ptIsGenericCategoricalStyle(options)) {
        var catValue = ptGenericCategoricalValue(props, options);
        var catColor = ptGenericCategoricalColor(catValue);

        return L.circleMarker(latlng, {
          pane: 'pane_pt_custom_point',
          radius: 5,
          color: '#555555',
          weight: 0.9,
          opacity: 0.90,
          fillColor: catColor,
          fillOpacity: 0.78
        });
      }

      return L.circleMarker(latlng, {
        pane: 'pane_pt_custom_point',
        radius: 5,
        color: color,
        weight: 1.6,
        opacity: 0.95,
        fillColor: color,
        fillOpacity: 0.55
      });
    };
  }

  function ptExternalOverlayTooltipClass(options) {
    options = options || {};

    return 'pt-external-overlay-tooltip' +
      (ptIsMlrsMineralCaseStyle(options) ? ' pt-mlrs-tooltip' : '') +
      (ptIsCarbAirContextStyle(options) ? ' pt-carb-air-tooltip' : '') +
      (ptIsAirNowAqiStyle(options) ? ' pt-airnow-tooltip' : '');
  }

  function ptExternalLeafletPopupOptions(options, baseOptions) {
    options = options || {};
    baseOptions = baseOptions || {};

    if (ptIsMlrsMineralCaseStyle(options)) {
      return Object.assign({}, baseOptions, {
        maxWidth: 520,
        minWidth: 285,
        className: 'pt-mlrs-leaflet-popup'
      });
    }

    if (ptIsAirNowAqiStyle(options)) {
      return Object.assign({}, baseOptions, {
        maxWidth: 440,
        minWidth: 245,
        className: 'pt-airnow-leaflet-popup'
      });
    }

    if (ptIsCarbAirContextStyle(options)) {
      return Object.assign({}, baseOptions, {
        maxWidth: 380,
        minWidth: 140,
        className: 'pt-carb-air-leaflet-popup'
      });
    }

    return Object.assign({}, baseOptions, {
      maxWidth: 460,
      minWidth: 220,
      className: 'pt-external-generic-leaflet-popup'
    });
  }

  function ptOnEachFeature(clickable, options) {
    options = options || {};

    return function(feature, layer) {
      var props = feature ? feature.properties : {};

      if (ptIsUicAquiferExemptionStyle(options) &&
          layer && layer.setStyle &&
          window.BRIM && window.BRIM.uicExplorer) {
        layer.on('mouseover', function() {
          var style = window.BRIM.uicExplorer.hoverStyle ?
            window.BRIM.uicExplorer.hoverStyle(options, feature) : null;
          if (!style) return;
          layer.setStyle(style);
          if (style.radius && layer.setRadius) layer.setRadius(style.radius);
          if (layer.bringToFront) layer.bringToFront();
        });
        layer.on('mouseout', function() {
          var style = window.BRIM.uicExplorer.featureStyle ?
            window.BRIM.uicExplorer.featureStyle(options, feature) : null;
          if (!style) return;
          layer.setStyle(style);
          if (style.radius && layer.setRadius) layer.setRadius(style.radius);
        });
      }

      var tooltipHtml = ptTooltipFromProperties(props, options);

      if (tooltipHtml) {
        layer.bindTooltip(tooltipHtml, {
          sticky: true,
          direction: 'auto',
          opacity: 0.95,
          className: ptExternalOverlayTooltipClass(options)
        });
      }

      if (clickable) {
        if (ptIsMlrsMineralCaseStyle(options)) {
          layer.on('click', function(e) {
            if (e && e.originalEvent && L.DomEvent) {
              L.DomEvent.stop(e.originalEvent);
            }
            ptOpenMlrsAggregatePopup(e && e.latlng ? e.latlng : null, props, options);
          });
        } else {
          layer.bindPopup(
            ptPopupFromProperties(props, options),
            ptExternalLeafletPopupOptions(options)
          );
        }
      }
    };
  }

  function ptAddCustomRecord(name, layer, color, sourceType, url, options) {
    ptCustomLayerSeq += 1;
    options = options || {};

    var rec = {
      id: 'pt_custom_' + ptCustomLayerSeq,
      name: name || ('Custom layer ' + ptCustomLayerSeq),
      layer: layer,
      color: color,
      sourceType: sourceType,
      url: url,
      visible: true,
      legendUrl: ptCleanText(options.legendUrl),
      legendNote: ptCleanText(options.legendNote),
      legendAdapter: ptCleanText(options.legendAdapter),
      infoAdapter: ptCleanText(options.infoAdapter),
      loadMode: ptCleanText(options.loadMode),
      whereClause: ptCleanText(options.whereClause),
      popupFields: ptCleanText(options.popupFields),
      popupAliases: ptCleanText(options.popupAliases),
      popupLinkTemplate: ptCleanText(options.popupLinkTemplate),
      popupLinkLabel: ptCleanText(options.popupLinkLabel),
      identifyUrl: ptCleanText(options.identifyUrl),
      identifyServiceUrl: ptCleanText(options.identifyServiceUrl),
      identifyLayerId: options.identifyLayerId !== undefined && options.identifyLayerId !== null && options.identifyLayerId !== '' ? Number(options.identifyLayerId) : null,
      identifyMode: ptCleanText(options.identifyMode),
      visualIdentify: !!options.visualIdentify,
      hoverFields: ptCleanText(options.hoverFields),
      hoverAliases: ptCleanText(options.hoverAliases),
      hoverBoldFields: ptCleanText(options.hoverBoldFields),
      hoverNoLabelFields: ptCleanText(options.hoverNoLabelFields),
      hoverRoundFields: ptCleanText(options.hoverRoundFields),
      hoverShowNativeFieldNames: ptCleanText(options.hoverShowNativeFieldNames),
      outFields: ptCleanText(options.outFields),
      styleFieldCandidates: ptCleanText(options.styleFieldCandidates),
      defaultStyleField: ptCleanText(options.defaultStyleField),
      defaultStyleMethod: ptCleanText(options.defaultStyleMethod),
      styleUnits: ptCleanText(options.styleUnits),
      styleLegendTitle: ptCleanText(options.styleLegendTitle),
      styleDirection: ptCleanText(options.styleDirection),
      defaultLabelField: ptCleanText(options.defaultLabelField),
      fieldCurationNotes: ptCleanText(options.fieldCurationNotes),
      showNativeFieldNames: ptCleanText(options.showNativeFieldNames),
      clickable: !!options.clickable,
      featureCount: options.featureCount !== undefined && options.featureCount !== null ? Number(options.featureCount) : null,
      refreshing: false,
      catalogIndex: options.catalogIndex !== undefined && options.catalogIndex !== null ? Number(options.catalogIndex) : null,
      catalogExtId: ptCleanText(options.catalogExtId),
      catalogDisplayNum: ptCleanText(options.catalogDisplayNum),
      loadBadge: ptCleanText(options.loadBadge),
      loadScore: ptCleanText(options.loadScore),
      loadNote: ptCleanText(options.loadNote),
      loadAuditBasis: ptCleanText(options.loadAuditBasis),
      loadNTests: ptCleanText(options.loadNTests),
      loadSuccessRate: ptCleanText(options.loadSuccessRate),
      loadMedianSeconds: ptCleanText(options.loadMedianSeconds),
      loadP90Seconds: ptCleanText(options.loadP90Seconds),
      loadMaxSeconds: ptCleanText(options.loadMaxSeconds),
      loadFeatureCapRate: ptCleanText(options.loadFeatureCapRate),
      retrievalUtc: ptCleanText(options.retrievalUtc),
      sourcePage: ptCleanText(options.sourcePage),
      sourceServiceUrl: ptCleanText(options.sourceServiceUrl || url),
      catalogKey: ptCleanText(options.catalogKey),
      catalogLoadToken: ptCleanText(options.catalogLoadToken),
      opsPromotedKey: ptCleanText(options.opsPromotedKey),
      opsPromotedSourceName: ptCleanText(options.opsPromotedSourceName),
      opsPromotedDisplayName: ptCleanText(options.opsPromotedDisplayName),
      opsPromotedGeneration: options.opsPromotedGeneration !== undefined && options.opsPromotedGeneration !== null ? Number(options.opsPromotedGeneration) : null,
      subsidenceObservationClassCounts: options.subsidenceObservationClassCounts || null
    };

    // If a slow quick-add catalog request was cancelled while an async
    // current-view query was still in flight, ignore the late result instead
    // of silently adding a layer after the user moved on.
    if (rec.catalogIndex !== null && !isNaN(rec.catalogIndex) &&
        rec.catalogLoadToken &&
        ptCatalogLoadWasCancelled(rec.catalogIndex, rec.catalogLoadToken)) {
      rec.catalogLoadSkipped = true;
      rec.opsPromotedSkipped = true;
      return rec;
    }

    // Ops Live can promote selected curated External Layers catalog rows into
    // one-click operational checkboxes.  If the user turns the Ops checkbox off
    // before an async current-view query returns, do not add the late layer.
    if (rec.opsPromotedKey) {
      var currentGen = ptOpsPromotedGeneration[rec.opsPromotedKey] || 0;
      if (ptOpsPromotedCancelled[rec.opsPromotedKey] ||
          (rec.opsPromotedGeneration !== null && rec.opsPromotedGeneration !== currentGen)) {
        rec.opsPromotedSkipped = true;
        return rec;
      }
    }

    if (!rec.legendUrl) {
      rec.legendUrl = ptDeriveLegendUrl(url, sourceType.toLowerCase().indexOf('mapserver') >= 0 ? 'map' : '');
    }

    ptCustomLayers.push(rec);
    layer.addTo(map);
    ptRenderCustomLayerList();
    ptRenderQuickCatalog();
    ptUpdateWcrCompletedDepthMapLegend();
    ptUpdateMlrsMineralCasesMapLegend();
    ptUpdateSgmaPrioritizationMapLegend();
    ptUpdateSubsidenceObservationMapLegend();
    if (window.BRIM && window.BRIM.uicExplorer &&
        window.BRIM.uicExplorer.isUicCatalogId(rec.catalogExtId)) {
      window.BRIM.uicExplorer.upsertExternal(rec);
    }
    return rec;
  }

  function ptRemoveCustomLayer(id) {
    var keep = [];

    ptCustomLayers.forEach(function(rec) {
      if (rec.id === id) {
        if (ptIsRefreshableUicSnapshotRecord(rec)) {
          rec._uicRefreshGeneration = Number(rec._uicRefreshGeneration || 0) + 1;
          rec.refreshing = false;
        }
        if (rec.layer && map.hasLayer(rec.layer)) {
          map.removeLayer(rec.layer);
        }
        if (window.BRIM && window.BRIM.uicExplorer) {
          window.BRIM.uicExplorer.removeExternal(rec.catalogExtId || rec.id);
        }
      } else {
        keep.push(rec);
      }
    });

    ptCustomLayers = keep;

    if (ptLastManualCurrentViewLayerId === id) {
      ptLastManualCurrentViewLayerId = null;
    }

    ptRenderCustomLayerList();
    ptRenderQuickCatalog();
    ptUpdateManualRefreshControl();
    ptUpdateWcrCompletedDepthMapLegend();
    ptUpdateMlrsMineralCasesMapLegend();
    ptUpdateSgmaPrioritizationMapLegend();
    ptUpdateSubsidenceObservationMapLegend();
    if (ptExternalPanelLayerCount() === 0) {
      ptRestoreExternalBaseAttribution();
      ptHideVisualIdentifyTooltip();
    }
    ptSetStatus('External overlay removed.', false);
  }

  function ptClearCustomLayers() {
    /*
     * Clear external URL/catalog overlays and reset quick-add UI state.
     *
     * IMPORTANT:
     * ptSetStatus() mirrors final messages into the row that is currently
     * loading. When the user clicks the ribbon-level Clear external button,
     * that mirroring is not wanted: stale per-row success/failure notes should
     * disappear so the catalog returns to a neutral browse state.
     */
    var keep = [];

    // The special Federal Land Status / SMA overlay lives outside the normal
    // ptCustomLayers registry so it can have a fixed, prominent control and
    // opacity slider.  Clear external and clear all should still treat it as
    // an External/session overlay.
    ptClearBlmSma(true);

    ptCustomLayers.forEach(function(rec) {
      // Ops Live-promoted catalog layers are owned by the Ops Live checkbox
      // that launched them.  Clear external should not remove those map layers
      // or desynchronize the Ops checkbox state from the map.  Clear All still
      // removes Ops layers later by clicking the Ops ribbon clear button.
      if (ptIsOpsPromotedRecord(rec)) {
        keep.push(rec);
        return;
      }

      if (ptIsRefreshableUicSnapshotRecord(rec)) {
        rec._uicRefreshGeneration = Number(rec._uicRefreshGeneration || 0) + 1;
        rec.refreshing = false;
      }
      if (rec.layer && map.hasLayer(rec.layer)) {
        map.removeLayer(rec.layer);
      }
      if (window.BRIM && window.BRIM.uicExplorer) {
        window.BRIM.uicExplorer.removeExternal(rec.catalogExtId || rec.id);
      }
    });

    ptCustomLayers = keep;

    // Manual URL-added overlays are ptCustomLayers records, so they were
    // removed above. Also reset the manual-add spinner/status/refresh UI.
    ptResetManualExternalAddState();

    // Reset quick-add loading/spinner state before setting the global status so
    // the "External overlays cleared" message is not written into a catalog row.
    // Mark any in-flight quick-add request as cancelled so a late async result
    // cannot silently re-add itself after Clear external.
    if (ptCatalogLoadingIdx !== null) {
      var pendingCatalogIdx = Number(ptCatalogLoadingIdx);
      ptMarkCatalogLoadingCancelled(ptCatalogLoadingIdx);
      if (PT2_CATALOG[pendingCatalogIdx] &&
          window.BRIM && window.BRIM.uicExplorer) {
        var pendingCatalogExtId = ptCatalogField(
          PT2_CATALOG[pendingCatalogIdx],
          'external_layer_id'
        );
        if (window.BRIM.uicExplorer.isUicCatalogId(pendingCatalogExtId)) {
          window.BRIM.uicExplorer.removeExternal(pendingCatalogExtId);
        }
      }
    }
    ptClearCatalogLoading(true);
    ptClearCatalogRowNotes();
    ptClearActionNotes();

    ptRenderCustomLayerList();
    ptRenderQuickCatalog();
    ptUpdateManualRefreshControl();
    ptUpdateWcrCompletedDepthMapLegend();
    ptUpdateMlrsMineralCasesMapLegend();
    ptUpdateSgmaPrioritizationMapLegend();
    ptUpdateSubsidenceObservationMapLegend();
    if (ptExternalPanelLayerCount() === 0) {
      ptRestoreExternalBaseAttribution();
      ptHideVisualIdentifyTooltip();
    }
    ptSetStatus('External overlays cleared.', false);
  }

  function ptClearAllPt2SessionLayers() {
    // Global UI reset for user-added/session layers and temporary map state.
    // This intentionally does not change the selected basemap or map extent.

    // Stop Draw/Label placement if active. Existing drawings and labels stay
    // in place; use Draw / Label > Clear to erase annotations deliberately.
    try {
      ptSetTeachingLabelPlacementActive(false);
      ptSetTeachingMarkupMode(false);
      var teachingWrap0 = document.getElementById('pt-teaching-markup-wrap');
      if (teachingWrap0) teachingWrap0.classList.add('pt-teaching-markup-collapsed');
      var teachingTab0 = document.getElementById('pt-teaching-markup-tab');
      if (teachingTab0) teachingTab0.setAttribute('aria-expanded', 'false');
    } catch (err) {
      console.warn('PT2 Clear All: Draw mode reset failed', err);
    }

    // Stop/clear measurements first so the map is no longer in drawing mode.
    try {
      ptClearMeasurements();
    } catch (err) {
      console.warn('PT2 Clear All: measurement clear failed', err);
    }

    // Clear external catalog/manual URL overlays.
    try {
      ptClearCustomLayers();
    } catch (err) {
      console.warn('PT2 Clear All: external layer clear failed', err);
    }

    // Clear main local overlay checkboxes via the main layer-control ribbon button.
    try {
      var mainClear = document.querySelector('.pt-main-layer-clear-btn');
      if (mainClear) {
        mainClear.click();
      } else {
        var checkedOverlays = document.querySelectorAll('.leaflet-control-layers-overlays input[type="checkbox"]:checked');
        Array.prototype.forEach.call(checkedOverlays, function(input) { input.click(); });
      }
    } catch (err) {
      console.warn('PT2 Clear All: local layer clear failed', err);
    }

    // Clear uploaded local GIS files via the upload panel ribbon button, if present.
    try {
      var uploadClear = document.getElementById('pt-local-clear-ribbon-btn');
      if (uploadClear) uploadClear.click();
    } catch (err) {
      console.warn('PT2 Clear All: local upload clear failed', err);
    }

    // Clear Ops live layers via the Ops ribbon button, if present.
    try {
      var opsClear = document.getElementById('pt-ops-clear-ribbon-btn');
      if (opsClear) opsClear.click();
    } catch (err) {
      console.warn('PT2 Clear All: Ops clear failed', err);
    }

    ptSetStatus('Cleared local overlays, external layers, uploads, Ops layers, and measurements.', false);
  }

  function ptToggleCustomLayer(id, visible) {
    ptCustomLayers.forEach(function(rec) {
      if (rec.id !== id) return;

      rec.visible = visible;

      if (visible) {
        if (!map.hasLayer(rec.layer)) {
          rec.layer.addTo(map);
        }
      } else {
        if (map.hasLayer(rec.layer)) {
          map.removeLayer(rec.layer);
        }
      }
      if (window.BRIM && window.BRIM.uicExplorer) {
        window.BRIM.uicExplorer.setExternalVisible(rec.catalogExtId || rec.id, visible);
      }
    });

    ptUpdateWcrCompletedDepthMapLegend();
    ptUpdateMlrsMineralCasesMapLegend();
    ptUpdateSgmaPrioritizationMapLegend();
    ptUpdateSubsidenceObservationMapLegend();
  }

  function ptOptionsFromCustomRecord(rec) {
    rec = rec || {};

    return {
      legendUrl: rec.legendUrl || '',
      legendNote: rec.legendNote || '',
      loadMode: rec.loadMode || '',
      whereClause: rec.whereClause || '',
      popupFields: rec.popupFields || '',
      popupAliases: rec.popupAliases || '',
      popupLinkTemplate: rec.popupLinkTemplate || '',
      popupLinkLabel: rec.popupLinkLabel || '',
      identifyUrl: rec.identifyUrl || '',
      identifyServiceUrl: rec.identifyServiceUrl || '',
      identifyLayerId: rec.identifyLayerId,
      identifyMode: rec.identifyMode || '',
      visualIdentify: !!rec.visualIdentify,
      hoverFields: rec.hoverFields || '',
      hoverAliases: rec.hoverAliases || '',
      hoverBoldFields: rec.hoverBoldFields || '',
      hoverNoLabelFields: rec.hoverNoLabelFields || '',
      hoverRoundFields: rec.hoverRoundFields || '',
      hoverShowNativeFieldNames: rec.hoverShowNativeFieldNames || '',
      outFields: rec.outFields || '',
      styleFieldCandidates: rec.styleFieldCandidates || '',
      defaultStyleField: rec.defaultStyleField || '',
      defaultStyleMethod: rec.defaultStyleMethod || '',
      styleUnits: rec.styleUnits || '',
      styleLegendTitle: rec.styleLegendTitle || '',
      styleDirection: rec.styleDirection || '',
      defaultLabelField: rec.defaultLabelField || '',
      fieldCurationNotes: rec.fieldCurationNotes || '',
      showNativeFieldNames: rec.showNativeFieldNames || '',
      retrievalUtc: rec.retrievalUtc || '',
      sourcePage: rec.sourcePage || '',
      sourceServiceUrl: rec.sourceServiceUrl || rec.url || '',
      catalogExtId: rec.catalogExtId || '',
      catalogIndex: rec.catalogIndex,
      catalogKey: rec.catalogKey || '',
      catalogLoadToken: rec.catalogLoadToken || '',
      clickable: !!rec.clickable,
      layerName: rec.name || 'External GIS layer'
    };
  }


  function ptRunArcgisCurrentViewFeatureQuery(url, bounds, whereText, options, callback) {
    options = options || {};
    whereText = ptCleanText(whereText) || '1=1';

    function applyOutFields(query) {
      var outFields = ptOutFieldsArray(options);

      if (outFields.length > 0 && typeof query.fields === 'function') {
        query = query.fields(outFields);
      }

      return query;
    }

    if (ptIsCpcOutlookStyle(options)) {
      // CPC layers are small, but their probability bands can be nested or
      // returned in an order that hides classes. Query by category so one
      // broad request cannot drop an entire A/B/N/EC class, then merge and
      // draw low-to-high probability client-side.
      var cpcCats = ['A', 'B', 'N', 'EC'];
      var cpcMerged = {
        type: 'FeatureCollection',
        features: []
      };
      var cpcResponses = [];
      var cpcFirstError = null;
      var cpcIdx = 0;

      function runNextCpc() {
        if (cpcIdx >= cpcCats.length) {
          if (cpcFirstError && cpcMerged.features.length === 0) {
            callback(cpcFirstError, cpcMerged, {
              ptCpcCategoryQueries: true,
              exceededTransferLimit: cpcResponses.some(function(resp) {
                return !!(resp && resp.exceededTransferLimit);
              })
            });
            return;
          }

          callback(null, cpcMerged, {
            ptCpcCategoryQueries: true,
            exceededTransferLimit: cpcResponses.some(function(resp) {
              return !!(resp && resp.exceededTransferLimit);
            })
          });
          return;
        }

        var cat = cpcCats[cpcIdx++];
        var catWhere = '(' + whereText + ") AND cat = '" + cat + "'";
        var catQuery = L.esri.query({ url: url })
          .where(catWhere)
          .intersects(bounds);

        catQuery = applyOutFields(catQuery);
        catQuery.run(function(error, fc, response) {
          if (error && !cpcFirstError) cpcFirstError = error;
          if (response) cpcResponses.push(response);
          if (fc && Array.isArray(fc.features)) {
            cpcMerged.features = cpcMerged.features.concat(fc.features);
          }
          runNextCpc();
        });
      }

      runNextCpc();
      return;
    }

    if (!ptIsDroughtMonitorStyle(options)) {
      var query = L.esri.query({
        url: url
      })
        .where(whereText)
        // Use intersects rather than within. For large polygon services,
        // many features cross the current map extent and are not fully
        // contained by it; within() can therefore return zero or misleading
        // partial-looking results. intersects() is the better default for
        // current-view screening overlays.
        .intersects(bounds);

      query = applyOutFields(query);
      query.run(callback);
      return;
    }

    // The U.S. Drought Monitor service may contain cumulative/nested polygons
    // and can behave poorly if a broad current-view query hits a transfer limit
    // before the higher-intensity D3/D4 features are returned. Query each DM
    // category separately, then merge client-side so reds/browns are not lost
    // under broader D0-D2 areas.
    var dmClasses = [0, 1, 2, 3, 4];
    var merged = {
      type: 'FeatureCollection',
      features: []
    };
    var responses = [];
    var firstError = null;
    var idx = 0;

    function runNext() {
      if (idx >= dmClasses.length) {
        if (firstError) {
          callback(firstError, merged, {
            ptDroughtCategoryQueries: true,
            exceededTransferLimit: responses.some(function(resp) {
              return !!(resp && resp.exceededTransferLimit);
            })
          });
          return;
        }

        callback(null, merged, {
          ptDroughtCategoryQueries: true,
          exceededTransferLimit: responses.some(function(resp) {
            return !!(resp && resp.exceededTransferLimit);
          })
        });
        return;
      }

      var dm = dmClasses[idx++];
      var dmWhere = '(' + whereText + ') AND DM = ' + String(dm);
      var dmQuery = L.esri.query({
        url: url
      })
        .where(dmWhere)
        .intersects(bounds);

      dmQuery = applyOutFields(dmQuery);
      dmQuery.run(function(error, featureCollection, response) {
        if (error && !firstError) {
          firstError = error;
        }

        if (featureCollection && Array.isArray(featureCollection.features)) {
          merged.features = merged.features.concat(featureCollection.features);
        }

        responses.push(response || {});
        runNext();
      });
    }

    runNext();
  }

  function ptIsRefreshableCurrentViewRecord(rec) {
    if (!rec || rec.loadMode !== 'current_view') return false;

    var src = String(rec.sourceType || '').toLowerCase();
    return src.indexOf('featureserver') >= 0 || src.indexOf('mapserver') >= 0;
  }


  function ptIsRefreshableImageServerRecord(rec) {
    if (!rec || rec.loadMode !== 'visual') return false;

    var src = String(rec.sourceType || '').toLowerCase();
    return src.indexOf('imageserver') >= 0;
  }


  function ptVisualRenderSettleMs(options) {
    options = options || {};

    var badge = ptCleanText(options.loadBadge || options.load_badge || '').toLowerCase();
    var score = Number(options.loadScore || options.load_score || '');

    if (badge.indexOf('red') >= 0 || badge.indexOf('heavy') >= 0 || (!isNaN(score) && score >= 4)) {
      return 12000;
    }
    if (badge.indexOf('orange') >= 0 || badge.indexOf('slow') >= 0 || (!isNaN(score) && score >= 3)) {
      return 8000;
    }
    if (badge.indexOf('yellow') >= 0 || badge.indexOf('moderate') >= 0 || (!isNaN(score) && score >= 2)) {
      return 5000;
    }
    if (badge.indexOf('green') >= 0 || badge.indexOf('easy') >= 0 || (!isNaN(score) && score <= 1)) {
      return 2500;
    }

    return 5000;
  }

  function ptAfterBrowserPaint(callback) {
    if (typeof window.requestAnimationFrame === 'function') {
      window.requestAnimationFrame(function() {
        window.requestAnimationFrame(function() {
          callback();
        });
      });
      return;
    }
    window.setTimeout(callback, 0);
  }

  function ptFinalizeVisualOverlayAfterSettle(options, terminalMsg, isError, explicitDelayMs) {
    options = options || {};

    if (isError) {
      ptSetExternalStatus(terminalMsg, true, options);
      return;
    }

    var waitMs = explicitDelayMs !== undefined && explicitDelayMs !== null ? Number(explicitDelayMs) : ptVisualRenderSettleMs(options);
    if (isNaN(waitMs) || waitMs < 0) waitMs = 0;

    var seconds = Math.max(1, Math.round(waitMs / 1000));
    var interimMsg = 'Image response loaded; drawing/rendering overlay on the map before marking it ready' +
      (waitMs > 0 ? ' (~' + seconds + ' sec settle check for this load class).' : '.') ;

    ptSetExternalStatus(interimMsg, false, options);

    ptAfterBrowserPaint(function() {
      window.setTimeout(function() {
        ptAfterBrowserPaint(function() {
          ptSetExternalStatus(terminalMsg, false, options);
        });
      }, waitMs);
    });
  }

  function ptIsRefreshableVisualMapServerRecord(rec) {
    if (!rec || rec.loadMode !== 'visual') return false;

    var src = String(rec.sourceType || '').toLowerCase();
    return src.indexOf('mapserver') >= 0;
  }

  function ptIsRefreshableVisualRasterRecord(rec) {
    return ptIsRefreshableImageServerRecord(rec) || ptIsRefreshableVisualMapServerRecord(rec);
  }

  function ptIsRefreshableLiveGeoJsonRecord(rec) {
    if (!rec || rec.loadMode !== 'live') return false;

    var src = String(rec.sourceType || '').toLowerCase();
    return src.indexOf('geojson') >= 0;
  }

  function ptIsRefreshableUicSnapshotRecord(rec) {
    if (!rec || rec.loadMode !== 'live_snapshot') return false;
    return ptIsUicAquiferExemptionStyle(rec);
  }

  function ptIsRefreshableExternalRecord(rec) {
    return ptIsRefreshableCurrentViewRecord(rec) ||
      ptIsRefreshableVisualRasterRecord(rec) ||
      ptIsRefreshableLiveGeoJsonRecord(rec) ||
      ptIsRefreshableUicSnapshotRecord(rec);
  }

  function ptFindCustomLayerById(id) {
    id = String(id || '');

    for (var i = 0; i < ptCustomLayers.length; i++) {
      if (ptCustomLayers[i] && ptCustomLayers[i].id === id) {
        return ptCustomLayers[i];
      }
    }

    return null;
  }

  function ptFindCustomLayerByCatalogIndex(idx) {
    idx = Number(idx);

    for (var i = 0; i < ptCustomLayers.length; i++) {
      var rec = ptCustomLayers[i];
      if (rec && rec.catalogIndex !== null && Number(rec.catalogIndex) === idx) {
        return rec;
      }
    }

    return null;
  }

  function ptRefreshButtonHtml(rec, extraClass) {
    if (!ptIsRefreshableExternalRecord(rec)) return '';

    extraClass = extraClass ? (' ' + extraClass) : '';

    var title = 'Reload this snapshot using the current map view';
    var label = '↻ Refresh current view';

    if (ptIsRefreshableVisualRasterRecord(rec)) {
      title = 'Redraw this visual server-rendered image overlay';
      label = '↻ Refresh image';
    } else if (ptIsRefreshableLiveGeoJsonRecord(rec)) {
      title = 'Reload this live GeoJSON feed';
      label = '↻ Refresh feed';
    } else if (ptIsRefreshableUicSnapshotRecord(rec)) {
      title = 'Retrieve a new complete authoritative snapshot; keep the current layer if retrieval fails';
      label = '↻ Refresh live source';
    }

    return '<button type="button" class="pt-tools-mini-btn pt-custom-refresh-btn' + extraClass + '" data-pt-custom-refresh="' + rec.id + '" ' +
      (rec.refreshing ? 'disabled aria-disabled="true"' : '') +
      ' title="' + title + '">' +
      (rec.refreshing ? 'Refreshing…' : label) +
    '</button>';
  }

  function ptUpdateManualRefreshControl() {
    var div = document.getElementById('pt-manual-refresh-wrap');
    if (!div) return;

    var rec = ptFindCustomLayerById(ptLastManualCurrentViewLayerId);

    if (!rec || !ptIsRefreshableCurrentViewRecord(rec)) {
      div.innerHTML = '';
      div.style.display = 'none';
      return;
    }

    div.style.display = 'block';
    div.innerHTML =
      '<div class="pt-tools-muted" style="margin-top:4px;">Manual current-view snapshot: pan/zoom, then refresh to requery this layer.</div>' +
      ptRefreshButtonHtml(rec, 'pt-manual-refresh-btn');
  }

  function ptSetManualCurrentViewRecord(rec) {
    if (!rec || !ptIsRefreshableCurrentViewRecord(rec)) return;

    // Quick-add catalog rows have their own local refresh button inside the
    // active catalog card.  The manual refresh area is reserved for layers that
    // were added through the advanced manual form.
    if (rec.catalogIndex !== null && !isNaN(rec.catalogIndex)) return;

    ptLastManualCurrentViewLayerId = rec.id;
    ptUpdateManualRefreshControl();
  }

  function ptRefreshCustomLayer(id) {
    var rec = null;

    ptCustomLayers.forEach(function(r) {
      if (r && r.id === id) rec = r;
    });

    if (!rec) {
      ptSetStatus('Could not find that external layer to refresh.', true);
      return;
    }

    if (!ptIsRefreshableExternalRecord(rec)) {
      ptSetStatus('Only current-view ArcGIS FeatureServer/MapServer snapshots, live GeoJSON feeds, or visual server-rendered image overlays can be refreshed.', true);
      return;
    }

    var options = ptOptionsFromCustomRecord(rec);

    if (ptIsRefreshableUicSnapshotRecord(rec)) {
      rec.refreshing = true;
      rec._uicRefreshGeneration = Number(rec._uicRefreshGeneration || 0) + 1;
      var generation = rec._uicRefreshGeneration;
      ptRenderCustomLayerList();
      ptRenderQuickCatalog();
      if (window.BRIM && window.BRIM.uicExplorer) {
        window.BRIM.uicExplorer.setExternalLoading(
          rec.catalogExtId,
          true,
          'Refreshing complete authoritative source…'
        );
      }
      ptSetExternalStatus(
        'Refreshing the complete authoritative UIC source. The current layer remains visible until the replacement succeeds…',
        false,
        options
      );
      ptRunArcgisAllFeatureQuery(
        rec.url,
        ptCleanText(rec.whereClause) || '1=1',
        options
      ).then(function(result) {
        if (generation !== rec._uicRefreshGeneration) return;
        var featureCollection = ptSortFeatureCollectionForDrawing(
          result.featureCollection,
          options
        );
        var features = featureCollection.features || [];
        options.retrievalUtc = new Date().toISOString();
        options.featureCount = features.length;
        options.sourceServiceUrl = rec.url;
        var replacement = ptExternalFeatureCollectionLayer(
          featureCollection,
          rec.color,
          0.04,
          options,
          !!rec.clickable
        );
        var oldLayer = rec.layer;
        var wasVisible = rec.visible && oldLayer && map.hasLayer(oldLayer);
        if (wasVisible) replacement.addTo(map);
        if (oldLayer && map.hasLayer(oldLayer)) map.removeLayer(oldLayer);
        rec.layer = replacement;
        rec.featureCount = features.length;
        rec.retrievalUtc = options.retrievalUtc;
        rec.refreshing = false;
        if (window.BRIM && window.BRIM.uicExplorer) {
          window.BRIM.uicExplorer.setExternalLoading(rec.catalogExtId, false, '');
          window.BRIM.uicExplorer.upsertExternal(rec);
        }
        ptRenderCustomLayerList();
        ptRenderQuickCatalog();
        ptSetExternalStatus(
          'UIC live source refreshed: ' + features.length.toLocaleString() +
            ' feature(s), retrieved ' + rec.retrievalUtc + '.',
          false,
          options
        );
      }).catch(function(error) {
        if (generation !== rec._uicRefreshGeneration) return;
        console.error(error);
        rec.refreshing = false;
        if (window.BRIM && window.BRIM.uicExplorer) {
          window.BRIM.uicExplorer.setExternalLoading(rec.catalogExtId, false, '');
          window.BRIM.uicExplorer.upsertExternal(rec);
          window.BRIM.uicExplorer.setExternalError(
            rec.catalogExtId,
            'Refresh failed; previous live snapshot retained. ' +
              (error && error.message ? error.message : '')
          );
        }
        ptRenderCustomLayerList();
        ptRenderQuickCatalog();
        ptSetExternalStatus(
          'UIC live refresh failed. The previous snapshot was left unchanged. ' +
            (error && error.message ? error.message : ''),
          true,
          options
        );
      });
      return;
    }

    if (ptIsRefreshableLiveGeoJsonRecord(rec)) {
      rec.refreshing = true;
      ptRenderCustomLayerList();
      ptRenderQuickCatalog();
      ptSetExternalStatus('Refreshing live GeoJSON feed: ' + rec.name + '...', false, options);

      fetch(rec.url, { cache: 'no-store' })
        .then(function(resp) {
          if (!resp.ok) throw new Error('HTTP ' + resp.status + ' ' + resp.statusText);
          return resp.json();
        })
        .then(function(geojson) {
          geojson = ptSortFeatureCollectionForDrawing(geojson, options);
          var features = geojson && Array.isArray(geojson.features) ? geojson.features : [];

          if (rec.layer && typeof rec.layer.clearLayers === 'function' && typeof rec.layer.addData === 'function') {
            rec.layer.clearLayers();
            rec.layer.addData(geojson);
          }

          rec.featureCount = features.length;
          rec.refreshing = false;
          ptRenderCustomLayerList();
          ptRenderQuickCatalog();
          ptSetExternalStatus('GeoJSON feed refreshed: ' + features.length.toLocaleString() + ' feature(s).', false, options);
        })
        .catch(function(err) {
          console.error(err);
          rec.refreshing = false;
          ptRenderCustomLayerList();
          ptRenderQuickCatalog();
          ptSetExternalStatus('Could not refresh this GeoJSON feed.', true, options);
        });
      return;
    }

    if (ptIsRefreshableVisualRasterRecord(rec)) {
      rec.refreshing = true;
      ptRenderCustomLayerList();
      ptRenderQuickCatalog();
      ptSetExternalStatus('Refreshing visual image overlay: ' + rec.name + '...', false, options);

      var finished = false;
      var fallbackTimer = null;

      function finishVisualRefresh(msg, isError) {
        if (finished) return;
        finished = true;
        if (fallbackTimer) window.clearTimeout(fallbackTimer);
        rec.refreshing = false;
        ptRenderCustomLayerList();
        ptRenderQuickCatalog();
        ptSetExternalStatus(msg, !!isError, options);
      }

      try {
        if (rec.layer && typeof rec.layer.once === 'function') {
          rec.layer.once('load', function() {
            ptSetExternalStatus('Image response loaded; drawing/rendering refreshed overlay on the map before marking it ready...', false, options);
            window.setTimeout(function() {
              finishVisualRefresh('Visual image overlay refreshed and ready.', false);
            }, ptVisualRenderSettleMs(options));
          });
          rec.layer.once('requesterror', function() {
            finishVisualRefresh('Visual image overlay refresh request failed or was blocked by the service/browser.', true);
          });
        }

        if (rec.layer && typeof rec.layer.redraw === 'function') {
          rec.layer.redraw();
        } else if (rec.layer && typeof rec.layer.refresh === 'function') {
          rec.layer.refresh();
        } else if (rec.layer && map.hasLayer(rec.layer)) {
          map.removeLayer(rec.layer);
          map.addLayer(rec.layer);
        }

        fallbackTimer = window.setTimeout(function() {
          finishVisualRefresh('Visual image overlay refresh requested; provider/browser draw may still be finishing. If the map looks blank, wait a few seconds or retry/refresh.', false);
        }, 12000);
      } catch (err) {
        console.error(err);
        finishVisualRefresh('Could not refresh this visual image overlay.', true);
      }
      return;
    }

    rec.refreshing = true;
    ptRenderCustomLayerList();
    ptRenderQuickCatalog();
    ptUpdateManualRefreshControl();
    ptSetExternalStatus('Refreshing ' + rec.name + ' using the current map view...', false, options);

    var src = String(rec.sourceType || '').toLowerCase();
    var isMap = src.indexOf('mapserver') >= 0;
    var whereText = ptCleanText(rec.whereClause) || '1=1';

    ptEnsureEsriLeaflet(function(ok) {
      if (!ok || !L.esri || !L.esri.query) {
        rec.refreshing = false;
        ptRenderCustomLayerList();
        ptRenderQuickCatalog();
        ptUpdateManualRefreshControl();
        ptSetExternalStatus('Could not load Esri Leaflet. Refresh requires browser access to the Esri Leaflet library.', true, options);
        return;
      }

      try {
        var bounds = map.getBounds();
        ptRunArcgisCurrentViewFeatureQuery(rec.url, bounds, whereText, options, function(error, featureCollection, response) {
          if (error) {
            console.error(error);
            rec.refreshing = false;
            ptRenderCustomLayerList();
            ptRenderQuickCatalog();
            ptUpdateManualRefreshControl();
            ptSetExternalStatus('Refresh failed. The previous snapshot was left unchanged.', true, options);
            return;
          }

          featureCollection = featureCollection || {
            type: 'FeatureCollection',
            features: []
          };

          featureCollection = ptSortFeatureCollectionForDrawing(featureCollection, options);
          var features = featureCollection.features || [];

          var newLayer = ptExternalFeatureCollectionLayer(
            featureCollection,
            rec.color,
            0.14,
            options,
            !!rec.clickable
          );

          if (rec.layer && map.hasLayer(rec.layer)) {
            map.removeLayer(rec.layer);
          }

          rec.layer = newLayer;
          rec.featureCount = features.length;
          rec.refreshing = false;
          rec.whereClause = whereText;
          ptUpdateOpsPromotedStreamflowMetric(options, features.length, response && response.exceededTransferLimit ? 'Transfer limit reported.' : '');

          if (rec.visible) {
            rec.layer.addTo(map);
          }

          ptRenderCustomLayerList();
          ptRenderQuickCatalog();
          ptUpdateManualRefreshControl();

          var limitNote = '';
          if (response && response.exceededTransferLimit) {
            limitNote = ' The service reported a transfer limit; zoom in and refresh for a more complete snapshot.';
          } else if (features.length >= 1000) {
            limitNote = ' This is a large snapshot; zoom in and refresh if it looks incomplete or sluggish.';
          }

          ptSetExternalStatus(
            'Refreshed current-view ' + (isMap ? 'MapServer' : 'FeatureServer') +
              ' layer: ' + features.length.toLocaleString() +
              ' feature(s) in the current map view.' + limitNote,
            features.length === 0,
            options
          );
        });
      } catch (err) {
        console.error(err);
        rec.refreshing = false;
        ptRenderCustomLayerList();
        ptRenderQuickCatalog();
        ptUpdateManualRefreshControl();
        ptSetExternalStatus('Could not refresh this current-view layer. The previous snapshot was left unchanged.', true, options);
      }
    });
  }


  function ptVisualIdentifyOptionsFromRecord(rec) {
    var options = ptOptionsFromCustomRecord(rec);
    options.layerName = rec && rec.name ? rec.name : options.layerName;
    return options;
  }

  function ptIsVisualIdentifyRecord(rec) {
    if (!rec || !rec.visible || !rec.visualIdentify) return false;
    if (!rec.layer || !map.hasLayer(rec.layer)) return false;

    var src = String(rec.sourceType || '').toLowerCase();
    var isVisualMapServer =
      src.indexOf('arcgis mapserver') >= 0 ||
      src.indexOf('arcgis tiled mapserver') >= 0;
    var isImageServer = src.indexOf('arcgis imageserver') >= 0;

    if (!isVisualMapServer && !isImageServer) return false;

    return !!(
      rec.identifyUrl ||
      rec.identifyServiceUrl ||
      rec.popupFields ||
      rec.hoverFields ||
      rec.popupLinkTemplate ||
      rec.clickable
    );
  }

  function ptVisualIdentifyRecords(wantHover) {
    var out = [];

    ptCustomLayers.forEach(function(rec) {
      if (!ptIsVisualIdentifyRecord(rec)) return;
      if (wantHover && !rec.hoverFields) return;
      out.push(rec);
    });

    // Newer layers sit visually above older layers.  Within that order, query
    // feature/MapServer records before ImageServer rasters.  Raster identify
    // commonly returns a value everywhere, so querying rasters first can mask
    // point-observation records that users can see on top of the raster.
    var newestFirst = out.reverse();
    var featureLike = [];
    var rasters = [];

    newestFirst.forEach(function(rec) {
      var src = String(rec && rec.sourceType || '').toLowerCase();
      if (src.indexOf('arcgis imageserver') >= 0) rasters.push(rec);
      else featureLike.push(rec);
    });

    return featureLike.concat(rasters);
  }

  function ptFeatureArrayFromCollection(featureCollection) {
    if (!featureCollection) return [];
    if (Array.isArray(featureCollection.features)) return featureCollection.features;
    return [];
  }

  function ptPropsFromFeature(feature) {
    if (!feature) return {};
    return feature.properties || feature.attributes || {};
  }

  function ptChooseIdentifyFeature(features) {
    features = features || [];
    if (!features.length) return null;

    // For the U.S. Drought Monitor, multiple polygons can occasionally be
    // returned near category boundaries. Prefer the highest-intensity DM code.
    var best = null;
    var bestDm = -Infinity;

    features.forEach(function(feature) {
      var props = ptPropsFromFeature(feature);
      var dm = Number(ptFieldValue(props, 'DM'));

      if (isFinite(dm) && dm > bestDm) {
        best = feature;
        bestDm = dm;
      }
    });

    if (best) return best;

    // DWR/TRE vertical-displacement point-location services can return more
    // than one feature near a click, especially where point/cell polygons stack
    // or overlap.  When the service exposes CODE + DataTable, prefer the
    // highest DataTable value so the popup points users to the most current
    // source table available for that location.
    var bestTable = null;
    var bestTableVal = -Infinity;

    features.forEach(function(feature) {
      var props = ptPropsFromFeature(feature);
      var code = ptFieldValue(props, 'CODE');
      var dataTable = Number(ptFieldValue(props, 'DataTable'));

      if (ptHasValue(code) && isFinite(dataTable) && dataTable > bestTableVal) {
        bestTable = feature;
        bestTableVal = dataTable;
      }
    });

    return bestTable || features[0];
  }

  function ptArcgisFeatureToGeoJsonFeature(feature) {
    feature = feature || {};

    if (feature.type === 'Feature') return feature;

    var attrs = feature.attributes || feature.properties || {};
    var geom = feature.geometry || null;
    var geometry = null;

    // ArcGIS REST query responses return point geometry as {x, y}.  Convert
    // that small subset to GeoJSON so the existing popup/tooltip helpers can
    // keep using properties consistently and the nearest-feature fallback can
    // rank point hits by distance.
    if (geom && geom.x !== undefined && geom.y !== undefined) {
      geometry = {
        type: 'Point',
        coordinates: [Number(geom.x), Number(geom.y)]
      };
    } else if (geom && geom.type && geom.coordinates) {
      geometry = geom;
    }

    return {
      type: 'Feature',
      geometry: geometry,
      properties: attrs
    };
  }

  function ptFeatureArrayFromArcgisQueryResponse(json) {
    json = json || {};
    var raw = Array.isArray(json.features) ? json.features : [];
    return raw.map(ptArcgisFeatureToGeoJsonFeature);
  }

  function ptFeaturePointLatLng(feature) {
    if (!feature || !feature.geometry) return null;

    var g = feature.geometry;

    if (g.type === 'Point' && Array.isArray(g.coordinates) && g.coordinates.length >= 2) {
      var lng = Number(g.coordinates[0]);
      var lat = Number(g.coordinates[1]);
      if (isFinite(lat) && isFinite(lng)) return L.latLng(lat, lng);
    }

    if (g.x !== undefined && g.y !== undefined) {
      var x = Number(g.x);
      var y = Number(g.y);
      if (isFinite(x) && isFinite(y)) return L.latLng(y, x);
    }

    return null;
  }

  function ptFeatureDistanceMeters(feature, latlng) {
    if (!latlng || !latlng.distanceTo) return Infinity;

    var featureLatLng = ptFeaturePointLatLng(feature);
    if (!featureLatLng) return Infinity;

    return latlng.distanceTo(featureLatLng);
  }

  function ptChooseClosestIdentifyFeature(features, latlng) {
    features = features || [];
    if (!features.length) return null;

    var best = null;
    var bestDist = Infinity;

    features.forEach(function(feature) {
      var d = ptFeatureDistanceMeters(feature, latlng);
      if (d < bestDist) {
        best = feature;
        bestDist = d;
      }
    });

    return best || ptChooseIdentifyFeature(features);
  }

  function ptIdentifyToleranceMetersForLatLng(latlng, pixelTolerance) {
    pixelTolerance = Number(pixelTolerance || 12);

    try {
      if (!map || !map.latLngToContainerPoint || !map.containerPointToLatLng || !latlng || !latlng.distanceTo) {
        return 50;
      }

      var p = map.latLngToContainerPoint(latlng);
      var p2 = L.point(p.x + pixelTolerance, p.y);
      var ll2 = map.containerPointToLatLng(p2);
      var meters = latlng.distanceTo(ll2);

      if (!isFinite(meters)) return 50;

      // Keep the fallback generous enough for tiny server-rendered points but
      // capped so a statewide view does not return a distant observation.
      return Math.max(15, Math.min(2500, meters));
    } catch (e) {
      return 50;
    }
  }

  function ptFeatureServerQueryOutFields(rec) {
    var fields = [];

    ['outFields', 'popupFields', 'hoverFields'].forEach(function(key) {
      ptSplitFieldList(rec && rec[key]).forEach(function(field) {
        if (fields.indexOf(field) < 0) fields.push(field);
      });
    });

    if (fields.indexOf('OBJECTID') < 0) fields.push('OBJECTID');

    return fields.length ? fields.join(',') : '*';
  }

  function ptQueryFeatureServerNearbyAtLatLng(rec, latlng, callback, originalError) {
    var url = ptNormalizeUrl(ptCleanText(rec && rec.identifyUrl) || ptCleanText(rec && rec.url));

    if (!url || !/\/FeatureServer\/\d+$/i.test(url)) {
      callback(originalError || new Error('FeatureServer identify URL is missing or invalid.'), null);
      return;
    }

    try {
      var distanceMeters = ptIdentifyToleranceMetersForLatLng(latlng, 12);
      var params = new URLSearchParams();

      params.set('f', 'json');
      params.set('where', '1=1');
      params.set('geometry', String(latlng.lng) + ',' + String(latlng.lat));
      params.set('geometryType', 'esriGeometryPoint');
      params.set('inSR', '4326');
      params.set('spatialRel', 'esriSpatialRelIntersects');
      params.set('distance', String(Math.round(distanceMeters)));
      params.set('units', 'esriSRUnit_Meter');
      params.set('outFields', ptFeatureServerQueryOutFields(rec));
      params.set('returnGeometry', 'true');
      params.set('outSR', '4326');
      params.set('resultRecordCount', '10');

      fetch(url.replace(/\/+$/, '') + '/query?' + params.toString(), { cache: 'no-store' })
        .then(function(resp) {
          if (!resp.ok) throw new Error('HTTP ' + resp.status + ' ' + resp.statusText);
          return resp.json();
        })
        .then(function(json) {
          if (json && json.error) {
            throw new Error(json.error.message || 'FeatureServer nearby query error');
          }

          var features = ptFeatureArrayFromArcgisQueryResponse(json);
          callback(null, ptChooseClosestIdentifyFeature(features, latlng));
        })
        .catch(function(error) {
          callback(originalError || error, null);
        });
    } catch (err) {
      callback(originalError || err, null);
    }
  }

  function ptIdentifyFeatureServerAtLatLng(rec, latlng, callback) {
    if (!L.esri || !L.esri.query) {
      // The browser already loaded Esri Leaflet before entering visual identify,
      // but keep a direct REST fallback so tiny point observation services are
      // still queryable if the Esri query helper is unavailable.
      ptQueryFeatureServerNearbyAtLatLng(rec, latlng, callback, new Error('Esri Leaflet query helper is unavailable.'));
      return;
    }

    try {
      var url = ptCleanText(rec.identifyUrl) || rec.url;
      var query = L.esri.query({ url: url })
        .where('1=1')
        .intersects(latlng);

      query.run(function(error, featureCollection) {
        if (error) {
          // Point symbols rendered by MapServer image layers can be effectively
          // impossible to click with an exact point intersection. Try a small
          // pixel-based nearby FeatureServer query before giving up.
          ptQueryFeatureServerNearbyAtLatLng(rec, latlng, callback, error);
          return;
        }

        var features = ptFeatureArrayFromCollection(featureCollection);

        if (features.length > 0) {
          callback(null, ptChooseIdentifyFeature(features));
          return;
        }

        // Important for provider-rendered point layers such as the DWR/C2VSimFG
        // subsidence observation points: the map image shows the point symbol,
        // but an exact FeatureServer point-intersection can return no record
        // unless the click lands precisely on the source coordinate.
        ptQueryFeatureServerNearbyAtLatLng(rec, latlng, callback, null);
      });
    } catch (err) {
      ptQueryFeatureServerNearbyAtLatLng(rec, latlng, callback, err);
    }
  }


  function ptQueryArcgisLayerAtLatLng(url, latlng, callback) {
    // Fallback identify path for MapServer *layer* endpoints that support
    // query.  Some NOAA/NWS visual MapServer layers draw correctly as images
    // but return empty results from identifyFeatures().  Querying the exact
    // layer endpoint by point intersection is often more reliable for hover and
    // click summaries while preserving official server-rendered cartography.
    if (!L.esri || !L.esri.query || !url) {
      callback(new Error('Esri Leaflet query helper is unavailable.'));
      return;
    }

    try {
      L.esri.query({ url: url })
        .where('1=1')
        .intersects(latlng)
        .run(function(error, featureCollection) {
          if (error) {
            callback(error);
            return;
          }

          var features = ptFeatureArrayFromCollection(featureCollection);
          callback(null, ptChooseIdentifyFeature(features));
        });
    } catch (err) {
      callback(err);
    }
  }

  function ptIdentifyMapServerAtLatLng(rec, latlng, callback) {
    if (!L.esri || !L.esri.identifyFeatures) {
      callback(new Error('Esri Leaflet identify helper is unavailable.'));
      return;
    }

    try {
      var serviceUrl = ptCleanText(rec.identifyServiceUrl);
      var layerId = rec.identifyLayerId;

      if (!serviceUrl) {
        var parsed = ptParseArcgisLayerUrl(rec.url, 'MapServer');
        serviceUrl = parsed.serviceUrl;
        layerId = parsed.isLayerEndpoint ? parsed.layerId : layerId;
      }

      function runLayerQueryFallback(originalError) {
        if (layerId === null || layerId === undefined || isNaN(Number(layerId))) {
          callback(originalError || null, null);
          return;
        }

        var layerUrl = serviceUrl.replace(/\/+$/, '') + '/' + Number(layerId);
        ptQueryArcgisLayerAtLatLng(layerUrl, latlng, function(queryError, queryFeature) {
          if (queryError) {
            // Preserve identify failure details in the console, but return the
            // query error so the visual-identify caller can continue to the
            // next active layer without disrupting the map.
            if (originalError) console.warn('BRIM MapServer identify failed before query fallback:', originalError);
            callback(queryError, null);
            return;
          }

          callback(null, queryFeature);
        });
      }

      var identify = L.esri.identifyFeatures({ url: serviceUrl })
        .on(map)
        .at(latlng);

      if (layerId !== null && layerId !== undefined && !isNaN(Number(layerId)) && typeof identify.layers === 'function') {
        identify = identify.layers('all:' + Number(layerId));
      }

      identify.run(function(error, featureCollection) {
        if (error) {
          runLayerQueryFallback(error);
          return;
        }

        var features = ptFeatureArrayFromCollection(featureCollection);
        if (features.length > 0) {
          callback(null, ptChooseIdentifyFeature(features));
          return;
        }

        runLayerQueryFallback(null);
      });
    } catch (err) {
      callback(err);
    }
  }

  function ptImageServerIdentifyValueFromResponse(json) {
    json = json || {};

    var candidates = [
      json.value,
      json.pixelValue,
      json.pixel_value,
      json.Values,
      json.values
    ];

    if (json.properties) {
      candidates.push(json.properties.Value);
      candidates.push(json.properties.value);
      candidates.push(json.properties.PixelValue);
      candidates.push(json.properties.pixelValue);
      candidates.push(json.properties.Values);
      candidates.push(json.properties.values);
    }

    for (var i = 0; i < candidates.length; i++) {
      var v = candidates[i];
      if (Array.isArray(v)) v = v.length ? v[0] : null;
      if (v === null || v === undefined) continue;
      v = ptCleanText(v);
      if (!v || /^nodata$/i.test(v)) continue;
      return v;
    }

    return '';
  }

  function ptImageServerUnitsForRecord(rec, options) {
    var units = ptCleanText(options && options.styleUnits);
    if (units) return units;

    var nm = ptCleanText(rec && rec.name).toLowerCase();
    if (nm.indexOf('annual rate') >= 0) return 'ft/yr';
    if (nm.indexOf('total since') >= 0 || nm.indexOf('since 2015') >= 0) return 'ft';
    return '';
  }

  function ptImageServerLabelForRecord(rec) {
    var nm = ptCleanText(rec && rec.name).toLowerCase();
    if (nm.indexOf('annual rate') >= 0) return 'Annual rate';
    if (nm.indexOf('total since') >= 0 || nm.indexOf('since 2015') >= 0) return 'Total displacement since June 2015';
    return 'Raster value';
  }

  function ptFormatImageServerRasterValue(value, units, label) {
    value = ptCleanText(value);

    if (!value) return 'No raster value returned';

    var n = Number(value);
    var valueText = value;

    if (isFinite(n)) {
      var absN = Math.abs(n);
      if (absN >= 10) valueText = n.toFixed(1);
      else if (absN >= 1) valueText = n.toFixed(2);
      else valueText = n.toFixed(3);
    }

    return ptCleanText(label || 'Raster value') + ': ' + valueText + (units ? ' ' + units : '');
  }

  function ptIdentifyImageServerAtLatLng(rec, latlng, callback) {
    var url = ptNormalizeUrl(ptCleanText(rec && rec.identifyUrl) || ptCleanText(rec && rec.url));

    if (!url || !/\/ImageServer$/i.test(url)) {
      callback(new Error('ImageServer identify URL is missing or invalid.'));
      return;
    }

    try {
      var size = map && map.getSize ? map.getSize() : { x: 800, y: 600 };
      var b = map && map.getBounds ? map.getBounds() : null;
      var extent = b ? [b.getWest(), b.getSouth(), b.getEast(), b.getNorth()].join(',') : '-125,32,-113,42.5';
      var params = new URLSearchParams();

      params.set('f', 'json');
      params.set('geometry', String(latlng.lng) + ',' + String(latlng.lat));
      params.set('geometryType', 'esriGeometryPoint');
      params.set('sr', '4326');
      params.set('returnGeometry', 'false');
      params.set('returnCatalogItems', 'false');
      params.set('mapExtent', extent);
      params.set('imageDisplay', String(Math.max(1, Math.round(size.x))) + ',' + String(Math.max(1, Math.round(size.y))) + ',96');

      fetch(url.replace(/\/+$/, '') + '/identify?' + params.toString(), { cache: 'no-store' })
        .then(function(resp) {
          if (!resp.ok) throw new Error('HTTP ' + resp.status + ' ' + resp.statusText);
          return resp.json();
        })
        .then(function(json) {
          if (json && json.error) {
            throw new Error(json.error.message || 'ImageServer identify error');
          }

          var options = ptVisualIdentifyOptionsFromRecord(rec);
          var rawValue = ptImageServerIdentifyValueFromResponse(json);
          var units = ptImageServerUnitsForRecord(rec, options);
          var label = ptImageServerLabelForRecord(rec);
          var display = ptFormatImageServerRasterValue(rawValue, units, label);

          if (!rawValue) {
            callback(null, null);
            return;
          }

          callback(null, {
            type: 'Feature',
            geometry: null,
            properties: {
              Layer: rec && rec.name ? rec.name : 'ImageServer raster',
              Raster_Value: rawValue,
              Raster_Units: units,
              Raster_Value_Display: display,
              Raster_Note: 'Provider raw pixel value from ImageServer identify; use for screening context and verify with DWR source metadata.',
              Latitude: latlng.lat,
              Longitude: latlng.lng
            }
          });
        })
        .catch(function(error) {
          callback(error);
        });
    } catch (err) {
      callback(err);
    }
  }

  function ptIdentifyVisualRecordAtLatLng(rec, latlng, callback) {
    var identifyUrl = ptCleanText(rec.identifyUrl);
    var src = String(rec && rec.sourceType || '').toLowerCase();

    if (src.indexOf('arcgis imageserver') >= 0 || /\/ImageServer$/i.test(identifyUrl)) {
      ptIdentifyImageServerAtLatLng(rec, latlng, callback);
      return;
    }

    if (/\/FeatureServer\/\d+$/i.test(identifyUrl)) {
      ptIdentifyFeatureServerAtLatLng(rec, latlng, callback);
      return;
    }

    ptIdentifyMapServerAtLatLng(rec, latlng, callback);
  }

  function ptIdentifyVisualRecordsAtLatLng(records, latlng, idx, callback) {
    records = records || [];
    idx = idx || 0;

    if (idx >= records.length) {
      callback(null, null, null);
      return;
    }

    var rec = records[idx];

    ptIdentifyVisualRecordAtLatLng(rec, latlng, function(error, feature) {
      if (error) {
        console.warn('BRIM visual MapServer identify failed for ' + (rec.name || rec.url), error);
        ptIdentifyVisualRecordsAtLatLng(records, latlng, idx + 1, callback);
        return;
      }

      if (feature) {
        callback(null, rec, feature);
        return;
      }

      ptIdentifyVisualRecordsAtLatLng(records, latlng, idx + 1, callback);
    });
  }

  function ptHideVisualIdentifyTooltip() {
    if (ptVisualIdentifyTooltip && map.hasLayer(ptVisualIdentifyTooltip)) {
      map.removeLayer(ptVisualIdentifyTooltip);
    }

    if (map && map.getContainer) {
      map.getContainer().classList.remove('pt-external-identify-hit');
    }
  }

  function ptHandleVisualIdentifyHover(latlng) {
    var seq = ++ptVisualIdentifySeq;

    if (ptMeasureInteractionActive) {
      ptHideVisualIdentifyTooltip();
      return;
    }

    var records = ptVisualIdentifyRecords(true);

    if (!records.length) {
      ptHideVisualIdentifyTooltip();
      return;
    }

    ptEnsureEsriLeaflet(function(ok) {
      if (!ok || !L.esri) return;

      ptIdentifyVisualRecordsAtLatLng(records, latlng, 0, function(error, rec, feature) {
        if (seq !== ptVisualIdentifySeq || ptMeasureInteractionActive) return;

        if (error || !rec || !feature) {
          ptHideVisualIdentifyTooltip();
          return;
        }

        var options = ptVisualIdentifyOptionsFromRecord(rec);
        var props = ptPropsFromFeature(feature);
        var html = ptTooltipFromProperties(props, options);

        if (!html) {
          ptHideVisualIdentifyTooltip();
          return;
        }

        var tooltipClassName = ptExternalOverlayTooltipClass(options);

        if (ptVisualIdentifyTooltip &&
            ptVisualIdentifyTooltip.options &&
            ptVisualIdentifyTooltip.options.className !== tooltipClassName) {
          ptHideVisualIdentifyTooltip();
          ptVisualIdentifyTooltip = null;
        }

        if (!ptVisualIdentifyTooltip) {
          ptVisualIdentifyTooltip = L.tooltip({
            direction: 'auto',
            opacity: 0.95,
            className: tooltipClassName,
            sticky: false
          });
        }

        if (map && map.getContainer) {
          map.getContainer().classList.add('pt-external-identify-hit');
        }

        ptVisualIdentifyTooltip
          .setLatLng(latlng)
          .setContent(html)
          .addTo(map);
      });
    });
  }

  function ptHandleVisualIdentifyClick(latlng) {
    if (ptMeasureInteractionActive) return;

    var records = ptVisualIdentifyRecords(false).filter(function(rec) {
      return rec.clickable || rec.popupFields || rec.popupLinkTemplate;
    });

    if (!records.length) return;

    ptEnsureEsriLeaflet(function(ok) {
      if (!ok || !L.esri) return;

      ptIdentifyVisualRecordsAtLatLng(records, latlng, 0, function(error, rec, feature) {
        if (error || !rec || !feature) return;

        var options = ptVisualIdentifyOptionsFromRecord(rec);
        var props = ptPropsFromFeature(feature);
        var html = ptPopupFromProperties(props, options);

        var popupOptions = ptExternalLeafletPopupOptions(options);

        L.popup(popupOptions)
          .setLatLng(latlng)
          .setContent(html)
          .openOn(map);
      });
    });
  }

  map.on('mousemove', function(e) {
    if (ptMeasureInteractionActive) {
      if (ptVisualIdentifyHoverTimer) {
        window.clearTimeout(ptVisualIdentifyHoverTimer);
        ptVisualIdentifyHoverTimer = null;
      }
      ptHideVisualIdentifyTooltip();
      return;
    }

    if (ptVisualIdentifyHoverTimer) {
      window.clearTimeout(ptVisualIdentifyHoverTimer);
    }

    ptVisualIdentifyHoverTimer = window.setTimeout(function() {
      ptHandleVisualIdentifyHover(e.latlng);
    }, PT2_VISUAL_IDENTIFY_HOVER_DELAY_MS);
  });

  map.on('mouseout dragstart zoomstart', function() {
    if (ptVisualIdentifyHoverTimer) {
      window.clearTimeout(ptVisualIdentifyHoverTimer);
      ptVisualIdentifyHoverTimer = null;
    }
    ptHideVisualIdentifyTooltip();
  });

  map.on('click', function(e) {
    ptHandleVisualIdentifyClick(e.latlng);
  });

  function ptSetActiveExternalExpanded(expanded) {
    ptActiveExternalExpanded = !!expanded;

    var body = document.getElementById('pt-active-external-body');
    var caret = document.getElementById('pt-active-external-caret');
    var toggle = document.getElementById('pt-active-external-toggle');

    if (body) {
      body.style.display = ptActiveExternalExpanded ? 'block' : 'none';
    }
    if (caret) {
      caret.textContent = ptActiveExternalExpanded ? '▾' : '▸';
    }
    if (toggle) {
      toggle.setAttribute('aria-expanded', ptActiveExternalExpanded ? 'true' : 'false');
      toggle.classList.toggle('pt-active-external-expanded', ptActiveExternalExpanded);
    }
  }

  function ptSyncActiveExternalHeader(count) {
    count = Number(count || 0);

    var label = document.getElementById('pt-active-external-label');
    var countSpan = document.getElementById('pt-active-external-count');
    var section = document.getElementById('pt-active-external-section');

    if (label) {
      label.textContent = 'Active external overlays';
    }
    if (countSpan) {
      countSpan.textContent = '(' + count.toLocaleString() + ')';
    }
    if (section) {
      section.classList.toggle('pt-active-external-has-layers', count > 0);
      section.classList.toggle('pt-active-external-empty', count === 0);
    }

    // After everything has been cleared, return to the compact state.
    if (count === 0) {
      ptActiveExternalExpanded = false;
    }

    ptSetActiveExternalExpanded(ptActiveExternalExpanded);
  }

  function ptExternalLoadBadgeValue(obj) {
    var badge = ptCleanText(
      (obj && (
        obj.loadBadgeOverride ||
        obj.load_badge_override ||
        obj.catalogLoadBadgeOverride ||
        obj.catalog_load_badge_override ||
        obj.loadBadge ||
        obj.load_badge ||
        obj.catalogLoadBadge ||
        obj.catalog_load_badge
      )) || ''
    ).toLowerCase();

    if (badge === 'green' || badge === 'yellow' || badge === 'orange' || badge === 'red' || badge === 'gray') {
      return badge;
    }

    return 'gray';
  }

  function ptExternalLoadBadgeLabel(badge) {
    badge = ptExternalLoadBadgeValue({ loadBadge: badge });

    if (badge === 'green') return 'fast / reliable';
    if (badge === 'yellow') return 'moderate';
    if (badge === 'orange') return 'slow / give it time';
    if (badge === 'red') return 'heavy / pretest';
    return 'unknown';
  }

  function ptExternalLoadBadgeNote(obj) {
    return ptCleanText(
      (obj && (obj.loadNote || obj.load_note || obj.catalogLoadNote || obj.catalog_load_note)) || ''
    );
  }

  function ptExternalLoadBadgeOverrideNote(obj) {
    var overrideBadge = ptCleanText(
      (obj && (
        obj.loadBadgeOverride ||
        obj.load_badge_override ||
        obj.catalogLoadBadgeOverride ||
        obj.catalog_load_badge_override
      )) || ''
    ).toLowerCase();

    var baseBadge = ptCleanText(
      (obj && (obj.loadBadge || obj.load_badge || obj.catalogLoadBadge || obj.catalog_load_badge)) || ''
    ).toLowerCase();

    if (!overrideBadge || overrideBadge === baseBadge) return '';

    return 'BRIM manual badge override from catalog.';
  }

  function ptExternalLoadAuditBasis(obj) {
    return ptCleanText(
      (obj && (obj.loadAuditBasis || obj.load_audit_basis || obj.catalogLoadAuditBasis || obj.catalog_load_audit_basis)) || ''
    );
  }

  function ptExternalFriendlyLoadNote(obj) {
    var note = ptExternalLoadBadgeNote(obj);
    if (!note) return '';

    note = note
      .replace(/^Multi-extent load audit:/i, 'Audited load:')
      .replace(/^Single-extent load audit:/i, 'Audited load:')
      .replace(/\bok\b/g, 'tests loaded')
      .replace(/few seconds/gi, 'some time');

    return note;
  }

  function ptExternalFriendlyLoadBasis(obj) {
    var basis = ptExternalLoadAuditBasis(obj);
    if (!basis) return '';
    if (/multi[_ -]?extent[_ -]?smoke/i.test(basis)) return 'BRIM load audit';
    if (/single[_ -]?extent[_ -]?smoke/i.test(basis)) return 'BRIM load audit';
    return basis;
  }

  function ptExternalLoadBadgeTitle(obj, baseTitle) {
    var badge = ptExternalLoadBadgeValue(obj);
    var title = baseTitle || 'External catalog layer';

    title += '; load: ' + badge + ' (' + ptExternalLoadBadgeLabel(badge) + ')';

    var note = ptExternalFriendlyLoadNote(obj);
    if (note) title += '; ' + note;

    var overrideNote = ptExternalLoadBadgeOverrideNote(obj);
    if (overrideNote) title += '; ' + overrideNote;

    var basis = ptExternalFriendlyLoadBasis(obj);
    if (basis) title += '; basis: ' + basis;

    return title;
  }

  function ptExternalCatalogIdBadgeHtml(displayNum, stableId, obj, baseTitle) {
    displayNum = ptCleanText(displayNum);
    stableId = ptCleanText(stableId);

    var num = displayNum ? ('#' + displayNum) : (stableId ? stableId.replace(/^EXT0*/, '#') : '');
    if (!num) return '';

    var badge = ptExternalLoadBadgeValue(obj);
    var title = baseTitle || 'External Layers panel display number';
    if (stableId) title += '; stable catalog ID ' + stableId;
    title = ptExternalLoadBadgeTitle(obj, title);

    return '<span class="pt-catalog-id-badge pt-catalog-load-badge pt-catalog-load-' +
      ptEscapeHtml(badge) + '" title="' + ptEscapeHtml(title) + '">' +
      ptEscapeHtml(num) + '</span> ';
  }

  function ptExternalLoadBadgeInlineHtml(obj) {
    var badge = ptExternalLoadBadgeValue(obj);
    return '<span class="pt-catalog-load-chip pt-catalog-load-' + ptEscapeHtml(badge) + '">' +
      ptEscapeHtml(badge + ' — ' + ptExternalLoadBadgeLabel(badge)) +
      '</span>';
  }

  // One behavior-preserving legend dispatch path serves catalog details and
  // active-layer details. Finalized catalog rows carry build-resolved adapter
  // keys; manual overlays fall back to the same predicates used historically.
  function ptLegacyLegendAdapterKeys(options) {
    var keys = [];
    if (ptIsDroughtMonitorStyle(options)) keys.push('drought_monitor');
    if (ptIsCpcOutlookStyle(options)) keys.push('cpc_outlook');
    if (ptIsStreamGaugeFlowStyle(options)) keys.push('stream_gauge_flow');
    if (ptIsWcrCompletedDepthStyle(options)) keys.push('wcr_completed_depth');
    if (ptIsFireYearStyle(options)) keys.push('fire_year');
    if (ptIsMlrsMineralCaseStyle(options)) keys.push('mlrs_mineral_cases');
    if (ptIsSgmaPrioritizationLayer(options)) keys.push('sgma_prioritization');
    if (ptIsNifcCurrentFirePerimeterStyle(options)) keys.push('nifc_current_fire');
    if (ptIsAmlFeatureStyle(options)) keys.push('aml_status');
    if (ptIsCalIpcRampStyle(options)) keys.push('calipc_ramp');
    if (ptIsSwrcbIrListingStatusStyle(options)) keys.push('swrcb_ir_status');
    if (ptIsSubsidenceObservationStyle(options)) keys.push('subsidence_observation');
    if (ptIsDwrTreInsarPointLocationStyle(options)) keys.push('dwr_tre_insar_points');
    if (ptIsGenericCategoricalStyle(options)) keys.push('generic_categorical');
    if (ptIsAlertCameraStyle(options)) keys.push('alert_camera');
    if (ptIsAlertCameraViewshedStyle(options)) keys.push('alert_camera_viewshed');
    return keys;
  }

  function ptLegendAdapterKeys(options) {
    options = options || {};
    var resolved = ptCleanText(options.legendAdapter || options.legend_adapter);
    var catalogRecord = !!ptCleanText(options.catalogExtId || options.external_layer_id) ||
      options.catalogIndex !== undefined && options.catalogIndex !== null;
    if (catalogRecord) {
      return resolved ? resolved.split(';').map(ptCleanText).filter(Boolean) : [];
    }
    return resolved ? resolved.split(';').map(ptCleanText).filter(Boolean) :
      ptLegacyLegendAdapterKeys(options);
  }

  function ptRegisteredLegendRenderer(adapter, options) {
    var renderers = {
      drought_monitor: function() { return ptDroughtMonitorLegendHtml(); },
      cpc_outlook: function() { return ptCpcOutlookLegendHtml(options); },
      stream_gauge_flow: function() { return ptStreamGaugeFlowLegendHtml(); },
      wcr_completed_depth: function() { return ptWcrCompletedDepthLegendHtml(); },
      fire_year: function() { return ptFireYearLegendHtml(); },
      mlrs_mineral_cases: function() { return ptMlrsMineralCasesLegendHtml(); },
      sgma_prioritization: function() { return ptSgmaPrioritizationLegendHtml(); },
      nifc_current_fire: function() { return ptNifcCurrentFireLegendHtml(); },
      aml_status: function() { return ptAmlStatusLegendHtml(); },
      calipc_ramp: function() { return ptCalIpcRampLegendHtml(options); },
      swrcb_ir_status: function() { return ptSwrcbIrListingStatusLegendHtml(); },
      subsidence_observation: function() { return ptSubsidenceObservationLegendHtml(); },
      dwr_tre_insar_points: function() { return ptDwrTreInsarPointLegendHtml(); },
      generic_categorical: function() { return ptGenericCategoricalLegendHtml(options); },
      alert_camera: function() { return ptAlertCameraLegendHtml(); },
      alert_camera_viewshed: function() { return ptAlertCameraViewshedLegendHtml(); }
    };
    return renderers[adapter] || null;
  }

  function ptRegisteredLegendHtml(options, context) {
    var html = '';
    ptLegendAdapterKeys(options).forEach(function(adapter) {
      // MLRS historically rendered in active-layer details and its map card,
      // but not in the collapsed catalog-detail pathway.
      if (context === 'catalog_detail' && adapter === 'mlrs_mineral_cases') return;
      var renderer = ptRegisteredLegendRenderer(adapter, options);
      if (renderer) html += renderer();
    });
    return html;
  }

  function ptRenderCustomLayerList() {
    var list = document.getElementById('pt-custom-layer-list');
    if (!list) return;

    var externalRecords = ptExternalPanelLayerRecords();
    ptSyncActiveExternalHeader(externalRecords.length);

    if (externalRecords.length === 0) {
      list.innerHTML = '<div class="pt-tools-muted">No external overlays added.</div>';
      return;
    }

    var html = '';

    externalRecords.forEach(function(rec) {
      var legendHtml = '';
      if (rec.legendUrl) {
        legendHtml = ' <span class="pt-tools-muted">|</span> ' +
          '<a href="' + ptEscapeHtml(rec.legendUrl) + '" target="_blank" title="' + ptEscapeHtml(rec.legendUrl) + '">Provider legend — opens external page</a>';
      }
      if (rec.legendNote) {
        legendHtml += '<div class="pt-tools-muted"><b>Map display notes:</b> ' + ptEscapeHtml(rec.legendNote) + '</div>';
      }

      legendHtml += ptRegisteredLegendHtml(rec, 'active_layer');

      if (rec.hoverFields || rec.popupFields) {
        legendHtml += '<div class="pt-tools-muted"><b>Fields:</b> curated hover/popup fields active</div>';
      }

      if (rec.whereClause) {
        legendHtml += '<div class="pt-tools-muted"><b>SQL filter:</b> <code>' + ptEscapeHtml(rec.whereClause) + '</code></div>';
      }

      if (rec.loadMode === 'current_view') {
        if (rec.featureCount !== null && !isNaN(rec.featureCount)) {
          legendHtml += '<div class="pt-tools-muted"><b>Current snapshot:</b> ' +
            Number(rec.featureCount).toLocaleString() + ' feature(s). Pan/zoom, then use Refresh current view to requery.</div>';
        } else {
          legendHtml += '<div class="pt-tools-muted"><b>Snapshot:</b> pan/zoom, then use Refresh current view to requery.</div>';
        }
      }
      if (rec.loadMode === 'live_snapshot') {
        legendHtml += '<div class="pt-tools-muted"><b>Complete live snapshot:</b> ' +
          (rec.featureCount !== null && !isNaN(rec.featureCount) ?
            Number(rec.featureCount).toLocaleString() + ' feature(s); ' : '') +
          'retrieved ' + ptEscapeHtml(rec.retrievalUtc || 'this session') + '.</div>';
      }

      var refreshButtonHtml = ptRefreshButtonHtml(rec, 'pt-active-list-refresh-btn');
      var uicLabelButtonHtml = ptIsUicAquiferExemptionStyle(rec) ?
        '<button type="button" class="pt-tools-mini-btn" data-pt-uic-label-toggle="' +
          rec.id + '" title="Show or hide compact labels for this live UIC layer">lbl</button>' : '';
      var activeActionHtml =
        '<div class="pt-active-layer-action-wrap">' +
          '<button type="button" class="pt-tools-mini-btn pt-external-remove-btn" data-pt-custom-remove="' + rec.id + '">Remove</button>' +
          uicLabelButtonHtml +
          refreshButtonHtml +
        '</div>';

      html +=
        '<div class="pt-custom-layer-row' + (rec.refreshing ? ' pt-custom-layer-refreshing' : '') + '">' +
          '<label title="' + ptEscapeHtml(rec.url) + '">' +
            '<input type="checkbox" data-pt-custom-toggle="' + rec.id + '" ' +
              (rec.visible ? 'checked' : '') + '/> ' +
            '<span class="pt-custom-swatch" style="background:' + rec.color + ';"></span>' +
            ptExternalCatalogIdBadgeHtml(
              rec.catalogDisplayNum,
              rec.catalogExtId,
              rec,
              'External Layers panel display number'
            ) +
            ptEscapeHtml(rec.name) +
          '</label>' +
          activeActionHtml +
          '<div class="pt-tools-muted">' +
            ptEscapeHtml(rec.sourceType) + ' — ' +
            '<a href="' + ptEscapeHtml(rec.url) + '" target="_blank" title="' + ptEscapeHtml(rec.url) + '">' +
              ptEscapeHtml(ptShortUrl(rec.url)) +
            '</a>' +
            legendHtml +
          '</div>' +
        '</div>';
    });

    list.innerHTML = html;
  }

  function ptParseArcgisLayerUrl(url, serverWord) {
    var re = new RegExp('^(.*\\/' + serverWord + ')\\/(\\d+)$', 'i');
    var m = String(url).match(re);

    if (m) {
      return {
        serviceUrl: m[1],
        layerId: Number(m[2]),
        isLayerEndpoint: true
      };
    }

    return {
      serviceUrl: url,
      layerId: null,
      isLayerEndpoint: false
    };
  }

  function ptAddGeoJsonLayer(name, url, clickable, color, options) {
    options = options || {};
    ptSetExternalStatus('Loading GeoJSON...', false, options);

    fetch(url)
      .then(function(resp) {
        if (!resp.ok) {
          throw new Error('HTTP ' + resp.status + ' ' + resp.statusText);
        }
        return resp.json();
      })
      .then(function(geojson) {
        geojson = ptSortFeatureCollectionForDrawing(geojson, options);

        var layer = L.geoJSON(geojson, {
          pane: 'pane_pt_custom_polygon',
          style: function(feature) {
            return ptStyleForExternalFeature(feature, color, 0.14, options);
          },
          pointToLayer: ptPointForColor(color, options),
          onEachFeature: ptOnEachFeature(clickable, options)
        });

        options.featureCount = geojson && Array.isArray(geojson.features) ? geojson.features.length : null;
        ptAddCustomRecord(name, layer, color, 'GeoJSON', url, options);
        ptSetExternalStatus('GeoJSON layer added' + (options.featureCount !== null ? ': ' + Number(options.featureCount).toLocaleString() + ' feature(s).' : '.') , false, options);
      })
      .catch(function(err) {
        console.error(err);
        ptSetExternalStatus(
          'Could not load GeoJSON. The URL may block browser access, require HTTPS, or not return valid GeoJSON.',
          true,
          options
        );
      });
  }

  function ptAddArcgisFeatureLayerCurrentView(name, url, clickable, color, whereClause, options) {

    options = options || {};

    ptEnsureEsriLeaflet(function(ok) {

      if (!ok || !L.esri || !L.esri.query) {
        ptSetExternalStatus(
          'Could not load Esri Leaflet. Current-view FeatureServer queries require browser access to the Esri Leaflet library.',
          true,
          options
        );
        return;
      }

      try {
        var bounds = map.getBounds();
        var whereText = ptCleanText(whereClause) || '1=1';

        ptSetExternalStatus('Querying FeatureServer features that intersect the current map view...', false, options);

        ptRunArcgisCurrentViewFeatureQuery(url, bounds, whereText, options, function(error, featureCollection, response) {

          if (error) {
            console.error(error);
            ptSetExternalStatus(
              'Current-view FeatureServer query failed. Try zooming in farther, clearing the SQL filter, or unchecking current-view mode to add the layer as a live FeatureServer overlay.',
              true,
              options
            );
            return;
          }

          featureCollection = featureCollection || {
            type: 'FeatureCollection',
            features: []
          };

          featureCollection = ptSortFeatureCollectionForDrawing(featureCollection, options);
          var features = featureCollection.features || [];

          if (features.length === 0) {
            ptSetExternalStatus('No features intersected the current map view. Try panning/zooming, clearing the SQL filter, or unchecking current-view mode.', true, options);
            return;
          }

          var subsidenceClassSummary = '';
          if (ptIsSubsidenceObservationStyle(options)) {
            featureCollection = ptAnnotateSubsidenceObservationFeatureCollection(featureCollection);
            options.subsidenceObservationClassCounts = ptSubsidenceObservationClassCountsFromFeatures(features);
            subsidenceClassSummary = ptSubsidenceObservationClassSummary(options.subsidenceObservationClassCounts);
            if (window.console && console.info) {
              console.info('BRIM subsidence observation mapped-location class counts', options.subsidenceObservationClassCounts);
            }
          }

          var layer = ptExternalFeatureCollectionLayer(
            featureCollection,
            color,
            0.14,
            options,
            clickable
          );

          options.loadMode = 'current_view';
          options.whereClause = whereText;
          options.clickable = !!clickable;
          options.featureCount = features.length;

          var rec = ptAddCustomRecord(
            name + ' (current view)',
            layer,
            color,
            'ArcGIS FeatureServer — current view',
            url,
            options
          );

          if (rec && (rec.opsPromotedSkipped || rec.catalogLoadSkipped)) {
            return;
          }

          rec.whereClause = whereText;
          rec.featureCount = features.length;
          ptUpdateOpsPromotedStreamflowMetric(options, features.length, response && response.exceededTransferLimit ? 'Transfer limit reported.' : '');
          if (!options.opsPromotedKey) {
            ptSetManualCurrentViewRecord(rec);
          }

          ptSetExternalStatus(
            'Current-view FeatureServer layer added: ' +
              features.length.toLocaleString() +
              ' feature(s). This is a static snapshot of the current map view.' +
              (subsidenceClassSummary ? ' Classes: ' + subsidenceClassSummary + '.' : ''),
            false,
            options
          );
        });

      } catch (err) {
        console.error(err);
        ptSetExternalStatus('Could not query FeatureServer for the current map view.', true, options);
      }
    });
  }


  function ptAddArcgisMapLayerCurrentView(name, url, clickable, color, whereClause, options) {

    options = options || {};

    ptEnsureEsriLeaflet(function(ok) {

      if (!ok || !L.esri || !L.esri.query) {
        ptSetExternalStatus(
          'Could not load Esri Leaflet. Current-view MapServer queries require browser access to the Esri Leaflet library.',
          true,
          options
        );
        return;
      }

      try {
        var parsed = ptParseArcgisLayerUrl(url, 'MapServer');

        if (!parsed.isLayerEndpoint) {
          ptResolveParentArcgisUrlForCurrentView(url, 'map', 'pt-manual-action-note');
          return;
        }

        var bounds = map.getBounds();
        var whereText = ptCleanText(whereClause) || '1=1';

        ptSetExternalStatus('Querying MapServer features that intersect the current map view...', false, options);

        var query = L.esri.query({
          url: url
        })
          .where(whereText)
          // Use intersects rather than within. Dense analytical layers such as
          // contours often cross the map extent; intersects() is the better
          // screening default for current-view snapshots.
          .intersects(bounds);

        var outFields = ptOutFieldsArray(options);

        if (outFields.length > 0 && typeof query.fields === 'function') {
          query = query.fields(outFields);
        }

        query.run(function(error, featureCollection, response) {

          if (error) {
            console.error(error);
            ptSetExternalStatus(
              'Current-view MapServer query failed. Try zooming in farther, checking the SQL filter, or adding the layer as a visual MapServer overlay instead.',
              true,
              options
            );
            return;
          }

          featureCollection = featureCollection || {
            type: 'FeatureCollection',
            features: []
          };

          featureCollection = ptSortFeatureCollectionForDrawing(featureCollection, options);
          var features = featureCollection.features || [];

          if (features.length === 0) {
            ptSetExternalStatus('No MapServer features intersected the current map view. Try panning/zooming or clearing the SQL filter.', true, options);
            return;
          }

          var layer = ptExternalFeatureCollectionLayer(
            featureCollection,
            color,
            0.14,
            options,
            clickable
          );

          options.loadMode = 'current_view';
          options.whereClause = whereText;
          options.clickable = !!clickable;
          options.featureCount = features.length;

          var rec = ptAddCustomRecord(
            name + ' (current view)',
            layer,
            color,
            'ArcGIS MapServer — current view',
            url,
            options
          );

          if (rec && (rec.opsPromotedSkipped || rec.catalogLoadSkipped)) {
            return;
          }

          rec.whereClause = whereText;
          rec.featureCount = features.length;
          ptUpdateOpsPromotedStreamflowMetric(options, features.length, response && response.exceededTransferLimit ? 'Transfer limit reported.' : '');
          if (!options.opsPromotedKey) {
            ptSetManualCurrentViewRecord(rec);
          }

          var limitNote = '';
          if (response && response.exceededTransferLimit) {
            limitNote = ' The service reported a transfer limit; zoom in and refresh current view for a more complete local snapshot.';
          } else if (features.length >= 1000) {
            limitNote = ' This is a large snapshot; if it looks incomplete or sluggish, zoom in and refresh current view.';
          }

          ptSetExternalStatus(
            'Current-view MapServer layer added: ' +
              features.length.toLocaleString() +
              ' feature(s). This is a static snapshot of the current map view.' +
              limitNote,
            false,
            options
          );
        });

      } catch (err) {
        console.error(err);
        ptSetExternalStatus('Could not query MapServer for the current map view.', true, options);
      }
    });
  }

  function ptFetchArcgisJson(url, params) {
    var fetchOptions = {cache: 'no-store'};
    if (params) {
      var body = new URLSearchParams();
      Object.keys(params).forEach(function(key) {
        if (params[key] !== undefined && params[key] !== null) {
          body.append(key, String(params[key]));
        }
      });
      fetchOptions.method = 'POST';
      fetchOptions.headers = {
        'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'
      };
      fetchOptions.body = body.toString();
    }
    return fetch(url, fetchOptions)
      .then(function(response) {
        if (!response.ok) {
          throw new Error('HTTP ' + response.status + ' ' + response.statusText);
        }
        return response.json();
      })
      .then(function(json) {
        if (json && json.error) {
          var details = Array.isArray(json.error.details) ? json.error.details.join(' | ') : '';
          throw new Error(
            'ArcGIS error ' + (json.error.code || '') + ': ' +
            (json.error.message || 'request failed') + (details ? ' | ' + details : '')
          );
        }
        return json;
      });
  }

  function ptRunArcgisAllFeatureQuery(url, whereText, options) {
    options = options || {};
    whereText = ptCleanText(whereText) || '1=1';
    var metadataUrl = url + (url.indexOf('?') >= 0 ? '&' : '?') + 'f=json';
    var queryUrl = url.replace(/\/+$/, '') + '/query';
    var metadata = null;
    var objectIdField = '';
    var objectIds = [];

    return ptFetchArcgisJson(metadataUrl)
      .then(function(meta) {
        metadata = meta || {};
        return ptFetchArcgisJson(queryUrl, {
          where: whereText,
          returnIdsOnly: 'true',
          returnGeometry: 'false',
          f: 'json'
        });
      })
      .then(function(idResponse) {
        objectIdField = ptCleanText(
          metadata.objectIdField ||
          metadata.objectIdFieldName ||
          idResponse.objectIdFieldName
        );
        objectIds = (idResponse.objectIds || []).map(Number).filter(function(id) {
          return isFinite(id);
        }).sort(function(a, b) { return a - b; });
        if (!objectIdField && objectIds.length) {
          throw new Error('Service did not report its object-ID field.');
        }
        var maxRecordCount = Number(metadata.maxRecordCount || 1000);
        if (!isFinite(maxRecordCount) || maxRecordCount < 1) maxRecordCount = 1000;
        var batchSize = Math.max(1, Math.min(maxRecordCount, 400));
        var batches = [];
        for (var i = 0; i < objectIds.length; i += batchSize) {
          batches.push(objectIds.slice(i, i + batchSize));
        }
        var outFields = ptOutFieldsArray(options);
        var outFieldsText = outFields.length ? outFields.join(',') : '*';
        var featureBatches = [];
        var chain = Promise.resolve();
        batches.forEach(function(batch) {
          chain = chain.then(function() {
            return ptFetchArcgisJson(queryUrl, {
              where: objectIdField + ' IN (' + batch.join(',') + ')',
              outFields: outFieldsText,
              returnGeometry: 'true',
              outSR: '4326',
              orderByFields: objectIdField + ' ASC',
              f: 'geojson'
            }).then(function(featureCollection) {
              var batchFeatures = featureCollection && Array.isArray(featureCollection.features) ?
                featureCollection.features : [];
              featureBatches = featureBatches.concat(batchFeatures);
              if (window.BRIM && window.BRIM.uicExplorer &&
                  window.BRIM.uicExplorer.setExternalLoading) {
                window.BRIM.uicExplorer.setExternalLoading(
                  options.catalogExtId,
                  true,
                  'Retrieved ' + featureBatches.length.toLocaleString() +
                    ' of ' + objectIds.length.toLocaleString() + ' records…'
                );
              }
            });
          });
        });
        return chain.then(function() {
          if (featureBatches.length !== objectIds.length) {
            throw new Error(
              'Service reported ' + objectIds.length + ' object IDs but returned ' +
              featureBatches.length + ' GeoJSON features.'
            );
          }
          return {
            featureCollection: {
              type: 'FeatureCollection',
              features: featureBatches
            },
            metadata: metadata,
            objectIdField: objectIdField,
            objectIds: objectIds
          };
        });
      });
  }

  function ptAddArcgisFeatureLayerSnapshot(name, url, clickable, color, whereClause, options) {
    options = options || {};
    if (window.BRIM && window.BRIM.uicExplorer &&
        window.BRIM.uicExplorer.catalogColor) {
      color = window.BRIM.uicExplorer.catalogColor(options.catalogExtId) || color;
    }
    options.loadMode = 'live_snapshot';
    options.whereClause = ptCleanText(whereClause) || '1=1';
    options.sourceServiceUrl = url;
    if (window.BRIM && window.BRIM.uicExplorer &&
        window.BRIM.uicExplorer.setExternalLoading) {
      window.BRIM.uicExplorer.setExternalLoading(
        options.catalogExtId,
        true,
        'Checking service metadata and object IDs…'
      );
    }
    ptSetExternalStatus(
      'Retrieving the complete authoritative UIC FeatureServer layer by object-ID batches…',
      false,
      options
    );
    ptRunArcgisAllFeatureQuery(url, options.whereClause, options)
      .then(function(result) {
        if (options.catalogIndex !== null && options.catalogIndex !== undefined &&
            options.catalogLoadToken &&
            ptCatalogLoadWasCancelled(options.catalogIndex, options.catalogLoadToken)) {
          return;
        }
        var featureCollection = ptSortFeatureCollectionForDrawing(
          result.featureCollection,
          options
        );
        var features = featureCollection.features || [];
        options.featureCount = features.length;
        options.retrievalUtc = new Date().toISOString();
        options.clickable = !!clickable;
        var layer = ptExternalFeatureCollectionLayer(
          featureCollection,
          color,
          0.04,
          options,
          clickable
        );
        var rec = ptAddCustomRecord(
          name,
          layer,
          color,
          'ArcGIS FeatureServer — complete live snapshot',
          url,
          options
        );
        if (rec && (rec.catalogLoadSkipped || rec.opsPromotedSkipped)) return;
        if (window.BRIM && window.BRIM.uicExplorer) {
          window.BRIM.uicExplorer.setExternalLoading(options.catalogExtId, false, '');
        }
        ptSetExternalStatus(
          'Authoritative UIC live snapshot added: ' +
            features.length.toLocaleString() +
            ' feature(s), retrieved ' + options.retrievalUtc + '.',
          false,
          options
        );
      })
      .catch(function(error) {
        console.error(error);
        if (window.BRIM && window.BRIM.uicExplorer &&
            window.BRIM.uicExplorer.setExternalError) {
          window.BRIM.uicExplorer.setExternalError(
            options.catalogExtId,
            error && error.message ? error.message : 'Live retrieval failed.'
          );
        }
        ptSetExternalStatus(
          'UIC live snapshot failed. No partial or previous layer was replaced. ' +
            (error && error.message ? error.message : ''),
          true,
          options
        );
      });
  }

  function ptAddArcgisFeatureLayer(name, url, clickable, color, whereClause, options) {

    options = options || {};

    ptEnsureEsriLeaflet(function(ok) {

      if (!ok || !L.esri || !L.esri.featureLayer) {
        ptSetExternalStatus(
          'Could not load Esri Leaflet. ArcGIS FeatureServer layers require browser access to the Esri Leaflet library.',
          true,
          options
        );
        return;
      }

      try {
        var layerOptions = {
          url: url,
          pane: 'pane_pt_custom_polygon',
          simplifyFactor: 0.35,
          precision: 5,
          style: function(feature) {
            return ptStyleForExternalFeature(feature, color, 0.13, options);
          },
          pointToLayer: ptPointForColor(color, options),
          onEachFeature: ptOnEachFeature(clickable, options)
        };

        var whereText = ptCleanText(whereClause);

        if (whereText) {
          layerOptions.where = whereText;
        }

        var liveOutFields = ptOutFieldsArray(options);

        if (liveOutFields.length > 0) {
          layerOptions.fields = liveOutFields;
        }

        var layer = L.esri.featureLayer(layerOptions);

        layer.on('requesterror', function(e) {
          console.warn('PT2 custom FeatureServer request error:', e);
          ptSetExternalStatus('FeatureServer request failed or was blocked by the browser/service.', true, options);
        });

        ptAddCustomRecord(name, layer, color, 'ArcGIS FeatureServer', url, options);
        ptSetExternalStatus('ArcGIS FeatureServer layer added.', false, options);

      } catch (err) {
        console.error(err);
        ptSetExternalStatus('Could not add FeatureServer layer. Try a specific layer URL ending in /FeatureServer/0.', true, options);
      }
    });
  }

  function ptAddArcgisTiledMapLayer(name, url, clickable, color, options) {

    options = options || {};

    ptEnsureEsriLeaflet(function(ok) {

      if (!ok || !L.esri || !L.esri.tiledMapLayer) {
        ptSetStatus(
          'Could not load Esri Leaflet. ArcGIS tiled MapServer layers require browser access to the Esri Leaflet library.',
          true
        );
        return;
      }

      try {
        var parsed = ptParseArcgisLayerUrl(url, 'MapServer');

        if (parsed.isLayerEndpoint) {
          ptSetStatus(
            'Tiled MapServer overlays need the parent service URL ending in /MapServer, not a sublayer URL such as /MapServer/0.',
            true
          );
          return;
        }

        var layer = L.esri.tiledMapLayer({
          url: parsed.serviceUrl,
          opacity: 0.65,
          pane: 'pane_pt_custom_polygon'
        });

        layer.on('loading', function() {
          ptSetStatus('Loading tiled MapServer overlay...', false);
        });

        layer.on('load', function() {
          ptSetStatus('ArcGIS tiled MapServer overlay loaded.', false);
        });

        layer.on('requesterror', function(e) {
          console.warn('PT2 custom tiled MapServer request error:', e);
          ptSetStatus('Tiled MapServer request failed or was blocked by the browser/service.', true);
        });

        options.loadMode = 'tiled';
        options.identifyServiceUrl = parsed.serviceUrl;
        options.identifyLayerId = 0;
        options.identifyMode = options.identifyUrl ? 'featureserver-query' : 'mapserver-identify';
        options.visualIdentify = !!(clickable || options.hoverFields || options.popupFields || options.popupLinkTemplate || options.identifyUrl);
        ptAddCustomRecord(name, layer, color, 'ArcGIS tiled MapServer', url, options);
        ptSetStatus('Tiled MapServer overlay requested. Waiting for service tiles to load...', false);

        if (clickable) {
          ptSetStatus(
            'Tiled MapServer visual overlay requested. BRIM will use identify/query for hover or click when configured and supported by the service.',
            false
          );
        }

      } catch (err) {
        console.error(err);
        ptSetStatus('Could not add tiled MapServer layer. Try a public cached MapServer parent service URL.', true);
      }
    });
  }


  function ptAddArcgisImageLayer(name, url, clickable, color, options) {

    options = options || {};

    ptEnsureEsriLeaflet(function(ok) {

      if (!ok || !L.esri || !L.esri.imageMapLayer) {
        ptSetStatus(
          'Could not load Esri Leaflet. ArcGIS ImageServer layers require browser access to the Esri Leaflet library.',
          true
        );
        return;
      }

      try {
        var opts = {
          url: ptNormalizeUrl(url),
          opacity: ptNumberOrNull(options.defaultOpacity) || 0.70,
          pane: 'pane_pt_custom_polygon'
        };

        var layer = L.esri.imageMapLayer(opts);
        var ptImageLoaded = false;

        layer.on('loading', function() {
          ptSetExternalStatus('Loading ImageServer visual overlay...', false, options);
        });

        layer.on('load', function() {
          ptImageLoaded = true;
          ptFinalizeVisualOverlayAfterSettle(
            options,
            'ArcGIS ImageServer visual overlay loaded and ready.',
            false
          );
        });

        layer.on('requesterror', function(e) {
          ptImageLoaded = true;
          console.warn('PT2 custom ImageServer request error:', e);
          ptSetExternalStatus('ImageServer request failed or was blocked by the browser/service.', true, options);
        });

        // ImageServer rows remain provider-rendered visual overlays, but selected
        // catalog rows can use ImageServer identify to expose raw pixel values
        // on hover/click without changing the map symbology.
        options.identifyUrl = options.identifyUrl || ptNormalizeUrl(url);
        options.visualIdentify = !!(clickable || options.hoverFields || options.popupFields || options.popupLinkTemplate || options.identifyUrl);
        ptAddCustomRecord(name, layer, color, 'ArcGIS ImageServer', url, options);

        window.setTimeout(function() {
          if (!ptImageLoaded) {
            ptSetExternalStatus('ImageServer overlay requested. Waiting for service image tiles...', false, options);
          }
        }, 250);

      } catch (err) {
        console.error(err);
        ptSetExternalStatus('Could not add ImageServer visual overlay: ' + err.message, true, options);
      }
    });
  }

  function ptAddArcgisMapLayer(name, url, clickable, color, options) {

    options = options || {};

    ptEnsureEsriLeaflet(function(ok) {

      if (!ok || !L.esri || !L.esri.dynamicMapLayer) {
        ptSetStatus(
          'Could not load Esri Leaflet. ArcGIS MapServer layers require browser access to the Esri Leaflet library.',
          true
        );
        return;
      }

      try {
        var parsed = ptParseArcgisLayerUrl(url, 'MapServer');
        var whereText = ptCleanText(options.whereClause);

        var opts = {
          url: parsed.serviceUrl,
          opacity: 0.55,
          pane: 'pane_pt_custom_polygon'
        };

        if (!parsed.isLayerEndpoint && ptIsNwmStreamAnalysisLayer(options, url)) {
          opts.layers = ptNwmStreamAnalysisLayerIds(options);
          opts.opacity = 0.82;
        }

        if (parsed.isLayerEndpoint) {
          opts.layers = [parsed.layerId];

          // ArcGIS MapServer layers are rendered by the server as images.
          // A SQL drawing filter must therefore be passed as layerDefs rather
          // than as a FeatureServer-style where query.
          if (whereText) {
            opts.layerDefs = {};
            opts.layerDefs[parsed.layerId] = whereText;
          }
        } else if (whereText) {
          ptSetStatus(
            'MapServer SQL drawing filters need a specific layer URL ending in /MapServer/0, /MapServer/1, etc. This URL ends at the parent /MapServer service. Open the service metadata and use the number listed under Layers, or clear the SQL filter.',
            true
          );
          return;
        }

        var layer = L.esri.dynamicMapLayer(opts);
        var ptVisualMapLoaded = false;

        layer.on('loading', function() {
          ptSetExternalStatus('Loading MapServer image overlay...', false, options);
        });

        layer.on('load', function() {
          ptVisualMapLoaded = true;
          var readyMsg = '';
          if (whereText) {
            readyMsg = 'ArcGIS MapServer image overlay loaded and ready with drawing filter: ' + whereText;
          } else if (ptIsNwmStreamAnalysisLayer(options, url)) {
            readyMsg = 'NWM Stream Analysis image overlay loaded and ready using scale-dependent streamflow sublayers.';
          } else {
            readyMsg = 'ArcGIS MapServer image overlay loaded and ready.';
          }
          ptFinalizeVisualOverlayAfterSettle(options, readyMsg, false);
        });

        layer.on('requesterror', function(e) {
          ptVisualMapLoaded = true;
          console.warn('PT2 custom MapServer request error:', e);
          ptSetExternalStatus('MapServer request failed or was blocked by the browser/service.', true, options);
        });

        options.identifyServiceUrl = parsed.serviceUrl;
        options.identifyLayerId = parsed.isLayerEndpoint ? parsed.layerId : null;
        options.identifyMode = options.identifyUrl ? 'featureserver-query' : 'mapserver-identify';
        options.visualIdentify = !!(clickable || options.hoverFields || options.popupFields || options.popupLinkTemplate || options.identifyUrl);
        ptAddCustomRecord(name, layer, color, 'ArcGIS MapServer', url, options);

        window.setTimeout(function() {
          if (!ptVisualMapLoaded) {
            if (whereText) {
              ptSetExternalStatus('MapServer image overlay requested with drawing filter: ' + whereText, false, options);
            } else {
              ptSetExternalStatus('MapServer image overlay requested. Waiting for the service image to load...', false, options);
            }
          }
        }, 250);

        if (clickable) {
          window.setTimeout(function() {
            if (!ptVisualMapLoaded) {
              ptSetExternalStatus(
                'MapServer visual overlay requested. BRIM will use identify/query for hover or click when configured and supported by the service.',
                false,
                options
              );
            }
          }, 500);
        }

      } catch (err) {
        console.error(err);
        ptSetStatus('Could not add MapServer layer. Try a public MapServer service or layer endpoint.', true);
      }
    });
  }

  function ptHandleAddCustomLayer(actionNoteId) {
    var urlInput = document.getElementById('pt-custom-url');
    var nameInput = document.getElementById('pt-custom-name');
    var typeInput = document.getElementById('pt-custom-type');
    var clickableInput = document.getElementById('pt-custom-clickable');
    var currentViewInput = document.getElementById('pt-custom-current-view');
    var whereInput = document.getElementById('pt-custom-where');
    var legendUrlInput = document.getElementById('pt-custom-legend-url');
    var legendNoteInput = document.getElementById('pt-custom-legend-note');
    var minZoomLiveInput = document.getElementById('pt-custom-min-zoom-live');
    var minZoomCurrentViewInput = document.getElementById('pt-custom-min-zoom-current-view');
    var popupFieldsInput = document.getElementById('pt-custom-popup-fields');
    var popupAliasesInput = document.getElementById('pt-custom-popup-aliases');
    var popupLinkTemplateInput = document.getElementById('pt-custom-popup-link-template');
    var popupLinkLabelInput = document.getElementById('pt-custom-popup-link-label');
    var hoverFieldsInput = document.getElementById('pt-custom-hover-fields');
    var hoverAliasesInput = document.getElementById('pt-custom-hover-aliases');
    var hoverBoldFieldsInput = document.getElementById('pt-custom-hover-bold-fields');
    var hoverNoLabelFieldsInput = document.getElementById('pt-custom-hover-no-label-fields');
    var hoverRoundFieldsInput = document.getElementById('pt-custom-hover-round-fields');
    var hoverShowNativeFieldNamesInput = document.getElementById('pt-custom-hover-show-native-field-names');
    var hoverAliasesInput = document.getElementById('pt-custom-hover-aliases');
    var hoverBoldFieldsInput = document.getElementById('pt-custom-hover-bold-fields');
    var hoverNoLabelFieldsInput = document.getElementById('pt-custom-hover-no-label-fields');
    var hoverRoundFieldsInput = document.getElementById('pt-custom-hover-round-fields');
    var hoverShowNativeFieldNamesInput = document.getElementById('pt-custom-hover-show-native-field-names');
    var defaultLabelFieldInput = document.getElementById('pt-custom-default-label-field');
    var outFieldsInput = document.getElementById('pt-custom-out-fields');
    var styleFieldCandidatesInput = document.getElementById('pt-custom-style-field-candidates');
    var defaultStyleFieldInput = document.getElementById('pt-custom-default-style-field');
    var fieldCurationNotesInput = document.getElementById('pt-custom-field-curation-notes');
    var showNativeFieldNamesInput = document.getElementById('pt-custom-show-native-field-names');

    actionNoteId = actionNoteId || 'pt-manual-action-note';
    ptClearActionNotes();

    if (ptManualAddInProgress && actionNoteId === 'pt-manual-action-note') {
      ptSetInlineNote('pt-manual-action-note', 'Adding manual overlay…', false);
    }

    var url = ptNormalizeUrl(urlInput ? urlInput.value : '');
    var name = nameInput && nameInput.value.trim() ?
      nameInput.value.trim() :
      'Custom layer ' + (ptExternalPanelLayerCount() + 1);

    var selectedType = typeInput ? typeInput.value : 'auto';
    var detectedType = ptDetectServiceType(url, selectedType);
    var clickable = clickableInput ? clickableInput.checked : false;
    var currentViewOnly = currentViewInput ? currentViewInput.checked : false;
    var rawWhereClause = whereInput ? whereInput.value.trim() : '';

    // Quick-add catalog rows can carry a default load mode that is more
    // specific than the manual current-view checkbox.  For example, cached
    // ArcGIS MapServer services such as the U.S. Drought Monitor should load
    // through Esri Leaflet's tiledMapLayer path, not the dynamicMapLayer path.
    var catalogDefaultLoadMode = '';
    var pendingCatalogRecord = null;
    if (ptPendingCatalogIdx !== null && PT2_CATALOG[ptPendingCatalogIdx]) {
      pendingCatalogRecord = PT2_CATALOG[ptPendingCatalogIdx];
      catalogDefaultLoadMode = ptCatalogField(pendingCatalogRecord, 'default_load_mode').toLowerCase();
    }

    var effectiveLoadMode = catalogDefaultLoadMode ||
      ptManualSelectedLoadMode(currentViewOnly);
    var sqlState = ptManualSqlState(
      url,
      selectedType,
      currentViewOnly,
      effectiveLoadMode,
      rawWhereClause
    );
    // A nonblank SQL value on a parent visual MapServer is retained only long
    // enough for the existing /MapServer/N validation message. Unsupported
    // loaders otherwise receive and record no SQL value.
    var whereClause = sqlState.appliedWhereClause ||
      sqlState.validationWhereClause;

    var layerOptions = {
      legendUrl: legendUrlInput ? legendUrlInput.value : '',
      legendNote: legendNoteInput ? legendNoteInput.value : '',
      legendAdapter: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'legend_adapter') : '',
      infoAdapter: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'info_adapter') : '',
      minZoomLive: minZoomLiveInput ? minZoomLiveInput.value : '',
      minZoomCurrentView: minZoomCurrentViewInput ? minZoomCurrentViewInput.value : '',
      whereClause: whereClause,
      loadMode: effectiveLoadMode,
      popupFields: popupFieldsInput ? popupFieldsInput.value : '',
      popupAliases: popupAliasesInput ? popupAliasesInput.value : '',
      popupLinkTemplate: popupLinkTemplateInput ? popupLinkTemplateInput.value : '',
      popupLinkLabel: popupLinkLabelInput ? popupLinkLabelInput.value : '',
      identifyUrl: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'identify_url') : '',
      hoverFields: hoverFieldsInput ? hoverFieldsInput.value : '', 
      hoverAliases: hoverAliasesInput ? hoverAliasesInput.value : '',
      hoverBoldFields: hoverBoldFieldsInput ? hoverBoldFieldsInput.value : '',
      hoverNoLabelFields: hoverNoLabelFieldsInput ? hoverNoLabelFieldsInput.value : '',
      hoverRoundFields: hoverRoundFieldsInput ? hoverRoundFieldsInput.value : '',
      hoverShowNativeFieldNames: hoverShowNativeFieldNamesInput ? hoverShowNativeFieldNamesInput.value : '',
      defaultLabelField: defaultLabelFieldInput ? defaultLabelFieldInput.value : '',
      outFields: outFieldsInput ? outFieldsInput.value : '',
      styleFieldCandidates: styleFieldCandidatesInput ? styleFieldCandidatesInput.value : '',
      defaultStyleField: defaultStyleFieldInput ? defaultStyleFieldInput.value : '',
      defaultStyleMethod: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'default_style_method') : '',
      styleUnits: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'style_units') : '',
      styleLegendTitle: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'style_legend_title') : '',
      styleDirection: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'style_direction') : '',
      layerName: name,
      fieldCurationNotes: fieldCurationNotesInput ? fieldCurationNotesInput.value : '',
      showNativeFieldNames: showNativeFieldNamesInput ? showNativeFieldNamesInput.value : '',
      clickable: clickable,
      catalogIndex: ptPendingCatalogIdx,
      catalogExtId: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'external_layer_id') : '',
      catalogDisplayNum: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'external_display_num') : '',
      loadBadge: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'load_badge') : '',
      loadBadgeOverride: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'load_badge_override') : '',
      loadScore: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'load_score') : '',
      loadScoreOverride: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'load_score_override') : '',
      loadNote: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'load_note') : '',
      loadAuditBasis: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'load_audit_basis') : '',
      loadNTests: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'load_n_tests') : '',
      loadSuccessRate: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'load_success_rate') : '',
      loadMedianSeconds: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'load_median_seconds') : '',
      loadP90Seconds: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'load_p90_seconds') : '',
      loadMaxSeconds: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'load_max_seconds') : '',
      loadFeatureCapRate: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'load_feature_cap_rate') : '',
      sourcePage: pendingCatalogRecord ? ptCatalogField(pendingCatalogRecord, 'source_page') : '',
      sourceServiceUrl: url,
      catalogKey: ptPendingCatalogIdx !== null && PT2_CATALOG[ptPendingCatalogIdx] ? ptCatalogRecordKey(PT2_CATALOG[ptPendingCatalogIdx]) : '',
      catalogLoadToken: ptPendingCatalogIdx !== null ? (ptCatalogActiveLoadTokens[ptPendingCatalogIdx] || '') : ''
    };

    // This add request has captured the pending quick-add catalog index.
    ptPendingCatalogIdx = null;

    if (!url) {
      var missingUrlMessage =
        'Paste a public service URL first, or choose a catalog layer.';
      ptSetStatus(missingUrlMessage, true);
      ptSetUicPreflightError(layerOptions, missingUrlMessage);
      return;
    }

    var requestedUicLayer = ptIsUicAquiferExemptionStyle(layerOptions);
    var activeUicLayerCount = ptCustomLayers.filter(function(record) {
      return ptIsUicAquiferExemptionStyle(record);
    }).length;
    var layerLimitReached = requestedUicLayer ?
      activeUicLayerCount >= 4 :
      ptExternalPanelLayerCount() >= PT2_MAX_CUSTOM_LAYERS;
    if (layerLimitReached) {
      var limitMessage = requestedUicLayer ?
        'All four curated UIC External sources are already active.' :
        'Limit reached: remove a custom layer before adding another.';
      ptSetStatus(limitMessage, true);
      ptSetUicPreflightError(layerOptions, limitMessage);
      return;
    }

    var problem = ptExplainUrlProblem(url, detectedType);
    if (problem) {
      ptSetStatus(problem, true);
      ptSetUicPreflightError(layerOptions, problem);
      return;
    }

    if (detectedType === 'map' && clickable && !currentViewOnly) {
      ptSetStatus('Adding visual MapServer overlay with BRIM identify-enabled hover/click where the service supports identify/query.', false);
    }

    if (currentViewOnly && (detectedType === 'map' || detectedType === 'feature') && ptIsParentArcgisServiceUrl(url)) {
      // Preserve the quick-add catalog row through the asynchronous parent
      // service metadata lookup.  The first ptHandleAddCustomLayer() call
      // captured and cleared ptPendingCatalogIdx above; the recursive call
      // after auto-resolving /MapServer -> /MapServer/N needs it again so the
      // active row can be highlighted and duplicate quick-adds prevented.
      if (layerOptions.catalogIndex !== null && !isNaN(layerOptions.catalogIndex)) {
        ptPendingCatalogIdx = layerOptions.catalogIndex;
      }
      ptResolveParentArcgisUrlForCurrentView(url, detectedType, actionNoteId);
      return;
    }

    if (detectedType === 'feature') {
      if (currentViewOnly) {
        if (!ptZoomCheck(layerOptions.minZoomCurrentView, 'Current-view FeatureServer loading', actionNoteId)) {
          ptSetUicPreflightError(
            layerOptions,
            'Load stopped at the current zoom. Zoom in and retry.'
          );
          return;
        }
      } else {
        if (!ptZoomCheck(layerOptions.minZoomLive, 'Live FeatureServer loading', actionNoteId)) {
          ptSetUicPreflightError(
            layerOptions,
            'Load stopped at the current zoom. Zoom in and retry.'
          );
          return;
        }
      }
    } else if (detectedType === 'map') {
      if (currentViewOnly) {
        if (!ptZoomCheck(layerOptions.minZoomCurrentView, 'Current-view MapServer loading', actionNoteId)) return;
      } else {
        if (!ptZoomCheck(layerOptions.minZoomLive, 'MapServer visual overlay', actionNoteId)) return;
      }
    } else if (detectedType === 'image') {
      if (!ptZoomCheck(layerOptions.minZoomLive, 'ImageServer visual overlay', actionNoteId)) return;
    }

    var color = PT2_CUSTOM_COLORS[ptExternalPanelLayerCount() % PT2_CUSTOM_COLORS.length];

    if (selectedType === 'auto') {
      ptSetStatus('Auto-detected: ' + ptServiceTypeLabel(detectedType) + '.', false);
    }

    if (detectedType === 'geojson') {
      ptAddGeoJsonLayer(name, url, clickable, color, layerOptions);
    } else if (detectedType === 'feature') {
      if (layerOptions.loadMode === 'live_snapshot') {
        ptAddArcgisFeatureLayerSnapshot(name, url, clickable, color, whereClause, layerOptions);
      } else if (currentViewOnly) {
        ptAddArcgisFeatureLayerCurrentView(name, url, clickable, color, whereClause, layerOptions);
      } else {
        ptAddArcgisFeatureLayer(name, url, clickable, color, whereClause, layerOptions);
      }
    } else if (detectedType === 'map') {
      if (currentViewOnly) {
        ptAddArcgisMapLayerCurrentView(name, url, clickable, color, whereClause, layerOptions);
      } else if (layerOptions.loadMode === 'tiled') {
        ptAddArcgisTiledMapLayer(name, url, clickable, color, layerOptions);
      } else {
        ptAddArcgisMapLayer(name, url, clickable, color, layerOptions);
      }
    } else if (detectedType === 'image') {
      ptAddArcgisImageLayer(name, url, clickable, color, layerOptions);
    }
  }

  // --------------------------------------------------------------------------
  // Starter catalog dropdowns
  // --------------------------------------------------------------------------

  function ptCatalogField(rec, fieldName) {
    if (!rec) return '';
    return ptCleanText(rec[fieldName]);
  }

  function ptCatalogPrimaryPanel(rec) {
    var panel = ptCatalogField(rec, 'primary_panel')
      .toLowerCase()
      .replace(/[_\s-]+/g, '_');

    // Explicit values keep the catalog easier to maintain:
    //   external = show only in External Layers
    //   ops_live = keep as shared source of truth, hide from External Layers
    //   both     = show in External Layers and also allow Ops Live promotion
    //   disabled = keep row in CSV, but hide/use nowhere in this build
    // Blank/unknown values are treated as external for backwards compatibility,
    // but the CSV should normally use one of the explicit values above.
    if (panel === 'ops') return 'ops_live';
    if (panel === 'ops_live' || panel === 'external' || panel === 'both' || panel === 'disabled') return panel;
    return 'external';
  }

  function ptCatalogVisibleInExternalPanel(rec) {
    var panel = ptCatalogPrimaryPanel(rec);

    // Catalog rows can be retained as the source of truth for Ops Live while
    // being hidden from the External Layers browser. Disabled rows remain in
    // the CSV for documentation/reactivation but are hidden from all panels.
    if (panel === 'ops_live' || panel === 'disabled') return false;

    // external and both remain visible in the External Layers browser.
    return true;
  }

  function ptCatalogVisibleRecordsForExternalPanel() {
    var rows = PT2_CATALOG.filter(ptCatalogVisibleInExternalPanel);

    // Keep the user-facing # badges tied to the final External panel order.
    // Stable catalog IDs still remain in the info panel/title for dev QA, but
    // the colored display numbers should read sequentially from top to bottom
    // after disabled/Ops-only rows are filtered out.
    rows.forEach(function(rec, i) {
      if (rec) rec.external_display_num = String(i + 1);
    });

    return rows;
  }

  function ptCatalogUnique(values) {
    var seen = {};
    var out = [];

    values.forEach(function(v) {
      v = ptCleanText(v);
      if (!v || seen[v]) return;
      seen[v] = true;
      out.push(v);
    });

    out.sort(function(a, b) {
      return a.localeCompare(b);
    });

    return out;
  }

  function ptCatalogFiltered(agency, theme) {
    return ptCatalogVisibleRecordsForExternalPanel().filter(function(rec) {
      var okAgency = !agency || ptCatalogField(rec, 'agency') === agency;
      var okTheme = !theme || ptCatalogField(rec, 'theme') === theme;
      return okAgency && okTheme;
    });
  }

  function ptFillSelect(selectEl, values, placeholder) {
    if (!selectEl) return;

    var html = '';

    if (placeholder) {
      html += '<option value="">' + ptEscapeHtml(placeholder) + '</option>';
    }

    values.forEach(function(v) {
      html += '<option value="' + ptEscapeHtml(v) + '">' + ptEscapeHtml(v) + '</option>';
    });

    selectEl.innerHTML = html;
  }

  function ptCatalogRecordKey(rec) {
    return [
      ptCatalogField(rec, 'external_layer_id'),
      ptCatalogField(rec, 'agency'),
      ptCatalogField(rec, 'theme'),
      ptCatalogField(rec, 'external_group'),
      ptCatalogField(rec, 'external_subgroup'),
      ptCatalogField(rec, 'display_name'),
      ptCatalogField(rec, 'service_url')
    ].join('||');
  }

  function ptGetSelectedCatalogRecord() {
    var layerSelect = document.getElementById('pt-catalog-layer');
    if (!layerSelect || !layerSelect.value) return null;

    var key = layerSelect.value;

    for (var i = 0; i < PT2_CATALOG.length; i++) {
      if (ptCatalogRecordKey(PT2_CATALOG[i]) === key) {
        return PT2_CATALOG[i];
      }
    }

    return null;
  }

  function ptUpdateCatalogInfo() {
    var rec = ptGetSelectedCatalogRecord();
    var div = document.getElementById('pt-catalog-info');
    if (!div) return;

    if (!rec) {
      div.innerHTML = '<span class="pt-tools-muted">Choose a catalog layer to preview its URL and notes.</span>';
      return;
    }

    var serviceUrl = ptCatalogField(rec, 'service_url');
    var extId = ptCatalogField(rec, 'external_layer_id');
    var extNum = ptCatalogField(rec, 'external_display_num');
    var sourcePage = ptCatalogField(rec, 'source_page');
    var notes = ptCatalogField(rec, 'notes');
    var geographicScope = ptCatalogField(rec, 'geographic_scope');
    var pt2UsageNote = ptCatalogField(rec, 'pt2_usage_note');
    var program = ptCatalogField(rec, 'program');
    var externalGroup = ptCatalogField(rec, 'external_group');
    var externalSubgroup = ptCatalogField(rec, 'external_subgroup');
    var serviceType = ptCatalogField(rec, 'service_type');
    var loadMode = ptCatalogField(rec, 'default_load_mode');
    var whereClause = ptCatalogField(rec, 'where_clause');
    var largeLayerWarning = ptCatalogField(rec, 'large_layer_warning');
    var minZoomLive = ptCatalogField(rec, 'min_zoom_live');
    var minZoomCurrentView = ptCatalogField(rec, 'min_zoom_current_view');
    var legendUrl = ptCatalogField(rec, 'legend_url') || ptDeriveLegendUrl(serviceUrl, serviceType);
    var legendNote = ptCatalogField(rec, 'legend_note');
    var popupFields = ptCatalogField(rec, 'popup_fields');
    var popupLinkTemplate = ptCatalogField(rec, 'popup_link_template');
    var popupLinkLabel = ptCatalogField(rec, 'popup_link_label');
    var hoverFields = ptCatalogField(rec, 'hover_fields');
    var defaultStyleField = ptCatalogField(rec, 'default_style_field');
    var bestUse = ptCatalogField(rec, 'best_use');

    var html =
      (extNum ? '<div><b>External display #:</b> #' + ptEscapeHtml(extNum) + '</div>' : '') +
      (extId ? '<div><b>Stable catalog ID:</b> ' + ptEscapeHtml(extId) + '</div>' : '') +
      '<div><b>Type:</b> ' + ptEscapeHtml(ptServiceTypeLabel(serviceType)) + '</div>' +
      (program ? '<div><b>Program:</b> ' + ptEscapeHtml(program) + '</div>' : '') +
      (externalGroup ? '<div><b>External group:</b> ' + ptEscapeHtml(externalGroup) + '</div>' : '') +
      (externalSubgroup ? '<div><b>Subgroup:</b> ' + ptEscapeHtml(externalSubgroup) + '</div>' : '') +
      (geographicScope ? '<div><b>Region:</b> ' + ptEscapeHtml(geographicScope) + '</div>' : '') +
      (pt2UsageNote ? '<div><b>BRIM usage note:</b> ' + ptEscapeHtml(pt2UsageNote) + '</div>' : '') +
      (loadMode ? '<div><b>Load type:</b> ' + ptEscapeHtml(ptLoadModeHumanLabel(loadMode)) + '</div>' : '') +
      (whereClause ? '<div><b>Filter:</b> <code>' + ptEscapeHtml(whereClause) + '</code></div>' : '') +
      (minZoomLive ? '<div><b>Min zoom:</b> ' + ptEscapeHtml(minZoomLive) + '</div>' : '') +
      (minZoomCurrentView ? '<div><b>Current-view min zoom:</b> ' + ptEscapeHtml(minZoomCurrentView) + '</div>' : '') +
      '<div><b>URL:</b> <a href="' + ptEscapeHtml(serviceUrl) + '" target="_blank">' + ptEscapeHtml(ptShortUrl(serviceUrl)) + '</a></div>' +
      (legendUrl ? '<div><b>Provider legend:</b> <a href="' + ptEscapeHtml(legendUrl) + '" target="_blank">opens external page</a></div>' : '') +
      (legendNote ? '<div><b>Map display notes:</b> ' + ptEscapeHtml(legendNote) + '</div>' : '') +
      (bestUse ? '<div><b>Best use:</b> ' + ptEscapeHtml(bestUse) + '</div>' : '') +
      (hoverFields ? '<div><b>Hover:</b> ' + ptEscapeHtml(hoverFields) + '</div>' : '') +
      (popupFields ? '<div><b>Popup:</b> curated fields</div>' : '') +
      (popupLinkTemplate ? '<div><b>Popup link:</b> ' + ptEscapeHtml(popupLinkLabel || 'Open related page') + '</div>' : '') +
      (defaultStyleField ? '<div><b>Future style field:</b> ' + ptEscapeHtml(defaultStyleField) + '</div>' : '');

    if (largeLayerWarning) {
      html += '<div style="color:#8B5A00;"><b>Large layer:</b> ' + ptEscapeHtml(largeLayerWarning) + '</div>';
    }

    if (notes) {
      html += '<div><b>Note:</b> ' + ptEscapeHtml(notes) + '</div>';
    }

    if (sourcePage) {
      html += '<div><a href="' + ptEscapeHtml(sourcePage) + '" target="_blank">Open source page</a></div>';
    }

    div.innerHTML = html;
  }

  function ptPopulateCatalogControls() {
    var agencySelect = document.getElementById('pt-catalog-agency');
    var themeSelect = document.getElementById('pt-catalog-theme');
    var layerSelect = document.getElementById('pt-catalog-layer');

    if (!agencySelect || !themeSelect || !layerSelect) return;

    if (!PT2_CATALOG.length) {
      agencySelect.innerHTML = '<option value="">No catalog loaded</option>';
      themeSelect.innerHTML = '<option value="">No catalog loaded</option>';
      layerSelect.innerHTML = '<option value="">No catalog loaded</option>';
      ptUpdateCatalogInfo();
      return;
    }

    var selectedAgency = agencySelect.value || '';
    var selectedTheme = themeSelect.value || '';

    var agencies = ptCatalogUnique(ptCatalogVisibleRecordsForExternalPanel().map(function(rec) {
      return ptCatalogField(rec, 'agency');
    }));

    ptFillSelect(agencySelect, agencies, 'All agencies / sources');

    if (selectedAgency && agencies.indexOf(selectedAgency) >= 0) {
      agencySelect.value = selectedAgency;
    } else {
      selectedAgency = '';
      agencySelect.value = '';
    }

    var themeRecords = ptCatalogFiltered(selectedAgency, '');
    var themes = ptCatalogUnique(themeRecords.map(function(rec) {
      return ptCatalogField(rec, 'theme');
    }));

    ptFillSelect(themeSelect, themes, 'All themes');

    if (selectedTheme && themes.indexOf(selectedTheme) >= 0) {
      themeSelect.value = selectedTheme;
    } else {
      selectedTheme = '';
      themeSelect.value = '';
    }

    var layerRecords = ptCatalogFiltered(selectedAgency, selectedTheme);

    var html = '<option value="">Choose a layer...</option>';

    layerRecords.forEach(function(rec) {
      var key = ptCatalogRecordKey(rec);
      html +=
        '<option value="' + ptEscapeHtml(key) + '">' +
        ptEscapeHtml(ptCatalogField(rec, 'display_name')) +
        '</option>';
    });

    layerSelect.innerHTML = html;
    ptUpdateCatalogInfo();
  }

  function ptApplyCatalogRecord(addNow, actionNoteId) {
    var rec = ptGetSelectedCatalogRecord();

    if (!rec) {
      ptSetStatus('Choose a catalog layer first.', true);
      return;
    }

    var nameInput = document.getElementById('pt-custom-name');
    var urlInput = document.getElementById('pt-custom-url');
    var typeInput = document.getElementById('pt-custom-type');
    var clickableInput = document.getElementById('pt-custom-clickable');
    var currentViewInput = document.getElementById('pt-custom-current-view');
    var whereInput = document.getElementById('pt-custom-where');
    var legendUrlInput = document.getElementById('pt-custom-legend-url');
    var legendNoteInput = document.getElementById('pt-custom-legend-note');
    var minZoomLiveInput = document.getElementById('pt-custom-min-zoom-live');
    var minZoomCurrentViewInput = document.getElementById('pt-custom-min-zoom-current-view');
    var popupFieldsInput = document.getElementById('pt-custom-popup-fields');
    var popupAliasesInput = document.getElementById('pt-custom-popup-aliases');
    var popupLinkTemplateInput = document.getElementById('pt-custom-popup-link-template');
    var popupLinkLabelInput = document.getElementById('pt-custom-popup-link-label');
    var hoverFieldsInput = document.getElementById('pt-custom-hover-fields');
    var hoverAliasesInput = document.getElementById('pt-custom-hover-aliases');
    var hoverBoldFieldsInput = document.getElementById('pt-custom-hover-bold-fields');
    var hoverNoLabelFieldsInput = document.getElementById('pt-custom-hover-no-label-fields');
    var hoverRoundFieldsInput = document.getElementById('pt-custom-hover-round-fields');
    var hoverShowNativeFieldNamesInput = document.getElementById('pt-custom-hover-show-native-field-names');
    var defaultLabelFieldInput = document.getElementById('pt-custom-default-label-field');
    var outFieldsInput = document.getElementById('pt-custom-out-fields');
    var styleFieldCandidatesInput = document.getElementById('pt-custom-style-field-candidates');
    var defaultStyleFieldInput = document.getElementById('pt-custom-default-style-field');
    var fieldCurationNotesInput = document.getElementById('pt-custom-field-curation-notes');
    var showNativeFieldNamesInput = document.getElementById('pt-custom-show-native-field-names');

    if (nameInput) nameInput.value = ptCatalogField(rec, 'display_name');
    if (urlInput) urlInput.value = ptCatalogField(rec, 'service_url');

    var serviceType = ptCatalogField(rec, 'service_type').toLowerCase();
    if (typeInput) {
      if (serviceType === 'feature' || serviceType === 'map' || serviceType === 'image' || serviceType === 'geojson') {
        typeInput.value = serviceType;
      } else {
        typeInput.value = 'auto';
      }
    }

    if (clickableInput) {
      clickableInput.checked = ptTruth(rec.default_clickable) && ptTruth(rec.supports_popups);
    }

    if (currentViewInput) {
      currentViewInput.checked = ptCatalogField(rec, 'default_load_mode').toLowerCase() === 'current_view';
    }

    if (whereInput) {
      whereInput.value = ptCatalogField(rec, 'where_clause');
    }
    if (legendUrlInput) legendUrlInput.value = ptCatalogField(rec, 'legend_url');
    if (legendNoteInput) legendNoteInput.value = ptCatalogField(rec, 'legend_note');
    if (minZoomLiveInput) minZoomLiveInput.value = ptCatalogField(rec, 'min_zoom_live');
    if (minZoomCurrentViewInput) minZoomCurrentViewInput.value = ptCatalogField(rec, 'min_zoom_current_view');
    if (popupFieldsInput) popupFieldsInput.value = ptCatalogField(rec, 'popup_fields');
    if (popupAliasesInput) popupAliasesInput.value = ptCatalogField(rec, 'popup_aliases');
    if (popupLinkTemplateInput) popupLinkTemplateInput.value = ptCatalogField(rec, 'popup_link_template');
    if (popupLinkLabelInput) popupLinkLabelInput.value = ptCatalogField(rec, 'popup_link_label');
    if (hoverFieldsInput) hoverFieldsInput.value = ptCatalogField(rec, 'hover_fields');
    if (hoverAliasesInput) hoverAliasesInput.value = ptCatalogField(rec, 'hover_aliases');
    if (hoverBoldFieldsInput) hoverBoldFieldsInput.value = ptCatalogField(rec, 'hover_bold_fields');
    if (hoverNoLabelFieldsInput) hoverNoLabelFieldsInput.value = ptCatalogField(rec, 'hover_no_label_fields');
    if (hoverRoundFieldsInput) hoverRoundFieldsInput.value = ptCatalogField(rec, 'hover_round_fields');
    if (hoverShowNativeFieldNamesInput) hoverShowNativeFieldNamesInput.value = ptCatalogField(rec, 'hover_show_native_field_names');
    if (defaultLabelFieldInput) defaultLabelFieldInput.value = ptCatalogField(rec, 'default_label_field');
    if (outFieldsInput) outFieldsInput.value = ptCatalogField(rec, 'out_fields');
    if (styleFieldCandidatesInput) styleFieldCandidatesInput.value = ptCatalogField(rec, 'style_field_candidates');
    if (defaultStyleFieldInput) defaultStyleFieldInput.value = ptCatalogField(rec, 'default_style_field');
    if (fieldCurationNotesInput) fieldCurationNotesInput.value = ptCatalogField(rec, 'field_curation_notes');
    if (showNativeFieldNamesInput) showNativeFieldNamesInput.value = ptCatalogField(rec, 'show_native_field_names');

    ptUpdatePopupSupportUI();
    ptUpdateLoadModeUI();

    if (addNow) {
      ptSetStatus('Adding catalog overlay...', false);
      ptHandleAddCustomLayer(actionNoteId || 'pt-catalog-action-note');
    } else {
      ptSetStatus(
        'Catalog layer loaded into the advanced manual fields. Review and click ' +
          ptUpdateManualAddActionLabel() + '.',
        false
      );
    }
  }


  // --------------------------------------------------------------------------
  // Quick-add catalog browser
  // --------------------------------------------------------------------------

  function ptCatalogSearchText(rec) {
    if (!rec) return '';

    return [
      ptCatalogField(rec, 'external_layer_id'),
      ptCatalogField(rec, 'external_display_num'),
      ptCatalogField(rec, 'agency'),
      ptCatalogField(rec, 'program'),
      ptCatalogField(rec, 'theme'),
      ptCatalogField(rec, 'display_name'),
      ptCatalogField(rec, 'service_type'),
      ptCatalogField(rec, 'geographic_scope'),
      ptCatalogField(rec, 'pt2_usage_note'),
      ptCatalogField(rec, 'popup_link_label'),
      ptCatalogField(rec, 'best_use'),
      ptCatalogField(rec, 'notes'),
      ptCatalogField(rec, 'useful_for_visualization')
    ].join(' ').toLowerCase();
  }

  function ptLoadModeHumanLabel(loadMode) {
    var mode = ptCleanText(loadMode).toLowerCase();

    if (mode === 'current_view') return 'interactive snapshot';
    if (mode === 'visual') return 'visual overlay';
    if (mode === 'live') return 'live feed';
    if (mode === 'tiled') return 'tiled overlay';

    return ptCleanText(loadMode);
  }

  function ptCatalogQuickTypeLine(rec) {
    var type = ptServiceTypeLabel(ptCatalogField(rec, 'service_type'));
    var agency = ptCatalogField(rec, 'agency');

    var bits = [];
    if (agency) bits.push(agency);
    if (type) bits.push(type);

    return bits.join(' • ');
  }

  function ptCatalogDetailHtml(rec) {
    if (!rec) return '';

    var serviceUrl = ptCatalogField(rec, 'service_url');
    var extId = ptCatalogField(rec, 'external_layer_id');
    var extNum = ptCatalogField(rec, 'external_display_num');
    var sourcePage = ptCatalogField(rec, 'source_page');
    var notes = ptCatalogField(rec, 'notes');
    var geographicScope = ptCatalogField(rec, 'geographic_scope');
    var pt2UsageNote = ptCatalogField(rec, 'pt2_usage_note');
    var program = ptCatalogField(rec, 'program');
    var externalGroup = ptCatalogField(rec, 'external_group');
    var externalSubgroup = ptCatalogField(rec, 'external_subgroup');
    var serviceType = ptCatalogField(rec, 'service_type');
    var loadMode = ptCatalogField(rec, 'default_load_mode');
    var whereClause = ptCatalogField(rec, 'where_clause');
    var largeLayerWarning = ptCatalogField(rec, 'large_layer_warning');
    var minZoomLive = ptCatalogField(rec, 'min_zoom_live');
    var minZoomCurrentView = ptCatalogField(rec, 'min_zoom_current_view');
    var legendUrl = ptCatalogField(rec, 'legend_url') || ptDeriveLegendUrl(serviceUrl, serviceType);
    var legendNote = ptCatalogField(rec, 'legend_note');
    var popupFields = ptCatalogField(rec, 'popup_fields');
    var popupLinkTemplate = ptCatalogField(rec, 'popup_link_template');
    var popupLinkLabel = ptCatalogField(rec, 'popup_link_label');
    var hoverFields = ptCatalogField(rec, 'hover_fields');
    var defaultStyleField = ptCatalogField(rec, 'default_style_field');
    var bestUse = ptCatalogField(rec, 'best_use');
    var usefulFor = ptCatalogField(rec, 'useful_for_visualization');
    var fieldNotes = ptCatalogField(rec, 'field_curation_notes');
    var loadBadge = ptCatalogField(rec, 'load_badge');
    var loadNote = ptExternalFriendlyLoadNote(rec);
    var loadAuditBasis = ptExternalFriendlyLoadBasis(rec);

    var html = '';

    if (extNum) html += '<div><b>External display #:</b> #' + ptEscapeHtml(extNum) + '</div>';
    if (extId) html += '<div><b>Stable catalog ID:</b> ' + ptEscapeHtml(extId) + '</div>';
    html += '<div><b>Type:</b> ' + ptEscapeHtml(ptServiceTypeLabel(serviceType)) + '</div>';
    if (externalGroup) html += '<div><b>External group:</b> ' + ptEscapeHtml(externalGroup) + '</div>';
    if (externalSubgroup) html += '<div><b>Subgroup:</b> ' + ptEscapeHtml(externalSubgroup) + '</div>';
    if (program) html += '<div><b>Program:</b> ' + ptEscapeHtml(program) + '</div>';
    if (geographicScope) html += '<div><b>Region:</b> ' + ptEscapeHtml(geographicScope) + '</div>';
    if (pt2UsageNote) html += '<div><b>BRIM usage note:</b> ' + ptEscapeHtml(pt2UsageNote) + '</div>';
    if (loadBadge || loadNote || ptCatalogField(rec, 'load_badge_override')) {
      var loadOverrideNote = ptExternalLoadBadgeOverrideNote(rec);
      html += '<div><b>Load behavior:</b> ' + ptExternalLoadBadgeInlineHtml(rec) +
        (loadOverrideNote ? ' <span style="color:#777;">' + ptEscapeHtml(loadOverrideNote) + '</span>' : '') +
        (loadNote ? ' <span style="color:#666;">' + ptEscapeHtml(loadNote) + '</span>' : '') +
        (loadAuditBasis ? ' <span style="color:#777;">[' + ptEscapeHtml(loadAuditBasis) + ']</span>' : '') +
        '</div>';
    }
    if (loadMode) html += '<div><b>Load type:</b> ' + ptEscapeHtml(ptLoadModeHumanLabel(loadMode)) + '</div>';
    if (whereClause) html += '<div><b>Filter:</b> <code>' + ptEscapeHtml(whereClause) + '</code></div>';
    if (minZoomLive) html += '<div><b>Min zoom:</b> ' + ptEscapeHtml(minZoomLive) + '</div>';
    if (minZoomCurrentView) html += '<div><b>Current-view min zoom:</b> ' + ptEscapeHtml(minZoomCurrentView) + '</div>';
    if (bestUse) html += '<div><b>Best use:</b> ' + ptEscapeHtml(bestUse) + '</div>';
    if (usefulFor) html += '<div><b>Useful for:</b> ' + ptEscapeHtml(usefulFor) + '</div>';
    var visualMapIdentify =
      serviceType === 'map' &&
      (loadMode === 'visual' || loadMode === 'tiled') &&
      (hoverFields || popupFields || popupLinkTemplate);

    if (hoverFields) html += '<div><b>Hover:</b> ' + ptEscapeHtml(hoverFields) + (visualMapIdentify ? ' <span style="color:#666;">(via map identify)</span>' : '') + '</div>';
    if (popupFields) html += '<div><b>Popup:</b> curated fields' + (visualMapIdentify ? ' <span style="color:#666;">(via map identify)</span>' : '') + '</div>';
    if (popupLinkTemplate) html += '<div><b>Popup link:</b> ' + ptEscapeHtml(popupLinkLabel || 'Open related page') + '</div>';

    if (defaultStyleField) html += '<div><b>Style field:</b> ' + ptEscapeHtml(defaultStyleField) + '</div>';
    if (fieldNotes) html += '<div><b>Field note:</b> ' + ptEscapeHtml(fieldNotes) + '</div>';

    if (largeLayerWarning) {
      html += '<div class="pt-catalog-warning"><b>Large layer:</b> ' + ptEscapeHtml(largeLayerWarning) + '</div>';
    }

    if (legendNote) {
      html += '<div><b>Map display notes:</b> ' + ptEscapeHtml(legendNote) + '</div>';
    }

    var styleInfoOptions = {
      defaultStyleField: defaultStyleField,
      defaultStyleMethod: ptCatalogField(rec, 'default_style_method'),
      styleUnits: ptCatalogField(rec, 'style_units'),
      styleLegendTitle: ptCatalogField(rec, 'style_legend_title'),
      layerName: ptCatalogField(rec, 'display_name'),
      legendAdapter: ptCatalogField(rec, 'legend_adapter'),
      catalogExtId: ptCatalogField(rec, 'external_layer_id')
    };
    html += ptRegisteredLegendHtml(styleInfoOptions, 'catalog_detail');

    if (notes) {
      html += '<div><b>Note:</b> ' + ptEscapeHtml(notes) + '</div>';
    }

    var sourcePageNorm = ptNormalizeUrl(sourcePage);
    var serviceUrlNorm = ptNormalizeUrl(serviceUrl);
    var showSourcePageLink = !!sourcePage && sourcePageNorm !== serviceUrlNorm;

    // Many catalog rows use the service endpoint itself as the source page.
    // Avoid showing two visually identical links in the expanded info panel:
    // keep a single raw Service URL row unless the source page is meaningfully
    // different from the actual service endpoint.
    if (legendUrl) {
      html += '<div><a href="' + ptEscapeHtml(legendUrl) + '" target="_blank" rel="noopener noreferrer">Provider legend — opens external page</a></div>';
    }

    if (showSourcePageLink) {
      html += '<div><a href="' + ptEscapeHtml(sourcePage) + '" target="_blank" rel="noopener noreferrer">Source page — opens external page</a></div>';
    }

    if (serviceUrl) {
      html += '<div><b>Service URL</b> — <a href="' + ptEscapeHtml(serviceUrl) + '" target="_blank" rel="noopener noreferrer" title="' + ptEscapeHtml(serviceUrl) + '">' + ptEscapeHtml(ptShortUrl(serviceUrl)) + '</a></div>';
    }

    return html;
  }

  function ptSetHiddenCatalogSelection(rec) {
    var layerSelect = document.getElementById('pt-catalog-layer');
    if (!layerSelect || !rec) return false;

    var key = ptCatalogRecordKey(rec);
    layerSelect.value = key;

    // The hidden legacy select is normally populated by ptPopulateCatalogControls().
    // Add a temporary option as a safety valve if the selected record is not
    // currently present because a filter value was changed programmatically.
    if (layerSelect.value !== key) {
      var opt = document.createElement('option');
      opt.value = key;
      opt.textContent = ptCatalogField(rec, 'display_name');
      layerSelect.appendChild(opt);
      layerSelect.value = key;
    }

    return layerSelect.value === key;
  }

  function ptAddCatalogQuickIndex(idx) {
    idx = Number(idx);
    var rec = PT2_CATALOG[idx];

    if (!rec) {
      ptSetInlineNote('pt-catalog-action-note', 'Catalog record not found.', true);
      return;
    }

    if (ptCatalogIndexIsActive(idx)) {
      var activeMsg = (ptCatalogField(rec, 'display_name') || 'This catalog layer') + ' is already active.';
      if (ptCatalogLoadingIdx !== null && Number(ptCatalogLoadingIdx) === idx) {
        ptClearCatalogLoading(true);
      }
      ptRenderQuickCatalog();
      ptSetCatalogRowNote(idx, activeMsg, false);
      ptSetStatus(activeMsg, false);
      return;
    }

    if (!ptSetHiddenCatalogSelection(rec)) {
      ptSetInlineNote('pt-catalog-action-note', 'Could not select that catalog record.', true);
      ptClearCatalogLoading();
      return;
    }

    ptPendingCatalogIdx = idx;

    ptSetCatalogLoading(
      idx,
      'Adding ' + (ptCatalogField(rec, 'display_name') || 'catalog overlay') +
        '... public GIS layers may take several seconds to a minute.'
    );

    ptApplyCatalogRecord(true, 'pt-catalog-row-note-' + idx);
  }

  function ptLoadCatalogQuickIndexIntoForm(idx) {
    idx = Number(idx);
    var rec = PT2_CATALOG[idx];

    if (!rec) {
      ptSetInlineNote('pt-catalog-action-note', 'Catalog record not found.', true);
      return;
    }

    if (!ptSetHiddenCatalogSelection(rec)) {
      ptSetInlineNote('pt-catalog-action-note', 'Could not select that catalog record.', true);
      return;
    }

    ptApplyCatalogRecord(false);

    ptRevealAdvancedManualForm();
  }

  function ptRevealAdvancedManualForm() {
    var advanced = document.getElementById('pt-advanced-manual-details');
    if (!advanced) return;

    advanced.open = true;

    var panelBody = advanced.closest ? advanced.closest('.pt-tools-body') : null;
    if (panelBody && panelBody.getBoundingClientRect && advanced.getBoundingClientRect) {
      var panelRect = panelBody.getBoundingClientRect();
      var advancedRect = advanced.getBoundingClientRect();
      panelBody.scrollTop = Math.max(
        0,
        panelBody.scrollTop + advancedRect.top - panelRect.top - 8
      );
    }

    var focusName = function() {
      var nameInput = document.getElementById('pt-custom-name');
      if (!nameInput || typeof nameInput.focus !== 'function') return;
      try {
        nameInput.focus({preventScroll: true});
      } catch (focusError) {
        nameInput.focus();
      }
    };

    if (typeof window.requestAnimationFrame === 'function') {
      window.requestAnimationFrame(focusName);
    } else {
      window.setTimeout(focusName, 0);
    }
  }


  function ptCatalogIndexIsActive(idx) {
    return !!ptFindCustomLayerByCatalogIndex(idx);
  }

  function ptCatalogGroupKey(groupName) {
    return String(groupName || 'Other');
  }

  function ptCatalogSubgroupKey(groupName, subgroupName) {
    return ptCatalogGroupKey(groupName) + ' :: ' + String(subgroupName || 'Other');
  }

  function ptCatalogExternalGroupName(rec) {
    return ptCatalogField(rec, 'external_group') ||
      ptCatalogField(rec, 'theme') ||
      ptCatalogField(rec, 'agency') ||
      'Other';
  }

  function ptCatalogExternalSubgroupName(rec) {
    return ptCatalogField(rec, 'external_subgroup') ||
      ptCatalogField(rec, 'theme') ||
      'Other';
  }

  function ptCatalogNumericField(rec, fieldName, fallback) {
    var value = parseFloat(ptCatalogField(rec, fieldName));
    if (!isFinite(value)) return fallback;
    return value;
  }

  function ptCapabilityDefinition(code) {
    var definition = PT2_CAPABILITY_DEFINITIONS && PT2_CAPABILITY_DEFINITIONS[code];
    if (!definition) return null;

    var label = ptCleanText(definition.label);
    var capability = ptCleanText(definition.capability);
    if (!label || !capability) return null;

    return {
      code: code,
      label: label,
      capability: capability
    };
  }

  function ptCatalogCapabilityEnabled(rec, fieldName) {
    if (!rec || !Object.prototype.hasOwnProperty.call(rec, fieldName)) return false;
    if (rec[fieldName] === true) return true;
    return ['true', 't', '1', 'yes', 'y'].indexOf(
      ptCleanText(rec[fieldName]).toLowerCase()
    ) >= 0;
  }

  function ptCapabilityBadgeHtml(kind, count, denominator) {
    var isLegend = kind === 'legend';
    var definition = ptCapabilityDefinition(isLegend ? 'LGND' : 'INFO');
    if (!definition) return '';

    var hasCount = isFinite(count) && isFinite(denominator);
    var accessibleLabel = definition.label;
    var countHtml = '';
    var countAttribute = '';
    if (hasCount) {
      count = Math.max(0, Math.round(Number(count)));
      denominator = Math.max(count, Math.round(Number(denominator)));
      if (count === 0 || denominator === 0) return '';
      accessibleLabel = count + ' of ' + denominator + ' layers — ' + definition.label;
      countHtml = ' <span class="pt-capability-badge-count" aria-hidden="true">' +
        ptEscapeHtml(String(count)) + '</span>';
      countAttribute = ' data-capability-count="' + ptEscapeHtml(String(count)) + '"' +
        ' data-capability-denominator="' + ptEscapeHtml(String(denominator)) + '"';
    }

    return '<span class="pt-capability-badge pt-capability-badge--' +
      (isLegend ? 'legend' : 'info') + '" data-capability="' +
      (isLegend ? 'legend' : 'info') + '"' + countAttribute +
      ' title="' + ptEscapeHtml(accessibleLabel) + '" aria-label="' +
      ptEscapeHtml(accessibleLabel) + '">' + ptEscapeHtml(definition.code) + countHtml + '</span>';
  }

  function ptCatalogLayerCapabilityBadgesHtml(rec) {
    var html = '';
    if (ptCatalogCapabilityEnabled(rec, 'has_legend')) {
      html += ptCapabilityBadgeHtml('legend');
    }
    if (ptCatalogCapabilityEnabled(rec, 'has_feature_info')) {
      html += ptCapabilityBadgeHtml('info');
    }
    if (!html) return '';
    return '<span class="pt-capability-badges pt-capability-badges--layer">' + html + '</span>';
  }

  function ptCatalogHierarchyCapabilityBadgesHtml(rec, hierarchy, denominator) {
    if (!rec || !isFinite(denominator) || denominator <= 0) return '';
    var legendCount = ptCatalogNumericField(rec, hierarchy + '_legend_count', 0);
    var infoCount = ptCatalogNumericField(rec, hierarchy + '_info_count', 0);
    var html = ptCapabilityBadgeHtml('legend', legendCount, denominator) +
      ptCapabilityBadgeHtml('info', infoCount, denominator);
    if (!html) return '';
    return '<span class="pt-capability-badges pt-capability-badges--parent">' + html + '</span>';
  }

  function ptCatalogHierarchyCountHtml(matchedCount, totalCount) {
    matchedCount = Math.max(0, Math.round(Number(matchedCount) || 0));
    totalCount = Math.max(matchedCount, Math.round(Number(totalCount) || matchedCount));
    if (matchedCount < totalCount) return '(' + matchedCount + ' of ' + totalCount + ')';
    return '(' + totalCount + ')';
  }

  function ptCapabilityDefinitionKeyHtml() {
    var legend = ptCapabilityDefinition('LGND');
    var info = ptCapabilityDefinition('INFO');
    if (!legend || !info) return '';

    return '<div id="pt-catalog-capability-key" class="pt-tools-muted pt-catalog-capability-key">' +
        '<span class="pt-capability-key-item"><span aria-hidden="true">' +
          ptCapabilityBadgeHtml('legend') + '</span>' +
          '<span>' + ptEscapeHtml(legend.label) + '</span></span>' +
        '<span class="pt-capability-key-separator" aria-hidden="true">·</span>' +
        '<span class="pt-capability-key-item"><span aria-hidden="true">' +
          ptCapabilityBadgeHtml('info') + '</span>' +
          '<span>' + ptEscapeHtml(info.label) + '</span></span>' +
        '<span class="pt-capability-key-separator" aria-hidden="true">·</span>' +
        '<span class="pt-capability-key-note">Provider legends may be linked in layer info.</span>' +
      '</div>';
  }

  function ptCatalogExternalGroupOrder(rec) {
    return ptCatalogNumericField(rec, 'external_group_order', 999);
  }

  function ptCatalogExternalSubgroupOrder(rec) {
    return ptCatalogNumericField(rec, 'external_subgroup_order', 999);
  }

  function ptCatalogPriorityOrder(rec) {
    return ptCatalogNumericField(rec, 'priority', 9999);
  }

  function ptCatalogGroupHasActiveLayer(groupRows) {
    groupRows = groupRows || [];

    return groupRows.some(function(rec) {
      return rec && ptCatalogIndexIsActive(rec.__pt2Index);
    });
  }

  function ptCatalogSubgroupHasActiveLayer(subgroupRows) {
    subgroupRows = subgroupRows || [];

    return subgroupRows.some(function(rec) {
      return rec && ptCatalogIndexIsActive(rec.__pt2Index);
    });
  }

  function ptCatalogGroupIsExpanded(groupName, groupRows, q) {
    var groupKey = ptCatalogGroupKey(groupName);

    // Search results should be visible immediately.  This avoids the confusing
    // case where a search finds rows but leaves them hidden inside collapsed
    // section headers.
    if (q) return true;

    // Keep active layers visible even when most catalog groups are collapsed.
    if (ptCatalogGroupHasActiveLayer(groupRows)) return true;

    return ptCatalogExpandedGroups[groupKey] === true;
  }

  function ptCatalogSubgroupIsExpanded(groupName, subgroupName, subgroupRows, q) {
    var subgroupKey = ptCatalogSubgroupKey(groupName, subgroupName);

    // Search results and active layers should never be hidden inside a
    // collapsed theme; this mirrors the parent group behavior.
    if (q) return true;
    if (ptCatalogSubgroupHasActiveLayer(subgroupRows)) return true;

    // Default to collapsed. A theme opens only when the user clicks it,
    // when a search is active, or when it contains an active layer.
    return ptCatalogExpandedSubgroups[subgroupKey] === true;
  }

  function ptSetVisibleCatalogGroupsExpanded(expanded) {
    (ptCatalogVisibleGroupNames || []).forEach(function(groupName) {
      ptCatalogExpandedGroups[ptCatalogGroupKey(groupName)] = !!expanded;
    });

    // Expand All should open parent groups and their theme rows. Collapse All
    // closes parent groups and resets theme state to the default collapsed
    // behavior for the next open group.
    ptCatalogExpandedSubgroups = {};
    if (expanded) {
      (PT2_CATALOG || []).forEach(function(rec) {
        if (!rec || !ptCatalogVisibleInExternalPanel(rec)) return;
        var groupName = ptCatalogExternalGroupName(rec);
        var subgroupName = ptCatalogExternalSubgroupName(rec);
        ptCatalogExpandedSubgroups[ptCatalogSubgroupKey(groupName, subgroupName)] = true;
      });
    }

    ptRenderQuickCatalog();
  }

  function ptRenderQuickCatalog() {
    var list = document.getElementById('pt-catalog-quick-list');
    var count = document.getElementById('pt-catalog-quick-count');
    var search = document.getElementById('pt-catalog-search');

    if (!list) return;

    var q = search ? ptCleanText(search.value).toLowerCase() : '';
    var rows = ptCatalogVisibleRecordsForExternalPanel().filter(function(rec) {
      if (!rec) return false;
      if (!q) return true;
      return ptCatalogSearchText(rec).indexOf(q) >= 0;
    });

    if (count) {
      count.textContent = rows.length.toLocaleString() + ' catalog layer' + (rows.length === 1 ? '' : 's');
    }

    if (!rows.length) {
      ptCatalogVisibleGroupNames = [];
      list.innerHTML = '<div class="pt-tools-muted">No catalog rows match the current search.</div>';
      return;
    }


    function ptCatalogDisplayNumber(rec) {
      var displayNum = ptCatalogField(rec, 'external_display_num');
      if (displayNum) return '#' + displayNum;

      // Fallback for older catalogs or partially edited rows.
      var extId = ptCatalogField(rec, 'external_layer_id');
      if (!extId) return '';
      return extId.replace(/^EXT0*/, '#');
    }

    function ptCatalogNumberBadgeHtml(rec) {
      var num = ptCatalogDisplayNumber(rec);
      if (!num) return '';
      return ptExternalCatalogIdBadgeHtml(
        num.replace(/^#/, ''),
        ptCatalogField(rec, 'external_layer_id'),
        rec,
        'External Layers panel display number'
      );
    }

    function ptCatalogRowHtml(rec) {
      var idx = rec.__pt2Index;
      var layerName = ptCatalogField(rec, 'display_name') || ('Catalog layer ' + (idx + 1));
      var typeLine = ptCatalogQuickTypeLine(rec);
      var detailId = 'pt-catalog-detail-' + idx;
      var isActive = ptCatalogIndexIsActive(idx);
      var isLoading = ptCatalogLoadingIdx !== null && Number(ptCatalogLoadingIdx) === Number(idx);
      var activeRec = isActive ? ptFindCustomLayerByCatalogIndex(idx) : null;
      var catalogRowActionHtml = '';
      var canRetryCurrentView = ptCatalogRecordIsRefreshableCurrentView(rec);
      var capabilityBadgesHtml = ptCatalogLayerCapabilityBadgesHtml(rec);

      if (isLoading) {
        // Loading rows intentionally show only Cancel plus a retry/refresh
        // action.  Do not show Remove at the same time: Remove is reserved for
        // fully active rows after the request finishes.  If a FeatureServer
        // response has already created an active record but the minimum spinner
        // or a delayed status still has the row in loading state, Cancel will
        // remove that provisional record and ignore any late result.
        catalogRowActionHtml =
          '<div class="pt-catalog-row-action-wrap">' +
            '<button type="button" class="pt-tools-mini-btn pt-external-remove-btn pt-catalog-row-remove-btn" data-pt-catalog-loading-cancel="' + idx + '" title="Cancel this loading request and ignore any late result">Cancel</button>' +
            '<button type="button" class="pt-tools-mini-btn pt-catalog-row-refresh-btn" data-pt-catalog-loading-retry="' + idx + '" title="Cancel this attempt and retry using the current map view">' +
              (canRetryCurrentView ? '↻ Retry/refresh' : 'Retry') +
            '</button>' +
          '</div>';
      } else if (activeRec) {
        catalogRowActionHtml =
          '<div class="pt-catalog-row-action-wrap">' +
            '<button type="button" class="pt-tools-mini-btn pt-external-remove-btn pt-catalog-row-remove-btn" data-pt-custom-remove="' + activeRec.id + '" title="Remove this external overlay">Remove</button>' +
            (ptIsRefreshableExternalRecord(activeRec) ? ptRefreshButtonHtml(activeRec, 'pt-catalog-row-refresh-btn') : '') +
          '</div>';
      }

      return '<div class="pt-catalog-row' + (isActive ? ' pt-catalog-row-active' : '') + (isLoading ? ' pt-catalog-loading' : '') + (activeRec && activeRec.refreshing ? ' pt-catalog-row-refreshing' : '') + '" data-pt-catalog-row="' + idx + '"' + (isLoading ? ' aria-busy="true"' : '') + '>' +
        '<div class="pt-catalog-row-main">' +
          '<button type="button" class="pt-catalog-add-small" title="' + (isLoading ? 'Loading overlay...' : (isActive ? 'This overlay is already active' : 'Add this overlay')) + '" data-pt-catalog-quick-add="' + idx + '"' + (isLoading ? ' disabled aria-disabled="true"' : '') + '>' +
            '<span class="pt-catalog-add-symbol">' + (isActive ? '✓' : '+') + '</span>' +
            '<span class="pt-catalog-spinner" aria-hidden="true"></span>' +
          '</button>' +
          '<div class="pt-catalog-row-text">' +
            '<div class="pt-catalog-layer-name">' +
              '<span class="pt-catalog-layer-label">' + ptCatalogNumberBadgeHtml(rec) + ptEscapeHtml(layerName) + '</span>' +
              (isActive ? '<span class="pt-catalog-active-badge">Active</span>' : '') +
              capabilityBadgesHtml +
            '</div>' +
            (typeLine ? '<div class="pt-tools-muted">' + ptEscapeHtml(typeLine) + '</div>' : '') +
          '</div>' +
          '<button type="button" class="pt-catalog-info-btn" data-pt-catalog-info="' + idx + '" aria-controls="' + detailId + '">info ▸</button>' +
        '</div>' +
        '<div id="pt-catalog-row-note-' + idx + '" class="pt-tools-local-status pt-catalog-row-note"></div>' +
        catalogRowActionHtml +
        '<div id="' + detailId + '" class="pt-catalog-detail">' +
          ptCatalogDetailHtml(rec) +
          '<div class="pt-tools-row" style="margin-top:5px;">' +
            '<button type="button" class="pt-tools-mini-btn" data-pt-catalog-quick-add="' + idx + '">Add overlay</button>' +
            '<button type="button" class="pt-tools-mini-btn" data-pt-catalog-load-form="' + idx + '">Load into advanced form</button>' +
          '</div>' +
        '</div>' +
      '</div>';
    }

    var groups = [];
    var byGroup = {};
    var groupOrders = {};

    rows.forEach(function(rec) {
      var groupName = ptCatalogExternalGroupName(rec);

      if (!byGroup[groupName]) {
        byGroup[groupName] = [];
        groups.push(groupName);
        groupOrders[groupName] = ptCatalogExternalGroupOrder(rec);
      }

      byGroup[groupName].push(rec);
      groupOrders[groupName] = Math.min(groupOrders[groupName], ptCatalogExternalGroupOrder(rec));
    });

    groups.sort(function(a, b) {
      var ao = groupOrders[a] || 999;
      var bo = groupOrders[b] || 999;
      if (ao !== bo) return ao - bo;
      return String(a).localeCompare(String(b));
    });

    ptCatalogVisibleGroupNames = groups.slice();

    var html = '';

    groups.forEach(function(groupName) {
      var groupRows = byGroup[groupName] || [];
      var groupKey = ptCatalogGroupKey(groupName);
      var hasActive = ptCatalogGroupHasActiveLayer(groupRows);
      var isExpanded = ptCatalogGroupIsExpanded(groupName, groupRows, q);
      var groupClass = isExpanded ? ' pt-catalog-group-expanded' : ' pt-catalog-group-collapsed';
      var groupTotal = ptCatalogNumericField(groupRows[0], 'group_layer_count', groupRows.length);
      groupTotal = Math.max(groupRows.length, groupTotal);
      var groupCapabilityBadgesHtml = ptCatalogHierarchyCapabilityBadgesHtml(
        groupRows[0], 'group', groupTotal
      );

      html += '<div class="pt-catalog-group' + groupClass + '" data-pt-catalog-group="' + ptEscapeHtml(groupKey) + '">' +
        '<button type="button" class="pt-catalog-group-title" data-pt-catalog-group-toggle="' + ptEscapeHtml(groupKey) + '" aria-expanded="' + (isExpanded ? 'true' : 'false') + '">' +
          '<span class="pt-catalog-group-toggle-symbol">' + (isExpanded ? '−' : '+') + '</span>' +
          '<span class="pt-catalog-group-label">' + ptEscapeHtml(groupName) + '</span>' +
          '<span class="pt-catalog-group-meta">' +
            '<span class="pt-catalog-group-count">' + ptCatalogHierarchyCountHtml(groupRows.length, groupTotal) + '</span>' +
            groupCapabilityBadgesHtml +
          '</span>' +
          (hasActive ? '<span class="pt-catalog-group-active-badge">Active</span>' : '') +
        '</button>' +
        '<div class="pt-catalog-group-rows">';

      var subgroups = [];
      var bySubgroup = {};
      var subgroupOrders = {};

      groupRows.forEach(function(rec) {
        var subgroupName = ptCatalogExternalSubgroupName(rec);

        if (!bySubgroup[subgroupName]) {
          bySubgroup[subgroupName] = [];
          subgroups.push(subgroupName);
          subgroupOrders[subgroupName] = ptCatalogExternalSubgroupOrder(rec);
        }

        bySubgroup[subgroupName].push(rec);
        subgroupOrders[subgroupName] = Math.min(subgroupOrders[subgroupName], ptCatalogExternalSubgroupOrder(rec));
      });

      subgroups.sort(function(a, b) {
        var ao = subgroupOrders[a] || 999;
        var bo = subgroupOrders[b] || 999;
        if (ao !== bo) return ao - bo;
        return String(a).localeCompare(String(b));
      });

      subgroups.forEach(function(subgroupName) {
        var subgroupRows = bySubgroup[subgroupName] || [];

        subgroupRows.sort(function(a, b) {
          var ap = ptCatalogPriorityOrder(a);
          var bp = ptCatalogPriorityOrder(b);
          if (ap !== bp) return ap - bp;
          return ptCatalogField(a, 'display_name').localeCompare(ptCatalogField(b, 'display_name'));
        });

        var showSubgroupHeading = (subgroups.length > 1 || subgroupName !== groupName);
        var subgroupKey = ptCatalogSubgroupKey(groupName, subgroupName);
        var subgroupTotal = ptCatalogNumericField(
          subgroupRows[0], 'subgroup_layer_count', subgroupRows.length
        );
        subgroupTotal = Math.max(subgroupRows.length, subgroupTotal);
        var subgroupCapabilityBadgesHtml = ptCatalogHierarchyCapabilityBadgesHtml(
          subgroupRows[0], 'subgroup', subgroupTotal
        );

        // If a parent group contains only one subgroup with the same label as
        // the parent, the subgroup header is hidden to avoid a duplicate row.
        // In that case the child rows must be shown whenever the parent group
        // is expanded; otherwise the parent +/- toggles but appears empty.
        // This affected the External Fire Perimeters group after the camera
        // rows were moved to Ops Live and the only remaining subgroup matched
        // the parent label.
        var subgroupExpanded = showSubgroupHeading ?
          ptCatalogSubgroupIsExpanded(groupName, subgroupName, subgroupRows, q) :
          true;

        if (showSubgroupHeading) {
          html += '<button type="button" class="pt-catalog-subgroup-title" data-pt-catalog-subgroup-toggle="' + ptEscapeHtml(subgroupKey) + '" aria-expanded="' + (subgroupExpanded ? 'true' : 'false') + '">' +
            '<span class="pt-catalog-subgroup-toggle-symbol">' + (subgroupExpanded ? '−' : '+') + '</span>' +
            '<span class="pt-catalog-subgroup-label">' + ptEscapeHtml(subgroupName) + '</span>' +
            '<span class="pt-catalog-subgroup-meta">' +
              '<span class="pt-catalog-subgroup-count">' + ptCatalogHierarchyCountHtml(subgroupRows.length, subgroupTotal) + '</span>' +
              subgroupCapabilityBadgesHtml +
            '</span>' +
          '</button>';
        }

        if (subgroupExpanded) {
          subgroupRows.forEach(function(rec) {
            html += ptCatalogRowHtml(rec);
          });
        }
      });

      html += '</div></div>';
    });

    list.innerHTML = html;
  }

  function ptToggleCatalogInfo(idx, button) {
    var detail = document.getElementById('pt-catalog-detail-' + idx);
    if (!detail) return;

    var open = detail.classList.toggle('pt-catalog-detail-open');

    if (button) {
      button.textContent = open ? 'info ▾' : 'info ▸';
    }
  }

  // --------------------------------------------------------------------------
  // Source-links modal
  // --------------------------------------------------------------------------

  function ptOpenSourceLinksModal() {

    var old = document.getElementById('pt-source-links-overlay');
    if (old) old.remove();

    var overlay = document.createElement('div');
    overlay.id = 'pt-source-links-overlay';
    overlay.className = 'pt-source-links-overlay';

    var panel = document.createElement('div');
    panel.className = 'pt-source-links-panel';

    var close = document.createElement('button');
    close.type = 'button';
    close.className = 'pt-source-links-close';
    close.textContent = 'Close';
    close.onclick = function() {
      overlay.remove();
    };

    var content = document.createElement('div');
    content.innerHTML =
      '<h1 style="margin-top:0;">GIS service sources to try</h1>' +
      '<p style="font-size:13px;color:#555;max-width: 460px;">' +
      'These links are starting points for finding public GIS services. For the Add Data tool, try to paste a direct service endpoint when possible, such as a URL ending in <code>/FeatureServer/0</code> or <code>/MapServer/3</code>.' +
      '</p>' +

      '<h2>How to recognize usable URLs</h2>' +
      '<ul>' +
        '<li><b>ArcGIS FeatureServer layer:</b> URL contains <code>/FeatureServer/0</code>, <code>/FeatureServer/1</code>, etc. These are vector layers that can often be styled and queried.</li>' +
        '<li><b>ArcGIS MapServer layer:</b> URL contains <code>/MapServer</code> or <code>/MapServer/3</code>. These are often server-rendered map images and are useful for large or pre-symbolized layers.</li>' +
        '<li><b>GeoJSON:</b> URL ends in <code>.geojson</code> or returns GeoJSON directly. These work best for small public datasets that allow browser access.</li>' +
        '<li><b>Hub / portal pages:</b> URLs containing <code>/datasets/</code> are usually information pages, not direct map services. Look for API, REST, ArcGIS GeoServices, or View in ArcGIS Online links.</li>' +
        '<li><b>Large national FeatureServer layers:</b> zoom to your area of interest and use <code>load current map view only</code> to avoid drawing the whole western U.S. or CONUS in the browser.</li>' +
        '<li><b>Legends:</b> MapServer catalog layers may include a legend link. If BRIM cannot draw a service legend directly, use the Legend link in the Active temporary layers list.</li>' +
      '</ul>' +

      '<h2>California water and state data</h2>' +
      '<ul>' +
        '<li><a target="_blank" href="https://data.cnra.ca.gov/organization/dwr/portal/data?groups=water">DWR / CNRA water data portal</a></li>' +
        '<li><a target="_blank" href="https://data.cnra.ca.gov/organization/dwr/portal/data?groups=water&res_format=ArcGIS+GeoServices+REST+API">DWR / CNRA water ArcGIS REST resources</a></li>' +
        '<li><a target="_blank" href="https://waterboards.maps.arcgis.com/">SWRCB / Water Boards ArcGIS Online</a></li>' +
        '<li><a target="_blank" href="https://data.ca.gov/">California Open Data portal</a></li>' +
      '</ul>' +

      '<h2>Biology, habitat, and conservation</h2>' +
      '<ul>' +
        '<li><a target="_blank" href="https://wildlife.ca.gov/Data/GIS/Map-Services">CDFW GIS map services</a></li>' +
        '<li><a target="_blank" href="https://wildlife.ca.gov/Data">CDFW data and GIS resources</a></li>' +
        '<li><a target="_blank" href="https://gis-fws.opendata.arcgis.com/">USFWS Open Data / ArcGIS Hub</a></li>' +
        '<li><a target="_blank" href="https://ecos.fws.gov/ecp/report/table/critical-habitat.html">USFWS critical habitat information</a></li>' +
      '</ul>' +

      '<h2>Air quality / smoke</h2>' +
      '<ul>' +
        '<li><a target="_blank" href="https://portal.airfire.org/">AirFire portal / BlueSky smoke tools</a></li>' +
        '<li><a target="_blank" href="https://fire.airnow.gov/">AirNow Fire & Smoke Map</a></li>' +
        '<li><a target="_blank" href="https://ww2.arb.ca.gov/applications/air-monitoring-sites-interactive-map">CARB air monitoring sites interactive map</a></li>' +
        '<li><a target="_blank" href="https://docs.airnowapi.org/webservices">AirNow API web services documentation</a></li>' +
        '<li><a target="_blank" href="https://community.purpleair.com/t/about-the-purpleair-api/7145">PurpleAir API information</a></li>' +
      '</ul>' +

      '<h2>BLM minerals / MLRS</h2>' +
      '<ul>' +
        '<li><a target="_blank" href="https://www.blm.gov/services/land-records/mlrs">BLM Mineral & Land Records System (MLRS)</a></li>' +
        '<li><a target="_blank" href="https://reports.blm.gov/reports/mlrs">MLRS reports</a></li>' +
        '<li><a target="_blank" href="https://gis.blm.gov/nlsdb/rest/services/Mining_Claims/MiningClaims/MapServer">BLM MLRS Mining Claims REST service</a></li>' +
        '<li><a target="_blank" href="https://gis.blm.gov/nlsdb/rest/services/HUB">BLM NLSDB / MLRS HUB REST services</a></li>' +
      '</ul>' +

      '<h2>Energy / Minerals → Underground Injection Control (UIC)</h2>' +
      '<ul>' +
        '<li><a target="_blank" href="https://www.epa.gov/uic/aquifer-exemption-data">EPA Aquifer Exemption Data</a></li>' +
        '<li><a target="_blank" href="https://www.epa.gov/uic/california-uic-program-oversight-arods">EPA California aquifer-exemption decisions</a></li>' +
        '<li><a target="_blank" href="https://www.conservation.ca.gov/calgem/Pages/Aquifer-Exemptions-Status.aspx">CalGEM Aquifer Exemptions Status</a></li>' +
        '<li><a target="_blank" href="https://services.arcgis.com/cJ9YHowT8TU7DUyn/ArcGIS/rest/services/Aquifer_Exemptions_Feature_Layer/FeatureServer">EPA Aquifer Exemptions GIS service</a></li>' +
        '<li><a target="_blank" href="https://gis.conservation.ca.gov/server/rest/services/CalGEM/Post_Primacy_Aquifer_Exemptions/FeatureServer">CalGEM post-primacy GIS service</a></li>' +
        '<li><a target="_blank" href="https://gis.conservation.ca.gov/server/rest/services/CalGEM/Primacy_Aquifer_Exemptions/FeatureServer">CalGEM 1983 primacy GIS service</a></li>' +
        '<li><a target="_blank" href="https://www.epa.gov/uic">EPA Underground Injection Control program</a></li>' +
        '<li><a target="_blank" href="https://www.epa.gov/uic/underground-injection-control-regulations-and-safe-drinking-water-act-provisions">EPA UIC regulations and Safe Drinking Water Act provisions</a></li>' +
        '<li><a target="_blank" href="https://www.ecfr.gov/current/title-40/chapter-I/subchapter-D/part-144/section-144.7">40 CFR 144.7 — Identification of USDWs and exempted aquifers</a></li>' +
        '<li><a target="_blank" href="https://www.ecfr.gov/current/title-40/chapter-I/subchapter-D/part-146/section-146.4">40 CFR 146.4 — Criteria for exempted aquifers</a></li>' +
      '</ul>' +

      '<h2>Federal lands, hazards, coastal, and operational data</h2>' +
      '<ul>' +
        '<li><a target="_blank" href="https://gbp-blm-egis.hub.arcgis.com/">BLM GBP Hub</a></li>' +
        '<li><a target="_blank" href="https://gbp-blm-egis.hub.arcgis.com/pages/aim">BLM AIM data hub</a></li>' +
        '<li><a target="_blank" href="https://gbp-blm-egis.hub.arcgis.com/search?groupIds=a67028668c2e402580001d61a5f59389">BLM AIM search group</a></li>' +
        '<li><a target="_blank" href="https://gis.blm.gov/arcgis/rest/services">BLM public ArcGIS REST services directory</a></li>' +
        '<li><a target="_blank" href="https://hazards.fema.gov/femaportal/wps/portal/NFHLWMS">FEMA National Flood Hazard Layer services</a></li>' +
        '<li><a target="_blank" href="https://noaa.maps.arcgis.com/">NOAA GeoPlatform / ArcGIS Online</a></li>' +
        '<li><a target="_blank" href="https://coast.noaa.gov/arcgis/rest/services">NOAA Coast ArcGIS REST services</a></li>' +
      '</ul>' +

      '<h2>Real-time water data</h2>' +
      '<ul>' +
        '<li><a target="_blank" href="https://waterservices.usgs.gov/docs/instantaneous-values/instantaneous-values-details/">USGS Instantaneous Values service documentation</a></li>' +
        '<li><a target="_blank" href="https://waterdata.usgs.gov/nwis/rt">USGS real-time water data</a></li>' +
        '<li><a target="_blank" href="https://cdec.water.ca.gov/">California Data Exchange Center</a></li>' +
      '</ul>' +

      '<h2>Notes</h2>' +
      '<p>' +
      'Not every public service will load from a standalone local HTML file. Some services block browser requests, require authentication, do not support the requested format, or are too large for interactive display. This tool is intended for quick screening overlays, not permanent data management.' +
      '</p>';

    panel.appendChild(close);
    panel.appendChild(content);
    overlay.appendChild(panel);
    document.body.appendChild(overlay);

    overlay.addEventListener('click', function(e) {
      if (e.target === overlay) {
        overlay.remove();
      }
    });
  }

  // --------------------------------------------------------------------------
  // Layer Explorer read-only modal
  // --------------------------------------------------------------------------

  function ptLayerExplorerElement(tagName, className, textValue) {
    var node = document.createElement(tagName);
    if (className) node.className = className;
    if (textValue !== undefined && textValue !== null) {
      node.textContent = String(textValue);
    }
    return node;
  }

  function ptLayerExplorerDetailRow(list, label, value) {
    var term = ptLayerExplorerElement('dt', 'pt-layer-explorer-detail-label', label);
    var description = ptLayerExplorerElement(
      'dd',
      'pt-layer-explorer-detail-value',
      value || 'UNKNOWN'
    );
    list.appendChild(term);
    list.appendChild(description);
  }

  function ptOpenLayerExplorer() {
    var old = document.getElementById('pt-layer-explorer-overlay');
    if (old) return;
    var previouslyFocused = document.activeElement;

    var overlay = ptLayerExplorerElement(
      'div',
      'pt-source-links-overlay pt-layer-explorer-overlay'
    );
    overlay.id = 'pt-layer-explorer-overlay';

    var panel = ptLayerExplorerElement(
      'section',
      'pt-source-links-panel pt-layer-explorer-panel'
    );
    panel.setAttribute('role', 'dialog');
    panel.setAttribute('aria-modal', 'true');
    panel.setAttribute('aria-labelledby', 'pt-layer-explorer-title');

    var close = ptLayerExplorerElement(
      'button',
      'pt-source-links-close pt-layer-explorer-close',
      'Close'
    );
    close.type = 'button';

    var content = ptLayerExplorerElement('div', 'pt-layer-explorer-content');
    var title = ptLayerExplorerElement('h1', '', 'Layer Explorer');
    title.id = 'pt-layer-explorer-title';

    var coverageNotice = ptLayerExplorerElement(
      'p',
      'pt-layer-explorer-notice',
      'Partial catalog: all 26 currently cataloged records are shown, but these records do not represent every BRIM layer.'
    );
    coverageNotice.id = 'pt-layer-explorer-coverage-notice';

    var isolationNotice = ptLayerExplorerElement(
      'p',
      'pt-layer-explorer-isolation',
      'Read-only metadata only. Layer Explorer cannot turn layers on or off or change map behavior.'
    );

    var controls = ptLayerExplorerElement('div', 'pt-layer-explorer-controls');
    var searchLabel = ptLayerExplorerElement('label', '', 'Search display name or stable ID');
    searchLabel.setAttribute('for', 'pt-layer-explorer-search');
    var search = ptLayerExplorerElement('input', 'pt-tools-input');
    search.id = 'pt-layer-explorer-search';
    search.type = 'search';
    search.placeholder = 'Search 26 catalog records…';

    var architectureLabel = ptLayerExplorerElement('label', '', 'Architecture');
    architectureLabel.setAttribute('for', 'pt-layer-explorer-architecture');
    var architectureSelect = ptLayerExplorerElement('select', 'pt-tools-select');
    architectureSelect.id = 'pt-layer-explorer-architecture';
    var allOption = ptLayerExplorerElement('option', '', 'All architectures');
    allOption.value = 'ALL';
    architectureSelect.appendChild(allOption);
    ptLayerExplorerModel.architectures.forEach(function(architecture) {
      var option = ptLayerExplorerElement('option', '', architecture);
      option.value = architecture;
      architectureSelect.appendChild(option);
    });

    var searchWrap = ptLayerExplorerElement('div', 'pt-layer-explorer-control');
    searchWrap.appendChild(searchLabel);
    searchWrap.appendChild(search);
    var architectureWrap = ptLayerExplorerElement('div', 'pt-layer-explorer-control');
    architectureWrap.appendChild(architectureLabel);
    architectureWrap.appendChild(architectureSelect);
    controls.appendChild(searchWrap);
    controls.appendChild(architectureWrap);

    var count = ptLayerExplorerElement('div', 'pt-layer-explorer-count');
    count.id = 'pt-layer-explorer-count';
    count.setAttribute('aria-live', 'polite');

    var layout = ptLayerExplorerElement('div', 'pt-layer-explorer-layout');
    var list = ptLayerExplorerElement('div', 'pt-layer-explorer-list');
    list.id = 'pt-layer-explorer-list';
    list.setAttribute('role', 'listbox');
    list.setAttribute('aria-label', 'Partial BRIM layer catalog records');
    var detail = ptLayerExplorerElement('article', 'pt-layer-explorer-detail');
    detail.id = 'pt-layer-explorer-detail';
    layout.appendChild(list);
    layout.appendChild(detail);

    content.appendChild(title);
    content.appendChild(coverageNotice);
    content.appendChild(isolationNotice);
    content.appendChild(controls);
    content.appendChild(count);
    content.appendChild(layout);
    panel.appendChild(close);
    panel.appendChild(content);
    overlay.appendChild(panel);
    document.body.appendChild(overlay);

    var selectedId = ptLayerExplorerModel.records.length ?
      ptLayerExplorerModel.records[0].stableId : '';

    function renderDetail(record) {
      detail.textContent = '';
      if (!record) {
        detail.appendChild(ptLayerExplorerElement(
          'p',
          'pt-tools-muted',
          'No catalog record matches the current search and architecture filter.'
        ));
        return;
      }

      detail.appendChild(ptLayerExplorerElement(
        'h2',
        'pt-layer-explorer-detail-title',
        record.displayName
      ));
      var metadata = ptLayerExplorerElement('dl', 'pt-layer-explorer-detail-list');
      ptLayerExplorerDetailRow(metadata, 'Stable ID', record.stableId);
      ptLayerExplorerDetailRow(metadata, 'Architecture', record.architecture);
      ptLayerExplorerDetailRow(metadata, 'Catalog category', record.catalogPath);
      ptLayerExplorerDetailRow(metadata, 'Implementation', record.implementation);
      ptLayerExplorerDetailRow(metadata, 'Renderer', record.renderer);
      ptLayerExplorerDetailRow(metadata, 'Domain metadata', record.domainReview);
      ptLayerExplorerDetailRow(metadata, 'Catalog confidence', record.confidence);
      detail.appendChild(metadata);
      detail.appendChild(ptLayerExplorerElement(
        'p',
        'pt-layer-explorer-detail-boundary',
        'Catalog authority: descriptive only · Runtime authority: unchanged'
      ));
    }

    function render() {
      var records = ptLayerExplorerModel.filterRecords(
        search.value,
        architectureSelect.value
      );
      var selectedRecord = null;

      records.forEach(function(record) {
        if (record.stableId === selectedId) selectedRecord = record;
      });
      if (!selectedRecord && records.length) {
        selectedRecord = records[0];
        selectedId = selectedRecord.stableId;
      }

      count.textContent = records.length + ' of ' +
        ptLayerExplorerModel.records.length + ' catalog records shown';
      list.textContent = '';

      records.forEach(function(record) {
        var button = ptLayerExplorerElement('button', 'pt-layer-explorer-record');
        button.type = 'button';
        button.setAttribute('role', 'option');
        button.setAttribute('data-pt-layer-explorer-id', record.stableId);
        button.setAttribute('aria-selected', record.stableId === selectedId ? 'true' : 'false');
        if (record.stableId === selectedId) {
          button.classList.add('pt-layer-explorer-record-selected');
        }
        button.appendChild(ptLayerExplorerElement(
          'span',
          'pt-layer-explorer-record-name',
          record.displayName
        ));
        button.appendChild(ptLayerExplorerElement(
          'span',
          'pt-layer-explorer-record-id',
          record.stableId + ' · ' + record.architecture
        ));
        button.addEventListener('click', function() {
          selectedId = record.stableId;
          render();
        });
        list.appendChild(button);
      });

      renderDetail(selectedRecord);
    }

    function closeExplorer() {
      document.removeEventListener('keydown', onKeydown);
      overlay.remove();
      if (previouslyFocused && previouslyFocused.focus) previouslyFocused.focus();
    }

    function onKeydown(event) {
      if (event.key === 'Escape') closeExplorer();
    }

    close.addEventListener('click', closeExplorer);
    overlay.addEventListener('click', function(event) {
      if (event.target === overlay) closeExplorer();
    });
    document.addEventListener('keydown', onKeydown);
    search.addEventListener('input', render);
    architectureSelect.addEventListener('change', render);

    render();
    search.focus();
  }

  // --------------------------------------------------------------------------
  // Build UI
  // --------------------------------------------------------------------------

  if (document.getElementById('pt-tools-adddata-wrap')) {
    return;
  }

  var measureWrap = document.createElement('div');
  measureWrap.id = 'pt-measure-wrap';
  measureWrap.className = 'pt-measure-wrap pt-measure-collapsed';
  measureWrap.innerHTML =
    '<div class="pt-measure-tab" id="pt-measure-tab">' +
      '<span>measure</span>' +
    '</div>' +
    '<div class="pt-measure-body">' +
      '<div class="pt-tools-row">' +
        '<button type="button" id="pt-measure-distance-btn" class="pt-tools-btn">Distance</button>' +
        '<button type="button" id="pt-measure-area-btn" class="pt-tools-btn">Area</button>' +
        '<button type="button" id="pt-measure-finish-btn" class="pt-tools-btn" style="display:none;">Finish</button>' +
        '<button type="button" id="pt-measure-clear-btn" class="pt-tools-btn">Clear</button>' +
      '</div>' +
      '<div id="pt-measure-status" class="pt-tools-local-status"></div>' +
      '<div class="pt-tools-muted">Click map points after choosing Distance or Area. Measurements are screening-level only.</div>' +
    '</div>';

  var teachingWrap = document.createElement('div');
  teachingWrap.id = 'pt-teaching-markup-wrap';
  teachingWrap.className = 'pt-teaching-markup-wrap pt-teaching-markup-collapsed';
  teachingWrap.innerHTML =
    '<button type="button" class="pt-teaching-markup-tab" id="pt-teaching-markup-tab" aria-expanded="false" aria-label="Draw / Label" title="Draw / Label">' +
      '<span>draw/label</span>' +
    '</button>' +
    '<div class="pt-teaching-markup-body">' +
      '<div class="pt-teaching-mode-tabs" role="tablist" aria-label="Draw or label mode">' +
        '<button type="button" id="pt-teaching-mode-draw" class="pt-teaching-mode-tab" role="tab" aria-selected="true" aria-controls="pt-teaching-draw-panel">Draw</button>' +
        '<button type="button" id="pt-teaching-mode-label" class="pt-teaching-mode-tab" role="tab" aria-selected="false" aria-controls="pt-teaching-label-panel" tabindex="-1">Label</button>' +
      '</div>' +
      '<div class="pt-teaching-common-actions">' +
        '<button type="button" id="pt-teaching-markup-undo-btn" class="pt-tools-btn pt-teaching-mini-btn">Undo</button>' +
        '<button type="button" id="pt-teaching-markup-clear-btn" class="pt-tools-btn pt-teaching-mini-btn">Clear</button>' +
      '</div>' +
      '<div id="pt-teaching-draw-panel" class="pt-teaching-tool-panel" role="tabpanel" aria-labelledby="pt-teaching-mode-draw">' +
        '<label class="pt-teaching-markup-check" title="Turn draw mode on/off">' +
          '<input type="checkbox" id="pt-teaching-markup-toggle"/> On' +
        '</label>' +
        '<div class="pt-teaching-markup-grid">' +
          '<label>C<br/><select id="pt-teaching-markup-color" class="pt-teaching-markup-select" title="Color">' +
            '<option value="#FFD84D">yellow</option>' +
            '<option value="#E53E3E">red</option>' +
            '<option value="#3388ff" selected>blue</option>' +
            '<option value="#2F855A">green</option>' +
            '<option value="#333333">gray</option>' +
          '</select></label>' +
          '<label>B<br/><select id="pt-teaching-markup-size" class="pt-teaching-markup-select" title="Brush size">' +
            '<option value="3">micro</option>' +
            '<option value="5">tiny</option>' +
            '<option value="9" selected>small</option>' +
            '<option value="18">medium</option>' +
            '<option value="30">large</option>' +
            '<option value="46">huge</option>' +
            '<option value="64">max</option>' +
          '</select></label>' +
          '<label>O<br/><select id="pt-teaching-markup-opacity" class="pt-teaching-markup-select" title="Opacity">' +
            '<option value="0.25">low</option>' +
            '<option value="0.45" selected>med</option>' +
            '<option value="0.65">high</option>' +
          '</select></label>' +
        '</div>' +
      '</div>' +
      '<div id="pt-teaching-label-panel" class="pt-teaching-tool-panel" role="tabpanel" aria-labelledby="pt-teaching-mode-label" hidden>' +
        '<label class="pt-teaching-label-input-label" for="pt-teaching-label-text">Label text</label>' +
        '<input type="text" id="pt-teaching-label-text" class="pt-teaching-label-input" autocomplete="off" placeholder="MW-3a"/>' +
        '<div class="pt-teaching-label-grid">' +
          '<label>Style<select id="pt-teaching-label-style" class="pt-teaching-markup-select">' +
            '<option value="point_text" selected>Point + text</option>' +
            '<option value="text_only">Text only</option>' +
          '</select></label>' +
          '<label>Size<select id="pt-teaching-label-size" class="pt-teaching-markup-select">' +
            '<option value="small" selected>Small</option>' +
            '<option value="medium">Medium</option>' +
            '<option value="large">Large</option>' +
          '</select></label>' +
          '<label>Color<select id="pt-teaching-label-color" class="pt-teaching-markup-select">' +
            '<option value="#FFD84D">yellow</option>' +
            '<option value="#E53E3E">red</option>' +
            '<option value="#3388ff" selected>blue</option>' +
            '<option value="#2F855A">green</option>' +
            '<option value="#333333">gray</option>' +
          '</select></label>' +
        '</div>' +
        '<div class="pt-teaching-label-guidance">Enter text, then click the map to place the label.</div>' +
      '</div>' +
      '<div id="pt-teaching-markup-status" class="pt-tools-local-status pt-teaching-markup-status"></div>' +
    '</div>';

  var clearAllWrap = document.createElement('div');
  clearAllWrap.id = 'pt-clear-all-wrap';
  clearAllWrap.className = 'pt-clear-all-wrap';
  clearAllWrap.innerHTML =
    '<button type="button" id="pt-clear-all-btn" class="pt-clear-all-btn" title="Clear local layers, external layers, uploads, Ops layers, and measurements; turn off Draw / Label placement. Use Draw / Label > Clear to erase annotations.">clear all</button>';

  var wrap = document.createElement('div');
  wrap.id = 'pt-tools-adddata-wrap';
  wrap.className = 'pt-tools-adddata-wrap pt-tools-collapsed';

  wrap.innerHTML =
    '<div class="pt-tools-tab" id="pt-tools-tab">' +
      '<span class="pt-tools-tab-title">External Layers <span id="pt-tools-caret">▸</span></span>' +
      '<button type="button" id="pt-external-clear-ribbon-btn" class="pt-tools-ribbon-clear" title="Clear external overlays">Clear external</button>' +
    '</div>' +
    '<div class="pt-tools-body">' +

      '<div id="pt-tools-status" class="pt-tools-status"></div>' +

      '<div class="pt-tools-section pt-active-external-section" id="pt-active-external-section">' +
        '<button type="button" id="pt-active-external-toggle" class="pt-active-external-toggle" aria-expanded="false" title="Expand/collapse active external overlay details">' +
          '<span id="pt-active-external-caret" class="pt-active-external-caret">▸</span>' +
          '<span id="pt-active-external-label">Active external overlays</span> ' +
          '<span id="pt-active-external-count" class="pt-active-external-count">(0)</span>' +
        '</button>' +
        '<div id="pt-active-external-body" class="pt-active-external-body" style="display:none;">' +
          '<div id="pt-custom-layer-list" class="pt-custom-layer-list"></div>' +
          '<div class="pt-tools-muted">Reference overlays are temporary external browser-session layers and are not saved into the BRIM cache. Use the ribbon button to clear all external overlays.</div>' +
        '</div>' +
      '</div>' +

      '<div class="pt-tools-section">' +
        '<div class="pt-tools-heading">Quick-add external overlays</div>' +
        '<input type="text" id="pt-catalog-search" class="pt-tools-input" placeholder="Search catalog layers, agencies, themes..."/>' +
        '<div class="pt-tools-muted">Click <b>+</b> to add a layer immediately. Use <b>info</b> for URL, legend, notes, hover/popup fields, and other metadata.</div>' +
        ptCapabilityDefinitionKeyHtml() +
        '<div class="pt-tools-muted pt-catalog-load-key" title="Badge colors are BRIM QA load-behavior hints from a multi-extent load audit; external service behavior can vary by zoom, viewport, and provider load.">Load hint: <span class="pt-catalog-id-badge pt-catalog-load-badge pt-catalog-load-green pt-load-key-token">fast</span> <span class="pt-catalog-id-badge pt-catalog-load-badge pt-catalog-load-yellow pt-load-key-token">moderate</span> <span class="pt-catalog-id-badge pt-catalog-load-badge pt-catalog-load-orange pt-load-key-token">slow</span> <span class="pt-catalog-id-badge pt-catalog-load-badge pt-catalog-load-red pt-load-key-token">heavy</span> <span class="pt-catalog-id-badge pt-catalog-load-badge pt-catalog-load-gray pt-load-key-token">unknown</span></div>' +
        '<div class="pt-catalog-group-controls">' +
          '<button type="button" id="pt-catalog-expand-all-btn" class="pt-tools-mini-btn">Expand all</button>' +
          '<button type="button" id="pt-catalog-collapse-all-btn" class="pt-tools-mini-btn">Collapse all</button>' +
        '</div>' +
        '<div id="pt-catalog-action-note" class="pt-tools-local-status"></div>' +
        '<div id="pt-catalog-quick-count" class="pt-tools-muted"></div>' +
        '<div id="pt-catalog-quick-list" class="pt-catalog-quick-list"></div>' +

        '<div id="pt-catalog-legacy-controls" style="display:none;">' +
          '<select id="pt-catalog-agency" class="pt-tools-select"></select>' +
          '<select id="pt-catalog-theme" class="pt-tools-select"></select>' +
          '<select id="pt-catalog-layer" class="pt-tools-select"></select>' +
          '<button type="button" id="pt-catalog-use-btn" class="pt-tools-btn">Load into form</button>' +
          '<button type="button" id="pt-catalog-add-btn" class="pt-tools-btn">Add selected overlay</button>' +
          '<div id="pt-catalog-info" class="pt-tools-muted"></div>' +
        '</div>' +
      '</div>' +


      '<details id="pt-fed-land-status-details" class="pt-tools-details pt-tools-special-details">' +
        '<summary>Federal Land Status</summary>' +
        '<div class="pt-tools-details-body">' +
          '<label class="pt-tools-check">' +
            '<input type="checkbox" id="pt-blm-sma-toggle"/> Fed/State Surface Management Agency (SMA)' +
          '</label>' +
          '<label class="pt-tools-small-label">Opacity</label>' +
          '<input type="range" min="0.15" max="0.90" step="0.05" value="0.58" id="pt-blm-sma-opacity" class="pt-tools-range"/>' +
          '<div class="pt-tools-muted">Live Federal/State SMA service. Useful over USGS Hydrography or imagery.</div>' +
        '</div>' +
      '</details>' +

      '<details id="pt-advanced-manual-details" class="pt-tools-details pt-tools-special-details">' +
        '<summary>Advanced manual URL add</summary>' +
        '<div class="pt-tools-details-body">' +
          '<label class="pt-tools-small-label">Layer name</label>' +
          '<input type="text" id="pt-custom-name" class="pt-tools-input" placeholder="e.g., frog habitat"/>' +
          '<label class="pt-tools-small-label">Service URL</label>' +
          '<textarea id="pt-custom-url" class="pt-tools-textarea" rows="3" placeholder="Paste FeatureServer/0, MapServer/0, or GeoJSON URL"></textarea>' +
          '<div class="pt-tools-muted">Tip: ArcGIS Hub <code>/datasets/...</code> pages usually will not load. Open the API / REST / ArcGIS GeoServices link and paste the direct service endpoint.</div>' +
          '<div id="pt-custom-url-tools" class="pt-tools-url-tools" style="display:none;">' +
            '<button type="button" id="pt-custom-try-zero-btn" class="pt-tools-btn pt-tools-url-helper-btn" style="display:none;">Try /0</button>' +
            '<button type="button" id="pt-custom-open-service-btn" class="pt-tools-btn pt-tools-url-helper-btn">Open service metadata</button>' +
            '<div id="pt-custom-url-helper-note" class="pt-tools-muted"></div>' +
            '<div id="pt-custom-sublayer-chooser" class="pt-tools-sublayer-chooser" style="display:none;">' +
              '<label class="pt-tools-small-label">Layer inside service</label>' +
              '<select id="pt-custom-sublayer-select" class="pt-tools-select"></select>' +
              '<button type="button" id="pt-custom-use-sublayer-btn" class="pt-tools-btn pt-tools-url-helper-btn">Use selected layer</button>' +
            '</div>' +
          '</div>' +
          '<label class="pt-tools-small-label">Type</label>' +
          '<select id="pt-custom-type" class="pt-tools-select">' +
            '<option value="auto" selected>Auto-detect from URL</option>' +
            '<option value="feature">ArcGIS FeatureServer layer</option>' +
            '<option value="map">ArcGIS MapServer layer</option>' +
            '<option value="image">ArcGIS ImageServer layer</option>' +
            '<option value="geojson">GeoJSON</option>' +
          '</select>' +
          '<label class="pt-tools-check" id="pt-custom-clickable-label">' +
            '<input type="checkbox" id="pt-custom-clickable"/> enable popups when supported' +
          '</label>' +
          '<div id="pt-custom-popup-help" class="pt-tools-muted">Popups are supported for FeatureServer and GeoJSON layers when attributes are returned. MapServer visual overlays can preserve official colors; curated catalog rows may use map identify for hover/click where supported.</div>' +
          '<label class="pt-tools-check" id="pt-custom-current-view-label">' +
            '<input type="checkbox" id="pt-custom-current-view"/> for FeatureServer/MapServer, load current map view only' +
          '</label>' +
          '<div id="pt-custom-load-help" class="pt-tools-muted">Use current-view loading for large FeatureServer or queryable MapServer sublayers. Zoom to the area of interest first.</div>' +
          '<label class="pt-tools-small-label">Optional SQL filter</label>' +
          '<input type="text" id="pt-custom-where" class="pt-tools-input" placeholder="e.g., STATE = \'CA\' or MSMT_YEAR = 2011"/>' +
          '<div id="pt-custom-where-help" class="pt-tools-muted">Optional. Applied when this action creates a new overlay; existing layers are unchanged. Supported for FeatureServer queries and MapServer sublayers. Not used for GeoJSON, ImageServer, or tiled layers.</div>' +
          '<input type="hidden" id="pt-custom-legend-url"/>' +
          '<input type="hidden" id="pt-custom-legend-note"/>' +
          '<input type="hidden" id="pt-custom-min-zoom-live"/>' +
          '<input type="hidden" id="pt-custom-min-zoom-current-view"/>' +
          '<input type="hidden" id="pt-custom-popup-fields"/>' +
          '<input type="hidden" id="pt-custom-popup-aliases"/>' +
          '<input type="hidden" id="pt-custom-popup-link-template"/>' +
          '<input type="hidden" id="pt-custom-popup-link-label"/>' +
          '<input type="hidden" id="pt-custom-hover-fields"/>' +
          '<input type="hidden" id="pt-custom-hover-aliases"/>' +
          '<input type="hidden" id="pt-custom-hover-bold-fields"/>' +
          '<input type="hidden" id="pt-custom-hover-no-label-fields"/>' +
          '<input type="hidden" id="pt-custom-hover-round-fields"/>' +
          '<input type="hidden" id="pt-custom-hover-show-native-field-names"/>' +
          '<input type="hidden" id="pt-custom-default-label-field"/>' +
          '<input type="hidden" id="pt-custom-out-fields"/>' +
          '<input type="hidden" id="pt-custom-style-field-candidates"/>' +
          '<input type="hidden" id="pt-custom-default-style-field"/>' +
          '<input type="hidden" id="pt-custom-field-curation-notes"/>' +
          '<input type="hidden" id="pt-custom-show-native-field-names"/>' +
          '<div class="pt-tools-row pt-tools-manual-add-row">' +
            '<button type="button" id="pt-custom-add-btn" class="pt-tools-btn">Add configured overlay</button>' +
            '<span id="pt-custom-add-spinner" class="pt-tools-spinner" aria-hidden="true"></span>' +
          '</div>' +
          '<div id="pt-manual-action-note" class="pt-tools-local-status pt-manual-action-note"></div>' +
          '<div id="pt-manual-refresh-wrap" class="pt-manual-refresh-wrap"></div>' +
          '<div class="pt-tools-muted">Up to 3 temporary external overlays. Use Quick-add for curated catalog rows when possible.</div>' +
        '</div>' +
      '</details>' +


    '</div>';

  el.appendChild(teachingWrap);
  el.appendChild(measureWrap);
  el.appendChild(clearAllWrap);
  el.appendChild(wrap);

  // Capture the normal footer attribution after the PT2 controls are attached
  // and before any user-added external overlays have been loaded.
  window.setTimeout(ptCaptureExternalBaseAttribution, 0);
  window.setTimeout(ptCaptureExternalBaseAttribution, 500);

  // Prevent panel interactions from panning/zooming the map.
  L.DomEvent.disableClickPropagation(teachingWrap);
  L.DomEvent.disableScrollPropagation(teachingWrap);
  L.DomEvent.disableClickPropagation(measureWrap);
  L.DomEvent.disableScrollPropagation(measureWrap);
  L.DomEvent.disableClickPropagation(clearAllWrap);
  L.DomEvent.disableScrollPropagation(clearAllWrap);
  L.DomEvent.disableClickPropagation(wrap);
  L.DomEvent.disableScrollPropagation(wrap);

  // --------------------------------------------------------------------------
  // CSS
  // --------------------------------------------------------------------------

  if (!document.getElementById('pt-tools-adddata-style')) {
    var style = document.createElement('style');
    style.id = 'pt-tools-adddata-style';

    style.innerHTML = `
      .pt-teaching-markup-wrap {
        position: absolute;
        top: 112px;
        left: 8px;
        width: 82px;
        max-width: calc(100vw - 24px);
        /* Keep the draw panel above measure/clear-all.  The expanded draw
         * menu overlaps the measure tab vertically, so draw must have the
         * higher stacking order or the measure tab captures clicks. */
        z-index: 10130;
        font-family: Arial, Helvetica, sans-serif;
        font-size: 10.5px;
        color: #222;
        pointer-events: auto;
      }

      .pt-teaching-markup-wrap:not(.pt-teaching-markup-collapsed) {
        z-index: 10140;
      }

      .pt-teaching-markup-tab {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        gap: 3px;
        width: 82px;
        height: 22px;
        box-sizing: border-box;
        background: rgba(239, 239, 236, 0.97);
        border: 1px solid rgba(108, 108, 98, 0.76);
        border-radius: 5px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.24);
        font-weight: 700;
        font-size: 12px;
        padding: 0 4px;
        cursor: pointer;
        user-select: none;
        color: #222;
        line-height: 20px;
        appearance: none;
        -webkit-appearance: none;
      }

      .pt-teaching-markup-tab:hover,
      .pt-teaching-markup-tab:focus-visible,
      .pt-teaching-markup-on .pt-teaching-markup-tab {
        background: rgba(232, 236, 239, 0.99);
        border-color: rgba(110, 110, 110, 0.95);
        outline: 2px solid #255E9B;
        outline-offset: 1px;
      }

      .pt-teaching-markup-body {
        display: block;
        position: absolute;
        top: 27px;
        left: 0;
        width: 250px;
        box-sizing: border-box;
        margin-top: 0;
        padding: 4px;
        border-radius: 6px;
        border: 1px solid rgba(145, 145, 145, 0.60);
        background: rgba(250, 250, 246, 0.96);
        box-shadow: 0 1px 6px rgba(0,0,0,0.25);
      }

      .pt-teaching-markup-collapsed .pt-teaching-markup-body {
        display: none;
      }

      .pt-teaching-mode-tabs {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 2px;
        padding: 2px;
        margin-bottom: 4px;
        border-radius: 5px;
        background: #e4e4df;
      }

      .pt-teaching-mode-tab {
        border: 1px solid transparent;
        border-radius: 4px;
        background: transparent;
        color: #333;
        font: 700 11px/20px Arial, Helvetica, sans-serif;
        cursor: pointer;
      }

      .pt-teaching-mode-tab[aria-selected="true"] {
        border-color: #8b8b82;
        background: #fff;
        color: #111;
        box-shadow: 0 1px 2px rgba(0,0,0,0.16);
      }

      .pt-teaching-mode-tab:focus-visible {
        outline: 2px solid #255E9B;
        outline-offset: 1px;
      }

      .pt-teaching-common-actions {
        display: flex;
        justify-content: flex-end;
        gap: 3px;
        align-items: center;
        margin-bottom: 4px;
      }

      .pt-teaching-tool-panel[hidden] {
        display: none;
      }

      .pt-teaching-tool-panel {
        min-width: 0;
      }

      .pt-teaching-markup-check {
        display: inline-flex;
        align-items: center;
        gap: 3px;
        font-weight: 700;
        cursor: pointer;
        margin: 0 0 4px 0;
        white-space: nowrap;
      }

      .pt-teaching-mini-btn {
        padding: 2px 5px !important;
        min-width: 38px;
        font-size: 11px !important;
        line-height: 15px !important;
      }

      .pt-teaching-markup-grid {
        display: grid;
        grid-template-columns: 1fr 1fr 1fr;
        gap: 3px;
        margin-bottom: 3px;
      }

      .pt-teaching-markup-select {
        width: 100%;
        box-sizing: border-box;
        font-size: 10px;
        padding: 1px 1px;
        border-radius: 4px;
        border: 1px solid #aaa;
        background: #ffffff;
      }

      .pt-teaching-label-input-label {
        display: block;
        margin-bottom: 2px;
        font-weight: 700;
      }

      .pt-teaching-label-input {
        display: block;
        width: 100%;
        box-sizing: border-box;
        margin: 0 0 5px 0;
        padding: 4px 5px;
        border: 1px solid #999;
        border-radius: 4px;
        font: 12px Arial, Helvetica, sans-serif;
      }

      .pt-teaching-label-grid {
        display: grid;
        grid-template-columns: 1.35fr 0.9fr 0.85fr;
        gap: 4px;
        align-items: end;
      }

      .pt-teaching-label-grid label {
        min-width: 0;
        font-size: 10px;
        font-weight: 700;
      }

      .pt-teaching-label-guidance {
        margin-top: 5px;
        color: #555;
        font-size: 10px;
        line-height: 1.2;
      }

      .pt-teaching-label-input:focus-visible,
      .pt-teaching-markup-select:focus-visible,
      .pt-teaching-markup-check input:focus-visible {
        outline: 2px solid #255E9B;
        outline-offset: 1px;
      }

      .pt-teaching-markup-status {
        margin-top: 2px;
        padding: 0;
        min-height: 0;
        font-size: 10px;
        line-height: 1.15;
      }

      .leaflet-container.pt-teaching-markup-active,
      .leaflet-container.pt-teaching-markup-active .leaflet-interactive {
        cursor: none !important;
      }

      .leaflet-container.pt-measure-active,
      .leaflet-container.pt-measure-active .leaflet-interactive {
        cursor: crosshair !important;
      }

      .leaflet-container.pt-measure-active .leaflet-popup,
      .leaflet-container.pt-measure-active .leaflet-tooltip:not(.pt-label) {
        display: none !important;
      }

      .leaflet-container.pt-measure-active .leaflet-pane.pt-measure-pane-suspended,
      .leaflet-container.pt-measure-active .leaflet-pane.pt-measure-pane-suspended * {
        pointer-events: none !important;
      }

      .pt-teaching-label-icon {
        width: 1px !important;
        height: 1px !important;
        overflow: visible;
        border: 0;
        background: transparent;
        cursor: move !important;
      }

      .leaflet-container.pt-teaching-label-active .pt-teaching-label-icon,
      .leaflet-container.pt-teaching-label-active .pt-teaching-label-icon * {
        cursor: move !important;
      }

      .pt-teaching-label-text {
        position: absolute;
        top: 0;
        left: 0;
        display: block;
        width: max-content;
        max-width: 280px;
        color: #3388ff;
        font-family: Arial, Helvetica, sans-serif;
        font-weight: 700;
        line-height: 1.1;
        overflow-wrap: anywhere;
        text-shadow:
          -1px -1px 0 #fff,
          1px -1px 0 #fff,
          -1px 1px 0 #fff,
          1px 1px 0 #fff,
          0 0 3px #fff,
          0 1px 4px rgba(255,255,255,0.92);
        user-select: none;
        pointer-events: auto;
        cursor: move;
      }

      .pt-teaching-label-point-text .pt-teaching-label-text {
        left: 11px;
        top: -2px;
      }

      .pt-teaching-label-point {
        position: absolute;
        left: 1px;
        top: 1px;
        width: 8px;
        height: 8px;
        box-sizing: border-box;
        border: 2px solid #3388ff;
        border-radius: 50%;
        box-shadow: 0 0 0 1px #fff, 0 1px 3px rgba(0,0,0,0.5);
        pointer-events: auto;
        cursor: move;
      }

      .pt-teaching-markup-brush-cursor {
        position: absolute;
        left: 0;
        top: 0;
        display: none;
        box-sizing: border-box;
        border: 1.5px solid #3388ff;
        border-radius: 999px;
        pointer-events: none;
        z-index: 10055;
        will-change: transform, width, height;
      }

      .leaflet-container.pt-teaching-markup-active.pt-teaching-markup-pan-ready,
      .leaflet-container.pt-teaching-markup-active.pt-teaching-markup-pan-ready .leaflet-interactive {
        cursor: grab !important;
      }

      .leaflet-container.pt-teaching-markup-active.pt-teaching-markup-panning,
      .leaflet-container.pt-teaching-markup-active.pt-teaching-markup-panning .leaflet-interactive {
        cursor: grabbing !important;
      }

      .pt-teaching-markup-wrap,
      .pt-teaching-markup-wrap *,
      .pt-measure-wrap,
      .pt-measure-wrap *,
      .pt-tools-adddata-wrap,
      .pt-tools-adddata-wrap * {
        cursor: auto;
      }

      .pt-teaching-markup-tab,
      .pt-teaching-markup-check,
      .pt-teaching-markup-check input,
      .pt-teaching-markup-wrap button,
      .pt-measure-wrap button,
      .pt-measure-tab,
      .pt-tools-adddata-wrap button,
      .pt-tools-tab,
      .pt-tools-tab * {
        cursor: pointer;
      }

      .pt-tools-adddata-wrap a,
      .pt-catalog-detail a,
      .pt-custom-layer-row a {
        cursor: pointer;
      }

      .pt-measure-wrap {
        position: absolute;
        top: 140px;
        left: 8px;
        width: 82px;
        max-width: calc(100vw - 24px);
        z-index: 10090;
        font-family: Arial, Helvetica, sans-serif;
        font-size: 12px;
        color: #222;
        pointer-events: auto;
      }

      .pt-clear-all-wrap {
        position: absolute;
        top: 168px;
        left: 8px;
        z-index: 9961;
        font-family: Arial, Helvetica, sans-serif;
        font-size: 12px;
        pointer-events: auto;
      }

      .pt-measure-tab,
      .pt-clear-all-btn {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        gap: 4px;
        width: 82px;
        height: 22px;
        box-sizing: border-box;
        background: rgba(239, 239, 236, 0.97);
        border: 1px solid rgba(108, 108, 98, 0.76);
        border-radius: 5px;
        box-shadow: 0 1px 4px rgba(0,0,0,0.26);
        font-weight: 700;
        font-size: 12px;
        padding: 0 4px;
        cursor: pointer;
        user-select: none;
        color: #222;
        line-height: 20px;
        appearance: none;
        -webkit-appearance: none;
        text-transform: lowercase;
        transition: background-color 0.10s ease, border-color 0.10s ease, box-shadow 0.10s ease, transform 0.05s ease;
      }

      .pt-clear-all-btn {
        font-weight: 700;
        white-space: nowrap;
      }

      .pt-measure-tab:hover,
      .pt-clear-all-btn:hover,
      .pt-clear-all-btn:focus {
        background: rgba(221, 238, 204, 0.98);
        border-color: rgba(66, 102, 47, 0.95);
        box-shadow: 0 1px 6px rgba(0,0,0,0.32);
        outline: none;
      }

      .pt-measure-tab:active,
      .pt-clear-all-btn:active {
        background: rgba(199, 223, 181, 0.98);
        box-shadow: inset 0 1px 3px rgba(0,0,0,0.28);
        transform: translateY(1px);
      }

      .pt-measure-body {
        margin-top: 5px;
        background: rgba(239, 239, 236, 0.97);
        border: 1px solid rgba(108, 108, 98, 0.76);
        border-radius: 6px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.30);
        padding: 8px;
        width: 315px;
        max-width: calc(100vw - 24px);
        box-sizing: border-box;
      }

      .pt-measure-collapsed .pt-measure-body {
        display: none;
      }

      .pt-tools-adddata-wrap {
        position: absolute;
        top: 252px;
        left: 8px;
        width: 340px;
        max-width: calc(100vw - 24px);
        z-index: 9950;
        font-family: Arial, Helvetica, sans-serif;
        font-size: 12px;
        color: #222;
        pointer-events: auto;
      }

      .pt-tools-tab {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        background: rgba(228, 241, 247, 0.97);
        border: 1px solid rgba(72, 117, 145, 0.76);
        border-radius: 5px;
        box-shadow: 0 1px 5px rgba(0,0,0,0.30);
        font-weight: 700;
        padding: 5px 8px;
        cursor: pointer;
        user-select: none;
      }

      .pt-tools-tab-title {
        white-space: nowrap;
      }

      .pt-tools-ribbon-clear {
        border: 1px solid rgba(98, 117, 74, 0.80);
        background: rgba(255, 255, 255, 0.92);
        border-radius: 4px;
        cursor: pointer;
        font-size: 11px;
        font-weight: 400 !important;
        padding: 2px 6px;
        line-height: 16px;
        appearance: none;
        -webkit-appearance: none;
        box-shadow: 0 1px 3px rgba(0,0,0,0.18);
        transition: background-color 0.10s ease, border-color 0.10s ease, box-shadow 0.10s ease, transform 0.05s ease;
      }

      .pt-tools-ribbon-clear:hover,
      .pt-tools-ribbon-clear:focus {
        background: rgba(221, 238, 204, 0.98);
        border-color: rgba(66, 102, 47, 0.95);
        box-shadow: 0 1px 5px rgba(0,0,0,0.28);
        outline: none;
      }

      .pt-tools-ribbon-clear:active {
        background: rgba(199, 223, 181, 0.98);
        box-shadow: inset 0 1px 3px rgba(0,0,0,0.28);
        transform: translateY(1px);
      }

      .pt-tools-body {
        margin-top: 5px;
        background: rgba(228, 241, 247, 0.97);
        border: 1px solid rgba(72, 117, 145, 0.76);
        border-radius: 6px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.30);
        padding: 8px;
        max-height: calc(100vh - 365px);
        overflow-y: auto;
        overscroll-behavior: contain;
        box-sizing: border-box;
      }

      .pt-tools-collapsed .pt-tools-body {
        display: none;
      }

      .pt-tools-section {
        border-top: 1px solid rgba(72, 117, 145, 0.24);
        padding-top: 7px;
        margin-top: 7px;
      }

      .pt-tools-section:first-child {
        border-top: none;
        margin-top: 0;
        padding-top: 0;
      }

      .pt-tools-heading {
        font-weight: 700;
        margin-bottom: 5px;
      }

      .pt-tools-details {
        border-top: 1px solid #ddd;
        padding-top: 7px;
        margin-top: 7px;
      }

      .pt-tools-details > summary {
        font-weight: 700;
        cursor: pointer;
        list-style-position: inside;
        padding: 2px 0;
      }

      .pt-tools-special-details {
        margin: 7px 0 0 0;
        padding: 6px;
        border: 1px solid rgba(72, 117, 145, 0.30);
        border-radius: 5px;
        background: rgba(199, 224, 237, 0.55);
      }

      .pt-tools-special-details > summary {
        padding: 1px 0;
      }

      .pt-tools-special-details .pt-tools-details-body {
        margin-top: 5px;
      }

      .pt-tools-details-body {
        margin-top: 6px;
      }

      .pt-catalog-quick-list {
        margin-top: 6px;
      }

      .pt-catalog-group-controls {
        display: flex;
        gap: 5px;
        align-items: center;
        margin: 5px 0 4px 0;
      }

      .pt-catalog-group {
        border-top: 1px solid #ddd;
        margin-top: 7px;
        padding-top: 5px;
      }

      .pt-catalog-group:first-child {
        border-top: none;
      }

      .pt-catalog-group-title {
        width: 100%;
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 5px;
        border: none;
        border-radius: 4px;
        font-weight: 700;
        font-size: 12px;
        line-height: 1.2;
        color: #222;
        background: rgba(204, 226, 238, 0.62);
        padding: 4px 5px;
        margin: 0 0 4px 0;
        text-align: left;
        cursor: pointer;
      }

      .pt-catalog-group-title:hover {
        background: rgba(218, 236, 245, 0.98);
      }

      .pt-catalog-group-toggle-symbol {
        flex: 0 0 12px;
        color: #222;
        font-weight: 700;
        text-align: center;
      }

      .pt-catalog-group-label {
        flex: 1 1 auto;
        min-width: 0;
      }

      .pt-catalog-group-count {
        flex: 0 0 auto;
        color: #666;
        font-weight: 400;
      }

      .pt-catalog-group-meta,
      .pt-catalog-subgroup-meta {
        display: inline-flex;
        flex: 0 1 auto;
        flex-wrap: wrap;
        align-items: center;
        gap: 2px 4px;
        min-width: 0;
      }

      .pt-catalog-group-active-badge {
        flex: 0 0 auto;
        margin-left: 3px;
        padding: 1px 5px;
        border-radius: 7px;
        background: rgba(46, 125, 50, 0.14);
        color: #1b5e20;
        font-size: 10.5px;
        font-weight: 700;
      }

      .pt-catalog-group-collapsed .pt-catalog-group-rows {
        display: none;
      }

      .pt-catalog-subgroup-title {
        position: relative;
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        justify-content: flex-start;
        gap: 5px;
        width: 100%;
        margin: 8px 2px 3px 0;
        padding: 0 2px 0 4px;
        border: 0;
        background: transparent;
        color: #54636B;
        font-size: 10.5px;
        font-weight: 700;
        letter-spacing: 0.02em;
        text-align: left;
        cursor: pointer;
      }

      .pt-catalog-subgroup-title::after {
        content: '';
        height: 1px;
        flex: 1 1 auto;
        background: rgba(84, 99, 107, 0.24);
      }

      .pt-catalog-subgroup-title:hover {
        color: #32434D;
      }

      .pt-catalog-subgroup-toggle-symbol {
        flex: 0 0 auto;
        width: 11px;
        color: #54636B;
        font-weight: 700;
        text-align: center;
      }

      .pt-catalog-subgroup-label {
        flex: 0 1 auto;
        min-width: 0;
      }

      .pt-catalog-subgroup-count {
        flex: 0 0 auto;
        color: #6B7880;
        font-size: 10px;
        font-weight: 600;
      }

      .pt-catalog-row {
        border: 1px solid rgba(72, 117, 145, 0.18);
        border-radius: 5px;
        background: rgba(255, 255, 255, 0.68);
        margin: 4px 0;
        padding: 4px;
      }


      .pt-catalog-row-active {
        background: rgba(210, 242, 214, 0.72);
        border-color: rgba(46, 125, 50, 0.35);
      }

      .pt-catalog-id-badge {
        display: inline-block;
        margin-right: 4px;
        padding: 0 4px;
        border-radius: 4px;
        background: rgba(0,0,0,0.06);
        color: #555;
        font-size: 10px;
        font-weight: 700;
        line-height: 1.35;
        vertical-align: 1px;
        white-space: nowrap;
        border: 1px solid rgba(0,0,0,0.08);
      }

      .pt-catalog-load-badge.pt-catalog-load-green {
        background: #2E7D32;
        border-color: #1B5E20;
        color: #fff;
      }

      .pt-catalog-load-badge.pt-catalog-load-yellow {
        background: #FFE082;
        border-color: #B7791F;
        color: #5D4200;
      }

      .pt-catalog-load-badge.pt-catalog-load-orange {
        background: #EF6C00;
        border-color: #B24A00;
        color: #fff;
      }

      .pt-catalog-load-badge.pt-catalog-load-red {
        background: #B71C1C;
        border-color: #7F0000;
        color: #fff;
      }

      .pt-catalog-load-badge.pt-catalog-load-gray {
        background: #757575;
        border-color: #4D4D4D;
        color: #fff;
      }

      .pt-catalog-load-chip {
        display: inline-block;
        padding: 1px 5px;
        border-radius: 7px;
        font-size: 10.5px;
        font-weight: 700;
        border: 1px solid rgba(0,0,0,0.12);
      }

      .pt-catalog-load-chip.pt-catalog-load-green { background: rgba(46,125,50,0.14); color: #1B5E20; }
      .pt-catalog-load-chip.pt-catalog-load-yellow { background: rgba(255,224,130,0.65); color: #5D4200; }
      .pt-catalog-load-chip.pt-catalog-load-orange { background: rgba(239,108,0,0.16); color: #A04000; }
      .pt-catalog-load-chip.pt-catalog-load-red { background: rgba(183,28,28,0.14); color: #8B0000; }
      .pt-catalog-load-chip.pt-catalog-load-gray { background: rgba(117,117,117,0.14); color: #4D4D4D; }

      .pt-catalog-load-key {
        margin-top: 2px;
        line-height: 1.25;
        font-size: 10.5px;
      }

      .pt-load-key-token {
        margin-right: 3px;
        font-size: 10px;
        line-height: 1.35;
        vertical-align: 0;
      }

      .pt-catalog-capability-key {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 2px 5px;
        margin-top: 3px;
        font-size: 10.5px;
        line-height: 1.3;
      }

      .pt-capability-key-item,
      .pt-capability-badges {
        display: inline-flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 2px;
        min-width: 0;
      }

      .pt-capability-key-separator {
        color: #879198;
      }

      .pt-capability-badge {
        display: inline-flex;
        flex: 0 0 auto;
        align-items: center;
        padding: 0 4px;
        border: 1px solid;
        border-radius: 4px;
        font-size: 9.5px;
        font-weight: 700;
        line-height: 1.35;
        letter-spacing: 0.015em;
        white-space: nowrap;
      }

      .pt-capability-badge--legend {
        background: #F1E8F5;
        border-color: #B79AC3;
        color: #4F2D5C;
      }

      .pt-capability-badge--info {
        background: #E3F2F0;
        border-color: #8DBAB4;
        color: #174F4A;
      }

      .pt-capability-badge-count {
        font-variant-numeric: tabular-nums;
      }

      .pt-capability-badges--layer {
        flex: 0 1 auto;
      }

      .pt-map-legend-close {
        position: absolute;
        top: 4px;
        right: 6px;
        border: none;
        background: transparent;
        color: #777;
        font-size: 15px;
        line-height: 1;
        font-weight: 700;
        cursor: pointer;
        padding: 1px 4px;
      }

      .pt-map-legend-close:hover {
        color: #222;
        background: rgba(0,0,0,0.06);
        border-radius: 4px;
      }

      .pt-catalog-active-badge {
        display: inline-block;
        margin-left: 6px;
        padding: 1px 5px;
        border-radius: 7px;
        background: rgba(46, 125, 50, 0.14);
        color: #1b5e20;
        font-size: 10.5px;
        font-weight: 700;
        vertical-align: middle;
      }

      .pt-catalog-row-main {
        display: grid;
        grid-template-columns: 28px 1fr auto;
        gap: 5px;
        align-items: start;
      }

      .pt-catalog-row.pt-catalog-loading {
        border-color: #666;
        background: rgba(255, 255, 255, 0.98);
        box-shadow: inset 0 0 0 1px rgba(0,0,0,0.08);
      }

      .pt-catalog-row.pt-catalog-loading .pt-catalog-layer-name::after {
        content: ' loading...';
        color: #555;
        font-weight: 400;
        font-style: italic;
      }

      @keyframes ptCatalogSpin {
        from { transform: rotate(0deg); }
        to { transform: rotate(360deg); }
      }

      .pt-catalog-add-small {
        width: 24px;
        height: 24px;
        display: flex;
        align-items: center;
        justify-content: center;
        line-height: 1;
        text-align: center;
        border: 1px solid #666;
        border-radius: 5px;
        background: #f7f7f7;
        font-weight: 700;
        cursor: pointer;
        padding: 0;
        box-sizing: border-box;
      }

      .pt-catalog-add-small:hover {
        background: #e9e9e9;
      }

      .pt-catalog-add-small:disabled {
        cursor: wait;
        opacity: 0.85;
      }

      .pt-catalog-spinner {
        display: none;
        width: 12px;
        height: 12px;
        margin: 0;
        border: 2px solid rgba(0,0,0,0.22);
        border-top-color: #222;
        border-radius: 50%;
        animation: ptCatalogSpin 0.8s linear infinite;
        box-sizing: border-box;
      }

      .pt-catalog-row.pt-catalog-loading .pt-catalog-add-symbol {
        display: none;
      }

      .pt-catalog-row.pt-catalog-loading .pt-catalog-spinner {
        display: block;
      }

      .pt-catalog-row-text {
        min-width: 0;
      }

      .pt-catalog-layer-name {
        display: flex;
        flex-wrap: wrap;
        align-items: baseline;
        gap: 2px 4px;
        font-weight: 600;
        line-height: 1.2;
      }

      .pt-catalog-layer-label {
        flex: 0 1 auto;
        min-width: 0;
        overflow-wrap: anywhere;
      }

      .pt-catalog-info-btn {
        border: none;
        background: transparent;
        color: #1b6aa5;
        cursor: pointer;
        font-size: 11px;
        padding: 2px 0 2px 4px;
        white-space: nowrap;
      }

      .pt-catalog-detail {
        display: none;
        margin: 5px 0 1px 33px;
        padding: 5px 6px;
        border-left: 3px solid #ddd;
        background: rgba(255,255,255,0.80);
        font-size: 11px;
        line-height: 1.28;
      }

      .pt-catalog-detail-open {
        display: block;
      }

      .pt-catalog-warning {
        color: #8B5A00;
      }

      .pt-tools-row {
        display: flex;
        flex-wrap: wrap;
        gap: 5px;
        margin: 4px 0;
      }

      .pt-tools-btn,
      .pt-tools-mini-btn {
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

      .pt-tools-btn {
        padding: 4px 7px;
      }

      .pt-tools-mini-btn {
        padding: 2px 5px;
        float: right;
      }

      .pt-tools-btn:hover,
      .pt-tools-mini-btn:hover,
      .pt-tools-btn:focus,
      .pt-tools-mini-btn:focus {
        background: rgba(221, 238, 204, 0.98);
        border-color: rgba(66, 102, 47, 0.95);
        box-shadow: 0 1px 5px rgba(0,0,0,0.24);
        outline: none;
      }

      .pt-tools-btn:active,
      .pt-tools-mini-btn:active {
        background: rgba(199, 223, 181, 0.98);
        box-shadow: inset 0 1px 3px rgba(0,0,0,0.22);
        transform: translateY(1px);
      }

      .pt-tools-mini-btn[disabled],
      .pt-tools-mini-btn[aria-disabled='true'] {
        opacity: 0.65;
        cursor: wait;
      }

      .pt-custom-refresh-btn {
        margin-left: 6px;
      }

      .pt-active-layer-action-wrap,
      .pt-catalog-row-action-wrap {
        display: flex;
        justify-content: flex-end;
        align-items: center;
        gap: 6px;
        flex-wrap: wrap;
        margin: 5px 0 2px 0;
      }

      .pt-active-layer-action-wrap .pt-tools-mini-btn,
      .pt-catalog-row-action-wrap .pt-tools-mini-btn {
        float: none;
        margin-left: 0;
      }

      .pt-external-remove-btn,
      .pt-catalog-row-remove-btn {
        font-weight: 600;
        background: #fbfaf5 !important;
        border-color: rgba(82, 72, 52, 0.66) !important;
        color: #1f2526 !important;
      }

      .pt-external-remove-btn:hover,
      .pt-catalog-row-remove-btn:hover {
        background: #dfead1 !important;
        border-color: #546b38 !important;
        color: #17210f !important;
      }

      .pt-external-remove-btn:active,
      .pt-catalog-row-remove-btn:active {
        background: #c8d9b8 !important;
        border-color: #43572d !important;
        color: #17210f !important;
      }

      .pt-catalog-row-refresh-wrap,
      .pt-manual-refresh-wrap {
        margin: 5px 0 2px 0;
      }

      .pt-catalog-row-refresh-btn,
      .pt-manual-refresh-btn {
        margin-left: 0;
      }

      .pt-catalog-row-refreshing,
      .pt-custom-layer-refreshing {
        border-left: 4px solid #5b8bd8;
        background: rgba(91, 139, 216, 0.07);
      }

      .pt-tools-btn:disabled,
      .pt-tools-btn[aria-disabled="true"] {
        opacity: 0.65;
        cursor: progress;
      }

      .pt-tools-btn-working {
        font-style: italic;
      }

      .pt-tools-spinner {
        display: none;
        width: 13px;
        min-width: 13px;
        height: 13px;
        border: 2px solid rgba(0, 0, 0, 0.18);
        border-top-color: rgba(0, 0, 0, 0.72);
        border-radius: 50%;
        animation: ptToolsSpin 0.8s linear infinite;
        align-self: center;
        vertical-align: middle;
        margin-left: 2px;
      }

      @keyframes ptToolsSpin {
        to { transform: rotate(360deg); }
      }

      .pt-tools-url-tools {
        margin: 3px 0 6px 0;
        padding: 4px 5px;
        border-left: 3px solid #bbb;
        background: rgba(0, 0, 0, 0.025);
      }

      .pt-tools-url-helper-btn {
        margin: 0 4px 3px 0;
        font-size: 11px;
        padding: 3px 6px;
      }

      .pt-tools-active-btn {
        background: #222 !important;
        color: #fff !important;
      }

      .pt-tools-wide-btn {
        width: 100%;
      }

      .pt-tools-input,
      .pt-tools-textarea,
      .pt-tools-select {
        width: 100%;
        box-sizing: border-box;
        font-size: 12px;
        margin: 2px 0 5px 0;
        border: 1px solid #aaa;
        border-radius: 3px;
        padding: 4px;
        font-family: Arial, Helvetica, sans-serif;
      }

      .pt-tools-small-label {
        display: block;
        font-weight: 700;
        font-size: 11px;
        margin-top: 4px;
      }

      .pt-tools-check {
        display: block;
        margin: 3px 0;
      }

      .pt-tools-range {
        width: 100%;
      }

      .pt-tools-muted {
        font-size: 11px;
        color: #555;
        line-height: 1.25;
        clear: both;
      }

      .pt-tools-muted code,
      .pt-source-links-panel code {
        background: #f2f2f2;
        border: 1px solid #ddd;
        border-radius: 3px;
        padding: 0 3px;
        font-size: 11px;
      }

      .pt-tools-disabled {
        opacity: 0.55;
      }

      .pt-tools-local-status {
        display: none;
        margin: 4px 0 5px 0;
        padding: 4px 5px;
        border-left: 3px solid currentColor;
        background: rgba(0, 0, 0, 0.035);
        font-size: 11px;
        line-height: 1.25;
      }

      .pt-manual-action-note {
        margin-top: 5px;
        margin-bottom: 6px;
        font-size: 11.5px;
      }

      .pt-tools-status {
        display: none;
        margin: 7px 0 0 0;
        padding: 5px 6px;
        border-left: 3px solid currentColor;
        border-radius: 3px;
        background: rgba(0, 0, 0, 0.035);
        min-height: 14px;
        font-size: 11px;
        line-height: 1.25;
      }

      .pt-tools-status-ok {
        color: #244C1E;
      }

      .pt-tools-status-error {
        color: #8B0000;
      }

      .pt-external-overlay-tooltip {
        font-size: 11px;
        line-height: 1.22;
        min-width: 150px;
        max-width: 420px;
        width: max-content;
        box-sizing: border-box;
        white-space: normal;
        overflow-wrap: break-word;
        word-break: normal;
      }

      .pt-external-overlay-tooltip.pt-mlrs-tooltip {
        min-width: 230px;
        max-width: 390px;
        white-space: normal;
        overflow-wrap: break-word;
        word-break: normal;
      }

      .pt-external-overlay-tooltip.pt-carb-air-tooltip {
        min-width: 0;
        max-width: 300px;
        width: max-content;
        white-space: normal;
        overflow-wrap: break-word;
        word-break: normal;
      }

      .pt-external-overlay-tooltip.pt-airnow-tooltip {
        min-width: 210px;
        max-width: 360px;
        white-space: normal;
        overflow-wrap: break-word;
        word-break: normal;
      }

      .pt-external-overlay-tooltip.pt-carb-air-tooltip .pt-external-hover {
        display: inline-block;
        max-width: 300px;
      }

      .pt-external-overlay-tooltip .pt-external-hover {
        display: block;
        max-width: 420px;
        white-space: normal;
        overflow-wrap: break-word;
        word-break: normal;
      }

      .pt-external-overlay-tooltip .pt-external-hover span {
        color: #555;
      }

      .pt-external-overlay-tooltip .pt-external-hover div {
        margin: 1px 0;
        white-space: normal;
        overflow-wrap: break-word;
        word-break: normal;
      }

      .pt-external-overlay-tooltip.pt-mlrs-tooltip .pt-external-hover div {
        overflow-wrap: break-word;
        word-break: normal;
      }

      .pt-external-overlay-tooltip.pt-carb-air-tooltip .pt-external-hover div {
        overflow-wrap: break-word;
        word-break: normal;
      }

      .pt-external-overlay-tooltip.pt-airnow-tooltip .pt-external-hover div {
        overflow-wrap: break-word;
        word-break: normal;
      }

      .pt-external-generic-leaflet-popup .leaflet-popup-content {
        min-width: 220px;
        max-width: 460px;
        line-height: 1.25;
        overflow-wrap: break-word;
        word-break: normal;
      }

      .pt-external-generic-leaflet-popup .pt-external-popup-table-wrap {
        max-width: 100%;
      }

      .pt-airnow-leaflet-popup .leaflet-popup-content {
        min-width: 245px;
        max-width: 440px;
        line-height: 1.25;
        overflow-wrap: break-word;
        word-break: normal;
      }

      .pt-airnow-leaflet-popup .pt-external-popup-table-wrap {
        max-width: 100%;
      }

      .pt-carb-air-leaflet-popup .leaflet-popup-content {
        min-width: 140px;
        max-width: 380px;
        width: auto !important;
        line-height: 1.25;
        overflow-wrap: break-word;
        word-break: normal;
      }

      .pt-carb-air-leaflet-popup .pt-external-popup-table-wrap {
        max-width: 100%;
      }

      .pt-mlrs-leaflet-popup .leaflet-popup-content {
        min-width: 285px;
        max-width: 520px;
        line-height: 1.25;
        overflow-wrap: break-word;
        word-break: normal;
      }

      .pt-mlrs-leaflet-popup .pt-external-popup-table-wrap {
        max-width: 100%;
      }


      .pt-mlrs-stack-leaflet-popup .leaflet-popup-content {
        min-width: 340px;
        max-width: 640px;
      }

      .pt-mlrs-stack-header {
        margin-bottom: 3px;
        font-size: 12px;
      }

      .pt-mlrs-stack-subnote,
      .pt-mlrs-stack-limit-note,
      .pt-mlrs-record-count-note {
        font-size: 11px;
        color: #56636a;
        line-height: 1.25;
        margin: 3px 0 5px 0;
      }

      .pt-mlrs-stack-limit-note {
        color: #8a5a00;
      }

      .pt-mlrs-popup-tabs {
        display: flex;
        flex-wrap: wrap;
        gap: 4px;
        margin: 6px 0 7px 0;
        max-height: 92px;
        overflow-y: auto;
        padding: 3px;
        border: 1px solid rgba(80, 95, 105, 0.25);
        border-radius: 5px;
        background: rgba(255, 255, 255, 0.66);
      }

      .pt-mlrs-popup-tab {
        border: 1px solid #9daab2;
        background: #f8fbfc;
        border-radius: 4px;
        padding: 2px 6px;
        font-size: 10.5px;
        line-height: 1.25;
        cursor: pointer;
        max-width: 190px;
        text-align: left;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .pt-mlrs-popup-tab:hover {
        background: #edf5f8;
      }

      .pt-mlrs-popup-tab-active {
        background: #d9edf5;
        border-color: #2b7b9f;
        color: #11384a;
        font-weight: 700;
      }

      .pt-mlrs-popup-record-panel {
        border-top: 1px solid rgba(80, 95, 105, 0.22);
        padding-top: 5px;
      }

      .pt-mlrs-legend-row {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 4px 10px;
      }

      .pt-mlrs-legend-item {
        display: inline-flex;
        align-items: center;
        white-space: nowrap;
        gap: 4px;
        margin-right: 2px;
      }

      .pt-mlrs-legend-swatch {
        display: inline-block;
        width: 13px;
        height: 10px;
        box-sizing: border-box;
        opacity: 0.88;
        flex: 0 0 auto;
      }

      .pt-sgma-legend-row {
        display: flex;
        flex-wrap: nowrap;
        align-items: center;
        gap: 12px;
        margin: 4px 0 5px 0;
      }

      .pt-sgma-legend-item {
        display: inline-flex;
        align-items: center;
        white-space: nowrap;
        gap: 4px;
        margin-right: 0;
        margin-top: 0;
      }

      .pt-sgma-legend-swatch {
        display: inline-block;
        width: 12px;
        height: 10px;
        border: 1px solid rgba(0,0,0,0.35);
        box-sizing: border-box;
        flex: 0 0 auto;
      }

      .pt-sgma-legend-note {
        line-height: 1.2;
      }

      .leaflet-container.pt-external-identify-hit,
      .leaflet-container.pt-external-identify-hit .leaflet-interactive {
        cursor: crosshair !important;
      }

      .pt-native-field-name {
        color: #666;
        font-style: italic;
        font-weight: normal;
      }

      .leaflet-popup-content,
      .leaflet-popup-content *,
      .pt-tools-panel,
      .pt-tools-panel .pt-tools-muted,
      .pt-tools-panel .pt-custom-layer-row {
        -webkit-user-select: text;
        user-select: text;
        cursor: auto;
      }

      .leaflet-popup-content {
        overflow-wrap: anywhere;
        word-break: normal;
      }

      .pt-tools-panel input[type="text"],
      .pt-tools-panel textarea {
        cursor: text;
      }

      .pt-tools-panel button,
      .pt-tools-panel select,
      .pt-tools-panel input[type="checkbox"],
      .pt-tools-panel label {
        cursor: pointer;
      }

      .leaflet-popup-content .pt-mlrs-popup-tab,
      .leaflet-popup-content .pt-mlrs-popup-tab * {
        cursor: pointer !important;
      }

      .pt-active-external-section {
        padding-top: 4px;
        padding-bottom: 5px;
      }

      .pt-active-external-toggle {
        width: 100%;
        display: flex;
        align-items: center;
        gap: 5px;
        padding: 3px 4px;
        border: 0;
        border-radius: 4px;
        background: rgba(226, 240, 246, 0.70);
        color: #1e2b2f;
        font-size: 12px;
        font-weight: 700;
        text-align: left;
        cursor: pointer;
      }

      .pt-active-external-toggle:hover,
      .pt-active-external-toggle:focus {
        background: rgba(213, 234, 244, 0.95);
        outline: none;
      }

      .pt-active-external-caret {
        width: 12px;
        text-align: center;
        font-weight: 700;
      }

      .pt-active-external-count {
        margin-left: auto;
        color: #5b6870;
        font-weight: 600;
      }

      .pt-active-external-has-layers .pt-active-external-count {
        color: #24512b;
      }

      .pt-active-external-body {
        margin-top: 5px;
      }

      .pt-custom-layer-list {
        margin-top: 6px;
      }

      .pt-custom-layer-row {
        border: 1px solid #ddd;
        border-radius: 4px;
        background: #fafafa;
        padding: 5px;
        margin: 4px 0;
      }

      .pt-custom-swatch {
        display: inline-block;
        width: 10px;
        height: 10px;
        border: 1px solid #555;
        margin: 0 4px 0 2px;
        vertical-align: middle;
      }

      .pt-source-links-overlay {
        position: fixed;
        top: 0;
        left: 0;
        width: 100vw;
        height: 100vh;
        background: rgba(0,0,0,0.62);
        z-index: 999999;
        display: flex;
        align-items: stretch;
        justify-content: center;
        box-sizing: border-box;
        padding: 34px;
      }

      .pt-source-links-panel {
        background: #ffffff;
        color: #222;
        width: 100%;
        height: 100%;
        max-width: 1180px;
        border-radius: 10px;
        box-shadow: 0 8px 30px rgba(0,0,0,0.35);
        overflow: auto;
        box-sizing: border-box;
        padding: 28px 34px;
        font-family: Arial, sans-serif;
        font-size: 14px;
        line-height: 1.45;
      }

      .pt-source-links-close {
        position: sticky;
        top: 0;
        float: right;
        z-index: 2;
        padding: 8px 14px;
        border: 1px solid #777;
        border-radius: 6px;
        background: #f7f7f7;
        cursor: pointer;
      }

      .pt-layer-explorer-entry {
        background: rgba(255, 255, 255, 0.45);
        border-radius: 5px;
        padding: 7px;
      }

      .pt-layer-explorer-open {
        width: 100%;
        margin-bottom: 5px;
        font-weight: 700;
      }

      .pt-layer-explorer-panel {
        display: flex;
        flex-direction: column;
        overflow: hidden;
      }

      .pt-layer-explorer-close {
        align-self: flex-end;
        flex: 0 0 auto;
        width: auto;
      }

      .pt-layer-explorer-content {
        display: flex;
        flex: 1 1 auto;
        flex-direction: column;
        min-height: 0;
      }

      .pt-layer-explorer-content h1 {
        margin: 0 80px 4px 0;
      }

      .pt-layer-explorer-notice {
        margin: 4px 0 6px 0;
        border: 1px solid #b78218;
        border-radius: 6px;
        background: #fff4cf;
        color: #5b3b00;
        padding: 8px 10px;
        font-weight: 700;
      }

      .pt-layer-explorer-isolation {
        margin: 0 0 10px 0;
        color: #4c5860;
        font-size: 13px;
      }

      .pt-layer-explorer-controls {
        display: grid;
        grid-template-columns: minmax(240px, 1fr) minmax(180px, 0.45fr);
        gap: 10px;
        margin-bottom: 7px;
      }

      .pt-layer-explorer-control label {
        display: block;
        margin-bottom: 3px;
        color: #263943;
        font-size: 12px;
        font-weight: 700;
      }

      .pt-layer-explorer-control .pt-tools-input,
      .pt-layer-explorer-control .pt-tools-select {
        width: 100%;
        margin: 0;
        font-size: 14px;
      }

      .pt-layer-explorer-count {
        margin-bottom: 6px;
        color: #4d5a61;
        font-size: 12px;
        font-weight: 700;
      }

      .pt-layer-explorer-layout {
        display: grid;
        flex: 1 1 auto;
        grid-template-columns: minmax(260px, 0.85fr) minmax(340px, 1.15fr);
        gap: 12px;
        min-height: 0;
      }

      .pt-layer-explorer-list,
      .pt-layer-explorer-detail {
        min-height: 0;
        overflow: auto;
        border: 1px solid #c7d2d8;
        border-radius: 7px;
        background: #f8fafb;
        padding: 8px;
      }

      .pt-layer-explorer-record {
        display: block;
        width: 100%;
        margin: 0 0 6px 0;
        border: 1px solid #b8c9d2;
        border-radius: 5px;
        background: #fff;
        color: #1f3039;
        padding: 8px 9px;
        text-align: left;
        cursor: pointer;
      }

      .pt-layer-explorer-record:hover,
      .pt-layer-explorer-record:focus {
        border-color: #37708d;
        background: #edf7fb;
        outline: none;
      }

      .pt-layer-explorer-record-selected {
        border-color: #245c78;
        background: #dceff8;
        box-shadow: inset 3px 0 0 #245c78;
      }

      .pt-layer-explorer-record-name,
      .pt-layer-explorer-record-id {
        display: block;
      }

      .pt-layer-explorer-record-name {
        font-weight: 700;
      }

      .pt-layer-explorer-record-id {
        margin-top: 2px;
        color: #5d6b72;
        font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace;
        font-size: 11px;
      }

      .pt-layer-explorer-detail {
        background: #fff;
        padding: 14px 16px;
      }

      .pt-layer-explorer-detail-title {
        margin: 0 0 12px 0;
        color: #1f3039;
        font-size: 20px;
        line-height: 1.25;
      }

      .pt-layer-explorer-detail-list {
        display: grid;
        grid-template-columns: minmax(130px, 0.42fr) minmax(180px, 1fr);
        gap: 0;
        margin: 0;
        border-top: 1px solid #d8e0e4;
      }

      .pt-layer-explorer-detail-label,
      .pt-layer-explorer-detail-value {
        margin: 0;
        border-bottom: 1px solid #d8e0e4;
        padding: 8px 6px;
      }

      .pt-layer-explorer-detail-label {
        color: #40535d;
        font-weight: 700;
      }

      .pt-layer-explorer-detail-value {
        overflow-wrap: anywhere;
      }

      .pt-layer-explorer-detail-boundary {
        margin: 12px 0 0 0;
        color: #435760;
        font-size: 12px;
        font-weight: 700;
      }

      @media (max-width: 720px) {
        .pt-layer-explorer-overlay {
          padding: 10px;
        }

        .pt-layer-explorer-panel {
          overflow: auto;
          padding: 16px;
        }

        .pt-layer-explorer-controls,
        .pt-layer-explorer-layout {
          display: block;
        }

        .pt-layer-explorer-control,
        .pt-layer-explorer-list {
          margin-bottom: 10px;
        }

        .pt-layer-explorer-list {
          max-height: 38vh;
        }

        .pt-layer-explorer-detail-list {
          grid-template-columns: 1fr;
        }

        .pt-layer-explorer-detail-label {
          border-bottom: none;
          padding-bottom: 0;
        }
      }
    `;

    document.head.appendChild(style);
  }

  function ptUpdatePopupSupportUI() {
    var typeInput = document.getElementById('pt-custom-type');
    var urlInput = document.getElementById('pt-custom-url');
    var clickableInput = document.getElementById('pt-custom-clickable');
    var clickableLabel = document.getElementById('pt-custom-clickable-label');
    var help = document.getElementById('pt-custom-popup-help');

    if (!typeInput || !clickableInput) return;

    var selectedType = typeInput.value || 'auto';
    var url = urlInput ? urlInput.value : '';
    var detectedType = ptDetectServiceType(url, selectedType);
    var currentViewInput = document.getElementById('pt-custom-current-view');
    var currentViewOnly = currentViewInput ? currentViewInput.checked : false;
    var isMapServer = detectedType === 'map';
    var isImageServer = detectedType === 'image';
    var dynamicMapServer = isMapServer && !currentViewOnly;
    var visualOnlyRaster = isImageServer;

    clickableInput.disabled = dynamicMapServer || visualOnlyRaster;

    if (dynamicMapServer || visualOnlyRaster) {
      clickableInput.checked = false;
    }

    if (clickableLabel) {
      clickableLabel.classList.toggle('pt-tools-disabled', dynamicMapServer || visualOnlyRaster);
    }

    if (help) {
      if (visualOnlyRaster) {
        help.textContent = 'ImageServer layers are visual raster overlays in this first implementation; popups are not enabled.';
      } else if (dynamicMapServer) {
        help.textContent = 'Dynamic MapServer layers are visual overlays. Curated catalog rows can use map identify for hover/click when configured.';
      } else if (isMapServer && currentViewOnly) {
        help.textContent = 'Current-view MapServer snapshots are drawn as local features; popups are supported when attributes are returned.';
      } else {
        help.textContent = 'Popups are supported for FeatureServer, GeoJSON, and current-view MapServer snapshots when attributes are returned.';
      }
    }
  }

  function ptUpdateLoadModeUI() {
    var typeInput = document.getElementById('pt-custom-type');
    var urlInput = document.getElementById('pt-custom-url');
    var currentViewInput = document.getElementById('pt-custom-current-view');
    var currentViewLabel = document.getElementById('pt-custom-current-view-label');
    var whereInput = document.getElementById('pt-custom-where');
    var help = document.getElementById('pt-custom-load-help');
    var whereHelp = document.getElementById('pt-custom-where-help');

    if (!typeInput || !currentViewInput) return;

    var selectedType = typeInput.value || 'auto';
    var url = urlInput ? urlInput.value : '';
    var detectedType = ptDetectServiceType(url, selectedType);
    var autoUnresolved = selectedType === 'auto' &&
      (detectedType === 'unknown' || detectedType === 'hub');
    var isFeatureServer = detectedType === 'feature' || autoUnresolved;
    var isMapServer = detectedType === 'map';
    var isImageServer = detectedType === 'image';
    var allowsCurrentView = isFeatureServer || isMapServer;
    var currentViewOnly = currentViewInput.checked;
    var urlTools = document.getElementById('pt-custom-url-tools');
    var tryZeroBtn = document.getElementById('pt-custom-try-zero-btn');
    var openServiceBtn = document.getElementById('pt-custom-open-service-btn');
    var urlHelperNote = document.getElementById('pt-custom-url-helper-note');
    var parentArcgisServiceUrl = ptIsParentArcgisServiceUrl(url);
    var arcgisRestUrl = ptIsArcgisRestLayerOrServiceUrl(url);

    if (urlTools) {
      urlTools.style.display = arcgisRestUrl || parentArcgisServiceUrl ? 'block' : 'none';
    }

    if (tryZeroBtn) {
      tryZeroBtn.style.display = parentArcgisServiceUrl ? 'inline-block' : 'none';
    }

    if (openServiceBtn) {
      openServiceBtn.style.display = arcgisRestUrl || parentArcgisServiceUrl ? 'inline-block' : 'none';
    }

    if (urlHelperNote) {
      if (parentArcgisServiceUrl) {
        urlHelperNote.textContent = 'This looks like a parent ArcGIS service. In current-view mode BRIM will try to auto-select the only mappable layer, or ask you to choose one if there are several.';
      } else if (arcgisRestUrl) {
        urlHelperNote.textContent = 'Open service metadata to review layer IDs, field names, and source details for SQL filters.';
      } else {
        urlHelperNote.textContent = '';
      }
    }

    currentViewInput.disabled = !allowsCurrentView;

    if (!allowsCurrentView) {
      currentViewInput.checked = false;
      currentViewOnly = false;
    }

    var sqlState = ptManualSqlState(
      url,
      selectedType,
      currentViewOnly,
      ptManualSelectedLoadMode(currentViewOnly),
      whereInput ? whereInput.value : ''
    );

    if (whereInput) {
      whereInput.disabled = !sqlState.allowsInput;
    }

    if (currentViewLabel) {
      currentViewLabel.classList.toggle('pt-tools-disabled', !allowsCurrentView);
    }

    if (help) {
      if (isFeatureServer) {
        help.textContent = 'Use current-view loading for large FeatureServer layers. Zoom to the area of interest first. The loaded layer will be a static snapshot.';
      } else if (isMapServer) {
        help.textContent = currentViewOnly ?
          'Current-view MapServer loading queries a specific /MapServer/N sublayer as features inside the current map extent. The loaded layer is a static snapshot.' :
          'Unchecked: MapServer is added as a server-rendered visual overlay. Checked: query the current view as local features when the service supports queries.';
      } else {
        help.textContent = 'Current-view loading applies to FeatureServer layers and queryable MapServer sublayer URLs.';
      }
    }

    if (whereHelp) {
      whereHelp.textContent = 'Optional. Applied when this action creates a new overlay; existing layers are unchanged. Supported for FeatureServer queries and MapServer sublayers. Not used for GeoJSON, ImageServer, or tiled layers.';
    }

    ptUpdateManualAddActionLabel();
  }


  function ptBind(id, eventName, handler) {
    var el0 = document.getElementById(id);
    if (el0) {
      el0.addEventListener(eventName, handler);
    }
  }

  // --------------------------------------------------------------------------
  // Event bindings
  // --------------------------------------------------------------------------

  ptBind('pt-measure-tab', 'click', function(e) {
    e.preventDefault();
    measureWrap.classList.toggle('pt-measure-collapsed');

  });

  ptBind('pt-teaching-markup-tab', 'click', function(e) {
    if (
      e.target && e.target.closest &&
      e.target.closest('input, select, button') &&
      !e.target.closest('#pt-teaching-markup-tab')
    ) {
      return;
    }
    e.preventDefault();
    teachingWrap.classList.toggle('pt-teaching-markup-collapsed');

    var isCollapsed = teachingWrap.classList.contains('pt-teaching-markup-collapsed');
    e.currentTarget.setAttribute('aria-expanded', isCollapsed ? 'false' : 'true');
    if (isCollapsed) {
      // Closing the shared panel should leave either active placement mode.
      ptSetTeachingMarkupMode(false);
      ptSetTeachingLabelPlacementActive(false);
    } else if (ptTeachingToolMode === 'label') {
      ptSetTeachingLabelPlacementActive(true);
    }

    var caret = document.getElementById('pt-teaching-markup-caret');
    if (caret) {
      caret.textContent = isCollapsed ? '▸' : '▾';
    }
  });

  function ptTeachingModeTabKeydown(e) {
    var isHorizontalArrow = e.key === 'ArrowLeft' || e.key === 'ArrowRight';
    if (!isHorizontalArrow && e.key !== 'Home' && e.key !== 'End') return;

    e.preventDefault();
    var nextMode = e.key === 'ArrowLeft' || e.key === 'Home' ? 'draw' : 'label';
    ptSetTeachingToolMode(nextMode);
    var nextTab = document.getElementById('pt-teaching-mode-' + nextMode);
    if (nextTab) nextTab.focus();
  }

  ptBind('pt-teaching-mode-draw', 'click', function() {
    ptSetTeachingToolMode('draw');
  });

  ptBind('pt-teaching-mode-label', 'click', function() {
    ptSetTeachingToolMode('label');
  });

  ptBind('pt-teaching-mode-draw', 'keydown', ptTeachingModeTabKeydown);
  ptBind('pt-teaching-mode-label', 'keydown', ptTeachingModeTabKeydown);

  ptBind('pt-teaching-markup-toggle', 'change', function(e) {
    ptSetTeachingMarkupMode(e.target.checked);
  });

  ptBind('pt-teaching-markup-color', 'change', ptTeachingUpdateStyleFromControls);
  ptBind('pt-teaching-markup-size', 'change', ptTeachingUpdateStyleFromControls);
  ptBind('pt-teaching-markup-opacity', 'change', ptTeachingUpdateStyleFromControls);
  ptBind('pt-teaching-label-style', 'change', ptTeachingUpdateLabelControls);
  ptBind('pt-teaching-label-size', 'change', ptTeachingUpdateLabelControls);
  ptBind('pt-teaching-label-color', 'change', ptTeachingUpdateLabelControls);

  ptBind('pt-teaching-markup-undo-btn', 'click', function(e) {
    e.preventDefault();
    e.stopPropagation();
    ptTeachingUndoLastStroke();
  });

  ptBind('pt-teaching-markup-clear-btn', 'click', function(e) {
    e.preventDefault();
    e.stopPropagation();
    ptTeachingClearMarkup();
  });

  ptInstallTeachingMarkupPointerHandlers();
  ptTeachingSyncControl();
  ptTeachingSyncToolMode();

  ptBind('pt-clear-all-btn', 'click', function(e) {
    e.preventDefault();
    e.stopPropagation();
    ptClearAllPt2SessionLayers();
  });

  ptBind('pt-tools-tab', 'click', function(e) {
    if (e.target && e.target.closest && e.target.closest('#pt-external-clear-ribbon-btn')) {
      return;
    }
    e.preventDefault();
    wrap.classList.toggle('pt-tools-collapsed');

    var caret = document.getElementById('pt-tools-caret');
    if (caret) {
      caret.textContent = wrap.classList.contains('pt-tools-collapsed') ? '▸' : '▾';
    }
  });

  ptBind('pt-external-clear-ribbon-btn', 'click', function(e) {
    e.preventDefault();
    e.stopPropagation();
    ptClearCustomLayers();
  });

  ptBind('pt-measure-distance-btn', 'click', function() {
    ptStartMeasure('distance');
  });

  ptBind('pt-measure-area-btn', 'click', function() {
    ptStartMeasure('area');
  });

  ptBind('pt-measure-finish-btn', 'click', function() {
    ptFinishMeasure();
  });

  ptBind('pt-measure-clear-btn', 'click', function() {
    ptClearMeasurements();
  });

  ptBind('pt-blm-sma-toggle', 'change', function(e) {
    ptToggleBlmSma(e.target.checked);
  });

  ptBind('pt-blm-sma-opacity', 'input', function() {
    ptSetBlmSmaOpacity();
  });

  ptBind('pt-catalog-agency', 'change', function() {
    ptPopulateCatalogControls();
  });

  ptBind('pt-catalog-theme', 'change', function() {
    ptPopulateCatalogControls();
  });

  ptBind('pt-catalog-layer', 'change', function() {
    ptUpdateCatalogInfo();
  });

  ptBind('pt-catalog-use-btn', 'click', function() {
    ptApplyCatalogRecord(false);
  });

  ptBind('pt-catalog-add-btn', 'click', function() {
    ptApplyCatalogRecord(true);
  });

  ptBind('pt-catalog-search', 'input', function() {
    ptRenderQuickCatalog();
  });

  ptBind('pt-catalog-expand-all-btn', 'click', function(e) {
    e.preventDefault();
    e.stopPropagation();
    ptSetVisibleCatalogGroupsExpanded(true);
  });

  ptBind('pt-catalog-collapse-all-btn', 'click', function(e) {
    e.preventDefault();
    e.stopPropagation();
    ptSetVisibleCatalogGroupsExpanded(false);
  });

  ptBind('pt-active-external-toggle', 'click', function(e) {
    e.preventDefault();
    e.stopPropagation();
    ptSetActiveExternalExpanded(!ptActiveExternalExpanded);
  });

  var quickCatalogList = document.getElementById('pt-catalog-quick-list');
  if (quickCatalogList) {
    quickCatalogList.addEventListener('click', function(e) {
      var groupBtn = e.target.closest ? e.target.closest('[data-pt-catalog-group-toggle]') : null;
      var subgroupBtn = e.target.closest ? e.target.closest('[data-pt-catalog-subgroup-toggle]') : null;
      var addBtn = e.target.closest ? e.target.closest('[data-pt-catalog-quick-add]') : null;
      var infoBtn = e.target.closest ? e.target.closest('[data-pt-catalog-info]') : null;
      var formBtn = e.target.closest ? e.target.closest('[data-pt-catalog-load-form]') : null;
      var refreshBtn = e.target.closest ? e.target.closest('[data-pt-custom-refresh]') : null;
      var removeBtn = e.target.closest ? e.target.closest('[data-pt-custom-remove]') : null;
      var loadingCancelBtn = e.target.closest ? e.target.closest('[data-pt-catalog-loading-cancel]') : null;
      var loadingRetryBtn = e.target.closest ? e.target.closest('[data-pt-catalog-loading-retry]') : null;

      if (loadingCancelBtn && quickCatalogList.contains(loadingCancelBtn)) {
        e.preventDefault();
        e.stopPropagation();
        ptCancelCatalogLoading(loadingCancelBtn.getAttribute('data-pt-catalog-loading-cancel'));
        return;
      }

      if (loadingRetryBtn && quickCatalogList.contains(loadingRetryBtn)) {
        e.preventDefault();
        e.stopPropagation();
        ptRetryCatalogLoading(loadingRetryBtn.getAttribute('data-pt-catalog-loading-retry'));
        return;
      }

      if (refreshBtn && quickCatalogList.contains(refreshBtn)) {
        e.preventDefault();
        e.stopPropagation();
        var refreshId = refreshBtn.getAttribute('data-pt-custom-refresh');
        var refreshRec = ptFindCustomLayerById(refreshId);
        if (refreshRec && refreshRec.catalogIndex !== null &&
            ptCatalogLoadingIdx !== null &&
            Number(refreshRec.catalogIndex) === Number(ptCatalogLoadingIdx)) {
          ptClearCatalogLoading(true);
        }
        ptRefreshCustomLayer(refreshId);
        return;
      }

      if (removeBtn && quickCatalogList.contains(removeBtn)) {
        e.preventDefault();
        e.stopPropagation();
        var removeId = removeBtn.getAttribute('data-pt-custom-remove');
        var removeRec = ptFindCustomLayerById(removeId);
        if (removeRec && removeRec.catalogIndex !== null &&
            ptCatalogLoadingIdx !== null &&
            Number(removeRec.catalogIndex) === Number(ptCatalogLoadingIdx)) {
          ptMarkCatalogLoadingCancelled(ptCatalogLoadingIdx);
          ptClearCatalogLoading(true);
        }
        ptRemoveCustomLayer(removeId);
        return;
      }

      if (groupBtn && quickCatalogList.contains(groupBtn)) {
        e.preventDefault();
        e.stopPropagation();
        var groupKey = groupBtn.getAttribute('data-pt-catalog-group-toggle');
        ptCatalogExpandedGroups[groupKey] = !(ptCatalogExpandedGroups[groupKey] === true);
        ptRenderQuickCatalog();
        return;
      }

      if (subgroupBtn && quickCatalogList.contains(subgroupBtn)) {
        e.preventDefault();
        e.stopPropagation();
        var subgroupKey = subgroupBtn.getAttribute('data-pt-catalog-subgroup-toggle');
        ptCatalogExpandedSubgroups[subgroupKey] = !(ptCatalogExpandedSubgroups[subgroupKey] === true);
        ptRenderQuickCatalog();
        return;
      }

      if (addBtn && quickCatalogList.contains(addBtn)) {
        e.preventDefault();
        e.stopPropagation();
        ptAddCatalogQuickIndex(addBtn.getAttribute('data-pt-catalog-quick-add'));
        return;
      }

      if (infoBtn && quickCatalogList.contains(infoBtn)) {
        e.preventDefault();
        e.stopPropagation();
        ptToggleCatalogInfo(infoBtn.getAttribute('data-pt-catalog-info'), infoBtn);
        return;
      }

      if (formBtn && quickCatalogList.contains(formBtn)) {
        e.preventDefault();
        e.stopPropagation();
        ptLoadCatalogQuickIndexIntoForm(formBtn.getAttribute('data-pt-catalog-load-form'));
        return;
      }
    });
  }

  ptBind('pt-custom-type', 'change', function() {
    ptUpdatePopupSupportUI();
    ptUpdateLoadModeUI();
  });

  ptBind('pt-custom-url', 'input', function() {
    ptHideSublayerChooser();
    ptUpdatePopupSupportUI();
    ptUpdateLoadModeUI();
  });

  ptBind('pt-custom-current-view', 'change', function() {
    ptUpdatePopupSupportUI();
    ptUpdateLoadModeUI();
  });

  ptBind('pt-custom-where', 'input', function() {
    ptUpdateManualAddActionLabel();
  });

  ptBind('pt-custom-try-zero-btn', 'click', function(e) {
    e.preventDefault();
    var urlInput = document.getElementById('pt-custom-url');
    if (!urlInput) return;

    var url = ptNormalizeUrl(urlInput.value);
    if (ptIsParentArcgisServiceUrl(url)) {
      urlInput.value = url + '/0';
      ptHideSublayerChooser();
      ptUpdatePopupSupportUI();
      ptUpdateLoadModeUI();
      ptSetInlineNote('pt-manual-action-note', 'Updated URL to /0. Review the source metadata if this service has multiple layers.', false);
    }
  });

  ptBind('pt-custom-use-sublayer-btn', 'click', function(e) {
    e.preventDefault();
    ptUseSelectedSublayerUrl();
  });

  ptBind('pt-custom-open-service-btn', 'click', function(e) {
    e.preventDefault();
    var urlInput = document.getElementById('pt-custom-url');
    var url = ptNormalizeUrl(urlInput ? urlInput.value : '');

    if (!url) {
      ptSetInlineNote('pt-manual-action-note', 'Paste a service URL first.', true);
      return;
    }

    if (ptIsArcgisRestLayerOrServiceUrl(url) || ptIsParentMapServerUrl(url)) {
      window.open(ptArcgisParentServiceUrl(url), '_blank', 'noopener');
    } else {
      ptSetInlineNote('pt-manual-action-note', 'This helper is for ArcGIS REST MapServer/FeatureServer URLs.', true);
    }
  });

  ptBind('pt-custom-add-btn', 'click', function() {
    ptSetManualAddLoading(true, 'Adding manual overlay…');

    // Defer the actual work very slightly so the disabled button, text change,
    // local status note, and spinner can paint before synchronous validation or
    // fast service errors return.
    window.setTimeout(function() {
      ptHandleAddCustomLayer('pt-manual-action-note');
    }, 75);
  });

  var manualRefreshWrap = document.getElementById('pt-manual-refresh-wrap');
  if (manualRefreshWrap) {
    manualRefreshWrap.addEventListener('click', function(e) {
      var refreshBtn = e.target.closest ? e.target.closest('[data-pt-custom-refresh]') : null;
      if (refreshBtn && manualRefreshWrap.contains(refreshBtn)) {
        e.preventDefault();
        e.stopPropagation();
        ptRefreshCustomLayer(refreshBtn.getAttribute('data-pt-custom-refresh'));
      }
    });
  }

  ptBind('pt-custom-clear-btn', 'click', function() {
    ptClearCustomLayers();
  });

  ptBind('pt-source-links-btn', 'click', function() {
    ptOpenSourceLinksModal();
  });

  var customLayerList = document.getElementById('pt-custom-layer-list');
  if (customLayerList) {
    customLayerList.addEventListener('change', function(e) {
      var id = e.target.getAttribute('data-pt-custom-toggle');
      if (id) {
        ptToggleCustomLayer(id, e.target.checked);
      }
    });

    customLayerList.addEventListener('click', function(e) {
      var uicLabelBtn = e.target.closest ? e.target.closest('[data-pt-uic-label-toggle]') : null;
      if (uicLabelBtn && customLayerList.contains(uicLabelBtn)) {
        e.preventDefault();
        e.stopPropagation();
        var uicLabelRec = ptFindCustomLayerById(
          uicLabelBtn.getAttribute('data-pt-uic-label-toggle')
        );
        if (uicLabelRec && window.BRIM && window.BRIM.uicExplorer &&
            window.BRIM.uicExplorer.toggleExternalLabels) {
          window.BRIM.uicExplorer.toggleExternalLabels(
            uicLabelRec.catalogExtId || uicLabelRec.id
          );
        }
        return;
      }

      var refreshBtn = e.target.closest ? e.target.closest('[data-pt-custom-refresh]') : null;
      if (refreshBtn && customLayerList.contains(refreshBtn)) {
        ptRefreshCustomLayer(refreshBtn.getAttribute('data-pt-custom-refresh'));
        return;
      }

      var removeBtn = e.target.closest ? e.target.closest('[data-pt-custom-remove]') : null;
      if (removeBtn && customLayerList.contains(removeBtn)) {
        ptRemoveCustomLayer(removeBtn.getAttribute('data-pt-custom-remove'));
      }
    });
  }

  window.BRIM = window.BRIM || {};
  window.BRIM.uicExternalBridge = {
    refreshBySourceKey: function(sourceKey) {
      var extId = {
        uic_epa_live: 'UIC_EPA_LIVE',
        uic_calgem_post_live: 'UIC_CALGEM_POST_LIVE',
        uic_calgem_primacy_live: 'UIC_CALGEM_PRIMACY_LIVE',
        uic_epa_reference_points: 'UIC_EPA_REFERENCE_POINTS'
      }[String(sourceKey || '')] || String(sourceKey || '');
      var rec = null;
      ptCustomLayers.forEach(function(item) {
        if (item && item.catalogExtId === extId) rec = item;
      });
      if (rec) ptRefreshCustomLayer(rec.id);
    },
    removeBySourceKey: function(sourceKey) {
      var extId = {
        uic_epa_live: 'UIC_EPA_LIVE',
        uic_calgem_post_live: 'UIC_CALGEM_POST_LIVE',
        uic_calgem_primacy_live: 'UIC_CALGEM_PRIMACY_LIVE',
        uic_epa_reference_points: 'UIC_EPA_REFERENCE_POINTS'
      }[String(sourceKey || '')] || String(sourceKey || '');
      var rec = null;
      ptCustomLayers.forEach(function(item) {
        if (item && item.catalogExtId === extId) rec = item;
      });
      if (rec) ptRemoveCustomLayer(rec.id);
    }
  };


  // --------------------------------------------------------------------------
  // Ops Live bridge for promoted External Layers catalog rows
  // --------------------------------------------------------------------------
  function ptFindCatalogRecordIndexByDisplayName(displayName) {
    displayName = ptCleanText(displayName);
    if (!displayName) return -1;

    for (var i = 0; i < PT2_CATALOG.length; i++) {
      if (ptCatalogField(PT2_CATALOG[i], 'display_name') === displayName) {
        return i;
      }
    }

    return -1;
  }

  function ptRemoveOpsPromotedLayerByKey(opsKey) {
    opsKey = ptCleanText(opsKey);
    if (!opsKey) return 0;

    ptOpsPromotedCancelled[opsKey] = true;
    ptOpsPromotedGeneration[opsKey] = (ptOpsPromotedGeneration[opsKey] || 0) + 1;

    var removed = 0;
    var keep = [];

    ptCustomLayers.forEach(function(rec) {
      if (rec && rec.opsPromotedKey === opsKey) {
        removed += 1;
        if (rec.layer && map.hasLayer(rec.layer)) {
          try { map.removeLayer(rec.layer); } catch(e) {}
        }
      } else {
        keep.push(rec);
      }
    });

    ptCustomLayers = keep;

    ptRenderCustomLayerList();
    ptRenderQuickCatalog();
    ptUpdateManualRefreshControl();

    if (ptCustomLayers.length === 0) {
      ptRestoreExternalBaseAttribution();
      ptHideVisualIdentifyTooltip();
    }

    return removed;
  }

  function ptOptionsFromCatalogRecordForOps(rec, idx, opsName, opsKey) {
    rec = rec || {};

    return {
      legendUrl: ptCatalogField(rec, 'legend_url'),
      legendNote: ptCatalogField(rec, 'legend_note'),
      legendAdapter: ptCatalogField(rec, 'legend_adapter'),
      infoAdapter: ptCatalogField(rec, 'info_adapter'),
      minZoomLive: ptCatalogField(rec, 'min_zoom_live'),
      minZoomCurrentView: ptCatalogField(rec, 'min_zoom_current_view'),
      whereClause: ptCatalogField(rec, 'where_clause'),
      loadMode: ptCatalogField(rec, 'default_load_mode').toLowerCase(),
      popupFields: ptCatalogField(rec, 'popup_fields'),
      popupAliases: ptCatalogField(rec, 'popup_aliases'),
      popupLinkTemplate: ptCatalogField(rec, 'popup_link_template'),
      popupLinkLabel: ptCatalogField(rec, 'popup_link_label'),
      identifyUrl: ptCatalogField(rec, 'identify_url'),
      hoverFields: ptCatalogField(rec, 'hover_fields'),
      hoverAliases: ptCatalogField(rec, 'hover_aliases'),
      hoverBoldFields: ptCatalogField(rec, 'hover_bold_fields'),
      hoverNoLabelFields: ptCatalogField(rec, 'hover_no_label_fields'),
      hoverRoundFields: ptCatalogField(rec, 'hover_round_fields'),
      hoverShowNativeFieldNames: ptCatalogField(rec, 'hover_show_native_field_names'),
      defaultLabelField: ptCatalogField(rec, 'default_label_field'),
      outFields: ptCatalogField(rec, 'out_fields'),
      styleFieldCandidates: ptCatalogField(rec, 'style_field_candidates'),
      defaultStyleField: ptCatalogField(rec, 'default_style_field'),
      defaultStyleMethod: ptCatalogField(rec, 'default_style_method'),
      styleUnits: ptCatalogField(rec, 'style_units'),
      styleLegendTitle: ptCatalogField(rec, 'style_legend_title'),
      styleDirection: ptCatalogField(rec, 'style_direction'),
      layerName: opsName,
      fieldCurationNotes: ptCatalogField(rec, 'field_curation_notes'),
      showNativeFieldNames: ptCatalogField(rec, 'show_native_field_names'),
      clickable: ptTruth(rec.default_clickable) && ptTruth(rec.supports_popups),
      catalogIndex: idx,
      catalogExtId: ptCatalogField(rec, 'external_layer_id'),
      catalogDisplayNum: ptCatalogField(rec, 'external_display_num'),
      loadBadge: ptCatalogField(rec, 'load_badge'),
      loadBadgeOverride: ptCatalogField(rec, 'load_badge_override'),
      loadScore: ptCatalogField(rec, 'load_score'),
      loadScoreOverride: ptCatalogField(rec, 'load_score_override'),
      loadNote: ptCatalogField(rec, 'load_note'),
      loadAuditBasis: ptCatalogField(rec, 'load_audit_basis'),
      loadNTests: ptCatalogField(rec, 'load_n_tests'),
      loadSuccessRate: ptCatalogField(rec, 'load_success_rate'),
      loadMedianSeconds: ptCatalogField(rec, 'load_median_seconds'),
      loadP90Seconds: ptCatalogField(rec, 'load_p90_seconds'),
      loadMaxSeconds: ptCatalogField(rec, 'load_max_seconds'),
      loadFeatureCapRate: ptCatalogField(rec, 'load_feature_cap_rate'),
      catalogKey: ptCatalogRecordKey(rec),
      opsPromotedKey: opsKey,
      opsPromotedGeneration: ptOpsPromotedGeneration[opsKey] || 0,
      opsPromotedSourceName: ptCatalogField(rec, 'display_name'),
      opsPromotedDisplayName: opsName
    };
  }

  function ptOpsAddCatalogLayer(request) {
    request = request || {};

    var sourceName = ptCleanText(request.sourceDisplayName || request.displayName);
    var opsName = ptCleanText(request.opsDisplayName || sourceName);
    var opsKey = ptCleanText(request.opsKey || opsName || sourceName);
    var color = ptCleanText(request.color) || '#2C7FB8';

    if (!sourceName || !opsName || !opsKey) {
      return { ok: false, message: 'Ops catalog request is missing a source name, display name, or key.' };
    }

    var idx = ptFindCatalogRecordIndexByDisplayName(sourceName);
    if (idx < 0) {
      return { ok: false, message: 'Catalog row not found: ' + sourceName };
    }

    var rec = PT2_CATALOG[idx];

    if (ptCatalogPrimaryPanel(rec) === 'disabled') {
      return {
        ok: false,
        message: 'Catalog row is disabled for this map build: ' + sourceName
      };
    }

    var url = ptNormalizeUrl(ptCatalogField(rec, 'service_url'));
    var serviceType = ptCatalogField(rec, 'service_type').toLowerCase();
    var detectedType = ptDetectServiceType(url, serviceType || 'auto');
    var loadMode = ptCatalogField(rec, 'default_load_mode').toLowerCase();
    var currentViewOnly = loadMode === 'current_view';
    var whereClause = ptCatalogField(rec, 'where_clause');
    var clickable = ptTruth(rec.default_clickable) && ptTruth(rec.supports_popups);
    var options = ptOptionsFromCatalogRecordForOps(rec, idx, opsName, opsKey);

    // Avoid duplicate promoted layers if the user clicks rapidly or toggles
    // off/on.  Ordinary External Layers remain untouched because only matching
    // opsPromotedKey records are removed.
    ptRemoveOpsPromotedLayerByKey(opsKey);
    ptOpsPromotedCancelled[opsKey] = false;
    ptOpsPromotedGeneration[opsKey] = (ptOpsPromotedGeneration[opsKey] || 0) + 1;
    options.opsPromotedGeneration = ptOpsPromotedGeneration[opsKey];

    if (!url) {
      return { ok: false, message: 'Catalog row has no service URL: ' + sourceName };
    }

    var problem = ptExplainUrlProblem(url, detectedType);
    if (problem) {
      return { ok: false, message: problem };
    }

    if (detectedType === 'feature') {
      if (currentViewOnly) {
        if (!ptZoomCheck(options.minZoomCurrentView, opsName, 'pt-tools-status')) {
          return { ok: false, message: 'Zoom in before loading ' + opsName + '.' };
        }
        ptAddArcgisFeatureLayerCurrentView(opsName, url, clickable, color, whereClause, options);
      } else {
        if (!ptZoomCheck(options.minZoomLive, opsName, 'pt-tools-status')) {
          return { ok: false, message: 'Zoom in before loading ' + opsName + '.' };
        }
        ptAddArcgisFeatureLayer(opsName, url, clickable, color, whereClause, options);
      }
    } else if (detectedType === 'map') {
      if (currentViewOnly) {
        if (!ptZoomCheck(options.minZoomCurrentView, opsName, 'pt-tools-status')) {
          return { ok: false, message: 'Zoom in before loading ' + opsName + '.' };
        }
        ptAddArcgisMapLayerCurrentView(opsName, url, clickable, color, whereClause, options);
      } else if (loadMode === 'tiled') {
        ptAddArcgisTiledMapLayer(opsName, url, clickable, color, options);
      } else {
        if (!ptZoomCheck(options.minZoomLive, opsName, 'pt-tools-status')) {
          return { ok: false, message: 'Zoom in before loading ' + opsName + '.' };
        }
        ptAddArcgisMapLayer(opsName, url, clickable, color, options);
      }
    } else if (detectedType === 'geojson') {
      ptAddGeoJsonLayer(opsName, url, clickable, color, options);
    } else {
      return { ok: false, message: 'Unsupported catalog service type for Ops promotion: ' + detectedType };
    }

    return {
      ok: true,
      message: 'Requested ' + opsName + ' from curated External Layers catalog.',
      sourceDisplayName: sourceName,
      opsDisplayName: opsName,
      opsKey: opsKey
    };
  }

  // Expose a narrow bridge for Ops Live.  The implementation remains in the
  // External Layers helper so Ops-promoted layers reuse the same curated URL,
  // hover, popup, field alias, and special styling logic as the External panel.
  window.ptOpsExternalCatalogBridge = {
    addLayer: ptOpsAddCatalogLayer,
    primaryPanelForDisplayName: function(displayName) {
      var idx = ptFindCatalogRecordIndexByDisplayName(displayName);
      if (idx < 0) return '';
      return ptCatalogPrimaryPanel(PT2_CATALOG[idx]);
    },
    isEnabledForDisplayName: function(displayName) {
      var idx = ptFindCatalogRecordIndexByDisplayName(displayName);
      if (idx < 0) return false;
      return ptCatalogPrimaryPanel(PT2_CATALOG[idx]) !== 'disabled';
    },
    refreshLayer: function(request) {
      request = request || {};
      var key = ptCleanText(request.opsKey || request.opsDisplayName || request.displayName);
      if (!key) return { ok: false, message: 'Ops refresh request is missing a layer key.' };

      var rec = null;
      for (var i = 0; i < ptCustomLayers.length; i++) {
        if (ptCustomLayers[i] && ptCustomLayers[i].opsPromotedKey === key) {
          rec = ptCustomLayers[i];
          break;
        }
      }

      if (!rec) {
        return { ok: false, message: 'Turn this Ops layer on before using rfrsh.' };
      }

      if (!ptIsRefreshableCurrentViewRecord(rec)) {
        return { ok: false, message: 'This promoted layer is not a current-view ArcGIS snapshot.' };
      }

      ptRefreshCustomLayer(rec.id);
      return { ok: true, message: 'Refreshing ' + rec.name + ' using the current map view…' };
    },
    removeLayer: function(request) {
      request = request || {};
      var key = ptCleanText(request.opsKey || request.opsDisplayName || request.displayName);
      var removed = ptRemoveOpsPromotedLayerByKey(key);
      return { ok: true, removed: removed };
    }
  };

  ptPopulateCatalogControls();
  ptRenderQuickCatalog();
  ptRenderCustomLayerList();
  ptUpdatePopupSupportUI();
  ptUpdateLoadModeUI();
  ptSetStatus('Tools ready. Catalog rows: ' + PT2_CATALOG.length + '.', false);
}
