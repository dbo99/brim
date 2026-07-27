function(el, x) {
  // Shared BRIM legend closeout helper.
  //
  // Purpose:
  //   Keep the common "x hides only this legend" behavior in one place while
  //   each layer controller continues to own its own active-layer/filter logic.
  //
  // Contract:
  //   - close button click stops propagation
  //   - caller decides what local state to change in onClose()
  //   - helper hides the legend div by default after onClose()
  //   - wiring is idempotent for a given button node
  window.BRIM = window.BRIM || {};

  if (!window.BRIM.legendCloseout) {
    window.BRIM.legendCloseout = {};
  }

  if (!document.getElementById('brim-detachable-map-card-css')) {
    var detachableStyle = document.createElement('style');
    detachableStyle.id = 'brim-detachable-map-card-css';
    detachableStyle.textContent =
      '.leaflet-control-container>.pt-map-legend-corner-foreground{z-index:10990!important}' +
      '.pt-map-legend-card{z-index:11000!important}' +
      '.pt-map-legend-card.pt-huc-theme-legend,.pt-map-legend-card.pt-ops-map-legend,.pt-map-legend-card.pt-ops-snow-legend-control,.pt-map-legend-card.pt-ops-cocorahs-legend-control,.pt-map-legend-card.pt-ops-usgs-streamflow-card,.pt-map-legend-card.pt-ops-usgs-groundwater-card,.pt-ops-scan-depth-control,.pt-ops-scan-legend-control{z-index:11010!important}' +
      '.pt-map-legend-card.pt-wcr-completed-depth-map-legend{z-index:11020!important}' +
      '.pt-map-legend-card.pt-mlrs-mineral-cases-map-legend{z-index:11021!important}' +
      '.pt-map-legend-card.pt-sgma-prioritization-map-legend{z-index:11022!important}' +
      '.pt-map-legend-card.pt-subsidence-observation-map-legend{z-index:11023!important}' +
      '.pt-map-legend-card.pt-map-card-undocked{z-index:11100!important}' +
      '.pt-map-legend-local{background:rgba(246,239,222,0.96)!important}' +
      '.pt-map-legend-external{background:rgba(225,240,251,0.97)!important}' +
      '.leaflet-control-container>.leaflet-bottom.leaflet-left.pt-map-legend-gap-managed{bottom:var(--pt-map-legend-bottom,4px)!important;top:auto!important;max-height:var(--pt-map-legend-max-height,calc(100% - 8px))!important}' +
      '.leaflet-control-container>.leaflet-bottom.leaflet-left>.pt-map-legend-card:not(.pt-map-card-undocked){margin-left:8px!important;margin-bottom:8px!important}' +
      '.leaflet-control-container>.pt-map-legend-corner-overflow{max-height:calc(100% - 8px)!important;overflow-y:auto!important;overscroll-behavior:contain;pointer-events:auto!important;touch-action:pan-y}' +
      '.leaflet-control-container>.leaflet-top.pt-map-legend-corner-overflow{top:4px!important}' +
      '.leaflet-control-container>.leaflet-bottom.pt-map-legend-corner-overflow{bottom:var(--pt-map-legend-bottom,4px)!important;max-height:var(--pt-map-legend-max-height,calc(100% - 8px))!important}' +
      '.pt-map-card-actions{display:inline-flex;align-items:center;gap:2px;flex:0 0 auto}.pt-map-legend-card .pt-map-card-actions{display:inline-flex!important;align-items:center!important;gap:2px!important;flex:0 0 auto!important}.pt-map-legend-card .pt-map-card-handle>.pt-map-card-actions{margin-left:auto!important}' +
      '.pt-map-legend-card .pt-map-card-actions>button{appearance:none!important;-webkit-appearance:none!important;position:static!important;top:auto!important;right:auto!important;bottom:auto!important;left:auto!important;display:inline-flex!important;align-items:center!important;justify-content:center!important;margin:0!important;transform:none!important;float:none!important;border:0!important;border-radius:0!important;box-shadow:none!important;background:transparent!important;color:#777!important;font:700 16px/1 Arial,Helvetica,sans-serif!important;padding:3px 4px!important;cursor:pointer!important}' +
      '.pt-map-legend-card .pt-map-card-actions>button:hover{border:0!important;box-shadow:none!important;background:transparent!important;color:#333!important}' +
      '.pt-map-legend-card .pt-map-card-actions>button:focus-visible{outline:2px solid #2b6cb0!important;outline-offset:1px!important}' +
      '.pt-map-card-auto-header{display:flex;align-items:flex-start;justify-content:space-between;gap:8px;margin:0 0 4px 0}.pt-map-card-auto-title{font-weight:700}.pt-map-card-auto-header button{position:static!important;top:auto!important;right:auto!important;margin:0!important}' +
      '.pt-map-card-dock{border:0;background:transparent;color:#777;font:bold 15px/1 Arial,Helvetica,sans-serif;padding:0 2px;cursor:pointer}' +
      '.pt-map-card-dock:hover{color:#222}' +
      '.pt-map-card-handle{cursor:default}.pt-map-card-undocked .pt-map-card-handle{cursor:move}' +
      '.pt-wcr-completed-depth-map-legend-head{display:flex;align-items:flex-start;justify-content:space-between;gap:8px;margin-bottom:4px}';
    document.head.appendChild(detachableStyle);
  }

  window.BRIM.legendCloseout.wire = function(div, selector, onClose, options) {
    if (!div || !selector) return null;
    var btn = div.querySelector(selector);
    if (!btn || btn.__brimLegendCloseoutWired) return btn || null;

    options = options || {};
    btn.__brimLegendCloseoutWired = true;

    btn.addEventListener('click', function(e) {
      if (e && e.preventDefault) e.preventDefault();
      if (e && e.stopPropagation) e.stopPropagation();

      if (typeof onClose === 'function') {
        onClose(e, btn, div);
      }

      if (options.hide !== false) {
        div.style.display = 'none';
      }
    }, false);

    return btn;
  };

  window.BRIM.legendCloseout.buttonHtml = function(extraClass, label) {
    extraClass = extraClass || '';
    label = label || 'Hide legend';
    return '<button type="button" class="pt-map-legend-close ' + extraClass +
      '" aria-label="' + label + '" title="' + label + '">&times;</button>';
  };

  window.BRIM.legendCloseout.actionsHtml = function(dockClass, closeClass, label) {
    label = label || 'legend';
    return '<span class="pt-map-card-actions">' +
      '<button type="button" class="pt-map-card-dock ' + (dockClass || '') +
      '" aria-label="Undock ' + label + '" title="Undock ' + label + '">&#x2197;</button>' +
      window.BRIM.legendCloseout.buttonHtml(closeClass || '', 'Hide ' + label) +
      '</span>';
  };

  window.BRIM.legendCloseout.makeDetachable = function(options) {
    options = options || {};
    var card = options.card;
    var map = options.map;
    if (!card || !map || !map.getContainer) return null;
    if (card.classList) card.classList.add('pt-map-legend-card');
    var dockMembers = options.dockMembers && options.dockMembers.length ? Array.prototype.slice.call(options.dockMembers) : [card];
    var coordinated = dockMembers.length !== 1 || dockMembers[0] !== card;
    var state = card.__brimDetachableState;
    if (!state) {
      state = {card: card, map: map, dockCssText: '', dockPlaces: [], floating: false, drag: null, destroyed: false};
      card.__brimDetachableState = state;
    }
    state.dockMembers = dockMembers;
    state.coordinated = coordinated;
    function raiseDockAncestor() {
      dockMembers.forEach(function(node) {
        var ancestor = node ? node.parentElement : null;
        while (ancestor && ancestor !== map.getContainer()) {
          if (ancestor.classList && (ancestor.classList.contains('leaflet-top') || ancestor.classList.contains('leaflet-bottom'))) {
            ancestor.classList.add('pt-map-legend-corner-foreground');
            break;
          }
          ancestor = ancestor.parentElement;
        }
      });
    }
    raiseDockAncestor();
    // Docking is a DOM restoration operation. Do not calculate or normalize a
    // managed dock position; preserve the placement owned by each legend.
    function capturePlace(node) {
      var parent = node ? node.parentNode : null;
      return {
        node: node,
        parent: parent,
        previous: node ? node.previousSibling : null,
        next: node ? node.nextSibling : null,
        index: parent ? Array.prototype.indexOf.call(parent.childNodes, node) : -1,
        cssText: node && node.style ? node.style.cssText : ''
      };
    }
    function restorePlace(place) {
      if (!place || !place.node || !place.parent) return;
      if (place.next && place.next.parentNode === place.parent) {
        place.parent.insertBefore(place.node, place.next);
      } else if (place.previous && place.previous.parentNode === place.parent) {
        place.parent.insertBefore(place.node, place.previous.nextSibling);
      } else {
        place.parent.insertBefore(place.node, place.parent.childNodes[Math.max(0, Math.min(place.index, place.parent.childNodes.length))] || null);
      }
      if (place.node.style) place.node.style.cssText = place.cssText || '';
    }
    function measuredRect() {
      var rects = dockMembers.map(function(node) { return node && node.getBoundingClientRect ? node.getBoundingClientRect() : null; }).filter(function(rect) {
        return rect && (rect.width || rect.height);
      });
      if (!rects.length) return card.getBoundingClientRect();
      return {
        left: Math.min.apply(null, rects.map(function(rect) { return rect.left; })),
        top: Math.min.apply(null, rects.map(function(rect) { return rect.top; })),
        right: Math.max.apply(null, rects.map(function(rect) { return rect.right; })),
        bottom: Math.max.apply(null, rects.map(function(rect) { return rect.bottom; }))
      };
    }
    function clamp(left, top) {
      var mapRect = map.getContainer().getBoundingClientRect();
      var cardRect = card.getBoundingClientRect();
      return {left: Math.max(4, Math.min(left, Math.max(4, mapRect.width - cardRect.width - 4))), top: Math.max(4, Math.min(top, Math.max(4, mapRect.height - cardRect.height - 4)))};
    }
    function setDockButton() {
      var btn = options.dockButton || card.querySelector(options.dockSelector || '.pt-map-card-dock');
      if (!btn) return;
      var text = (state.floating ? 'Dock ' : 'Undock ') + (options.label || 'legend');
      btn.setAttribute('aria-label', text);
      btn.setAttribute('title', text);
      var mode = state.floating ? 'dock' : 'undock';
      if (btn.getAttribute('data-pt-map-card-mode') !== mode) {
        btn.setAttribute('data-pt-map-card-mode', mode);
        btn.innerHTML = state.floating ? '&#x21A9;' : '&#x2197;';
      }
    }
    function undock() {
      if (state.floating || state.destroyed) return;
      var mapContainer = map.getContainer();
      var mapRect = mapContainer.getBoundingClientRect();
      var rect = measuredRect();
      state.dockPlaces = dockMembers.map(capturePlace);
      state.dockCssText = card.style.cssText;
      mapContainer.appendChild(card);
      if (coordinated) dockMembers.forEach(function(node) { card.appendChild(node); });
      card.classList.add('pt-map-card-undocked');
      card.style.setProperty('position', 'absolute', 'important');
      card.style.setProperty('left', (rect.left - mapRect.left) + 'px', 'important');
      card.style.setProperty('top', (rect.top - mapRect.top) + 'px', 'important');
      card.style.setProperty('margin', '0', 'important');
      card.style.setProperty('z-index', '11100', 'important');
      var pos = clamp(rect.left - mapRect.left, rect.top - mapRect.top);
      card.style.setProperty('left', pos.left + 'px', 'important');
      card.style.setProperty('top', pos.top + 'px', 'important');
      state.floating = true;
      setDockButton();
      scheduleResponsiveLegendOverflow();
    }
    function dock() {
      if (!state.floating || state.destroyed) return;
      state.floating = false;
      card.classList.remove('pt-map-card-undocked');
      card.style.cssText = state.dockCssText || '';
      state.dockPlaces.forEach(restorePlace);
      if (coordinated && card.parentNode) card.parentNode.removeChild(card);
      raiseDockAncestor();
      setDockButton();
      scheduleResponsiveLegendOverflow(card);
    }
    function endDrag() {
      state.drag = null;
      document.removeEventListener('pointermove', moveDrag, true);
      document.removeEventListener('pointerup', endDrag, true);
      document.removeEventListener('pointercancel', endDrag, true);
    }
    function moveDrag(e) {
      if (!state.drag) return;
      if (e && e.preventDefault) e.preventDefault();
      var pos = clamp(state.drag.left + e.clientX - state.drag.x, state.drag.top + e.clientY - state.drag.y);
      card.style.setProperty('left', pos.left + 'px', 'important');
      card.style.setProperty('top', pos.top + 'px', 'important');
    }
    function startDrag(e) {
      if (!state.floating || state.destroyed || (e.button !== undefined && e.button !== 0)) return;
      if (e.target && e.target.closest && e.target.closest('button,input,select,a')) return;
      if (e && e.preventDefault) e.preventDefault();
      if (e && e.stopPropagation) e.stopPropagation();
      var mapRect = map.getContainer().getBoundingClientRect();
      var rect = card.getBoundingClientRect();
      state.drag = {x: e.clientX, y: e.clientY, left: rect.left - mapRect.left, top: rect.top - mapRect.top};
      document.addEventListener('pointermove', moveDrag, true);
      document.addEventListener('pointerup', endDrag, true);
      document.addEventListener('pointercancel', endDrag, true);
    }
    function keepInsideViewport() {
      if (!state.floating || state.destroyed) return;
      var left = parseFloat(card.style.left) || 0;
      var top = parseFloat(card.style.top) || 0;
      var pos = clamp(left, top);
      card.style.setProperty('left', pos.left + 'px', 'important');
      card.style.setProperty('top', pos.top + 'px', 'important');
    }
    var dockButton = options.dockButton || card.querySelector(options.dockSelector || '.pt-map-card-dock');
    if (dockButton && !dockButton.__brimDetachWired) {
      dockButton.__brimDetachWired = true;
      state.dockButton = dockButton;
      state.dockClick = function(e) { if (e) { e.preventDefault(); e.stopPropagation(); } if (state.floating) dock(); else undock(); };
      dockButton.addEventListener('click', state.dockClick, false);
    }
    var handle = options.handle || card.querySelector(options.handleSelector || '.pt-map-card-handle');
    if (handle && !handle.__brimDragWired) {
      handle.__brimDragWired = true;
      handle.style.touchAction = 'none';
      handle.addEventListener('pointerdown', startDrag, true);
      state.handle = handle;
      state.startDrag = startDrag;
    }
    setDockButton();
    if (!state.resizeWired && map.on) {
      map.on('resize', keepInsideViewport);
      state.resizeWired = true;
      state.keepInsideViewport = keepInsideViewport;
    }
    state.dock = dock;
    state.undock = undock;
    state.destroy = function(removeCard, skipDock) {
      endDrag();
      if (state.floating && !skipDock) dock();
      if (state.resizeWired && map.off) map.off('resize', state.keepInsideViewport);
      if (state.dockButton && state.dockClick) state.dockButton.removeEventListener('click', state.dockClick, false);
      if (state.handle && state.startDrag) state.handle.removeEventListener('pointerdown', state.startDrag, true);
      state.destroyed = true;
      if (removeCard && card.parentNode) card.parentNode.removeChild(card);
      delete card.__brimDetachableState;
      scheduleResponsiveLegendOverflow();
    };
    return state;
  };

  var map = this;
  var mapContainer = map && map.getContainer ? map.getContainer() : null;
  if (!mapContainer) return;

  var legendCards = [
    {card: '.pt-blm-office-legend', close: '.pt-blm-office-close', handle: '.pt-blm-office-title-row', label: 'BLM office legend'},
    {card: '.pt-conv-panel', close: '.pt-conv-close', handle: '.pt-conv-head', dock: '.pt-conv-float', label: 'Water Conveyance legend'},
    {card: '.pt-calsim3-explorer', close: '.pt-calsim3-close', handle: '.pt-calsim3-head', dock: '.pt-calsim3-dock', label: 'CalSim3.0 Network Explorer'},
    {card: '.pt-huc-theme-legend', close: '.pt-huc-theme-legend-close', parentHandle: true, label: 'HUC thematic-fill legend'},
    {card: '.pt-cnrfc-local-catalog-legend', close: '.pt-cnrfc-local-close', label: 'CNRFC Local catalog legend'},
    {card: '.pt-usgs-gw-local-legend', close: '.pt-usgs-gw-local-close', handle: '.pt-usgs-gw-local-head', dock: '.pt-usgs-gw-local-dock', label: 'USGS groundwater catalog legend'},
    {card: '.pt-usgs-sw-local-legend', close: '.pt-usgs-sw-local-close', label: 'USGS streamgage catalog legend'},
    {card: '.pt-swrcb-pod-legend', close: '.pt-swrcb-pod-close', handle: '.pt-swrcb-pod-title-row', label: 'SWRCB water-rights legend'},
    {card: '.pt-blm-gw-legend', close: '.pt-blm-gw-legend-close', label: 'BLM groundwater-well legend'},
    {card: '.pt-springs-local-legend', close: '.pt-springs-local-close', label: 'Springs legend'},
    {card: '.pt-wsr-ref-legend', close: '.pt-wsr-close', handle: '.pt-wsr-head', label: 'Wild and Scenic Rivers legend'},
    {card: '.pt-wcr-completed-depth-map-legend', close: '.pt-wcr-completed-depth-map-legend-close', handle: '.pt-wcr-completed-depth-map-legend-head', dock: '.pt-wcr-completed-depth-map-legend-dock', label: 'WCR completed-depth legend'},
    {card: '.pt-mlrs-mineral-cases-map-legend', close: '.pt-mlrs-map-legend-close', label: 'MLRS mineral-cases legend'},
    {card: '.pt-sgma-prioritization-map-legend', close: '.pt-sgma-map-legend-close', label: 'SGMA prioritization legend'},
    {card: '.pt-subsidence-observation-map-legend', close: '.pt-subsidence-observation-map-legend-close', label: 'Subsidence-observation legend'},
    {card: '.pt-uic-explorer', close: '.pt-uic-external-close', handle: '.pt-uic-head', dock: '.pt-uic-external-dock', label: 'External UIC Explorer'},
    {card: '.pt-ops-map-legend', close: '.pt-ops-map-legend-close', handle: '.pt-ops-map-legend-titlebar', label: 'Ops Live map legend'},
    {card: '.pt-ops-usgs-streamflow-card', close: '.pt-ops-usgs-streamflow-filter-close', handle: '.pt-ops-usgs-streamflow-filter-title', dock: '.pt-ops-usgs-streamflow-card-dock', label: 'USGS streamflow Ops Live legend and filters'},
    {card: '.pt-ops-usgs-groundwater-card', close: '.pt-ops-usgs-groundwater-filter-close', handle: '.pt-ops-usgs-groundwater-filter-title', dock: '.pt-ops-usgs-groundwater-card-dock', label: 'USGS groundwater Ops Live legend and filters'},
    {card: '.pt-ops-snow-legend-control', close: '.pt-ops-snow-legend-close', handle: '.pt-ops-snow-titlebar', label: 'Snow-pillow SWE legend'},
    {card: '.pt-ops-cocorahs-legend-control', close: '.pt-ops-cocorahs-legend-close', handle: '.pt-ops-cocorahs-legend-head', label: 'CoCoRaHS precipitation legend'}
  ];

  var responsiveOverflowScheduled = false;
  var responsiveFocusCard = null;

  function isVisibleDockedLegend(card) {
    if (!card || !mapContainer.contains(card) || card.classList.contains('pt-map-card-undocked')) return false;
    if (card.style && card.style.display === 'none') return false;
    return !window.getComputedStyle || window.getComputedStyle(card).display !== 'none';
  }

  function wireOverflowCorner(corner) {
    if (!corner || corner.__brimLegendOverflowWired) return;
    corner.__brimLegendOverflowWired = true;
    function stopMapGesture(e) {
      if (corner.classList.contains('pt-map-legend-corner-overflow') && e && e.stopPropagation) e.stopPropagation();
    }
    corner.addEventListener('wheel', stopMapGesture, {passive: true});
    corner.addEventListener('mousewheel', stopMapGesture, {passive: true});
    corner.addEventListener('DOMMouseScroll', stopMapGesture, {passive: true});
    corner.addEventListener('pointerdown', stopMapGesture, {passive: true});
    corner.addEventListener('mousedown', stopMapGesture, {passive: true});
    corner.addEventListener('touchstart', stopMapGesture, {passive: true});
    corner.addEventListener('touchmove', stopMapGesture, {passive: true});
  }

  function bringLegendHeaderIntoView(card, corner) {
    if (!card || !corner || !corner.classList.contains('pt-map-legend-corner-overflow')) return;
    var header = card.querySelector('.pt-map-card-handle');
    if (!header) return;
    var cornerRect = corner.getBoundingClientRect();
    var headerRect = header.getBoundingClientRect();
    if (headerRect.top < cornerRect.top + 4) {
      corner.scrollTop -= cornerRect.top + 4 - headerRect.top;
    } else if (headerRect.bottom > cornerRect.bottom - 4) {
      corner.scrollTop += headerRect.bottom - (cornerRect.bottom - 4);
    }
  }

  function visibleObstacleRect(id, mapRect) {
    var node = document.getElementById(id);
    if (!node || !mapContainer.contains(node)) return null;
    if (node.style && node.style.display === 'none') return null;
    if (window.getComputedStyle && window.getComputedStyle(node).display === 'none') return null;
    var rect = node.getBoundingClientRect();
    if (!rect || !rect.width || !rect.height) return null;
    if (rect.right <= mapRect.left || rect.left >= mapRect.right || rect.bottom <= mapRect.top || rect.top >= mapRect.bottom) return null;
    return rect;
  }

  function lowerLeftSafeGap(mapRect) {
    var externalRect = visibleObstacleRect('pt-tools-adddata-wrap', mapRect);
    var uploadRect = visibleObstacleRect('pt-local-upload-wrap', mapRect);
    var safeTop = mapRect.top + 8;
    var safeBottom = mapRect.bottom - 8;
    if (externalRect) safeTop = Math.max(safeTop, Math.min(mapRect.bottom, externalRect.bottom + 8));
    if (uploadRect) safeBottom = Math.min(safeBottom, Math.max(mapRect.top, uploadRect.top - 8));
    return {
      top: safeTop,
      bottom: Math.max(safeTop, safeBottom),
      hasMeasuredObstacles: !!(externalRect || uploadRect)
    };
  }

  function setCornerLayoutProperty(corner, name, value) {
    if (!corner || !corner.style) return;
    if (corner.style.getPropertyValue(name) !== value) corner.style.setProperty(name, value);
  }

  function recalculateResponsiveLegendOverflow() {
    responsiveOverflowScheduled = false;
    var mapRect = mapContainer.getBoundingClientRect();
    var mapAvailableHeight = Math.max(0, mapRect.height - 8);
    var focusCard = responsiveFocusCard;
    responsiveFocusCard = null;
    var corners = mapContainer.querySelectorAll('.leaflet-control-container > .leaflet-top, .leaflet-control-container > .leaflet-bottom');

    Array.prototype.forEach.call(corners, function(corner) {
      var previousScrollTop = corner.scrollTop || 0;
      corner.classList.remove('pt-map-legend-corner-overflow');
      corner.scrollTop = 0;

      var allCards = corner.querySelectorAll('.pt-map-legend-card');
      var visibleCards = [];
      Array.prototype.forEach.call(allCards, function(card) {
        var visible = isVisibleDockedLegend(card);
        if (visible) {
          visibleCards.push(card);
          if (card.__brimLegendResponsiveVisible !== true && !focusCard) focusCard = card;
        }
        card.__brimLegendResponsiveVisible = visible;
      });
      if (!visibleCards.length) {
        corner.classList.remove('pt-map-legend-gap-managed');
        setCornerLayoutProperty(corner, '--pt-map-legend-bottom', '');
        setCornerLayoutProperty(corner, '--pt-map-legend-max-height', '');
        return;
      }

      var stackTop = Infinity;
      var stackBottom = -Infinity;
      visibleCards.forEach(function(card) {
        var rect = card.getBoundingClientRect();
        stackTop = Math.min(stackTop, rect.top);
        stackBottom = Math.max(stackBottom, rect.bottom);
      });
      var stackHeight = Math.max(0, stackBottom - stackTop);
      var availableHeight = mapAvailableHeight;
      var targetTop = mapRect.top + 4;
      var targetBottom = mapRect.bottom - 4;
      var isLowerLeft = corner.classList.contains('leaflet-bottom') && corner.classList.contains('leaflet-left');

      if (isLowerLeft) {
        var safeGap = lowerLeftSafeGap(mapRect);
        availableHeight = Math.max(0, safeGap.bottom - safeGap.top);
        targetTop = safeGap.top;
        targetBottom = safeGap.bottom;
        if (safeGap.hasMeasuredObstacles) {
          corner.classList.add('pt-map-legend-gap-managed');
          var bottomOffset = Math.max(0, mapRect.bottom - safeGap.bottom);
          if (stackHeight <= availableHeight) {
            var desiredTop = safeGap.top + ((availableHeight - stackHeight) / 2);
            var computedBottom = window.getComputedStyle ? parseFloat(window.getComputedStyle(corner).bottom) : 0;
            if (!isFinite(computedBottom)) computedBottom = 0;
            bottomOffset = Math.max(0, computedBottom - (desiredTop - stackTop));
          }
          setCornerLayoutProperty(corner, '--pt-map-legend-bottom', bottomOffset + 'px');
          setCornerLayoutProperty(corner, '--pt-map-legend-max-height', Math.max(0, availableHeight) + 'px');
        } else {
          corner.classList.remove('pt-map-legend-gap-managed');
          setCornerLayoutProperty(corner, '--pt-map-legend-bottom', '');
          setCornerLayoutProperty(corner, '--pt-map-legend-max-height', '');
        }
      } else {
        corner.classList.remove('pt-map-legend-gap-managed');
        setCornerLayoutProperty(corner, '--pt-map-legend-bottom', '');
        setCornerLayoutProperty(corner, '--pt-map-legend-max-height', '');
      }

      var needsOverflow = stackHeight > availableHeight ||
        (!isLowerLeft && (stackTop < targetTop || stackBottom > targetBottom));
      if (!needsOverflow) return;

      corner.classList.add('pt-map-legend-corner-overflow');
      wireOverflowCorner(corner);
      corner.scrollTop = previousScrollTop;
      if (focusCard && corner.contains(focusCard)) bringLegendHeaderIntoView(focusCard, corner);
    });
  }

  function scheduleResponsiveLegendOverflow(focusCard) {
    if (focusCard) responsiveFocusCard = focusCard;
    if (responsiveOverflowScheduled || !mapContainer) return;
    responsiveOverflowScheduled = true;
    if (window.requestAnimationFrame) window.requestAnimationFrame(recalculateResponsiveLegendOverflow);
    else setTimeout(recalculateResponsiveLegendOverflow, 0);
  }
  window.BRIM.legendCloseout.scheduleLayout = scheduleResponsiveLegendOverflow;

  function autoHeader(card, closeButton, label) {
    var old = card.querySelector('.pt-map-card-auto-header');
    if (old) return old;
    var header = document.createElement('div');
    header.className = 'pt-map-card-auto-header pt-map-card-handle';
    var title = document.createElement('span');
    title.className = 'pt-map-card-auto-title';
    title.textContent = label;
    var actions = document.createElement('span');
    actions.className = 'pt-map-card-actions';
    header.appendChild(title);
    header.appendChild(actions);
    card.insertBefore(header, card.firstChild);
    actions.appendChild(closeButton);
    return header;
  }

  function compactHeaderActions(handle, dockButton, closeButton) {
    if (!handle || !dockButton || !closeButton) return;
    dockButton.classList.add('pt-map-card-action');
    closeButton.classList.add('pt-map-card-action');
    var actions = closeButton.closest ? closeButton.closest('.pt-map-card-actions') : null;
    if (!actions || !handle.contains(actions)) {
      var parent = closeButton.parentNode;
      if (!parent) return;
      actions = document.createElement('span');
      actions.className = 'pt-map-card-actions';
      parent.insertBefore(actions, dockButton.parentNode === parent ? dockButton : closeButton);
      actions.appendChild(dockButton);
      actions.appendChild(closeButton);
      return;
    }
    if (dockButton.parentNode !== actions || dockButton.nextSibling !== closeButton) {
      actions.insertBefore(dockButton, closeButton);
    }
  }

  function enhanceLegendCard(definition) {
    var cards = mapContainer.querySelectorAll(definition.card);
    Array.prototype.forEach.call(cards, function(card) {
      var existingAutoHeader = card.querySelector('.pt-map-card-auto-header');
      var closeButtons = card.querySelectorAll(definition.close);
      var closeButton = closeButtons.length ? closeButtons[0] : null;
      if (existingAutoHeader && closeButtons.length > 1) {
        for (var closeIndex = 0; closeIndex < closeButtons.length; closeIndex++) {
          if (!existingAutoHeader.contains(closeButtons[closeIndex])) {
            var staleClose = existingAutoHeader.querySelector(definition.close);
            if (staleClose && staleClose.parentNode) staleClose.parentNode.removeChild(staleClose);
            closeButton = closeButtons[closeIndex];
            existingAutoHeader.querySelector('.pt-map-card-actions').appendChild(closeButton);
            break;
          }
        }
      }
      if (!closeButton) return;

      var dockButton = card.querySelector(definition.dock || '.pt-map-card-dock');
      if (!dockButton) {
        dockButton = document.createElement('button');
        dockButton.type = 'button';
        dockButton.className = 'pt-map-card-dock';
        closeButton.parentNode.insertBefore(dockButton, closeButton);
      } else {
        dockButton.classList.add('pt-map-card-dock');
      }

      var handle = definition.handle ? card.querySelector(definition.handle) : null;
      if (!handle) {
        var closeParent = closeButton.parentElement;
        if (definition.parentHandle && closeParent && closeParent !== card) {
          handle = closeParent;
          handle.classList.add('pt-map-card-handle');
        } else {
          handle = autoHeader(card, closeButton, definition.label);
          var actions = handle.querySelector('.pt-map-card-actions');
          if (dockButton.parentNode !== actions) actions.insertBefore(dockButton, closeButton);
        }
      } else {
        handle.classList.add('pt-map-card-handle');
        if (dockButton.parentNode === closeButton.parentNode && dockButton.nextSibling !== closeButton) {
          closeButton.parentNode.insertBefore(dockButton, closeButton);
        }
      }
      compactHeaderActions(handle, dockButton, closeButton);

      card.classList.add('pt-map-legend-card');

      var state = window.BRIM.legendCloseout.makeDetachable({
        card: card,
        map: map,
        handleSelector: '.pt-map-card-handle',
        dockSelector: definition.dock || '.pt-map-card-dock',
        label: definition.label
      });
    });
  }

  function enhanceAllLegendCards() {
    legendCards.forEach(enhanceLegendCard);
  }

  var gapResizeObserver = null;
  if (typeof window.ResizeObserver === 'function') {
    gapResizeObserver = new window.ResizeObserver(function() {
      scheduleResponsiveLegendOverflow();
    });
  }

  function observeGapLayoutNodes() {
    if (!gapResizeObserver) return;
    [mapContainer, document.getElementById('pt-tools-adddata-wrap'), document.getElementById('pt-local-upload-wrap')].forEach(function(node) {
      if (!node || node.__brimLegendGapObserved) return;
      node.__brimLegendGapObserved = true;
      gapResizeObserver.observe(node);
    });
  }

  var legendObserver = new MutationObserver(function(records) {
    records.forEach(function(record) {
      Array.prototype.forEach.call(record.removedNodes || [], function(node) {
        if (!node || node.nodeType !== 1) return;
        var removedCards = [];
        if (node.matches && node.matches('.pt-map-legend-card')) removedCards.push(node);
        if (node.querySelectorAll) removedCards = removedCards.concat(Array.prototype.slice.call(node.querySelectorAll('.pt-map-legend-card')));
        removedCards.forEach(function(card) {
          var state = card.__brimDetachableState;
          var isDockedCoordinator = state && state.coordinated && !state.floating;
          if (!mapContainer.contains(card) && !isDockedCoordinator && state && state.destroy) {
            card.__brimDetachableState.destroy(false, true);
          }
        });
      });
    });
    records.forEach(function(record) {
      var card = record.target && record.target.closest ? record.target.closest('.pt-map-legend-card') : null;
      if (card && card.style.display === 'none' && card.__brimDetachableState && card.__brimDetachableState.floating) {
        card.__brimDetachableState.dock();
        card.style.display = 'none';
      }
    });
    enhanceAllLegendCards();
    observeGapLayoutNodes();
    scheduleResponsiveLegendOverflow();
  });
  legendObserver.observe(mapContainer, {childList: true, subtree: true, characterData: true, attributes: true, attributeFilter: ['style']});
  enhanceAllLegendCards();
  observeGapLayoutNodes();
  scheduleResponsiveLegendOverflow();
  if (map.on) map.on('resize', function() { scheduleResponsiveLegendOverflow(); });
  window.addEventListener('resize', function() { scheduleResponsiveLegendOverflow(); }, false);
  mapContainer.addEventListener('click', function(e) {
    var target = e && e.target && e.target.closest ? e.target.closest('#pt-tools-adddata-wrap, #pt-local-upload-wrap') : null;
    if (!target) return;
    [0, 60, 240].forEach(function(delay) {
      setTimeout(function() {
        observeGapLayoutNodes();
        scheduleResponsiveLegendOverflow();
      }, delay);
    });
  }, true);
  [0, 300, 1000].forEach(function(delay) {
    setTimeout(function() {
      enhanceAllLegendCards();
      observeGapLayoutNodes();
      scheduleResponsiveLegendOverflow();
    }, delay);
  });
}
