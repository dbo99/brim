# ==== leaflet_ops_live_shared_helpers.r ====================================
##
## PURPOSE:
##   Shared Ops Live CSS, generic utilities, status helpers, and link helpers.
##
## DESIGN:
##   This file is sourced by `leaflet_ops_live_helpers.r`.
##   It returns browser-side JavaScript as text for injection into the Ops Live
##   htmlwidgets/onRender function. Keep edits narrow and feature-specific.
##
## NOTE:
##   Do not put feature-specific query logic here unless it is shared across several modules.
## ============================================================================

pt_ops_live_shared_helpers_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // Shared CSS
  // --------------------------------------------------------------------------
  if (!document.getElementById('pt-ops-live-style')) {
    var style = document.createElement('style');
    style.id = 'pt-ops-live-style';
    style.innerHTML = `


      /* ------------------------------------------------------------------
       * BRIM panel tones + shared button affordances.
       *
       * Earth-tone tints are intentionally subtle so the panels can be named
       * during training without overwhelming the map. Buttons share one green
       * hover/press language across local, external, upload, and Ops panels.
       * ------------------------------------------------------------------ */
      .pt-brim-ui-panel,
      .leaflet-control-layers {
        border-radius: 8px !important;
        border: 1px solid rgba(74, 67, 53, 0.42) !important;
        box-shadow: 0 2px 8px rgba(0,0,0,0.20) !important;
      }

      /* Never let panel-tint classes alter the actual Leaflet map or panes.
       * The map container can contain the text of every control, so a broad
       * text scan can accidentally classify it if not explicitly guarded. */
      .leaflet-container.pt-brim-ui-panel,
      .leaflet-container.pt-brim-panel-local,
      .leaflet-container.pt-brim-panel-external,
      .leaflet-container.pt-brim-panel-upload,
      .leaflet-container.pt-brim-panel-tools,
      .leaflet-container.pt-brim-panel-ops,
      .leaflet-pane.pt-brim-ui-panel,
      .leaflet-control-container.pt-brim-ui-panel {
        background: transparent !important;
        border: none !important;
        box-shadow: none !important;
      }

      .pt-brim-panel-local,
      .leaflet-control-layers {
        background: rgba(246, 239, 223, 0.96) !important; /* warm sand */
      }

      .pt-brim-panel-tools {
        background: rgba(239, 233, 219, 0.96) !important; /* light taupe */
      }

      .pt-brim-panel-external {
        background: rgba(232, 238, 224, 0.96) !important; /* pale sage */
      }

      .pt-brim-panel-upload {
        background: rgba(226, 237, 230, 0.96) !important; /* green gray */
      }

      .pt-brim-panel-ops,
      .pt-ops-live-header,
      .pt-ops-live-body {
        background: rgba(226, 238, 235, 0.96) !important; /* muted blue-green */
      }

      /* Carry panel tints through common inner wrappers while leaving form
       * fields and buttons readable. This fixes panels whose outer ribbon was
       * tinted but whose expanded content stayed default gray/white. */
      .pt-brim-ui-panel > div:not(.pt-ops-live-header):not(.pt-ops-live-body),
      .pt-brim-ui-panel > form,
      .pt-brim-ui-panel .leaflet-control-layers-list,
      .pt-brim-ui-panel .leaflet-control-layers-base,
      .pt-brim-ui-panel .leaflet-control-layers-overlays,
      .pt-brim-ui-panel .leaflet-control-layers-separator,
      .pt-brim-ui-panel .pt-main-layer-section,
      .pt-brim-ui-panel .pt-main-layer-body,
      .pt-brim-ui-panel .pt-external-link-block,
      .pt-brim-ui-panel .pt-upload-section,
      .pt-brim-ui-panel .pt-local-upload-section {
        background-color: transparent !important;
      }

      /* Make the entire open panel visibly tinted, not just the top ribbon. */
      .pt-brim-panel-local .leaflet-control-layers-list,
      .pt-brim-panel-local .leaflet-control-layers-base,
      .pt-brim-panel-local .leaflet-control-layers-overlays,
      .pt-brim-panel-local form,
      .pt-brim-panel-local label,
      .pt-brim-panel-local .leaflet-control-layers-separator {
        background-color: rgba(246, 239, 223, 0.96) !important;
      }

      .pt-brim-panel-external,
      .pt-brim-panel-external > div,
      .pt-brim-panel-external form,
      .pt-brim-panel-external label,
      .pt-brim-panel-external .pt-external-panel,
      .pt-brim-panel-external .pt-external-body,
      .pt-brim-panel-external .pt-external-section,
      .pt-brim-panel-external .pt-external-link-block {
        background-color: rgba(232, 238, 224, 0.96) !important;
      }

      .pt-brim-panel-upload,
      .pt-brim-panel-upload > div,
      .pt-brim-panel-upload form,
      .pt-brim-panel-upload label,
      .pt-brim-panel-upload .pt-upload-panel,
      .pt-brim-panel-upload .pt-local-upload-panel,
      .pt-brim-panel-upload .pt-upload-section,
      .pt-brim-panel-upload .pt-local-upload-section {
        background-color: rgba(226, 237, 230, 0.96) !important;
      }

      .leaflet-control-layers .pt-main-layer-title {
        background: rgba(238, 228, 206, 0.98) !important;
      }

      .pt-ops-live-header {
        border-color: rgba(54, 84, 86, 0.45) !important;
      }

      .pt-ops-live-body {
        border-color: rgba(54, 84, 86, 0.45) !important;
      }

      .pt-brim-btn,
      .pt-brim-ui-panel button,
      .leaflet-control-layers button,
      .pt-main-layer-clear-btn,
      .pt-ops-ribbon-clear {
        appearance: none !important;
        -webkit-appearance: none !important;
        border: 1px solid rgba(82, 72, 52, 0.66) !important;
        border-radius: 5px !important;
        background: #fbfaf5 !important;
        color: #1f2526 !important;
        box-shadow: 0 1px 2px rgba(0,0,0,0.20) !important;
        cursor: pointer !important;
        transition: background-color 0.12s ease, border-color 0.12s ease,
          box-shadow 0.12s ease, transform 0.05s ease !important;
      }

      .pt-brim-btn:hover,
      .pt-brim-ui-panel button:hover,
      .leaflet-control-layers button:hover,
      .pt-main-layer-clear-btn:hover,
      .pt-ops-ribbon-clear:hover,
      .pt-brim-btn.pt-brim-btn-hover,
      .pt-ops-ribbon-clear.pt-ops-ribbon-clear-hover {
        background: #dfead1 !important;
        border-color: #546b38 !important;
        color: #17210f !important;
        box-shadow: 0 1px 7px rgba(0,0,0,0.32) !important;
      }

      .pt-brim-btn:active,
      .pt-brim-ui-panel button:active,
      .leaflet-control-layers button:active,
      .pt-main-layer-clear-btn:active,
      .pt-ops-ribbon-clear:active,
      .pt-brim-btn.pt-brim-btn-pressed,
      .pt-ops-ribbon-clear.pt-ops-ribbon-clear-pressed {
        background: #c8d9b8 !important;
        border-color: #43572d !important;
        color: #17210f !important;
        box-shadow: inset 0 1px 5px rgba(0,0,0,0.36) !important;
        transform: translateY(1px) !important;
      }

      .pt-brim-btn:focus-visible,
      .pt-brim-ui-panel button:focus-visible,
      .leaflet-control-layers button:focus-visible,
      .pt-main-layer-clear-btn:focus-visible,
      .pt-ops-ribbon-clear:focus-visible {
        outline: 2px solid #6f7e42 !important;
        outline-offset: 2px !important;
      }

      /*
       * Fixed Ops panel:
       * - independent of Leaflet's top-right control stack
       * - one internal scrollable body
       * - collapsible from the header
       */
      .pt-ops-live-wrap {
        position: fixed;
        right: 12px;
        bottom: 30px;
        z-index: 10000;
        width: 365px;
        max-width: calc(100vw - 24px);
        max-height: 48vh;
        display: flex;
        flex-direction: column;
        font-family: Arial, Helvetica, sans-serif;
        font-size: 12px;
        line-height: 1.25;
      }

      .pt-ops-live-header {
        flex: 0 0 auto;
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 8px;
        background: rgba(255, 255, 255, 0.96);
        border: 1px solid rgba(0,0,0,0.35);
        border-radius: 8px 8px 0 0;
        box-shadow: 0 2px 9px rgba(0,0,0,0.25);
        font-weight: 700;
        padding: 7px 8px 7px 12px;
        cursor: pointer;
        user-select: none;
        line-height: 18px;
      }

      .pt-ops-live-title {
        display: inline-flex;
        align-items: center;
        gap: 4px;
        min-width: 0;
        white-space: nowrap;
      }

      .pt-ops-live-active-count {
        display: inline-block;
        margin-left: 3px;
        color: #666;
        font-weight: 400;
        font-size: 11px;
        white-space: nowrap;
      }

      .pt-ops-live-active-count.pt-ops-active-count-on {
        color: #106b21;
      }

      .pt-ops-ribbon-clear-shell {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        flex: 0 0 auto;
        padding: 1px;
        border-radius: 6px;
        background: rgba(255,255,255,0.82);
        box-shadow: 0 1px 2px rgba(0,0,0,0.18);
      }

      #pt-ops-clear-ribbon-btn.pt-ops-ribbon-clear {
        appearance: none !important;
        -webkit-appearance: none !important;
        border: 1px solid #777 !important;
        border-radius: 5px !important;
        background: #f7f7f7 !important;
        cursor: pointer !important;
        font: 11px/1.15 Arial, Helvetica, sans-serif !important;
        font-weight: 700 !important;
        padding: 4px 8px !important;
        color: #222 !important;
        box-shadow: 0 1px 2px rgba(0,0,0,0.25) !important;
        transition: background-color 0.10s ease, border-color 0.10s ease,
          box-shadow 0.10s ease, transform 0.05s ease !important;
        pointer-events: auto !important;
        position: relative;
      }

      #pt-ops-clear-ribbon-btn.pt-ops-ribbon-clear:hover,
      #pt-ops-clear-ribbon-btn.pt-ops-ribbon-clear.pt-ops-ribbon-clear-hover,
      #pt-ops-clear-ribbon-btn.pt-ops-ribbon-clear:focus {
        background: #dfead1 !important;
        border-color: #546b38 !important;
        color: #17210f !important;
        box-shadow: 0 1px 7px rgba(0,0,0,0.38) !important;
      }

      #pt-ops-clear-ribbon-btn.pt-ops-ribbon-clear:active,
      #pt-ops-clear-ribbon-btn.pt-ops-ribbon-clear.pt-ops-ribbon-clear-pressed {
        background: #c8d9b8 !important;
        border-color: #43572d !important;
        color: #17210f !important;
        box-shadow: inset 0 1px 5px rgba(0,0,0,0.45) !important;
        transform: translateY(1px) !important;
      }

      #pt-ops-clear-ribbon-btn.pt-ops-ribbon-clear:focus-visible {
        outline: 2px solid #6f7e42 !important;
        outline-offset: 2px !important;
      }

      .pt-ops-live-wrap.pt-ops-collapsed .pt-ops-live-header {
        border-radius: 8px;
      }

      .pt-ops-live-caret {
        float: none;
        font-size: 11px;
      }

      .pt-ops-live-body {
        flex: 1 1 auto;
        min-height: 0;
        overflow-y: auto !important;
        overflow-x: hidden !important;
        overscroll-behavior: contain;
        background: rgba(255, 255, 255, 0.96);
        border: 1px solid rgba(0,0,0,0.35);
        border-top: none;
        border-radius: 0 0 8px 8px;
        box-shadow: 0 2px 9px rgba(0,0,0,0.25);
        padding: 8px 10px;
        box-sizing: border-box;
      }

      .pt-ops-live-wrap.pt-ops-collapsed {
        max-height: none;
      }

      .pt-ops-live-wrap.pt-ops-collapsed .pt-ops-live-body {
        display: none;
      }

      .pt-ops-panel h4 {
        margin: 8px 0 6px 0;
        font-size: 13px;
      }
      .pt-ops-panel h4:first-child {
        margin-top: 0;
      }
      .pt-ops-panel .pt-ops-small {
        font-size: 11px;
        color: #555;
        margin-bottom: 6px;
      }
      .pt-ops-panel .pt-ops-section {
        border-top: 1px solid #ddd;
        margin-top: 7px;
        padding-top: 6px;
      }
      .pt-ops-subgroup {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 7px;
        margin: 8px 5px 5px 5px;
        font-size: 10px;
        font-weight: 500;
        color: #63716c;
        letter-spacing: 0.01em;
        text-align: center;
        line-height: 1.15;
        white-space: normal;
      }
      .pt-ops-subgroup::before,
      .pt-ops-subgroup::after {
        content: "";
        flex: 1 1 26px;
        min-width: 18px;
        border-top: 1px solid rgba(80, 100, 95, 0.20);
      }
      .pt-ops-panel label {
        display: block;
        margin: 2px 0;
        cursor: pointer;
      }
      .pt-ops-panel input[type='checkbox'] {
        margin-right: 5px;
      }
      .pt-ops-layer-row {
        display: flex;
        align-items: baseline;
        flex-wrap: wrap;
        gap: 3px;
        margin: 2px 0;
      }
      .pt-ops-layer-row.pt-ops-layer-loading {
        color: #9a5a00;
      }
      .pt-ops-layer-help {
        margin: -1px 0 4px 21px;
        font-size: 10.3px;
        line-height: 1.22;
        color: #66746f;
      }
      .pt-ops-layer-label {
        display: inline-flex !important;
        align-items: baseline;
        margin: 0 !important;
        cursor: pointer;
        min-width: 0;
      }
      .pt-ops-row-links {
        display: inline-flex;
        align-items: baseline;
        gap: 3px;
        margin-left: 4px;
        font-size: 10.5px;
        white-space: nowrap;
      }
      .pt-ops-row-links a {
        color: #2f5f8f;
        text-decoration: none;
        border-bottom: 1px dotted rgba(47,95,143,0.55);
      }
      .pt-ops-row-links a:hover {
        color: #123f68;
        border-bottom-color: rgba(18,63,104,0.9);
      }
      .pt-ops-row-link-sep {
        color: #aaa;
      }
      .pt-ops-external-link-block {
        margin: 4px 0 6px 20px;
        padding: 4px 6px;
        border-left: 3px solid #ddd;
        background: rgba(0,0,0,0.025);
        font-size: 11px;
      }
      .pt-ops-external-link-title {
        font-weight: 700;
        color: #333;
        margin-bottom: 1px;
      }
      .pt-ops-external-link-note {
        color: #666;
        font-size: 10.5px;
        margin-bottom: 2px;
      }
      .pt-ops-external-link-row {
        display: inline-flex;
        align-items: baseline;
        flex-wrap: wrap;
        gap: 4px;
      }
      .pt-ops-external-link-row a {
        color: #2f5f8f;
        text-decoration: none;
        border-bottom: 1px dotted rgba(47,95,143,0.55);
      }
      .pt-ops-external-link-row a:hover {
        color: #123f68;
        border-bottom-color: rgba(18,63,104,0.9);
      }
      .pt-ops-layer-spinner {
        display: none;
        margin-left: 4px;
        color: #9a5a00;
        font-size: 10px;
        font-weight: 400;
      }
      .pt-ops-layer-row.pt-ops-layer-loading .pt-ops-layer-spinner {
        display: inline;
      }
      .pt-ops-panel button {
        font-size: 11px;
        padding: 3px 7px;
        margin: 2px 4px 3px 0;
        border: 1px solid #999;
        border-radius: 5px;
        background: #f7f7f7;
        cursor: pointer;
      }
      .pt-ops-status-row {
        border-top: 1px solid #ddd;
        padding-top: 4px;
        margin-top: 4px;
      }
      .pt-ops-ok { color: #106b21; }
      .pt-ops-warn { color: #9a5a00; }
      .pt-ops-bad { color: #9c1c1c; }
      .pt-ops-muted { color: #666; }
      .pt-ops-legend-line {
        white-space: nowrap;
      }
      .pt-ops-swatch {
        display: inline-block;
        width: 14px;
        height: 10px;
        margin-right: 5px;
        border: 1px solid rgba(0,0,0,0.25);
        vertical-align: middle;
      }
      .pt-ops-links a {
        display: block;
        margin: 2px 0;
      }

      .pt-ops-active-links {
        margin-top: 3px;
        font-size: 11px;
      }

      .pt-ops-active-links a {
        display: inline;
        margin-right: 8px;
      }

      .pt-ops-legend-note {
        margin-top: 3px;
        font-size: 11px;
        color: #666;
      }

      /* Top-left map legend for selected Ops Live layers.  This is separate
       * from the Ops panel's active-overlay notes so users can keep the legend
       * visible while the bottom-right Ops panel is collapsed or scrolled. */
      .pt-ops-map-legend {
        background: rgba(226, 238, 235, 0.98) !important;
        border: 1px solid rgba(90, 120, 116, 0.55) !important;
        border-radius: 7px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.22);
        color: #222;
        font: 11px/1.20 Arial, Helvetica, sans-serif;
        /* Keep Ops map legends in the same general upper-left zone as
         * SCAN/SWE controls: right of the zoom/Notes stack. The wider, shorter
         * card is nudged high to preserve a cleaner gap above the HUC-fill dropdown, while staying clear
         * of the zoom/Notes stack and the Measure/Clear All controls. */
        position: absolute !important;
        left: 96px !important;
        top: 2px !important;
        margin: 0 !important;
        width: 390px;
        max-width: min(390px, calc(100vw - 138px));
        box-sizing: border-box;
        padding: 7px 9px;
        pointer-events: auto;
        /* Keep Ops legends above the External Layers ribbon/panel while
         * retaining the natural stacked layout in the upper-left review area. */
        z-index: 10010;
      }

      .pt-ops-map-legend h4 {
        margin: 0 0 4px 0;
        font-size: 12px;
        line-height: 1.10;
      }

      .pt-ops-map-legend-section {
        margin-top: 5px;
        padding-top: 4px;
        border-top: 1px solid rgba(0,0,0,0.12);
      }

      .pt-ops-legend-row {
        display: grid;
        grid-template-columns: minmax(0, 1.1fr) minmax(145px, 0.9fr);
        column-gap: 10px;
        align-items: start;
      }

      .pt-ops-legend-grid-2 {
        display: grid;
        grid-template-columns: 1fr 1fr;
        column-gap: 12px;
        row-gap: 1px;
        margin-top: 2px;
      }

      .pt-ops-legend-footnotes {
        grid-template-columns: 1fr 1fr;
        margin-top: 3px;
      }

      .pt-ops-map-legend-section:first-child {
        margin-top: 0;
        padding-top: 0;
        border-top: none;
      }

      .pt-ops-legend-gradient {
        height: 12px;
        border: 1px solid rgba(0,0,0,0.35);
        border-radius: 3px;
        margin: 3px 0 1px 0;
      }

      .pt-ops-legend-gradient-reservoir {
        background: linear-gradient(to right, #C6DBEF 0%, #9ECAE1 25%, #6BAED6 50%, #2171B5 75%, #08306B 100%);
      }

      .pt-ops-legend-scale {
        display: flex;
        justify-content: space-between;
        font-size: 10px;
        color: #555;
        margin-bottom: 3px;
      }

      .pt-ops-legend-textline {
        margin: 1px 0;
        white-space: normal;
      }

      .pt-ops-legend-small {
        color: #666;
        font-size: 10.5px;
        line-height: 1.18;
      }

      .pt-ops-legend-circle {
        display: inline-block;
        width: 11px;
        height: 11px;
        border-radius: 50%;
        margin-right: 5px;
        vertical-align: -1px;
        border: 1.4px solid rgba(0,0,0,0.48);
      }

      .pt-ops-legend-stale {
        border-style: dashed !important;
        border-width: 2px !important;
      }

      .pt-ops-legend-multipoint {
        display: inline-block;
        width: 14px;
        height: 14px;
        border-radius: 4px;
        margin-right: 5px;
        vertical-align: -3px;
        border: 2px solid #7B241C;
        background: rgba(246, 190, 103, 0.94);
        position: relative;
      }

      .pt-ops-legend-multipoint:before {
        content: '';
        position: absolute;
        left: 3px;
        top: 3px;
        width: 3px;
        height: 3px;
        border-radius: 50%;
        background: #111;
        box-shadow: 5px 0 0 #111, 0 5px 0 #111, 5px 5px 0 #111;
      }


      /* Hover card for the NWS Watches / Warnings / Advisories layer. */
      .leaflet-tooltip.pt-ops-wwa-tooltip {
        background: rgba(255, 255, 255, 0.97);
        border: 1px solid rgba(0,0,0,0.55);
        border-radius: 4px;
        box-shadow: 0 2px 9px rgba(0,0,0,0.30);
        color: #111;
        padding: 0;
        max-width: 340px;
        white-space: normal;
        pointer-events: auto;
      }

      .leaflet-tooltip.pt-ops-wwa-tooltip::before {
        display: none;
      }

      .pt-ops-wwa-card {
        font: 12px/1.25 Arial, Helvetica, sans-serif;
        min-width: 230px;
      }

      .pt-ops-wwa-item {
        border-top: 1px solid rgba(0,0,0,0.18);
        border-left: 5px solid #777;
        padding: 6px 8px 6px 8px;
      }

      .pt-ops-wwa-item:first-child {
        border-top: none;
      }

      .pt-ops-wwa-warning { border-left-color: #d7191c; }
      .pt-ops-wwa-watch   { border-left-color: #fdae61; }
      .pt-ops-wwa-advisory{ border-left-color: #2c7bb6; }

      .pt-ops-wwa-title {
        font-weight: 700;
        font-size: 13px;
      }

      .pt-ops-wwa-expires {
        margin-top: 2px;
      }

      .pt-ops-wwa-meta {
        margin-top: 2px;
        color: #444;
        font-size: 11px;
      }

      .pt-ops-wwa-meta a {
        color: #1f5e9c;
        text-decoration: none;
      }

      .pt-ops-wwa-meta a:hover {
        text-decoration: underline;
      }

      .pt-ops-res-tooltip {
        background: rgba(255, 255, 255, 0.96);
        border: 1px solid rgba(0,0,0,0.45);
        border-radius: 4px;
        box-shadow: 0 2px 7px rgba(0,0,0,0.25);
        color: #111;
        padding: 5px 7px;
        font: 12px/1.25 Arial, Helvetica, sans-serif;
        white-space: nowrap;
      }

      .pt-ops-res-popup {
        font: 12px/1.35 Arial, Helvetica, sans-serif;
        min-width: 230px;
      }

      .pt-ops-res-popup a {
        color: #1f5e9c;
        text-decoration: none;
      }

      .pt-ops-res-popup a:hover {
        text-decoration: underline;
      }

      /* Compact hover card for WPC QPF polygon/range identify. */
      .leaflet-tooltip.pt-ops-wpc-qpf-tooltip {
        background: rgba(255, 255, 255, 0.97);
        border: 1px solid rgba(0,0,0,0.48);
        border-radius: 4px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.25);
        color: #111;
        padding: 6px 8px;
        min-width: 420px;
        max-width: 620px;
        white-space: normal;
        pointer-events: none;
      }

      .leaflet-container.pt-ops-wpc-qpf-hover-on,
      .leaflet-container.pt-ops-wpc-qpf-hover-on .leaflet-pane,
      .leaflet-container.pt-ops-wpc-qpf-hover-on .leaflet-image-layer {
        cursor: crosshair !important;
      }

      .leaflet-tooltip.pt-ops-wpc-qpf-tooltip::before {
        display: none;
      }

      .pt-ops-wpc-qpf-card {
        font: 12px/1.30 Arial, Helvetica, sans-serif;
      }

      .pt-ops-wpc-qpf-title {
        font-weight: 700;
        font-size: 13px;
      }

      .pt-ops-wpc-qpf-row {
        margin-top: 1px;
      }

      .pt-ops-wpc-qpf-muted {
        color: #555;
        font-size: 11px;
      }

    `;
    document.head.appendChild(style);
  }


  // --------------------------------------------------------------------------
  // BRIM panel and button polish.
  // --------------------------------------------------------------------------
  function ptButtonText(btn) {
    if (!btn) return '';
    return String(btn.textContent || btn.innerText || '').replace(/\s+/g, ' ').trim();
  }

  function ptApplyButtonStateHandlers(btn) {
    if (!btn || btn.getAttribute('data-pt-brim-button-polished') === 'true') return;

    btn.setAttribute('data-pt-brim-button-polished', 'true');
    btn.classList.add('pt-brim-btn');

    btn.addEventListener('mouseenter', function() {
      btn.classList.add('pt-brim-btn-hover');
    });

    btn.addEventListener('mouseleave', function() {
      btn.classList.remove('pt-brim-btn-hover');
      btn.classList.remove('pt-brim-btn-pressed');
    });

    btn.addEventListener('mousedown', function() {
      btn.classList.add('pt-brim-btn-pressed');
    });

    btn.addEventListener('mouseup', function() {
      btn.classList.remove('pt-brim-btn-pressed');
    });

    btn.addEventListener('touchstart', function() {
      btn.classList.add('pt-brim-btn-pressed');
    }, {passive: true});

    btn.addEventListener('touchend', function() {
      btn.classList.remove('pt-brim-btn-pressed');
    });
  }

  function ptIsMapOrPaneElement(el) {
    if (!el || !el.classList) return false;

    return el.classList.contains('leaflet-container') ||
      el.classList.contains('leaflet-map-pane') ||
      el.classList.contains('leaflet-pane') ||
      el.classList.contains('leaflet-tile-pane') ||
      el.classList.contains('leaflet-overlay-pane') ||
      el.classList.contains('leaflet-marker-pane') ||
      el.classList.contains('leaflet-tooltip-pane') ||
      el.classList.contains('leaflet-popup-pane') ||
      el.classList.contains('leaflet-control-container') ||
      el.classList.contains('html-widget');
  }

  function ptClearBrimPanelClasses(el) {
    if (!el || !el.classList) return;

    [
      'pt-brim-ui-panel',
      'pt-brim-panel-ops',
      'pt-brim-panel-local',
      'pt-brim-panel-external',
      'pt-brim-panel-upload',
      'pt-brim-panel-tools'
    ].forEach(function(cls) { el.classList.remove(cls); });

    el.removeAttribute('data-pt-brim-panel-polished');
  }

  function ptLooksLikeCompactControlPanel(panel) {
    if (!panel || !panel.getBoundingClientRect) return false;

    var r = panel.getBoundingClientRect();

    // Normal controls/panels are narrow. The map container and map panes are
    // very large and must not receive background tints. Use generous thresholds
    // so the expanded external/upload panels still qualify.
    if (r.width > 700 || r.height > 900) return false;

    return true;
  }

  function ptClassifyControlPanel(panel) {
    if (!panel) return;

    if (ptIsMapOrPaneElement(panel) || !ptLooksLikeCompactControlPanel(panel)) {
      ptClearBrimPanelClasses(panel);
      return;
    }

    var txt = String(panel.textContent || '').replace(/\s+/g, ' ').trim().toLowerCase();
    var classes = [
      'pt-brim-panel-ops',
      'pt-brim-panel-local',
      'pt-brim-panel-external',
      'pt-brim-panel-upload',
      'pt-brim-panel-tools'
    ];

    // Re-classify each pass because several BRIM controls start collapsed or
    // receive content after page load.  The previous one-shot classification
    // could mark a panel before its identifying text was present, leaving it
    // untinted later.
    classes.forEach(function(cls) { panel.classList.remove(cls); });

    var cls = '';
    if (txt.indexOf('ops live layers') >= 0) {
      cls = 'pt-brim-panel-ops';
    } else if (txt.indexOf('basemaps / local layers') >= 0 || txt.indexOf('huc fill') >= 0 ||
        (txt.indexOf('basemaps') >= 0 && txt.indexOf('no basemap') >= 0)) {
      cls = 'pt-brim-panel-local';
    } else if (txt.indexOf('external layers') >= 0 || txt.indexOf('external overlays') >= 0 ||
        txt.indexOf('quick-add external') >= 0 || txt.indexOf('catalog layers') >= 0) {
      cls = 'pt-brim-panel-external';
    } else if (txt.indexOf('local gis uploads') >= 0 || txt.indexOf('upload local gis') >= 0 ||
        txt.indexOf('active local files') >= 0 || txt.indexOf('style selected local layer') >= 0) {
      cls = 'pt-brim-panel-upload';
    } else if (txt.indexOf('measure') >= 0 || txt.indexOf('clear all') >= 0) {
      cls = 'pt-brim-panel-tools';
    }

    if (cls) {
      panel.classList.add('pt-brim-ui-panel');
      panel.classList.add(cls);
      panel.setAttribute('data-pt-brim-panel-polished', 'true');
    }
  }

  function ptPolishBrimPanelsAndButtons() {
    // Defensive cleanup in case a previous broad scan touched the map element.
    Array.prototype.forEach.call(document.querySelectorAll(
      '.leaflet-container, .leaflet-map-pane, .leaflet-pane, .leaflet-control-container'
    ), ptClearBrimPanelClasses);

    var panelSelectors = [
      '.leaflet-control',
      '.leaflet-control-layers',
      '.pt-huc-theme-control',
      '.pt-ops-live-wrap',
      '.pt-ops-live-header',
      '.pt-ops-live-body',
      '.pt-external-panel',
      '.pt-local-upload-panel',
      '.pt-upload-panel'
    ];

    panelSelectors.forEach(function(sel) {
      Array.prototype.forEach.call(document.querySelectorAll(sel), ptClassifyControlPanel);
    });

    Array.prototype.forEach.call(document.querySelectorAll('button'), function(btn) {
      var txt = ptButtonText(btn).toLowerCase();

      // Do not restyle hidden browser or third-party internals that happen to
      // use buttons. The visible BRIM/Leaflet controls have readable text.
      if (!txt) return;

      if (txt === 'clear all' || txt === 'clear ops' || txt === 'clear local layers' ||
          txt === 'clear external' || txt === 'clear uploads' || txt === 'close' ||
          txt.indexOf('clear') === 0 || txt === 'measure') {
        ptApplyButtonStateHandlers(btn);
      }
    });
  }

  function ptInstallBrimPanelUiPolish() {
    if (window.ptBrimPanelUiPolishInstalled) {
      ptPolishBrimPanelsAndButtons();
      return;
    }

    window.ptBrimPanelUiPolishInstalled = true;
    ptPolishBrimPanelsAndButtons();

    var timer = null;
    var observer = new MutationObserver(function() {
      if (timer) window.clearTimeout(timer);
      timer = window.setTimeout(ptPolishBrimPanelsAndButtons, 80);
    });

    observer.observe(document.body, {
      childList: true,
      subtree: true
    });

    window.setTimeout(ptPolishBrimPanelsAndButtons, 250);
    window.setTimeout(ptPolishBrimPanelsAndButtons, 1000);
  }

  ptInstallBrimPanelUiPolish();
  
  // --------------------------------------------------------------------------
  // Helper functions
  // --------------------------------------------------------------------------
  function fmtTime(d) {
    try {
      return d.toLocaleString();
    } catch(e) {
      return String(d);
    }
  }
  
  function nowLocal() {
    return fmtTime(new Date());
  }
  
  function escapeHtml(s) {
    return String(s)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&#039;');
  }
  
  function safeUrlBase(url) {
    return url.replace(/\/$/, '');
  }
  
  function getMapSizeForExport() {
    var s = map.getSize();
    var w = Math.max(400, Math.min(1800, Math.round(s.x)));
    var h = Math.max(300, Math.min(1200, Math.round(s.y)));
    return {w: w, h: h};
  }
  
  function getBbox3857() {
    var b = map.getBounds();
    var sw = map.options.crs.project(b.getSouthWest());
    var ne = map.options.crs.project(b.getNorthEast());
    return [sw.x, sw.y, ne.x, ne.y].join(',');
  }
  
  var activeLayers = {};
  var activeLegendDefs = {};
  var statusRows = {};
  var checkboxByName = {};
  var rowByName = {};
  var opsDefByName = {};
  var opsLinkBlocks = [];

  var wwaHoverActive = false;
  var wwaHoverTimer = null;
  var wwaHoverSeq = 0;
  var wwaHoverTooltip = null;
  var wwaLastMouseLatLng = null;
  
  function updateOpsHeaderCount() {
    var countEl = document.getElementById('pt-ops-active-count');
    if (!countEl) return;

    var n = Object.keys(activeLayers).length;
    countEl.textContent = n > 0 ? '(' + n + ' active)' : '(none active)';
    countEl.classList.toggle('pt-ops-active-count-on', n > 0);
  }

  function setOpsLayerLoading(name, isLoading) {
    var row = rowByName[name];
    if (!row) return;

    row.classList.toggle('pt-ops-layer-loading', !!isLoading);

    if (isLoading) {
      row.setAttribute('aria-busy', 'true');
    } else {
      row.removeAttribute('aria-busy');
    }
  }

  function isOpsLayerLoading(name) {
    var row = rowByName[name];
    return !!(row && row.classList && row.classList.contains('pt-ops-layer-loading'));
  }

  function clearAllOpsLayerLoading() {
    Object.keys(rowByName).forEach(function(name) {
      setOpsLayerLoading(name, false);
    });
  }
  
  function recordStatus(name, msg, cssClass) {
    statusRows[name] = {
      msg: msg,
      cssClass: cssClass || 'pt-ops-muted',
      time: nowLocal()
    };
    redrawStatus();
  }
  
  function redrawStatus() {
    if (!statusDiv) return;
    var keys = Object.keys(statusRows).sort();
    if (!keys.length) {
      statusDiv.innerHTML = '<h4>Ops status / freshness</h4><div class="pt-ops-muted">No Ops overlays loaded yet.</div>';
      return;
    }
    var html = '<h4>Ops status / freshness</h4>';
    keys.forEach(function(k) {
      var r = statusRows[k];
      html += '<div class="pt-ops-status-row"><b>' + escapeHtml(k) + '</b><br>' +
              '<span class="' + r.cssClass + '">' + escapeHtml(r.msg) + '</span><br>' +
              '<span class="pt-ops-muted">Checked: ' + escapeHtml(r.time) + '</span></div>';
    });
    statusDiv.innerHTML = html;
  }
  
  function qpeLegendHtml(label) {
    return '<div class="pt-ops-section"><b>' + escapeHtml(label) + '</b><br>' +
      '<div class="pt-ops-legend-line"><span class="pt-ops-swatch" style="background:#ccecff"></span>Very light / low end</div>' +
      '<div class="pt-ops-legend-line"><span class="pt-ops-swatch" style="background:#6ec6ff"></span>Light</div>' +
      '<div class="pt-ops-legend-line"><span class="pt-ops-swatch" style="background:#31d843"></span>Moderate</div>' +
      '<div class="pt-ops-legend-line"><span class="pt-ops-swatch" style="background:#fff04a"></span>Heavier</div>' +
      '<div class="pt-ops-legend-line"><span class="pt-ops-swatch" style="background:#ff9a26"></span>Heavy</div>' +
      '<div class="pt-ops-legend-line"><span class="pt-ops-swatch" style="background:#e31a1c"></span>Very heavy</div>' +
      '<div class="pt-ops-legend-line"><span class="pt-ops-swatch" style="background:#d900ff"></span>Extreme / upper end</div>' +
      '<div class="pt-ops-muted">Approximate color-family guide only; exact breaks and ramps come from each source service.</div></div>';
  }

  function eroLegendHtml() {
    return '<div class="pt-ops-section"><b>WPC Excessive Rainfall Outlook</b><br>' +
      '<div class="pt-ops-legend-line"><span class="pt-ops-swatch" style="background:#38a800"></span>Marginal</div>' +
      '<div class="pt-ops-legend-line"><span class="pt-ops-swatch" style="background:#ffff00"></span>Slight</div>' +
      '<div class="pt-ops-legend-line"><span class="pt-ops-swatch" style="background:#ff0000"></span>Moderate</div>' +
      '<div class="pt-ops-legend-line"><span class="pt-ops-swatch" style="background:#ff00ff"></span>High</div></div>';
  }

  function compactUrlForTitle(url) {
    if (!url) return '';

    try {
      var u = new URL(url);
      var host = u.hostname.replace(/^www\./, '');
      var path = u.pathname || '';
      var firstPath = path.split('/').filter(Boolean)[0] || '';
      return firstPath ? host + '/' + firstPath + '/…' : host + '/…';
    } catch(e) {
      var txt = String(url || '');
      return txt.length > 24 ? txt.substring(0, 21) + '…' : txt;
    }
  }

  function ptOpsCompactLinkLabel(label) {
    var x = String(label || '')
      .trim()
      .toLowerCase();

    if (!x) return 'info';
    if (x === 'legend' || x === 'leg' || x === 'lgnd') return 'lgnd';
    if (x === 'source' || x === 'service' || x === 'geojson' || x === 'api' || x === 'srce') return 'srce';
    if (x === 'refresh' || x === 'rfrsh') return 'rfrsh';
    if (x === 'summary') return 'summary';
    if (x === 'guide') return 'guide';

    return label;
  }

  function ptOpsShouldDisplayLink(url) {
    if (!url) return false;

    // BRIM live-feed products are fetched internally by the app. Avoid exposing
    // thin public GitHub-hosted GeoJSON/CSV/summary links in the user-facing
    // Ops panel; keep user-facing links focused on official sources, guides,
    // legends, and refresh controls.
    var txt = String(url || '').toLowerCase();
    if (txt.indexOf('github.io/brim-live-data-feeds') >= 0) return false;
    if (txt.indexOf('raw.githubusercontent.com/dbo99/brim-live-data-feeds') >= 0) return false;

    return true;
  }

  function linkHtml(label, url, titlePrefix) {
    if (!ptOpsShouldDisplayLink(url)) return '';

    var title = (titlePrefix || 'Open link') + ': ' + compactUrlForTitle(url);

    return '<a href="' + escapeHtml(url) + '" target="_blank" rel="noopener" title="' +
      escapeHtml(title) + '">' + escapeHtml(ptOpsCompactLinkLabel(label)) + '</a>';
  }

  function infoLabelForDef(def, opts) {
    def = def || {};
    opts = opts || {};

    if (opts.infoLabel) return ptOpsCompactLinkLabel(opts.infoLabel);
    if (def.category && String(def.category).toLowerCase().indexOf('satellite') >= 0) return 'guide';

    return 'info';
  }

  function layerLinkItems(def) {
    def = def || {};

    var opts = def.layer && def.layer.options ? def.layer.options : {};
    var items = [];

    // Prefer layer options when they exist, but fall back to the Ops definition
    // itself. Some custom L.layerGroup()/WMS layers do not preserve custom
    // link metadata in layer.options, while their Ops definitions do.
    var legendUrl = opts.legendUrl || def.legendUrl || '';
    var infoUrl = opts.infoUrl || def.infoUrl || '';
    var sourceUrl = opts.sourceUrl || def.sourceUrl || opts.url || def.url || '';
    var infoLabel = opts.infoLabel || def.infoLabel || '';

    function pushUnique(label, url, titlePrefix) {
      var html = linkHtml(label, url, titlePrefix);
      if (!html) return;

      var key = String(url || '').trim().toLowerCase() + '|' + ptOpsCompactLinkLabel(label);
      if (items.some(function(item) { return item.key === key; })) return;

      // Avoid two visually identical links to the same endpoint, such as
      // "srce · srce" when an Ops row has both infoUrl and sourceUrl set to
      // the same FeatureServer/MapServer URL.
      var sameUrl = String(url || '').trim().toLowerCase();
      if (items.some(function(item) { return item.url === sameUrl; })) return;

      items.push({key: key, url: sameUrl, html: html});
    }

    if (legendUrl) {
      pushUnique('lgnd', legendUrl, 'Open legend');
    }

    if (infoUrl) {
      pushUnique(infoLabelForDef(def, { infoLabel: infoLabel }), infoUrl, 'Open guide/info');
    }

    if (sourceUrl) {
      pushUnique('srce', sourceUrl, 'Open source');
    }

    return items.map(function(item) { return item.html; }).filter(function(x) { return !!x; });
  }

  function ptOpsDefIsRefreshable(def) {
    def = def || {};
    var layer = def.layer || null;
    var opts = layer && layer.options ? layer.options : {};

    if (def.refreshable === true || opts.refreshable === true) return true;
    if (layer && typeof layer.refreshCurrentView === 'function') return true;

    // Promoted External catalog layers often represent current-view
    // FeatureServer/MapServer snapshots.  Show a compact rfrsh control and let
    // the bridge decide whether the active catalog row can really refresh.
    if (opts && opts.opsKey) return true;

    return false;
  }

  function ptOpsRefreshActionHtml(def) {
    if (!ptOpsDefIsRefreshable(def)) return '';

    var name = String(def && def.name ? def.name : '');
    if (!name) return '';

    return '<a href="#" class="pt-ops-rfrsh-link" data-pt-ops-action="refresh" data-pt-ops-name="' +
      escapeHtml(name) +
      '" title="Refresh this Ops layer using the current map view">rfrsh</a>';
  }

  function layerRowLinksHtml(def) {
    var items = [];
    var refreshHtml = ptOpsRefreshActionHtml(def);
    if (refreshHtml) items.push(refreshHtml);

    items = items.concat(layerLinkItems(def));
    if (!items.length) return '';

    return '<span class="pt-ops-row-links">' +
      items.join('<span class="pt-ops-row-link-sep">·</span>') +
      '</span>';
  }

  function addOpsExternalLinks(block) {
    block = block || {};
    block.links = Array.isArray(block.links) ? block.links : [];
    block.subgroup = block.subgroup || '';
    opsLinkBlocks.push(block);
  }

  function opsExternalLinkBlockHtml(block) {
    block = block || {};

    var links = (Array.isArray(block.links) ? block.links : [])
      .map(function(item) {
        item = item || {};
        return linkHtml(item.label || 'link', item.url || '', item.title || 'Open external product');
      })
      .filter(function(x) { return !!x; });

    if (!links.length) return '';

    return '<div class="pt-ops-external-link-block">' +
      '<div class="pt-ops-external-link-title">' + escapeHtml(block.title || 'External links') + '</div>' +
      (block.note ? '<div class="pt-ops-external-link-note">' + escapeHtml(block.note) + '</div>' : '') +
      '<div class="pt-ops-external-link-row">' +
      links.join('<span class="pt-ops-row-link-sep">·</span>') +
      '</div>' +
      '</div>';
  }

  function opsExternalLinksForCategory(category, subgroup) {
    var html = '';
    var subgroupKey = subgroup || '';

    opsLinkBlocks.forEach(function(block) {
      var blockSubgroup = block && block.subgroup ? block.subgroup : '';
      if (block && block.category === category && blockSubgroup === subgroupKey) {
        html += opsExternalLinkBlockHtml(block);
      }
    });

    return html;
  }

  function activeOverlayLinksHtml(def) {
    def = def || {};

    var links = [];

    function pushActiveLink(label, url, titlePrefix) {
      var html = linkHtml(label, url, titlePrefix);
      if (html) links.push(html);
    }

    if (def.legendUrl) {
      pushActiveLink('lgnd', def.legendUrl, 'Open legend');
    }

    if (def.infoUrl) {
      pushActiveLink(ptOpsCompactLinkLabel(def.infoLabel || 'info'), def.infoUrl, 'Open guide/info');
    }

    if (def.sourceUrl) {
      pushActiveLink('srce', def.sourceUrl, 'Open source');
    }

    if (!links.length && !def.legendNote) {
      return '';
    }

    var html = '';

    if (links.length) {
      html += '<div class="pt-ops-active-links">' + links.join(' · ') + '</div>';
    }

    if (def.legendNote) {
      html += '<div class="pt-ops-legend-note">' + escapeHtml(def.legendNote) + '</div>';
    }

    return html;
  }
  


)---"
}
