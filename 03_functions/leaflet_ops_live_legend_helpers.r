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

  function opsGroundwaterLegendHtml() {
    return '<div class="pt-ops-map-legend-section">' +
      ptOpsMapLegendTitleHtml('USGS GW monitoring wells measured in last 800 days', 'usgs_groundwater') +
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
      '</div>' +
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

  function opsUsgsStreamflowLegendHtml(def) {
    return '<div class="pt-ops-map-legend-section">' +
      ptOpsMapLegendTitleHtml('USGS streamflow | California | Ops Live', 'usgs_streamflow') +
      ptOpsLegendMetricHtml(def) +
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
      '<div class="pt-ops-legend-small" style="margin-top:3px;"><span class="pt-ops-legend-circle" style="background:#D9EAF7;border-color:#3182BD;border-style:dashed;"></span>Stage-only site. This Ops layer is not a flood-stage renderer.</div>' +
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
      if (def.legendType) activeTypes[def.legendType] = true;
    });
    Object.keys(ptOpsMapLegendHiddenTypes).forEach(function(type) {
      if (!activeTypes[type]) delete ptOpsMapLegendHiddenTypes[type];
    });

    var seen = {};
    var html = '';
    keys.forEach(function(k) {
      var def = activeLegendDefs[k] || {};
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
      html += '<div class="pt-ops-section"><b>' + escapeHtml(k) + '</b><br><span class="pt-ops-muted">' + escapeHtml(def.note || '') + '</span>' + activeOverlayLinksHtml(def) + '</div>';
      if (def.legendType && !seen[def.legendType]) {
        if (def.legendType === 'qpe') html += qpeLegendHtml('QPE / QPF colors');
        if (def.legendType === 'airnow_aqi') html += '<div class="pt-ops-section"><b>AirNow AQI legend</b><br><span class="pt-ops-muted">A compact AirNow AQI category legend is shown on the map while this layer is active.</span></div>';
        if (def.legendType === 'ero') html += eroLegendHtml();
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
