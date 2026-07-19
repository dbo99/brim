# ==== leaflet_ops_live_cnrfc_forecast_points_helpers.r ======================
##
## PURPOSE:
##   Browser-side Ops Live helper for the local, preprocessed CNRFC active
##   river/reservoir forecast-point layer.
##
## DESIGN:
##   The active point records are produced by 53_ as a slim map-ready RDS and
##   embedded into the final HTML.  This keeps first implementation simple and
##   avoids relying on browser fetches against CNRFC XML endpoints.
## ============================================================================

pt_ops_live_cnrfc_forecast_points_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // CNRFC active river/reservoir forecast points.
  // --------------------------------------------------------------------------

  function ptCnrfcFpTrim(value) {
    if (value === null || value === undefined) return '';
    return String(value).trim();
  }

  function ptCnrfcFpBool(value) {
    if (value === true) return true;
    if (value === false || value === null || value === undefined) return false;
    var txt = String(value).trim().toLowerCase();
    return txt === 'true' || txt === 't' || txt === '1' || txt === 'yes' || txt === 'y';
  }

  function ptCnrfcFpNumber(value) {
    if (value === null || value === undefined || value === false || value === true) return null;
    if (typeof value === 'string') {
      value = value.trim();
      if (!value || ['NA', 'NULL', 'NAN'].indexOf(value.toUpperCase()) >= 0) return null;
    }
    var n = Number(value);
    return isFinite(n) ? n : null;
  }

  function ptCnrfcFpUrl(label, url) {
    url = ptCnrfcFpTrim(url);
    if (!url) return '';
    return '<div style="margin:1px 0;"><a href="' + escapeHtml(url) + '" target="_blank" rel="noopener">' + escapeHtml(label) + '</a></div>';
  }

  function ptCnrfcFpRoleLabel(p) {
    p = p || {};
    var role = ptCnrfcFpTrim(p.map_point_role);
    if (role === 'water_supply_index') return 'Water supply index';
    if (role === 'reservoir_inflow_and_release') return 'Reservoir deterministic inflow + planned release point';
    if (role === 'reservoir_inflow') return 'Reservoir deterministic inflow point';
    if (role === 'reservoir_release') return 'Reservoir planned release point';
    if (role === 'river_and_reservoir') return 'River/reservoir forecast point';
    if (role === 'ensemble_forecast_only') return 'River ensemble forecast point';
    if (role === 'river_forecast') return 'River deterministic forecast point';
    return 'CNRFC active forecast point';
  }

  function ptCnrfcFpEnsureStyle() {
    if (document.getElementById('pt-ops-cnrfc-fp-style')) return;

    var style = document.createElement('style');
    style.id = 'pt-ops-cnrfc-fp-style';
    style.innerHTML = `
      .pt-cnrfc-fp-divicon {
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .pt-cnrfc-fp-marker {
        position: relative;
        display: block;
        box-sizing: border-box;
        transform: none;
        margin: 0;
        border: 2px solid #0b2f4a;
        box-shadow: 0 1px 3px rgba(0,0,0,0.34);
      }
      .pt-cnrfc-fp-circle {
        width: 13px;
        height: 13px;
        border-radius: 50%;
      }
      .pt-cnrfc-fp-square {
        width: 15px;
        height: 15px;
        border-radius: 2px;
      }
      .pt-cnrfc-fp-index {
        width: 20px;
        height: 20px;
        border-radius: 3px;
        border-width: 3px;
        background: #ffd92f;
        border-color: #8e0152;
      }
      .pt-cnrfc-fp-index-central {
        width: 24px;
        height: 24px;
        border-width: 3px;
      }
      .pt-cnrfc-fp-river { background: #2b8cbe; border-color: #084081; }
      .pt-cnrfc-fp-reservoir { background: #7bccc4; border-color: #08589e; }
      .pt-cnrfc-fp-ensemble-only { background: #9e9ac8; border-color: #3f007d; }
      .pt-cnrfc-fp-halo {
        box-shadow: 0 0 0 4px rgba(35, 35, 35, 0.24), 0 1px 3px rgba(0,0,0,0.34);
      }
      .pt-cnrfc-fp-slot {
        position: absolute;
        left: 50%;
        transform: translateX(-50%);
        width: 5px;
        height: 5px;
        border-radius: 50%;
        background: #ffffff;
        border: 1px solid rgba(0,0,0,0.7);
        box-sizing: border-box;
      }
      .pt-cnrfc-fp-slot-inflow { top: 1px; }
      .pt-cnrfc-fp-slot-release { bottom: 1px; }
      .leaflet-tooltip.pt-ops-cnrfc-fp-tooltip {
        background: rgba(255,255,255,0.96);
        border: 1px solid rgba(0,0,0,0.42);
        border-radius: 4px;
        box-shadow: 0 2px 7px rgba(0,0,0,0.22);
        color: #111;
        padding: 5px 7px;
        font: 12px/1.25 Arial, Helvetica, sans-serif;
        white-space: pre;
      }
      .pt-ops-cnrfc-fp-popup {
        font: 12px/1.35 Arial, Helvetica, sans-serif;
        min-width: 235px;
        max-width: 365px;
      }
      .pt-ops-cnrfc-fp-popup a { color: #1f5e9c; text-decoration: none; }
      .pt-ops-cnrfc-fp-popup a:hover { text-decoration: underline; }
    `;
    document.head.appendChild(style);
  }

  function ptCnrfcFpIconHtml(p) {
    p = p || {};
    var role = ptCnrfcFpTrim(p.map_point_role);
    var id = ptCnrfcFpTrim(p.cnrfc_id).toUpperCase();
    var hasEnsemble = ptCnrfcFpBool(p.has_active_ensemble_forecast);
    var hasIndex = ptCnrfcFpBool(p.has_water_supply_index);
    var hasInflow = ptCnrfcFpBool(p.is_active_reservoir_inflow);
    var hasRelease = ptCnrfcFpBool(p.is_active_reservoir_release);

    var cls = ['pt-cnrfc-fp-marker'];

    if (hasIndex || role === 'water_supply_index') {
      cls.push('pt-cnrfc-fp-square', 'pt-cnrfc-fp-index', 'pt-cnrfc-fp-halo');
      if (id === 'MLIC0') cls.push('pt-cnrfc-fp-index-central');
    } else if (role === 'reservoir_inflow' || role === 'reservoir_release' || role === 'reservoir_inflow_and_release') {
      cls.push('pt-cnrfc-fp-square', 'pt-cnrfc-fp-reservoir');
      if (hasEnsemble) cls.push('pt-cnrfc-fp-halo');
    } else if (role === 'river_and_reservoir') {
      // Overlap records are symbolized by the active product actually present.
      // Most are reservoirs with inflow/release products; a small number remain river circles.
      if (hasInflow || hasRelease) {
        cls.push('pt-cnrfc-fp-square', 'pt-cnrfc-fp-reservoir');
      } else {
        cls.push('pt-cnrfc-fp-circle', 'pt-cnrfc-fp-river');
      }
      if (hasEnsemble) cls.push('pt-cnrfc-fp-halo');
    } else if (role === 'ensemble_forecast_only') {
      // Some active ensemble-only records are ordinary river points that are not
      // in the deterministic riverFcst XML.  Do not give them a separate purple
      // class; show them as river points with the normal ensemble halo unless
      // an inflow/release product says they should be reservoir squares.
      if (hasInflow || hasRelease) {
        cls.push('pt-cnrfc-fp-square', 'pt-cnrfc-fp-reservoir');
      } else {
        cls.push('pt-cnrfc-fp-circle', 'pt-cnrfc-fp-river');
      }
      cls.push('pt-cnrfc-fp-halo');
    } else {
      cls.push('pt-cnrfc-fp-circle', 'pt-cnrfc-fp-river');
      if (hasEnsemble) cls.push('pt-cnrfc-fp-halo');
    }

    var html = '<span class="' + cls.join(' ') + '">';
    if (hasInflow || hasRelease) {
      if (hasInflow) html += '<span class="pt-cnrfc-fp-slot pt-cnrfc-fp-slot-inflow"></span>';
      if (hasRelease) html += '<span class="pt-cnrfc-fp-slot pt-cnrfc-fp-slot-release"></span>';
    }
    html += '</span>';
    return html;
  }

  function ptCnrfcFpIcon(p) {
    var role = ptCnrfcFpTrim(p && p.map_point_role);
    var id = ptCnrfcFpTrim(p && p.cnrfc_id).toUpperCase();
    var isIndex = role === 'water_supply_index' || ptCnrfcFpBool(p && p.has_water_supply_index);
    var size = isIndex ? (id === 'MLIC0' ? 30 : 24) : 20;
    return L.divIcon({
      className: 'pt-cnrfc-fp-divicon',
      html: ptCnrfcFpIconHtml(p),
      iconSize: [size, size],
      iconAnchor: [size / 2, size / 2],
      popupAnchor: [0, -size / 2]
    });
  }

  function ptCnrfcFpPopupHtml(p) {
    p = p || {};
    var id = ptCnrfcFpTrim(p.cnrfc_id);
    var name = ptCnrfcFpTrim(p.display_name) || id || 'CNRFC forecast point';
    var roleLabel = ptCnrfcFpRoleLabel(p);
    var hasRiverDet = ptCnrfcFpBool(p.is_active_river_forecast);
    var hasResInflow = ptCnrfcFpBool(p.is_active_reservoir_inflow);
    var hasResRelease = ptCnrfcFpBool(p.is_active_reservoir_release);
    var hasEns = ptCnrfcFpBool(p.has_active_ensemble_forecast);
    var hasIndex = ptCnrfcFpBool(p.has_water_supply_index);
    var hasDet = hasRiverDet || hasResInflow || hasResRelease;
    var productSummary = hasIndex ? 'water supply index' : (hasDet && hasEns ? 'deterministic + ensemble' : (hasEns ? 'ensemble' : (hasDet ? 'deterministic' : 'active CNRFC product')));

    var html = '<div class="pt-ops-cnrfc-fp-popup">' +
      '<div><b>' + escapeHtml(id || name) + '</b></div>' +
      (name && name !== id ? '<div>' + escapeHtml(name) + '</div>' : '') +
      '<div><b>Type:</b> ' + escapeHtml(roleLabel) + '</div>' +
      '<div><b>Products:</b> ' + escapeHtml(productSummary) + '</div>';

    if (hasRiverDet && id) {
      html += ptCnrfcFpUrl('CNRFC 5-day deterministic', 'https://www.cnrfc.noaa.gov/graphicalRVF.php?id=' + encodeURIComponent(id));
    }
    if (hasResInflow) {
      html += ptCnrfcFpUrl('CNRFC 5-day deterministic inflow', 'https://www.cnrfc.noaa.gov/reservoir.php?id=' + encodeURIComponent(id));
    }
    if (hasResRelease) {
      html += ptCnrfcFpUrl('Planned release by facility operator', 'https://www.cnrfc.noaa.gov/reservoirRelease.php?id=' + encodeURIComponent(id));
    }
    if (hasEns && id) {
      var ensLabel = (hasResInflow || hasResRelease) ? 'CNRFC 10-day ensemble inflow' : 'CNRFC 10-day ensemble';
      html += ptCnrfcFpUrl(ensLabel, 'https://www.cnrfc.noaa.gov/ensembleProduct.php?id=' + encodeURIComponent(id) + '&prodID=3&days=10');
    }
    if (hasIndex) {
      html += ptCnrfcFpUrl('CNRFC water-supply index', ptCnrfcFpTrim(p.cnrfc_ensemble_url) || ptCnrfcFpTrim(p.water_supply_index_url));
      var area = ptCnrfcFpTrim(p.water_supply_index_area);
      if (area) html += '<div style="margin-top:3px;color:#555;">Symbolic BRIM point for ' + escapeHtml(area) + ' index.</div>';
    }

    html += '</div>';
    return html;
  }

  function ptCnrfcFpTooltipText(p) {
    p = p || {};
    var id = ptCnrfcFpTrim(p.cnrfc_id);
    var name = ptCnrfcFpTrim(p.display_name);
    var role = ptCnrfcFpRoleLabel(p);
    return [id || 'CNRFC', name || '', role].filter(function(x) { return !!x; }).join('\n');
  }

  function ptCnrfcFpLegendCounts() {
    var counts = window.ptCnrfcFpCounts || {};
    var visible = counts.visible;
    var total = counts.total;
    if (visible == null && total != null) visible = total;
    if (total == null && visible != null) total = visible;
    if (visible == null || total == null) return '';
    return 'Showing ' + Number(visible || 0).toLocaleString() + ' / ' + Number(total || 0).toLocaleString() + ' forecast points';
  }

  function ptCnrfcFpRefreshLegendCounts(counts) {
    if (counts) window.ptCnrfcFpCounts = counts;
    var el = document.getElementById('pt-cnrfc-fp-counts');
    if (!el) return;
    el.textContent = ptCnrfcFpLegendCounts();
  }

  function ptCnrfcFpLegendHtml() {
    return '<div class="pt-ops-map-legend-section">' +
      '<h4>CNRFC forecast points | river/reservoir</h4>' +
      '<div class="pt-ops-legend-small">Active CNRFC XML points. Halo = ensemble product available.</div>' +
      '<div class="pt-ops-legend-grid-2">' +
        '<div class="pt-ops-legend-textline"><span class="pt-ops-legend-circle" style="background:#2b8cbe;border-color:#084081;"></span>River forecast</div>' +
        '<div class="pt-ops-legend-textline"><span class="pt-ops-legend-circle" style="border-radius:0;background:#7bccc4;border-color:#08589e;"></span>Reservoir product</div>' +
        '<div class="pt-ops-legend-textline"><span class="pt-ops-legend-circle" style="border-radius:0;background:#ffd92f;border:3px solid #8e0152;"></span>Water supply index</div>' +
      '</div>' +
      '<div class="pt-ops-legend-small" style="margin-top:3px;">Reservoir square: upper dot = inflow product; lower dot = release product.</div>' +
      '<div class="pt-ops-legend-small" id="pt-cnrfc-fp-counts" style="margin-top:5px;border-top:1px solid rgba(0,0,0,0.12);padding-top:4px;">' + ptCnrfcFpLegendCounts() + '</div>' +
      '</div>';
  }

  var CnrfcRiverReservoirForecastLayer = L.Layer.extend({
    initialize: function(options) {
      this.options = options || {};
      this._layerGroup = null;
    },
    onAdd: function(mapObj) {
      ptCnrfcFpEnsureStyle();
      var name = this.options.name || 'CNRFC forecast points | river/reservoir';
      var records = this.options.records || [];
      var sourceUrl = this.options.sourceUrl || 'https://www.cnrfc.noaa.gov/';

      window.ptCnrfcFpCounts = { visible: records.length, total: records.length };
      activeLegendDefs[name] = {
        note: this.options.note || '',
        legendType: 'cnrfc_forecast_points',
        sourceUrl: sourceUrl,
        infoUrl: 'https://www.cnrfc.noaa.gov/',
        infoLabel: 'CNRFC'
      };
      redrawLegend();

      var group = L.layerGroup();
      var added = 0;

      records.forEach(function(p) {
        var lat = ptCnrfcFpNumber(p.lat);
        var lon = ptCnrfcFpNumber(p.lon);
        if (lat === null || lon === null || Math.abs(lat) > 90 || Math.abs(lon) > 180) return;

        var marker = L.marker([lat, lon], {
          icon: ptCnrfcFpIcon(p),
          keyboard: false,
          riseOnHover: true,
          // Do not set a native title attribute; it creates a second browser tooltip.
        });
        marker.bindTooltip(ptCnrfcFpTooltipText(p), {
          direction: 'top',
          sticky: false,
          offset: [0, -10],
          opacity: 0.92,
          className: 'pt-ops-cnrfc-fp-tooltip'
        });
        marker.on('mouseout', function() {
          try { marker.closeTooltip(); } catch(e) {}
        });
        marker.bindPopup(ptCnrfcFpPopupHtml(p), {
          maxWidth: 390,
          maxHeight: 520,
          autoPan: true,
          keepInView: true
        });
        group.addLayer(marker);
        added += 1;
      });

      this._layerGroup = group;
      group.addTo(mapObj);
      ptCnrfcFpRefreshLegendCounts({ visible: added, total: records.length });
      setOpsLayerLoading(name, false);
      recordStatus(name, 'Showing ' + added.toLocaleString() + ' of ' + records.length.toLocaleString() + ' active CNRFC river/reservoir forecast points.', 'pt-ops-ok');
    },
    onRemove: function(mapObj) {
      var name = this.options.name || 'CNRFC forecast points | river/reservoir';
      if (this._layerGroup) {
        try { mapObj.removeLayer(this._layerGroup); } catch(e) {}
        this._layerGroup = null;
      }
      delete activeLegendDefs[name];
      redrawLegend();
      setOpsLayerLoading(name, false);
    },
    forceRemove: function(mapObj) {
      this.onRemove(mapObj);
    }
  });

)---"
}
