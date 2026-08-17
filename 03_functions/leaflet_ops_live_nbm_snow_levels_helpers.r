# ==== leaflet_ops_live_nbm_snow_levels_helpers.r ============================
##
## PURPOSE:
##   Browser-side Ops Live consumer for the public BRIM Live NOAA/NBM modeled
##   snow-level contour product.
##
## CONTRACT:
##   - no request or geometry at initial BRIM page load;
##   - fetch/validate the manifest only when the Ops row is enabled;
##   - fetch one content-addressed GeoJSON target at a time;
##   - retain at most four parsed/validated targets in an in-memory LRU;
##   - keep UTC cycle/valid/lead state exact and derive Pacific display text;
##   - own and idempotently remove all Leaflet/DOM/listener/timer resources.
## ============================================================================

pt_ops_live_nbm_snow_levels_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // Ops Live NOAA/NBM modeled snow-level contours
  // --------------------------------------------------------------------------

  var PT_SNOW_PRODUCT_NAME = 'NBM Snow Levels';
  var PT_SNOW_PRODUCT_ID = 'winter_storm_levels';
  var PT_SNOW_SOURCE_ID = 'nbm_snow_level';
  var PT_SNOW_CONTRACT_VERSION = '1.0.0';
  var PT_SNOW_PACIFIC_ZONE = 'America/Los_Angeles';
  var PT_SNOW_TARGET_CACHE_LIMIT = 4;
  var PT_SNOW_REFRESH_MS = 15 * 60 * 1000;
  var PT_SNOW_LEADS = [1, 6, 12, 18, 24, 30, 36, 42, 48, 60, 72];
  var PT_SNOW_DOMAIN = [-130, 30, -112, 44.5];
  var PT_SNOW_STYLE = {
    // Fixed 0--20,000 ft mapping: never rescale by target or cycle.
    levelColors: [
      '#2c5aa0', '#2e6fb0', '#3185bd', '#379bc5', '#45afcb',
      '#5fc1cf', '#7acfd0', '#98d8c8', '#b8ddb5', '#d5dfa0',
      '#ece08b', '#f2d273', '#f3bf5f', '#f2ab52', '#ee9448',
      '#e77d43', '#dd6740', '#d25240', '#c44142', '#b53245',
      '#a52347'
    ],
    minorWeight: 1.8,
    majorWeight: 2.6,
    haloColor: '#ffffff',
    minorHaloWeight: 4.2,
    majorHaloWeight: 5.0,
    opacity: 0.94,
    haloOpacity: 0.84,
    majorIntervalFt: 5000,
    labelMinZoom: 6,
    labelMinLengthM: 65000
  };

  var ptSnowTargetCache = new Map();
  var ptSnowLabelsVisible = true;

  function ptSnowNow() {
    return Date.now();
  }

  function ptSnowPerfNow() {
    return window.performance && typeof window.performance.now === 'function'
      ? window.performance.now()
      : Date.now();
  }

  function ptSnowFinite(value) {
    return typeof value === 'number' && isFinite(value);
  }

  function ptSnowDate(value, context) {
    var date = new Date(value);
    if (!value || isNaN(date.getTime())) {
      throw new Error((context || 'Timestamp') + ' is not valid UTC/Z time.');
    }
    if (String(value).slice(-1) !== 'Z') {
      throw new Error((context || 'Timestamp') + ' must use canonical UTC/Z.');
    }
    return date;
  }

  function ptSnowRequire(object, fields, context) {
    if (!object || typeof object !== 'object' || Array.isArray(object)) {
      throw new Error(context + ' must be an object.');
    }
    fields.forEach(function(field) {
      if (!Object.prototype.hasOwnProperty.call(object, field)) {
        throw new Error(context + ' is missing ' + field + '.');
      }
    });
  }

  function ptSnowSameArray(left, right) {
    if (!Array.isArray(left) || !Array.isArray(right) || left.length !== right.length) return false;
    return left.every(function(value, index) { return Number(value) === Number(right[index]); });
  }

  function ptSnowFormatPacific(value, compactZeroMinutes) {
    var date = ptSnowDate(value, 'Display timestamp');
    var parts = new Intl.DateTimeFormat('en-US', {
      timeZone: PT_SNOW_PACIFIC_ZONE,
      weekday: 'short',
      month: 'short',
      day: 'numeric',
      hour: 'numeric',
      minute: '2-digit',
      hour12: true,
      timeZoneName: 'short'
    }).formatToParts(date);
    var out = {};
    parts.forEach(function(part) {
      if (part.type !== 'literal') out[part.type] = part.value;
    });
    var clock = out.hour + (compactZeroMinutes && out.minute === '00' ? '' : ':' + out.minute);
    return [out.weekday, out.month, out.day].join(' ') + ' \u00b7 ' +
      clock + ' ' + out.dayPeriod + ' ' + out.timeZoneName;
  }

  function ptSnowUtcHour(value) {
    var date = ptSnowDate(value, 'UTC display timestamp');
    var hour = String(date.getUTCHours()).padStart(2, '0');
    var minute = String(date.getUTCMinutes()).padStart(2, '0');
    return hour + (minute === '00' ? '' : minute) + 'Z';
  }

  function ptSnowCycleShort(value) {
    var date = ptSnowDate(value, 'Cycle timestamp');
    return String(date.getUTCMonth() + 1).padStart(2, '0') + '/' +
      String(date.getUTCDate()).padStart(2, '0') + ' ' +
      String(date.getUTCHours()).padStart(2, '0') + 'Z';
  }

  function ptSnowLevelText(value) {
    return Number(value).toLocaleString('en-US') + ' ft';
  }

  function ptSnowPad(value, width) {
    return String(Math.trunc(Number(value))).padStart(width, '0');
  }

  function ptSnowBytesText(value) {
    value = Number(value) || 0;
    if (value < 1024) return value + ' B';
    if (value < 1024 * 1024) return (value / 1024).toFixed(1) + ' KB';
    return (value / (1024 * 1024)).toFixed(2) + ' MB';
  }

  function ptSnowBufferHex(buffer) {
    return Array.prototype.map.call(new Uint8Array(buffer), function(value) {
      return value.toString(16).padStart(2, '0');
    }).join('');
  }

  function ptSnowLevelColor(levelFt) {
    var index = Math.round(Number(levelFt) / 1000);
    index = Math.max(0, Math.min(PT_SNOW_STYLE.levelColors.length - 1, index));
    return PT_SNOW_STYLE.levelColors[index];
  }

  function ptSnowLegendGradient() {
    var last = PT_SNOW_STYLE.levelColors.length - 1;
    return 'linear-gradient(to right,' + PT_SNOW_STYLE.levelColors.map(function(color, index) {
      return color + ' ' + ((index / last) * 100).toFixed(1) + '%';
    }).join(',') + ')';
  }

  function ptSnowCacheGet(path) {
    if (!ptSnowTargetCache.has(path)) return null;
    var value = ptSnowTargetCache.get(path);
    ptSnowTargetCache.delete(path);
    ptSnowTargetCache.set(path, value);
    return value;
  }

  function ptSnowCacheSet(path, value) {
    if (ptSnowTargetCache.has(path)) ptSnowTargetCache.delete(path);
    ptSnowTargetCache.set(path, value);
    while (ptSnowTargetCache.size > PT_SNOW_TARGET_CACHE_LIMIT) {
      ptSnowTargetCache.delete(ptSnowTargetCache.keys().next().value);
    }
  }

  function ptSnowCacheClear() {
    ptSnowTargetCache.clear();
  }

  function ptSnowValidateTargetEntry(entry, index) {
    ptSnowRequire(entry, [
      'source_id', 'cycle_time_utc', 'valid_time_utc', 'valid_from_utc',
      'valid_through_utc', 'lead_hours', 'retrieval_url', 'inventory_url',
      'inventory_record', 'path', 'media_type', 'sha256', 'bytes',
      'feature_count', 'contour_levels_ft_msl', 'source_grid',
      'output_bbox_wgs84'
    ], 'Manifest target ' + index);

    if (entry.source_id !== PT_SNOW_SOURCE_ID) throw new Error('Target source identity is unsupported.');
    if (!/^nbm\/snow-level\/winter_storm_levels_nbm_snow_level_[0-9]{10}_f[0-9]{3}_[0-9a-f]{12}\.geojson$/.test(entry.path)) {
      throw new Error('Target path is not a safe content-addressed Snow Levels path.');
    }
    if (entry.media_type !== 'application/geo+json') throw new Error('Target media type is unsupported.');
    if (!/^[0-9a-f]{64}$/.test(entry.sha256)) throw new Error('Target SHA-256 is invalid.');
    var hashInName = entry.path.match(/_([0-9a-f]{12})\.geojson$/);
    if (!hashInName || entry.sha256.slice(0, 12) !== hashInName[1]) {
      throw new Error('Target filename and SHA-256 identity disagree.');
    }
    if (!Number.isInteger(entry.bytes) || entry.bytes < 1 ||
        !Number.isInteger(entry.feature_count) || entry.feature_count < 1) {
      throw new Error('Target byte or feature count is invalid.');
    }
    if (typeof entry.retrieval_url !== 'string' || !/^https:\/\//.test(entry.retrieval_url) ||
        typeof entry.inventory_url !== 'string' || !/^https:\/\//.test(entry.inventory_url) ||
        typeof entry.inventory_record !== 'string' || !entry.inventory_record) {
      throw new Error('Target retrieval provenance is invalid.');
    }

    var cycle = ptSnowDate(entry.cycle_time_utc, 'Target cycle_time_utc');
    var valid = ptSnowDate(entry.valid_time_utc, 'Target valid_time_utc');
    var validFrom = ptSnowDate(entry.valid_from_utc, 'Target valid_from_utc');
    var validThrough = ptSnowDate(entry.valid_through_utc, 'Target valid_through_utc');
    if (!Number.isInteger(entry.lead_hours) || PT_SNOW_LEADS.indexOf(entry.lead_hours) < 0) {
      throw new Error('Target lead_hours is outside the accepted forecast set.');
    }
    if (valid.getTime() !== cycle.getTime() + entry.lead_hours * 3600000) {
      throw new Error('Target cycle, lead, and valid time do not agree.');
    }
    if (validFrom.getTime() !== valid.getTime() - 3 * 3600000 ||
        validThrough.getTime() !== valid.getTime() + 3 * 3600000) {
      throw new Error('Target validity window is not the accepted plus/minus three hours.');
    }

    if (!Array.isArray(entry.contour_levels_ft_msl) || !entry.contour_levels_ft_msl.length ||
        entry.contour_levels_ft_msl.some(function(level) {
          return !Number.isInteger(level) || level < 0 || level > 20000 || level % 1000 !== 0;
        }) || new Set(entry.contour_levels_ft_msl).size !== entry.contour_levels_ft_msl.length) {
      throw new Error('Target contour levels are invalid.');
    }
    if (!Array.isArray(entry.output_bbox_wgs84) || entry.output_bbox_wgs84.length !== 4 ||
        entry.output_bbox_wgs84.some(function(value) { return !ptSnowFinite(value); }) ||
        entry.output_bbox_wgs84[0] >= entry.output_bbox_wgs84[2] ||
        entry.output_bbox_wgs84[1] >= entry.output_bbox_wgs84[3] ||
        entry.output_bbox_wgs84[0] < PT_SNOW_DOMAIN[0] - 0.000001 ||
        entry.output_bbox_wgs84[1] < PT_SNOW_DOMAIN[1] - 0.000001 ||
        entry.output_bbox_wgs84[2] > PT_SNOW_DOMAIN[2] + 0.000001 ||
        entry.output_bbox_wgs84[3] > PT_SNOW_DOMAIN[3] + 0.000001) {
      throw new Error('Target output bbox is outside the accepted display domain.');
    }
    ptSnowRequire(entry.source_grid, [
      'rows', 'columns', 'resolution_m', 'finite_coverage', 'min_m', 'max_m'
    ], 'Target source_grid');
    if (!Number.isInteger(entry.source_grid.rows) || entry.source_grid.rows < 1 ||
        !Number.isInteger(entry.source_grid.columns) || entry.source_grid.columns < 1 ||
        !ptSnowFinite(entry.source_grid.resolution_m) || entry.source_grid.resolution_m <= 0 ||
        !ptSnowFinite(entry.source_grid.finite_coverage) || entry.source_grid.finite_coverage < 0 ||
        entry.source_grid.finite_coverage > 1 || !ptSnowFinite(entry.source_grid.min_m) ||
        !ptSnowFinite(entry.source_grid.max_m) || entry.source_grid.min_m > entry.source_grid.max_m) {
      throw new Error('Target source-grid diagnostics are invalid.');
    }
    return entry;
  }

  function ptSnowBuildCycles(manifest) {
    var byCycle = Object.create(null);
    manifest.targets.forEach(function(entry) {
      if (!byCycle[entry.cycle_time_utc]) byCycle[entry.cycle_time_utc] = [];
      byCycle[entry.cycle_time_utc].push(entry);
    });
    var cycles = Object.keys(byCycle).sort(function(a, b) { return Date.parse(b) - Date.parse(a); })
      .map(function(cycleUtc) {
        var targets = byCycle[cycleUtc].slice().sort(function(a, b) {
          return Date.parse(a.valid_time_utc) - Date.parse(b.valid_time_utc);
        });
        var leads = targets.map(function(entry) { return entry.lead_hours; });
        if (!ptSnowSameArray(leads, PT_SNOW_LEADS)) {
          throw new Error('Retained cycle ' + cycleUtc + ' does not contain the exact 11-target forecast set.');
        }
        return {cycle_time_utc: cycleUtc, targets: targets};
      });
    if (cycles.length !== 2) throw new Error('Manifest must contain exactly two complete retained cycles.');
    if (cycles[0].cycle_time_utc !== manifest.cycle_time_utc) {
      throw new Error('Manifest current cycle does not match the newest retained cycle.');
    }
    return cycles;
  }

  function ptSnowValidateManifest(manifest) {
    ptSnowRequire(manifest, [
      'product_id', 'schema_version', 'contract_version', 'status', 'source',
      'domain', 'contour', 'freshness', 'cycle_time_utc', 'retrieval_time_utc',
      'publication_time_utc', 'target_count', 'diagnostics', 'targets'
    ], 'Snow Levels manifest');
    if (manifest.product_id !== PT_SNOW_PRODUCT_ID) throw new Error('Manifest product_id is unsupported.');
    if (manifest.schema_version !== PT_SNOW_CONTRACT_VERSION ||
        manifest.contract_version !== PT_SNOW_CONTRACT_VERSION) {
      throw new Error('Snow Levels manifest contract version is unsupported.');
    }
    if (['current', 'delayed_but_usable', 'stale_last_known_good', 'expired'].indexOf(manifest.status) < 0) {
      throw new Error('Manifest status is unsupported.');
    }
    ptSnowRequire(manifest.source, [
      'id', 'name', 'agency', 'parameter', 'definition', 'source_unit',
      'output_unit', 'product_url', 'retrieval_base_url',
      'alternate_retrieval_base_url'
    ], 'Manifest source');
    if (manifest.source.id !== PT_SNOW_SOURCE_ID || manifest.source.parameter !== 'SNOWLVL' ||
        manifest.source.definition !== 'Elevation where wet-bulb temperature reaches 0.5 degrees C' ||
        manifest.source.output_unit !== 'ft above mean sea level') {
      throw new Error('Manifest NOAA/NBM SNOWLVL physical identity is unsupported.');
    }
    ptSnowRequire(manifest.domain, ['id', 'label', 'bbox_wgs84'], 'Manifest domain');
    if (manifest.domain.id !== 'winter_storm_levels_west_v1' ||
        !ptSnowSameArray(manifest.domain.bbox_wgs84, PT_SNOW_DOMAIN)) {
      throw new Error('Manifest display domain is unsupported.');
    }
    ptSnowRequire(manifest.contour, [
      'geometry', 'datum', 'unit', 'minimum_ft', 'maximum_ft', 'interval_ft',
      'simplify_tolerance_m'
    ], 'Manifest contour');
    if (manifest.contour.geometry !== 'LineString' || manifest.contour.datum !== 'mean_sea_level' ||
        manifest.contour.unit !== 'ft_msl' || Number(manifest.contour.minimum_ft) !== 0 ||
        Number(manifest.contour.maximum_ft) !== 20000 || Number(manifest.contour.interval_ft) !== 1000) {
      throw new Error('Manifest contour contract is unsupported.');
    }
    ptSnowRequire(manifest.freshness, [
      'current_after_hours', 'delayed_after_hours', 'expire_after_hours',
      'valid_tolerance_hours'
    ], 'Manifest freshness');
    if (Number(manifest.freshness.current_after_hours) !== 9 ||
        Number(manifest.freshness.delayed_after_hours) !== 15 ||
        Number(manifest.freshness.expire_after_hours) !== 24 ||
        Number(manifest.freshness.valid_tolerance_hours) !== 3) {
      throw new Error('Manifest freshness thresholds are unsupported.');
    }
    ptSnowDate(manifest.cycle_time_utc, 'Manifest cycle_time_utc');
    ptSnowDate(manifest.retrieval_time_utc, 'Manifest retrieval_time_utc');
    if (manifest.publication_time_utc !== null) {
      throw new Error('Manifest publication_time_utc must remain null under contract 1.0.0.');
    }
    if (!Array.isArray(manifest.targets) || manifest.target_count !== manifest.targets.length ||
        manifest.target_count !== 22) {
      throw new Error('Manifest must reference exactly 22 retained targets.');
    }
    ptSnowRequire(manifest.diagnostics, [
      'expected_current_cycle_target_count', 'actual_current_cycle_target_count',
      'retained_cycle_count', 'complete_bundle_validated'
    ], 'Manifest diagnostics');
    if (manifest.diagnostics.expected_current_cycle_target_count !== 11 ||
        manifest.diagnostics.actual_current_cycle_target_count !== 11 ||
        manifest.diagnostics.retained_cycle_count !== 2 ||
        manifest.diagnostics.complete_bundle_validated !== true) {
      throw new Error('Manifest does not declare a complete validated two-cycle bundle.');
    }
    var identities = Object.create(null);
    manifest.targets.forEach(function(entry, index) {
      ptSnowValidateTargetEntry(entry, index);
      var key = entry.cycle_time_utc + '|' + entry.valid_time_utc;
      if (identities[key]) throw new Error('Manifest repeats a cycle/valid target identity.');
      identities[key] = true;
    });
    manifest._ptSnowCycles = ptSnowBuildCycles(manifest);
    return manifest;
  }

  function ptSnowFreshness(manifest, nowMs) {
    nowMs = Number(nowMs);
    var currentCycle = manifest._ptSnowCycles ? manifest._ptSnowCycles[0] : ptSnowBuildCycles(manifest)[0];
    var cycleAgeHours = (nowMs - Date.parse(manifest.cycle_time_utc)) / 3600000;
    var hasActiveTarget = currentCycle.targets.some(function(entry) {
      return nowMs >= Date.parse(entry.valid_from_utc) && nowMs <= Date.parse(entry.valid_through_utc);
    });
    if (!hasActiveTarget || cycleAgeHours > Number(manifest.freshness.expire_after_hours)) return 'expired';
    if (cycleAgeHours > Number(manifest.freshness.delayed_after_hours)) return 'stale_last_known_good';
    if (cycleAgeHours > Number(manifest.freshness.current_after_hours)) return 'delayed_but_usable';
    return 'current';
  }

  function ptSnowDefaultTargetIndex(targets, nowMs) {
    var active = targets.map(function(entry, index) { return {entry: entry, index: index}; })
      .filter(function(item) {
        return nowMs >= Date.parse(item.entry.valid_from_utc) &&
          nowMs <= Date.parse(item.entry.valid_through_utc);
      })
      .sort(function(left, right) {
        var delta = Math.abs(Date.parse(left.entry.valid_time_utc) - nowMs) -
          Math.abs(Date.parse(right.entry.valid_time_utc) - nowMs);
        return delta || left.index - right.index;
      });
    return active.length ? active[0].index : -1;
  }

  function ptSnowClosestTargetIndex(targets, desiredValidUtc, nowMs) {
    if (desiredValidUtc) {
      var exact = targets.findIndex(function(entry) { return entry.valid_time_utc === desiredValidUtc; });
      if (exact >= 0) return exact;
      return targets.map(function(entry, index) { return {entry: entry, index: index}; })
        .sort(function(left, right) {
          return Math.abs(Date.parse(left.entry.valid_time_utc) - Date.parse(desiredValidUtc)) -
            Math.abs(Date.parse(right.entry.valid_time_utc) - Date.parse(desiredValidUtc));
        })[0].index;
    }
    var active = ptSnowDefaultTargetIndex(targets, nowMs);
    if (active >= 0) return active;
    return targets.map(function(entry, index) { return {entry: entry, index: index}; })
      .sort(function(left, right) {
        return Math.abs(Date.parse(left.entry.valid_time_utc) - nowMs) -
          Math.abs(Date.parse(right.entry.valid_time_utc) - nowMs);
      })[0].index;
  }

  function ptSnowValidateGeoJson(payload, entry) {
    ptSnowRequire(payload, ['type', 'contract_version', 'bbox', 'features'], 'Snow Levels GeoJSON');
    if (payload.type !== 'FeatureCollection' || payload.contract_version !== PT_SNOW_CONTRACT_VERSION) {
      throw new Error('Target root type or contract version is unsupported.');
    }
    if (!ptSnowSameArray(payload.bbox, entry.output_bbox_wgs84)) {
      throw new Error('Target root bbox does not match its manifest entry.');
    }
    if (!Array.isArray(payload.features) || payload.features.length !== entry.feature_count) {
      throw new Error('Target feature_count does not match its manifest entry.');
    }
    var ids = Object.create(null);
    var levels = Object.create(null);
    var vertices = 0;
    payload.features.forEach(function(feature, index) {
      ptSnowRequire(feature, ['type', 'id', 'properties', 'geometry'], 'Target feature ' + index);
      if (feature.type !== 'Feature' || !/^[0-9]{10}_f[0-9]{3}_[0-9]{5}_[0-9]{3}$/.test(feature.id) || ids[feature.id]) {
        throw new Error('Target feature ID is invalid or duplicated.');
      }
      ids[feature.id] = true;
      var p = feature.properties;
      ptSnowRequire(p, [
        'product_id', 'source_id', 'parameter', 'definition', 'level_ft_msl',
        'label', 'unit', 'cycle_time_utc', 'valid_time_utc', 'lead_hours',
        'segment', 'length_m'
      ], 'Target feature properties');
      if (p.product_id !== PT_SNOW_PRODUCT_ID || p.source_id !== PT_SNOW_SOURCE_ID ||
          p.parameter !== 'snow_level' ||
          p.definition !== 'height of the wet-bulb 0.5 degree C surface' || p.unit !== 'ft_msl') {
        throw new Error('Target feature physical identity is invalid.');
      }
      if (p.cycle_time_utc !== entry.cycle_time_utc || p.valid_time_utc !== entry.valid_time_utc ||
          Number(p.lead_hours) !== Number(entry.lead_hours)) {
        throw new Error('Target feature time identity is invalid.');
      }
      if (!Number.isInteger(p.level_ft_msl) || p.level_ft_msl < 0 ||
          p.level_ft_msl > 20000 || p.level_ft_msl % 1000 !== 0 ||
          typeof p.label !== 'string' || !p.label || !Number.isInteger(p.segment) ||
          p.segment < 1 || !ptSnowFinite(p.length_m) || p.length_m < 0) {
        throw new Error('Target feature contour attributes are invalid.');
      }
      var expectedId = entry.cycle_time_utc.replace(/[-:T]/g, '').slice(0, 10) +
        '_f' + ptSnowPad(entry.lead_hours, 3) + '_' + ptSnowPad(p.level_ft_msl, 5) +
        '_' + ptSnowPad(p.segment, 3);
      if (feature.id !== expectedId) throw new Error('Target feature ID does not match its time/elevation/segment identity.');
      levels[p.level_ft_msl] = true;
      if (!feature.geometry || feature.geometry.type !== 'LineString' ||
          !Array.isArray(feature.geometry.coordinates) || feature.geometry.coordinates.length < 2) {
        throw new Error('Target feature geometry is not a usable LineString.');
      }
      feature.geometry.coordinates.forEach(function(coord) {
        if (!Array.isArray(coord) || coord.length < 2 || !ptSnowFinite(coord[0]) || !ptSnowFinite(coord[1]) ||
            coord[0] < PT_SNOW_DOMAIN[0] - 0.000001 || coord[0] > PT_SNOW_DOMAIN[2] + 0.000001 ||
            coord[1] < PT_SNOW_DOMAIN[1] - 0.000001 || coord[1] > PT_SNOW_DOMAIN[3] + 0.000001) {
          throw new Error('Target coordinate is invalid or outside the accepted domain.');
        }
        vertices += 1;
      });
    });
    var emitted = Object.keys(levels).map(Number).sort(function(a, b) { return a - b; });
    var declared = entry.contour_levels_ft_msl.slice().sort(function(a, b) { return a - b; });
    if (!ptSnowSameArray(emitted, declared)) throw new Error('Target emitted contour levels do not match the manifest.');
    return {featureCount: payload.features.length, vertexCount: vertices};
  }

  function ptSnowInstallCss() {
    if (document.getElementById('pt-ops-nbm-snow-levels-style')) return;
    var style = document.createElement('style');
    style.id = 'pt-ops-nbm-snow-levels-style';
    style.textContent = `
      .pt-ops-nbm-snow-card{width:360px;max-width:min(360px,calc(100vw - 24px));max-height:min(610px,calc(100vh - 24px));overflow:hidden;background:rgba(240,247,245,.98)!important;border:1px solid rgba(54,84,86,.52)!important;border-radius:8px;box-shadow:0 2px 9px rgba(0,0,0,.26);color:#202a2e;font:12px/1.28 Arial,Helvetica,sans-serif}
      .pt-ops-nbm-snow-card-head{display:flex;align-items:flex-start;justify-content:space-between;gap:8px;padding:8px 9px 6px;border-bottom:1px solid rgba(54,84,86,.20)}
      .pt-ops-nbm-snow-card-title{font-size:14px;font-weight:700}.pt-ops-nbm-snow-card-subtitle{color:#586b70;font-size:10.5px;margin-top:1px}
      .pt-ops-nbm-snow-card-body{max-height:min(548px,calc(100vh - 82px));overflow:auto;padding:8px 9px 9px}
      .pt-ops-nbm-snow-state{margin:0 0 7px;padding:6px 7px;border:1px solid;border-radius:5px;font-size:11px;font-weight:700}
      .pt-ops-nbm-snow-state-current{color:#175538;background:#eef8f1;border-color:#8eb9a0}.pt-ops-nbm-snow-state-loading{color:#704900;background:#fff5dc;border-color:#d2b16c}
      .pt-ops-nbm-snow-state-delayed,.pt-ops-nbm-snow-state-previous{color:#77500d;background:#fff8e8;border-color:#d8ba79}.pt-ops-nbm-snow-state-stale,.pt-ops-nbm-snow-state-expired,.pt-ops-nbm-snow-state-error{color:#862c2b;background:#fff0ef;border-color:#d8a3a0}
      .pt-ops-nbm-snow-time{padding:8px;border:1px solid rgba(54,84,86,.24);border-radius:6px;background:rgba(255,255,255,.72)}
      .pt-ops-nbm-snow-kicker{color:#66757a;font-size:9.5px;font-weight:700;letter-spacing:.07em;text-transform:uppercase}.pt-ops-nbm-snow-valid{font-size:18px;font-weight:750;margin-top:1px}
      .pt-ops-nbm-snow-valid-utc,.pt-ops-nbm-snow-cycle-line{color:#617076;font-size:10.5px;margin-top:1px}.pt-ops-nbm-snow-cycle-line{margin:5px 0 4px}
      .pt-ops-nbm-snow-cycle-select,.pt-ops-nbm-snow-target-select{width:100%;min-width:0;height:30px;border:1px solid #aab8bc;border-radius:4px;background:#fff;color:#202a2e;font:11px Arial,Helvetica,sans-serif}
      .pt-ops-nbm-snow-nav{display:grid;grid-template-columns:32px 1fr 32px;gap:4px;margin-top:5px}.pt-ops-nbm-snow-nav button{height:30px;margin:0!important;padding:0!important;font-size:17px!important}
      .pt-ops-nbm-snow-controls{display:flex;align-items:center;justify-content:space-between;gap:8px;margin-top:8px}.pt-ops-nbm-snow-controls label{display:inline-flex;align-items:center;margin:0!important;font-size:11px}
      .pt-ops-nbm-snow-legend{margin-top:8px;padding:7px 8px;border:1px solid rgba(54,84,86,.22);border-radius:5px;background:rgba(255,255,255,.62)}
      .pt-ops-nbm-snow-legend-title{font-weight:700;margin-bottom:5px}.pt-ops-nbm-snow-ramp{height:9px;border:1px solid rgba(42,57,63,.46);border-radius:3px;box-shadow:0 0 0 1px rgba(255,255,255,.76)}
      .pt-ops-nbm-snow-ramp-ticks{display:grid;grid-template-columns:repeat(5,1fr);margin-top:3px;color:#4d5d62;font-size:9.5px}.pt-ops-nbm-snow-ramp-ticks span{text-align:center}.pt-ops-nbm-snow-ramp-ticks span:first-child{text-align:left}.pt-ops-nbm-snow-ramp-ticks span:last-child{text-align:right}.pt-ops-nbm-snow-interval{margin-top:4px;color:#5d6c71;font-size:9.5px}
      .pt-ops-nbm-snow-note{margin-top:7px;color:#58686d;font-size:10.5px}.pt-ops-nbm-snow-details{margin-top:7px;padding-top:6px;border-top:1px solid rgba(54,84,86,.18);font-size:10px;color:#596a70}.pt-ops-nbm-snow-details summary{cursor:pointer;color:#315d6e;font-weight:700}
      .pt-ops-nbm-snow-metrics{margin-top:5px;overflow-wrap:anywhere}.pt-nbm-snow-label-wrap{background:transparent;border:0}.pt-nbm-snow-label{display:inline-block;transform:translate(-50%,-50%);padding:1px 3px;border:1px solid rgba(76,94,105,.48);border-radius:2px;color:#203e4d;background:rgba(255,255,255,.88);box-shadow:0 1px 2px rgba(0,0,0,.18);font:700 10px/1.1 Arial,Helvetica,sans-serif;white-space:nowrap}
      .leaflet-tooltip.pt-nbm-snow-tooltip{box-sizing:border-box;width:max-content;min-width:236px;max-width:300px;padding:6px 8px;white-space:normal;font:11px/1.24 Arial,Helvetica,sans-serif;font-variant-numeric:tabular-nums}.pt-nbm-snow-tooltip-title{color:#20343d;font-size:13px;font-weight:700;line-height:1.15;margin-bottom:3px}.pt-nbm-snow-tooltip-row{display:grid;grid-template-columns:30px auto;align-items:baseline;column-gap:6px;color:#405158;white-space:nowrap}.pt-nbm-snow-tooltip-row+.pt-nbm-snow-tooltip-row{margin-top:1px}.pt-nbm-snow-tooltip-label{color:#69777c;font-weight:600}.pt-nbm-snow-tooltip-value{color:#35484f}
      .pt-nbm-snow-popup{min-width:240px;max-width:320px;font:11px/1.32 Arial,Helvetica,sans-serif}.pt-nbm-snow-popup-kicker{color:#586970;font-size:10px;font-weight:700;letter-spacing:.04em;text-transform:uppercase}.pt-nbm-snow-popup-title{font-size:16px;font-weight:700;margin:1px 0 6px}.pt-nbm-snow-popup-row{margin-top:5px}.pt-nbm-snow-popup-label{font-weight:700}.pt-nbm-snow-popup-secondary{color:#68777c;font-size:10px}.pt-nbm-snow-popup-note{margin-top:7px;padding-top:6px;border-top:1px solid #d5dddd;color:#5d6b70}
      @media(max-width:720px){.pt-ops-nbm-snow-card{width:calc(100vw - 16px);max-width:calc(100vw - 16px);max-height:58vh}.pt-ops-nbm-snow-card-body{max-height:calc(58vh - 48px)}}
    `;
    document.head.appendChild(style);
  }

  ptSnowInstallCss();

  function ptSnowContourStyle(feature, halo) {
    var major = Number(feature.properties.level_ft_msl) % PT_SNOW_STYLE.majorIntervalFt === 0;
    return {
      pane: 'pane_ops',
      color: halo ? PT_SNOW_STYLE.haloColor : ptSnowLevelColor(feature.properties.level_ft_msl),
      opacity: halo ? PT_SNOW_STYLE.haloOpacity : PT_SNOW_STYLE.opacity,
      weight: halo
        ? (major ? PT_SNOW_STYLE.majorHaloWeight : PT_SNOW_STYLE.minorHaloWeight)
        : (major ? PT_SNOW_STYLE.majorWeight : PT_SNOW_STYLE.minorWeight),
      interactive: !halo,
      lineCap: 'round',
      lineJoin: 'round'
    };
  }

  function ptSnowFeatureHtml(feature, entry, popup) {
    var level = feature && feature.properties ? feature.properties.level_ft_msl : null;
    var title = ptSnowLevelText(level) + ' MSL';
    if (!popup) {
      return '<div class="pt-nbm-snow-tooltip-title">' + escapeHtml(title) + '</div>' +
        '<div class="pt-nbm-snow-tooltip-row"><span class="pt-nbm-snow-tooltip-label">Valid</span>' +
        '<span class="pt-nbm-snow-tooltip-value">' + escapeHtml(ptSnowFormatPacific(entry.valid_time_utc, true)) + '</span></div>' +
        '<div class="pt-nbm-snow-tooltip-row"><span class="pt-nbm-snow-tooltip-label">Run</span>' +
        '<span class="pt-nbm-snow-tooltip-value">' + escapeHtml(ptSnowFormatPacific(entry.cycle_time_utc, true)) +
        ' \u00b7 +' + escapeHtml(entry.lead_hours) + ' h</span></div>';
    }
    return '<div class="pt-nbm-snow-popup">' +
      '<div class="pt-nbm-snow-popup-kicker">Modeled snow level</div>' +
      '<div class="pt-nbm-snow-popup-title">' + escapeHtml(title) + '</div>' +
      '<div class="pt-nbm-snow-popup-row"><div class="pt-nbm-snow-popup-label">Valid</div>' +
        escapeHtml(ptSnowFormatPacific(entry.valid_time_utc)) +
        '<div class="pt-nbm-snow-popup-secondary">' + escapeHtml(ptSnowUtcHour(entry.valid_time_utc)) + '</div></div>' +
      '<div class="pt-nbm-snow-popup-row"><div class="pt-nbm-snow-popup-label">NBM run</div>' +
        escapeHtml(ptSnowFormatPacific(entry.cycle_time_utc)) +
        '<div class="pt-nbm-snow-popup-secondary">' + escapeHtml(ptSnowUtcHour(entry.cycle_time_utc)) + '</div></div>' +
      '<div class="pt-nbm-snow-popup-row"><span class="pt-nbm-snow-popup-label">Forecast lead</span> +' +
        escapeHtml(entry.lead_hours) + ' h</div>' +
      '<div class="pt-nbm-snow-popup-row"><span class="pt-nbm-snow-popup-label">Source</span> NOAA / National Blend of Models</div>' +
      '<div class="pt-nbm-snow-popup-note">Modeled snow-level elevation; not an observed snow level.</div></div>';
  }

  function ptSnowMidpoint(coordinates) {
    var lengths = [];
    var total = 0;
    for (var i = 1; i < coordinates.length; i += 1) {
      var dx = (coordinates[i][0] - coordinates[i - 1][0]) *
        Math.cos((coordinates[i][1] + coordinates[i - 1][1]) * Math.PI / 360);
      var dy = coordinates[i][1] - coordinates[i - 1][1];
      var length = Math.sqrt(dx * dx + dy * dy);
      lengths.push(length);
      total += length;
    }
    var halfway = total / 2;
    var walked = 0;
    for (var j = 0; j < lengths.length; j += 1) {
      if (walked + lengths[j] >= halfway) {
        var ratio = lengths[j] ? (halfway - walked) / lengths[j] : 0;
        return [
          coordinates[j][0] + (coordinates[j + 1][0] - coordinates[j][0]) * ratio,
          coordinates[j][1] + (coordinates[j + 1][1] - coordinates[j][1]) * ratio
        ];
      }
      walked += lengths[j];
    }
    return coordinates[Math.floor(coordinates.length / 2)];
  }

  var PtOpsNbmSnowLevelsLayer = L.Layer.extend({
    initialize: function(options) {
      this.options = options || {};
      this._map = null;
      this._removed = true;
      this._manifest = null;
      this._cycles = [];
      this._freshness = null;
      this._displayGroup = null;
      this._labelGroup = null;
      this._displayedEntry = null;
      this._displayedGeoJson = null;
      this._timeState = {cycle_utc: null, valid_time_utc: null, lead_hours: null};
      this._cycleIndex = 0;
      this._targetIndex = -1;
      this._requestToken = 0;
      this._manifestAbort = null;
      this._targetAbort = null;
      this._refreshTimer = null;
      this._visibilityHandler = null;
      this._mapMoveHandler = null;
      this._cardControl = null;
      this._card = null;
      this._detachable = null;
      this._ui = {};
      this._labelsVisible = ptSnowLabelsVisible;
      this._lastMetrics = null;
      this._diagnostics = {manifestLoads: [], targetLoads: [], errors: [], activations: 0, removals: 0};
    },

    getTimeState: function() {
      return {
        cycle_utc: this._timeState.cycle_utc,
        valid_time_utc: this._timeState.valid_time_utc,
        lead_hours: this._timeState.lead_hours
      };
    },

    getSelectionState: function() {
      var state = this.getTimeState();
      return {
        cycle_utc: state.cycle_utc,
        valid_time_utc: state.valid_time_utc,
        lead_hours: state.lead_hours,
        cycle_index: this._targetIndex >= 0 ? this._cycleIndex : null,
        target_index: this._targetIndex >= 0 ? this._targetIndex : null,
        cycle_role: this._targetIndex < 0 ? null :
          (this._cycleIndex === 0 ? 'current' : (this._cycleIndex === 1 ? 'previous' : 'retained'))
      };
    },

    _emitTimeSelection: function(reason) {
      if (typeof window.CustomEvent !== 'function' || typeof window.dispatchEvent !== 'function') return;
      var detail = this.getSelectionState();
      detail.product_id = PT_SNOW_PRODUCT_ID;
      detail.reason = reason || 'target selection';
      window.dispatchEvent(new window.CustomEvent('brim:nbm-time-selection', {detail: detail}));
    },

    selectTarget: function(cycleIndex, targetIndex, reason) {
      return this._requestTarget(cycleIndex, targetIndex, reason || 'controller target selection');
    },

    stepValidTime: function(delta) {
      delta = Number(delta);
      if (!Number.isInteger(delta) || delta === 0 || this._targetIndex < 0) return false;
      var targetIndex = this._targetIndex + delta;
      var cycle = this._cycles[this._cycleIndex];
      if (!cycle || targetIndex < 0 || targetIndex >= cycle.targets.length) return false;
      return this.selectTarget(this._cycleIndex, targetIndex, 'step valid time');
    },

    selectCycle: function(cycleIndex) {
      return this._chooseCycle(Number(cycleIndex));
    },

    getDiagnostics: function() {
      return this._diagnostics;
    },

    _setBanner: function(kind, text) {
      if (!this._ui.banner) return;
      this._ui.banner.className = 'pt-ops-nbm-snow-state pt-ops-nbm-snow-state-' + kind;
      this._ui.banner.textContent = text;
    },

    _recordError: function(error) {
      this._diagnostics.errors.push({
        time_utc: new Date().toISOString(),
        message: String(error && error.message ? error.message : error)
      });
    },

    _createCard: function() {
      var self = this;
      this._cardControl = L.control({position: 'bottomleft'});
      this._cardControl.onAdd = function() {
        var div = L.DomUtil.create('div', 'pt-ops-nbm-snow-card pt-map-legend-card');
        div.innerHTML =
          '<div class="pt-ops-nbm-snow-card-head pt-map-card-handle">' +
            '<div><div class="pt-ops-nbm-snow-card-title">NBM Snow Levels</div>' +
            '<div class="pt-ops-nbm-snow-card-subtitle">NOAA/NBM modeled snow-level elevation</div></div>' +
            '<span class="pt-map-card-actions"><button type="button" class="pt-map-card-dock pt-ops-nbm-snow-dock" aria-label="Undock NBM Snow Levels card" title="Undock NBM Snow Levels card">&#x2197;</button>' +
            '<button type="button" class="pt-map-legend-close pt-ops-nbm-snow-close" aria-label="Hide NBM Snow Levels card" title="Hide NBM Snow Levels card">&times;</button></span>' +
          '</div>' +
          '<div class="pt-ops-nbm-snow-card-body">' +
            '<div class="pt-ops-nbm-snow-state pt-ops-nbm-snow-state-loading" role="status" aria-live="polite">Loading manifest\u2026</div>' +
            '<div class="pt-ops-nbm-snow-time">' +
              '<div class="pt-ops-nbm-snow-kicker">Forecast valid time \u00b7 Pacific</div>' +
              '<div class="pt-ops-nbm-snow-valid">\u2014</div>' +
              '<div class="pt-ops-nbm-snow-valid-utc">Valid UTC and lead unavailable</div>' +
              '<div class="pt-ops-nbm-snow-cycle-line">NBM cycle unavailable</div>' +
              '<select class="pt-ops-nbm-snow-cycle-select" aria-label="NBM cycle" disabled></select>' +
              '<div class="pt-ops-nbm-snow-nav">' +
                '<button type="button" class="pt-ops-nbm-snow-prev" aria-label="Previous forecast target" title="Previous forecast target" disabled>&#x2039;</button>' +
                '<select class="pt-ops-nbm-snow-target-select" aria-label="Forecast valid time" disabled></select>' +
                '<button type="button" class="pt-ops-nbm-snow-next" aria-label="Next forecast target" title="Next forecast target" disabled>&#x203a;</button>' +
              '</div>' +
            '</div>' +
            '<div class="pt-ops-nbm-snow-controls"><label><input type="checkbox" class="pt-ops-nbm-snow-label-toggle" checked> contour labels</label>' +
              '<button type="button" class="pt-ops-nbm-snow-refresh" title="Fetch and validate the current public manifest">Recheck feed</button></div>' +
            '<div class="pt-ops-nbm-snow-legend"><div class="pt-ops-nbm-snow-legend-title">Snow level (ft MSL)</div>' +
              '<div class="pt-ops-nbm-snow-ramp" aria-label="Fixed color scale from 0 to 20,000 feet MSL"></div>' +
              '<div class="pt-ops-nbm-snow-ramp-ticks"><span>0</span><span>5k</span><span>10k</span><span>15k</span><span>20k ft</span></div>' +
              '<div class="pt-ops-nbm-snow-interval">Fixed scale \u00b7 1,000-ft contours \u00b7 5,000-ft index emphasis</div></div>' +
            '<div class="pt-ops-nbm-snow-note">Modeled guidance, not an observed snow level, freezing level, precipitation type, snow depth, or accumulation. Useful alongside independently selected precipitation forecasts.</div>' +
            '<details class="pt-ops-nbm-snow-details"><summary>Source, timing, and load details</summary>' +
              '<div class="pt-ops-nbm-snow-source">NOAA/NWS/NCEP MDL \u00b7 deterministic NBM SNOWLVL \u00b7 wet-bulb 0.5 \u00b0C surface.</div>' +
              '<div class="pt-ops-nbm-snow-metrics">Waiting for the runtime feed.</div></details>' +
          '</div>';
        L.DomEvent.disableClickPropagation(div);
        L.DomEvent.disableScrollPropagation(div);
        self._card = div;
        self._captureUi();
        self._wireCard();
        return div;
      };
      this._cardControl.addTo(this._map);
      var shared = window.BRIM && window.BRIM.legendCloseout;
      if (shared && this._card) {
        this._detachable = shared.makeDetachable({
          card: this._card,
          map: this._map,
          handleSelector: '.pt-ops-nbm-snow-card-head',
          dockSelector: '.pt-ops-nbm-snow-dock',
          label: 'NBM Snow Levels card'
        });
      }
    },

    _captureUi: function() {
      if (!this._card) return;
      this._ui = {
        banner: this._card.querySelector('.pt-ops-nbm-snow-state'),
        valid: this._card.querySelector('.pt-ops-nbm-snow-valid'),
        validUtc: this._card.querySelector('.pt-ops-nbm-snow-valid-utc'),
        cycleLine: this._card.querySelector('.pt-ops-nbm-snow-cycle-line'),
        cycleSelect: this._card.querySelector('.pt-ops-nbm-snow-cycle-select'),
        targetSelect: this._card.querySelector('.pt-ops-nbm-snow-target-select'),
        previous: this._card.querySelector('.pt-ops-nbm-snow-prev'),
        next: this._card.querySelector('.pt-ops-nbm-snow-next'),
        labels: this._card.querySelector('.pt-ops-nbm-snow-label-toggle'),
        ramp: this._card.querySelector('.pt-ops-nbm-snow-ramp'),
        refresh: this._card.querySelector('.pt-ops-nbm-snow-refresh'),
        metrics: this._card.querySelector('.pt-ops-nbm-snow-metrics')
      };
      if (this._ui.ramp) this._ui.ramp.style.background = ptSnowLegendGradient();
      if (this._ui.labels) this._ui.labels.checked = this._labelsVisible;
    },

    _wireCard: function() {
      var self = this;
      var shared = window.BRIM && window.BRIM.legendCloseout;
      if (shared && this._card) {
        shared.wire(this._card, '.pt-ops-nbm-snow-close', function() {});
      }
      if (this._ui.refresh) this._ui.refresh.addEventListener('click', function() { self.refreshCurrentView(); });
      if (this._ui.labels) this._ui.labels.addEventListener('change', function() {
        if (window.ptNbmSnowLevelsSetLabelsVisible) {
          window.ptNbmSnowLevelsSetLabelsVisible(!!self._ui.labels.checked);
        } else {
          self.setLabelsVisible(!!self._ui.labels.checked);
        }
      });
      if (this._ui.cycleSelect) this._ui.cycleSelect.addEventListener('change', function() {
        self.selectCycle(Number(self._ui.cycleSelect.value));
      });
      if (this._ui.targetSelect) this._ui.targetSelect.addEventListener('change', function() {
        self.selectTarget(self._cycleIndex, Number(self._ui.targetSelect.value), 'valid-time selection');
      });
      if (this._ui.previous) this._ui.previous.addEventListener('click', function() {
        self.stepValidTime(-1);
      });
      if (this._ui.next) this._ui.next.addEventListener('click', function() {
        self.stepValidTime(1);
      });
    },

    _setControlsBusy: function(busy) {
      var hasTargets = this._cycles.length && this._cycleIndex >= 0;
      var targets = hasTargets ? this._cycles[this._cycleIndex].targets : [];
      if (this._ui.cycleSelect) this._ui.cycleSelect.disabled = !!busy || this._cycles.length < 2;
      if (this._ui.targetSelect) this._ui.targetSelect.disabled = !!busy || !targets.length;
      if (this._ui.previous) this._ui.previous.disabled = !!busy || this._targetIndex <= 0;
      if (this._ui.next) this._ui.next.disabled = !!busy || this._targetIndex < 0 || this._targetIndex >= targets.length - 1;
      if (this._ui.refresh) this._ui.refresh.disabled = !!busy;
    },

    _renderSelectors: function() {
      if (!this._ui.cycleSelect || !this._ui.targetSelect) return;
      this._ui.cycleSelect.innerHTML = '';
      this._cycles.forEach(function(cycle, index) {
        var option = document.createElement('option');
        option.value = String(index);
        option.textContent = (index === 0 ? 'Current run \u00b7 ' : 'Previous run \u00b7 ') +
          ptSnowCycleShort(cycle.cycle_time_utc);
        this._ui.cycleSelect.appendChild(option);
      }, this);
      this._ui.cycleSelect.value = String(this._cycleIndex);
      this._ui.targetSelect.innerHTML = '';
      var targets = this._cycles[this._cycleIndex] ? this._cycles[this._cycleIndex].targets : [];
      targets.forEach(function(entry, index) {
        var option = document.createElement('option');
        option.value = String(index);
        option.textContent = ptSnowFormatPacific(entry.valid_time_utc) + ' \u00b7 +' + entry.lead_hours + ' h';
        this._ui.targetSelect.appendChild(option);
      }, this);
      if (this._targetIndex >= 0) this._ui.targetSelect.value = String(this._targetIndex);
      this._setControlsBusy(false);
    },

    _updateIdentity: function(entry) {
      if (!entry) {
        if (this._ui.valid) this._ui.valid.textContent = '\u2014';
        if (this._ui.validUtc) this._ui.validUtc.textContent = 'Valid UTC and lead unavailable';
        if (this._ui.cycleLine) this._ui.cycleLine.textContent = 'NBM cycle unavailable';
        return;
      }
      if (this._ui.valid) this._ui.valid.textContent = ptSnowFormatPacific(entry.valid_time_utc);
      if (this._ui.validUtc) this._ui.validUtc.textContent =
        'Valid ' + ptSnowUtcHour(entry.valid_time_utc) + ' \u00b7 +' + entry.lead_hours + ' h';
      if (this._ui.cycleLine) this._ui.cycleLine.textContent =
        'NBM run: ' + ptSnowFormatPacific(entry.cycle_time_utc) + ' \u00b7 ' + ptSnowUtcHour(entry.cycle_time_utc);
    },

    _updateBanner: function() {
      if (this._cycleIndex === 1) {
        var priorFreshness = this._freshness === 'delayed_but_usable' ? 'Delayed' :
          (this._freshness === 'stale_last_known_good' ? 'Stale' :
          (this._freshness === 'expired' ? 'Expired' : 'Current'));
        this._setBanner('previous', 'Previous run selected \u00b7 Feed: ' + priorFreshness);
        return;
      }
      if (this._freshness === 'current') {
        this._setBanner('current', 'Current feed');
      } else if (this._freshness === 'delayed_but_usable') {
        this._setBanner('delayed', 'Delayed feed \u00b7 still usable under the upstream contract.');
      } else if (this._freshness === 'stale_last_known_good') {
        this._setBanner('stale', 'Stale last-known-good \u00b7 verify before operational use.');
      } else {
        this._setBanner('expired', 'Expired \u00b7 no contours are shown as current guidance.');
      }
    },

    _updateMetrics: function(metrics) {
      if (!this._ui.metrics || !metrics) return;
      this._ui.metrics.textContent =
        (metrics.cacheHit ? 'Validated memory cache' : 'Network ' + ptSnowBytesText(metrics.bytes)) +
        ' \u00b7 fetch ' + metrics.fetchMs.toFixed(1) + ' ms' +
        ' \u00b7 hash ' + metrics.hashMs.toFixed(1) + ' ms' +
        ' \u00b7 parse ' + metrics.parseMs.toFixed(1) + ' ms' +
        ' \u00b7 render ' + metrics.renderMs.toFixed(1) + ' ms' +
        ' \u00b7 switch ' + metrics.switchMs.toFixed(1) + ' ms' +
        ' \u00b7 ' + metrics.featureCount + ' lines / ' + metrics.vertexCount.toLocaleString('en-US') + ' vertices' +
        ' \u00b7 cache ' + ptSnowTargetCache.size + '/' + PT_SNOW_TARGET_CACHE_LIMIT + '.';
    },

    _targetUrl: function(entry) {
      return new URL(entry.path, this.options.manifestUrl).toString();
    },

    _fetchManifest: async function(token) {
      if (this._manifestAbort) {
        try { this._manifestAbort.abort(); } catch(e) {}
      }
      this._manifestAbort = typeof AbortController === 'function' ? new AbortController() : null;
      var start = ptSnowPerfNow();
      var response = await fetch(this.options.manifestUrl, {
        cache: 'no-store',
        signal: this._manifestAbort ? this._manifestAbort.signal : undefined
      });
      if (!response.ok) throw new Error('Manifest request failed with HTTP ' + response.status + '.');
      var buffer = await response.arrayBuffer();
      if (this._removed || token !== this._requestToken) return null;
      var parseStart = ptSnowPerfNow();
      var parsed;
      try {
        parsed = JSON.parse(new TextDecoder('utf-8').decode(buffer));
      } catch(error) {
        throw new Error('Manifest JSON is malformed.');
      }
      var manifest = ptSnowValidateManifest(parsed);
      var result = {
        manifest: manifest,
        bytes: buffer.byteLength,
        fetchMs: parseStart - start,
        parseMs: ptSnowPerfNow() - parseStart
      };
      this._diagnostics.manifestLoads.push({
        cycle_time_utc: manifest.cycle_time_utc,
        bytes: result.bytes,
        fetchMs: result.fetchMs,
        parseMs: result.parseMs
      });
      if (this._diagnostics.manifestLoads.length > 100) this._diagnostics.manifestLoads.shift();
      return result;
    },

    _fetchTarget: async function(entry) {
      var cached = ptSnowCacheGet(entry.path);
      if (cached) {
        return {
          payload: cached.payload,
          bytes: cached.bytes,
          featureCount: cached.featureCount,
          vertexCount: cached.vertexCount,
          cacheHit: true,
          fetchMs: 0,
          hashMs: 0,
          parseMs: 0
        };
      }
      if (this._targetAbort) {
        try { this._targetAbort.abort(); } catch(e) {}
      }
      this._targetAbort = typeof AbortController === 'function' ? new AbortController() : null;
      var fetchStart = ptSnowPerfNow();
      var response = await fetch(this._targetUrl(entry), {
        cache: 'force-cache',
        signal: this._targetAbort ? this._targetAbort.signal : undefined
      });
      if (!response.ok) throw new Error('Selected target request failed with HTTP ' + response.status + '.');
      var buffer = await response.arrayBuffer();
      var fetchMs = ptSnowPerfNow() - fetchStart;
      if (buffer.byteLength !== entry.bytes) throw new Error('Selected target byte count does not match the manifest.');
      if (!window.crypto || !window.crypto.subtle || typeof window.crypto.subtle.digest !== 'function') {
        throw new Error('This browser cannot verify the selected target SHA-256.');
      }
      var hashStart = ptSnowPerfNow();
      var digest = ptSnowBufferHex(await window.crypto.subtle.digest('SHA-256', buffer));
      var hashMs = ptSnowPerfNow() - hashStart;
      if (digest !== entry.sha256) throw new Error('Selected target SHA-256 does not match the manifest.');
      var parseStart = ptSnowPerfNow();
      var payload;
      try {
        payload = JSON.parse(new TextDecoder('utf-8').decode(buffer));
      } catch(error) {
        throw new Error('Selected target GeoJSON is malformed.');
      }
      var geometry = ptSnowValidateGeoJson(payload, entry);
      var parsed = {
        payload: payload,
        bytes: buffer.byteLength,
        featureCount: geometry.featureCount,
        vertexCount: geometry.vertexCount,
        cacheHit: false,
        fetchMs: fetchMs,
        hashMs: hashMs,
        parseMs: ptSnowPerfNow() - parseStart
      };
      ptSnowCacheSet(entry.path, parsed);
      return parsed;
    },

    _buildDisplayGroup: function(payload, entry) {
      var halo = L.geoJSON(payload, {
        pane: 'pane_ops',
        style: function(feature) { return ptSnowContourStyle(feature, true); },
        interactive: false
      });
      var core = L.geoJSON(payload, {
        pane: 'pane_ops',
        style: function(feature) { return ptSnowContourStyle(feature, false); },
        onEachFeature: function(feature, layer) {
          var baseStyle = ptSnowContourStyle(feature, false);
          layer.bindTooltip(ptSnowFeatureHtml(feature, entry, false), {
            className: 'pt-nbm-snow-tooltip', sticky: true, direction: 'top', opacity: 0.96
          });
          layer.bindPopup(ptSnowFeatureHtml(feature, entry, true), {maxWidth: 340});
          layer.on('mouseover', function() {
            layer.setStyle({weight: baseStyle.weight + 1.4, opacity: 1});
            if (typeof layer.bringToFront === 'function') layer.bringToFront();
          });
          layer.on('mouseout', function() { layer.setStyle(baseStyle); });
        }
      });
      return L.layerGroup([halo, core]);
    },

    _renderLabels: function() {
      if (!this._labelGroup || !this._map) return 0;
      this._labelGroup.clearLayers();
      if (this._removed || !this._labelsVisible || !this._displayedGeoJson ||
          this._map.getZoom() < PT_SNOW_STYLE.labelMinZoom) return 0;
      var zoom = this._map.getZoom();
      var perLevelLimit = zoom >= 8 ? 3 : (zoom >= 7 ? 2 : 1);
      var counts = Object.create(null);
      var occupied = [];
      var bounds = this._map.getBounds().pad(-0.03);
      var mapObj = this._map;
      var group = this._labelGroup;
      var candidates = this._displayedGeoJson.features.slice().sort(function(left, right) {
        var levelDiff = Number(left.properties.level_ft_msl) - Number(right.properties.level_ft_msl);
        if (levelDiff) return levelDiff;
        return Number(right.properties.length_m) - Number(left.properties.length_m) ||
          String(left.id).localeCompare(String(right.id));
      });
      var total = 0;
      candidates.forEach(function(feature) {
        var level = Number(feature.properties.level_ft_msl);
        counts[level] = counts[level] || 0;
        if (counts[level] >= perLevelLimit || Number(feature.properties.length_m) < PT_SNOW_STYLE.labelMinLengthM) return;
        var coordinate = ptSnowMidpoint(feature.geometry.coordinates);
        var latlng = L.latLng(coordinate[1], coordinate[0]);
        if (!bounds.contains(latlng)) return;
        var point = mapObj.latLngToContainerPoint(latlng);
        var width = ptSnowLevelText(level).length * 6 + 10;
        var box = {left: point.x - width / 2 - 8, right: point.x + width / 2 + 8, top: point.y - 12, bottom: point.y + 12};
        var overlap = occupied.some(function(other) {
          return !(box.right < other.left || box.left > other.right || box.bottom < other.top || box.top > other.bottom);
        });
        if (overlap) return;
        occupied.push(box);
        L.marker(latlng, {
          pane: 'pane_labels_poly',
          interactive: false,
          keyboard: false,
          icon: L.divIcon({
            className: 'pt-nbm-snow-label-wrap',
            html: '<span class="pt-nbm-snow-label">' + escapeHtml(ptSnowLevelText(level)) + '</span>',
            iconSize: [1, 1],
            iconAnchor: [0, 0]
          })
        }).addTo(group);
        counts[level] += 1;
        total += 1;
      });
      return total;
    },

    setLabelsVisible: function(show) {
      ptSnowLabelsVisible = !!show;
      this._labelsVisible = ptSnowLabelsVisible;
      if (this._ui.labels) this._ui.labels.checked = this._labelsVisible;
      Array.prototype.forEach.call(
        document.querySelectorAll('[data-pt-ops-action="nbm-snow-labels"]'),
        function(toggle) { toggle.checked = ptSnowLabelsVisible; }
      );
      this._renderLabels();
    },

    showCard: function() {
      if (this._card) {
        this._card.style.display = '';
        var shared = window.BRIM && window.BRIM.legendCloseout;
        if (shared && typeof shared.scheduleLayout === 'function') shared.scheduleLayout(this._card);
      }
    },

    _removeDisplayedField: function() {
      if (this._displayGroup && this._map) {
        try { this._map.removeLayer(this._displayGroup); } catch(e) {}
      }
      this._displayGroup = null;
      this._displayedEntry = null;
      this._displayedGeoJson = null;
      this._timeState = {cycle_utc: null, valid_time_utc: null, lead_hours: null};
      if (this._labelGroup) this._labelGroup.clearLayers();
    },

    _requestTarget: async function(cycleIndex, targetIndex, reason) {
      cycleIndex = Number(cycleIndex);
      targetIndex = Number(targetIndex);
      if (this._removed || !this._cycles[cycleIndex] ||
          targetIndex < 0 || targetIndex >= this._cycles[cycleIndex].targets.length) return;
      var entry = this._cycles[cycleIndex].targets[targetIndex];
      var token = ++this._requestToken;
      var switchStart = ptSnowPerfNow();
      this._setControlsBusy(true);
      this._setBanner('loading', 'Loading and validating ' + ptSnowFormatPacific(entry.valid_time_utc) + '\u2026');
      setOpsLayerLoading(PT_SNOW_PRODUCT_NAME, true);
      recordStatus(PT_SNOW_PRODUCT_NAME, 'Loading one selected content-addressed contour target\u2026', 'pt-ops-warn');
      var candidate = null;
      try {
        var fetched = await this._fetchTarget(entry);
        if (this._removed || token !== this._requestToken) return;
        var renderStart = ptSnowPerfNow();
        candidate = this._buildDisplayGroup(fetched.payload, entry);
        candidate.addTo(this._map);
        if (typeof window.requestAnimationFrame === 'function') {
          await new Promise(function(resolve) {
            window.requestAnimationFrame(function() { window.requestAnimationFrame(resolve); });
          });
        }
        if (this._removed || token !== this._requestToken) {
          try { this._map.removeLayer(candidate); } catch(cancelError) {}
          return;
        }
        var renderMs = ptSnowPerfNow() - renderStart;
        if (this._displayGroup) {
          try { this._map.removeLayer(this._displayGroup); } catch(e) {}
        }
        this._displayGroup = candidate;
        this._displayedEntry = entry;
        this._displayedGeoJson = fetched.payload;
        this._cycleIndex = cycleIndex;
        this._targetIndex = targetIndex;
        this._timeState = {
          cycle_utc: entry.cycle_time_utc,
          valid_time_utc: entry.valid_time_utc,
          lead_hours: entry.lead_hours
        };
        this._renderSelectors();
        this._updateIdentity(entry);
        var labels = this._renderLabels();
        var metrics = {
          reason: reason || 'target selection',
          path: entry.path,
          cacheHit: fetched.cacheHit,
          bytes: fetched.bytes,
          fetchMs: fetched.fetchMs,
          hashMs: fetched.hashMs,
          parseMs: fetched.parseMs,
          renderMs: renderMs,
          switchMs: ptSnowPerfNow() - switchStart,
          featureCount: fetched.featureCount,
          vertexCount: fetched.vertexCount,
          labelCount: labels
        };
        this._lastMetrics = metrics;
        this._diagnostics.targetLoads.push(metrics);
        if (this._diagnostics.targetLoads.length > 100) this._diagnostics.targetLoads.shift();
        this._updateMetrics(metrics);
        this._updateBanner();
        this._emitTimeSelection(reason);
        setOpsLayerLoading(PT_SNOW_PRODUCT_NAME, false);
        recordStatus(
          PT_SNOW_PRODUCT_NAME,
          (cycleIndex === 0 ? 'Current run' : 'Previous run') +
            ' \u00b7 valid ' + ptSnowFormatPacific(entry.valid_time_utc) +
            ' \u00b7 ' + entry.feature_count + ' contour lines.',
          cycleIndex === 0 && this._freshness === 'current' ? 'pt-ops-ok' : 'pt-ops-warn'
        );
      } catch(error) {
        if (this._removed || token !== this._requestToken || (error && error.name === 'AbortError')) return;
        if (candidate && candidate !== this._displayGroup && this._map) {
          try { this._map.removeLayer(candidate); } catch(candidateError) {}
        }
        this._recordError(error);
        var retained = !!this._displayedEntry;
        this._setBanner('error',
          (error && error.message ? error.message : String(error)) +
          (retained ? ' Previously validated contours remain displayed.' : ' No Snow Levels contours are displayed.'));
        if (retained) {
          this._cycleIndex = this._cycles.findIndex(function(cycle) {
            return cycle.cycle_time_utc === this._displayedEntry.cycle_time_utc;
          }, this);
          if (this._cycleIndex < 0) this._cycleIndex = 0;
          this._targetIndex = this._cycles[this._cycleIndex].targets.findIndex(function(candidate) {
            return candidate.path === this._displayedEntry.path;
          }, this);
          this._renderSelectors();
          this._updateIdentity(this._displayedEntry);
        } else {
          this._setControlsBusy(false);
        }
        setOpsLayerLoading(PT_SNOW_PRODUCT_NAME, false);
        recordStatus(PT_SNOW_PRODUCT_NAME,
          (error && error.message ? error.message : String(error)) +
            (retained ? ' Previously validated contours retained.' : ''), 'pt-ops-bad');
      }
    },

    _chooseCycle: function(cycleIndex) {
      if (this._removed || !this._cycles[cycleIndex]) return false;
      var desired = this._displayedEntry ? this._displayedEntry.valid_time_utc : null;
      var targetIndex = ptSnowClosestTargetIndex(this._cycles[cycleIndex].targets, desired, ptSnowNow());
      return this.selectTarget(cycleIndex, targetIndex, 'cycle selection');
    },

    _applyManifest: async function(loaded) {
      if (!loaded || this._removed) return;
      var priorEntry = this._displayedEntry;
      var priorCycleIndex = this._cycleIndex;
      this._manifest = loaded.manifest;
      this._cycles = loaded.manifest._ptSnowCycles;
      this._freshness = ptSnowFreshness(this._manifest, ptSnowNow());
      if (this._ui.metrics) {
        this._ui.metrics.textContent = 'Manifest ' + ptSnowBytesText(loaded.bytes) +
          ' \u00b7 fetch ' + loaded.fetchMs.toFixed(1) + ' ms \u00b7 parse/validate ' +
          loaded.parseMs.toFixed(1) + ' ms \u00b7 2 cycles / 22 targets.';
      }
      if (this._freshness === 'expired') {
        this._cycleIndex = 0;
        this._targetIndex = -1;
        this._renderSelectors();
        this._removeDisplayedField();
        this._updateIdentity(null);
        this._updateBanner();
        this._setControlsBusy(true);
        setOpsLayerLoading(PT_SNOW_PRODUCT_NAME, false);
        recordStatus(PT_SNOW_PRODUCT_NAME, 'Feed expired or has no currently active target; contours suppressed.', 'pt-ops-bad');
        return;
      }
      var desiredValid = priorEntry ? priorEntry.valid_time_utc : null;
      var cycleIndex = 0;
      if (priorEntry && priorCycleIndex === 1) {
        cycleIndex = this._cycles.findIndex(function(cycle) {
          return cycle.cycle_time_utc === priorEntry.cycle_time_utc;
        });
        if (cycleIndex < 0) cycleIndex = Math.min(1, this._cycles.length - 1);
      }
      var targetIndex = priorEntry
        ? ptSnowClosestTargetIndex(this._cycles[cycleIndex].targets, desiredValid, ptSnowNow())
        : ptSnowDefaultTargetIndex(this._cycles[0].targets, ptSnowNow());
      if (targetIndex < 0) {
        this._removeDisplayedField();
        this._setBanner('expired', 'No currently displayable forecast target. Contours are suppressed.');
        setOpsLayerLoading(PT_SNOW_PRODUCT_NAME, false);
        recordStatus(PT_SNOW_PRODUCT_NAME, 'No currently displayable forecast target.', 'pt-ops-bad');
        return;
      }
      this._cycleIndex = cycleIndex;
      this._targetIndex = targetIndex;
      this._renderSelectors();
      var selected = this._cycles[cycleIndex].targets[targetIndex];
      if (priorEntry && priorEntry.path === selected.path && this._displayGroup) {
        this._timeState = {
          cycle_utc: selected.cycle_time_utc,
          valid_time_utc: selected.valid_time_utc,
          lead_hours: selected.lead_hours
        };
        this._updateIdentity(selected);
        this._updateBanner();
        this._setControlsBusy(false);
        setOpsLayerLoading(PT_SNOW_PRODUCT_NAME, false);
        recordStatus(PT_SNOW_PRODUCT_NAME, 'Manifest revalidated; displayed immutable target is unchanged.', 'pt-ops-ok');
        return;
      }
      await this._requestTarget(cycleIndex, targetIndex, priorEntry ? 'manifest refresh' : 'activation');
    },

    refreshCurrentView: function() {
      if (this._removed) return;
      var self = this;
      var token = ++this._requestToken;
      this._setControlsBusy(true);
      this._setBanner('loading', 'Fetching and validating the current public manifest\u2026');
      setOpsLayerLoading(PT_SNOW_PRODUCT_NAME, true);
      recordStatus(PT_SNOW_PRODUCT_NAME, 'Fetching the public Snow Levels manifest\u2026', 'pt-ops-warn');
      return this._fetchManifest(token).then(function(loaded) {
        if (self._removed || token !== self._requestToken || !loaded) return;
        return self._applyManifest(loaded);
      }).catch(function(error) {
        if (self._removed || token !== self._requestToken || (error && error.name === 'AbortError')) return;
        self._recordError(error);
        var retained = !!self._displayedEntry;
        if (self._manifest && ptSnowFreshness(self._manifest, ptSnowNow()) === 'expired') {
          self._freshness = 'expired';
          self._removeDisplayedField();
          self._updateIdentity(null);
          retained = false;
        }
        self._setBanner('error', 'Manifest unavailable or invalid: ' +
          (error && error.message ? error.message : String(error)) +
          (retained ? ' Previously validated contours remain displayed.' : ' No Snow Levels contours are displayed.'));
        self._setControlsBusy(false);
        setOpsLayerLoading(PT_SNOW_PRODUCT_NAME, false);
        recordStatus(PT_SNOW_PRODUCT_NAME,
          'Manifest unavailable or invalid.' + (retained ? ' Previously validated contours retained.' : ''),
          'pt-ops-bad');
      });
    },

    _registerTimeController: function() {
      window.BRIM = window.BRIM || {};
      window.BRIM.opsLiveTimeControllers = window.BRIM.opsLiveTimeControllers || {};
      window.BRIM.opsLiveTimeControllers.nbmSnowLevels = this;
    },

    _unregisterTimeController: function() {
      if (window.BRIM && window.BRIM.opsLiveTimeControllers &&
          window.BRIM.opsLiveTimeControllers.nbmSnowLevels === this) {
        delete window.BRIM.opsLiveTimeControllers.nbmSnowLevels;
      }
    },

    onAdd: function(mapObj) {
      if (!this._removed) return;
      this._removed = false;
      this._map = mapObj;
      this._diagnostics.activations += 1;
      this._requestToken += 1;
      this._labelGroup = L.layerGroup().addTo(mapObj);
      this._createCard();
      this._registerTimeController();
      this.setLabelsVisible(ptSnowLabelsVisible);
      var self = this;
      this._mapMoveHandler = function() { self._renderLabels(); };
      mapObj.on('zoomend moveend', this._mapMoveHandler);
      this._visibilityHandler = function() {
        if (!self._removed && document.visibilityState === 'visible') self.refreshCurrentView();
      };
      document.addEventListener('visibilitychange', this._visibilityHandler);
      this._refreshTimer = window.setInterval(function() { self.refreshCurrentView(); }, PT_SNOW_REFRESH_MS);
      activeLegendDefs[PT_SNOW_PRODUCT_NAME] = {
        unifiedCard: true,
        legendType: 'nbm_snow_levels',
        note: 'NOAA/NBM modeled snow-level elevation. Runtime manifest and one selected immutable contour target are validated on demand.',
        sourceUrl: this.options.sourceUrl || '',
        infoUrl: this.options.manifestUrl || '',
        infoLabel: 'feed manifest'
      };
      redrawLegend();
      this.refreshCurrentView();
    },

    onRemove: function(mapObj) {
      if (this._removed) return;
      this._removed = true;
      this._diagnostics.removals += 1;
      this._requestToken += 1;
      if (this._manifestAbort) { try { this._manifestAbort.abort(); } catch(e) {} }
      if (this._targetAbort) { try { this._targetAbort.abort(); } catch(e2) {} }
      this._manifestAbort = null;
      this._targetAbort = null;
      if (this._refreshTimer) window.clearInterval(this._refreshTimer);
      this._refreshTimer = null;
      if (this._visibilityHandler) document.removeEventListener('visibilitychange', this._visibilityHandler);
      this._visibilityHandler = null;
      if (this._mapMoveHandler && mapObj) {
        try { mapObj.off('zoomend moveend', this._mapMoveHandler); } catch(e3) {}
      }
      this._mapMoveHandler = null;
      this._removeDisplayedField();
      if (this._labelGroup && mapObj) {
        try { mapObj.removeLayer(this._labelGroup); } catch(e4) {}
      }
      this._labelGroup = null;
      if (this._detachable && typeof this._detachable.destroy === 'function') {
        try { this._detachable.destroy(false); } catch(e5) {}
      }
      this._detachable = null;
      if (this._cardControl && mapObj) {
        try { mapObj.removeControl(this._cardControl); } catch(e6) {}
      }
      this._cardControl = null;
      this._card = null;
      this._ui = {};
      this._manifest = null;
      this._cycles = [];
      this._freshness = null;
      this._cycleIndex = 0;
      this._targetIndex = -1;
      this._unregisterTimeController();
      delete activeLegendDefs[PT_SNOW_PRODUCT_NAME];
      setOpsLayerLoading(PT_SNOW_PRODUCT_NAME, false);
      redrawLegend();
      recordStatus(PT_SNOW_PRODUCT_NAME, 'Layer turned off.', 'pt-ops-muted');
      this._map = null;
    },

    forceRemove: function(mapObj) {
      this.onRemove(mapObj || this._map);
    }
  });

  window.ptNbmSnowLevelsSetLabelsVisible = function(show) {
    ptSnowLabelsVisible = !!show;
    Array.prototype.forEach.call(
      document.querySelectorAll('[data-pt-ops-action="nbm-snow-labels"]'),
      function(toggle) { toggle.checked = ptSnowLabelsVisible; }
    );
    var controller = window.BRIM && window.BRIM.opsLiveTimeControllers
      ? window.BRIM.opsLiveTimeControllers.nbmSnowLevels
      : null;
    if (controller && typeof controller.setLabelsVisible === 'function') {
      controller.setLabelsVisible(ptSnowLabelsVisible);
    }
  };

  window.ptNbmSnowLevelsShowCard = function() {
    var controller = window.BRIM && window.BRIM.opsLiveTimeControllers
      ? window.BRIM.opsLiveTimeControllers.nbmSnowLevels
      : null;
    if (controller && typeof controller.showCard === 'function') controller.showCard();
  };

  if (includeNbmSnowLevels && NBM_SNOW_LEVELS_MANIFEST_URL) {
    addOpsLayer({
      category: 'Forecasts / Outlooks',
      subgroup: 'Weather Forecasts / Outlooks',
      name: PT_SNOW_PRODUCT_NAME,
      panelOrder: -100,
      sourceUrl: 'https://vlab.noaa.gov/web/mdl/nbm-weather-elements',
      infoUrl: NBM_SNOW_LEVELS_MANIFEST_URL,
      infoLabel: 'feed manifest',
      refreshable: true,
      extraRowHtml: '<label class="pt-ops-row-mini-toggle" title="Show/hide NBM Snow Levels contour labels"><input type="checkbox" data-pt-ops-action="nbm-snow-labels" checked>lbl</label><a href="#" class="pt-ops-row-mini-action" data-pt-ops-action="nbm-snow-card" title="Show the NBM Snow Levels time and legend card">lgnd</a>',
      helperText: 'NOAA/NBM modeled snow-level elevation. Choose valid time in Pacific time; current and previous NBM cycles are available.',
      layer: new PtOpsNbmSnowLevelsLayer({
        name: PT_SNOW_PRODUCT_NAME,
        manifestUrl: NBM_SNOW_LEVELS_MANIFEST_URL,
        sourceUrl: 'https://vlab.noaa.gov/web/mdl/nbm-weather-elements'
      })
    });
  }

  )---"
}
