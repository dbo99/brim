# ==== leaflet_ops_live_nbm_snow_levels_helpers.r ============================
##
## PURPOSE:
##   Browser-side Ops Live peer consumers for public BRIM Live NOAA/NBM
##   snow-level contours and native preceding-six-hour QPF images.
##
## CONTRACT:
##   - no request or geometry at initial BRIM page load;
##   - keep Snow and QPF data/layer lifecycles independent;
##   - share one manifest-driven NBM forecast-time inventory and card;
##   - fetch one selected content-addressed target per active product;
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
  var PT_SNOW_DOMAIN = [-130, 30, -112, 44.5];
  var PT_QPF_PRODUCT_NAME = 'NBM 6-Hour QPF';
  var PT_QPF_PRODUCT_ID = 'nbm_qpf';
  var PT_QPF_SOURCE_ID = 'noaa_nbm_core_conus_apcp';
  var PT_QPF_SCHEMA_VERSION = '1.0.0';
  var PT_QPF_PALETTE_ID = 'brim_nbm_qpf_6h_west_v1';
  var PT_QPF_PALETTE_VERSION = 1;
  var PT_QPF_CAPS = [3, 4, 6, 8, 10, 12, 15, 20];
  var PT_QPF_DOMAIN = [-130, 30, -112, 44.5];
  var PT_QPF_LEAFLET_BOUNDS = [[30, -130], [44.5, -112]];
  var PT_QPF_DEFAULT_OPACITY = 0.55;
  var PT_QPF_MAX_IMAGE_BYTES = 10 * 1024 * 1024;
  var PT_QPF_NUMERIC_CACHE_LIMIT = 3;
  var PT_QPF_NUMERIC_UNCOMPRESSED_BYTES = 1055520;
  var PT_QPF_NUMERIC_COLUMNS = 720;
  var PT_QPF_NUMERIC_ROWS = 733;
  var PT_QPF_NUMERIC_NODATA = 65535;
  var PT_QPF_NUMERIC_SCALE = 0.001;
  var PT_QPF_NUMERIC_EXTENT = [
    -14471533.8031256, 3503549.84350437,
    -12467782.9688466, 5543147.2038618
  ];
  var PT_QPF_FORECAST_STATE_BINDING = 'nbm_qpf_forecast_state_sha256_v1';
  var PT_QPF_CLASSES = [
    [0.01, 0.1, '#D9F0D3'], [0.1, 0.25, '#A6DBA0'],
    [0.25, 0.5, '#62BD73'], [0.5, 1, '#2F9E55'],
    [1, 1.5, '#146B38'], [1.5, 2, '#FFF59D'],
    [2, 2.5, '#FFE066'], [2.5, 3, '#FDBE55'],
    [3, 3.5, '#F79441'], [3.5, 4, '#F05A3C'],
    [4, 4.5, '#D73027'], [4.5, 5, '#BD1F2D'],
    [5, 5.5, '#9E1737'], [5.5, 6, '#7A123D'],
    [6, 7, '#5B0B55'], [7, 8, '#480A6A'],
    [8, 10, '#5F0A87'], [10, 12, '#7D1A9A'],
    [12, 15, '#A542B0'], [15, 20, '#CD86CF'],
    [20, null, '#F1C6E7']
  ];
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
  var ptQpfNumericCache = new Map();
  var ptQpfNumericInflight = new Map();
  var ptSnowLabelsVisible = true;
  var ptQpfOpacity = PT_QPF_DEFAULT_OPACITY;

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

  function ptNbmValidSnowLead(value) {
    return Number.isInteger(value) && value > 0 && value <= 999 &&
      (value === 1 || value % 6 === 0);
  }

  function ptNbmValidQpfLead(value) {
    return Number.isInteger(value) && value >= 6 && value <= 999 && value % 6 === 0;
  }

  function ptNbmStateKey(cycleUtc, validUtc, leadHours) {
    return String(cycleUtc || '') + '|' + String(validUtc || '') + '|' + String(Number(leadHours));
  }

  function ptNbmHorizonLabel(leadHours) {
    var lead = Number(leadHours);
    var day = Math.max(1, Math.ceil(lead / 24));
    return 'Day ' + day + ' \u00b7 +' + lead + ' h';
  }

  function ptNbmHorizonCue(leadHours) {
    var lead = Number(leadHours);
    if (lead >= 168) return 'extended range \u00b7 lower confidence';
    if (lead >= 72) return 'medium range';
    return 'near range';
  }

  function ptNbmBuildInventory(snowCycles, qpfCycles, snowActive, qpfActive) {
    var byCycle = Object.create(null);

    function ensureCycle(cycleUtc) {
      if (!byCycle[cycleUtc]) {
        byCycle[cycleUtc] = {cycle_time_utc: cycleUtc, byState: Object.create(null)};
      }
      return byCycle[cycleUtc];
    }

    if (snowActive) {
      (snowCycles || []).forEach(function(cycle) {
        (cycle.targets || []).forEach(function(target) {
          var key = ptNbmStateKey(target.cycle_time_utc, target.valid_time_utc, target.lead_hours);
          var item = ensureCycle(target.cycle_time_utc).byState[key] || {
            cycle_utc: target.cycle_time_utc,
            valid_time_utc: target.valid_time_utc,
            lead_hours: target.lead_hours,
            snowTarget: null,
            qpfTarget: null,
            qpfCycle: null
          };
          item.snowTarget = target;
          ensureCycle(target.cycle_time_utc).byState[key] = item;
        });
      });
    }

    if (qpfActive) {
      (qpfCycles || []).forEach(function(cycle) {
        (cycle.targets || []).forEach(function(target) {
          var key = ptNbmStateKey(target.cycle_utc, target.valid_time_utc, target.lead_hours);
          var item = ensureCycle(target.cycle_utc).byState[key] || {
            cycle_utc: target.cycle_utc,
            valid_time_utc: target.valid_time_utc,
            lead_hours: target.lead_hours,
            snowTarget: null,
            qpfTarget: null,
            qpfCycle: null
          };
          item.qpfTarget = target;
          item.qpfCycle = cycle;
          ensureCycle(target.cycle_utc).byState[key] = item;
        });
      });
    }

    return Object.keys(byCycle).sort(function(a, b) {
      return Date.parse(b) - Date.parse(a);
    }).map(function(cycleUtc) {
      var targets = Object.keys(byCycle[cycleUtc].byState).map(function(key) {
        return byCycle[cycleUtc].byState[key];
      }).sort(function(a, b) {
        return Number(a.lead_hours) - Number(b.lead_hours) ||
          Date.parse(a.valid_time_utc) - Date.parse(b.valid_time_utc);
      });
      return {cycle_time_utc: cycleUtc, targets: targets};
    });
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

  function ptQpfExpectedCap(maximum) {
    maximum = Number(maximum);
    if (!ptSnowFinite(maximum) || maximum < 0) return null;
    for (var i = 0; i < PT_QPF_CAPS.length; i += 1) {
      if (maximum <= PT_QPF_CAPS[i]) return PT_QPF_CAPS[i];
    }
    return 20;
  }

  function ptQpfValidatePalette(palette) {
    ptSnowRequire(palette, [
      'palette_id', 'palette_version', 'display_units', 'class_interval',
      'below_0_01_in', 'nodata', 'overflow', 'classes'
    ], 'QPF palette');
    if (palette.palette_id !== PT_QPF_PALETTE_ID ||
        Number(palette.palette_version) !== PT_QPF_PALETTE_VERSION ||
        palette.display_units !== 'in' ||
        palette.class_interval !== 'lower-inclusive upper-exclusive' ||
        palette.below_0_01_in !== 'transparent' || palette.nodata !== 'transparent' ||
        palette.overflow !== '>=20 in uses the final fixed class' ||
        !Array.isArray(palette.classes) || palette.classes.length !== PT_QPF_CLASSES.length) {
      throw new Error('QPF fixed palette identity is unsupported.');
    }
    palette.classes.forEach(function(item, index) {
      ptSnowRequire(item, ['lower_inclusive_in', 'upper_exclusive_in', 'color_hex', 'alpha_u8'],
        'QPF palette class ' + index);
      var expected = PT_QPF_CLASSES[index];
      var upper = item.upper_exclusive_in === null ? null : Number(item.upper_exclusive_in);
      if (Number(item.lower_inclusive_in) !== expected[0] || upper !== expected[1] ||
          String(item.color_hex).toUpperCase() !== expected[2] || Number(item.alpha_u8) !== 255) {
        throw new Error('QPF fixed palette class ' + index + ' is unsupported.');
      }
    });
    return palette;
  }

  function ptQpfForecastStateText(entry) {
    var fields = [
      ['binding', PT_QPF_FORECAST_STATE_BINDING],
      ['product_id', PT_QPF_PRODUCT_ID],
      ['source_id', PT_QPF_SOURCE_ID],
      ['parameter', 'APCP'],
      ['level', 'surface'],
      ['cycle_utc', entry.cycle_utc],
      ['lead_hours', String(Number(entry.lead_hours))],
      ['valid_time_utc', entry.valid_time_utc],
      ['accumulation_start_utc', entry.accumulation_start_utc],
      ['accumulation_end_utc', entry.accumulation_end_utc],
      ['accumulation_hours', '6'],
      ['source_inventory_semantics', entry.source_inventory_semantics],
      ['native_units', 'kg/m^2'],
      ['normalized_units', 'mm'],
      ['stored_numeric_units', 'in'],
      ['display_units', 'in'],
      ['grid_contract_id', 'nbm_qpf_lossless_webp_v1'],
      ['columns', '720'],
      ['rows', '733'],
      ['crs', 'EPSG:3857'],
      ['bounds_wgs84', '-130,30,-112,44.5'],
      ['extent_m', '-14471533.803125564,3503549.843504374,-12467782.96884664,5543147.203861799'],
      ['row_order', 'north_to_south'],
      ['column_order', 'west_to_east'],
      ['pixel_is_area', 'true'],
      ['palette_id', PT_QPF_PALETTE_ID],
      ['palette_version', String(PT_QPF_PALETTE_VERSION)],
      ['image_path', entry.image_path],
      ['image_sha256', entry.sha256],
      ['image_media_type', 'image/webp'],
      ['image_encoding', 'lossless_vp8l_rgba8'],
      ['numeric_path', entry.numeric.path],
      ['numeric_sha256', entry.numeric.sha256],
      ['numeric_media_type', 'application/octet-stream'],
      ['numeric_encoding', 'uint16_le'],
      ['numeric_compression', 'gzip'],
      ['numeric_scale', '0.001'],
      ['numeric_offset', '0'],
      ['numeric_nodata', '65535']
    ];
    return fields.map(function(field) { return field[0] + '=' + field[1]; }).join('\n');
  }

  function ptQpfValidateTarget(entry, cycleUtc, index) {
    ptSnowRequire(entry, [
      'product_id', 'forecast_state_id', 'source_id', 'parameter', 'level',
      'cycle_utc', 'lead_hours', 'lead_end_hours',
      'accumulation_start_utc', 'accumulation_end_utc', 'valid_time_utc',
      'accumulation_hours', 'source_parameter', 'source_level',
      'source_inventory_semantics', 'native_units', 'normalized_units',
      'stored_numeric_units', 'display_units', 'grid_contract_id',
      'columns', 'rows', 'crs', 'extent_m', 'row_order', 'column_order',
      'pixel_is_area', 'image_path', 'image_media_type', 'image_encoding',
      'image_width', 'image_height', 'bounds_wgs84', 'bytes', 'sha256',
      'image', 'numeric', 'palette_id', 'palette_version'
    ], 'QPF target ' + index);
    if (entry.product_id !== PT_QPF_PRODUCT_ID || entry.source_id !== PT_QPF_SOURCE_ID ||
        entry.parameter !== 'APCP' || entry.level !== 'surface' ||
        entry.cycle_utc !== cycleUtc || !ptNbmValidQpfLead(entry.lead_hours) ||
        Number(entry.lead_end_hours) !== Number(entry.lead_hours) ||
        Number(entry.accumulation_hours) !== 6 || entry.source_parameter !== 'APCP' ||
        entry.source_level !== 'surface' || entry.native_units !== 'kg/m^2' ||
        entry.normalized_units !== 'mm' || entry.stored_numeric_units !== 'in' ||
        entry.display_units !== 'in') {
      throw new Error('QPF target source, time, or unit identity is unsupported.');
    }
    var cycle = ptSnowDate(entry.cycle_utc, 'QPF target cycle_utc');
    var valid = ptSnowDate(entry.valid_time_utc, 'QPF target valid_time_utc');
    var start = ptSnowDate(entry.accumulation_start_utc, 'QPF target accumulation_start_utc');
    var end = ptSnowDate(entry.accumulation_end_utc, 'QPF target accumulation_end_utc');
    if (valid.getTime() !== cycle.getTime() + entry.lead_hours * 3600000 ||
        end.getTime() !== valid.getTime() || start.getTime() !== valid.getTime() - 6 * 3600000 ||
        entry.source_inventory_semantics !== (entry.lead_hours - 6) + '-' + entry.lead_hours + ' hour acc fcst') {
      throw new Error('QPF target is not the native preceding-six-hour accumulation ending at valid time.');
    }
    var imageMatch = String(entry.image_path).match(
      /^docs\/data\/nbm-qpf\/nbm\/qpf\/nbm_qpf_([0-9]{8}T[0-9]{6}Z)_f([0-9]{3})_([0-9a-f]{12})\.webp$/
    );
    var numericMatch = String(entry.numeric && entry.numeric.path).match(
      /^docs\/data\/nbm-qpf\/nbm\/qpf\/nbm_qpf_([0-9]{8}T[0-9]{6}Z)_f([0-9]{3})_([0-9a-f]{12})\.u16le\.gz$/
    );
    var cycleToken = entry.cycle_utc.replace(/[-:]/g, '').replace('.000Z', 'Z');
    if (!imageMatch || imageMatch[1] !== cycleToken || Number(imageMatch[2]) !== entry.lead_hours ||
        !/^[0-9a-f]{64}$/.test(entry.sha256) || imageMatch[3] !== entry.sha256.slice(0, 12)) {
      throw new Error('QPF image path is not a safe content-addressed identity.');
    }
    ptSnowRequire(entry.image, ['path', 'media_type', 'encoding', 'bytes', 'sha256'],
      'QPF target nested image identity');
    if (Object.keys(entry.image).length !== 5 || entry.image.path !== entry.image_path ||
        entry.image.media_type !== entry.image_media_type ||
        entry.image.encoding !== entry.image_encoding || Number(entry.image.bytes) !== Number(entry.bytes) ||
        entry.image.sha256 !== entry.sha256) {
      throw new Error('QPF nested image identity is invalid.');
    }
    ptSnowRequire(entry.numeric, [
      'path', 'media_type', 'encoding', 'compression', 'stored_units', 'scale',
      'offset', 'nodata', 'compressed_bytes', 'uncompressed_bytes', 'sha256'
    ], 'QPF target numeric identity');
    if (Object.keys(entry.numeric).length !== 11 || !numericMatch || numericMatch[1] !== cycleToken ||
        Number(numericMatch[2]) !== entry.lead_hours ||
        !/^[0-9a-f]{64}$/.test(entry.numeric.sha256) ||
        numericMatch[3] !== entry.numeric.sha256.slice(0, 12) ||
        entry.numeric.media_type !== 'application/octet-stream' ||
        entry.numeric.encoding !== 'uint16_le' || entry.numeric.compression !== 'gzip' ||
        entry.numeric.stored_units !== 'in' || Number(entry.numeric.scale) !== PT_QPF_NUMERIC_SCALE ||
        Number(entry.numeric.offset) !== 0 || Number(entry.numeric.nodata) !== PT_QPF_NUMERIC_NODATA ||
        !Number.isInteger(entry.numeric.compressed_bytes) || entry.numeric.compressed_bytes < 1 ||
        Number(entry.numeric.uncompressed_bytes) !== PT_QPF_NUMERIC_UNCOMPRESSED_BYTES) {
      throw new Error('QPF numeric target contract is invalid.');
    }
    if (entry.image_media_type !== 'image/webp' || entry.image_encoding !== 'lossless_vp8l_rgba8' ||
        Number(entry.image_width) !== PT_QPF_NUMERIC_COLUMNS ||
        Number(entry.image_height) !== PT_QPF_NUMERIC_ROWS ||
        !ptSnowSameArray(entry.bounds_wgs84, PT_QPF_DOMAIN) ||
        entry.grid_contract_id !== 'nbm_qpf_lossless_webp_v1' ||
        Number(entry.columns) !== PT_QPF_NUMERIC_COLUMNS || Number(entry.rows) !== PT_QPF_NUMERIC_ROWS ||
        entry.crs !== 'EPSG:3857' || !ptSnowSameArray(entry.extent_m, PT_QPF_NUMERIC_EXTENT) ||
        entry.row_order !== 'north_to_south' || entry.column_order !== 'west_to_east' ||
        entry.pixel_is_area !== true || !Number.isInteger(entry.bytes) || entry.bytes < 1 ||
        entry.bytes > PT_QPF_MAX_IMAGE_BYTES || entry.palette_id !== PT_QPF_PALETTE_ID ||
        Number(entry.palette_version) !== PT_QPF_PALETTE_VERSION ||
        !/^[0-9a-f]{64}$/.test(entry.forecast_state_id)) {
      throw new Error('QPF target image/numeric grid, palette, or forecast-state identity is unsupported.');
    }
    return entry;
  }

  function ptQpfValidateCycle(cycle, index) {
    ptSnowRequire(cycle, [
      'cycle_utc', 'cycle_status', 'cycle_max_qpf_in', 'legend_cap_in',
      'legend_overflow', 'target_count', 'complete_required_leads_hours', 'targets'
    ], 'QPF cycle ' + index);
    var cycleDate = ptSnowDate(cycle.cycle_utc, 'QPF cycle_utc');
    if ([0, 6, 12, 18].indexOf(cycleDate.getUTCHours()) < 0 ||
        cycleDate.getUTCMinutes() !== 0 || cycleDate.getUTCSeconds() !== 0 ||
        cycle.cycle_status !== 'complete' || !ptSnowFinite(cycle.cycle_max_qpf_in) ||
        cycle.cycle_max_qpf_in < 0 || PT_QPF_CAPS.indexOf(Number(cycle.legend_cap_in)) < 0 ||
        typeof cycle.legend_overflow !== 'boolean' ||
        Number(cycle.legend_cap_in) !== ptQpfExpectedCap(cycle.cycle_max_qpf_in) ||
        cycle.legend_overflow !== (Number(cycle.cycle_max_qpf_in) > 20) ||
        !Array.isArray(cycle.complete_required_leads_hours) ||
        !Array.isArray(cycle.targets) || cycle.targets.length < 1 ||
        Number(cycle.target_count) !== cycle.targets.length) {
      throw new Error('QPF cycle is incomplete or has invalid fixed legend metadata.');
    }
    var paths = Object.create(null);
    cycle.targets.forEach(function(entry, targetIndex) {
      ptQpfValidateTarget(entry, cycle.cycle_utc, targetIndex);
      if (paths[entry.image_path]) throw new Error('QPF cycle repeats a target path.');
      paths[entry.image_path] = true;
    });
    var leads = cycle.targets.map(function(entry) { return entry.lead_hours; });
    if (new Set(leads).size !== leads.length ||
        leads.some(function(lead, leadIndex) {
          return !ptNbmValidQpfLead(lead) || (leadIndex > 0 && lead <= leads[leadIndex - 1]);
        }) || !ptSnowSameArray(cycle.complete_required_leads_hours, leads)) {
      throw new Error('QPF cycle leads must be unique ordered six-hour periods matching the declared complete inventory.');
    }
    return cycle;
  }

  function ptQpfValidateNumericRepresentation(numeric) {
    ptSnowRequire(numeric, [
      'contract_id', 'media_type', 'encoding', 'compression', 'stored_units',
      'scale', 'offset', 'nodata', 'valid_stored_min', 'valid_stored_max',
      'represented_min', 'represented_max', 'uncompressed_bytes',
      'grid_contract_id', 'columns', 'rows', 'crs', 'bounds_wgs84', 'extent_m',
      'row_order', 'column_order', 'pixel_is_area'
    ], 'QPF numeric representation');
    if (numeric.contract_id !== 'nbm_qpf_uint16_le_gzip_v1' ||
        numeric.media_type !== 'application/octet-stream' || numeric.encoding !== 'uint16_le' ||
        numeric.compression !== 'gzip' || numeric.stored_units !== 'in' ||
        Number(numeric.scale) !== PT_QPF_NUMERIC_SCALE || Number(numeric.offset) !== 0 ||
        Number(numeric.nodata) !== PT_QPF_NUMERIC_NODATA ||
        Number(numeric.valid_stored_min) !== 0 || Number(numeric.valid_stored_max) !== 65534 ||
        Number(numeric.represented_min) !== 0 || Number(numeric.represented_max) !== 65.534 ||
        Number(numeric.uncompressed_bytes) !== PT_QPF_NUMERIC_UNCOMPRESSED_BYTES ||
        numeric.grid_contract_id !== 'nbm_qpf_lossless_webp_v1' ||
        Number(numeric.columns) !== PT_QPF_NUMERIC_COLUMNS ||
        Number(numeric.rows) !== PT_QPF_NUMERIC_ROWS || numeric.crs !== 'EPSG:3857' ||
        !ptSnowSameArray(numeric.bounds_wgs84, PT_QPF_DOMAIN) ||
        !ptSnowSameArray(numeric.extent_m, PT_QPF_NUMERIC_EXTENT) ||
        numeric.row_order !== 'north_to_south' || numeric.column_order !== 'west_to_east' ||
        numeric.pixel_is_area !== true) {
      throw new Error('QPF fixed gzip uint16 little-endian numeric contract is unsupported.');
    }
    return numeric;
  }

  function ptQpfValidateForecastStateBinding(binding) {
    ptSnowRequire(binding, ['algorithm', 'digest', 'canonicalization', 'binds'],
      'QPF forecast-state binding');
    if (binding.algorithm !== PT_QPF_FORECAST_STATE_BINDING || binding.digest !== 'sha256' ||
        binding.canonicalization !== 'ordered UTF-8 key=value lines' ||
        !Array.isArray(binding.binds) || binding.binds.length !== 3 ||
        binding.binds[0] !== 'forecast metadata' ||
        binding.binds[1] !== 'image path and SHA-256' ||
        binding.binds[2] !== 'numeric path and SHA-256') {
      throw new Error('QPF image/numeric forecast-state binding contract is unsupported.');
    }
    return binding;
  }

  function ptQpfValidateManifest(manifest) {
    ptSnowRequire(manifest, [
      'schema_version', 'product_id', 'generated_at_utc', 'source', 'palette',
      'spatial_representation', 'numeric_representation', 'forecast_state_binding',
      'freshness', 'retention_mode',
      'current_cycle_utc', 'previous_cycle_utc', 'cycles'
    ], 'QPF manifest');
    if (manifest.schema_version !== PT_QPF_SCHEMA_VERSION || manifest.product_id !== PT_QPF_PRODUCT_ID) {
      throw new Error('QPF manifest schema or product identity is unsupported.');
    }
    ptSnowDate(manifest.generated_at_utc, 'QPF generated_at_utc');
    ptSnowRequire(manifest.source, [
      'source_id', 'agency', 'dataset', 'family', 'domain', 'parameter', 'level',
      'field_kind', 'native_units', 'normalized_units', 'display_units'
    ], 'QPF source');
    if (manifest.source.source_id !== PT_QPF_SOURCE_ID ||
        manifest.source.agency !== 'NOAA/NWS/NCEP/MDL' ||
        manifest.source.dataset !== 'National Blend of Models' ||
        manifest.source.family !== 'core' || manifest.source.domain !== 'conus' ||
        manifest.source.parameter !== 'APCP' || manifest.source.level !== 'surface' ||
        manifest.source.field_kind !== 'deterministic accumulated precipitation' ||
        manifest.source.native_units !== 'kg/m^2' || manifest.source.normalized_units !== 'mm' ||
        manifest.source.display_units !== 'in') {
      throw new Error('QPF deterministic NBM Core surface APCP identity is unsupported.');
    }
    ptQpfValidatePalette(manifest.palette);
    ptSnowRequire(manifest.spatial_representation, [
      'contract_id', 'media_type', 'encoding', 'crs', 'bounds_wgs84', 'extent_m',
      'image_width', 'image_height', 'pixel_size_m', 'row_order', 'column_order',
      'pixel_is_area', 'leaflet_bounds', 'default_leaflet_opacity'
    ], 'QPF spatial representation');
    var spatial = manifest.spatial_representation;
    if (spatial.contract_id !== 'nbm_qpf_lossless_webp_v1' || spatial.media_type !== 'image/webp' ||
        spatial.encoding !== 'lossless VP8L RGBA8 WebP' || spatial.crs !== 'EPSG:3857' ||
        !ptSnowSameArray(spatial.bounds_wgs84, PT_QPF_DOMAIN) ||
        !ptSnowSameArray(spatial.extent_m, [
          -14471533.8031256, 3503549.84350437, -12467782.9688466, 5543147.2038618
        ]) || !ptSnowSameArray(spatial.pixel_size_m, [2782.98726983229, 2782.53391590372]) ||
        Number(spatial.image_width) !== 720 || Number(spatial.image_height) !== 733 ||
        spatial.row_order !== 'north_to_south' || spatial.column_order !== 'west_to_east' ||
        spatial.pixel_is_area !== true || !Array.isArray(spatial.leaflet_bounds) ||
        spatial.leaflet_bounds.length !== 2 ||
        !ptSnowSameArray(spatial.leaflet_bounds[0], PT_QPF_LEAFLET_BOUNDS[0]) ||
        !ptSnowSameArray(spatial.leaflet_bounds[1], PT_QPF_LEAFLET_BOUNDS[1]) ||
        Number(spatial.default_leaflet_opacity) !== PT_QPF_DEFAULT_OPACITY) {
      throw new Error('QPF fixed EPSG:3857 WebP display contract is unsupported.');
    }
    ptQpfValidateNumericRepresentation(manifest.numeric_representation);
    ptQpfValidateForecastStateBinding(manifest.forecast_state_binding);
    ptSnowRequire(manifest.freshness, [
      'basis', 'current_through_hours', 'delayed_through_hours',
      'stale_through_hours', 'expired_after_hours', 'product_status_independent_from_snow'
    ], 'QPF freshness');
    if (manifest.freshness.basis !== 'source_cycle_age' ||
        Number(manifest.freshness.current_through_hours) !== 9 ||
        Number(manifest.freshness.delayed_through_hours) !== 15 ||
        Number(manifest.freshness.stale_through_hours) !== 24 ||
        Number(manifest.freshness.expired_after_hours) !== 24 ||
        manifest.freshness.product_status_independent_from_snow !== true) {
      throw new Error('QPF freshness contract is unsupported.');
    }
    if (!Array.isArray(manifest.cycles) || (manifest.cycles.length !== 1 && manifest.cycles.length !== 2)) {
      throw new Error('QPF manifest must retain one bootstrap or two steady cycles.');
    }
    manifest.cycles.forEach(ptQpfValidateCycle);
    for (var i = 1; i < manifest.cycles.length; i += 1) {
      if (Date.parse(manifest.cycles[i - 1].cycle_utc) <= Date.parse(manifest.cycles[i].cycle_utc)) {
        throw new Error('QPF cycles are not newest-to-oldest.');
      }
    }
    if (manifest.current_cycle_utc !== manifest.cycles[0].cycle_utc) {
      throw new Error('QPF current cycle identity is invalid.');
    }
    if (manifest.cycles.length === 1) {
      if (manifest.retention_mode !== 'bootstrap' || manifest.previous_cycle_utc !== null) {
        throw new Error('QPF one-cycle state is not explicit bootstrap state.');
      }
    } else if (manifest.retention_mode !== 'steady' ||
        manifest.previous_cycle_utc !== manifest.cycles[1].cycle_utc) {
      throw new Error('QPF two-cycle state is not valid steady state.');
    }
    manifest._ptQpfCycles = manifest.cycles;
    return manifest;
  }

  function ptQpfFreshness(cycleUtc, nowMs) {
    var ageHours = (Number(nowMs) - ptSnowDate(cycleUtc, 'QPF freshness cycle').getTime()) / 3600000;
    if (ageHours <= 9) return 'current';
    if (ageHours <= 15) return 'delayed';
    if (ageHours <= 24) return 'stale';
    return 'expired';
  }

  function ptQpfFindPair(manifest, snowState, nowMs) {
    if (!snowState || !snowState.cycle_utc || !snowState.valid_time_utc ||
        !Number.isInteger(Number(snowState.lead_hours))) {
      return {status: 'no_snow_selection', cycle: null, target: null, freshness: null};
    }
    var lead = Number(snowState.lead_hours);
    if (lead === 1) return {status: 'snow_only', cycle: null, target: null, freshness: null};
    if (!ptNbmValidQpfLead(lead)) {
      return {status: 'unsupported_lead', cycle: null, target: null, freshness: null};
    }
    var cycles = manifest && manifest._ptQpfCycles ? manifest._ptQpfCycles : [];
    var cycle = cycles.find(function(item) { return item.cycle_utc === snowState.cycle_utc; });
    if (!cycle) return {status: 'no_same_cycle', cycle: null, target: null, freshness: null};
    var target = cycle.targets.find(function(item) {
      return item.cycle_utc === snowState.cycle_utc && item.valid_time_utc === snowState.valid_time_utc &&
        Number(item.lead_hours) === lead;
    });
    if (!target) return {status: 'no_exact_target', cycle: cycle, target: null, freshness: null};
    var freshness = ptQpfFreshness(cycle.cycle_utc, nowMs);
    if (freshness === 'expired') {
      return {status: 'expired', cycle: cycle, target: null, freshness: freshness};
    }
    return {status: 'paired', cycle: cycle, target: target, freshness: freshness};
  }

  function ptQpfTargetUrl(entry, manifestUrl) {
    var base = new URL(manifestUrl);
    var relative = String(entry.image_path || '').replace(/^docs\//, '');
    if (!/^data\/nbm-qpf\/nbm\/qpf\//.test(relative)) {
      throw new Error('QPF target path cannot be resolved under the public product root.');
    }
    var manifestSuffix = '/data/nbm-qpf/nbm_qpf_manifest.json';
    if (base.pathname.slice(-manifestSuffix.length) !== manifestSuffix) {
      throw new Error('QPF manifest URL is outside the supported public product path.');
    }
    var pagesPrefix = base.pathname.slice(0, -manifestSuffix.length);
    return new URL(pagesPrefix + '/' + relative, base.origin).toString();
  }

  function ptQpfNumericUrl(entry, manifestUrl) {
    var base = new URL(manifestUrl);
    var relative = String(entry && entry.numeric && entry.numeric.path || '').replace(/^docs\//, '');
    if (!/^data\/nbm-qpf\/nbm\/qpf\/.+\.u16le\.gz$/.test(relative)) {
      throw new Error('QPF numeric target path cannot be resolved under the public product root.');
    }
    var manifestSuffix = '/data/nbm-qpf/nbm_qpf_manifest.json';
    if (base.pathname.slice(-manifestSuffix.length) !== manifestSuffix) {
      throw new Error('QPF manifest URL is outside the supported public product path.');
    }
    var pagesPrefix = base.pathname.slice(0, -manifestSuffix.length);
    return new URL(pagesPrefix + '/' + relative, base.origin).toString();
  }

  async function ptQpfDigestHex(buffer, dependencies) {
    dependencies = dependencies || {};
    if (typeof dependencies.digestHex === 'function') {
      return String(await dependencies.digestHex(buffer)).toLowerCase();
    }
    if (!window.crypto || !window.crypto.subtle || typeof window.crypto.subtle.digest !== 'function') {
      throw new Error('This browser cannot verify QPF numeric SHA-256 identities.');
    }
    var view = buffer instanceof ArrayBuffer ? buffer :
      buffer.buffer.slice(buffer.byteOffset, buffer.byteOffset + buffer.byteLength);
    return ptSnowBufferHex(await window.crypto.subtle.digest('SHA-256', view));
  }

  async function ptQpfVerifyForecastState(entry, dependencies) {
    if (typeof TextEncoder !== 'function') {
      throw new Error('This browser cannot encode the QPF forecast-state identity.');
    }
    var expected = await ptQpfDigestHex(new TextEncoder().encode(ptQpfForecastStateText(entry)), dependencies);
    if (expected !== entry.forecast_state_id) {
      throw new Error('QPF image and numeric companion forecast_state_id does not match canonical metadata.');
    }
    return expected;
  }

  async function ptQpfDefaultGunzip(buffer) {
    if (typeof window.DecompressionStream !== 'function' || typeof window.Response !== 'function' ||
        typeof window.Blob !== 'function') {
      throw new Error('This browser cannot explicitly decompress the QPF gzip application payload.');
    }
    var stream = new window.Blob([buffer], {type: 'application/gzip'}).stream()
      .pipeThrough(new window.DecompressionStream('gzip'));
    return new window.Response(stream).arrayBuffer();
  }

  async function ptQpfDecodeNumericPayload(compressedBuffer, entry, dependencies) {
    dependencies = dependencies || {};
    ptQpfValidateTarget(entry, entry && entry.cycle_utc, 0);
    var compressed = compressedBuffer instanceof ArrayBuffer ? compressedBuffer :
      (ArrayBuffer.isView(compressedBuffer)
        ? compressedBuffer.buffer.slice(compressedBuffer.byteOffset,
          compressedBuffer.byteOffset + compressedBuffer.byteLength)
        : null);
    if (!compressed || compressed.byteLength !== Number(entry.numeric.compressed_bytes)) {
      throw new Error('Selected QPF numeric compressed byte count does not match the manifest.');
    }
    var signature = new Uint8Array(compressed, 0, Math.min(2, compressed.byteLength));
    if (signature.length !== 2 || signature[0] !== 0x1f || signature[1] !== 0x8b) {
      throw new Error('Selected QPF numeric payload is not RFC1952 gzip application data.');
    }
    var digest = await ptQpfDigestHex(compressed, dependencies);
    if (digest !== entry.numeric.sha256) {
      throw new Error('Selected QPF numeric SHA-256 does not match the manifest.');
    }
    await ptQpfVerifyForecastState(entry, dependencies);
    var uncompressed;
    try {
      uncompressed = typeof dependencies.gunzip === 'function'
        ? await dependencies.gunzip(compressed)
        : await ptQpfDefaultGunzip(compressed);
    } catch(error) {
      throw new Error('Selected QPF numeric gzip payload is corrupt or unsupported.');
    }
    if (ArrayBuffer.isView(uncompressed)) {
      uncompressed = uncompressed.buffer.slice(
        uncompressed.byteOffset, uncompressed.byteOffset + uncompressed.byteLength
      );
    }
    if (!(uncompressed instanceof ArrayBuffer) ||
        uncompressed.byteLength !== PT_QPF_NUMERIC_UNCOMPRESSED_BYTES ||
        uncompressed.byteLength !== Number(entry.numeric.uncompressed_bytes)) {
      throw new Error('Selected QPF numeric uncompressed byte count is invalid.');
    }
    var cellCount = PT_QPF_NUMERIC_COLUMNS * PT_QPF_NUMERIC_ROWS;
    var values = new Uint16Array(cellCount);
    var source = new DataView(uncompressed);
    for (var index = 0; index < cellCount; index += 1) {
      values[index] = source.getUint16(index * 2, true);
    }
    return {
      forecast_state_id: entry.forecast_state_id,
      cycle_utc: entry.cycle_utc,
      valid_time_utc: entry.valid_time_utc,
      lead_hours: entry.lead_hours,
      numeric_path: entry.numeric.path,
      numeric_sha256: entry.numeric.sha256,
      columns: PT_QPF_NUMERIC_COLUMNS,
      rows: PT_QPF_NUMERIC_ROWS,
      extent_m: PT_QPF_NUMERIC_EXTENT.slice(),
      scale: PT_QPF_NUMERIC_SCALE,
      offset: 0,
      nodata: PT_QPF_NUMERIC_NODATA,
      values: values
    };
  }

  function ptQpfNumericCacheGet(key) {
    if (!ptQpfNumericCache.has(key)) return null;
    var value = ptQpfNumericCache.get(key);
    ptQpfNumericCache.delete(key);
    ptQpfNumericCache.set(key, value);
    return value;
  }

  function ptQpfNumericCacheSet(key, value) {
    if (ptQpfNumericCache.has(key)) ptQpfNumericCache.delete(key);
    ptQpfNumericCache.set(key, value);
    while (ptQpfNumericCache.size > PT_QPF_NUMERIC_CACHE_LIMIT) {
      ptQpfNumericCache.delete(ptQpfNumericCache.keys().next().value);
    }
  }

  function ptQpfNumericCacheClear() {
    ptQpfNumericCache.clear();
    ptQpfNumericInflight.forEach(function(flight) {
      if (flight && flight.abort) {
        try { flight.abort.abort(); } catch(error) {}
      }
    });
    ptQpfNumericInflight.clear();
  }

  async function ptQpfFetchNumericFrame(entry, manifestUrl, abortController) {
    var response = await fetch(ptQpfNumericUrl(entry, manifestUrl), {
      cache: 'force-cache',
      signal: abortController ? abortController.signal : undefined
    });
    if (!response.ok) {
      throw new Error('Selected QPF numeric target request failed with HTTP ' + response.status + '.');
    }
    var contentEncoding = response.headers && typeof response.headers.get === 'function'
      ? response.headers.get('content-encoding') : null;
    if (contentEncoding && String(contentEncoding).toLowerCase() !== 'identity') {
      throw new Error('Selected QPF numeric target must be an explicit gzip application payload, not HTTP Content-Encoding.');
    }
    return ptQpfDecodeNumericPayload(await response.arrayBuffer(), entry);
  }

  function ptQpfNumericAcquire(entry, manifestUrl) {
    var key = entry.forecast_state_id;
    var cached = ptQpfNumericCacheGet(key);
    if (cached) return {promise: Promise.resolve(cached), release: function() {}};
    var flight = ptQpfNumericInflight.get(key);
    if (flight && (flight.numeric_path !== entry.numeric.path ||
        flight.numeric_sha256 !== entry.numeric.sha256)) {
      throw new Error('QPF in-flight numeric identity conflicts with the selected forecast state.');
    }
    if (!flight) {
      var abort = typeof AbortController === 'function' ? new AbortController() : null;
      flight = {
        refs: 0,
        pending: true,
        abort: abort,
        numeric_path: entry.numeric.path,
        numeric_sha256: entry.numeric.sha256,
        promise: null
      };
      flight.promise = ptQpfFetchNumericFrame(entry, manifestUrl, abort).then(function(frame) {
        ptQpfNumericCacheSet(key, frame);
        return frame;
      }).finally(function() {
        flight.pending = false;
        if (ptQpfNumericInflight.get(key) === flight) ptQpfNumericInflight.delete(key);
      });
      ptQpfNumericInflight.set(key, flight);
    }
    flight.refs += 1;
    var released = false;
    return {
      promise: flight.promise,
      release: function() {
        if (released) return;
        released = true;
        flight.refs = Math.max(0, flight.refs - 1);
        if (flight.pending && flight.refs === 0 && flight.abort) {
          try { flight.abort.abort(); } catch(error) {}
          if (ptQpfNumericInflight.get(key) === flight) ptQpfNumericInflight.delete(key);
        }
      }
    };
  }

  function ptQpfLngLatToCell(latlng, frame) {
    if (!latlng || !ptSnowFinite(Number(latlng.lat)) || !ptSnowFinite(Number(latlng.lng)) ||
        !frame || !Array.isArray(frame.extent_m) || frame.extent_m.length !== 4) return null;
    var latitude = Number(latlng.lat);
    var longitude = Number(latlng.lng);
    if (latitude <= -85.0511287798066 || latitude >= 85.0511287798066) return null;
    var radius = 6378137;
    var x = radius * longitude * Math.PI / 180;
    var y = radius * Math.log(Math.tan(Math.PI / 4 + latitude * Math.PI / 360));
    var extent = frame.extent_m;
    if (x < extent[0] || x > extent[2] || y < extent[1] || y > extent[3]) return null;
    var columnFraction = (x - extent[0]) / (extent[2] - extent[0]);
    var rowFraction = (extent[3] - y) / (extent[3] - extent[1]);
    var column = columnFraction === 1 ? frame.columns - 1 : Math.floor(columnFraction * frame.columns);
    var row = rowFraction === 1 ? frame.rows - 1 : Math.floor(rowFraction * frame.rows);
    if (column < 0 || column >= frame.columns || row < 0 || row >= frame.rows) return null;
    return {row: row, column: column, index: row * frame.columns + column};
  }

  function ptQpfNumericValueAt(frame, latlng) {
    var cell = ptQpfLngLatToCell(latlng, frame);
    if (!cell || !frame.values || cell.index >= frame.values.length) return null;
    var stored = Number(frame.values[cell.index]);
    if (stored === Number(frame.nodata)) return null;
    return {
      row: cell.row,
      column: cell.column,
      index: cell.index,
      stored: stored,
      value_in: stored * Number(frame.scale) + Number(frame.offset)
    };
  }

  function ptQpfCreateDefaultNumericHoverProvider() {
    return {
      attachSelectedFrame: function(context) {
        if (!context || !context.map || !context.manifest || !context.target) {
          throw new Error('QPF numeric hover context is incomplete.');
        }
        var target = context.target;
        ptQpfValidateTarget(target, target.cycle_utc, 0);
        var exact = context.exact_identity || {};
        if (exact.cycle_utc !== target.cycle_utc || exact.valid_time_utc !== target.valid_time_utc ||
            Number(exact.lead_hours) !== Number(target.lead_hours) ||
            context.target_url !== ptQpfTargetUrl(target, context.manifest_url)) {
          throw new Error('QPF numeric hover context does not match the displayed image identity.');
        }
        var listed = (context.manifest._ptQpfCycles || []).some(function(cycle) {
          return cycle.targets.some(function(candidate) {
            return candidate.cycle_utc === target.cycle_utc &&
              candidate.valid_time_utc === target.valid_time_utc &&
              Number(candidate.lead_hours) === Number(target.lead_hours) &&
              candidate.forecast_state_id === target.forecast_state_id &&
              candidate.image_path === target.image_path &&
              candidate.numeric.path === target.numeric.path;
          });
        });
        if (!listed) throw new Error('QPF numeric hover target is not present in the validated manifest.');

        var mapObj = context.map;
        var detached = false;
        var suppressed = false;
        var listenersAttached = false;
        var tooltip = null;
        var lastLatLng = null;
        var timer = null;
        var acquisition = ptQpfNumericAcquire(target, context.manifest_url);
        var frame = null;
        function status(kind, text) {
          if (typeof context.onStatus === 'function') context.onStatus(kind, text);
        }
        function closeTooltip() {
          if (tooltip) {
            try { mapObj.removeLayer(tooltip); } catch(error) {}
            tooltip = null;
          }
        }
        function renderAt(latlng) {
          if (detached || suppressed || !frame || !latlng) {
            closeTooltip();
            return;
          }
          var sample = ptQpfNumericValueAt(frame, latlng);
          if (!sample) {
            closeTooltip();
            return;
          }
          var html = '<div class="pt-ops-nbm-qpf-hover-card">' +
            '<div class="pt-ops-nbm-qpf-hover-title">NBM 6-Hour QPF</div>' +
            '<div class="pt-ops-nbm-qpf-hover-amount">' +
              escapeHtml(sample.value_in.toFixed(3)) + ' in</div>' +
            '<div class="pt-ops-nbm-qpf-hover-period">Preceding six hours</div>' +
            '<div class="pt-ops-nbm-qpf-hover-valid">Valid ' +
              escapeHtml(ptSnowFormatPacific(target.valid_time_utc, true)) + '</div>' +
            '<div class="pt-ops-nbm-qpf-hover-meta">+' + escapeHtml(target.lead_hours) +
              ' h · exact selected grid cell</div></div>';
          if (!tooltip) {
            tooltip = L.tooltip({
              permanent: true, direction: 'top', offset: [8, -10], opacity: 0.97,
              interactive: false, className: 'pt-ops-nbm-qpf-hover-tooltip'
            });
          }
          tooltip.setLatLng(latlng).setContent(html).addTo(mapObj);
        }
        function schedule(event) {
          if (!event || !event.latlng) return;
          lastLatLng = event.latlng;
          if (timer) window.clearTimeout(timer);
          timer = window.setTimeout(function() {
            timer = null;
            renderAt(lastLatLng);
          }, 35);
        }
        function attachListeners() {
          if (listenersAttached || detached) return;
          listenersAttached = true;
          mapObj.on('mousemove', schedule);
          mapObj.on('mouseout zoomstart movestart', closeTooltip);
          var container = mapObj.getContainer && mapObj.getContainer();
          if (container) container.classList.add('pt-ops-nbm-qpf-numeric-hover-on');
        }
        status('loading', 'Loading and verifying the exact selected-frame numeric QPF grid…');
        acquisition.promise.then(function(loaded) {
          if (detached) return;
          if (!loaded || loaded.forecast_state_id !== target.forecast_state_id ||
              loaded.numeric_path !== target.numeric.path) {
            throw new Error('Decoded QPF numeric grid does not match the displayed forecast state.');
          }
          frame = loaded;
          attachListeners();
          status('ready', 'Exact 0.001-in selected-frame numeric hover ready · move across the QPF surface.');
        }).catch(function(error) {
          if (detached || (error && error.name === 'AbortError')) return;
          closeTooltip();
          status('error', (error && error.message ? error.message : String(error)) +
            ' Numeric hover is disabled; the validated image remains displayed.');
        });
        return {
          detach: function() {
            if (detached) return;
            detached = true;
            if (timer) window.clearTimeout(timer);
            timer = null;
            acquisition.release();
            if (listenersAttached) {
              mapObj.off('mousemove', schedule);
              mapObj.off('mouseout zoomstart movestart', closeTooltip);
            }
            listenersAttached = false;
            var container = mapObj.getContainer && mapObj.getContainer();
            if (container) container.classList.remove('pt-ops-nbm-qpf-numeric-hover-on');
            closeTooltip();
            frame = null;
          },
          setSuppressed: function(value) {
            suppressed = !!value;
            if (suppressed) closeTooltip();
            else if (lastLatLng) renderAt(lastLatLng);
          },
          isReady: function() { return !!frame && listenersAttached && !detached; }
        };
      },
      clearCache: ptQpfNumericCacheClear
    };
  }

  function ptQpfLegendGradient(palette, cap) {
    cap = Number(cap);
    var stops = ['transparent 0%', 'transparent ' + ((0.01 / cap) * 100).toFixed(4) + '%'];
    palette.classes.forEach(function(item) {
      var lower = Number(item.lower_inclusive_in);
      var upper = item.upper_exclusive_in === null ? cap : Math.min(Number(item.upper_exclusive_in), cap);
      if (lower >= cap || upper <= lower) return;
      var start = ((lower / cap) * 100).toFixed(4) + '%';
      var end = ((upper / cap) * 100).toFixed(4) + '%';
      stops.push(item.color_hex + ' ' + start, item.color_hex + ' ' + end);
    });
    return 'linear-gradient(to right,' + stops.join(',') + ')';
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
    if (!ptNbmValidSnowLead(entry.lead_hours)) {
      throw new Error('Target lead_hours is not f001 or a positive six-hour forecast state.');
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
    var expectedLeads = null;
    var cycles = Object.keys(byCycle).sort(function(a, b) { return Date.parse(b) - Date.parse(a); })
      .map(function(cycleUtc) {
        var targets = byCycle[cycleUtc].slice().sort(function(a, b) {
          return Date.parse(a.valid_time_utc) - Date.parse(b.valid_time_utc);
        });
        var leads = targets.map(function(entry) { return entry.lead_hours; });
        if (!leads.length || new Set(leads).size !== leads.length ||
            leads.some(function(lead, leadIndex) {
              return !ptNbmValidSnowLead(lead) || (leadIndex > 0 && lead <= leads[leadIndex - 1]);
            })) {
          throw new Error('Retained Snow cycle ' + cycleUtc + ' has invalid, duplicate, or unordered leads.');
        }
        if (expectedLeads === null) {
          expectedLeads = leads;
        } else if (!ptSnowSameArray(leads, expectedLeads)) {
          throw new Error('Retained Snow cycles do not expose one coherent manifest-declared lead inventory.');
        }
        return {cycle_time_utc: cycleUtc, targets: targets};
      });
    if (cycles.length !== 1 && cycles.length !== 2) {
      throw new Error('Manifest must contain one bootstrap or two steady complete retained cycles.');
    }
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
    if (!Array.isArray(manifest.targets) || manifest.targets.length < 2 ||
        manifest.target_count !== manifest.targets.length) {
      throw new Error('Manifest target_count must match the nonempty retained target inventory.');
    }
    ptSnowRequire(manifest.diagnostics, [
      'expected_current_cycle_target_count', 'actual_current_cycle_target_count',
      'retained_cycle_count', 'complete_bundle_validated'
    ], 'Manifest diagnostics');
    if (!Number.isInteger(manifest.diagnostics.expected_current_cycle_target_count) ||
        manifest.diagnostics.expected_current_cycle_target_count < 1 ||
        manifest.diagnostics.actual_current_cycle_target_count !==
          manifest.diagnostics.expected_current_cycle_target_count ||
        (manifest.diagnostics.retained_cycle_count !== 1 &&
          manifest.diagnostics.retained_cycle_count !== 2) ||
        manifest.diagnostics.complete_bundle_validated !== true) {
      throw new Error('Manifest does not declare a complete validated one- or two-cycle bundle.');
    }
    var identities = Object.create(null);
    manifest.targets.forEach(function(entry, index) {
      ptSnowValidateTargetEntry(entry, index);
      var key = entry.cycle_time_utc + '|' + entry.valid_time_utc;
      if (identities[key]) throw new Error('Manifest repeats a cycle/valid target identity.');
      identities[key] = true;
    });
    manifest._ptSnowCycles = ptSnowBuildCycles(manifest);
    if (manifest._ptSnowCycles.length !== manifest.diagnostics.retained_cycle_count) {
      throw new Error('Manifest diagnostics retained-cycle count does not match its target inventory.');
    }
    if (manifest._ptSnowCycles[0].targets.length !==
        manifest.diagnostics.actual_current_cycle_target_count) {
      throw new Error('Manifest diagnostics do not match the published current Snow lead inventory.');
    }
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
      .pt-ops-nbm-snow-card{width:380px;max-width:min(380px,calc(100vw - 24px));max-height:min(680px,calc(100vh - 24px));overflow:hidden;background:rgba(240,247,245,.98)!important;border:1px solid rgba(54,84,86,.52)!important;border-radius:8px;box-shadow:0 2px 9px rgba(0,0,0,.26);color:#202a2e;font:12px/1.28 Arial,Helvetica,sans-serif}
      .pt-ops-nbm-snow-card-head{display:flex;align-items:flex-start;justify-content:space-between;gap:8px;padding:8px 9px 6px;border-bottom:1px solid rgba(54,84,86,.20)}
      .pt-ops-nbm-snow-card-title{font-size:14px;font-weight:700}.pt-ops-nbm-snow-card-subtitle{color:#586b70;font-size:10.5px;margin-top:1px}
      .pt-ops-nbm-snow-card-body{max-height:min(618px,calc(100vh - 82px));overflow:auto;padding:8px 9px 9px}
      .pt-ops-nbm-snow-state{margin:0 0 7px;padding:6px 7px;border:1px solid;border-radius:5px;font-size:11px;font-weight:700}
      .pt-ops-nbm-snow-state-current{color:#175538;background:#eef8f1;border-color:#8eb9a0}.pt-ops-nbm-snow-state-loading{color:#704900;background:#fff5dc;border-color:#d2b16c}
      .pt-ops-nbm-snow-state-delayed,.pt-ops-nbm-snow-state-previous{color:#77500d;background:#fff8e8;border-color:#d8ba79}.pt-ops-nbm-snow-state-stale,.pt-ops-nbm-snow-state-expired,.pt-ops-nbm-snow-state-error{color:#862c2b;background:#fff0ef;border-color:#d8a3a0}
      .pt-ops-nbm-snow-time{padding:8px;border:1px solid rgba(54,84,86,.24);border-radius:6px;background:rgba(255,255,255,.72)}
      .pt-ops-nbm-snow-kicker{color:#66757a;font-size:9.5px;font-weight:700;letter-spacing:.07em;text-transform:uppercase}.pt-ops-nbm-snow-valid{font-size:18px;font-weight:750;margin-top:1px}
      .pt-ops-nbm-snow-valid-utc,.pt-ops-nbm-snow-cycle-line{color:#617076;font-size:10.5px;margin-top:1px}.pt-ops-nbm-snow-cycle-line{margin:5px 0 4px}
      .pt-ops-nbm-snow-cycle-select,.pt-ops-nbm-snow-target-select{width:100%;min-width:0;height:30px;border:1px solid #aab8bc;border-radius:4px;background:#fff;color:#202a2e;font:11px Arial,Helvetica,sans-serif}
      .pt-ops-nbm-snow-nav{display:grid;grid-template-columns:32px 1fr 32px;gap:4px;margin-top:5px}.pt-ops-nbm-snow-nav button{height:30px;margin:0!important;padding:0!important;font-size:17px!important}
      .pt-ops-nbm-snow-controls{display:flex;align-items:center;justify-content:space-between;gap:8px;margin-top:8px}.pt-ops-nbm-snow-controls label{display:inline-flex;align-items:center;margin:0!important;font-size:11px}
      .pt-ops-nbm-qpf-controls{display:grid;grid-template-columns:auto 1fr auto;align-items:center;gap:6px;margin-top:7px;padding:6px 7px;border:1px solid rgba(54,84,86,.22);border-radius:5px;background:rgba(255,255,255,.62)}.pt-ops-nbm-qpf-controls label{display:inline-flex;align-items:center;margin:0!important;font-size:11px;white-space:nowrap}.pt-ops-nbm-qpf-opacity{width:100%;min-width:70px}.pt-ops-nbm-qpf-opacity-value{min-width:31px;color:#53656b;font-size:10px;text-align:right;font-variant-numeric:tabular-nums}
      .pt-ops-nbm-qpf-state{margin-top:6px;padding:5px 6px;border:1px solid rgba(54,84,86,.22);border-radius:4px;background:rgba(255,255,255,.62);color:#506168;font-size:10.5px}.pt-ops-nbm-qpf-state[data-kind="paired"]{color:#175538;background:#eef8f1;border-color:#8eb9a0}.pt-ops-nbm-qpf-state[data-kind="loading"]{color:#704900;background:#fff5dc;border-color:#d2b16c}.pt-ops-nbm-qpf-state[data-kind="unavailable"],.pt-ops-nbm-qpf-state[data-kind="error"]{color:#862c2b;background:#fff0ef;border-color:#d8a3a0}.pt-ops-nbm-qpf-state[data-kind="hidden"],.pt-ops-nbm-qpf-state[data-kind="snow-only"]{color:#5b686d;background:#f4f5f5;border-color:#c7cdcf}
      .pt-ops-nbm-snow-legend{margin-top:8px;padding:7px 8px;border:1px solid rgba(54,84,86,.22);border-radius:5px;background:rgba(255,255,255,.62)}
      .pt-ops-nbm-snow-legend-title{font-weight:700;margin-bottom:5px}.pt-ops-nbm-snow-ramp{height:9px;border:1px solid rgba(42,57,63,.46);border-radius:3px;box-shadow:0 0 0 1px rgba(255,255,255,.76)}
      .pt-ops-nbm-snow-ramp-ticks{display:grid;grid-template-columns:repeat(5,1fr);margin-top:3px;color:#4d5d62;font-size:9.5px}.pt-ops-nbm-snow-ramp-ticks span{text-align:center}.pt-ops-nbm-snow-ramp-ticks span:first-child{text-align:left}.pt-ops-nbm-snow-ramp-ticks span:last-child{text-align:right}.pt-ops-nbm-snow-interval{margin-top:4px;color:#5d6c71;font-size:9.5px}
      .pt-ops-nbm-qpf-legend{margin-top:7px;padding:7px 8px;border:1px solid rgba(54,84,86,.22);border-radius:5px;background:rgba(255,255,255,.62)}.pt-ops-nbm-qpf-legend[hidden]{display:none}.pt-ops-nbm-qpf-legend-title{display:flex;align-items:baseline;justify-content:space-between;gap:6px;font-weight:700;margin-bottom:5px}.pt-ops-nbm-qpf-cap{color:#5d6c71;font-size:9.5px;font-weight:600}.pt-ops-nbm-qpf-ramp{height:12px;border:1px solid rgba(42,57,63,.46);border-radius:2px}.pt-ops-nbm-qpf-ramp-ticks{position:relative;height:14px;margin-top:2px;color:#4d5d62;font-size:9px}.pt-ops-nbm-qpf-ramp-ticks span{position:absolute;transform:translateX(-50%);white-space:nowrap}.pt-ops-nbm-qpf-ramp-ticks span:first-child{transform:none}.pt-ops-nbm-qpf-ramp-ticks span:last-child{transform:translateX(-100%)}.pt-ops-nbm-qpf-interval{color:#5d6c71;font-size:9.5px}.pt-ops-nbm-qpf-overflow{display:inline-flex;align-items:center;gap:4px;margin-top:3px;color:#6b476c;font-size:9.5px}.pt-ops-nbm-qpf-overflow-swatch{width:13px;height:8px;background:#F1C6E7;border:1px solid rgba(42,57,63,.4)}
      .pt-ops-nbm-snow-note{margin-top:7px;color:#58686d;font-size:10.5px}.pt-ops-nbm-snow-details{margin-top:7px;padding-top:6px;border-top:1px solid rgba(54,84,86,.18);font-size:10px;color:#596a70}.pt-ops-nbm-snow-details summary{cursor:pointer;color:#315d6e;font-weight:700}
      .pt-ops-nbm-snow-metrics{margin-top:5px;overflow-wrap:anywhere}.pt-nbm-snow-label-wrap{background:transparent;border:0}.pt-nbm-snow-label{display:inline-block;transform:translate(-50%,-50%);padding:1px 3px;border:1px solid rgba(76,94,105,.48);border-radius:2px;color:#203e4d;background:rgba(255,255,255,.88);box-shadow:0 1px 2px rgba(0,0,0,.18);font:700 10px/1.1 Arial,Helvetica,sans-serif;white-space:nowrap}
      .leaflet-tooltip.pt-nbm-snow-tooltip{box-sizing:border-box;width:max-content;min-width:236px;max-width:300px;padding:6px 8px;white-space:normal;font:11px/1.24 Arial,Helvetica,sans-serif;font-variant-numeric:tabular-nums}.pt-nbm-snow-tooltip-title{color:#20343d;font-size:13px;font-weight:700;line-height:1.15;margin-bottom:3px}.pt-nbm-snow-tooltip-row{display:grid;grid-template-columns:30px auto;align-items:baseline;column-gap:6px;color:#405158;white-space:nowrap}.pt-nbm-snow-tooltip-row+.pt-nbm-snow-tooltip-row{margin-top:1px}.pt-nbm-snow-tooltip-label{color:#69777c;font-weight:600}.pt-nbm-snow-tooltip-value{color:#35484f}
      .leaflet-container.pt-ops-nbm-qpf-numeric-hover-on{cursor:crosshair}.leaflet-tooltip.pt-ops-nbm-qpf-hover-tooltip{box-sizing:border-box;min-width:218px;max-width:280px;padding:7px 9px;white-space:normal;font:11px/1.24 Arial,Helvetica,sans-serif;font-variant-numeric:tabular-nums}.pt-ops-nbm-qpf-hover-title{color:#20343d;font-size:12px;font-weight:700}.pt-ops-nbm-qpf-hover-amount{margin:2px 0;color:#123b28;font-size:20px;font-weight:800;line-height:1.05}.pt-ops-nbm-qpf-hover-period{color:#405158;font-weight:700}.pt-ops-nbm-qpf-hover-valid{margin-top:2px;color:#35484f}.pt-ops-nbm-qpf-hover-meta{margin-top:2px;color:#69777c;font-size:9.5px}
      .pt-nbm-snow-popup{min-width:240px;max-width:320px;font:11px/1.32 Arial,Helvetica,sans-serif}.pt-nbm-snow-popup-kicker{color:#586970;font-size:10px;font-weight:700;letter-spacing:.04em;text-transform:uppercase}.pt-nbm-snow-popup-title{font-size:16px;font-weight:700;margin:1px 0 6px}.pt-nbm-snow-popup-row{margin-top:5px}.pt-nbm-snow-popup-label{font-weight:700}.pt-nbm-snow-popup-secondary{color:#68777c;font-size:10px}.pt-nbm-snow-popup-note{margin-top:7px;padding-top:6px;border-top:1px solid #d5dddd;color:#5d6b70}
      .pt-ops-nbm-product-section{margin-top:8px;padding:7px 8px;border:1px solid rgba(54,84,86,.22);border-radius:6px;background:rgba(255,255,255,.58)}.pt-ops-nbm-product-section[hidden]{display:none}.pt-ops-nbm-product-head{display:flex;align-items:center;justify-content:space-between;gap:8px;margin-bottom:5px;font-weight:700}.pt-ops-nbm-product-head button{font-size:10px!important;padding:2px 6px!important}.pt-ops-nbm-horizon{margin-top:3px;color:#5d6c71;font-size:10px}.pt-ops-nbm-hover-note{margin-top:5px;color:#5d6c71;font-size:9.5px}\n      @media(max-width:720px){.pt-ops-nbm-snow-card{width:calc(100vw - 16px);max-width:calc(100vw - 16px);max-height:62vh}.pt-ops-nbm-snow-card-body{max-height:calc(62vh - 48px)}.pt-ops-nbm-qpf-controls{grid-template-columns:auto minmax(72px,1fr) auto}}
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


  var PtOpsNbmForecastController = L.Class.extend({
    initialize: function(options) {
      this.options = options || {};
      this._map = null;
      this._removed = true;
      this._snowActive = false;
      this._qpfActive = false;
      this._manifest = null;
      this._cycles = [];
      this._freshness = null;
      this._qpfManifest = null;
      this._qpfFreshness = null;
      this._inventoryCycles = [];
      this._cycleIndex = -1;
      this._targetIndex = -1;
      this._timeState = {cycle_utc: null, valid_time_utc: null, lead_hours: null};
      this._displayGroup = null;
      this._labelGroup = null;
      this._displayedEntry = null;
      this._displayedGeoJson = null;
      this._qpfOverlay = null;
      this._qpfObjectUrl = null;
      this._qpfDisplayedEntry = null;
      this._qpfDisplayedCycle = null;
      this._qpfOpacity = ptQpfOpacity;
      this._qpfNumericHoverProvider = ptQpfCreateDefaultNumericHoverProvider();
      this._qpfNumericHoverHandle = null;
      this._qpfNumericHoverStatus = 'idle';
      this._snowHoverOwned = false;
      this._requestToken = 0;
      this._qpfManifestToken = 0;
      this._qpfTargetToken = 0;
      this._manifestAbort = null;
      this._targetAbort = null;
      this._qpfManifestAbort = null;
      this._qpfTargetAbort = null;
      this._qpfPendingPath = null;
      this._qpfPendingPromise = null;
      this._refreshTimer = null;
      this._visibilityHandler = null;
      this._mapMoveHandler = null;
      this._cardControl = null;
      this._card = null;
      this._detachable = null;
      this._ui = {};
      this._labelsVisible = ptSnowLabelsVisible;
      this._lastMetrics = null;
      this._diagnostics = {
        manifestLoads: [], targetLoads: [], qpfManifestLoads: [], qpfTargetLoads: [],
        qpfNumericStates: [],
        qpfOverlayReplacements: 0, qpfOverlayRemovals: 0, errors: [],
        activations: {snow: 0, qpf: 0}, removals: {snow: 0, qpf: 0},
        inventoryBuilds: 0, selections: 0
      };
    },

    getTimeState: function() {
      return {
        cycle_utc: this._timeState.cycle_utc,
        valid_time_utc: this._timeState.valid_time_utc,
        lead_hours: this._timeState.lead_hours
      };
    },

    getSelectionState: function() {
      var cycle = this._inventoryCycles[this._cycleIndex];
      var target = cycle && cycle.targets[this._targetIndex];
      return {
        cycle_utc: this._timeState.cycle_utc,
        valid_time_utc: this._timeState.valid_time_utc,
        lead_hours: this._timeState.lead_hours,
        cycle_index: target ? this._cycleIndex : null,
        target_index: target ? this._targetIndex : null,
        cycle_role: !target ? null : (this._cycleIndex === 0 ? 'current' :
          (this._cycleIndex === 1 ? 'previous' : 'retained')),
        snow_available: !!(target && target.snowTarget),
        qpf_available: !!(target && target.qpfTarget),
        active_products: {snow_levels: this._snowActive, qpf_6_hour: this._qpfActive}
      };
    },

    getDiagnostics: function() { return this._diagnostics; },

    getQpfState: function() {
      return {
        active: this._qpfActive,
        opacity: this._qpfOpacity,
        cycle_utc: this._qpfDisplayedEntry ? this._qpfDisplayedEntry.cycle_utc : null,
        valid_time_utc: this._qpfDisplayedEntry ? this._qpfDisplayedEntry.valid_time_utc : null,
        lead_hours: this._qpfDisplayedEntry ? this._qpfDisplayedEntry.lead_hours : null,
        image_path: this._qpfDisplayedEntry ? this._qpfDisplayedEntry.image_path : null,
        freshness: this._qpfFreshness,
        forecast_state_id: this._qpfDisplayedEntry ? this._qpfDisplayedEntry.forecast_state_id : null,
        numeric_path: this._qpfDisplayedEntry && this._qpfDisplayedEntry.numeric
          ? this._qpfDisplayedEntry.numeric.path : null,
        numeric_hover_status: this._qpfNumericHoverStatus,
        numeric_hover_ready: !!(this._qpfNumericHoverHandle &&
          typeof this._qpfNumericHoverHandle.isReady === 'function' &&
          this._qpfNumericHoverHandle.isReady())
      };
    },

    _emitTimeSelection: function(reason) {
      var detail = this.getSelectionState();
      detail.product_id = 'nbm_forecast_guidance';
      detail.reason = reason || 'target selection';
      if (typeof window.CustomEvent === 'function' && typeof window.dispatchEvent === 'function') {
        window.dispatchEvent(new window.CustomEvent('brim:nbm-time-selection', {detail: detail}));
      }
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
      if (this._diagnostics.errors.length > 100) this._diagnostics.errors.shift();
    },

    _createCard: function() {
      var self = this;
      this._cardControl = L.control({position: 'bottomleft'});
      this._cardControl.onAdd = function() {
        var div = L.DomUtil.create('div', 'pt-ops-nbm-snow-card pt-map-legend-card');
        div.innerHTML =
          '<div class="pt-ops-nbm-snow-card-head pt-map-card-handle">' +
            '<div><div class="pt-ops-nbm-snow-card-title">NBM Forecast Guidance</div>' +
            '<div class="pt-ops-nbm-snow-card-subtitle">One exact cycle + valid-time state for active NBM products</div></div>' +
            '<span class="pt-map-card-actions"><button type="button" class="pt-map-card-dock pt-ops-nbm-snow-dock" aria-label="Undock NBM Forecast Guidance card" title="Undock NBM Forecast Guidance card">&#x2197;</button>' +
            '<button type="button" class="pt-map-legend-close pt-ops-nbm-snow-close" aria-label="Hide NBM Forecast Guidance card" title="Hide NBM Forecast Guidance card">&times;</button></span>' +
          '</div>' +
          '<div class="pt-ops-nbm-snow-card-body">' +
            '<div class="pt-ops-nbm-snow-state pt-ops-nbm-snow-state-loading" role="status" aria-live="polite">Loading active NBM feed…</div>' +
            '<div class="pt-ops-nbm-snow-time">' +
              '<div class="pt-ops-nbm-snow-kicker">Forecast valid time · Pacific</div>' +
              '<div class="pt-ops-nbm-snow-valid">—</div>' +
              '<div class="pt-ops-nbm-snow-valid-utc">Valid UTC and lead unavailable</div>' +
              '<div class="pt-ops-nbm-snow-cycle-line">NBM cycle unavailable</div>' +
              '<select class="pt-ops-nbm-snow-cycle-select" aria-label="NBM cycle" disabled></select>' +
              '<div class="pt-ops-nbm-snow-nav">' +
                '<button type="button" class="pt-ops-nbm-snow-prev" aria-label="Previous forecast target" title="Previous forecast target" disabled>&#x2039;</button>' +
                '<select class="pt-ops-nbm-snow-target-select" aria-label="Forecast valid time" disabled></select>' +
                '<button type="button" class="pt-ops-nbm-snow-next" aria-label="Next forecast target" title="Next forecast target" disabled>&#x203a;</button>' +
              '</div><div class="pt-ops-nbm-horizon">Manifest-declared horizon unavailable</div>' +
            '</div>' +
            '<section class="pt-ops-nbm-product-section pt-ops-nbm-snow-section" hidden>' +
              '<div class="pt-ops-nbm-product-head"><span>NBM Snow Levels</span><button type="button" class="pt-ops-nbm-snow-refresh">Recheck feed</button></div>' +
              '<div class="pt-ops-nbm-snow-controls"><label><input type="checkbox" class="pt-ops-nbm-snow-label-toggle" checked> contour labels</label></div>' +
              '<div class="pt-ops-nbm-snow-product-state pt-ops-nbm-qpf-state" data-kind="loading" role="status" aria-live="polite">Snow manifest loading…</div>' +
              '<div class="pt-ops-nbm-snow-legend"><div class="pt-ops-nbm-snow-legend-title">Snow level (ft MSL)</div>' +
                '<div class="pt-ops-nbm-snow-ramp" aria-label="Fixed color scale from 0 to 20,000 feet MSL"></div>' +
                '<div class="pt-ops-nbm-snow-ramp-ticks"><span>0</span><span>5k</span><span>10k</span><span>15k</span><span>20k ft</span></div>' +
                '<div class="pt-ops-nbm-snow-interval">Fixed scale · 1,000-ft contours · 5,000-ft index emphasis</div></div>' +
            '</section>' +
            '<section class="pt-ops-nbm-product-section pt-ops-nbm-qpf-section" hidden>' +
              '<div class="pt-ops-nbm-product-head"><span>NBM 6-Hour QPF</span><button type="button" class="pt-ops-nbm-qpf-refresh">Recheck feed</button></div>' +
              '<div class="pt-ops-nbm-qpf-controls"><span>Opacity</span>' +
                '<input type="range" class="pt-ops-nbm-qpf-opacity" min="0.15" max="0.85" step="0.05" value="0.55" aria-label="QPF surface opacity">' +
                '<span class="pt-ops-nbm-qpf-opacity-value">55%</span></div>' +
              '<div class="pt-ops-nbm-qpf-state" data-kind="loading" role="status" aria-live="polite">QPF manifest loading…</div>' +
              '<div class="pt-ops-nbm-qpf-legend" hidden><div class="pt-ops-nbm-qpf-legend-title"><span>Preceding six-hour precipitation (in)</span><span class="pt-ops-nbm-qpf-cap"></span></div>' +
                '<div class="pt-ops-nbm-qpf-ramp" aria-label="Fixed NBM six-hour precipitation color classes"></div>' +
                '<div class="pt-ops-nbm-qpf-ramp-ticks"></div>' +
                '<div class="pt-ops-nbm-qpf-interval">Native six-hour accumulation ending at selected valid time · fixed global colors</div>' +
                '<div class="pt-ops-nbm-qpf-overflow" hidden><span class="pt-ops-nbm-qpf-overflow-swatch"></span><span>≥20 in fixed overflow class</span></div></div>' +
              '<div class="pt-ops-nbm-hover-note" data-kind="idle" role="status" aria-live="polite">Exact numeric hover loads only for the selected QPF frame; palette colors are never reverse-mapped.</div>' +
            '</section>' +
            '<div class="pt-ops-nbm-snow-note">Modeled NBM guidance, not observations. SNOWLVL is modeled snow-level elevation—not freezing level, accumulation, or precipitation type. QPF is a native preceding-six-hour accumulation, not precipitation accumulated from initialization.</div>' +
            '<details class="pt-ops-nbm-snow-details"><summary>Source, timing, and load details</summary>' +
              '<div class="pt-ops-nbm-snow-source">NOAA/NWS/NCEP MDL · deterministic NBM SNOWLVL and NBM Core surface APCP.</div>' +
              '<div class="pt-ops-nbm-snow-metrics">Waiting for an active runtime feed.</div></details>' +
          '</div>';
        L.DomEvent.disableClickPropagation(div);
        L.DomEvent.disableScrollPropagation(div);
        self._card = div;
        self._captureUi();
        self._wireCard();
        self._updateProductSections();
        self._renderSelectors();
        return div;
      };
      this._cardControl.addTo(this._map);
      var shared = window.BRIM && window.BRIM.legendCloseout;
      if (shared && this._card) {
        this._detachable = shared.makeDetachable({
          card: this._card, map: this._map,
          handleSelector: '.pt-ops-nbm-snow-card-head',
          dockSelector: '.pt-ops-nbm-snow-dock',
          label: 'NBM Forecast Guidance card'
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
        horizon: this._card.querySelector('.pt-ops-nbm-horizon'),
        snowSection: this._card.querySelector('.pt-ops-nbm-snow-section'),
        qpfSection: this._card.querySelector('.pt-ops-nbm-qpf-section'),
        snowState: this._card.querySelector('.pt-ops-nbm-snow-product-state'),
        labels: this._card.querySelector('.pt-ops-nbm-snow-label-toggle'),
        snowRefresh: this._card.querySelector('.pt-ops-nbm-snow-refresh'),
        qpfRefresh: this._card.querySelector('.pt-ops-nbm-qpf-refresh'),
        qpfOpacity: this._card.querySelector('.pt-ops-nbm-qpf-opacity'),
        qpfOpacityValue: this._card.querySelector('.pt-ops-nbm-qpf-opacity-value'),
        qpfState: this._card.querySelector('.pt-ops-nbm-qpf-section .pt-ops-nbm-qpf-state'),
        qpfLegend: this._card.querySelector('.pt-ops-nbm-qpf-legend'),
        qpfRamp: this._card.querySelector('.pt-ops-nbm-qpf-ramp'),
        qpfRampTicks: this._card.querySelector('.pt-ops-nbm-qpf-ramp-ticks'),
        qpfCap: this._card.querySelector('.pt-ops-nbm-qpf-cap'),
        qpfOverflow: this._card.querySelector('.pt-ops-nbm-qpf-overflow'),
        qpfHoverNote: this._card.querySelector('.pt-ops-nbm-hover-note'),
        ramp: this._card.querySelector('.pt-ops-nbm-snow-ramp'),
        metrics: this._card.querySelector('.pt-ops-nbm-snow-metrics')
      };
      if (this._ui.ramp) this._ui.ramp.style.background = ptSnowLegendGradient();
      if (this._ui.labels) this._ui.labels.checked = this._labelsVisible;
      this.setQpfOpacity(this._qpfOpacity);
    },

    _wireCard: function() {
      var self = this;
      var shared = window.BRIM && window.BRIM.legendCloseout;
      if (shared && this._card) shared.wire(this._card, '.pt-ops-nbm-snow-close', function() {});
      if (this._ui.snowRefresh) this._ui.snowRefresh.addEventListener('click', function() { self.refreshProduct('snow'); });
      if (this._ui.qpfRefresh) this._ui.qpfRefresh.addEventListener('click', function() { self.refreshProduct('qpf'); });
      if (this._ui.labels) this._ui.labels.addEventListener('change', function() { self.setLabelsVisible(!!self._ui.labels.checked); });
      if (this._ui.qpfOpacity) this._ui.qpfOpacity.addEventListener('input', function() { self.setQpfOpacity(Number(self._ui.qpfOpacity.value)); });
      if (this._ui.cycleSelect) this._ui.cycleSelect.addEventListener('change', function() { self.selectCycle(Number(self._ui.cycleSelect.value)); });
      if (this._ui.targetSelect) this._ui.targetSelect.addEventListener('change', function() { self.selectTarget(self._cycleIndex, Number(self._ui.targetSelect.value), 'valid-time selection'); });
      if (this._ui.previous) this._ui.previous.addEventListener('click', function() { self.stepValidTime(-1); });
      if (this._ui.next) this._ui.next.addEventListener('click', function() { self.stepValidTime(1); });
    },

    _setProductState: function(product, kind, text) {
      var node = product === 'snow' ? this._ui.snowState : this._ui.qpfState;
      if (!node) return;
      node.setAttribute('data-kind', kind);
      node.textContent = text;
    },

    _updateProductSections: function() {
      if (this._ui.snowSection) this._ui.snowSection.hidden = !this._snowActive;
      if (this._ui.qpfSection) this._ui.qpfSection.hidden = !this._qpfActive;
    },

    _setControlsBusy: function(busy) {
      var cycle = this._inventoryCycles[this._cycleIndex];
      var targets = cycle ? cycle.targets : [];
      if (this._ui.cycleSelect) this._ui.cycleSelect.disabled = !!busy || this._inventoryCycles.length < 2;
      if (this._ui.targetSelect) this._ui.targetSelect.disabled = !!busy || !targets.length;
      if (this._ui.previous) this._ui.previous.disabled = !!busy || this._targetIndex <= 0;
      if (this._ui.next) this._ui.next.disabled = !!busy || this._targetIndex < 0 || this._targetIndex >= targets.length - 1;
      if (this._ui.snowRefresh) this._ui.snowRefresh.disabled = !!busy || !this._snowActive;
      if (this._ui.qpfRefresh) this._ui.qpfRefresh.disabled = !!busy || !this._qpfActive;
    },

    _renderSelectors: function() {
      if (!this._ui.cycleSelect || !this._ui.targetSelect) return;
      this._ui.cycleSelect.innerHTML = '';
      this._inventoryCycles.forEach(function(cycle, index) {
        var option = document.createElement('option');
        option.value = String(index);
        option.textContent = (index === 0 ? 'Newest run · ' : 'Retained run · ') + ptSnowCycleShort(cycle.cycle_time_utc);
        this._ui.cycleSelect.appendChild(option);
      }, this);
      if (this._cycleIndex >= 0) this._ui.cycleSelect.value = String(this._cycleIndex);
      this._ui.targetSelect.innerHTML = '';
      var targets = this._inventoryCycles[this._cycleIndex] ? this._inventoryCycles[this._cycleIndex].targets : [];
      targets.forEach(function(entry, index) {
        var option = document.createElement('option');
        option.value = String(index);
        var available = entry.snowTarget && entry.qpfTarget ? ' · Snow + QPF' : (entry.snowTarget ? ' · Snow' : ' · QPF');
        option.textContent = ptSnowFormatPacific(entry.valid_time_utc) + ' · ' + ptNbmHorizonLabel(entry.lead_hours) + available;
        this._ui.targetSelect.appendChild(option);
      }, this);
      if (this._targetIndex >= 0) this._ui.targetSelect.value = String(this._targetIndex);
      this._setControlsBusy(false);
    },

    _updateIdentity: function(entry) {
      if (!entry) {
        if (this._ui.valid) this._ui.valid.textContent = '—';
        if (this._ui.validUtc) this._ui.validUtc.textContent = 'Valid UTC and lead unavailable';
        if (this._ui.cycleLine) this._ui.cycleLine.textContent = 'NBM cycle unavailable';
        if (this._ui.horizon) this._ui.horizon.textContent = 'Manifest-declared horizon unavailable';
        return;
      }
      if (this._ui.valid) this._ui.valid.textContent = ptSnowFormatPacific(entry.valid_time_utc);
      if (this._ui.validUtc) this._ui.validUtc.textContent = 'Valid ' + ptSnowUtcHour(entry.valid_time_utc) + ' · +' + entry.lead_hours + ' h';
      if (this._ui.cycleLine) this._ui.cycleLine.textContent = 'NBM run: ' + ptSnowFormatPacific(entry.cycle_utc) + ' · ' + ptSnowUtcHour(entry.cycle_utc);
      if (this._ui.horizon) this._ui.horizon.textContent = ptNbmHorizonLabel(entry.lead_hours) + ' · ' + ptNbmHorizonCue(entry.lead_hours);
    },

    _updateBanner: function() {
      var cycle = this._inventoryCycles[this._cycleIndex];
      var entry = cycle && cycle.targets[this._targetIndex];
      if (!entry) { this._setBanner('loading', 'Waiting for an active NBM manifest.'); return; }
      var parts = [];
      if (this._snowActive) parts.push(entry.snowTarget ? 'Snow exact' : 'Snow unavailable');
      if (this._qpfActive) parts.push(entry.qpfTarget ? 'QPF exact' : 'QPF unavailable');
      var incomplete = (this._snowActive && !entry.snowTarget) || (this._qpfActive && !entry.qpfTarget);
      this._setBanner(incomplete ? 'delayed' : 'current', parts.join(' · ') + ' · ' + ptNbmHorizonLabel(entry.lead_hours));
    },

    _updateMetrics: function(metrics) {
      if (!this._ui.metrics || !metrics) return;
      this._ui.metrics.textContent =
        (metrics.cacheHit ? 'Validated memory cache' : 'Network ' + ptSnowBytesText(metrics.bytes)) +
        ' · fetch ' + metrics.fetchMs.toFixed(1) + ' ms · hash ' + metrics.hashMs.toFixed(1) +
        ' ms · parse ' + metrics.parseMs.toFixed(1) + ' ms · render ' + metrics.renderMs.toFixed(1) +
        ' ms · ' + metrics.featureCount + ' lines / ' + metrics.vertexCount.toLocaleString('en-US') +
        ' vertices · cache ' + ptSnowTargetCache.size + '/' + PT_SNOW_TARGET_CACHE_LIMIT + '.';
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
      var controller = this;
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
            controller._setSnowHoverOwned(true);
            layer.setStyle({weight: baseStyle.weight + 1.4, opacity: 1});
            if (typeof layer.bringToFront === 'function') layer.bringToFront();
          });
          layer.on('mouseout', function() { controller._setSnowHoverOwned(false); layer.setStyle(baseStyle); });
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

    _setQpfState: function(kind, text) {
      if (!this._ui.qpfState) return;
      this._ui.qpfState.setAttribute('data-kind', kind);
      this._ui.qpfState.textContent = text;
    },

    _setQpfHoverStatus: function(kind, text) {
      this._qpfNumericHoverStatus = kind;
      if (this._ui.qpfHoverNote) {
        this._ui.qpfHoverNote.setAttribute('data-kind', kind);
        this._ui.qpfHoverNote.textContent = text;
      }
      this._diagnostics.qpfNumericStates.push({
        time_utc: new Date().toISOString(), kind: kind, text: text,
        forecast_state_id: this._qpfDisplayedEntry ? this._qpfDisplayedEntry.forecast_state_id : null
      });
      if (this._diagnostics.qpfNumericStates.length > 100) this._diagnostics.qpfNumericStates.shift();
    },

    _renderQpfLegend: function(cycle) {
      if (!this._ui.qpfLegend) return;
      if (!cycle || !this._qpfManifest) {
        this._ui.qpfLegend.hidden = true;
        return;
      }
      var cap = Number(cycle.legend_cap_in);
      this._ui.qpfLegend.hidden = false;
      if (this._ui.qpfCap) {
        this._ui.qpfCap.textContent = 'display cap ' + cap + (cycle.legend_overflow ? '+' : '') + ' in';
      }
      if (this._ui.qpfRamp) {
        this._ui.qpfRamp.style.background = ptQpfLegendGradient(this._qpfManifest.palette, cap);
      }
      if (this._ui.qpfRampTicks) {
        var candidates = cap <= 4 ? [0.01, 0.5, 1, 2, cap] :
          (cap <= 8 ? [0.01, 1, 2, 4, 6, cap] : [0.01, 1, 2, 4, 8, cap]);
        var ticks = candidates.filter(function(value, index, values) {
          return value <= cap && values.indexOf(value) === index;
        });
        this._ui.qpfRampTicks.innerHTML = ticks.map(function(value) {
          var label = value === 0.01 ? '0.01' : String(value);
          return '<span style="left:' + ((value / cap) * 100).toFixed(3) + '%">' +
            escapeHtml(label) + '</span>';
        }).join('');
      }
      if (this._ui.qpfOverflow) this._ui.qpfOverflow.hidden = !cycle.legend_overflow;
    },

    setQpfOpacity: function(value) {
      value = Number(value);
      if (!ptSnowFinite(value)) return false;
      value = Math.max(0.15, Math.min(0.85, value));
      ptQpfOpacity = value;
      this._qpfOpacity = value;
      if (this._ui.qpfOpacity) this._ui.qpfOpacity.value = String(value);
      if (this._ui.qpfOpacityValue) this._ui.qpfOpacityValue.textContent = Math.round(value * 100) + '%';
      if (this._qpfOverlay && typeof this._qpfOverlay.setOpacity === 'function') {
        this._qpfOverlay.setOpacity(value);
      }
      return true;
    },

    _removeQpfOverlay: function() {
      this._clearQpfNumericHover();
      var hadOverlay = !!this._qpfOverlay;
      if (this._qpfOverlay && this._map) {
        try { this._map.removeLayer(this._qpfOverlay); } catch(e) {}
      }
      this._qpfOverlay = null;
      if (this._qpfObjectUrl && window.URL && typeof window.URL.revokeObjectURL === 'function') {
        try { window.URL.revokeObjectURL(this._qpfObjectUrl); } catch(e2) {}
      }
      this._qpfObjectUrl = null;
      this._qpfDisplayedEntry = null;
      this._qpfDisplayedCycle = null;
      this._qpfFreshness = null;
      if (hadOverlay) this._diagnostics.qpfOverlayRemovals += 1;
    },

    _fetchQpfManifest: async function(token) {
      if (this._qpfManifestAbort) {
        try { this._qpfManifestAbort.abort(); } catch(e) {}
      }
      this._qpfManifestAbort = typeof AbortController === 'function' ? new AbortController() : null;
      var started = ptSnowPerfNow();
      var response = await fetch(this.options.qpfManifestUrl, {
        cache: 'no-store',
        signal: this._qpfManifestAbort ? this._qpfManifestAbort.signal : undefined
      });
      if (!response.ok) throw new Error('QPF manifest request failed with HTTP ' + response.status + '.');
      var buffer = await response.arrayBuffer();
      if (buffer.byteLength < 2 || buffer.byteLength > 512 * 1024) {
        throw new Error('QPF manifest byte size is outside the supported contract.');
      }
      if (this._removed || token !== this._qpfManifestToken) return null;
      var parseStarted = ptSnowPerfNow();
      var parsed;
      try {
        parsed = JSON.parse(new TextDecoder('utf-8').decode(buffer));
      } catch(error) {
        throw new Error('QPF manifest JSON is malformed.');
      }
      var manifest = ptQpfValidateManifest(parsed);
      var loaded = {
        manifest: manifest,
        bytes: buffer.byteLength,
        fetchMs: parseStarted - started,
        parseMs: ptSnowPerfNow() - parseStarted
      };
      this._diagnostics.qpfManifestLoads.push({
        cycle_utc: manifest.current_cycle_utc,
        retention_mode: manifest.retention_mode,
        bytes: loaded.bytes,
        fetchMs: loaded.fetchMs,
        parseMs: loaded.parseMs
      });
      if (this._diagnostics.qpfManifestLoads.length > 100) this._diagnostics.qpfManifestLoads.shift();
      return loaded;
    },

    _loadQpfTarget: async function(entry, token) {
      if (this._qpfTargetAbort) {
        try { this._qpfTargetAbort.abort(); } catch(e) {}
      }
      this._qpfTargetAbort = typeof AbortController === 'function' ? new AbortController() : null;
      var fetchStarted = ptSnowPerfNow();
      var response = await fetch(ptQpfTargetUrl(entry, this.options.qpfManifestUrl), {
        cache: 'force-cache',
        signal: this._qpfTargetAbort ? this._qpfTargetAbort.signal : undefined
      });
      if (!response.ok) throw new Error('Selected QPF target request failed with HTTP ' + response.status + '.');
      var buffer = await response.arrayBuffer();
      var fetchMs = ptSnowPerfNow() - fetchStarted;
      if (buffer.byteLength !== entry.bytes) {
        throw new Error('Selected QPF target byte count does not match the manifest.');
      }
      if (!window.crypto || !window.crypto.subtle || typeof window.crypto.subtle.digest !== 'function') {
        throw new Error('This browser cannot verify the selected QPF target SHA-256.');
      }
      var hashStarted = ptSnowPerfNow();
      var digest = ptSnowBufferHex(await window.crypto.subtle.digest('SHA-256', buffer));
      var hashMs = ptSnowPerfNow() - hashStarted;
      if (digest !== entry.sha256) throw new Error('Selected QPF target SHA-256 does not match the manifest.');
      if (this._removed || token !== this._qpfTargetToken) return null;
      if (!window.URL || typeof window.URL.createObjectURL !== 'function' || typeof window.Image !== 'function') {
        throw new Error('This browser cannot decode the selected QPF WebP safely.');
      }
      var objectUrl = window.URL.createObjectURL(new Blob([buffer], {type: 'image/webp'}));
      var decodeStarted = ptSnowPerfNow();
      try {
        var dimensions = await new Promise(function(resolve, reject) {
          var image = new window.Image();
          image.onload = function() { resolve({width: image.naturalWidth, height: image.naturalHeight}); };
          image.onerror = function() { reject(new Error('Selected QPF WebP could not be decoded.')); };
          image.src = objectUrl;
        });
        if (dimensions.width !== entry.image_width || dimensions.height !== entry.image_height) {
          throw new Error('Selected QPF WebP dimensions do not match the manifest.');
        }
      } catch(error) {
        try { window.URL.revokeObjectURL(objectUrl); } catch(revokeError) {}
        throw error;
      }
      if (this._removed || token !== this._qpfTargetToken) {
        try { window.URL.revokeObjectURL(objectUrl); } catch(staleError) {}
        return null;
      }
      return {
        objectUrl: objectUrl,
        bytes: buffer.byteLength,
        fetchMs: fetchMs,
        hashMs: hashMs,
        decodeMs: ptSnowPerfNow() - decodeStarted
      };
    },


    _clearQpfNumericHover: function() {
      if (this._qpfNumericHoverHandle && typeof this._qpfNumericHoverHandle.detach === 'function') {
        try { this._qpfNumericHoverHandle.detach(); } catch(error) { this._recordError(error); }
      }
      this._qpfNumericHoverHandle = null;
      if (this._qpfNumericHoverStatus !== 'idle') {
        this._setQpfHoverStatus('idle',
          'Exact numeric hover loads only for the selected QPF frame; palette colors are never reverse-mapped.');
      }
    },

    _syncQpfNumericHover: function(entry) {
      this._clearQpfNumericHover();
      if (!this._qpfActive || !entry || !this._qpfNumericHoverProvider ||
          typeof this._qpfNumericHoverProvider.attachSelectedFrame !== 'function') {
        this._setQpfHoverStatus('idle', 'Exact numeric QPF hover is unavailable for this selection.');
        return;
      }
      var self = this;
      var stateId = entry.forecast_state_id;
      var statusSeen = false;
      try {
        var handle = this._qpfNumericHoverProvider.attachSelectedFrame({
          map: this._map,
          manifest: this._qpfManifest,
          manifest_url: this.options.qpfManifestUrl,
          target: entry,
          target_url: ptQpfTargetUrl(entry, this.options.qpfManifestUrl),
          numeric_url: ptQpfNumericUrl(entry, this.options.qpfManifestUrl),
          exact_identity: {
            cycle_utc: entry.cycle_utc,
            valid_time_utc: entry.valid_time_utc,
            lead_hours: entry.lead_hours
          },
          suppress_when: 'snow_contour_hover',
          onStatus: function(kind, text) {
            if (!self._qpfActive || !self._qpfDisplayedEntry ||
                self._qpfDisplayedEntry.forecast_state_id !== stateId) return;
            statusSeen = true;
            self._setQpfHoverStatus(kind, text);
            if (kind === 'error') self._recordError(new Error(text));
          }
        });
        if (handle && typeof handle.detach === 'function') {
          this._qpfNumericHoverHandle = handle;
          if (typeof handle.setSuppressed === 'function') handle.setSuppressed(this._snowHoverOwned);
          if (!statusSeen) {
            this._setQpfHoverStatus('ready', 'Selected-frame numeric QPF hover provider attached.');
          }
        } else {
          this._setQpfHoverStatus('error', 'QPF numeric hover provider did not return a detachable handle.');
        }
      } catch(error) {
        this._recordError(error);
        this._setQpfHoverStatus('error',
          (error && error.message ? error.message : String(error)) +
          ' Numeric hover is disabled; the validated image remains displayed.');
      }
    },

    setQpfNumericHoverProvider: function(provider) {
      if (provider !== null && (!provider || typeof provider.attachSelectedFrame !== 'function')) {
        throw new Error('QPF numeric hover provider must expose attachSelectedFrame(context).');
      }
      var previous = this._qpfNumericHoverProvider;
      this._clearQpfNumericHover();
      if (previous && previous !== provider && typeof previous.clearCache === 'function') {
        previous.clearCache();
      }
      this._qpfNumericHoverProvider = provider;
      this._syncQpfNumericHover(this._qpfDisplayedEntry);
      return true;
    },

    _setSnowHoverOwned: function(owned) {
      this._snowHoverOwned = !!owned;
      if (this._qpfNumericHoverHandle && typeof this._qpfNumericHoverHandle.setSuppressed === 'function') {
        this._qpfNumericHoverHandle.setSuppressed(this._snowHoverOwned);
      }
    },

    _removeSnowDisplayed: function() {
      if (this._displayGroup && this._map) {
        try { this._map.removeLayer(this._displayGroup); } catch(error) {}
      }
      this._displayGroup = null;
      this._displayedEntry = null;
      this._displayedGeoJson = null;
      if (this._labelGroup) this._labelGroup.clearLayers();
      this._setSnowHoverOwned(false);
    },

    _loadSnowSelection: async function(state, reason) {
      if (!this._snowActive) return null;
      var entry = state && state.snowTarget;
      if (!entry) {
        this._requestToken += 1;
        if (this._targetAbort) { try { this._targetAbort.abort(); } catch(error) {} }
        this._removeSnowDisplayed();
        this._setProductState('snow', 'unavailable',
          'No exact Snow target exists for this cycle + lead + valid time. No substitute is used.');
        setOpsLayerLoading(PT_SNOW_PRODUCT_NAME, false);
        recordStatus(PT_SNOW_PRODUCT_NAME, 'Exact selected Snow state is unavailable.', 'pt-ops-warn');
        return null;
      }
      if (this._displayedEntry && this._displayedEntry.path === entry.path && this._displayGroup) {
        this._setProductState('snow', 'paired', 'Exact Snow target displayed · +' + entry.lead_hours + ' h.');
        return entry;
      }
      var token = ++this._requestToken;
      var switchStart = ptSnowPerfNow();
      this._removeSnowDisplayed();
      this._setProductState('snow', 'loading', 'Loading exact Snow target ' + ptNbmHorizonLabel(entry.lead_hours) + '…');
      setOpsLayerLoading(PT_SNOW_PRODUCT_NAME, true);
      try {
        var fetched = await this._fetchTarget(entry);
        if (this._removed || !this._snowActive || token !== this._requestToken) return null;
        var renderStart = ptSnowPerfNow();
        var candidate = this._buildDisplayGroup(fetched.payload, entry);
        candidate.addTo(this._map);
        if (typeof window.requestAnimationFrame === 'function') {
          await new Promise(function(resolve) {
            window.requestAnimationFrame(function() { window.requestAnimationFrame(resolve); });
          });
        }
        if (this._removed || !this._snowActive || token !== this._requestToken) {
          try { this._map.removeLayer(candidate); } catch(cancelError) {}
          return null;
        }
        this._displayGroup = candidate;
        this._displayedEntry = entry;
        this._displayedGeoJson = fetched.payload;
        var labels = this._renderLabels();
        var metrics = {
          reason: reason || 'shared time selection',
          path: entry.path, cacheHit: fetched.cacheHit, bytes: fetched.bytes,
          fetchMs: fetched.fetchMs, hashMs: fetched.hashMs, parseMs: fetched.parseMs,
          renderMs: ptSnowPerfNow() - renderStart, switchMs: ptSnowPerfNow() - switchStart,
          featureCount: fetched.featureCount, vertexCount: fetched.vertexCount, labelCount: labels
        };
        this._lastMetrics = metrics;
        this._diagnostics.targetLoads.push(metrics);
        if (this._diagnostics.targetLoads.length > 100) this._diagnostics.targetLoads.shift();
        this._updateMetrics(metrics);
        this._setProductState('snow', 'paired', 'Exact Snow target displayed · ' + ptNbmHorizonLabel(entry.lead_hours) + '.');
        setOpsLayerLoading(PT_SNOW_PRODUCT_NAME, false);
        recordStatus(PT_SNOW_PRODUCT_NAME, 'Exact target valid ' + ptSnowFormatPacific(entry.valid_time_utc) + ' displayed.', 'pt-ops-ok');
        return entry;
      } catch(error) {
        if (this._removed || !this._snowActive || token !== this._requestToken ||
            (error && error.name === 'AbortError')) return null;
        this._recordError(error);
        this._removeSnowDisplayed();
        this._setProductState('snow', 'error',
          (error && error.message ? error.message : String(error)) + ' No Snow contours are displayed.');
        setOpsLayerLoading(PT_SNOW_PRODUCT_NAME, false);
        recordStatus(PT_SNOW_PRODUCT_NAME, 'Selected exact target failed validation.', 'pt-ops-bad');
        return null;
      }
    },

    _loadQpfSelection: function(state, reason) {
      if (!this._qpfActive) return Promise.resolve(null);
      var entry = state && state.qpfTarget;
      if (!entry) {
        this._qpfTargetToken += 1;
        if (this._qpfTargetAbort) { try { this._qpfTargetAbort.abort(); } catch(error) {} }
        this._qpfPendingPath = null;
        this._qpfPendingPromise = null;
        this._removeQpfOverlay();
        this._renderQpfLegend(null);
        this._clearQpfNumericHover();
        this._setQpfState(state && Number(state.lead_hours) === 1 ? 'snow-only' : 'unavailable',
          state && Number(state.lead_hours) === 1
            ? 'Snow +1 h is intentionally Snow-only; QPF begins at its manifest-declared first lead.'
            : 'No exact QPF target exists for this cycle + lead + valid time. No substitute is used.');
        setOpsLayerLoading(PT_QPF_PRODUCT_NAME, false);
        recordStatus(PT_QPF_PRODUCT_NAME, 'Exact selected QPF state is unavailable.', 'pt-ops-warn');
        return Promise.resolve(null);
      }
      var cycle = state.qpfCycle;
      this._renderQpfLegend(cycle);
      this._qpfFreshness = ptQpfFreshness(entry.cycle_utc, ptSnowNow());
      if (this._qpfDisplayedEntry && this._qpfDisplayedEntry.image_path === entry.image_path && this._qpfOverlay) {
        this._setQpfState('paired', 'Exact QPF displayed · preceding 6 h ending ' +
          ptSnowFormatPacific(entry.valid_time_utc, true) + '.');
        this._syncQpfNumericHover(entry);
        return Promise.resolve(entry);
      }
      if (this._qpfPendingPath === entry.image_path && this._qpfPendingPromise) return this._qpfPendingPromise;
      var self = this;
      var token = ++this._qpfTargetToken;
      this._removeQpfOverlay();
      this._setQpfState('loading', 'Loading exact QPF target ' + ptNbmHorizonLabel(entry.lead_hours) + '…');
      setOpsLayerLoading(PT_QPF_PRODUCT_NAME, true);
      var pending = this._loadQpfTarget(entry, token).then(function(loaded) {
        if (self._qpfPendingPath === entry.image_path) {
          self._qpfPendingPath = null;
          self._qpfPendingPromise = null;
        }
        if (!loaded || self._removed || !self._qpfActive || token !== self._qpfTargetToken) return null;
        var candidate = L.imageOverlay(loaded.objectUrl, PT_QPF_LEAFLET_BOUNDS, {
          pane: 'pane_ops_qpf', opacity: self._qpfOpacity, interactive: false,
          className: 'pt-nbm-qpf-image'
        });
        self._qpfOverlay = candidate;
        self._qpfObjectUrl = loaded.objectUrl;
        candidate.addTo(self._map);
        self._qpfDisplayedEntry = entry;
        self._qpfDisplayedCycle = cycle;
        self._diagnostics.qpfOverlayReplacements += 1;
        self._diagnostics.qpfTargetLoads.push({
          reason: reason || 'shared time selection', image_path: entry.image_path,
          cycle_utc: entry.cycle_utc, valid_time_utc: entry.valid_time_utc,
          lead_hours: entry.lead_hours, bytes: loaded.bytes, fetchMs: loaded.fetchMs,
          hashMs: loaded.hashMs, decodeMs: loaded.decodeMs
        });
        if (self._diagnostics.qpfTargetLoads.length > 100) self._diagnostics.qpfTargetLoads.shift();
        self._setQpfState('paired', 'Exact QPF displayed · preceding 6 h ending ' +
          ptSnowFormatPacific(entry.valid_time_utc, true) + '.');
        self._syncQpfNumericHover(entry);
        setOpsLayerLoading(PT_QPF_PRODUCT_NAME, false);
        recordStatus(PT_QPF_PRODUCT_NAME, 'Exact target valid ' +
          ptSnowFormatPacific(entry.valid_time_utc) + ' displayed.',
          self._qpfFreshness === 'current' ? 'pt-ops-ok' : 'pt-ops-warn');
        return entry;
      }).catch(function(error) {
        if (self._qpfPendingPath === entry.image_path) {
          self._qpfPendingPath = null;
          self._qpfPendingPromise = null;
        }
        if (self._removed || !self._qpfActive || token !== self._qpfTargetToken ||
            (error && error.name === 'AbortError')) return null;
        self._recordError(error);
        self._removeQpfOverlay();
        self._clearQpfNumericHover();
        self._setQpfState('error',
          (error && error.message ? error.message : String(error)) + ' No QPF surface is displayed.');
        setOpsLayerLoading(PT_QPF_PRODUCT_NAME, false);
        recordStatus(PT_QPF_PRODUCT_NAME, 'Selected exact target failed validation.', 'pt-ops-bad');
        return null;
      });
      this._qpfPendingPath = entry.image_path;
      this._qpfPendingPromise = pending;
      return pending;
    },

    _findStateIndex: function(cycleUtc, validUtc, leadHours) {
      for (var cycleIndex = 0; cycleIndex < this._inventoryCycles.length; cycleIndex += 1) {
        if (this._inventoryCycles[cycleIndex].cycle_time_utc !== cycleUtc) continue;
        var targetIndex = this._inventoryCycles[cycleIndex].targets.findIndex(function(entry) {
          return entry.valid_time_utc === validUtc && Number(entry.lead_hours) === Number(leadHours);
        });
        if (targetIndex >= 0) return {cycleIndex: cycleIndex, targetIndex: targetIndex};
      }
      return null;
    },

    _defaultStateIndex: function() {
      if (!this._inventoryCycles.length) return null;
      var cycle = this._inventoryCycles[0];
      var nowMs = ptSnowNow();
      var ranked = cycle.targets.map(function(entry, index) {
        var activeSnow = entry.snowTarget &&
          nowMs >= Date.parse(entry.snowTarget.valid_from_utc) &&
          nowMs <= Date.parse(entry.snowTarget.valid_through_utc);
        return {index: index, activeSnow: !!activeSnow,
          delta: Math.abs(Date.parse(entry.valid_time_utc) - nowMs)};
      }).sort(function(left, right) {
        if (left.activeSnow !== right.activeSnow) return left.activeSnow ? -1 : 1;
        return left.delta - right.delta || left.index - right.index;
      });
      return {cycleIndex: 0, targetIndex: ranked.length ? ranked[0].index : 0};
    },

    _rebuildInventory: function(reason) {
      var prior = this.getTimeState();
      this._inventoryCycles = ptNbmBuildInventory(
        this._cycles, this._qpfManifest ? this._qpfManifest._ptQpfCycles : [],
        this._snowActive, this._qpfActive
      );
      this._diagnostics.inventoryBuilds += 1;
      var selected = prior.cycle_utc
        ? this._findStateIndex(prior.cycle_utc, prior.valid_time_utc, prior.lead_hours) : null;
      if (!selected) selected = this._defaultStateIndex();
      if (!selected) {
        this._cycleIndex = -1;
        this._targetIndex = -1;
        this._timeState = {cycle_utc: null, valid_time_utc: null, lead_hours: null};
        this._renderSelectors();
        this._updateIdentity(null);
        this._updateBanner();
        return Promise.resolve(null);
      }
      return this.selectTarget(selected.cycleIndex, selected.targetIndex,
        reason || 'manifest-driven inventory rebuild');
    },

    selectTarget: function(cycleIndex, targetIndex, reason) {
      cycleIndex = Number(cycleIndex);
      targetIndex = Number(targetIndex);
      var cycle = this._inventoryCycles[cycleIndex];
      var state = cycle && cycle.targets[targetIndex];
      if (this._removed || !state) return Promise.resolve(null);
      this._cycleIndex = cycleIndex;
      this._targetIndex = targetIndex;
      this._timeState = {
        cycle_utc: state.cycle_utc,
        valid_time_utc: state.valid_time_utc,
        lead_hours: state.lead_hours
      };
      this._diagnostics.selections += 1;
      this._renderSelectors();
      this._updateIdentity(state);
      this._updateBanner();
      this._emitTimeSelection(reason);
      return Promise.all([
        this._loadSnowSelection(state, reason),
        this._loadQpfSelection(state, reason)
      ]).then(function() { return state; });
    },

    stepValidTime: function(delta) {
      delta = Number(delta);
      if (!Number.isInteger(delta) || delta === 0 || this._targetIndex < 0) return false;
      var cycle = this._inventoryCycles[this._cycleIndex];
      var targetIndex = this._targetIndex + delta;
      if (!cycle || targetIndex < 0 || targetIndex >= cycle.targets.length) return false;
      this.selectTarget(this._cycleIndex, targetIndex, 'step valid time');
      return true;
    },

    selectCycle: function(cycleIndex) {
      cycleIndex = Number(cycleIndex);
      var cycle = this._inventoryCycles[cycleIndex];
      if (!cycle) return false;
      var desired = this.getTimeState();
      var targetIndex = cycle.targets.findIndex(function(entry) {
        return Number(entry.lead_hours) === Number(desired.lead_hours);
      });
      if (targetIndex < 0) {
        targetIndex = cycle.targets.map(function(entry, index) {
          return {index: index, delta: Math.abs(Date.parse(entry.valid_time_utc) - Date.parse(desired.valid_time_utc))};
        }).sort(function(left, right) { return left.delta - right.delta || left.index - right.index; })[0].index;
      }
      this.selectTarget(cycleIndex, targetIndex, 'cycle selection');
      return true;
    },


    _applySnowManifest: function(loaded, reason) {
      if (!loaded || this._removed || !this._snowActive) return Promise.resolve(null);
      this._manifest = loaded.manifest;
      this._cycles = loaded.manifest._ptSnowCycles;
      this._freshness = ptSnowFreshness(this._manifest, ptSnowNow());
      this._setProductState('snow', this._freshness === 'current' ? 'paired' : 'loading',
        'Snow manifest validated · ' + this._cycles.length + ' cycles · ' +
        this._cycles[0].targets.length + ' manifest-declared leads.');
      if (this._ui.metrics) {
        this._ui.metrics.textContent = 'Snow manifest ' + ptSnowBytesText(loaded.bytes) +
          ' · fetch ' + loaded.fetchMs.toFixed(1) + ' ms · validate ' +
          loaded.parseMs.toFixed(1) + ' ms · ' + this._cycles.length + ' cycles / ' +
          this._cycles.reduce(function(total, cycle) { return total + cycle.targets.length; }, 0) +
          ' targets.';
      }
      return this._rebuildInventory(reason || 'Snow manifest refresh');
    },

    _applyQpfManifest: function(loaded, reason) {
      if (!loaded || this._removed || !this._qpfActive) return Promise.resolve(null);
      this._qpfManifest = loaded.manifest;
      var cycles = loaded.manifest._ptQpfCycles;
      this._setQpfState('paired', 'QPF manifest validated · ' + cycles.length + ' cycle' +
        (cycles.length === 1 ? '' : 's') + ' · ' +
        cycles[0].targets.length + ' manifest-declared leads.');
      return this._rebuildInventory(reason || 'QPF manifest refresh');
    },

    _refreshSnowManifest: function(reason) {
      if (this._removed || !this._snowActive || !this.options.manifestUrl) return Promise.resolve(null);
      var self = this;
      var token = ++this._requestToken;
      this._setProductState('snow', 'loading', 'Fetching and validating the public Snow manifest…');
      setOpsLayerLoading(PT_SNOW_PRODUCT_NAME, true);
      return this._fetchManifest(token).then(function(loaded) {
        if (!loaded || self._removed || !self._snowActive || token !== self._requestToken) return null;
        setOpsLayerLoading(PT_SNOW_PRODUCT_NAME, false);
        return self._applySnowManifest(loaded, reason);
      }).catch(function(error) {
        if (self._removed || !self._snowActive || token !== self._requestToken ||
            (error && error.name === 'AbortError')) return null;
        self._recordError(error);
        self._setProductState('snow', 'error', 'Snow manifest unavailable or invalid.' +
          (self._displayedEntry ? ' Previously validated exact Snow remains displayed.' : ''));
        setOpsLayerLoading(PT_SNOW_PRODUCT_NAME, false);
        recordStatus(PT_SNOW_PRODUCT_NAME, 'Snow manifest unavailable or invalid.', 'pt-ops-bad');
        return null;
      });
    },

    _refreshQpfManifest: function(reason) {
      if (this._removed || !this._qpfActive || !this.options.qpfManifestUrl) return Promise.resolve(null);
      var self = this;
      var token = ++this._qpfManifestToken;
      this._clearQpfNumericHover();
      this._setQpfState('loading', 'Fetching and validating the public QPF manifest…');
      setOpsLayerLoading(PT_QPF_PRODUCT_NAME, true);
      return this._fetchQpfManifest(token).then(function(loaded) {
        if (!loaded || self._removed || !self._qpfActive || token !== self._qpfManifestToken) return null;
        setOpsLayerLoading(PT_QPF_PRODUCT_NAME, false);
        return self._applyQpfManifest(loaded, reason);
      }).catch(function(error) {
        if (self._removed || !self._qpfActive || token !== self._qpfManifestToken ||
            (error && error.name === 'AbortError')) return null;
        self._recordError(error);
        self._setQpfState('error', 'QPF manifest unavailable or invalid.' +
          (self._qpfDisplayedEntry ? ' Previously validated exact QPF remains displayed.' : ''));
        setOpsLayerLoading(PT_QPF_PRODUCT_NAME, false);
        recordStatus(PT_QPF_PRODUCT_NAME, 'QPF manifest unavailable or invalid.', 'pt-ops-bad');
        return null;
      });
    },

    refreshProduct: function(product, reason) {
      return product === 'qpf'
        ? this._refreshQpfManifest(reason || 'QPF layer refresh')
        : this._refreshSnowManifest(reason || 'Snow layer refresh');
    },

    refreshCurrentView: function() {
      var requests = [];
      if (this._snowActive) requests.push(this._refreshSnowManifest('shared scheduled refresh'));
      if (this._qpfActive) requests.push(this._refreshQpfManifest('shared scheduled refresh'));
      return Promise.all(requests);
    },

    _registerTimeController: function() {
      window.BRIM = window.BRIM || {};
      window.BRIM.opsLiveTimeControllers = window.BRIM.opsLiveTimeControllers || {};
      window.BRIM.opsLiveTimeControllers.nbmForecast = this;
      window.BRIM.opsLiveTimeControllers.nbmSnowLevels = this;
    },

    _unregisterTimeController: function() {
      var controllers = window.BRIM && window.BRIM.opsLiveTimeControllers;
      if (!controllers) return;
      if (controllers.nbmForecast === this) delete controllers.nbmForecast;
      if (controllers.nbmSnowLevels === this) delete controllers.nbmSnowLevels;
    },

    _startSharedLifecycle: function(mapObj) {
      if (!this._removed) return;
      this._removed = false;
      this._map = mapObj;
      this._createCard();
      this._registerTimeController();
      var self = this;
      this._mapMoveHandler = function() { self._renderLabels(); };
      mapObj.on('zoomend moveend', this._mapMoveHandler);
      this._visibilityHandler = function() {
        if (!self._removed && document.visibilityState === 'visible') self.refreshCurrentView();
      };
      document.addEventListener('visibilitychange', this._visibilityHandler);
      this._refreshTimer = window.setInterval(function() { self.refreshCurrentView(); }, PT_SNOW_REFRESH_MS);
    },

    activateProduct: function(product, mapObj) {
      if (product !== 'snow' && product !== 'qpf') return false;
      this._startSharedLifecycle(mapObj);
      if (product === 'snow') {
        if (this._snowActive) return true;
        this._snowActive = true;
        this._diagnostics.activations.snow += 1;
        if (!this._labelGroup) this._labelGroup = L.layerGroup().addTo(mapObj);
        activeLegendDefs[PT_SNOW_PRODUCT_NAME] = {
          unifiedCard: true, legendType: 'nbm_snow_levels',
          note: 'NOAA/NBM modeled snow-level elevation. Exact selected content-addressed target only.',
          sourceUrl: this.options.sourceUrl || '', infoUrl: this.options.manifestUrl || '',
          infoLabel: 'feed manifest'
        };
        this.setLabelsVisible(ptSnowLabelsVisible);
        recordStatus(PT_SNOW_PRODUCT_NAME, 'Fetching the public Snow manifest…', 'pt-ops-warn');
      } else {
        if (this._qpfActive) return true;
        this._qpfActive = true;
        this._diagnostics.activations.qpf += 1;
        activeLegendDefs[PT_QPF_PRODUCT_NAME] = {
          unifiedCard: true, legendType: 'nbm_qpf',
          note: 'NOAA/NBM native preceding-six-hour QPF. Exact selected content-addressed target only.',
          sourceUrl: this.options.sourceUrl || '', infoUrl: this.options.qpfManifestUrl || '',
          infoLabel: 'feed manifest'
        };
        this.setQpfOpacity(ptQpfOpacity);
        recordStatus(PT_QPF_PRODUCT_NAME, 'Fetching the public QPF manifest…', 'pt-ops-warn');
      }
      this._updateProductSections();
      redrawLegend();
      this.refreshProduct(product, product + ' activation');
      return true;
    },

    deactivateProduct: function(product, mapObj) {
      if (product === 'snow') {
        if (!this._snowActive) return true;
        this._snowActive = false;
        this._diagnostics.removals.snow += 1;
        this._requestToken += 1;
        if (this._manifestAbort) { try { this._manifestAbort.abort(); } catch(error1) {} }
        if (this._targetAbort) { try { this._targetAbort.abort(); } catch(error2) {} }
        this._manifestAbort = null;
        this._targetAbort = null;
        this._removeSnowDisplayed();
        if (this._labelGroup && mapObj) {
          try { mapObj.removeLayer(this._labelGroup); } catch(error3) {}
        }
        this._labelGroup = null;
        delete activeLegendDefs[PT_SNOW_PRODUCT_NAME];
        setOpsLayerLoading(PT_SNOW_PRODUCT_NAME, false);
        recordStatus(PT_SNOW_PRODUCT_NAME, 'Layer turned off.', 'pt-ops-muted');
      } else if (product === 'qpf') {
        if (!this._qpfActive) return true;
        this._qpfActive = false;
        this._diagnostics.removals.qpf += 1;
        this._qpfManifestToken += 1;
        this._qpfTargetToken += 1;
        if (this._qpfManifestAbort) { try { this._qpfManifestAbort.abort(); } catch(error4) {} }
        if (this._qpfTargetAbort) { try { this._qpfTargetAbort.abort(); } catch(error5) {} }
        this._qpfManifestAbort = null;
        this._qpfTargetAbort = null;
        this._qpfPendingPath = null;
        this._qpfPendingPromise = null;
        this._removeQpfOverlay();
        this._renderQpfLegend(null);
        this._clearQpfNumericHover();
        if (this._qpfNumericHoverProvider &&
            typeof this._qpfNumericHoverProvider.clearCache === 'function') {
          this._qpfNumericHoverProvider.clearCache();
        }
        delete activeLegendDefs[PT_QPF_PRODUCT_NAME];
        setOpsLayerLoading(PT_QPF_PRODUCT_NAME, false);
        recordStatus(PT_QPF_PRODUCT_NAME, 'Layer turned off.', 'pt-ops-muted');
      } else {
        return false;
      }
      this._updateProductSections();
      redrawLegend();
      if (this._snowActive || this._qpfActive) {
        this._rebuildInventory(product + ' deactivation');
      } else {
        this._stopSharedLifecycle(mapObj || this._map);
      }
      return true;
    },

    _stopSharedLifecycle: function(mapObj) {
      if (this._removed) return;
      this._removed = true;
      this._requestToken += 1;
      this._qpfManifestToken += 1;
      this._qpfTargetToken += 1;
      if (this._refreshTimer) window.clearInterval(this._refreshTimer);
      this._refreshTimer = null;
      if (this._visibilityHandler) document.removeEventListener('visibilitychange', this._visibilityHandler);
      this._visibilityHandler = null;
      if (this._mapMoveHandler && mapObj) {
        try { mapObj.off('zoomend moveend', this._mapMoveHandler); } catch(error) {}
      }
      this._mapMoveHandler = null;
      this._removeSnowDisplayed();
      this._removeQpfOverlay();
      this._clearQpfNumericHover();
      if (this._qpfNumericHoverProvider &&
          typeof this._qpfNumericHoverProvider.clearCache === 'function') {
        this._qpfNumericHoverProvider.clearCache();
      }
      if (this._detachable && typeof this._detachable.destroy === 'function') {
        try { this._detachable.destroy(false); } catch(detachError) {}
      }
      this._detachable = null;
      if (this._cardControl && mapObj) {
        try { mapObj.removeControl(this._cardControl); } catch(controlError) {}
      }
      this._cardControl = null;
      this._card = null;
      this._ui = {};
      this._manifest = null;
      this._cycles = [];
      this._freshness = null;
      this._qpfManifest = null;
      this._qpfFreshness = null;
      this._inventoryCycles = [];
      this._cycleIndex = -1;
      this._targetIndex = -1;
      this._timeState = {cycle_utc: null, valid_time_utc: null, lead_hours: null};
      this._unregisterTimeController();
      this._map = null;
    },

    showCard: function() {
      if (!this._card) return;
      this._card.style.display = '';
      var shared = window.BRIM && window.BRIM.legendCloseout;
      if (shared && typeof shared.scheduleLayout === 'function') shared.scheduleLayout(this._card);
    }
  });

  var PtOpsNbmProductLayer = L.Layer.extend({
    initialize: function(controller, product) {
      this._controller = controller;
      this._product = product;
      this._active = false;
      this._map = null;
    },
    onAdd: function(mapObj) {
      if (this._active) return;
      this._active = true;
      this._map = mapObj;
      this._controller.activateProduct(this._product, mapObj);
    },
    onRemove: function(mapObj) {
      if (!this._active) return;
      this._active = false;
      this._controller.deactivateProduct(this._product, mapObj || this._map);
      this._map = null;
    },
    refreshCurrentView: function() {
      return this._controller.refreshProduct(this._product, this._product + ' row refresh');
    },
    forceRemove: function(mapObj) { this.onRemove(mapObj || this._map); }
  });

  var ptNbmForecastController = null;
  if ((includeNbmSnowLevels && NBM_SNOW_LEVELS_MANIFEST_URL) ||
      (includeNbmQpf && NBM_QPF_MANIFEST_URL)) {
    ptNbmForecastController = new PtOpsNbmForecastController({
      manifestUrl: NBM_SNOW_LEVELS_MANIFEST_URL,
      qpfManifestUrl: NBM_QPF_MANIFEST_URL,
      sourceUrl: 'https://vlab.noaa.gov/web/mdl/nbm-weather-elements'
    });
  }

  window.ptNbmSnowLevelsSetLabelsVisible = function(show) {
    ptSnowLabelsVisible = !!show;
    Array.prototype.forEach.call(
      document.querySelectorAll('[data-pt-ops-action="nbm-snow-labels"]'),
      function(toggle) { toggle.checked = ptSnowLabelsVisible; }
    );
    if (ptNbmForecastController) ptNbmForecastController.setLabelsVisible(ptSnowLabelsVisible);
  };

  window.ptNbmSnowLevelsShowCard = function() {
    if (ptNbmForecastController) ptNbmForecastController.showCard();
  };

  window.ptNbmForecastRegisterQpfNumericHoverProvider = function(provider) {
    if (!ptNbmForecastController) return false;
    return ptNbmForecastController.setQpfNumericHoverProvider(provider);
  };

  if (ptNbmForecastController && includeNbmSnowLevels && NBM_SNOW_LEVELS_MANIFEST_URL) {
    addOpsLayer({
      category: 'Forecasts / Outlooks',
      subgroup: 'Weather Forecasts / Outlooks',
      name: PT_SNOW_PRODUCT_NAME,
      panelOrder: -100,
      sourceUrl: 'https://vlab.noaa.gov/web/mdl/nbm-weather-elements',
      infoUrl: NBM_SNOW_LEVELS_MANIFEST_URL,
      infoLabel: 'feed manifest',
      refreshable: true,
      extraRowHtml: '<label class="pt-ops-row-mini-toggle" title="Show/hide NBM Snow Levels contour labels"><input type="checkbox" data-pt-ops-action="nbm-snow-labels" checked>lbl</label>',
      helperText: 'NOAA/NBM forecast snow-level elevation. +1 h is Snow-only.',
      layer: new PtOpsNbmProductLayer(ptNbmForecastController, 'snow')
    });
  }

  if (ptNbmForecastController && includeNbmQpf && NBM_QPF_MANIFEST_URL) {
    addOpsLayer({
      category: 'Forecasts / Outlooks',
      subgroup: 'Weather Forecasts / Outlooks',
      name: PT_QPF_PRODUCT_NAME,
      panelOrder: -99,
      sourceUrl: 'https://vlab.noaa.gov/web/mdl/nbm-weather-elements',
      infoUrl: NBM_QPF_MANIFEST_URL,
      infoLabel: 'feed manifest',
      refreshable: true,
      helperText: 'NOAA/NBM precipitation forecast for the preceding six hours.',
      layer: new PtOpsNbmProductLayer(ptNbmForecastController, 'qpf')
    });
  }

  )---"
}
