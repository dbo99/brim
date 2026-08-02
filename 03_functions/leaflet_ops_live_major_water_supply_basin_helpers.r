# ==== leaflet_ops_live_major_water_supply_basin_helpers.r ===================
##
## PURPOSE:
##   Build-time payload preparation and browser-side controller for the Ops
##   Live Major Water-Supply Basin Forecasts layer.
##
## CONTRACT:
##   - static geometry and literal mappings are embedded once at map build time;
##   - CNRFC and CBRFC live payloads are fetched and committed independently;
##   - metric_state, not record status, controls thematic eligibility;
##   - popup HTML is generated only when a polygon is opened;
##   - this helper never requests reservoir data.
# ============================================================================

pt_ops_live_major_basin_row_list <- function(path, expected_rows, label) {
  if (!file.exists(path)) stop("Missing ", label, ": ", path)
  x <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(x) != expected_rows) {
    stop(label, " must contain exactly ", expected_rows, " rows; found ", nrow(x))
  }
  lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE]))
}

pt_ops_live_major_basin_geojson <- function(x) {
  required <- c(
    "geometry_id", "geometry_role", "view_group", "display_name",
    "river_name", "reservoir_name", "generalization_note", "display_order"
  )
  if (!inherits(x, "sf") || nrow(x) != 23L) {
    stop("Major water-supply basin map geometry must be an sf object with exactly 23 rows.")
  }
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("Major basin geometry is missing: ", paste(missing, collapse = ", "))
  if (is.na(sf::st_crs(x)) || sf::st_crs(x)$epsg != 4326L) {
    stop("Major water-supply basin map geometry must use EPSG:4326.")
  }
  if (any(!sf::st_is_valid(x)) || any(sf::st_is_empty(x))) {
    stop("Major water-supply basin map geometry contains invalid or empty features.")
  }

  compact <- x[, c(required, attr(x, "sf_column")), drop = FALSE]
  geojson_path <- tempfile("brim_major_water_supply_", fileext = ".geojson")
  on.exit(unlink(geojson_path), add = TRUE)
  suppressWarnings(sf::st_write(compact, geojson_path, driver = "GeoJSON", quiet = TRUE))
  jsonlite::fromJSON(geojson_path, simplifyVector = FALSE)
}

pt_ops_live_major_basin_payload <- function(geometry) {
  config_dir <- "00_config"
  list(
    geometry = pt_ops_live_major_basin_geojson(geometry),
    productMapping = pt_ops_live_major_basin_row_list(
      file.path(config_dir, "major_water_supply_basin_product_mapping.csv"),
      54L,
      "major water-supply basin product mapping"
    ),
    geometryCatalog = pt_ops_live_major_basin_row_list(
      file.path(config_dir, "major_water_supply_basin_geometry_catalog.csv"),
      23L,
      "major water-supply basin geometry catalog"
    ),
    componentManifest = pt_ops_live_major_basin_row_list(
      file.path(config_dir, "major_water_supply_basin_component_manifest.csv"),
      18L,
      "major water-supply basin component manifest"
    ),
    reservoirCrosswalk = pt_ops_live_major_basin_row_list(
      file.path(config_dir, "major_water_supply_basin_reservoir_crosswalk.csv"),
      14L,
      "major water-supply basin reservoir crosswalk"
    ),
    relatedLinks = pt_ops_live_major_basin_row_list(
      file.path(config_dir, "major_water_supply_basin_related_links.csv"),
      21L,
      "major water-supply basin related-link registry"
    )
  )
}

pt_ops_live_major_water_supply_basin_js <- function() {
  r"---(
  // --------------------------------------------------------------------------
  // Major Water-Supply Basin Forecasts.
  // --------------------------------------------------------------------------
  var PT_MAJOR_BASIN_LAYER_NAME = 'Major Water-Supply Basin Forecasts';
  var PT_MAJOR_CNRFC_KEYS = MAJOR_WATER_SUPPLY_PRODUCT_MAPPING
    .filter(function(row) { return row.source_family === 'CNRFC'; })
    .map(function(row) { return row.forecast_key; });
  var PT_MAJOR_CBRFC_KEYS = [
    'CBRFC:GLDA3:APR_JUL_WSUP',
    'CBRFC:GLDA3:WATER_YEAR_INFLOW',
    'CBRFC:LKSA3:LOCAL_INTERVENING_MONTHLY'
  ];
  var PT_MAJOR_PERCENT_CLASSES = [
    {min: -Infinity, max: 50, color: '#8c510a', label: '< 50%'},
    {min: 50, max: 75, color: '#d8b365', label: '50–74%'},
    {min: 75, max: 90, color: '#f6e8c3', label: '75–89%'},
    {min: 90, max: 110, color: '#c7eae5', label: '90–109%'},
    {min: 110, max: 125, color: '#80cdc1', label: '110–124%'},
    {min: 125, max: 150, color: '#35978f', label: '125–149%'},
    {min: 150, max: Infinity, color: '#01665e', label: '≥ 150%'}
  ];
  var PT_MAJOR_VOLUME_COLORS = ['#eff3ff', '#bdd7e7', '#6baed6', '#3182bd', '#08519c'];
  var PT_MAJOR_INDEX_OPTIONS = [
    ['SACC0_FNF', 'Sacramento Valley'],
    ['VNSC0_FNF', 'San Joaquin Valley'],
    ['MLIC0_FNF', 'Central Valley']
  ];
  var PT_MAJOR_BASIN_OPTIONS = MAJOR_WATER_SUPPLY_GEOMETRY_CATALOG
    .filter(function(row) { return row.view_group === 'major_basin' && String(row.include_in_display).toUpperCase() === 'TRUE'; })
    .map(function(row) {
      return {
        id: row.geometry_id, label: row.display_name, river: row.river_name,
        reservoir: row.reservoir_name, lid: row.nws_lid, sourceUrl: row.source_url
      };
    })
    .sort(function(a, b) { return a.label.localeCompare(b.label); });

  function ptMajorChoiceHtml(group, label, rows, value, extraClass) {
    var html = '<fieldset class="pt-major-choice ' + escapeHtml(extraClass || '') + '"><legend>' + escapeHtml(label) + '</legend><div class="pt-major-segments" role="radiogroup" aria-label="' + escapeHtml(label) + '">';
    rows.forEach(function(row, index) {
      var id = 'pt-major-' + group + '-' + String(row[0]).replace(/[^a-z0-9_-]/gi, '-');
      html += '<input type="radio" id="' + escapeHtml(id) + '" name="pt-major-' + escapeHtml(group) + '" value="' + escapeHtml(row[0]) + '" data-state-key="' + escapeHtml(group) + '"' + (row[0] === value ? ' checked' : '') + (row[2] ? ' disabled aria-disabled="true"' : '') + '><label for="' + escapeHtml(id) + '">' + escapeHtml(row[1]) + '</label>';
    });
    return html + '</div></fieldset>';
  }

  function ptMajorHighlightedText(value, query) {
    var text = String(value || '');
    var needle = String(query || '').trim();
    if (!needle) return escapeHtml(text);
    var offset = text.toLowerCase().indexOf(needle.toLowerCase());
    if (offset < 0) return escapeHtml(text);
    return escapeHtml(text.slice(0, offset)) + '<mark>' + escapeHtml(text.slice(offset, offset + needle.length)) + '</mark>' + escapeHtml(text.slice(offset + needle.length));
  }

  function ptMajorOwn(obj, key) {
    return !!obj && Object.prototype.hasOwnProperty.call(obj, key);
  }

  function ptMajorFiniteOrNull(value, label) {
    if (value === null || value === undefined) return;
    if (typeof value !== 'number' || !isFinite(value)) {
      throw new Error(label + ' must be a finite number or null.');
    }
  }

  function ptMajorRejectNonFinite(value, path) {
    if (value === null || value === undefined) return;
    if (typeof value === 'number') {
      if (!isFinite(value)) throw new Error(path + ' contains a non-finite number.');
      return;
    }
    if (Array.isArray(value)) {
      value.forEach(function(item, index) { ptMajorRejectNonFinite(item, path + '[' + index + ']'); });
      return;
    }
    if (typeof value === 'object') {
      Object.keys(value).forEach(function(key) { ptMajorRejectNonFinite(value[key], path + '.' + key); });
    }
  }

  function ptMajorRejectSpatialPayload(value, path) {
    if (!value || typeof value !== 'object') return;
    Object.keys(value).forEach(function(key) {
      var lower = key.toLowerCase();
      if (lower === 'geometry' || lower === 'geometries' || lower === 'coordinates') {
        throw new Error(path + ' includes forbidden spatial field ' + key + '.');
      }
      ptMajorRejectSpatialPayload(value[key], path + '.' + key);
    });
  }

  function ptMajorValidateMetricState(record, key) {
    var states = record && record.metric_state;
    if (!states || typeof states !== 'object' || Array.isArray(states)) {
      throw new Error(record.forecast_key + ' is missing metric_state.');
    }
    Object.keys(states).forEach(function(metric) {
      var state = states[metric];
      if (!state || typeof state !== 'object' || typeof state.status !== 'string') {
        throw new Error(record.forecast_key + ' has invalid metric_state.' + metric + '.');
      }
      if (typeof state.map_eligible !== 'boolean' || typeof state.popup_eligible !== 'boolean') {
        throw new Error(record.forecast_key + ' metric eligibility must be boolean.');
      }
      if (ptMajorOwn(record, metric)) ptMajorFiniteOrNull(record[metric], record.forecast_key + '.' + metric);
    });
    if (key && !ptMajorOwn(states, key)) {
      throw new Error(record.forecast_key + ' is missing metric state for ' + key + '.');
    }
  }

  function ptMajorRequireMetricStates(record, metrics) {
    ptMajorValidateMetricState(record);
    metrics.forEach(function(metric) {
      if (!ptMajorOwn(record.metric_state, metric)) {
        throw new Error(record.forecast_key + ' is missing metric state for ' + metric + '.');
      }
    });
  }

  function ptMajorValidateOptionalProduct7Median(record) {
    var hasValue = ptMajorOwn(record, 'percent_median');
    var hasState = ptMajorOwn(record.metric_state, 'percent_median');
    if (hasValue !== hasState) {
      throw new Error(record.forecast_key + ' percent_median value/state pair is incomplete.');
    }
    if (!hasValue) return false;
    if (record.percent_median !== null && (typeof record.percent_median !== 'number' || !isFinite(record.percent_median))) {
      throw new Error(record.forecast_key + '.percent_median must be a finite direct percentage or null.');
    }
    var state = record.metric_state.percent_median;
    ['status','value_origin','source_issue_at','valid_through','stale_since','missing_reason','map_eligible','popup_eligible']
      .forEach(function(key) {
        if (!ptMajorOwn(state, key)) throw new Error(record.forecast_key + ' metric_state.percent_median is missing ' + key + '.');
      });
    if (typeof state.status !== 'string' || typeof state.value_origin !== 'string') {
      throw new Error(record.forecast_key + ' percent_median status/value_origin must be strings.');
    }
    ['source_issue_at','valid_through','stale_since','missing_reason'].forEach(function(key) {
      if (state[key] !== null && typeof state[key] !== 'string') {
        throw new Error(record.forecast_key + ' percent_median ' + key + ' must be a string or null.');
      }
    });
    if (record.percent_median === null && (state.map_eligible || state.popup_eligible)) {
      throw new Error(record.forecast_key + ' null percent_median cannot be map/popup eligible.');
    }
    if (record.percent_median === null && !state.missing_reason) {
      throw new Error(record.forecast_key + ' null percent_median requires a missing_reason.');
    }
    return true;
  }

  function ptMajorValidateEnvelope(payload, expected) {
    if (!payload || typeof payload !== 'object' || Array.isArray(payload)) throw new Error('Payload is not an object.');
    if (payload.schema_version !== '1.0') throw new Error('Unexpected schema_version.');
    if (payload.product_id !== expected.product) throw new Error('Unexpected product_id.');
    if (payload.roster_version !== expected.roster) throw new Error('Unexpected roster_version.');
    if (['bootstrap', 'steady_state'].indexOf(payload.publication_mode) < 0) throw new Error('Unexpected publication_mode.');
    if (payload.expected_record_count !== expected.keys.length || payload.actual_record_count !== expected.keys.length) {
      throw new Error('Producer record count fields do not match the frozen roster.');
    }
    if (!Array.isArray(payload.records) || payload.records.length !== expected.keys.length) {
      throw new Error('Records array length does not match the frozen roster.');
    }
    var keys = payload.records.map(function(record) { return record && record.forecast_key; });
    if (new Set(keys).size !== keys.length) throw new Error('Duplicate forecast_key in payload.');
    keys.forEach(function(key, index) {
      if (key !== expected.keys[index]) throw new Error('Forecast roster order/key mismatch at record ' + (index + 1) + '.');
    });
    ptMajorRejectNonFinite(payload, 'payload');
    ptMajorRejectSpatialPayload(payload, 'payload');
  }

  function ptMajorValidateCnrfc(payload, mapping) {
    mapping = mapping || MAJOR_WATER_SUPPLY_PRODUCT_MAPPING;
    var rows = mapping.filter(function(row) { return row.source_family === 'CNRFC'; });
    var keys = rows.map(function(row) { return row.forecast_key; });
    ptMajorValidateEnvelope(payload, {
      product: 'major_water_supply_basin_forecasts',
      roster: 'cnrfc-major-water-supply-v1.1.0',
      keys: keys
    });
    var product7MedianCount = 0;
    var product7Count = 0;
    payload.records.forEach(function(record, index) {
      if (record.product_type !== rows[index].product_type) {
        throw new Error(record.forecast_key + ' product family/type mismatch.');
      }
      if (record.product_type === 'water_year_fnf' || record.product_type === 'water_year_index') {
        if (record.forecast_statistic !== 'median') {
          throw new Error(record.forecast_key + ' must retain forecast_statistic=median.');
        }
        ptMajorRequireMetricStates(record, ['forecast_volume', 'percent_mean', 'percent_median']);
      } else if (record.product_type === 'ten_day_streamflow_volume_accumulation') {
        ptMajorRequireMetricStates(record, [
          'day_3_median_volume', 'day_5_median_volume', 'day_10_median_volume',
          'day_3_deterministic_volume', 'day_5_deterministic_volume'
        ]);
      } else if (record.product_type === 'april_july_streamflow_volume_forecast') {
        if (record.forecast_statistic !== '50_percent_exceedance') {
          throw new Error(record.forecast_key + ' must retain forecast_statistic=50_percent_exceedance.');
        }
        ptMajorRequireMetricStates(record, ['forecast_volume', 'normal_average_volume', 'percent_average']);
        product7Count += 1;
        if (ptMajorValidateOptionalProduct7Median(record)) product7MedianCount += 1;
      } else {
        throw new Error(record.forecast_key + ' has an unsupported CNRFC product family.');
      }
    });
    if (product7Count !== 18 || (product7MedianCount !== 0 && product7MedianCount !== product7Count)) {
      throw new Error('CNRFC Product 7 percent_median must be absent from all 18 records or supplied as a complete value/state pair for all 18.');
    }
    return payload;
  }

  function ptMajorHasProduct7MedianContract(payload) {
    var records = (payload && payload.records || []).filter(function(record) {
      return record.product_type === 'april_july_streamflow_volume_forecast';
    });
    return records.length === 18 && records.every(function(record) {
      return ptMajorOwn(record, 'percent_median') && ptMajorOwn(record.metric_state, 'percent_median');
    });
  }

  function ptMajorHasDirectMetric(record, metric) {
    var state = record && record.metric_state && record.metric_state[metric];
    return !!(
      record && ptMajorOwn(record, metric) && typeof record[metric] === 'number' && isFinite(record[metric]) &&
      state && typeof state.value_origin === 'string' && state.value_origin !== 'none'
    );
  }

  function ptMajorViewCapabilities(cnrfcPayload, cbrfcPayload) {
    var cnrfcContract = ptMajorHasProduct7MedianContract(cnrfcPayload);
    var cnrfcIndex = ptMajorRecordIndex(cnrfcPayload);
    var cbrfcIndex = ptMajorRecordIndex(cbrfcPayload);
    return {
      cnrfcAprilJulyPercentMedian: cnrfcContract,
      comparisonAprilJulyPercentMedian: cnrfcContract &&
        ptMajorHasDirectMetric(cnrfcIndex['CNRFC:MLIC0:APR_JUL_VOLUME'], 'percent_median') &&
        ptMajorHasDirectMetric(cbrfcIndex[PT_MAJOR_CBRFC_KEYS[0]], 'percent_median')
    };
  }

  function ptMajorMonthIndex(value) {
    var match = /^(\d{4})-(\d{2})$/.exec(String(value || ''));
    return match ? Number(match[1]) * 12 + Number(match[2]) - 1 : null;
  }

  function ptMajorValidateCbrfc(payload) {
    ptMajorValidateEnvelope(payload, {
      product: 'cbrfc_major_water_supply_forecasts',
      roster: 'cbrfc-colorado-river-v1.3.0',
      keys: PT_MAJOR_CBRFC_KEYS
    });
    var april = payload.records[0];
    var waterYear = payload.records[1];
    var local = payload.records[2];
    if (april.product_type !== 'april_july_water_supply_forecast' ||
        waterYear.product_type !== 'water_year_unregulated_inflow_forecast' ||
        local.product_type !== 'lake_mead_local_intervening_monthly_forecast') {
      throw new Error('CBRFC product-family structure mismatch.');
    }
    ptMajorRequireMetricStates(april, ['forecast_volume', 'percent_average', 'percent_median']);
    ptMajorRequireMetricStates(waterYear, ['forecast_volume', 'percent_average']);
    if (ptMajorOwn(waterYear.metric_state, 'percent_median') || ptMajorOwn(waterYear, 'percent_median')) {
      throw new Error('CBRFC Lake Powell water-year record must not publish percent_median.');
    }
    if (!Array.isArray(local.monthly_forecasts) || local.monthly_forecasts.length !== 12) {
      throw new Error('LKSA3 must remain one record with exactly 12 monthly outlooks.');
    }
    if ((ptMajorOwn(local, 'forecast_volume') && local.forecast_volume !== null) ||
        (ptMajorOwn(local, 'percent_median') && local.percent_median !== null)) {
      throw new Error('LKSA3 must not contain a record-level aggregate.');
    }
    var seenMonths = {};
    var previousMonth = null;
    local.monthly_forecasts.forEach(function(month, index) {
      var monthIndex = ptMajorMonthIndex(month.forecast_month);
      if (monthIndex === null || seenMonths[month.forecast_month]) throw new Error('LKSA3 month roster is invalid.');
      if (previousMonth !== null && monthIndex !== previousMonth + 1) throw new Error('LKSA3 months are not in consecutive order.');
      seenMonths[month.forecast_month] = true;
      previousMonth = monthIndex;
      ptMajorRequireMetricStates(month, ['forecast_volume', 'percent_median']);
      if (month.source_date_override_applied === true) {
        ['raw_forecast_month_label', 'source_date_override_id', 'source_date_override_reason', 'source_date_override_evidence_url']
          .forEach(function(key) {
            if (!month[key]) throw new Error('LKSA3 corrected date provenance is incomplete at month ' + (index + 1) + '.');
          });
      }
    });
    function rejectObservedMetric(value, path) {
      if (!value || typeof value !== 'object') return;
      Object.keys(value).forEach(function(key) {
        var lower = key.toLowerCase();
        if (lower.indexOf('observed_to_date') >= 0 && lower.indexOf('policy') < 0) {
          throw new Error(path + ' includes an unsupported observed-to-date value metric.');
        }
        rejectObservedMetric(value[key], path + '.' + key);
      });
    }
    rejectObservedMetric(payload, 'payload');
    return payload;
  }

  function ptMajorRecordIndex(payload) {
    var out = {};
    (payload && payload.records || []).forEach(function(record) { out[record.forecast_key] = record; });
    return out;
  }

  function ptMajorSourceSlot(options) {
    var slot = {
      id: options.id,
      url: options.url,
      accepted: null,
      acceptedIndex: {},
      acceptedAt: null,
      inFlight: null,
      state: 'idle',
      lastError: null,
      lastAttemptAt: null
    };
    slot.refresh = function() {
      if (slot.inFlight) return slot.inFlight;
      slot.state = 'fetching';
      slot.lastAttemptAt = new Date();
      if (options.onChange) options.onChange(slot);
      var fetchImpl = options.fetchImpl || window.fetch.bind(window);
      slot.inFlight = fetchImpl(slot.url, {cache: 'no-store'})
        .then(function(response) {
          if (!response || !response.ok) throw new Error('HTTP ' + (response ? response.status : 'failure'));
          return response.json();
        })
        .then(function(payload) {
          var accepted = options.validate(payload);
          slot.accepted = accepted;
          slot.acceptedIndex = ptMajorRecordIndex(accepted);
          slot.acceptedAt = new Date();
          slot.state = 'accepted';
          slot.lastError = null;
          if (options.onChange) options.onChange(slot);
          return accepted;
        })
        .catch(function(error) {
          slot.state = 'error';
          slot.lastError = error instanceof Error ? error : new Error(String(error));
          if (options.onChange) options.onChange(slot);
          throw slot.lastError;
        })
        .finally(function() {
          slot.inFlight = null;
          if (options.onChange) options.onChange(slot);
        });
      return slot.inFlight;
    };
    return slot;
  }

  function ptMajorMetricEligible(state) {
    return !!state && state.map_eligible === true;
  }

  function ptMajorFormatKaf(value, digits) {
    if (value === null || value === undefined || !isFinite(Number(value))) return 'Not available';
    var kaf = Number(value);
    if (Math.abs(kaf) >= 1000) {
      return (kaf / 1000).toLocaleString(undefined, {minimumFractionDigits: digits === undefined ? 2 : digits, maximumFractionDigits: digits === undefined ? 3 : digits}) + ' MAF';
    }
    return kaf.toLocaleString(undefined, {maximumFractionDigits: digits === undefined ? 1 : digits}) + ' kaf';
  }

  function ptMajorFormatPercent(value) {
    if (value === null || value === undefined || !isFinite(Number(value))) return 'Not available';
    return Number(value).toLocaleString(undefined, {maximumFractionDigits: 1}) + '%';
  }

  function ptMajorFormatMonth(value) {
    var match = /^(\d{4})-(\d{2})$/.exec(String(value || ''));
    if (!match) return String(value || '');
    var names = ['January','February','March','April','May','June','July','August','September','October','November','December'];
    return names[Number(match[2]) - 1] + ' ' + match[1];
  }

  function ptMajorAccumulationLabel(horizon, standalone) {
    var labels = {day_3: '3-day total', day_5: '5-day total', day_10: '10-day total'};
    var label = labels[horizon] || String(horizon || '').replace(/_/g, ' ');
    return standalone ? label + ' volume' : label;
  }

  function ptMajorFormatTimestamp(value) {
    var raw = String(value || '');
    if (!raw) return '';
    try {
      if (/^\d{4}-\d{2}-\d{2}$/.test(raw)) {
        return new Intl.DateTimeFormat('en-US', {
          timeZone: 'UTC', month: 'short', day: 'numeric', year: 'numeric'
        }).format(new Date(raw + 'T12:00:00Z'));
      }
      return new Intl.DateTimeFormat('en-US', {
        timeZone: 'America/Los_Angeles', month: 'short', day: 'numeric', year: 'numeric',
        hour: 'numeric', minute: '2-digit', timeZoneName: 'short'
      }).format(new Date(raw));
    } catch(e) {
      return raw;
    }
  }

  function ptMajorTimeHtml(label, value) {
    var raw = String(value || '');
    if (!raw) return '';
    return '<span class="pt-major-time">' + (label ? escapeHtml(label) + ' ' : '') + '<time datetime="' + escapeHtml(raw) + '" title="' + escapeHtml(raw) + '">' + escapeHtml(ptMajorFormatTimestamp(raw)) + '</time></span>';
  }

  function ptMajorPercentClass(value) {
    if (value === null || value === undefined || !isFinite(Number(value))) return null;
    var numeric = Number(value);
    for (var i = 0; i < PT_MAJOR_PERCENT_CLASSES.length; i++) {
      var cls = PT_MAJOR_PERCENT_CLASSES[i];
      if (numeric >= cls.min && numeric < cls.max) return cls;
    }
    return PT_MAJOR_PERCENT_CLASSES[PT_MAJOR_PERCENT_CLASSES.length - 1];
  }

  function ptMajorVolumeDomain(values, options) {
    options = options || {};
    var numeric = (values || []).map(Number).filter(function(value) { return isFinite(value) && value >= 0; });
    var minimumSample = Number(options.minimumSample || 1);
    if (numeric.length < minimumSample) return {mode: 'insufficient', count: numeric.length, minimumSample: minimumSample};
    var min = Math.min.apply(null, numeric);
    var max = Math.max.apply(null, numeric);
    if (min === max || options.singleMidpoint === true) return {mode: 'midpoint', count: numeric.length, min: min, max: max};
    return {mode: 'range', count: numeric.length, min: min, max: max};
  }

  function ptMajorVolumeColor(value, domain) {
    if (!domain || domain.mode === 'insufficient' || value === null || !isFinite(Number(value))) return '#d9d9d9';
    if (domain.mode === 'midpoint') return PT_MAJOR_VOLUME_COLORS[2];
    var low = Math.sqrt(Math.max(0, domain.min));
    var high = Math.sqrt(Math.max(0, domain.max));
    var transformed = Math.sqrt(Math.max(0, Number(value)));
    var ratio = high === low ? 0.5 : (transformed - low) / (high - low);
    var index = Math.max(0, Math.min(PT_MAJOR_VOLUME_COLORS.length - 1, Math.floor(ratio * PT_MAJOR_VOLUME_COLORS.length)));
    return PT_MAJOR_VOLUME_COLORS[index];
  }

  function ptMajorLegendUnit(maximum) {
    return Number(maximum) >= 1000 ? 'MAF' : 'kaf';
  }

  function ptMajorLegendVolume(value, unit) {
    var number = Number(value);
    if (!isFinite(number)) return 'Not available';
    if (unit === 'MAF') return (number / 1000).toLocaleString(undefined, {maximumFractionDigits: 3}) + ' MAF';
    return number.toLocaleString(undefined, {maximumFractionDigits: 1}) + ' kaf';
  }

  function ptMajorViewConfig(view, period, measure, horizon, capabilities) {
    capabilities = capabilities || {};
    var matrix = {
      ca_major: {
        label: 'California forecast basins',
        periods: {
          water_year: {label: 'Water year', family: 'water_year', measures: [['forecast_volume','Forecast volume'],['percent_mean','Percent of mean'],['percent_median','Percent of median']]},
          april_july: {label: 'April–July', family: 'april_july', measures: [['forecast_volume','Forecast volume'],['percent_average','Percent of mean']]},
          short_range: {label: 'Short range', family: 'ten_day_accumulation', measures: [['median','Median ensemble'],['deterministic','Deterministic']], horizons: [['day_3','3-day total'],['day_5','5-day total'],['day_10','10-day total']]}
        }
      },
      ca_indices: {
        label: 'California forecast indices',
        periods: {
          water_year: {label: 'Water year', family: 'water_year_index', measures: [['forecast_volume','Forecast volume'],['percent_mean','Percent of mean'],['percent_median','Percent of median']]},
          april_july: {label: 'April–July', family: 'april_july', measures: [['forecast_volume','Forecast volume'],['percent_average','Percent of mean']]}
        }
      },
      colorado: {
        label: 'Colorado River',
        periods: {
          glda_april_july: {label: 'Lake Powell — April–July', family: 'april_july', key: PT_MAJOR_CBRFC_KEYS[0], measures: [['forecast_volume','Forecast volume'],['percent_average','Percent of mean'],['percent_median','Percent of median']]},
          glda_water_year: {label: 'Lake Powell — water year', family: 'water_year', key: PT_MAJOR_CBRFC_KEYS[1], measures: [['forecast_volume','Forecast volume'],['percent_average','Percent of mean']]},
          lksa_current_month: {label: 'Lake Mead Local — current month', family: 'monthly_local_intervening', key: PT_MAJOR_CBRFC_KEYS[2], measures: [['selected_month_forecast_volume','Monthly forecast volume'],['selected_month_percent_median','Percent of median']]}
        }
      },
      comparison: {
        label: 'California–Colorado comparison',
        periods: {
          water_year: {label: 'Water year', family: 'water_year', measures: [['forecast_volume','Forecast volume'],['percent_average','Percent of mean']]},
          april_july: {label: 'April–July', family: 'april_july', measures: [['forecast_volume','Forecast volume'],['percent_average','Percent of mean']]}
        }
      }
    };
    if (capabilities.cnrfcAprilJulyPercentMedian) {
      matrix.ca_major.periods.april_july.measures.push(['percent_median','Percent of median']);
      matrix.ca_indices.periods.april_july.measures.push(['percent_median','Percent of median']);
    }
    if (capabilities.comparisonAprilJulyPercentMedian) {
      matrix.comparison.periods.april_july.measures.push(['percent_median','Percent of median']);
    }
    var selectedView = matrix[view] || matrix.ca_major;
    var periodKeys = Object.keys(selectedView.periods);
    var selectedPeriodKey = selectedView.periods[period] ? period :
      (view === 'comparison' && period === 'short_range' ? 'april_july' : periodKeys[0]);
    var selectedPeriod = selectedView.periods[selectedPeriodKey];
    var measureKeys = selectedPeriod.measures.map(function(row) { return row[0]; });
    var selectedMeasure = measureKeys.indexOf(measure) >= 0 ? measure : measureKeys[0];
    var horizons = selectedPeriod.horizons || [];
    if (selectedMeasure === 'deterministic') horizons = horizons.filter(function(row) { return row[0] !== 'day_10'; });
    var horizonKeys = horizons.map(function(row) { return row[0]; });
    var selectedHorizon = horizonKeys.indexOf(horizon) >= 0 ? horizon : (horizonKeys[0] || '');
    return {matrix: matrix, viewKey: view in matrix ? view : 'ca_major', view: selectedView, periodKey: selectedPeriodKey, period: selectedPeriod, measure: selectedMeasure, horizons: horizons, horizon: selectedHorizon};
  }

  function ptMajorSelectedMetric(config, row, record) {
    if (config.periodKey === 'short_range') return config.horizon + '_' + config.measure + '_volume';
    if (config.measure === 'selected_month_forecast_volume') return 'forecast_volume';
    if (config.measure === 'selected_month_percent_median') return 'percent_median';
    if (
      config.viewKey === 'comparison' && config.measure === 'percent_average' &&
      row && row.source_family === 'CNRFC' && record &&
      record.metric_state && record.metric_state.percent_mean
    ) return 'percent_mean';
    return config.measure;
  }

  function ptMajorIncludesView(row, view) {
    var authority = view === 'ca_major' ? 'california_major_basins' :
      view === 'ca_indices' ? 'california_indexes' :
      view === 'colorado' ? 'colorado_supply' : 'california_colorado_comparison';
    return String(row.view_applicability || '').split('|').indexOf(authority) >= 0;
  }

  function ptMajorSelectionRows(config, options) {
    options = options || {};
    if (config.viewKey === 'colorado') {
      return MAJOR_WATER_SUPPLY_PRODUCT_MAPPING.filter(function(row) { return row.forecast_key === config.period.key; });
    }
    if (config.viewKey === 'comparison') {
      var keys = config.periodKey === 'water_year' ?
        ['CNRFC:MLIC0:WY_INDEX', PT_MAJOR_CBRFC_KEYS[1]] :
        ['CNRFC:MLIC0:APR_JUL_VOLUME', PT_MAJOR_CBRFC_KEYS[0]];
      return keys.map(function(key) {
        return MAJOR_WATER_SUPPLY_PRODUCT_MAPPING.filter(function(row) { return row.forecast_key === key; })[0];
      }).filter(Boolean);
    }
    return MAJOR_WATER_SUPPLY_PRODUCT_MAPPING.filter(function(row) {
      return ptMajorIncludesView(row, config.viewKey) && row.product_family === config.period.family;
    }).filter(function(row) {
      return config.viewKey !== 'ca_indices' || row.geometry_id === (options.indexGeometryId || 'MLIC0_FNF');
    });
  }

  function ptMajorSelectedMonth(record, metric) {
    var eligible = (record && record.monthly_forecasts || []).filter(function(month) {
      return ptMajorMetricEligible(month.metric_state && month.metric_state[metric]);
    });
    return eligible.length === 1 ? eligible[0] : null;
  }

  function ptMajorMeasurement(row, config, cnrfcIndex, cbrfcIndex) {
    var source = row.source_family === 'CBRFC' ? cbrfcIndex : cnrfcIndex;
    var record = source[row.forecast_key];
    var holder = record;
    var metric = ptMajorSelectedMetric(config, row, record);
    if (record && row.product_family === 'monthly_local_intervening') holder = ptMajorSelectedMonth(record, metric);
    var state = holder && holder.metric_state ? holder.metric_state[metric] : null;
    var value = holder && ptMajorOwn(holder, metric) ? holder[metric] : null;
    return {row: row, record: record, holder: holder, metric: metric, state: state, value: value};
  }

  function ptMajorDisplayState(measurement, showEndedReference) {
    var directValue = !!(
      measurement && measurement.value !== null && measurement.value !== undefined &&
      isFinite(Number(measurement.value))
    );
    var current = !!(directValue && ptMajorMetricEligible(measurement.state));
    var reference = !!(
      !current && showEndedReference && directValue && measurement.state &&
      measurement.state.status === 'expired' && measurement.state.popup_eligible === true
    );
    return {eligible: current || reference, current: current, reference: reference};
  }

  function ptMajorHtmlLink(url, label) {
    url = String(url || '');
    if (!/^https?:\/\//i.test(url)) return '';
    return '<a href="' + escapeHtml(url) + '" target="_blank" rel="noopener noreferrer">' + escapeHtml(label) + '</a>';
  }

  function ptMajorStateText(state) {
    if (!state) return 'not published';
    var text = String(state.status || 'unknown').replace(/_/g, ' ');
    if (state.missing_reason) text += ' — ' + state.missing_reason;
    return text;
  }

  function ptMajorStatusLabel(status, expiredPeriod) {
    var key = String(status || 'unknown').toLowerCase();
    var labels = {
      current: 'Current', current_partial: 'Current partial', stale: 'Stale',
      expired: expiredPeriod ? 'Forecast period ended' : 'Expired', unavailable: 'Unavailable',
      not_yet_valid: 'Upcoming', unknown: 'Status unavailable'
    };
    return labels[key] || String(status || 'unknown').replace(/_/g, ' ');
  }

  function ptMajorStatusBadge(status, expiredPeriod) {
    var key = String(status || 'unknown').toLowerCase().replace(/[^a-z0-9_-]/g, '-');
    return '<span class="pt-major-status pt-major-status-' + key + '">' + escapeHtml(ptMajorStatusLabel(status, expiredPeriod)) + '</span>';
  }

  function ptMajorMetricLabel(metric, record) {
    var labels = {
      forecast_volume: 'Forecast volume', normal_average_volume: 'Mean reference volume',
      percent_mean: 'Percent of mean', percent_average: 'Percent of mean', percent_median: 'Percent of median',
      day_3_median_volume: '3-day total volume — median ensemble',
      day_5_median_volume: '5-day total volume — median ensemble',
      day_10_median_volume: '10-day total volume — median ensemble',
      day_3_deterministic_volume: '3-day total volume — deterministic',
      day_5_deterministic_volume: '5-day total volume — deterministic'
    };
    return labels[metric] || metric.replace(/_/g, ' ');
  }

  function ptMajorSourceMetricLabel(metric, record) {
    var labels = {
      normal_average_volume: 'Normal-average volume',
      percent_mean: 'Percent mean', percent_average: 'Percent average', percent_median: 'Percent median'
    };
    if (record && record.source_percentage_label && metric === 'percent_median') {
      return String(record.source_percentage_label);
    }
    if (record && record.product_type === 'april_july_streamflow_volume_forecast') {
      if (metric === 'forecast_volume') return 'Median Forecast';
      if (metric === 'percent_average') return 'Percent of Mean';
      if (metric === 'percent_median') return 'Percent of Median';
    }
    return labels[metric] || '';
  }

  function ptMajorIsPercent(metric) { return metric.indexOf('percent_') >= 0; }
  function ptMajorFormatMetric(metric, value) { return ptMajorIsPercent(metric) ? ptMajorFormatPercent(value) : ptMajorFormatKaf(value); }

  var PT_MAJOR_POPUP_METRICS = [
    'forecast_volume','normal_average_volume','percent_mean','percent_average','percent_median',
    'day_3_median_volume','day_5_median_volume','day_10_median_volume',
    'day_3_deterministic_volume','day_5_deterministic_volume'
  ];

  function ptMajorPopupMetricEntries(record) {
    if (!record || !record.metric_state) return [];
    return PT_MAJOR_POPUP_METRICS.filter(function(metric) {
      var state = record.metric_state[metric];
      return state && (state.popup_eligible === true || !!state.missing_reason);
    }).map(function(metric) { return {metric: metric, state: record.metric_state[metric]}; });
  }

  function ptMajorCommonStateValue(entries, key) {
    if (!entries.length) return '';
    var values = entries.map(function(entry) { return String(entry.state && entry.state[key] || ''); });
    return values.every(function(value) { return value === values[0]; }) ? values[0] : '';
  }

  function ptMajorProductTitle(record, row) {
    var family = row && row.product_family || '';
    var year = record && record.water_year ? ' ' + record.water_year : '';
    if (family === 'water_year' || family === 'water_year_index') return 'Water year' + year;
    if (family === 'april_july') return 'April–July' + year;
    if (family === 'ten_day_accumulation') return 'Short-range accumulated volume';
    if (family === 'monthly_local_intervening') return 'Lake Mead Local monthly outlook';
    return record && record.forecast_period ? record.forecast_period : 'Forecast';
  }

  function ptMajorForecastStatisticLabel(record, row) {
    if (!record || !row || row.source_family !== 'CNRFC') return '';
    if (record.forecast_statistic === 'median') return 'Median ensemble forecast';
    if (record.forecast_statistic === '50_percent_exceedance') return '50% exceedance forecast';
    return '';
  }

  function ptMajorSelectedClass(metric, options) {
    return options && options.selectedProduct && options.selectedMetric === metric ? ' is-selected' : '';
  }

  function ptMajorPrimaryMetricSpecs(record) {
    var states = record && record.metric_state || {};
    var meanMetric = states.percent_mean ? 'percent_mean' : (states.percent_average ? 'percent_average' : '');
    return [
      {position: 'forecast-volume', metric: 'forecast_volume', label: 'Forecast volume'},
      {position: 'percent-mean', metric: meanMetric, label: 'Percent of mean'},
      {position: 'percent-median', metric: states.percent_median ? 'percent_median' : '', label: 'Percent of median'}
    ];
  }

  function ptMajorUnavailableMetricLabel(state) {
    if (!state) return 'Not included in accepted feed';
    var reason = String(state.missing_reason || '').toLowerCase();
    if (/not[ _-]?published|does not publish/.test(reason)) return 'Not published';
    return 'Not available';
  }

  function ptMajorPrimaryMetricCellHtml(record, spec, options) {
    var metric = spec.metric;
    var state = metric && record.metric_state ? record.metric_state[metric] : null;
    var published = !!(
      metric && state && state.popup_eligible === true &&
      ptMajorOwn(record, metric) && record[metric] !== null && record[metric] !== undefined
    );
    var selected = published ? ptMajorSelectedClass(metric, options) : '';
    var value = published ? ptMajorFormatMetric(metric, record[metric]) : ptMajorUnavailableMetricLabel(state);
    var aria = selected ? 'Selected map metric: ' + spec.label + ', ' + value : spec.label + ': ' + value;
    var html = '<div class="pt-major-metric pt-major-metric-' + spec.position + selected + '" aria-label="' + escapeHtml(aria) + '">';
    html += '<strong>' + escapeHtml(value) + '</strong><span>' + escapeHtml(spec.label) + '</span>';
    if (selected) html += '<small>Map metric</small>';
    return html + '</div>';
  }

  function ptMajorSharedTimingHtml(record, entries) {
    var issue = ptMajorCommonStateValue(entries, 'source_issue_at') || record.forecast_issued_at || record.forecast_issue_date || '';
    var valid = ptMajorCommonStateValue(entries, 'valid_through');
    var pieces = [];
    if (issue) pieces.push(ptMajorTimeHtml('Issued', issue));
    if (valid) pieces.push(ptMajorTimeHtml('valid through', valid));
    return pieces.length ? '<div class="pt-major-product-timing">' + pieces.join(' · ') + '</div>' : '';
  }

  function ptMajorShortRangeHtml(record, row, options, entries) {
    var status = record.status || (entries[0] && entries[0].state.status) || 'unknown';
    var html = '<section class="pt-major-product-card' + (status === 'unavailable' ? ' is-unavailable' : '') + '" aria-label="Short-range forecast comparison">';
    html += '<div class="pt-major-product-heading"><h4>' + escapeHtml(ptMajorProductTitle(record, row)) + '</h4>' + ptMajorStatusBadge(status, false) + '</div>';
    html += '<table class="pt-major-short-range"><thead><tr><th scope="col">Accumulation period</th><th scope="col">Median ensemble</th><th scope="col">Deterministic</th></tr></thead><tbody>';
    ['day_3','day_5','day_10'].forEach(function(horizon) {
      html += '<tr><th scope="row">' + escapeHtml(ptMajorAccumulationLabel(horizon, true)) + '</th>';
      ['median','deterministic'].forEach(function(statistic) {
        var metric = horizon + '_' + statistic + '_volume';
        var state = record.metric_state && record.metric_state[metric];
        var selected = ptMajorSelectedClass(metric, options);
        var value = !state || state.status === 'unavailable' ? ptMajorUnavailableMetricLabel(state) : ptMajorFormatKaf(record[metric]);
        html += '<td class="pt-major-comparison-value' + selected + '"' + (selected ? ' aria-label="Selected map metric"' : '') + '><strong>' + escapeHtml(value) + '</strong>' + (selected ? '<small>Map metric</small>' : '') + '</td>';
      });
      html += '</tr>';
    });
    html += '</tbody></table>';
    if (status === 'unavailable') html += '<div class="pt-major-product-note">CNRFC does not publish this short-range product at this location.</div>';
    html += ptMajorSharedTimingHtml(record, entries);
    return html + '</section>';
  }

  function ptMajorMonthlyHtml(record, row, options) {
    var current = ptMajorSelectedMonth(record, 'forecast_volume') || ptMajorSelectedMonth(record, 'percent_median');
    var html = '<section class="pt-major-product-card" aria-label="Lake Mead Local monthly forecast">';
    html += '<div class="pt-major-product-heading"><h4>' + escapeHtml(ptMajorProductTitle(record, row)) + '</h4>' + ptMajorStatusBadge(record.status || 'unknown', false) + '</div>';
    if (current) {
      html += '<div class="pt-major-current-month' + (options && options.selectedProduct ? ' is-selected' : '') + '"><div><strong>' + escapeHtml(ptMajorFormatMonth(current.forecast_month)) + '</strong><span>' + escapeHtml(ptMajorFormatKaf(current.forecast_volume)) + ' · Percent of median: ' + escapeHtml(ptMajorFormatPercent(current.percent_median)) + '</span></div>' + ptMajorStatusBadge(current.status || 'current', false) + '</div>';
    } else {
      html += '<div class="pt-major-product-note">No single current map-eligible month is available.</div>';
    }
    html += '<details class="pt-major-monthly-disclosure"><summary>Full 12-month outlook</summary><table><thead><tr><th scope="col">Month</th><th scope="col">Volume</th><th scope="col">Percent of median</th><th scope="col">State</th></tr></thead><tbody>';
    (record.monthly_forecasts || []).forEach(function(month) {
      var label = month.forecast_month ? ptMajorFormatMonth(month.forecast_month) : (month.raw_forecast_month_label || '');
      html += '<tr class="pt-major-month-' + escapeHtml(String(month.status || 'unknown').replace(/[^a-z0-9_-]/gi, '-').toLowerCase()) + '"><th scope="row">' + escapeHtml(label) + '</th><td>' + escapeHtml(ptMajorFormatKaf(month.forecast_volume)) + '</td><td>' + escapeHtml(ptMajorFormatPercent(month.percent_median)) + '</td><td>' + ptMajorStatusBadge(month.status || 'unknown', false) + '</td></tr>';
    });
    html += '</tbody></table><div class="pt-major-product-note">Monthly outlooks are not summed; no total-Lake-Mead value is inferred.</div></details>';
    return html + '</section>';
  }

  function ptMajorStandardProductHtml(record, row, options, entries) {
    var status = record.status || (entries[0] && entries[0].state.status) || 'unknown';
    var expired = row && row.product_family === 'april_july' && status === 'expired';
    var html = '<section class="pt-major-product-card' + (expired ? ' is-expired' : '') + '" aria-label="' + escapeHtml(ptMajorProductTitle(record, row)) + ' forecast">';
    html += '<div class="pt-major-product-heading"><h4>' + escapeHtml(ptMajorProductTitle(record, row)) + '</h4>' + ptMajorStatusBadge(status, expired) + '</div>';
    var statisticLabel = ptMajorForecastStatisticLabel(record, row);
    if (statisticLabel) html += '<div class="pt-major-forecast-statistic">' + escapeHtml(statisticLabel) + '</div>';
    html += '<div class="pt-major-metrics">';
    ptMajorPrimaryMetricSpecs(record).forEach(function(spec) { html += ptMajorPrimaryMetricCellHtml(record, spec, options); });
    html += '</div>';
    var referenceState = record.metric_state && record.metric_state.normal_average_volume;
    if (referenceState && referenceState.popup_eligible === true && record.normal_average_volume !== null && record.normal_average_volume !== undefined) {
      html += '<div class="pt-major-reference-volume">Mean reference volume: <strong>' + escapeHtml(ptMajorFormatKaf(record.normal_average_volume)) + '</strong></div>';
    }
    html += ptMajorSharedTimingHtml(record, entries);
    return html + '</section>';
  }

  function ptMajorSourceLinksHtml(record) {
    var seen = {};
    var links = [
      ['source_url','Official forecast'],['retrieval_url','Structured data'],
      ['summary_url','Summary dashboard'],['archive_url','Forecast archive']
    ].map(function(row) {
      var url = String(record && record[row[0]] || '');
      if (!url || seen[url]) return '';
      seen[url] = true;
      return ptMajorHtmlLink(url, row[1]);
    }).filter(Boolean);
    return links.length ? '<div class="pt-major-source-links">' + links.join(' · ') + '</div>' : '';
  }

  function ptMajorRecordProvenanceHtml(record, row, entries) {
    if (!record) return '<div class="pt-ops-muted">No accepted record.</div>';
    var issue = ptMajorCommonStateValue(entries, 'source_issue_at') || record.forecast_issued_at || record.forecast_issue_date || '';
    var updated = ptMajorCommonStateValue(entries, 'source_data_updated_at') || record.source_data_updated_at || '';
    var valid = ptMajorCommonStateValue(entries, 'valid_through') || record.valid_through || '';
    var origin = ptMajorCommonStateValue(entries, 'value_origin') || record.value_origin || '';
    var html = '<section class="pt-major-record-provenance"><h5>' + escapeHtml(ptMajorProductTitle(record, row)) + '</h5><dl>';
    html += '<dt>Product identifier</dt><dd>' + escapeHtml(record.forecast_key || '') + '</dd>';
    html += '<dt>Status</dt><dd>' + escapeHtml(ptMajorStatusLabel(record.status || 'unknown', row && row.product_family === 'april_july')) + '</dd>';
    if (issue) html += '<dt>Issued</dt><dd>' + ptMajorTimeHtml('', issue) + '</dd>';
    if (updated) html += '<dt>Source updated</dt><dd>' + ptMajorTimeHtml('', updated) + '</dd>';
    if (valid) html += '<dt>Valid through</dt><dd>' + ptMajorTimeHtml('', valid) + '</dd>';
    if (origin) html += '<dt>Value origin</dt><dd>' + escapeHtml(String(origin).replace(/_/g, ' ')) + '</dd>';
    if (record.missing_reason) html += '<dt>Availability note</dt><dd>' + escapeHtml(record.missing_reason) + '</dd>';
    var forecastStatistic = record.forecast_statistic === '50_percent_exceedance' ? '50% Exceedance' : ptMajorForecastStatisticLabel(record, row);
    if (forecastStatistic) html += '<dt>Forecast statistic</dt><dd>' + escapeHtml(forecastStatistic) + '</dd>';
    var sourceTerms = entries.map(function(entry) { return ptMajorSourceMetricLabel(entry.metric, record); }).filter(Boolean).filter(function(label, index, labels) { return labels.indexOf(label) === index; });
    if (sourceTerms.length) html += '<dt>Source terminology</dt><dd>' + sourceTerms.map(escapeHtml).join('; ') + '</dd>';
    html += '</dl>';
    var validity = entries.map(function(entry) { return String(entry.state.valid_through || ''); });
    if (validity.filter(Boolean).length && !validity.every(function(value) { return value === validity[0]; })) {
      html += '<table class="pt-major-metric-timing"><caption>Metric timing differences</caption><thead><tr><th scope="col">Metric</th><th scope="col">State</th><th scope="col">Valid through</th></tr></thead><tbody>';
      entries.forEach(function(entry) {
        html += '<tr><th scope="row">' + escapeHtml(ptMajorSourceMetricLabel(entry.metric, record) || ptMajorMetricLabel(entry.metric, record)) + '</th><td>' + escapeHtml(ptMajorStatusLabel(entry.state.status, false)) + '</td><td>' + (entry.state.valid_through ? ptMajorTimeHtml('', entry.state.valid_through) : 'Not supplied') + '</td></tr>';
      });
      html += '</tbody></table>';
    }
    if (Array.isArray(record.monthly_forecasts)) {
      record.monthly_forecasts.filter(function(month) { return month.source_date_override_applied; }).forEach(function(month) {
        html += '<div class="pt-major-correction"><b>Correction provenance</b><div>Displayed ' + escapeHtml(ptMajorFormatMonth(month.forecast_month)) + '; official raw label retained as “' + escapeHtml(month.raw_forecast_month_label || '') + '.”</div><div>' + escapeHtml(month.source_date_override_id || '') + ': ' + escapeHtml(month.source_date_override_reason || '') + ' ' + ptMajorHtmlLink(month.source_date_override_evidence_url, 'Correction evidence') + '</div></div>';
      });
    }
    html += ptMajorSourceLinksHtml(record);
    return html + '</section>';
  }

  function ptMajorRecordForecastParts(record, row, options) {
    if (!record) return {summary: '<section class="pt-major-product-card"><div class="pt-major-product-note">No accepted record.</div></section>', provenance: ''};
    var entries = ptMajorPopupMetricEntries(record);
    var family = row && row.product_family || '';
    var summary = Array.isArray(record.monthly_forecasts) ? ptMajorMonthlyHtml(record, row, options) :
      (family === 'ten_day_accumulation' ? ptMajorShortRangeHtml(record, row, options, entries) : ptMajorStandardProductHtml(record, row, options, entries));
    return {summary: summary, provenance: ptMajorRecordProvenanceHtml(record, row, entries)};
  }

  function ptMajorCompositionHtml(id) {
    var rows = MAJOR_WATER_SUPPLY_COMPONENT_MANIFEST.filter(function(row) { return row.derived_geometry_id === id; })
      .sort(function(a, b) { return Number(a.component_order) - Number(b.component_order); });
    if (!rows.length) return '';
    var target = MAJOR_WATER_SUPPLY_GEOMETRY_CATALOG.filter(function(row) { return row.geometry_id === id; })[0] || {};
    function sourceId(row) {
      return String(row.source_id_value || row.component_geometry_id || '').replace(/_FNF$/, '').replace(/_MODEL$/, '');
    }
    function componentName(row) {
      var catalog = MAJOR_WATER_SUPPLY_GEOMETRY_CATALOG.filter(function(item) { return item.geometry_id === row.component_geometry_id; })[0];
      return catalog && catalog.display_name ? catalog.display_name : sourceId(row);
    }
    var targetId = target.nws_lid || String(id).replace(/_FNF$/, '');
    var formula = targetId + ' = ' + rows.map(sourceId).join(' + ');
    var names = rows.map(componentName);
    var html = '<section class="pt-major-composition"><h4>Basin / index composition</h4><div class="pt-major-composition-kind">BRIM geometry assembly</div><code>' + escapeHtml(formula) + '</code><div>' + escapeHtml(names.join('; ')) + '</div><div class="pt-major-composition-authority">Reviewed component manifest ' + escapeHtml(rows[0].component_manifest_id || '') + (target.source_url ? ' · ' + ptMajorHtmlLink(target.source_url, 'CNRFC source page') : '') + '</div><small>This formula describes the retained map footprint only. Forecast values and percentages always come directly from accepted producer records; no official forecast arithmetic is inferred.</small></section>';
    return html;
  }

  function ptMajorInstallStyle() {
    if (document.getElementById('pt-major-basin-style')) return;
    var style = document.createElement('style');
    style.id = 'pt-major-basin-style';
    style.textContent =
      '.pt-major-basin-card{box-sizing:border-box;width:370px;max-width:calc(100vw - 36px);max-height:68vh;overflow:auto;background:rgba(255,255,255,.98);border:1px solid rgba(38,55,67,.42);border-radius:8px;padding:9px;font:12px/1.3 Arial,Helvetica,sans-serif;color:#26343e;box-shadow:0 3px 12px rgba(0,0,0,.24)}' +
      '.pt-major-basin-title{position:sticky;top:-9px;z-index:3;display:flex;align-items:flex-start;gap:6px;font-weight:700;margin:-9px -9px 5px;padding:9px 9px 6px;background:rgba(255,255,255,.99);border-bottom:1px solid #d8dee2;border-radius:8px 8px 0 0}.pt-major-basin-title-text{flex:1;min-width:0}.pt-major-basin-context{display:flex;align-items:center;justify-content:space-between;gap:8px;margin-bottom:6px}.pt-major-basin-subtitle{font-size:11px;color:#4f5e68;min-width:0}.pt-major-basin-health{display:flex;gap:4px;flex-wrap:wrap;justify-content:flex-end}.pt-major-health-badge{display:inline-flex;align-items:center;gap:2px;margin:0;padding:1px 6px;border:1px solid #aeb8bf;border-radius:10px;background:#f4f6f7;color:inherit;font:inherit;font-size:10px;line-height:1.3;white-space:nowrap;cursor:pointer}.pt-major-health-badge:focus-visible{outline:2px solid #1f78a8;outline-offset:2px}.pt-major-health-current{border-color:#6f9b7c;background:#edf6ef;color:#1f5c31}.pt-major-health-retained,.pt-major-health-unavailable{border-color:#b79a64;background:#faf4e8;color:#74541e}' +
      '.pt-major-choice{min-width:0;border:0;padding:0;margin:0 0 6px}.pt-major-choice legend{margin:0 0 3px;padding:0;font-size:10px;font-weight:700;letter-spacing:.03em;text-transform:uppercase;color:#5a6872}.pt-major-segments{display:flex;flex-wrap:wrap;gap:3px}.pt-major-choice input{position:absolute;opacity:0;pointer-events:none}.pt-major-choice label{box-sizing:border-box;flex:1 1 auto;min-width:0;padding:4px 6px;border:1px solid #aeb8bf;border-radius:4px;background:#f7f8f8;color:#31424e;text-align:center;cursor:pointer;line-height:1.15}.pt-major-choice input:checked+label{border-color:#2e6f94;background:#e8f3f9;color:#174f70;box-shadow:inset 0 0 0 1px #2e6f94;font-weight:700}.pt-major-choice input:focus-visible+label{outline:2px solid #1f78a8;outline-offset:1px}.pt-major-choice input:disabled+label{opacity:.45;cursor:not-allowed}.pt-major-choice-view label{flex-basis:46%}.pt-major-choice-period label{flex-basis:28%}.pt-major-basin-selection{border-top:1px solid #d8dee2;padding-top:4px}.pt-major-basin-selection>label{display:block;margin-top:5px}.pt-major-basin-picker{margin:1px 0 5px}.pt-major-basin-picker>summary,.pt-major-basin-status>summary{cursor:pointer;font-weight:700;outline-offset:2px}.pt-major-basin-picker-tools{padding:6px;margin-top:4px;background:#f3f6f7;border-radius:5px}.pt-major-basin-picker-tools>label:first-child{display:block;font-size:10px;font-weight:700}.pt-major-basin-search{box-sizing:border-box;width:100%;margin:2px 0 4px;padding:4px 6px}.pt-major-basin-picker-tools>div{display:flex;gap:4px;margin-bottom:4px}.pt-major-basin-picker-tools button,.pt-major-basin-locate{font-size:10px;padding:2px 7px}.pt-major-basin-list{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:3px 6px;margin-top:5px}.pt-major-basin-option{display:flex;align-items:flex-start;gap:2px;min-width:0}.pt-major-basin-option>label{display:flex;align-items:flex-start;gap:3px;min-width:0;flex:1}.pt-major-basin-option span{min-width:0}.pt-major-basin-option b,.pt-major-basin-option small{display:block;overflow-wrap:anywhere}.pt-major-basin-option b{font-size:10px}.pt-major-basin-option small{color:#65727a;font-size:9px}.pt-major-basin-option mark{background:#ffe89c}.pt-major-basin-locate{padding:0 3px;line-height:17px}.pt-major-basin-empty{grid-column:1/-1;color:#6a7379}' +
      '.pt-major-basin-legend{margin-top:6px}.pt-major-legend-block{border:1px solid #b9c6ce;border-radius:6px;background:#f7fafb;padding:7px}.pt-major-legend-block.is-reference{border-color:#a9926b;background:#faf7f0}.pt-major-legend-kicker{font-size:9px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:#64737c}.pt-major-legend-title{font-size:13px;font-weight:700;margin-top:1px}.pt-major-legend-meta{color:#4b5e6b}.pt-major-legend-status{font-size:10px;color:#5d6b73;margin-top:1px}.pt-major-legend-unit{font-size:10px;font-weight:700;margin-top:4px}.pt-major-legend-scale{display:grid;gap:3px;margin-top:5px}.pt-major-legend-volume{grid-template-columns:repeat(5,minmax(0,1fr))}.pt-major-legend-percent{grid-template-columns:repeat(4,minmax(0,1fr))}.pt-major-legend-scale>div{min-width:0;font-size:9px;text-align:center}.pt-major-legend-scale .pt-major-basin-swatch{display:block;width:100%;height:10px;margin:0 0 2px}.pt-major-legend-note,.pt-major-legend-empty{margin-top:5px;font-size:10px;color:#5d6b73}.pt-major-legend-empty{padding:5px;border-left:3px solid #a58a58;background:#fff}.pt-major-basin-notice{margin-top:5px;color:#6b4d10}.pt-major-basin-official{margin-top:6px;font-weight:700}.pt-major-official-label{display:block;font-size:9px;letter-spacing:.06em;text-transform:uppercase;color:#64737c}.pt-major-basin-actions{display:flex;justify-content:flex-end;margin-top:6px}.pt-major-basin-refresh{font-size:11px;padding:3px 9px}.pt-major-basin-status{margin-top:6px;padding-top:5px;border-top:1px solid #d8dee2}.pt-major-basin-status-body{padding:5px 2px 1px;color:#4d5b64}.pt-major-source-evidence{margin:5px 0;padding:6px;border:1px solid #c8d0d5;border-radius:5px;background:#f7f9fa;outline-offset:2px}.pt-major-source-evidence:focus{outline:2px solid #1f78a8}.pt-major-source-evidence h4{margin:0 0 4px}.pt-major-source-evidence dl{display:grid;grid-template-columns:92px 1fr;gap:2px 6px;margin:0}.pt-major-source-evidence dt{font-weight:700}.pt-major-source-evidence dd{margin:0;min-width:0;overflow-wrap:anywhere}.pt-major-family-health{margin-top:4px}.pt-major-retained-notice{margin-top:4px;padding:4px;border-left:3px solid #b08a45;background:#fff7e8;color:#704d12}.pt-major-source-links{margin-top:4px}.pt-major-status-health{padding:4px 5px;background:#f3f5f6;border-radius:4px}.pt-major-status-health .ok{color:#176b2c}.pt-major-status-health .warn{color:#8a4b08}.pt-major-attribution{margin-top:5px}.pt-major-attribution small{display:block;margin-top:3px;color:#67747c}' +
      '.pt-major-basin-popup{box-sizing:border-box;width:500px;max-width:calc(100vw - 82px);max-height:66vh;overflow-x:hidden;overflow-y:auto;padding:0 5px 2px 0;font:12px/1.35 Arial,Helvetica,sans-serif}.pt-major-basin-popup h4{margin:0;font-size:13px}.pt-major-basin-popup h5{margin:8px 0 3px;font-size:12px}.pt-major-basin-popup-header{display:flex;align-items:flex-start;justify-content:space-between;gap:8px;margin:0 0 7px;padding:0 0 6px;border-bottom:1px solid #c9c9c9}.pt-major-basin-popup-name{font-size:15px;font-weight:700;line-height:1.2}.pt-major-basin-popup-secondary{font-size:11px;color:#555;margin-top:2px}.pt-major-basin-popup>h4{margin:7px 0 5px}.pt-major-product-card{margin:0 0 7px;padding:7px;border:1px solid #c9d1d6;border-radius:5px;background:#f8fafb}.pt-major-product-card.is-expired{background:#f6f4ef;color:#57534b}.pt-major-product-card.is-unavailable{background:#f6f6f6}.pt-major-product-heading{display:flex;align-items:flex-start;justify-content:space-between;gap:8px;margin-bottom:3px}.pt-major-forecast-statistic{margin:0 0 5px;font-size:10px;color:#5d6267}.pt-major-status{display:inline-block;white-space:nowrap;border:1px solid #aeb6bc;border-radius:9px;padding:0 6px;font-size:10px;line-height:16px;background:#fff;color:#343a40}.pt-major-status-current{border-color:#74a985;background:#edf7f0;color:#175a2a}.pt-major-status-current_partial{border-color:#7c9dbb;background:#eef5fb;color:#245271}.pt-major-status-expired{border-color:#b9ad97;background:#f5f0e7;color:#6a5430}.pt-major-status-unavailable{border-color:#aaa;background:#eee;color:#555}.pt-major-status-not_yet_valid{border-color:#aeb8c2;background:#f3f5f7;color:#4b5660}.pt-major-metrics{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:5px}.pt-major-metric{min-width:0;padding:5px;border:1px solid transparent;border-radius:4px;background:#fff}.pt-major-metric strong,.pt-major-metric span{display:block}.pt-major-metric strong{font-size:15px;line-height:1.15}.pt-major-metric span{font-size:10px;color:#555;margin-top:2px}.pt-major-metric small,.pt-major-comparison-value small{display:block;color:#174f73;font-weight:700;margin-top:2px}.pt-major-metric.is-selected,.pt-major-comparison-value.is-selected,.pt-major-current-month.is-selected{border-color:#347aa3;background:#eef7fc;box-shadow:inset 0 0 0 1px #347aa3}.pt-major-reference-volume{margin:5px 5px 0;font-size:10px;color:#5d6267}.pt-major-reference-volume strong{color:inherit}.pt-major-product-timing{margin-top:5px;font-size:10px;color:#5d6267}.pt-major-time:empty{display:none}.pt-major-product-note{margin-top:5px;font-size:11px;color:#5d6267}.pt-major-short-range,.pt-major-monthly-disclosure table,.pt-major-metric-timing{border-collapse:collapse;width:100%;table-layout:fixed}.pt-major-basin-popup thead{background:#fff}.pt-major-basin-popup td,.pt-major-basin-popup th{padding:3px 4px;border-bottom:1px solid #ddd;text-align:left;vertical-align:top}.pt-major-short-range th:first-child{width:42%}.pt-major-comparison-value strong{white-space:nowrap}.pt-major-current-month{display:flex;align-items:center;justify-content:space-between;gap:8px;padding:7px;border:1px solid #b9c7d0;border-radius:4px;background:#fff}.pt-major-current-month strong,.pt-major-current-month span{display:block}.pt-major-current-month strong{font-size:14px}.pt-major-current-month span{margin-top:2px}.pt-major-basin-popup details{margin-top:6px}.pt-major-basin-popup summary{cursor:pointer;font-weight:700;outline-offset:2px}.pt-major-monthly-disclosure[open] summary{margin-bottom:5px}.pt-major-month-current{background:#edf7f0}.pt-major-month-expired{color:#6b665d}.pt-major-month-not_yet_valid{color:#5e6670}.pt-major-link-list{display:flex;flex-wrap:wrap;gap:3px 10px;margin:3px 0 7px}.pt-major-link-list div{white-space:normal}.pt-major-provenance{margin-top:8px;padding-top:6px;border-top:1px solid #c9c9c9}.pt-major-provenance-body{padding:2px 3px 0}.pt-major-record-provenance{padding:2px 0 7px;border-bottom:1px solid #ddd}.pt-major-record-provenance dl,.pt-major-geometry-details dl{display:grid;grid-template-columns:105px 1fr;gap:2px 7px;margin:3px 0}.pt-major-record-provenance dt,.pt-major-geometry-details dt{font-weight:700}.pt-major-record-provenance dd,.pt-major-geometry-details dd{margin:0;min-width:0;overflow-wrap:anywhere}.pt-major-source-links{margin-top:4px}.pt-major-correction{margin-top:6px;padding:5px;border-left:3px solid #8f7a50;background:#f6f3ec}.pt-major-metric-timing caption{text-align:left;font-weight:700;margin:4px 0 2px}.pt-major-metric-timing th:first-child{width:38%}@media(max-width:700px){.pt-major-basin-popup{width:430px;max-width:calc(100vw - 70px)}.pt-major-metric strong{font-size:13px}.pt-major-short-range,.pt-major-monthly-disclosure table,.pt-major-metric-timing{font-size:11px}.pt-major-record-provenance dl,.pt-major-geometry-details dl{grid-template-columns:90px 1fr}}' +
      '.pt-major-basin-swatch{display:inline-block;width:18px;height:10px;margin-right:4px;border:1px solid rgba(0,0,0,.3);vertical-align:middle}.pt-major-composition{margin-top:8px;padding:7px;border:1px solid #c5d0d6;border-radius:5px;background:#f7fafb}.pt-major-composition-kind{font-size:10px;font-weight:700;text-transform:uppercase;color:#60717c}.pt-major-composition code{display:block;margin:3px 0;padding:4px;background:#fff;border:1px solid #d7dfe3;white-space:normal}.pt-major-composition-authority{margin-top:4px;font-size:10px}.pt-major-composition small{display:block;margin-top:4px;color:#5d6b73}@media(max-width:430px){.pt-major-basin-card{width:350px}.pt-major-basin-list{grid-template-columns:1fr}.pt-major-legend-percent{grid-template-columns:repeat(3,minmax(0,1fr))}}';
    document.head.appendChild(style);
  }

  var MajorWaterSupplyBasinForecastLayer = L.Layer.extend({
    initialize: function(options) {
      this.options = options || {};
      this._map = null;
      this._cardControl = null;
      this._card = null;
      this._featureLayers = {};
      this._displayGroup = L.layerGroup();
      this._contextGroup = L.layerGroup();
      this._state = {
        active: false,
        visible: false,
        docked: true,
        floatingPosition: null,
        view: 'ca_major',
        period: 'water_year',
        measure: 'forecast_volume',
        horizon: 'day_3',
        indexGeometryId: 'MLIC0_FNF',
        selectedBasins: PT_MAJOR_BASIN_OPTIONS.map(function(row) { return row.id; }),
        focusedGeometryId: PT_MAJOR_BASIN_OPTIONS.length ? PT_MAJOR_BASIN_OPTIONS[0].id : '',
        basinSearch: '',
        autoZoom: false,
        showContext: false,
        showEndedReference: false,
        cnrfcAccepted: null,
        cbrfcAccepted: null,
        cnrfcHealth: 'idle',
        cbrfcHealth: 'idle',
        thematicDomain: null,
        lastRefreshStatus: 'idle'
      };
      var self = this;
      this._cnrfc = ptMajorSourceSlot({id: 'CNRFC', url: this.options.cnrfcUrl, validate: function(p) { return ptMajorValidateCnrfc(p); }, onChange: function() { self._sourceChanged(); }});
      this._cbrfc = ptMajorSourceSlot({id: 'CBRFC', url: this.options.cbrfcUrl, validate: ptMajorValidateCbrfc, onChange: function() { self._sourceChanged(); }});
      this._geoJson = L.geoJSON(this.options.geometry, {
        style: function() { return {color:'#777', weight:1.2, fillColor:'#d9d9d9', fillOpacity:0.12}; },
        onEachFeature: function(feature, layer) {
          var id = feature && feature.properties && feature.properties.geometry_id;
          if (!id) return;
          self._featureLayers[id] = layer;
          if (feature.properties.geometry_role === 'context_only') {
            layer.bindPopup(function() { return self._contextPopup(id); }, {maxWidth: 380});
          } else {
            layer.bindTooltip(function() { return self._hoverHtml(id); }, {sticky: true, className: 'pt-major-basin-hover'});
            layer.bindPopup(function() { return self._popupHtml(id); }, {maxWidth: 560});
          }
        }
      });
    },

    onAdd: function(mapObj) {
      this._map = mapObj;
      if (window.BRIM && window.BRIM.majorWaterSupplyForecast) {
        window.BRIM.majorWaterSupplyForecast.activeController = this;
      }
      this._state.active = true;
      this._state.visible = true;
      this._displayGroup.addTo(mapObj);
      this._contextGroup.addTo(mapObj);
      this._ensureCard();
      this._showCardAuthoritatively();
      this._render();
      if (!this._cnrfc.accepted || !this._cbrfc.accepted) {
        this.refreshCurrentView();
      } else {
        setOpsLayerLoading(PT_MAJOR_BASIN_LAYER_NAME, false);
        this._recordCombinedStatus();
      }
    },

    onRemove: function(mapObj) {
      this._state.active = false;
      this._state.visible = false;
      try { mapObj.closeTooltip(); } catch(e) {}
      this._displayGroup.clearLayers();
      this._contextGroup.clearLayers();
      try { mapObj.removeLayer(this._displayGroup); } catch(e) {}
      try { mapObj.removeLayer(this._contextGroup); } catch(e) {}
      if (this._card) this._card.style.display = 'none';
      setOpsLayerLoading(PT_MAJOR_BASIN_LAYER_NAME, false);
    },

    forceRemove: function(mapObj) {
      this.onRemove(mapObj || this._map);
    },

    refreshCurrentView: function() {
      var self = this;
      this._state.lastRefreshStatus = 'refreshing';
      setOpsLayerLoading(PT_MAJOR_BASIN_LAYER_NAME, true);
      recordStatus(PT_MAJOR_BASIN_LAYER_NAME, 'Refreshing CNRFC and CBRFC independently…', 'pt-ops-warn');
      return Promise.allSettled([this._cnrfc.refresh(), this._cbrfc.refresh()]).then(function(results) {
        self._state.lastRefreshStatus = results.some(function(result) { return result.status === 'rejected'; }) ? 'partial_failure' : 'accepted';
        setOpsLayerLoading(PT_MAJOR_BASIN_LAYER_NAME, false);
        self._recordCombinedStatus();
        return results;
      });
    },

    _sourceChanged: function() {
      this._state.cnrfcAccepted = this._cnrfc.accepted;
      this._state.cbrfcAccepted = this._cbrfc.accepted;
      this._state.cnrfcHealth = this._cnrfc.state;
      this._state.cbrfcHealth = this._cbrfc.state;
      if (this._state.active) this._render();
      if (!this._cnrfc.inFlight && !this._cbrfc.inFlight) setOpsLayerLoading(PT_MAJOR_BASIN_LAYER_NAME, false);
    },

    _recordCombinedStatus: function() {
      var failures = [this._cnrfc, this._cbrfc].filter(function(slot) { return slot.lastError; });
      var accepted = [this._cnrfc, this._cbrfc].filter(function(slot) { return slot.accepted; }).map(function(slot) { return slot.id; });
      var message = accepted.length ? 'Accepted source snapshots: ' + accepted.join(', ') + '.' : 'No accepted source snapshot.';
      if (failures.length) message += ' Latest refresh failed: ' + failures.map(function(slot) { return slot.id; }).join(', ') + '; prior accepted data retained.';
      recordStatus(PT_MAJOR_BASIN_LAYER_NAME, message, failures.length ? 'pt-ops-warn' : 'pt-ops-good');
    },

    _ensureCard: function() {
      if (this._cardControl) return;
      ptMajorInstallStyle();
      var self = this;
      this._cardControl = L.control({position: 'bottomleft'});
      this._cardControl.onAdd = function() {
        var div = L.DomUtil.create('div', 'pt-major-basin-card pt-map-legend-card');
        div.style.visibility = 'hidden';
        div.innerHTML =
          '<div class="pt-major-basin-title pt-map-card-handle"><span class="pt-major-basin-title-text">Major Water-Supply Basin Forecasts</span>' +
          (window.BRIM && window.BRIM.legendCloseout ? window.BRIM.legendCloseout.actionsHtml('pt-major-basin-dock','pt-major-basin-close','Major Water-Supply Basin Forecasts card') : '<button type="button" class="pt-major-basin-close">×</button>') + '</div>' +
          '<div class="pt-major-basin-context"><div class="pt-major-basin-subtitle"></div><div class="pt-major-basin-health" aria-label="Forecast source health"></div></div>' +
          '<div class="pt-major-basin-controls"></div>' +
          '<div class="pt-major-basin-selection">' +
            '<details class="pt-major-basin-picker"><summary>Basins</summary><div class="pt-major-basin-picker-tools"><label>Find basin or river…<input type="search" class="pt-major-basin-search" placeholder="Find basin or river…"></label><div><button type="button" class="pt-major-basin-all">All</button><button type="button" class="pt-major-basin-none">None</button></div><label><input type="checkbox" class="pt-major-basin-auto"> Auto-zoom to selected basin</label></div><div class="pt-major-basin-list"></div></details>' +
            '<div class="pt-major-basin-index-choice"></div>' +
            '<label class="pt-major-basin-context-row"><input type="checkbox" class="pt-major-basin-context-toggle"> Show full Colorado River Basin context (HUC2 14 and 15)</label>' +
            '<label class="pt-major-basin-ended-row"><input type="checkbox" class="pt-major-basin-ended"> Show ended-period values for reference</label>' +
          '</div>' +
          '<div class="pt-major-basin-legend"></div><div class="pt-major-basin-notice" aria-live="polite"></div>' +
          '<div class="pt-major-basin-official"></div>' +
          '<div class="pt-major-basin-actions"><button type="button" class="pt-major-basin-refresh">Refresh</button></div>' +
          '<details class="pt-major-basin-status" id="pt-major-basin-source-status"><summary>Data status &amp; sources</summary><div class="pt-major-basin-status-body"></div></details>';
        L.DomEvent.disableClickPropagation(div);
        L.DomEvent.disableScrollPropagation(div);
        self._card = div;
        self._wireCard();
        return div;
      };
      this._cardControl.addTo(this._map);
      var shared = window.BRIM && window.BRIM.legendCloseout;
      if (shared && this._card) {
        shared.makeDetachable({card:this._card, map:this._map, handleSelector:'.pt-major-basin-title', dockSelector:'.pt-major-basin-dock', label:'Major Water-Supply Basin Forecasts card'});
      }
    },

    _showCardAuthoritatively: function() {
      if (!this._card || !this._state.visible) return;
      this._card.style.display = '';
      this._card.style.visibility = 'hidden';
      var shared = window.BRIM && window.BRIM.legendCloseout;
      if (shared && shared.scheduleLayout) shared.scheduleLayout(this._card);
      var card = this._card;
      var reveal = function() { if (card && card.style.display !== 'none') card.style.visibility = 'visible'; };
      if (window.requestAnimationFrame) window.requestAnimationFrame(reveal); else reveal();
    },

    _wireCard: function() {
      var self = this;
      var card = this._card;
      var shared = window.BRIM && window.BRIM.legendCloseout;
      if (shared) shared.wire(card, '.pt-major-basin-close', function() {
        self._state.visible = false;
        window.setTimeout(function() { self._syncSharedCardState(); }, 0);
      });
      card.addEventListener('change', function(e) {
        var key = e.target && e.target.getAttribute('data-state-key');
        if (key && ['view','period','measure','horizon','indexGeometryId'].indexOf(key) >= 0) {
          self._state[key] = e.target.value;
          if (key === 'indexGeometryId') self._state.focusedGeometryId = e.target.value;
          self._render();
          if (key === 'indexGeometryId') self._focusGeometry(e.target.value, self._state.autoZoom);
          return;
        }
        if (e.target.classList.contains('pt-major-basin-check')) {
          var id = e.target.value;
          var selected = self._state.selectedBasins.slice();
          var offset = selected.indexOf(id);
          if (e.target.checked && offset < 0) selected.push(id);
          if (!e.target.checked && offset >= 0) selected.splice(offset, 1);
          self._state.selectedBasins = selected;
          if (e.target.checked) self._state.focusedGeometryId = id;
          self._render();
          if (e.target.checked) self._focusGeometry(id, self._state.autoZoom);
          return;
        }
        if (e.target.classList.contains('pt-major-basin-auto')) self._state.autoZoom = !!e.target.checked;
        if (e.target.classList.contains('pt-major-basin-context-toggle')) { self._state.showContext = !!e.target.checked; self._render(); }
        if (e.target.classList.contains('pt-major-basin-ended')) { self._state.showEndedReference = !!e.target.checked; self._render(); }
      });
      card.addEventListener('input', function(e) {
        if (!e.target.classList.contains('pt-major-basin-search')) return;
        self._state.basinSearch = e.target.value;
        self._renderBasinSelector();
      });
      card.addEventListener('click', function(e) {
        var target = e.target;
        var healthBadge = target.closest && target.closest('.pt-major-health-badge');
        if (healthBadge && card.contains(healthBadge)) {
          e.preventDefault();
          var details = card.querySelector('.pt-major-basin-status');
          var section = card.querySelector('#' + healthBadge.getAttribute('aria-controls'));
          details.open = true;
          self._syncHealthExpanded(true);
          if (section) {
            section.focus({preventScroll: true});
            if (section.scrollIntoView) section.scrollIntoView({block: 'nearest'});
          }
          return;
        }
        if (target.classList.contains('pt-major-basin-refresh')) { e.preventDefault(); self.refreshCurrentView(); return; }
        if (target.classList.contains('pt-major-basin-all')) { e.preventDefault(); self._state.selectedBasins = PT_MAJOR_BASIN_OPTIONS.map(function(row) { return row.id; }); self._render(); return; }
        if (target.classList.contains('pt-major-basin-none')) { e.preventDefault(); self._state.selectedBasins = []; self._render(); return; }
        if (target.classList.contains('pt-major-basin-locate')) {
          e.preventDefault();
          var id = target.getAttribute('data-geometry-id');
          if (self._state.selectedBasins.indexOf(id) < 0) self._state.selectedBasins.push(id);
          self._state.focusedGeometryId = id;
          self._render();
          self._focusGeometry(id, self._state.autoZoom);
        }
      });
      card.querySelector('.pt-major-basin-dock').addEventListener('click', function() {
        window.setTimeout(function() { self._syncSharedCardState(); }, 0);
      });
      card.querySelector('.pt-major-basin-status').addEventListener('toggle', function(e) {
        self._syncHealthExpanded(e.target.open);
      });
    },

    _config: function() {
      var capabilities = ptMajorViewCapabilities(this._cnrfc.accepted, this._cbrfc.accepted);
      var config = ptMajorViewConfig(this._state.view, this._state.period, this._state.measure, this._state.horizon, capabilities);
      this._state.view = config.viewKey;
      this._state.period = config.periodKey;
      this._state.measure = config.measure;
      this._state.horizon = config.horizon;
      return config;
    },

    _renderControls: function(config) {
      if (!this._card) return;
      var viewDescription = config.viewKey === 'ca_major' ? 'Shasta, Trinity, and principally west-slope Sierra water-supply forecast basins.' : '';
      var shortRange = config.periodKey === 'short_range';
      var viewRows = Object.keys(config.matrix).map(function(key) { return [key, config.matrix[key].label]; });
      var periodRows = Object.keys(config.view.periods).map(function(key) {
        var label = config.viewKey === 'colorado' ? {
          glda_april_july: 'Powell Apr–July', glda_water_year: 'Powell water year', lksa_current_month: 'Mead local'
        }[key] : config.view.periods[key].label;
        return [key, label];
      });
      var html = ptMajorChoiceHtml('view', 'View', viewRows, config.viewKey, 'pt-major-choice-view');
      html += ptMajorChoiceHtml('period', config.viewKey === 'colorado' ? 'Colorado product' : 'Period / product', periodRows, config.periodKey, 'pt-major-choice-period');
      html += ptMajorChoiceHtml('measure', shortRange ? 'Forecast type' : 'Measure', config.period.measures, config.measure, 'pt-major-choice-measure');
      if (config.horizons.length) html += ptMajorChoiceHtml('horizon', 'Accumulation period', config.horizons, config.horizon, 'pt-major-choice-horizon');
      this._card.querySelector('.pt-major-basin-controls').innerHTML = html;
      this._card.querySelector('.pt-major-basin-controls').setAttribute('aria-label', 'Forecast controls. ' + config.view.label + (viewDescription ? '. ' + viewDescription : ''));
    },

    _renderBasinSelector: function() {
      if (!this._card) return;
      var selected = this._state.selectedBasins;
      var query = this._state.basinSearch.trim().toLowerCase();
      var matches = PT_MAJOR_BASIN_OPTIONS.filter(function(row) {
        return !query || [row.label,row.river,row.reservoir,row.lid].join(' ').toLowerCase().indexOf(query) >= 0;
      });
      this._card.querySelector('.pt-major-basin-picker summary').textContent = selected.length === PT_MAJOR_BASIN_OPTIONS.length ? 'Basins (' + selected.length + ')' : 'Basins (' + selected.length + ' of ' + PT_MAJOR_BASIN_OPTIONS.length + ')';
      var search = this._card.querySelector('.pt-major-basin-search');
      if (search.value !== this._state.basinSearch) search.value = this._state.basinSearch;
      this._card.querySelector('.pt-major-basin-auto').checked = this._state.autoZoom;
      this._card.querySelector('.pt-major-basin-list').innerHTML = matches.length ? matches.map(function(row) {
        var checked = selected.indexOf(row.id) >= 0;
        var identity = [row.reservoir, row.lid].filter(Boolean).join(' · ');
        return '<div class="pt-major-basin-option"><label><input type="checkbox" class="pt-major-basin-check" value="' + escapeHtml(row.id) + '"' + (checked ? ' checked' : '') + '><span><b>' + ptMajorHighlightedText(row.label, query) + '</b>' + (identity ? '<small>' + ptMajorHighlightedText(identity, query) + '</small>' : '') + '</span></label><button type="button" class="pt-major-basin-locate" data-geometry-id="' + escapeHtml(row.id) + '" aria-label="Locate ' + escapeHtml(row.label) + '">⌖</button></div>';
      }).join('') : '<div class="pt-major-basin-empty">No matching basin or river.</div>';
    },

    _renderSelectionTools: function(config) {
      var picker = this._card.querySelector('.pt-major-basin-picker');
      picker.style.display = config.viewKey === 'ca_major' ? '' : 'none';
      if (config.viewKey === 'ca_major') this._renderBasinSelector();
      var indexChoice = this._card.querySelector('.pt-major-basin-index-choice');
      indexChoice.style.display = config.viewKey === 'ca_indices' ? '' : 'none';
      if (config.viewKey === 'ca_indices') indexChoice.innerHTML = ptMajorChoiceHtml('indexGeometryId', 'Index', PT_MAJOR_INDEX_OPTIONS, this._state.indexGeometryId, 'pt-major-choice-index');
      var showContext = config.viewKey === 'colorado' || config.viewKey === 'comparison';
      var context = this._card.querySelector('.pt-major-basin-context-row');
      context.style.display = showContext ? '' : 'none';
      this._card.querySelector('.pt-major-basin-context-toggle').checked = this._state.showContext;
    },

    _focusGeometry: function(id, zoom) {
      var layer = this._featureLayers[id];
      if (!layer) return;
      if (layer.bringToFront) layer.bringToFront();
      if (zoom && this._map && layer.getBounds) this._map.fitBounds(layer.getBounds(), {padding:[32,32], maxZoom:8});
      if (layer.openTooltip) {
        try { layer.openTooltip(); } catch(e) {}
      }
    },

    _syncHealthExpanded: function(expanded) {
      if (!this._card) return;
      Array.prototype.forEach.call(this._card.querySelectorAll('.pt-major-health-badge'), function(button) {
        button.setAttribute('aria-expanded', expanded ? 'true' : 'false');
      });
    },

    _compactHealthHtml: function(slot, expanded) {
      var state = slot.state === 'fetching' ? 'refreshing' : (slot.accepted ? (slot.lastError ? 'retained' : 'current') : 'unavailable');
      var icon = state === 'current' ? '&#10003;' : (state === 'refreshing' ? '&#8635;' : '&#9888;');
      var tooltipState = state === 'current' ? 'accepted' : (state === 'retained' ? 'retained after refresh failure' : state);
      var description = slot.id + ' feed ' + tooltipState + ' — activate for details and source links';
      return '<button type="button" class="pt-major-health-badge pt-major-health-' + escapeHtml(state) + '" aria-expanded="' + (expanded ? 'true' : 'false') + '" aria-controls="pt-major-status-' + escapeHtml(slot.id) + '" aria-label="' + escapeHtml(description) + '" title="' + escapeHtml(description) + '"><span aria-hidden="true">' + icon + '</span> ' + escapeHtml(slot.id + ' ' + state) + '</button>';
    },

    _contextualOfficialUrl: function(sourceId, config, measurements) {
      var chosen = measurements.filter(function(m) { return m.row.source_family === sourceId; });
      if (config.viewKey === 'ca_major' && this._state.focusedGeometryId) {
        var focused = chosen.filter(function(m) { return m.row.geometry_id === this._state.focusedGeometryId; }, this);
        if (focused.length) chosen = focused;
      }
      var record = chosen.length && chosen[0].record;
      return record && (record.source_url || record.summary_url || '') || '';
    },

    _familyHealthHtml: function(slot) {
      var families = slot.accepted && slot.accepted.family_health;
      if (!families || typeof families !== 'object') return 'Not included in accepted payload';
      var entries = Object.keys(families).map(function(key) {
        var health = families[key] && families[key].health || 'unknown';
        return String(key).replace(/_/g, ' ') + ': ' + String(health).replace(/_/g, ' ');
      });
      return entries.length ? entries.join(' · ') : 'No family summaries';
    },

    _sourceEvidenceHtml: function(slot, config, measurements) {
      var accepted = slot.accepted;
      var state = slot.state === 'fetching' ? 'refreshing' : (accepted ? (slot.lastError ? 'retained' : 'current') : 'unavailable');
      var acceptedAt = slot.acceptedAt ? ptMajorFormatTimestamp(slot.acceptedAt.toISOString()) : 'No accepted snapshot';
      var generatedAt = accepted && accepted.generated_at ? ptMajorFormatTimestamp(accepted.generated_at) : 'Not included in accepted payload';
      var count = accepted ? String(accepted.actual_record_count) + '/' + String(accepted.expected_record_count) : 'No accepted snapshot';
      var refresh = slot.state === 'fetching' ? 'In progress' : (slot.lastError ? 'Failed' + (slot.lastAttemptAt ? ' ' + ptMajorFormatTimestamp(slot.lastAttemptAt.toISOString()) : '') + ': ' + slot.lastError.message : (accepted ? 'Accepted and validated' : 'Not yet accepted'));
      var officialHome = slot.id === 'CNRFC' ? 'https://www.cnrfc.noaa.gov/' : 'https://www.cbrfc.noaa.gov/';
      var contextualUrl = this._contextualOfficialUrl(slot.id, config, measurements);
      var html = '<section class="pt-major-source-evidence" id="pt-major-status-' + escapeHtml(slot.id) + '" tabindex="-1" aria-label="' + escapeHtml(slot.id + ' source status and links') + '"><h4>' + escapeHtml(slot.id) + ' source evidence</h4><dl>';
      html += '<dt>Health</dt><dd>' + escapeHtml(state) + '</dd>';
      html += '<dt>Accepted at</dt><dd>' + escapeHtml(acceptedAt) + '</dd>';
      html += '<dt>Payload generated</dt><dd>' + escapeHtml(generatedAt) + '</dd>';
      html += '<dt>Schema</dt><dd>' + escapeHtml(accepted && accepted.schema_version || 'No accepted snapshot') + '</dd>';
      html += '<dt>Roster</dt><dd>' + escapeHtml(accepted && accepted.roster_version || 'No accepted snapshot') + '</dd>';
      html += '<dt>Records</dt><dd>' + escapeHtml(count) + '</dd>';
      html += '<dt>Last refresh</dt><dd>' + escapeHtml(refresh) + '</dd></dl>';
      html += '<div class="pt-major-family-health"><b>Family health:</b> ' + escapeHtml(this._familyHealthHtml(slot)) + '</div>';
      if (slot.lastError && accepted) html += '<div class="pt-major-retained-notice">Latest refresh failed; BRIM is continuing with the last accepted snapshot.</div>';
      html += '<div class="pt-major-source-links">' + ptMajorHtmlLink(slot.url, 'BRIM Live canonical ' + slot.id + ' feed') + ' · ' + ptMajorHtmlLink(officialHome, 'Official NOAA/NWS ' + slot.id + ' source');
      if (contextualUrl) html += ' · ' + ptMajorHtmlLink(contextualUrl, 'Official ' + slot.id + ' forecast for selected product');
      return html + '</div></section>';
    },

    _statusHtml: function(config, measurements) {
      var last = String(this._state.lastRefreshStatus || 'idle').replace(/_/g, ' ');
      var html = this._sourceEvidenceHtml(this._cnrfc, config, measurements) + this._sourceEvidenceHtml(this._cbrfc, config, measurements);
      html += '<div><b>Last combined refresh:</b> ' + escapeHtml(last) + '</div>';
      html += '<div class="pt-major-attribution"><b>Forecast data:</b> ' + ptMajorHtmlLink('https://www.cnrfc.noaa.gov/', 'NOAA/NWS CNRFC') + ' and ' + ptMajorHtmlLink('https://www.cbrfc.noaa.gov/', 'CBRFC') + '<br><b>Basin geometry:</b> CNRFC forecast-watershed sources, CBRFC operational basin groups, and USGS WBD context; generalized for BRIM.<small>Source attribution does not imply agency endorsement of BRIM.</small></div>';
      return html;
    },

    _officialLinksHtml: function(config, measurements) {
      var self = this;
      var links = ['CNRFC','CBRFC'].map(function(sourceId) {
        var url = self._contextualOfficialUrl(sourceId, config, measurements);
        return url ? ptMajorHtmlLink(url, 'Official ' + sourceId + ' forecast') : '';
      }).filter(Boolean);
      return links.length ? '<span class="pt-major-official-label">Selected product</span>' + links.join(' · ') : '';
    },

    _measurements: function(config) {
      var self = this;
      return ptMajorSelectionRows(config, {indexGeometryId: this._state.indexGeometryId}).filter(function(row) {
        return config.viewKey !== 'ca_major' || self._state.selectedBasins.indexOf(row.geometry_id) >= 0;
      }).map(function(row) {
        return ptMajorMeasurement(row, config, self._cnrfc.acceptedIndex, self._cbrfc.acceptedIndex);
      });
    },

    _volumeDomain: function(config, measurements) {
      var showEndedReference = this._state.showEndedReference;
      var values = measurements.filter(function(m) {
        return ptMajorDisplayState(m, showEndedReference).eligible && Number(m.value) >= 0;
      }).map(function(m) { return Number(m.value); });
      if (config.viewKey === 'ca_major') return ptMajorVolumeDomain(values, {minimumSample: 5});
      if (config.viewKey === 'comparison') return ptMajorVolumeDomain(values, {minimumSample: 2});
      return ptMajorVolumeDomain(values, {minimumSample: 1, singleMidpoint: measurements.length === 1});
    },

    _renderGeometry: function(config, measurements) {
      this._displayGroup.clearLayers();
      this._contextGroup.clearLayers();
      var selectedById = {};
      measurements.forEach(function(m) { selectedById[m.row.geometry_id] = m; });
      var visibleIds = Object.keys(selectedById);
      if (config.viewKey === 'colorado') {
        ['GLDA3_CBRFC_MODELED_UPSTREAM','LKSA3_CBRFC_LOCAL_INTERVENING'].forEach(function(id) { if (visibleIds.indexOf(id) < 0) visibleIds.push(id); });
      }
      var isPercent = ptMajorIsPercent(ptMajorSelectedMetric(config));
      var domain = isPercent ? null : this._volumeDomain(config, measurements);
      var self = this;
      visibleIds.forEach(function(id) {
        var layer = self._featureLayers[id];
        if (!layer || id === 'BDBC1_FNF') return;
        var measurement = selectedById[id];
        var displayState = ptMajorDisplayState(measurement, self._state.showEndedReference);
        var eligible = displayState.eligible;
        var fill = '#d9d9d9';
        if (eligible) fill = isPercent ? ptMajorPercentClass(measurement.value).color : ptMajorVolumeColor(measurement.value, domain);
        var selected = !!measurement;
        layer.setStyle({
          color: displayState.reference ? '#725f3d' : (eligible ? '#283f52' : '#777'),
          weight: displayState.reference ? 2 : (eligible ? 1.7 : 1.1),
          dashArray: displayState.reference ? '6 3' : (eligible ? null : '4 3'),
          fillColor: fill,
          fillOpacity: displayState.reference ? 0.34 : (eligible ? 0.66 : (selected ? 0.06 : 0)),
          opacity: selected ? 1 : 0.72
        });
        self._displayGroup.addLayer(layer);
        if (selected && layer.bringToFront) layer.bringToFront();
      });
      if (this._state.showContext && (config.viewKey === 'colorado' || config.viewKey === 'comparison')) {
        ['HUC2_14_UPPER_COLORADO_CONTEXT','HUC2_15_LOWER_COLORADO_CONTEXT'].forEach(function(id) {
          var layer = self._featureLayers[id];
          if (!layer) return;
          layer.setStyle({color:'#645f55',weight:1.3,dashArray:'6 4',fillColor:'#eee9df',fillOpacity:0.035,opacity:0.85});
          self._contextGroup.addLayer(layer);
        });
      }
      return domain;
    },

    _selectedContextLabel: function(config, measurements) {
      if (config.viewKey === 'ca_major') {
        if (!measurements.length) return 'No basins selected';
        if (measurements.length === 1) {
          var item = PT_MAJOR_BASIN_OPTIONS.filter(function(row) { return row.id === measurements[0].row.geometry_id; })[0];
          return item ? item.label : measurements[0].row.geometry_id;
        }
        return measurements.length + ' forecast basins';
      }
      if (config.viewKey === 'ca_indices') {
        var index = PT_MAJOR_INDEX_OPTIONS.filter(function(row) { return row[0] === this._state.indexGeometryId; }, this)[0];
        return index ? index[1] + ' index' : 'Forecast index';
      }
      return config.viewKey === 'colorado' ? config.period.label : config.view.label;
    },

    _legendHtml: function(config, domain, measurements) {
      var metric = ptMajorSelectedMetric(config);
      var measureLabel = config.period.measures.filter(function(row) { return row[0] === config.measure; })[0][1];
      var metricLabel = config.periodKey === 'short_range' ? ptMajorAccumulationLabel(config.horizon, true) + ' · ' + measureLabel : measureLabel;
      var referenceMode = !!(this._state.showEndedReference && (measurements || []).some(function(m) { return ptMajorDisplayState(m, true).reference; }));
      var eligibleCount = (measurements || []).filter(function(m) { return ptMajorDisplayState(m, referenceMode).eligible; }).length;
      var status = referenceMode ? 'Reference only · forecast period ended' : (eligibleCount ? 'Current map-eligible values' : 'No current map-eligible values');
      var html = '<section class="pt-major-legend-block' + (referenceMode ? ' is-reference' : '') + '" aria-label="Thematic legend"><div class="pt-major-legend-kicker">Thematic legend</div><div class="pt-major-legend-title">' + escapeHtml(this._selectedContextLabel(config, measurements || [])) + '</div><div class="pt-major-legend-meta">' + escapeHtml(config.period.label + ' · ' + metricLabel) + '</div><div class="pt-major-legend-status">' + escapeHtml(status) + ' · n=' + eligibleCount + '</div>';
      if (ptMajorIsPercent(metric)) {
        if (!referenceMode && (measurements || []).length && !(measurements || []).some(function(m) { return ptMajorDisplayState(m, false).current; })) {
          return html + '<div class="pt-major-legend-empty">The selected forecast period ended. Enable reference values to display the direct published percentages.</div></section>';
        }
        html += '<div class="pt-major-legend-scale pt-major-legend-percent" role="list">' + PT_MAJOR_PERCENT_CLASSES.map(function(cls) { return '<div role="listitem"><span class="pt-major-basin-swatch" style="background:' + cls.color + '"></span><span>' + cls.label + '</span></div>'; }).join('') + '</div>';
        return html + '<div class="pt-major-legend-note">Fixed absolute classes; direct producer percentages only.' + (referenceMode ? ' Subdued reference styling.' : '') + '</div></section>';
      }
      if (!domain || domain.mode === 'insufficient') {
        return html + '<div class="pt-major-legend-empty">No ' + (referenceMode ? 'direct ended-period reference' : 'current map-eligible') + ' values for this selection.</div></section>';
      }
      var unit = ptMajorLegendUnit(domain.max);
      if (domain.mode === 'midpoint') {
        return html + '<div class="pt-major-legend-scale pt-major-legend-volume is-midpoint" role="list"><div role="listitem"><span class="pt-major-basin-swatch" style="background:' + PT_MAJOR_VOLUME_COLORS[2] + '"></span><span>' + ptMajorLegendVolume(domain.min, unit) + '</span></div></div><div class="pt-major-legend-note">Single/equal-value midpoint; exact value retained.' + (referenceMode ? ' Reference only.' : '') + '</div></section>';
      }
      html += '<div class="pt-major-legend-unit">Unit: ' + escapeHtml(unit) + '</div><div class="pt-major-legend-scale pt-major-legend-volume" role="list">' + PT_MAJOR_VOLUME_COLORS.map(function(color, index) {
          var ratio = index / (PT_MAJOR_VOLUME_COLORS.length - 1);
          var value = Math.pow(Math.sqrt(domain.min) + ratio * (Math.sqrt(domain.max) - Math.sqrt(domain.min)), 2);
          return '<div role="listitem"><span class="pt-major-basin-swatch" style="background:' + color + '"></span><span>' + ptMajorLegendVolume(value, unit) + '</span></div>';
        }).join('') + '</div><div class="pt-major-legend-note">Square-root volume scale; one display unit.' + (referenceMode ? ' Subdued reference styling.' : '') + '</div>';
      return html + '</section>';
    },

    _noticeHtml: function(config, measurements) {
      var notices = [];
      if (config.periodKey === 'april_july' && measurements.length && measurements.every(function(m) { return m.state && m.state.status === 'expired'; })) {
        notices.push(this._state.showEndedReference ? 'Showing direct published ended-period values for reference; source map eligibility remains expired.' : 'Forecast period ended; enable “Show ended-period values for reference” to display direct published values.');
      }
      if (config.periodKey === 'short_range') {
        var mh = measurements.filter(function(m) { return m.row.forecast_key === 'CNRFC:MHBC1:10D_VOLUME_ACCUM'; })[0];
        if (mh && (!mh.state || mh.state.status === 'unavailable')) notices.push('MHBC1 short-range product is not published by CNRFC.');
      }
      if (config.period.family === 'monthly_local_intervening') {
        var local = measurements[0];
        if (local && local.holder) notices.push('Lake Mead Local ' + ptMajorFormatMonth(local.holder.forecast_month) + ' forecast selected.');
      }
      if (measurements.length && !measurements.some(function(m) { return ptMajorMetricEligible(m.state) && m.value !== null; })) {
        notices.push('No map-eligible values for the selected metric.');
      }
      [this._cnrfc, this._cbrfc].forEach(function(slot) {
        if (slot.lastError && slot.accepted) notices.push(slot.id + ' refresh failed; continuing to display the last accepted ' + slot.id + ' snapshot.');
      });
      return notices.map(function(text) { return '<div>' + escapeHtml(text) + '</div>'; }).join('');
    },

    _render: function() {
      if (!this._state.active) return;
      var config = this._config();
      this._renderControls(config);
      this._renderSelectionTools(config);
      var measurements = this._measurements(config);
      var ended = measurements.length > 0 && measurements.some(function(m) { return m.state && m.state.status === 'expired'; });
      var endedRow = this._card && this._card.querySelector('.pt-major-basin-ended-row');
      if (endedRow) endedRow.style.display = ended ? '' : 'none';
      if (this._card) this._card.querySelector('.pt-major-basin-ended').checked = this._state.showEndedReference;
      var domain = this._renderGeometry(config, measurements);
      this._state.thematicDomain = domain;
      if (this._card) {
        var statusDetails = this._card.querySelector('.pt-major-basin-status');
        var statusExpanded = !!statusDetails.open;
        var subtitle = config.view.label + ' · ' + config.period.label;
        if (config.viewKey === 'ca_indices') subtitle = config.view.label + ' · ' + this._selectedContextLabel(config, measurements) + ' · ' + config.period.label;
        this._card.querySelector('.pt-major-basin-subtitle').textContent = subtitle;
        this._card.querySelector('.pt-major-basin-health').innerHTML = this._compactHealthHtml(this._cnrfc, statusExpanded) + this._compactHealthHtml(this._cbrfc, statusExpanded);
        this._card.querySelector('.pt-major-basin-legend').innerHTML = this._legendHtml(config, domain, measurements);
        this._card.querySelector('.pt-major-basin-notice').innerHTML = this._noticeHtml(config, measurements);
        this._card.querySelector('.pt-major-basin-official').innerHTML = this._officialLinksHtml(config, measurements);
        this._card.querySelector('.pt-major-basin-status-body').innerHTML = this._statusHtml(config, measurements);
      }
      var shared = window.BRIM && window.BRIM.legendCloseout;
      if (shared && shared.scheduleLayout && this._card && this._card.style.display !== 'none') shared.scheduleLayout(this._card);
      this._syncSharedCardState();
    },

    _syncSharedCardState: function() {
      if (!this._card) return;
      var sharedState = this._card.__brimDetachableState;
      this._state.docked = !(sharedState && sharedState.floating);
      this._state.visible = this._card.style.display !== 'none';
      this._state.floatingPosition = this._state.docked ? null : {
        left: this._card.style.left || '',
        top: this._card.style.top || ''
      };
    },

    getState: function() {
      this._syncSharedCardState();
      return {
        active: this._state.active,
        visible: this._state.visible,
        docked: this._state.docked,
        floatingPosition: this._state.floatingPosition,
        view: this._state.view,
        period: this._state.period,
        measure: this._state.measure,
        horizon: this._state.horizon,
        indexGeometryId: this._state.indexGeometryId,
        selectedBasins: this._state.selectedBasins.slice(),
        focusedGeometryId: this._state.focusedGeometryId,
        basinSearch: this._state.basinSearch,
        autoZoom: this._state.autoZoom,
        showContext: this._state.showContext,
        showEndedReference: this._state.showEndedReference,
        cnrfcAccepted: this._state.cnrfcAccepted,
        cbrfcAccepted: this._state.cbrfcAccepted,
        cnrfcHealth: this._state.cnrfcHealth,
        cbrfcHealth: this._state.cbrfcHealth,
        thematicDomain: this._state.thematicDomain,
        lastRefreshStatus: this._state.lastRefreshStatus
      };
    },

    _measurementForGeometry: function(id) {
      var config = this._config();
      var measurement = this._measurements(config).filter(function(m) { return m.row.geometry_id === id; })[0];
      return {config: config, measurement: measurement};
    },

    _hoverHtml: function(id) {
      var selected = this._measurementForGeometry(id);
      var m = selected.measurement;
      var props = (this._featureLayers[id] && this._featureLayers[id].feature.properties) || {};
      if (!m) {
        if (selected.config.viewKey === 'colorado' && this._cbrfc.accepted) {
          var instruction = id === 'LKSA3_CBRFC_LOCAL_INTERVENING' ? 'Select Lake Mead Local to display the current monthly forecast.' : 'Select a Lake Powell product to display its forecast.';
          return '<b>' + escapeHtml(props.display_name || id) + '</b><br>' + escapeHtml(instruction);
        }
        return '<b>' + escapeHtml(props.display_name || id) + '</b>';
      }
      if (!m.record) return '<b>' + escapeHtml(props.display_name || id) + '</b><br>Awaiting accepted source data.';
      var displayState = ptMajorDisplayState(m, this._state.showEndedReference);
      if (m.state && m.state.status === 'expired' && !displayState.reference) {
        return '<b>' + escapeHtml(m.record.display_name || props.display_name || id) + '</b><br>Forecast period ended.<br><span class="pt-ops-muted">Enable ended-period values to display the published value for reference.</span>';
      }
      var holder = m.holder || m.record;
      var period = holder.forecast_month ? ptMajorFormatMonth(holder.forecast_month) :
        (selected.config.periodKey === 'short_range' ? ptMajorAccumulationLabel(selected.config.horizon, true) : (m.record.forecast_period || selected.config.period.label));
      var html = '<b>' + escapeHtml(m.record.display_name || props.display_name || id) + '</b><br>' + escapeHtml(period) + '<br>';
      if (m.row.product_family === 'monthly_local_intervening') {
        if (!holder) return html + 'No single current map-eligible month.';
        html += 'Monthly volume: ' + escapeHtml(ptMajorFormatKaf(holder.forecast_volume)) + '<br>Percent of median: ' + escapeHtml(ptMajorFormatPercent(holder.percent_median));
      } else if (ptMajorIsPercent(m.metric)) {
        html += escapeHtml(ptMajorMetricLabel(m.metric, m.record)) + ': ' + escapeHtml(ptMajorFormatPercent(m.value)) + '<br>Forecast volume: ' + escapeHtml(ptMajorFormatKaf(m.record.forecast_volume));
      } else {
        html += escapeHtml(ptMajorMetricLabel(m.metric, m.record)) + ': ' + escapeHtml(ptMajorFormatKaf(m.value));
        ['percent_mean','percent_average','percent_median'].forEach(function(metric) {
          if (ptMajorOwn(m.record, metric) && m.record[metric] !== null) html += '<br>' + escapeHtml(ptMajorMetricLabel(metric, m.record)) + ': ' + escapeHtml(ptMajorFormatPercent(m.record[metric]));
        });
      }
      html += '<br><span class="pt-ops-muted">' + escapeHtml(displayState.reference ? 'Reference only — forecast period ended' : ptMajorStateText(m.state)) + '</span>';
      return html;
    },

    _recordForecastHtml: function(record, row, options) {
      return ptMajorRecordForecastParts(record, row, options || {}).summary;
    },

    _popupHtml: function(id) {
      var layer = this._featureLayers[id];
      var props = layer && layer.feature ? layer.feature.properties : {};
      var rows = MAJOR_WATER_SUPPLY_PRODUCT_MAPPING.filter(function(row) { return row.geometry_id === id; });
      var self = this;
      var config = this._config();
      var selectedKeys = {};
      ptMajorSelectionRows(config, {indexGeometryId: this._state.indexGeometryId}).forEach(function(row) { selectedKeys[row.forecast_key] = true; });
      var familyOrder = {water_year:1,water_year_index:1,monthly_local_intervening:1,ten_day_accumulation:2,april_july:3};
      rows = rows.slice().sort(function(a, b) {
        var selectedOrder = Number(!!selectedKeys[b.forecast_key]) - Number(!!selectedKeys[a.forecast_key]);
        return selectedOrder || (familyOrder[a.product_family] || 9) - (familyOrder[b.product_family] || 9) || Number(a.display_order || 0) - Number(b.display_order || 0);
      });
      var parts = rows.map(function(row) {
        var index = row.source_family === 'CBRFC' ? self._cbrfc.acceptedIndex : self._cnrfc.acceptedIndex;
        var record = index[row.forecast_key];
        return ptMajorRecordForecastParts(record, row, {
          selectedProduct: !!selectedKeys[row.forecast_key],
          selectedMetric: ptMajorSelectedMetric(config, row, record)
        });
      });
      var forecasts = parts.map(function(part) { return part.summary; }).join('');
      var provenance = parts.map(function(part) { return part.provenance; }).filter(Boolean).join('');
      var directLinks = MAJOR_WATER_SUPPLY_RELATED_LINKS.filter(function(link) { return link.geometry_id === id; });
      var hasCrosswalk = MAJOR_WATER_SUPPLY_RESERVOIR_CROSSWALK.some(function(row) { return row.geometry_id === id; });
      if (hasCrosswalk) {
        directLinks = directLinks.concat(MAJOR_WATER_SUPPLY_RELATED_LINKS.filter(function(link) { return link.geometry_scope === 'california_reservoir_crosswalk'; }));
      }
      var related = directLinks.filter(function(link) {
        return ['reservoir_metadata','reservoir_plots','reservoir_operations','water_operations','reservoir_conditions'].indexOf(link.link_type) >= 0;
      });
      var additional = directLinks.filter(function(link) { return related.indexOf(link) < 0; });
      if (props.view_group === 'major_basin' || props.view_group === 'index_component' || props.view_group === 'index') {
        additional = additional.concat(MAJOR_WATER_SUPPLY_RELATED_LINKS.filter(function(link) { return link.geometry_scope === 'california_forecast_basins'; }));
      }
      function linksHtml(items) {
        var seen = {};
        return '<div class="pt-major-link-list">' + items.map(function(link) {
          if (!link.url || seen[link.url]) return '';
          seen[link.url] = true;
          return '<div>' + ptMajorHtmlLink(link.url, link.link_label) + ' <span class="pt-ops-muted">(' + escapeHtml(link.source_agency) + ')</span></div>';
        }).filter(Boolean).join('') + '</div>';
      }
      var selectedRow = rows.filter(function(row) { return selectedKeys[row.forecast_key]; })[0] || rows[0];
      var selectedIndex = selectedRow && selectedRow.source_family === 'CBRFC' ? this._cbrfc.acceptedIndex : this._cnrfc.acceptedIndex;
      var selectedRecord = selectedRow && selectedIndex ? selectedIndex[selectedRow.forecast_key] : null;
      var lid = selectedRecord && selectedRecord.nws_lid ? selectedRecord.nws_lid : (rows.length ? String(rows[0].forecast_key || '').split(':')[1] : '');
      var name = props.display_name || (selectedRecord && selectedRecord.display_name) || id;
      var secondary = [];
      if (props.river_name && String(name).toLowerCase().indexOf(String(props.river_name).toLowerCase()) < 0) secondary.push(props.river_name);
      if (lid) secondary.push('LID ' + lid);
      var html = '<div class="pt-major-basin-popup"><div class="pt-major-basin-popup-header"><div><div class="pt-major-basin-popup-name">' + escapeHtml(name) + '</div>' + (secondary.length ? '<div class="pt-major-basin-popup-secondary">' + secondary.map(escapeHtml).join(' · ') + '</div>' : '') + '</div>' + (selectedRecord ? ptMajorStatusBadge(selectedRecord.status || 'unknown', selectedRow && selectedRow.product_family === 'april_july') : '') + '</div>';
      html += '<h4>Forecast summary</h4>' + (forecasts || '<div class="pt-ops-muted">No mapped forecast products.</div>');
      if (related.length) html += '<h4>Related reservoir and operating links</h4>' + linksHtml(related);
      if (additional.length) html += '<h4>Additional sources</h4>' + linksHtml(additional);
      html += '<details class="pt-major-provenance"><summary>Details &amp; provenance</summary><div class="pt-major-provenance-body">';
      if (provenance) html += '<h4>Forecast source &amp; timing</h4>' + provenance;
      html += '<section class="pt-major-geometry-details"><h4>Geometry &amp; methodology</h4><dl>';
      if (lid) html += '<dt>LID</dt><dd>' + escapeHtml(lid) + '</dd>';
      html += '<dt>Geometry role</dt><dd>' + escapeHtml(String(props.geometry_role || '').replace(/_/g, ' ')) + '</dd>';
      if (props.generalization_note) html += '<dt>Method</dt><dd>' + escapeHtml(props.generalization_note) + '</dd>';
      html += '</dl></section>' + ptMajorCompositionHtml(id) + '</div></details>';
      return html + '</div>';
    },

    _contextPopup: function(id) {
      var props = this._featureLayers[id] && this._featureLayers[id].feature.properties || {};
      return '<div class="pt-major-basin-popup"><h4>Colorado River Basin context</h4><b>' + escapeHtml(props.display_name || id) + '</b><div>Broad USGS hydrologic region shown for context only; it is not an operational forecast footprint and carries no forecast value.</div></div>';
    }
  });

  function makeMajorWaterSupplyBasinForecastLayer(options) {
    return new MajorWaterSupplyBasinForecastLayer(options);
  }

  window.BRIM = window.BRIM || {};
  window.BRIM.majorWaterSupplyForecast = {
    validateCnrfc: ptMajorValidateCnrfc,
    validateCbrfc: ptMajorValidateCbrfc,
    sourceSlot: ptMajorSourceSlot,
    metricEligible: ptMajorMetricEligible,
    displayState: ptMajorDisplayState,
    formatKaf: ptMajorFormatKaf,
    formatPercent: ptMajorFormatPercent,
    formatTimestamp: ptMajorFormatTimestamp,
    accumulationLabel: ptMajorAccumulationLabel,
    recordForecastParts: ptMajorRecordForecastParts,
    compositionHtml: ptMajorCompositionHtml,
    percentClass: ptMajorPercentClass,
    percentClasses: PT_MAJOR_PERCENT_CLASSES,
    volumeDomain: ptMajorVolumeDomain,
    volumeColor: ptMajorVolumeColor,
    legendUnit: ptMajorLegendUnit,
    viewCapabilities: ptMajorViewCapabilities,
    viewConfig: ptMajorViewConfig,
    choiceHtml: ptMajorChoiceHtml,
    highlightedText: ptMajorHighlightedText,
    basinOptions: PT_MAJOR_BASIN_OPTIONS.slice(),
    selectedMonth: ptMajorSelectedMonth,
    selectedMetric: ptMajorSelectedMetric,
    selectionRows: ptMajorSelectionRows,
    indexOptions: PT_MAJOR_INDEX_OPTIONS.slice(),
    measurement: ptMajorMeasurement,
    cbrfcKeys: PT_MAJOR_CBRFC_KEYS.slice(),
    cnrfcKeys: PT_MAJOR_CNRFC_KEYS.slice()
  };
  )---"
}
