# ==== leaflet_ops_live_nbm_accumulated_qpf_helpers.r =======================
##
## PURPOSE:
##   Browser-only selected-cycle accumulation of the existing NOAA/NBM
##   six-hour QPF numeric sidecars. No producer, endpoint, public asset, or
##   preprocessing change is part of this consumer.
##
## CONTRACT:
##   - require one exact complete f006--f240 cycle;
##   - retain only verified compressed interval buffers plus one uint32 result;
##   - fetch with bounded concurrency and fail closed on any interval failure;
##   - decode/sum/palette in one cycle-scoped Worker;
##   - render atomically through one persistent Leaflet canvas layer;
##   - keep accumulation endpoints independent from snapshot valid time.
## ============================================================================

pt_ops_live_nbm_accumulated_qpf_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // Ops Live NOAA/NBM accumulated QPF consumer
  // --------------------------------------------------------------------------

  var PT_ACCUM_START_DEFAULT = 0;
  var PT_ACCUM_END_DEFAULT = 240;
  var PT_ACCUM_STEP_HOURS = 6;
  var PT_ACCUM_FETCH_CONCURRENCY = 6;
  var PT_ACCUM_RESULT_NODATA = 0xffffffff;
  var PT_ACCUM_CANVAS_OPACITY = 0.64;
  var PT_ACCUM_RANGE_STATE = {start: PT_ACCUM_START_DEFAULT, end: PT_ACCUM_END_DEFAULT};
  var PT_ACCUM_PALETTE = [
    {lower: 0, upper: 0.001, color: [231, 238, 241, 36], label: '0'},
    {lower: 0.001, upper: 0.01, color: [225, 245, 254, 210], label: 'trace'},
    {lower: 0.01, upper: 0.05, color: [200, 230, 201, 225], label: '0.01'},
    {lower: 0.05, upper: 0.10, color: [165, 214, 167, 230], label: '0.05'},
    {lower: 0.10, upper: 0.25, color: [102, 187, 106, 235], label: '0.10'},
    {lower: 0.25, upper: 0.50, color: [38, 166, 91, 238], label: '0.25'},
    {lower: 0.50, upper: 0.75, color: [11, 125, 62, 240], label: '0.50'},
    {lower: 0.75, upper: 1.00, color: [255, 241, 118, 242], label: '0.75'},
    {lower: 1.00, upper: 1.50, color: [255, 202, 40, 244], label: '1'},
    {lower: 1.50, upper: 2.00, color: [251, 140, 0, 246], label: '1.5'},
    {lower: 2.00, upper: 3.00, color: [244, 81, 30, 247], label: '2'},
    {lower: 3.00, upper: 4.00, color: [211, 47, 47, 248], label: '3'},
    {lower: 4.00, upper: 5.00, color: [173, 20, 87, 249], label: '4'},
    {lower: 5.00, upper: 7.50, color: [123, 31, 162, 250], label: '5'},
    {lower: 7.50, upper: 10.0, color: [81, 45, 168, 250], label: '7.5'},
    {lower: 10.0, upper: 15.0, color: [57, 73, 171, 250], label: '10'},
    {lower: 15.0, upper: 20.0, color: [3, 145, 155, 250], label: '15'},
    {lower: 20.0, upper: 30.0, color: [0, 96, 100, 252], label: '20'},
    {lower: 30.0, upper: 40.0, color: [69, 39, 160, 252], label: '30'},
    {lower: 40.0, upper: null, color: [45, 18, 72, 255], label: '40+'}
  ];

  function ptNbmAccumValidateRange(startLead, endLead) {
    var start = Number(startLead);
    var end = Number(endLead);
    if (!Number.isInteger(start) || !Number.isInteger(end) ||
        start < 0 || end > 240 || start % PT_ACCUM_STEP_HOURS !== 0 ||
        end % PT_ACCUM_STEP_HOURS !== 0 || start >= end) {
      throw new Error('Accumulated QPF range must use exact six-hour boundaries with 0 <= start < end <= 240.');
    }
    return {start: start, end: end, duration: end - start};
  }

  function ptNbmAccumExpectedLeads(startLead, endLead) {
    var range = ptNbmAccumValidateRange(startLead, endLead);
    var leads = [];
    for (var lead = range.start + PT_ACCUM_STEP_HOURS;
      lead <= range.end; lead += PT_ACCUM_STEP_HOURS) {
      leads.push(lead);
    }
    return leads;
  }

  function ptNbmAccumValidateCycle(cycle) {
    if (!cycle || typeof cycle !== 'object' || !Array.isArray(cycle.targets)) {
      throw new Error('Selected NBM cycle has no validated QPF target inventory.');
    }
    var expected = ptNbmAccumExpectedLeads(0, 240);
    var actual = cycle.targets.map(function(target) { return Number(target.lead_hours); });
    if (cycle.cycle_status !== 'complete' || actual.length !== expected.length ||
        !ptSnowSameArray(actual, expected) || !ptSnowSameArray(cycle.complete_required_leads_hours, expected)) {
      throw new Error('Accumulated QPF requires one complete selected-cycle f006 through f240 inventory.');
    }
    cycle.targets.forEach(function(target, index) {
      ptQpfValidateTarget(target, cycle.cycle_utc, index);
      if (target.cycle_utc !== cycle.cycle_utc) {
        throw new Error('Accumulated QPF target belongs to the wrong NBM cycle.');
      }
    });
    return cycle;
  }

  function ptNbmAccumRequiredTargets(cycle, startLead, endLead) {
    ptNbmAccumValidateCycle(cycle);
    var byLead = Object.create(null);
    cycle.targets.forEach(function(target) { byLead[Number(target.lead_hours)] = target; });
    return ptNbmAccumExpectedLeads(startLead, endLead).map(function(lead) {
      var target = byLead[lead];
      if (!target || target.cycle_utc !== cycle.cycle_utc || Number(target.lead_hours) !== lead) {
        throw new Error('Accumulated QPF exact selected-cycle interval +' + lead + ' h is unavailable.');
      }
      return target;
    });
  }

  function ptNbmAccumAddDecodedGrid(totals, values, nodata) {
    if (!(values instanceof Uint16Array)) throw new Error('Accumulated QPF source grid must be uint16.');
    var result = totals;
    if (result === null || result === undefined) {
      result = new Uint32Array(values.length);
      for (var first = 0; first < values.length; first += 1) {
        result[first] = values[first] === nodata ? PT_ACCUM_RESULT_NODATA : values[first];
      }
      return result;
    }
    if (!(result instanceof Uint32Array) || result.length !== values.length) {
      throw new Error('Accumulated QPF result/source grid size disagrees.');
    }
    for (var index = 0; index < values.length; index += 1) {
      var sourceNoData = values[index] === nodata;
      var resultNoData = result[index] === PT_ACCUM_RESULT_NODATA;
      if (sourceNoData !== resultNoData) {
        throw new Error('Accumulated QPF source grids disagree on the common NoData mask.');
      }
      if (!sourceNoData) result[index] += values[index];
    }
    return result;
  }

  function ptNbmAccumSubtractDecodedGrid(totals, values, nodata) {
    if (!(totals instanceof Uint32Array) || !(values instanceof Uint16Array) ||
        totals.length !== values.length) {
      throw new Error('Accumulated QPF subtraction grid size disagrees.');
    }
    for (var index = 0; index < values.length; index += 1) {
      var sourceNoData = values[index] === nodata;
      var resultNoData = totals[index] === PT_ACCUM_RESULT_NODATA;
      if (sourceNoData !== resultNoData) {
        throw new Error('Accumulated QPF source grids disagree on the common NoData mask.');
      }
      if (!sourceNoData) {
        if (values[index] > totals[index]) throw new Error('Accumulated QPF incremental subtraction would underflow.');
        totals[index] -= values[index];
      }
    }
    return totals;
  }

  function ptNbmAccumFullFromDecoded(frames) {
    if (!Array.isArray(frames) || !frames.length) throw new Error('Accumulated QPF needs at least one interval grid.');
    var totals = null;
    frames.forEach(function(frame) {
      totals = ptNbmAccumAddDecodedGrid(totals, frame.values, Number(frame.nodata));
    });
    return totals;
  }

  function ptNbmAccumPaletteRgba(totals, scale, palette) {
    if (!(totals instanceof Uint32Array)) throw new Error('Accumulated QPF palette input must be uint32.');
    var classes = palette || PT_ACCUM_PALETTE;
    var rgba = new Uint8ClampedArray(totals.length * 4);
    for (var index = 0; index < totals.length; index += 1) {
      var stored = totals[index];
      if (stored === PT_ACCUM_RESULT_NODATA) continue;
      var inches = stored * scale;
      var selected = classes[classes.length - 1];
      for (var classIndex = 0; classIndex < classes.length; classIndex += 1) {
        var item = classes[classIndex];
        if (inches >= item.lower && (item.upper === null || inches < item.upper)) {
          selected = item;
          break;
        }
      }
      var offset = index * 4;
      rgba[offset] = selected.color[0];
      rgba[offset + 1] = selected.color[1];
      rgba[offset + 2] = selected.color[2];
      rgba[offset + 3] = selected.color[3];
    }
    return rgba;
  }

  function ptNbmAccumEndpointParts(cycleUtc, leadHours) {
    var cycle = ptSnowDate(cycleUtc, 'Accumulated QPF cycle');
    var endpoint = new Date(cycle.getTime() + Number(leadHours) * 3600000);
    var parts = new Intl.DateTimeFormat('en-US', {
      timeZone: PT_SNOW_PACIFIC_ZONE,
      weekday: 'short', month: 'short', day: 'numeric',
      hour: 'numeric', minute: '2-digit', hour12: true, timeZoneName: 'short'
    }).formatToParts(endpoint);
    var values = {};
    parts.forEach(function(part) { if (part.type !== 'literal') values[part.type] = part.value; });
    var clock = values.hour + (values.minute === '00' ? '' : ':' + values.minute) + ' ' + values.dayPeriod;
    return {
      weekday: String(values.weekday || '').toUpperCase(),
      clock: clock,
      date: values.month + ' ' + values.day,
      lead: '+' + Number(leadHours) + ' h',
      compact: String(values.weekday || '') + ' ' + clock,
      hover: String(values.weekday || '') + ' ' + clock + ' ' + String(values.timeZoneName || ''),
      utc_compact: String(endpoint.getUTCDate()).padStart(2, '0') + '/' +
        String(endpoint.getUTCHours()).padStart(2, '0') + 'Z'
    };
  }

  function ptNbmAccumQuickRange(action, startLead) {
    var start = Number(startLead);
    if (action === 'full') return ptNbmAccumValidateRange(0, 240);
    var duration = action === 'next24' ? 24 : (action === 'next72' ? 72 : null);
    if (duration === null || start + duration > 240) return null;
    return ptNbmAccumValidateRange(start, start + duration);
  }

  function PtNbmAccumCompressedLoader(manifestUrl, dependencies) {
    this.manifestUrl = manifestUrl;
    this.dependencies = dependencies || {};
    this.cycleUtc = null;
    this.generation = 0;
    this.cache = new Map();
    this.inflight = new Map();
    this.controllers = new Map();
    this.metrics = {requests: 0, transferredBytes: 0, fetchMs: 0, hashMs: 0, deduplicated: 0};
  }

  PtNbmAccumCompressedLoader.prototype.setCycle = function(cycleUtc) {
    cycleUtc = String(cycleUtc || '');
    if (this.cycleUtc === cycleUtc) return;
    this.clear();
    this.cycleUtc = cycleUtc;
  };

  PtNbmAccumCompressedLoader.prototype._identityMatches = function(left, entry) {
    return left && left.cycle_utc === entry.cycle_utc &&
      left.forecast_state_id === entry.forecast_state_id &&
      left.numeric_path === entry.numeric.path && left.numeric_sha256 === entry.numeric.sha256;
  };

  PtNbmAccumCompressedLoader.prototype.acquire = function(entry) {
    var self = this;
    if (!entry || entry.cycle_utc !== this.cycleUtc) {
      return Promise.reject(new Error('Accumulated QPF compressed request is bound to the wrong selected cycle.'));
    }
    var key = entry.forecast_state_id;
    var cached = this.cache.get(key);
    if (cached) {
      if (!this._identityMatches(cached, entry)) {
        return Promise.reject(new Error('Accumulated QPF compressed cache identity conflict.'));
      }
      return Promise.resolve({entry: entry, buffer: cached.buffer, cacheHit: true,
        bytes: cached.buffer.byteLength, fetchMs: 0, hashMs: 0});
    }
    var flight = this.inflight.get(key);
    if (flight) {
      if (!this._identityMatches(flight, entry)) {
        return Promise.reject(new Error('Accumulated QPF in-flight compressed identity conflict.'));
      }
      this.metrics.deduplicated += 1;
      return flight.promise;
    }
    var generation = this.generation;
    var abort = typeof AbortController === 'function' ? new AbortController() : null;
    var flightRecord = {
      cycle_utc: entry.cycle_utc,
      forecast_state_id: entry.forecast_state_id,
      numeric_path: entry.numeric.path,
      numeric_sha256: entry.numeric.sha256,
      promise: null
    };
    var fetchFn = this.dependencies.fetchFn || fetch;
    var started = ptSnowPerfNow();
    this.metrics.requests += 1;
    flightRecord.promise = Promise.resolve(fetchFn(ptQpfNumericUrl(entry, this.manifestUrl), {
      cache: 'force-cache', signal: abort ? abort.signal : undefined
    })).then(async function(response) {
      if (!response || !response.ok) {
        throw new Error('Accumulated QPF interval request failed with HTTP ' +
          (response ? response.status : 'unknown') + '.');
      }
      var contentEncoding = response.headers && typeof response.headers.get === 'function'
        ? response.headers.get('content-encoding') : null;
      if (contentEncoding && String(contentEncoding).toLowerCase() !== 'identity') {
        throw new Error('Accumulated QPF interval must be explicit gzip application data, not HTTP Content-Encoding.');
      }
      var buffer = await response.arrayBuffer();
      var fetchMs = ptSnowPerfNow() - started;
      if (!(buffer instanceof ArrayBuffer) || buffer.byteLength !== Number(entry.numeric.compressed_bytes)) {
        throw new Error('Accumulated QPF interval compressed byte count does not match the manifest.');
      }
      var signature = new Uint8Array(buffer, 0, Math.min(2, buffer.byteLength));
      if (signature.length !== 2 || signature[0] !== 0x1f || signature[1] !== 0x8b) {
        throw new Error('Accumulated QPF interval is not RFC1952 gzip application data.');
      }
      var hashStarted = ptSnowPerfNow();
      var digest = await ptQpfDigestHex(buffer, self.dependencies);
      if (digest !== entry.numeric.sha256) {
        throw new Error('Accumulated QPF interval SHA-256 does not match the manifest.');
      }
      await ptQpfVerifyForecastState(entry, self.dependencies);
      var hashMs = ptSnowPerfNow() - hashStarted;
      if (generation !== self.generation || self.cycleUtc !== entry.cycle_utc) {
        var stale = new Error('Accumulated QPF interval request became stale.');
        stale.name = 'AbortError';
        throw stale;
      }
      var stored = {
        cycle_utc: entry.cycle_utc, forecast_state_id: entry.forecast_state_id,
        numeric_path: entry.numeric.path, numeric_sha256: entry.numeric.sha256,
        buffer: buffer
      };
      self.cache.set(key, stored);
      self.metrics.transferredBytes += buffer.byteLength;
      self.metrics.fetchMs += fetchMs;
      self.metrics.hashMs += hashMs;
      return {entry: entry, buffer: buffer, cacheHit: false, bytes: buffer.byteLength,
        fetchMs: fetchMs, hashMs: hashMs};
    }).finally(function() {
      self.inflight.delete(key);
      self.controllers.delete(key);
    });
    this.inflight.set(key, flightRecord);
    if (abort) this.controllers.set(key, abort);
    return flightRecord.promise;
  };

  PtNbmAccumCompressedLoader.prototype.loadMany = async function(entries, onProgress, concurrency) {
    var self = this;
    var next = 0;
    var completed = 0;
    var results = new Array(entries.length);
    var limit = Math.max(1, Math.min(Number(concurrency) || PT_ACCUM_FETCH_CONCURRENCY, entries.length));
    async function runner() {
      while (next < entries.length) {
        var index = next;
        next += 1;
        results[index] = await self.acquire(entries[index]);
        completed += 1;
        if (typeof onProgress === 'function') onProgress(completed, entries.length);
      }
    }
    var runners = [];
    for (var index = 0; index < limit; index += 1) runners.push(runner());
    await Promise.all(runners);
    return results;
  };

  PtNbmAccumCompressedLoader.prototype.compressedBytes = function() {
    var bytes = 0;
    this.cache.forEach(function(entry) { bytes += entry.buffer.byteLength; });
    return bytes;
  };

  PtNbmAccumCompressedLoader.prototype.clear = function() {
    this.generation += 1;
    this.controllers.forEach(function(controller) {
      try { controller.abort(); } catch(error) {}
    });
    this.controllers.clear();
    this.inflight.clear();
    this.cache.clear();
  };

  function ptNbmAccumWorkerMain() {
    var cache = new Map();
    var cycleUtc = null;
    var columns = 0;
    var rows = 0;
    var scale = 0.001;
    var nodata = 65535;
    var palette = [];
    var resultState = null;

    function postOk(requestId, payload, transfers) {
      var message = Object.assign({requestId: requestId, ok: true}, payload || {});
      self.postMessage(message, transfers || []);
    }

    function postError(requestId, error) {
      self.postMessage({requestId: requestId, ok: false,
        error: String(error && error.message ? error.message : error)});
    }

    async function gunzip(entry) {
      var stream = new Blob([entry.buffer], {type: 'application/gzip'}).stream()
        .pipeThrough(new DecompressionStream('gzip'));
      var expanded = await new Response(stream).arrayBuffer();
      if (expanded.byteLength !== columns * rows * 2 ||
          expanded.byteLength !== Number(entry.uncompressed_bytes)) {
        throw new Error('Accumulated QPF interval uncompressed byte count is invalid.');
      }
      var view = new DataView(expanded);
      var values = new Uint16Array(columns * rows);
      for (var index = 0; index < values.length; index += 1) {
        values[index] = view.getUint16(index * 2, true);
      }
      return values;
    }

    function findTarget(targets, lead) {
      for (var index = 0; index < targets.length; index += 1) {
        if (Number(targets[index].lead_hours) === Number(lead)) return targets[index];
      }
      return null;
    }

    async function compute(message) {
      var started = performance.now();
      var targets = message.targets || [];
      var expectedCount = (Number(message.end) - Number(message.start)) / 6;
      if (message.cycle_utc !== cycleUtc || !targets.length || targets.length !== expectedCount) {
        throw new Error('Accumulated QPF Worker request is not bound to the exact selected cycle/window.');
      }
      targets.forEach(function(target) {
        var cached = cache.get(target.forecast_state_id);
        if (!cached || cached.cycle_utc !== cycleUtc || cached.numeric_path !== target.numeric_path ||
            cached.numeric_sha256 !== target.numeric_sha256) {
          throw new Error('Accumulated QPF Worker is missing a verified compressed interval.');
        }
      });

      var mode = 'full';
      var operation = null;
      var changedTarget = null;
      if (resultState && resultState.cycle_utc === cycleUtc) {
        if (message.start === resultState.start && message.end === resultState.end + 6) {
          mode = 'incremental'; operation = 'add'; changedTarget = findTarget(targets, message.end);
        } else if (message.start === resultState.start && message.end === resultState.end - 6) {
          mode = 'incremental'; operation = 'subtract';
          changedTarget = {lead_hours: resultState.end,
            forecast_state_id: resultState.target_ids[resultState.end]};
        } else if (message.start === resultState.start + 6 && message.end === resultState.end) {
          mode = 'incremental'; operation = 'subtract';
          changedTarget = {lead_hours: resultState.start + 6,
            forecast_state_id: resultState.target_ids[resultState.start + 6]};
        } else if (message.start === resultState.start - 6 && message.end === resultState.end) {
          mode = 'incremental'; operation = 'add'; changedTarget = findTarget(targets, resultState.start);
        }
      }

      var candidate;
      if (mode === 'incremental' && changedTarget && changedTarget.forecast_state_id) {
        candidate = new Uint32Array(resultState.totals);
        var changedEntry = cache.get(changedTarget.forecast_state_id);
        if (!changedEntry) throw new Error('Accumulated QPF incremental interval is not cached.');
        var changedValues = await gunzip(changedEntry);
        if (operation === 'add') ptNbmAccumAddDecodedGrid(candidate, changedValues, nodata);
        else ptNbmAccumSubtractDecodedGrid(candidate, changedValues, nodata);
      } else {
        mode = 'full';
        candidate = null;
        for (var targetIndex = 0; targetIndex < targets.length; targetIndex += 1) {
          var entry = cache.get(targets[targetIndex].forecast_state_id);
          var values = await gunzip(entry);
          candidate = ptNbmAccumAddDecodedGrid(candidate, values, nodata);
        }
      }

      var targetIds = Object.create(null);
      targets.forEach(function(target) { targetIds[Number(target.lead_hours)] = target.forecast_state_id; });
      resultState = {
        cycle_utc: cycleUtc, start: Number(message.start), end: Number(message.end),
        totals: candidate, target_ids: targetIds
      };
      var rgba = ptNbmAccumPaletteRgba(candidate, scale, palette);
      var cachedCompressedBytes = 0;
      cache.forEach(function(entry) { cachedCompressedBytes += entry.buffer.byteLength; });
      postOk(message.requestId, {
        mode: mode, operation: operation, totals: candidate, rgba: rgba,
        decode_sum_palette_ms: performance.now() - started,
        cached_compressed_bytes: cachedCompressedBytes,
        interval_count: targets.length
      }, [rgba.buffer]);
    }

    self.onmessage = function(event) {
      var message = event.data || {};
      Promise.resolve().then(async function() {
        if (message.type === 'init') {
          cache.clear();
          resultState = null;
          cycleUtc = message.cycle_utc;
          columns = Number(message.columns);
          rows = Number(message.rows);
          scale = Number(message.scale);
          nodata = Number(message.nodata);
          palette = message.palette || [];
          postOk(message.requestId, {initialized: true});
          return;
        }
        if (message.type === 'cache') {
          if (message.cycle_utc !== cycleUtc) throw new Error('Accumulated QPF Worker cache cycle is stale.');
          (message.entries || []).forEach(function(entry) {
            var prior = cache.get(entry.forecast_state_id);
            if (prior && (prior.numeric_path !== entry.numeric_path ||
                prior.numeric_sha256 !== entry.numeric_sha256)) {
              throw new Error('Accumulated QPF Worker compressed identity conflict.');
            }
            cache.set(entry.forecast_state_id, entry);
          });
          postOk(message.requestId, {cached: (message.entries || []).length});
          return;
        }
        if (message.type === 'compute') {
          await compute(message);
          return;
        }
        throw new Error('Accumulated QPF Worker received an unsupported message.');
      }).catch(function(error) { postError(message.requestId, error); });
    };
  }

  function ptNbmAccumWorkerSource() {
    return "'use strict';\n" +
      'var PT_ACCUM_RESULT_NODATA=' + PT_ACCUM_RESULT_NODATA + ';\n' +
      ptNbmAccumAddDecodedGrid.toString() + '\n' +
      ptNbmAccumSubtractDecodedGrid.toString() + '\n' +
      ptNbmAccumPaletteRgba.toString() + '\n(' + ptNbmAccumWorkerMain.toString() + ')();';
  }

  var PtNbmAccumCanvasLayer = L.Layer.extend({
    initialize: function() {
      this._map = null;
      this._canvas = null;
      this._context = null;
      this._resetBound = this._reset.bind(this);
      this._animateBound = this._animateZoom.bind(this);
    },
    onAdd: function(mapObj) {
      this._map = mapObj;
      if (!this._canvas) {
        this._canvas = L.DomUtil.create('canvas', 'leaflet-image-layer leaflet-zoom-animated pt-nbm-accum-canvas');
        this._canvas.width = PT_QPF_NUMERIC_COLUMNS;
        this._canvas.height = PT_QPF_NUMERIC_ROWS;
        this._canvas.style.opacity = String(PT_ACCUM_CANVAS_OPACITY);
        this._canvas.setAttribute('aria-hidden', 'true');
        this._context = this._canvas.getContext('2d', {alpha: true});
      }
      mapObj.getPane('pane_ops_qpf').appendChild(this._canvas);
      mapObj.on('viewreset zoomend moveend resize', this._resetBound);
      if (mapObj.options.zoomAnimation) mapObj.on('zoomanim', this._animateBound);
      this._reset();
    },
    _reset: function() {
      if (!this._map || !this._canvas) return;
      var northWest = this._map.latLngToLayerPoint(L.latLng(44.5, -130));
      var southEast = this._map.latLngToLayerPoint(L.latLng(30, -112));
      L.DomUtil.setPosition(this._canvas, northWest);
      this._canvas.style.width = Math.max(1, southEast.x - northWest.x) + 'px';
      this._canvas.style.height = Math.max(1, southEast.y - northWest.y) + 'px';
    },
    _animateZoom: function(event) {
      if (!this._map || !this._canvas || !this._map._latLngBoundsToNewLayerBounds) return;
      var bounds = L.latLngBounds(PT_QPF_LEAFLET_BOUNDS);
      var scale = this._map.getZoomScale(event.zoom);
      var offset = this._map._latLngBoundsToNewLayerBounds(bounds, event.zoom, event.center).min;
      L.DomUtil.setTransform(this._canvas, offset, scale);
    },
    setPixels: function(rgba) {
      if (!this._context || !(rgba instanceof Uint8ClampedArray) ||
          rgba.length !== PT_QPF_NUMERIC_COLUMNS * PT_QPF_NUMERIC_ROWS * 4) {
        throw new Error('Accumulated QPF canvas received an invalid RGBA grid.');
      }
      var image = this._context.createImageData(PT_QPF_NUMERIC_COLUMNS, PT_QPF_NUMERIC_ROWS);
      image.data.set(rgba);
      this._context.putImageData(image, 0, 0);
    },
    clear: function() {
      if (this._context) this._context.clearRect(0, 0, PT_QPF_NUMERIC_COLUMNS, PT_QPF_NUMERIC_ROWS);
    },
    onRemove: function(mapObj) {
      mapObj.off('viewreset zoomend moveend resize', this._resetBound);
      mapObj.off('zoomanim', this._animateBound);
      if (this._canvas && this._canvas.parentNode) this._canvas.parentNode.removeChild(this._canvas);
      this._map = null;
    }
  });

  function PtNbmAccumConsumer(options) {
    this.options = options || {};
    this.controller = null;
    this.map = null;
    this.active = false;
    this.visible = false;
    this.manifest = null;
    this.cycle = null;
    this.cycleSignature = null;
    this.loader = new PtNbmAccumCompressedLoader(this.options.manifestUrl || '', {});
    this.worker = null;
    this.workerUrl = null;
    this.workerReady = null;
    this.workerRequests = new Map();
    this.workerRequestId = 0;
    this.workerCached = new Set();
    this.canvasLayer = null;
    this.resultFrame = null;
    this.resultIdentity = null;
    this.token = 0;
    this.pendingIdentity = null;
    this.pendingPromise = null;
    this.queuedIdentity = null;
    this.queuedReason = null;
    this.card = null;
    this.section = null;
    this.ui = {};
    this.cardHandlers = [];
    this.density = 'day';
    this.dragFrame = null;
    this.hoverTimer = null;
    this.hoverTooltip = null;
    this.lastLatLng = null;
    this.snowHoverOwned = false;
    this.hoverMove = null;
    this.hoverClose = null;
    this.heapBaseline = null;
    this.diagnostics = {
      loads: [], errors: [], cycleClears: 0, incrementalUpdates: 0,
      fullRecomputes: 0, staleResults: 0, workerStarts: 0
    };
  }

  PtNbmAccumConsumer.prototype.bindController = function(controller) { this.controller = controller; };

  PtNbmAccumConsumer.prototype._listen = function(node, name, handler) {
    if (!node) return;
    node.addEventListener(name, handler);
    this.cardHandlers.push({node: node, name: name, handler: handler});
  };

  PtNbmAccumConsumer.prototype._unwireCard = function() {
    this.cardHandlers.forEach(function(item) {
      try { item.node.removeEventListener(item.name, item.handler); } catch(error) {}
    });
    this.cardHandlers = [];
    if (this.dragFrame && typeof window.cancelAnimationFrame === 'function') {
      window.cancelAnimationFrame(this.dragFrame);
    }
    this.dragFrame = null;
  };

  PtNbmAccumConsumer.prototype.mountCard = function(card) {
    if (!card || this.card === card) return;
    this._unwireCard();
    this.card = card;
    var section = document.createElement('section');
    section.className = 'pt-ops-nbm-product-section pt-ops-nbm-accum-section';
    section.hidden = !this.visible;
    section.innerHTML =
      '<div class="pt-ops-nbm-product-head"><span>NBM Accumulated QPF</span>' +
        '<button type="button" class="pt-ops-nbm-accum-refresh">Recheck feed</button></div>' +
      '<div class="pt-ops-nbm-accum-state pt-ops-nbm-qpf-state" data-kind="loading" role="status" aria-live="polite">Waiting for the exact selected NBM cycle…</div>' +
      '<div class="pt-ops-nbm-accum-range" aria-label="Accumulated QPF forecast window">' +
        '<div class="pt-ops-nbm-accum-endpoint pt-ops-nbm-accum-start-endpoint"></div>' +
        '<div class="pt-ops-nbm-accum-arrow" aria-hidden="true">&#x2192;</div>' +
        '<div class="pt-ops-nbm-accum-endpoint pt-ops-nbm-accum-end-endpoint"></div>' +
      '</div>' +
      '<div class="pt-ops-nbm-accum-slider-shell">' +
        '<div class="pt-ops-nbm-accum-track"><div class="pt-ops-nbm-accum-selection"></div></div>' +
        '<div class="pt-ops-nbm-accum-ticks" aria-hidden="true"></div>' +
        '<input class="pt-ops-nbm-accum-start" type="range" min="0" max="234" step="6" aria-label="Accumulation start time">' +
        '<input class="pt-ops-nbm-accum-end" type="range" min="6" max="240" step="6" aria-label="Accumulation end time">' +
      '</div>' +
      '<div class="pt-ops-nbm-accum-range-meta"><strong class="pt-ops-nbm-accum-duration">240-hour accumulation</strong>' +
        '<span class="pt-ops-nbm-density" role="group" aria-label="Range display density">' +
          '<button type="button" data-density="day" aria-pressed="true">Day</button><span>|</span>' +
          '<button type="button" data-density="6hr" aria-pressed="false">6 hr</button></span></div>' +
      '<div class="pt-ops-nbm-accum-actions">' +
        '<button type="button" data-action="next24">Next 24 h</button>' +
        '<button type="button" data-action="next72">Next 72 h</button>' +
        '<button type="button" data-action="full"><span class="wide">Next 10 days</span><span class="narrow">Next 10 d</span></button></div>' +
      '<div class="pt-ops-nbm-accum-legend"><div class="pt-ops-nbm-qpf-legend-title"><span>Accumulated precipitation (in)</span><span class="pt-ops-nbm-qpf-cap">fixed · 40+ top class</span></div>' +
        '<div class="pt-ops-nbm-accum-ramp" aria-label="Fixed accumulated precipitation color classes"></div>' +
        '<div class="pt-ops-nbm-accum-legend-ticks"><span>0</span><span>0.1</span><span>0.5</span><span>1</span><span>2</span><span>5</span><span>10</span><span>20</span><span>40+</span></div>' +
        '<div class="pt-ops-nbm-qpf-interval">Fixed nonlinear prototype scale · exact hover is not clipped by the 40+ class</div></div>' +
      '<div class="pt-ops-nbm-accum-metrics">No accumulated grid loaded.</div>' +
      '<div class="pt-ops-nbm-hover-note">Duration uses forecast hours; Pacific labels may shift at daylight-saving transitions.</div>';
    var note = card.querySelector('.pt-ops-nbm-snow-note');
    var body = card.querySelector('.pt-ops-nbm-snow-card-body');
    if (note && note.parentNode) note.parentNode.insertBefore(section, note);
    else if (body) body.appendChild(section);
    this.section = section;
    this.ui = {
      state: section.querySelector('.pt-ops-nbm-accum-state'),
      startEndpoint: section.querySelector('.pt-ops-nbm-accum-start-endpoint'),
      endEndpoint: section.querySelector('.pt-ops-nbm-accum-end-endpoint'),
      start: section.querySelector('.pt-ops-nbm-accum-start'),
      end: section.querySelector('.pt-ops-nbm-accum-end'),
      selection: section.querySelector('.pt-ops-nbm-accum-selection'),
      ticks: section.querySelector('.pt-ops-nbm-accum-ticks'),
      duration: section.querySelector('.pt-ops-nbm-accum-duration'),
      refresh: section.querySelector('.pt-ops-nbm-accum-refresh'),
      metrics: section.querySelector('.pt-ops-nbm-accum-metrics'),
      ramp: section.querySelector('.pt-ops-nbm-accum-ramp')
    };
    this.ui.start.value = String(PT_ACCUM_RANGE_STATE.start);
    this.ui.end.value = String(PT_ACCUM_RANGE_STATE.end);
    this.ui.ramp.style.background = 'linear-gradient(to right,' + PT_ACCUM_PALETTE.map(function(item, index) {
      var startPct = index / PT_ACCUM_PALETTE.length * 100;
      var endPct = (index + 1) / PT_ACCUM_PALETTE.length * 100;
      var color = 'rgba(' + item.color.slice(0, 3).join(',') + ',' +
        (item.color[3] / 255).toFixed(3) + ')';
      return color + ' ' + startPct.toFixed(2) + '%,' + color + ' ' + endPct.toFixed(2) + '%';
    }).join(',') + ')';
    this._wireCard();
    this._renderRange();
    this._renderTicks();
  };

  PtNbmAccumConsumer.prototype._wireCard = function() {
    var self = this;
    function scheduleRange() {
      if (self.dragFrame) return;
      var callback = function() { self.dragFrame = null; self._renderRange(); };
      self.dragFrame = typeof window.requestAnimationFrame === 'function'
        ? window.requestAnimationFrame(callback) : window.setTimeout(callback, 16);
    }
    this._listen(this.ui.start, 'input', function() {
      var start = Number(self.ui.start.value);
      var end = Number(self.ui.end.value);
      if (start >= end) self.ui.start.value = String(end - 6);
      PT_ACCUM_RANGE_STATE.start = Number(self.ui.start.value);
      scheduleRange();
    });
    this._listen(this.ui.end, 'input', function() {
      var start = Number(self.ui.start.value);
      var end = Number(self.ui.end.value);
      if (end <= start) self.ui.end.value = String(start + 6);
      PT_ACCUM_RANGE_STATE.end = Number(self.ui.end.value);
      scheduleRange();
    });
    this._listen(this.ui.start, 'change', function() { self._commitRange('start handle'); });
    this._listen(this.ui.end, 'change', function() { self._commitRange('end handle'); });
    this._listen(this.ui.refresh, 'click', function() {
      if (self.controller) self.controller.refreshProduct('accum', 'accumulated QPF card refresh');
    });
    Array.prototype.forEach.call(this.section.querySelectorAll('[data-density]'), function(button) {
      self._listen(button, 'click', function() {
        self.density = button.getAttribute('data-density') === '6hr' ? '6hr' : 'day';
        Array.prototype.forEach.call(self.section.querySelectorAll('[data-density]'), function(other) {
          other.setAttribute('aria-pressed', String(other === button));
        });
        self._renderTicks();
      });
    });
    Array.prototype.forEach.call(this.section.querySelectorAll('[data-action]'), function(button) {
      self._listen(button, 'click', function() {
        var range = ptNbmAccumQuickRange(button.getAttribute('data-action'), PT_ACCUM_RANGE_STATE.start);
        if (!range) return;
        self.setRange(range.start, range.end, 'quick action ' + button.getAttribute('data-action'));
      });
    });
  };

  PtNbmAccumConsumer.prototype._endpointHtml = function(parts) {
    if (!parts) return '<span class="weekday">—</span><span class="clock">Pacific time unavailable</span>';
    return '<span class="weekday">' + escapeHtml(parts.weekday) + '</span>' +
      '<span class="clock">' + escapeHtml(parts.clock) + '</span>' +
      '<span class="date">' + escapeHtml(parts.date) + '</span>' +
      '<span class="lead">' + escapeHtml(parts.lead) + '</span>';
  };

  PtNbmAccumConsumer.prototype._renderRange = function() {
    var range = ptNbmAccumValidateRange(PT_ACCUM_RANGE_STATE.start, PT_ACCUM_RANGE_STATE.end);
    var cycleUtc = this.cycle ? this.cycle.cycle_utc : null;
    var startParts = cycleUtc ? ptNbmAccumEndpointParts(cycleUtc, range.start) : null;
    var endParts = cycleUtc ? ptNbmAccumEndpointParts(cycleUtc, range.end) : null;
    if (this.ui.startEndpoint) this.ui.startEndpoint.innerHTML = this._endpointHtml(startParts);
    if (this.ui.endEndpoint) this.ui.endEndpoint.innerHTML = this._endpointHtml(endParts);
    if (this.ui.duration) this.ui.duration.textContent = range.duration + '-hour accumulation';
    if (this.ui.selection) {
      this.ui.selection.style.left = (range.start / 240 * 100) + '%';
      this.ui.selection.style.right = ((240 - range.end) / 240 * 100) + '%';
    }
    if (this.ui.start) {
      this.ui.start.setAttribute('aria-valuetext', startParts
        ? startParts.compact + ' · ' + startParts.date + ' · ' + startParts.lead : '+' + range.start + ' h');
    }
    if (this.ui.end) {
      this.ui.end.setAttribute('aria-valuetext', endParts
        ? endParts.compact + ' · ' + endParts.date + ' · ' + endParts.lead : '+' + range.end + ' h');
    }
    if (this.section) {
      var next24 = this.section.querySelector('[data-action="next24"]');
      var next72 = this.section.querySelector('[data-action="next72"]');
      if (next24) next24.disabled = range.start + 24 > 240;
      if (next72) next72.disabled = range.start + 72 > 240;
    }
  };

  PtNbmAccumConsumer.prototype._renderTicks = function() {
    if (!this.ui.ticks) return;
    var cycleUtc = this.cycle ? this.cycle.cycle_utc : null;
    var html = [];
    for (var lead = 0; lead <= 240; lead += 6) {
      var major = lead % 24 === 0;
      if (this.density === 'day' && !major) continue;
      var label = '';
      if (major && cycleUtc) label = ptNbmAccumEndpointParts(cycleUtc, lead).weekday;
      else if (major) label = '+' + lead;
      html.push('<span class="' + (major ? 'major' : 'minor') + '" style="left:' +
        (lead / 240 * 100).toFixed(3) + '%">' + (major ? '<b>' + escapeHtml(label) + '</b>' : '') + '</span>');
    }
    this.ui.ticks.className = 'pt-ops-nbm-accum-ticks density-' + this.density;
    this.ui.ticks.innerHTML = html.join('');
  };

  PtNbmAccumConsumer.prototype._setState = function(kind, text) {
    if (!this.ui.state) return;
    this.ui.state.setAttribute('data-kind', kind);
    this.ui.state.textContent = text;
  };

  PtNbmAccumConsumer.prototype.setVisible = function(visible) {
    this.visible = !!visible;
    if (this.section) this.section.hidden = !this.visible;
  };

  PtNbmAccumConsumer.prototype.setManifestLoading = function() {
    if (this.active) this._setState('loading', 'Fetching and validating the public selected-cycle QPF inventory…');
  };

  PtNbmAccumConsumer.prototype.failManifest = function(error) {
    if (!this.active) return;
    this._clearCycleState();
    this._fail(error || new Error('QPF manifest unavailable or invalid.'));
  };

  PtNbmAccumConsumer.prototype.applyManifest = function(manifest, loaded) {
    this.manifest = manifest;
    if (this.ui.metrics && loaded) {
      this.ui.metrics.textContent = 'Manifest ' + ptSnowBytesText(loaded.bytes) + ' · fetch ' +
        loaded.fetchMs.toFixed(1) + ' ms · validate ' + loaded.parseMs.toFixed(1) + ' ms.';
    }
  };

  PtNbmAccumConsumer.prototype.activate = function(mapObj) {
    if (this.active) return;
    this.active = true;
    this.map = mapObj;
    this.setVisible(true);
    this.canvasLayer = this.canvasLayer || new PtNbmAccumCanvasLayer();
    this.canvasLayer.addTo(mapObj);
    this._attachHover();
    if (window.performance && window.performance.memory) {
      this.heapBaseline = Number(window.performance.memory.usedJSHeapSize) || null;
    }
  };

  PtNbmAccumConsumer.prototype._cycleIdentity = function(cycle) {
    return cycle.cycle_utc + '|' + cycle.targets.map(function(target) {
      return target.forecast_state_id + ':' + target.numeric.path + ':' + target.numeric.sha256;
    }).join('|');
  };

  PtNbmAccumConsumer.prototype.selectCycle = function(cycleUtc, cycle, reason) {
    if (!this.active) return Promise.resolve(null);
    try {
      ptNbmAccumValidateCycle(cycle);
      if (cycle.cycle_utc !== cycleUtc) throw new Error('Accumulated QPF selected cycle identity is inconsistent.');
    } catch(error) {
      this._clearCycleState();
      this._fail(error);
      return Promise.resolve(null);
    }
    var signature = this._cycleIdentity(cycle);
    if (this.cycleSignature !== signature) {
      this._clearCycleState();
      this.cycle = cycle;
      this.cycleSignature = signature;
      this.loader.setCycle(cycle.cycle_utc);
    } else {
      this.cycle = cycle;
    }
    this._renderRange();
    this._renderTicks();
    return this._requestCompute(reason || 'shared NBM cycle selection');
  };

  PtNbmAccumConsumer.prototype.setRange = function(startLead, endLead, reason) {
    var range = ptNbmAccumValidateRange(startLead, endLead);
    PT_ACCUM_RANGE_STATE.start = range.start;
    PT_ACCUM_RANGE_STATE.end = range.end;
    if (this.ui.start) this.ui.start.value = String(range.start);
    if (this.ui.end) this.ui.end.value = String(range.end);
    this._renderRange();
    return this._requestCompute(reason || 'accumulation range selection');
  };

  PtNbmAccumConsumer.prototype._requestCompute = function(reason) {
    if (!this.active || !this.cycle) return Promise.resolve(null);
    var identity = this.cycle.cycle_utc + '|' + PT_ACCUM_RANGE_STATE.start + '|' + PT_ACCUM_RANGE_STATE.end;
    if (this.resultIdentity === identity && this.resultFrame) {
      this._setState('paired', 'Exact selected-cycle accumulation displayed atomically · ' +
        (PT_ACCUM_RANGE_STATE.end - PT_ACCUM_RANGE_STATE.start) + ' forecast hours.');
      setOpsLayerLoading(PT_ACCUM_PRODUCT_NAME, false);
      return Promise.resolve(this.resultFrame);
    }
    if (this.pendingIdentity === identity && this.pendingPromise) {
      if (this.queuedIdentity) {
        this.queuedIdentity = identity;
        this.queuedReason = reason;
      }
      return this.pendingPromise;
    }
    if (this.pendingPromise) {
      this.token += 1;
      this.queuedIdentity = identity;
      this.queuedReason = reason;
      this._clearDisplayed();
      this._setState('loading', 'Latest forecast window queued; finishing the bounded active request…');
      return this.pendingPromise;
    }
    var self = this;
    var pending = this._loadAndCompute(reason);
    this.pendingIdentity = identity;
    this.pendingPromise = pending;
    pending.finally(function() {
      if (self.pendingPromise === pending) {
        self.pendingIdentity = null;
        self.pendingPromise = null;
        var queuedIdentity = self.queuedIdentity;
        var queuedReason = self.queuedReason;
        self.queuedIdentity = null;
        self.queuedReason = null;
        var currentIdentity = self.cycle
          ? self.cycle.cycle_utc + '|' + PT_ACCUM_RANGE_STATE.start + '|' + PT_ACCUM_RANGE_STATE.end
          : null;
        if (queuedIdentity && queuedIdentity === currentIdentity && self.active) {
          self._requestCompute(queuedReason || 'coalesced forecast-window update');
        }
      }
    });
    return pending;
  };

  PtNbmAccumConsumer.prototype._commitRange = function(reason) {
    return this.setRange(Number(this.ui.start.value), Number(this.ui.end.value), reason);
  };

  PtNbmAccumConsumer.prototype._clearDisplayed = function() {
    this.resultFrame = null;
    this.resultIdentity = null;
    if (this.canvasLayer) this.canvasLayer.clear();
    this._closeHover();
    var container = this.map && this.map.getContainer ? this.map.getContainer() : null;
    if (container) container.classList.remove('pt-ops-nbm-accum-hover-on');
  };

  PtNbmAccumConsumer.prototype._terminateWorker = function() {
    this.workerRequests.forEach(function(request) {
      request.reject(new Error('Accumulated QPF Worker was cleared.'));
    });
    this.workerRequests.clear();
    if (this.worker) {
      try { this.worker.terminate(); } catch(error) {}
    }
    this.worker = null;
    this.workerReady = null;
    this.workerCached.clear();
    if (this.workerUrl && window.URL && typeof window.URL.revokeObjectURL === 'function') {
      try { window.URL.revokeObjectURL(this.workerUrl); } catch(error2) {}
    }
    this.workerUrl = null;
  };

  PtNbmAccumConsumer.prototype._clearCycleState = function() {
    this.token += 1;
    this.loader.clear();
    this._terminateWorker();
    this._clearDisplayed();
    this.pendingIdentity = null;
    this.pendingPromise = null;
    this.queuedIdentity = null;
    this.queuedReason = null;
    this.cycle = null;
    this.cycleSignature = null;
    this.diagnostics.cycleClears += 1;
  };

  PtNbmAccumConsumer.prototype._workerRequest = function(type, payload, transfers) {
    var self = this;
    var requestId = ++this.workerRequestId;
    return new Promise(function(resolve, reject) {
      self.workerRequests.set(requestId, {resolve: resolve, reject: reject});
      self.worker.postMessage(Object.assign({type: type, requestId: requestId}, payload || {}), transfers || []);
    });
  };

  PtNbmAccumConsumer.prototype._ensureWorker = function() {
    if (this.workerReady) return this.workerReady;
    if (!this.cycle || typeof window.Worker !== 'function' || typeof window.DecompressionStream !== 'function') {
      return Promise.reject(new Error('This browser cannot run the required accumulated-QPF decode Worker.'));
    }
    var self = this;
    this.workerUrl = window.URL.createObjectURL(new Blob([ptNbmAccumWorkerSource()], {type: 'text/javascript'}));
    this.worker = new window.Worker(this.workerUrl);
    this.diagnostics.workerStarts += 1;
    this.worker.onmessage = function(event) {
      var message = event.data || {};
      var pending = self.workerRequests.get(message.requestId);
      if (!pending) return;
      self.workerRequests.delete(message.requestId);
      if (message.ok) pending.resolve(message);
      else pending.reject(new Error(message.error || 'Accumulated QPF Worker failed.'));
    };
    this.worker.onerror = function(event) {
      var error = new Error(event && event.message ? event.message : 'Accumulated QPF Worker crashed.');
      self.workerRequests.forEach(function(pending) { pending.reject(error); });
      self.workerRequests.clear();
    };
    this.workerReady = this._workerRequest('init', {
      cycle_utc: this.cycle.cycle_utc,
      columns: PT_QPF_NUMERIC_COLUMNS, rows: PT_QPF_NUMERIC_ROWS,
      scale: PT_QPF_NUMERIC_SCALE, nodata: PT_QPF_NUMERIC_NODATA,
      palette: PT_ACCUM_PALETTE
    });
    return this.workerReady;
  };

  PtNbmAccumConsumer.prototype._cacheInWorker = async function(loaded) {
    var entries = [];
    var transfers = [];
    loaded.forEach(function(item) {
      if (this.workerCached.has(item.entry.forecast_state_id)) return;
      var copy = item.buffer.slice(0);
      entries.push({
        cycle_utc: item.entry.cycle_utc,
        forecast_state_id: item.entry.forecast_state_id,
        lead_hours: item.entry.lead_hours,
        numeric_path: item.entry.numeric.path,
        numeric_sha256: item.entry.numeric.sha256,
        uncompressed_bytes: item.entry.numeric.uncompressed_bytes,
        buffer: copy
      });
      transfers.push(copy);
    }, this);
    if (!entries.length) return;
    await this._workerRequest('cache', {cycle_utc: this.cycle.cycle_utc, entries: entries}, transfers);
    entries.forEach(function(entry) { this.workerCached.add(entry.forecast_state_id); }, this);
  };

  PtNbmAccumConsumer.prototype._loadAndCompute = async function(reason) {
    if (!this.active || !this.cycle) return null;
    var self = this;
    var range = ptNbmAccumValidateRange(PT_ACCUM_RANGE_STATE.start, PT_ACCUM_RANGE_STATE.end);
    var targets;
    try { targets = ptNbmAccumRequiredTargets(this.cycle, range.start, range.end); }
    catch(error) { this._fail(error); return null; }
    var token = ++this.token;
    var cycleUtc = this.cycle.cycle_utc;
    var started = ptSnowPerfNow();
    var beforeMetrics = Object.assign({}, this.loader.metrics);
    this._clearDisplayed();
    this._setState('loading', 'Loading 0 / ' + targets.length + ' exact six-hour intervals…');
    setOpsLayerLoading(PT_ACCUM_PRODUCT_NAME, true);
    try {
      var loaded = await this.loader.loadMany(targets, function(done, total) {
        if (token === self.token) self._setState('loading', 'Loading ' + done + ' / ' + total +
          ' verified six-hour intervals…');
      }, PT_ACCUM_FETCH_CONCURRENCY);
      if (!this.active || token !== this.token || !this.cycle || this.cycle.cycle_utc !== cycleUtc) {
        this.diagnostics.staleResults += 1;
        return null;
      }
      await this._ensureWorker();
      await this._cacheInWorker(loaded);
      var computeTargets = targets.map(function(target) {
        return {lead_hours: target.lead_hours, forecast_state_id: target.forecast_state_id,
          numeric_path: target.numeric.path, numeric_sha256: target.numeric.sha256};
      });
      var computed = await this._workerRequest('compute', {
        cycle_utc: cycleUtc, start: range.start, end: range.end, targets: computeTargets
      });
      if (!this.active || token !== this.token || !this.cycle || this.cycle.cycle_utc !== cycleUtc ||
          PT_ACCUM_RANGE_STATE.start !== range.start || PT_ACCUM_RANGE_STATE.end !== range.end) {
        this.diagnostics.staleResults += 1;
        return null;
      }
      var totals = computed.totals instanceof Uint32Array
        ? computed.totals : new Uint32Array(computed.totals);
      var rgba = computed.rgba instanceof Uint8ClampedArray
        ? computed.rgba : new Uint8ClampedArray(computed.rgba);
      this.resultFrame = {
        cycle_utc: cycleUtc, start_lead_hours: range.start, end_lead_hours: range.end,
        columns: PT_QPF_NUMERIC_COLUMNS, rows: PT_QPF_NUMERIC_ROWS,
        extent_m: PT_QPF_NUMERIC_EXTENT.slice(), scale: PT_QPF_NUMERIC_SCALE,
        offset: 0, nodata: PT_ACCUM_RESULT_NODATA, values: totals
      };
      this.resultIdentity = cycleUtc + '|' + range.start + '|' + range.end;
      this.canvasLayer.setPixels(rgba);
      var container = this.map && this.map.getContainer ? this.map.getContainer() : null;
      if (container) container.classList.add('pt-ops-nbm-accum-hover-on');
      var elapsed = ptSnowPerfNow() - started;
      var transferred = this.loader.metrics.transferredBytes - beforeMetrics.transferredBytes;
      var requests = this.loader.metrics.requests - beforeMetrics.requests;
      var fetchMs = this.loader.metrics.fetchMs - beforeMetrics.fetchMs;
      var hashMs = this.loader.metrics.hashMs - beforeMetrics.hashMs;
      var resultBytes = totals.byteLength;
      var memoryEstimate = this.loader.compressedBytes() * 2 + resultBytes * 2 + rgba.byteLength +
        PT_QPF_NUMERIC_UNCOMPRESSED_BYTES;
      var heapDelta = null;
      if (this.heapBaseline !== null && window.performance && window.performance.memory) {
        heapDelta = Number(window.performance.memory.usedJSHeapSize) - this.heapBaseline;
      }
      var metric = {
        reason: reason, cycle_utc: cycleUtc, start: range.start, end: range.end,
        interval_count: targets.length, request_count: requests, transferred_bytes: transferred,
        wall_ms: elapsed, fetch_ms: fetchMs, hash_ms: hashMs,
        decode_sum_palette_ms: computed.decode_sum_palette_ms,
        mode: computed.mode, operation: computed.operation || null,
        compressed_cache_bytes: this.loader.compressedBytes(),
        working_memory_estimate_bytes: memoryEstimate, heap_delta_bytes: heapDelta
      };
      this.diagnostics.loads.push(metric);
      if (this.diagnostics.loads.length > 100) this.diagnostics.loads.shift();
      if (computed.mode === 'incremental') this.diagnostics.incrementalUpdates += 1;
      else this.diagnostics.fullRecomputes += 1;
      if (this.ui.metrics) {
        this.ui.metrics.textContent = (computed.mode === 'incremental' ? 'Incremental exact update' : 'Exact full recompute') +
          ' · ' + targets.length + ' intervals · ' + requests + ' requests / ' + ptSnowBytesText(transferred) +
          ' · total ' + elapsed.toFixed(1) + ' ms · fetch ' + fetchMs.toFixed(1) +
          ' ms · SHA ' + hashMs.toFixed(1) + ' ms · decode/sum/palette ' +
          Number(computed.decode_sum_palette_ms).toFixed(1) + ' ms · compressed cache ' +
          ptSnowBytesText(this.loader.compressedBytes()) + ' · working estimate ' +
          ptSnowBytesText(memoryEstimate) + '.';
      }
      this._setState('paired', 'Exact selected-cycle accumulation displayed atomically · ' +
        range.duration + ' forecast hours.');
      setOpsLayerLoading(PT_ACCUM_PRODUCT_NAME, false);
      recordStatus(PT_ACCUM_PRODUCT_NAME, 'Exact +' + range.start + ' to +' + range.end +
        ' h accumulation displayed.', 'pt-ops-ok');
      return this.resultFrame;
    } catch(error) {
      if (!this.active || token !== this.token || (error && error.name === 'AbortError')) return null;
      this._fail(error);
      return null;
    }
  };

  PtNbmAccumConsumer.prototype._recordError = function(error) {
    this.diagnostics.errors.push({time_utc: new Date().toISOString(),
      message: String(error && error.message ? error.message : error)});
    if (this.diagnostics.errors.length > 100) this.diagnostics.errors.shift();
  };

  PtNbmAccumConsumer.prototype._fail = function(error) {
    this.token += 1;
    this._recordError(error);
    this._clearDisplayed();
    this._setState('error', (error && error.message ? error.message : String(error)) +
      ' No accumulated precipitation surface is displayed.');
    setOpsLayerLoading(PT_ACCUM_PRODUCT_NAME, false);
    recordStatus(PT_ACCUM_PRODUCT_NAME, 'Accumulation failed closed; no partial surface is displayed.', 'pt-ops-bad');
  };

  PtNbmAccumConsumer.prototype._attachHover = function() {
    if (!this.map || this.hoverMove) return;
    var self = this;
    this.hoverMove = function(event) {
      if (!event || !event.latlng) return;
      self.lastLatLng = event.latlng;
      if (self.hoverTimer) window.clearTimeout(self.hoverTimer);
      self.hoverTimer = window.setTimeout(function() {
        self.hoverTimer = null;
        self._renderHover(self.lastLatLng);
      }, 35);
    };
    this.hoverClose = function() { self._closeHover(); };
    this.map.on('mousemove', this.hoverMove);
    this.map.on('mouseout zoomstart movestart', this.hoverClose);
  };

  PtNbmAccumConsumer.prototype._renderHover = function(latlng) {
    if (!this.active || this.snowHoverOwned || !this.resultFrame || !this.resultIdentity || !latlng) {
      this._closeHover();
      return;
    }
    var expected = this.resultFrame.cycle_utc + '|' + this.resultFrame.start_lead_hours + '|' +
      this.resultFrame.end_lead_hours;
    if (expected !== this.resultIdentity) { this._closeHover(); return; }
    var sample = ptQpfNumericValueAt(this.resultFrame, latlng);
    if (!sample) { this._closeHover(); return; }
    var start = ptNbmAccumEndpointParts(this.resultFrame.cycle_utc, this.resultFrame.start_lead_hours);
    var end = ptNbmAccumEndpointParts(this.resultFrame.cycle_utc, this.resultFrame.end_lead_hours);
    var duration = this.resultFrame.end_lead_hours - this.resultFrame.start_lead_hours;
    var html = '<div class="pt-ops-nbm-qpf-hover-card">' +
      '<div class="pt-ops-nbm-qpf-hover-title">NBM Accumulated QPF</div>' +
      '<div class="pt-ops-nbm-qpf-hover-summary">' + escapeHtml(sample.value_in.toFixed(3)) +
        ' in <span aria-hidden="true">·</span> ' + duration + ' hr</div>' +
      '<div class="pt-ops-nbm-qpf-hover-local">' + escapeHtml(start.hover) + ' &#x2192; ' +
        escapeHtml(end.hover) + '</div>' +
      '<div class="pt-ops-nbm-qpf-hover-utc">(' + escapeHtml(start.utc_compact) + ' &#x2192; ' +
        escapeHtml(end.utc_compact) + ')</div>' +
      '<div class="pt-ops-nbm-qpf-hover-meta">exact grid cell</div></div>';
    if (!this.hoverTooltip) {
      this.hoverTooltip = L.tooltip({permanent: true, direction: 'top', offset: [8, -10],
        opacity: 0.97, interactive: false, className: 'pt-ops-nbm-qpf-hover-tooltip'});
    }
    this.hoverTooltip.setLatLng(latlng).setContent(html).addTo(this.map);
  };

  PtNbmAccumConsumer.prototype._closeHover = function() {
    if (this.hoverTooltip && this.map) {
      try { this.map.removeLayer(this.hoverTooltip); } catch(error) {}
    }
    this.hoverTooltip = null;
  };

  PtNbmAccumConsumer.prototype.setSnowHoverOwned = function(owned) {
    this.snowHoverOwned = !!owned;
    if (this.snowHoverOwned) this._closeHover();
    else if (this.lastLatLng) this._renderHover(this.lastLatLng);
  };

  PtNbmAccumConsumer.prototype._detachHover = function() {
    if (this.hoverTimer) window.clearTimeout(this.hoverTimer);
    this.hoverTimer = null;
    if (this.map && this.hoverMove) this.map.off('mousemove', this.hoverMove);
    if (this.map && this.hoverClose) this.map.off('mouseout zoomstart movestart', this.hoverClose);
    this.hoverMove = null;
    this.hoverClose = null;
    this.lastLatLng = null;
    this._closeHover();
  };

  PtNbmAccumConsumer.prototype.deactivate = function(mapObj) {
    if (!this.active) { this.setVisible(false); return; }
    this.active = false;
    this.setVisible(false);
    this._clearCycleState();
    this.loader.clear();
    this.manifest = null;
    this._detachHover();
    var container = this.map && this.map.getContainer ? this.map.getContainer() : null;
    if (container) container.classList.remove('pt-ops-nbm-accum-hover-on');
    if (this.canvasLayer && (mapObj || this.map)) {
      try { (mapObj || this.map).removeLayer(this.canvasLayer); } catch(error) {}
    }
    this.canvasLayer = null;
    this.map = null;
  };

  PtNbmAccumConsumer.prototype.destroy = function() {
    this.deactivate(this.map);
    this._unwireCard();
    this.card = null;
    this.section = null;
    this.ui = {};
  };

  PtNbmAccumConsumer.prototype.getState = function() {
    return {
      active: this.active,
      cycle_utc: this.cycle ? this.cycle.cycle_utc : null,
      start_lead_hours: PT_ACCUM_RANGE_STATE.start,
      end_lead_hours: PT_ACCUM_RANGE_STATE.end,
      duration_hours: PT_ACCUM_RANGE_STATE.end - PT_ACCUM_RANGE_STATE.start,
      result_ready: !!this.resultFrame,
      result_identity: this.resultIdentity,
      compressed_cache_entries: this.loader.cache.size,
      compressed_cache_bytes: this.loader.compressedBytes(),
      inflight_requests: this.loader.inflight.size,
      diagnostics: this.diagnostics
    };
  };

  function ptNbmAccumInstallCss() {
    if (document.getElementById('pt-ops-nbm-accum-style')) return;
    var style = document.createElement('style');
    style.id = 'pt-ops-nbm-accum-style';
    style.textContent = `
      .pt-ops-nbm-accum-range{display:grid;grid-template-columns:minmax(0,1fr) 20px minmax(0,1fr);align-items:start;gap:4px;margin-top:7px;padding:7px 5px 4px;border:1px solid rgba(54,84,86,.20);border-radius:5px;background:rgba(255,255,255,.66);font-variant-numeric:tabular-nums}.pt-ops-nbm-accum-endpoint{display:grid;grid-template-columns:auto auto;grid-template-areas:"weekday weekday" "clock clock" "date lead";column-gap:5px;min-width:0;text-align:center}.pt-ops-nbm-accum-endpoint .weekday{grid-area:weekday;color:#2a4954;font-size:16px;font-weight:800;letter-spacing:.055em}.pt-ops-nbm-accum-endpoint .clock{grid-area:clock;color:#1f3036;font-size:16px;font-weight:750;white-space:nowrap}.pt-ops-nbm-accum-endpoint .date{grid-area:date;color:#5d6c71;font-size:10px;text-align:right;white-space:nowrap}.pt-ops-nbm-accum-endpoint .lead{grid-area:lead;color:#68777c;font-size:9.5px;text-align:left;white-space:nowrap}.pt-ops-nbm-accum-arrow{padding-top:20px;color:#4e666e;font-size:18px;text-align:center}
      .pt-ops-nbm-accum-slider-shell{position:relative;height:46px;margin:5px 9px 0}.pt-ops-nbm-accum-track{position:absolute;left:0;right:0;top:11px;height:5px;border-radius:4px;background:#cbd5d8}.pt-ops-nbm-accum-selection{position:absolute;top:0;bottom:0;border-radius:4px;background:#287989}.pt-ops-nbm-accum-slider-shell input[type=range]{position:absolute;left:0;top:1px;width:100%;height:24px;margin:0;padding:0;background:transparent;pointer-events:none;-webkit-appearance:none;appearance:none}.pt-ops-nbm-accum-slider-shell input[type=range]::-webkit-slider-runnable-track{height:5px;background:transparent}.pt-ops-nbm-accum-slider-shell input[type=range]::-webkit-slider-thumb{width:18px;height:18px;margin-top:-7px;border:2px solid #fff;border-radius:50%;background:#1f6575;box-shadow:0 0 0 1px #294d56,0 1px 3px rgba(0,0,0,.35);pointer-events:auto;-webkit-appearance:none}.pt-ops-nbm-accum-slider-shell input[type=range]::-moz-range-track{height:5px;background:transparent}.pt-ops-nbm-accum-slider-shell input[type=range]::-moz-range-thumb{width:16px;height:16px;border:2px solid #fff;border-radius:50%;background:#1f6575;box-shadow:0 0 0 1px #294d56;pointer-events:auto}.pt-ops-nbm-accum-ticks{position:absolute;left:0;right:0;top:25px;height:19px}.pt-ops-nbm-accum-ticks span{position:absolute;top:0;width:1px;height:5px;background:#809197}.pt-ops-nbm-accum-ticks span b{position:absolute;top:6px;left:0;transform:translateX(-50%);color:#68777c;font-size:7.5px;font-weight:700;white-space:nowrap}.pt-ops-nbm-accum-ticks span:first-child b{transform:none}.pt-ops-nbm-accum-ticks span:last-child b{transform:translateX(-100%)}.pt-ops-nbm-accum-ticks.density-6hr span.minor{height:3px;background:#a7b4b8}
      .pt-ops-nbm-accum-range-meta{display:flex;align-items:center;justify-content:space-between;gap:8px;margin-top:1px}.pt-ops-nbm-accum-duration{font-size:11px}.pt-ops-nbm-density{display:inline-flex;align-items:center;gap:3px;color:#78878b;font-size:10px}.pt-ops-nbm-density button{margin:0!important;padding:1px 3px!important;border:0!important;background:transparent!important;color:#476169!important;font-size:10px!important;box-shadow:none!important}.pt-ops-nbm-density button[aria-pressed=true]{color:#173f4a!important;font-weight:800!important;text-decoration:underline}.pt-ops-nbm-accum-actions{display:grid;grid-template-columns:1fr 1fr 1.25fr;gap:4px;margin-top:6px}.pt-ops-nbm-accum-actions button{min-width:0;margin:0!important;padding:4px 3px!important;font-size:9.5px!important}.pt-ops-nbm-accum-actions .narrow{display:none}.pt-ops-nbm-accum-legend{margin-top:7px;padding:7px 8px;border:1px solid rgba(54,84,86,.22);border-radius:5px;background:rgba(255,255,255,.62)}.pt-ops-nbm-accum-ramp{height:13px;border:1px solid rgba(42,57,63,.46);border-radius:2px}.pt-ops-nbm-accum-legend-ticks{display:grid;grid-template-columns:repeat(9,1fr);margin-top:2px;color:#4d5d62;font-size:8px}.pt-ops-nbm-accum-legend-ticks span{text-align:center}.pt-ops-nbm-accum-legend-ticks span:first-child{text-align:left}.pt-ops-nbm-accum-legend-ticks span:last-child{text-align:right}.pt-ops-nbm-accum-metrics{margin-top:5px;color:#5d6c71;font-size:9px;overflow-wrap:anywhere}.leaflet-container.pt-ops-nbm-accum-hover-on{cursor:crosshair}.pt-nbm-accum-canvas{image-rendering:auto;transform-origin:0 0}
      @media(max-width:430px){.pt-ops-nbm-accum-range{grid-template-columns:minmax(0,1fr) 16px minmax(0,1fr);padding-left:2px;padding-right:2px}.pt-ops-nbm-accum-endpoint .weekday,.pt-ops-nbm-accum-endpoint .clock{font-size:14px}.pt-ops-nbm-accum-endpoint .date{font-size:9px}.pt-ops-nbm-accum-endpoint .lead{font-size:8.5px}.pt-ops-nbm-accum-actions .wide{display:none}.pt-ops-nbm-accum-actions .narrow{display:inline}.pt-ops-nbm-accum-ticks span b{font-size:7px}}
    `;
    document.head.appendChild(style);
  }

  ptNbmAccumInstallCss();

  var ptNbmAccumConsumer = null;
  if (ptNbmForecastController && includeNbmAccumQpf && NBM_QPF_MANIFEST_URL) {
    ptNbmAccumConsumer = new PtNbmAccumConsumer({manifestUrl: NBM_QPF_MANIFEST_URL});
    ptNbmForecastController.setAccumulatedQpfConsumer(ptNbmAccumConsumer);
    addOpsLayer({
      category: 'Forecasts / Outlooks',
      subgroup: 'Weather Forecasts / Outlooks',
      name: PT_ACCUM_PRODUCT_NAME,
      panelOrder: -93,
      sourceUrl: 'https://vlab.noaa.gov/web/mdl/nbm-weather-elements',
      refreshable: true,
      helperText: 'NOAA/NBM precipitation total for a selected forecast window.',
      layer: new PtOpsNbmProductLayer(ptNbmForecastController, 'accum')
    });
  }

  )---"
}
