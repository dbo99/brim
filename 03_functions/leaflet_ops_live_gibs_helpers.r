# Focused GIBS imagery family: qualified provider binding and satellite-only selection ownership.
# Loaded only by leaflet_ops_live_helpers.r. No request at construction/render time.
pt_ops_live_gibs_js <- function() {
  r"---(
  var ptGibs = (function() {
  'use strict';
  const WMTS = 'http://www.opengis.net/wmts/1.0';
  const OWS = 'http://www.opengis.net/ows/1.1';
  const ROOT = 'https://gibs.earthdata.nasa.gov/wmts/epsg3857/best/';
  const CAPABILITIES = ROOT + '1.0.0/WMTSCapabilities.xml';
  const MATRIX_LINK = 'https://www.star.nesdis.noaa.gov/GOES/sector.php?sat=G18&sector=psw';
  const DAY = 86400000;
  const PRODUCTS = Object.freeze([
    ['GOES-West_ABI_GeoColor', 'GOES-West GeoColor', 'instant', 7, 'png', 'ops_goes_geocolor', 'https://www.star.nesdis.noaa.gov/GOES/documents/QuickGuide_CIRA_Geocolor_20171019.pdf', 'Geostationary', 'GOES-18', 'ABI — GeoColor'],
    ['GOES-West_ABI_Band13_Clean_Infrared', 'GOES-West Clean IR (Band 13)', 'instant', 6, 'png', 'ops_goes_infrared', 'https://www.star.nesdis.noaa.gov/GOES/documents/ABIQuickGuide_Band13.pdf', 'Geostationary', 'GOES-18', 'ABI — Band 13 Clean Infrared'],
    ['VIIRS_NOAA20_CorrectedReflectance_TrueColor', 'VIIRS NOAA-20 True Color', 'daily', 9, 'jpg', 'ops_viirs_noaa20_true_color', 'https://www.earthdata.nasa.gov/data/instruments/viirs/land-near-real-time-data', 'Polar-orbiting', 'NOAA-20', 'VIIRS — Corrected Reflectance True Color'],
    ['VIIRS_NOAA21_CorrectedReflectance_TrueColor', 'VIIRS NOAA-21 True Color', 'daily', 9, 'jpg', 'ops_viirs_noaa21_true_color', 'https://www.earthdata.nasa.gov/data/instruments/viirs/land-near-real-time-data', 'Polar-orbiting', 'NOAA-21', 'VIIRS — Corrected Reflectance True Color'],
    ['MODIS_Terra_CorrectedReflectance_TrueColor', 'MODIS Terra True Color', 'daily', 9, 'jpg', 'ops_modis_terra_true_color', 'https://www.earthdata.nasa.gov/data/instruments/modis/near-real-time-data', 'Polar-orbiting', 'Terra', 'MODIS — Corrected Reflectance True Color']
  ].map(([id, label, kind, zoom, ext, stableId, infoUrl, orbit, satellite, instrumentProduct], order) => Object.freeze({id, label, kind, zoom, ext, stableId, infoUrl, order, orbit, satellite, instrumentProduct,
    sourceUrl:'https://gibs.earthdata.nasa.gov/layer-metadata/v1.0/' + id + '.json',
    matrix:'GoogleMapsCompatible_Level' + zoom, mime:ext === 'png' ? 'image/png' : 'image/jpeg'})));
  const fail = message => { throw new Error(message); };
  function dateValue(value, kind) {
    const re = kind === 'daily' ? /^\d{4}-\d{2}-\d{2}$/ : /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z$/;
    if (!re.test(value)) return fail('Unsupported ' + kind + ' date: ' + value);
    const full = kind === 'daily' ? value + 'T00:00:00Z' : value;
    const n = Date.parse(full);
    if (!Number.isFinite(n) || new Date(n).toISOString().replace('.000Z', 'Z') !== full)
      return fail('Malformed date: ' + value);
    return n;
  }
  function duration(value, kind) {
    const m = /^P(?:(\d+)D)?(?:T(?:(\d+)H)?(?:(\d+)M)?(?:(\d+)S)?)?$/.exec(value);
    if (!m || !m.slice(1).some(Boolean) || value.endsWith('T')) return fail('Unsupported period: ' + value);
    const n = (+m[1] || 0) * DAY + (+m[2] || 0) * 3600000 + (+m[3] || 0) * 60000 + (+m[4] || 0) * 1000;
    if (!Number.isSafeInteger(n) || n <= 0 || (kind === 'daily' && !/^P[1-9]\d*D$/.test(value)))
      return fail('Unsupported period: ' + value);
    return n;
  }
  function domain(values, kind) {
    if (!values.length) return fail('Missing advertised time values');
    const tokens = values.flatMap(v => v.split(',').map(x => x.trim()));
    if (tokens.length > 20000) return fail('Time domain exceeds qualification limit');
    return tokens.map(token => {
      const parts = token.split('/');
      if (parts.length === 1) { const n = dateValue(token, kind); return {start:n, end:n, step:0}; }
      if (parts.length !== 3) return fail('Unsupported time domain: ' + token);
      const start = dateValue(parts[0], kind), end = dateValue(parts[1], kind), step = duration(parts[2], kind);
      if (start > end || (end - start) % step !== 0) return fail('Invalid or unaligned interval: ' + token);
      return {start, end, step};
    });
  }
  const advertised = (ranges, n) => ranges.some(r => n >= r.start && n <= r.end && (r.step === 0 ? n === r.start : (n - r.start) % r.step === 0));
  function latestInstant(ranges, now) {
    // Values are selected only from provider-declared grids, anchored at their start.
    // No guessed cadence, clock-minus-cadence, historical default or pixel inference.
    const candidates = ranges.filter(r => r.start <= now).map(r => r.step ?
      r.start + Math.floor((Math.min(now, r.end) - r.start) / r.step) * r.step : r.start);
    return candidates.length ? Math.max(...candidates) : null;
  }
  const utcDay = n => new Date(n).toISOString().slice(0, 10);
  function dailyWindow(ranges, now) {
    const today = dateValue(utcDay(now), 'daily');
    return Array.from({length:8}, (_, i) => today - i * DAY).filter(n => advertised(ranges, n)).map(utcDay);
  }
  const kids = (node, name, ns) => Array.from(node.childNodes || []).filter(n => n.nodeType === 1 && n.localName === name && n.namespaceURI === ns);
  function one(node, name, ns) {
    const a = kids(node, name, ns); if (a.length !== 1) return fail('Missing/duplicate ' + name); return a[0];
  }
  const txt = node => (node.textContent || '').trim();
  const childText = (node, name, ns = WMTS) => txt(one(node, name, ns));
  const identifier = node => childText(node, 'Identifier', OWS);
  function coverageLimits(link, product) {
    const containers = kids(link, 'TileMatrixSetLimits', WMTS);
    if (!containers.length) {
      if (product.kind === 'instant') return fail('New GOES coverage structure: limits absent; review required');
      return {status:'NOT_ADVERTISED', records:[], issues:[], warning:'Coverage not established by tile transport', coverageProven:false};
    }
    if (product.kind !== 'instant') return fail('Unexpected daily coverage limits require review');
    if (containers.length !== 1) return fail('Multiple coverage-limit containers require review');
    const container = containers[0], fields = ['TileMatrix','MinTileRow','MaxTileRow','MinTileCol','MaxTileCol'];
    if (Array.from(container.childNodes || []).some(n => n.nodeType === 1 && (n.namespaceURI !== WMTS || n.localName !== 'TileMatrixLimits')))
      return fail('Unexpected coverage-limit element/namespace');
    const records = kids(container, 'TileMatrixLimits', WMTS).map(node => {
      if (Array.from(node.childNodes || []).some(n => n.nodeType === 1 && (n.namespaceURI !== WMTS || !fields.includes(n.localName))))
        return fail('Unexpected limit field/namespace');
      const raw = fields.map(f => childText(node, f));
      if (!raw.every(v => /^(0|[1-9]\d*)$/.test(v) && Number.isSafeInteger(+v))) return fail('Noncanonical limit value');
      return raw.map(Number);
    });
    const valid = records.length === product.zoom + 1 &&
      Array.from({length:product.zoom + 1}, (_, z) => z).every(z => records.filter(r => r[0] === z).length === 1) &&
      records.every(([z,a,b,c,d]) => a <= b && c <= d && b < 2**z && d < 2**z);
    if (valid) return {status:'DECLARED_LIMITS_VALID', records, issues:[], warning:'Declared limits respected; useful pixel coverage Unverified', coverageProven:false};
    // Exact authenticated anomaly signatures, NOT footprints or substitute tile bounds.
    // Any changed unresolved structure requires review. Complete valid limits above
    // take precedence. Never wrap columns, merge fragments or copy a regional mask.
    const common = '0,0,0,0,1;1,0,1,0,0;1,0,1,2,2;2,0,2,0,1;2,0,2,4,4;3,0,4,0,3;3,0,4,8,9;4,0,9,0,6;4,0,9,17,19;5,0,18,0,13;5,0,18,35,39';
    const expected = product.id === 'GOES-West_ABI_GeoColor' ? common + ';6,2,38,0,27;6,2,38,71,79' :
      product.id === 'GOES-West_ABI_Band13_Clean_Infrared' ? common : null;
    if (!expected || records.map(r => r.join(',')).join(';') !== expected) return fail('New unresolved GOES coverage structure; review required');
    return {status:'KNOWN_INCONSISTENT_GOES_LIMITS', records,
      issues:['Repeated matrix levels 1–' + (product.zoom - 1), 'Missing native level ' + product.zoom, 'Out-of-grid columns retained without normalization'],
      warning:'Precise provider coverage UNKNOWN; selected viewport uses the valid core grid. Blank areas may remain.', coverageProven:false};
  }
  function tileAllowed(product, binding, coords) {
    const {x,y,z} = coords;
    if (!binding || !binding.ok || ![x,y,z].every(Number.isInteger) || z < 0 || z > product.zoom || x < 0 || y < 0 || x >= 2**z || y >= 2**z) return false;
    const limits = binding.coverageLimits;
    if (limits.status === 'DECLARED_LIMITS_VALID') {
      const r = limits.records.find(row => row[0] === z);
      return !!r && y >= r[1] && y <= r[2] && x >= r[3] && x <= r[4];
    }
    return limits.status === 'KNOWN_INCONSISTENT_GOES_LIMITS' || (product.kind === 'daily' && limits.status === 'NOT_ADVERTISED');
  }
  function parseCapabilities(xml, parseXML) {
    if (typeof xml !== 'string' || xml.length > 10*1024*1024 || /<!DOCTYPE|<!ENTITY/i.test(xml)) return fail('Unsupported XML declarations');
    const doc = parseXML ? parseXML(xml) : new DOMParser().parseFromString(xml, 'application/xml');
    if (!doc || doc.getElementsByTagName('parsererror').length) return fail('Malformed XML');
    const root = doc.documentElement;
    if (!root || root.localName !== 'Capabilities' || root.namespaceURI !== WMTS || root.getAttribute('version') !== '1.0.0')
      return fail('Unsupported capabilities namespace/version');
    const contents = one(root, 'Contents', WMTS);
    const layers = kids(contents, 'Layer', WMTS), matrices = kids(contents, 'TileMatrixSet', WMTS);
    const matches = (nodes, id) => nodes.filter(n => kids(n, 'Identifier', OWS).some(x => txt(x) === id));
    const results = {};
    PRODUCTS.forEach(p => {
      try {
        const found = matches(layers, p.id);
        if (found.length !== 1) return fail(found.length ? 'Duplicate product ID' : 'Product ID missing');
        const layer = found[0]; if (identifier(layer) !== p.id) return fail('Product identity mismatch');
        if (!kids(layer, 'Format', WMTS).some(n => txt(n) === p.mime)) return fail('Expected format missing: ' + p.mime);
        const styles = kids(layer, 'Style', WMTS).filter(n => identifier(n) === 'default' && ['true','1'].includes(n.getAttribute('isDefault')));
        if (styles.length !== 1) return fail('Expected default style missing/duplicate');
        const links = kids(layer, 'TileMatrixSetLink', WMTS).filter(n => childText(n, 'TileMatrixSet') === p.matrix);
        if (links.length !== 1) return fail('Expected matrix binding missing/duplicate: ' + p.matrix);
        const sets = matches(matrices, p.matrix); if (sets.length !== 1) return fail('Matrix definition missing/duplicate');
        const set = sets[0]; if (identifier(set) !== p.matrix) return fail('Matrix identity mismatch');
        if (!['urn:ogc:def:crs:EPSG::3857','urn:ogc:def:crs:EPSG:6.18:3:3857','urn:ogc:def:crs:EPSG:6.18.3:3857','EPSG:3857','http://www.opengis.net/def/crs/EPSG/0/3857'].includes(childText(set, 'SupportedCRS', OWS))) return fail('Unsupported CRS');
        const levels = kids(set, 'TileMatrix', WMTS);
        if (levels.length !== p.zoom + 1) return fail('Native zoom differs');
        for (let z = 0; z <= p.zoom; z++) {
          const entries = matches(levels, String(z)); if (entries.length !== 1) return fail('Matrix level missing/duplicate');
          const v = entries[0], top = childText(v, 'TopLeftCorner').split(/\s+/).map(Number);
          if (identifier(v) !== String(z) || childText(v,'TileWidth') !== '256' || childText(v,'TileHeight') !== '256' ||
              childText(v,'MatrixWidth') !== String(2 ** z) || childText(v,'MatrixHeight') !== String(2 ** z) || top.length !== 2 ||
              !Number.isFinite(top[0]) || !Number.isFinite(top[1]) || Math.abs(top[0] + 20037508.342789) > 1 || Math.abs(top[1] - 20037508.342789) > 1 ||
              Math.abs(+childText(v,'ScaleDenominator') / (559082264.028718 / 2 ** z) - 1) > 0.0001 || !Number.isFinite(+childText(v,'ScaleDenominator')))
            return fail('Unsupported Web Mercator grid');
        }
        const dims = kids(layer, 'Dimension', WMTS).filter(n => ['Time','time'].includes(identifier(n)));
        if (dims.length !== 1) return fail('Time dimension missing/duplicate');
        const dim = dims[0];
        if (childText(dim, 'UOM', OWS) !== 'ISO8601') return fail('Unsupported time units');
        const defaults = kids(dim, 'Default', WMTS);
        if (defaults.length !== 1) return fail('Time default missing/duplicate');
        dateValue(txt(defaults[0]), p.kind); // Validate, but never use default as availability authority.
        results[p.id] = {ok:true, ranges:domain(kids(dim, 'Value', WMTS).map(txt), p.kind),coverageLimits:coverageLimits(links[0],p)};
      } catch (e) { results[p.id] = {ok:false, reason:e.message}; }
    });
    return results;
  }
  function tileURL(product, binding, time) {
    if (!binding || !binding.ok || !advertised(binding.ranges, dateValue(time, product.kind))) return fail('Unadvertised request time');
    return ROOT + product.id + '/default/' + encodeURIComponent(time) + '/' + product.matrix + '/{z}/{y}/{x}.' + product.ext;
  }
  async function fetchMetadata(fetcher, {timeoutMs=25000, cap=10*1024*1024, signal} = {}) {
    const abort = new AbortController(); let reader, timer, rejectCancelled;
    const cancel = () => { abort.abort(); if (rejectCancelled) rejectCancelled(new Error('Metadata cancelled')); };
    const cancelled = new Promise((_, reject) => { rejectCancelled=reject; });
    if (signal) { signal.addEventListener('abort', cancel, {once:true}); if (signal.aborted) cancel(); }
    const work = (async () => {
      const response = await fetcher(CAPABILITIES, {mode:'cors', credentials:'omit', cache:'no-store', redirect:'error', signal:abort.signal});
      if (!response.ok) return fail('Metadata HTTP ' + response.status);
      const length = response.headers.get('content-length');
      if (length && (!/^\d+$/.test(length) || +length > cap)) return fail('Metadata exceeds 10 MiB cap or invalid length');
      if (!response.body || !response.body.getReader) return fail('Bounded streaming metadata read unavailable');
      reader = response.body.getReader(); const decoder = new TextDecoder('utf-8', {fatal:true});
      let size=0, text='';
      while (true) {
        const chunk = await reader.read(); if (chunk.done) break;
        size += chunk.value.byteLength; if (size > cap) return fail('Metadata exceeds 10 MiB response cap');
        text += decoder.decode(chunk.value, {stream:true});
      }
      text += decoder.decode(); return text;
    })();
    try {
      return await Promise.race([work, cancelled, new Promise((_, reject) => { timer=setTimeout(() => reject(new Error('Metadata timeout')), timeoutMs); })]);
    } finally { if (signal) signal.removeEventListener('abort', cancel); clearTimeout(timer); abort.abort(); if (reader) { try { Promise.resolve(reader.cancel()).catch(()=>{}); } catch (_) { /* cancellation only */ } } }
  }
  function createOwner({fetcher, parseXML, now=Date.now, fetchOptions={}}) {
    let current=null, pending=null, dead=false;
    const abort=new AbortController();
    function check(force=false) {
      if (dead) return Promise.resolve({status:'DESTROYED'});
      if (pending) return pending;
      if (current && !force) return Promise.resolve(current);
      pending=(async () => {
        let result;
        try {
          const xml=await fetchMetadata(fetcher, {...fetchOptions, signal:abort.signal});
          result={status:'PARSED', products:parseCapabilities(xml,parseXML), checkedAt:new Date(now()).toISOString()};
        } catch(e) { result={status:'UNKNOWN', reason:e.message, checkedAt:new Date(now()).toISOString()}; }
        if (dead) return {status:'DESTROYED'};
        current=result; return result;
      })().finally(() => { pending=null; });
      return pending;
    }
    return {check, destroy:() => { dead=true; current=null; abort.abort(); }};
  }
  return {WMTS,OWS,ROOT,CAPABILITIES,MATRIX_LINK,DAY,PRODUCTS,dateValue,duration,domain,advertised,latestInstant,utcDay,dailyWindow,parseCapabilities,tileURL,tileAllowed,fetchMetadata,createOwner};
})();

  // One lazy metadata owner per map. Opening the map/panel constructs no tiles
  // and starts no request. Selection or explicit imagery rfrsh is the trigger.
  var ptGibsOwner = ptGibs.createOwner({fetcher:function(url, opts) { return fetch(url, opts); }});
  var ptGibsLayers = [], ptGibsDestroyed = false;
  map.on('unload', function() {
    ptGibsDestroyed = true;
    ptGibsOwner.destroy();
    ptGibsLayers.forEach(function(layer) { layer.destroyGibs(); });
  });

  function ptGibsMetadataHtml() {
    return '<details id="pt-gibs-meta" class="pt-ops-small"><summary aria-label="Satellite imagery metadata">meta</summary>' +
      ptGibs.PRODUCTS.map(function(product) {
        return '<div data-gibs-meta-id="' + escapeHtml(product.stableId) + '"><b>' + escapeHtml(product.label) + '</b><dl>' +
          '<dt>Orbit</dt><dd>' + escapeHtml(product.orbit) + '</dd>' +
          '<dt>Satellite</dt><dd>' + escapeHtml(product.satellite) + '</dd>' +
          '<dt>Instrument / product</dt><dd>' + escapeHtml(product.instrumentProduct) + '</dd></dl></div>';
      }).join('') + '</details>';
  }

  function ptGibsRowHtml(def) {
    var p = def.layer && def.layer.gibsProduct;
    if (!p) return '';
    var key = 'pt-gibs-' + p.stableId;
    return '<div class="pt-ops-layer-help" id="' + key + '">' +
      '<div id="' + key + '-status" role="status" aria-live="polite">Not checked — select to check imagery availability.</div>' +
      (p.kind === 'daily' ? '<div id="' + key + '-date-wrap" hidden><label for="' + key + '-date">UTC data date: </label>' +
        '<select id="' + key + '-date" data-pt-gibs-date="' + p.stableId + '" aria-describedby="' + key + '-status"><option value="">Choose advertised date</option></select></div>' : '') +
      '<details id="' + key + '-details" hidden><summary>Imagery timing &amp; coverage</summary><div id="' + key + '-detail-text"></div></details></div>';
  }

  function makeGibsImageryLayer(stableId) {
    var product = ptGibs.PRODUCTS.find(function(p) { return p.stableId === stableId; });
    if (!product) throw new Error('Unknown GIBS product identity');
    var generation = 0, tileGeneration = 0, tile = null, layer;
    var state = {active:false, metadata:'NOT_CHECKED', reason:'', binding:null, checkedAt:null,
      dates:[], chosen:'', initialized:false, request:null, pending:false, tileErrors:0, received:0};
    function blocked() { return !!(state.binding && !state.binding.ok); }
    function dropTiles() {
      tileGeneration++;
      if (tile) {
        // Leaflet installs its map-event cleanup on the tile's remove event.
        // Let that listener run before discarding the tile's remaining listeners.
        if (map.hasLayer(tile)) map.removeLayer(tile);
        tile.off();
        tile = null;
      }
      state.request = null; state.pending = false; state.tileErrors = 0; state.received = 0;
    }
    function present() {
      if (ptGibsDestroyed) return;
      var key = 'pt-gibs-' + product.stableId;
      var status = document.getElementById(key + '-status');
      var dateWrap = document.getElementById(key + '-date-wrap');
      var dateSelect = document.getElementById(key + '-date');
      var details = document.getElementById(key + '-details');
      var detailText = document.getElementById(key + '-detail-text');
      var chk = checkboxByName[product.label];
      if (chk) { chk.disabled = blocked() && !state.active; chk.setAttribute('aria-describedby', key + '-status'); }
      var words = state.metadata === 'CHECKING' ? 'Checking provider metadata…' :
        state.metadata === 'UNKNOWN' ? 'Metadata check failed; availability UNKNOWN.' :
        blocked() ? 'Imagery binding unavailable — explicit rfrsh can recheck.' :
        state.metadata === 'PARSED' ? 'Core imagery binding verified.' : 'Not checked — select to check imagery availability.';
      if (state.reason) words += ' ' + state.reason;
      if (state.active && state.binding && state.binding.ok && state.binding.coverageLimits.status === 'KNOWN_INCONSISTENT_GOES_LIMITS') {
        words += ' Provider coverage limits inconsistent; precise coverage UNKNOWN.';
      }
      if (state.request) {
        if (product.kind === 'daily') {
          var offset = (ptGibs.dateValue(ptGibs.utcDay(Date.now()), 'daily') - ptGibs.dateValue(state.request, 'daily')) / ptGibs.DAY;
          words += ' Requested UTC data date: ' + state.request + ' (' + offset + ' UTC calendar days ago; not acquisition age).';
        } else {
          var pacific = formatLosAngelesCompactParts(new Date(state.request));
          words += ' Requested advertised time: ' + state.request +
            (pacific && pacific.tz ? ' / ' + pacific.text + ' ' + pacific.tz : '') + ' (' + ((Date.now() - ptGibs.dateValue(state.request, 'instant')) / 3600000).toFixed(2) + ' hours ago).';
        }
        words += state.tileErrors ? ' Tile transport errors: ' + state.tileErrors + '; coverage may be partial.' :
          state.pending ? ' Tiles loading.' : state.received ? ' Tiles received; coverage Unverified.' : ' No tiles received in this view.';
        words += ' Exact pixel acquisition time Unverified.';
      } else if (!state.active && state.metadata === 'PARSED') words += ' Layer off.';
      if (status) status.textContent = words;
      if (dateWrap) dateWrap.hidden = !state.active;
      if (dateSelect) {
        // Preserve the select itself (and keyboard focus); replace options only
        // when the advertised recent window changes, never on each tile event.
        var signature = state.dates.join('|');
        if (dateSelect.getAttribute('data-date-options') !== signature) {
          dateSelect.innerHTML = '<option value="">Choose advertised date</option>' + state.dates.map(function(d) {
            return '<option value="' + escapeHtml(d) + '">' + escapeHtml(d) + '</option>';
          }).join('');
          dateSelect.setAttribute('data-date-options', signature);
        }
        dateSelect.value = state.dates.indexOf(state.chosen) >= 0 ? state.chosen : '';
        dateSelect.disabled = !state.active || !state.dates.length || state.metadata === 'CHECKING';
      }
      if (details) details.hidden = !state.active && !state.reason;
      if (detailText) {
        var limits = state.binding && state.binding.ok && state.binding.coverageLimits;
        detailText.textContent = product.id + ' · ' + product.matrix + '. BRIM metadata check: ' + (state.checkedAt || 'None') +
          '. Request time is not independently verified pixel acquisition time. Imagery remains at its requested time until explicit rfrsh; pan/zoom do not recheck metadata. ' +
          (limits ? limits.warning + (limits.issues.length ? ' Details: ' + limits.issues.join('; ') + '.' : '') : 'Coverage Unverified.') +
          ' Blank tiles do not establish global unavailability. No automatic date fallback or age rejection.';
      }
      setOpsLayerLoading(product.label, state.metadata === 'CHECKING' || (state.active && state.pending));
      if (state.active) {
        recordStatus(product.label, words, 'pt-ops-muted');
      }
    }
    function draw() {
      // Detach the old date before changing its label or mounting any new tiles.
      generation++; dropTiles(); state.reason = '';
      var binding = state.binding, clock = Date.now();
      if (!state.active || !binding || !binding.ok) { present(); return; }
      var requested;
      if (product.kind === 'daily') {
        state.dates = ptGibs.dailyWindow(binding.ranges, clock);
        if (!state.initialized) {
          state.initialized = true;
          var yesterday = ptGibs.utcDay(ptGibs.dateValue(ptGibs.utcDay(clock), 'daily') - ptGibs.DAY);
          state.chosen = state.dates.indexOf(yesterday) >= 0 ? yesterday : '';
        }
        if (state.dates.indexOf(state.chosen) < 0) {
          state.reason = state.dates.length ? 'Choose an advertised UTC data date; no automatic fallback.' : 'No advertised date in the recent eight-day window.';
          present(); return;
        }
        requested = state.chosen;
      } else {
        var instant = ptGibs.latestInstant(binding.ranges, clock);
        if (instant === null) { state.reason = 'No non-future advertised instant.'; present(); return; }
        requested = new Date(instant).toISOString().replace('.000Z', 'Z');
      }
      var epoch = tileGeneration;
      var current = L.tileLayer(ptGibs.tileURL(product, binding, requested), {
        pane:'pane_ops', opacity:0.82, attribution:'NASA GIBS / NOAA',
        maxNativeZoom:product.zoom, maxZoom:20, noWrap:true, keepBuffer:0,
        // Stable registration order determines imagery stacking even on re-add.
        zIndex:200 + product.order
      });
      var validTile = current._isValidTile;
      current._isValidTile = function(coords) {
        return ptGibs.tileAllowed(product, binding, coords) && validTile.call(this, coords);
      };
      tile = current; state.request = requested;
      function live() { return !ptGibsDestroyed && state.active && tileGeneration === epoch && tile === current; }
      current.on('loading', function() { if (live()) { state.pending = true; present(); } });
      current.on('tileload', function() { if (live()) { state.received++; } });
      current.on('tileerror', function() { if (live()) { state.tileErrors++; present(); } });
      current.on('load', function() { if (live()) { state.pending = false; present(); } });
      // Errors persist for this dated layer through later batches and panning.
      // Only an explicit new request/date creates a new transport observation.
      present(); current.addTo(map);
    }
    function check(force) {
      if (ptGibsDestroyed) return Promise.resolve();
      var epoch = ++generation;
      state.metadata = 'CHECKING'; state.reason = ''; present();
      return ptGibsOwner.check(force).then(function(result) {
        if (ptGibsDestroyed || generation !== epoch || result.status === 'DESTROYED') return;
        state.metadata = result.status; state.checkedAt = result.checkedAt;
        if (result.status !== 'PARSED') {
          state.reason = 'Recheck failed: ' + result.reason + (state.request ? ' Previous dated imagery retained.' : ' No image requested.');
          present(); return;
        }
        state.binding = result.products[product.id];
        if (!state.binding || !state.binding.ok) {
          state.binding = state.binding || {ok:false, reason:'Product binding missing'};
          dropTiles(); state.reason = state.binding.reason;
          if (state.active) map.removeLayer(layer);
          present(); return;
        }
        state.reason = '';
        if (state.active) draw(); else present();
      });
    }
    layer = new (L.Layer.extend({
      options:{name:product.label, sourceUrl:product.sourceUrl, infoUrl:product.infoUrl, infoLabel:'info', refreshable:true},
      onAdd:function() {
        // Only this family participates; normal onRemove owns row/tile cleanup.
        ptGibsLayers.forEach(function(other) {
          if (other !== layer && map.hasLayer(other)) map.removeLayer(other);
        });
        state.active = true; check(false);
      },
      onRemove:function() {
        generation++; state.active = false; dropTiles();
        // A clear/off cancels this controller's callbacks, not the shared fetch.
        if (state.metadata === 'CHECKING') { state.metadata = 'NOT_CHECKED'; state.reason = ''; }
        if (activeLayers[product.label] === layer) delete activeLayers[product.label];
        var chk = checkboxByName[product.label]; if (chk) chk.checked = false;
        delete statusRows[product.label];
        if (!ptGibsDestroyed) redrawStatus();
        setOpsLayerLoading(product.label, false); present(); updateOpsHeaderCount();
      }
    }))();
    layer.gibsProduct = product;
    layer.gibsState = state;
    layer.canActivate = function() { return !ptGibsDestroyed && !blocked(); };
    layer.refreshCurrentView = function() { return check(true); };
    layer.chooseDate = function(date) {
      if (!state.active || product.kind !== 'daily' || !state.binding || !state.binding.ok || state.metadata === 'CHECKING') return;
      state.chosen = String(date); state.initialized = true; draw();
    };
    layer.cancelGibsCheck = function() {
      generation++;
      if (state.metadata === 'CHECKING') { state.metadata = 'NOT_CHECKED'; state.reason = ''; }
      setOpsLayerLoading(product.label, false); present();
    };
    layer.destroyGibs = function() { generation++; state.active = false; dropTiles(); };
    ptGibsLayers.push(layer);
    return layer;
  }

)---"
}
