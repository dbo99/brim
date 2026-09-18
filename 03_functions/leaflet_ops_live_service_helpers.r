# ==== leaflet_ops_live_service_helpers.r ===================================
##
## PURPOSE:
##   Ops Live service endpoint constants and freshness/status check helpers.
##
## DESIGN:
##   This file is sourced by `leaflet_ops_live_helpers.r`.
##   It returns browser-side JavaScript as text for injection into the Ops Live
##   htmlwidgets/onRender function. Keep edits narrow and feature-specific.
## ============================================================================

pt_ops_live_service_helpers_js <- function() {

  r"---(
  // --------------------------------------------------------------------------
  // Service endpoints and freshness checks
  // --------------------------------------------------------------------------
  var MRMS = 'https://mapservices.weather.noaa.gov/raster/rest/services/obs/mrms_qpe/ImageServer';
  var RFC_QPE = 'https://mapservices.weather.noaa.gov/raster/rest/services/obs/rfc_qpe/MapServer';
  var WPC_QPF = 'https://mapservices.weather.noaa.gov/vector/rest/services/precip/wpc_qpf/MapServer';
  var WPC_ERO = 'https://mapservices.weather.noaa.gov/vector/rest/services/hazards/wpc_precip_hazards/MapServer';
  var NOAA_RADAR = 'https://mapservices.weather.noaa.gov/eventdriven/rest/services/radar/radar_base_reflectivity/MapServer';
  var NWS_WWA = 'https://mapservices.weather.noaa.gov/eventdriven/rest/services/WWA/watch_warn_adv/MapServer';
  var SURFACE_OBS = 'https://mapservices.weather.noaa.gov/vector/rest/services/obs/surface_obs/MapServer';
  function mapServerLegendUrl(baseUrl) {
    return safeUrlBase(baseUrl) + '/legend?f=html';
  }
  
  function checkReturnUpdates(name, baseUrl, expected) {
    var url = safeUrlBase(baseUrl) + '/returnUpdates?f=pjson&_=' + Date.now();
    fetch(url)
      .then(function(resp) { return resp.json(); })
      .then(function(json) {
        var text = '';
        if (json && json.fullUpdate) text += 'fullUpdate: ' + json.fullUpdate + '; ';
        if (json && json.layers && json.layers.length) text += 'layers reported: ' + json.layers.length + '; ';
        if (!text) text = 'metadata returned; ';
        recordStatus(name + ' metadata', text + 'expected: ' + expected, 'pt-ops-ok');
      })
      .catch(function() {
        recordStatus(name + ' metadata', 'Freshness request failed; expected: ' + expected, 'pt-ops-warn');
      });
  }
  

  function ptOpsRadarMrmsMetadata(def) {
    var type = def && def.legendType;
    if (['radar_iem', 'radar_noaa', 'mrms_qpe'].indexOf(type) < 0) return null;
    var mrms = type === 'mrms_qpe', iem = type === 'radar_iem';
    var status = statusRows[iem ? '' : mrms ? 'MRMS metadata' : 'NOAA radar metadata'];
    return [
      {key: 'source', label: 'Source', value: iem ? 'Iowa Environmental Mesonet (IEM)' : mrms ? 'NOAA/NWS MRMS QPE' : 'NOAA/NWS MRMS radar, layer 3'},
      {key: 'timing', label: mrms ? 'Displayed accumulation dates' : 'Displayed frame time', value: 'Unverified'},
      {key: 'scale', label: mrms ? 'Exact breaks, units and selected raster-function scale' : iem ? 'Visual key' : 'Exact layer-3 BRIM visual scale',
        value: mrms ? 'Unverified in BRIM' : iem ? 'Provider legend image; values not reinterpreted by BRIM' : 'Unverified — not embedded in BRIM'},
      {key: 'check', label: 'BRIM service check', value: status ? status.time : 'No service check recorded'},
      {key: 'context', label: 'Timing context', value: mrms ?
        'The latest service metadata row is not bound to this displayed raster. Product names do not establish accumulation dates.' :
        'Image or tile loading and service metadata do not establish the displayed frame time.'}
    ];
  }

  function ptOpsMrmsMetadataDate(value) {
    // Format only a recognizable unbound service field, never an image endpoint.
    var numeric = typeof value === 'number' && isFinite(value) && Math.floor(value) === value;
    var iso = typeof value === 'string' && /^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{3})?Z$/.test(value);
    if (!numeric && !iso) return 'Unverified';
    var d = new Date(value);
    if (isNaN(d.getTime())) return 'Unverified';
    var text = d.toISOString();
    if (iso && text.replace('.000Z', 'Z') !== value.replace('.000Z', 'Z')) return 'Unverified';
    return text;
  }

  function ptOpsRadarMrmsStatus(name, message) {
    // A healthy metadata response still leaves displayed-product timing unknown.
    recordStatus(name, message, 'pt-ops-warn');
    ptOpsUpdateRadarMrmsMetadata();
  }

  function checkMrmsLatest() {
    var owner = statusRows;
    var url = safeUrlBase(MRMS) + '/query?f=json&where=1%3D1&outFields=name,idp_validendtime,idp_filedate,idp_ingestdate&returnGeometry=false&orderByFields=idp_validendtime%20DESC&resultRecordCount=1&_=' + Date.now();
    var qualification = 'Displayed-raster association: Unverified; Displayed accumulation dates: Unverified. ' +
      'Service update expectation: near :04 after hour; not verified displayed-raster timing.';
    fetch(url)
      .then(function(resp) { return resp.json(); })
      .then(function(json) {
        if (statusRows !== owner) return; // Clear owns the status session.
        var f = json && !json.error && json.features && json.features[0] && json.features[0].attributes;
        if (!f) throw new Error('No feature attributes returned');
        ptOpsRadarMrmsStatus('MRMS metadata', 'Service metadata received. Unbound latest service metadata row — valid/end field: ' +
          ptOpsMrmsMetadataDate(f.idp_validendtime) + '. ' + qualification);
      })
      .catch(function() {
        if (statusRows !== owner) return;
        ptOpsRadarMrmsStatus('MRMS metadata', 'Service metadata check failed. ' + qualification);
      });
  }
  
  function ptOpsRfcQpeCheckTime(value) {
    // This formats the BRIM check instant only, never a displayed-image date.
    // Reuse QPF's deliberate Los Angeles formatter; reject its local fallback.
    if (typeof value !== 'string' || !/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{3})?Z$/.test(value)) return 'Unverified';
    var d = new Date(value);
    if (isNaN(d.getTime()) || d.toISOString().replace('.000Z', 'Z') !== value.replace('.000Z', 'Z')) return 'Unverified';
    var parts;
    try { parts = formatLosAngelesCompactParts(d); } catch (e) { return 'Unverified'; }
    if (!parts || typeof parts.text !== 'string' || !parts.text || !/^(PST|PDT)$/.test(parts.tz)) return 'Unverified';
    return parts.text + ' ' + parts.tz + ' (' + value + ')';
  }

  function ptOpsRfcQpeMetadata(def, checked) {
    // One RFC copy/value owner for the panel and cards. The existing status row
    // owns the last real check; presentation alone never creates an instant.
    var status = statusRows['NWS QPE Mosaic metadata'];
    var check = arguments.length > 1 ? checked : status && status.rfcQpeCheck;
    return {
      interval: 'Unverified',
      fields: [
        {key: 'method', label: 'Product method', value: def ? def.period :
          Object.keys(ptOpsRfcQpeLegends).map(function(id) { return ptOpsRfcQpeLegends[id].period; }).join(' ')},
        {key: 'cutoff', label: 'Scheduled cutoff',
          value: '4 a.m. PST / 5 a.m. PDT (12Z). This convention does not date the displayed image.'},
        {key: 'refresh', label: 'Expected service refresh',
          value: 'hourly near :55; daily revisions possible 4 a.m.–1 p.m. PST / 5 a.m.–2 p.m. PDT (12–21 UTC).'},
        {key: 'check', label: 'BRIM check', value: check || 'No check recorded'},
        {key: 'provider', label: 'Provider publication/update', value: 'Unverified'},
        {key: 'context', label: 'Context',
          value: 'The BRIM check is shared RFC service context, not a per-image request or accumulation date. ' +
            'Schedule expectation, not a data guarantee; no frame-date metadata requested.'}
      ]
    };
  }

  function checkRfcQpe() {
    // The service/footprint metadata has no verified association with the
    // currently displayed export. Do not fetch it merely to claim freshness.
    var checked = ptOpsRfcQpeCheckTime(new Date().toISOString());
    var metadata = ptOpsRfcQpeMetadata(null, checked);
    recordStatus('NWS QPE Mosaic metadata',
      'Displayed accumulation interval: ' + metadata.interval + '; ' +
      metadata.fields.map(function(field) { return field.label + ': ' + field.value; }).join('; '),
      'pt-ops-warn');
    statusRows['NWS QPE Mosaic metadata'].rfcQpeCheck = checked;
    ptOpsUpdateRfcQpeMetadata();
  }
  
  function checkWpcQpf() {
    checkReturnUpdates('WPC QPF', WPC_QPF, 'twice daily at 06Z and 18Z');
  }
  
  function checkWpcEro() {
    checkReturnUpdates('WPC ERO', WPC_ERO, 'Day 1 required 0100Z/0830Z/1500Z; updates possible anytime');
  }

  function checkNoaaRadar(isCurrent) {
    var owner = statusRows;
    var qualification = 'Displayed frame time: Unverified. Service update expectation: roughly every 5-10 minutes ' +
      'from the MRMS radar base-reflectivity service; not verified displayed-frame timing.';
    fetch(safeUrlBase(NOAA_RADAR) + '/returnUpdates?f=pjson&_=' + Date.now())
      .then(function(resp) { return resp.json(); })
      .then(function(json) {
        if (statusRows !== owner || (isCurrent && !isCurrent())) return;
        if (!json || typeof json !== 'object' || Array.isArray(json) || json.error) throw new Error('Invalid service metadata');
        ptOpsRadarMrmsStatus('NOAA radar metadata', 'Service metadata received; no displayed-frame association. ' + qualification);
      })
      .catch(function() {
        if (statusRows !== owner || (isCurrent && !isCurrent())) return;
        ptOpsRadarMrmsStatus('NOAA radar metadata', 'Service metadata check failed. ' + qualification);
      });
  }

  function checkNwsWwa() {
    checkReturnUpdates('NWS watches/warnings/advisories', NWS_WWA, 'roughly every 5 minutes');
  }

  function checkSurfaceObs() {
    checkReturnUpdates('NWS surface observations', SURFACE_OBS, 'roughly every 10 minutes; individual stations may report less often');
  }



  // Forecast-card metadata is separate from BRIM service checks and existing RFC time helpers.
  function ptForecastDateParts(value) {
    if (typeof value !== 'string') return null;
    var m = /^(\d{4})-(\d{2})-(\d{2})(?:[ T](\d{2}):(\d{2})(?::(\d{2}))?)?$/.exec(value);
    if (!m) return null;
    var p = m.slice(1).map(function(x) { return x === undefined ? 0 : Number(x); });
    if (p[0] < 1000) return null;
    var d = new Date(Date.UTC(p[0], p[1]-1, p[2], p[3], p[4], p[5]));
    return d.getUTCFullYear() === p[0] && d.getUTCMonth()+1 === p[1] && d.getUTCDate() === p[2] &&
      d.getUTCHours() === p[3] && d.getUTCMinutes() === p[4] && d.getUTCSeconds() === p[5] ? {parts:p, date:d} : null;
  }

  function ptForecastCalendarDate(value) {
    if (typeof value === 'number') {
      // These typed fields are epoch milliseconds, not an auto-detected epoch unit.
      if (!Number.isSafeInteger(value) || Math.abs(value) < 100000000000 || value % 86400000 !== 0) return null;
      var d = new Date(value);
      return isFinite(d.getTime()) ? d.toISOString().slice(0,10) : null;
    }
    if (typeof value !== 'string' || !/^\d{4}-\d{2}-\d{2}$/.test(value)) return null;
    return ptForecastDateParts(value) ? value : null;
  }

  function ptWpcForecastInterval(attrs) {
    var m = /^(\d{2})Z (\d{2})\/(\d{2})\/(\d{2}) - (\d{2})Z (\d{2})\/(\d{2})\/(\d{2})$/.exec(attrs.valid_time || '');
    var a = ptForecastDateParts(attrs.start_time), b = ptForecastDateParts(attrs.end_time);
    if (!m || !a || !b) return null;
    function matches(p, off) {
      return p[3] === Number(m[off]) && p[1] === Number(m[off+1]) && p[2] === Number(m[off+2]) &&
        p[0] % 100 === Number(m[off+3]) && p[4] === 0 && p[5] === 0;
    }
    if (!matches(a.parts,1) || !matches(b.parts,5) || b.date <= a.date) return null;
    var fmt = new Intl.DateTimeFormat('en-US', {timeZone:'America/Los_Angeles', year:'numeric', month:'short', day:'numeric', hour:'numeric', minute:'2-digit', timeZoneName:'short'});
    return {start:a.date.toISOString(), end:b.date.toISOString(), text:fmt.format(a.date) + ' – ' + fmt.format(b.date)};
  }

  function ptForecastMetadata(family, features, incomplete) {
    var out = {issued:'Unverified', valid:'Unverified', utc:'Unverified',
      scope: family === 'qpf' ? 'Provider forecast metadata; image-cycle match Unverified' : 'Loaded-feature metadata only; not a global census'};
    if (!Array.isArray(features) || !features.length) { out.scope += '; no loaded metadata'; return out; }
    if (incomplete) { out.scope += '; incomplete response — dates Unverified'; return out; }
    var tuples = features.map(function(f) {
      var a = f && (f.attributes || f.properties) || {};
      if (family === 'cpc') {
        var issue = ptForecastCalendarDate(a.fcst_date), start = ptForecastCalendarDate(a.start_date), end = ptForecastCalendarDate(a.end_date);
        return issue && start && end && start <= end ? {issued:issue, valid:start+' – '+end, utc:'Calendar dates; no time-of-day conversion'} : null;
      }
      var interval = ptWpcForecastInterval(a);
      if (!interval) return null;
      return {issued:ptForecastDateParts(a.issue_time) ? a.issue_time+' (time zone Unverified)' : 'Unverified',
        valid:interval.text, utc:interval.start+' – '+interval.end};
    });
    if (tuples.some(function(t) { return !t; })) { out.scope += '; missing/invalid metadata — dates Unverified'; return out; }
    if (tuples.some(function(t) { return JSON.stringify(t) !== JSON.stringify(tuples[0]); })) {
      out.scope += '; mixed metadata — dates Unverified'; return out;
    }
    Object.assign(out,tuples[0]);
    if (family === 'qpf') out.scope += '; bounded attribute sample, not global unanimity';
    return out;
  }

  // Presentation only. Inputs here are validated ISO instants or calendar dates;
  // this never assigns a timezone to an unqualified provider issue string.
  function ptForecastCompactInterval(start, end) {
    function parts(value) {
      if (!/^\d{4}-\d{2}-\d{2}T.*Z$/.test(value)) return null;
      var d = new Date(value); if (!isFinite(d.getTime())) return null;
      var p = {};
      new Intl.DateTimeFormat('en-US', {timeZone:'America/Los_Angeles',weekday:'short',year:'numeric',month:'numeric',day:'numeric',hour:'numeric',minute:'2-digit',second:'2-digit',hour12:true,timeZoneName:'short'}).formatToParts(d).forEach(function(x) { p[x.type] = x.value; });
      return p;
    }
    var a = parts(start), b = parts(end);
    if (!a || !b || Date.parse(end) <= Date.parse(start)) return 'Unverified';
    var years = a.year !== b.year;
    function label(p) { return p.weekday+' '+p.month+'/'+p.day+(years ? '/'+p.year : '')+' '+p.hour+
      (p.minute !== '00' || p.second !== '00' ? ':'+p.minute : '')+(p.second !== '00' ? ':'+p.second : '')+' '+p.dayPeriod; }
    return label(a)+(a.timeZoneName !== b.timeZoneName ? ' '+a.timeZoneName : '')+' → '+label(b)+' '+b.timeZoneName;
  }

  function ptForecastCompactUtc(start, end) {
    var a = new Date(start), b = new Date(end);
    if (!/Z$/.test(start) || !/Z$/.test(end) || !isFinite(a.getTime()) || !isFinite(b.getTime()) || b <= a) return '';
    var years = a.getUTCFullYear() !== b.getUTCFullYear(), months = years || a.getUTCMonth() !== b.getUTCMonth();
    function label(d) { return (months ? (d.getUTCMonth()+1)+'/' : '')+d.getUTCDate()+(years ? '/'+d.getUTCFullYear() : '')+'/'+
      String(d.getUTCHours()).padStart(2,'0')+(d.getUTCMinutes() || d.getUTCSeconds() ? String(d.getUTCMinutes()).padStart(2,'0') : '')+
      (d.getUTCSeconds() ? ':'+String(d.getUTCSeconds()).padStart(2,'0') : '')+'Z'; }
    return label(a)+' → '+label(b);
  }

  function ptForecastCompactCalendar(value, includeYear) {
    var p = ptForecastDateParts(value); if (!p) return 'Unverified';
    var date = p.parts;
    return ['Sun','Mon','Tue','Wed','Thu','Fri','Sat'][p.date.getUTCDay()]+' '+date[1]+'/'+date[2]+(includeYear ? '/'+date[0] : '');
  }

  function ptForecastPresentation(family, metadata) {
    var out = {issued:metadata.issued, valid:metadata.valid, utc:metadata.utc, scope:metadata.scope};
    if (family === 'cpc') {
      var dates = /^(\d{4}-\d{2}-\d{2}) – (\d{4}-\d{2}-\d{2})$/.exec(metadata.valid);
      if (dates) out.valid = ptForecastCompactCalendar(dates[1],dates[1].slice(0,4)!==dates[2].slice(0,4))+' – '+ptForecastCompactCalendar(dates[2],dates[1].slice(0,4)!==dates[2].slice(0,4));
      if (ptForecastCalendarDate(metadata.issued)) out.issued = ptForecastCompactCalendar(metadata.issued,false);
    } else {
      var ends = metadata.utc.split(' – ');
      if (ends.length === 2) out.valid = ptForecastCompactInterval(ends[0],ends[1]);
      var issue = metadata.issued.replace(' (time zone Unverified)',''), p = ptForecastDateParts(issue);
      if (p) {
        var v = p.parts, hasClock = /[ T]\d{2}:\d{2}/.test(issue);
        out.issued = ptForecastCompactCalendar(issue,false)+(hasClock ? ' '+(v[3]%12 || 12)+(v[4] || v[5] ? ':'+String(v[4]).padStart(2,'0') : '')+(v[5] ? ':'+String(v[5]).padStart(2,'0') : '')+' '+(v[3]<12?'AM':'PM') : '')+' · zone unverified';
      }
    }
    // Unavailable/mixed/incomplete qualifiers remain visible; full wording stays in details.
    var qualifiers = metadata.scope.split(';').slice(1).filter(function(x) { return !/bounded attribute sample|not a global census/.test(x); });
    if (family === 'qpf') qualifiers = qualifiers.filter(function(x) { return !/^\s*image-cycle match unverified\s*$/i.test(x); });
    out.scope = family === 'qpf' ? qualifiers.join(';').trim() : 'Loaded-feature metadata'+(qualifiers.length ? ';'+qualifiers.join(';') : '');
    if (/mixed|incomplete|missing|invalid|failed/i.test(metadata.scope) && !qualifiers.length) out.scope += '; '+metadata.scope;
    return out;
  }

  function ptWpcQpfMetadata(owner) {
    if (owner._isRemoved || !owner._map) return;
    var seq = owner._forecastSeq, def = activeLegendDefs[owner.options.name];
    if (!def) return;
    def.forecastMetadata = ptForecastMetadata('qpf', [], false);
    redrawLegend();
    var url = safeUrlBase(owner.options.url) + '/' + owner.options.layers[0] +
      '/query?f=json&where=1%3D1&outFields=product,valid_time,issue_time,start_time,end_time&returnGeometry=false&returnDistinctValues=false&orderByFields=objectid%20ASC&resultRecordCount=100';
    function current() { return !owner._isRemoved && owner._map && seq === owner._forecastSeq && activeLegendDefs[owner.options.name] === def; }
    fetch(url, {cache:'no-store'}).then(function(r) { if (!r.ok) throw new Error('HTTP '+r.status); return r.json(); })
      .then(function(j) {
        if (!current()) return;
        def.forecastMetadata = ptForecastMetadata('qpf', j && !j.error ? j.features : [], !!(j && j.exceededTransferLimit));
        redrawLegend();
      }).catch(function() {
        if (!current()) return;
        def.forecastMetadata = ptForecastMetadata('qpf', [], false);
        def.forecastMetadata.scope += '; metadata request failed'; redrawLegend();
      });
  }

)---"
}
