/* Review transport only. Original SCAN CSV cells and source tables are lossless.
 * Limits mirror the public producer contract and are checked in cross-language QA. */
(function(root, factory) {
  const api = factory();
  if (typeof module === 'object' && module.exports) module.exports = api;
  else root.BRIM.soilMoistureTransport = api;
})(typeof window === 'object' ? window : this, function() {
  'use strict';
  const limits = Object.freeze({index:262144, bundle:6000000, expanded_scan:12000000,
    cache_compact_bytes:12000000, cache_entries:8, table_cells:1000000,
    table_rows:50000, table_columns:64});
  const tables = ['scan_depth_style.csv', 'scan_sms_monthly_context.csv',
    'scan_sms_prior_wy_fallback_traces.csv', 'scan_sms_waterday_percentiles.csv',
    'scan_soil_moisture_current_wy_trace.csv'];
  // Version 2 preserves these original CSV header sequences exactly.
  const tableSchemas = {
  "scan_depth_style.csv": [
    "depth_in",
    "depth_label",
    "depth_order",
    "depth_color_hex",
    "depth_role"
  ],
  "scan_sms_monthly_context.csv": [
    "site_code",
    "depth_in",
    "month_date",
    "calendar_month",
    "actual_sms_pct",
    "actual_n_days",
    "actual_start_date",
    "actual_end_date",
    "record_start_date",
    "record_end_date",
    "record_start_wy",
    "record_end_wy",
    "record_label",
    "ref_p30",
    "ref_p50",
    "ref_p70",
    "ref_n_years",
    "monthly_ref_ok",
    "ref_start_date",
    "ref_end_date",
    "ref_start_wy",
    "ref_end_wy",
    "ref_label",
    "ref_wy_label",
    "current_wy_excluded",
    "context_years_back",
    "context_stat_min_years",
    "min_days_per_month",
    "display_timezone"
  ],
  "scan_sms_prior_wy_fallback_traces.csv": [
    "station_uid",
    "station_name",
    "site_code",
    "depth_in",
    "water_year",
    "water_day",
    "obs_date",
    "sms_pct",
    "trace_order",
    "trace_label",
    "n_days_in_trace",
    "water_day_min",
    "water_day_max",
    "water_day_span",
    "min_daily_rows_per_prior_wy",
    "min_waterday_span_per_prior_wy",
    "daily_ribbon_min_years",
    "daily_ribbon_min_days",
    "fallback_reason"
  ],
  "scan_sms_waterday_percentiles.csv": [
    "station_uid",
    "station_name",
    "site_code",
    "depth_in",
    "water_day",
    "p00",
    "p10",
    "p30",
    "p50",
    "p70",
    "p90",
    "p100",
    "n_obs",
    "n_years",
    "years_min",
    "years_max",
    "climatology_ok",
    "min_years_for_context",
    "current_water_year_excluded",
    "build_time_utc"
  ],
  "scan_soil_moisture_current_wy_trace.csv": [
    "station_uid",
    "station_name",
    "site_code",
    "depth_in",
    "depth_label",
    "depth_order",
    "depth_color_hex",
    "water_year",
    "water_day",
    "obs_date",
    "obs_datetime_local",
    "sms_pct",
    "sensor_count",
    "sensor_id"
  ]
};
  const need = (ok, message) => { if (!ok) throw Error(message); };
  const byteSize = value => new TextEncoder().encode(JSON.stringify(value)).byteLength;

  function descriptor(d) {
    need(d && /^[a-z0-9-]+\.json$/.test(d.path) && Number.isInteger(d.bytes) &&
      d.bytes > 0 && d.bytes <= limits.bundle && /^[a-f0-9]{64}$/.test(d.sha256),
      'Invalid scoped companion');
    return d;
  }

  function decodeScan(body, stationId) {
    need(body?.schema === 'sm1-scan-popup-2', 'Unsupported SCAN transport schema');
    need(body.feature?.type === 'Feature' &&
      body.feature.properties?.station_triplet === stationId, 'SCAN station mismatch');
    need(body.tables && JSON.stringify(Object.keys(body.tables).sort()) ===
      JSON.stringify(tables.slice().sort()), 'SCAN table inventory mismatch');
    const restored = {};
    let cells = 0, expanded = 0;
    for (const [name, table] of Object.entries(body.tables)) {
      const columns = table?.columns, rows = table?.rows;
      need(Array.isArray(columns) && columns.length > 0 && columns.length <= limits.table_columns &&
        columns.every(c => typeof c === 'string' && c && !['__proto__','prototype','constructor'].includes(c)) &&
        new Set(columns).size === columns.length, 'Invalid SCAN columns');
      need(JSON.stringify(columns) === JSON.stringify(tableSchemas[name]), 'SCAN column schema');
      need(Array.isArray(rows) && rows.length <= limits.table_rows, 'SCAN row bound');
      cells += rows.length * columns.length;
      need(cells <= limits.table_cells, 'SCAN cell bound');
      restored[name] = rows.map(row => {
        need(Array.isArray(row) && row.length === columns.length &&
          row.every(value => typeof value === 'string'), 'Invalid SCAN row/cell');
        const record = Object.fromEntries(columns.map((key, i) => [key, row[i]]));
        expanded += byteSize(record);
        need(expanded <= limits.expanded_scan, 'Expanded SCAN bound');
        return record;
      });
    }
    const decoded = {schema:'sm1-scan-popup-1', feature:body.feature, tables:restored};
    need(byteSize(decoded) <= limits.expanded_scan, 'Expanded SCAN bound');
    return decoded;
  }

  function decode(body, source, stationId) {
    if (source === 'scan') return decodeScan(body, stationId);
    need(source === 'snotel' && body?.schema === 'sm1-snotel-popup-1' &&
      body.metadata?.stationTriplet === stationId && body.data?.stationTriplet === stationId &&
      Array.isArray(body.data.data), 'Invalid SNOTEL companion');
    return body;
  }

  // Frozen local review package; deliberately separate from either SM1 envelope.
  const snotelStatic = Object.freeze({id:'SNOTEL_CA_STATIC_CANDIDATE_1',
    contract:'snotel-awdb-history-1', adapter:'snotel-awdb-offline-1.2.0',
    manifest:'653d674b2bd1f008ddb4ff66f741962607cd9bd1194669d5f4c573026893df03',
    package:Object.freeze({path:'PACKAGE_MANIFEST.json',bytes:6861,
      sha256:'9ac245bea0bdf3aaf57815e058aea1f006c475962fc4dbae5c95cf8471ef3044'}),
    columns:Object.freeze(['provider_date','source_timestamp_utc','observation_state',
      'archive_current','display_status','display_value','display_reason','qc_flag',
      'qa_flag','revision_count','source_observation_refs','source_query_refs'])});
  function staticDescriptor(d) {
    need(d && /^(?:[A-Za-z0-9_-]+\/)*[A-Za-z0-9_-]+\.(?:json|md)$/.test(d.path) &&
      Number.isInteger(d.bytes) && d.bytes > 0 && d.bytes <= limits.bundle &&
      /^[a-f0-9]{64}$/.test(d.sha256), 'Invalid static candidate descriptor');
    return d;
  }
  function staticDate(date) {
    need(typeof date === 'string' && /^\d{4}-\d{2}-\d{2}$/.test(date) &&
      Number.isFinite(Date.parse(date)) && new Date(date).toISOString().slice(0,10) === date,
      'Invalid SNOTEL provider date');
    return Date.parse(date);
  }
  function staticCell(cell) {
    need(cell && typeof cell === 'object', 'Missing SNOTEL field-presence wrapper');
    const variants = cell.representations || [cell];
    need(Array.isArray(variants) && variants.length > 0 && variants.length <= 256,
      'Invalid SNOTEL field variants');
    for (const v of variants) need(v && typeof v.present === 'boolean' &&
      (v.present ? Object.hasOwn(v,'value') : !Object.hasOwn(v,'value')),
      'Invalid SNOTEL field presence');
    return variants;
  }
  function admitSnotelStaticRecord(record, archiveIds) {
    need(record && Object.keys(record).length === snotelStatic.columns.length &&
      snotelStatic.columns.every(k => Object.hasOwn(record,k)), 'Invalid SNOTEL record shape');
    const time = staticDate(record.provider_date);
    need(record.source_timestamp_utc === new Date(time + 86400000).toISOString().replace('.000Z','Z').replace('T00:','T08:'),
      'SNOTEL END timestamp mismatch');
    need(typeof record.observation_state === 'string' && record.observation_state &&
      typeof record.display_reason === 'string' && record.display_reason, 'Missing SNOTEL state/reason');
    const current = staticCell(record.archive_current), qc = staticCell(record.qc_flag);
    staticCell(record.qa_flag); // QA is evidence, never an independent display veto.
    if (record.display_status === 'PLOT_NUMERIC') {
      need(record.observation_state === 'OBSERVED' && Number.isFinite(record.display_value) &&
        current.every(v => v.present && v.value === record.display_value) &&
        qc.every(v => v.present && ['V','E','K','N'].includes(v.value)), 'Invalid SNOTEL numeric decision');
    } else need(['DISPLAY_GAP','UNRESOLVED_HOLD'].includes(record.display_status) &&
      record.display_value === null, 'Invalid SNOTEL nonnumeric decision');
    need(Array.isArray(archiveIds) && archiveIds.length > 0 && archiveIds.length <= 256 &&
      new Set(archiveIds).size === archiveIds.length, 'Invalid SNOTEL archive inventory');
    for (const field of ['source_observation_refs','source_query_refs']) {
      const refs = record[field];
      need(Array.isArray(refs) && refs.length <= 256 && refs.every(r => Array.isArray(r) &&
        r.length === 2 && Number.isInteger(r[0]) && r[0] >= 0 && r[0] < archiveIds.length &&
        Number.isInteger(r[1]) && r[1] >= 0), 'Invalid SNOTEL source reference');
    }
    need(Number.isInteger(record.revision_count) && record.revision_count >= 0 &&
      record.revision_count === record.source_observation_refs.length &&
      record.source_query_refs.length > 0, 'Invalid SNOTEL revision/query counts');
    return record; // Keep all wrappers, variants and references, including null/absence.
  }
  function decodeSnotelStatic(body, sensorIdentity) {
    need(body?.candidate_id === snotelStatic.id && body.sensor_identity === sensorIdentity &&
      /^\d+:CA:SNTL$/.test(body.station_triplet) && Number.isInteger(body.signed_depth) &&
      body.signed_depth < 0 && Number.isInteger(body.ordinal) && body.ordinal > 0 &&
      sensorIdentity === `${body.station_triplet}|SMS:${body.signed_depth}:${body.ordinal}`,
      'SNOTEL exact sensor mismatch');
    need(body.unit_native === 'pct' && body.duration === 'DAILY' && body.period_ref === 'END' &&
      body.source_timestamp_mapping === 'END D -> (D+1)T08:00:00Z; fixed GMT-08',
      'SNOTEL static unit/time contract');
    need(JSON.stringify(body.columns) === JSON.stringify(snotelStatic.columns) &&
      Array.isArray(body.rows) && body.rows.length > 0 && body.rows.length <= limits.table_rows &&
      body.rows.length * body.columns.length <= limits.table_cells, 'SNOTEL static row/column bound');
    staticDescriptor(body.provenance);
    const begin = staticDate(body.provider_date_begin), end = staticDate(body.provider_date_end);
    need(begin <= end && body.provider_date_end <= '2026-09-20', 'SNOTEL static interval');
    let previous = -Infinity;
    const records = body.rows.map(cells => {
      need(Array.isArray(cells) && cells.length === body.columns.length, 'SNOTEL static row width');
      const record = admitSnotelStaticRecord(Object.fromEntries(body.columns.map((k,i) => [k,cells[i]])),body.archive_ids);
      const time = staticDate(record.provider_date);
      need(time > previous && time >= begin && time <= end, 'SNOTEL static row order/interval');
      previous = time;
      return record;
    });
    return {contract:snotelStatic.id, records, raw:body};
  }

  // read(descriptor, signal) must return JSON only after exact wire byte/hash
  // verification. The controller supplies its existing bounded transport.
  class SnotelStaticReader {
    constructor(read, cache = new BundleCache()) { this.read = read; this.cache = cache; this.context = null; }
    async load(signal) {
      const pkg = await this.read(snotelStatic.package,signal);
      need(pkg.candidate_id === snotelStatic.id && Array.isArray(pkg.files) && pkg.files.length === 23,
        'Invalid SNOTEL compact package');
      const files = new Map();
      for (const d of pkg.files) { staticDescriptor(d); need(!files.has(d.path),'Duplicate static path'); files.set(d.path,d); }
      const get = async path => { need(files.has(path),'Static file not in pinned package'); return this.read(files.get(path),signal); };
      const manifest = await get('package/CANDIDATE_MANIFEST.json');
      need(files.get('package/CANDIDATE_MANIFEST.json').sha256 === snotelStatic.manifest &&
        manifest.candidate_id === snotelStatic.id && manifest.candidate_status === 'LOCAL_REVIEW_ONLY' &&
        manifest.source_contract === snotelStatic.contract && manifest.source_adapter_version === snotelStatic.adapter,
        'SNOTEL candidate contract mismatch');
      const schema = await get('package/CANDIDATE_SCHEMA.json'), policy = await get('package/DISPLAY_POLICY.json');
      need(manifest.schema_sha256 === files.get('package/CANDIDATE_SCHEMA.json').sha256 &&
        manifest.display_policy_sha256 === files.get('package/DISPLAY_POLICY.json').sha256 &&
        JSON.stringify(schema.history_columns) === JSON.stringify(snotelStatic.columns) &&
        policy.candidate_id === snotelStatic.id, 'SNOTEL schema/policy mismatch');
      const sample = await get('package/sample/SAMPLE_MANIFEST.json'), stations = await get('package/sample/STATIONS.json');
      const resolution = await get('package/sample/REFERENCE_RESOLUTION.json');
      need(sample.source_candidate_manifest_sha256 === snotelStatic.manifest && sample.candidate_id === snotelStatic.id &&
        stations.candidate_id === snotelStatic.id && resolution.candidate_id === snotelStatic.id &&
        Array.isArray(stations.stations) && stations.stations.length === sample.station_count &&
        stations.stations.length <= 32, 'SNOTEL sample catalog mismatch');
      const context = {pkg,files,manifest,schema,policy,sample,stations,resolution};
      const seen = new Set(); let count = 0;
      for (const st of stations.stations) {
        need(/^\d+:CA:SNTL$/.test(st.stationTriplet) && !seen.has(st.stationTriplet) &&
          typeof st.name === 'string' && Array.isArray(st.sensors), 'Invalid SNOTEL catalog station');
        seen.add(st.stationTriplet); const sensors = new Set();
        for (const s of st.sensors) {
          need(s.sensor_identity === `${st.stationTriplet}|SMS:${s.signed_depth}:${s.ordinal}` &&
            Number.isInteger(s.signed_depth) && s.signed_depth < 0 && Number.isInteger(s.ordinal) && s.ordinal > 0 &&
            s.unit_native === 'pct' && s.depth_unit_native === 'in' && !sensors.has(s.sensor_identity),
            'Invalid SNOTEL catalog sensor');
          sensors.add(s.sensor_identity); count++; this.included(context,s.history);
        }
        this.included(context,st.native_metadata_provenance);
      }
      need(count === stations.sensor_count && count === sample.sensor_count && count <= 98,'SNOTEL catalog counts');
      signal?.throwIfAborted(); this.context = context;
      return context;
    }
    included(context, descriptor) {
      staticDescriptor(descriptor);
      const d = context.files.get('package/sample/' + descriptor.path);
      const original = context.manifest.files.find(f => f.path === descriptor.path);
      need(d && original && d.bytes === descriptor.bytes && d.sha256 === descriptor.sha256 &&
        original.bytes === d.bytes && original.sha256 === d.sha256, 'Unbound SNOTEL sample member');
      return d;
    }
    async cached(d, signal) {
      signal?.throwIfAborted();
      const key = snotelStatic.manifest + ':' + d.path + ':' + d.sha256;
      let body = this.cache.get(key);
      if (!body) { body = await this.read(d,signal); signal?.throwIfAborted(); this.cache.set(key,body,d.bytes); }
      return body;
    }
    async history(stationTriplet, sensorIdentity, signal) {
      const c = this.context; need(c,'SNOTEL catalog not loaded');
      const st = c.stations.stations.find(s => s.stationTriplet === stationTriplet);
      const sensor = st?.sensors.find(s => s.sensor_identity === sensorIdentity);
      need(sensor,'Unknown exact SNOTEL sensor');
      const body = await this.cached(this.included(c,sensor.history),signal);
      const decoded = decodeSnotelStatic(body,sensorIdentity);
      const provenance = await this.cached(this.included(c,body.provenance),signal);
      need(provenance.candidate_id === snotelStatic.id && provenance.station_triplet === stationTriplet,
        'SNOTEL provenance identity mismatch');
      for (const r of decoded.records) for (const [slot,index] of r.source_query_refs)
        need(provenance.archives[body.archive_ids[slot]]?.query_ledger?.[index], 'Missing SNOTEL query reference');
      signal?.throwIfAborted();
      return {...decoded, context:{...c, provenance}};
    }
    async evidence(history, record, signal) {
      need(history.records.includes(record) && history.context &&
        history.context.manifest === this.context?.manifest, 'Unowned SNOTEL record');
      const c = history.context, observations = [];
      for (const [slot,index] of record.source_observation_refs) {
        const archive = history.raw.archive_ids[slot], link = c.resolution.archives[archive]?.observations;
        const d = link && c.files.get(link.path);
        need(d && d.sha256 === link.sha256 && d.bytes === link.bytes,'Unbound SNOTEL observation sidecar');
        const encoded = await this.cached(d,signal), cells = encoded.rows?.[index];
        need(encoded.archive_partition === archive && Array.isArray(encoded.columns) && encoded.columns.length <= limits.table_columns &&
          Array.isArray(cells) && cells.length === encoded.columns.length, 'Invalid SNOTEL observation encoding');
        const pairs = [];
        for (let j=0;j<cells.length;j++) {
          const key = encoded.columns[j], cell = encoded.dictionaries?.[j]?.[cells[j]];
          need(typeof key === 'string' && !['__proto__','constructor','prototype'].includes(key) &&
            Number.isInteger(cells[j]) && cells[j] >= 0 && cell && typeof cell.present === 'boolean',
            'Invalid SNOTEL observation dictionary');
          if (cell.present) pairs.push([key,cell.value]);
        }
        const observation = Object.fromEntries(pairs);
        need(observation.sensor_identity === history.raw.sensor_identity && observation.provider_date === record.provider_date,
          'SNOTEL observation identity/date mismatch');
        observations.push(observation);
      }
      signal?.throwIfAborted();
      return {observations, queries:record.source_query_refs.map(([slot,index]) =>
        c.provenance.archives[history.raw.archive_ids[slot]].query_ledger[index])};
    }
  }

  // Retain compact bodies only; expanded rows exist for the selected popup.
  // Byte accounting is serialized compact UTF-8, not an asserted heap-byte count.
  class BundleCache {
    constructor() { this.entries = new Map(); this.bytes = 0; }
    get size() { return this.entries.size; }
    get(key) {
      const entry = this.entries.get(key);
      if (!entry) return undefined;
      this.entries.delete(key); this.entries.set(key, entry);
      return entry.body;
    }
    set(key, body, bytes) {
      need(Number.isInteger(bytes) && bytes > 0 && bytes <= limits.bundle, 'Cache byte bound');
      const previous = this.entries.get(key);
      if (previous) { this.bytes -= previous.bytes; this.entries.delete(key); }
      while (this.entries.size && (this.entries.size >= limits.cache_entries ||
        this.bytes + bytes > limits.cache_compact_bytes)) {
        const oldest = this.entries.keys().next().value;
        this.bytes -= this.entries.get(oldest).bytes; this.entries.delete(oldest);
      }
      this.entries.set(key, {body, bytes}); this.bytes += bytes;
    }
    clear() { this.entries.clear(); this.bytes = 0; }
  }
  return {limits, tables, descriptor, decodeScan, decode, BundleCache,
    snotelStatic, staticDescriptor, admitSnotelStaticRecord, decodeSnotelStatic, SnotelStaticReader};
});
