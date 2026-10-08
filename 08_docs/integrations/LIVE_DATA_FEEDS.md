# BRIM live-data-feed integration

The live-feed generator is intentionally maintained as a separate public
repository:

`https://github.com/dbo99/brim-live-data-feeds`

The main BRIM source repository is private. The public feed repository
supports free scheduled runners and publication of non-sensitive feed
artifacts.

## Baseline integration choice

Do not embed the live-feed repository or its published data in this private
repository. Keep the repositories side by side for cross-repository work:

```text
Documents/
  BRIM_v0.38_source_repo/
  brim-live-data-feeds/
```

BRIM should depend on documented feed URLs, manifests, schemas, and fallback
behavior—not on an accidental nested working copy.

A Git submodule is intentionally deferred. A simple documented sibling repo
avoids duplicated history and initialization friction. If exact cross-repo
version pinning becomes necessary, record the tested feed commit SHA in this
document or a small interface-version file.

## Interface responsibilities

- Feed repo: acquire, normalize, publish, and monitor live artifacts.
- Main BRIM repo: fetch/read artifacts, render layers, manage UI and fallback.
- Sample feed fixtures may be added under `sample_data/`; production feed
  snapshots should remain outside this repository.

## NBM Snow Levels, six-hour QPF, and accumulated-QPF consumers

Ops Live consumes the external `winter_storm_levels` contract at runtime from:

`https://dbo99.github.io/brim-live-data-feeds/data/winter-storm-levels/winter_storm_levels_manifest.json`

`NBM Snow Levels`, `NBM 6-Hour QPF`, and `NBM Accumulated QPF (0–10 d)` are separate,
lazy Ops Live rows. The
initial BRIM HTML contains their consumer code but no manifest, target,
contour geometry, QPF image, or numeric grid. Enabling any row fetches only its
required manifest; snapshot rows fetch one selected immutable target, while the
accumulator fetches the exact required existing numeric intervals. A failure or
unavailable state in one
feed does not turn off, replace, or reinterpret the other product.

The Snow consumer validates a complete one-cycle bootstrap or two-cycle steady
`1.0.0` inventory and fetches one selected content-addressed GeoJSON target.
The public bootstrap published on 2026-08-19 has 41 current-cycle states:
`f001`, then native six-hour states from `f006` through `f240`. Selected
immutable targets use a bounded in-memory cache; the changing manifest is
rechecked without relying on a stale browser cache.

BRIM preserves `cycle_time_utc`, `valid_time_utc`, and `lead_hours` as the
canonical controller state. User-facing time is derived in the browser with
the `America/Los_Angeles` timezone. The public producer and its publication,
retention, schema, and source logic remain external to BRIM.

The peer rows share one focused controller at
`BRIM.opsLiveTimeControllers.nbmForecast`; the compatibility alias
`BRIM.opsLiveTimeControllers.nbmSnowLevels` references the same object. Its
public selection state and step/select methods are keyed by exact UTC cycle,
valid time, and lead. A committed selection emits
`brim:nbm-time-selection`. The selectable inventory is the union of only the
currently active products' validated manifests. Each state independently holds
an exact Snow target, an exact QPF target, both, or neither; the consumer never
uses a nearby valid time or another cycle as a substitute.

The shared `NBM Forecast Guidance` card exists while at least one peer row is
active. It shows only the active product sections, uses a select control that
scales to the manifest-declared horizon, labels states as `Day N · +lead`, and
adds a restrained near/medium/extended-range confidence cue. The row checkbox
is the sole product toggle. Snow `f001` remains Snow-only. There are no browser
constants for a ten-target, `f072`, forty-target, or `f240` horizon.

### NBM QPF image and exact-hover contract

The `NBM 6-Hour QPF` peer row consumes the public QPF manifest at runtime from:

`https://dbo99.github.io/brim-live-data-feeds/data/nbm-qpf/nbm_qpf_manifest.json`

The QPF contract is `product_id = nbm_qpf`, public schema `1.0.0`, and
deterministic NBM Core CONUS surface APCP. Each image is the native preceding
six-hour precipitation accumulation ending at its `valid_time_utc`; it is not
accumulation from model initialization and is not observed precipitation.

The consumer validates a complete one-cycle `bootstrap` or two-cycle `steady`
manifest plus locked source, palette, spatial, and freshness metadata. The
accepted leads come from each validated manifest cycle and must be unique,
strictly ordered native six-hour periods. When both peers are active, pairing
requires exact equality of Snow and QPF `cycle_utc`, `valid_time_utc`, and
`lead_hours`. Snow `f001` is explicitly Snow-only. A missing, delayed, expired,
or unpublished exact counterpart is shown as unavailable; another cycle or
nearby valid time is never substituted.

Only the selected immutable lossless RGBA WebP is fetched. Its byte count,
SHA-256, WebP decode, and 720 by 733 dimensions are checked before one
noninteractive Leaflet image overlay replaces the prior overlay. The fixed
EPSG:3857 pixels are placed directly in authoritative Leaflet bounds
`[[30,-130],[44.5,-112]]` on `pane_ops_qpf`, below Snow contours on `pane_ops`.
No browser recoloring, cycle autoscaling, numeric interpolation, or palette
reverse-lookup is performed. Default opacity is `0.55`; the shared card exposes
the QPF opacity control only while the QPF peer is active.

WPC QPF hover is not a palette lookup: it queries the authoritative ArcGIS
MapServer at the cursor and displays returned numeric `qpf` attributes. NBM QPF
now follows the same exact-value principle through an immutable numeric
companion on every display target. The browser never derives a value from WebP
colors or class midpoints.

The numeric contract is `nbm_qpf_uint16_le_gzip_v1`: a headerless 720 by 733
row-major little-endian `uint16` grid carried as an explicit RFC1952 gzip
application payload, not HTTP `Content-Encoding`. Stored values are inches at
scale `0.001` and offset `0`; `65535` is NoData and `0..65534` are valid. The
uncompressed payload is exactly 1,055,520 bytes. It uses the same fixed
EPSG:3857 extent, north-to-south rows, west-to-east columns, and pixel-is-area
semantics as the WebP.

For the selected frame only, the default provider resolves the numeric path
from the same validated manifest target, verifies compressed bytes and
SHA-256, recomputes the producer's canonical `forecast_state_id`, explicitly
decompresses gzip, verifies the expanded byte count, and decodes little-endian
cells. Cursor longitude/latitude is projected to the authoritative EPSG:3857
extent before row/column lookup. Outside-grid and NoData cells have no tooltip.
Exact inches, the preceding-six-hour period, and Pacific valid time appear in a
compact crosshair tooltip.

Decoded grids use a three-frame LRU and in-flight request deduplication; only the
selected frame is requested. Stale loads are ignored and unreferenced in-flight
requests are aborted. Selection, cycle, refresh, QPF-off, Clear Ops, Clear All,
and teardown detach listeners/tooltips and release the selected grid. QPF-off
and final teardown also clear the numeric cache. When Snow contours are active,
Snow feature hover owns arbitration, immediately suppresses the QPF tooltip,
and returns ownership on mouseout.

The extension seam `ptNbmForecastRegisterQpfNumericHoverProvider(provider)` is
retained for a reviewed alternate provider. It attaches only through
`provider.attachSelectedFrame(context)`, receives the exact manifest/display/
numeric identity and status callback, and must return an idempotent `detach()`
handle. Registering an alternate provider replaces and clears the default
provider safely.

The legend always uses the locked global QPF class colors. Only the producer's
validated cycle-level cap may change, from the allowed set
`3, 4, 6, 8, 10, 12, 15, 20` inches. Each peer's toggle-off aborts only that
product's in-flight work and removes only its resources. The shared card,
controller aliases, visibility listener, move/zoom listener, and refresh timer
remain until the final active NBM peer is removed; final teardown removes them
idempotently.

### NBM accumulated-QPF zero-storage contract

`NBM Accumulated QPF (0–10 d)` is a browser-only consumer of the numeric companions
already listed by the selected exact-cycle QPF manifest. It introduces no feed
endpoint, producer request, public asset, or stored accumulation product. For
an exact six-hour-boundary window `[a,b]`, it requires every existing interval
ending at `a+6, a+12, ..., b`; one missing, corrupt, identity-mismatched, or
NoData-mask-disagreeing interval removes the accumulated surface and reports
the window unavailable. Partial totals are never rendered.

The consumer keeps a selected-cycle cache of verified compressed numeric
ArrayBuffers and uses six bounded concurrent requests. One cycle-scoped Worker
decompresses intervals sequentially, sums stored uint16 thousandths directly
into a uint32 result, verifies the common NoData mask, and applies a fixed
nonlinear accumulated-precipitation palette. A persistent canvas layer on
`pane_ops_qpf` replaces the prior window atomically. One-boundary changes use
exact integer add/subtract in the Worker; larger jumps recompute from verified
compressed inputs. The numeric result—not palette inversion—owns exact hover.

The shared NBM run selector remains the sole cycle control. Snow and six-hour
QPF retain the single-valid-time controls; accumulated QPF has independent
start/end leads, calendar-first Pacific endpoint labels, Day/6-hour display
density, and Next 24 h / Next 72 h / Next 10 days actions. Scientific duration
is forecast elapsed time, including across daylight-saving transitions. NBM
six-hour QPF and accumulated QPF are mutually exclusive precipitation rasters;
Snow remains independently compatible above either raster.

## Dendra prepared daily soil context (opt-in)

`add_ops_dendra_daily` defaults to `FALSE`; `ops_dendra_daily_index_url` has no
assumed public target. With an explicitly configured reviewed static index, the
ordinary Ops Live assembly registers `Soil moisture | dendra | selected stations`.
Its controller is `BRIM.opsLiveDendra`; the regular Ops checkbox, Clear ops and
Tools' Clear All own activation/removal. This optional row does not add a default
Guide Product or change existing delivery classifications.

`leaflet_ops_live_dendra_helpers.r` injects the preserved map-lab 0.2 display core,
`js/dendra_reader.js` and `js/dendra_layer.js` through the existing Ops entry point.
The unified control uses the accepted moisture change/latest/availability modes,
optional values, exact depth/site-sensor choice and 7/14/30-day windows. The popup
retains daily overview/WY charts, coverage, gaps, latest-point identity, source links
and a verified selected-sensor CSV. Temperature is an explicitly labeled 20 cm
90-day companion, loaded on demand; 60 cm moisture never implies 60 cm temperature.

The index must be `dendra-daily-1.1.0` / `dendra-integration-1` with modern safeguards.
It supplies authoritative R comparison summaries and small recent moisture rows.
Only selected immutable histories are fetched; stream, depth, parameter, units,
policy, generation and file integrity are checked. No Dendra API request is made.
An abortable activation/selection sequence prevents late responses after a sensor
change, popup close, off, Clear ops or Clear All. A failed static request gets at
most one index refresh and a visible unavailable state. Hash membership governs
cache reuse across generations. Each parameter ages against the wall clock;
frozen examples remain frozen. Static hosting must allow distributed HTML CORS.

`05_map_build/dev_sandbox/build_dendra_ops_review.r` builds a focused public-data
fixture using the actual Ops and Tools assemblies. An optional existing public
outline GeoJSON provides map context. It does not run preprocessing or establish
full production-map, real Actions, or public-host acceptance.


### Unpublished Dendra local candidate admission

`js/dendra_reader.js` exposes pure `parseLocalCandidate` and
`parseLocalCandidateSeries` adapters for `dendra-00g-local-candidate-2`.
The former admits the small fixture envelope; the latter admits selected exact
series with the same stream/row contract. Neither performs I/O, installs a feed,
reads Parquet, or connects the candidate to the hosted index lifecycle.
Map coordinates, publication eligibility, producer primary selection and change
summaries are not inferred from this local contract.

Series identity is the pair `export_local_series_key` + `stream_id`. Explicit
`depth_cm: null` / `depth_status: "UNKNOWN"` stays unknown and is labeled
"Unknown depth"; older unspecified-depth wording is retained for older records.
Visible UNKNOWN labels retain supplied orientation. When same-station labels
would collide, they add a stream-ID suffix starting at six characters and
lengthening only to distinguish the peer IDs. Repeated provider IDs in distinct
exports use the supplied export column and, when needed, a unique source-hash
suffix. These fragments are display text only: full stream IDs and export keys
remain unchanged in the model and selection identifiers. Known-depth labels are
unchanged; ordinary UNKNOWN labels do not display full export keys.

Processing provenance accepts exactly `dendra-bulk-daily-2` and
`dendra-bulk-daily-3`. An accepted v2 row requires `RESOLVED_CLEAR`; an accepted v3
row requires `PROVIDER_READY_TO_USE`. These are explicit version/quality pairs,
not interchangeable statuses. Source-quality reasons remain intact without a
new reason-based admission constraint. Existing withheld/missing/unresolved
rules remain unchanged, and unknown processing versions fail admission.

Resolved Percent and VolumetricWaterContent streams consume the producer's
`daily_mean_vwc_percent` unchanged, including zero. Conversion metadata is retained
for provenance and never applied again. Unsupported units fail admission.
Daily status, fixed UTC−08 dates, true day-of-water-year, leap-aligned plot day and
sample diagnostics are retained; fractional positive expected sample counts are
allowed. Missing or withheld rows remain unplotted; absent dates are not filled.

Local station models require explicit exact-series choice when the producer has
not supplied a primary. Shared filter predicates can return qualifying series
without inventing a displayed sensor, and count each station once. Numeric depth
criteria exclude unknown depth; other criteria can include valid unknown-depth
observations. This source/component support does not establish full-map or hosted
acceptance for the unpublished candidate.

### Unpublished Candidate-3 delivery reader

`03_functions/js/dendra_reader.js` additionally exports `createReader(contract,
options)` and `Candidate3Reader` for the explicit
`dendra-candidate3-delivery-1` contract. The factory's native
`dendra-daily-1.1.0` and `dendra-daily-2.0.0` routes return the existing `Reader`;
its native validation, limits and browser callers are unchanged. The pure local
Candidate-2/H1 adapters above retain their closed envelope and unit admission.
Candidate-3 delivery is not relabeled as any of these older contracts.

This component route admits a pinned generation, source binding and station
index, then descriptors for an explicit finite list of exact stream IDs.
`loadCatalog(signal)` reads metadata only. `history(exactSeriesId, waterYears,
signal)` reads the requested bulk WY partitions and, when supplied, the pinned
routine bridge, API generation, prepared daily output and selected stream
lineage/control metadata. It never reads Parquet, acquires observations, runs
producer reconciliation, calculates daily means or evaluates provider QA codes.

The caller supplies `manifest`, optional `bridge`, `selection`, a finite
`components` list of exact `{path, bytes, sha256}` pins, and a bounded
`readBytes(pin, signal, maxBytes)` transport. Locators are opaque allowlist keys;
the reader does not turn original absolute provenance paths into filesystem or
network access. An offline partial mirror must map each exact original locator
to its separately verified copy. The reader verifies bytes and SHA-256 before
use and cross-checks descriptor membership, candidate identity, bridge and
prepared lineage. Omitted requested partitions fail as unavailable, not empty.
No fallback search, provider URL, hosted destination or default fetch is added.

Candidate-3 uses separate 1 MiB generation/source-binding/prepared/WY bounds and
256 KiB station/stream/routine metadata bounds. Selection remains bounded to
1024 streams, 1500 stations, 256 WYs per stream and 93696 selected rows. Serialized
history/state caches share the existing 24-entry/12-MB policy; an oversized entry
fails. Reload and `cancel()` invalidate the snapshot/cache, abort pending reads
and prevent late results from committing. An external signal cancels the current
selection. `getState(exactSeriesId, waterYears)` returns a copy of a completed,
cached selection and fails after cancellation or eviction.

The reader-owned state separates the original source record from its unchanged
chart projection (`date`, `v`, `ok`, `wy`, `dowy`, `x`, sample diagnostics and
display flags). It retains `CANDIDATE3_BULK` versus `SEALED_API` authority, original
bulk record/component/manifest evidence after supersession, API output/seal
lineage, query views and empty intervals, gaps, source checks, producer admission
outcomes, quality disposition, material-change dates and recomputed dates.
Producer material-change declarations refer to the immediate prepared parent;
the browser does not derive them by comparing against the original bulk.
Private QA details remain in this internal state and are not added to ordinary
chart rows, popup fields or DOM attributes.

Only producer-admitted complete API coverage can supersede the bulk view. Missing
prepared days within that coverage fail admission. Successful empty coverage
stays distinct from numeric zero, quality withholding, unqueried history and
an unadmitted replacement that retains prior admitted values. A successful
overlap/source check can have no material changes. Bulk `query_complete` is not
treated as API coverage. Raw zero can remain in a withheld record's evidence
while its display value stays null/unplotted.

Identity remains exact station/stream/export identity, including separate
same-station UNKNOWN depths and no implicit primary or coordinates. Percent ×1
and resolved VolumetricWaterContent ×100 are metadata: authoritative percent
values are used without conversion again. Dimensionless ×1 is retained as
unresolved native-only evidence, with no percent presentation. Fractional finite
positive `expected_samples` is supported; actual `n_*` counts remain nonnegative
integers. Fixed UTC−08 dates, ending-year WY, true DOWY and leap-aligned plot day
remain separate. Bulk v2/`RESOLVED_CLEAR` and v3/`PROVIDER_READY_TO_USE` retain their
distinct admission checks; sealed API provenance is neither alias.

`qa/test_dendra_candidate3_delivery.js` exercises this real exported reader with
a separately pinned immutable partial mirror and independent assertions. Oracle
records do not feed the adapter. Synthetic in-memory corruption and legacy
specimens are separate from producer evidence. This establishes source/component
support only: `dendra_layer`, shared soil controllers, runtime configuration,
public geometry/primary/change-summary policy, CSV/UI wiring and hosted
activation are unchanged and require their own later gate.

## SNOTEL California static candidate component adapter

`soil_moisture_transport.js` explicitly admits the frozen local review contract
`SNOTEL_CA_STATIC_CANDIDATE_1` / `snotel-awdb-history-1`, produced by
`snotel-awdb-offline-1.2.0`. It is separate from the unchanged
`brim-soil-moisture-1` SNOTEL catalog and `sm1-snotel-popup-1` pilot companion.
The compact-package manifest is pinned at
`9ac245bea0bdf3aaf57815e058aea1f006c475962fc4dbae5c95cf8471ef3044`;
its candidate manifest is
`653d674b2bd1f008ddb4ff66f741962607cd9bd1194669d5f4c573026893df03`.
The compact package contains the representative Blue Lakes station and three
full exact-sensor histories, not all 32 California stations / 98 sensors.
Missing package members fail closed; external archive paths are provenance,
never fallback read locations.

The existing controller exposes `registerSnotelStatic(name, packageManifestUrl)`
for an explicit local component caller only. It requires an HTTP loopback URL,
uses the existing bounded byte/hash verifier with redirects refused, and refuses a second SNOTEL
registration in either pilot/static order. There is no configured Product,
R-builder hook, hosted destination or automatic activation. Catalog loading
does not read histories. Exact sensor selection loads one pinned history plus
its station provenance; existing popup cancellation, source epochs, layer-off
and reset ownership remain authoritative. The shared eight-entry / 12 MB
compact-body LRU and 6 MB member cap remain unchanged; histories also have the
existing 50,000-row / 1,000,000-cell bounds. No live/latest observation, primary
sensor, recent change or reference capability is inferred from this archive.

Identity remains `stationTriplet|SMS:<signed inches>:<ordinal>`. Positive depth
labels do not replace signed machine identity. Provider date D maps to source
timestamp `(D+1)T08:00:00Z`, fixed GMT-08 year-round: an instantaneous DAILY/END
boundary, not a Dendra arithmetic daily mean. Chart dates, ending water year,
true DOWY and leap-aligned coordinates use the provider date; the source
timestamp remains separately available in the model/readout.

`soil_moisture_charts.snotelStatic` consumes producer `display_status` and exact
`display_value`. Numeric V/E/K/N decisions, including zero, remain numeric;
gaps and holds remain null trace breaks. QA is not a veto. There is no scaling,
magnitude rejection, interpolation, fill, clipping or latest-wins selection.
The ordinary seven-field chart-row shape is unchanged. `row.raw` retains state,
reason, field absence/null, all flag variants, revision count and observation/
query references; the model retains its pinned manifest/schema/policy/history/
provenance context. The reader can resolve the producer's lossless observation
dictionaries and query ledger on demand without acquiring raw HTTP bodies.
No application-owned sidecar is introduced. The component does not expose a
new CSV action or private provenance UI; the existing pilot CSV is unchanged.

`qa/test_snotel_ca_static_candidate.js <compact-package-root> <saved-SM1-root>`
verifies the frozen inputs and executes the real transport/model/controller
with offline DOM/event doubles. It covers all 14 real fixture cases, all 23,100
Blue Lakes boundaries, source reference reconstruction, malformed-input guards,
exact selection and cancellation. Synthetic corruption probes are identified
separately from producer cases. Pilot/SCAN/Dendra regressions remain in
`qa/test_soil_popup_presentation.js`, `qa/test_soil_water_year_views.js` and
`qa/test_soil_moisture_transport.js`. Component success establishes no full-map,
native-layout, hosted or live-feed acceptance.

## SM1 shared soil-moisture review mode

`ops_soil_moisture_shared=TRUE` plus `ops_soil_moisture_indexes` (named
`scan`, `dendra`, optionally `snotel` static index URLs) opts into the shared
left card. Existing SCAN/dendra enable flags and URLs remain required. SNOTEL
also requires `ops_soil_moisture_snotel_pilot=TRUE`. Defaults remain false/empty;
no production destination or publication workflow is installed.

For an isolated saved-data build, set the R session option
`brim.map_display_overrides` to a uniquely named list of existing `MAP_DISPLAY`
settings before calling `build_final_map_only()`. The central display
configuration applies those explicit values after its defaults each time it is
sourced, including by the final builder. Unknown or duplicate setting names
fail instead of being ignored. The option is absent by default; shared-soil
flags and URLs remain false/empty and ordinary legacy SCAN remains available.
Keep saved-fixture URLs and review settings in the task's separate launch
configuration, and bind the three common network indices plus the paired
dendra native index to their verified saved generation. This override does
not fetch data, change caches, or establish hosted activation.

The native dendra reader resolves relative static index URLs against the
document URL in browsers, while retaining its HTTP(S) and raw-provider guards.
Shared SCAN/SNOTEL popups use viewport-aware width and padding, including when
the map is resized; narrow screens do not reserve the desktop card's gutter.

The ordinary Ops switches own source activation. The common controller uses
BRIM legendCloseout docking and the Local Reference normalization helper.
Network membership is immediate; criteria have independent draft/applied state.
Auto starts on with 200 ms debounce, Auto-zoom off. Apply validates first;
Reset filters preserves networks and Auto preferences. Clear Ops/All resets
controller state and cancels requests; Clear Local is independent.

`soil_moisture_engine.js` implements explicit default, nominal/range and advanced
exact-depth selection with same-sensor age/value predicates. The presentation
intervals in `soil_moisture_depths.js` never change retained millimetre depths.
Unknown BLM metadata disables the unavailable control and never passes an active
BLM restriction. In advanced exact Any/Every mode only, the displayed sensor is
independent of depths admitting a station, and that difference is labeled.
Common modes never substitute incompatible
statistics: SCAN source reference remains in its popup; pilot SNOTEL DAILY reduction
is unresolved; both common recent-change/reference capabilities are unavailable.
Dendra retains accepted 7/14/30 means, exact sensors, daily/WY charts and the
explicit 20 cm temperature companion. Shared popup mode holds its generation
on a history failure rather than independently refreshing only half the view.

Common `brim-soil-moisture-1` indices are independently generated and limited to
256 KiB each, 1,500 stations and 10,000 sensors. `sensor_defaults` factors repeated
metadata; per-sensor fields override those defaults. History selection loads one
hash/byte-bound station bundle (6 MB maximum, eight cached bundles) for SCAN or
pilot SNOTEL, or the unchanged D2A bounded reader for dendra. No query-time raw
observations, BLM GIS, all-station history download or cross-network generation
is introduced. D2A's 256 KiB / 2,000-file / full-stream-diagnostics limits remain.

`05_map_build/dev_sandbox/build_soil_moisture_review.r` builds the actual Ops/Tools
assembly with the normal legend helper and Ops pane. It is a public snapshot
review, not a full production map. The real BLM join is held because identified
`04_processed_data/rds/blm_managed_core_3310.rds` was absent; never substitute a
production/ship copy or synthetic polygon. `soil_moisture_blm_helpers.R` contains
the pure USGS 48/49 EPSG:3310 method and fingerprint cache; synthetic parity is
separate from real-geometry acceptance. Source/prepared snapshots remain intact.


### SM1R1 correction contracts

Common markers explicitly use the established `pane_ops` (560); opaque Local
context remains below them, with tooltip/popup ordering unchanged. The focused
build now calls `pt_add_panes()` directly. Other-depth admission is rendered by
one escaped applied-result helper in both list and actual pointer hover, with
matching value/date/age; pending Auto-off criteria do not leak into the note.

`soil_moisture_transport.js` decodes version-2 SCAN column/row bundles to the exact
original table objects expected by unchanged SCAN plots and CSV. It requires all
five original ordered column schemas. Limits remain 6,000,000 wire bytes and add
12,000,000 decoded JSON bytes, 1,000,000 cells, 50,000 rows/table and 64 columns.
The LRU retains only compact bodies, at most eight and 12,000,000 serialized UTF-8
bytes; decoded rows belong to the selected popup. This is bounded accounting,
not a heap-byte guarantee. Invalid or late responses are not admitted to cache.

`qa/preflight_soil_moisture_products.js` exercises actual controller/transport and
unchanged Dendra reader validation without network. The offline producer requires
its path explicitly and installs a fresh prepared directory only after both
producer and consumer preflight pass. No additional provider fetch, publication,
new station history, or scientific/statistical policy is introduced.

### SM2A archive reader and catalog shards (local review)

The D2A limits above describe version 1 compatibility. `dendra_reader.js` now
strictly dispatches `dendra-daily-2.0.0` and integration 2: complete catalog first,
selected stream manifest and immutable WY history on demand, and full-history CSV
only by explicit request. The native root remains 256000 bytes; catalog/stream
shards 262144, JSON WY partitions 1000000, CSV WY partitions 512000, file inventory
20000, catalog shards 128, selected streams 1024 and years per stream 256.
The reader cache holds at most 24 entries / 12000000 serialized bytes; selected
rows cap at 93696 and full CSV at 32000000 bytes. Limits apply during reads.

The common controller supports `brim-soil-moisture-2` for large Dendra catalogs
with at most 128 hash-bound shards, each capped at 262144 bytes. Source activation
commits only after common shards and native catalog validate together; stale,
malformed, missing or cancelled loads remain an explicit source failure. Search
and Any/Every/exact-depth/age membership use the entire enabled local catalog.
The shared search/filter owner, Ops pane, hover explanations and lossless SCAN
codec remain authoritative. Protected native coordinates cannot fall back to old inventory XY.

The chart defaults to current plus nine prior WYs, with all acquired history on
demand. Neither choice changes storage or computes reference ribbons. Per-stream
cutoffs distinguish later unqueried dates from queried empty days. Companion
selection uses exact metadata and explicit choice for multiple probes; the real
review input still has only the verified 20 cm temperature. Source statistics
and NRCS disabled mean-based capabilities remain unchanged. No active workflow,
production integration, statewide observed coverage or real BLM join is implied.

### Soil presentation and depth selection

The single `soil_moisture_controller.js` owns source membership, draft/applied
criteria, map markers, results, the persistent map key and detachable controls.
The title/actions, source switches and filter-free key sit above the scrolling
control/results region. The card uses the Ops Live visual family with separate
Map display, Depth, Station filters and Results sections; infrequent criteria
remain collapsed. Closing hides both surfaces; the reopen button restores them. Clear Ops
or Clear All destroys both and cancels selected requests; Clear Local does not
own soil state. Disabling the final soil source removes the card, reopen
control, their listeners and their timers through the same teardown owner;
reactivation recreates one card while retaining filter preferences. Local native event containment on the soil card stops propagation
without setting a Leaflet private one-shot click flag. No global event patch is
installed. `soil_moisture_ui.js` supplies escaped presentation, not another state
owner or a second subscription to source changes.

Default selection uses the station's declared primary sensor, including an
unavailable/unknown-depth catalog record. Nominal shortcuts are inclusive
canonical intervals: 50–50.8, 100–101.6, 200–203.2, 500–508 and 1000–1016 mm.
Verified ungrouped catalog depths (150, 250, 400, 600, 750, 800 and 1200 mm)
remain stable singleton steps even before source activation; other actual depths
can supplement this inventory. A step is a filter choice, not a claim that a
source has observations there. Equal handles select a complete
step; different handles select all depths between their real bounds. Surface
is zero, the upper open end has no numeric cap, and unknown depth matches only
when separately included. Native range inputs and From/To selectors share the
same draft. Drag input changes feedback only; release applies when Auto is on.
Auto-off requires Apply. Search/value edits retain the bounded 200 ms debounce.
Changing label units does not change selection. Loaded catalog depths remain in
the step inventory when a source is disabled, so toggles do not move handles.

Range selection requires at least one eligible in-scope sensor. One sensor must
satisfy both value and age. Display preference is an eligible explicit instrument
choice, then the declared primary within scope, then primary rank and stable ID;
never a wettest/driest choice. Advanced exact Any/Every remains available and
retains its separate admitting/display-sensor explanation. Entering range/default
mode clears hidden exact constraints; Reset returns the station-default mode.
Min VWC % and Max VWC % are visible normal controls; blank means no bound.
`soil_moisture_engine.percentVwc()` admits only a current prepared sensor with
`unit: "percent VWC"`, `capabilities.latest_vwc: true` and an eligible finite
0–100 observation. It does not guess native fractions, multiply values or
infer resolution from magnitude. Unresolved records remain catalog-visible
without an absolute bound and fail an active absolute bound. A later fixed
Dendra scale product must supply declared resolution/normalized values through
this boundary; no new producer field names are assumed here.

Dendra remains the primary exploration model: selected daily mean/date and exact
sensor precede a History/Water years view, persistent sensor/parameter and archive
controls, record-fit/0–100% axes, individual years and All/Last 3/Current shortcuts.
Its existing reader, scientific core, full acquired history/CSV, sampling readout,
gap segmentation and source-supported temperature pairing remain authoritative.
Coverage, saved change summaries, source dates/flags and calculation methods are
secondary details. Popup sensor changes do not change the shared map sensor; a
Map/Viewing note identifies differences. User-facing temperature values and chart coordinates are Fahrenheit (two
decimal places for value readouts); validated Celsius rows, native metadata,
quality decisions and CSV exports remain unchanged. Date ticks span the actual
selected daily interval, including temperature and SNOTEL short records.
Temperature remains separate and states
when it does not match the selected moisture depth/orientation.

`soil_moisture_charts.js` presents retained NRCS rows, never computes historical
statistics. SCAN's adapter in `leaflet_ops_live_scan_helpers.r` retains monthly
medians, monthly support/reference criteria, daily percentile min/max bands and
the supplied fallback traces. Its source `water_day` coordinates remain aligned
with its saved reference product; dendra's separate leap-aligned plotting day
must not be substituted. Band/trace selection does not recompute the reference
cohort. Missing underlying reference-year traces are explained explicitly.
Pilot SNOTEL shows only its acquired daily dates/WYs and retained QC/QA, with unresolved
DAILY reduction and unknown sample support. It has no invented historical band.
Chart readouts support pointer and keyboard inspection; native layout, hit testing
and assistive-technology acceptance remain separate from offline DOM tests.

Standard Leaflet keyboard-accessible divIcon markers distinguish dendra
(circle), SCAN (square) and SNOTEL (triangle). The key explains shapes when
multiple sources are enabled; metric color and the hollow/dashed unavailable
state are independent of provider identity. Normal hovers contain only station,
provider, depth, value/date/age and any applied-match explanation. The owned
rich tooltip has a 180–240 px width boundary and three-line hierarchy. Markers
use accessible names rather than native `title` tooltips. The controller tracks
one hover owner, closing it on pointer leave, replacement, redraw, map movement,
popup transition and marker/source removal; shared Clear Ops/All teardown also
unbinds the discarded marker's tooltips/listeners. No global tooltip hiding is
used, and the shared Dendra popup-only renderer emits no legacy map labels.
Popup sensor
labels omit technical IDs; duplicate-depth instruments remain individually
selectable, with IDs retained in technical details and CSV. Source flags,
generation/cutoff timestamps and reduction limitations stay available in
collapsed methods/details. Provider daily values are not relabeled as means.

Map moisture bins remain <10, 10–<25, 25–<50 and ≥50 percent VWC; these are numeric
values, not hazard/normality classes. Wetting/drying consumes only accepted dendra
7/14/30 completed-window versus preceding-window summaries, with ±0.50 percentage
points a valid neutral class. Unavailable/unsupported observations use a distinct
hollow/patterned symbol. The primary row is Moisture, Wetting / Drying and
Context. Coverage/data availability has its own Advanced control/key. Context
remains disabled with a capability reason: no fixed history-sidecar reference
adapter is connected. Existing SCAN popup bands do not authorize map classes.
Future source-supported Context vocabulary is Much below, Below, Near normal,
Above, Much above and No context; no such categories or colors are assigned to
current observations. Dendra/SNOTEL reference capability remains unavailable.
Counts use stations as denominators, explicitly describe
overlapping categories, and cannot be interpreted as land-area proportions.
Observation age uses the current reader clock even for saved previews. Water-year
shortcuts use ending-year WY at fixed PST; rollover never crops acquired prior
years or rebases the reference cohort. NRCS observations retain date-only strings.

Operational label checklist: verify the configured shared/legacy path, actual
index mode/generation, publication status, acquisition cutoff and last observation
separately; retain Saved preview/not-live for replay, SNOTEL pilot while scoped as
a pilot, exact parameter units and unresolved support. Do not remove limitations
merely because flags or URLs might later be activated. Ordinary flags/URLs remain
off/empty until separately authorized hosted integration and review.

### Soil capability dependencies (D3/B17)

Additional SNOTEL years need an authorized producer acquisition/contract with
verified archive coverage. Dendra reference ribbons need a reviewed scientific
definition, cohort/support rules and producer product; they are not a consumer
recalculation. A common seasonal anomaly needs comparable statistic, era, depth
and support across networks. Regional risk/hazard classes, interpolation, spatial
summaries, assimilation and climate interpretation require separate science/data
work. These dependencies do not defer the supported dendra daily/WY, temperature,
coverage, CSV or source-provenance features. Installed soilDB/runtime, current
remote compatibility, publication and production activation are separate gates.


### Optional recent-history hover presentation (deferred)

The shared map hover owner is `redraw()` in
`03_functions/js/soil_moisture_controller.js`, where each applied result binds
its tooltip. The separate legacy dendra renderer already uses `miniChart()` in
`dendra_layer.js` with its validated recent rows; that is not a common producer
contract. No new shared mini-plot, placeholder or hover request is installed.

The smallest later presentation interface would pass the already selected
sensor identity and units, approximately 30 ordered daily dates with their
accepted/missing values, and the acquisition/observation cutoff needed to label
coverage. These are consumer requirements, not proposed payload field names or
a producer schema. The presentation must preserve gaps, show an honest missing
state, and make no history request on hover. Adoption waits for the Live history
contract convergence and source-specific statistic/support decisions.

Additional historical years, universal SCAN traces, reference ribbons, common
anomalies and new Last 3/All products remain deferred to that convergence. Real
On BLM/distance context remains a data-capability task; unknown stays unknown.
A SNOTEL frozen-soil warning requires authoritative wording/source verification;
this UI does not add one. Saved/not-live/pilot labels remain until operational
activation changes the actual product mode.
