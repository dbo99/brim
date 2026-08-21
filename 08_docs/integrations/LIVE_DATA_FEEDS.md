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
