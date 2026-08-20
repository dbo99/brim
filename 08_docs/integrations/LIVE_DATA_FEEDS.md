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

## NBM Snow Levels and QPF peer consumers

Ops Live consumes the external `winter_storm_levels` contract at runtime from:

`https://dbo99.github.io/brim-live-data-feeds/data/winter-storm-levels/winter_storm_levels_manifest.json`

`NBM Snow Levels` and `NBM 6-Hour QPF` are separate, lazy Ops Live rows. The
initial BRIM HTML contains their consumer code but no manifest, target,
contour geometry, or QPF image. Enabling either row fetches only that product's
manifest and selected immutable target. A failure or unavailable state in one
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
