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

## NBM Snow Levels consumer

Ops Live consumes the external `winter_storm_levels` contract at runtime from:

`https://dbo99.github.io/brim-live-data-feeds/data/winter-storm-levels/winter_storm_levels_manifest.json`

The `NBM Snow Levels` row is lazy: initial BRIM HTML contains the consumer
code but no manifest, target, or contour geometry. Enabling the row fetches and
validates the two-cycle `1.0.0` manifest, then fetches only the selected
content-addressed GeoJSON target. Selected immutable targets use a bounded
in-memory cache; the changing manifest is rechecked without relying on a stale
browser cache.

BRIM preserves `cycle_time_utc`, `valid_time_utc`, and `lead_hours` as the
canonical controller state. User-facing time is derived in the browser with
the `America/Los_Angeles` timezone. The public producer and its publication,
retention, schema, and source logic remain external to BRIM.

The active consumer registers a narrow controller seam at
`BRIM.opsLiveTimeControllers.nbmSnowLevels`. Its public selection state and
step/select methods are keyed by the actual UTC cycle and valid time, and a
committed selection emits `brim:nbm-time-selection`. This permits a future
paired NBM product to follow the selected valid time without reaching into
Snow Levels DOM controls or geometry internals; it is not a generic product
framework and does not implement paired-product synchronization.

Future NBM QPF pairing must define its accumulation interval separately.
Snow Level is effectively instantaneous at `valid_time_utc`, while a paired
QPF slice may represent an interval ending at that same valid time (for
example, a preceding six-hour accumulation). This consumer does not choose or
implement that QPF contract, and shared selection must not infer it from
integer forecast lead alone.
