# California Desert National Conservation Lands focused pipeline

This directory owns the fail-closed source refresh and derivative build for
`Local → Reference → CA Desert National Conservation Lands`. The broad
reference-layer batch does not own this layer after the focused upgrade.

## Acquisition

The acquisition stage makes no processed or production changes. It reads the
tracked source contract and writes a new immutable UTC-named directory beneath
an explicitly supplied external root:

```bash
Rscript 02_preprocess/71_desert_ncl_pipeline/acquire_authoritative_source.R \
  --output-root /path/to/isolated/01_raw_data/desert_ncl_authoritative_snapshots
```

The source query preserves the package's reviewed `where`, field, geometry,
and EPSG:3310 settings. Layer metadata, count, object-ID, and GeoJSON response
bodies are retained untouched with request definitions, response metadata,
bytes, and SHA-256 hashes. Completion requires exactly 11 unique records,
`NLCS002009` through `NLCS002019`, unique nonblank GlobalIDs, exact object-ID
completeness, polygon geometry, and no empty feature geometry. A changed
semantic universe fails closed and creates `FAILED.json`, not `COMPLETE.json`.

Raw snapshots and later RDS/GPKG/QA/cache/HTML artifacts remain external to
Git. Production is never a pipeline destination.

## Candidate

`build_desert_ncl_candidate.R` reads one completed snapshot, the canonical
tracked research tables, BRIM's accepted unsimplified field-office boundaries,
and current accepted related Local Reference RDS files. It writes only to a new
external candidate directory. The selected two-metre EPSG:3310 display
simplification is topology preserving; the builder requires exact polygon-part
and hole retention, zero invalid/empty records, and no more than 0.01 percent
absolute area change for any unit. Raw geometry remains untouched in the
immutable acquisition snapshot. The untouched snapshot contains 173 parts, 25
holes, and 84,148 vertices; two records have ring self-intersections. The
builder records those source conditions separately, applies `st_make_valid`,
and uses the resulting valid unsimplified 173-part, 32-hole, 84,155-vertex
derivative as the simplification baseline.
