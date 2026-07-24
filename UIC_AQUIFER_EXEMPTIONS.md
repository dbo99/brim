# Underground Injection Control (UIC) aquifer exemptions

## Production scope

Production BRIM presents California aquifer-exemption data only as
authoritative, on-demand External layers under:

**External Layers → Energy / Minerals → Underground Injection Control (UIC)**

The full subgroup label defines UIC where users encounter it. UIC is the final
subgroup in Energy / Minerals. Its registry-derived badges remain continuous
with the rest of the External catalog.

The normal map build embeds no UIC geometry or label coordinates and has no
dependency on UIC raw snapshots, candidate products, approved comparison
products, or caches. No UIC service request occurs at map startup.

## Production External rows

Rows appear in this order:

1. `UIC — EPA mapped boundaries` — polygon;
2. `UIC — CalGEM post-primacy approvals` — polygon;
3. `UIC — CalGEM 1983 historic subset` — historic polygon subset;
4. `EPA exemption reference points` — centroid/locator point.

All four rows default off and are fetched only after a user enables them.
Feature counts and retrieval status are derived from the live response rather
than hardcoded.

The point row is an overview aid. Its features are explicitly described as
centroids/locators—not wells, injection wells, or additional exemption areas.
They use simple circles without clustering or map labels.

## Interpretation warning

Aquifer-exemption polygons show mapped surface footprints only. Exemptions may
be limited to particular formations, zones, depths, elevations, or structural
boundaries. A surface intersection does not establish that all underlying
groundwater is exempt. Review the applicable EPA Record of Decision and
supporting documents for project-level interpretation.

The CalGEM 1983 primacy layer is a partial historic shaded subset and is not a
complete map of all 1983 exemptions. Its historic status is communicated in
row text and warnings, not by changing its source outline to a dashed line.
Its associated historic date fields are not exposed because validation found
parse failures and implausible values.

## Authoritative services

- EPA mapped polygons:
  `https://services.arcgis.com/cJ9YHowT8TU7DUyn/ArcGIS/rest/services/Aquifer_Exemptions_Feature_Layer/FeatureServer/1`
  with `State='CA'`;
- EPA exemption reference points: the same service, layer `0`, with
  `State='CA'`;
- CalGEM post-primacy:
  `https://gis.conservation.ca.gov/server/rest/services/CalGEM/Post_Primacy_Aquifer_Exemptions/FeatureServer/0`;
- CalGEM 1983 primacy shaded subset:
  `https://gis.conservation.ca.gov/server/rest/services/CalGEM/Primacy_Aquifer_Exemptions/FeatureServer/0`.

The popup and source-links UI provides the authoritative service/homepage and
regulatory context destinations. It does not expose routine object-ID query
links, an “Inspect source record” action, or raw JSON.

EPA California polygon records currently have blank
`More_Info_Hyperlink` values. BRIM does not invent record-specific decision
URLs. CalGEM legacy FTP documentation values are preserved and labeled
legacy/unverified.

## EPA county-location audit

EPA service layer `2` currently returns four California records. They are four
distinct Round Mountain Oil Field zone records in Kern County:

- `9_207` — Jewett Sand;
- `9_208` — Pyramid Hill Sand;
- `9_209` — Vedder Formation;
- `9_210` — Walker Formation.

All four have `Data_Quality_Category = "County location available only"`,
zero-valued centroid coordinates, and the same full Kern County polygon. The
four IDs are absent from the EPA mapped-polygon and reference-point layers.

Production BRIM therefore omits layer `2`. Rendering the shared county polygon
would visually imply an exemption footprint that the source does not provide.
The records remain available to the pipeline for retrieval, QA, and source
reconciliation.

## External map behavior

The External loader obtains service metadata and the complete object-ID
inventory, then downloads GeoJSON in bounded batches. Refresh builds a
replacement off-map and swaps it in only after complete retrieval. An error
leaves the prior successful snapshot intact. Per-source clear and the normal
External clear action remove the loaded layers.

The shared blue External UIC Explorer provides:

- source rows with geometry type, live count, retrieval time, refresh, clear,
  and polygon-label controls;
- a separate Finder that zooms to records without changing filters;
- a primary Field/project filter;
- Advanced County, Decision year, and Formation/zone filters;
- removable chips, debounced Auto mode, staged Apply, Reset, shown/total
  counts, and Zoom to shown;
- OR logic within a filter and AND logic across filters;
- optional polygon labels at zoom 8 or closer; point labels are omitted;
- official source links and interpretation text without raw service-query UI.

County is standardized only from verified source properties. Deterministic
normalization accepts the 58 California county names and documented
multi-county strings; the exact source string remains available. CalGEM 1983
`AreaName` is an exemption area, not a county, so historic County is left
unavailable and is never inferred from geometry.

### Styles and overlaps

The three polygon sources keep centralized, source-specific, solid outlines
with very low default fill. The shared **Distinguish overlaps** control is off
by default and applies only to polygon records. When enabled, it retains the
source outline and assigns a stable record-ID hash to one of six restrained,
colorblind-conscious fills. Colors repeat and carry no category meaning; the
key states exactly:

`Record colors repeat · no category meaning`

The browser never rebuilds, dissolves, simplifies, or otherwise reconstructs
source geometry to distinguish overlaps.

## Developer/research pipeline

Source:

```r
source("02_preprocess/67_build_uic_aquifer_exemptions.R")
```

Check current services without replacing retained products:

```r
check <- run_uic_pipeline("check_only")
```

Build a complete review candidate:

```r
candidate <- run_uic_pipeline("refresh_candidate")
```

Build a separate review map:

```r
run_uic_pipeline(
  "build_sandbox_map",
  candidate_id = candidate$candidate_id,
  data_variant = "candidate"
)
```

Promote a reviewed candidate to the pipeline's approved comparison baseline:

```r
run_uic_pipeline(
  "approve_candidate",
  candidate_id = candidate$candidate_id,
  confirm = "APPROVE UIC CANDIDATE"
)
```

The workflow preserves immutable raw snapshots and source attributes, repairs
invalid geometry only in processed copies, retains exact overlaps, produces
full and map-oriented research products, and writes schema, geometry,
missingness, link, duplicate-footprint, cross-source, count-reconciliation,
and old-versus-new QA.

Approval requires the exact candidate ID, exact confirmation phrase, and a
passing QA result. It archives the previous comparison baseline and records
retrieval and approval timestamps separately. Check, refresh, sandbox build,
and production External loading can never invoke approval.

This entire workflow is independent of production BRIM. There is no candidate
production-build wrapper, and `build_final_map_only()` never resolves these
products. Generated raw, candidate, approved, archive, QA, and sandbox HTML
remain outside Git under BRIM's external-data policies.

## Production validation

A release check should cover:

1. all four External rows default off and cause zero UIC startup requests;
2. EPA/post-primacy/historic polygon loading, off/on, refresh, failure
   retention, labels, and clear behavior;
3. point loading, simple-circle rendering, no clustering, no point labels, and
   centroid/locator wording;
4. source order, final subgroup placement, full subgroup name, and continuous
   derived badges;
5. Explorer close, detach/redock, responsive overflow, Finder, connected
   filters, Auto on/off, Apply, Reset, chips, counts, and zoom;
6. solid source outlines and the default-off, polygon-only overlap distinction
   with its exact repeat/no-category key;
7. popup warnings and official links, with no raw JSON or object-ID inspection
   action;
8. Measure/Draw coexistence and **Clear external layers** behavior;
9. absence of Local UIC rows, panes, labels, controls, cache reads, startup
   serialization, profiling hooks, renderer switches, candidate notices, and
   candidate-specific output naming;
10. a normal production build:

```r
setwd("/Users/davidoconnor_mbp22/Documents/BRIM_v0.38_codex_ship")
source("run_build_map.r")
build_final_map_only()
```
