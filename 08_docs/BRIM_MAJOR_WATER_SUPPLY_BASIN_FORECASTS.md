# BRIM major water-supply basin forecast geometry

## Phase B2 boundary

Phase B2 establishes static geometry, literal product mapping, reservoir
crosswalks, and supporting links. It does not fetch or calculate forecast
values, build BRIM HTML, implement the Phase C browser controller, or migrate
the current Local layer. Geometry assembly never implies forecast arithmetic.

The retained inventory has 23 objects:

- 15 authoritative assembled California FNF watersheds;
- four California geometry-only dissolves (`BDBC1_FNF`, `SACC0_FNF`,
  `VNSC0_FNF`, and `MLIC0_FNF`);
- two generalized operational CBRFC geometries; and
- two context-only USGS WBD HUC2 geometries.

Twenty-one objects have forecast/internal roles and two are context-only.
Twenty distinct geometry IDs are product-mapped. `BDBC1_FNF` remains an
internal, unthemed index component; the two HUC2 records have no forecast
authority, hover authority, thematic domain, or product mapping.

## Final producer contract and product mapping

`00_config/major_water_supply_basin_product_mapping.csv` is the intended
future browser authority. The geometry catalog retains its older
`forecast_key`, `product_type`, and `source_url` fields only for reproducibility
and rollback; browser code must not derive keys from LIDs or treat those legacy
fields as the multi-product mapping authority.

The mapping has exactly 54 literal producer keys in deterministic order:

- 51 CNRFC records copied verbatim from the frozen BRIM Live roster;
- `CBRFC:GLDA3:APR_JUL_WSUP`;
- `CBRFC:GLDA3:WATER_YEAR_INFLOW`; and
- `CBRFC:LKSA3:LOCAL_INTERVENING_MONTHLY`.

The CBRFC feed contract is schema `1.0`, product ID
`cbrfc_major_water_supply_forecasts`, roster version
`cbrfc-colorado-river-v1.3.0`, in the exact order above. Both GLDA3 products
map to `GLDA3_CBRFC_MODELED_UPSTREAM`; LKSA3 maps to
`LKSA3_CBRFC_LOCAL_INTERVENING`. There is no total-Lake-Mead live record.

Every mapping explicitly records source family, product type/family, forecast
period, geometry, applicable views and measures, default and allowed popup
metrics, producer-link roles, and display order. CBRFC records use the
producer-supplied `source_url`, `retrieval_url`, `summary_url`, and
`archive_url`; BRIM must not construct those URLs from LIDs.

The LKSA3 producer object is one structural record containing an ordered
12-month array, not 12 forecast records. Phase C will select one current
map-eligible month, never sum monthly values, retain the ordered series in the
popup, preserve raw/corrected date-label provenance, and exclude expired
months from thematic domains. The reviewed July 1, 2026 source printed January
2026 between December 2026 and February 2027. Producer override
`CBRFC_LKSA3_LOCAL_JANUARY_ROLLOVER_2026` corrects only that raw label to
January 2027 after confirmation from the June 1 archive; volume, percentage,
units, row order, and raw label provenance remain unchanged.

## California geometry and reservoirs

The authoritative California inputs remain
`cnrfc_fnf_delta_wgs84.rds` (15 assembled FNF polygons) and
`cnrfc_basins_wgs84.rds` (detailed components used by BDBC1). A five-character
LID is not a geometry key. The exact BDBC1 recipe includes `SHDC1_FNF` plus
`WHSC1`, `BDBC1`, `CWAC1`, `CWCC1`, `COTC1`, `KWKC1` (Keswick local), and
`RDGC1` (Clear Creek at Igo). SACC0 combines BDBC1, ORDC1, HLEC1, and FOLC1;
VNSC0 combines FRAC1, EXQC1, NDPC1, and NMSC1; MLIC0 combines SACC0 and
VNSC0. These are reviewed display footprints, not complete contributing
watersheds; visible Sierra gaps are intentional.

`00_config/major_water_supply_basin_reservoir_crosswalk.csv` contains exactly
14 reviewed California associations: SCSC1→SCC, TMDC1→TRM, SHDC1→SHA,
CEGC1→CLE, ORDC1→ORO, HLEC1→ENG, FOLC1→FOL, NDPC1→DNP, EXQC1→EXC,
FRAC1→MIL, PFTC1→PNF, ISAC1→ISB, CMPC1→PAR, and NMSC1→NML. Michigan Bar and
Bend Bridge remain forecast-location context, not reservoir names. MHBC1,
BDBC1, and the three index geometries have no arbitrary reservoir association.

All 19 Phase B1 authoritative and display WKB SHA-256 values are frozen in
`major_water_supply_basin_phase_b1_geometry_hashes.csv`. Phase B2 fails if any
changes. The old 15-feature Local display cache is read only for input hash
provenance and is never rewritten.

## Colorado operational geometry

The durable source manifest contains the smallest reviewed CBRFC source set:
the five `CBRFC_Basins` shapefile components plus `CBRFC_Outlets.zip` for
independent QA. The approximately 302 MB zones archive is deliberately absent.
The source has 537 EPSG:4269 basin polygons and 534 EPSG:4269 outlet points.
All new downloads match the final reconnaissance inventory exactly:

| file | bytes | SHA-256 |
|---|---:|---|
| `CBRFC_Basins.shp` | 2,799,040 | `92caac614a4f07299e46015b3cf330ec3571e0469964c9c4da854a09701ad379` |
| `CBRFC_Basins.shx` | 4,396 | `84e09490f5c0c903c6c0c892fb01fdda4908dde1f36652ceb65ad5985168e5bc` |
| `CBRFC_Basins.dbf` | 164,330 | `91141e9a7852ad5c74c2d78dcb6736dda9b7dae4714ed750fcfc674e97175e2f` |
| `CBRFC_Basins.prj` | 165 | `0b9041e921d9ebb43247d314608fe9e38a0b008ee793067fc1806199ea1fb9dd` |
| `CBRFC_Basins.qpj` | 298 | `c99ea35ee06ac74ff9f60dd23334a8b310f274e0e5f7fb37a9c0a3a632cfcfbd` |
| `CBRFC_Outlets.zip` | 33,521 | `5feb9cead490211c2170267959aebc1cc44eff6c9429f86f81220a9374980250` |

The basin family reports server timestamp 2023-04-14 21:11:10 UTC; the
outlet archive reports 2026-02-19 17:27:04 UTC. The config records individual
retrieval timestamps and official CBRFC URLs.

### GLDA3 generalized modeled upstream supply area

`GLDA3_CBRFC_MODELED_UPSTREAM` unions all 203 rows where `reg == "UC"`.
It is a reproducible thematic union of the CBRFC Upper Colorado modeled basin
set, not a directly published cumulative GLDA3 watershed. The source files
contain no documented explicit upstream-routing graph. The union excludes the
Great Divide Closed Basin and portions of the below-Glen-Canyon corridor that
are included in HUC2 14.

The union is one valid part, area 107,832.882184 square miles. Display keep
`0.05` reduces 9,154 vertices to 459 and serialized geometry from 148,682 to
9,085 bytes. Area becomes 107,800.852232 square miles, a -0.02970333% change,
inside the 0.05% guardrail. GLDA3 outlet `GLDA3L_F` intersects the union
(0 m distance under the documented 250 m coordinate tolerance); no buffer is
applied.

### LKSA3 generalized local-intervening-flow source area

`LKSA3_CBRFC_LOCAL_INTERVENING` unions the official groups `MEAD` (10),
`LITCOL` (26), `VIRGIN` (22), and `MUDLV` (21): 79 distinct LC basins. It
includes no UC, Gila, or below-Mead basin group. This is a reviewed thematic
representation because no exact published local-intervening polygon was found.
It represents contributing land below Glen Canyon and above Lake Mead, not
storage, evaporation, withdrawals, releases, return flow, or any other
mass-balance term.

The source union is one valid part, area 59,009.120809 square miles. Display
keep `0.05` reduces 7,345 vertices to 369 and serialized geometry from 119,775
to 7,646 bytes. Area becomes 59,011.181209 square miles, a +0.00349166% change,
inside the 0.05% guardrail. LKSA3 outlet `LKSA3L_H` intersects the union
(0 m under the 250 m coordinate tolerance); no buffer is applied.

The unsimplified local union deliberately retains one 1.486-square-mile
interior gap. The one-time mapshaper `keep=0.05` display simplification fills
that gap. This is recorded by geometry ID as a display-only exception: no
explicit hole-fill operation runs, the authoritative source keeps and measures
the ring, the display retains its single polygon part, and the total area
change remains inside the CBRFC guardrail. It is not a general ring-removal
policy.

The deferred QA-only total-Mead recipe is `reg == "UC"` plus the four local
groups. It resolves 282 basins and 166,842.002990 square miles. It is not a
retained geometry and has no forecast value.

## HUC2 context

The original production archives are copied checksum-identically to the
realistic workspace with their literal `HU2` filenames. Documentation and
labels use HUC2. WBD 14 has SHA-256
`673df784ad15e4bd006e6a5ed3d869294f07abbbedec105c6e3cf9bb66498935`;
WBD 15 has SHA-256
`c9cc5e2254d2ed18a4d4e6a089e6f0621ed3506a9ee95704c4608f0bf24c9420`.
Each contains exactly one matching EPSG:4269 HUC2 polygon.

Every benchmark starts from the original archive geometry. Keeps 0.005, 0.01,
0.02, and 0.05 all remain valid one-part/no-hole geometries. Rendered comparison
supports one consistent `keep=0.01`: HUC2 14 reduces 109,538 to 1,097 vertices
with -0.00428448% area change; HUC2 15 reduces 156,601 to 1,568 vertices with
+0.00385383% area change. No smoothing, bridging, filling, or normalization is
applied. HUC2 15 retains its expected eastern/southeastern California extent.

## Supporting links versus forecast provenance

`major_water_supply_basin_related_links.csv` supplies the future popup heading
“Related reservoir and operating links.” Exactly one `geometry_id` or
`geometry_scope` is populated per row. It contains reviewed CDEC FNF and
reservoir pages, the applicable USACE California plots index, and Bureau of
Reclamation operating/condition pages for Glen Canyon/Lake Powell and the
Lower Colorado/Lake Mead. Reclamation—not the California USACE plot model—is
used for GLDA3 operations. These are supporting links only; no producer CBRFC
forecast URL is duplicated into the registry and no live reservoir value is
fetched.

## Processing, QA, and outputs

Run only the focused path:

```r
source("run_build_map.r")
preprocess_major_water_supply_basin_geometry()
source("qa/test_major_water_supply_basin_geometry.R")
```

CBRFC source geometry is transformed from EPSG:4269 to EPSG:5070 for selection,
dissolve, topology, area, and outlet QA. The retained unsimplified output is
transformed to the existing EPSG:3310 contract; every display is simplified
exactly once from that retained geometry and transformed to WGS84. California
processing remains unchanged: originals use keep 0.20, reviewed derived
dissolves use keep 0.10 and their narrow, derived-only cleanup/normalization.

The focused outputs are:

- `04_processed_data/rds/major_water_supply_basin_geometry_3310.rds`;
- `04_processed_data/cache/latest/major_water_supply_basin_geometry_map.rds`;
- inventory, mapping, source/checksum, selector, hash, geometry/simplification,
  hole/part, outlet, HUC2 benchmark, deferred-candidate, resolution, and input
  provenance CSVs under `04_processed_data/qa`; and
- five PNGs under
  `04_processed_data/qa/major_water_supply_basin_geometry_maps`.

Source-only QA validates all static contracts without external data. Runtime QA
requires 23 valid nonempty outputs in preserved order, 54 mappings to 20 IDs,
exact selectors, unchanged 19 Phase B1 hashes, unchanged source/old Local-cache
hashes, the local gap exception, outlet proximity without buffers, complete
HUC2 candidates, and no forecast-arithmetic fields. Full HTML and browser
acceptance remain Phase C work.

## Phase C Ops Live behavior

The Ops Live layer `Major Water-Supply Basin Forecasts` embeds the single
23-feature WGS84 geometry product and joins live records only by the 54 literal
`forecast_key` mappings. CNRFC and CBRFC use independent requests, validation,
accepted snapshots, errors, and refresh state; a failed latest request retains
that source's prior accepted browser-session snapshot.

The default is California forecast basins / Water year / Forecast volume. The
configuration matrix exposes only the approved California forecast-basin,
California forecast-index, Colorado, and California–Colorado comparison
combinations. The forecast-basin view covers Shasta, Trinity, and principally
west-slope Sierra water-supply forecast basins. Lake Mead
Local selects one nested current map-eligible month and never calculates a
monthly total. HUC2 14 and 15 are optional outline-only context and never enter
forecast joins, hover, or thematic domains.

The short-range card control is labeled `Accumulation period`, with `3-day
total`, `5-day total`, and `10-day total` options; deterministic mode exposes
only the first two. Standalone legend, hover, popup, and accessibility wording
uses `3-day total volume`, `5-day total volume`, or `10-day total volume`.
`Median ensemble` and `Deterministic` are short-range Product 2 forecast types
only. Product 9 has one `forecast_statistic=median` volume, presented as a
median ensemble forecast; Product 7 has one
`forecast_statistic=50_percent_exceedance` volume. Neither long-range product
has a deterministic alternative. Changing away from Short range normalizes the
shared control to the destination period's first valid measure, so a stale
deterministic selection cannot resolve against a water-year, April–July, index,
or Colorado record. `Percent of median` remains a separate historical-reference
ratio and does not describe the forecast statistic.

Forecast popups remain lazy and use a summary-first design. Water-year and
April–July values are grouped into compact product cards, short-range median
and deterministic volumes share a three-row comparison table, and Lake Mead
Local presents the current month before a native disclosure containing the
ordered 12-month outlook. Related official links follow the forecast summary.
Source timing, archive links, correction evidence, technical identifiers, and
geometry methodology remain available under a collapsed `Details &
provenance` disclosure. Primary timestamps are formatted consistently in
Pacific time while their original values remain in semantic `time` metadata.

Water-year and April–July cards mirror the same three primary positions:
`Forecast volume`, `Percent of mean`, and `Percent of median`. The current CNRFC
Product 7 payload omits `percent_median`, even though the official headline
publishes it; the empty third position therefore says `Not included in accepted
feed`, not `Not published`, and links to the selected official forecast. `Not
published` is reserved for an explicit producer metric state or missing reason.
No missing percentage is inferred or represented as zero. The consumer accepts
an additive direct `percent_median` plus `metric_state.percent_median` only as a
complete all-18 Product 7 contract. That contract activates the April–July
measure for California forecast basins and indices; the comparison measure also
requires a direct MLIC0 and GLDA3 April–July value. Water-year comparison still
omits median. April–July `normal_average_volume` remains secondary context
labeled `Mean reference volume`, below the aligned cells. Primary controls,
legends, hover, popups, comparison labels, and accessibility text consistently
use `Percent of mean` and `Percent of median`; producer terms such as `Percent
average`, `Percent mean`, `Percent median`, `Median Forecast`, and `50%
Exceedance` remain available only in source provenance. Lake Mead Local retains
its supported monthly volume and percent-of-median structure rather than
inventing a mean statistic.

Volume remains kaf internally and uses a square-root display scale over current,
visible, metric-level map-eligible values. California forecast basins require at
least five eligible values; comparison requires both values; a one-feature or
equal-value selection uses the stable midpoint tone. One legend uses kaf when
its maximum is below 1,000 kaf and otherwise uses MAF. Percent displays use the
fixed classes `<50`, `50–74`, `75–89`, `90–109`, `110–124`, `125–149`, and
`>=150` percent, preserving direct source values without calculating missing
percentages. The rich lower-left control/legend card reuses the shared BRIM
close, dock/undock, drag, collision, stack, and initial-position lifecycle.

Colorado display state is product-specific. Powell April–July is expired after
July 31 and remains an outline by default; `Show ended-period values for
reference` may display its direct published value with subdued fill and a
distinct dashed outline without changing producer `map_eligible`. Powell water
year themes only `GLDA3_CBRFC_MODELED_UPSTREAM`; Lake Mead Local themes only
`LKSA3_CBRFC_LOCAL_INTERVENING` using the single current nested month. The
unselected operational geometry is an identity/reference outline and never
inherits the selected product value or reports an accepted CBRFC snapshot as
unavailable. HUC2 remains context-only in both modes.

The rich shared card uses native radio groups for the four views and each
view's finite period/product and measure choices. Short-range forecast type and
accumulation choices appear only in that product family. California indices
have direct Sacramento Valley, San Joaquin Valley, and Central Valley buttons;
only the selected index geometry is present in the thematic layer, avoiding
overlap masking. California forecast basins retain one controller-owned
15-basin selection with an alphabetized searchable checklist, All/None,
per-basin locate controls, and an explicit `Auto-zoom to selected basin`
preference. Filtering updates retained polygon visibility, styles, domains,
hover interpretation, the legend, and official link without refetching or
creating layers.

The card header shows compact independent text-and-icon health badge buttons.
Each button opens `Data status & sources`, reveals and focuses its RFC subsection,
and exposes an expanded state plus a descriptive tooltip without putting roster
IDs in the compact header. Each source subsection reports health, browser
acceptance time, payload generation time, schema, roster, accepted/expected
record count, family health, latest refresh outcome, and retained-snapshot use.
It clearly distinguishes the BRIM Live canonical payload from the official RFC
source and repeats the selected official product link. The contained
horizontal/wrapping thematic legend identifies the selected basin/index/product,
period, metric, unit, eligibility count, and current/reference state. It omits a
normal ramp when no current value is eligible. A direct `Official CNRFC forecast`
or `Official CBRFC forecast` link continues to follow the current selection.

## BRIM Live Product 7 percent-of-median amendment boundary

The official Product 7 page contains exactly one inline `chart.setTitle`
headline of the form `Median Forecast: … | … of Mean | … of Median`, tied to the
validated Product 7 title, year label, LID, and issuance time. The current BRIM
Live parser captures all three values but retains only headline captures 2 and
3; capture 4 is omitted from the record and signature. The producer amendment
must parse capture 4 with the existing strict percent-token parser, publish it as
`percent_median`, and publish a parallel `metric_state.percent_median`. It must
add the direct token to semantic signature material and advance the parser
diagnostic from `cnrfc_prod7_semantic_v1` to `cnrfc_prod7_semantic_v2`.

The field is additive: the 51 forecast keys, order, identities, product types,
and geometry-free boundary do not change, so schema `1.0` and roster
`cnrfc-major-water-supply-v1.1.0` can remain unchanged under the documented
unknown-additive-key policy. Producer validation must require the field/state
pair for all 18 Product 7 records, retain a directly published zero and values
above 100, use null plus explicit missing state/reason when the unique headline
token is absent, and reject malformed or duplicate/ambiguous headline values.
Freshness, expiry, source-time regression, last-known-good, controlled-date
bootstrap, and same-issue-revision rules must mirror the other direct Product 7
metrics. Neither producer nor consumer may calculate the percentage from volume
or a reference value.

`major_water_supply_basin_component_manifest.csv` is also the sole browser
configuration authority for displayed composition formulas. The popup labels
these explicitly as **BRIM geometry assembly**, never as forecast arithmetic.
The reviewed geometry formulas are BDBC1's eight-component union, `SACC0 =
BDBC1 + ORDC1 + HLEC1 + FOLC1`, `VNSC0 = FRAC1 + EXQC1 + NDPC1 + NMSC1`, and
`MLIC0 = SACC0 + VNSC0`. They describe retained map footprints only. No
official SHDC1/PITC1 forecast formula was verified, so none is displayed; live
volumes, percentages, and index values remain direct producer records.

## Rollback and migration

Phase B2 is additive. Rollback restores the prior 19-row catalog and Phase B1
builder/output pair; the frozen Phase B1 per-feature hashes prove equivalence.
The old Local cache stays untouched. Phase C should switch browser authority to
the 54-row mapping only after rendered/browser acceptance, while retaining the
catalog fields temporarily for comparison. No CBRFC union may be relabeled as
an exact published forecast watershed during migration.
