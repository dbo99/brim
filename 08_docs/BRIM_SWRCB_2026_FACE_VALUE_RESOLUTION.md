# BRIM SWRCB 2026 face-value resolution

## Scope and source roles

The SWRCB-provided 2026 BLM water-right export is the authoritative BRIM
source for:

1. membership in the official 2026 BLM water-right list; and
2. corrected face values for IDs present in that export.

The authoritative records are immutable:

- `01_raw_data/swrcb_water_rights/All_your_water_right_7_1_2026_8_56_30.csv`
- `01_raw_data/swrcb_water_rights/All_your_water_right_7_1_2026_8_56_30.csv.xlsx`

They must not be edited, deduplicated, regenerated, or overwritten. A future
corrected SWRCB export must be retained as a new dated source, registered in
configuration and the source manifest, and audited before the overlay points
to it.

The relevant processing roles remain separate:

- `02_preprocess/16_swrcb_waterrights_pod.r` builds the public statewide and
  BLM-relevant POD/WR products. It carries the public
  `wr_face_value_amount` and `wr_face_value_units`, does not apply the 2026
  correction export, and is not run by `rebuild_core_cache_and_map()`. Its
  audited normal settings are `ALLOW_DOWNLOADS <- FALSE` and
  `REFRESH_EXISTING_CACHE <- FALSE`.
- `02_preprocess/61_update_swrcb_pod_blm_distance_fields.R` computes BLM
  proximity from the existing map-ready SWRCB cache. It does not fetch or
  resolve face values and is also not run by `rebuild_core_cache_and_map()`.
- The authoritative overlay is applied during core-cache construction in
  `05_map_build/02_cache_blocks/04_cache_admin_water_reference_layers.r`.
  The reusable policy is implemented in
  `03_functions/swrcb_face_value_helpers.r`.

Normal core-cache builds therefore encounter a static tracked CSV; the old
duplicate warning was not evidence of a live fetch or recent source drift.
The defective warning and fallback logic are present in the initial v0.38 Git
baseline, commit `3e99921`, so the behavior predates the current correction
branch.
The audited CSV is 506,104 bytes with MD5
`77805f683c036a94e6cfc3534d52ad9e`. The paired XLSX is 174,369 bytes with
MD5 `6599fb78caba1e41a651d0326cd4df4e`. The files and hashes matched in the
source repository, `BRIM_v0.38_codex_ship`, and the production project at the
time of this audit.

## S014142 local authoritative audit

The audit was read-only. It covered the source repository,
`/Users/davidoconnor_mbp22/Documents/BRIM_v0.38_codex_ship`, and
`/Users/davidoconnor_mbp22/Documents/BRIM_v0.38`.

The CSV has 2,279 data rows. The XLSX has one visible worksheet,
`All_your_water_right_7_1_2026_8`, with the same 2,279 data rows plus its
header. A semantic cell-by-cell comparison found no CSV/XLSX differences
after accounting for Excel date and numeric serialization. The workbook has
no hidden rows or columns, formulas, merged cells, comments/notes, defined
names, drawings, tables, external links, hyperlinks, data validation,
conditional formatting, or row grouping that establishes precedence.
Formatting differences on the two relevant rows do not encode a correction.

The two S014142 records are worksheet rows 2253 and 2278:

- row 2253: face value `0.0061`, effective date `12/31/2025`;
- row 2278: face value `0.4`, effective date `1/6/1994`.

The rows share the basic right, source, county, HUC, watershed, and field
office context. Other inconsistent text does not establish which face value
is authoritative. No project-local QA record, source note, email export, or
SWRCB documentation supplies a reviewed correction.

The source-proximate cached public record also does not resolve the conflict.
It reports public `face_value_amount = 0`, while retaining
`ini_reported_div_amount = 0.0061`, `use_direct_div_annual_amount = 0.4`, and
POD `direct_div_amount = 0.4`. That record explains how both positive numbers
can coexist as distinct source fields, but it does not establish which field
the SWRCB-provided workbook intended as its authoritative face value.

Classification: **unresolved**. Neither row order, effective date, maximum,
minimum, status, neighboring IDs, nor the lossy public value is a valid
precedence rule. The tracked reviewed-override table is consequently
header-only.

Resolving S014142 requires either:

- a corrected, dated SWRCB export; or
- a specifically documented authoritative SWRCB decision identifying the
  intended face value for S014142.

An inference is not sufficient.

## Deterministic resolution hierarchy

The resolver applies this hierarchy:

1. If the ID is absent from the 2026 export, use the public value and report
   `SWRCB/CalWATRS public data; ID absent from 2026 export`.
2. If the ID has one distinct nonmissing numeric value, use it and classify
   `single_row`.
3. If duplicate rows have the same numeric value, use it and classify
   `duplicate_rows_same_value`.
4. If the complete distinct numeric set is exactly zero plus one positive
   value below 1 AFY, use the positive value and classify
   `duplicate_zero_truncation_resolved`.
5. Resolve every other conflict only through a validated reviewed override,
   classified `reviewed_authoritative_override`.
6. If any conflict remains, stop the core-cache build before cache saves.
   Public data must never resolve an authoritative conflict.
7. Keep an ID present with no usable value separate as `blank_or_missing`.
   BRIM retains the existing, explicitly reported public-value policy for this
   condition.

The zero-truncation rule is deliberately narrow. It does not resolve zero plus
1, zero plus a value above 1, zero plus multiple positive values, or two
positive values.

The four current conflicts classify as follows:

| ID | Authoritative values (AFY) | Result |
|---|---:|---|
| A022008 | 0, 0.1 | automatic 0.1 |
| S014140 | 0, 0.0153 | automatic 0.0153 |
| S014141 | 0, 0.0061 | automatic 0.0061 |
| S014142 | 0.0061, 0.4 | unresolved build blocker |

The public service is known to reduce at least some positive fractional values
below 1 AFY to zero. That is why it remains an absent-ID fallback, not an
authoritative-conflict resolver.

## Reviewed overrides

Reviewed decisions belong in:

`00_config/swrcb_2026_face_value_conflict_resolutions.csv`

The table may be header-only. It requires:

- normalized unique water-right ID;
- finite nonnegative resolved AFY;
- `resolution_type = reviewed_authoritative_override`;
- a value basis identifying a conflicting authoritative row or a specifically
  documented independent authoritative source;
- a meaningful resolution note;
- a meaningful source reference; and
- an ISO `reviewed_date`.

The resolver rejects blank or duplicate IDs, placeholder/invalid/negative
values, IDs that are not current unresolved conflicts, and values not supported
by the stated authoritative basis. It never changes the raw export and never
erases the original conflict values.

## Provenance and QA

Core-cache construction retains compact provenance for each matched POD row:

- normalized ID;
- authoritative row and distinct-value counts;
- original raw and numeric values;
- original conflict flag;
- automatic-resolution and reviewed-override flags;
- resolved authoritative AFY;
- resolution method;
- source reference; and
- final selected source text.

The browser payload continues to carry only the compact display and filter
fields it needs. Full original conflict rows are written only to QA when
`WRITE_QA` is enabled.

QA reports authoritative source/ID counts, single values, equal duplicates,
automatic resolutions, reviewed overrides, blockers, blank values, absent-ID
public use, affected retained rows, numeric changes from public, and
numeric-equal values whose provenance was corrected. A separate compact
conflict-resolution QA table reports the ID, authoritative/public/selected
values, method, override flag, retained POD count, and source text.

## Production blast radius and downstream consumers

The audited production map-ready cache contained 3,510 rows. All four
conflicted IDs were present, each on exactly one retained POD row:

| ID | POD feature ID | Layer ID | Existing public value | Correct authoritative outcome |
|---|---:|---|---:|---|
| A022008 | 149528 | `swrcb_pod_wr_2096` | 0.1 | 0.1; provenance corrected |
| S014140 | 131113 | `swrcb_pod_wr_330` | 0 | 0.0153 |
| S014141 | 156464 | `swrcb_pod_wr_2335` | 0 | 0.0061 |
| S014142 | 160361 | `swrcb_pod_wr_2458` | 0 | reviewed positive value or build stop |

The change is not popup-only. `face_afy` drives:

- cache and final-map marker radius;
- the final `pt_swrcb_face_bin`;
- Local AFY browser filters;
- AFY legend-bin counts;
- hover and popup value/source text;
- the final self-contained map payload; and
- `face_afy`/`face_bin` QA columns if distance preprocessor `61_` is
  separately rerun.

S014140 and S014141 move from the zero bin to the `>0–1` bin. A resolved
S014142 would also move from zero to `>0–1`, whichever of its two current
positive values is authoritatively selected. A022008 remains numerically in
the same bin but gains correct authoritative provenance. Corresponding marker
sizes and legend/filter counts change exactly with those bins.

The correction does not change retained row count, geometry/CRS, coordinates,
POD feature IDs, water-right IDs, layer IDs, official-list membership,
status grouping, spatial/name screening classification, owner/source/county
fields, the three Local SWRCB layer memberships, or BLM-distance values. If
preprocessor `61_` is later rerun after a successful resolved cache build, its
copied face-value/bin QA fields update but its computed distance fields and
sidecar join schema do not.

## Build implications

The minimum successful downstream rebuild remains:

```r
setwd("/Users/davidoconnor_mbp22/Documents/BRIM_v0.38_codex_ship")
source("run_build_map.r")
rebuild_core_cache_and_map()
```

With the currently audited inputs and header-only override table, that command
is expected to stop at S014142 before the core cache save phase. This is the
intended scientific-safety gate, not a successful production-build state.
Run the source-only fixture test without production data:

```r
setwd("/path/to/BRIM_v0.38_source_repo")
source("qa/test_swrcb_face_value_resolution.R")
```
