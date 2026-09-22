# BLM well inventories: NOC and Albion

## Independent assertions and source history

NOC is a well inventory supplied, according to the maintainer, by an
agency-level hydrologist. The drilling mix has not been established. Its
Local-panel name is **GW wells | BLM NOC inventory**, legend title
**BLM NOC well inventory**, source line **Source: BLM National Operations
Center**, and symbol explanation **NOC well record**. These descriptions
identify the inventory source, not universal BLM drilling, ownership or
current monitoring. Raw ownership, driller and submitted comments remain unchanged.
Albion is a later field ground-truthing inventory with limited water-level
sampling. Some physical sites occur in both. An installation record and a
found/not-found field visit describe different assertions; overlapping
locations do not justify merging identities or overwriting either source.
Albion's Local-panel name is **GW sites | 2025 Mojave limited field inventory**;
its legend and authored source description are **Mojave limited field inventory
(2025)**. The panel name contains no parenthesized year. Both inventories'
record counts are added dynamically by the existing UI, never embedded in the
configured names. The popup label **Field-reported basin** describes the unchanged
reported basin value and remains distinct from the spatial Bulletin 118 basin.
Their shared normalizer, distance sidecar and browser helper are implementation
relationships, not evidence that one inventory derives from the other.

The technical layer ID `blm_noc_drilled_wells`, source key `noc_blm_drilled`,
`noc_*` record IDs, API, flags and filenames retain their established identities.
The Local registry and group helper accept historical plain, categorized,
counted and Labels names for both inventories as inputs to their current visible groups. Final
builder registration/count/Labels strings and the independent inline-label
pair in `00_config/config_labels.r` use the same new display name. NOC labels
retain minimum zoom 9; Albion labels retain minimum zoom 10.

The maintainer recalls files in full BRIM raw-data folders with July 6, 2026
dates. This is file-history recollection, not a verified export/acquisition
date or complete chronology. The maintainer states that PII was removed;
the correction work does not certify a privacy audit. Original workbooks
and raw exports remain external and are not published with this document.

The supplied provenance inspection reports that the original and BRIM NOC
workbooks have the same worksheet, 288 populated rows, 62 columns and stored
nonempty cell values, but different file bytes. It does not establish identical
formatting, package metadata or historical editing steps. The NOC CSV shares
the header order and row count; lossless workbook-to-CSV export equivalence
has not been claimed. Historical Albion workbook/CSV results remain evidence
of the earlier state, not competing corrected authority.

## Inputs and preserved identities

The ordinary normalizer is `02_preprocess/18_blm_groundwater_well_inventory.r`.
NOC uses the existing CSV finder under the active project's `01_raw_data/blm/`,
including `NOC_BLMdrilled.csv`, with the existing UTF-8/Latin-1 reader and
normalization rules. Both source columns headed `Attachments` are retained
through distinct cleaned column names. Raw attributes remain separate from
display fields. In the inspected export, the existing broad coordinate rule
excludes OBJECTID 19121 (Sagebrush), explaining 288 raw versus 287 retained
rows; no record-specific exclusion or permanent NOC count constraint is added.

`NOC_Albion_fields.csv` keeps its legacy filename and required auxiliary role.
It contains two independent field-name lists (20 NOC and nine legacy Albion
entries), not site observations or a row-aligned field crosswalk. Its original
single-byte encoding, including a nonbreaking space, is preserved. It is
recorded in QA; popup fields are explicitly authored in the normalizer and
are not dynamically selected from that list. Its author's exact intent and
creation history are unverified.

Albion normalization requires these exact reviewed source files:

- `00_config/blm_well_inventory/brim_mojave_fieldcheck_corrected_master.csv`
- `00_config/blm_well_inventory/report_table_a2_confirmed_monitored_well_results.csv`
- `00_config/blm_well_inventory/brim_mojave_fieldcheck_corrections.csv`

They are byte-preserved payloads from the accepted September 1, 2026 focused
Mojave handoff. The master owns final values and geometry; the dedicated
measurement table owns carried monitoring measurements; corrections own
historical identifier/action lineage. Missing or contradictory corrected
inputs fail before product writes. There is no legacy `albion_rev1` or
retained-NOC fallback. `final_site_uid` supplies `record_uid`; joins use keys,
never row positions. Historical BRIM identifiers, report numbers and Field
Maps numbers remain separate. Field Maps numbers are not asserted to be
globally unique, and the supplied source has no Field Maps GlobalID/GUID.

All master fields and namespaced measurement/correction fields survive in
the dedicated Albion intermediate. The combined map intermediate uses the
established compact common schema plus two Albion observation booleans,
with distinct basin/provenance content
in its escaped popup. Original-source values are retained separately from
curated display values. Numeric coordinates keep supplied precision, including
co-located but distinct sites; spatial basins are not recalculated here.

## Current-input QA and historical correction actions

`blm_gw_well_inventory_source_summary_latest.csv` accounts for rows entering
the current normalization run. The corrected Albion master is already
deduplicated, so its `duplicate_rows_excluded` is zero: for the accepted
version, 138 current input rows minus zero removals yield 138 map-ready rows.
Input and output totals are counted from the current data, not fixed at 138.
NOC retains its independent coordinate-exclusion accounting.

`blm_gw_well_inventory_duplicate_exclusions_latest.csv` preserves the two
historical `REMOVE_DUPLICATE` actions against the supplied baseline. Those
actions are not removals from the current corrected master. Historical
migration accounts for 132 baseline sites minus two removals plus eight
additions equaling 138 retained sites. Both QA files keep their existing
filenames and columns; their accounting bases remain separate.

## Accepted interpretation and remaining uncertainty

The accepted version contains 138 sites: 48 ordinary wells present, 85 not
found, four unknown/unverified and one spring/spring-box (record 34). That
spring has its own status and symbol; it is not counted as a confirmed well.
The layer total is expressed as sites. The seven separate spring-survey sites
from the broader report are outside this inventory.

Duplicate report records 36, 65, 73, 74, 83, 110 and 115 are excluded; their
retained counterparts are 34, 64, 72, 72, 82, 109 and 114. Records 137–144 are
retained additions. Record 145 remains a distinct accepted physical site with
Field Maps number 160. The reviewed physical-site/legacy rotation across
85–96 is preserved by the corrections, rather than matching displaced labels.

Settled names are TW-2 (118), PW-0 (119) and OW-2 (123); Fenner 1, Amboy 1 and
Cadiz 3 are source lineage only. Record 85 remains Fenner Well. All ten final
name blanks remain blank: 8, 13, 87, 93, 121, 124, 125, 126, 127 and 145.
Neutral `Site <report number>` hover/popup titles are presentation only;
unnamed map labels are suppressed and stale names are not restored.

The human-review set is 121, 124, 125, 126, 127, 135, 136 and 145. These are
retained sites with unresolved attributes, distinct from the four unknown
field statuses. The first five lack defensible final names; 145 also lacks
a complete historical identity. Records 135/136 retain latitude 34.712737,
longitude -115.125172 and the unresolved Chuckwalla naming/reported-basin
versus spatial Fenner Valley conflict. No speculative coordinate correction
is applied; independent geometry/history evidence would be needed.

Contractor-reported basin and spatial Bulletin 118 ID/name remain distinct,
including ten documented disagreements. Spatial basin supplies the existing
operational grouping; the popup exposes both concepts. The 26 master
`monitoring_performed` flags are labeled **Monitoring reported**, separate
from the dedicated table's 17 ordinary-well measurement rows. Depth, total
depth, date and notes come from that table, preserving production, UNK, NR
and blank qualifiers rather than coercing them to zero or invented numbers.

## Validation and regeneration

### Observation evidence and popup presentation

Albion carries two nonmissing logical fields through normalization, the
combined inventory, shared cache preparation and its browser projection:
`water_level_recorded` and `lab_sample_documented`. A missing or invalid
observation schema fails explicitly; it is not presented as zero observations.
The combined union has missing values on NOC rows, and the cache owner removes
these two columns from the NOC child. NOC's browser schema and controls are
unchanged.

`water_level_recorded` requires both a keyed entry in the dedicated A2
measurement table and a finite numeric depth-to-water value. Zero and signed
values remain eligible. Production, UNK, NR, blank/NA and nonfinite values
are not numerical readings. The 26 master monitoring flags are a separate
concept; neither they nor old Table 2 values supply eligibility.
`lab_sample_documented` comes only from the master's explicitly validated
`laboratory_sample_collected` True/False text, converted deliberately to a
boolean while preserving the original source value. In-situ readings, dates
and monitoring participation do not imply laboratory sampling.

The existing legend has one mutually exclusive observation row: **All**,
**Water level recorded**, **Lab sampled**, **Both**. Observation, status and
BLM-distance restrictions intersect; markers, labels and Showing x/y use the
same filtered set. All clears only the observation restriction. Reset and the
existing off/clear/re-add lifecycle restore All. Status totals retain their
existing whole-inventory basis. For the accepted tables, All is 138 sites,
water level is 10 (21, 63, 64, 84, 93, 126, 138, 139, 144, 145), lab sampled
is nine (63, 64, 84, 93, 118, 126, 143, 144, 145), and Both is seven (63, 64,
84, 93, 126, 144, 145). These are regression expectations, not runtime lists.
False means this criterion is not documented in the approved tables; it does
not prove that no measurement or sample ever occurred.

Normal OFF-to-ON Albion activation fits all valid inventory coordinates with
38-pixel padding, excluding the registration point and other layers. Duplicate
activation notifications do not repeat the fit. Filtering, observations, labels,
Reset, Clear and disabling do not navigate; a later user re-enable fits again.
Bounds are not hardcoded; label zoom stays unchanged. Empty/invalid coordinates do not move
the map. NOC and the separate Springs navigation behavior are unchanged.

Routine popups omit Original Table 2 name, Historical BRIM key, resolved
Identity notes and the Review status row. Source fields and correction crosswalks remain intact in
the dedicated intermediate. HUMAN REVIEW sites retain their source-supported
identity notes as **Unresolved attributes**; their names, coordinate/basin
qualifications and underlying `review_status` field are not reinterpreted.
Only the visible status row is suppressed, including its HUMAN REVIEW badge;
the eight source review flags and conditional Unresolved attributes remain. Field findings,
source attribution, basin concepts, measurements and current accepted/neutral
titles remain. **Lab sample documented** says Yes or Not documented. A2 dates
remain Measurement date, never laboratory collection dates. The prior A2/A3
date differences for 63/64 remain unresolved; A3 is not a runtime or ordinary
test dependency, and this feature adds no chemistry or sample-date display.

The maintainer's NO_CHANGE disposition retains records 62 (Wileys Well Rip
Rap) and 79 (Wiley Well), including their accepted identifiers, coordinates
and not_found findings. It is not independent location verification and
authorizes no relocation, merger or corresponding NOC correction.

New popup strings and flags require a separately authorized normalization,
focused cache refresh and HTML build. Generated popups are never patched by
hand. Site keys and geometry remain unchanged, so the accepted distance
sidecar is intended for reuse after verifying complete unique record keys,
matching source keys, WGS84 coordinates within the shared helper's 1e-10
degree tolerance, valid nonnegative distance fields and unchanged bound BLM
geometry. The cache consumer does not equate a normalized file's new path,
mtime or bytes with a new distance run. Preserve the original sidecar's
`blm_distance_run_time`, `input_well_inventory_rds`,
`input_well_inventory_mtime`, `input_blm_lands_rds` and
`input_blm_lands_mtime`; record the new normalization binding separately.
An applicability conflict requires review, not silent distance recomputation
or rewritten provenance.

### Focused checks

`qa/fixtures/blm_well_inventory/acceptance_contract.json` and the focused
source/cache/JavaScript tests record this version's expected values. These
counts and hashes are regression evidence, not universal runtime constraints
on future authoritative NOC exports or BLM geometry updates.

Source structural checks, synthetic pure-function checks and offline DOM
checks do not establish processed-data parity or human visual acceptance.
The source implementation gate runs no real normalization, distance stage,
cache save, final build or browser. Later isolated execution must prove all
14 handoff requirements and unrelated-cache preservation. NOC comparison
against the accepted preceding presentation permits only the exact
`source_display` and `layer_name` constants plus the matching authored Source
row value. Albion comparison permits its exact dataset `source_display`,
`layer_name` and, where present, `source_full` descriptions, the matching Source
row value, and the **Reported basin** to **Field-reported basin** label change.
Basin values and every other popup byte remain exact. The already-removed Review
status row must stay absent. Every other field and named attribute, including
spatial metadata, distances, row names and agr, stays exact. The source QA owns an executable strict
text-delta comparator for subsequent regeneration checks; it does not waive
blank/NA differences or historical/current identity checks. Actual commands, output effects and the five
permitted distance-provenance differences are documented in
[BUILD.md](../../BUILD.md#blm-well-inventory-focused-regeneration).
