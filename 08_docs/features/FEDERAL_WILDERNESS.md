# Federal Wilderness

## Purpose and scope

`Local → Reference → Federal Wilderness` is BRIM’s interactive reference layer
for congressionally designated Federal Wilderness. It is distinct from
Wilderness Study Areas, proposed or recommended wilderness, and inventory
lands. The layer is a planning and orientation reference; controlling laws,
current agency direction, closures, permits, and field conditions remain
authoritative.

The accepted snapshot contains 197 source components representing 158 named
wildernesses. It retains 194 California components and three intentional
western-Nevada context components. A source component is not synonymous with a
named wilderness: a wilderness can contain multiple noncontiguous or
agency-administered components.

## Authority and identity

The geometry authority is the untouched, checksummed official 197-feature BLM
California source ZIP delivered in the 2026-08-05 implementation handoff. It is
not redownloaded or queried by the browser. The older 192-feature BRIM geometry
is not an authority for the upgraded layer.

- Component identity: normalized source `GlobalID`, stored as `component_id`
  (`blmca-…`).
- Named-wilderness identity: source `FAU_ID`, stored as `wilderness_id`
  (`fw-…`).
- All 16 original source attributes remain unchanged in the derived candidate.
- Canonical aliases and display fields are additive.
- Component, semantic, document, and policy tables remain separate keyed
  lookups. One-to-many documents are never joined to geometry.

The focused pipeline is documented beside
`02_preprocess/68_federal_wilderness_pipeline/`. The accepted display candidate
uses `simplify_keep=0.5`; validity repair and simplification occur only on a
derived copy, with the existing early simplification hook before enrichment. Candidate generation,
processed-RDS promotion, shared-cache refresh, and realistic map build are
separate fail-closed gates.

## Counts and filters

The controller always distinguishes semantic and geometry counts. Its summary
uses the form:

`42/158 wildernesses · 51/197 components`

Managing-agency counts are component counts. The other two semantic facets and
the geographic component facet are explicit:

- Managing agency: BLM, USFS, NPS, FWS.
- Management pattern: single-agency or shared/multi-agency wilderness.
- Designation history: 136 original-designation-only wildernesses and 22 with
  one or more subsequent public laws, derived from the resolved 158-row legal
  validation table.
- Geographic context: California or western Nevada context.

All categories start enabled. Named-feature search uses the official name,
alternate names, package abbreviation/alias, source name, agency, semantic ID,
component ID, `GlobalID`, `FAU_ID`, and public-law identifiers. Punctuation,
slashes, dashes, underscores, and ampersands normalize consistently in the
shared search engine. A named result selects all visible mapped components and
uses the accepted shared zoom behavior.

## Styling and labels

Default styling preserves BRIM’s accepted component-agency colors exactly:

- BLM `#B8860B`
- USFS `#228B22`
- NPS `#54278F`
- FWS `#1F78B4`

“Distinguish named units” is off by default and available only when exactly one
agency is selected. It deterministically hashes `wilderness_id` to a fill color,
applies that fill to every component of the named wilderness, and keeps the
selected agency’s stroke. It restyles existing Leaflet paths and creates no
parallel geometry or large unit legend. Enabling a second agency turns the mode
off and restores the agency fill; it does not turn itself back on.

The inline `lbl` lifecycle is backed by the shared Local Reference semantic
label system. It uses the canonical official name and renders one label per
currently visible named wilderness. The cache retains one ranked point-on-
surface anchor per source component; agency filters select an anchor from the
largest currently visible component, so a shared wilderness never floats over
a filtered-out agency component. `lbl` remains an independent display
preference, and Distinguish named units does not change label membership.

## Hover and popup interpretation

Hover is deliberately compact: official name, selected component agency,
resolved designation year, and the approximate total named-wilderness area in BRIM's
square-mile convention (for example, `Approx. area: 142 mi²`), plus shared or
western-Nevada cues when relevant. It does not substitute a selected source
component's acreage for the named-wilderness total.

The map-ready geometry carries only draw, filter, label, hover, and stable-key
fields. Named-wilderness, component, document, agency, office, policy, source,
and URL-template data are normalized once in the browser payload. Popup HTML is
constructed only when a component is selected; 197 complete popup strings are
not embedded in the geometry cache.

Click popups use the shared responsive, keyboard-accessible tab shell:

1. **Overview** — named and component identity, designation and original law,
   managing agencies, selected agency/unit, state, component count, distinct
   published and component acreages, summary, shared-management explanation,
   and acreage caveat.
2. **Use & access** — area-specific access context plus centrally maintained,
   qualified Wilderness designation guidance and current official links.
3. **Laws & official documents** — direct original designation law, each later
   law separately and chronologically, official pages/plans/maps, Wilderness
   Act/source references, and secondary research in a collapsed area.

Technical IDs, designation-validation evidence, component context, geometry
caveats, and provenance are collapsed. Hover and popup read the same resolved
designation record, whose accepted status totals are 154 `PASS`, four
`PASS WITH DOCUMENTED EXPLANATION`, and zero unresolved. The three High Rock popups identify Nevada, BLM,
Black Rock Field Office (BLM Nevada), Winnemucca District, and BRIM’s western
Nevada context explicitly.

## Lifecycle and regression contract

The layer uses the existing Local Reference controller and the existing
Leaflet group. Filtering removes/adds owned paths inside that group; it does not
clone layers. Layer-off closes owned popups/tooltips, removes all owned paths
and the card, clears pending callbacks, resets filters/search/zoom defaults,
turns distinguish mode off, and leaves no controller-created groups. Repeated
off/on cycles must remain duplicate-free. Clear Local and Clear All exercise the
same overlay-removal path.

Focused QA lives in:

- `qa/test_local_reference_federal_wilderness.R`
- `qa/qa_local_reference_federal_wilderness_candidate.R`
- `qa/test_local_reference_filter_engine.js`
- `qa/test_local_reference_controller.js`

Human visual acceptance of a realistic isolated HTML is required before any
Git checkpoint or release. Production changes require the normal merge,
backup, manifest, rollback, and hash-verification gates.

## Accepted implementation checkpoint — 2026-08-06

Human visual acceptance is complete for the isolated realistic-build candidate.
The accepted implementation evidence is:

- 197 mapped components and 158 semantic named wildernesses;
- component totals of 105 BLM, 75 USFS, 15 NPS, and 2 FWS;
- geographic totals of 194 California and three western-Nevada components;
- derived display geometry at `simplify_keep=0.5`, with 390,055 final vertices;
- untouched, checksummed authoritative source retained separately from every
  derived display, cache, and browser product;
- final Federal Wilderness RDS: 3,728,960 bytes, SHA-256
  `ec83f0499549eaaada2f2b4b40efafb9b1da473ec778a5d6df1d02c9442d46b7`;
- final complete reference cache: 22,657,894 bytes, SHA-256
  `69e4958a3622ae683daef962e7cfe8d106b3808cf28e8601eff4aaebdfb198bc`;
- accepted isolated HTML:
  `codex_ship/06_output/html/PortaTreasure2_core_20260806_113127.html`,
  207,122,419 bytes, SHA-256
  `d70017f42e393199bdc290f7a0ee1087566a75c47646573b0640f5f2c214fedc`;
- legal validation totals of 154 `PASS`, four
  `PASS WITH DOCUMENTED EXPLANATION`, and zero `UNRESOLVED` records; and
- Federal Wilderness filter-card width of 330 px, exactly matching the measured
  Wilderness Study Areas card in both docked and detached desktop tests.

Focused source QA passed through:

- `Rscript qa/test_local_reference_phase1.R`;
- `Rscript qa/test_local_reference_phase2_trails.R`;
- `Rscript qa/test_local_reference_wsa_hover_layout.R`;
- `Rscript qa/test_local_reference_federal_wilderness.R`;
- `node qa/test_local_reference_filter_engine.js`; and
- `node qa/test_local_reference_controller.js`.

Realistic browser acceptance covered the 2160-by-1182 desktop viewport, docked
and detached WSA/Federal cards, 1280-by-720 short-height fallback, 390-by-700
narrow behavior, named-wilderness search, agency filtering and all facets,
staged Apply, Reset, deterministic Distinguish mode, hover square-mile
formatting, official-versus-component popup acreage, corrected designation-law
content, popup tabs and keyboard behavior, responsive popup containment, ten
duplicate-free layer off/on cycles, Clear Local, Clear All, and browser-console
review. The compact hover reports the named wilderness's approximate total area
in square miles; authoritative and selected-component native acreage remain
separately labeled in the popup.

The production workspace and production artifacts remained unchanged through
implementation, isolated build, QA, and human visual acceptance. This checkpoint
does not authorize merge, production synchronization, or deployment.
