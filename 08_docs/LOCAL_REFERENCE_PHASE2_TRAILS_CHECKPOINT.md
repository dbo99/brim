# Local Reference Phase 2 — Trails narrative and shared popup checkpoint

## Checkpoint status

The focused Phase 2 correction is implemented for the six user-facing
National Scenic/Historic Trails. Trails and Wilderness Study Areas now use one
shared Local Reference tabbed-popup shell. The accepted data, styles, joins,
filters, selection, zoom, hover, count, and lifecycle contracts remain intact.

This is an uncommitted review checkpoint. Production was not modified. Nothing
was staged, committed, pushed, merged, or added to a pull request.

Final isolated build:

- HTML: `PortaTreasure2_core_20260805_153737.html`
- Path:
  `PortaTreasure2_core_20260805_153737.html (generated in the isolated realistic workspace; not tracked)`
- Size: 206,327,861 bytes
- SHA-256:
  `ca7228e2e96f12080fc093ab117cefe7bf3fc31b634f7818590e3a1b8091bc8d`
- Build stage: `build_final_map_only()` after the focused Trails and WSA cache
  work; no broad preprocessor was run.

## Bounded feature inventory

Trails remain exactly six semantic features joined by exact `NLCS_ID` and
represented by 177 line components:

| NLCS ID | Raw BRIM name | Segment | Components | Display name |
|---|---|---:|---:|---|
| NLCS000280 | California Historic Trail | CA00000001 | 114 | California National Historic Trail |
| NLCS000281 | Pony Express Trail | CA00000002 | 6 | Pony Express National Historic Trail |
| NLCS000282 | Old Spanish Trail | CA00000003 | 28 | Old Spanish National Historic Trail |
| NLCS000283 | Juan Bautista de Anza Trail | CA00000004 | 7 | Juan Bautista de Anza National Historic Trail |
| NLCS000284 | Pacific Crest Trail | CA00000005 | 11 | Pacific Crest National Scenic Trail |
| NLCS000285 | Butterfield Overland National Historic Trail | CA00000006 | 11 | Butterfield Overland National Historic Trail |

Stable source contracts remain: semantic feature = `NLCS_ID`; source segment =
`NSHT_SGMNT_NO`; geometry/audit identity = `GlobalID`; raw source name =
`NLCS_NAME`.

WSA remains exactly 63 semantic features and 104 polygon components, with
recommendation totals 4 / 46 / 11 / 2 and unchanged stable-key, source-value,
office-evidence, acreage, category, filter, selection, and zoom contracts.

## California narrative provenance finding

The audited package text was:

> The route network crosses the homelands of many Indigenous nations.
> Interpretation should address Indigenous persistence as well as displacement,
> violence, disease, resource loss, and other lasting effects of mass westward
> migration.

Its complete trace is:

- canonical package origin: `brim_trails_reference.csv`, data row 2,
  `indigenous_context`;
- source-controlled intake row:
  `00_config/local_reference_trails_reference.csv`, data row 2;
- curated replacement:
  `00_config/local_reference_trails_curated_overrides.csv`, data row 4;
- compact provenance row:
  `00_config/local_reference_trails_narrative_provenance.csv`, data row 4;
- source-register claim: `SRC012`;
- source: *Four National Historic Trails Long-Range Interpretive Plan*;
- publisher: National Park Service;
- direct URL:
  `https://www.nps.gov/cali/learn/management/upload/2010-07-22-NationalHistoricTrails-LRIP-FinalDocument.pdf`;
- source scope: multi-trail California/Pony plan with directly applicable
  California Trail material;
- original treatment: unsupported package synthesis with an unattributed,
  prescriptive sentence;
- final treatment: high-confidence attributed paraphrase, verified
  2026-08-05, approved for normal popup display.

The phrase `Interpretation should address` was removed. The displayed
replacement is:

> The NPS interpretive plan describes the overland-trail corridors as crossing
> American Indian homelands and documents resource loss, disease, violence, and
> loss of homeland, while also emphasizing Indigenous survival.

The normal popup exposes the NPS document attribution and direct source link.
It is not presented as BRIM policy or guidance.

## Six-trail narrative audit

All 18 displayed narrative fields—summary, significance, and Indigenous/Tribal
context for each trail—have compact approved provenance. Every source is
directly applicable to the trail or to an official multi-trail plan that
expressly covers it. No direct quotations are used; treatments are
`curated_summary` or `attributed_paraphrase`.

| Trail | Direct evidence | Correction |
|---|---|---|
| California | NPS Four Trails LRIP (`SRC012`) | Removed prescriptive package synthesis; rewrote significance and Indigenous context as attributed NPS paraphrases. |
| Pony Express | NPS Four Trails LRIP (`SRC018`) and NPS Pyramid Lake War interpretation (`SRC053`) | Replaced broad package wording with trail-specific evidence about Paiute lands, water sources, encroachment, and conflict. |
| Old Spanish | Final BLM–NPS Comprehensive Administrative Strategy (`SRC020`) | Removed `Interpretation should` voice; limited claims to documented Indigenous paths, livelihood changes, captivity, and enslavement. |
| Juan Bautista de Anza | 2023 NPS Foundation Document (`SRC027`) | Removed the unsupported package count of more than 70 Tribal communities; retained directly supported pathways, harms, and continuing traditions. |
| Pacific Crest | USFS Foundation Document (`SRC034`) | Removed `Visitor language should` voice; retained the official land-acknowledgement facts as an attributed paraphrase. |
| Butterfield Overland | Final NPS Special Resource Study (`SRC041`) | Removed unsupported characterization of Tribal comments; retained the documented 49-tribe consultation finding. |

Route representation, access caveats, and management summaries retain their
existing source/verification paths. BRIM-authored language is limited to
neutral map, geometry, access, currency, and responsibility caveats.

The compact provenance schema is:

`source_nlcs_id`, `narrative_field`, `narrative_text`, `source_register_id`,
`source_title`, `source_agency`, `source_url`, `source_register_url`,
`source_scope`, `source_applicability`, `text_treatment`, `verified_on`,
`confidence`, `normal_popup_approved`, package origin, override origin, and
`audit_note`.

The full research register is not shipped in the browser payload.

## Shared popup-card architecture

The maintained R helpers are `pt_local_reference_popup_row()`,
`pt_local_reference_popup_section()`, and
`pt_local_reference_tabbed_popup()`. They suppress empty values, labels,
sections, and tabs and escape source values before composing trusted BRIM-owned
markup.

Trails tabs:

1. Overview
2. Management
3. History & context
4. Resources

WSA tabs:

1. Overview
2. Recommendation
3. Management
4. Sources & details

Both layers use the same CSS and JavaScript tab controller. A newly opened
popup starts on Overview; a different feature resets to Overview. Arrow keys,
Home, End, Enter, and Space operate real `<button role="tab">` elements with
correct `aria-selected`, `aria-controls`, tab focus, and visible focus styling.
Tab changes do not pan, zoom, recreate the layer, or interfere with hover.

Empty dynamic history content is omitted. When present, headings are
`Historical significance`, `Indigenous and Tribal context`, or `History and
cultural context`, according to approved populated fields. The former empty
`Significance:` row cannot be emitted.

Popup sizing and scrolling:

- desktop content width: target 430 px, constrained to 400–460 px;
- narrow content width: `calc(100vw - 56px)`, with a two-row tab grid;
- card maximum height: `min(72vh, 620px)`;
- desktop panel height: `min(54vh, 450px)`;
- narrow panel height: `min(50vh, 390px)`;
- header and tabs are sticky; only the panel scrolls;
- initial auto-pan padding: top-left 16/84 px and bottom-right 16/24 px;
- on viewports at or below 520 px, fixed Leaflet controls are hidden only
  while a shared tabbed popup is open, preventing them from covering the
  sticky title and tabs; popup close restores them.

## Shared clickable-control cursor correction

The shared card—not Trails- or WSA-specific overrides—now applies:

- enabled card buttons and `<summary>` controls: `cursor: pointer`;
- disabled card buttons: `cursor: not-allowed`;
- ordinary links retain browser link behavior;
- headings, labels, and popup prose retain normal selection behavior;
- the card, tab panels, and noninteractive regions do not receive pointer
  cursors;
- the surrounding Leaflet map remains `grab`.

The mounted checks measured enabled Trails and WSA tabs as `pointer`, a
disabled fixture action as `not-allowed`, popup title text as non-pointer, and
the surrounding map as non-pointer/normal Leaflet cursor. Keyboard, ARIA,
touch/coarse-pointer, event propagation, tab, popup, hover, zoom, and teardown
contracts remain unchanged.

## WSA presentation-shell reuse

WSA content was reorganized only into the shared four-tab shell. Accepted WSA
recommendation labels and caution, raw `WSA_RCMND`, raw `GIS_ACRES`, compact
hover square-mile conversion, native popup acreage, office evidence, and Red
Mountain source-zero/geometry-derived-anomaly treatment remain unchanged.

The mounted WSA fixture verifies all four tabs, conditional content, Overview
default, keyboard navigation, internal scroll, desktop sizing, two verified
offices, Red Mountain, source-value escaping, no blank headings, map stability,
single-popup behavior, layer-off/reset teardown, and filter-card lifecycle.

## Provisional Trails tokens

The six fixed identity colors remain unchanged and provisional:

| Trail | Stroke |
|---|---|
| California | `#8C510A` |
| Pony Express | `#C51B7D` |
| Old Spanish | `#6A3D9A` |
| Juan Bautista de Anza | `#006D77` |
| Pacific Crest | `#2166AC` |
| Butterfield Overland | `#B54800` |

All use transparent fill, 2.4 px solid stroke, no dash pattern, and a line
legend swatch. Scenic/Historic remains secondary metadata/filtering and does
not alter line pattern.

## Cache safety and payload

The focused Trails refresh changed only the `trails` child from
`f4c724358f9b0be8ec1608866c87e5bbeb6297bb46fc9dcd2eb4b0de2a1176b2`
to
`f013790ed8fe4d0d417ad99b65d5bc3f148f9d5e3d4cdb900e16cc544cd936e5`.
All siblings were unchanged. A later focused preparation produced the same
`f013790e…` child byte-for-byte, so the fail-closed writer correctly stopped as
an idempotent no-op rather than rewriting the cache.

The isolated WSA shell refresh changed only `wildernessstudyarea` from
`601f8608…` to `04dcefd3c0505b0a16572d36c08d32966223ad190d184559085287f051fa110b`.
Trails and every other shared and non-shared cache child were unchanged.

- Trails controller payload estimate: 6,878 bytes.
- Six structured popup strings: 28,593 bytes before widget wrapping.

## Focused verification

All commands passed unless explicitly described as a safety stop:

- R parse of 10 changed R/config/test files — PASS.
- `Rscript qa/test_local_reference_phase1.R` — PASS.
- `Rscript qa/test_local_reference_phase2_trails.R` — PASS.
- `Rscript qa/test_local_reference_wsa_hover_layout.R` — PASS.
- `node qa/test_local_reference_filter_engine.js` — PASS.
- `node qa/test_local_reference_controller.js` — PASS.
- `node qa/test_local_reference_trails_controller.js` — PASS.
- wrapped JavaScript parse of `brim_local_reference_controller.js` — PASS.
- actual WSA source/processed/cache/payload/Leaflet test with isolated paths:
  `Rscript qa/test_local_reference_wsa_not_stated.R` — PASS.
- `git diff --check` — PASS.
- focused Trails cache refresh — original write PASS; later identical-input
  refresh stopped before write by design; direct fresh/cached equality PASS.
- focused WSA cache refresh — PASS, WSA-only child change.
- `build_final_map_only()` — PASS.

Mounted DOM checks passed for Trails and WSA shared tabs, keyboard navigation,
one visible panel, internal scrolling, popup sizing, safe escaping, link/tab
event isolation, cursor behavior, map stability, one active popup, and
lifecycle teardown. The WSA fixture result includes `passed: true` for all
listed assertions.

The final realistic 480 px WSA check measured:

- popup: x 18, y 84.109, width 444, height 464.891, right 462, bottom 549;
- controls while open: hidden;
- selected tab cursor: `pointer`;
- title cursor: `auto`;
- map cursor: `grab`;
- popup-open lifecycle class: present.

The final realistic Trail click was blocked by browser security review after
the final local-page reload. It was not retried or routed through another
surface. The shared CSS/component, mounted Trails fixture, source/controller
tests, final HTML static checks, and final WSA realistic view all verify the
shared behavior; an exact final-build Trail visual remains a manual target.

## Changed source manifest

- `00_config/config_local_reference_interactions.r`
- `00_config/local_reference_trails_aliases.csv`
- `00_config/local_reference_trails_curated_overrides.csv`
- `00_config/local_reference_trails_narrative_provenance.csv`
- `00_config/local_reference_trails_reference.csv`
- `02_preprocess/11_reference_layers_batch.r`
- `03_functions/js/brim_local_reference_controller.js`
- `03_functions/leaflet_layer_local_reference_helpers.r`
- `03_functions/local_reference_interaction_helpers.r`
- `05_map_build/02_cache_blocks/05_cache_final_point_tweaks.r`
- `05_map_build/07_refresh_local_reference_trails_cache.r`
- `08_docs/LOCAL_REFERENCE_PHASE2_TRAILS_CHECKPOINT.md`
- `qa/fixtures/local_reference_trails_hover_dom.html`
- `qa/fixtures/local_reference_trails_popup_dom.html`
- `qa/fixtures/local_reference_trails_source_snapshot.csv`
- `qa/fixtures/local_reference_wsa_hover_dom.html`
- `qa/fixtures/local_reference_wsa_popup_dom.html`
- `qa/test_local_reference_controller.js`
- `qa/test_local_reference_phase1.R`
- `qa/test_local_reference_phase2_trails.R`
- `qa/test_local_reference_trails_controller.js`
- `qa/test_local_reference_wsa_hover_layout.R`
- `qa/test_local_reference_wsa_not_stated.R`
- `qa/artifacts/local_reference_phase2_trails_checkpoint/`

## Screenshot and manual-review targets

Final-build WSA narrow screenshot:

`qa/artifacts/local_reference_phase2_trails_checkpoint/popup_architecture_screenshots_final/10_wsa_bitterbrush_overview_narrow_480.png`

The directory also contains the mounted review set for California Overview,
California History & context, California Resources, Old Spanish Management,
Butterfield Management, a narrow Trails card, typical WSA, two-office WSA, Red
Mountain, and narrow WSA. Only the WSA narrow image above was recaptured from
the exact `153737` build after the security block.

Manual review should confirm:

- the exact final-build Trails card at desktop and 480 px;
- all six provisional colors on BRIM basemaps;
- California/Pony overlap; Old Spanish, Anza, PCT, and Butterfield alignments;
- the six approved narrative attributions and direct resource links;
- Butterfield draft-plan/final-study wording;
- popup opening/closing, sticky header, tab keyboard behavior, and internal
  scrolling;
- chips, Auto, Auto-zoom, Zoom to results, and semantic counts;
- hover/popup map stability and layer-off/Reset/Clear Local/Clear All teardown;
- WSA two-office and Red Mountain popup regressions.

## Remaining decisions

- Accept or revise the six provisional colors after realistic basemap review.
- Confirm Trails maximum zoom and padding during final manual interaction.
- Optionally supplement segment-level management evidence later; do not infer
  it from raw `MNG_AGCY`, geometry overlap, GIS publication, or partner status.
- Complete exact-final-build Trails visual acceptance before checkpointing.
