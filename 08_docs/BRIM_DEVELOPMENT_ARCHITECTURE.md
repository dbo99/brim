# BRIM development architecture

## Purpose

This document records durable application and development contracts. It does not replace current code, registries, schemas, or focused tests; those remain the highest technical authority.

## Application boundary

BRIM is a self-contained Leaflet HTML application built primarily through R/htmlwidgets. It combines:

- Local embedded/reference datasets;
- External on-demand web services;
- Ops Live/current-condition feeds;
- upload/measure/draw utilities;
- legends, filter/explorer cards, hover, and popups.

These families have different data-loading and lifecycle contracts. Do not move a layer between them merely to simplify implementation.

## Registry-driven design

A user-facing layer should have one maintained definition for:

- identity/display name/order/group;
- source and semantic key;
- style tokens;
- legend/filter/search behavior;
- hover/popup composition;
- counts;
- lifecycle ownership;
- source/provenance links.

Map styling, legend swatches, filter ordering, category labels, and counts should derive from the same definition. Avoid duplicate constants and geometry-order-dependent styling.

## Descriptive layer catalog boundary

`08_docs/catalog/BRIM_LAYER_CATALOG.csv` is a descriptive metadata mirror only (`CATALOG_AUTHORITY=DESCRIPTIVE_ONLY`). Its approved application consumers are bounded build-time projections: the retained read-only M04 Layer Explorer fallback receives a compact safe subset of descriptive fields, while BRIM Guide uses only reviewed descriptive implementation markers to enrich otherwise runtime-derived Product records. Neither browser surface fetches the CSV.

The Layer Explorer is observational only (`CATALOG_RUNTIME_CONTROL=NONE`). It does not create or determine map layers, order, default visibility, layer-control registration, controllers, lifecycle, clear/reset behavior, labels, legends, popups, status/freshness, or map-product requests. Existing R/Leaflet/htmlwidgets/JavaScript construction and controllers remain authoritative (`RUNTIME_AUTHORITY=UNCHANGED`). The focused parity test maintains an exact allowlist for the build-time read-only adapter and continues to forbid catalog consumption by runtime-control, preprocessing/cache, and BRIM Live paths.

The current catalog and `qa/test_descriptive_layer_catalog.R` parity test retain the six-record HUC proof (`huc2`, `huc4`, `huc6`, `huc8`, `huc10`, and `huc12`) and the four-record heterogeneous proof: ordinary Local `gw_bull118`, External `EXT143`, Ops Live `ops_u_s_drought_monitor`, and custom-controller `usgs_streamgages`. The tested descriptive population also includes 16 diversity-selected records spanning ordinary and custom Local layers, shared and custom-loader External resources, and hosted-vector, raster/image, observational, and forecast Ops Live products.

The resulting 26-record population exercises the unchanged 15-field descriptive schema across simple overlays, shared search/filter/card behavior, combined and virtualized custom controllers, tiled and image services, bespoke External loading, product-specific status/error lifecycles, and nontrivial clear/reset ownership. This establishes `SCHEMA_DIVERSITY_PROVEN=YES` for the selected records and permits future descriptive population to proceed incrementally without another proof-only schema worker.

Catalog and Layer Explorer coverage is explicitly partial. The current 26 records do not represent every BRIM layer. This boundary does not claim that all 280 visible BRIM Products are cataloged or validated, authorize catalog-driven rendering, replace custom controllers, resolve External lifecycle gaps, normalize Ops Live status semantics, or confirm producer-side contracts. The proof-role classification remains separate from each record's catalog architecture.

## BRIM Guide foundation

BRIM Guide is one build-time-compiled, embedded browser surface owned by:

- `03_functions/leaflet_guide_helpers.r` for runtime-inventory adaptation, validation, current-profile projection, compact authored Guide content, asset embedding, and htmlwidgets registration;
- `03_functions/js/leaflet_brim_guide.js` for the V4 shell, deterministic search, shallow browse, detail/history/focus behavior, and the small `window.BRIM_GUIDE` host API;
- `03_functions/css/leaflet_brim_guide.css` for styles namespaced under `#brim-guide-root`;
- `05_map_build/04_build_portatreasure2_core_map.r` for compilation after the actual `OVERLAY_GROUPS` and `MAP_DISPLAY` inclusion state are known.

The startup overlay and Guide share the small application-identity seam in `03_functions/leaflet_loading_helpers.r`; identity wording is not independently maintained in browser code. The existing Tools-panel Layer Explorer entry opens BRIM Guide when its host API is available. The prior 26-record descriptive modal remains a read-only fallback and retains its tests and authority boundary.

Guide basic Product coverage is automatic for every included visible Product in the current build. Product existence and ordinary paths derive from current runtime/build authority before embedding:

- Local Products reconcile actual visible `OVERLAY_GROUPS` to `LOCAL_LAYER_REGISTRY`; label-only groups are explicit non-Products, and the two CalSim geometry children reconcile to one visible Product;
- External Products derive from `external_service_catalog.csv` rows visible in the External panel;
- Ops Live category/subgroup/name definitions are parity-validated from current `addOpsLayer()` definitions, with durable Guide keys and build flags supplied by the compact identity seam next to the centralized Ops definition owner;
- basemaps derive from `pt_base_groups()` and the small current Tools set derives from its existing visible controls.

Local registry keys and External layer IDs remain the primary stable IDs. Ops, basemap, and tool Products use compact durable keys near their existing structured authority. IDs do not depend on display order, counts, or profile. Paths derive from the same runtime grouping structures and present the Local point category as `Monitoring Sites/Records`. `BRIM_LAYER_CATALOG.csv` may enrich a matching Product but cannot create, activate, hide, or suppress it.

The repository currently has one actual output profile: `default`, naming the existing `MAP_DISPLAY` plus `OVERLAY_GROUPS` build. Projection occurs before the Guide bundle is embedded. The projection contract removes excluded records, aliases/search content, relationships, Quick Access membership, and count contributions rather than hiding them in browser state. Do not add a runtime profile selector or invent DOI/public/custom publication policy without an authoritative repository profile mechanism.

The V4 shell provides A Explore, B Methods & Guides, C Resources, and D Updates; Home reset, Escape/close/focus restoration; exact-path Product detail; verified stable-ID Quick Access; and controlled browse by primary subject and data/guidance mode. Search is deterministic and gives precedence to exact titles and reviewed aliases before paths, subject/mode/family, provider/program, concise source-supported summaries, related Resource titles, and conservative typo recovery. Runtime IDs, raw URLs, controller variables, and DOM text are not semantic search inputs.

Guide I1 is read-only with respect to map state. Product detail reports the exact BRIM path, but `show_on_map` and `configure_on_map` are deferred to Guide I2; Guide metadata contains no layer/controller callbacks. The legacy Notes surface remains independently reachable from the map and from Methods & Guides. Initial Articles, Resources, Updates, and Quick Access are deliberately compact rather than a rich-content migration.

New Product onboarding extends current runtime authority rather than a parallel Guide inventory: add the Local registry/controller group, External catalog row, Ops `addOpsLayer()` definition plus its stable Ops identity, or reviewed basemap/tool definition as appropriate. Focused Guide tests must then prove stable-ID/path parity, projection, search boundaries, payload size, and descriptive-catalog independence. Keep the browser implementation to one namespaced JS source, one namespaced CSS source, and the existing R compile seam; do not introduce a frontend build, production Python compiler, duplicate controller registry, or runtime Guide-data request.

## Semantic features and geometry components

Normal UI counts represent semantic user-facing features unless a layer contract explicitly chooses another primary unit. Geometry components/parts remain internal QA and rendering detail.

Bounds, selection, chips, and zoom operate on the complete semantic feature across all components.

## Shared Local Reference interaction contract

The accepted Local Reference framework supports layer-specific enablement of:

- concise safe hover;
- structured/tabbed popup cards;
- semantic counts;
- category filters;
- single- and multi-valued facets with OR semantics within a facet;
- optional layer-specific quick views that set an approved facet value;
- autocomplete and removable named-feature chips;
- staged Apply and immediate Auto modes;
- separate Auto-zoom and Zoom to results;
- detached/docked cards;
- idempotent layer-off/Clear Local/Clear All teardown.

New Local Reference layers should extend the shared registry/controller/helpers rather than fork them. Category filters are added only when the source supports meaningful stable categories; search/selection may be the primary control for otherwise neutral layers.

Filter-aware Local Reference labels use the same applied semantic and geometry
IDs as the rendered map; staged state is never a second visibility truth.
Cache records supply stable semantic/component anchors, while the controller
only selects among them. See
`08_docs/features/LOCAL_REFERENCE_SEMANTIC_LABELS.md`.

California ACECs use this same contract. Their category filter is intentionally
hidden because all 238 current source records share one designation type;
meaningful filtering instead uses value-family, planning-framework, and
spatially derived current field-office-context facets. The unchanged ACEC
source administrative-unit coding remains separately visible in the popup;
field-office intersection is not presented as management responsibility. The
six value-family colors are
centralized in the ACEC facet registry: one applied family uses its thematic
color, while the default, DRECP-only, and multi-family states use the neutral
ACEC style. See `08_docs/features/ACEC.md`.

## Popup contract

- Compose trusted BRIM-owned structure from individually escaped values.
- Suppress empty labels, rows, sections, and tabs.
- Use accessible semantic controls and keyboard operation.
- Keep header/tabs visible; use content-driven height up to a viewport cap and internal scrolling only for genuinely long content.
- Recalculate after disclosure open/close and meaningful width changes.
- Tab changes do not pan/zoom or recreate geometry.
- Narrative interpretation/policy language requires traceable provenance and attribution; BRIM's own voice should be limited to neutral data, map, currency, access-verification, and limitation statements.

## Lifecycle and ownership

Every layer/controller must have one authoritative ownership model for all Leaflet objects and UI resources.

Layer-off and the relevant clear actions must remove:

- base/filtered/replacement geometry;
- selected/hover highlights;
- popups/tooltips;
- cards and temporary DOM;
- LayerGroups/FeatureGroups added to the map;
- listeners, observers, timers, animation frames, and deferred callbacks;
- stale registry/controller references and staged/applied selection state.

Teardown is idempotent and safe after partial initialization. Repeated off/on cycles must not duplicate geometry, cards, or listeners.

## Styling and color semantics

Color meaning is layer-specific. Agency colors are opt-in only when verified management agency is the approved primary color basis. Trail/unit identity, status, recommendation, region, or neutral styling may be more appropriate.

Missing/shared/unknown states need explicit stable treatment. Colors and patterns are provisional until reviewed on BRIM basemaps.

## Management and provenance

Designation, administration, local management, partners, BLM role, office responsibility, and data stewardship are separate fields. Publication or spatial intersection alone does not prove management authority.

User-facing narrative and management claims retain evidence, verification date, scope, and confidence. Technical provenance should be available without overloading the initial popup.

## Performance and payload

- Precompute stable display fields during processing/cache stages rather than doing joins or geometry work on hover.
- Keep browser payloads compact; do not ship full research workbooks/registers.
- Use bounded on-demand loading for External layers.
- Preserve complete retrieval before swap; failures should retain the prior successful snapshot where applicable.
- Avoid duplicate geometry, labels, hidden copies, and unbounded listeners.
- Test realistic HTML size, browser responsiveness, and narrow viewports.

## Build and environment separation

Author in the lean source repository, integrate/test in `codex_ship`, and deploy to production only after merge and backup. See `BUILD.md`.

## Testing standard

A feature is not complete until focused tests prove:

- exact identity/join/count contracts;
- source-value preservation;
- style/filter/search/chip/zoom behavior;
- hover/popup safety and accessibility;
- idempotent lifecycle/clear behavior;
- sibling cache/layer integrity;
- realistic mounted-browser behavior;
- human visual acceptance;
- production smoke acceptance after deployment.
