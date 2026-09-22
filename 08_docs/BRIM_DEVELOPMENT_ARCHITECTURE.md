# BRIM development architecture

## Purpose

This document records durable application and development contracts. It does not replace current code, registries, schemas, or focused tests; those remain the highest technical authority.

## Accepted-work checkpoint and remaining program

The current layer-reliability work is an accepted-work checkpoint candidate,
not full Phase A completion, a merged-source claim or release approval.
The maintainer accepted the five-product satellite browser behavior and
reported “E54 browser check passes” for focused Radar/QPF finishing. Native
records that say `browser=NOT_RUN` remain separate from that human report.

Outstanding Phase A obligations remain the original national-streamflow
outline/color/legend explanation (A14), broader Ops status/freshness semantics
(A23), specifically unrecovered Springs browser acceptance and cumulative
dense-layer/performance acceptance. Source/installed-library tests do not
supply those human observations. Promoted streamflow refresh/cancellation
repairs do not close A14. Provider-limited displayed frame/accumulation dates,
unqualified WPC issue-time zones and independent image/metadata cycle
association retain the qualifications documented below. They are not newly
verified by this checkpoint.

Phase A reliability work, Phase B water-record/hydrologic work and Phase C
navigation/labels/basemaps/links are sequential development phases, distinct
from historical Guide R17C C1/C2 labels. Remaining A work must receive its
explicit disposition before B starts from accepted merged A main; C follows
accepted B main. The complete inherited B/C inventory and unresolved Mojave
handoff, history-window design, field-office zoom, initial-view direction and
polygon-reference decisions remain open. NBM renaming and the Guide matrix
addition remain deferred; the accepted satellite Ops matrix link already
exists. No excluded feature program is revived.

One checkpoint commit does not imply full phase closure or waive human visual
acceptance. Git integration, merge and final integrated release are separate
approvals. R17C2 selectors and accepted artifacts remain frozen until an
explicit release transaction. Ordinary source/offline QA, installed-library
contract QA and external-input native/build evidence have different scopes;
their commands, fixtures and limitations are documented in DATA.md.

## Application boundary

BRIM is a self-contained Leaflet HTML application built primarily through R/htmlwidgets. It combines:

- Local embedded/reference datasets;
- External on-demand web services;
- Ops Live/current-condition feeds;
- upload/measure/draw utilities;
- legends, filter/explorer cards, hover, and popups.

These families have different data-loading and lifecycle contracts. Do not move a layer between them merely to simplify implementation.

Retained inputs in an isolated UI preview are build fixtures, not runtime defaults
or certification of current data. Normal Local USGS distance updates derive
on-BLM classification and distances from station coordinates and canonical BLM
managed geometry, writing the streamgage index or Local groundwater distance
sidecar. The streamgage index reader preserves `site_no` as character text,
including leading zeroes, while other CSV columns retain their inferred types.
Future generated indexes must retain that identifier text; ingestion does not
reconstruct zeroes already lost upstream. Final builds join those normal inputs
by site identifier, preserving existing nonmissing cache values before filling
gaps. Membership/current-data
halos and groundwater overlap auditing have separate consumers; restoring an
audit index does not regenerate cached halo membership.

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

`08_docs/catalog/BRIM_LAYER_CATALOG.csv` is a descriptive metadata mirror only (`CATALOG_AUTHORITY=DESCRIPTIVE_ONLY`). Its bounded build-time projections remain the retained read-only M04 Layer Explorer implementation and the reviewed descriptive implementation markers that enrich otherwise runtime-derived Guide Product records. The External Layers panel no longer presents the M04 implementation or another Guide entry, and no browser surface fetches the CSV.

The Layer Explorer is observational only (`CATALOG_RUNTIME_CONTROL=NONE`). It does not create or determine map layers, order, default visibility, layer-control registration, controllers, lifecycle, clear/reset behavior, labels, legends, popups, status/freshness, or map-product requests. Existing R/Leaflet/htmlwidgets/JavaScript construction and controllers remain authoritative (`RUNTIME_AUTHORITY=UNCHANGED`). The focused parity test maintains an exact allowlist for the build-time read-only adapter and continues to forbid catalog consumption by runtime-control, preprocessing/cache, and BRIM Live paths.

The current catalog and `qa/test_descriptive_layer_catalog.R` parity test retain the six-record HUC proof (`huc2`, `huc4`, `huc6`, `huc8`, `huc10`, and `huc12`) and the four-record heterogeneous proof: ordinary Local `gw_bull118`, External `EXT143`, Ops Live `ops_u_s_drought_monitor`, and custom-controller `usgs_streamgages`. The tested descriptive population also includes 16 diversity-selected records spanning ordinary and custom Local layers, shared and custom-loader External resources, and hosted-vector, raster/image, observational, and forecast Ops Live products.

The resulting 26-record population exercises the unchanged 15-field descriptive schema across simple overlays, shared search/filter/card behavior, combined and virtualized custom controllers, tiled and image services, bespoke External loading, product-specific status/error lifecycles, and nontrivial clear/reset ownership. This establishes `SCHEMA_DIVERSITY_PROVEN=YES` for the selected records and permits future descriptive population to proceed incrementally without another proof-only schema worker.

Catalog and Layer Explorer coverage is explicitly partial. The current 26 records do not represent every BRIM layer. This boundary does not claim that all 272 post-basemap Guide Products are cataloged or validated, authorize catalog-driven rendering, replace custom controllers, resolve External lifecycle gaps, normalize Ops Live status semantics, or confirm producer-side contracts. The proof-role classification remains separate from each record's catalog architecture.

## BRIM Guide foundation

BRIM Guide is one build-time-compiled, embedded browser surface owned by:

- `00_config/guide_resources.json` for the canonical schema-v3 authored metadata of 233 Resource records: 228 published and five staged;
- `00_config/guide_product_enrichment.json` for the compact, source-backed Product enrichment records that are keyed only by existing stable Product IDs;
- `03_functions/leaflet_guide_helpers.r` for runtime-inventory adaptation, enrichment validation, current-profile projection, compact authored Guide content, asset embedding, and htmlwidgets registration;
- `03_functions/js/leaflet_brim_guide.js` for the V4.1 shell, deterministic Product and Resource search, combined browse filters, detail/history/focus behavior, one Resource Explorer model/controller, and the small `window.BRIM_GUIDE` host API;
- `03_functions/css/leaflet_brim_guide.css` for styles namespaced under `#brim-guide-root`;
- `05_map_build/04_build_portatreasure2_core_map.r` for compilation after the actual `OVERLAY_GROUPS` and `MAP_DISPLAY` inclusion state are known.

The startup overlay and Guide share the small application-identity seam in `03_functions/leaflet_loading_helpers.r`; identity wording is not independently maintained in browser code. The compact upper-left map-toolbar control labeled `BRIM Guide`, with accessible name `Open BRIM Guide`, is the sole primary Guide entry. It always opens Explore/Home with search focused, and Guide close restores focus to that control. The External Layers panel has no Guide entry or delegation. The prior 26-record descriptive modal implementation remains read-only source with its tests and authority boundary, but it is not presented as a current panel entry.

Guide basic Product coverage is automatic for every included visible Product in the current build. Product existence and ordinary paths derive from current runtime/build authority before embedding:

- Local Products reconcile actual visible `OVERLAY_GROUPS` to `LOCAL_LAYER_REGISTRY`; label-only groups are explicit non-Products, and the two CalSim geometry children reconcile to one visible Product;
- External Products derive from `external_service_catalog.csv` rows visible in the External panel;
- Ops Live category/subgroup/name definitions are parity-validated from current `addOpsLayer()` definitions, with durable Guide keys and build flags supplied by the compact identity seam next to the centralized Ops definition owner;
- the Guide compiler derives basemap identities from `pt_base_groups()` only to prove their explicit profile exclusion; basemap records are not embedded in Guide Products, while the four current Tools derive from their existing visible controls. Implementation ownership does not determine user-facing entity type: the SMA overlay remains a Layer even though the External Layers controller also owns its toggle.

Local registry keys and External layer IDs remain the primary stable IDs. Ops, basemap, and tool Products use compact durable keys near their existing structured authority. IDs do not depend on display order, counts, or profile. Paths derive from the same runtime grouping structures and present the Local point category as `Monitoring Sites/Records`. `guide_product_enrichment.json` can enrich only an existing, included Product and is rejected for unknown, duplicate, or unsorted stable IDs; it cannot create Products or change runtime map behavior. Its source references support editorial review but are not embedded in the browser payload. `BRIM_LAYER_CATALOG.csv` may enrich a matching Product but cannot create, activate, hide, or suppress it.

`guide_resources.json` is one canonical schema-v3 dataset with 233 ordered
Resource records: 228 published and five staged. R14 appended the exact 133
R13-rebased target-200 candidates as staged records, and R15C publishes that
complete tranche by changing only `publication_state` after fresh bounded
verification of all 133 exact canonical URLs.
R10 published exactly 34 Wave-2 records by changing only
`publication_state`. The five earlier held IDs remain staged: the subject-
review IDs remain `resource_nasa_giovanni`,
`resource_usgs_earthexplorer`, and `resource_usgs_water_data_apis`; the held
taxonomy-resolution IDs are `resource_nrcs_web_soil_survey` and
`resource_usda_cropland_data_layer`. An empty controlled subject set is valid
for a Resource when exact evidence does not support an assignment; 15
newly published R15C records preserve that empty value. Final-ID aliases, build-time `res.*` migration
aliases, future human search aliases, publication state, controlled taxonomy,
variables, use scopes, geography, labeled access points, and public access
class are distinct validated fields. The registry is the tracked authority for
that reviewed descriptive metadata, but not for Product relationships. Each
Resource has one controlled Resource Type machine ID, one controlled temporal-
character machine ID, and one geographic-scope class machine ID plus an
independent normalized named-place array. One vocabulary in the Guide compiler
owns the exact labels projected from those IDs at build time. Temporal character
is separate from cadence, update frequency, freshness, and runtime status.
Resource granularity remains unchanged canonical editorial metadata and is not
a public facet.
`guide_product_resource_relationships.json` is the sole authored Product–
Resource relationship and Resource map-representation authority. Its
schema-version-2 Product and Resource arrays must equal the complete compiled
Product universe and canonical Resource universe exactly—currently 272 Product
records and 233 Resource review/representation records. Every Product
independently declares delivery classification, coverage review state, an
optional reviewed coverage disposition, evidence, and zero or more exact
canonical Resource links. Every Resource has exactly one map-review record;
reviewed representation is cross-validated from reverse Product links instead
of copied into another manual relationship list. Delivery classification is
secondary and independent of Resource map presence. `not_yet_reviewed`,
provenance-only, internal-only, and missing-candidate outcomes may correctly
have zero links; no fake Resource is required.

The compiler validates exact Product/Resource set equality, controlled values,
evidence paths, disposition and representation cardinality, duplicates, and
every link against the complete 233-Resource registry. The current authority has
97 reviewed and 175 not-yet-reviewed Products with 120 exact links: 13 direct,
63 selected-product, and 44 source-reference roles. The three DWR/TRE Altamira
Products remain one source family; `EXT033` remains the separate DWR/USGS/TRE
multiple-source composite. The unresolved intake remains deferred under its
existing evidence packets. Staged Resource records remain canonical but are
removed before browser projection.
Across all 233 Resource review records, representation is exactly three direct,
25 selected-products, 200 not currently mapped, and five not yet reviewed. The
133 R14 additions are all reviewed as not currently mapped and R14 applies no
Product-link action, preserving 270 Products and 86 canonical links at that
stage. The base R16B batch adds one exact SPK source-reference link. The
consolidated visual-review correction adds three exact selected-product links
for Drought.gov California and CoCoRaHS, removes two unsupported
provider-similarity links from California Water Watch, and leaves California
Water Watch not currently mapped. The final currentness correction links the
exact 2024 Integrated Report line and polygon Products to their evergreen
statewide Surface Water Quality Assessment parent; the complete Product
authority contains no 2026 Integrated Report Product. The CNRFC relationship
review adds selected-product links for the Local river/reservoir and weather
station catalogs and source-reference links for the Ops major water-supply and
reservoir-storage Products. The CNRFC Resource consequently represents exactly
seven current Products, preserving its seven exact links. With the subsequent WPC ERO and 25 BLM
source-reference additions, current public views are 28 in BRIM map, 200 beyond
the map, and 228 all. Current authority is 272 Products and 120 links, covering
92 Products and 28 Resources.

### Combined Resource content and finishing behavior

The current combined wave adds eighteen canonical Resources, including one
BLM California Wildfire Dashboard and one action with no Product relationship.
The archived official BLM notice dated 2022-06-29 refers through
`https://bit.ly/39IubK0` to ArcGIS item `1c4565c092da44478befc12722cf0486`;
the submitted trailing `#` is retained. The referral establishes the exact
external destination, not current legal restrictions, dataset ownership,
update frequency, completeness, or interactive browser health.
Weather Lab and the existing OPERA parent's DIST action are external discovery
links. The published USGS service Resource uses `https://api.waterdata.usgs.gov/`
and preserves its former name as a search alias; operational API consumers are
unchanged. Petitions, forecasts, reconstructed classifications, and operating
methods remain separately described.

The startup map uses zoom 6.5 at longitude -118.73584 / latitude 37.36, retaining
half-step zoom controls, wheel settings, startup layers, and label settings.
The shared native search cancel receives a targeted pointer-cursor rule; editable
text retains its text cursor. The existing input/focus/lifecycle handling remains
unchanged. Actual hit-area styling and focus behavior require human visual
review where browser policy prevents automation; no fallback clear button is
introduced without supported-browser evidence.

Ops delivery badges abbreviate the existing Guide Delivery presentations.
`products[].delivery_class` and `delivery_evidence_refs` in
`00_config/guide_product_resource_relationships.json` remain the sole authored
classification and evidence authority. `pt_ops_live_delivery_projection()` in the
existing Ops R wrapper reads that local registry at build time and joins by
stable Product ID through `pt_ops_live_guide_identity_registry()`. The compact
ID/source-token/class projection enters the existing `htmlwidgets::onRender`
data. The definition helper resolves the four existing runtime constants when
each row registers; no second M/E list, browser Guide dependency, network lookup,
or runtime class inference is involved. Guide compilation and relationship
presentations continue to use the same registry. Unlinked Products can have a
valid delivery classification without a public Resource relationship.

`brim_managed` (Guide **BRIM-managed**, Ops **BRIM-M**) means BRIM prepares,
curates, or maintains the data product or collection in addition to map display.
The exact prepared-feed map still identifies 16 prepared and 34 unprepared Ops
consumer identities. The NWS weather-station and CNRFC river/reservoir forecast-
point collections are also Managed: their detailed text-file auditing and
curation are maintainer-reported provenance, not a newly reproduced inventory
audit. These two collections do not become scheduled/prepared-feed consumers.
`brim_enhanced` (Guide **BRIM-enhanced**, Ops **BRIM-E**) requires a specific
current BRIM presentation, interpretation, or interaction feature, such as
interpreted legends, tailored hover/popups, meaningful period handling, or custom
current-view rendering. A generic service connection or checkbox is insufficient;
the standard IEM NEXRAD WMS is Provider-hosted. `provider_hosted` and
`not_applicable` have no M/E badge. Missing eligible authored classes, duplicate
IDs/tokens, and invalid classes fail build-time validation. Unknown or malformed
optional browser metadata yields no badge without changing layer availability.

The six QPE Products `ops_qpe_mrms_1hr`, `ops_qpe_mrms_1day`,
`ops_qpe_mrms_3day`, `ops_qpe_rfc_1day`, `ops_qpe_rfc_3day` and
`ops_qpe_rfc_7day` are `brim_enhanced`, based on their accepted BRIM map-surface
legends and added presentation. Each receives exactly one existing BRIM-E badge.
This does not claim BRIM produces the QPE or verifies displayed accumulation
dates. The two radar Products `ops_radar_iem_nexrad` and `ops_radar_noaa_mrms`
remain `provider_hosted` and unbadged; having a legend is not a general badge
rule. Current eligible Ops delivery subtotals are 18 Managed, 29 Enhanced and
two Provider-hosted. The disabled wind-barb identity remains ineligible. All
eight radar/QPE Products remain unlinked to Resources, with no compiled Guide
relationship delivery occurrences; their class still comes from the shared
registry. No provider, request, legend, duration or lifecycle changes follow
from the six classification decisions.

### QPE selection ownership

The six MRMS/RFC QPE Products named above form one exact, independent Ops
selection group through the internal `qpeProductId` on their existing
`ArcGISExportLayer` registrations. At most one is active; zero remains valid
and all default OFF. Selection uses the normal Ops deactivation and owner
teardown, including checkbox/count, images, pending work, map listeners and
legend cards. Direct activation of a registered QPE wrapper also enters Ops
ownership so OFF/Clear can remove it. Removed or superseded image callbacks
cannot take ownership after switching, view changes or OFF/re-enable, including
A → B → A. A failed replacement does not restore the previous QPE.

The QPE, twelve-product forecast and satellite groups remain independent;
one member of each can coexist. QPE membership uses the six stable product IDs,
never `legendType`, category or provider similarity. It does not extend WPC's
five-product QPF metadata/hover logic. Provider URLs, selectors, raster functions,
opacity, scales, badges, refresh eligibility and Unverified accumulation dates
are unchanged. Existing shared service metadata remains explicitly unbound to
the displayed image; selection creates no new timing or acquisition claim.

### Radar selection ownership

Exactly `ops_radar_iem_nexrad` and `ops_radar_noaa_mrms` share one local
single-active Radar group. Both retain checkbox controls, default OFF, and
zero active is valid. The stable IEM owner creates a fresh native WMS tile
controller on each activation; NOAA retains its ArcGIS image controller.
The IEM owner's exposed WMS options and inner tile construction share one
specification, including `layers: 'nexrad-n0q-900913'`. Only its WMS option
keys are projected into the tile constructor; Radar identity, name, legend
type and note remain owner metadata rather than new provider parameters.

Peer selection uses existing Ops deactivation and actual Leaflet removal.
Ownership generations reject old tile, image, metadata and scheduled callbacks.
OFF, Clear Ops and Clear All detach only BRIM-owned IEM tile handlers before
removing the native tile; Leaflet's own listeners are retained for teardown.
Direct supported owner activation follows the same exclusivity policy.
QPE, WPC/CPC forecast, satellite, NBM and unrelated layer groups remain
independent; Radar selection does not clear them. Provider selectors, map
legends, timing qualifications and the two unbadged Radar rows are unchanged.

### WPC / CPC forecast cards and selection ownership

The five WPC QPF products (Day 1, Day 2, Day 3, 3-day and 7-day), three WPC
ERO products (Days 1–3), and four CPC outlooks (6–10 / 8–14 day temperature
and precipitation) form one exact twelve-product Ops selection group. Zero
selected products remains valid. A small local coordinator at the actual
`ArcGISExportLayer`, `WpcEroCurrentViewLayer` and `CatalogPromotedExternalLayer`
entrypoints uses normal Ops checkbox and Leaflet owner removal. Programmatic
Ops-wrapper activation uses the same policy. Separate catalog copies outside
Ops are not members. NBM, radar, QPE, satellites and other selections remain
independent. The QPF-owner predicate still identifies only the five QPF products;
metadata and spatial sampling are not generalized to ERO or CPC.

The Ops menu places ERO Day 1, Day 2 and Day 3 immediately after the complete
five-row WPC QPF block in Forecasts / Outlooks → Weather Forecasts / Outlooks.
Only ERO category/subgroup/order metadata controls this placement; all other
rows retain their relative order, including NBM. ERO's scientific identity,
Guide classification, provider binding and controller are unchanged.

Replacement, OFF and Clear invalidate old images, metadata, cursor and CPC
bridge callbacks through their existing owners. QPF refresh and view changes
invalidate pending requests; ordinary pan/zoom redraws imagery without starting
forecast-metadata requests. Closing a legend hides its card, not its layer.

Each forecast product has a canonical map card using `ptOpsSyncLegendCards`
and `BRIM.legendCloseout`. Forecast-only CSS provides a padded, rounded light-blue
340px card, capped at 360px and the available map width minus 24px, with a
flex header and top-right dock/close controls. The existing corner overflow and
floating-card owners retain scrolling, drag and docking. Text updates preserve
card identity, disclosure, focus and dismissal state. Visual forecast keys remain
on the map, never inside the Ops panel.

QPF retains all 19 exact captured NOAA unique-value renderer values/colors
(selectors 1, 2, 3, 9 and 11). The compact contour-boundary scale displays the
18 positive values. Two contiguous positive-color strips place ticks at adjacent
contour boundaries, in low-to-high
order; spacing is schematic. The join tick repeats visually, while the accessible
list includes each positive boundary once. Integer boundary ticks at 2 inches
and above omit decimal zeroes; 2.50 and lower fractional labels retain their formatting.
This formatter is confined to legend ticks, not hover amounts. A noncollapsing
gap reserves both label tracks between strips without widening the card.
The separate white-zero swatch is replaced by the exact note
`No color = no forecast precipitation`; no permanent missing-data warning is
shown on a healthy card. The captured zero value/color and hover semantics are
unchanged. The captured 20 color is a narrow terminal boundary cap, not an
asserted open-ended interval. No values are merged,
recolored, interpolated or assigned invented endpoints. Source/method details
retain the unique-value renderer limitation: boundary inclusion and top-bin
semantics are not asserted. This is capture-derived presentation, not a fresh
live-renderer assertion or a verified class-breaks scale. Card geometry, map
colors and other forecast-family keys are unchanged.

Healthy QPF cards omit permanent range and image-cycle warnings. The collapsed
Source / method / timing disclosure retains exactly:
`Legend uses provider boundary values; endpoint inclusion is not separately specified`
and `Image and metadata cycle alignment is not independently confirmed`.
Actual missing, mixed, invalid, incomplete and failed-metadata warnings remain
available in their conditional states; no-data is not converted to zero.

The compact noninteractive QPF hover shows the active product, existing amount
in inches, actual validated duration, compact Pacific validity and subordinate
UTC interval. Its amount and timing come from the same selected cursor-query
response, never unrelated layer-global metadata. Missing, conflicting, incomplete
or mixed interval evidence leaves the amount visible with `Valid time unverified`
and no invented hours. Units, scalar/threshold/range formatting, valid zero and
the maximum legitimate intersecting contour policy are unchanged. Missing,
blank, boolean, invalid, nonfinite and negative values cannot become zero.
The existing tiny-envelope fallback is labeled `Nearby WPC polygon`; ordinary
queries say `WPC polygon`. The normal hover has no image-match warning suffix;
the technical qualification remains in the legend disclosure. No interpolation,
new request or exact-grid assertion is introduced. The content-sized tooltip is
capped at 320px and map width minus 24px, with wrapping date lines and no minimum
width floor. Full scales, issue details and extended qualifications remain in
the legend disclosure.

QPF layer-global context still uses its bounded selected-layer attribute query
at activation/explicit refresh, without geometry. Export images are not cycle-bound
to those attributes, and sample agreement is not global unanimity. WPC interval
validation requires explicit `Z` evidence in `valid_time` and agreement with
full-year start/end fields. Compact weekday/month-day labels preserve real clock
boundaries, nonzero minutes, endpoint-specific PST/PDT and cross-year identity;
full values remain in the disclosure. Zone-less WPC `issue_time` is a provider
clock labeled `zone unverified`, never converted to Pacific or UTC. CPC calendar
dates remain dates. Existing RFC time formatting is unchanged.

The four CPC Ops rows use display-only `panelLabel` abbreviations `Temp.` and
`Precip.` for their existing 6–10 and 8–14 day outlooks. Canonical names, catalog
and Guide identities, full map-card titles, actions, badges and row order remain
unchanged. The existing row layout still wraps at narrow widths; no font,
panel-width, clipping or no-wrap override implements these shorter labels.

CPC keys group Below / Near / Above normal with neutral in the middle, keeping
all seven existing probability bins and actual BRIM mapped colors on each side.
Equal Chances and Unknown remain separately visible, with Unknown explicitly
not a valid forecast. CPC hover, popup and styling implementations are unchanged.

ERO keys retain the four actual mapped fills and captured risk labels. Slight
uses BRIM/renderer `#fffe00`; the captured provider legend PNG uses `#ffff00`, a
recorded difference rather than permission to recolor the map. Unknown/missing
risk is not confirmed Marginal even though the pre-existing map fallback is
green. ERO style, hover and popup functions remain unchanged.

CPC keys come from a read-only descriptor of `ptCpcOutlookColor`, including all
seven Above/Below probability branches and separate Near Normal, Equal Chances
and unknown meanings. Temperature and precipitation retain their distinct
palettes, labeled **BRIM display colors**, with official comparison links.
Existing CPC hover/popup formatting, style and catalog options remain unchanged.
Its existing promoted-layer bridge conveys only metadata from accepted current
generations. ERO/CPC cards compare metadata tuples across loaded records;
partial, mixed, invalid or empty responses stay qualified. Typed CPC epoch-ms
fields become UTC calendar dates; genuine date-only fields stay dates. Missing
GIS dates are not invented or substituted for forecast dates.

`qa/test_ops_wpc_cpc_legends.js` reuses the existing forecast and promoted-layer
fixtures and authenticates the full offline provider capture under
`qa/fixtures/wpc_cpc/`. The capture and synthetic CPC snapshots are QA/provenance
only: no runtime fallback, embedded provider response or render input. Existing
delivery classification and evidence-reference contracts are unchanged; the
Legend/SCAN QA file remains required in staged repository validation because
product delivery evidence references it.

### Satellite / imagery request ownership

`leaflet_ops_live_gibs_helpers.r`, loaded by the existing Ops helper assembly,
owns five initially-off Leaflet imagery products, ordered GeoColor,
GOES-West Clean IR (Band 13), NOAA-20 VIIRS True Color, NOAA-21 VIIRS True Color and
MODIS Terra True Color. GeoColor and infrared retain `ops_goes_geocolor` and
`ops_goes_infrared`; Terra retains `ops_modis_terra_true_color`. VIIRS uses the
new distinct `ops_viirs_noaa20_true_color` and `ops_viirs_noaa21_true_color` IDs.
`ops_goes_water_vapor` is retired from active registration and Guide add actions;
its historical identity is not reassigned. Air Mass is not included. The current
catalog requires exact active Product membership, so the retired Product row
and its Resource edge are removed from that projection; historical source and
evidence retain them. No new Resource or matrix Guide entry is established.

The GIBS wrapper enforces single-active selection only within this five-product
family. Selecting another satellite removes the previous wrapper through its
normal OFF cleanup, leaving non-satellite layers and the shared lazy metadata
owner unchanged. Turning a satellite off never selects another. Tile removal
must complete before remaining tile listeners are cleared: Leaflet attaches its
map-event unsubscription to the tile's `remove` event. Clearing that listener
first leaves a detached tile subscribed to zoom and interrupts active redraw.
Pan/zoom retain the selected request and metadata binding; only explicit `rfrsh`
rechecks metadata. OFF, Clear Ops, Clear All and map destruction remove owned
tiles through the same cleanup path.

The panel uses the display-only subgroup headings `GOES-West (geostationary)`
and `Daily true color (polar orbiters)`. One collapsed native `meta` disclosure
contains five local entries, each with Orbit, Satellite and Instrument / product.
The GOES entries identify GOES-18/ABI GeoColor and Band 13 Clean Infrared; the daily
entries identify NOAA-20/VIIRS, NOAA-21/VIIRS and Terra/MODIS Corrected Reflectance
True Color. The disclosure makes no request and changes no selection. External
`info` and `srce` links remain separate. The Pacific Southwest matrix link stays
above the choices. Native toggle accessibility and compact row fit require
browser acceptance.

The exact GIBS products are `GOES-West_ABI_GeoColor` (PNG, Level7),
`GOES-West_ABI_Band13_Clean_Infrared` (PNG, Level6),
`VIIRS_NOAA20_CorrectedReflectance_TrueColor`,
`VIIRS_NOAA21_CorrectedReflectance_TrueColor` and
`MODIS_Terra_CorrectedReflectance_TrueColor` (JPEG, Level9). Delivery uses NASA
GIBS EPSG:3857 WMTS, with no NOAA `_current` fallback. The two retained GOES
viewer relationships describe those same product families, not their delivery
endpoint. VIIRS Resource coverage review remains not yet reviewed, with zero
invented links. The partial descriptive layer catalog has no satellite records
and remains unchanged.

The first imagery checkbox selection or explicit imagery `rfrsh` starts one
shared per-map capabilities request. Opening the map or Ops panel alone starts
none. The request is bounded to 10 MiB and 25 seconds, uses CORS with omitted
credentials, and has no automatic retry, polling, persisted XML or proxy fallback.
Subsequent selections reuse the parsed result; explicit refresh coalesces and
rechecks metadata. A failed check means availability UNKNOWN. Missing or
unsupported bindings disable only that product's add action while preserving
its explanation, links and explicit recheck. Layer generation ownership isolates
pending metadata and tile callbacks across refresh, date changes, off/re-add,
Clear Ops, Clear All and map destruction. Clearing one selection does not cancel
shared metadata needed by the newly selected satellite. Non-satellite overlays
keep their existing stacking behavior.

GOES requests the newest non-future advertised instant using the provider's
interval grid, not `Default` or an invented cadence. `Imagery time` shows that
requested UTC timestamp alongside the existing Pacific formatter when available.
Only the selected imagery values receive semantic strong emphasis in the
small-print timing surface; lower Ops status messages remain plain text.
Daily imagery uses `Imagery date (UTC)` without conversion to a Pacific calendar
day and offers advertised UTC today plus seven prior
dates, respecting gaps, and initially uses yesterday only if advertised. Missing
or expired dates require a manual choice; pan/zoom and refresh preserve a still
eligible user date. Date changes detach the previous layer before relabeling.
No blank/error fallback or 48-hour rejection is performed. Failed rechecks may
retain the previous explicitly dated image with a warning. Metadata check time,
requested imagery time/date, tile transport, coverage and actual pixel acquisition
are distinct. The displayed imagery time/date is the provider-advertised value
used for the imagery request; it does not establish exact pixel acquisition time
at the viewed location. Absolute time/date remains visible without relative-age
wording or a relative-age timer. There is no continuous-live
claim: refresh is explicit, and pan/zoom use the currently bound date/time.

Core product/style/time/format, namespaces, EPSG:3857 origin/resolution and native
matrix dimensions are validated. Complete unique valid coverage limits constrain
requests. Only the exact authenticated inconsistent GOES limit record signatures
(repeated levels, missing native level and out-of-grid columns) permit selected
viewport requests against the valid core grid with precise coverage UNKNOWN.
Those signatures identify anomalies; they are not footprints or replacement
bounds. Values are neither normalized nor merged. New unresolved limit structures
fail that product's qualification. Daily products require their qualified absence
of limits. `noWrap`, native zoom caps and the normal Leaflet viewport queue prevent
out-of-grid requests and background surveys; no sandbox regional mask is used.
Transport errors persist through batch completion and never prove coverage.

Satellite rows alone use accessible external `info` and exact layer-metadata
`srce` links. The Pacific Southwest GOES-West matrix link sits immediately below
the section metadata disclosure and above the choices. Requested dates and concise
status are inline, with timing and coverage details available separately; no imagery legend gallery is added.
The immutable captured WMTS ZIP under `qa/fixtures/gibs_wmts` is test-only and
must never enter render inputs or act as a live fallback. Source/offline tests
exercise the implemented parser and panel/lifecycle wiring. BRIM browser visual,
coverage, CORS, accessibility and performance acceptance remains a separate gate.

The RFC daily-analysis, three-day and seven-day QPE Products have independently
keyed map legends (`ops_qpe_rfc_1day`, `ops_qpe_rfc_3day`, `ops_qpe_rfc_7day`).
Their existing ArcGISExportLayer registrations are ordered daily → three-day →
seven-day and remain off until selected. Daily and seven-day internal names/IDs
are unchanged. The legend helper owns one card per active registration;
freshness/status rows do not own cards. All three use Enhanced delivery and one
BRIM-E badge per Ops row, no Resource links and unchanged provider raster rendering
at 0.64 opacity.

The current Product census is 272, with 50 Ops identity records and 49 eligible
Ops Products. The original ordered universe is preserved after reversing the
RFC three-day addition and the satellite delta (retired Water Vapor; added
NOAA-20 and NOAA-21 VIIRS). The 233 Resource records and 228 published Resources
are unchanged. There are 120 Product–Resource links and 240 compiled
relationship occurrences. The retired Water Vapor edge accounts for the one
link reduction; the two same-product GOES Image Viewer edges remain.

The scales reproduce the product-specific published entries for RFC MapServer
image layers 32 (Today's Analysis), 40 (Last 3 Days) and 56 (Last 7 Days);
their parent/footprint selectors are 29/31, 37/39 and 53/55 respectively.
The official
[legend endpoint](https://mapservices.weather.noaa.gov/raster/rest/services/obs/rfc_qpe/MapServer/legend?f=pjson)
supplies the original labels, embedded-PNG hashes and decoded RGBA evidence in
`qa/test_ops_radar_qpe_contracts.js`. Layer 40 was checked independently even
though its 17 label/swatch tuples match layer 56. Each solid swatch retains its
16px interior and 2px transparent margin.

Numeric classes sort by numeric lower bound, ascending in DOM and visual
row order, with compact range labels and original provider wording retained
in accessible labels and titles. The card fits two columns at ordinary width
and one when narrow; a width cap prevents a third column when floating.
The transparent “Less than 0.01” provider classification remains internal but
is omitted from the primary grid with “Below 0.01 in omitted from this key.”
Missing data remains separate. This is legend presentation only, not a
zero-precipitation inference, scale coercion, raster change or runtime request.

All cards show **Displayed accumulation interval: Unverified** outside one
collapsed-by-default **Source, method & timing** disclosure. Its labeled rows
distinguish the daily analysis from rolling 24-hour QPE and describe
three/seven-day sums of daily 24-hour analyses. The scheduled cutoff is
**4 a.m. PST / 5 a.m. PDT (12Z)**; an unknown frame date cannot select one season.
Accumulation, provider publication/update, BRIM retrieval/check and expected
service refresh are separate concepts.

The [official service description](https://mapservices.weather.noaa.gov/raster/rest/services/obs/rfc_qpe/MapServer)
documents hourly refresh near :55 and possible daily revisions during
4 a.m.–1 p.m. PST / 5 a.m.–2 p.m. PDT (12–21 UTC); these are expectations, not
guarantees of new precipitation. Footprint fields alias Reference Time, GIS
File Date and GIS Ingest Date, but the reviewed metadata establishes no exact
accumulation-date association with the current non-time-enabled export.
No RFC temporal metadata binding is implemented. Image completion or generic
metadata success cannot date the displayed image.

The RFC check therefore reports an explicit unverified warning and the
schedule without requesting unbound metadata. Its separately labeled BRIM
check instant uses the existing QPF America/Los_Angeles formatter, preserving
the UTC instant secondarily and rejecting missing/invalid dates and the
formatter's host-local fallback. This check time never supplies an
accumulation endpoint. Verified interval derivation and display remain
unimplemented pending a provider/export association contract. No polling,
countdown, layer reconstruction or provider request accompanies card redraw,
close, dock or resize.

The RFC-only `ptOpsRfcQpeMetadata` helper owns the panel/card metadata copy and
uses the existing RFC product definitions for method text. The existing
`NWS QPE Mosaic metadata` status row owns the last BRIM check value; before
that event, cards say **No check recorded**. The instant is shared RFC service
context, not a per-image request. Checks update mounted metadata text without
remounting cards, losing disclosure/focus state or reopening hidden cards.
Clear retains its existing ownership and clears the status row; redraw,
showing a card and docking never create a check instant. Formatter errors
remain **Unverified**.

The maintainer-reported A6 preview review passed with the RFC metadata follow-up
(E3); that report does not establish browser fit, keyboard behavior or
responsiveness for this expanded disclosure. Those checks require a later
reviewed candidate. The preference to expose useful legend metadata in future
Ops Live work requires separately approved scopes; it does not authorize
changes to MRMS or other layers.

Cards reuse `BRIM.legendCloseout` actions, drag/dock destruction and scheduled
lower-left safe-gap/overflow layout. Closing hides only that activation's card;
ordinary redraws preserve its node and hidden/docking state. Off/on restores it.
Off, Clear Ops and Clear All destroy even floating or hidden cards. MRMS and WPC
QPF retain the generic guide while RFC-only selections use their specific scales.
The offline fixture exercises actual registrations, legend code and panel/clear
callers with controlled DOM, map, metadata and timers. It does not establish
mounted-browser appearance/accessibility, provider-frame validity or performance.

Promoted national streamflow (`ops_streamflow_multiagency`, Ops key
`ops_live_agency_streamflow_gages`) uses the existing External catalog loader and
custom-layer records. Refresh reconstruction retains its Ops key, display name,
and activation generation. Each promoted current-view record also tracks its
latest refresh request. Dependency and query completions check record membership,
cancellation, activation, and request identity before changing geometry or state.
Eligible failures retain the previous snapshot and terminate loading; superseded
successes and failures cannot change the newer request. Disable, Clear Ops, and
Clear All invalidate ownership through the existing promoted removal path.
Clear External continues to preserve Ops-owned records.

Initial FeatureServer current-view responses check the captured Ops activation
generation and cancellation flag before preparing a collection, constructing a
layer, or reporting a result. This check does not require a registered record;
valid first loads can proceed. The later `ptAddCustomRecord` cancellation and
generation guard remains a second protection before registration and insertion.

`CatalogPromotedExternalLayer` correlates status with the stable Ops key, current
activation, and request identity rather than display-name equality. Its existing
panel refresh interface preserves terminal status even for synchronous responses.
Retries from an earlier activation cannot start work after re-enable. Refresh
progress remains pending, while terminal errors end loading even when their
guidance mentions loading or checking. Ordinary External records do not require
Ops ownership metadata. This lifecycle contract does not change observation
timestamps, source freshness thresholds, or the meaning of retained data.

`qa/test_ops_promoted_streamflow.js` runs the owning Tools functions, the actual
R-embedded Ops wrapper, and the panel refresh/checkbox/clear callers with controlled
query completions, rendering, DOM dependencies, and timers. Its offline cases cover
completion ordering, removal/clear/re-enable, terminal status, synchronous results,
and ordinary External success/failure controls. Initial-response cases count
preparation and construction attempts as well as insertion/removal, including
obsolete responses after off, re-enable, Clear Ops, and Clear All. Unexpected
networking fails the harness. Controlled counters establish avoided boundary work,
not elapsed performance, native geometry rendering, tile-network behavior, basemap
responsiveness, or mounted-browser/provider correctness.

The lower `Ops status` list reports operational status events. `Status recorded`
is the time BRIM recorded that event, not a provider check, observation, forecast
validity, imagery time or data-freshness timestamp. Stored timestamps, event
lifecycle and product-specific timing remain unchanged.

The legacy Ops source footer retains its source meaning.
Its catalog reconciliation, including the six named
air-quality leads, remains a deferred watchlist follow-up requiring a
separate approval; no source deletion or freshness redesign is implied.

One compact, noninteractive badge follows the primary title and stays with its
last short word while earlier text wraps. Managed titles and pale-blue badges
use normal-weight dark-blue text; Enhanced badges use a quiet neutral outline.
Each row badge supplies one accessible Managed/Enhanced expansion. The visible
key explains both codes, preserves original-source credit, and notes varying
features and refresh schedules. Classification supplies no ownership,
endorsement, loading, health, freshness, or lifecycle authority. Operational
loading, warning, error, and disabled states retain styling priority.

The paired forecast rows use the same two-line presentation: `Water-Supply Basin
Forecasts` above `CNRFC / CBRFC`, and `Reservoirs | storage + forecast links` above
`CDEC / CNRFC / USACE`. Each badge belongs to the primary title; the agency line
uses smaller neutral text. Both original internal names, stable Product IDs,
Forecasts / Outlooks grouping, symbols, and lifecycle remain unchanged. The
reservoir's accessible description distinguishes observed-storage symbols from
popup links to forecasts and operations. Shared row markup associates each
checkbox with its main label and keeps action links and label/z controls outside
that label. The text and wrapping tools share a column beside the checkbox at
the existing panel width. Rendered fit and startup framing require human visual
acceptance where browser policy prevents automation. CNRFC's panel title is
`CNRFC river/reservoir forecast points`; its original internal name
`CNRFC forecast points | river/reservoir` and separate CNRFC homepage link remain.

The documentation-only C2 amendment preserves the old 97 entries, all 27/278
children, and the A5 amendment. It accounts for 129 named rows, 305 children,
22 metadata findings, eleven related-work items, and all 270 Product dispositions.
The 151 deferred Products and remaining residual groups stay explicit; a
classification is not proof of metadata completeness. All other exclusions and
individual holds and all 45 housekeeping/dependency holds remain. Candidate
acceptance, commit, PR, and release facts remain pending.

### WPC ERO Resource ownership and evidence

`resource_noaa_wpc_excessive_rainfall_outlook` represents the Weather
Prediction Center Excessive Rainfall Outlook (ERO), with canonical action
`https://www.wpc.ncep.noaa.gov/#page=ero` and separate Day 1–3 actions at
`https://www.wpc.ncep.noaa.gov/qpf/ero.php?day=N&opt=curr`, where the authored
values of `N` are exactly 1, 2, and 3. These are explicit Resource access
points, not generated endpoints or background requests.

`03_functions/leaflet_ops_live_layer_definition_helpers.r` owns the stable
`pt_ops_live_guide_identity_registry()` entries `ops_wpc_ero_day_1`,
`ops_wpc_ero_day_2`, and `ops_wpc_ero_day_3`, and their `addOpsLayer()`
definitions. Each uses `WpcEroCurrentViewLayer`, `legendType: 'ero'`, and the
corresponding literal source action above; layer IDs are 0, 1, and 2.
`03_functions/leaflet_ops_live_service_helpers.r::pt_ops_live_service_helpers_js()`
declares `WPC_ERO` as the hazards `wpc_precip_hazards/MapServer` service and
`checkWpcEro()` as its freshness-check owner. It separately declares
`WPC_QPF` as `precip/wpc_qpf/MapServer` with `checkWpcQpf()`. ERO's
excessive-rainfall risk is therefore a distinct product family from QPF's
precipitation-amount forecasts; a shared provider and the `/qpf/` URL directory
are not evidence that the two Resources are interchangeable.

The Product-centric relationship registry moves only ERO Day 1's selected
link from `resource_noaa_wpc_qpf` to the ERO parent and adds the Day 2 and
Day 3 links. The exact physical operations are R17C_REL_011–014; all three
resulting links use `selected_product_from_broader_resource`. The ERO
Resource is `selected_products_in_brim`. All three Product delivery classes
remain `brim_enhanced`, and their `delivery_evidence_refs` are unchanged.
QPF's direct Product links and all other retained links remain intact.
Guide compilation continues through
`03_functions/leaflet_guide_helpers.r::pt_build_guide_bundle()`; these authored
relationship decisions add no Product or map controller.

The dated proposal and endpoint observations are preserved in
`BRIM_G_R17C_G1_R1_LOCAL_EVIDENCE.zip` (SHA-256
`6d5e86b4ec7f9ebb85ea1d4836462ca651eafdfa83e69943b64c56cc14d0ee0b`),
under archive prefix `BRIM_G_R17C_G1_R1_LOCAL_EVIDENCE/evidence/`:
`04_RELATIONSHIP_MUTATIONS.json`, `product_review_changes[0..2]` and
R17C_REL_011–014 (member SHA-256
`4dd13ff2d64dd8f7b7d638e8f25b3bc08fa50b1453e1074ef414af31ec06dbec`),
and `05_ENDPOINT_EVIDENCE.json`, EP010 and EP030–032. Those bounded dated
observations support source/action identity, not ongoing freshness or visual
acceptance. Current tracked definitions supply the Product ownership evidence.

Maintainer-approved BRIM-R17C1-A1 replaces the planned intake-ledger citation
with this architecture document in exactly six reference-array positions:
`coverage_evidence_refs[2]` and `resource_links[0].evidence_refs[2]` for each
of the three ERO Products (zero-based positions). The other references remain
`03_functions/leaflet_ops_live_layer_definition_helpers.r` and
`00_config/guide_resources.json`. The unchanged validator requires existing
tracked source paths. The tracked intake ledger remains documentation-only; it is not a runtime
catalog or an automatic replacement for these architecture citations.

### Documentation-only intake decisions

`08_docs/catalog/BRIM_RESOURCE_INTAKE_LEDGER.json` records 97 intake entries,
27 embedded water-year action children, and 278 station/name alias children.
Its schema version is 1 and role is `DOCUMENTATION_ONLY`. It preserves
submitted versus curated values, immutable packet/member/hash/row provenance,
endpoint limitations, exact mutation grains, approval decisions, holds,
dependencies, and separately pending acceptance records. The unchanged
relationship validator checks the referenced tracked file exists; the builder
does not parse it as a catalog or embed its contents. Only reviewed edits to the two existing runtime
registries change Guide behavior; there is no automatic publisher, feed,
profile, facet, or current-year calculation.

BRIM-R17C-G2-01 scope approval and BRIM-R17C1-EXEC-01 execution approval are
separate from visual acceptance. The appended C2 amendment records the approved
combined scope and execution, including Weather Lab and the exact L082 dashboard.
All other individual holds/exclusions and all 45 dependency holds remain. In-progress entries retain `APPROVED_*`, never
`IMPLEMENTED`; PR, accepted SHA/tree, and implemented dates remain null.
The ledger documents the original WPC citation proposal separately from the
A1 amendment. Its 24-transition historical ctime exception retains cause
`UNESTABLISHED` and gives no waiver for fresh drift or cleanup release.

The consolidated CNRFC Resource retains seven Product links and exactly
24 actions: one canonical landing, the Water Resources regional forecast map,
the HD6RSA daily basin QPF/freezing-level page, 15 station views, five
CSV/archive actions, and the dated WY2026 SACC0 action. Retired CSV-child IDs, migration aliases, and human search names are
preserved on the parent. This is an authored finite set, not a generated
station menu or an equivalence join. C2 retains every C1 action and label. The current 233-Resource corpus has
457 action rows / 456 distinct URLs, 452 unique public actions, and 690 alias
occurrences across the three existing namespaces. The staged/public USGS URL
overlap is deliberate and does not merge the two stable identities.

The 15 station labels name the river, forecast point and FNF water-year trend
product, retaining the original station codes and default-year URLs. FNF means
full natural flow; these links do not assert current observations, regulated
releases or reservoir storage. Michigan Bar is a river forecast point, with no
reservoir label. Lake McClure / New Exchequer is the curated EXQC1 display name.
The Water Resources map provides the wider network; HD6RSA provides 24-hour
basin QPF and freezing levels for Days 1–6, not general temperature. Both are
external pages, without an automatic BRIM feed or update guarantee. Canonical
stays first, the two broad pages follow, and the prior 20 actions retain order.
The documentation-only ledger amendment preserves the original 97 intake rows.
C2 preserves the 15 labels and two broad actions before appending SACC0.
Its scope approval does not imply human visual acceptance or release of any
dependency hold.

BRIM's CNRFC FNF display geometries were created by grouping and dissolving
downloadable CNRFC subbasin geometries outside the current scripted
preprocessing pipeline. The current pipeline reads and generalizes the
prepared geometry while retaining river, reservoir, and CNRFC/NWS identifiers.
Where CDEC and CNRFC FNF products represent the same river-reservoir system,
BRIM uses a common display geometry; minor differences in agency watershed
delineations are not represented separately. These are BRIM's final grouped
display geometries; CNRFC provides the downloadable source subbasins and the
forecast/FNF context. The current preprocessor does not reconstruct the
original grouping or dissolve, and the exact historical GIS toolchain is not
established. BRIM does not assert that CDEC and CNRFC FNF values or source
boundaries are always identical.

`guide_product_enrichment.json` owns editorial Product content only and no
longer owns or supplies Resource relationships. Relationships and map presence
are never inferred from Resource text, providers, URLs, publication, or
geographic intersection. All temporary R12A relationship objects and the
compiler adapter are absent; permanent compatibility shadow, dual authority,
and fallback are prohibited. The browser receives the 228 published records in
registry order with one exact 23-field shape: identity and reviewed descriptive
fields, labeled access points, Resource Type and temporal machine IDs with
controlled labels, geography scope ID/label plus named places, reviewed map
state/representation, exact represented Products with secondary delivery and
source-list context, and normalized search text. Migration aliases, legacy
relationship flags/subtypes, raw evidence, and runtime authority are excluded.
Publication is not a Guide profile. The
registry is descriptive Guide content only: it cannot create or control layers,
visibility, order, controllers, lifecycle, clear/reset behavior, legends,
popups, status/freshness, or BRIM Live behavior.

The repository currently has one actual output profile: `default`, naming the existing `MAP_DISPLAY` plus `OVERLAY_GROUPS` build. Publication projection occurs before relationships, Resource search text, counts, facets, adaptation, or embedding, while profile projection explicitly excludes all runtime-derived basemap records. The five staged Resources therefore contribute zero browser payload or visible-count/search/facet authority. Public views remain exactly 28 `In BRIM map`, 200 `Beyond the map`, and 228 `All Resources`. This is a Guide-content boundary only: basemap construction, controls, ordering, assets, and defaults remain unchanged. The projection contract removes excluded records, aliases/search content, relationships, Quick Access membership, and count contributions rather than hiding them in browser state. Collections prune excluded members and disappear only when no members remain. Do not add a runtime profile selector or invent DOI/public/custom publication policy without an authoritative repository profile mechanism.

The standalone HTML embeds the projected Resource records and makes no runtime
Resource-data request or browser-storage copy. Raw bookmark exports, intake workbooks, candidate
inventories, and unresolved reconciliation evidence remain External and are
not tracked wholesale or shipped in the browser payload. Any future inventory
import requires separate reconciliation and approval before it can change the
registry, aliases, relationships, profiles, or visible Guide content.
Publication and fresh endpoint QA for the 133 R14 candidates were deferred in
R14 and are complete in R15C. R15C changes no Guide UI/runtime/network
behavior, runs no preprocessor, and regenerates no cache.

The accepted V4 shell uses a dark contour outer field around one large warm off-white surface, a compact fixed left rail, and a search utility band confined to the main column. The rail owns Home identity, compact A Explore / B Methods & Guides / C Resources / D Updates navigation, one bounded typed Quick Access list, About / Contact, and lower DOI/BLM marks. The main Explore view uses a compact two-column identity introduction, scope note, three visible compact single-select facet groups (`Where in BRIM`, `Primary Subject`, and `Information Type`), and the complete profile-projected layer/tool inventory sorted case-insensitively by display name with stable ID as the tie-breaker. Each group holds zero or one selected value and selections across groups combine with search by AND. `Where in BRIM` is a radio-style dimension: choosing another value replaces the prior value, while its removable active chip restores the unfiltered state. Primary Subject and Information Type likewise replace the prior value, and their active value or chip can clear that group. Product taxonomy remains multi-valued. Removable chips appear directly beneath search only while browse filters are active, with at most one chip per group, and a contextual inline `Clear all` retains the existing reset contract by clearing query, facets, and collection/result context before returning focus to search. No modifier-key or touch gesture enables within-group multi-selection. `Entity type` remains record metadata rather than a permanent facet; `Tools` is a Where choice, so `Tool / Workflow` is omitted from the permanent Information Type choices. The A–Z inventory uses the same query/facet state, Product corpus, and compact result renderer in one bounded scroll region so its first rows and browse facets remain visible on desktop; an adjacent live derived status reports the current count against the complete projected Product count and marks any subset as filtered. The shell has no masthead, footer, card grid, pill navigation, pagination, virtualization, or duplicate responsive implementation. Intermediate layouts retain a reduced rail; the mobile layout becomes one full-screen surface with one upper-right close control, search below the top bar, compact horizontal A–D navigation, the same Quick Access list, and no footer or horizontal overflow.

V4.1 extends that same shell with one Resource Explorer. The compact Resources
destination is a gateway with exactly three actions: open Resources represented
in the BRIM map, explore Resources beyond the map, or search all Resources. Its orientation copy
distinguishes A · Explore—Products available through BRIM—from C · Resources,
which contains datasets, viewers, portals, official sources, and supporting
libraries linked to BRIM or useful beyond it. While that gateway is active, the
shared utility search has a Resource-specific label and placeholder. Focus or
whitespace alone leaves the gateway in place; the first non-whitespace input
opens `All Resources` with the complete query and input focus preserved under
C · Resources. Later keystrokes and query clearing stay within that Explorer
view, and its Back action restores the Resource gateway. Explicit entry into
any of the three Resource views retains the same in-view search behavior. The
shared utility bar binds its visible query, filter tokens, counts, clear
actions, and routing to the active top-level section. Inactive A · Explore
Product filters may be preserved privately but are hidden and inactive in C ·
Resources; C-owned Resource filters clear without routing into A · Explore.
The Explorer uses a
64-pixel Resource spine on wide layouts; an intermediate disclosure layout;
and a measured one-pane search/results, facets, or detail flow at narrow width.
Its three primary views appear in the exact order `In BRIM map` (28), `Beyond
the map` (200), and `All Resources` (228). Membership derives only from each
Resource's reviewed `map_representation`: three direct matches plus 25 selected-
products Resources form `In BRIM map`, while 200 not-currently-mapped Resources
form `Beyond the map`. The primary views form one
mutually exclusive radio-style control, selecting one replaces the prior view,
and primary-view changes do not create chips or clear secondary refinements.
Search applies NFKD normalization, punctuation and whitespace
folding, AND across query tokens and facet dimensions, OR within selected
providers, fixed semantic-field weights, conservative one-edit title/alias
recovery for tokens of at least five characters, and stable title/ID ties.
Resource Explorer defaults to title A-Z without a query or context. With a
query or context, its default order is relevance, normalized displayed title,
then stable ID; explicit user-selected sorts remain authoritative. Explore's
`Search matches` surface still orders by relevance then source order, while
`All Layers & Tools A-Z` remains alphabetical even when filtered. Cross-surface
alphabetical tie unification and any bare-`scc` discovery are separately deferred
scope; neither is implemented by the current Resource search contract.
Selected providers offers eleven named agency choices and five provider types
in three always-open fieldsets: Federal, State, and Provider types. Federal contains
BLM, EPA, FEMA, NASA, NOAA, USACE, USBR, USDA and USGS, in that order. State contains
DWR and Water Boards. Provider types contains, in alphabetical displayed and DOM
order, County; Nonprofits / NGOs; Other; Private; Regional/local. The existing model
declares this fixed roster once. All sixteen choices remain visible, focusable,
and removable at zero matches. Counts describe distinct public Resources under
the other filters, and never choose which controls exist.

County means reviewed county operation, not geographic location. Regional/local
covers other local public-service organizations; Private covers reviewed
companies/commercial services; Nonprofits / NGOs covers reviewed nonprofit,
community, conservation, and advocacy organizations without asserting tax status.
Ten terminology aliases provide NGO/nonprofit discovery for the five reviewed
Resources. Original provider attribution and roles remain unchanged.
Other is the full published Resource universe minus the union of every offered
non-Other group, computed during full-catalog initialization before query, view,
facets, or checkbox narrowing. Hidden legacy families never subtract from Other.
Non-Other groups may overlap; OR selection deduplicates Resource identities.

The visible helper reads "Filter by agency or type; search above for any provider."
Other's checkbox has a concise accessible description of its full-catalog
complement meaning without an additional visible paragraph. Full Resource search
remains above the filter block. Existing
hidden-family and exact-provider selections retain honest removable chips;
NIFC does not become BLM. Native fieldsets, normal-weight text, associated
24-pixel-or-larger labels and visible focus retain usable controls. Each compact
checkbox/label/count unit wraps as a whole where it fits; counts immediately
follow their own labels. Labels use 13-pixel-equivalent text and quiet headings.
Federal spans the full width and uses balanced three- or five-column rows when
the container can accommodate them, with wrapping at smaller widths. State uses
its intrinsic width beside Provider types; they stack in that order when the
available width or enlarged text requires it. The provider container's font-relative
queries do not scale the main search, titles, results or other facets. No group
toggle, provider dropdown, provider-only scroller, or extra category is present.

The existing JavaScript Resource Explorer model owns one explicit provider
mapping, exposed through its standalone test seam. Family selectors use stable
`family:` IDs, separate from raw attribution strings. Membership matches the
display provider or a named `display_provider`, `publisher`, `maintainer`, or
`partner` using NFC, trimmed/collapsed whitespace, and lowercase only. A
`data_owner` alone is excluded. There is no substring, URL-domain, title,
related-Product, or search-text inference. Five exact Resource rules associate
FEMA National Flood Hazard Layer Viewer with FEMA, Drought.gov California with
NOAA, Safe to Swim Map with Water Boards, and the two OpenET Resources with
Nonprofits / NGOs; each checks its ID, original
display provider, title, and canonical URL before association. A guard mismatch
fails instead of silently broadening a generic attribution. Remaining
providers stay discoverable through Other and name-based search. The full mapping stays in JavaScript,
without changing the catalog JSON or any original attribution, role, or URL.

Selected families combine with OR; query, primary view, other facets, and exact
Product context retain AND behavior. Counts derive from distinct matching public
Resource IDs under the other dimensions, ignoring current provider selections.
Joint NASA/NSIDC and NASA/USDA associations never duplicate a result. The
legacy mapping supports old selections without generating additional default
checkboxes. Removable active chips stay above the scrollable workspace.
Existing exact-provider selections, including local providers, remain exact and
explicitly labeled in removable chips; unavailable stale values also stay
visible and removable. Stale provider-only query text cannot hide the shortlist.
Clear providers preserves the main query and other facets, then focuses the
first BLM checkbox when the filter pane is visible, otherwise search. Reset all
retains the full reset contract. Native checkboxes use associated labels and
unique IDs. No group-level selection control is present.
Subject and Information Type retain their immediately visible compact wrapped
single-select choices.
A sentence-case `More filters` disclosure progressively reveals native,
long-vocabulary-capable single-select control for the already projected
Resource type. Resource Type state and counts use its controlled machine ID,
while options, chips, badges, and detail use the build-derived controlled label.
Resource granularity remains projected editorial search/detail metadata rather
than a facet. Temporal character is excluded from search and filtering and is
shown compactly in selected detail only when its controlled ID is not `unknown`.
Geographic scope and named geography remain projected search/detail metadata,
not facets; search uses the normalized scope/place labels, and an `unknown`
scope label is omitted from public detail while retained in canonical data.
Temporal, geographic, named-geography, access-point-type, granularity,
verification, and priority facets remain deferred, and there are no placeholder
controls for them. There is no relationship-subtype, delivery-class, or
multiple-source primary filter. Every result and selected detail shows exactly
one restrained map-representation label and selected detail adds the matching
plain-language statement: direct representation, selected products from a
broader Resource, or not currently represented in the BRIM map. Related Product
rows carry relationship role and delivery class only as secondary detail.
Multiple-source Products add one exact sentence naming all canonical source
Resources in registry order instead of creating another primary category.
The V4.1 warm-neutral shell remains authoritative: All Resources stays neutral,
in-map representation receives restrained green emphasis, and beyond-map
representation and external access actions
receive restrained muted-blue emphasis. Labels and accessible selected states,
not color alone, remain authoritative.
Resource filter buttons retain their existing minimum interactive heights and
visible focus outline while using slightly tighter padding and modest five-pixel
corners. Resource informational and relationship badges use the same modest
corner treatment; active removable filter chips retain their separate compact
chip treatment.
Sort remains native. On wide desktop, refinement receives about two-fifths of
the result-list layout and one-third of the selected-detail layout, preserving
dense result/detail scanning without compressing visible filters. Search,
orientation copy, and the primary-view ribbon remain stationary above a bounded
workspace. The Refine sidebar reserves an in-layout bottom action row for More filters
and Reset all. Its constrained filter body is the single native scroll owner
for provider shortcuts, other facets and expanded Resource type controls; there
is no separate provider-list scroll region. Opening More filters focuses and
natively reveals Resource type within this body; closing returns focus to the
disclosure. Captured filter scroll state follows the body, while the action row
never overlays focused controls. Active chips and the primary-view ribbon remain above it. The single results/detail
content region is non-scrolling. The middle results list and right selected-
detail pane are separate native vertical scroll owners, so middle scrolling
cannot move or blank the right detail. Controller rerenders preserve the
independent sidebar, results, and detail positions. Selecting a wide-layout
result saves the current browsing position and aligns that existing selected
row once at the top of the results viewport beside detail reset to its own top,
without changing sort order; subsequent user scrolling is not overridden.
Selecting another row repeats that bounded promotion and detail-top reset.
Closing detail restores the saved result-list position and selected-row focus,
while Reset all returns all three positions to the top. The outer Guide and
three-column workspace remain stationary.
Narrow layouts keep the shared one-pane results, filters, or detail flow. Results
and detail retain the Guide main region as their scroll owner. The narrow filter
pane instead bounds its filter body above an in-layout More filters/Reset footer,
with captured/restored filter scrolling owned by that body and no second outer
scroller. The last expanded control remains before the footer in reading and
keyboard order. Non-sticky orientation and primary-view rows cannot overlay
focused filter controls. Intermediate disclosure layouts retain their existing
main-region scrolling. Actual reachability, focus visibility and compaction at
the required viewports and enlarged text still require rendered review; static
structure and state tests do not establish visual acceptance.
Results initially reveal 25; repeated `Show
more` activation reveals further 25-record increments until all 228 are
reachable, without pagination, virtualization, or hundreds of hidden startup
cards.
Only the selected Resource renders full metadata, exact related Products, and
role-labeled safe access links. Product relationships support reverse detail and
exact Product-context navigation but do not determine primary view membership.
BLM selection includes BLM California, BLM Maps and Geospatial Data, and the
public California Wildfire Dashboard. Opening BLM California retains its 25 related
Products through the existing detail renderer and shared Product navigation,
including ACECs, Federal Wilderness, Wilderness Study Areas, BLM National PLSS /
CadNSDI, BLM-CA PLSS Aliquots / Sections and BLM Surface Management Agency context.
Each link retains its declared role and exact Product reverse Resource entry.
These are related Products, not extra Resource results. Shared HUC/groundwater
links do not make USGS, PRISM or DWR BLM publishers, and publication/hosting does
not establish management authority. The Maps and Geospatial Data collection
adds 25 exact source-reference links for existing consumed services; the wildfire
dashboard has none. These decisions imply no broader land-classification
equivalence, new Product, or map layer.

The exact canonical access point matching
`canonicalUrl` is promoted to the restrained external-blue `Open official
resource` primary action; any other configured access points retain their
projected labels, URLs, and lower visual priority. When broad official landing
or sector pages and configured views are both retained, the broad pages precede
configured views. The NOAA GOES Image Viewer keeps its canonical official action
and presents California-relevant Pacific Southwest and U.S. Pacific Coast sector
pages before its Pacific Southwest GeoColor and Fire Temperature views.
The Resource loader keeps public HTTPS as the universal external-URL default.
Its only HTTP exceptions are an internal exact mapping from the reviewed
Resource ID to the full approved TID WISKI, Kings River Water Association, and
Orange County Hydrology URLs. Registry loading and compiled Guide-bundle
validation call the same validator and exact mapping for canonical,
access-point, and public-source validation; it does not permit host, provider,
suffix, wildcard, or fallback matching. These links are user-initiated external
navigation only: BRIM does not fetch or embed them at runtime or treat them as
machine-facing services. A future HTTP exception requires explicit review, and
the current three-record contract does not establish endpoint monitoring.
Product-to-Resource
navigation enters the same
Explorer with an exact Product-ID relationship filter and relevance boost.
Nested Escape closes Resource detail or facets before root Escape closes Guide,
and Resource state restores when returning to the compact Guide. One shared
model and controller own all responsive layouts; replacement and teardown are
listener-idempotent. The Explorer does not fetch, persist, activate map layers,
or introduce a second Resource authority.

GUIDE-I2B-R15B preserved the 205-Resource authority and its 67 published / 138
staged split while repairing 25 R15A-reviewed endpoint actions and the separate
USBR canonical homepage host. It removes the invalid Sacramento County and
Kern River target identities and substitutes the accepted broad SnowTrax and
Santa Barbara County Real-Time Hydrology records. Both
replacement relationship records remain reviewed
`not_currently_mapped_in_brim`; no Product link changes, so the relationship
authority remains 270 Product records, 205 Resource records, and 86 links.
GUIDE-I2B-R15C then performed fresh bounded verification of all 133 exact
canonical URLs and published the complete revised target by changing only
`publication_state`; this point-in-time gate does not establish continuous
endpoint monitoring. CDEC Reservoir Conditions, the Napa OneRain
root, iSnobal, and the Santa Barbara map route are each retained exactly once
as a configured subordinate or alternate access point on the canonical parent
Resource; none remains as a separate canonical proposal in the revised target.

GUIDE-I2B-R16B implements the balanced USACE/USBR parent-and-access-point
model. It adds exactly 13 published canonical Resources, merges seven exact
over-granular cards into their parents, and adds 123 curated access points with
the approved family counts. Six retired Sacramento District cards remain
discoverable through stable-ID aliases and truthfully labeled current or
legacy/standby child actions on the Sacramento District Water Control Data
System; the retired 2024 Long-Term Operations Record of Decision remains one
child action of the existing LTO program. Report code `scc` is labeled
`Success Dam & Lake`, and the false Sacramento River / Clear Creek phrase is
not public search metadata. Water Control Manuals remains absent as a held
empty-collection candidate.

The base R16B Product relationship is the exact source-code-to-child-URL
crosswalk-proven `source_reference` from `ops_cdec_reservoir_storage` to the
Sacramento District parent. Its existing CDEC relationship and the two existing
USBR links are preserved. The Sacramento parent is reviewed as
`selected_products_in_brim`; the other 12 additions are reviewed as
`not_currently_mapped_in_brim`. The consolidated visual-review correction adds
the exact Drought.gov California and two CoCoRaHS selected-product links and
removes the two unsupported California Water Watch links. The CNRFC relationship
review preserves every existing Product link and adds exactly four:
`cnrfc_stream` and `cnrfc_precip_weather_station_catalog` select the broader
CNRFC Resource, while `ops_major_water_supply_forecasts` and
`ops_cdec_reservoir_storage` gain CNRFC source references. At the C1 gate, authority was
therefore 215 Resources, 210 published, five staged, 270 Products, 215
relationship Resource records, and 96 canonical links: 13 direct, 64 selected,
and 19 source-reference roles. The 96 links cover 72 Products and 27 Resources.
The final currentness
correction retains the stable statewide Integrated Report Resource, replaces
its obsolete 2010 action with the evergreen Water Boards assessment-program
page, adds accurate 2024 and 2026 cycle actions, and links its exact 2024 line
and polygon Products; no 2026 Product exists in the current 272-Product
authority. The C1 public views were 27 / 183 / 210. Resource indexing and matching semantics remain unchanged:
truthful access-point labels plus curated
`search_aliases` preserve reservoir, project, Section 7, regional, program, and
legacy names without indexing URLs or stable IDs. The 123-child distribution
is 30 Sacramento WCDS, five Los Angeles water management, 50 CVO, two national
or regional directories, one LTO decision, five Lower Colorado, six Upper
Colorado, three Colorado Basin hub, three Klamath, two Truckee/TROA, five RISE,
three CVP Water Supply, four Hydromet, and four AgriMet; CVO retains 42
searchable/user-facing and eight ordinary accounting/derived actions.

V4 behavior includes Home reset, Escape/close/focus restoration, typed stable-ID Quick Access, and combined visible facets over BRIM section, multi-valued subject tags, and multi-valued `Information Type` tags. Layer details show `Find in layer list` only for a verified navigable path and otherwise use a source-backed layer-purpose summary; Tool details show `What this tool does` plus `How to open it` only when a current UI control is verified. The maintained information-type metadata vocabulary is `Static Reference`, `Live Observation`, `Forecast / Outlook`, `Model / Simulation`, `Historical Context`, `Screening / Derived`, `External On-Demand Service`, and `Tool / Workflow`; the last remains Tools metadata but is not a permanent browse choice. `Model / Simulation` is assigned only to exact verified model or simulation systems and can coexist with forecast, observation, historical, or screening metadata. User-facing result types distinguish Layer, Tool, Collection, Method, Resource, and Update while the internal Product umbrella remains unchanged. Result secondary lines are generic by entity: Layers use a verified path or purpose, Tools use an action summary, and Collections use exact member scope. Search is deterministic and gives precedence to exact titles and reviewed aliases, then explicit subject and Information Type tags, provider/program, reviewed capability terms, concise source-supported summaries, related Resource titles, and conservative title/alias typo recovery. Ordinary search does not tokenize broad group/subgroup labels, BRIM path components, Product family labels, runtime IDs, raw URLs, controller variables, DOM text, or editorial source references. Exact complete normalized BRIM paths remain discoverable through a separate equality check rather than ordinary token indexing.

Primary Subject is an explicit controlled taxonomy: `Groundwater`, `Surface Water`, `Water Quality`, `Snow & SWE`, `Soil Moisture`, `Precipitation`, `Weather & Forecasts`, `Fire Weather`, `Climate & Drought`, `Fire & Burn Areas`, `Ecology & Habitat`, `Air Quality`, `Water Rights`, `Geology & Geophysics`, `Conservation Lands & Designations`, `Land Ownership & Administration`, `Energy & Minerals`, and `Infrastructure & Conveyance`. Assignments derive from exact Local/Ops stable-ID rules, exact maintained External themes with reviewed stable-ID overrides, or an exact enrichment record. Broad parent groups, subgroups, paths, panel titles, display-name token overlap, and unmatched-record fallback do not assign public subjects. A Product with no confident domain subject remains searchable and present in A–Z with an empty subject array; ordinary Tools rely on `Where in BRIM = Tools` rather than a generic map-tools subject. Current reviewed decisions classify all six PRISM/BCMv8 HUC levels under `Climate & Drought`, Integrated Report records under `Water Quality` plus `Surface Water`, multi-agency streamflow only under `Surface Water`, CPC outlooks under climate/weather rather than land administration, and the BLM Surface Management Agency Layer under `Land Ownership & Administration`. The two contaminated-site records retain empty subject arrays because two records do not justify a new controlled subject, while the two recreation/access records remain structured-basic and subjectless pending a broader reviewed taxonomy case.

Guide I2A2 preserves automatic structured-basic coverage for all 270 post-basemap Products and applies reviewed `SOURCE_BACKED_RICH` vitals to 84 Products. Wave 1 deepens 60 existing records—20 Local, 20 External, and 20 Ops Live—while retaining the prior 24 rich records, including the four current Tools and the SMA Layer. Every Wave 1 record has a concise source-backed summary and at least two meaningful detail areas drawn from capabilities, timing/period boundaries, BRIM processing, and geometry/interpretation limits; Ops Live records also relate to the maintained BRIM Live Update Timing Method. Rich records can also carry a verified optional access hint, multiple subject and information-type tags, and role-labeled Method relationships; Product–Resource relationships come only from the sole relationship registry. Empty sections are omitted. Structured sections, entity presentation, and relationship roles are compiled or selected generically; the browser JS contains no record-ID-specific content branch. Enrichment remains optional: a newly registered Product without an enrichment record continues to compile as `STRUCTURED_BASIC`.

Quick Access contains 11 verified typed destinations. Eight open one exact Layer: `HUC8 – PRISM/BCMv8`, `Groundwater Basins – Bulletin 118`, `NBM Snow Levels`, `Water-Supply Basin Forecasts`, `Delta Operations`, `USDA / SCAN Soil Moisture`, `Snow-Pillow SWE`, and `Water conveyance | BRIM mapped`. Three open exact stable-ID collections through the shared result renderer: `Fire Perimeters` (`EXT070`, `EXT072`, and `EXT074`), `USGS Streamflow` (Local `usgs_streamgages` plus Ops Live `ops_streamflow_usgs_ca`), and `USGS Groundwater` (Local `usgs_wells` plus Ops Live `product-ops-usgs-groundwater`). Every destination declares `entryKind` and a restrained visible type label; collections declare exact `memberIds`. Collection rows retain each Product's title, BRIM section/path, provider, and entity type rather than merging subsystem identities. The conveyance destination uses stable ID `brim_mapped_conveyance`, the normal curated combined layer assembled by BRIM from multiple reviewed sources. Older source-specific `Major Conveyance` and DeltaMAPP layers remain rollback/QA inputs and are not substituted into Quick Access. `SCAN Soil Moisture` remains a Guide-only display title for stable ID `ops_scan_soil_moisture`; the runtime label and exact map path are unchanged, and its subject tag is exactly `Soil Moisture`.

The initial maintained Methods set contains exactly seven entries: How BRIM Works; Display Geometry & Generalization; SCAN Soil Moisture Statistical Context; Snow-Pillow SWE Statistical Context; USGS Groundwater History Summaries; BRIM Live Update Timing; and BRIM Under the Hood. How BRIM Works presents the practical Local Layers, Ops Live, External Layers, and Tools distinctions; HUC processing remains in the shared HUC method/detail content. The Display Geometry Method compiles the current public disclosure rows from the maintained polygon-generalization registry. Local GIS File Upload, External GIS URL Overlay, Measure, and Draw / Label carry concise source-backed action summaries, verified access hints, supported capabilities, and limitations without fabricated layer paths. The SMA record is instead a Layer at `External Layers / Federal Land Status / Fed/State Surface Management Agency (SMA)`. The initial Updates set contains three reverse-chronological, repository-verified entries: Read-only Layer Explorer added (2026-08-24), NBM accumulated QPF forecast windows added (2026-08-20), and NBM legend links simplified (2026-08-19). Resource records remain source links, not runtime data dependencies; the HUC8 record points to the official PRISM normals page and the USGS ScienceBase BCMv8 catalog item.

Guide typography follows the accepted V4 prototype and current BRIM DOI/BLM asset conventions; no separate official DOI stylesheet exists in repository authority (`DOI_STYLE_SOURCE_STATUS=ACCEPTED_V4_AND_CURRENT_BRIM_REFERENCE_ONLY`). The status is therefore `DOI_BLM_ALIGNED_TO_ACCEPTED_V4`, not a claim of DOI design-system compliance. A condensed sans stack is reserved for Guide identity and section headings. Navigation, controls, Product titles, paths, metadata, results, facets, tables, and relationship labels use the ordinary UI sans stack. The serif reading stack is permitted only for long Method paragraphs. Fonts remain system stacks: there is no webfont dependency or copied font file.

Guide I2A2 remains read-only with respect to map state. Layer detail pages report an exact verified BRIM path when one exists; Tool detail pages report only source-backed action/capability text and a verified access hint. Runtime/controller ownership remains internal and Guide metadata contains no layer/controller callbacks. Timing text distinguishes the observation/forecast period shown, BRIM retrieval time where supported, and upstream publication cadence; it does not imply refresh guarantees, producer freshness, or map-health status. About / Contact provides an encoded `mailto:doconnor@blm.gov` draft action with subject `BRIM Guide feedback`; there is no contact backend, persistence, or send claim. Legacy Notes is absent from current builds: there is no toolbar entry, Guide destination, search record, embedded payload, hidden renderer, modal, or handler. Historical Notes content remains available only through older HTML artifacts or Git history and is not migrated into current Methods content.

New Product onboarding extends current runtime authority rather than a parallel Guide inventory: add the Local registry/controller group, External catalog row, Ops `addOpsLayer()` definition plus its stable Ops identity, or reviewed tool definition as appropriate. Add exactly one corresponding Product record to `guide_product_resource_relationships.json`; if its canonical Resource already exists, that relationship registry is the only relationship-data edit, and `not_yet_reviewed` with zero links is valid. If a new canonical Resource is required, add it to `guide_resources.json` and link it from the Product record. Ordinary relationship additions are data-only and do not require compiler, JavaScript, CSS, QA-source, or architecture changes unless the schema or vocabulary changes. Runtime basemap additions remain excluded from the Guide Product projection and require the explicit basemap identity/exclusion parity check to be reconciled. Focused Guide tests must then prove stable-ID/path parity, projection, search boundaries, payload size, and descriptive-catalog independence. Keep the browser implementation to one namespaced JS source, one namespaced CSS source, and the existing R compile seam; do not introduce a frontend build, production Python compiler, duplicate controller registry, or runtime Guide-data request.

## Semantic features and geometry components

### BLM well inventory ownership

`02_preprocess/18_blm_groundwater_well_inventory.r` owns independent NOC
CSV ingestion and explicit corrected Albion ingestion. Its pure attribute
functions can be extracted for synthetic QA without executing initialization
or output writes. Albion `record_uid` is the accepted `final_site_uid`;
historical keys remain separate. The dedicated Albion intermediate retains
all curated master fields and namespaced correction/measurement lineage;
the combined intermediate carries the common map schema plus the logical
Albion fields `water_level_recorded` and `lab_sample_documented`. The shared
cache owner validates those fields for Albion and excludes them from the
NOC child, preserving NOC's cache and browser schemas. Albion's browser
projection also requires complete logical fields; missing schema never
silently becomes a zero observation count.

`03_functions/blm_gw_well_inventory_cache_helpers.r` is the single owner of
well-family cache preparation, keyed distance joins and display fields.
The ordinary administrative/water cache block and
`05_map_build/14_refresh_blm_gw_well_inventory_cache.r` share it. The focused
caller requires complete matching distance keys, sources and coordinates
and validates output collisions before saving the two well children with
the existing cache-save utility. It performs no normalization, distance
recomputation or other cache refresh. Normal full-build dispatch retains
its existing stages; focused operations are explicit runner functions.

The shared Local well/spring helper keeps the existing well layer identities,
clustering, filters and teardown ownership. Albion adds a distinct spring
status, site totals, intersecting observation/status/BLM filters and neutral
hover IDs for intentionally unnamed sites;
those IDs never become authoritative names or map labels. Normal Albion
OFF-to-ON activation fits all valid inventory coordinates once with 38-pixel
padding. Filters, labels, Reset, Clear and removal do not navigate; empty or
invalid bounds are ignored. The popup omits Review status while retaining the
underlying field and source-supported Unresolved attributes.

The Local panel uses GW wells | BLM NOC inventory and GW sites | 2025 Mojave
limited field inventory, with record counts supplied dynamically. Their legend
titles are BLM NOC well inventory and Mojave limited field inventory (2025).
NOC's source line identifies BLM National Operations Center; its symbol text
remains NOC well record. This identifies an inventory source without asserting
universal drilling, ownership or current monitoring. Albion uses Field-reported
basin for the unchanged reported value, separately from spatial basin attribution.
Technical IDs, API and filenames remain unchanged. Registry aliases and the
Local group helper route historical names to the corresponding current Points
group and Labels companion; final builder counts/registration and the independent
`config_labels.r` inline pairs use those names. NOC/Albion label minima remain
9/10. The generic inline-label matcher, existing well controllers and separate
Springs controller remain unchanged. Source
history and unresolved attributes belong in
[BLM well inventories](features/BLM_WELL_INVENTORIES.md); execution boundaries
and output paths belong in [BUILD.md](../BUILD.md).

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

The combined payload policy approved by `BRIM-R17C2-G2-02` enforces a 940000-byte
hard cap for the full default Guide JSON and a 626502-byte growth cap over the
unchanged 313498-byte historical baseline. Both yield a 940000-byte total
ceiling; the nonnegative-growth check and 200000-byte combined Guide JS/CSS
guard remain. The pre-polish C1 default fixture measured 832232 bytes,
518734 above the historical baseline, with 2768 bytes of headroom. That earlier
measurement does not establish the size of a changed corpus: each candidate
requires its own full compiler measurement against the same caps. The complete L082-inclusive selected metadata measured 896387 bytes, with
582889 bytes of growth and 43613 bytes of headroom. The single allowance covers
the complete approved wave; the full serializer and baseline are unchanged.
Browser responsiveness, performance
judgment, and human visual acceptance remain pending; a payload-size assertion
alone does not establish them.

## Build and environment separation

Author in the lean source repository, integrate/test in `codex_ship`, and deploy to production only after merge and backup. See `BUILD.md`.

## Testing standard

Resource publication staging tests cover both the historical R17B fixture
(206 published Resources becoming 205), frozen C1 fixture (210 becoming 209),
and current C2 fixture (228 becoming 227). Each stages only `resource_doi`, preserves ordered unaffected IDs and full
records, excludes all five already-staged Resources, and proves that restoring
the one publication state restores the original fixture. Historical golden
hashes and the 139-ID historical cohort normalization remain protected.

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
