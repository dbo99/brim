# ==== install_delta_ops_promote_patch.R ======================================
#
# PURPOSE:
#   Apply narrow in-place edits needed to wire the Delta Ops live feed into BRIM.
#   This preserves your current local helper/config files and creates timestamped
#   .bak_delta_ops_* backups before each edit.
#
# RUN FROM PORTATREASURE2 ROOT:
#   source("05_map_build/dev_sandbox/install_delta_ops_promote_patch.R")
# ============================================================================

root <- getwd()
message("BRIM root: ", root)

ts <- format(Sys.time(), "%Y%m%d_%H%M%S")

read_file <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")
write_file <- function(path, txt) writeLines(strsplit(txt, "\n", fixed = TRUE)[[1]], path, useBytes = TRUE)

backup_file <- function(path) {
  bak <- paste0(path, ".bak_delta_ops_", ts)
  if (!file.copy(path, bak, overwrite = FALSE)) stop("Could not create backup: ", bak)
  message("Backup: ", bak)
}

insert_after_once <- function(txt, token, insert, label) {
  if (grepl(insert, txt, fixed = TRUE)) {
    message("Already installed: ", label)
    return(txt)
  }
  pos <- regexpr(token, txt, fixed = TRUE)[[1]]
  if (is.na(pos) || pos < 0) stop("Token not found for ", label, ": ", token)
  end <- pos + nchar(token) - 1
  paste0(substr(txt, 1, end), insert, substr(txt, end + 1, nchar(txt)))
}

insert_before_once <- function(txt, token, insert, label) {
  if (grepl(insert, txt, fixed = TRUE)) {
    message("Already installed: ", label)
    return(txt)
  }
  pos <- regexpr(token, txt, fixed = TRUE)[[1]]
  if (is.na(pos) || pos < 0) stop("Token not found for ", label, ": ", token)
  paste0(substr(txt, 1, pos - 1), insert, substr(txt, pos, nchar(txt)))
}

replace_once <- function(txt, token, replacement, label) {
  if (grepl(replacement, txt, fixed = TRUE)) {
    message("Already installed: ", label)
    return(txt)
  }
  pos <- regexpr(token, txt, fixed = TRUE)[[1]]
  if (is.na(pos) || pos < 0) stop("Token not found for ", label, ": ", token)
  paste0(
    substr(txt, 1, pos - 1),
    replacement,
    substr(txt, pos + nchar(token), nchar(txt))
  )
}

# ---- 1. leaflet_ops_live_helpers.r -----------------------------------------

ops_helpers <- file.path(root, "03_functions", "leaflet_ops_live_helpers.r")
if (!file.exists(ops_helpers)) stop("Missing: ", ops_helpers)
backup_file(ops_helpers)
txt <- read_file(ops_helpers)

txt <- insert_after_once(
  txt,
  'pt_ops_live_source_module("leaflet_ops_live_snow_pillow_helpers.r", "Ops Live snow-pillow / SWE helper")',
  '\npt_ops_live_source_module("leaflet_ops_live_delta_ops_helpers.r", "Ops Live Delta operations helper")',
  "source Delta Ops helper"
)

txt <- insert_after_once(
  txt,
  "  var SNOW_PILLOW_PRIOR_WY_FALLBACK_TRACES_URL = data && data.snowPillowPriorWyFallbackTracesUrl ? String(data.snowPillowPriorWyFallbackTracesUrl) : '';",
  "\n  var includeDeltaOpsDailySummary = !!(data && data.includeDeltaOpsDailySummary);\n  var DELTA_OPS_DAILY_SUMMARY_URL = data && data.deltaOpsDailySummaryUrl ? String(data.deltaOpsDailySummaryUrl) : '';\n  var DELTA_OPS_DAILY_SUMMARY_SUMMARY_URL = data && data.deltaOpsDailySummarySummaryUrl ? String(data.deltaOpsDailySummarySummaryUrl) : '';\n  var DELTA_OPS_X2_REFERENCE_URL = data && data.deltaOpsX2ReferenceUrl ? String(data.deltaOpsX2ReferenceUrl) : '';",
  "Delta Ops browser data variables"
)

txt <- insert_after_once(
  txt,
  "__PT_OPS_LIVE_SNOW_PILLOW_HELPERS_JS__",
  "\n\n__PT_OPS_LIVE_DELTA_OPS_HELPERS_JS__",
  "Delta Ops JS token"
)

txt <- insert_after_once(
  txt,
  '    "__PT_OPS_LIVE_SNOW_PILLOW_HELPERS_JS__" = "pt_ops_live_snow_pillow_js",',
  '\n    "__PT_OPS_LIVE_DELTA_OPS_HELPERS_JS__" = "pt_ops_live_delta_ops_js",',
  "Delta Ops JS helper map"
)

txt <- insert_after_once(
  txt,
  "      snowPillowPriorWyFallbackTracesUrl = if (!is.null(map_display$ops_snow_pillow_prior_wy_fallback_traces_url)) {\n        map_display$ops_snow_pillow_prior_wy_fallback_traces_url\n      } else {\n        \"\"\n      },",
  "\n      includeDeltaOpsDailySummary = if (!is.null(map_display$add_ops_delta_ops_daily_summary)) {\n        isTRUE(map_display$add_ops_delta_ops_daily_summary)\n      } else {\n        FALSE\n      },\n      deltaOpsDailySummaryUrl = if (!is.null(map_display$ops_delta_ops_daily_summary_url)) {\n        map_display$ops_delta_ops_daily_summary_url\n      } else {\n        \"\"\n      },\n      deltaOpsDailySummarySummaryUrl = if (!is.null(map_display$ops_delta_ops_daily_summary_summary_url)) {\n        map_display$ops_delta_ops_daily_summary_summary_url\n      } else {\n        \"\"\n      },\n      deltaOpsX2ReferenceUrl = if (!is.null(map_display$ops_delta_ops_x2_reference_url)) {\n        map_display$ops_delta_ops_x2_reference_url\n      } else {\n        \"\"\n      },",
  "Delta Ops data list entries"
)

write_file(ops_helpers, txt)

# ---- 2. config_map_display.r ------------------------------------------------

config <- file.path(root, "00_config", "config_map_display.r")
if (!file.exists(config)) stop("Missing: ", config)
backup_file(config)
txt <- read_file(config)

insert_cfg <- '\n  # Delta Ops Daily Summary / CVP-SWP snapshot\n  add_ops_delta_ops_daily_summary = TRUE,\n  ops_delta_ops_daily_summary_url = "https://dbo99.github.io/brim-live-data-feeds/data/delta_ops_daily_summary_features.geojson",\n  ops_delta_ops_daily_summary_summary_url = "https://dbo99.github.io/brim-live-data-feeds/data/delta_ops_daily_summary_summary.json",\n  ops_delta_ops_x2_reference_url = "https://dbo99.github.io/brim-live-data-feeds/data/delta_ops_x2_reference.geojson",\n'

if (!grepl("add_ops_delta_ops_daily_summary", txt, fixed = TRUE)) {
  token <- '  add_ops_live_layers = TRUE,'
  txt <- insert_after_once(txt, token, insert_cfg, "Delta Ops config block")
}
write_file(config, txt)

# ---- 3. leaflet_ops_live_layer_definition_helpers.r ------------------------

layer_defs <- file.path(root, "03_functions", "leaflet_ops_live_layer_definition_helpers.r")
if (!file.exists(layer_defs)) stop("Missing: ", layer_defs)
backup_file(layer_defs)
txt <- read_file(layer_defs)

block <- "\n\n  if (includeDeltaOpsDailySummary && DELTA_OPS_DAILY_SUMMARY_URL) {\n    addOpsLayer({\n      category: 'Hydro Observations',\n      subgroup: 'Delta operations',\n      name: 'Delta ops snapshot | CVP/SWP',\n      sourceUrl: 'https://water.ca.gov/-/media/DWR-Website/Web-Pages/Programs/State-Water-Project/Operations-And-Maintenance/Files/Operations-Control-Office/Delta-Status-And-Operations/Delta-Operations-Daily-Summary.pdf',\n      infoUrl: DELTA_OPS_DAILY_SUMMARY_SUMMARY_URL || DELTA_OPS_DAILY_SUMMARY_URL,\n      infoLabel: DELTA_OPS_DAILY_SUMMARY_SUMMARY_URL ? 'summary' : 'GeoJSON',\n      refreshable: true,\n      extraRowHtml: '<label class=\\\"pt-ops-row-mini-toggle\\\" title=\\\"Show/hide Delta Ops labels\\\"><input type=\\\"checkbox\\\" data-pt-ops-action=\\\"delta-labels\\\" checked>lbl</label><a href=\\\"#\\\" class=\\\"pt-ops-row-mini-action\\\" data-pt-ops-action=\\\"delta-ops-zoom\\\" title=\\\"Zoom to Delta Ops snapshot extent\\\">z</a>',\n      helperText: 'Daily DWR Delta Ops snapshot: exports, gates, outflow, OMR, X2, Delta status/control, East Side Streams estimate, and San Luis split. Values are preliminary.',\n      layer: new DeltaOpsDailySummaryLayer({\n        name: 'Delta ops snapshot | CVP/SWP',\n        url: DELTA_OPS_DAILY_SUMMARY_URL,\n        summaryUrl: DELTA_OPS_DAILY_SUMMARY_SUMMARY_URL,\n        x2ReferenceUrl: DELTA_OPS_X2_REFERENCE_URL,\n        sourceUrl: 'https://water.ca.gov/-/media/DWR-Website/Web-Pages/Programs/State-Water-Project/Operations-And-Maintenance/Files/Operations-Control-Office/Delta-Status-And-Operations/Delta-Operations-Daily-Summary.pdf',\n        note: 'DWR Delta Operations Daily Summary, parsed daily by the BRIM live-feed workflow. Displays a presentation-style Delta/CVP/SWP operating snapshot. Preliminary data; subject to revision without notice.',\n        zoomOnAdd: true\n      })\n    });\n  }\n"

if (!grepl("Delta ops snapshot | CVP/SWP", txt, fixed = TRUE)) {
  token <- "  addOpsLayer({\n    category: 'Observations',\n    subgroup: 'Flows / levels / snow / moisture / etc',\n    name: 'Streamflow | multi-agency | Nat\\'l',"
  if (grepl(token, txt, fixed = TRUE)) {
    txt <- insert_before_once(txt, token, block, "Delta Ops layer definition")
  } else {
    token2 <- "  if (includeUsgsStreamflowLatest && USGS_STREAMFLOW_LATEST_URL) {"
    txt <- insert_before_once(txt, token2, block, "Delta Ops layer definition")
  }
}
write_file(layer_defs, txt)

# ---- 4. leaflet_ops_live_panel_helpers.r -----------------------------------

panel <- file.path(root, "03_functions", "leaflet_ops_live_panel_helpers.r")
if (!file.exists(panel)) stop("Missing: ", panel)
backup_file(panel)
txt <- read_file(panel)

# Add Hydro Observations to category/subgroup sort if not already present.
if (!grepl("'Hydro Observations'", txt, fixed = TRUE) && !grepl("\"Hydro Observations\"", txt, fixed = TRUE)) {
  txt <- replace_once(
    txt,
    "    var subgroupOrder = {",
    "    var subgroupOrder = {\n      'Hydro Observations': [\n        'Delta operations',\n        'Precip / QPE',\n        'Flows / levels / snow / moisture / etc'\n      ],",
    "Hydro Observations subgroup sort"
  )

  txt <- replace_once(
    txt,
    "    var categoryOrder = [\n      'Observations',",
    "    var categoryOrder = [\n      'Hydro Observations',\n      'Observations',",
    "Hydro Observations category sort"
  )
} else {
  message("Already installed/present: Hydro Observations category/subgroup sort")
}

# Include optional extra row HTML from layer definition.
txt <- replace_once(
  txt,
  "        layerRowLinksHtml(def) +\n        '</div>';",
  "        layerRowLinksHtml(def) +\n        (def.extraRowHtml ? String(def.extraRowHtml) : '') +\n        '</div>';",
  "Ops row extra HTML"
)

# Add click handling for Delta Ops zoom before refresh handling.
txt <- insert_after_once(
  txt,
  "    body.addEventListener('click', function(e) {",
  "\n      var deltaZoomLink = e.target && e.target.closest ? e.target.closest('[data-pt-ops-action=\"delta-ops-zoom\"]') : null;\n      if (deltaZoomLink && body.contains(deltaZoomLink)) {\n        e.preventDefault();\n        e.stopPropagation();\n        if (window.ptDeltaOpsZoomToDefault && typeof window.ptDeltaOpsZoomToDefault === 'function') {\n          window.ptDeltaOpsZoomToDefault();\n        }\n        return;\n      }\n",
  "Delta Ops zoom row action"
)

# Add change handling for small labels checkbox before normal checks are wired.
txt <- insert_before_once(
  txt,
  "    var checks = body.querySelectorAll('input[data-pt-ops-index]');",
  "    body.addEventListener('change', function(e) {\n      var deltaLabelToggle = e.target && e.target.closest ? e.target.closest('[data-pt-ops-action=\"delta-labels\"]') : null;\n      if (!deltaLabelToggle || !body.contains(deltaLabelToggle)) return;\n      e.preventDefault();\n      e.stopPropagation();\n      var show = !!deltaLabelToggle.checked;\n      window.ptDeltaOpsLabelsVisible = show;\n      if (window.ptDeltaOpsSetLabelsVisible && typeof window.ptDeltaOpsSetLabelsVisible === 'function') {\n        window.ptDeltaOpsSetLabelsVisible(show);\n      }\n    });\n\n",
  "Delta Ops labels row toggle"
)

write_file(panel, txt)

message("Delta Ops promotion patch installed.")
message("Next: run 02_preprocess/50_build_delta_ops_x2_lookup.R, then run/rebuild GitHub feed and final BRIM map.")
