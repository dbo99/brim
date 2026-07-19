# ==== RO_001_ops_live_panel_ui_sandbox.R ==================================
#
# PURPOSE:
#   Build a standalone, no-data Ops Live panel sandbox for the RO_001
#   reorganization workstream.
#
#   This is intentionally NOT a production BRIM map patch. It does not source
#   the main BRIM build, does not query live services, and does not modify any
#   existing helper files. Its only job is to make a fast visual mockup of the
#   proposed Ops Live group order, subgroup labels, and compact row-link labels:
#
#     rfrsh · lgnd · srce
#
# WHY THIS EXISTS:
#   Full BRIM HTML builds are slow enough that it is worth approving the panel
#   layout before touching production Ops helpers. After this sandbox is approved,
#   the next patches can wire the same ideas into the real Ops panel.
#
# OUTPUT:
#   06_output/html/dev_sandbox/RO_001_ops_live_panel_ui_sandbox_<timestamp>.html
#
# SAFE DEFAULTS:
#   - Base R only; no package installs.
#   - Writes a timestamped HTML file.
#   - Does not overwrite existing outputs.
#   - Opens the output in your browser by default.
#
# RUN FROM PROJECT ROOT:
#   local({
#     old_wd <- getwd(); on.exit(setwd(old_wd), add = TRUE)
#     setwd("C:/Users/doconnor/OneDrive - DOI/Documents/PortaTreasure2")
#     source("05_map_build/dev_sandbox/RO_001_ops_live_panel_ui_sandbox.R")
#   })
# ============================================================================

# ---- Settings ---------------------------------------------------------------

open_in_browser <- TRUE

# ---- Locate project root ----------------------------------------------------

find_project_root <- function(start_dir = getwd(), max_depth = 8) {
  cur <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)

  for (i in seq_len(max_depth)) {
    has_expected_dirs <- dir.exists(file.path(cur, "03_functions")) &&
      dir.exists(file.path(cur, "05_map_build")) &&
      dir.exists(file.path(cur, "06_output"))

    if (has_expected_dirs) return(cur)

    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }

  stop(
    "Could not find the BRIM project root from: ", start_dir, "\n",
    "Run this script from the PortaTreasure2 project root, or from a folder ",
    "inside that project."
  )
}

project_root <- find_project_root()
out_dir <- file.path(project_root, "06_output", "html", "dev_sandbox")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_file <- file.path(out_dir, paste0("RO_001_ops_live_panel_ui_sandbox_", ts, ".html"))

# ---- Small HTML helpers -----------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (length(x) == 1 && is.na(x)) return(y)
  x
}

html_escape <- function(x) {
  x <- as.character(x %||% "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

link_html <- function(label, class = "") {
  label <- html_escape(label)
  class_attr <- if (nzchar(class)) paste0(" ", html_escape(class)) else ""
  paste0("<a href=\"#\" class=\"mock-link", class_attr, "\" onclick=\"return false;\">", label, "</a>")
}

row_html <- function(name, links = character(), note = NULL, checked = FALSE) {
  checked_attr <- if (isTRUE(checked)) " checked" else ""
  link_bits <- character()

  if ("rfrsh" %in% links) link_bits <- c(link_bits, link_html("rfrsh", "mock-refresh"))
  if ("lgnd" %in% links) link_bits <- c(link_bits, link_html("lgnd"))
  if ("guide" %in% links) link_bits <- c(link_bits, link_html("guide"))
  if ("summary" %in% links) link_bits <- c(link_bits, link_html("summary"))
  if ("srce" %in% links) link_bits <- c(link_bits, link_html("srce"))

  links_html <- if (length(link_bits)) {
    paste0("<span class=\"pt-ops-row-links\">", paste(link_bits, collapse = "<span class=\"sep\">·</span>"), "</span>")
  } else {
    ""
  }

  note_html <- if (!is.null(note) && nzchar(note)) {
    paste0("<div class=\"row-note\">", html_escape(note), "</div>")
  } else {
    ""
  }

  paste0(
    "<div class=\"pt-ops-layer-row\">",
    "<label class=\"pt-ops-layer-label\"><input type=\"checkbox\"", checked_attr, "> ",
    "<span class=\"pt-ops-layer-name\">", html_escape(name), "</span></label>",
    links_html,
    note_html,
    "</div>"
  )
}

subgroup_html <- function(title, rows, note = NULL) {
  note_html <- if (!is.null(note) && nzchar(note)) {
    paste0("<div class=\"subgroup-note\">", html_escape(note), "</div>")
  } else {
    ""
  }

  paste0(
    "<div class=\"pt-ops-subgroup\">",
    "<div class=\"pt-ops-subgroup-title\">", html_escape(title), "</div>",
    note_html,
    paste(rows, collapse = "\n"),
    "</div>"
  )
}

section_html <- function(title, subgroups = list(), rows = character(), note = NULL) {
  note_html <- if (!is.null(note) && nzchar(note)) {
    paste0("<div class=\"section-note\">", html_escape(note), "</div>")
  } else {
    ""
  }

  body <- c(unlist(subgroups, use.names = FALSE), rows)

  paste0(
    "<section class=\"pt-ops-section\">",
    "<h3>", html_escape(title), "</h3>",
    note_html,
    paste(body, collapse = "\n"),
    "</section>"
  )
}

external_link_block <- function(title, links, note = "External links, not map layers.") {
  bits <- vapply(links, function(x) link_html(x), character(1))
  paste0(
    "<div class=\"pt-ops-external-link-block\">",
    "<div class=\"pt-ops-external-link-title\">", html_escape(title), "</div>",
    "<div class=\"pt-ops-external-link-note\">", html_escape(note), "</div>",
    "<div class=\"pt-ops-external-link-row\">", paste(bits, collapse = "<span class=\"sep\">·</span>"), "</div>",
    "</div>"
  )
}

# ---- Proposed RO_001 sandbox layout ----------------------------------------

observations <- section_html(
  "Observations",
  note = "Candidate subgroup layout. Cameras are treated as visual observations because they support fire, snowline, smoke, clouds, access, and storm ground-truth checks.",
  subgroups = list(
    subgroup_html(
      "Visual / cameras",
      rows = c(
        row_html("ALERTCalifornia Cameras", c("rfrsh", "srce"), checked = TRUE),
        row_html("ALERTCalifornia Camera Viewsheds", c("rfrsh", "srce"))
      )
    ),
    subgroup_html(
      "Precipitation / radar / QPE",
      rows = c(
        row_html("IEM NEXRAD Radar (WMS)", c("srce")),
        row_html("NOAA MRMS Radar Reflectivity", c("lgnd", "srce")),
        row_html("MRMS QPE 1-hour", c("lgnd", "srce")),
        row_html("MRMS QPE 1-day", c("lgnd", "srce")),
        row_html("MRMS QPE 3-day", c("lgnd", "srce")),
        row_html("NWS QPE Mosaic 1-day", c("lgnd", "srce")),
        row_html("NWS QPE Mosaic 7-day", c("lgnd", "srce")),
        row_html("CoCoRaHS California Daily Reports", c("summary", "srce")),
        row_html("CoCoRaHS 50-State Daily Reports", c("summary", "srce"))
      ),
      note = "QPE and station reports read as observed/recent precipitation rather than forecasts."
    ),
    subgroup_html(
      "Flows / levels / moisture",
      rows = c(
        row_html("Streamflow: Live Agency Gages", c("rfrsh", "srce")),
        row_html("Streamflow: USGS NWIS Latest", c("summary", "srce")),
        row_html("Groundwater: USGS Latest Levels", c("summary", "srce")),
        row_html("Soil Moisture: SCAN Latest", c("summary", "srce")),
        row_html("Snow: Snow Pillow / SWE Latest", c("summary", "srce"))
      ),
      note = "Working subgroup name. Alternatives: Hydrologic observations; Water conditions; Flow / storage / levels."
    ),
    subgroup_html(
      "Wind",
      rows = c(
        row_html("NWS Surface Wind Barbs", c("lgnd", "srce"), note = "Kept at the bottom of Observations for now.")
      )
    )
  )
)

reservoirs <- section_html(
  "Reservoirs",
  rows = c(
    row_html("Reservoir Ops: CDEC / CNRFC / USACE", c("summary", "srce"))
  ),
  note = "Kept directly after Observations because popups mix observed storage, forecast/release links, and source context."
)

forecasts <- section_html(
  "Forecasts / Outlooks",
  subgroups = list(
    subgroup_html(
      "QPF",
      rows = c(
        row_html("WPC QPF Day 1", c("lgnd", "srce")),
        row_html("WPC QPF Day 2", c("lgnd", "srce")),
        row_html("WPC QPF Day 3", c("lgnd", "srce")),
        row_html("WPC QPF 3-day", c("lgnd", "srce")),
        row_html("WPC QPF 7-day", c("lgnd", "srce")),
        external_link_block("CNRFC QPF graphics", c("1-day total", "3-day total", "6-day total")),
        external_link_block("CW3E QPF comparison", c("multi-model"))
      )
    ),
    subgroup_html(
      "CPC outlooks",
      rows = c(
        row_html("CPC 6-10 Day Temperature Outlook", c("rfrsh", "srce")),
        row_html("CPC 6-10 Day Precipitation Outlook", c("rfrsh", "srce")),
        row_html("CPC 8-14 Day Temperature Outlook", c("rfrsh", "srce")),
        row_html("CPC 8-14 Day Precipitation Outlook", c("rfrsh", "srce"))
      ),
      note = "Putting CPC here avoids the word Climate under Drought sending users to the wrong place."
    )
  )
)

hazards <- section_html(
  "Hazards",
  rows = c(
    row_html("NWS Watches / Warnings / Advisories", c("lgnd", "srce")),
    row_html("WPC ERO Day 1", c("lgnd", "srce")),
    row_html("WPC ERO Day 2", c("lgnd", "srce")),
    row_html("WPC ERO Day 3", c("lgnd", "srce"))
  )
)

fire <- section_html(
  "Fire",
  rows = c(
    row_html("NIFC Current Wildfire Perimeters", c("rfrsh", "srce")),
    row_html("CAL FIRE Recent Large Fire Perimeters", c("rfrsh", "srce"), note = "QA must confirm existing hover fields survive promotion: fire name, year, GIS acres.")
  ),
  note = "Only active/recent perimeter layers here. Cameras stay in Observations. The full historical CAL FIRE perimeter layer should probably remain External for now."
)

drought <- section_html(
  "Drought",
  rows = c(
    row_html("U.S. Drought Monitor", c("rfrsh", "lgnd", "srce"))
  ),
  note = "Shorter heading avoids confusing users looking for CPC climate outlooks. Room remains for future drought/water-shortage layers."
)

weather_ref <- section_html(
  "Weather Offices / Boundaries",
  rows = c(
    row_html("NWS WFO Boundaries", c("rfrsh", "srce"))
  ),
  note = "Possible replacement for 'Weather Reference'. Alternatives: Weather boundaries; NWS offices; Forecast offices."
)

satellite <- section_html(
  "Satellite / Imagery",
  rows = c(
    row_html("NOAA GOES GeoColor", c("guide", "srce")),
    row_html("NOAA GOES Infrared", c("guide", "srce")),
    row_html("NOAA GOES Water Vapor", c("guide", "srce")),
    row_html("NASA MODIS Terra True Color", c("guide", "srce"))
  ),
  note = "Kept as a separate image-heavy group rather than folded into Observations."
)

satellite_calls <- paste0(
  "<section class=\"notes-card\">",
  "<h2>Satellite/imagery call check from current code</h2>",
  "<p><b>NOAA GOES ImageServer layers:</b> current code points to <code>MERGEDGC_current</code>, <code>ABI13_current</code>, and <code>ABI10_current</code>. The ArcGIS export helper builds <code>/exportImage</code> calls from the current map bbox/size and appends <code>_=&lt;Date.now()&gt;</code>. I do not see a hard-coded date in those GOES calls.</p>",
  "<p><b>NASA MODIS Terra WMTS:</b> current code builds a GIBS tile URL with <code>/default/default/GoogleMapsCompatible_Level9/{z}/{y}/{x}.jpg</code>. I do not see a hard-coded date, but <code>default/default</code> means the source service controls the default/latest available date. If it appears stuck, a later production patch could explicitly set or diagnose the WMTS time dimension.</p>",
  "</section>"
)

panel_html <- paste(
  observations,
  reservoirs,
  forecasts,
  hazards,
  fire,
  drought,
  weather_ref,
  satellite,
  sep = "\n"
)

# ---- HTML document ----------------------------------------------------------

html <- paste0(
'<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>RO_001 Ops Live panel UI sandbox</title>
<style>
  :root {
    --ops-bg: #e5f1ee;
    --ops-border: #8aa8a2;
    --ops-header: #d7e8e4;
    --ops-section: #c7ded8;
    --text: #1f2d2b;
    --muted: #5f6f6b;
    --link: #2f5f8f;
    --map-water: #cdeeff;
    --map-land: #eeeeee;
  }
  body {
    margin: 0;
    font-family: Arial, Helvetica, sans-serif;
    color: var(--text);
    background: linear-gradient(90deg, var(--map-water) 0 32%, var(--map-land) 32% 100%);
  }
  .mock-map {
    min-height: 100vh;
    position: relative;
    overflow: hidden;
  }
  .mock-map:before {
    content: "";
    position: absolute;
    inset: 0;
    background-image:
      linear-gradient(28deg, transparent 0 47%, rgba(90,130,110,0.28) 48%, transparent 51%),
      linear-gradient(118deg, transparent 0 58%, rgba(80,120,170,0.25) 59%, transparent 61%),
      radial-gradient(circle at 46% 43%, rgba(255,218,80,0.32) 0 1px, transparent 2px),
      radial-gradient(circle at 53% 65%, rgba(255,218,80,0.28) 0 1px, transparent 2px);
    background-size: 100% 100%, 100% 100%, 34px 34px, 43px 43px;
    opacity: 0.7;
  }
  .left-note {
    position: absolute;
    left: 16px;
    top: 16px;
    width: 430px;
    max-width: calc(100vw - 430px);
    background: rgba(255,255,255,0.86);
    border: 1px solid #c6c6c6;
    border-radius: 8px;
    padding: 14px 16px;
    box-shadow: 0 2px 10px rgba(0,0,0,0.15);
    font-size: 13px;
    line-height: 1.35;
  }
  .left-note h1 {
    font-size: 18px;
    margin: 0 0 8px 0;
  }
  .left-note ul {
    margin: 8px 0 0 18px;
    padding: 0;
  }
  .left-note li {
    margin: 3px 0;
  }
  .pt-ops-live-wrap {
    position: absolute;
    right: 14px;
    top: 12px;
    width: 405px;
    max-width: calc(100vw - 28px);
    border: 1px solid var(--ops-border);
    border-radius: 6px;
    background: var(--ops-bg);
    box-shadow: 0 2px 11px rgba(0,0,0,0.25);
    overflow: hidden;
    z-index: 10;
  }
  .pt-ops-live-header {
    background: var(--ops-header);
    border-bottom: 1px solid var(--ops-border);
    padding: 8px 10px;
    display: flex;
    align-items: center;
    justify-content: space-between;
    gap: 10px;
    font-weight: 700;
  }
  .pt-ops-live-active-count {
    font-weight: 400;
    color: #555;
    font-size: 12px;
    margin-left: 4px;
  }
  .pt-ops-ribbon-clear {
    font-size: 12px;
    border: 1px solid #888;
    background: #f7f7f7;
    border-radius: 4px;
    padding: 4px 8px;
    cursor: pointer;
  }
  .pt-ops-live-body {
    max-height: calc(100vh - 74px);
    overflow-y: auto;
    padding: 10px 10px 12px 10px;
    box-sizing: border-box;
  }
  .pt-ops-small {
    font-size: 11px;
    color: #555;
    line-height: 1.25;
    margin-bottom: 8px;
  }
  .pt-ops-section {
    border-top: 1px solid rgba(0,0,0,0.14);
    margin-top: 8px;
    padding-top: 7px;
  }
  .pt-ops-section:first-of-type {
    border-top: none;
    margin-top: 0;
    padding-top: 0;
  }
  .pt-ops-section h3 {
    font-size: 13px;
    margin: 0 0 5px 0;
  }
  .section-note,
  .subgroup-note,
  .row-note {
    color: var(--muted);
    font-size: 10.5px;
    line-height: 1.25;
  }
  .section-note {
    margin: -1px 0 5px 0;
  }
  .pt-ops-subgroup {
    margin: 6px 0 7px 12px;
  }
  .pt-ops-subgroup-title {
    color: #36524b;
    font-weight: 700;
    font-size: 11px;
    text-transform: uppercase;
    letter-spacing: 0.02em;
    margin: 4px 0 2px 0;
  }
  .pt-ops-layer-row {
    display: flex;
    align-items: baseline;
    flex-wrap: wrap;
    gap: 3px;
    margin: 2px 0;
  }
  .pt-ops-layer-label {
    display: inline-flex;
    align-items: baseline;
    margin: 0;
    cursor: pointer;
    min-width: 0;
    font-size: 12px;
  }
  .pt-ops-layer-label input {
    margin: 0 5px 0 0;
  }
  .pt-ops-row-links {
    display: inline-flex;
    align-items: baseline;
    gap: 3px;
    margin-left: 4px;
    font-size: 10.5px;
    white-space: nowrap;
  }
  .mock-link {
    color: var(--link);
    text-decoration: none;
    border-bottom: 1px dotted rgba(47,95,143,0.55);
  }
  .mock-link:hover {
    color: #123f68;
    border-bottom-color: rgba(18,63,104,0.9);
  }
  .mock-refresh {
    font-weight: 700;
  }
  .sep {
    color: #999;
    padding: 0 1px;
  }
  .row-note {
    flex-basis: 100%;
    margin-left: 20px;
  }
  .pt-ops-external-link-block {
    margin: 4px 0 6px 20px;
    padding: 4px 6px;
    border-left: 3px solid #cbd4d2;
    background: rgba(0,0,0,0.025);
    font-size: 11px;
  }
  .pt-ops-external-link-title {
    font-weight: 700;
    color: #333;
    margin-bottom: 1px;
  }
  .pt-ops-external-link-note {
    color: #666;
    font-size: 10.5px;
    margin-bottom: 2px;
  }
  .pt-ops-external-link-row {
    display: inline-flex;
    align-items: baseline;
    flex-wrap: wrap;
    gap: 4px;
  }
  .notes-card {
    margin-top: 10px;
    padding: 8px;
    background: rgba(255,255,255,0.55);
    border: 1px solid rgba(0,0,0,0.12);
    border-radius: 4px;
    font-size: 11px;
    line-height: 1.32;
  }
  .notes-card h2 {
    margin: 0 0 5px 0;
    font-size: 12px;
  }
  code {
    background: rgba(255,255,255,0.75);
    padding: 0 2px;
    border-radius: 2px;
  }
  @media (max-width: 900px) {
    .left-note { display: none; }
    .pt-ops-live-wrap { left: 10px; right: 10px; width: auto; }
  }
</style>
</head>
<body>
<div class="mock-map">
  <div class="left-note">
    <h1>RO_001 Ops Live panel UI sandbox</h1>
    <p>This standalone mockup is for visual approval only. It does not load map data or touch production helpers.</p>
    <ul>
      <li>Proposed compact row links: <b>rfrsh · lgnd · srce</b></li>
      <li>Cameras are under <b>Observations → Visual / cameras</b>.</li>
      <li>Reservoirs sit immediately after Observations and before Forecasts / Outlooks.</li>
      <li>Fire keeps fire perimeters, with cameras out of Fire.</li>
      <li>Drought is separated from CPC outlooks to avoid climate/outlook confusion.</li>
    </ul>
  </div>

  <div class="pt-ops-live-wrap">
    <div class="pt-ops-live-header">
      <span>Ops Live Layers <span id="active-count" class="pt-ops-live-active-count">(none active)</span> ▾</span>
      <button type="button" id="clear-btn" class="pt-ops-ribbon-clear">Clear ops</button>
    </div>
    <div class="pt-ops-live-body">
      <div class="pt-ops-small">RO_001 sandbox only. Suggested subgroup labels and row-link vocabulary for the next production patches.</div>
      ', panel_html, '
', satellite_calls, '
    </div>
  </div>
</div>
<script>
(function() {
  var countEl = document.getElementById("active-count");
  var checks = Array.prototype.slice.call(document.querySelectorAll(".pt-ops-live-body input[type=checkbox]"));
  function updateCount() {
    var n = checks.filter(function(chk) { return chk.checked; }).length;
    countEl.textContent = n ? "(" + n + " active)" : "(none active)";
  }
  checks.forEach(function(chk) { chk.addEventListener("change", updateCount); });
  document.getElementById("clear-btn").addEventListener("click", function() {
    checks.forEach(function(chk) { chk.checked = false; });
    updateCount();
  });
  updateCount();
})();
</script>
</body>
</html>
')

writeLines(html, out_file, useBytes = TRUE)

message("RO_001 Ops Live panel UI sandbox written:")
message("  ", normalizePath(out_file, winslash = "/", mustWork = TRUE))
message("")
message("This is a visual sandbox only. It does not build BRIM, fetch live data, or patch production helpers.")

if (isTRUE(open_in_browser)) {
  browseURL(normalizePath(out_file, winslash = "/", mustWork = TRUE))
}
