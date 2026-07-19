# install_delta_ops_view_percent_patch.R
# Surgical patch for Delta Ops Ops Live layer:
# 1. Reframe default Delta Ops view to the QA screenshot view.
# 2. Ensure "% Inflow Diverted" status line displays a percent sign, e.g. 6.6% (3-day avg).
#
# Run from PortaTreasure2 root.

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
target <- file.path(root, "03_functions", "leaflet_ops_live_delta_ops_helpers.r")

if (!file.exists(target)) {
  stop("Could not find target helper: ", target)
}

txt <- readLines(target, warn = FALSE)

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup <- paste0(target, ".bak_delta_ops_view_percent_", stamp)
file.copy(target, backup, overwrite = FALSE)

# ---------------------------------------------------------------------------
# 1. Default view.
# Matched to user's QA screenshot:
# bottom display showed about Lat/Lon 38.11002, -121.47775 and Zoom 10.
# Note: Leaflet JS setView order is [lat, lon], zoom.
# ---------------------------------------------------------------------------
new_view <- "setView([38.11002, -121.47775], 10)"

# Replace the two Delta Ops default view calls:
#   map.setView([...], ...)
#   this._map.setView([...], ...)
txt <- gsub(
  "map\\.setView\\(\\[[0-9.-]+,\\s*[0-9.-]+\\],\\s*[0-9.]+\\)",
  paste0("map.", new_view),
  txt
)
txt <- gsub(
  "this\\._map\\.setView\\(\\[[0-9.-]+,\\s*[0-9.-]+\\],\\s*[0-9.]+\\)",
  paste0("this._map.", new_view),
  txt
)

# ---------------------------------------------------------------------------
# 2. Percent-label helper.
# Preserve parenthetical text such as "(3-day avg)" while inserting % after
# the numeric value when the feed/old cached GeoJSON has "6.6 (3-day avg)".
# ---------------------------------------------------------------------------
helper_fn <- c(
  "",
  "  function ptDeltaOpsEnsurePercentText(x) {",
  "    var s = String(x == null ? '' : x).trim();",
  "    if (!s || s.indexOf('%') >= 0 || /^NA$/i.test(s)) return s;",
  "    var suffix = '';",
  "    var suffixMatch = s.match(/\\s*(\\([^)]*\\))\\s*$/);",
  "    if (suffixMatch) {",
  "      suffix = ' ' + suffixMatch[1];",
  "      s = s.replace(/\\s*\\([^)]*\\)\\s*$/, '').trim();",
  "    }",
  "    var numMatch = s.match(/-?[0-9]+(?:,[0-9]{3})*(?:\\.[0-9]+)?/);",
  "    if (!numMatch) return String(x == null ? '' : x);",
  "    return numMatch[0] + '%' + suffix;",
  "  }",
  ""
)

if (!any(grepl("function ptDeltaOpsEnsurePercentText", txt, fixed = TRUE))) {
  anchor <- grep("^  function ptDeltaOpsShortDate\\(", txt)
  if (length(anchor) != 1) {
    stop("Could not find insertion point before ptDeltaOpsShortDate(). No changes written.")
  }
  txt <- append(txt, helper_fn, after = anchor[1] - 1)
}

if (!any(grepl("diverted = ptDeltaOpsEnsurePercentText\\(diverted\\)", txt))) {
  idx <- grep("var diverted = featureByKey\\.percent_inflow_diverted", txt)
  if (length(idx) != 1) {
    stop("Could not find diverted status-line code. No changes written.")
  }
  txt <- append(txt, "    diverted = ptDeltaOpsEnsurePercentText(diverted);", after = idx[1])
}

writeLines(txt, target)

message("Patched: ", target)
message("Backup:  ", backup)
message("")
message("Default Delta Ops view is now: map.setView([38.11002, -121.47775], 10)")
message("Diverted status line now forces percent text when missing.")
