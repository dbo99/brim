# ==== 43_build_external_catalog_organization_tree.R ==========================
#
# PURPOSE:
#   Build a lightweight, reviewable External Layers organization tree from:
#     1. 00_config/external_service_catalog.csv
#     2. optional random-adds audit output from EL_001
#
# WHY:
#   BRIM's External Layers panel is CSV-driven.  As candidate layers grow, the
#   panel needs higher-level grouping that can be reviewed and adjusted without
#   editing JavaScript.  This O&M script writes draft CSV/HTML/Markdown views so
#   grouping decisions can be made before bulk layer adds.
#
# OUTPUTS:
#   08_docs/external_catalog_organization/external_catalog_tree_draft_latest.csv
#   08_docs/external_catalog_organization/external_catalog_tree_draft_latest.html
#   08_docs/external_catalog_organization/external_catalog_tree_draft_latest.md
#
# NOTES:
#   This script does not modify the BRIM map.  The live External panel reads the
#   external_group / external_subgroup columns directly from the catalog if they
#   are present.
#
# IMPORTANT PATH NOTE:
#   08_docs outputs are review/QA products only.  They are not required by the
#   production map build.  The build-relevant organization fields live in
#   00_config/external_service_catalog.csv.

# ---- Root detection ---------------------------------------------------------

find_project_root <- function(start = getwd()) {
  p <- normalizePath(start, winslash = "/", mustWork = TRUE)

  for (i in seq_len(8)) {
    if (file.exists(file.path(p, "00_config", "external_service_catalog.csv"))) {
      return(p)
    }
    parent <- dirname(p)
    if (identical(parent, p)) break
    p <- parent
  }

  stop("Could not find BRIM project root from: ", start)
}

project_root <- find_project_root()
message("BRIM project root: ", project_root)

catalog_path <- file.path(project_root, "00_config", "external_service_catalog.csv")
org_dir <- file.path(project_root, "08_docs", "external_catalog_organization")
dir.create(org_dir, recursive = TRUE, showWarnings = FALSE)

audit_ranked_latest <- file.path(
  project_root,
  "08_docs", "random_external_layer_audit", "output",
  "random_external_layer_ranked_plan_latest.csv"
)

# ---- Helpers ----------------------------------------------------------------

read_csv_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

num_or_default <- function(x, default = 999) {
  y <- suppressWarnings(as.numeric(x))
  y[is.na(y)] <- default
  y
}

assign_candidate_group <- function(candidate_id, title = "", theme = "", provider = "") {
  text <- tolower(paste(candidate_id, title, theme, provider, sep = " | "))

  if (grepl("space|aurora|swpc", text)) {
    return("Explore / experimental")
  }
  if (grepl("soil|moisture|smap|grace|nldas|lis|casma|nwm", text)) {
    return("Water / groundwater / water quality")
  }
  if (grepl("insar|subsidence|mine|mining|aml|calgem|oil/gas|oil and gas|geothermal|renewable|transmission|substation|drecp", text)) {
    return("Mining / energy / infrastructure")
  }
  if (grepl("highway|railroad|rail", text)) {
    return("Transportation / access")
  }
  if (grepl("blm.*recreation|recreation|ohv", text)) {
    return("BLM / land status / monitoring")
  }
  if (grepl("spc|fire weather|convective|mesoscale", text)) {
    return("Fire / hazards / emergency")
  }
  if (grepl("wpc|wssi|snow|drought|cpc|hazard|ndfd|wind|ndvi|vegetation|vhi|quickdri|vegdri|nohrsc", text)) {
    return("Weather / snow / drought")
  }
  if (grepl("gde|groundwater|water|impaired", text)) {
    return("Water / groundwater / water quality")
  }

  "Explore / experimental"
}

candidate_subgroup <- function(candidate_id, title = "", theme = "", provider = "") {
  text <- tolower(paste(candidate_id, title, theme, provider, sep = " | "))

  if (grepl("insar|subsidence", text)) return("Subsidence / infrastructure risk")
  if (grepl("soil|smap|grace|nldas|lis|casma|moisture|nwm", text)) return("Soil moisture")
  if (grepl("aml|mine|mining|smara", text)) return("Mining / abandoned mines")
  if (grepl("calgem|oil/gas|oil and gas|geothermal", text)) return("Oil / gas / geothermal")
  if (grepl("renewable|drecp|transmission|substation", text)) return("Energy / transmission / planning")
  if (grepl("highway", text)) return("Highways")
  if (grepl("rail", text)) return("Railroads")
  if (grepl("recreation|ohv", text)) return("BLM recreation / access")
  if (grepl("spc.*fire|fire weather", text)) return("Fire weather outlooks")
  if (grepl("mesoscale", text)) return("Mesoscale discussions")
  if (grepl("convective", text)) return("Convective outlooks")
  if (grepl("wssi|winter storm", text)) return("Winter weather")
  if (grepl("snow|nohrsc", text)) return("Snow analysis")
  if (grepl("cpc.*hazard|week-2|week 2|rapid-onset", text)) return("Hazards / week-2 outlooks")
  if (grepl("wind|ndfd", text)) return("Wind")
  if (grepl("drought|quickdri|vegdri|vhi|ndvi|vegetation", text)) return("Drought / vegetation")
  if (grepl("gde|groundwater-dependent", text)) return("GDE / groundwater ecology")
  if (grepl("impaired|303|305|tmdl", text)) return("Water quality / impaired waters")
  if (grepl("soil|smap|grace|nldas|lis|casma|moisture", text)) return("Soil moisture")
  if (grepl("space|aurora", text)) return("Space weather links")

  if (!is.na(theme) && nzchar(theme)) return(theme)
  "Explore / experimental"
}

group_order_lookup <- c(
  "Water / groundwater / water quality" = 10,
  "Water operations / flood / coastal" = 20,
  "Weather / snow / drought" = 30,
  "Fire / hazards / emergency" = 40,
  "Ecology / habitat / species" = 50,
  "BLM / land status / monitoring" = 60,
  "Mining / energy / infrastructure" = 70,
  "Transportation / access" = 80,
  "Administrative / boundaries / planning" = 90,
  "Explore / experimental" = 99
)

# ---- Existing catalog rows --------------------------------------------------

catalog <- read_csv_safe(catalog_path)
if (is.null(catalog)) stop("Catalog not found: ", catalog_path)

for (nm in c("external_group", "external_group_order", "external_subgroup", "external_subgroup_order", "external_layer_id", "priority")) {
  if (!nm %in% names(catalog)) catalog[[nm]] <- ""
}

existing <- data.frame(
  row_type = "current_catalog",
  candidate_id = "",
  external_layer_id = catalog$external_layer_id,
  external_display_num = "",
  display_name = if ("display_name" %in% names(catalog)) catalog$display_name else "",
  agency_or_provider = if ("agency" %in% names(catalog)) catalog$agency else "",
  theme = if ("theme" %in% names(catalog)) catalog$theme else "",
  external_group = catalog$external_group,
  external_group_order = catalog$external_group_order,
  external_subgroup = catalog$external_subgroup,
  external_subgroup_order = catalog$external_subgroup_order,
  primary_panel = if ("primary_panel" %in% names(catalog)) catalog$primary_panel else "",
  priority = catalog$priority,
  recommended_patch_bucket = "already in catalog",
  recommended_ui_tier = "",
  service_type = if ("service_type" %in% names(catalog)) catalog$service_type else "",
  source_page = if ("source_page" %in% names(catalog)) catalog$source_page else "",
  notes = if ("notes" %in% names(catalog)) catalog$notes else "",
  stringsAsFactors = FALSE
)

# ---- Candidate audit rows ---------------------------------------------------

candidates <- data.frame()
ranked <- read_csv_safe(audit_ranked_latest)

if (!is.null(ranked) && nrow(ranked) > 0) {
  for (nm in c(
    "candidate_id", "title", "provider", "theme", "recommended_patch_bucket",
    "recommended_ui_tier", "service_type", "source_link_plan", "recommended_panel"
  )) {
    if (!nm %in% names(ranked)) ranked[[nm]] <- ""
  }

  cand_group <- mapply(assign_candidate_group, ranked$candidate_id, ranked$title, ranked$theme, ranked$provider)
  cand_sub <- mapply(candidate_subgroup, ranked$candidate_id, ranked$title, ranked$theme, ranked$provider)
  cand_order <- unname(group_order_lookup[cand_group])
  cand_order[is.na(cand_order)] <- 99

  candidates <- data.frame(
    row_type = "candidate_from_random_adds_audit",
    candidate_id = ranked$candidate_id,
    external_layer_id = "",
    external_display_num = "",
    display_name = ranked$title,
    agency_or_provider = ranked$provider,
    theme = ranked$theme,
    external_group = cand_group,
    external_group_order = cand_order,
    external_subgroup = cand_sub,
    external_subgroup_order = 900,
    primary_panel = ranked$recommended_panel,
    priority = "",
    recommended_patch_bucket = ranked$recommended_patch_bucket,
    recommended_ui_tier = ranked$recommended_ui_tier,
    service_type = ranked$service_type,
    source_page = ranked$source_link_plan,
    notes = paste("Audit candidate;", ranked$recommended_patch_bucket),
    stringsAsFactors = FALSE
  )
} else {
  message("No EL_001 ranked-plan CSV found. Tree will include current catalog only: ", audit_ranked_latest)
}

# ---- Combine, sort, write CSV ----------------------------------------------

combined <- rbind(existing, candidates)
combined$external_group_order_num <- num_or_default(combined$external_group_order, 999)
combined$external_subgroup_order_num <- num_or_default(combined$external_subgroup_order, 999)
combined$priority_num <- num_or_default(combined$priority, 9999)
combined <- combined[order(
  combined$external_group_order_num,
  combined$external_group,
  combined$external_subgroup_order_num,
  combined$external_subgroup,
  combined$row_type,
  combined$priority_num,
  combined$display_name
), ]

# Mirror the map build's user-facing External panel numbering for current
# catalog rows. Disabled and Ops-only rows stay in the tree/report but do not
# receive a visible External panel number, matching the production map. Candidate
# rows remain blank because they are not in the map yet.
primary_panel_norm <- tolower(trimws(as.character(combined$primary_panel)))
primary_panel_norm <- gsub("[_\\s-]+", "_", primary_panel_norm)
cur <- combined$row_type == "current_catalog" & !(primary_panel_norm %in% c("ops_live", "ops", "disabled"))
combined$external_display_num[cur] <- as.character(seq_len(sum(cur)))

combined$external_group_order_num <- NULL
combined$external_subgroup_order_num <- NULL
combined$priority_num <- NULL

csv_out <- file.path(org_dir, "external_catalog_tree_draft_latest.csv")
utils::write.csv(combined, csv_out, row.names = FALSE, na = "")
message("Wrote: ", csv_out)

# ---- Markdown tree ----------------------------------------------------------

# Keep this report writer intentionally simple and robust.  Earlier versions
# used an explicit file connection, which can be fragile when the script is
# sourced inside a wrapper/local() block or when OneDrive/RStudio briefly holds
# a file handle.  Here we build all lines in memory and let writeLines() open
# and close the file path directly.  These Markdown/HTML files are QA products
# only; they are not inputs to the BRIM production build.
md_out <- file.path(org_dir, "external_catalog_tree_draft_latest.md")

md_lines <- c(
  "# BRIM External Layers organization draft",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "This draft combines current External catalog rows with random-adds audit candidates when the EL_001 ranked-plan CSV is available.",
  "Edit `external_group`, `external_group_order`, `external_subgroup`, and `external_subgroup_order` in `00_config/external_service_catalog.csv` to reorganize the live External Layers panel.",
  "Visible `external_display_num` values mirror the External panel order for current visible rows. Use `external_layer_id` for stable QA/dev references; run `02_preprocess/44_assign_external_catalog_ids.R` after adding rows with blank IDs.",
  ""
)

for (grp in unique(combined$external_group)) {
  grp_rows <- combined[combined$external_group == grp, , drop = FALSE]
  md_lines <- c(md_lines, paste0("## ", grp, " (", nrow(grp_rows), ")"))
  for (sub in unique(grp_rows$external_subgroup)) {
    sub_rows <- grp_rows[grp_rows$external_subgroup == sub, , drop = FALSE]
    md_lines <- c(md_lines, "", paste0("### ", sub, " (", nrow(sub_rows), ")"))
    for (i in seq_len(nrow(sub_rows))) {
      prefix <- if (sub_rows$row_type[i] == "current_catalog") "current" else "candidate"
      display_num <- if (nzchar(sub_rows$external_display_num[i])) paste0(" #", sub_rows$external_display_num[i]) else ""
      ext_id <- if (nzchar(sub_rows$external_layer_id[i])) paste0(" `", sub_rows$external_layer_id[i], "`") else ""
      cid <- if (nzchar(sub_rows$candidate_id[i])) paste0(" — `", sub_rows$candidate_id[i], "`") else ""
      md_lines <- c(md_lines, paste0("- [", prefix, "]", display_num, ext_id, " ", sub_rows$display_name[i], cid))
    }
    md_lines <- c(md_lines, "")
  }
}

writeLines(md_lines, md_out, useBytes = TRUE)
message("Wrote: ", md_out)

# ---- HTML tree --------------------------------------------------------------

html_out <- file.path(org_dir, "external_catalog_tree_draft_latest.html")
rows_html <- character()
for (grp in unique(combined$external_group)) {
  grp_rows <- combined[combined$external_group == grp, , drop = FALSE]
  rows_html <- c(rows_html, paste0('<h2>', html_escape(grp), ' <span class="count">(', nrow(grp_rows), ')</span></h2>'))
  for (sub in unique(grp_rows$external_subgroup)) {
    sub_rows <- grp_rows[grp_rows$external_subgroup == sub, , drop = FALSE]
    rows_html <- c(rows_html, paste0('<h3>', html_escape(sub), ' <span class="count">(', nrow(sub_rows), ')</span></h3>'))
    li <- paste0(
      '<li class="', html_escape(sub_rows$row_type), '"><span class="tag">',
      ifelse(sub_rows$row_type == "current_catalog", "current", "candidate"),
      '</span> ',
      ifelse(nzchar(sub_rows$external_display_num), paste0('<code>#', html_escape(sub_rows$external_display_num), '</code> '), ''),
      ifelse(nzchar(sub_rows$external_layer_id), paste0('<code>', html_escape(sub_rows$external_layer_id), '</code> '), ''),
      html_escape(sub_rows$display_name),
      ifelse(nzchar(sub_rows$candidate_id), paste0(' <code>', html_escape(sub_rows$candidate_id), '</code>'), ''),
      '</li>'
    )
    rows_html <- c(rows_html, '<ul>', li, '</ul>')
  }
}

html <- paste0(
  '<!doctype html><html><head><meta charset="utf-8"><title>BRIM External catalog organization draft</title>',
  '<style>',
  'body{font-family:Arial,Helvetica,sans-serif;margin:24px;color:#222;line-height:1.35;}',
  'h1{font-size:22px;} h2{font-size:18px;margin-top:24px;border-top:1px solid #ddd;padding-top:12px;}',
  'h3{font-size:14px;margin:12px 0 5px 0;color:#444;}',
  'ul{margin-top:4px;} li{margin:3px 0;} .tag{display:inline-block;min-width:64px;font-size:10px;border-radius:8px;padding:1px 6px;background:#e7eef2;color:#234;}',
  '.candidate_from_random_adds_audit .tag{background:#f0ead7;color:#5a4200;} .count{color:#777;font-weight:400;} code{font-size:11px;}',
  '.note{background:#f7f7f7;border-left:4px solid #777;padding:10px 12px;margin:12px 0;}',
  '</style></head><body>',
  '<h1>BRIM External catalog organization draft</h1>',
  '<div class="note">Generated: ', html_escape(format(Sys.time(), "%Y-%m-%d %H:%M:%S")), '<br>',
  'Current catalog rows plus EL_001 random-adds candidates when available. Subsidence candidates are grouped under Mining / energy / infrastructure.</div>',
  paste(rows_html, collapse = '\n'),
  '</body></html>'
)
writeLines(html, html_out, useBytes = TRUE)
message("Wrote: ", html_out)

message("Done. This was an organization/reporting script only; no map build was run.")
