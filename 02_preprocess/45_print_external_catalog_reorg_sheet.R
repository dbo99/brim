# 45_print_external_catalog_reorg_sheet.R
# -----------------------------------------------------------------------------
# Purpose
#   Create a printable External Layers group/theme planning worksheet for BRIM.
#
#   This script is intended for paper-and-pencil reorganization of the External
#   Layers catalog.  It prints the current catalog fully expanded, then leaves
#   large blank columns where Dave can write a proposed NEW parent group and
#   proposed NEW theme for each layer.
#
# Inputs
#   Required map catalog input:
#     00_config/external_service_catalog.csv
#
#   Optional QA/audit inputs:
#     08_docs/random_external_layer_audit/output/random_external_layer_*.csv
#     08_docs/random_external_layer_audit/input/BRIM_random_adds_master_inventory.csv
#
# Outputs
#   06_output/reports/external_catalog_reorg/
#     - external_catalog_group_theme_planning_latest.pdf
#     - external_catalog_group_theme_planning_<timestamp>.pdf
#     - external_catalog_group_theme_working_sheet_latest.csv
#     - external_catalog_group_theme_working_sheet_<timestamp>.csv
#     - external_catalog_candidate_planning_latest.csv, when audit rows exist
#
# Important
#   This is a QA/reorganization helper only.
#   It does NOT feed the BRIM map build.
#   It does NOT modify 00_config/external_service_catalog.csv.
#
# Why this version is simpler than earlier drafts
#   Earlier planning sheets printed current parent + child/subgroup + theme.
#   That was too crowded and implied too many hierarchy levels.  This version
#   treats the desired organizing model as one parent group plus one theme per
#   layer.  The current external_subgroup is retained only in the digital CSV as
#   a reference; it is not a main handwritten planning column.
# -----------------------------------------------------------------------------

## ---- Locate project root ----------------------------------------------------

find_brim_project_root <- function() {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  candidates <- unique(c(
    wd,
    dirname(wd),
    normalizePath(file.path(wd, ".."), winslash = "/", mustWork = FALSE)
  ))
  for (p in candidates) {
    if (file.exists(file.path(p, "00_config", "external_service_catalog.csv"))) {
      return(p)
    }
  }
  stop(
    "Could not find BRIM project root. Set working directory to PortaTreasure2 ",
    "or a direct child folder before sourcing this script."
  )
}

project_root <- find_brim_project_root()
message("BRIM project root: ", project_root)

catalog_path <- file.path(project_root, "00_config", "external_service_catalog.csv")
out_dir <- file.path(project_root, "06_output", "reports", "external_catalog_reorg")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

## ---- Small helpers ----------------------------------------------------------

as_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

as_num_order <- function(x, fallback = 999) {
  out <- suppressWarnings(as.numeric(as_chr(x)))
  out[is.na(out)] <- fallback
  out
}

first_nonblank <- function(...) {
  vals <- list(...)
  for (v in vals) {
    v <- as_chr(v)
    if (length(v) > 0 && nzchar(v[1])) return(v[1])
  }
  ""
}

clip_text <- function(x, max_chars) {
  x <- gsub("\\s+", " ", as_chr(x))
  ifelse(nchar(x) > max_chars, paste0(substr(x, 1, max_chars - 1), "..."), x)
}

blank_if_other <- function(x) {
  x <- as_chr(x)
  ifelse(tolower(x) %in% c("other", "other / uncategorized"), "", x)
}

read_first_existing_csv <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (length(paths) == 0) return(NULL)
  message("Reading optional candidate/audit table: ", paths[1])
  utils::read.csv(paths[1], stringsAsFactors = FALSE, check.names = FALSE)
}

first_col <- function(df, nms) {
  if (is.null(df) || nrow(df) == 0) return(character(0))
  out <- rep("", nrow(df))
  for (nm in nms) {
    if (nm %in% names(df)) {
      vals <- as_chr(df[[nm]])
      idx <- !nzchar(out) & nzchar(vals)
      out[idx] <- vals[idx]
    }
  }
  out
}

draft_parent_for_candidate <- function(candidate_id, title = "") {
  key <- toupper(paste(candidate_id, title))
  out <- rep("Explore / experimental", length(key))

  out[grepl("SPC_", key)] <- "SPC outlooks / discussions"
  out[grepl("WPC|CPC|NOHRSC|NDFD|QUICKDRI|VEGDRI|VHI|NDVI|GIBS|SPORT|NLDAS|CASMA|SMAP|GRACE|WIND", key)] <- "Weather / snow / drought"
  out[grepl("GDE|IMPAIRED|TMDL|NWM_SOIL", key)] <- "Water / groundwater / water quality"
  out[grepl("AML|MINE|MINES|CALGEM|INSAR|SUBSIDENCE|TRANSMISSION|SUBSTATION|RENEWABLE", key)] <- "Mining / energy / infrastructure"
  out[grepl("HIGHWAY|RAIL", key)] <- "Transportation / access"
  out[grepl("DRECP|BLM_RECREATION", key)] <- "BLM / recreation / planning"
  out[grepl("FIRE|FRAP", key) & !grepl("SPC_", key)] <- "Fire / hazards"
  out[grepl("SWPC|AURORA|SPACE", key)] <- "Explore / experimental"

  out
}

draft_theme_for_candidate <- function(candidate_id, title = "") {
  key <- toupper(paste(candidate_id, title))
  out <- rep("Needs review", length(key))

  out[grepl("GDE", key)] <- "GDE / NCCAG"
  out[grepl("IMPAIRED|TMDL", key)] <- "Water quality / impaired waters"
  out[grepl("SOIL|NWM|SPORT|NLDAS|CASMA|SMAP|GRACE", key)] <- "Soil moisture / drought"
  out[grepl("NOHRSC|SNOW|WSSI", key)] <- "Snow / winter weather"
  out[grepl("SPC_FIREWX", key)] <- "Fire weather outlooks"
  out[grepl("SPC_CONVECTIVE", key)] <- "Convective outlooks"
  out[grepl("SPC_MESO", key)] <- "Mesoscale discussions"
  out[grepl("CPC", key)] <- "Climate outlooks / hazards"
  out[grepl("QUICKDRI|VEGDRI|VHI|NDVI|GIBS", key)] <- "Vegetation / remote sensing"
  out[grepl("AML|MINE", key)] <- "Mines / AML"
  out[grepl("CALGEM", key)] <- "Oil / gas / geothermal"
  out[grepl("INSAR|SUBSIDENCE", key)] <- "Subsidence / infrastructure risk"
  out[grepl("TRANSMISSION|SUBSTATION|RENEWABLE", key)] <- "Energy infrastructure"
  out[grepl("HIGHWAY", key)] <- "Roads / highways"
  out[grepl("RAIL", key)] <- "Railroads"
  out[grepl("DRECP", key)] <- "Planning / conservation"
  out[grepl("BLM_RECREATION", key)] <- "Recreation / access"
  out[grepl("SWPC|AURORA|SPACE", key)] <- "Space weather"

  out
}

draft_group_order <- function(parent) {
  ord <- c(
    "Water / groundwater / water quality" = 10,
    "Weather / snow / drought" = 20,
    "SPC outlooks / discussions" = 25,
    "Fire / hazards" = 30,
    "Mining / energy / infrastructure" = 40,
    "Transportation / access" = 50,
    "BLM / recreation / planning" = 60,
    "Explore / experimental" = 90
  )
  out <- unname(ord[parent])
  out[is.na(out)] <- 999
  out
}

## ---- Read and sort catalog --------------------------------------------------

catalog <- utils::read.csv(
  catalog_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

required_cols <- c(
  "agency", "program", "theme", "external_group", "external_group_order",
  "external_subgroup", "external_subgroup_order", "external_layer_id",
  "display_name", "primary_panel", "service_type", "service_url",
  "default_load_mode", "priority", "min_zoom_current_view", "min_zoom_live",
  "large_layer_warning"
)
for (nm in required_cols) {
  if (!nm %in% names(catalog)) catalog[[nm]] <- ""
  catalog[[nm]] <- as_chr(catalog[[nm]])
}

# Match External-panel population: External and both-panel rows only.
primary <- tolower(catalog$primary_panel)
external_rows <- primary %in% c("", "external", "both")
external_rows <- external_rows & nzchar(catalog$display_name) & nzchar(catalog$service_url)
cat_ext <- catalog[external_rows, , drop = FALSE]

cat_ext$external_group[!nzchar(cat_ext$external_group)] <- "Other / uncategorized"
cat_ext$external_subgroup[!nzchar(cat_ext$external_subgroup)] <- "Other"
cat_ext$theme[!nzchar(cat_ext$theme)] <- cat_ext$external_subgroup[!nzchar(cat_ext$theme)]
cat_ext$theme[!nzchar(cat_ext$theme)] <- "Other"

# Desired planning model: one group and one theme per layer.  The subgroup is
# still kept in the working CSV because the current panel may use it, but it is
# not a main planning level in the printed PDF.
cat_ext$planning_group <- cat_ext$external_group
cat_ext$planning_theme <- cat_ext$theme

cat_ext$external_group_order_num <- as_num_order(cat_ext$external_group_order)
cat_ext$external_subgroup_order_num <- as_num_order(cat_ext$external_subgroup_order)
cat_ext$priority_num <- as_num_order(cat_ext$priority)

cat_ext <- cat_ext[order(
  cat_ext$external_group_order_num,
  cat_ext$planning_group,
  cat_ext$external_subgroup_order_num,
  cat_ext$planning_theme,
  cat_ext$priority_num,
  cat_ext$agency,
  cat_ext$display_name
), , drop = FALSE]

cat_ext$external_display_num <- seq_len(nrow(cat_ext))
cat_ext$display_badge <- paste0("#", cat_ext$external_display_num)

## ---- Optional random-adds candidate inventory -------------------------------

# Optional and advisory only.  Reads EL_001 audit outputs if present so the PDF
# can include not-yet-added/potential layers at the bottom.
audit_paths <- c(
  file.path(project_root, "08_docs", "random_external_layer_audit", "output", "random_external_layer_ranked_plan_latest.csv"),
  file.path(project_root, "08_docs", "random_external_layer_audit", "output", "random_external_layer_audit_latest.csv"),
  file.path(project_root, "08_docs", "random_external_layer_audit", "input", "BRIM_random_adds_master_inventory.csv")
)

cand_raw <- read_first_existing_csv(audit_paths)
cand_plan <- data.frame()
if (!is.null(cand_raw) && nrow(cand_raw) > 0) {
  cand_id <- first_col(cand_raw, c("candidate_id", "id"))
  cand_title <- first_col(cand_raw, c("resolved_title", "title", "candidate_title", "name"))
  cand_provider <- first_col(cand_raw, c("resolved_provider", "provider", "agency"))
  cand_service <- first_col(cand_raw, c("service_type", "resolved_service_type"))
  cand_bucket <- first_col(cand_raw, c("recommended_patch_bucket", "patch_bucket", "batch"))
  cand_path <- first_col(cand_raw, c("recommended_implementation_path", "implementation_path"))
  cand_panel <- first_col(cand_raw, c("recommended_panel", "panel"))
  cand_tier <- first_col(cand_raw, c("recommended_ui_tier", "ui_tier"))
  cand_reason <- first_col(cand_raw, c("reject_or_defer_reason", "first_patch_notes", "summary_take", "notes"))

  fully_added_ids <- c(
    "AML_CA_FEATURES", "HIGHWAYS_NHS", "BLM_RECREATION", "NOHRSC_SNOW",
    "SPC_MESO_DISC", "RAILROADS"
  )
  partial_added_ids <- c("SPC_FIREWX", "SPC_CONVECTIVE", "CALGEM_WELLS")
  existing_upgrade_ids <- c("IMPAIRED_WATERS", "NWM_SOIL_MOISTURE")

  include <- nzchar(cand_id) & !(cand_id %in% fully_added_ids)
  cand_status <- ifelse(
    cand_id %in% partial_added_ids, "partial in catalog; review remaining",
    ifelse(cand_id %in% existing_upgrade_ids, "existing row / upgrade candidate", "potential / not added")
  )

  cand_group <- draft_parent_for_candidate(cand_id, cand_title)
  cand_theme <- draft_theme_for_candidate(cand_id, cand_title)

  cand_plan <- data.frame(
    candidate_id = cand_id[include],
    candidate_title = cand_title[include],
    provider = cand_provider[include],
    service_type = cand_service[include],
    audit_bucket = cand_bucket[include],
    implementation_path = cand_path[include],
    recommended_panel = cand_panel[include],
    ui_tier = cand_tier[include],
    candidate_status = cand_status[include],
    suggested_group = cand_group[include],
    suggested_theme = cand_theme[include],
    audit_notes = cand_reason[include],
    proposed_group_order = "",
    proposed_group = "",
    proposed_theme_order = "",
    proposed_theme = "",
    proposed_action = "",
    paper_notes = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  cand_plan$suggested_group_order <- draft_group_order(cand_plan$suggested_group)
  cand_plan <- cand_plan[order(
    cand_plan$suggested_group_order,
    cand_plan$suggested_group,
    cand_plan$suggested_theme,
    cand_plan$candidate_id
  ), , drop = FALSE]

  message("Optional candidate planning rows: ", nrow(cand_plan))
} else {
  message("No random-adds audit table found; printable candidate section will be skipped.")
}

## ---- Write working CSVs -----------------------------------------------------

# This CSV is more editable than the PDF.  It is safe as a scratch sheet, but
# the map still reads only 00_config/external_service_catalog.csv.
working <- data.frame(
  external_display_num = cat_ext$external_display_num,
  display_badge = cat_ext$display_badge,
  external_layer_id = cat_ext$external_layer_id,
  current_group_order = cat_ext$external_group_order,
  current_group = cat_ext$planning_group,
  current_theme = cat_ext$planning_theme,
  current_panel_subgroup_reference = cat_ext$external_subgroup,
  display_name = cat_ext$display_name,
  agency = cat_ext$agency,
  program = cat_ext$program,
  service_type = cat_ext$service_type,
  default_load_mode = cat_ext$default_load_mode,
  min_zoom_current_view = cat_ext$min_zoom_current_view,
  min_zoom_live = cat_ext$min_zoom_live,
  large_layer_warning = cat_ext$large_layer_warning,
  proposed_group_order = "",
  proposed_group = "",
  proposed_theme_order = "",
  proposed_theme = "",
  action_keep_move_delete = "",
  paper_notes = "",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
csv_timestamped <- file.path(out_dir, paste0("external_catalog_group_theme_working_sheet_", ts, ".csv"))
csv_latest <- file.path(out_dir, "external_catalog_group_theme_working_sheet_latest.csv")
utils::write.csv(working, csv_timestamped, row.names = FALSE, na = "")
utils::write.csv(working, csv_latest, row.names = FALSE, na = "")
message("Wrote: ", csv_latest)

if (nrow(cand_plan) > 0) {
  cand_csv_timestamped <- file.path(out_dir, paste0("external_catalog_candidate_planning_", ts, ".csv"))
  cand_csv_latest <- file.path(out_dir, "external_catalog_candidate_planning_latest.csv")
  utils::write.csv(cand_plan, cand_csv_timestamped, row.names = FALSE, na = "")
  utils::write.csv(cand_plan, cand_csv_latest, row.names = FALSE, na = "")
  message("Wrote: ", cand_csv_latest)
}

## ---- Build printable row list ----------------------------------------------

items <- list()
add_item <- function(type, group = "", theme = "", row = NULL) {
  items[[length(items) + 1L]] <<- list(type = type, group = group, theme = theme, row = row)
}

last_group <- NULL
last_theme <- NULL
for (i in seq_len(nrow(cat_ext))) {
  g <- cat_ext$planning_group[i]
  th <- cat_ext$planning_theme[i]
  if (!identical(g, last_group)) {
    add_item("group", group = g)
    last_group <- g
    last_theme <- NULL
  }
  if (!identical(th, last_theme)) {
    add_item("theme", group = g, theme = th)
    last_theme <- th
  }
  add_item("layer", group = g, theme = th, row = cat_ext[i, , drop = FALSE])
}

if (nrow(cand_plan) > 0) {
  add_item("potential_section")
  last_cand_group <- NULL
  last_cand_theme <- NULL
  for (i in seq_len(nrow(cand_plan))) {
    g <- cand_plan$suggested_group[i]
    th <- cand_plan$suggested_theme[i]
    if (!identical(g, last_cand_group)) {
      add_item("candidate_group", group = g)
      last_cand_group <- g
      last_cand_theme <- NULL
    }
    if (!identical(th, last_cand_theme)) {
      add_item("candidate_theme", group = g, theme = th)
      last_cand_theme <- th
    }
    add_item("candidate", group = g, theme = th, row = cand_plan[i, , drop = FALSE])
  }
}

item_height <- function(item) {
  switch(item$type,
         group = 0.34,
         theme = 0.24,
         layer = 0.42,
         potential_section = 0.52,
         candidate_group = 0.30,
         candidate_theme = 0.22,
         candidate = 0.44,
         0.30)
}

page_h <- 17
page_w <- 11
margin_l <- 0.32
margin_r <- 0.28
margin_t <- 0.35
margin_b <- 0.35
header_h <- 0.86
usable_top <- page_h - margin_t - header_h
usable_bottom <- margin_b

pages <- list()
current <- list()
y_remaining <- usable_top - usable_bottom
for (item in items) {
  h <- item_height(item)
  if (length(current) > 0 && y_remaining < h) {
    pages[[length(pages) + 1L]] <- current
    current <- list()
    y_remaining <- usable_top - usable_bottom
  }
  current[[length(current) + 1L]] <- item
  y_remaining <- y_remaining - h
}
if (length(current) > 0) pages[[length(pages) + 1L]] <- current

## ---- Draw PDF ---------------------------------------------------------------

pdf_timestamped <- file.path(out_dir, paste0("external_catalog_group_theme_planning_", ts, ".pdf"))
pdf_latest <- file.path(out_dir, "external_catalog_group_theme_planning_latest.pdf")
legacy_pdf_latest <- file.path(out_dir, "external_catalog_parent_child_planning_latest.pdf")

grDevices::pdf(pdf_timestamped, width = page_w, height = page_h, paper = "special", onefile = TRUE)
pdf_open <- TRUE
on.exit({
  if (isTRUE(pdf_open)) try(grDevices::dev.off(), silent = TRUE)
}, add = TRUE)

library(grid)

draw_text <- function(label, x, y, size = 8, fontface = "plain", just = c("left", "center"), col = "#111111") {
  grid::grid.text(
    label,
    x = grid::unit(x, "in"),
    y = grid::unit(y, "in"),
    just = just,
    gp = grid::gpar(fontsize = size, fontface = fontface, col = col)
  )
}

draw_rect <- function(x, y, w, h, fill = NA, col = "#dddddd", lwd = 0.5) {
  grid::grid.rect(
    x = grid::unit(x, "in"),
    y = grid::unit(y, "in"),
    width = grid::unit(w, "in"),
    height = grid::unit(h, "in"),
    just = c("left", "bottom"),
    gp = grid::gpar(fill = fill, col = col, lwd = lwd)
  )
}

draw_line <- function(x0, x1, y, col = "#cfcfcf", lwd = 0.45) {
  grid::grid.lines(
    x = grid::unit(c(x0, x1), "in"),
    y = grid::unit(c(y, y), "in"),
    gp = grid::gpar(col = col, lwd = lwd)
  )
}

# Column geometry for 11 x 17 portrait.
x0 <- margin_l
x_num <- x0
x_layer <- x0 + 0.45
x_current <- x0 + 3.62
x_new_group <- x0 + 5.66
x_new_theme <- x0 + 7.34
x_notes <- x0 + 9.02
x_right <- page_w - margin_r

for (p in seq_along(pages)) {
  grid::grid.newpage()

  title <- "BRIM External Layers - group / theme planning worksheet"
  subtitle <- paste0(
    "Generated ", format(Sys.time(), "%Y-%m-%d %I:%M %p"),
    " | External rows: ", nrow(cat_ext),
    " | Page ", p, " of ", length(pages)
  )
  draw_text(title, margin_l, page_h - 0.28, size = 11, fontface = "bold", just = c("left", "top"))
  draw_text(subtitle, margin_l, page_h - 0.50, size = 7.2, col = "#444444", just = c("left", "top"))
  draw_text(
    "Planning model: one parent group plus one theme per layer. Candidate/future layers are appended at the bottom when audit output exists.",
    margin_l, page_h - 0.68, size = 6.8, col = "#555555", just = c("left", "top")
  )

  y <- page_h - margin_t - header_h
  draw_rect(x0, y - 0.24, x_right - x0, 0.24, fill = "#f0f0f0", col = "#cccccc")
  draw_text("#", x_num + 0.05, y - 0.12, size = 6.8, fontface = "bold")
  draw_text("Layer", x_layer, y - 0.12, size = 6.8, fontface = "bold")
  draw_text("Current group / theme", x_current, y - 0.12, size = 6.8, fontface = "bold")
  draw_text("NEW group", x_new_group, y - 0.12, size = 6.8, fontface = "bold")
  draw_text("NEW theme", x_new_theme, y - 0.12, size = 6.8, fontface = "bold")
  draw_text("Notes / action", x_notes, y - 0.12, size = 6.8, fontface = "bold")
  y <- y - 0.28

  for (item in pages[[p]]) {
    h <- item_height(item)

    if (item$type == "group") {
      draw_rect(x0, y - h + 0.035, x_right - x0, h - 0.055, fill = "#dbeaf3", col = "#b7cedc")
      draw_text(item$group, x0 + 0.08, y - h / 2 + 0.01, size = 8.2, fontface = "bold")

    } else if (item$type == "theme") {
      line_y <- y - h / 2 + 0.005
      draw_line(x0 + 0.1, x_right - 0.1, line_y, col = "#c8d3d9", lwd = 0.55)
      label <- paste0("  ", item$theme, "  ")
      draw_rect(x0 + 3.75, line_y - 0.07, 2.50, 0.14, fill = "white", col = NA)
      draw_text(label, x0 + 5.00, line_y, size = 6.9, fontface = "bold", just = c("center", "center"), col = "#485965")

    } else if (item$type == "layer") {
      r <- item$row
      row_y <- y - h

      draw_rect(x0, row_y, x_right - x0, h, fill = NA, col = "#e1e1e1", lwd = 0.35)
      draw_rect(x_new_group - 0.06, row_y + 0.04, 1.50, h - 0.08, fill = NA, col = "#bdbdbd", lwd = 0.45)
      draw_rect(x_new_theme - 0.06, row_y + 0.04, 1.50, h - 0.08, fill = NA, col = "#bdbdbd", lwd = 0.45)
      draw_rect(x_notes - 0.06, row_y + 0.04, x_right - x_notes, h - 0.08, fill = NA, col = "#bdbdbd", lwd = 0.45)

      badge <- paste0("#", r$external_display_num)
      stable <- if (nzchar(r$external_layer_id)) r$external_layer_id else ""
      draw_text(badge, x_num + 0.05, row_y + h / 2 + 0.05, size = 6.8, fontface = "bold")
      if (nzchar(stable)) draw_text(stable, x_num + 0.05, row_y + 0.08, size = 4.8, col = "#777777")

      draw_text(clip_text(r$display_name, 44), x_layer, row_y + h - 0.105, size = 6.7, fontface = "bold", just = c("left", "top"))
      meta_line <- paste0(
        clip_text(first_nonblank(r$agency, r$program), 32),
        " | ",
        clip_text(first_nonblank(r$service_type, r$default_load_mode), 12)
      )
      draw_text(meta_line, x_layer, row_y + 0.08, size = 5.5, col = "#555555", just = c("left", "bottom"))

      current_line_1 <- paste0("G: ", clip_text(blank_if_other(r$planning_group), 30))
      current_line_2 <- paste0("T: ", clip_text(blank_if_other(r$planning_theme), 30))
      draw_text(current_line_1, x_current, row_y + h - 0.105, size = 5.8, col = "#333333", just = c("left", "top"))
      draw_text(current_line_2, x_current, row_y + 0.09, size = 5.8, col = "#555555", just = c("left", "bottom"))

    } else if (item$type == "potential_section") {
      draw_rect(x0, y - h + 0.04, x_right - x0, h - 0.07, fill = "#f5ead6", col = "#d9c19a", lwd = 0.6)
      draw_text("Potential / not-yet-added layer candidates", x0 + 0.08, y - 0.14, size = 8.4, fontface = "bold", just = c("left", "top"))
      draw_text(
        "From the random-adds audit output. Complete first-wave adds are omitted; partial/existing-upgrade candidates remain for planning.",
        x0 + 0.08, y - 0.31, size = 5.9, col = "#5f4b2b", just = c("left", "top")
      )

    } else if (item$type == "candidate_group") {
      draw_rect(x0, y - h + 0.035, x_right - x0, h - 0.055, fill = "#eee2c7", col = "#d1bc90")
      draw_text(item$group, x0 + 0.08, y - h / 2 + 0.01, size = 7.9, fontface = "bold", col = "#3f321d")

    } else if (item$type == "candidate_theme") {
      line_y <- y - h / 2 + 0.005
      draw_line(x0 + 0.1, x_right - 0.1, line_y, col = "#d8c7a6", lwd = 0.55)
      label <- paste0("  ", item$theme, "  ")
      draw_rect(x0 + 3.75, line_y - 0.065, 2.50, 0.13, fill = "white", col = NA)
      draw_text(label, x0 + 5.00, line_y, size = 6.6, fontface = "bold", just = c("center", "center"), col = "#6a5327")

    } else if (item$type == "candidate") {
      r <- item$row
      row_y <- y - h

      draw_rect(x0, row_y, x_right - x0, h, fill = NA, col = "#e1d5bd", lwd = 0.35)
      draw_rect(x_new_group - 0.06, row_y + 0.04, 1.50, h - 0.08, fill = NA, col = "#bdbdbd", lwd = 0.45)
      draw_rect(x_new_theme - 0.06, row_y + 0.04, 1.50, h - 0.08, fill = NA, col = "#bdbdbd", lwd = 0.45)
      draw_rect(x_notes - 0.06, row_y + 0.04, x_right - x_notes, h - 0.08, fill = NA, col = "#bdbdbd", lwd = 0.45)

      draw_text("CAND", x_num + 0.02, row_y + h / 2 + 0.045, size = 5.3, fontface = "bold", col = "#7a5a1c")
      draw_text(clip_text(r$candidate_id, 11), x_num + 0.02, row_y + 0.08, size = 4.4, col = "#7a5a1c")

      draw_text(clip_text(r$candidate_title, 44), x_layer, row_y + h - 0.105, size = 6.5, fontface = "bold", just = c("left", "top"))
      meta_line <- paste0(clip_text(r$provider, 26), " | ", clip_text(first_nonblank(r$service_type, r$implementation_path), 15))
      draw_text(meta_line, x_layer, row_y + 0.08, size = 5.3, col = "#5c5140", just = c("left", "bottom"))

      current_line_1 <- paste0("G: ", clip_text(r$suggested_group, 30))
      current_line_2 <- paste0("T: ", clip_text(r$suggested_theme, 30))
      draw_text(current_line_1, x_current, row_y + h - 0.105, size = 5.6, col = "#4d4030", just = c("left", "top"))
      draw_text(current_line_2, x_current, row_y + 0.09, size = 5.4, col = "#665842", just = c("left", "bottom"))
    }

    y <- y - h
  }

  draw_text(
    "Paper edits only. Transfer final group/theme decisions to 00_config/external_service_catalog.csv. Candidate rows guide future audit/add patches.",
    margin_l, 0.18, size = 6.2, col = "#555555", just = c("left", "bottom")
  )
}

grDevices::dev.off()
pdf_open <- FALSE

file.copy(pdf_timestamped, pdf_latest, overwrite = TRUE)
# Also overwrite the earlier parent/child latest filename so an accidentally
# reopened old PDF is not mistaken for the current version.
file.copy(pdf_timestamped, legacy_pdf_latest, overwrite = TRUE)

message("Wrote: ", pdf_latest)
message("Also refreshed legacy filename: ", legacy_pdf_latest)
message("Wrote: ", csv_latest)
if (nrow(cand_plan) > 0) message("Candidate planning rows included: ", nrow(cand_plan))
message("Pages: ", length(pages))
message("Open/print the PDF on 11 x 17 inch portrait paper.")
