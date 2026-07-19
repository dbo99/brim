# 46_apply_external_group_theme_reorg.R
# -----------------------------------------------------------------------------
# Purpose:
#   Apply a hand-edited External Layers group/theme planning CSV to BRIM's
#   production external-service catalog.
#
# Intended use:
#   This is a one-off / occasional O&M helper for re-organizing the External
#   Layers panel after reviewing a printable planning sheet. It is intentionally
#   conservative: it backs up the existing catalog, only changes rows that can be
#   matched to the working sheet, and reports rows that were not matched.
#
# Production file changed:
#   00_config/external_service_catalog.csv
#
# Planning input bundled with this patch:
#   00_config/external_catalog_group_theme_working_sheet_20260610_163124_dbo.csv
#
# Notes:
#   * Uses new_group -> external_group.
#   * Uses new_theme -> theme.
#   * Sets external_subgroup = theme so the panel uses one parent group and
#     one child/theme level.
#   * Recalculates group/theme order from the working-sheet order unless the
#     optional proposed_group_order / proposed_theme_order columns are filled.
#   * Disables DWR FERC Project Boundaries per Dave's review note.
#   * Does not add candidate/future layers that are not already in the catalog.
#   * Does not rebuild BRIM; run build_final_map_only() after this script.
# -----------------------------------------------------------------------------

# ---- paths ------------------------------------------------------------------
project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
message("BRIM project root: ", project_root)

catalog_path <- file.path(project_root, "00_config", "external_service_catalog.csv")
working_sheet_path <- file.path(
  project_root,
  "00_config",
  "external_catalog_group_theme_working_sheet_20260610_163124_dbo.csv"
)

if (!file.exists(catalog_path)) {
  stop("External catalog not found: ", catalog_path)
}
if (!file.exists(working_sheet_path)) {
  stop("Working sheet not found: ", working_sheet_path)
}

# ---- helpers ----------------------------------------------------------------
trim_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

blank_to_na <- function(x) {
  x <- trim_chr(x)
  x[x == ""] <- NA_character_
  x
}

first_nonblank <- function(x, fallback = "") {
  x <- trim_chr(x)
  x <- x[x != ""]
  if (length(x) == 0) fallback else x[[1]]
}

safe_int <- function(x) {
  suppressWarnings(as.integer(trim_chr(x)))
}

# Normalize only the handful of names Dave explicitly accepted or that prevent
# accidental split groups caused by spacing/case variants. Do not broadly rename
# Dave's group/theme language here.
normalize_group_theme <- function(x, kind = c("group", "theme")) {
  kind <- match.arg(kind)
  x <- trim_chr(x)
  x <- gsub("[[:space:]]+", " ", x)

  # Accepted/explicit tweaks from review discussion.
  x[x == "Convective Precip"] <- "Convective storms"
  x[tolower(x) == "planning"] <- "Planning"
  x[x == "Surface management / tribal lands"] <- "Surface Management / Tribal Lands"
  x[x == "Surface Management / Tribal lands"] <- "Surface Management / Tribal Lands"
  x[x == "surface management / tribal lands"] <- "Surface Management / Tribal Lands"

  # Common accidental trailing-space variant seen in the working sheet.
  x[x == "Districts/Service/Planning "] <- "Districts/Service/Planning"
  x
}

# ---- read inputs -------------------------------------------------------------
catalog <- read.csv(catalog_path, stringsAsFactors = FALSE, check.names = FALSE,
                    fileEncoding = "UTF-8")
ws <- read.csv(working_sheet_path, stringsAsFactors = FALSE, check.names = FALSE,
               fileEncoding = "UTF-8-BOM")

required_catalog_cols <- c("external_layer_id", "display_name", "primary_panel", "theme")
missing_catalog <- setdiff(required_catalog_cols, names(catalog))
if (length(missing_catalog) > 0) {
  stop("Catalog missing required column(s): ", paste(missing_catalog, collapse = ", "))
}

required_ws_cols <- c("external_layer_id", "display_name", "new_group", "new_theme")
missing_ws <- setdiff(required_ws_cols, names(ws))
if (length(missing_ws) > 0) {
  stop("Working sheet missing required column(s): ", paste(missing_ws, collapse = ", "))
}

# Ensure group/order columns exist in the production catalog.
for (nm in c("external_group", "external_group_order", "external_subgroup", "external_subgroup_order")) {
  if (!nm %in% names(catalog)) catalog[[nm]] <- ""
}

# ---- prepare working-sheet choices -----------------------------------------
ws$row_in_sheet <- seq_len(nrow(ws))
ws$external_layer_id <- trim_chr(ws$external_layer_id)
ws$display_name <- trim_chr(ws$display_name)
ws$new_group <- normalize_group_theme(ws$new_group, "group")
ws$new_theme <- normalize_group_theme(ws$new_theme, "theme")

# Only rows with actual new group/theme choices are intended to update the map.
ws_update <- ws[ws$new_group != "" | ws$new_theme != "", , drop = FALSE]

if (nrow(ws_update) == 0) {
  stop("No nonblank new_group/new_theme values found in working sheet. Nothing to apply.")
}

# Create group/theme order lookup. If proposed order columns exist and are filled,
# use them. Otherwise use first appearance in the marked-up working sheet.
ws_update$group_for_order <- ifelse(ws_update$new_group != "", ws_update$new_group, NA_character_)
ws_update$theme_for_order <- ifelse(ws_update$new_theme != "", ws_update$new_theme, NA_character_)

if ("proposed_group_order" %in% names(ws_update)) {
  ws_update$proposed_group_order_int <- safe_int(ws_update$proposed_group_order)
} else {
  ws_update$proposed_group_order_int <- NA_integer_
}
if ("proposed_theme_order" %in% names(ws_update)) {
  ws_update$proposed_theme_order_int <- safe_int(ws_update$proposed_theme_order)
} else {
  ws_update$proposed_theme_order_int <- NA_integer_
}

# Build group/theme order lookups. Guard all aggregate() calls because R throws
# ``no rows to aggregate`` when optional proposed-order columns exist but are
# completely blank, which is normal for hand-marked planning sheets.
group_input <- ws_update[!is.na(ws_update$group_for_order), , drop = FALSE]
if (nrow(group_input) == 0) {
  stop("No usable new_group values found after normalization. Nothing to apply.")
}

group_order <- aggregate(row_in_sheet ~ group_for_order, data = group_input, FUN = min)
names(group_order) <- c("group", "first_row")

explicit_group_input <- group_input[!is.na(group_input$proposed_group_order_int), , drop = FALSE]
if (nrow(explicit_group_input) > 0) {
  explicit_group <- aggregate(proposed_group_order_int ~ group_for_order,
                              data = explicit_group_input,
                              FUN = min)
  names(explicit_group) <- c("group", "explicit_order")
  group_order <- merge(group_order, explicit_group, by = "group", all.x = TRUE)
} else {
  group_order$explicit_order <- NA_integer_
}

group_order <- group_order[order(ifelse(is.na(group_order$explicit_order), 999999L, group_order$explicit_order), group_order$first_row), ]
group_order$order_value <- seq(10L, by = 10L, length.out = nrow(group_order))

theme_order <- ws_update[!is.na(ws_update$group_for_order) & !is.na(ws_update$theme_for_order),
                         c("group_for_order", "theme_for_order", "row_in_sheet", "proposed_theme_order_int"), drop = FALSE]
if (nrow(theme_order) > 0) {
  theme_first <- aggregate(row_in_sheet ~ group_for_order + theme_for_order, data = theme_order, FUN = min)

  theme_explicit_input <- theme_order[!is.na(theme_order$proposed_theme_order_int), , drop = FALSE]
  if (nrow(theme_explicit_input) > 0) {
    theme_explicit <- aggregate(proposed_theme_order_int ~ group_for_order + theme_for_order,
                                data = theme_explicit_input, FUN = min)
    theme_order <- merge(theme_first, theme_explicit, by = c("group_for_order", "theme_for_order"), all.x = TRUE)
  } else {
    theme_order <- theme_first
    theme_order$proposed_theme_order_int <- NA_integer_
  }

  theme_order <- theme_order[order(theme_order$group_for_order,
                                   ifelse(is.na(theme_order$proposed_theme_order_int), 999999L, theme_order$proposed_theme_order_int),
                                   theme_order$row_in_sheet), ]
  theme_order$order_value <- ave(theme_order$row_in_sheet, theme_order$group_for_order, FUN = function(z) seq(10L, by = 10L, length.out = length(z)))
} else {
  theme_order <- data.frame(group_for_order = character(), theme_for_order = character(), order_value = integer())
}

# ---- backup -----------------------------------------------------------------
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup_path <- file.path(project_root, "00_config", paste0("external_service_catalog_backup_before_EL_003k_", stamp, ".csv"))
write.csv(catalog, backup_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
message("Backed up current catalog to: ", backup_path)

# ---- apply row updates -------------------------------------------------------
catalog$external_layer_id <- trim_chr(catalog$external_layer_id)
catalog$display_name <- trim_chr(catalog$display_name)

matched_catalog_rows <- integer(0)
unmatched_ws <- character(0)

for (i in seq_len(nrow(ws_update))) {
  id <- ws_update$external_layer_id[[i]]
  nm <- ws_update$display_name[[i]]

  idx <- integer(0)
  if (id != "") idx <- which(catalog$external_layer_id == id)
  if (length(idx) == 0 && nm != "") idx <- which(catalog$display_name == nm)

  if (length(idx) == 0) {
    unmatched_ws <- c(unmatched_ws, paste0(id, " | ", nm))
    next
  }

  # If a duplicate ID/name somehow exists, update all matches; this is safer than
  # silently updating the first and leaving a duplicate inconsistent.
  new_group <- ws_update$new_group[[i]]
  new_theme <- ws_update$new_theme[[i]]

  if (new_group != "") catalog$external_group[idx] <- new_group
  if (new_theme != "") {
    catalog$theme[idx] <- new_theme
    catalog$external_subgroup[idx] <- new_theme
  }

  matched_catalog_rows <- c(matched_catalog_rows, idx)
}
matched_catalog_rows <- sort(unique(matched_catalog_rows))

# ---- apply order values ------------------------------------------------------
# Assign group order by normalized group lookup, preserving existing order for
# rows/groups not touched by the planning sheet.
for (i in seq_len(nrow(group_order))) {
  g <- group_order$group[[i]]
  catalog$external_group_order[catalog$external_group == g] <- as.character(group_order$order_value[[i]])
}

if (nrow(theme_order) > 0) {
  for (i in seq_len(nrow(theme_order))) {
    g <- theme_order$group_for_order[[i]]
    t <- theme_order$theme_for_order[[i]]
    catalog$external_subgroup_order[catalog$external_group == g & catalog$theme == t] <- as.character(theme_order$order_value[[i]])
  }
}

# ---- disable known row -------------------------------------------------------
ferc_idx <- grep("DWR FERC Project Boundaries", catalog$display_name, ignore.case = TRUE)
if (length(ferc_idx) > 0) {
  catalog$primary_panel[ferc_idx] <- "disabled"
  message("Disabled DWR FERC Project Boundaries row(s): ", paste(catalog$external_layer_id[ferc_idx], collapse = ", "))
} else {
  warning("Could not find DWR FERC Project Boundaries row to disable.")
}

# ---- write updated catalog ---------------------------------------------------
write.csv(catalog, catalog_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
message("Updated catalog written to: ", catalog_path)
message("Rows updated from working sheet: ", length(matched_catalog_rows))
if (length(unmatched_ws) > 0) {
  message("Working-sheet rows not found in current catalog (likely future candidates or renamed rows):")
  message(paste(" -", unmatched_ws, collapse = "\n"))
}

# ---- write review table ------------------------------------------------------
review_dir <- file.path(project_root, "06_output", "reports", "external_catalog_reorg")
dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)
review_path <- file.path(review_dir, paste0("external_catalog_after_EL_003k_", stamp, ".csv"))
review_cols <- intersect(c("external_layer_id", "display_name", "primary_panel", "external_group_order", "external_group", "external_subgroup_order", "theme", "external_subgroup"), names(catalog))
write.csv(catalog[, review_cols, drop = FALSE], review_path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
message("Review table written to: ", review_path)

message("Done. Next step: run build_final_map_only() and inspect the External Layers panel.")
