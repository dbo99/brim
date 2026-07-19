# 47_tune_spc_forecast_hover_popups.R
# -----------------------------------------------------------------------------
# Purpose
#   One-off/safe O&M script to tune BRIM External catalog rows for SPC forecast
#   layers.  It updates only existing SPC rows in 00_config/external_service_catalog.csv.
#
# Why this is a script instead of a full replacement CSV
#   The External catalog has been changing rapidly during EL_003 QA.  This script
#   preserves the user's current local catalog, creates a timestamped backup, and
#   only edits the SPC hover/popup/link fields needed for this patch.
#
# What it changes
#   * SPC fire wx rows: concise hover using dn + valid/expire; popup link to SPC
#     fire-weather page; curated popup fields. The paired helper patch resolves
#     both native keys (dn/valid/expire) and MapServer alias keys
#     (Outlook/Valid Date Time/Expiration Date).
#   * SPC convective rows: hover starts with label2 (risk category), then valid
#     window; popup link to SPC outlook page; curated popup fields.
#   * SPC mesoscale discussion row: popup link to SPC MD page; concise hover.
#
# What it does NOT change
#   * Does not add/remove layers.
#   * Does not change group/theme/order fields.
#   * Does not change service URLs.
#   * It does switch SPC rows to current_view MapServer query mode so BRIM
#     draws the polygons client-side. This avoids intermittent NOAA ArcGIS
#     exportImage/visual MapServer 400 errors seen during QA, at the cost of
#     approximating SPC colors with BRIM styling rather than using the exact
#     server-rendered image.
# -----------------------------------------------------------------------------

project_root <- getwd()
cat_path <- file.path(project_root, "00_config", "external_service_catalog.csv")

if (!file.exists(cat_path)) {
  stop("External catalog not found: ", cat_path)
}

catalog <- read.csv(cat_path, stringsAsFactors = FALSE, check.names = FALSE)

required_cols <- c(
  "display_name", "popup_fields", "popup_aliases", "popup_link_template",
  "popup_link_label", "hover_fields", "hover_aliases", "hover_bold_fields",
  "hover_no_label_fields", "out_fields", "field_curation_notes", "default_load_mode", "default_style_field", "default_style_method", "style_legend_title"
)

missing_cols <- setdiff(required_cols, names(catalog))
if (length(missing_cols) > 0) {
  stop("Catalog is missing required columns: ", paste(missing_cols, collapse = ", "))
}

# Backup before editing.
ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
backup_path <- file.path(
  project_root,
  "00_config",
  paste0("external_service_catalog_backup_before_EL_003w_", ts, ".csv")
)
write.csv(catalog, backup_path, row.names = FALSE, na = "")
message("Backup written: ", backup_path)

clean <- function(x) trimws(ifelse(is.na(x), "", x))
name <- clean(catalog$display_name)

is_fire <- grepl("^SPC fire wx \\|", name, ignore.case = TRUE)
is_convective <- grepl("^SPC convective \\|", name, ignore.case = TRUE)
is_meso <- grepl("^SPC mesoscale discussions$", name, ignore.case = TRUE)

fire_aliases <- paste(
  "spc_category=Fire-weather category",
  "spc_valid_pacific=Valid from",
  "spc_expire_pacific=Valid through",
  "spc_valid_utc=Provider/UTC valid from",
  "spc_expire_utc=Provider/UTC valid through",
  "idp_filedate=GIS file date",
  "idp_ingestdate=GIS ingest date",
  "idp_source=GIS source",
  sep = ";"
)

conv_aliases <- paste(
  "spc_category=Risk category",
  "label=Risk label",
  "dn=Risk code",
  "spc_valid_pacific=Valid from",
  "spc_expire_pacific=Valid through",
  "spc_valid_utc=Provider/UTC valid from",
  "spc_expire_utc=Provider/UTC valid through",
  "issue=Issued",
  "idp_filedate=GIS file date",
  "idp_ingestdate=GIS ingest date",
  "idp_source=GIS source",
  sep = ";"
)

meso_aliases <- paste(
  "name=Discussion",
  "idp_filedate=GIS file date",
  "idp_ingestdate=GIS ingest date",
  "idp_source=GIS source",
  sep = ";"
)

catalog[is_fire, "popup_fields"] <- "spc_category;spc_interpretation;spc_valid_pacific;spc_expire_pacific;spc_valid_utc;spc_expire_utc;idp_filedate;idp_ingestdate;idp_source"
catalog[is_fire, "popup_aliases"] <- fire_aliases
catalog[is_fire, "popup_link_template"] <- "https://www.spc.noaa.gov/products/fire_wx/"
catalog[is_fire, "popup_link_label"] <- "Open SPC fire-weather outlook page"
catalog[is_fire, "hover_fields"] <- "spc_category;spc_interpretation;spc_valid_pacific;spc_expire_pacific"
catalog[is_fire, "hover_aliases"] <- "spc_category=Fire wx;spc_interpretation=Meaning;spc_valid_pacific=Valid from;spc_expire_pacific=Valid through"
catalog[is_fire, "hover_bold_fields"] <- "spc_category"
catalog[is_fire, "hover_no_label_fields"] <- "spc_category"
catalog[is_fire, "out_fields"] <- "dn;label;label2;valid;expire;idp_filedate;idp_ingestdate;idp_source;objectid"
catalog[is_fire, "default_load_mode"] <- "current_view"
catalog[is_fire, "default_style_field"] <- "spc_category"
catalog[is_fire, "default_style_method"] <- "spc_forecast_category"
catalog[is_fire, "style_legend_title"] <- "SPC fire-weather category"
catalog[is_fire, "field_curation_notes"] <- "EL_003y: SPC hover uses Pacific time and BRIM-friendly category interpretation; popup includes Pacific and provider/UTC time. Category translated from label2/label/dn in BRIM."

catalog[is_convective, "popup_fields"] <- "spc_category;spc_interpretation;spc_valid_pacific;spc_expire_pacific;spc_valid_utc;spc_expire_utc;issue;idp_filedate;idp_ingestdate;idp_source"
catalog[is_convective, "popup_aliases"] <- conv_aliases
catalog[is_convective, "popup_link_template"] <- "https://www.spc.noaa.gov/products/outlook/"
catalog[is_convective, "popup_link_label"] <- "Open SPC convective outlook page"
catalog[is_convective, "hover_fields"] <- "spc_category;spc_interpretation;spc_valid_pacific;spc_expire_pacific"
catalog[is_convective, "hover_aliases"] <- "spc_category=Risk;spc_interpretation=Meaning;spc_valid_pacific=Valid from;spc_expire_pacific=Valid through"
catalog[is_convective, "hover_bold_fields"] <- "spc_category"
catalog[is_convective, "hover_no_label_fields"] <- "spc_category"
catalog[is_convective, "out_fields"] <- "label2;label;dn;valid;expire;issue;idp_filedate;idp_ingestdate;idp_source;objectid"
catalog[is_convective, "default_load_mode"] <- "current_view"
catalog[is_convective, "default_style_field"] <- "spc_category"
catalog[is_convective, "default_style_method"] <- "spc_forecast_category"
catalog[is_convective, "style_legend_title"] <- "SPC convective category"
catalog[is_convective, "field_curation_notes"] <- "EL_003y: SPC hover uses Pacific time and BRIM-friendly category interpretation; popup includes Pacific and provider/UTC valid window plus official SPC link."

catalog[is_meso, "popup_fields"] <- "name;idp_filedate;idp_ingestdate;idp_source"
catalog[is_meso, "popup_aliases"] <- meso_aliases
catalog[is_meso, "popup_link_template"] <- "https://www.spc.noaa.gov/products/md/"
catalog[is_meso, "popup_link_label"] <- "Open SPC mesoscale discussion page"
catalog[is_meso, "hover_fields"] <- "name;idp_filedate"
catalog[is_meso, "hover_aliases"] <- "name=Discussion;idp_filedate=GIS file date"
catalog[is_meso, "hover_bold_fields"] <- "name"
catalog[is_meso, "hover_no_label_fields"] <- "name"
catalog[is_meso, "out_fields"] <- "name;idp_filedate;idp_ingestdate;idp_source;objectid"
catalog[is_meso, "default_load_mode"] <- "current_view"
catalog[is_meso, "default_style_field"] <- ""
catalog[is_meso, "default_style_method"] <- "spc_forecast_category"
catalog[is_meso, "style_legend_title"] <- "SPC mesoscale discussion"
catalog[is_meso, "field_curation_notes"] <- "EL_003y: SPC MD hover/popup tuned with official SPC MD page link."

n_fire <- sum(is_fire)
n_convective <- sum(is_convective)
n_meso <- sum(is_meso)

write.csv(catalog, cat_path, row.names = FALSE, na = "")
message("Updated catalog: ", cat_path)
message("SPC fire rows updated: ", n_fire)
message("SPC convective rows updated: ", n_convective)
message("SPC mesoscale rows updated: ", n_meso)

if (n_fire == 0 || n_convective == 0 || n_meso == 0) {
  warning("One or more SPC row groups had zero matches. Review display_name values before relying on this patch.")
}
