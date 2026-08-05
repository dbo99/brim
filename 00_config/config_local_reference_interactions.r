# ==== config_local_reference_interactions.r ==================================
##
## PURPOSE:
##   Authoritative styling, legend, filtering, search, count, and source-field
##   contracts for the 11 user-facing Local > Reference layers.
##
## SCOPE:
##   This registry intentionally excludes Wild & Scenic Rivers, CalSim3.0,
##   uploads, External, Ops Live, and BRIM Live. Only Wilderness Study Areas is
##   executable in the Phase 1 checkpoint; the other ten rows are contracts.
##
## IMPORTANT:
##   - color_basis is layer-specific. The agency palette is never a fallback.
##   - BLM publication or administration alone never selects BLM symbology.
##   - All map and legend colors remain provisional pending realistic visual QA.
##   - Category rows carry separate map/legend cartographic tokens.

PT_LOCAL_REFERENCE_LAYER_IDS <- c(
  "national_scenic_historic_trails",
  "national_monuments",
  "ca_desert_ncl",
  "wilderness_study_areas",
  "federal_wilderness",
  "drecp",
  "acec",
  "grazing_allotments",
  "counties",
  "rwqcb_regions",
  "water_districts"
)

PT_LOCAL_REFERENCE_BLM_ROLE_VALUES <- c(
  "sole_manager",
  "co_manager",
  "administering_partner",
  "local_land_manager",
  "planning_authority_on_blm_lands",
  "program_administrator",
  "data_steward_only",
  "no_identified_management_role",
  "unknown"
)
PT_LOCAL_REFERENCE_BLM_ROLE_LABELS <- stats::setNames(
  c(
    "Sole manager", "Co-manager", "Administering partner",
    "Local land manager", "Planning authority on BLM lands",
    "Program administrator", "Data steward only",
    "No identified management role", "Unknown"
  ),
  PT_LOCAL_REFERENCE_BLM_ROLE_VALUES
)

PT_LOCAL_REFERENCE_MANAGEMENT_ROLE_SCHEMA <- data.frame(
  field_name = c(
    "layer_id", "semantic_feature_key", "geometry_key",
    "designation_authority", "administering_agency",
    "local_managing_agency", "co_managing_agencies", "blm_role",
    "blm_role_summary", "blm_office", "blm_office_url",
    "management_role_source", "management_role_source_url",
    "management_role_verified_on", "management_role_confidence",
    "provenance_class", "limitations"
  ),
  storage_type = c(
    rep("character", 13), "date", "controlled_character",
    "controlled_character", "character"
  ),
  required = c(
    TRUE, TRUE, FALSE, rep(FALSE, 10), FALSE, TRUE, TRUE, FALSE
  ),
  stringsAsFactors = FALSE
)

PT_LOCAL_REFERENCE_MANAGEMENT_RECOMMENDATIONS <- data.frame(
  layer_id = PT_LOCAL_REFERENCE_LAYER_IDS,
  display_placement = c(
    "popup_segment_verified_only", "hover_and_popup_when_verified",
    "popup_when_verified", "popup", "hover_and_popup_when_verified",
    "popup_authority_context", "popup", "popup", "omit", "omit", "omit"
  ),
  evidence_requirement = c(
    "separate trail-wide administrator from geometry-level manager",
    "feature or geometry-level managing-agency evidence",
    "verified unit or management-category evidence",
    "official BLM WSA family evidence; office requires feature evidence",
    "geometry-component managing-agency evidence",
    "planning-authority evidence; do not imply land management",
    "official BLM designation and plan evidence",
    "administration evidence separate from underlying ownership",
    "not required for basic administrative context",
    "not required; regional authority belongs in compact metadata",
    "not required until governance is normalized"
  ),
  stringsAsFactors = FALSE
)

pt_local_reference_category_rows <- function(
  category_key,
  label,
  source_values,
  fill_color,
  stroke_color,
  fill_opacity,
  stroke_weight,
  dash_array = "",
  legend_swatch_style = "polygon",
  sort_order = seq_along(category_key),
  include_when_absent = TRUE,
  provisional = TRUE
) {
  data.frame(
    category_key = as.character(category_key),
    label = as.character(label),
    source_values = as.character(source_values),
    fill_color = as.character(fill_color),
    stroke_color = as.character(stroke_color),
    fill_opacity = as.numeric(fill_opacity),
    stroke_weight = as.numeric(stroke_weight),
    dash_array = as.character(dash_array),
    legend_swatch_style = as.character(legend_swatch_style),
    sort_order = as.integer(sort_order),
    include_when_absent = as.logical(include_when_absent),
    provisional = as.logical(provisional),
    stringsAsFactors = FALSE
  )
}

PT_LOCAL_REFERENCE_AGENCY_CATEGORIES <- pt_local_reference_category_rows(
  category_key = c(
    "blm", "usfs", "nps", "fws", "other_federal",
    "shared_multi", "unknown"
  ),
  label = c(
    "Bureau of Land Management",
    "U.S. Forest Service",
    "National Park Service",
    "U.S. Fish & Wildlife Service",
    "Other federal",
    "Shared / multi-agency",
    "Unknown / unverified"
  ),
  source_values = c(
    "BLM|6",
    "USFS|US FOREST SERVICE|8",
    "NPS|5",
    "FWS|USFWS|4",
    "OTHER FEDERAL",
    "SHARED|MULTI|MULTI-AGENCY",
    ""
  ),
  fill_color = c(
    "#C69A3C", "#72A57B", "#C78467", "#5A9FA4", "#8998A6",
    "#A58CBD", "#B0B0B0"
  ),
  stroke_color = c(
    "#916100", "#2F6B3A", "#9C4A2F", "#006D77", "#4F6478",
    "#6F4C8E", "#6B6B6B"
  ),
  fill_opacity = c(0.20, 0.20, 0.20, 0.20, 0.18, 0.18, 0.14),
  stroke_weight = c(1.5, 1.5, 1.5, 1.5, 1.4, 1.7, 1.4),
  dash_array = c("", "", "", "", "", "6,3", "2,3"),
  legend_swatch_style = c(
    "polygon", "polygon", "polygon", "polygon", "polygon",
    "dashed_polygon", "dotted_polygon"
  )
)

pt_local_reference_neutral_categories <- function(
  fill_color = "#D8D4C8",
  stroke_color = "#6B6963",
  fill_opacity = 0.04,
  stroke_weight = 1.2
) {
  pt_local_reference_category_rows(
    category_key = c("context", "unknown"),
    label = c("Context boundary", "Unknown / not stated"),
    source_values = c("CONTEXT", ""),
    fill_color = c(fill_color, "#B0B0B0"),
    stroke_color = c(stroke_color, "#6B6B6B"),
    fill_opacity = c(fill_opacity, min(fill_opacity, 0.03)),
    stroke_weight = c(stroke_weight, stroke_weight),
    dash_array = c("", "2,3"),
    legend_swatch_style = c("polygon", "dotted_polygon")
  )
}

PT_LOCAL_REFERENCE_CATEGORY_DEFINITIONS <- list(
  national_scenic_historic_trails = pt_local_reference_category_rows(
    category_key = c(
      "nlcs000280", "nlcs000281", "nlcs000282", "nlcs000283",
      "nlcs000284", "nlcs000285", "unknown"
    ),
    label = c(
      "California Historic Trail",
      "Pony Express Trail",
      "Old Spanish Trail",
      "Juan Bautista de Anza Trail",
      "Pacific Crest Trail",
      "Butterfield Overland National Historic Trail",
      "Unknown trail identity"
    ),
    source_values = c(
      "NLCS000280", "NLCS000281", "NLCS000282", "NLCS000283",
      "NLCS000284", "NLCS000285", ""
    ),
    fill_color = rep("transparent", 7),
    stroke_color = c(
      "#8C510A", "#C51B7D", "#6A3D9A", "#006D77",
      "#2166AC", "#B54800", "#6B6B6B"
    ),
    fill_opacity = rep(0, 7),
    stroke_weight = c(rep(2.4, 6), 2.1),
    dash_array = rep("", 7),
    legend_swatch_style = rep("line", 7)
  ),
  national_monuments = PT_LOCAL_REFERENCE_AGENCY_CATEGORIES,
  ca_desert_ncl = pt_local_reference_neutral_categories(),
  wilderness_study_areas = pt_local_reference_category_rows(
    category_key = c(
      "suitable", "non_suitable", "no_recommendation", "unknown"
    ),
    label = c(
      "Recommended suitable",
      "Recommended non-suitable",
      "No recommendation",
      "Not stated"
    ),
    source_values = c(
      "SUITABLE",
      "NON-SUITABLE|NON SUITABLE",
      "NO RECOMMENDATION",
      ""
    ),
    fill_color = c("#6EA990", "#D58A70", "#88A8BA", "#B0B0B0"),
    stroke_color = c("#287461", "#A14E38", "#4C708A", "#6B6B6B"),
    fill_opacity = c(0.22, 0.20, 0.20, 0.14),
    stroke_weight = c(1.5, 1.5, 1.5, 1.4),
    dash_array = c("", "", "", "2,3"),
    legend_swatch_style = c(
      "polygon", "polygon", "polygon", "dotted_polygon"
    )
  ),
  federal_wilderness = PT_LOCAL_REFERENCE_AGENCY_CATEGORIES,
  drecp = pt_local_reference_neutral_categories(
    fill_color = "#D8D0BE",
    stroke_color = "#756F63"
  ),
  acec = pt_local_reference_neutral_categories(
    fill_color = "#D8D0BE",
    stroke_color = "#756F63"
  ),
  grazing_allotments = pt_local_reference_category_rows(
    category_key = "unknown",
    label = "Status not yet normalized",
    source_values = "",
    fill_color = "#D5CFBF",
    stroke_color = "#6B6963",
    fill_opacity = 0.04,
    stroke_weight = 1.2,
    dash_array = "2,3",
    legend_swatch_style = "dotted_polygon"
  ),
  counties = pt_local_reference_neutral_categories(
    fill_color = "#FFFFFF",
    stroke_color = "#666666",
    fill_opacity = 0.01,
    stroke_weight = 1.0
  ),
  rwqcb_regions = pt_local_reference_category_rows(
    category_key = c(paste0("region_", 1:9), "unknown"),
    label = c(
      "Region 1 – North Coast",
      "Region 2 – San Francisco Bay",
      "Region 3 – Central Coast",
      "Region 4 – Los Angeles",
      "Region 5 – Central Valley",
      "Region 6 – Lahontan",
      "Region 7 – Colorado River Basin",
      "Region 8 – Santa Ana",
      "Region 9 – San Diego",
      "Unknown region"
    ),
    source_values = c(as.character(1:9), ""),
    fill_color = c(
      "#89CD66", "#BA5F27", "#A8A800", "#704489", "#397DBD",
      "#267300", "#BA328B", "#B0893F", "#002673", "#B0B0B0"
    ),
    stroke_color = c(
      "#4F8A2F", "#6B3515", "#666600", "#3C2450", "#1F4E7A",
      "#174400", "#731F56", "#6F5525", "#001545", "#6B6B6B"
    ),
    fill_opacity = c(rep(0.28, 9), 0.14),
    stroke_weight = c(rep(1.2, 9), 1.2),
    dash_array = c(rep("", 9), "2,3"),
    legend_swatch_style = c(rep("polygon", 9), "dotted_polygon")
  ),
  water_districts = pt_local_reference_neutral_categories(
    fill_color = "#D9D9D9",
    stroke_color = "#737373",
    fill_opacity = 0.025,
    stroke_weight = 1.2
  )
)

LOCAL_REFERENCE_INTERACTION_REGISTRY <- data.frame(
  layer_id = PT_LOCAL_REFERENCE_LAYER_IDS,
  source_nickname = c(
    "trails", "monuments", "cadesert_ncl", "wildernessstudyarea",
    "fedwilderness", "drecp", "acec", "allotments", "county",
    "rwqcb_regions", "water_districts"
  ),
  display_name = c(
    "National Scenic/Historic Trails",
    "National Monuments",
    "CA Desert National Conservation Lands",
    "Wilderness Study Areas",
    "Federal Wilderness",
    "DRECP",
    "ACECs",
    "Grazing Allotments",
    "Counties",
    "RWQCB Regions",
    "Water Districts"
  ),
  implementation_status = c(
    "registry_contract", "registry_contract", "registry_contract",
    "phase1_wsa", "registry_contract", "registry_contract",
    "registry_contract", "registry_contract", "registry_contract",
    "registry_contract", "registry_contract"
  ),
  enrichment_depth = c(
    "rich", "rich", "rich", "rich", "rich", "moderate",
    "rich", "moderate", "basic", "basic", "basic"
  ),
  color_basis = c(
    "trail_identity",
    "verified_managing_agency_component",
    "verified_unit_or_neutral",
    "normalized_recommendation_status",
    "verified_managing_agency_component",
    "neutral_program_context",
    "normalized_plan_family_or_neutral",
    "normalized_allotment_status",
    "neutral_administrative_context",
    "region_identity",
    "neutral_feature_context"
  ),
  color_source_field = c(
    "NLCS_ID", "AGENCY_COD", "", "WSA_RCMND", "ManagingAg", "",
    "", "", "", "rwqcb_region_num", ""
  ),
  palette_key = c(
    "trail_identity_v1", "agency_provisional_v1", "neutral_context_v1",
    "wsa_recommendation_provisional_v1", "agency_provisional_v1",
    "neutral_context_v1", "neutral_context_v1", "allotment_status_deferred",
    "neutral_context_v1", "rwqcb_provider_provisional_v1",
    "neutral_context_v1"
  ),
  category_definition = PT_LOCAL_REFERENCE_LAYER_IDS,
  unknown_style = rep("unknown", 11),
  shared_management_style = c(
    "metadata_only", "shared_multi", "metadata_only", "metadata_only",
    "shared_multi", "metadata_only", "metadata_only", "metadata_only",
    "metadata_only", "metadata_only", "metadata_only"
  ),
  legend_mode = c(
    "planned_interactive", "planned_interactive", "none", "interactive",
    "planned_interactive", "none", "none", "planned_interactive", "none",
    "planned_interactive", "none"
  ),
  filter_mode = c(
    "planned_category_search", "planned_category_search", "none",
    "category_search", "planned_category_search", "none", "none",
    "planned_category_plus_feature_search", "none",
    "planned_category_search", "none"
  ),
  auto_supported = c(
    TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE
  ),
  auto_default = c(
    TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE
  ),
  count_mode = c(
    "semantic_feature",
    "semantic_and_geometry_component",
    "semantic_feature",
    "semantic_and_geometry_component",
    "semantic_and_geometry_component",
    "geometry_component",
    "semantic_feature_deferred",
    "semantic_and_geometry_component",
    "semantic_feature",
    "semantic_feature",
    "semantic_and_geometry_component"
  ),
  primary_count_mode = rep("semantic_feature", 11),
  primary_count_label = c(
    "National Scenic/Historic Trails", "National Monuments",
    "CA Desert National Conservation Lands", "Wilderness Study Areas",
    "Federal Wilderness Areas", "DRECP areas", "ACECs",
    "Grazing Allotments", "Counties", "RWQCB Regions", "Water Districts"
  ),
  show_component_count = rep(FALSE, 11),
  component_count_label = c(
    "mapped trail segments", rep("mapped polygon components", 10)
  ),
  category_heading = c(
    "", "", "", "BLM recommendation for wilderness designation",
    "", "", "", "", "", "", ""
  ),
  feature_selection_supported = c(
    FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  ),
  feature_selection_mode = c(
    "none", "none", "none", "semantic_feature_multi",
    "none", "none", "none", "none", "none", "none", "none"
  ),
  feature_display_field = c(
    "NLCS_NAME", "NLCS_NAME", "NLCS_NAME", "pt_wsa_name",
    "NAME", "", "ACEC_NAME", "ALLOT_NAME", "county_name",
    "rwqcb_region_name", "agency_display"
  ),
  auto_zoom_supported = c(
    FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  ),
  auto_zoom_default = c(
    FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  ),
  zoom_padding = rep(36, 11),
  zoom_max = c(12, 11, 11, 12, 11, 9, 11, 12, 9, 9, 12),
  preserve_view_on_reset = rep(TRUE, 11),
  retention_enabled = c(
    FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE
  ),
  stringsAsFactors = FALSE
)

LOCAL_REFERENCE_INTERACTION_REGISTRY$search_fields <- I(list(
  c("NLCS_NAME", "NLCS_ID", "TRAIL_TYPE"),
  c("NLCS_NAME", "AGENCY_COD"),
  "NLCS_NAME",
  c("NLCS_NAME", "WSACODE_ca", "CASEFILE_N", "NLCS_ID", "GlobalID"),
  c("NLCS_NAME", "ManagingAg"),
  character(0),
  c("ACEC_NAME", "LUP_NAME"),
  c("ALLOT_NAME", "ALLOT_NO"),
  "county_name",
  c("rwqcb_region_name", "rwqcb_region_num"),
  "agency_display"
))

## Search-field contracts are kept separate from the older generic search
## matrix because named-feature selection does not treat typing as a map
## filter. Only WSA enables this capability in Phase 1; the remaining rows are
## forward contracts for later layer-specific review.
LOCAL_REFERENCE_INTERACTION_REGISTRY$feature_search_fields <-
  LOCAL_REFERENCE_INTERACTION_REGISTRY$search_fields

LOCAL_REFERENCE_INTERACTION_REGISTRY$category_sort_order <- I(lapply(
  PT_LOCAL_REFERENCE_LAYER_IDS,
  function(layer_id) {
    definition <- PT_LOCAL_REFERENCE_CATEGORY_DEFINITIONS[[layer_id]]
    definition$category_key[order(definition$sort_order)]
  }
))

PT_LOCAL_REFERENCE_RETAINED_FIELD_ALIASES <- list(
  wilderness_study_areas = list(
    nlcs_id = c("NLCS_ID"),
    global_id = c("GlobalID", "GLOBALID", "globalid"),
    name = c("NLCS_NAME"),
    recommendation = c("WSA_RCMND"),
    casefile = c("CASEFILE_N", "CASEFILE_NO"),
    admin_state = c("ADMIN_ST"),
    record_date = c("ROD_DATE"),
    wsa_code = c("WSACODE_ca", "WSACODE_CA"),
    flpma_section = c("FLPMA_SEC_", "FLPMA_SEC_ca", "FLPMA_SEC"),
    modify_date = c("MODIFY_DAT", "MODIFY_DATE"),
    gis_acres = c("GIS_ACRES"),
    sma_id = c("SMA_ID"),
    fau_id = c("FAU_ID")
  )
)

PT_LOCAL_REFERENCE_WSA_SEED_PATH <- file.path(
  "00_config", "local_reference_wsa_seed.csv"
)
PT_LOCAL_REFERENCE_WSA_CURATED_PATH <- file.path(
  "00_config", "local_reference_wsa_curated.csv"
)

## Hover-only abbreviations for directly verified BLM offices. Keep the
## complete official office names in structured data and click popups.
PT_LOCAL_REFERENCE_WSA_HOVER_OFFICE_ABBREVIATIONS <- c(
  "Eagle Lake Field Office" = "Eagle Lake",
  "Applegate Field Office" = "Applegate"
)

PT_LOCAL_REFERENCE_WSA_MANUAL_REVIEW <- data.frame(
  source_name = c(
    "San Benito Wilderness Study Area",
    "Trinity Alps (Subunit 4) Wilderness Study Area"
  ),
  seed_reference_name = c(
    "San Benito Mountain ISA",
    "Trinity Alps Subunit"
  ),
  stringsAsFactors = FALSE
)

PT_LOCAL_REFERENCE_WSA_SOURCE_ONLY <- c(
  "Wall Canyon Wilderness Study Area",
  "Sheldon Contiguous Wilderness Study Area"
)

PT_LOCAL_REFERENCE_WSA_SEED_ONLY <- c(
  "Brushy Mountain / English Ridge Subunit",
  "Carson Iceberg"
)

PT_LOCAL_REFERENCE_WSA_CARD_CAUTION <- paste(
  "Historical recommendation, not current WSA status.",
  "Management continues under the applicable FLPMA authority;",
  "verify current plans, closures, and field-office direction."
)

PT_LOCAL_REFERENCE_WSA_POPUP_CAUTION <- paste(
  "Historical BLM recommendation, not a Congressional wilderness designation",
  "or current WSA status. FLPMA §603 study areas and §202 areas can have",
  "different authorities and management histories; verify current land-use",
  "plans, closures, and field-office direction."
)

PT_LOCAL_REFERENCE_WSA_SOURCE_URL <- paste0(
  "https://gis.blm.gov/caarcgis/rest/services/NCL/",
  "BLM_CA_WildernessStudyAreas/FeatureServer/0"
)
PT_LOCAL_REFERENCE_WSA_DETAIL_URL <- paste0(
  "https://www.blm.gov/programs/national-conservation-lands/about/",
  "maps-data-and-resources/wsa-detail-table"
)
PT_LOCAL_REFERENCE_WSA_DOCUMENTS_URL <- paste0(
  "https://www.blm.gov/learn/blm-library/agency-publications/",
  "select-state-publications/state-wilderness-documents"
)
PT_LOCAL_REFERENCE_WSA_CALIFORNIA_URL <- paste0(
  "https://www.blm.gov/programs/national-conservation-lands/california"
)
PT_LOCAL_REFERENCE_WSA_MANAGEMENT_VERIFIED_ON <- "2026-07-25"
