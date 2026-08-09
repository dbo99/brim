# ==== config_local_reference_interactions.r ==================================
##
## PURPOSE:
##   Authoritative styling, legend, filtering, search, count, and source-field
##   contracts for the 11 user-facing Local > Reference layers.
##
## SCOPE:
##   This registry intentionally excludes Wild & Scenic Rivers, CalSim3.0,
##   uploads, External, Ops Live, and BRIM Live. Trails and Wilderness Study
##   Areas are executable in Phase 2. Federal Wilderness is the accepted
##   Phase 3 implementation. ACECs are the focused Phase 4 implementation;
##   the other seven rows remain contracts.
##
## IMPORTANT:
##   - color_basis is layer-specific. The agency palette is never a fallback.
##   - BLM publication or administration alone never selects BLM symbology.
##   - All map and legend colors remain provisional pending realistic visual QA.
##   - Category rows carry separate map/legend cartographic tokens.
##   - Phase 3 enables Federal Wilderness beside the accepted Trails/WSA work.

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

## Federal Wilderness retains the exact agency colors used by the existing
## BRIM layer. These are accepted layer constants, not the provisional shared
## agency palette above.
PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_AGENCY_CATEGORIES <-
  pt_local_reference_category_rows(
    category_key = c("blm", "usfs", "nps", "fws", "unknown"),
    label = c(
      "Bureau of Land Management",
      "U.S. Forest Service",
      "National Park Service",
      "U.S. Fish & Wildlife Service",
      "Unknown / unverified"
    ),
    source_values = c("6|BLM", "8|USFS", "5|NPS", "4|FWS|USFWS", ""),
    fill_color = c("#B8860B", "#228B22", "#54278F", "#1F78B4", "#737373"),
    stroke_color = c("#B8860B", "#228B22", "#54278F", "#1F78B4", "#737373"),
    fill_opacity = c(0.18, 0.18, 0.18, 0.18, 0.12),
    stroke_weight = c(1.6, 1.6, 1.6, 1.6, 1.4),
    dash_array = c("", "", "", "", "2,3"),
    legend_swatch_style = c(
      "polygon", "polygon", "polygon", "polygon", "dotted_polygon"
    ),
    include_when_absent = c(TRUE, TRUE, TRUE, TRUE, FALSE),
    provisional = FALSE
  )

PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_FACETS <- list(
  list(
    facet_key = "management_pattern",
    label = "Management pattern",
    record_field = "pt_fw_management_pattern",
    count_mode = "semantic_feature",
    values = data.frame(
      value_key = c("single_agency", "shared_multi_agency"),
      label = c("Single-agency wilderness", "Shared or multi-agency wilderness"),
      sort_order = 1:2,
      stringsAsFactors = FALSE
    )
  ),
  list(
    facet_key = "designation_history",
    label = "Designation history",
    record_field = "pt_fw_designation_history",
    count_mode = "semantic_feature",
    values = data.frame(
      value_key = c("original_only", "has_subsequent_law"),
      label = c("Original designation only", "Has subsequent public law"),
      sort_order = 1:2,
      stringsAsFactors = FALSE
    )
  ),
  list(
    facet_key = "geographic_context",
    label = "Geographic context",
    record_field = "pt_fw_geographic_context",
    count_mode = "geometry_component",
    values = data.frame(
      value_key = c("california", "western_nevada_context"),
      label = c("California", "Western Nevada context"),
      sort_order = 1:2,
      stringsAsFactors = FALSE
    )
  )
)

PT_LOCAL_REFERENCE_ACEC_VALUE_FAMILY_STYLES <- data.frame(
  value_key = c(
    "water_aquatic", "wildlife_and_habitat",
    "botanical_or_ecological", "cultural_archaeological_historic",
    "scenic", "other_or_unresolved"
  ),
  label = c(
    "Fish or aquatic resources", "Wildlife and habitat",
    "Natural systems or processes", "Cultural or historic",
    "Scenic", "Natural hazard or other"
  ),
  swatch_color = c(
    "#3B82A0", "#7A9A4A", "#4F8C68",
    "#A66A43", "#8A6DAA", "#B58A3D"
  ),
  sort_order = 1:6,
  stringsAsFactors = FALSE
)

PT_LOCAL_REFERENCE_ACEC_VALUE_THEMATIC_STYLE <- list(
  multiple_selection = "neutral",
  fill_opacity = 0.22,
  stroke_weight = 2,
  stroke_darken = 0.28
)

## ACEC overlap colors are an identity aid only. The browser assigns them to
## the fixed semantic graph once, then applies them only when both endpoints
## of at least one reviewed overlap pair are currently visible.
PT_LOCAL_REFERENCE_ACEC_OVERLAP_STYLE <- list(
  palette = c(
    "#0072B2", "#E69F00", "#009E73",
    "#CC79A7", "#56B4E9", "#D55E00"
  ),
  fill_opacity = 0.27,
  stroke_weight = 2.2,
  stroke_darken = 0.30,
  minimum_overlap_area_m2 = 1
)

PT_LOCAL_REFERENCE_ACEC_OVERLAP_METADATA <- list(
  source_snapshot_sha256 =
    "0e2658c269476fa629fa7da83b93097e76655042a56bc1cc9d96e10fa6193d00",
  source_geometry_role = "current authoritative BLM geometry before display simplification",
  geometry_processing = "sf::st_make_valid in EPSG:3310; no simplification",
  area_criterion = "intersection area strictly greater than 1 square metre",
  historical_geometry_role = "QA comparison only"
)

PT_LOCAL_REFERENCE_ACEC_CURRENT_FIELD_OFFICES_PATH <- file.path(
  "00_config", "local_reference_acec_current_field_offices.csv"
)
PT_LOCAL_REFERENCE_ACEC_FIELD_OFFICE_CONTEXT_PATH <- file.path(
  "00_config", "local_reference_acec_field_office_context.csv"
)
PT_LOCAL_REFERENCE_ACEC_CURRENT_FIELD_OFFICES <- utils::read.csv(
  PT_LOCAL_REFERENCE_ACEC_CURRENT_FIELD_OFFICES_PATH,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
PT_LOCAL_REFERENCE_ACEC_CURRENT_FIELD_OFFICES <-
  PT_LOCAL_REFERENCE_ACEC_CURRENT_FIELD_OFFICES[
    order(PT_LOCAL_REFERENCE_ACEC_CURRENT_FIELD_OFFICES$sort_order),
    , drop = FALSE
  ]
PT_LOCAL_REFERENCE_ACEC_FIELD_OFFICE_CONTEXT_METADATA <- list(
  geometry_source = "BRIM field_office_outer_wgs84.rds",
  geometry_source_sha256 =
    "89884eb36cafda640ed13165b47beb18005ac086f0549bb5c0dbf1b46ef197c6",
  boundary_snapshot_date = "2025-06-23",
  geometry_processing = paste(
    "existing unsimplified BRIM field-office derivative and authoritative ACEC",
    "geometry repaired and intersected in EPSG:3310"
  ),
  minimum_intersection_area_m2 = 100,
  complete_coverage_percent = 99.5,
  presentation_additional_office_minimum_percent = 1,
  presentation_rule = paste(
    "show the largest office relationship and any additional relationship",
    "covering at least 1 percent; retain all relationships in technical details"
  ),
  relationship_role = paste(
    "spatial context only; does not establish administrative responsibility"
  )
)

PT_LOCAL_REFERENCE_ACEC_AREA_PRESENTATION <- list(
  primary_area = "current BLM source GIS acreage",
  material_difference_minimum_acres = 10,
  material_difference_minimum_percent = 0.5,
  derived_area_default_location = "collapsed technical details"
)

PT_LOCAL_REFERENCE_ACEC_FACETS <- list(
  list(
    facet_key = "relevant_value_family",
    label = "Relevant and important values",
    record_field = "pt_acec_value_families",
    count_mode = "semantic_feature",
    multivalue_delimiter = ";",
    collapsible = TRUE,
    open_default = TRUE,
    thematic_style = PT_LOCAL_REFERENCE_ACEC_VALUE_THEMATIC_STYLE,
    values = PT_LOCAL_REFERENCE_ACEC_VALUE_FAMILY_STYLES
  ),
  list(
    facet_key = "planning_framework",
    label = "Planning framework",
    record_field = "pt_acec_planning_framework",
    count_mode = "semantic_feature",
    multivalue_delimiter = "",
    collapsible = TRUE,
    open_default = FALSE,
    values = data.frame(
      value_key = c(
        "drecp", "central_california_plan",
        "northwest_california_integrated_plan",
        "california_desert_other_plan", "northern_california_other_plan"
      ),
      label = c(
        "DRECP", "Central California Plan",
        "Northwest California Integrated Plan",
        "California Desert other plan", "Northern California other plan"
      ),
      sort_order = 1:5,
      stringsAsFactors = FALSE
    )
  ),
  list(
    facet_key = "field_office_context",
    label = "Field office context",
    record_field = "pt_acec_field_office_context",
    count_mode = "semantic_feature",
    multivalue_delimiter = ";",
    collapsible = TRUE,
    open_default = FALSE,
    values = data.frame(
      value_key = PT_LOCAL_REFERENCE_ACEC_CURRENT_FIELD_OFFICES$office_key,
      label = PT_LOCAL_REFERENCE_ACEC_CURRENT_FIELD_OFFICES$current_official_name,
      sort_order = as.integer(
        PT_LOCAL_REFERENCE_ACEC_CURRENT_FIELD_OFFICES$sort_order
      ),
      stringsAsFactors = FALSE
    )
  )
)

PT_LOCAL_REFERENCE_ACEC_QUICK_VIEWS <- list(
  list(
    quick_view_key = "fish_aquatic",
    label = "Fish / aquatic values",
    facet_key = "relevant_value_family",
    value_key = "water_aquatic"
  ),
  list(
    quick_view_key = "drecp",
    label = "DRECP ACECs",
    facet_key = "planning_framework",
    value_key = "drecp"
  ),
  list(
    quick_view_key = "wildlife_habitat",
    label = "Wildlife / habitat",
    facet_key = "relevant_value_family",
    value_key = "wildlife_and_habitat"
  ),
  list(
    quick_view_key = "cultural_historic",
    label = "Cultural / historic",
    facet_key = "relevant_value_family",
    value_key = "cultural_archaeological_historic"
  ),
  list(
    quick_view_key = "scenic",
    label = "Scenic",
    facet_key = "relevant_value_family",
    value_key = "scenic"
  ),
  list(
    quick_view_key = "natural_systems",
    label = "Natural systems",
    facet_key = "relevant_value_family",
    value_key = "botanical_or_ecological"
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
      "California",
      "Pony Express",
      "Old Spanish",
      "Juan Bautista de Anza",
      "Pacific Crest",
      "Butterfield Overland",
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
    legend_swatch_style = rep("line", 7),
    include_when_absent = c(rep(TRUE, 6), FALSE)
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
  federal_wilderness = PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_AGENCY_CATEGORIES,
  drecp = pt_local_reference_neutral_categories(
    fill_color = "#D8D0BE",
    stroke_color = "#756F63"
  ),
  acec = pt_local_reference_category_rows(
    category_key = c("acec", "unknown"),
    label = c("Area of Critical Environmental Concern", "Unknown / unresolved"),
    source_values = c("ACEC", ""),
    fill_color = c("#B86F52", "#B0B0B0"),
    stroke_color = c("#7A3F2E", "#6B6B6B"),
    fill_opacity = c(0.12, 0.08),
    stroke_weight = c(1.6, 1.4),
    dash_array = c("", "2,3"),
    legend_swatch_style = c("polygon", "dotted_polygon"),
    include_when_absent = c(TRUE, FALSE)
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
    "phase2_trails", "registry_contract", "registry_contract",
    "phase1_wsa", "phase3_federal_wilderness", "registry_contract",
    "phase4_acec", "registry_contract", "registry_contract",
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
    "wsa_recommendation_provisional_v1", "federal_wilderness_agency_accepted_v1",
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
    "interactive", "planned_interactive", "none", "interactive",
    "interactive", "none", "interactive", "planned_interactive", "none",
    "planned_interactive", "none"
  ),
  filter_mode = c(
    "category_search", "planned_category_search", "none",
    "category_search", "faceted_category_search", "none", "faceted_category_search",
    "planned_category_plus_feature_search", "none",
    "planned_category_search", "none"
  ),
  auto_supported = c(
    TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE
  ),
  auto_default = c(
    TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE
  ),
  count_mode = c(
    "semantic_feature",
    "semantic_and_geometry_component",
    "semantic_feature",
    "semantic_and_geometry_component",
    "semantic_and_geometry_component",
    "geometry_component",
    "semantic_and_geometry_component",
    "semantic_and_geometry_component",
    "semantic_feature",
    "semantic_feature",
    "semantic_and_geometry_component"
  ),
  primary_count_mode = rep("semantic_feature", 11),
  primary_count_label = c(
    "trails", "National Monuments",
    "CA Desert National Conservation Lands", "Wilderness Study Areas",
    "named wildernesses", "DRECP areas", "ACECs",
    "Grazing Allotments", "Counties", "RWQCB Regions", "Water Districts"
  ),
  show_component_count = c(FALSE, FALSE, FALSE, FALSE, TRUE, rep(FALSE, 6)),
  show_category_count = c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE),
  category_filter_visible = c(
    TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE
  ),
  category_count_mode = c(
    "semantic_feature", "semantic_feature", "semantic_feature",
    "semantic_feature", "geometry_component", rep("semantic_feature", 6)
  ),
  component_count_label = c(
    "mapped trail segments", rep("mapped polygon components", 3),
    "mapped components", rep("mapped polygon components", 6)
  ),
  category_heading = c(
    "Trail", "", "", "BLM recommendation for wilderness designation",
    "Managing agency", "", "", "", "", "", ""
  ),
  card_caution = c(
    paste(
      "Mapped trail lines are reference representations and do not imply",
      "a continuous maintained route, public access, or current passability."
    ),
    "", "", paste(
      "Historical recommendation, not current WSA status.",
      "Management continues under the applicable FLPMA authority;",
      "verify current plans, closures, and field-office direction."
    ),
    paste(
      "Federal Wilderness boundaries and managing agencies are reference data.",
      "Verify current access, closures, permits, and agency direction."
    ),
    "", paste(
      "An ACEC boundary is a BLM planning designation, not an ownership or",
      "cadastral boundary and not proof of public access or site-specific uses."
    ), "", "", "", ""
  ),
  popup_layout = ifelse(
    PT_LOCAL_REFERENCE_LAYER_IDS %in% c(
      "national_scenic_historic_trails", "wilderness_study_areas",
      "federal_wilderness", "acec"
    ),
    "tabbed_card",
    "standard"
  ),
  feature_selection_supported = c(
    TRUE, FALSE, FALSE, TRUE, TRUE, FALSE,
    TRUE, FALSE, FALSE, FALSE, FALSE
  ),
  feature_selection_mode = c(
    "semantic_feature_multi", "none", "none", "semantic_feature_multi",
    "semantic_feature_multi", "none", "semantic_feature_multi", "none", "none", "none", "none"
  ),
  feature_display_field = c(
    "pt_trails_official_name", "NLCS_NAME", "NLCS_NAME", "pt_wsa_name",
    "pt_fw_official_name", "", "pt_acec_official_name", "ALLOT_NAME", "county_name",
    "rwqcb_region_name", "agency_display"
  ),
  auto_zoom_supported = c(
    TRUE, FALSE, FALSE, TRUE, TRUE, FALSE,
    TRUE, FALSE, FALSE, FALSE, FALSE
  ),
  auto_zoom_default = c(
    TRUE, FALSE, FALSE, TRUE, TRUE, FALSE,
    TRUE, FALSE, FALSE, FALSE, FALSE
  ),
  zoom_padding = rep(36, 11),
  zoom_max = c(12, 11, 11, 12, 11, 9, 11, 12, 9, 9, 12),
  preserve_view_on_reset = rep(TRUE, 11),
  retention_enabled = c(
    TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE
  ),
  distinguish_units_supported = c(
    FALSE, FALSE, FALSE, FALSE, TRUE, FALSE,
    FALSE, FALSE, FALSE, FALSE, FALSE
  ),
  stringsAsFactors = FALSE
)

LOCAL_REFERENCE_INTERACTION_REGISTRY$search_fields <- I(list(
  c("NLCS_NAME", "NLCS_ID", "NSHT_SGMNT_NO", "TRAIL_TYPE"),
  c("NLCS_NAME", "AGENCY_COD"),
  "NLCS_NAME",
  c("NLCS_NAME", "WSACODE_ca", "CASEFILE_N", "NLCS_ID", "GlobalID"),
  c(
    "pt_fw_official_name", "NLCS_NAME", "wilderness_id", "component_id",
    "GlobalID", "FAU_ID", "ManagingAg", "pt_fw_agency_name",
    "pt_fw_alternate_names", "pt_fw_wilderness_abbreviation",
    "pt_fw_designation_year", "pt_fw_original_public_law"
  ),
  character(0),
  c(
    "pt_acec_official_name", "pt_acec_legacy_name", "pt_acec_aliases",
    "pt_acec_governing_plan", "pt_acec_planning_framework",
    "pt_acec_source_admin_unit", "pt_acec_field_office_context_names",
    "acec_id", "component_id",
    "pt_acec_global_id"
  ),
  c("ALLOT_NAME", "ALLOT_NO"),
  "county_name",
  c("rwqcb_region_name", "rwqcb_region_num"),
  "agency_display"
))

## Search-field contracts are kept separate from the older generic search
## matrix because named-feature selection does not treat typing as a map
## filter. Trails, WSA, and Federal Wilderness enable this capability; the
## remaining rows are forward contracts for later layer-specific review.
LOCAL_REFERENCE_INTERACTION_REGISTRY$feature_search_fields <-
  LOCAL_REFERENCE_INTERACTION_REGISTRY$search_fields
LOCAL_REFERENCE_INTERACTION_REGISTRY$feature_search_fields[[1]] <- c(
  "pt_trails_official_name", "pt_trails_common_name",
  "pt_trails_abbreviation", "pt_trails_alias_search", "pt_trails_nlcs_id"
)
LOCAL_REFERENCE_INTERACTION_REGISTRY$feature_search_fields[[5]] <- c(
  "pt_fw_official_name", "NLCS_NAME", "wilderness_id", "component_id",
  "GlobalID", "FAU_ID", "pt_fw_agency_name", "pt_fw_alternate_names",
  "pt_fw_wilderness_abbreviation", "pt_fw_original_public_law"
)
LOCAL_REFERENCE_INTERACTION_REGISTRY$feature_search_fields[[7]] <- c(
  "pt_acec_official_name", "pt_acec_legacy_name", "pt_acec_aliases",
  "pt_acec_governing_plan", "pt_acec_planning_framework",
  "pt_acec_source_admin_unit", "pt_acec_field_office_context_names",
  "acec_id", "component_id",
  "pt_acec_global_id"
)

LOCAL_REFERENCE_INTERACTION_REGISTRY$filter_facets <- I(lapply(
  PT_LOCAL_REFERENCE_LAYER_IDS,
  function(layer_id) {
    if (identical(layer_id, "federal_wilderness")) {
      return(PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_FACETS)
    }
    if (identical(layer_id, "acec")) {
      return(PT_LOCAL_REFERENCE_ACEC_FACETS)
    }
    list()
  }
))

LOCAL_REFERENCE_INTERACTION_REGISTRY$quick_views <- I(lapply(
  PT_LOCAL_REFERENCE_LAYER_IDS,
  function(layer_id) {
    if (identical(layer_id, "acec")) {
      return(PT_LOCAL_REFERENCE_ACEC_QUICK_VIEWS)
    }
    list()
  }
))

LOCAL_REFERENCE_INTERACTION_REGISTRY$category_sort_order <- I(lapply(
  PT_LOCAL_REFERENCE_LAYER_IDS,
  function(layer_id) {
    definition <- PT_LOCAL_REFERENCE_CATEGORY_DEFINITIONS[[layer_id]]
    definition$category_key[order(definition$sort_order)]
  }
))

PT_LOCAL_REFERENCE_RETAINED_FIELD_ALIASES <- list(
  national_scenic_historic_trails = list(
    nlcs_id = c("NLCS_ID"),
    global_id = c("GlobalID", "GLOBALID", "globalid"),
    name = c("NLCS_NAME"),
    source_segment = c("NSHT_SGMNT_NO", "NSHT_SGMNT"),
    trail_type = c("TRAIL_TYPE"),
    management_agency = c("MNG_AGCY"),
    admin_state = c("ADMIN_ST"),
    condition_category = c("NHT_CND_CTGY", "NHT_CND_CT"),
    create_date = c("CREATE_DAT", "CREATE_DATE"),
    modify_date = c("MODIFY_DATE", "MODIFY_DAT")
  ),
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
  ),
  federal_wilderness = list(
    nlcs_id = c("NLCS_ID"),
    global_id = c("GlobalID", "GLOBALID", "globalid"),
    name = c("NLCS_NAME"),
    agency_code = c("ManagingAg"),
    agency_name = c("managing_agency", "ManagingAgency"),
    admin_state = c("ADMIN_ST", "geographic_state", "State"),
    gis_acres = c("GIS_Acres", "GISAcres", "GIS_ACRES"),
    modify_date = c("Modify_Dat", "Modify_Date", "MODIFY_DATE"),
    fau_id = c("FAU_ID")
  ),
  acec = list(
    global_id = c("GlobalID", "GLOBALID", "globalid"),
    name = c("ACEC_NAME"),
    governing_plan = c("LUP_NAME"),
    nepa_number = c("NEPA_NUM"),
    rod_date = c("ROD_DATE"),
    gis_acres = c("GIS_ACRES"),
    admin_state = c("ADMIN_ST"),
    admin_unit = c("CA_ADMIN_unit_code"),
    modify_date = c("BLM_MODIFY_DATE"),
    last_edited_date = c("last_edited_date"),
    relevance_cultural = c("ACEC_RLVNCE_CUL"),
    relevance_fish = c("ACEC_RLVNCE_FRSC"),
    relevance_historic = c("ACEC_RLVNCE_HIS"),
    relevance_hazard = c("ACEC_RLVNCE_NHAZ"),
    relevance_natural_process = c("ACEC_RLVNCE_NPRO"),
    relevance_natural_system = c("ACEC_RLVNCE_NSYS"),
    relevance_scenic = c("ACEC_RLVNCE_SCE"),
    relevance_wildlife = c("ACEC_RLVNCE_WRSC"),
    management_protect = c("SPCL_MGMT_ATTN_RX_PRTCT"),
    management_prevent = c("SPCL_MGMT_ATTN_RX_PRVNT")
  )
)

PT_LOCAL_REFERENCE_ACEC_COMPONENTS_PATH <- file.path(
  "00_config", "local_reference_acec_components.csv"
)
PT_LOCAL_REFERENCE_ACEC_REFERENCE_PATH <- file.path(
  "00_config", "local_reference_acec_reference.csv"
)
PT_LOCAL_REFERENCE_ACEC_VALUES_PATH <- file.path(
  "00_config", "local_reference_acec_values.csv"
)
PT_LOCAL_REFERENCE_ACEC_DOCUMENTS_PATH <- file.path(
  "00_config", "local_reference_acec_documents.csv"
)
PT_LOCAL_REFERENCE_ACEC_MANAGEMENT_PATH <- file.path(
  "00_config", "local_reference_acec_management_prescriptions.csv"
)
PT_LOCAL_REFERENCE_ACEC_PLANNING_PATH <- file.path(
  "00_config", "local_reference_acec_planning_history.csv"
)
PT_LOCAL_REFERENCE_ACEC_RELATIONSHIPS_PATH <- file.path(
  "00_config", "local_reference_acec_relationships.csv"
)
PT_LOCAL_REFERENCE_ACEC_SOURCES_PATH <- file.path(
  "00_config", "local_reference_acec_source_register.csv"
)
PT_LOCAL_REFERENCE_ACEC_OFFICES_PATH <- file.path(
  "00_config", "local_reference_acec_office_assignments.csv"
)
PT_LOCAL_REFERENCE_ACEC_ACCESS_PATH <- file.path(
  "00_config", "local_reference_acec_access_land_status.csv"
)
PT_LOCAL_REFERENCE_ACEC_OVERRIDES_PATH <- file.path(
  "00_config", "local_reference_acec_curated_overrides.csv"
)
PT_LOCAL_REFERENCE_ACEC_UI_FILTERS_PATH <- file.path(
  "00_config", "local_reference_acec_ui_filter_lookup.csv"
)
PT_LOCAL_REFERENCE_ACEC_WSA_NAME_CONTEXT_PATH <- file.path(
  "00_config", "local_reference_acec_wsa_name_context.csv"
)
PT_LOCAL_REFERENCE_ACEC_OVERLAP_PAIRS_PATH <- file.path(
  "00_config", "local_reference_acec_overlap_pairs.csv"
)

PT_LOCAL_REFERENCE_ACEC_SOURCE_URL <- paste0(
  "https://gis.blm.gov/caarcgis/rest/services/Planning/",
  "BLM_CA_ACEC/FeatureServer/0"
)

PT_LOCAL_REFERENCE_ACEC_BOUNDARY_CAVEAT <- paste(
  "The mapped ACEC boundary is a BLM planning-designation boundary. It is not",
  "an ownership or cadastral boundary and does not by itself establish public",
  "access or site-specific allowable uses. Consult the governing land-use",
  "plan and current authoritative BLM information for management decisions."
)

PT_LOCAL_REFERENCE_ACEC_MANAGEMENT_CAVEAT <- paste(
  "Detailed travel management, grazing, minerals, recreation, access,",
  "closures, rights-of-way, development, and exceptions are plan-specific",
  "and may vary within the mapped area."
)

PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_COMPONENTS_PATH <- file.path(
  "00_config", "local_reference_federal_wilderness_components.csv"
)
PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_REFERENCE_PATH <- file.path(
  "00_config", "local_reference_federal_wilderness_reference.csv"
)
PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_DESIGNATION_VALIDATION_PATH <- file.path(
  "00_config", "local_reference_federal_wilderness_designation_validation.csv"
)
PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_DOCUMENTS_PATH <- file.path(
  "00_config", "local_reference_federal_wilderness_documents.csv"
)
PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_COMMON_POLICY_PATH <- file.path(
  "00_config", "local_reference_federal_wilderness_common_policy_language.csv"
)
PT_LOCAL_REFERENCE_FEDERAL_WILDERNESS_SOURCE_REGISTER_PATH <- file.path(
  "00_config", "local_reference_federal_wilderness_source_register.csv"
)

PT_LOCAL_REFERENCE_TRAILS_REFERENCE_PATH <- file.path(
  "00_config", "local_reference_trails_reference.csv"
)
PT_LOCAL_REFERENCE_TRAILS_ALIASES_PATH <- file.path(
  "00_config", "local_reference_trails_aliases.csv"
)
PT_LOCAL_REFERENCE_TRAILS_CURATED_OVERRIDES_PATH <- file.path(
  "00_config", "local_reference_trails_curated_overrides.csv"
)
PT_LOCAL_REFERENCE_TRAILS_NARRATIVE_PROVENANCE_PATH <- file.path(
  "00_config", "local_reference_trails_narrative_provenance.csv"
)

PT_LOCAL_REFERENCE_TRAILS_HISTORIC_CAUTION <- paste(
  "Mapped National Historic Trail lines may represent corridors, alternatives,",
  "traces, roads, sites, or approximate alignments. They do not imply a",
  "continuous maintained route, a legal boundary, or public access."
)
PT_LOCAL_REFERENCE_TRAILS_SCENIC_CAUTION <- paste(
  "The Pacific Crest Trail is substantially continuous, but mapped alignment",
  "does not guarantee current passability. Closures, hazards, permits, and",
  "land-manager requirements vary; verify current conditions."
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
