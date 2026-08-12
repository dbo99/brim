#!/usr/bin/env Rscript

read_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

assert_contains <- function(text, needle, label = needle) {
  if (!grepl(needle, text, fixed = TRUE)) {
    stop("Missing expected generalization contract: ", label, call. = FALSE)
  }
}

inventory_path <-
  "08_docs/features/local_geometry_generalization_inventory.csv"
inventory <- read.csv(
  inventory_path,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = character()
)

expected_generalized <- c(
  "blm_field_office_boundaries",
  "bulletin118",
  "cnrfc_product_availability",
  "cnrfc_fnf_delta",
  paste0("huc", c(2, 4, 6, 8, 10, 12)),
  "wsr_blm_lines",
  "wsr_segments",
  "wsr_corridor_blm",
  "wsr_corridor_lsrs_area",
  "wsr_corridor_lsrs_status",
  "national_monuments",
  "ca_desert_ncl",
  "federal_wilderness",
  "acec",
  "counties",
  "water_districts",
  "nps_national_parks_context",
  "nps_national_preserve_context"
)

stopifnot(
  nrow(inventory) == 34L,
  !anyDuplicated(inventory$layer_id),
  sum(inventory$generalized_by_brim) == 23L,
  sum(!inventory$generalized_by_brim) == 11L,
  setequal(
    inventory$layer_id[inventory$generalized_by_brim],
    expected_generalized
  ),
  all(inventory$status == "PASS"),
  !any(grepl("REVIEW", inventory$status, fixed = TRUE)),
  all(nzchar(inventory$code_source))
)

expected_parameters <- c(
  blm_field_office_boundaries = "keep=0.20",
  bulletin118 = "keep=0.05",
  cnrfc_product_availability = "keep=0.12",
  cnrfc_fnf_delta = "keep=0.80",
  huc2 = "keep=0.03",
  huc4 = "keep=0.03",
  huc6 = "keep=0.03",
  huc8 = "keep=0.03",
  huc10 = "keep=0.03",
  huc12 = "keep=0.03",
  wsr_blm_lines = "keep=0.06",
  wsr_segments = "keep=0.06",
  wsr_corridor_blm = "keep=0.06",
  wsr_corridor_lsrs_area = "keep=0.06",
  wsr_corridor_lsrs_status = "keep=0.06",
  national_monuments = "dTolerance=1 m",
  ca_desert_ncl = "dTolerance=2 m",
  federal_wilderness = "keep=0.50",
  acec = "dTolerance=1 m",
  counties = "keep=0.05",
  water_districts = "keep=0.12",
  nps_national_parks_context =
    "boundary dTolerance=2 m; land/interest dTolerance=5 m",
  nps_national_preserve_context =
    "boundary dTolerance=2 m; land/interest dTolerance=5 m"
)
parameter_index <- match(names(expected_parameters), inventory$layer_id)
stopifnot(
  !anyNA(parameter_index),
  identical(
    inventory$active_parameter[parameter_index],
    unname(expected_parameters)
  )
)

generalized_rows <- inventory$generalized_by_brim
fnf_row <- inventory$layer_id == "cnrfc_fnf_delta"
public_disclosure <-
  "Generalized display geometry. Check authoritative source for boundary-sensitive use."
stopifnot(
  all(nzchar(inventory$public_disclosure[generalized_rows])),
  grepl("no existing natural legend/card", inventory$public_disclosure[fnf_row]),
  all(
    inventory$public_disclosure[generalized_rows & !fnf_row] ==
      public_disclosure
  ),
  all(grepl(
    "Not applicable",
    inventory$public_disclosure[!generalized_rows],
    fixed = TRUE
  )),
  !any(grepl(
    "ms_simplify|st_simplify|dTolerance|simplify_keep",
    inventory$public_disclosure,
    perl = TRUE
  ))
)

source("00_config/config_local_reference_interactions.r")
source("03_functions/local_reference_interaction_helpers.r")
pt_validate_local_reference_config()

expected_registry_disclosures <- stats::setNames(
  rep(public_disclosure, 6),
  c(
    "national_monuments", "ca_desert_ncl", "federal_wilderness",
    "acec", "counties", "water_districts"
  )
)
registry_index <- match(
  names(expected_registry_disclosures),
  LOCAL_REFERENCE_INTERACTION_REGISTRY$layer_id
)
stopifnot(
  length(LOCAL_REFERENCE_INTERACTION_REGISTRY$generalization_disclosure) ==
    nrow(LOCAL_REFERENCE_INTERACTION_REGISTRY),
  identical(
    LOCAL_REFERENCE_INTERACTION_REGISTRY$generalization_disclosure[registry_index],
    unname(expected_registry_disclosures)
  ),
  identical(
    LOCAL_REFERENCE_INTERACTION_REGISTRY$layer_id[
      nzchar(LOCAL_REFERENCE_INTERACTION_REGISTRY$generalization_disclosure)
    ],
    names(expected_registry_disclosures)
  )
)

core_config <- read_text("05_map_build/02_build_core_map_cache.r")
core_polygons <-
  read_text("05_map_build/02_cache_blocks/01_prepare_core_polygons.r")
admin_water <- read_text(
  "05_map_build/02_cache_blocks/04_cache_admin_water_reference_layers.r"
)
reference_cache <-
  read_text("05_map_build/02_cache_blocks/05_cache_final_point_tweaks.r")
spatial_helpers <- read_text("03_functions/spatial_helpers.r")
local_helpers <- read_text("03_functions/leaflet_layer_local_reference_helpers.r")
local_payload <- read_text("03_functions/local_reference_interaction_helpers.r")
local_controller <-
  read_text("03_functions/js/brim_local_reference_controller.js")
huc_controller <- read_text("03_functions/js/brim_huc_theme_control.js")
bulletin_controller <-
  read_text("03_functions/js/brim_bulletin118_theme_control.js")

for (needle in c(
  "huc2 = 0.03", "huc4 = 0.03", "huc6 = 0.03", "huc8 = 0.03",
  "huc10 = 0.03", "huc12 = 0.03", "gw = 0.05", "county = 0.05",
  "cnrfc_fnf_delta = 0.80", "water_districts = 0.12"
)) {
  assert_contains(core_config, needle)
}
assert_contains(spatial_helpers, "keep_shapes = TRUE")
assert_contains(spatial_helpers, "explode = FALSE")
assert_contains(spatial_helpers, "keep >= 0.999")
assert_contains(core_polygons, 'keep = keep_for("gw")')
assert_contains(core_polygons, 'keep = keep_for("county")')
assert_contains(admin_water, "keep = 0.20")
assert_contains(admin_water, 'keep = keep_for("cnrfc_fnf_delta")')
assert_contains(admin_water, 'keep = keep_for("water_districts")')
assert_contains(reference_cache, "simplify_sf_for_web(")

manifest <- read.csv(
  "00_config/reference_layers_manifest.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
manifest_expected <- c(
  trails = 1,
  wildernessstudyarea = 1,
  drecp = 1,
  allotments = 1,
  gsps = 1,
  gwbasins_adjd = 1,
  wsr_blm_lines = 0.06,
  wsr_segments = 0.06,
  wsr_corridor_blm = 0.06,
  wsr_corridor_lsrs_area = 0.06,
  wsr_corridor_lsrs_status = 0.06
)
manifest_index <- match(names(manifest_expected), manifest$nickname)
stopifnot(
  !anyNA(manifest_index),
  identical(manifest$simplify_keep[manifest_index], unname(manifest_expected))
)

fixed_contracts <- list(
  "02_preprocess/70_national_monuments_pipeline/build_national_monuments_candidate.R" =
    "NM_SELECTED_SIMPLIFY_TOLERANCE_M <- 1",
  "02_preprocess/71_desert_ncl_pipeline/build_desert_ncl_candidate.R" =
    "CDNCL_SELECTED_SIMPLIFY_TOLERANCE_M <- 2",
  "02_preprocess/69_acec_pipeline/build_acec_current_candidate.R" =
    "simplify_tolerance_m = 1",
  "02_preprocess/68_federal_wilderness_pipeline/build_federal_wilderness_197_candidate.R" =
    "simplify_keep = 0.5",
  "02_preprocess/70_national_monuments_pipeline/build_nps_park_preserve_context.R" =
    "selected_boundary_simplify_tolerance_m"
)
for (path in names(fixed_contracts)) {
  assert_contains(read_text(path), fixed_contracts[[path]], path)
}
nps_source <- read_text(
  "02_preprocess/70_national_monuments_pipeline/build_nps_park_preserve_context.R"
)
assert_contains(nps_source, 'if (role == "legislative_boundary")')
assert_contains(nps_source, "NPS_CONTEXT_SELECTED_BOUNDARY_TOLERANCE_M <- 2")
assert_contains(nps_source, "NPS_CONTEXT_SELECTED_LAND_INTEREST_TOLERANCE_M <- 5")

assert_contains(
  local_payload,
  "generalization_disclosure = row$generalization_disclosure"
)
assert_contains(local_controller, "pt-lr-generalization-disclosure")
assert_contains(local_controller, "Generalized display geometry. ")
assert_contains(local_controller, "Check authoritative source for boundary-sensitive use.</div>")
assert_contains(local_helpers, "pt-blm-office-generalization-note")
assert_contains(local_helpers, "pt-cnrfc-basin-generalization-note")
assert_contains(local_helpers, public_disclosure)
assert_contains(huc_controller, "pt-huc-theme-generalization-note")
assert_contains(bulletin_controller, "pt-bulletin118-generalization-note")
public_ui_text <- paste(
  read_text("00_config/config_local_reference_interactions.r"),
  local_helpers,
  local_controller,
  huc_controller,
  bulletin_controller
)
stopifnot(
  !grepl(
    "vertex retention|[0-9]+ m tolerance\\.",
    public_ui_text,
    perl = TRUE
  )
)

message(
  "Local geometry generalization inventory/disclosure tests passed: ",
  nrow(inventory), " audited; ", sum(generalized_rows), " generalized; ",
  sum(!generalized_rows), " not generalized."
)
