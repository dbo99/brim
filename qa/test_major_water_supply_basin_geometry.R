# ==== test_major_water_supply_basin_geometry.R ==============================
# Focused configuration and runtime QA for Phase B2 major water-supply basin
# geometry. Set BRIM_MAJOR_BASIN_SOURCE_ONLY=true in the lean source repo.
# =============================================================================

suppressPackageStartupMessages(library(readr))
source(file.path("00_config", "config_paths.r"))

SOURCE_ONLY <- identical(
  tolower(Sys.getenv("BRIM_MAJOR_BASIN_SOURCE_ONLY", unset = "false")),
  "true"
)
CA_IDS <- c(
  "SCSC1_FNF", "TMDC1_FNF", "SHDC1_FNF", "CEGC1_FNF", "ORDC1_FNF",
  "HLEC1_FNF", "FOLC1_FNF", "NDPC1_FNF", "EXQC1_FNF", "FRAC1_FNF",
  "PFTC1_FNF", "ISAC1_FNF", "MHBC1_FNF", "CMPC1_FNF", "NMSC1_FNF",
  "BDBC1_FNF", "SACC0_FNF", "VNSC0_FNF", "MLIC0_FNF"
)
CBRFC_IDS <- c(
  "GLDA3_CBRFC_MODELED_UPSTREAM",
  "LKSA3_CBRFC_LOCAL_INTERVENING"
)
CONTEXT_IDS <- c(
  "HUC2_14_UPPER_COLORADO_CONTEXT",
  "HUC2_15_LOWER_COLORADO_CONTEXT"
)
ALL_IDS <- c(CA_IDS, CBRFC_IDS, CONTEXT_IDS)
CBRFC_KEYS <- c(
  "CBRFC:GLDA3:APR_JUL_WSUP",
  "CBRFC:GLDA3:WATER_YEAR_INFLOW",
  "CBRFC:LKSA3:LOCAL_INTERVENING_MONTHLY"
)

qa_assert <- function(condition, message_text) {
  if (!isTRUE(condition)) stop("QA FAILED: ", message_text, call. = FALSE)
  invisible(TRUE)
}
read_config <- function(name) {
  path <- file.path(DIR$config, name)
  qa_assert(file.exists(path), paste0("Missing config: ", path))
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = character()
  )
}
read_csv_path <- function(path) {
  qa_assert(file.exists(path), paste0("Missing CSV: ", path))
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = character()
  )
}
assert_unique <- function(x, fields, label) {
  key <- do.call(paste, c(x[fields], sep = "\r"))
  qa_assert(!anyDuplicated(key), paste0(label, " has duplicate ", paste(fields, collapse = "+"), "."))
}
reviewed_logical <- function(x, label) {
  value <- toupper(trimws(as.character(x)))
  qa_assert(all(value %in% c("TRUE", "FALSE")), paste0(label, " is not explicit TRUE/FALSE."))
  value == "TRUE"
}
sha256_file <- function(path) {
  digest::digest(path, algo = "sha256", serialize = FALSE, file = TRUE)
}
geometry_sha256 <- function(x) {
  raw <- sf::st_as_binary(sf::st_geometry(x), EWKB = TRUE)[[1]]
  digest::digest(raw, algo = "sha256", serialize = FALSE)
}

catalog <- read_config("major_water_supply_basin_geometry_catalog.csv")
components <- read_config("major_water_supply_basin_component_manifest.csv")
selectors <- read_config("major_water_supply_basin_cbrfc_selector_manifest.csv")
sources <- read_config("major_water_supply_basin_source_manifest.csv")
mapping <- read_config("major_water_supply_basin_product_mapping.csv")
crosswalk <- read_config("major_water_supply_basin_reservoir_crosswalk.csv")
links <- read_config("major_water_supply_basin_related_links.csv")
phase_b1_hashes <- read_config("major_water_supply_basin_phase_b1_geometry_hashes.csv")

catalog$display_order <- suppressWarnings(as.integer(catalog$display_order))
catalog$display_simplify_keep <- suppressWarnings(as.numeric(catalog$display_simplify_keep))
catalog$include_in_display <- reviewed_logical(catalog$include_in_display, "Catalog display flag")
mapping$display_order <- suppressWarnings(as.integer(mapping$display_order))
crosswalk$display_order <- suppressWarnings(as.integer(crosswalk$display_order))
links$display_order <- suppressWarnings(as.integer(links$display_order))
links$required <- reviewed_logical(links$required, "Related-link required flag")
selectors$selector_order <- suppressWarnings(as.integer(selectors$selector_order))
selectors$expected_match_count <- suppressWarnings(as.integer(selectors$expected_match_count))

qa_assert(nrow(catalog) == 23L, "Catalog must contain 23 retained objects.")
qa_assert(identical(catalog$geometry_id, ALL_IDS), "Catalog additive order or IDs changed.")
assert_unique(catalog, "geometry_id", "Catalog")
assert_unique(catalog, "display_order", "Catalog")
qa_assert(
  sum(catalog$geometry_role == "context_only") == 2L &&
    sum(catalog$geometry_role != "context_only") == 21L,
  "Catalog must contain 21 forecast/internal roles and two context roles."
)
context <- catalog$geometry_id %in% CONTEXT_IDS
qa_assert(
  all(catalog$forecast_key[context] == "") &&
    all(catalog$product_type[context] == "") &&
    all(catalog$rfc[context] == "") &&
    all(catalog$nws_lid[context] == "") &&
    all(catalog$display_simplify_keep[context] == 0.01),
  "HUC2 context rows must have no forecast authority and keep=0.01."
)
qa_assert(
  catalog$reservoir_name[catalog$geometry_id == "MHBC1_FNF"] == "" &&
    all(catalog$reservoir_name[catalog$geometry_id %in% c(
      "BDBC1_FNF", "SACC0_FNF", "VNSC0_FNF", "MLIC0_FNF"
    )] == ""),
  "Michigan Bar and index/internal geometries must not imply reservoirs."
)
qa_assert(
  all(catalog$display_simplify_keep[catalog$geometry_id %in% CBRFC_IDS] == 0.05),
  "Both CBRFC operational geometries must use keep=0.05."
)

qa_assert(nrow(components) == 18L, "California component manifest must retain 18 rows.")
assert_unique(components, c("component_manifest_id", "component_order"), "Component manifest")
bdbc <- components[components$derived_geometry_id == "BDBC1_FNF", ]
qa_assert(
  nrow(bdbc) == 8L && all(c("KWKC1_MODEL", "RDGC1_MODEL") %in% bdbc$component_geometry_id),
  "BDBC1 must retain all eight components, including KWKC1 and RDGC1."
)

qa_assert(
  nrow(selectors) == 5L &&
    identical(selectors$expected_match_count, c(203L, 10L, 26L, 22L, 21L)) &&
    identical(selectors$selection_value, c("UC", "MEAD", "LITCOL", "VIRGIN", "MUDLV")),
  "CBRFC selectors/counts differ from the reviewed recipe."
)
qa_assert(nrow(sources) == 8L, "Source manifest must contain eight files.")
assert_unique(sources, "source_id", "Source manifest")
qa_assert(
  setequal(sources$source_id, c(
    "CBRFC_BASINS_SHP", "CBRFC_BASINS_SHX", "CBRFC_BASINS_DBF",
    "CBRFC_BASINS_PRJ", "CBRFC_BASINS_QPJ", "CBRFC_OUTLETS_ZIP",
    "WBD_HUC2_14_ZIP", "WBD_HUC2_15_ZIP"
  )),
  "Source manifest has an unexpected file family."
)

qa_assert(nrow(mapping) == 54L, "Product mapping must contain exactly 54 rows.")
assert_unique(mapping, "forecast_key", "Product mapping")
assert_unique(mapping, "display_order", "Product mapping")
qa_assert(
  identical(mapping$forecast_key[52:54], CBRFC_KEYS) &&
    sum(mapping$source_family == "CNRFC") == 51L &&
    sum(mapping$source_family == "CBRFC") == 3L,
  "Mapping must preserve 51 literal CNRFC keys followed by the exact CBRFC roster."
)
qa_assert(
  all(mapping$geometry_id %in% catalog$geometry_id) &&
    !any(mapping$geometry_id %in% c(CONTEXT_IDS, "BDBC1_FNF")) &&
    length(unique(mapping$geometry_id)) == 20L,
  "Mappings must resolve to exactly 20 forecast-capable, non-BDBC geometries."
)
qa_assert(
  all(mapping$geometry_id[mapping$forecast_key %in% CBRFC_KEYS[1:2]] == CBRFC_IDS[[1]]) &&
    mapping$geometry_id[mapping$forecast_key == CBRFC_KEYS[[3]]] == CBRFC_IDS[[2]],
  "CBRFC product-to-geometry mappings are wrong."
)
qa_assert(
  all(mapping$source_link_roles[mapping$source_family == "CBRFC"] ==
    "source_url|retrieval_url|summary_url|archive_url") &&
    grepl("ordered monthly array", mapping$decision_note[mapping$forecast_key == CBRFC_KEYS[[3]]]),
  "CBRFC source-link or Lake Mead monthly-array mapping contract changed."
)
qa_assert(
  all(!grepl("\\{|\\}|paste|sub\\(", mapping$forecast_key)) &&
    all(!grepl("\\{|\\}|paste|sub\\(", mapping$geometry_id)),
  "Browser-oriented mapping contains a derived/template key expression."
)

expected_crosswalk <- c(
  SCSC1_FNF = "SCC", TMDC1_FNF = "TRM", SHDC1_FNF = "SHA",
  CEGC1_FNF = "CLE", ORDC1_FNF = "ORO", HLEC1_FNF = "ENG",
  FOLC1_FNF = "FOL", NDPC1_FNF = "DNP", EXQC1_FNF = "EXC",
  FRAC1_FNF = "MIL", PFTC1_FNF = "PNF", ISAC1_FNF = "ISB",
  CMPC1_FNF = "PAR", NMSC1_FNF = "NML"
)
qa_assert(nrow(crosswalk) == 14L, "Reservoir crosswalk must contain 14 rows.")
assert_unique(crosswalk, "geometry_id", "Reservoir crosswalk")
assert_unique(crosswalk, "reservoir_record_key", "Reservoir crosswalk")
qa_assert(
  identical(setNames(crosswalk$reservoir_record_key, crosswalk$geometry_id), expected_crosswalk) &&
    !"MHBC1_FNF" %in% crosswalk$geometry_id,
  "Reservoir crosswalk differs from the reviewed 14 associations."
)

assert_unique(links, "link_id", "Related-link registry")
assert_unique(links, "display_order", "Related-link registry")
one_identity <- xor(nzchar(links$geometry_id), nzchar(links$geometry_scope))
qa_assert(all(one_identity), "Each supporting link needs exactly one geometry identity/scope.")
qa_assert(
  all(grepl("^https://", links$url)) &&
    !any(grepl("localhost|127\\.0\\.0\\.1|/Users/|file:", links$url)),
  "Related-link URLs must be valid HTTPS and non-local."
)
qa_assert(
  length(intersect(links$url, catalog$source_url[nzchar(catalog$source_url)])) == 0L,
  "Producer forecast URL was duplicated into supporting links."
)
qa_assert(
  all(c("Bureau of Reclamation", "California Department of Water Resources", "U.S. Army Corps of Engineers") %in% links$source_agency),
  "Related-link agency families are incomplete."
)

if (SOURCE_ONLY) {
  message(
    "Major-basin Phase B2 source-only QA passed: 23 geometry records, 54 literal ",
    "product mappings, five CBRFC selectors, 14 reservoir associations, and ",
    "reviewed supporting-link separation."
  )
} else {
  qa_assert(
    requireNamespace("sf", quietly = TRUE) && requireNamespace("digest", quietly = TRUE),
    "Runtime QA requires sf and digest."
  )
  paths <- list(
    baseline = file.path(DIR$rds, "major_water_supply_basin_geometry_3310.rds"),
    display = file.path(DIR$cache_last, "major_water_supply_basin_geometry_map.rds"),
    geometry = file.path(DIR$qa, "major_water_supply_basin_geometry_qa_latest.csv"),
    resolution = file.path(DIR$qa, "major_water_supply_basin_component_resolution_latest.csv"),
    provenance = file.path(DIR$qa, "major_water_supply_basin_input_provenance_latest.csv"),
    inventory = file.path(DIR$qa, "major_water_supply_basin_geometry_inventory_latest.csv"),
    mapping = file.path(DIR$qa, "major_water_supply_basin_product_mapping_audit_latest.csv"),
    selector = file.path(DIR$qa, "major_water_supply_basin_cbrfc_selector_audit_latest.csv"),
    hashes = file.path(DIR$qa, "major_water_supply_basin_per_feature_hash_audit_latest.csv"),
    holes = file.path(DIR$qa, "major_water_supply_basin_hole_part_audit_latest.csv"),
    outlets = file.path(DIR$qa, "major_water_supply_basin_outlet_proximity_audit_latest.csv"),
    huc = file.path(DIR$qa, "major_water_supply_basin_huc2_simplification_benchmark_latest.csv"),
    future = file.path(DIR$qa, "major_water_supply_basin_future_total_mead_candidate_latest.csv")
  )
  qa_assert(all(file.exists(unlist(paths))), "A required Phase B2 output/QA artifact is missing.")
  baseline <- readRDS(paths$baseline)
  display <- readRDS(paths$display)
  qa_assert(inherits(baseline, "sf") && inherits(display, "sf"), "Geometry outputs are not sf.")
  qa_assert(nrow(baseline) == 23L && nrow(display) == 23L, "Geometry outputs need 23 rows.")
  qa_assert(
    identical(as.character(baseline$geometry_id), ALL_IDS) &&
      identical(as.character(display$geometry_id), ALL_IDS),
    "Existing California order or additive Colorado order changed."
  )
  qa_assert(
    identical(as.integer(sf::st_crs(baseline)$epsg), 3310L) &&
      identical(as.integer(sf::st_crs(display)$epsg), 4326L) &&
      all(sf::st_is_valid(baseline)) && all(sf::st_is_valid(display)) &&
      !any(sf::st_is_empty(baseline)) && !any(sf::st_is_empty(display)),
    "Output CRS, validity, or non-empty contract failed."
  )

  hashes <- read_csv_path(paths$hashes)
  b1_current <- hashes[match(phase_b1_hashes$geometry_id, hashes$geometry_id), ]
  qa_assert(
    identical(b1_current$baseline_sha256, phase_b1_hashes$baseline_sha256) &&
      identical(b1_current$display_sha256, phase_b1_hashes$display_sha256),
    "One or more Phase B1 per-feature hashes changed."
  )
  for (index in seq_len(nrow(baseline))) {
    qa_assert(
      identical(geometry_sha256(baseline[index, ]), hashes$baseline_sha256[[index]]) &&
        identical(geometry_sha256(display[index, ]), hashes$display_sha256[[index]]),
      paste0("Per-feature hash audit does not match output for ", baseline$geometry_id[[index]], ".")
    )
  }

  geometry <- read_csv_path(paths$geometry)
  cbrfc_geometry <- geometry[geometry$geometry_id %in% CBRFC_IDS, ]
  qa_assert(
    all(abs(as.numeric(cbrfc_geometry$simplification_area_change_pct)) <= 0.05) &&
      all(as.numeric(cbrfc_geometry$simplification_keep) == 0.05) &&
      all(as.integer(cbrfc_geometry$source_processing_crs) == 5070L),
    "CBRFC simplification/processing guardrail failed."
  )
  holes <- read_csv_path(paths$holes)
  local_hole <- holes[holes$geometry_id == CBRFC_IDS[[2]], ]
  qa_assert(
    as.integer(local_hole$source_hole_count) == 1L &&
      as.integer(local_hole$display_hole_count) == 0L &&
      abs(as.numeric(local_hole$approved_source_hole_area_sq_mi) - 1.486) <= 0.02 &&
      toupper(local_hole$display_hole_exception_applied) == "TRUE",
    "Local source-gap preservation/display-only exception failed."
  )
  selector_audit <- read_csv_path(paths$selector)
  qa_assert(
    identical(as.integer(selector_audit$actual_match_count), c(203L, 10L, 26L, 22L, 21L)),
    "Runtime CBRFC selector counts changed."
  )
  outlet_audit <- read_csv_path(paths$outlets)
  qa_assert(
    nrow(outlet_audit) == 2L &&
      all(as.numeric(outlet_audit$distance_to_union_meters) <= 250) &&
      all(toupper(outlet_audit$arbitrary_buffer_applied) == "FALSE"),
    "Outlet proximity or no-buffer contract failed."
  )
  huc <- read_csv_path(paths$huc)
  qa_assert(
    nrow(huc) == 8L && setequal(as.numeric(huc$simplify_keep), c(0.005, 0.01, 0.02, 0.05)) &&
      all(toupper(huc$display_valid) == "TRUE"),
    "HUC2 benchmark candidates are incomplete or invalid."
  )
  future <- read_csv_path(paths$future)
  qa_assert(
    as.integer(future$basin_count) == 282L &&
      toupper(future$retained_geometry) == "FALSE" &&
      toupper(future$forecast_value_attached) == "FALSE",
    "Deferred total-Mead QA-only candidate contract failed."
  )

  resolution <- read_csv_path(paths$resolution)
  qa_assert(nrow(resolution) == 18L && all(as.integer(resolution$match_count) == 1L), "California source selectors changed.")
  provenance <- read_csv_path(paths$provenance)
  for (index in seq_len(nrow(provenance))) {
    qa_assert(file.exists(provenance$input_path[[index]]), paste0("Missing recorded input: ", provenance$input_path[[index]]))
    qa_assert(identical(sha256_file(provenance$input_path[[index]]), provenance$sha256[[index]]), paste0("Recorded input changed: ", provenance$input_path[[index]]))
  }
  qa_assert("cnrfc_fnf_delta_map" %in% provenance$input_name, "Old Local 15-basin map cache hash is not retained in provenance.")
  prohibited <- c("forecast_value", "forecast_volume", "forecast_percent", "forecast_sum", "forecast_average")
  qa_assert(!any(names(baseline) %in% prohibited) && !any(names(display) %in% prohibited), "Geometry output contains forecast arithmetic.")
  map_dir <- file.path(DIR$qa, "major_water_supply_basin_geometry_maps")
  expected_maps <- file.path(map_dir, c(
    "glda3_cbrfc_modeled_upstream_comparison.png",
    "lksa3_local_intervening_comparison.png",
    "huc2_14_15_simplification_comparison.png",
    "combined_california_colorado_forecast_geometry.png",
    "combined_forecast_plus_huc2_context.png"
  ))
  qa_assert(all(file.exists(expected_maps)) && all(file.info(expected_maps)$size > 1000), "Rendered QA maps are missing or empty.")

  message(
    "Major-basin Phase B2 runtime QA passed: 23 valid geometries, 54 mappings, ",
    "all 19 Phase B1 feature hashes preserved, exact CBRFC selectors, explicit ",
    "local display-gap exception, HUC2 benchmark, outlet proximity, and stable inputs."
  )
}
