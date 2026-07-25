#!/usr/bin/env Rscript

## Source-only fixture for the Springs compact payload/location preparation.
## This does not claim browser timing; it verifies record and coordinate
## integrity before the browser controller is involved.

source("03_functions/leaflet_layer_local_well_spring_helpers.r")

fixture <- data.frame(
  spring_id = c("spring_1", "spring_2", "spring_3", "spring_4", "spring_5"),
  pt_lat = c(35, 35, 35.00000001, 36, 37),
  pt_lng = c(-120, -120, -120, -119, -118),
  spring_name_display = c(
    "Alpha Spring",
    "Alternate Alpha",
    "Near Alpha",
    "Survey Spring",
    "Far Spring"
  ),
  spring_source_key = c(
    "nhd",
    "nhd",
    "nhd",
    "survey_2015_16",
    "nhd"
  ),
  spring_source_display = c(
    "NHD",
    "NHD",
    "NHD",
    "2015–16 Mojave survey",
    "NHD"
  ),
  on_blm_ca = c(TRUE, TRUE, FALSE, FALSE, FALSE),
  dist_to_blm_mi = c(0, 0, 2, 4, 9),
  google_search_url = paste0("https://example.test/search/", seq_len(5)),
  source_report_url = c(NA, NA, NA, "https://example.test/report", NA),
  stringsAsFactors = FALSE
)

payload <- pt_prepare_springs_virtualized_payload(fixture)
meta <- payload$metadata

stopifnot(
  meta$analyticalRecordCount == nrow(fixture),
  meta$validCoordinateCount == nrow(fixture),
  meta$recordCount == nrow(fixture),
  meta$uniqueCoordinateCount == 4L,
  meta$duplicateLocationCount == 1L,
  meta$recordsAtDuplicateLocations == 2L,
  meta$maxRecordsAtLocation == 2L,
  sum(payload$locations$record_count) == nrow(fixture),
  identical(sort(payload$records$spring_id), sort(fixture$spring_id)),
  !anyDuplicated(payload$records$spring_id),
  setequal(
    names(payload$records),
    names(fixture)
  ),
  is.numeric(meta$approximatePayloadBytes),
  meta$approximatePayloadBytes > 0
)

duplicate_location <- payload$locations[
  payload$locations$record_count > 1L,
  ,
  drop = FALSE
]
duplicate_start <- duplicate_location$record_start[[1]] + 1L
duplicate_end <- duplicate_start + duplicate_location$record_count[[1]] - 1L
duplicate_ids <- payload$records$spring_id[duplicate_start:duplicate_end]

stopifnot(setequal(duplicate_ids, c("spring_1", "spring_2")))

invalid_fixture <- fixture
invalid_fixture$pt_lat[[1]] <- NA_real_
invalid_result <- try(
  pt_prepare_springs_virtualized_payload(invalid_fixture),
  silent = TRUE
)
stopifnot(inherits(invalid_result, "try-error"))

duplicate_id_fixture <- fixture
duplicate_id_fixture$spring_id[[2]] <- duplicate_id_fixture$spring_id[[1]]
duplicate_id_result <- try(
  pt_prepare_springs_virtualized_payload(duplicate_id_fixture),
  silent = TRUE
)
stopifnot(inherits(duplicate_id_result, "try-error"))

cat("Springs virtualization R preparation fixture: OK\n")
