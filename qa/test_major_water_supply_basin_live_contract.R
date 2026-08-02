# Separate network smoke test; controlled/unit QA does not source this file.

urls <- c(
  CNRFC = "https://dbo99.github.io/brim-live-data-feeds/data/major_water_supply_basin_forecasts.json",
  CBRFC = "https://dbo99.github.io/brim-live-data-feeds/data/cbrfc_major_water_supply_forecasts.json"
)
payloads <- lapply(urls, function(url) jsonlite::fromJSON(url, simplifyVector = FALSE))
cnrfc <- payloads$CNRFC
cbrfc <- payloads$CBRFC

stopifnot(
  identical(cnrfc$schema_version, "1.0"),
  identical(cnrfc$product_id, "major_water_supply_basin_forecasts"),
  identical(cnrfc$roster_version, "cnrfc-major-water-supply-v1.1.0"),
  cnrfc$expected_record_count == 51L,
  cnrfc$actual_record_count == 51L,
  length(cnrfc$records) == 51L,
  cnrfc$publication_mode %in% c("bootstrap", "steady_state"),
  identical(cbrfc$schema_version, "1.0"),
  identical(cbrfc$product_id, "cbrfc_major_water_supply_forecasts"),
  identical(cbrfc$roster_version, "cbrfc-colorado-river-v1.3.0"),
  cbrfc$expected_record_count == 3L,
  cbrfc$actual_record_count == 3L,
  identical(vapply(cbrfc$records, `[[`, character(1), "forecast_key"), c(
    "CBRFC:GLDA3:APR_JUL_WSUP",
    "CBRFC:GLDA3:WATER_YEAR_INFLOW",
    "CBRFC:LKSA3:LOCAL_INTERVENING_MONTHLY"
  )),
  cbrfc$publication_mode %in% c("bootstrap", "steady_state")
)

message("Major water-supply basin canonical live-contract smoke test passed.")
