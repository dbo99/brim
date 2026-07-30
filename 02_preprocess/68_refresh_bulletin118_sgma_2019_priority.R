#!/usr/bin/env Rscript

# ==== 68_refresh_bulletin118_sgma_2019_priority.R ============================
##
## PURPOSE:
##   Refresh the small, tracked, attribute-only crosswalk for DWR's final 2019
##   SGMA basin prioritization.
##
## CONTRACT:
##   - Queries MapServer table 2 only.
##   - Requests OBJECTID, Basin_Subbasin_Number, and Priority only.
##   - Uses returnGeometry=false and rejects any returned geometry.
##   - Requires the exact 515-row, one-code-per-row, 46/48/11/410 snapshot.
##   - Writes deterministic code-sorted CSV; no geometry or raw JSON is kept.

source("00_config/config_paths.r")
source("00_config/config_source_files.r")
source("03_functions/bulletin118_data_helpers.r")

required_packages <- c("httr2", "jsonlite")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

TABLE_URL <- PT_BULLETIN118_SGMA_SERVICE
FEATURE_LAYER_URL <- sub("/2$", "/1", TABLE_URL)
QUERY_URL <- paste0(TABLE_URL, "/query")
OUTPUT_CSV <- SRC$bull118_sgma_2019_priority
REQUIRED_SOURCE_FIELDS <- c(
  "OBJECTID",
  "Basin_Subbasin_Number",
  "Priority"
)

pt_dwr_get_json <- function(url) {
  response <- httr2::request(url) |>
    httr2::req_url_query(f = "json") |>
    httr2::req_user_agent(
      "BRIM Bulletin 118 SGMA 2019 attribute crosswalk refresh"
    ) |>
    httr2::req_timeout(seconds = 60) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()

  data <- jsonlite::fromJSON(
    httr2::resp_body_string(response),
    simplifyVector = TRUE
  )
  if (!is.null(data$error)) {
    stop(
      "DWR ArcGIS response error: ",
      paste(capture.output(str(data$error)), collapse = " "),
      call. = FALSE
    )
  }
  data
}

pt_dwr_query_attributes <- function() {
  response <- httr2::request(QUERY_URL) |>
    httr2::req_method("POST") |>
    httr2::req_body_form(
      where = "1=1",
      outFields = paste(REQUIRED_SOURCE_FIELDS, collapse = ","),
      returnGeometry = "false",
      orderByFields = "Basin_Subbasin_Number ASC",
      f = "json"
    ) |>
    httr2::req_user_agent(
      "BRIM Bulletin 118 SGMA 2019 attribute crosswalk refresh"
    ) |>
    httr2::req_timeout(seconds = 60) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform()

  data <- jsonlite::fromJSON(
    httr2::resp_body_string(response),
    simplifyVector = TRUE
  )
  if (!is.null(data$error)) {
    stop(
      "DWR ArcGIS query error: ",
      paste(capture.output(str(data$error)), collapse = " "),
      call. = FALSE
    )
  }
  data
}

message("Checking DWR table schema: ", TABLE_URL)
metadata <- pt_dwr_get_json(TABLE_URL)
metadata_fields <- as.character(metadata$fields$name)
if (!setequal(metadata_fields, REQUIRED_SOURCE_FIELDS)) {
  stop(
    "DWR table schema changed. Expected exactly: ",
    paste(REQUIRED_SOURCE_FIELDS, collapse = ", "),
    "; found: ",
    paste(metadata_fields, collapse = ", "),
    ".",
    call. = FALSE
  )
}

message("Checking DWR renderer colors: ", FEATURE_LAYER_URL)
feature_metadata <- pt_dwr_get_json(FEATURE_LAYER_URL)
renderer_infos <- feature_metadata$drawingInfo$renderer$uniqueValueInfos
renderer_values <- as.character(renderer_infos$value)
renderer_rgb <- renderer_infos$symbol$color
renderer_hex <- vapply(seq_along(renderer_values), function(i) {
  rgb <- as.integer(renderer_rgb[[i]])[1:3]
  sprintf("#%02X%02X%02X", rgb[[1]], rgb[[2]], rgb[[3]])
}, character(1))
names(renderer_hex) <- renderer_values
expected_renderer <- PT_BULLETIN118_PRIORITY_COLORS[
  PT_BULLETIN118_PRIORITY_LEVELS
]
if (!identical(
  unname(renderer_hex[PT_BULLETIN118_PRIORITY_LEVELS]),
  unname(expected_renderer)
)) {
  stop(
    "DWR SGMA 2019 renderer colors changed. Found: ",
    paste(names(renderer_hex), renderer_hex, collapse = "; "),
    ".",
    call. = FALSE
  )
}

message(
  "Querying DWR attributes only: outFields=",
  paste(REQUIRED_SOURCE_FIELDS, collapse = ","),
  "; returnGeometry=false"
)
query <- pt_dwr_query_attributes()
if (is.null(query$features) || is.null(query$features$attributes)) {
  stop("DWR query did not return a feature-attribute table.", call. = FALSE)
}
if ("geometry" %in% names(query$features) ||
    "geometry" %in% names(query$features$attributes)) {
  stop("DWR query unexpectedly returned geometry.", call. = FALSE)
}

attributes <- query$features$attributes
if (!setequal(names(attributes), REQUIRED_SOURCE_FIELDS)) {
  stop(
    "DWR query returned unexpected attribute schema: ",
    paste(names(attributes), collapse = ", "),
    ".",
    call. = FALSE
  )
}

crosswalk <- data.frame(
  basin_subbasin_number = pt_bulletin118_normalize_key(
    attributes$Basin_Subbasin_Number
  ),
  sgma_2019_priority = pt_bulletin118_normalize_priority(
    attributes$Priority
  ),
  source_objectid = suppressWarnings(as.integer(attributes$OBJECTID)),
  source_service = rep(TABLE_URL, nrow(attributes)),
  source_accessed = rep(format(Sys.Date(), "%Y-%m-%d"), nrow(attributes)),
  stringsAsFactors = FALSE
)
crosswalk <- crosswalk[
  order(crosswalk$basin_subbasin_number, method = "radix"),
  ,
  drop = FALSE
]
row.names(crosswalk) <- NULL
crosswalk <- pt_validate_bulletin118_sgma_crosswalk(crosswalk)

utils::write.csv(
  crosswalk,
  file = OUTPUT_CSV,
  row.names = FALSE,
  na = "",
  quote = TRUE,
  fileEncoding = "UTF-8"
)

message("Saved deterministic attribute-only crosswalk: ", OUTPUT_CSV)
message(
  "QA passed: 515 rows; 515 unique codes; 0 duplicates; ",
  "46 High / 48 Medium / 11 Low / 410 Very Low; no geometry."
)
