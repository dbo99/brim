# ==== bulletin118_data_helpers.r ============================================
##
## PURPOSE:
##   Validate and join the small attribute-only DWR 2019 SGMA basin-priority
##   crosswalk to BRIM's retained Bulletin 118 geometry.
##
## DESIGN:
##   - Join only by basin/subbasin code.
##   - Preserve BRIM row order, geometry, and existing analytical attributes.
##   - Fail unless the current 515-row one-to-one contract is exact.
##   - Keep fixed %BLM bins/colors in one place for later HUC reuse.

PT_BULLETIN118_SGMA_SERVICE <- paste0(
  "https://gis.water.ca.gov/arcgis/rest/services/Geoscientific/",
  "i08_B118_SGMA_2019_Basin_Prioritization/MapServer/2"
)

PT_BULLETIN118_SGMA_SOURCE_PAGE <- paste0(
  "https://gis.water.ca.gov/arcgis/rest/services/Geoscientific/",
  "i08_B118_SGMA_2019_Basin_Prioritization/MapServer"
)

PT_BULLETIN118_EXPECTED_ROWS <- 515L

PT_BULLETIN118_PRIORITY_LEVELS <- c(
  "High",
  "Medium",
  "Low",
  "Very Low"
)

PT_BULLETIN118_PRIORITY_EXPECTED_COUNTS <- c(
  "High" = 46L,
  "Medium" = 48L,
  "Low" = 11L,
  "Very Low" = 410L
)

## DWR MapServer renderer colors, verified from layer 1 metadata.
PT_BULLETIN118_PRIORITY_COLORS <- c(
  "High" = "#FF0000",
  "Medium" = "#FFFF00",
  "Low" = "#55FF00",
  "Very Low" = "#0070FF",
  "No matched value" = "#9E9E9E"
)

PT_BULLETIN118_UNIFORM_FILL <- "#8B5A2B"
PT_BULLETIN118_UNIFORM_FILL_OPACITY <- 0.20
PT_BULLETIN118_BOUNDARY_COLOR <- "#5A381E"

## Fixed absolute bins selected from the read-only Bulletin 118 + HUC2/4/6/8/
## 10/12 distribution audit. The yellow-to-brown sequence is colorblind-
## conscious, gives exact zero a neutral treatment, and stays compatible with
## BRIM's Local brown identity.
PT_BULLETIN118_BLM_BIN_LEVELS <- c(
  "0%",
  ">0\u20131%",
  ">1\u20135%",
  ">5\u201315%",
  ">15\u201330%",
  ">30\u201350%",
  ">50%",
  "Missing"
)

PT_BULLETIN118_BLM_COLORS <- c(
  "0%" = "#F5F5F5",
  ">0\u20131%" = "#FFF7BC",
  ">1\u20135%" = "#FEE391",
  ">5\u201315%" = "#FEC44F",
  ">15\u201330%" = "#FE9929",
  ">30\u201350%" = "#D95F0E",
  ">50%" = "#993404",
  "Missing" = "#9E9E9E"
)

pt_bulletin118_normalize_key <- function(x) {
  trimws(as.character(x))
}

pt_bulletin118_layer_id <- function(subbasin_num) {
  paste0(
    "gw_bull118_",
    pt_bulletin118_normalize_key(subbasin_num)
  )
}

pt_bulletin118_normalize_priority <- function(x) {
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_

  normalized <- c(
    "high" = "High",
    "medium" = "Medium",
    "low" = "Low",
    "very low" = "Very Low"
  )
  hit <- match(tolower(x), names(normalized))
  out <- rep(NA_character_, length(x))
  out[!is.na(hit)] <- unname(normalized[hit[!is.na(hit)]])
  out
}

pt_bulletin118_blm_bin <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  out <- rep("Missing", length(x))

  out[!is.na(x) & x == 0] <- "0%"
  out[!is.na(x) & x > 0 & x <= 1] <- ">0\u20131%"
  out[!is.na(x) & x > 1 & x <= 5] <- ">1\u20135%"
  out[!is.na(x) & x > 5 & x <= 15] <- ">5\u201315%"
  out[!is.na(x) & x > 15 & x <= 30] <- ">15\u201330%"
  out[!is.na(x) & x > 30 & x <= 50] <- ">30\u201350%"
  out[!is.na(x) & x > 50] <- ">50%"

  factor(out, levels = PT_BULLETIN118_BLM_BIN_LEVELS, ordered = TRUE)
}

pt_validate_bulletin118_sgma_crosswalk <- function(x) {
  required <- c(
    "basin_subbasin_number",
    "sgma_2019_priority",
    "source_objectid",
    "source_service",
    "source_accessed"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop(
      "Bulletin 118 SGMA crosswalk is missing field(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(x) != PT_BULLETIN118_EXPECTED_ROWS) {
    stop(
      "Bulletin 118 SGMA crosswalk must contain exactly ",
      PT_BULLETIN118_EXPECTED_ROWS,
      " rows; found ",
      nrow(x),
      ".",
      call. = FALSE
    )
  }

  keys <- pt_bulletin118_normalize_key(x$basin_subbasin_number)
  priorities <- pt_bulletin118_normalize_priority(x$sgma_2019_priority)
  objectids <- suppressWarnings(as.integer(x$source_objectid))
  services <- trimws(as.character(x$source_service))
  accessed <- trimws(as.character(x$source_accessed))

  if (anyNA(keys) || any(keys == "")) {
    stop("Bulletin 118 SGMA crosswalk contains a missing basin code.", call. = FALSE)
  }
  if (anyDuplicated(keys)) {
    duplicates <- unique(keys[duplicated(keys)])
    stop(
      "Bulletin 118 SGMA crosswalk contains duplicate basin code(s): ",
      paste(utils::head(duplicates, 10), collapse = ", "),
      call. = FALSE
    )
  }
  if (anyNA(priorities)) {
    bad <- unique(as.character(x$sgma_2019_priority[is.na(priorities)]))
    stop(
      "Bulletin 118 SGMA crosswalk contains unsupported priority value(s): ",
      paste(utils::head(bad, 10), collapse = ", "),
      call. = FALSE
    )
  }
  if (anyNA(objectids) || anyDuplicated(objectids)) {
    stop(
      "Bulletin 118 SGMA crosswalk source OBJECTIDs must be complete and unique.",
      call. = FALSE
    )
  }
  if (any(services != PT_BULLETIN118_SGMA_SERVICE)) {
    stop("Bulletin 118 SGMA crosswalk has an unexpected source service.", call. = FALSE)
  }
  if (anyNA(accessed) || any(accessed == "") ||
      any(!grepl("^\\d{4}-\\d{2}-\\d{2}$", accessed)) ||
      any(is.na(as.Date(accessed, format = "%Y-%m-%d")))) {
    stop(
      "Bulletin 118 SGMA crosswalk source_accessed must use YYYY-MM-DD.",
      call. = FALSE
    )
  }
  if (!identical(keys, sort(keys, method = "radix"))) {
    stop(
      "Bulletin 118 SGMA crosswalk rows must be deterministically sorted by code.",
      call. = FALSE
    )
  }

  counts <- table(factor(
    priorities,
    levels = PT_BULLETIN118_PRIORITY_LEVELS
  ))
  if (!identical(
    unname(as.integer(counts)),
    unname(as.integer(PT_BULLETIN118_PRIORITY_EXPECTED_COUNTS))
  )) {
    stop(
      "Bulletin 118 SGMA crosswalk priority counts do not match ",
      "46 High / 48 Medium / 11 Low / 410 Very Low. Found: ",
      paste(names(counts), as.integer(counts), collapse = "; "),
      call. = FALSE
    )
  }

  out <- x
  out$basin_subbasin_number <- keys
  out$sgma_2019_priority <- priorities
  out$source_objectid <- objectids
  out$source_service <- services
  out$source_accessed <- accessed
  out
}

pt_read_bulletin118_sgma_crosswalk <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Missing tracked Bulletin 118 SGMA crosswalk: ",
      path,
      "\nRun source('02_preprocess/68_refresh_bulletin118_sgma_2019_priority.R').",
      call. = FALSE
    )
  }

  x <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character(0)
  )
  pt_validate_bulletin118_sgma_crosswalk(x)
}

pt_bulletin118_geometry_summary <- function(x) {
  geometry_type <- as.character(sf::st_geometry_type(x))
  validity <- sf::st_is_valid(x)
  list(
    types = table(geometry_type),
    empty = sum(sf::st_is_empty(x)),
    valid = sum(validity %in% TRUE),
    invalid = sum(validity %in% FALSE),
    validity_na = sum(is.na(validity))
  )
}

pt_enrich_bulletin118_sgma_2019 <- function(gw, crosswalk) {
  if (!inherits(gw, "sf")) {
    stop("Bulletin 118 retained product must be an sf object.", call. = FALSE)
  }
  if (!"subbasin_num" %in% names(gw)) {
    stop("Bulletin 118 retained product is missing subbasin_num.", call. = FALSE)
  }
  if (nrow(gw) != PT_BULLETIN118_EXPECTED_ROWS) {
    stop(
      "Bulletin 118 retained product must contain exactly 515 rows; found ",
      nrow(gw),
      ".",
      call. = FALSE
    )
  }

  crosswalk <- pt_validate_bulletin118_sgma_crosswalk(crosswalk)
  before_names <- names(gw)
  before_attributes <- lapply(
    before_names[before_names != attr(gw, "sf_column")],
    function(nm) gw[[nm]]
  )
  names(before_attributes) <- before_names[
    before_names != attr(gw, "sf_column")
  ]
  before_geometry <- sf::st_geometry(gw)
  before_geometry_summary <- pt_bulletin118_geometry_summary(gw)

  brim_keys <- pt_bulletin118_normalize_key(gw$subbasin_num)
  source_keys <- crosswalk$basin_subbasin_number

  if (anyNA(brim_keys) || any(brim_keys == "")) {
    stop("Bulletin 118 retained product contains a missing subbasin_num.", call. = FALSE)
  }
  if (anyDuplicated(brim_keys)) {
    duplicates <- unique(brim_keys[duplicated(brim_keys)])
    stop(
      "Bulletin 118 retained product contains duplicate subbasin_num value(s): ",
      paste(utils::head(duplicates, 10), collapse = ", "),
      call. = FALSE
    )
  }

  match_index <- match(brim_keys, source_keys)
  brim_unmatched <- brim_keys[is.na(match_index)]
  source_unmatched <- setdiff(source_keys, brim_keys)
  if (length(brim_unmatched) || length(source_unmatched)) {
    stop(
      "Blocking Bulletin 118 SGMA join QA: ",
      length(brim_unmatched),
      " BRIM code(s) unmatched; ",
      length(source_unmatched),
      " DWR code(s) unmatched. BRIM examples: ",
      paste(utils::head(brim_unmatched, 10), collapse = ", "),
      "; DWR examples: ",
      paste(utils::head(source_unmatched, 10), collapse = ", "),
      ". Names were not used for matching.",
      call. = FALSE
    )
  }

  out <- gw
  out$sgma_2019_priority <- crosswalk$sgma_2019_priority[match_index]
  out$sgma_2019_source_objectid <- crosswalk$source_objectid[match_index]

  if (nrow(out) != nrow(gw) ||
      !identical(brim_keys, pt_bulletin118_normalize_key(out$subbasin_num))) {
    stop("Bulletin 118 SGMA join changed row count or order.", call. = FALSE)
  }

  for (nm in names(before_attributes)) {
    if (!identical(out[[nm]], before_attributes[[nm]])) {
      stop(
        "Bulletin 118 SGMA join changed existing field: ",
        nm,
        ".",
        call. = FALSE
      )
    }
  }
  if (!identical(sf::st_geometry(out), before_geometry)) {
    stop("Bulletin 118 SGMA join changed geometry.", call. = FALSE)
  }

  after_geometry_summary <- pt_bulletin118_geometry_summary(out)
  if (!identical(before_geometry_summary, after_geometry_summary)) {
    stop(
      "Bulletin 118 SGMA join changed geometry type/empty/validity counts.",
      call. = FALSE
    )
  }

  priority_counts <- table(factor(
    out$sgma_2019_priority,
    levels = PT_BULLETIN118_PRIORITY_LEVELS
  ))
  if (!identical(
    unname(as.integer(priority_counts)),
    unname(as.integer(PT_BULLETIN118_PRIORITY_EXPECTED_COUNTS))
  )) {
    stop("Bulletin 118 SGMA joined priority counts failed.", call. = FALSE)
  }
  if (anyNA(out$sgma_2019_priority)) {
    stop("Bulletin 118 SGMA join produced missing priority values.", call. = FALSE)
  }

  message(
    "Bulletin 118 SGMA 2019 join QA passed: 515 before / 515 after; ",
    "515 unique BRIM codes; 515 unique DWR codes; 515 exact matches; ",
    "0 unmatched; priorities 46 High / 48 Medium / 11 Low / 410 Very Low; ",
    "geometry/order/existing attributes unchanged."
  )
  out
}
