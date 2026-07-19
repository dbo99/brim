# ==== 52_build_cnrfc_basin_product_availability.R ===========================
##
## PURPOSE:
##   Build stable preprocessed CNRFC basin product-availability products.
##
## DESIGN NOTE:
##   This preprocessor currently reuses the tested QA engines for:
##     - CNRFC Water Resources Update XML product extraction
##     - CNRFC basin product-availability checks/cache
##
##   That keeps this durable step aligned with the audit work while the product
##   logic is still being validated.  Once the behavior is fully settled, the
##   QA engine can be refactored into shared functions or moved fully here.
##
## DEFAULTS:
##   Full basin/FNF pool, high-value product mode, Weather.gov checks off,
##   WRU XML products on, resume-from-cache on.
##
## OUTPUTS:
##   04_processed_data/rds/cnrfc_basin_product_availability_long.rds
##   04_processed_data/rds/cnrfc_basin_product_availability_matrix.rds
##   04_processed_data/rds/cnrfc_basin_product_availability_bins.rds
##   04_processed_data/rds/cnrfc_basin_water_resources_update_matches.rds
##   04_processed_data/rds/cnrfc_basin_product_availability_summary.rds
##   04_processed_data/rds/cnrfc_basin_product_availability_check_cache.rds
##   04_processed_data/rds/cnrfc_basin_product_availability_manifest.rds
##   04_processed_data/rds/cnrfc_basin_product_availability_map.rds
##   04_processed_data/rds/cnrfc_basin_product_availability_map_bins.rds
##   04_processed_data/rds/cnrfc_basin_product_availability_forecast_group_bins.rds
##   04_processed_data/rds/cnrfc_basin_product_availability_map_summary.rds

# ==== 1. Project and package setup ==========================================

if (!dir.exists("00_config") || !dir.exists("04_processed_data")) {
  stop(
    "Run this script from the BRIM project root. Expected folders like ",
    "00_config/ and 04_processed_data/ were not found."
  )
}

source("00_config/config_paths.r")
source("03_functions/cache_helpers.r")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(sf)
})

RUN_TS <- make_timestamp()

dir.create(DIR$rds, showWarnings = FALSE, recursive = TRUE)
dir.create(DIR$qa, showWarnings = FALSE, recursive = TRUE)

pt_log <- function(...) {
  message(format(Sys.time(), "%H:%M:%S"), " | ", ...)
}

pt_latest_file <- function(pattern, dir = DIR$qa) {
  files <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return(NA_character_)
  files[order(file.info(files)$mtime, decreasing = TRUE)][[1]]
}

pt_read_or_empty <- function(path) {
  if (is.na(path) || !file.exists(path)) return(tibble())
  readr::read_csv(path, show_col_types = FALSE)
}

pt_copy_if_exists <- function(src, dst) {
  if (!is.na(src) && file.exists(src)) {
    file.copy(src, dst, overwrite = TRUE)
    TRUE
  } else {
    FALSE
  }
}

# ==== 2. Options =============================================================

REFRESH_WRU_XML <- isTRUE(getOption("BRIM_CNRFC_PREPROCESS_REFRESH_WRU_XML", FALSE))

# These are also read by qa_cnrfc_basin_product_availability_audit.r.
if (is.null(getOption("BRIM_CNRFC_BASIN_MAX_IDS", NULL))) {
  options(BRIM_CNRFC_BASIN_MAX_IDS = Inf)
}
if (is.null(getOption("BRIM_CNRFC_BASIN_SAMPLE_STRATEGY", NULL))) {
  options(BRIM_CNRFC_BASIN_SAMPLE_STRATEGY = "all")
}
if (is.null(getOption("BRIM_CNRFC_BASIN_PRODUCT_MODE", NULL))) {
  options(BRIM_CNRFC_BASIN_PRODUCT_MODE = "high_value")
}
if (is.null(getOption("BRIM_CNRFC_BASIN_CHECK_WEATHER_GOV", NULL))) {
  options(BRIM_CNRFC_BASIN_CHECK_WEATHER_GOV = FALSE)
}
if (is.null(getOption("BRIM_CNRFC_BASIN_CHECK_WATER_RESOURCES_UPDATE", NULL))) {
  options(BRIM_CNRFC_BASIN_CHECK_WATER_RESOURCES_UPDATE = TRUE)
}
if (is.null(getOption("BRIM_CNRFC_BASIN_USE_WRU_XML_PRODUCTS", NULL))) {
  options(BRIM_CNRFC_BASIN_USE_WRU_XML_PRODUCTS = TRUE)
}
if (is.null(getOption("BRIM_CNRFC_BASIN_RESUME_FROM_CACHE", NULL))) {
  options(BRIM_CNRFC_BASIN_RESUME_FROM_CACHE = TRUE)
}
if (is.null(getOption("BRIM_CNRFC_BASIN_REQUEST_DELAY_SEC", NULL))) {
  options(BRIM_CNRFC_BASIN_REQUEST_DELAY_SEC = 0.10)
}

# ==== 3. Ensure WRU XML matrix exists =======================================

wru_xml_matrix_path <- pt_latest_file("^cnrfc_wru_xml_product_matrix_[0-9_]+\\.csv$")

if (REFRESH_WRU_XML || is.na(wru_xml_matrix_path) || !file.exists(wru_xml_matrix_path)) {
  wru_script <- "qa/qa_cnrfc_water_resources_update_kml_audit.r"
  if (!file.exists(wru_script)) {
    stop("Missing WRU XML audit script: ", wru_script)
  }
  pt_log("Refreshing CNRFC WRU XML product matrix via QA audit...")
  old_wru_options <- options(
    BRIM_CNRFC_WRU_KML_MAX_FILES = getOption("BRIM_CNRFC_WRU_KML_MAX_FILES", 100L),
    BRIM_CNRFC_WRU_KML_FETCH = TRUE,
    BRIM_CNRFC_WRU_KML_REQUEST_DELAY_SEC = getOption("BRIM_CNRFC_WRU_KML_REQUEST_DELAY_SEC", 0.10),
    BRIM_CNRFC_WRU_KML_MAX_CONTEXT_IDS = getOption("BRIM_CNRFC_WRU_KML_MAX_CONTEXT_IDS", 120L)
  )
  on.exit(options(old_wru_options), add = TRUE)
  source(wru_script, local = FALSE)
  wru_xml_matrix_path <- pt_latest_file("^cnrfc_wru_xml_product_matrix_[0-9_]+\\.csv$")
} else {
  pt_log("Reusing latest CNRFC WRU XML matrix: ", wru_xml_matrix_path)
}

# ==== 4. Run basin availability engine ======================================

basin_script <- "qa/qa_cnrfc_basin_product_availability_audit.r"
if (!file.exists(basin_script)) {
  stop("Missing basin availability audit script: ", basin_script)
}

pt_log("Building CNRFC basin product availability via tested QA engine...")
source(basin_script, local = FALSE)

# ==== 5. Locate latest outputs ==============================================

long_path <- pt_latest_file("^cnrfc_basin_product_availability_long_[0-9_]+\\.csv$")
matrix_path <- pt_latest_file("^cnrfc_basin_product_availability_matrix_[0-9_]+\\.csv$")
bins_path <- pt_latest_file("^cnrfc_basin_product_availability_bins_[0-9_]+\\.csv$")
wru_matches_path <- pt_latest_file("^cnrfc_basin_water_resources_update_matches_[0-9_]+\\.csv$")
summary_path <- pt_latest_file("^cnrfc_basin_product_availability_summary_[0-9_]+\\.csv$")
cache_path <- file.path(DIR$qa, "cnrfc_basin_product_availability_check_cache.csv")

required <- c(long_path, matrix_path, bins_path, summary_path)
if (any(is.na(required) | !file.exists(required))) {
  stop("Basin availability engine did not produce all required output CSVs.")
}

# ==== 6. Read and write stable products =====================================

availability_long <- pt_read_or_empty(long_path)
availability_matrix <- pt_read_or_empty(matrix_path)
availability_bins <- pt_read_or_empty(bins_path)
wru_matches <- pt_read_or_empty(wru_matches_path)
availability_summary <- pt_read_or_empty(summary_path)
availability_cache <- pt_read_or_empty(cache_path)
wru_xml_matrix <- pt_read_or_empty(wru_xml_matrix_path)

out_long <- file.path(DIR$rds, "cnrfc_basin_product_availability_long.rds")
out_matrix <- file.path(DIR$rds, "cnrfc_basin_product_availability_matrix.rds")
out_bins <- file.path(DIR$rds, "cnrfc_basin_product_availability_bins.rds")
out_wru <- file.path(DIR$rds, "cnrfc_basin_water_resources_update_matches.rds")
out_summary <- file.path(DIR$rds, "cnrfc_basin_product_availability_summary.rds")
out_cache <- file.path(DIR$rds, "cnrfc_basin_product_availability_check_cache.rds")
out_wru_xml <- file.path(DIR$rds, "cnrfc_wru_xml_product_matrix.rds")
out_manifest <- file.path(DIR$rds, "cnrfc_basin_product_availability_manifest.rds")

manifest <- tibble(
  run_timestamp = RUN_TS,
  product = c(
    "availability_long",
    "availability_matrix",
    "availability_bins",
    "wru_matches",
    "availability_summary",
    "availability_cache",
    "wru_xml_matrix"
  ),
  source_csv = c(
    long_path,
    matrix_path,
    bins_path,
    wru_matches_path,
    summary_path,
    cache_path,
    wru_xml_matrix_path
  ),
  output_rds = c(
    out_long,
    out_matrix,
    out_bins,
    out_wru,
    out_summary,
    out_cache,
    out_wru_xml
  ),
  rows = c(
    nrow(availability_long),
    nrow(availability_matrix),
    nrow(availability_bins),
    nrow(wru_matches),
    nrow(availability_summary),
    nrow(availability_cache),
    nrow(wru_xml_matrix)
  )
)

saveRDS(availability_long, out_long)
saveRDS(availability_matrix, out_matrix)
saveRDS(availability_bins, out_bins)
saveRDS(wru_matches, out_wru)
saveRDS(availability_summary, out_summary)
saveRDS(availability_cache, out_cache)
saveRDS(wru_xml_matrix, out_wru_xml)
saveRDS(manifest, out_manifest)

# ==== 6A. Build map-ready CNRFC basin product-availability polygons ===========
##
## PURPOSE:
##   Prepare a slim local/static polygon product for BRIM map implementation.
##   This is intentionally separate from Ops Live CNRFC point products.
##
## DESIGN:
##   - Geometry comes from the existing core-map cache outputs.
##   - Availability attributes come from the 52_ product matrix.
##   - Broad styling fields are precomputed for future browser-side display modes.
##   - This script does not modify the Leaflet map yet; it only prepares RDS/CSV
##     products for the next implementation patch.

pt_first_existing <- function(paths) {
  paths <- paths[!is.na(paths) & nzchar(paths)]
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

pt_to_yesno <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    tolower(x) %in% c("true", "t", "yes", "y", "1", "available", "found", "id_found_in_summary_page") ~ TRUE,
    tolower(x) %in% c("false", "f", "no", "n", "0", "not_available", "not_found", "summary_page_loaded_id_not_found") ~ FALSE,
    TRUE ~ FALSE
  )
}

pt_safe_chr <- function(x, fallback = "") {
  x <- as.character(x)
  x[is.na(x)] <- fallback
  x
}

pt_pick_col <- function(df, candidates, fallback = NA_character_) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) rep(fallback, nrow(df)) else df[[hit[[1]]]]
}

pt_read_cnrfc_geom <- function(path, source_label, source_priority) {
  if (is.na(path) || !file.exists(path)) {
    return(NULL)
  }

  x <- readRDS(path)
  if (!inherits(x, "sf") || nrow(x) == 0) {
    return(NULL)
  }

  nm <- names(x)
  id_candidates <- c(
    "cnrfc_id", "Basin", "basin", "BASIN",
    "nws5id", "NWS5ID", "nwsid", "NWSID",
    "id", "ID"
  )
  id_hits <- id_candidates[id_candidates %in% nm]
  id_col <- if (length(id_hits) > 0) id_hits[[1]] else NA_character_

  if (is.na(id_col)) {
    warning(
      "CNRFC basin geometry cache lacks a recognized ID field and was skipped: ",
      path,
      " | available fields: ", paste(nm, collapse = ", ")
    )
    return(NULL)
  }

  pt_log("Using CNRFC geometry ID field for ", source_label, ": ", id_col)

  geometry_display_name <- pt_safe_chr(pt_pick_col(
    x,
    c("display_name", "label", "basin_name", "name", "Basin", "River", "Descript", "DESCRIPTION")
  ))
  geometry_forecast_group <- pt_safe_chr(pt_pick_col(
    x,
    c("forecast_group", "fcst_group", "ForecastGr", "Forecast_Group", "group")
  ))

  x |>
    dplyr::mutate(
      cnrfc_id = toupper(trimws(as.character(.data[[id_col]]))),
      geometry_source = source_label,
      geometry_source_priority = source_priority,
      geometry_display_name = geometry_display_name,
      geometry_forecast_group = geometry_forecast_group
    ) |>
    dplyr::filter(!is.na(.data$cnrfc_id), .data$cnrfc_id != "") |>
    dplyr::select(
      dplyr::all_of(c(
        "cnrfc_id",
        "geometry_source",
        "geometry_source_priority",
        "geometry_display_name",
        "geometry_forecast_group"
      )),
      geometry
    ) |>
    sf::st_transform(4326)
}

pt_cnrfc_availability_fill <- function(product_bin) {
  product_bin <- as.character(product_bin)
  dplyr::case_when(
    product_bin == "temp + WY/water-supply + QPF/snow-level" ~ "#1a9850",
    product_bin == "temp + QPF/snow-level" ~ "#91cf60",
    product_bin == "WY/water-supply + QPF/snow-level" ~ "#fee08b",
    product_bin == "WY/water-supply products only" ~ "#ffd92f",
    product_bin == "QPF/snow-level row only" ~ "#fdae61",
    product_bin == "other ensemble products only" ~ "#abd9e9",
    product_bin == "no confirmed basin products in this sample" ~ "#bdbdbd",
    TRUE ~ "#f0f0f0"
  )
}

pt_cnrfc_availability_display <- function(product_bin) {
  product_bin <- as.character(product_bin)
  dplyr::case_when(
    product_bin == "temp + WY/water-supply + QPF/snow-level" ~ "temp + water supply + 6-day daily QPF/FrzingLvl",
    product_bin == "temp + QPF/snow-level" ~ "temp + 6-day daily QPF/FrzingLvl",
    product_bin == "WY/water-supply + QPF/snow-level" ~ "water supply + 6-day daily QPF/FrzingLvl",
    product_bin == "WY/water-supply products only" ~ "water supply products",
    product_bin == "QPF/snow-level row only" ~ "6-day daily QPF/FrzingLvl",
    product_bin == "other ensemble products only" ~ "other ensemble products only",
    product_bin == "no confirmed basin products in this sample" ~ "no confirmed basin products",
    TRUE ~ dplyr::coalesce(product_bin, "No product bin assigned")
  )
}

pt_cnrfc_product_link <- function(url, label) {
  paste0("<a href='", htmltools::htmlEscape(url), "' target='_blank'>", htmltools::htmlEscape(label), "</a>")
}

pt_cnrfc_nonempty <- function(x) {
  !is.na(x) & nzchar(trimws(as.character(x)))
}

pt_cnrfc_available_ensemble_links <- function(availability_long) {
  if (!is.data.frame(availability_long) || nrow(availability_long) == 0) {
    return(tibble(cnrfc_id = character(), water_supply_product_links_html = character()))
  }

  required_cols <- c("cnrfc_id", "product_family", "product_key", "product_label", "product_url", "availability_class")
  if (!all(required_cols %in% names(availability_long))) {
    return(tibble(cnrfc_id = character(), water_supply_product_links_html = character()))
  }

  # These are the water-supply / water-year / seasonal volume products that
  # users usually mean when they ask for CNRFC water-supply products.  Do not
  # point every basin at prodID=9: some basins have WY Accum Vol and Seasonal
  # Trend products, while prodID=9 is not available for that location.
  water_supply_keys <- c(
    "seasonal_trend_plot",
    "water_year_trend_plot",
    "water_year_trend_plot_or_related",
    "water_year_accumulated_volume",
    "multi_water_year_accumulated_volume"
  )

  availability_long |>
    dplyr::filter(
      .data$product_family == "ensemble_product",
      .data$availability_class == "available",
      .data$product_key %in% water_supply_keys,
      pt_cnrfc_nonempty(.data$product_url),
      pt_cnrfc_nonempty(.data$product_label)
    ) |>
    dplyr::mutate(
      cnrfc_id = toupper(trimws(as.character(.data$cnrfc_id))),
      product_id_sort = suppressWarnings(as.numeric(.data$product_id)),
      link_html = pt_cnrfc_product_link(.data$product_url, .data$product_label)
    ) |>
    dplyr::arrange(.data$cnrfc_id, .data$product_id_sort, .data$product_label) |>
    dplyr::group_by(.data$cnrfc_id) |>
    dplyr::summarise(
      water_supply_product_links_html = paste(unique(.data$link_html), collapse = " &middot; "),
      water_supply_specific_product_count = dplyr::n_distinct(.data$product_key),
      .groups = "drop"
    )
}

pt_cnrfc_group_palette <- function(groups) {
  groups <- sort(unique(groups[!is.na(groups) & groups != ""]))
  if (length(groups) == 0) return(stats::setNames(character(0), character(0)))
  cols <- c(
    "#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3",
    "#fdb462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd",
    "#ccebc5", "#ffed6f", "#a6cee3", "#1f78b4", "#b2df8a",
    "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00"
  )
  stats::setNames(rep(cols, length.out = length(groups)), groups)
}

pt_cnrfc_basin_popup <- function(df) {
  vapply(seq_len(nrow(df)), function(i) {
    raw_id <- pt_safe_chr(df$cnrfc_id[i], "NA")
    id <- htmltools::htmlEscape(raw_id)
    nm <- htmltools::htmlEscape(pt_safe_chr(df$display_name[i], "CNRFC basin"))
    fg <- htmltools::htmlEscape(pt_safe_chr(df$forecast_group_display[i], "Not classified"))
    bin <- htmltools::htmlEscape(pt_safe_chr(
      df$product_availability_label_display[i],
      pt_safe_chr(df$product_bin_observed[i], "No product bin assigned")
    ))
    ens <- htmltools::htmlEscape(pt_safe_chr(df$ensemble_products_yes[i], "None confirmed"))

    yn <- function(x) isTRUE(x) || identical(x, TRUE)

    links <- character(0)
    if (yn(df$has_basin_temperature[i])) {
      links <- c(links, pt_cnrfc_product_link(
        paste0("https://www.cnrfc.noaa.gov/temperaturePlots_hc.php?id=", raw_id),
        "Basin mean temperature forecast"
      ))
    }
    water_links <- pt_safe_chr(df$water_supply_product_links_html[i], "")
    if (nzchar(trimws(water_links))) {
      links <- c(links, paste0("<b>Water supply products:</b> ", water_links))
    }
    if (yn(df$has_qpf_snow_level_row[i])) {
      links <- c(links, pt_cnrfc_product_link(
        "https://www.cnrfc.noaa.gov/awipsProducts/RNOHD6RSA.php",
        "6-day daily QPF/FrzingLvl"
      ))
    }
    if (yn(df$has_any_ensemble_product[i])) {
      links <- c(links, pt_cnrfc_product_link(
        paste0("https://www.cnrfc.noaa.gov/ensembleProduct.php?id=", raw_id),
        "CNRFC ensemble products"
      ))
    }

    links_html <- if (length(links) == 0) {
      "<span style='font-size:11px;color:#666;'>No linked product confirmed in this availability audit.</span>"
    } else {
      paste(links, collapse = "<br/>")
    }

    paste(
      c(
        paste0("<b>", id, "</b>"),
        nm,
        paste0("<b>Forecast group:</b> ", fg),
        paste0("<b>Product summary:</b> ", bin),
        paste0("<b>Confirmed ensemble products:</b> ", ens),
        links_html
      ),
      collapse = "<br/>"
    )
  }, character(1))
}
geom_paths <- c(
  cnrfc_basins = pt_first_existing(c(
    file.path(DIR$cache_last, "cnrfc_basins_map.rds"),
    file.path(DIR$rds, "cnrfc_basins_map.rds")
  )),
  cnrfc_fnf_delta = pt_first_existing(c(
    file.path(DIR$cache_last, "cnrfc_fnf_delta_map.rds"),
    file.path(DIR$rds, "cnrfc_fnf_delta_map.rds")
  ))
)

geom_list <- list(
  pt_read_cnrfc_geom(geom_paths[["cnrfc_basins"]], "cnrfc_basins_map", 1L),
  pt_read_cnrfc_geom(geom_paths[["cnrfc_fnf_delta"]], "cnrfc_fnf_delta_map", 2L)
)
geom_list <- geom_list[!vapply(geom_list, is.null, logical(1))]

cnrfc_basin_product_availability_map <- NULL
cnrfc_basin_product_availability_map_bins <- tibble()
cnrfc_basin_forecast_group_bins <- tibble()
cnrfc_basin_product_availability_map_summary <- tibble()

if (length(geom_list) == 0) {
  warning("No CNRFC basin geometry cache found; map-ready basin availability polygons were not written.")
} else {
  basin_geom <- do.call(rbind, geom_list) |>
    dplyr::arrange(geometry_source_priority, cnrfc_id) |>
    dplyr::distinct(cnrfc_id, .keep_all = TRUE)

  availability_join <- availability_matrix |>
    dplyr::mutate(cnrfc_id = toupper(trimws(as.character(.data$cnrfc_id))))

  water_supply_links <- pt_cnrfc_available_ensemble_links(availability_long)
  if (nrow(water_supply_links) > 0) {
    availability_join <- availability_join |>
      dplyr::left_join(water_supply_links, by = "cnrfc_id")
  } else {
    availability_join <- availability_join |>
      dplyr::mutate(
        water_supply_product_links_html = NA_character_,
        water_supply_specific_product_count = 0L
      )
  }

  group_palette <- pt_cnrfc_group_palette(availability_join$forecast_group)

  cnrfc_basin_product_availability_map <- basin_geom |>
    dplyr::left_join(availability_join, by = "cnrfc_id") |>
    dplyr::mutate(
      display_name = dplyr::coalesce(as.character(.data$display_name), .data$geometry_display_name, .data$cnrfc_id),
      forecast_group_display = dplyr::coalesce(as.character(.data$forecast_group), .data$geometry_forecast_group, "Not classified"),
      product_bin_observed = dplyr::coalesce(as.character(.data$product_bin_observed), "no confirmed basin products in this sample"),
      product_availability_label_display = pt_cnrfc_availability_display(.data$product_bin_observed),
      has_ensemble_landing = pt_to_yesno(.data$has_ensemble_landing),
      has_any_ensemble_product = pt_to_yesno(.data$has_any_ensemble_product),
      has_wy_or_water_supply_product = pt_to_yesno(.data$has_wy_or_water_supply_product),
      has_water_resources_update = pt_to_yesno(.data$has_water_resources_update),
      has_basin_temperature = pt_to_yesno(.data$has_basin_temperature),
      has_qpf_snow_level_row = pt_to_yesno(.data$has_qpf_snow_level_row),
      water_supply_product_links_html = dplyr::coalesce(as.character(.data$water_supply_product_links_html), ""),
      water_supply_specific_product_count = dplyr::coalesce(as.integer(.data$water_supply_specific_product_count), 0L),
      fill_product_availability = pt_cnrfc_availability_fill(.data$product_bin_observed),
      fill_water_supply_availability = dplyr::if_else(.data$has_wy_or_water_supply_product, "#2c7fb8", "#d9d9d9"),
      fill_qpf_snow_level_availability = dplyr::if_else(.data$has_qpf_snow_level_row, "#fdae61", "#d9d9d9"),
      fill_temperature_availability = dplyr::if_else(.data$has_basin_temperature, "#d73027", "#d9d9d9"),
      fill_ensemble_availability = dplyr::if_else(.data$has_any_ensemble_product, "#756bb1", "#d9d9d9"),
      fill_forecast_group = dplyr::coalesce(unname(group_palette[.data$forecast_group_display]), "#f0f0f0"),
      stroke_col = "#4d4d4d",
      line_weight = 1.2,
      fill_opacity = 0.44,
      map_default_fill = .data$fill_product_availability,
      map_secondary_panel_modes = "product_availability;forecast_group;water_supply;ensemble;qpf_snow_level;temperature",
      hover_text = paste0(
        .data$cnrfc_id,
        "\n", .data$display_name,
        "\nForecast group: ", .data$forecast_group_display
      )
    )

  cnrfc_basin_product_availability_map$popup_html <- pt_cnrfc_basin_popup(sf::st_drop_geometry(cnrfc_basin_product_availability_map))

  cnrfc_basin_product_availability_map_bins <- cnrfc_basin_product_availability_map |>
    sf::st_drop_geometry() |>
    dplyr::count(product_bin_observed, fill_product_availability, name = "basins") |>
    dplyr::arrange(dplyr::desc(basins), product_bin_observed) |>
    dplyr::mutate(run_timestamp = RUN_TS)

  cnrfc_basin_forecast_group_bins <- cnrfc_basin_product_availability_map |>
    sf::st_drop_geometry() |>
    dplyr::count(forecast_group_display, fill_forecast_group, name = "basins") |>
    dplyr::arrange(dplyr::desc(basins), forecast_group_display) |>
    dplyr::mutate(run_timestamp = RUN_TS)

  cnrfc_basin_product_availability_map_summary <- tibble(
    run_timestamp = RUN_TS,
    metric = c(
      "map_ready_basin_polygons",
      "availability_matrix_rows",
      "geometry_source_cnrfc_basins_rows",
      "geometry_source_fnf_delta_rows",
      "basins_with_product_matrix_match",
      "basins_with_wy_or_water_supply",
      "basins_with_qpf_snow_level",
      "basins_with_basin_temperature",
      "basins_with_any_ensemble_product",
      "basins_without_confirmed_product_bin"
    ),
    value = c(
      nrow(cnrfc_basin_product_availability_map),
      nrow(availability_matrix),
      sum(sf::st_drop_geometry(cnrfc_basin_product_availability_map)$geometry_source == "cnrfc_basins_map", na.rm = TRUE),
      sum(sf::st_drop_geometry(cnrfc_basin_product_availability_map)$geometry_source == "cnrfc_fnf_delta_map", na.rm = TRUE),
      sum(!is.na(sf::st_drop_geometry(cnrfc_basin_product_availability_map)$run_timestamp)),
      sum(cnrfc_basin_product_availability_map$has_wy_or_water_supply_product, na.rm = TRUE),
      sum(cnrfc_basin_product_availability_map$has_qpf_snow_level_row, na.rm = TRUE),
      sum(cnrfc_basin_product_availability_map$has_basin_temperature, na.rm = TRUE),
      sum(cnrfc_basin_product_availability_map$has_any_ensemble_product, na.rm = TRUE),
      sum(cnrfc_basin_product_availability_map$product_bin_observed == "no confirmed basin products in this sample", na.rm = TRUE)
    )
  )
}

out_basin_map <- file.path(DIR$rds, "cnrfc_basin_product_availability_map.rds")
out_basin_map_bins <- file.path(DIR$rds, "cnrfc_basin_product_availability_map_bins.rds")
out_basin_group_bins <- file.path(DIR$rds, "cnrfc_basin_product_availability_forecast_group_bins.rds")
out_basin_map_summary <- file.path(DIR$rds, "cnrfc_basin_product_availability_map_summary.rds")

## Keep a duplicate copy in cache/latest so the final-map builder can find the
## product layer in the same place it reads most Local-layer map-ready caches.
out_basin_map_cache <- file.path(DIR$cache_last, "cnrfc_basin_product_availability_map.rds")
out_basin_map_bins_cache <- file.path(DIR$cache_last, "cnrfc_basin_product_availability_map_bins.rds")
out_basin_group_bins_cache <- file.path(DIR$cache_last, "cnrfc_basin_product_availability_forecast_group_bins.rds")
out_basin_map_summary_cache <- file.path(DIR$cache_last, "cnrfc_basin_product_availability_map_summary.rds")

if (!is.null(cnrfc_basin_product_availability_map)) {
  saveRDS(cnrfc_basin_product_availability_map, out_basin_map)
  saveRDS(cnrfc_basin_product_availability_map_bins, out_basin_map_bins)
  saveRDS(cnrfc_basin_forecast_group_bins, out_basin_group_bins)
  saveRDS(cnrfc_basin_product_availability_map_summary, out_basin_map_summary)

  saveRDS(cnrfc_basin_product_availability_map, out_basin_map_cache)
  saveRDS(cnrfc_basin_product_availability_map_bins, out_basin_map_bins_cache)
  saveRDS(cnrfc_basin_forecast_group_bins, out_basin_group_bins_cache)
  saveRDS(cnrfc_basin_product_availability_map_summary, out_basin_map_summary_cache)

  readr::write_csv(cnrfc_basin_product_availability_map_bins, file.path(DIR$qa, paste0("cnrfc_basin_product_availability_map_bins_", RUN_TS, ".csv")))
  readr::write_csv(cnrfc_basin_forecast_group_bins, file.path(DIR$qa, paste0("cnrfc_basin_product_availability_forecast_group_bins_", RUN_TS, ".csv")))
  readr::write_csv(cnrfc_basin_product_availability_map_summary, file.path(DIR$qa, paste0("cnrfc_basin_product_availability_map_summary_", RUN_TS, ".csv")))

  readr::write_csv(cnrfc_basin_product_availability_map_bins, file.path(DIR$qa, "cnrfc_basin_product_availability_map_bins_latest.csv"))
  readr::write_csv(cnrfc_basin_forecast_group_bins, file.path(DIR$qa, "cnrfc_basin_product_availability_forecast_group_bins_latest.csv"))
  readr::write_csv(cnrfc_basin_product_availability_map_summary, file.path(DIR$qa, "cnrfc_basin_product_availability_map_summary_latest.csv"))
}

# Keep a small human-readable manifest in QA.
out_manifest_csv <- file.path(DIR$qa, paste0("cnrfc_basin_product_availability_preprocess_manifest_", RUN_TS, ".csv"))
readr::write_csv(manifest, out_manifest_csv)

# Also maintain stable latest CSV copies for the small summary/bin tables.
pt_copy_if_exists(bins_path, file.path(DIR$qa, "cnrfc_basin_product_availability_bins_latest.csv"))
pt_copy_if_exists(summary_path, file.path(DIR$qa, "cnrfc_basin_product_availability_summary_latest.csv"))
pt_copy_if_exists(matrix_path, file.path(DIR$qa, "cnrfc_basin_product_availability_matrix_latest.csv"))

pt_print_tibble <- function(x) {
  print(tibble::as_tibble(x), n = Inf)
}

# ==== 7. Console summary =====================================================

message("CNRFC basin product availability preprocess complete.")
message("  Basin availability matrix RDS: ", out_matrix, " (", nrow(availability_matrix), " rows)")
message("  Basin availability long RDS:   ", out_long, " (", nrow(availability_long), " rows)")
message("  Cache RDS:                     ", out_cache, " (", nrow(availability_cache), " rows)")
message("  WRU XML matrix RDS:            ", out_wru_xml, " (", nrow(wru_xml_matrix), " rows)")
message("  Manifest CSV:                  ", out_manifest_csv)
if (!is.null(cnrfc_basin_product_availability_map)) {
  message("  Map-ready basin polygons RDS:   ", out_basin_map, " (", nrow(cnrfc_basin_product_availability_map), " rows)")
  message("  Map-ready basin bins RDS:       ", out_basin_map_bins, " (", nrow(cnrfc_basin_product_availability_map_bins), " rows)")
}
message("")
message("Observed product bins:")
pt_print_tibble(availability_bins)
if (!is.null(cnrfc_basin_product_availability_map)) {
  message("")
  message("Map-ready CNRFC basin availability bins:")
  pt_print_tibble(cnrfc_basin_product_availability_map_bins)
  message("")
  message("Map-ready CNRFC basin forecast-group bins:")
  pt_print_tibble(cnrfc_basin_forecast_group_bins)
  message("")
  message("Map-ready CNRFC basin availability summary:")
  pt_print_tibble(cnrfc_basin_product_availability_map_summary)
}
message("")
message("Important interpretation:")
message("  The matrix rows are CNRFC basin/FNF-basin feature IDs, not all WRU XML station IDs.")
message("  WRU XML includes additional river/reservoir/forecast-point IDs that belong in future 53_.")
