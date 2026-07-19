# SWE024b - Snow pillow / SWE O&M readiness audit
# Read-only audit: writes QA outputs only. Does not modify BRIM products.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(jsonlite)
})

# ---- Locate project root ----------------------------------------------------
if (basename(getwd()) == "brim-live-data-feeds") {
  setwd("..")
}

if (!file.exists("run_build_map.r")) {
  candidate_root <- "C:/Users/doconnor/OneDrive - DOI/Documents/PortaTreasure2"
  if (dir.exists(candidate_root) && file.exists(file.path(candidate_root, "run_build_map.r"))) {
    setwd(candidate_root)
  }
}

if (!file.exists("run_build_map.r")) {
  stop("Could not locate PortaTreasure2 project root. Please setwd() to PortaTreasure2 and rerun.")
}

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
out_dir <- file.path("04_processed_data", "qa", "snow_pillow_om_audit", stamp)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n============================================================\n")
cat("SWE024b - snow pillow / SWE O&M readiness audit\n")
cat("Output directory:\n  ", out_dir, "\n", sep = "")
cat("============================================================\n")

# ---- Helpers ----------------------------------------------------------------
file_md5_safe <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  unname(tools::md5sum(path))
}

file_mtime_safe <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  as.character(file.info(path)$mtime)
}

file_size_mb_safe <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  round(file.info(path)$size / 1024^2, 3)
}

safe_read_csv <- function(path) {
  if (!file.exists(path)) return(NULL)
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

safe_write_csv <- function(x, path) {
  readr::write_csv(x, path)
  invisible(path)
}

is_scalarish <- function(x) {
  is.atomic(x) && length(x) == 1L
}

scalar_to_chr <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (length(x) == 0L) return(NA_character_)
  as.character(x[[1]])
}

count_tbl <- function(df, ...) {
  if (is.null(df) || nrow(df) == 0) return(tibble())
  df |> count(..., name = "n", sort = TRUE)
}

# ---- Expected files ---------------------------------------------------------
expected <- tibble::tribble(
  ~role, ~path,
  "latest_geojson", "brim-live-data-feeds/docs/data/snow_pillow_latest.geojson",
  "latest_summary", "brim-live-data-feeds/docs/data/snow_pillow_latest_summary.json",
  "current_trace_docs", "brim-live-data-feeds/docs/data/snow_pillow_current_wy_trace.csv",
  "current_trace_summary", "brim-live-data-feeds/docs/data/snow_pillow_current_wy_trace_summary.json",
  "waterday_docs", "brim-live-data-feeds/docs/data/snow_pillow_swe_waterday_percentiles.csv",
  "monthly_docs", "brim-live-data-feeds/docs/data/snow_pillow_swe_monthly_context.csv",
  "fallback_docs", "brim-live-data-feeds/docs/data/snow_pillow_swe_prior_wy_fallback_traces.csv",
  "waterday_input", "brim-live-data-feeds/data/input/snow_pillow_swe_waterday_percentiles.csv",
  "monthly_input", "brim-live-data-feeds/data/input/snow_pillow_swe_monthly_context.csv",
  "fallback_input", "brim-live-data-feeds/data/input/snow_pillow_swe_prior_wy_fallback_traces.csv",
  "daily_history_rds", "04_processed_data/rds/snow_pillow_swe_daily_history.rds",
  "waterday_rds", "04_processed_data/rds/snow_pillow_swe_waterday_percentiles.rds",
  "monthly_rds", "04_processed_data/rds/snow_pillow_swe_monthly_context.rds",
  "history_run_summary", "04_processed_data/qa/snow_pillow_history_context_run_summary.csv",
  "history_station_summary", "04_processed_data/qa/snow_pillow_history_context_station_summary.csv"
)

file_manifest <- expected |>
  mutate(
    exists = file.exists(path),
    size_mb = vapply(path, file_size_mb_safe, numeric(1)),
    mtime = vapply(path, file_mtime_safe, character(1)),
    md5 = vapply(path, file_md5_safe, character(1))
  )

safe_write_csv(file_manifest, file.path(out_dir, "swe024_file_manifest.csv"))

# ---- docs/data vs data/input consistency ------------------------------------
consistency_pairs <- tibble::tribble(
  ~product, ~docs_role, ~input_role,
  "waterday_percentiles", "waterday_docs", "waterday_input",
  "monthly_context", "monthly_docs", "monthly_input",
  "prior_wy_fallback", "fallback_docs", "fallback_input"
)

consistency <- consistency_pairs |>
  left_join(file_manifest |> select(docs_role = role, docs_path = path, docs_md5 = md5, docs_exists = exists, docs_mtime = mtime, docs_size_mb = size_mb), by = "docs_role") |>
  left_join(file_manifest |> select(input_role = role, input_path = path, input_md5 = md5, input_exists = exists, input_mtime = mtime, input_size_mb = size_mb), by = "input_role") |>
  mutate(match = docs_exists & input_exists & !is.na(docs_md5) & docs_md5 == input_md5)

cat("\n==== docs/data vs data/input consistency ====\n")
print(consistency |> select(product, docs_role, input_role, docs_md5, docs_exists, docs_mtime, docs_size_mb, input_md5, input_exists, input_mtime, input_size_mb, match), n = Inf, width = Inf)
safe_write_csv(consistency, file.path(out_dir, "swe024_docs_vs_input_consistency.csv"))

# ---- Latest summary JSON, safely flattened ----------------------------------
latest_summary_path <- "brim-live-data-feeds/docs/data/snow_pillow_latest_summary.json"
summary_scalar <- tibble()
summary_nested <- tibble()

if (file.exists(latest_summary_path)) {
  latest_summary <- jsonlite::read_json(latest_summary_path, simplifyVector = FALSE)

  summary_scalar <- tibble(
    field = names(latest_summary),
    is_scalar = vapply(latest_summary, is_scalarish, logical(1)),
    value = vapply(latest_summary, function(x) if (is_scalarish(x)) scalar_to_chr(x) else NA_character_, character(1)),
    nested_length = vapply(latest_summary, length, integer(1))
  )

  summary_nested <- summary_scalar |>
    filter(!is_scalar) |>
    select(field, nested_length)

  safe_write_csv(summary_scalar, file.path(out_dir, "swe024_latest_summary_scalar_fields.csv"))
  safe_write_csv(summary_nested, file.path(out_dir, "swe024_latest_summary_nested_fields.csv"))

  cat("\n==== Latest summary scalar fields of interest ====\n")
  summary_scalar |>
    filter(grepl("awdb|delta|row|latest|snotel|cdec|stale|guard", field, ignore.case = TRUE)) |>
    select(field, value, nested_length) |>
    print(n = Inf, width = Inf)

  cat("\n==== Latest summary nested fields ====\n")
  print(summary_nested, n = Inf, width = Inf)
}

# ---- Latest GeoJSON source/status audit -------------------------------------
latest_props <- NULL
latest_geojson_path <- "brim-live-data-feeds/docs/data/snow_pillow_latest.geojson"
if (file.exists(latest_geojson_path)) {
  gj <- jsonlite::fromJSON(latest_geojson_path, simplifyVector = TRUE)
  latest_props <- tibble::as_tibble(gj$features$properties)

  latest_source_counts <- latest_props |>
    count(live_provider_key, latest_swe_source_label, latest_swe_report_status, name = "stations", sort = TRUE)

  cat("\n==== Latest GeoJSON source/status counts ====\n")
  print(latest_source_counts, n = Inf, width = Inf)
  safe_write_csv(latest_source_counts, file.path(out_dir, "swe024_latest_geojson_source_status_counts.csv"))

  if ("swe_delta_suppressed_stale_latest" %in% names(latest_props)) {
    stale_delta_rows <- latest_props |>
      filter(!is.na(swe_delta_suppressed_stale_latest), swe_delta_suppressed_stale_latest == TRUE) |>
      select(any_of(c(
        "station_name", "provider_station_id", "live_provider_key",
        "latest_swe_in", "latest_swe_date_local", "latest_swe_age_days",
        "latest_swe_report_status", "latest_swe_source_label",
        "swe_delta_1day_in", "swe_delta_3day_in", "swe_delta_7day_in",
        "swe_delta_suppressed_reason"
      ))) |>
      arrange(desc(latest_swe_age_days), station_name)

    cat("\n==== Stale-latest delta suppression rows ====\n")
    print(stale_delta_rows, n = Inf, width = Inf)
    safe_write_csv(stale_delta_rows, file.path(out_dir, "swe024_stale_delta_suppression_rows.csv"))
  }
}

# ---- Current-WY trace audit --------------------------------------------------
trace_path <- "brim-live-data-feeds/docs/data/snow_pillow_current_wy_trace.csv"
trace <- safe_read_csv(trace_path)
if (!is.null(trace)) {
  trace_source_counts <- trace |>
    count(live_provider_key, source_element, swe_source_class, swe_source_label, name = "rows", sort = TRUE)

  trace_max_values <- trace |>
    group_by(live_provider_key, source_element, swe_source_class, swe_source_label) |>
    summarise(
      rows = n(),
      stations = n_distinct(station_uid),
      max_swe = max(swe_in, na.rm = TRUE),
      p99_swe = as.numeric(quantile(swe_in, 0.99, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    arrange(desc(max_swe))

  cat("\n==== Current-WY trace source counts ====\n")
  print(trace_source_counts, n = Inf, width = Inf)
  cat("\n==== Current-WY trace max values ====\n")
  print(trace_max_values, n = Inf, width = Inf)

  safe_write_csv(trace_source_counts, file.path(out_dir, "swe024_current_wy_trace_source_counts.csv"))
  safe_write_csv(trace_max_values, file.path(out_dir, "swe024_current_wy_trace_max_values.csv"))
}

# ---- Column inventory --------------------------------------------------------
csv_roles <- file_manifest |>
  filter(exists, grepl("docs$|input$|trace", role), grepl("\\.csv$", path))

column_inventory <- list()
for (i in seq_len(nrow(csv_roles))) {
  role <- csv_roles$role[i]
  path <- csv_roles$path[i]
  dat <- safe_read_csv(path)
  if (is.null(dat)) next

  inv <- tibble(
    role = role,
    path = path,
    n_rows = nrow(dat),
    column = names(dat),
    class = vapply(dat, function(z) paste(class(z), collapse = "/"), character(1)),
    all_na = vapply(dat, function(z) all(is.na(z)), logical(1)),
    n_na = vapply(dat, function(z) sum(is.na(z)), integer(1))
  )

  column_inventory[[role]] <- inv
}

column_inventory <- bind_rows(column_inventory)
safe_write_csv(column_inventory, file.path(out_dir, "swe024_column_inventory.csv"))

cat("\n==== All-NA columns, if any ====\n")
all_na_cols <- column_inventory |> filter(all_na)
print(all_na_cols, n = Inf, width = Inf)

# ---- Historical daily RDS and two-median readiness ---------------------------
hist_path <- "04_processed_data/rds/snow_pillow_swe_daily_history.rds"
if (file.exists(hist_path)) {
  hist <- readRDS(hist_path)

  # Normalize date column name for audit output only.
  if (!"obs_date" %in% names(hist) && "obs_date_local" %in% names(hist)) {
    hist <- hist |> mutate(obs_date = as.Date(obs_date_local))
  } else if ("obs_date" %in% names(hist)) {
    hist <- hist |> mutate(obs_date = as.Date(obs_date))
  }

  hist_source_counts <- hist |>
    count(live_provider_key, provider, source_element, name = "rows") |>
    arrange(live_provider_key, source_element)

  hist_max_values <- hist |>
    group_by(live_provider_key, provider, source_element) |>
    summarise(
      rows = n(),
      stations = n_distinct(station_uid),
      max_swe = max(swe_in, na.rm = TRUE),
      p99_swe = as.numeric(quantile(swe_in, 0.99, na.rm = TRUE)),
      first_date = min(obs_date, na.rm = TRUE),
      last_date = max(obs_date, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(max_swe))

  cat("\n==== Historical daily RDS source counts ====\n")
  print(hist_source_counts, n = Inf, width = Inf)
  cat("\n==== Historical daily RDS max values ====\n")
  print(hist_max_values, n = Inf, width = Inf)

  safe_write_csv(hist_source_counts, file.path(out_dir, "swe024_history_daily_source_counts.csv"))
  safe_write_csv(hist_max_values, file.path(out_dir, "swe024_history_daily_max_values.csv"))

  # Current WY from latest GeoJSON when available, otherwise max WY in trace/history.
  current_wy <- NA_integer_
  if (!is.null(latest_props) && "latest_water_year_calc" %in% names(latest_props)) {
    current_wy <- suppressWarnings(max(as.integer(latest_props$latest_water_year_calc), na.rm = TRUE))
  }
  if (!is.finite(current_wy) || is.na(current_wy)) {
    current_wy <- suppressWarnings(max(as.integer(hist$water_year), na.rm = TRUE))
  }

  last_complete_wy <- current_wy - 1L
  rolling_start_wy <- last_complete_wy - 29L
  rolling_end_wy <- last_complete_wy

  cat("\n==== Future two-median popup support setup ====\n")
  cat("Current WY:", current_wy, "\n")
  cat("Fixed median window: WY1991-WY2020\n")
  cat("Rolling 30 complete WY window: WY", rolling_start_wy, "-WY", rolling_end_wy, "\n", sep = "")

  # Median support by station/day for two proposed windows.
  normal_support_by_station <- function(data, wy_start, wy_end, label) {
    data |>
      filter(water_year >= wy_start, water_year <= wy_end) |>
      group_by(station_uid, provider_station_id, station_name, live_provider_key, water_day) |>
      summarise(
        normal_type = label,
        wy_start = wy_start,
        wy_end = wy_end,
        n_years = n_distinct(water_year),
        median_swe_in = median(swe_in, na.rm = TRUE),
        .groups = "drop"
      )
  }

  fixed_norm <- normal_support_by_station(hist, 1991L, 2020L, "WY1991-WY2020")
  rolling_norm <- normal_support_by_station(hist, rolling_start_wy, rolling_end_wy, paste0("WY", rolling_start_wy, "-WY", rolling_end_wy))
  norm <- bind_rows(fixed_norm, rolling_norm)

  # Latest station/day support, if latest GeoJSON available.
  latest_norm_join <- tibble()
  if (!is.null(latest_props) && all(c("station_uid", "latest_water_day_calc") %in% names(latest_props))) {
    latest_base <- latest_props |>
      transmute(
        station_uid,
        provider_station_id,
        station_name,
        live_provider_key,
        latest_swe_in = suppressWarnings(as.numeric(latest_swe_in)),
        latest_swe_date_local,
        latest_swe_report_status,
        latest_water_day_calc = suppressWarnings(as.integer(latest_water_day_calc))
      )

    latest_norm_join <- latest_base |>
      left_join(norm, by = c("station_uid", "latest_water_day_calc" = "water_day"), relationship = "many-to-many") |>
      mutate(
        support_ok_10yr = !is.na(n_years) & n_years >= 10,
        median_positive = !is.na(median_swe_in) & median_swe_in > 0,
        pct_median = if_else(support_ok_10yr & median_positive & !is.na(latest_swe_in), 100 * latest_swe_in / median_swe_in, NA_real_)
      )

    support_summary <- latest_norm_join |>
      group_by(normal_type, wy_start, wy_end, live_provider_key) |>
      summarise(
        stations = n_distinct(station_uid),
        stations_with_support_10yr = n_distinct(station_uid[support_ok_10yr]),
        stations_with_positive_median = n_distinct(station_uid[support_ok_10yr & median_positive]),
        stations_with_pct_median = n_distinct(station_uid[!is.na(pct_median)]),
        .groups = "drop"
      ) |>
      arrange(normal_type, live_provider_key)

    cat("\n==== Future two-median popup support summary ====\n")
    print(support_summary, n = Inf, width = Inf)

    safe_write_csv(support_summary, file.path(out_dir, "swe024_two_median_popup_support_summary.csv"))
    safe_write_csv(
      latest_norm_join |>
        select(station_uid, provider_station_id, station_name, live_provider_key, latest_swe_in, latest_swe_date_local, latest_swe_report_status, normal_type, wy_start, wy_end, n_years, median_swe_in, support_ok_10yr, median_positive, pct_median),
      file.path(out_dir, "swe024_two_median_latest_station_day_audit.csv")
    )
  }
}

# ---- Water-year rollover checklist ------------------------------------------
rollover <- tibble::tribble(
  ~step_order, ~task, ~notes,
  1, "Confirm current water year has advanced after Oct 1", "Latest-feed scripts should automatically start a new current-WY trace using water-year logic.",
  2, "Run SWE historical context rebuild", "Run 02_preprocess/39_build_snow_pillow_history_context.r after prior WY is complete; current WY should be excluded from percentiles.",
  3, "Upload SWE context CSVs", "Upload both docs/data and data/input copies of waterday percentiles, monthly context, and fallback traces.",
  4, "Run SWE latest GitHub Action", "Refresh latest GeoJSON and current-WY trace after context products are uploaded.",
  5, "Run SCAN historical/context refresh", "Likely run SCAN scripts 34-38, then publish docs/data and data/input equivalents as applicable.",
  6, "Run O&M audit", "Rerun this audit to verify docs/input consistency, source fields, stale-delta suppression, and normal-period support.",
  7, "Spot-check BRIM popups", "Check several CDEC, SNOTEL, sparse-history, stale, and fallback stations."
)

safe_write_csv(rollover, file.path(out_dir, "swe024_water_year_rollover_checklist.csv"))
cat("\n==== Water-year rollover checklist ====\n")
print(rollover, n = Inf, width = Inf)

# ---- Run summary -------------------------------------------------------------
run_summary <- tibble::tibble(
  metric = c(
    "audit_time_local",
    "project_root",
    "output_directory",
    "docs_input_all_match",
    "latest_geojson_exists",
    "current_trace_exists",
    "daily_history_rds_exists"
  ),
  value = c(
    format(Sys.time(), "%Y-%m-%d %I:%M %p %Z"),
    getwd(),
    out_dir,
    as.character(all(consistency$match, na.rm = TRUE)),
    as.character(file.exists(latest_geojson_path)),
    as.character(file.exists(trace_path)),
    as.character(file.exists(hist_path))
  )
)

safe_write_csv(run_summary, file.path(out_dir, "swe024_run_summary.csv"))
cat("\n==== SWE024 run summary ====\n")
print(run_summary, n = Inf, width = Inf)

cat("\nDone: SWE024b O&M readiness audit complete.\n")
cat("QA output directory:\n  ", out_dir, "\n", sep = "")
