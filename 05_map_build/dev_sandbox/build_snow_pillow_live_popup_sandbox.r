# ==== build_snow_pillow_live_popup_sandbox.r ================================
## SWE011:
##   - Plot A monthly reference ribbon now uses a complete repeated monthly
##     climatology sequence across the display window, rather than only the
##     months where actual station data are retained.
##   - This keeps the gray Plot A reference band/median as a regular repeating
##     seasonal pattern while the black actual line still breaks across gaps.
## SWE010:
##   - Use SCAN-style, station-specific period-of-analysis wording in Plot A/B
##     notes while keeping SWE-specific thresholds and data-status logic.
##   - Read POA labels from updated SWE historical context products when present.
##
## SWE009g:
##   - Quantify Plot A monthly-mean retention criteria in popup notes.
##   - Default monthly-reference density threshold is now 60% unless overridden.
## SWE009c:
##   - Clean up Plot A/B geometry after review showed visually noisy SVGs.
##   - Force station-specific plot input filtering with base subsetting.
##   - Print per-station plot input row counts for QA.
##   - Smooth daily percentile lines/ribbons for display only.
##   - Draw Plot B using p10-p90 and p30-p70 only; suppress p00/p100 tails.
##   - Break current-WY and monthly lines across data gaps.
##   - Keep Plot A to one retained station-month row and reduce dot clutter.
##
## SWE009a:
##   - Tighten Plot A/B formatting after first historical-context review.
##   - Use monthly reference fields when available for Plot A p30-p70 ribbon.
##   - Scale Plot B by current trace and p90 rather than p100/outlier tails.
##   - Move explanatory notes outside the plot border and make notes match plot titles.
##   - Default historical sandbox to 10 stations to keep self-contained output small.
##
## SWE009:
##   - Read SWE historical context products and add SCAN-like popup plots:
##     Plot A recent monthly SWE context and Plot B daily percentile ribbons.
##   - Use historical-context colors for marker fill; retain freshness/status in popup text.
##   - Keep this sandbox subset-first so historical SVGs do not bloat the dev HTML.
##
## SWE006a:
##   - Compact hover latest-SWE formatting: 4.0" - 6/3/26.
##   - Replace raw feed status codes in the popup with user-facing data-status labels.
##   - Center the SWE y-axis label and keep the plot compact.
##
## SWE006:
##   - Polish popup layout to better match SCAN: compact station metadata line,
##     narrower popup/SVG, black current-WY trace, green latest point, no grid lines.
##   - Use SCAN-derived water-year/water-day helpers and place latest marker on
##     the last plotted observation rather than assuming the summary date/value.
##   - This is still a latest-feed sandbox only; historical A/B context comes in
##     the next context-products patch.
##
## SWE005c:
##   - Fix Leaflet popup/hover binding so each marker shows only its own row.
##   - Apply station limiting before expensive popup/SVG generation.
##   - Default sandbox to a smart 50-station subset; full all-station sandbox is opt-in.
##
## SWE005b:
##   - Enforce truly self-contained sandbox output.
##   - Remove stale snow-pillow sandbox sidecar folders after successful save.
##
## SWE005:
##   Fast sandbox for the BRIM Ops Live snow-pillow / SWE popup and hover.
##
## PURPOSE:
##   Build a small standalone Leaflet HTML map for developing the future BRIM
##   Ops Live snow-pillow layer without rebuilding the full BRIM product.
##
## DESIGN:
##   - Reads the local live-feed products already written by:
##       brim-live-data-feeds/scripts/build_snow_pillow_latest.R
##   - Uses the same browser-facing files that the future Ops Live layer will
##     fetch from GitHub Pages.
##   - Keeps this as a development sandbox. It is not sourced by the final BRIM
##     map builder.
##   - Writes a timestamped standalone HTML file so an older open sandbox is not
##     overwritten during popup/plot iteration.
##
## INPUTS:
##   brim-live-data-feeds/docs/data/snow_pillow_latest.geojson
##   brim-live-data-feeds/docs/data/snow_pillow_latest_summary.json
##   brim-live-data-feeds/docs/data/snow_pillow_current_wy_trace.csv
##   brim-live-data-feeds/docs/data/snow_pillow_current_wy_trace_summary.json
##   brim-live-data-feeds/docs/data/snow_pillow_swe_waterday_percentiles.csv
##   brim-live-data-feeds/docs/data/snow_pillow_swe_monthly_context.csv
##
## OUTPUT:
##   06_output/html/dev/snow_pillow_live_popup_sandbox_<timestamp>.html
## ============================================================================

# ==== 1. Project sanity check ================================================

if (!dir.exists("brim-live-data-feeds") || !dir.exists("05_map_build")) {
  stop(
    "Run this script from the PortaTreasure2 / BRIM project root.\n",
    "Expected folders like brim-live-data-feeds/ and 05_map_build/ were not found."
  )
}

# ==== 2. Packages ============================================================

required_pkgs <- c(
  "sf", "dplyr", "readr", "jsonlite", "lubridate", "leaflet",
  "htmlwidgets", "htmltools", "tibble", "stringr", "rmarkdown"
)

missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  stop(
    "Missing required R packages: ", paste(missing_pkgs, collapse = ", "),
    "\nInstall them before running the snow-pillow popup sandbox."
  )
}

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(jsonlite)
  library(lubridate)
  library(leaflet)
  library(htmlwidgets)
  library(htmltools)
  library(tibble)
  library(stringr)
})

# ==== 3. Paths and switches ==================================================

LATEST_GEOJSON <- file.path(
  "brim-live-data-feeds", "docs", "data", "snow_pillow_latest.geojson"
)

LATEST_SUMMARY_JSON <- file.path(
  "brim-live-data-feeds", "docs", "data", "snow_pillow_latest_summary.json"
)

CURRENT_WY_TRACE_CSV <- file.path(
  "brim-live-data-feeds", "docs", "data", "snow_pillow_current_wy_trace.csv"
)

CURRENT_WY_TRACE_SUMMARY_JSON <- file.path(
  "brim-live-data-feeds", "docs", "data", "snow_pillow_current_wy_trace_summary.json"
)

SWE_WATERDAY_PERCENTILES_CSV <- file.path(
  "brim-live-data-feeds", "docs", "data", "snow_pillow_swe_waterday_percentiles.csv"
)

SWE_MONTHLY_CONTEXT_CSV <- file.path(
  "brim-live-data-feeds", "docs", "data", "snow_pillow_swe_monthly_context.csv"
)

OUT_DIR <- file.path("06_output", "html", "dev")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

RUN_TS <- format(Sys.time(), tz = "America/Los_Angeles", "%Y%m%d_%H%M%S")
OUT_HTML <- file.path(OUT_DIR, paste0("snow_pillow_live_popup_sandbox_", RUN_TS, ".html"))

## Development limiter.
##
## IMPORTANT:
##   A self-contained all-station sandbox can become very large because every
##   popup and per-station SVG is embedded in the HTML.  The production Ops Live
##   layer will fetch hosted files and render popup plots in the browser; this
##   sandbox is only for rapid visual iteration.  Therefore the sandbox defaults
##   to a representative 50-station subset unless explicitly told to build all
##   stations.
##
## Full sandbox, intentionally:
##   Sys.setenv(SNOW_PILLOW_SANDBOX_FULL = "TRUE")
##
## Custom subset:
##   Sys.setenv(SNOW_PILLOW_SANDBOX_STATION_LIMIT = "80")
SANDBOX_FULL <- toupper(trimws(Sys.getenv("SNOW_PILLOW_SANDBOX_FULL", unset = "FALSE"))) %in% c("TRUE", "T", "1", "YES", "Y")
DEFAULT_STATION_LIMIT <- if (SANDBOX_FULL) 0L else 10L
STATION_LIMIT <- suppressWarnings(as.integer(Sys.getenv(
  "SNOW_PILLOW_SANDBOX_STATION_LIMIT",
  unset = as.character(DEFAULT_STATION_LIMIT)
)))
if (is.na(STATION_LIMIT) || STATION_LIMIT < 0) STATION_LIMIT <- DEFAULT_STATION_LIMIT

## Map symbols should show hydrologic-context colors only for genuinely current
## observations.  A stale October value can be hydrologically interesting in the
## popup, but it should not color the map as "much above normal" months later.
FRESH_CONTEXT_MAX_AGE_DAYS <- suppressWarnings(as.integer(Sys.getenv(
  "SNOW_PILLOW_CONTEXT_MAX_AGE_DAYS",
  unset = "2"
)))
if (is.na(FRESH_CONTEXT_MAX_AGE_DAYS) || FRESH_CONTEXT_MAX_AGE_DAYS < 0) {
  FRESH_CONTEXT_MAX_AGE_DAYS <- 2L
}

## Plot A monthly-reference ribbon guardrail.  Some CDEC stations have long
## nominal periods of record but sparse monthly coverage.  The actual recent
## monthly line is still shown, but the gray monthly-reference band/median is
## hidden unless the retained monthly record is reasonably dense across the
## display window.
MONTHLY_REF_MIN_DENSITY <- suppressWarnings(as.numeric(Sys.getenv(
  "SNOW_PILLOW_MONTHLY_REF_MIN_DENSITY",
  unset = "0.60"
)))
if (is.na(MONTHLY_REF_MIN_DENSITY) || MONTHLY_REF_MIN_DENSITY <= 0 || MONTHLY_REF_MIN_DENSITY > 1) {
  MONTHLY_REF_MIN_DENSITY <- 0.60
}

## Plot A monthly-mean retention rule.  The historical-context CSV normally
## carries this information through `n_daily_obs` / `monthly_mean_ok`; keep a
## sandbox fallback so the explanatory note remains quantified even if an older
## context product is being tested.
MONTHLY_MEAN_MIN_DAILY_OBS <- suppressWarnings(as.integer(Sys.getenv(
  "SNOW_PILLOW_MONTHLY_MEAN_MIN_DAILY_OBS",
  unset = "10"
)))
if (is.na(MONTHLY_MEAN_MIN_DAILY_OBS) || MONTHLY_MEAN_MIN_DAILY_OBS < 1) {
  MONTHLY_MEAN_MIN_DAILY_OBS <- 10L
}

## Compact popup plot dimensions. These are intentionally close to final-popup
## size so the sandbox is a realistic preview of the Ops Live experience.
SVG_WIDTH  <- 500L
SVG_HEIGHT <- 370L

# ==== 4. Small helpers =======================================================


`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
}

pt_path_check <- function(path, label) {
  if (!file.exists(path)) {
    stop("Missing ", label, ": ", path)
  }
  invisible(path)
}

pt_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[is.na(x) | x == "" | toupper(x) %in% c("NA", "NULL", "NAN")] <- NA_character_
  x
}

pt_num <- function(x) {
  suppressWarnings(as.numeric(gsub(",", "", as.character(x))))
}

pt_esc <- function(x) {
  htmltools::htmlEscape(as.character(x))
}

pt_display <- function(x, fallback = "Not available") {
  x <- pt_chr(x)
  ifelse(is.na(x), fallback, x)
}

pt_fmt_num <- function(x, digits = 1, suffix = "") {
  x <- suppressWarnings(as.numeric(x))
  out <- rep("Not available", length(x))
  ok <- !is.na(x)
  out[ok] <- paste0(formatC(x[ok], format = "f", digits = digits, big.mark = ","), suffix)
  out <- sub("(\\.\\d*?)0+([^0-9]|$)", "\\1\\2", out)
  out <- sub("\\.([^0-9]|$)", "\\1", out)
  out
}

pt_fmt_short_date <- function(x) {
  x <- suppressWarnings(as.Date(x))
  out <- rep("date not available", length(x))
  ok <- !is.na(x)
  out[ok] <- paste0(
    as.integer(format(x[ok], "%m")), "/",
    as.integer(format(x[ok], "%d")), "/",
    format(x[ok], "%y")
  )
  out
}

pt_fmt_mon_year <- function(x) {
  x <- suppressWarnings(as.Date(x))
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  out[ok] <- format(x[ok], "%b %Y")
  out
}

pt_first_nonmissing <- function(x, fallback = NA_character_) {
  if (is.null(x) || length(x) == 0) return(fallback)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(fallback)
  as.character(x[[1]])
}

pt_poa_label_from_dates <- function(start_date, end_date, fallback = "POA unavailable") {
  start_txt <- pt_fmt_mon_year(start_date)
  end_txt <- pt_fmt_mon_year(end_date)
  if (length(start_txt) == 0 || length(end_txt) == 0 || is.na(start_txt[1]) || is.na(end_txt[1])) {
    return(fallback)
  }
  paste0(start_txt[1], "–", end_txt[1])
}

pt_fmt_hover_swe <- function(swe_in, swe_date, report_status) {
  swe <- suppressWarnings(as.numeric(swe_in))
  status <- pt_chr(report_status)
  out <- rep("Latest SWE: no valid current-WY SWE reported", length(swe))

  ok <- !is.na(swe) & swe >= 0
  out[ok] <- paste0(
    "Latest SWE: ",
    formatC(swe[ok], format = "f", digits = 1),
    "\" - ",
    pt_fmt_short_date(swe_date[ok])
  )

  stale <- ok & status == "stale_last_value"
  out[stale] <- paste0(out[stale], " (stale)")

  out
}

pt_swe_data_status <- function(report_status, staleness_class, age_days = NA_integer_) {
  report_status <- pt_chr(report_status)
  staleness_class <- pt_chr(staleness_class)
  age_days <- suppressWarnings(as.integer(age_days))

  dplyr::case_when(
    report_status == "stale_last_value" |
      staleness_class == "very_stale_gt_21_days" ~ "Stale measurement",
    report_status == "missing_recent_value" |
      staleness_class == "no_valid_current_wy_swe" ~ "No valid SWE reported",
    report_status == "reported_zero" &
      staleness_class %in% c("fresh_0_2_days", "recent_3_7_days") ~ "Current zero SWE",
    report_status == "reported_positive" &
      staleness_class %in% c("fresh_0_2_days", "recent_3_7_days") ~ "Current SWE reported",
    report_status %in% c("reported_zero", "reported_positive") &
      staleness_class == "stale_8_21_days" ~ "Older measurement",
    TRUE ~ "Check source"
  )
}

## Use the same water-year / water-day convention used by the SCAN historical
## context builder.  This is simple date arithmetic from Oct. 1 and naturally
## supports leap-year water years up to water_day 366.  Historical-context
## builders can later decide whether to retain, smooth, or combine Feb. 29 rows.
pt_swe_water_year <- function(date) {
  date <- as.Date(date)
  yr <- lubridate::year(date)
  ifelse(lubridate::month(date) >= 10, yr + 1L, yr)
}

pt_water_day <- function(date) {
  date <- as.Date(date)
  wy <- pt_swe_water_year(date)
  wy_start <- as.Date(sprintf("%04d-10-01", wy - 1L))
  as.integer(date - wy_start) + 1L
}

pt_month_ticks <- function() {
  tibble::tibble(
    water_day = c(1, 32, 62, 93, 124, 152, 183, 213, 244, 274, 305, 336),
    label = c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
  )
}

pt_status_fill <- function(report_status, staleness_class) {
  report_status <- pt_chr(report_status)
  staleness_class <- pt_chr(staleness_class)

  dplyr::case_when(
    report_status == "reported_positive" & staleness_class %in% c("fresh_0_2_days", "recent_3_7_days") ~ "#2C7FB8",
    report_status == "reported_positive" ~ "#7BCCC4",
    report_status == "reported_zero" & staleness_class %in% c("fresh_0_2_days", "recent_3_7_days") ~ "#E0F3F8",
    report_status == "reported_zero" ~ "#D9D9D9",
    report_status == "stale_last_value" ~ "#BDBDBD",
    report_status == "missing_recent_value" ~ "#F0F0F0",
    TRUE ~ "#F0F0F0"
  )
}

pt_marker_radius <- function(swe_in, report_status) {
  swe_in <- pt_num(swe_in)
  report_status <- pt_chr(report_status)

  out <- rep(5.5, length(swe_in))
  ok <- !is.na(swe_in) & swe_in > 0
  out[ok] <- pmin(15, 6 + sqrt(swe_in[ok]) * 1.8)
  out[report_status %in% c("missing_recent_value", "stale_last_value")] <- 6
  out
}

pt_svg_points <- function(x, y) {
  paste0(
    format(round(x, 1), trim = TRUE), ",",
    format(round(y, 1), trim = TRUE),
    collapse = " "
  )
}

pt_svg_line_points <- function(df, x_col, y_col, xf, yf) {
  df <- df |>
    dplyr::filter(!is.na(.data[[x_col]]), !is.na(.data[[y_col]])) |>
    dplyr::arrange(.data[[x_col]])

  if (nrow(df) < 2) return("")

  paste0(
    format(round(xf(df[[x_col]]), 1), trim = TRUE), ",",
    format(round(yf(df[[y_col]]), 1), trim = TRUE),
    collapse = " "
  )
}

pt_smooth_numeric <- function(x, k = 7L) {
  ## Small base-R centered smoother for display only.  This avoids adding a
  ## dependency (zoo/slider) for the sandbox while reducing the day-to-day
  ## quantile jaggedness that made SWE ribbons/median visually noisy.
  x <- suppressWarnings(as.numeric(x))
  n <- length(x)
  if (n == 0L || k <= 1L) return(x)
  half <- floor(k / 2L)
  out <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    idx <- seq.int(max(1L, i - half), min(n, i + half))
    vals <- x[idx]
    vals <- vals[is.finite(vals)]
    out[i] <- if (length(vals) == 0L) NA_real_ else mean(vals)
  }
  out
}

pt_svg_polyline_html <- function(df, x_col, y_col, xf, yf, attrs, gap_col = NULL, max_gap = Inf) {
  ## Like pt_svg_line_points(), but returns one or more <polyline> elements.
  ## This lets us break the line where monthly/daily records have long gaps,
  ## instead of connecting across missing periods with misleading diagonals.
  df <- df |>
    dplyr::filter(!is.na(.data[[x_col]]), !is.na(.data[[y_col]])) |>
    dplyr::arrange(.data[[x_col]])

  if (nrow(df) < 2) return("")

  if (!is.null(gap_col) && gap_col %in% names(df) && is.finite(max_gap)) {
    gap_vals <- df[[gap_col]]
    gap_num <- if (inherits(gap_vals, "Date")) as.numeric(gap_vals) else suppressWarnings(as.numeric(gap_vals))
    new_seg <- c(TRUE, diff(gap_num) > max_gap)
    new_seg[is.na(new_seg)] <- TRUE
    df$.seg_id <- cumsum(new_seg)
  } else {
    df$.seg_id <- 1L
  }

  pieces <- split(df, df$.seg_id)
  paste0(
    vapply(pieces, function(seg) {
      if (nrow(seg) < 2) return("")
      pts <- paste0(
        format(round(xf(seg[[x_col]]), 1), trim = TRUE), ",",
        format(round(yf(seg[[y_col]]), 1), trim = TRUE),
        collapse = " "
      )
      paste0("<polyline points='", pts, "' ", attrs, "/>")
    }, character(1)),
    collapse = ""
  )
}

pt_svg_ribbon_polygon <- function(df, lo_col, hi_col, xf, yf) {
  df <- df |>
    dplyr::filter(!is.na(.data$water_day), !is.na(.data[[lo_col]]), !is.na(.data[[hi_col]])) |>
    dplyr::arrange(.data$water_day)

  if (nrow(df) < 2) return("")

  lo <- paste0(
    format(round(xf(df$water_day), 1), trim = TRUE), ",",
    format(round(yf(df[[lo_col]]), 1), trim = TRUE)
  )

  hi <- paste0(
    format(round(rev(xf(df$water_day)), 1), trim = TRUE), ",",
    format(round(rev(yf(df[[hi_col]])), 1), trim = TRUE)
  )

  paste(c(lo, hi), collapse = " ")
}

pt_svg_ribbon_polygon_x <- function(df, x_col, lo_col, hi_col, xf, yf) {
  df <- df |>
    dplyr::filter(!is.na(.data[[x_col]]), !is.na(.data[[lo_col]]), !is.na(.data[[hi_col]])) |>
    dplyr::arrange(.data[[x_col]])

  if (nrow(df) < 2) return("")

  lo <- paste0(
    format(round(xf(df[[x_col]]), 1), trim = TRUE), ",",
    format(round(yf(df[[lo_col]]), 1), trim = TRUE)
  )

  hi <- paste0(
    format(round(rev(xf(df[[x_col]])), 1), trim = TRUE), ",",
    format(round(rev(yf(df[[hi_col]])), 1), trim = TRUE)
  )

  paste(c(lo, hi), collapse = " ")
}

pt_context_label <- function(swe_in, p10, p30, p70, p90, context_ok = TRUE) {
  swe_in <- suppressWarnings(as.numeric(swe_in))
  p10 <- suppressWarnings(as.numeric(p10))
  p30 <- suppressWarnings(as.numeric(p30))
  p70 <- suppressWarnings(as.numeric(p70))
  p90 <- suppressWarnings(as.numeric(p90))
  context_ok <- isTRUE(context_ok)

  dplyr::case_when(
    !context_ok | is.na(swe_in) | is.na(p10) | is.na(p30) | is.na(p70) | is.na(p90) ~ "No context",
    swe_in <= p10 ~ "Much below normal",
    swe_in <= p30 ~ "Below normal",
    swe_in <= p70 ~ "Near normal",
    swe_in <= p90 ~ "Above normal",
    swe_in >  p90 ~ "Much above normal",
    TRUE ~ "No context"
  )
}

pt_context_fill <- function(x) {
  x <- pt_chr(x)
  dplyr::case_when(
    x == "Much below normal" ~ "#8C510A",
    x == "Below normal"      ~ "#D8B365",
    x == "Near normal"       ~ "#7F7F7F",
    x == "Above normal"      ~ "#92C5DE",
    x == "Much above normal" ~ "#2166AC",
    TRUE                     ~ "#FFFFFF"
  )
}

pt_context_fill_opacity <- function(x) {
  x <- pt_chr(x)
  ifelse(is.na(x) | x == "No context", 0.18, 0.88)
}

pt_swe_month_date <- function(water_year, water_month) {
  water_year <- suppressWarnings(as.integer(water_year))
  water_month <- suppressWarnings(as.integer(water_month))
  cal_month <- c(10L, 11L, 12L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L)[pmax(1L, pmin(12L, water_month))]
  cal_year <- ifelse(water_month <= 3L, water_year - 1L, water_year)
  as.Date(sprintf("%04d-%02d-01", cal_year, cal_month))
}

pt_swe_water_month <- function(date) {
  ## Water-month convention for Plot A reference repetition:
  ## Oct=1, Nov=2, Dec=3, Jan=4, ..., Sep=12.
  mo <- as.integer(format(as.Date(date), "%m"))
  ifelse(mo >= 10L, mo - 9L, mo + 3L)
}

pt_build_trace_svg <- function(trace_df, pct_df, monthly_df, latest_row) {

  trace_df <- trace_df |>
    dplyr::mutate(
      obs_date_local = as.Date(.data$obs_date_local),
      swe_in = pt_num(.data$swe_in)
    )

  if ("water_day" %in% names(trace_df)) {
    trace_df$water_day <- suppressWarnings(as.integer(trace_df$water_day))
  } else {
    trace_df$water_day <- pt_water_day(trace_df$obs_date_local)
  }

  trace_df <- trace_df |>
    dplyr::filter(!is.na(.data$obs_date_local), !is.na(.data$water_day), !is.na(.data$swe_in), .data$swe_in >= 0) |>
    dplyr::arrange(.data$obs_date_local, .data$water_day) |>
    dplyr::group_by(.data$obs_date_local, .data$water_day) |>
    dplyr::summarise(swe_in = dplyr::last(.data$swe_in), .groups = "drop") |>
    dplyr::arrange(.data$water_day, .data$obs_date_local)

  if (nrow(trace_df) == 0) {
    return(paste0(
      "<div class='pt-swe-plot-empty'>No current-water-year SWE trace available for this station.</div>"
    ))
  }

  daily_context_min_years <- 10L
  if ("min_years_for_context" %in% names(pct_df)) {
    vals <- suppressWarnings(as.integer(stats::na.omit(pct_df$min_years_for_context)))
    if (length(vals) > 0 && is.finite(vals[1])) daily_context_min_years <- vals[1]
  }

  daily_max_by_date_years <- NA_integer_
  if ("n_years" %in% names(pct_df)) {
    vals <- suppressWarnings(as.integer(stats::na.omit(pct_df$n_years)))
    if (length(vals) > 0 && is.finite(max(vals))) daily_max_by_date_years <- max(vals)
  }

  daily_poa_label <- "POA unavailable"
  if ("daily_ref_label" %in% names(pct_df)) {
    daily_poa_label <- pt_first_nonmissing(pct_df$daily_ref_label, "POA unavailable")
  } else if (all(c("daily_ref_start_date", "daily_ref_end_date") %in% names(pct_df))) {
    daily_poa_label <- pt_poa_label_from_dates(pct_df$daily_ref_start_date, pct_df$daily_ref_end_date)
  }

  pct_df <- pct_df |>
    dplyr::mutate(
      water_day = suppressWarnings(as.integer(.data$water_day)),
      context_ok = as.logical(.data$context_ok),
      p00_swe_in = if ("p00_swe_in" %in% names(pct_df)) pt_num(.data$p00_swe_in) else NA_real_,
      p10_swe_in = pt_num(.data$p10_swe_in),
      p30_swe_in = pt_num(.data$p30_swe_in),
      p50_swe_in = pt_num(.data$p50_swe_in),
      p70_swe_in = pt_num(.data$p70_swe_in),
      p90_swe_in = pt_num(.data$p90_swe_in),
      p100_swe_in = if ("p100_swe_in" %in% names(pct_df)) pt_num(.data$p100_swe_in) else NA_real_
    ) |>
    dplyr::mutate(
      ## Older or future context products may omit p00/p100. Fall back to
      ## p10/p90 so the full-ribbon drawing logic remains robust.
      p00_swe_in = dplyr::coalesce(.data$p00_swe_in, .data$p10_swe_in),
      p100_swe_in = dplyr::coalesce(.data$p100_swe_in, .data$p90_swe_in)
    ) |>
    dplyr::filter(.data$context_ok, !is.na(.data$water_day)) |>
    dplyr::group_by(.data$water_day) |>
    dplyr::summarise(
      p00_swe_in = mean(.data$p00_swe_in, na.rm = TRUE),
      p10_swe_in = mean(.data$p10_swe_in, na.rm = TRUE),
      p30_swe_in = mean(.data$p30_swe_in, na.rm = TRUE),
      p50_swe_in = mean(.data$p50_swe_in, na.rm = TRUE),
      p70_swe_in = mean(.data$p70_swe_in, na.rm = TRUE),
      p90_swe_in = mean(.data$p90_swe_in, na.rm = TRUE),
      p100_swe_in = mean(.data$p100_swe_in, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$water_day) |>
    dplyr::mutate(
      ## Smooth display percentile curves only.  The source CSV remains daily.
      p00_swe_in = pt_smooth_numeric(.data$p00_swe_in, k = 7L),
      p10_swe_in = pt_smooth_numeric(.data$p10_swe_in, k = 7L),
      p30_swe_in = pt_smooth_numeric(.data$p30_swe_in, k = 7L),
      p50_swe_in = pt_smooth_numeric(.data$p50_swe_in, k = 7L),
      p70_swe_in = pt_smooth_numeric(.data$p70_swe_in, k = 7L),
      p90_swe_in = pt_smooth_numeric(.data$p90_swe_in, k = 7L),
      p100_swe_in = pt_smooth_numeric(.data$p100_swe_in, k = 7L)
    )

  monthly_mean_min_daily_obs <- MONTHLY_MEAN_MIN_DAILY_OBS
  if ("monthly_min_daily_obs" %in% names(monthly_df)) {
    vals <- suppressWarnings(as.integer(stats::na.omit(monthly_df$monthly_min_daily_obs)))
    if (length(vals) > 0 && is.finite(vals[1])) monthly_mean_min_daily_obs <- vals[1]
  } else if ("min_daily_obs" %in% names(monthly_df)) {
    vals <- suppressWarnings(as.integer(stats::na.omit(monthly_df$min_daily_obs)))
    if (length(vals) > 0 && is.finite(vals[1])) monthly_mean_min_daily_obs <- vals[1]
  } else if ("n_daily_obs" %in% names(monthly_df) && "monthly_mean_ok" %in% names(monthly_df)) {
    vals <- suppressWarnings(as.integer(monthly_df$n_daily_obs[as.logical(monthly_df$monthly_mean_ok)]))
    vals <- vals[!is.na(vals)]
    if (length(vals) > 0 && is.finite(min(vals))) monthly_mean_min_daily_obs <- min(vals)
  }

  monthly_context_min_years <- 10L
  if ("min_years_for_monthly_context" %in% names(monthly_df)) {
    vals <- suppressWarnings(as.integer(stats::na.omit(monthly_df$min_years_for_monthly_context)))
    if (length(vals) > 0 && is.finite(vals[1])) monthly_context_min_years <- vals[1]
  }

  monthly_poa_label <- "POA unavailable"
  if ("monthly_ref_label" %in% names(monthly_df)) {
    monthly_poa_label <- pt_first_nonmissing(monthly_df$monthly_ref_label, "POA unavailable")
  } else if (all(c("monthly_ref_start_date", "monthly_ref_end_date") %in% names(monthly_df))) {
    monthly_poa_label <- pt_poa_label_from_dates(monthly_df$monthly_ref_start_date, monthly_df$monthly_ref_end_date)
  }

  monthly_prepped <- monthly_df |>
    dplyr::mutate(
      water_year = suppressWarnings(as.integer(.data$water_year)),
      water_month = suppressWarnings(as.integer(.data$water_month)),
      month_date = pt_swe_month_date(.data$water_year, .data$water_month),
      swe_month_mean_in = pt_num(.data$swe_month_mean_in),
      n_daily_obs = if ("n_daily_obs" %in% names(monthly_df)) suppressWarnings(as.integer(.data$n_daily_obs)) else NA_integer_,
      monthly_mean_ok = as.logical(.data$monthly_mean_ok),
      monthly_ref_ok = if ("monthly_ref_ok" %in% names(monthly_df)) as.logical(.data$monthly_ref_ok) else FALSE,
      monthly_ref_p30_swe_in = if ("monthly_ref_p30_swe_in" %in% names(monthly_df)) pt_num(.data$monthly_ref_p30_swe_in) else NA_real_,
      monthly_ref_p50_swe_in = if ("monthly_ref_p50_swe_in" %in% names(monthly_df)) pt_num(.data$monthly_ref_p50_swe_in) else NA_real_,
      monthly_ref_p70_swe_in = if ("monthly_ref_p70_swe_in" %in% names(monthly_df)) pt_num(.data$monthly_ref_p70_swe_in) else NA_real_
    )

  ## SWE011: Keep the actual monthly trace and the monthly reference sequence
  ## separate.  The actual black line is drawn only for retained station-months
  ## and breaks across data gaps.  The gray reference band/median is a repeated
  ## monthly climatology: one Oct/Nov/.../Sep reference value repeated across
  ## the display window, independent of which actual station-months happened to
  ## be retained.  This avoids diagonal or irregular reference ribbons caused by
  ## sparse actual observations.
  monthly_ref_by_month <- monthly_prepped |>
    dplyr::filter(
      .data$monthly_ref_ok,
      !is.na(.data$water_month),
      !is.na(.data$monthly_ref_p30_swe_in),
      !is.na(.data$monthly_ref_p50_swe_in),
      !is.na(.data$monthly_ref_p70_swe_in)
    ) |>
    dplyr::group_by(.data$water_month) |>
    dplyr::summarise(
      monthly_ref_ok = TRUE,
      monthly_ref_p30_swe_in = dplyr::first(stats::na.omit(.data$monthly_ref_p30_swe_in)) %||% NA_real_,
      monthly_ref_p50_swe_in = dplyr::first(stats::na.omit(.data$monthly_ref_p50_swe_in)) %||% NA_real_,
      monthly_ref_p70_swe_in = dplyr::first(stats::na.omit(.data$monthly_ref_p70_swe_in)) %||% NA_real_,
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$water_month)

  monthly_df <- monthly_prepped |>
    dplyr::filter(.data$monthly_mean_ok, !is.na(.data$month_date), !is.na(.data$swe_month_mean_in)) |>
    dplyr::group_by(.data$water_year, .data$water_month, .data$month_date) |>
    dplyr::summarise(
      swe_month_mean_in = mean(.data$swe_month_mean_in, na.rm = TRUE),
      n_daily_obs = suppressWarnings(max(.data$n_daily_obs, na.rm = TRUE)),
      monthly_ref_ok = any(.data$monthly_ref_ok, na.rm = TRUE),
      monthly_ref_p30_swe_in = dplyr::first(stats::na.omit(.data$monthly_ref_p30_swe_in)) %||% NA_real_,
      monthly_ref_p50_swe_in = dplyr::first(stats::na.omit(.data$monthly_ref_p50_swe_in)) %||% NA_real_,
      monthly_ref_p70_swe_in = dplyr::first(stats::na.omit(.data$monthly_ref_p70_swe_in)) %||% NA_real_,
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$month_date)

  monthly_ref_density <- NA_real_
  monthly_ref_dense_enough <- FALSE
  monthly_ref_expected_months <- NA_integer_
  if (nrow(monthly_df) >= 3 && any(!is.na(monthly_df$month_date))) {
    month_seq <- seq.Date(
      from = as.Date(format(min(monthly_df$month_date, na.rm = TRUE), "%Y-%m-01")),
      to   = as.Date(format(max(monthly_df$month_date, na.rm = TRUE), "%Y-%m-01")),
      by   = "month"
    )
    monthly_ref_expected_months <- length(month_seq)
    monthly_ref_density <- nrow(monthly_df) / max(1L, monthly_ref_expected_months)
    monthly_ref_dense_enough <- is.finite(monthly_ref_density) &&
      monthly_ref_density >= MONTHLY_REF_MIN_DENSITY
  }

  left <- 48
  right <- 10
  top <- 20
  bottom <- 34
  context_h <- if (nrow(monthly_df) >= 3) 88 else 0
  gap <- if (context_h > 0) 16 else 0
  plot_top <- top + context_h + gap
  plot_w <- SVG_WIDTH - left - right
  plot_h <- SVG_HEIGHT - plot_top - bottom

  x_min <- 1
  x_max <- 366

  ## Scale the main daily plot to the full station-specific historical
  ## percentile envelope after SWE QC.  This intentionally includes p00/p100
  ## so unusually high/low historical ranges are visible instead of silently
  ## clipped, matching the SCAN-style daily-envelope interpretation.
  vals_axis <- c(
    trace_df$swe_in,
    if (nrow(pct_df) > 0) c(
      pct_df$p00_swe_in, pct_df$p10_swe_in, pct_df$p30_swe_in,
      pct_df$p50_swe_in, pct_df$p70_swe_in, pct_df$p90_swe_in,
      pct_df$p100_swe_in
    ) else numeric(0)
  )
  vals_axis <- vals_axis[is.finite(vals_axis)]
  y_min <- 0
  y_max <- max(1, ceiling(max(vals_axis, na.rm = TRUE) * 1.12))
  if (!is.finite(y_max) || y_max <= 0) y_max <- 10

  xf <- function(x) left + (as.numeric(x) - x_min) / (x_max - x_min) * plot_w
  yf <- function(y) {
    yy <- plot_top + plot_h - (as.numeric(y) - y_min) / (y_max - y_min) * plot_h
    pmax(plot_top, pmin(plot_top + plot_h, yy))
  }

  month_ticks <- pt_month_ticks()

  y_ticks <- pretty(c(0, y_max), n = 4)
  y_ticks <- y_ticks[y_ticks >= 0 & y_ticks <= y_max]
  if (!0 %in% y_ticks) y_ticks <- sort(unique(c(0, y_ticks)))

  y_tick_labels <- paste0(
    vapply(y_ticks, function(y) {
      yy <- yf(y)
      paste0(
        "<text x='", left - 7, "' y='", round(yy + 3, 1), "' text-anchor='end' font-size='9' fill='#555'>", pt_esc(pt_fmt_num(y, 0)), "</text>"
      )
    }, character(1)),
    collapse = ""
  )

  x_tick_labels <- paste0(
    vapply(seq_len(nrow(month_ticks)), function(i) {
      xx <- xf(month_ticks$water_day[i])
      paste0(
        "<line x1='", round(xx, 1), "' x2='", round(xx, 1), "' y1='", plot_top + plot_h, "' y2='", plot_top + plot_h + 4, "' stroke='#555' stroke-width='1'/>",
        "<text x='", round(xx, 1), "' y='", SVG_HEIGHT - 12, "' text-anchor='middle' font-size='9' fill='#555'>", month_ticks$label[i], "</text>"
      )
    }, character(1)),
    collapse = ""
  )

  ribbon_defs <- tibble::tribble(
    ~lo,            ~hi,             ~fill,
    ## Match the SCAN daily-envelope concept: show the full available
    ## prior-year percentile envelope with nested bands, while using smoothed
    ## display curves so SWE daily quantiles do not create visual noise.
    "p00_swe_in",  "p10_swe_in",  "#EAD6B8",
    "p10_swe_in",  "p30_swe_in",  "#F4EAD8",
    "p30_swe_in",  "p70_swe_in",  "#ECECEC",
    "p70_swe_in",  "p90_swe_in",  "#DDEEF7",
    "p90_swe_in",  "p100_swe_in", "#BFD7EA"
  )

  ribbons <- ""
  median_line <- ""
  daily_context_ok <- nrow(pct_df) >= 30

  if (daily_context_ok) {
    ribbons <- paste0(
      vapply(seq_len(nrow(ribbon_defs)), function(i) {
        pts <- pt_svg_ribbon_polygon(pct_df, ribbon_defs$lo[i], ribbon_defs$hi[i], xf, yf)
        if (!nzchar(pts)) return("")
        paste0("<polygon points='", pts, "' fill='", ribbon_defs$fill[i], "' opacity='0.95'/>")
      }, character(1)),
      collapse = ""
    )

    median_line <- pt_svg_polyline_html(
      pct_df,
      "water_day", "p50_swe_in", xf, yf,
      attrs = "fill='none' stroke='#666666' stroke-width='1.1' stroke-dasharray='3 3' stroke-linejoin='round' stroke-linecap='round'"
    )
  }

  trace_line <- pt_svg_polyline_html(
    trace_df,
    "water_day", "swe_in", xf, yf,
    attrs = "fill='none' stroke='#111111' stroke-width='2.0' stroke-linejoin='round' stroke-linecap='round'",
    gap_col = "water_day",
    max_gap = 3
  )

  latest_trace <- dplyr::slice_tail(trace_df, n = 1)
  latest_pt <- ""

  if (nrow(latest_trace) == 1 && !is.na(latest_trace$water_day[1]) && !is.na(latest_trace$swe_in[1])) {
    latest_pt <- paste0(
      "<circle cx='", round(xf(latest_trace$water_day[1]), 1), "' cy='", round(yf(latest_trace$swe_in[1]), 1), "' r='4' fill='#009E73' stroke='#FFFFFF' stroke-width='1.2'/>",
      "<circle cx='", round(xf(latest_trace$water_day[1]), 1), "' cy='", round(yf(latest_trace$swe_in[1]), 1), "' r='6' fill='none' stroke='#009E73' stroke-width='1' opacity='0.7'/>"
    )
  }

  context_svg <- ""
  if (nrow(monthly_df) >= 3) {
    cx0 <- left
    cy0 <- top + 14
    ch <- context_h - 30
    cw <- plot_w

    c_min_date <- min(monthly_df$month_date, na.rm = TRUE)
    c_max_date <- max(monthly_df$month_date, na.rm = TRUE)

    monthly_ref_plot <- tibble::tibble(
      month_date = as.Date(character()),
      water_month = integer(),
      monthly_ref_ok = logical(),
      monthly_ref_p30_swe_in = numeric(),
      monthly_ref_p50_swe_in = numeric(),
      monthly_ref_p70_swe_in = numeric()
    )

    if (monthly_ref_dense_enough && nrow(monthly_ref_by_month) >= 3 && !is.na(c_min_date) && !is.na(c_max_date)) {
      monthly_ref_plot <- tibble::tibble(
        month_date = seq.Date(
          from = as.Date(format(c_min_date, "%Y-%m-01")),
          to   = as.Date(format(c_max_date, "%Y-%m-01")),
          by   = "month"
        )
      ) |>
        dplyr::mutate(water_month = pt_swe_water_month(.data$month_date)) |>
        dplyr::left_join(monthly_ref_by_month, by = "water_month") |>
        dplyr::filter(
          .data$monthly_ref_ok,
          !is.na(.data$monthly_ref_p30_swe_in),
          !is.na(.data$monthly_ref_p50_swe_in),
          !is.na(.data$monthly_ref_p70_swe_in)
        ) |>
        dplyr::arrange(.data$month_date)
    }

    c_vals <- c(
      monthly_df$swe_month_mean_in,
      monthly_ref_plot$monthly_ref_p70_swe_in
    )
    c_vals <- c_vals[is.finite(c_vals)]
    c_y_min <- 0
    c_y_max <- max(1, ceiling(max(c_vals, na.rm = TRUE) * 1.10))
    if (!is.finite(c_y_max) || c_y_max <= 0) c_y_max <- y_max

    cxf <- function(d) cx0 + ((as.numeric(d) - as.numeric(c_min_date)) / max(1, as.numeric(c_max_date - c_min_date))) * cw
    cyf <- function(y) {
      yy <- cy0 + ch - (as.numeric(y) - c_y_min) / (c_y_max - c_y_min) * ch
      pmax(cy0, pmin(cy0 + ch, yy))
    }

    actual_line <- pt_svg_polyline_html(
      monthly_df,
      "month_date", "swe_month_mean_in", cxf, cyf,
      attrs = "fill='none' stroke='#111111' stroke-width='1.25' stroke-linejoin='round' stroke-linecap='round'",
      gap_col = "month_date",
      max_gap = 50
    )

    monthly_ref_df <- if (monthly_ref_dense_enough && nrow(monthly_ref_plot) >= 3) {
      monthly_ref_plot
    } else {
      monthly_ref_plot[0, , drop = FALSE]
    }

    monthly_ref_ribbon <- ""
    monthly_ref_median <- ""
    if (nrow(monthly_ref_df) >= 3) {
      pts_ref <- pt_svg_ribbon_polygon_x(
        monthly_ref_df, "month_date",
        "monthly_ref_p30_swe_in", "monthly_ref_p70_swe_in",
        cxf, cyf
      )
      if (nzchar(pts_ref)) {
        monthly_ref_ribbon <- paste0("<polygon points='", pts_ref, "' fill='#ECECEC' opacity='0.95'/>")
      }
      monthly_ref_median <- pt_svg_polyline_html(
        monthly_ref_df,
        "month_date", "monthly_ref_p50_swe_in", cxf, cyf,
        attrs = "fill='none' stroke='#777777' stroke-width='0.9' stroke-dasharray='3 3' stroke-linejoin='round' stroke-linecap='round'",
        gap_col = "month_date",
        max_gap = 50
      )
    }

    actual_points <- paste0(
      vapply(seq_len(nrow(monthly_df)), function(i) {
        if (is.na(monthly_df$month_date[i]) || is.na(monthly_df$swe_month_mean_in[i])) return("")
        paste0(
          "<circle cx='", round(cxf(monthly_df$month_date[i]), 1),
          "' cy='", round(cyf(monthly_df$swe_month_mean_in[i]), 1),
          "' r='1.25' fill='#111111' opacity='0.55'/>"
        )
      }, character(1)),
      collapse = ""
    )

    c_latest <- dplyr::slice_tail(monthly_df, n = 1)
    c_latest_dot <- if (nrow(c_latest) == 1) {
      paste0(
        "<circle cx='", round(cxf(c_latest$month_date[1]), 1), "' cy='", round(cyf(c_latest$swe_month_mean_in[1]), 1),
        "' r='3' fill='#009E73' stroke='white' stroke-width='1'/>"
      )
    } else ""

    lab_years <- sort(unique(lubridate::year(monthly_df$month_date)))
    lab_years <- lab_years[lab_years %% 2 == 0]
    if (length(lab_years) == 0) lab_years <- sort(unique(lubridate::year(monthly_df$month_date)))[seq(1, length(unique(lubridate::year(monthly_df$month_date))), length.out = min(4, length(unique(lubridate::year(monthly_df$month_date)))))] |> unique()

    year_labels <- paste0(
      vapply(lab_years, function(yr) {
        xd <- as.Date(paste0(yr, "-01-01"))
        if (xd < c_min_date || xd > c_max_date) return("")
        paste0("<text x='", round(cxf(xd), 1), "' y='", cy0 + ch + 11, "' text-anchor='middle' font-size='9' fill='#666'>", yr, "</text>")
      }, character(1)),
      collapse = ""
    )

    context_svg <- paste0(
      "<text x='", left, "' y='", top + 7, "' font-size='10' fill='#333'>A. Recent monthly SWE context</text>",
      "<rect x='", cx0, "' y='", cy0, "' width='", cw, "' height='", ch, "' fill='#F7F7F7' stroke='#D8D8D8' stroke-width='1'/>",
      monthly_ref_ribbon,
      monthly_ref_median,
      actual_line,
      actual_points,
      c_latest_dot,
      "<text x='", left - 6, "' y='", round(cyf(c_y_min) + 3, 1), "' text-anchor='end' font-size='8' fill='#666'>", pt_esc(pt_fmt_num(c_y_min, 0)), "</text>",
      "<text x='", left - 6, "' y='", round(cyf(c_y_max) + 3, 1), "' text-anchor='end' font-size='8' fill='#666'>", pt_esc(pt_fmt_num(c_y_max, 0)), "</text>",
      year_labels
    )
  }

  lower_title <- if (daily_context_ok) {
    "B. Daily envelope — current WY vs historical daily percentiles"
  } else {
    "B. Daily trace — historical envelope limited"
  }

  daily_note <- if (daily_context_ok) {
    paste0(
      "ribbons = prior-year daily SWE percentiles (min–10th, 10th–30th, 30th–70th, 70th–90th, 90th–max) where ≥",
      daily_context_min_years,
      " years of data are available; max by date = ",
      if (!is.na(daily_max_by_date_years)) paste0(daily_max_by_date_years, " yrs") else "NA",
      "; stats POA ", daily_poa_label,
      ". Dashed=smoothed median; black=current WY; green dot=latest plotted observation."
    )
  } else {
    paste0(
      "current WY shown only; daily percentile envelope hidden because <",
      daily_context_min_years,
      " years of data are available for this station/day; stats POA ",
      daily_poa_label,
      "."
    )
  }

  monthly_note <- if (exists("monthly_ref_dense_enough") && isTRUE(monthly_ref_dense_enough)) {
    paste0(
      "black = actual monthly mean SWE; points = station-months retained with ≥", monthly_mean_min_daily_obs,
      " daily SWE observations; gray band/line = repeated historical monthly 30th–70th percentile and median when ≥",
      monthly_context_min_years,
      " years of data are available and retained monthly coverage meets threshold (",
      round(100 * monthly_ref_density), "% of months; threshold ",
      round(100 * MONTHLY_REF_MIN_DENSITY), "%); stats POA ", monthly_poa_label,
      "; plot shows recent/available record for space."
    )
  } else if (exists("monthly_ref_density") && is.finite(monthly_ref_density)) {
    paste0(
      "black = actual monthly mean SWE; points = station-months retained with ≥", monthly_mean_min_daily_obs,
      " daily SWE observations; repeated gray reference hidden because retained monthly coverage is sparse (",
      round(100 * monthly_ref_density), "% of months; threshold ", round(100 * MONTHLY_REF_MIN_DENSITY),
      "%); stats POA ", monthly_poa_label,
      "; plot shows recent/available record for space."
    )
  } else {
    paste0(
      "black = actual monthly mean SWE where available; points = station-months retained with ≥", monthly_mean_min_daily_obs,
      " daily SWE observations; repeated gray reference hidden because monthly context is limited or retained monthly coverage could not be computed (threshold ",
      round(100 * MONTHLY_REF_MIN_DENSITY), "%); stats POA ", monthly_poa_label,
      "; plot shows recent/available record for space."
    )
  }

  plot_note <- paste0(
    "<div class='pt-swe-note'><b>A. Recent monthly SWE context:</b> ", pt_esc(monthly_note), "</div>",
    "<div class='pt-swe-note'><b>", pt_esc(lower_title), ":</b> ", pt_esc(daily_note), "</div>"
  )

  paste0(
    "<div class='pt-swe-plot-shell'>",
    "<svg class='pt-swe-svg' width='", SVG_WIDTH, "' height='", SVG_HEIGHT, "' viewBox='0 0 ", SVG_WIDTH, " ", SVG_HEIGHT, "' role='img'>",
    "<rect x='0' y='0' width='", SVG_WIDTH, "' height='", SVG_HEIGHT, "' fill='#FFFFFF'/>",
    context_svg,
    "<text x='", left, "' y='", plot_top - 6, "' font-size='10' fill='#333'>", pt_esc(lower_title), "</text>",
    "<rect x='", left, "' y='", plot_top, "' width='", plot_w, "' height='", plot_h, "' fill='#F7F7F7' stroke='#D0D0D0' stroke-width='1'/>",
    ribbons,
    y_tick_labels,
    x_tick_labels,
    median_line,
    trace_line,
    latest_pt,
    "<line x1='", left, "' x2='", SVG_WIDTH - right, "' y1='", plot_top + plot_h, "' y2='", plot_top + plot_h, "' stroke='#777' stroke-width='1'/>",
    "<line x1='", left, "' x2='", left, "' y1='", plot_top, "' y2='", plot_top + plot_h, "' stroke='#777' stroke-width='1'/>",
    "<text x='10' y='", round(plot_top + plot_h / 2, 1), "' font-size='9' fill='#555' text-anchor='middle' transform='rotate(-90 10,", round(plot_top + plot_h / 2, 1), ")'>SWE (in)</text>",
    "</svg>",
    "</div>",
    plot_note
  )
}

pt_make_popup <- function(row, trace_df, pct_df, monthly_df) {

  row <- tibble::as_tibble(row)
  station_uid <- pt_chr(row$station_uid[1])

  ## Use base subsetting here instead of dplyr non-standard evaluation.  This is
  ## intentionally defensive: during sandbox iteration we saw a symptom where
  ## the station-specific metadata changed by marker but the embedded plots were
  ## effectively reused across stations.  Base equality against the selected
  ## station_uid makes the plot input unambiguous and avoids any tidy-eval
  ## masking/recycling surprises.
  station_trace <- trace_df[!is.na(trace_df$station_uid) & trace_df$station_uid == station_uid, , drop = FALSE]
  station_pct <- pct_df[!is.na(pct_df$station_uid) & pct_df$station_uid == station_uid, , drop = FALSE]
  station_monthly <- monthly_df[!is.na(monthly_df$station_uid) & monthly_df$station_uid == station_uid, , drop = FALSE]

  trace_svg <- pt_build_trace_svg(station_trace, station_pct, station_monthly, row)

  official_station_url <- pt_chr(row$official_station_url[1])
  official_data_url <- pt_chr(row$official_data_url[1])

  links <- character(0)
  if (!is.na(official_station_url)) {
    links <- c(links, paste0("<a href='", pt_esc(official_station_url), "' target='_blank'>Official station page</a>"))
  }
  if (!is.na(official_data_url) && !identical(official_data_url, official_station_url)) {
    links <- c(links, paste0("<a href='", pt_esc(official_data_url), "' target='_blank'>Official data page</a>"))
  }

  river_basin <- pt_display(row$river_basin[1])
  if (identical(river_basin, "Not available")) {
    river_basin <- "Not assigned yet"
  }

  meta_line <- paste0(
    pt_display(row$provider_station_id[1]),
    " · elev. ", pt_fmt_num(row$elevation_ft[1], 0, " ft"),
    " · HUC8: ", river_basin
  )

  data_status <- pt_swe_data_status(
    row$latest_swe_report_status[1],
    row$latest_swe_staleness_class[1],
    row$latest_swe_age_days[1]
  )

  paste0(
    "<div class='pt-swe-popup'>",
    "<div class='pt-swe-title'>", pt_esc(row$station_name[1]), "</div>",
    "<div class='pt-swe-subtitle'>", pt_esc(meta_line), "</div>",
    "<div class='pt-swe-provider'>Source: ", pt_esc(row$provider[1]), "</div>",
    "<table class='pt-swe-table'>",
    "<tr><td>Latest SWE</td><td><b>", pt_esc(pt_display(row$latest_swe_display[1])), "</b></td></tr>",
    "<tr><td>Hydrologic context</td><td>", pt_esc(
      if (!isTRUE(row$latest_context_is_current[1]) && !is.na(row$swe_context_label_raw[1]) && row$swe_context_label_raw[1] != "No context") {
        paste0("No current context (latest valid context would be ", row$swe_context_label_raw[1], ")")
      } else {
        pt_display(row$swe_context_label[1], "No context")
      }
    ), "</td></tr>",
    "<tr><td>Data status</td><td>", pt_esc(data_status), "</td></tr>",
    "<tr><td>Snow depth</td><td>", pt_esc(pt_display(row$latest_snow_depth_display[1])), "</td></tr>",
    "<tr><td>Period of record</td><td>", pt_esc(pt_display(row$period_of_record[1])), "</td></tr>",
    "<tr><td>Current WY obs</td><td>", pt_esc(pt_fmt_num(row$n_current_wy_swe_obs[1], 0)), " SWE observations; ",
      pt_esc(pt_fmt_num(row$n_current_wy_swe_positive_obs[1], 0)), " positive; ",
      pt_esc(pt_fmt_num(row$n_current_wy_swe_zero_obs[1], 0)), " zero</td></tr>",
    "</table>",
    trace_svg,
    if (length(links) > 0) paste0("<div class='pt-swe-links'>", paste(links, collapse = " | "), "</div>") else "",
    "</div>"
  )
}

pt_make_hover <- function(row) {
  row <- tibble::as_tibble(row)
  river_basin <- pt_display(row$river_basin[1])
  if (identical(river_basin, "Not available")) river_basin <- "Not assigned yet"

  meta_line <- paste0(
    pt_display(row$provider_station_id[1]),
    " · elev. ", pt_fmt_num(row$elevation_ft[1], 0, " ft"),
    " · HUC8: ", river_basin
  )

  data_status <- pt_swe_data_status(
    row$latest_swe_report_status[1],
    row$latest_swe_staleness_class[1],
    row$latest_swe_age_days[1]
  )

  paste0(
    "<div class='pt-swe-hover'>",
    "<b>", pt_esc(row$station_name[1]), "</b><br/>",
    pt_esc(meta_line), "<br/>",
    "Source: ", pt_esc(row$provider[1]), "<br/>",
    pt_esc(pt_fmt_hover_swe(row$latest_swe_in[1], row$latest_swe_date_local[1], row$latest_swe_report_status[1])),
    "</div>"
  )
}

# ==== 5. Read feed files =====================================================

pt_path_check(LATEST_GEOJSON, "snow-pillow latest GeoJSON")
pt_path_check(LATEST_SUMMARY_JSON, "snow-pillow latest summary JSON")
pt_path_check(CURRENT_WY_TRACE_CSV, "snow-pillow current-WY trace CSV")
pt_path_check(CURRENT_WY_TRACE_SUMMARY_JSON, "snow-pillow current-WY trace summary JSON")
pt_path_check(SWE_WATERDAY_PERCENTILES_CSV, "snow-pillow water-day percentile context CSV")
pt_path_check(SWE_MONTHLY_CONTEXT_CSV, "snow-pillow monthly context CSV")

latest_summary <- jsonlite::fromJSON(LATEST_SUMMARY_JSON)
trace_summary <- jsonlite::fromJSON(CURRENT_WY_TRACE_SUMMARY_JSON)

message("Reading snow-pillow latest GeoJSON: ", LATEST_GEOJSON)
latest_sf <- sf::st_read(LATEST_GEOJSON, quiet = TRUE)

message("Reading snow-pillow current-WY trace CSV: ", CURRENT_WY_TRACE_CSV)
trace <- readr::read_csv(CURRENT_WY_TRACE_CSV, show_col_types = FALSE) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    obs_date_local = as.Date(.data$obs_date_local),
    swe_in = pt_num(.data$swe_in)
  ) |>
  dplyr::filter(is.na(.data$swe_in) | .data$swe_in >= 0)

message("Reading snow-pillow water-day percentile CSV: ", SWE_WATERDAY_PERCENTILES_CSV)
pct_context <- readr::read_csv(SWE_WATERDAY_PERCENTILES_CSV, show_col_types = FALSE) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    water_day = suppressWarnings(as.integer(.data$water_day)),
    context_ok = as.logical(.data$context_ok)
  )

message("Reading snow-pillow monthly context CSV: ", SWE_MONTHLY_CONTEXT_CSV)
monthly_context <- readr::read_csv(SWE_MONTHLY_CONTEXT_CSV, show_col_types = FALSE) |>
  tibble::as_tibble()

latest_context_lookup <- latest_sf |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  dplyr::mutate(
    latest_swe_in = pt_num(.data$latest_swe_in),
    latest_swe_age_days = suppressWarnings(as.integer(.data$latest_swe_age_days)),
    latest_swe_report_status = pt_chr(.data$latest_swe_report_status),
    latest_context_water_day = pt_water_day(as.Date(.data$latest_swe_date_local))
  ) |>
  dplyr::select(
    "station_uid", "latest_context_water_day", "latest_swe_in",
    "latest_swe_age_days", "latest_swe_report_status"
  ) |>
  dplyr::left_join(
    pct_context |>
      dplyr::select(
        .data$station_uid,
        latest_context_water_day = .data$water_day,
        .data$context_ok,
        .data$n_years,
        .data$p10_swe_in, .data$p30_swe_in, .data$p70_swe_in, .data$p90_swe_in
      ),
    by = c("station_uid", "latest_context_water_day")
  ) |>
  dplyr::mutate(
    latest_context_is_current = !is.na(.data$latest_swe_age_days) &
      .data$latest_swe_age_days <= FRESH_CONTEXT_MAX_AGE_DAYS &
      .data$latest_swe_report_status %in% c("reported_positive", "reported_zero"),
    swe_context_label_raw = vapply(seq_len(dplyr::n()), function(i) {
      pt_context_label(
        latest_swe_in[i], p10_swe_in[i], p30_swe_in[i], p70_swe_in[i], p90_swe_in[i],
        isTRUE(context_ok[i])
      )
    }, character(1)),
    swe_context_label = dplyr::if_else(
      .data$latest_context_is_current,
      .data$swe_context_label_raw,
      "No context"
    ),
    swe_context_years = suppressWarnings(as.integer(.data$n_years))
  ) |>
  dplyr::select(
    "station_uid", "latest_context_water_day", "swe_context_label",
    "swe_context_label_raw", "latest_context_is_current", "swe_context_years"
  )

latest_sf <- latest_sf |>
  dplyr::mutate(
    latest_swe_in = pt_num(.data$latest_swe_in),
    latest_swe_age_days = suppressWarnings(as.integer(.data$latest_swe_age_days))
  ) |>
  dplyr::left_join(latest_context_lookup, by = "station_uid") |>
  dplyr::mutate(
    station_fill_col = pt_context_fill(.data$swe_context_label),
    station_fill_opacity = pt_context_fill_opacity(.data$swe_context_label),
    station_radius = pt_marker_radius(.data$latest_swe_in, .data$latest_swe_report_status),
    station_stroke_col = dplyr::case_when(
      .data$latest_swe_report_status == "missing_recent_value" ~ "#777777",
      .data$latest_swe_report_status == "stale_last_value" ~ "#777777",
      .data$latest_swe_staleness_class == "very_stale_gt_21_days" ~ "#777777",
      TRUE ~ "#333333"
    )
  )

## Keep the development sandbox small by default, and apply the station limit
## BEFORE building popup SVGs.  Building every station popup first is slow and
## can inflate the self-contained HTML into the hundreds of MB.
if (STATION_LIMIT > 0 && nrow(latest_sf) > STATION_LIMIT) {
  message(
    "Snow-pillow sandbox subset mode: keeping ", STATION_LIMIT,
    " representative stations. Set SNOW_PILLOW_SANDBOX_FULL=TRUE for all stations."
  )

  ## Build a balanced subset rather than simply taking the first N stations.
  ## The first SWE005c subset was too biased toward fresh positive CDEC stations
  ## because there were more than 50 of those alone.  For popup development we
  ## need examples of positives, zeros, stale values, and missing/no-valid-SWE
  ## stations across providers so all wording and symbology paths can be tested.
  ranked_sandbox_stations <- latest_sf |>
    dplyr::mutate(
      .sandbox_pick_group = dplyr::case_when(
        .data$latest_swe_report_status == "reported_positive" &
          .data$latest_swe_staleness_class %in% c("fresh_0_2_days", "recent_3_7_days") ~ 1L,
        .data$latest_swe_report_status == "reported_zero" &
          .data$latest_swe_staleness_class %in% c("fresh_0_2_days", "recent_3_7_days") ~ 2L,
        .data$latest_swe_report_status == "stale_last_value" ~ 3L,
        .data$latest_swe_report_status == "missing_recent_value" ~ 4L,
        TRUE ~ 5L
      )
    ) |>
    dplyr::arrange(.data$.sandbox_pick_group, .data$provider, dplyr::desc(.data$latest_swe_in), .data$station_name)

  per_provider_status_quota <- max(3L, ceiling(STATION_LIMIT / 10))

  balanced_seed <- ranked_sandbox_stations |>
    dplyr::group_by(.data$provider, .data$.sandbox_pick_group) |>
    dplyr::slice_head(n = per_provider_status_quota) |>
    dplyr::ungroup()

  balanced_fill <- ranked_sandbox_stations |>
    dplyr::filter(!.data$station_uid %in% balanced_seed$station_uid)

  latest_sf <- dplyr::bind_rows(balanced_seed, balanced_fill) |>
    dplyr::distinct(.data$station_uid, .keep_all = TRUE) |>
    dplyr::slice_head(n = STATION_LIMIT) |>
    dplyr::select(-".sandbox_pick_group")
} else {
  message(
    "Snow-pillow sandbox full mode: building all ", nrow(latest_sf),
    " stations. This can create a large self-contained HTML."
  )
}

selected_station_uids <- latest_sf$station_uid
trace <- trace |>
  dplyr::filter(.data$station_uid %in% selected_station_uids)

pct_context <- pct_context |>
  dplyr::filter(.data$station_uid %in% selected_station_uids)

monthly_context <- monthly_context |>
  dplyr::filter(.data$station_uid %in% selected_station_uids)

latest_plain <- latest_sf |>
  sf::st_drop_geometry() |>
  tibble::as_tibble()

plot_input_qa <- latest_plain |>
  dplyr::select("station_uid", "provider_station_id", "station_name") |>
  dplyr::left_join(
    trace |>
      dplyr::count(.data$station_uid, name = "trace_rows"),
    by = "station_uid"
  ) |>
  dplyr::left_join(
    pct_context |>
      dplyr::count(.data$station_uid, name = "pct_rows"),
    by = "station_uid"
  ) |>
  dplyr::left_join(
    monthly_context |>
      dplyr::count(.data$station_uid, name = "monthly_rows"),
    by = "station_uid"
  ) |>
  dplyr::mutate(
    trace_rows = dplyr::coalesce(.data$trace_rows, 0L),
    pct_rows = dplyr::coalesce(.data$pct_rows, 0L),
    monthly_rows = dplyr::coalesce(.data$monthly_rows, 0L)
  )

latest_sf$popup_html <- vapply(seq_len(nrow(latest_plain)), function(i) {
  pt_make_popup(dplyr::slice(latest_plain, i), trace, pct_context, monthly_context)
}, character(1))

latest_sf$hover_html <- vapply(seq_len(nrow(latest_plain)), function(i) {
  pt_make_hover(dplyr::slice(latest_plain, i))
}, character(1))

popup_bytes <- nchar(latest_sf$popup_html, type = "bytes", allowNA = TRUE)

## Leaflet popup/label binding must be supplied as per-feature HTML lists.
## Do NOT use formula syntax like ~htmltools::HTML(hover_html), because HTML()
## can treat the entire column vector as one HTML object and every marker then
## shows many stations at once.
popup_list <- lapply(latest_sf$popup_html, htmltools::HTML)
label_list <- lapply(latest_sf$hover_html, htmltools::HTML)

# ==== 6. QA summary ==========================================================

message("\nSnow-pillow popup sandbox input summary:")
message("  Latest GeoJSON rows:  ", nrow(latest_sf))
message("  Trace rows:           ", nrow(trace))
message("  Stations with traces: ", dplyr::n_distinct(trace$station_uid))
message("  Percentile rows:      ", nrow(pct_context))
message("  Monthly rows:         ", nrow(monthly_context))
message("  Latest build time:    ", latest_summary$build_time_local %||% "Not available")
message("  Trace build time:     ", trace_summary$build_time_local %||% "Not available")
message("  Output HTML:          ", OUT_HTML)
message("  Popup HTML bytes avg: ", format(round(mean(popup_bytes, na.rm = TRUE)), big.mark = ","))
message("  Popup HTML bytes max: ", format(round(max(popup_bytes, na.rm = TRUE)), big.mark = ","))

sandbox_status_counts <- latest_sf |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  dplyr::count(
    provider,
    latest_swe_report_status,
    latest_swe_staleness_class,
    sort = TRUE
  )

print(sandbox_status_counts, n = Inf, width = Inf)

sandbox_context_counts <- latest_sf |>
  sf::st_drop_geometry() |>
  tibble::as_tibble() |>
  dplyr::count(swe_context_label, latest_context_is_current, sort = TRUE)

print(sandbox_context_counts, n = Inf, width = Inf)

# ==== 7. Build map ===========================================================

status_html <- paste0(
  "<div class='pt-swe-status'><b>Snow pillows / SWE sandbox</b><br/>",
  "Latest feed: ", pt_esc(latest_summary$build_time_local %||% "Not available"), "<br/>",
  "Stations: ", nrow(latest_sf), " | Trace rows: ", format(nrow(trace), big.mark = ","), "<br/>",
  "This is a development sandbox only; it does not rebuild the full BRIM map.</div>"
)

legend_html <- paste0(
  "<div class='pt-swe-legend'><b>SWE context</b><br/>",
  "<span style='background:#8C510A'></span> Much below<br/>",
  "<span style='background:#D8B365'></span> Below<br/>",
  "<span style='background:#7F7F7F'></span> Near normal<br/>",
  "<span style='background:#92C5DE'></span> Above<br/>",
  "<span style='background:#2166AC'></span> Much above<br/>",
  "<span style='background:#FFFFFF'></span> No context<br/>",
  "</div>"
)

m <- leaflet::leaflet(latest_sf, options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "CartoDB Positron") |>
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Esri World Imagery") |>
  leaflet::addCircleMarkers(
    group = "Snow pillows / SWE latest",
    radius = ~station_radius,
    stroke = TRUE,
    weight = 1.2,
    color = ~station_stroke_col,
    fillColor = ~station_fill_col,
    fillOpacity = ~station_fill_opacity,
    opacity = 0.95,
    popup = popup_list,
    label = label_list,
    labelOptions = leaflet::labelOptions(
      direction = "auto",
      opacity = 0.98,
      textsize = "12px",
      className = "pt-swe-tooltip"
    ),
    popupOptions = leaflet::popupOptions(
      maxWidth = 560,
      minWidth = 535,
      autoPan = TRUE
    )
  ) |>
  leaflet::addLayersControl(
    baseGroups = c("CartoDB Positron", "Esri World Imagery"),
    overlayGroups = c("Snow pillows / SWE latest"),
    options = leaflet::layersControlOptions(collapsed = FALSE)
  ) |>
  leaflet::addControl(html = htmltools::HTML(status_html), position = "topright") |>
  leaflet::addControl(html = htmltools::HTML(legend_html), position = "bottomleft")

m <- htmlwidgets::onRender(
  m,
  "function(el, x) {
    if (document.getElementById('pt-swe-sandbox-style')) return;
    var style = document.createElement('style');
    style.id = 'pt-swe-sandbox-style';
    style.innerHTML = `
      .pt-swe-popup {
        width: 520px;
        max-width: 520px;
        font: 12px/1.35 Arial, Helvetica, sans-serif;
        color: #111;
      }
      .pt-swe-title { font-weight: bold; font-size: 15px; margin-bottom: 1px; }
      .pt-swe-subtitle { color: #555; margin-bottom: 2px; }
      .pt-swe-provider { color: #555; margin-bottom: 5px; font-size: 11.5px; }
      .pt-swe-table { border-collapse: collapse; width: 100%; margin: 4px 0 6px 0; }
      .pt-swe-table td { border-top: 1px solid #E6E6E6; padding: 3px 5px; vertical-align: top; }
      .pt-swe-table td:first-child { width: 120px; color: #555; font-weight: bold; }
      .pt-swe-plot-shell { border: 1px solid #DDD; border-radius: 4px; padding: 3px; background: #FFF; overflow: hidden; }
      .pt-swe-svg { max-width: 100%; height: auto; display: block; }
      .pt-swe-note { color: #666; font-size: 10.5px; margin-top: 4px; }
      .pt-swe-links { margin-top: 5px; }
      .pt-swe-links a { color: #1f5e9c; text-decoration: none; }
      .pt-swe-links a:hover { text-decoration: underline; }
      .pt-swe-plot-empty { padding: 14px; color: #666; background: #F7F7F7; border: 1px solid #DDD; }
      .leaflet-tooltip.pt-swe-tooltip {
        background: rgba(255,255,255,0.97);
        border: 1px solid rgba(0,0,0,0.38);
        border-radius: 4px;
        box-shadow: 0 2px 7px rgba(0,0,0,0.20);
        color: #111;
        padding: 6px 8px;
        font: 12px/1.25 Arial, Helvetica, sans-serif;
        white-space: nowrap;
      }
      .pt-swe-status, .pt-swe-legend {
        background: rgba(255,255,255,0.94);
        border: 1px solid #AAA;
        border-radius: 5px;
        padding: 7px 9px;
        box-shadow: 0 1px 5px rgba(0,0,0,0.25);
        font: 12px/1.25 Arial, Helvetica, sans-serif;
      }
      .pt-swe-legend span {
        display: inline-block;
        width: 11px;
        height: 11px;
        border: 1px solid rgba(0,0,0,0.25);
        margin-right: 5px;
        vertical-align: -1px;
      }
    `;
    document.head.appendChild(style);
  }"
)

# ==== 8. Save ================================================================

## Write a truly standalone HTML file.
##
## Why this block is stricter than a normal htmlwidgets::saveWidget() call:
##   htmlwidgets can write a sibling *_files folder when self-contained
##   conversion is not actually completed.  For this sandbox, that folder makes
##   06_output/html/dev noisy and easy to confuse with the final BRIM outputs.
##   Therefore we:
##     1. require Pandoc availability before saving;
##     2. delete any pre-existing sidecar folder for the target filename;
##     3. save with selfcontained = TRUE;
##     4. inspect the written HTML for references to *_files; and
##     5. remove old snow-pillow sandbox sidecar folders only after the current
##        HTML has passed the self-contained check.

if (!rmarkdown::pandoc_available()) {
  stop(
    "Pandoc is required to write a truly self-contained sandbox HTML.\n",
    "In RStudio this is usually available automatically. If not, install or ",
    "configure Pandoc before rerunning this sandbox."
  )
}

sidecar_dir <- sub("\\.html$", "_files", OUT_HTML)

if (dir.exists(sidecar_dir)) {
  unlink(sidecar_dir, recursive = TRUE, force = TRUE)
}

htmlwidgets::saveWidget(
  widget = m,
  file = OUT_HTML,
  selfcontained = TRUE
)

## Confirm the HTML does not reference its own sidecar folder or any other
## snow-pillow sandbox sidecar folder.  Use fixed-string checks so Windows path
## separators and regex metacharacters do not matter.
html_txt <- readLines(OUT_HTML, warn = FALSE)
current_sidecar_name <- basename(sidecar_dir)
all_sidecar_refs <- grep(
  "snow_pillow_live_popup_sandbox_.*_files",
  html_txt,
  value = TRUE
)
current_sidecar_refs <- grep(current_sidecar_name, html_txt, value = TRUE, fixed = TRUE)

if (length(current_sidecar_refs) > 0 || length(all_sidecar_refs) > 0) {
  stop(
    "Sandbox HTML still references a *_files sidecar folder, so it is not ",
    "truly self-contained. Output left in place for inspection: ", OUT_HTML
  )
}

## At this point the current HTML is self-contained.  Remove old sidecar
## folders left by earlier non-self-contained sandbox runs.  This intentionally
## targets only snow-pillow sandbox sidecars, not SCAN or other dev sandboxes.
old_sidecars <- list.dirs(OUT_DIR, recursive = FALSE, full.names = TRUE)
old_sidecars <- old_sidecars[grepl("^snow_pillow_live_popup_sandbox_.*_files$", basename(old_sidecars))]

if (length(old_sidecars) > 0) {
  unlink(old_sidecars, recursive = TRUE, force = TRUE)
  message("Removed snow-pillow sandbox sidecar folder(s): ")
  message(paste0("  ", old_sidecars, collapse = "\n"))
}

message("\nWrote self-contained snow-pillow / SWE popup sandbox HTML:")
message("  ", OUT_HTML)

