# ==== 02_cache_huc_climate_theme.r ===========================================
##
## PURPOSE:
##   Build HUC popups and climate/recharge thematic color fields.
##
## NOTE:
##   This file is sourced by 05_map_build/02_build_core_map_cache.r.
##   It expects objects created earlier in that script and creates map-ready
##   cache objects in the calling environment. Do not source this file alone
##   unless you have already created the required input objects.
## ============================================================================

# ---- 8.1 HUC popups ---------------------------------------------------------

huc_map <- purrr::imap(huc_map, function(x, nm) {
  lvl <- as.integer(gsub("^huc", "", nm))
  x$popup_html <- pt_make_huc_popups(x, lvl)
  x
})

# ---- 8A. Enrich HUC layers with PRISM / BCMv8 climate metrics ---------------
##
## PURPOSE:
##   Join geometry-free HUC climate/recharge tables onto the existing simplified
##   HUC map layers.
##
## WHY THIS APPROACH:
##   This avoids creating duplicate HUC polygon geometry in the final HTML.
##   The existing HUC layers get richer popups, but file-size growth is limited
##   to numeric attributes and popup text.
##
## DISPLAY MASK:
##   For HUC6, BCMv8 recharge values are suppressed in the map popup when less
##   than 75% of the HUC6 is covered by the hydrologic-California BCMv8 raster.
##   The analytical table remains unchanged; only map display is masked.

pt_read_huc_climate_table <- function(huc_level) {
  
  table_path <- file.path(
    DIR$rds,
    paste0("huc", huc_level, "_climate_recharge_table.rds")
  )
  
  full_path <- file.path(
    DIR$rds,
    paste0("huc", huc_level, "_climate_recharge_full.rds")
  )
  
  if (file.exists(table_path)) {
    out <- readRDS(table_path)
  } else if (file.exists(full_path)) {
    message("Table RDS not found; reading full sf and dropping geometry: ", full_path)
    out <- sf::st_drop_geometry(readRDS(full_path))
  } else {
    message("No HUC", huc_level, " climate/recharge table found; skipping.")
    return(NULL)
  }
  
  if (inherits(out, "sf")) {
    out <- sf::st_drop_geometry(out)
  }
  
  out
}

pt_apply_bcmv8_display_skip <- function(climate_tbl, huc_level) {
  
  ## Only apply this display mask to HUC6 for now.
  ## The extracted analytical values remain in the source table.
  
  if (huc_level != 6) {
    return(climate_tbl)
  }
  
  if (!"rech_valid_frac" %in% names(climate_tbl)) {
    return(climate_tbl)
  }
  
  climate_tbl |>
    dplyr::mutate(
      bcmv8_recharge_note = dplyr::if_else(
        .data$rech_valid_frac < 0.95,
        paste0(
          "BCMv8 recharge not reported because less than 95% of this HUC ",
          "is covered by the hydrologic-California BCMv8 raster domain."
        ),
        NA_character_
      ),
      rech_in = dplyr::if_else(
        !is.na(.data$bcmv8_recharge_note),
        NA_real_,
        .data$rech_in
      ),
      rech_kaf = dplyr::if_else(
        !is.na(.data$bcmv8_recharge_note),
        NA_real_,
        .data$rech_kaf
      ),
      rech_eff_pct = dplyr::if_else(
        !is.na(.data$bcmv8_recharge_note),
        NA_real_,
        .data$rech_eff_pct
      )
    )
}

pt_join_huc_climate_table <- function(huc_sf, huc_level) {
  
  code_col <- paste0("huc", huc_level)
  
  climate_tbl <- pt_read_huc_climate_table(huc_level)
  
  if (is.null(climate_tbl)) {
    return(huc_sf)
  }
  
  climate_tbl <- pt_apply_bcmv8_display_skip(
    climate_tbl = climate_tbl,
    huc_level = huc_level
  )
  
  keep_cols <- c(
    code_col,
    "map_in",
    "ppt_kaf",
    "rech_in",
    "rech_kaf",
    "rech_eff_pct",
    "ppt_valid_frac",
    "rech_valid_frac",
    "bcmv8_recharge_note"
  )
  
  optional_cols <- c("bcmv8_recharge_note")
  required_cols <- setdiff(keep_cols, optional_cols)
  
  missing_cols <- setdiff(required_cols, names(climate_tbl))
  
  if (length(missing_cols) > 0) {
    warning(
      "HUC", huc_level, " climate table is missing expected column(s): ",
      paste(missing_cols, collapse = ", "),
      ". Skipping climate join."
    )
    return(huc_sf)
  }
  
  climate_tbl <- climate_tbl |>
    dplyr::select(dplyr::any_of(keep_cols)) |>
    dplyr::distinct(.data[[code_col]], .keep_all = TRUE)
  
  huc_sf <- huc_sf |>
    dplyr::left_join(
      climate_tbl,
      by = code_col
    )
  
  huc_sf$popup_html <- pt_make_huc_popups(
    sfobj = huc_sf,
    lvl = huc_level
  )
  
  huc_sf
}

for (lvl in c(2, 4, 6, 8, 10, 12)) {
  
  nm <- paste0("huc", lvl)
  
  if (nm %in% names(huc_map)) {
    huc_map[[nm]] <- pt_join_huc_climate_table(
      huc_sf = huc_map[[nm]],
      huc_level = lvl
    )
  }
}

# ---- 8B. Add HUC thematic fill color columns --------------------------------
##
## PURPOSE:
##   Add lightweight precomputed thematic color columns to the existing HUC map
##   layers. The color breaks are computed separately for each HUC level so HUC12
##   values are not forced into bins designed for much larger watersheds.
##
## DESIGN:
##   - Precipitation metrics use a blue sequential scale.
##   - Recharge metrics use a green sequential scale.
##   - Each HUC level gets its own quantile-based bins for each metric.
##   - The final Leaflet dropdown restyles the existing HUC polygons in place;
##     no duplicate HUC geometries are created.
##
## CREATED FIELDS:
##   pt_is_huc
##   pt_huc_level
##   ppt_in_fill_col,   ppt_in_fill_label,   ppt_in_bin
##   ppt_kaf_fill_col,  ppt_kaf_fill_label,  ppt_kaf_bin
##   rech_in_fill_col,  rech_in_fill_label,  rech_in_bin
##   rech_kaf_fill_col, rech_kaf_fill_label, rech_kaf_bin
##
## IMPORTANT INTERPRETATION:
##   Colors are scaled within each HUC level. This makes each level readable,
##   especially HUC12, but colors should not be interpreted as directly
##   comparable across HUC levels without reading the active legend.

pt_theme_palette <- function(kind, n) {
  
  if (n <= 0) {
    return(character(0))
  }
  
  if (kind == "blue") {
    ## Dry-to-wet, multi-hue ramp.  White is reserved for no-data / no-fill.
    ## The light warm low end helps very dry HUCs remain visible while the
    ## blue/purple high end reads as wetter annual PRISM precipitation.
    base_cols <- c(
      "#fff7ec", "#fee8c8", "#d9f0d3", "#a6dba0",
      "#67a9cf", "#2c7fb8", "#253494", "#542788"
    )
  } else if (kind == "green") {
    ## Low recharge starts pale yellow-green, then moves through green into
    ## teal/blue for the wettest/highest-recharge bins.  White remains no-data.
    base_cols <- c(
      "#ffffe5", "#d9f0a3", "#addd8e", "#78c679",
      "#31a354", "#006837", "#1d91c0", "#084081"
    )
  } else {
    stop("Unknown HUC theme palette kind: ", kind)
  }
  
  grDevices::colorRampPalette(base_cols)(n)
}

pt_format_theme_value <- function(x, digits = 1) {
  
  if (is.na(x)) {
    return("NA")
  }
  
  if (abs(x) >= 1000) {
    return(formatC(x, format = "f", digits = 0, big.mark = ","))
  }
  
  if (abs(x) >= 100 && digits <= 0) {
    return(formatC(x, format = "f", digits = 0, big.mark = ","))
  }
  
  formatC(x, format = "f", digits = digits, big.mark = ",")
}

pt_format_theme_range <- function(lo, hi, digits = 1, max_digits = 3) {
  
  if (is.na(lo) || is.na(hi)) {
    return("NA")
  }
  
  if (isTRUE(all.equal(lo, hi, tolerance = 1e-12))) {
    return(pt_format_theme_value(lo, digits = max(digits, 0)))
  }
  
  for (d in seq(max(digits, 0), max_digits)) {
    lo_txt <- pt_format_theme_value(lo, digits = d)
    hi_txt <- pt_format_theme_value(hi, digits = d)
    if (!identical(lo_txt, hi_txt)) {
      return(paste0(lo_txt, " - ", hi_txt))
    }
  }
  
  paste0(
    signif(lo, digits = 4),
    " - ",
    signif(hi, digits = 4)
  )
}

pt_huc_theme_probs <- function(n_intervals) {
  
  ## Use a mild upper-tail emphasis instead of evenly spaced quantiles.  This
  ## avoids spending too many legend rows on near-zero bins for HUC10/HUC12
  ## recharge while still keeping the map readable across skewed values.
  if (n_intervals <= 1) {
    return(c(0, 1))
  }
  
  base <- c(0, 0.10, 0.25, 0.40, 0.55, 0.70, 0.82, 0.90, 0.96, 1.00)
  stats::approx(
    x = seq(0, 1, length.out = length(base)),
    y = base,
    xout = seq(0, 1, length.out = n_intervals + 1),
    ties = "ordered"
  )$y
}

pt_huc_theme_breaks <- function(vals, n_intervals) {
  
  probs <- pt_huc_theme_probs(n_intervals)
  breaks <- as.numeric(stats::quantile(vals, probs = probs, na.rm = TRUE, type = 8))
  breaks <- sort(unique(breaks))
  
  ## If repeated quantiles collapsed too far, fall back to pretty breaks.
  if (length(breaks) < 3) {
    breaks <- pretty(range(vals, na.rm = TRUE), n = min(n_intervals, 6))
    breaks <- breaks[
      breaks >= min(vals, na.rm = TRUE) &
        breaks <= max(vals, na.rm = TRUE)
    ]
    breaks <- sort(unique(c(
      min(vals, na.rm = TRUE),
      breaks,
      max(vals, na.rm = TRUE)
    )))
  }
  
  breaks
}

pt_make_huc_level_theme <- function(x, n_bins = 9, palette_kind = "blue", digits = 1) {
  
  x_num <- suppressWarnings(as.numeric(x))
  ok <- is.finite(x_num)
  
  out <- tibble::tibble(
    fill_col = rep("#FFFFFF", length(x_num)),
    fill_label = rep("No data", length(x_num)),
    bin_id = rep(NA_integer_, length(x_num))
  )
  
  if (sum(ok) == 0) {
    return(out)
  }
  
  vals <- x_num[ok]
  vals_unique <- sort(unique(vals))
  
  if (length(vals_unique) == 1) {
    one_col <- pt_theme_palette(palette_kind, 3)[2]
    one_lab <- pt_format_theme_value(vals_unique[1], digits = digits)
    out$fill_col[ok] <- one_col
    out$fill_label[ok] <- one_lab
    out$bin_id[ok] <- 1L
    return(out)
  }
  
  zero_like <- ok & x_num == 0
  positive <- ok & x_num > 0
  has_zero_bin <- any(zero_like) && any(positive)
  
  if (has_zero_bin) {
    pos_vals <- x_num[positive]
    n_pos_bins <- min(max(n_bins - 1, 1), length(unique(pos_vals)))
    pos_breaks <- pt_huc_theme_breaks(pos_vals, n_pos_bins)
    
    if (length(pos_breaks) < 2) {
      pos_breaks <- range(pos_vals, na.rm = TRUE)
    }
    
    cut_breaks <- pos_breaks
    cut_breaks[1] <- -Inf
    cut_breaks[length(cut_breaks)] <- Inf
    
    pos_bin <- cut(
      x_num[positive],
      breaks = cut_breaks,
      include.lowest = TRUE,
      right = FALSE,
      labels = FALSE
    )
    
    n_cols <- length(pos_breaks)
    cols <- pt_theme_palette(palette_kind, n_cols)
    
    labels <- c(
      "0",
      vapply(seq_len(length(pos_breaks) - 1), function(i) {
        pt_format_theme_range(pos_breaks[i], pos_breaks[i + 1], digits = digits)
      }, character(1))
    )
    
    out$fill_col[zero_like] <- cols[1]
    out$fill_label[zero_like] <- labels[1]
    out$bin_id[zero_like] <- 1L
    
    ok_pos_bin <- !is.na(pos_bin)
    pos_indices <- which(positive)
    out$fill_col[pos_indices[ok_pos_bin]] <- cols[pos_bin[ok_pos_bin] + 1L]
    out$fill_label[pos_indices[ok_pos_bin]] <- labels[pos_bin[ok_pos_bin] + 1L]
    out$bin_id[pos_indices[ok_pos_bin]] <- as.integer(pos_bin[ok_pos_bin] + 1L)
    
    return(out)
  }
  
  ## Use fewer bins than observations where needed, but use more bins than the
  ## original 7-bin display so high-end values do not disappear into one very
  ## broad terminal class.
  n_bins_eff <- min(n_bins, length(vals_unique))
  raw_breaks <- pt_huc_theme_breaks(vals, n_bins_eff)
  
  if (length(raw_breaks) < 2) {
    one_col <- pt_theme_palette(palette_kind, 3)[2]
    out$fill_col[ok] <- one_col
    out$fill_label[ok] <- pt_format_theme_value(mean(vals, na.rm = TRUE), digits = digits)
    out$bin_id[ok] <- 1L
    return(out)
  }
  
  cut_breaks <- raw_breaks
  cut_breaks[1] <- -Inf
  cut_breaks[length(cut_breaks)] <- Inf
  
  bin_id <- cut(
    x_num,
    breaks = cut_breaks,
    include.lowest = TRUE,
    right = FALSE,
    labels = FALSE
  )
  
  n_cols <- length(raw_breaks) - 1
  cols <- pt_theme_palette(palette_kind, n_cols)
  
  labels <- vapply(seq_len(n_cols), function(i) {
    pt_format_theme_range(raw_breaks[i], raw_breaks[i + 1], digits = digits)
  }, character(1))
  
  ok_bin <- !is.na(bin_id)
  out$fill_col[ok_bin] <- cols[bin_id[ok_bin]]
  out$fill_label[ok_bin] <- labels[bin_id[ok_bin]]
  out$bin_id[ok_bin] <- as.integer(bin_id[ok_bin])
  
  out
}

pt_add_huc_theme_cols <- function(x, huc_level) {
  
  x$pt_is_huc <- TRUE
  x$pt_huc_level <- huc_level
  
  theme_specs <- list(
    ppt_in = list(
      value_col = "map_in",
      fill_col = "ppt_in_fill_col",
      label_col = "ppt_in_fill_label",
      bin_col = "ppt_in_bin",
      palette = "blue",
      digits = 1
    ),
    ppt_kaf = list(
      value_col = "ppt_kaf",
      fill_col = "ppt_kaf_fill_col",
      label_col = "ppt_kaf_fill_label",
      bin_col = "ppt_kaf_bin",
      palette = "blue",
      digits = 0
    ),
    rech_in = list(
      value_col = "rech_in",
      fill_col = "rech_in_fill_col",
      label_col = "rech_in_fill_label",
      bin_col = "rech_in_bin",
      palette = "green",
      digits = 1
    ),
    rech_kaf = list(
      value_col = "rech_kaf",
      fill_col = "rech_kaf_fill_col",
      label_col = "rech_kaf_fill_label",
      bin_col = "rech_kaf_bin",
      palette = "green",
      digits = 0
    )
  )
  
  for (spec in theme_specs) {
    
    if (spec$value_col %in% names(x)) {
      theme_tbl <- pt_make_huc_level_theme(
        x = x[[spec$value_col]],
        n_bins = 9,
        palette_kind = spec$palette,
        digits = spec$digits
      )
    } else {
      theme_tbl <- tibble::tibble(
        fill_col = rep("#FFFFFF", nrow(x)),
        fill_label = rep("No data", nrow(x)),
        bin_id = rep(NA_integer_, nrow(x))
      )
    }
    
    x[[spec$fill_col]] <- theme_tbl$fill_col
    x[[spec$label_col]] <- theme_tbl$fill_label
    x[[spec$bin_col]] <- theme_tbl$bin_id
  }
  
  x
}

huc_map <- purrr::imap(huc_map, function(x, nm) {
  
  huc_level <- as.integer(gsub("^huc", "", nm))
  
  pt_add_huc_theme_cols(
    x = x,
    huc_level = huc_level
  )
})

gw_map$popup_html <- pt_make_gw_popups(gw_map)
county_map$popup_html <- pt_make_county_popups(county_map)

