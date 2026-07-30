# ==== leaflet_huc_theme_helpers.r ===========================================
##
## PURPOSE:
##   Build and inject the payload for the unified HUC thematic card.
##
## DESIGN:
##   The final map builder should assemble the map, not carry hundreds of
##   lines of HUC-specific UI and JavaScript. This helper keeps the existing
##   behavior but makes the HUC theme system easier to maintain.

pt_huc_group_name <- function(nm) {
  pt_layer_group_name(
    pt_note_group_name(paste0(toupper(nm), " **"))
  )
}

PT_HUC_THEME_REGISTRY <- list(
  none = list(label = "Boundaries only (no fill)"),
  blm_pct = list(label = "BLM-managed land \u2014 %"),
  ppt_in = list(label = "PRISM precip - in/yr"),
  ppt_kaf = list(label = "PRISM precip - kaf/yr"),
  rech_in = list(label = "BCMv8 recharge - in/yr"),
  rech_kaf = list(label = "BCMv8 recharge - kaf/yr")
)

PT_HUC_DEFAULT_THEME <- "none"

pt_huc_theme_col <- function(df, nm, default = "#FFFFFF") {
  if (nm %in% names(df)) {
    out <- as.character(df[[nm]])
    out[is.na(out) | out == ""] <- default
    return(out)
  }
  rep(default, nrow(df))
}

pt_huc_theme_label_col <- function(df, nm, default = "No data") {
  if (nm %in% names(df)) {
    out <- as.character(df[[nm]])
    out[is.na(out) | out == ""] <- default
    return(out)
  }
  rep(default, nrow(df))
}

pt_huc_theme_bin_col <- function(df, nm) {
  if (nm %in% names(df)) {
    return(suppressWarnings(as.integer(df[[nm]])))
  }
  rep(NA_integer_, nrow(df))
}

pt_huc_legend_one <- function(df, huc_layer, theme_key, title, fill_col, label_col, bin_col) {

  if (!all(c(fill_col, label_col, bin_col) %in% names(df))) {
    return(list(
      title = paste(toupper(huc_layer), title),
      rows = list()
    ))
  }

  legend_df <- df |>
    dplyr::transmute(
      bin = pt_huc_theme_bin_col(df, bin_col),
      color = pt_huc_theme_col(df, fill_col),
      label = pt_huc_theme_label_col(df, label_col)
    ) |>
    dplyr::filter(
      !is.na(.data$bin),
      !is.na(.data$color),
      .data$color != "#FFFFFF",
      !is.na(.data$label),
      .data$label != "No data"
    ) |>
    dplyr::distinct(.data$bin, .data$color, .data$label) |>
    dplyr::arrange(.data$bin)

  rows <- lapply(seq_len(nrow(legend_df)), function(i) {
    list(
      color = legend_df$color[i],
      label = legend_df$label[i]
    )
  })

  list(
    title = paste(toupper(huc_layer), title),
    theme = theme_key,
    rows = rows
  )
}

pt_huc_blm_legend_one <- function(df, huc_layer) {
  if (!"percentBLMland" %in% names(df)) {
    stop(
      toupper(huc_layer),
      " thematic cache is missing percentBLMland. Rebuild the core cache.",
      call. = FALSE
    )
  }

  percent_blm <- pt_blm_pct_values(
    df$percentBLMland,
    paste(toupper(huc_layer), "percentBLMland values")
  )

  list(
    title = paste(toupper(huc_layer), "BLM-managed land \u2014 %"),
    theme = "blm_pct",
    rows = pt_blm_pct_legend_rows(percent_blm)
  )
}

pt_build_huc_theme_data <- function(huc_all) {

  huc_theme_df <- purrr::imap_dfr(huc_all, function(x, nm) {

    df <- sf::st_drop_geometry(x)
    code_col <- nm

    if (!code_col %in% names(df)) {
      return(tibble::tibble())
    }
    if (!"percentBLMland" %in% names(df)) {
      stop(
        toupper(nm),
        " thematic cache is missing percentBLMland. Rebuild the core cache.",
        call. = FALSE
      )
    }

    percent_blm <- pt_blm_pct_values(
      df$percentBLMland,
      paste(toupper(nm), "percentBLMland values")
    )
    blm_bins <- as.character(pt_blm_pct_bin(percent_blm))

    tibble::tibble(
      layer_id = paste0(nm, "_", as.character(df[[code_col]])),
      huc_layer = nm,
      huc_label = toupper(nm),
      percent_blm = percent_blm,
      blm_pct = unname(PT_BLM_PCT_COLORS[blm_bins]),
      ppt_in   = pt_huc_theme_col(df, "ppt_in_fill_col"),
      ppt_kaf  = pt_huc_theme_col(df, "ppt_kaf_fill_col"),
      rech_in  = pt_huc_theme_col(df, "rech_in_fill_col"),
      rech_kaf = pt_huc_theme_col(df, "rech_kaf_fill_col")
    )
  })

  if (anyDuplicated(huc_theme_df$layer_id)) {
    duplicate_ids <- unique(huc_theme_df$layer_id[duplicated(huc_theme_df$layer_id)])
    stop(
      "HUC theme layer IDs must be unique. Duplicate examples: ",
      paste(utils::head(duplicate_ids, 5), collapse = ", ")
    )
  }

  # Do not pass named vectors/lists directly into htmlwidgets::onRender().
  # Explicit records avoid jsonlite keep_vec_names warnings.
  huc_theme_lookup <- lapply(seq_len(nrow(huc_theme_df)), function(i) {
    list(
      layer_id  = as.character(huc_theme_df$layer_id[i]),
      huc_layer = as.character(huc_theme_df$huc_layer[i]),
      huc_label = as.character(huc_theme_df$huc_label[i]),
      percent_blm = if (is.na(huc_theme_df$percent_blm[i])) {
        NULL
      } else {
        unname(huc_theme_df$percent_blm[i])
      },
      blm_pct   = as.character(huc_theme_df$blm_pct[i]),
      ppt_in    = as.character(huc_theme_df$ppt_in[i]),
      ppt_kaf   = as.character(huc_theme_df$ppt_kaf[i]),
      rech_in   = as.character(huc_theme_df$rech_in[i]),
      rech_kaf  = as.character(huc_theme_df$rech_kaf[i])
    )
  })

  huc_theme_lookup <- unname(huc_theme_lookup)

  huc_theme_legends_nested <- purrr::imap(huc_all, function(x, nm) {

    df <- sf::st_drop_geometry(x)

    list(
      blm_pct = pt_huc_blm_legend_one(
        df = df,
        huc_layer = nm
      ),
      ppt_in = pt_huc_legend_one(
        df = df,
        huc_layer = nm,
        theme_key = "ppt_in",
        title = "PRISM precip, in/yr",
        fill_col = "ppt_in_fill_col",
        label_col = "ppt_in_fill_label",
        bin_col = "ppt_in_bin"
      ),
      ppt_kaf = pt_huc_legend_one(
        df = df,
        huc_layer = nm,
        theme_key = "ppt_kaf",
        title = "PRISM precip volume, kaf/yr",
        fill_col = "ppt_kaf_fill_col",
        label_col = "ppt_kaf_fill_label",
        bin_col = "ppt_kaf_bin"
      ),
      rech_in = pt_huc_legend_one(
        df = df,
        huc_layer = nm,
        theme_key = "rech_in",
        title = "BCMv8 recharge, in/yr",
        fill_col = "rech_in_fill_col",
        label_col = "rech_in_fill_label",
        bin_col = "rech_in_bin"
      ),
      rech_kaf = pt_huc_legend_one(
        df = df,
        huc_layer = nm,
        theme_key = "rech_kaf",
        title = "BCMv8 recharge volume, kaf/yr",
        fill_col = "rech_kaf_fill_col",
        label_col = "rech_kaf_fill_label",
        bin_col = "rech_kaf_bin"
      )
    )
  })

  huc_theme_legend_records <- purrr::flatten(
    purrr::imap(huc_theme_legends_nested, function(theme_list, huc_layer) {
      purrr::imap(theme_list, function(legend, theme_name) {
        list(
          huc_layer = as.character(huc_layer),
          theme = as.character(theme_name),
          legend = legend
        )
      })
    })
  )

  huc_level_records <- purrr::imap(huc_all, function(x, nm) {
    list(
      huc_layer = as.character(nm),
      huc_label = toupper(as.character(nm)),
      group_name = pt_huc_group_name(nm),
      expected_count = nrow(x)
    )
  })

  huc_theme_records <- lapply(names(PT_HUC_THEME_REGISTRY), function(theme_id) {
    list(
      id = theme_id,
      label = PT_HUC_THEME_REGISTRY[[theme_id]]$label
    )
  })

  list(
    data = list(
      default_theme = PT_HUC_DEFAULT_THEME,
      themes = unname(huc_theme_records),
      lookup = unname(huc_theme_lookup),
      legends = unname(huc_theme_legend_records),
      levels = unname(huc_level_records)
    ),
    lookup_count = length(huc_theme_lookup),
    legend_levels = names(huc_theme_legends_nested)
  )
}

pt_add_huc_theme_controls <- function(m, huc_all, js_path = "03_functions/js/brim_huc_theme_control.js") {

  huc_theme <- pt_build_huc_theme_data(huc_all)

  message("HUC theme lookup rows: ", huc_theme$lookup_count)
  message("HUC theme legend levels: ", paste(huc_theme$legend_levels, collapse = ", "))

  if (!file.exists(js_path)) {
    stop("HUC theme JavaScript file not found: ", js_path)
  }

  huc_js <- paste(readLines(js_path, warn = FALSE), collapse = "\n")

  htmlwidgets::onRender(
    m,
    huc_js,
    data = huc_theme$data
  )
}
