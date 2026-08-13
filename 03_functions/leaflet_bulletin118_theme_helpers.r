# ==== leaflet_bulletin118_theme_helpers.r ===================================
##
## PURPOSE:
##   Build the compact browser payload and inject the unified thematic card for
##   the Local Bulletin 118 groundwater-basin layer.

if (!exists("pt_polygon_generalization_public_note", mode = "function")) {
  source("03_functions/polygon_generalization_helpers.r")
}

pt_build_bulletin118_theme_data <- function(gw) {
  required <- c(
    "subbasin_num",
    "sgma_2019_priority",
    "percentBLMland"
  )
  missing <- setdiff(required, names(gw))
  if (length(missing)) {
    stop(
      "Bulletin 118 thematic cache is missing field(s): ",
      paste(missing, collapse = ", "),
      ". Rebuild the core cache.",
      call. = FALSE
    )
  }
  if (nrow(gw) != PT_BULLETIN118_EXPECTED_ROWS) {
    stop(
      "Bulletin 118 thematic cache must contain 515 rows; found ",
      nrow(gw),
      ".",
      call. = FALSE
    )
  }

  keys <- pt_bulletin118_normalize_key(gw$subbasin_num)
  if (anyNA(keys) || any(keys == "") || anyDuplicated(keys)) {
    stop(
      "Bulletin 118 thematic cache requires 515 complete unique subbasin_num values.",
      call. = FALSE
    )
  }

  priorities <- pt_bulletin118_normalize_priority(gw$sgma_2019_priority)
  if (anyNA(priorities)) {
    stop(
      "Bulletin 118 thematic cache has missing/unrecognized SGMA priority values.",
      call. = FALSE
    )
  }
  priority_counts <- table(factor(
    priorities,
    levels = PT_BULLETIN118_PRIORITY_LEVELS
  ))
  if (!identical(
    unname(as.integer(priority_counts)),
    unname(as.integer(PT_BULLETIN118_PRIORITY_EXPECTED_COUNTS))
  )) {
    stop(
      "Bulletin 118 thematic cache does not reproduce the official ",
      "46/48/11/410 priority counts.",
      call. = FALSE
    )
  }

  percent_blm <- pt_blm_pct_values(
    gw$percentBLMland,
    "Bulletin 118 %BLM values"
  )
  blm_bins <- as.character(pt_blm_pct_bin(percent_blm))

  layer_ids <- pt_bulletin118_layer_id(keys)
  if (anyDuplicated(layer_ids)) {
    stop("Bulletin 118 browser layer IDs are not unique.", call. = FALSE)
  }

  value_or_blank <- function(field, i) {
    if (!field %in% names(gw) || is.na(gw[[field]][[i]])) {
      return("")
    }
    trimws(as.character(gw[[field]][[i]]))
  }

  records <- lapply(seq_along(layer_ids), function(i) {
    priority <- priorities[[i]]
    blm_bin <- blm_bins[[i]]
    list(
      layer_id = layer_ids[[i]],
      percent_blm = if (is.na(percent_blm[[i]])) {
        NULL
      } else {
        unname(percent_blm[[i]])
      },
      sgma_2019_priority = priority,
      sgma_color = unname(
        PT_BULLETIN118_PRIORITY_COLORS[[
          if (is.na(priority)) "No matched value" else priority
        ]]
      ),
      blm_bin = blm_bin,
      blm_color = unname(PT_BLM_PCT_COLORS[[
        if (is.na(blm_bin)) "Missing" else blm_bin
      ]]),
      display_label = value_or_blank("label", i),
      basin_name = value_or_blank("basin_name", i),
      subbasin_name = value_or_blank("subbasin_name", i),
      subbasin_num = keys[[i]],
      basin_num = value_or_blank("basin_num", i)
    )
  })

  priority_legend_levels <- c(
    PT_BULLETIN118_PRIORITY_LEVELS,
    "No matched value"
  )
  priority_legend_counts <- c(
    as.integer(priority_counts),
    sum(is.na(priorities))
  )
  priority_rows <- lapply(seq_along(priority_legend_levels), function(i) {
    level <- priority_legend_levels[[i]]
    list(
      color = unname(PT_BULLETIN118_PRIORITY_COLORS[[level]]),
      label = level,
      count = unname(as.integer(priority_legend_counts[[i]]))
    )
  })

  blm_rows <- pt_blm_pct_legend_rows(percent_blm)

  list(
    records = unname(records),
    group_name = unname(pt_layer_group_name("GW \u2013 Bull. 118")),
    expected_count = nrow(gw),
    default_theme = "basins",
    themes = list(
      basins = list(
        title = "Basins only",
        rows = list(list(
          color = PT_BULLETIN118_UNIFORM_FILL,
          label = "Bulletin 118 basins",
          count = nrow(gw)
        ))
      ),
      sgma_2019 = list(
        title = "DWR SGMA 2019 Basin Prioritization",
        rows = priority_rows,
        note = "Final 2019 DWR categories; counts describe the retained 515-basin snapshot."
      ),
      blm_pct = list(
        title = "BLM-managed land \u2014 %",
        rows = blm_rows
      )
    ),
    styles = list(
      basins = list(
        fill_color = PT_BULLETIN118_UNIFORM_FILL,
        fill_opacity = PT_BULLETIN118_UNIFORM_FILL_OPACITY
      ),
      sgma_2019 = list(fill_opacity = 0.48),
      blm_pct = list(fill_opacity = 0.58)
    ),
    generalization_disclosure =
      pt_polygon_generalization_public_note("bulletin118"),
    source_url = PT_BULLETIN118_SGMA_SOURCE_PAGE
  )
}

pt_add_bulletin118_theme_controls <- function(
  m,
  gw,
  js_path = "03_functions/js/brim_bulletin118_theme_control.js"
) {
  theme_data <- pt_build_bulletin118_theme_data(gw)
  if (!file.exists(js_path)) {
    stop("Bulletin 118 theme JavaScript file not found: ", js_path)
  }

  message(
    "Bulletin 118 thematic lookup rows: ",
    length(theme_data$records)
  )

  htmlwidgets::onRender(
    m,
    paste(readLines(js_path, warn = FALSE), collapse = "\n"),
    data = theme_data
  )
}
