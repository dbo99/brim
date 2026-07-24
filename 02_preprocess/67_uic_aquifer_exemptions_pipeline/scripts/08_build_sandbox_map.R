# Standalone comparison map for candidate or approved UIC products.

uic_build_sandbox_map <- function(candidate_id = NULL, data_variant = c("candidate", "approved")) {
  data_variant <- match.arg(data_variant)
  uic_require_packages(c("leaflet", "htmlwidgets", "htmltools", "sf"))
  display <- uic_read_config("display_config.csv")
  root <- if (identical(data_variant, "approved")) {
    UIC_APPROVED_DIR
  } else {
    if (is.null(candidate_id) || !nzchar(candidate_id)) {
      stop("Candidate sandbox build requires candidate_id.", call. = FALSE)
    }
    uic_candidate_paths(candidate_id)$root
  }
  output_dir <- file.path(UIC_QA_DIR, "sandbox_maps")
  uic_ensure_dirs(output_dir)
  map <- leaflet::leaflet(
    options = leaflet::leafletOptions(preferCanvas = TRUE)
  ) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
    leaflet::addScaleBar(position = "bottomright")
  groups <- character()
  for (i in seq_len(nrow(display))) {
    row <- display[i, , drop = FALSE]
    path <- file.path(root, "map_ready", paste0(row$source_key, "_map.rds"))
    if (!file.exists(path)) stop("Missing map-ready UIC cache: ", path)
    object <- readRDS(path)
    group <- paste0(row$display_name, " (", nrow(object), ")")
    groups <- c(groups, group)
    map <- leaflet::addPolygons(
      map,
      data = object,
      group = group,
      layerId = paste0(row$source_key, "::", object$source_id),
      color = row$color,
      weight = row$weight,
      dashArray = row$dash_array,
      fillColor = row$fill_color,
      fillOpacity = row$fill_opacity,
      opacity = 0.9,
      popup = lapply(object$popup_html, htmltools::HTML),
      label = lapply(object$hover_html, htmltools::HTML),
      highlightOptions = leaflet::highlightOptions(
        weight = row$hover_weight,
        fillOpacity = row$hover_fill_opacity,
        bringToFront = TRUE
      )
    )
  }
  map <- leaflet::addLayersControl(
    map,
    overlayGroups = groups,
    options = leaflet::layersControlOptions(collapsed = FALSE)
  ) |>
    leaflet::hideGroup(groups)
  output <- file.path(
    output_dir,
    paste0("uic_", data_variant, "_", candidate_id %||% "approved", ".html")
  )
  pandoc_available <- requireNamespace("rmarkdown", quietly = TRUE) &&
    nzchar(rmarkdown::find_pandoc()$dir %||% "")
  htmlwidgets::saveWidget(
    map,
    output,
    selfcontained = pandoc_available,
    libdir = if (pandoc_available) NULL else paste0(
      tools::file_path_sans_ext(basename(output)),
      "_files"
    )
  )
  if (!pandoc_available) {
    message(
      "Pandoc is unavailable; wrote portable sandbox HTML with a sibling ",
      "dependency directory instead of a single self-contained file."
    )
  }
  output
}
