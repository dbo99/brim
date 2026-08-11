# Focused source/runtime contracts for Local Reference Phase 5 National Monuments.

suppressPackageStartupMessages({
  library(sf)
  library(htmltools)
})

source("00_config/config_local_reference_interactions.r")
source("00_config/config_labels.r")
source("03_functions/label_helpers.r")
source("03_functions/local_reference_interaction_helpers.r")

pt_validate_local_reference_config()
pt_validate_local_reference_nm_research()

registry <- pt_local_reference_config_row(layer_id = "national_monuments")
stopifnot(
  identical(registry$implementation_status[[1]], "phase5_national_monuments"),
  identical(
    registry$color_basis[[1]],
    "verified_administering_agency_component_or_shared_boundary"
  ),
  isTRUE(registry$feature_selection_supported[[1]]),
  isTRUE(registry$auto_zoom_supported[[1]]),
  isTRUE(registry$retention_enabled[[1]]),
  identical(registry$popup_layout[[1]], "tabbed_card"),
  !isTRUE(registry$category_filter_visible[[1]])
)

render_helper_path <- file.path(
  "03_functions", "leaflet_layer_local_reference_helpers.r"
)
render_helper <- paste(readLines(render_helper_path, warn = FALSE), collapse = "\n")
stopifnot(
  grepl(
    'nm %in% c\\(\\s*"trails", "monuments", "cadesert_ncl", "wildernessstudyarea",\\s*"fedwilderness", "acec"\\s*\\)',
    render_helper
    , perl = TRUE
  ),
  grepl("pt_nps_context_hover_html <- function", render_helper, fixed = TRUE),
  grepl("legislative_boundary_area_sq_mi", render_helper, fixed = TRUE),
  grepl('~", area, " mi²', render_helper, fixed = TRUE),
  !grepl(
    'className = "pt-nps-context-hover-tooltip",\\s*style = list\\([^)]*"min-width"',
    render_helper,
    perl = TRUE
  ),
  grepl(
    "land$hover_html <- pt_nps_context_hover_html(land)",
    render_helper,
    fixed = TRUE
  ),
  grepl(
    "boundary$hover_html <- pt_nps_context_hover_html(boundary)",
    render_helper,
    fixed = TRUE
  ),
  !grepl(" · NPS land / interest</div>", render_helper, fixed = TRUE),
  !grepl(" · legislative boundary (not ownership)</div>", render_helper, fixed = TRUE)
)

map_builder <- paste(readLines(
  file.path("05_map_build", "04_build_portatreasure2_core_map.r"),
  warn = FALSE
), collapse = "\n")
stopifnot(
  grepl('semantic_id_field = "pt_feature_id"', map_builder, fixed = TRUE),
  grepl('"nps_park_preserve_context_map.rds"', map_builder, fixed = TRUE),
  grepl("nps_context = layers$nps_park_preserve_context", map_builder, fixed = TRUE),
  grepl(
    '"Reference – National Monuments" = pt_count_reference_layer_rows(',
    map_builder,
    fixed = TRUE
  )
)

categories <- pt_local_reference_categories("national_monuments")
stopifnot(
  identical(
    categories$category_key,
    c("blm", "usfs", "nps", "fws", "shared_multi", "unknown")
  ),
  identical(
    categories$fill_color[1:4],
    c("#B8860B", "#228B22", "#54278F", "#1F78B4")
  ),
  identical(categories$fill_color[[5]], "#92962A"),
  identical(categories$stroke_color[[5]], "#766717"),
  identical(
    categories$fill_color[1:4],
    pt_local_reference_accepted_agency_color(c("blm", "usfs", "nps", "fws"))
  ),
  identical(
    categories$fill_color[[5]],
    pt_local_reference_accepted_agency_color("blm_usfs_shared")
  ),
  identical(categories$dash_array[[5]], "6,3"),
  !any(categories$provisional)
)

facets <- registry$filter_facets[[1]]
quick_views <- registry$quick_views[[1]]
stopifnot(
  identical(
    vapply(facets, `[[`, character(1), "facet_key"),
    c(
      "administering_agency", "designation_authority",
      "management_pattern", "recent_change", "blm_usfs_quick_view"
    )
  ),
  identical(
    facets[[1]]$values$swatch_color,
    c("#B8860B", "#228B22", "#54278F", "#1F78B4")
  ),
  all(!vapply(facets[1:4], `[[`, logical(1), "collapsible")),
  !isTRUE(facets[[5]]$visible),
  identical(facets[[3]]$values$label[[2]], "Shared / multi-agency"),
  identical(
    vapply(quick_views, `[[`, character(1), "quick_view_key"),
    c("blm_involved", "shared_blm_usfs", "recent_2024_2025")
  ),
  !isTRUE(registry$distinguish_units_supported[[1]])
)

controller_source <- paste(readLines(
  file.path("03_functions", "js", "brim_local_reference_controller.js"),
  warn = FALSE
), collapse = "\n")
stopifnot(
  grepl("var nationalMonuments", controller_source, fixed = TRUE),
  grepl("pt-nm-shared-boundary-row", controller_source, fixed = TRUE),
  grepl("Shared whole boundary:", controller_source, fixed = TRUE),
  grepl("swatch(sharedCategory)", controller_source, fixed = TRUE),
  grepl("pt-nm-count-cue", controller_source, fixed = TRUE),
  grepl("counts: matching / total", controller_source, fixed = TRUE),
  grepl("matching current results;", controller_source, fixed = TRUE),
  grepl("setAttribute('title', countDescription)", controller_source, fixed = TRUE),
  !grepl("Map colors", controller_source, fixed = TRUE),
  !grepl("pt-nm-map-key", controller_source, fixed = TRUE),
  !grepl("pt-nm-map-display", controller_source, fixed = TRUE),
  !grepl("border:2px dashed #766717", controller_source, fixed = TRUE),
  grepl("var npsContextData", controller_source, fixed = TRUE),
  grepl("data-pt-nm-context", controller_source, fixed = TRUE),
  grepl("function resetNpsContext", controller_source, fixed = TRUE),
  grepl("Context only; it does not change", controller_source, fixed = TRUE),
  grepl("pt-nps-context-hover-tooltip", controller_source, fixed = TRUE),
  grepl(
    "pt-nps-context-hover-tooltip\\{[^}]*width:max-content!important;[^}]*max-width:min\\(320px",
    controller_source,
    perl = TRUE
  ),
  !grepl(
    "pt-nps-context-hover-tooltip\\{[^}]*min-width",
    controller_source,
    perl = TRUE
  ),
  grepl('left:7px', controller_source, fixed = TRUE),
  grepl('padding:3px 4px 3px 29px', controller_source, fixed = TRUE)
)

registration <- pt_local_reference_label_registration(
  source_nickname = "monuments"
)
stopifnot(
  identical(registration$label_id[[1]], "monuments"),
  identical(
    registration$anchor_strategy[[1]],
    "polygon_visible_component_point_on_surface"
  ),
  isTRUE(registration$visible_component_aware[[1]])
)

candidate_path <- Sys.getenv("BRIM_NM_CANDIDATE_RDS", unset = "")
if (nzchar(candidate_path)) {
  if (!file.exists(candidate_path)) stop("BRIM_NM_CANDIDATE_RDS does not exist.")
  candidate <- readRDS(candidate_path)
  prepared <- pt_prepare_local_reference_national_monuments(
    candidate,
    validate_snapshot = TRUE,
    build_display = TRUE
  )
  qa <- stats::setNames(
    as.integer(pt_local_reference_nm_qa(prepared)$value),
    pt_local_reference_nm_qa(prepared)$metric
  )
  stopifnot(
    identical(unname(qa[["semantic_monuments"]]), 20L),
    identical(unname(qa[["display_geometry_records"]]), 22L),
    identical(unname(qa[["polygon_parts"]]), 24432L),
    identical(unname(qa[["blm_involved"]]), 9L),
    identical(unname(qa[["usfs_involved"]]), 7L),
    identical(unname(qa[["nps_involved"]]), 7L),
    identical(unname(qa[["usfws_involved"]]), 1L),
    identical(unname(qa[["presidential_proclamation"]]), 17L),
    identical(unname(qa[["act_of_congress"]]), 3L),
    identical(
      length(unique(prepared$pt_local_reference_semantic_key[
        prepared$pt_nm_management_pattern == "single_agency"
      ])),
      16L
    ),
    identical(unname(qa[["shared_multi_agency"]]), 4L),
    identical(unname(qa[["shared_blm_usfs"]]), 3L),
    identical(unname(qa[["recent_2024_2025"]]), 4L),
    identical(
      as.integer(table(factor(
        prepared$pt_local_reference_category_key,
        levels = c("blm", "usfs", "nps", "fws", "shared_multi")
      ))),
      c(7L, 5L, 7L, 1L, 2L)
    ),
    identical(
      sort(prepared$pt_nm_display_agency_key[
        prepared$monument_id == "nm_ca_sand_to_snow"
      ]),
      c("blm", "usfs")
    ),
    identical(
      prepared$pt_nm_display_agency_key[
        prepared$monument_id == "nm_ca_berryessa_snow_mountain"
      ],
      "shared_multi"
    ),
    identical(
      prepared$pt_nm_display_agency_key[
        prepared$monument_id == "nm_ca_santa_rosa_san_jacinto"
      ],
      "shared_multi"
    ),
    identical(
      prepared$pt_nm_display_agency_key[
        prepared$monument_id == "nm_ca_carrizo_plain"
      ],
      "blm"
    ),
    identical(
      prepared$pt_nm_display_agency_key[
        prepared$monument_id == "nm_ca_giant_sequoia"
      ],
      "usfs"
    ),
    identical(
      prepared$pt_nm_display_agency_key[
        prepared$monument_id == "nm_ca_cabrillo"
      ],
      "nps"
    ),
    identical(
      sort(prepared$pt_nm_display_agency_key[
        prepared$monument_id == "nm_ca_tule_lake"
      ]),
      c("fws", "nps")
    ),
    all(
      prepared$pt_nm_administering_agencies[
        prepared$monument_id == "nm_ca_tule_lake"
      ] == "nps|usfws"
    ),
    !any(
      prepared$pt_local_reference_category_key == "nps" &
        !grepl(
          "(^|\\|)nps(\\||$)",
          prepared$pt_nm_administering_agencies
        )
    ),
    all(grepl(
      "Selected component:",
      prepared$pt_reference_hover_html[
        prepared$monument_id %in% c("nm_ca_sand_to_snow", "nm_ca_tule_lake")
      ],
      fixed = TRUE
    )),
    all(grepl(
      "Agency-administered component",
      prepared$popup_html[prepared$monument_id == "nm_ca_sand_to_snow"],
      fixed = TRUE
    )),
    all(grepl(
      "Complete shared monument boundary",
      prepared$pt_reference_hover_html[
        prepared$monument_id %in% c(
          "nm_ca_berryessa_snow_mountain",
          "nm_ca_santa_rosa_san_jacinto"
        )
      ],
      fixed = TRUE
    )),
    all(nzchar(prepared$pt_reference_label_text)),
    all(grepl("data-pt-lr-tabbed-popup", prepared$popup_html, fixed = TRUE)),
    !any(grepl("&lt;a", prepared$popup_html, fixed = TRUE)),
    !any(sf::st_is_empty(prepared)),
    all(sf::st_is_valid(prepared))
  )
  labels <- pt_make_local_reference_labels(prepared, registration)
  stopifnot(
    inherits(labels, "sf"),
    nrow(labels) == 22L,
    length(unique(labels$semantic_feature_key)) == 20L
  )
  pt_validate_local_reference_label_anchors(labels, prepared, registration)
  payload <- pt_local_reference_controller_payload(
    list(monuments = prepared),
    list(monuments = labels)
  )
  stopifnot(
    length(payload) == 1L,
    identical(payload[[1]]$layer_id[[1]], "national_monuments"),
    length(payload[[1]]$records) == 22L,
    length(payload[[1]]$features) == 20L,
    length(payload[[1]]$facets) == 5L,
    sum(vapply(payload[[1]]$facets, `[[`, logical(1), "visible")) == 4L,
    length(payload[[1]]$quick_views) == 3L,
    identical(payload[[1]]$semantic_labels$semantic_feature_count, 20L),
    identical(payload[[1]]$semantic_labels$anchor_count, 22L),
    is.null(payload[[1]]$acec)
  )
}

context_candidate_path <- Sys.getenv(
  "BRIM_NPS_PARK_PRESERVE_CONTEXT_RDS", unset = ""
)
if (nzchar(context_candidate_path)) {
  if (!file.exists(context_candidate_path)) {
    stop("BRIM_NPS_PARK_PRESERVE_CONTEXT_RDS does not exist.")
  }
  context_candidate <- readRDS(context_candidate_path)
  pt_validate_local_reference_nps_context(context_candidate)
  context_payload <- pt_local_reference_nps_context_payload(context_candidate)
  stopifnot(
    identical(names(context_payload), c("national_park", "national_preserve")),
    identical(context_payload$national_park$unit_count, 9L),
    identical(context_payload$national_park$expected_layer_count, 18L),
    identical(context_payload$national_preserve$unit_count, 1L),
    identical(context_payload$national_preserve$expected_layer_count, 2L),
    all(vapply(
      context_candidate,
      function(layer) nrow(layer) == 10L &&
        setequal(layer$unit_code, c(
          "CHIS", "DEVA", "JOTR", "KICA", "LAVO",
          "MOJA", "PINN", "REDW", "SEQU", "YOSE"
        )),
      logical(1)
    )),
    identical(
      context_candidate$boundaries$states[
        context_candidate$boundaries$unit_code == "DEVA"
      ],
      "CA-NV"
    )
  )
}

cat("Local Reference Phase 5 National Monuments tests passed.\n")
