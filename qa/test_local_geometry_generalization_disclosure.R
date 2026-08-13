#!/usr/bin/env Rscript

read_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

source("03_functions/polygon_generalization_helpers.r")
registry <- pt_polygon_generalization_read_registry()
inventory <- utils::read.csv(
  "08_docs/features/local_geometry_generalization_inventory.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = ""
)
inventory$public_disclosure[is.na(inventory$public_disclosure)] <- ""

warning_text <- "Check authoritative source for boundary-sensitive use."
disclosed <- registry$disclosure_required == "yes"
meter_rows <- disclosed &
  registry$parameter_type == "distance_tolerance_m" &
  !registry$layer_id %in% c("huc8", "huc10", "huc12", "rwqcb_regions")
meter_phrases <- paste0(
  sprintf("%g", as.numeric(registry$parameter_value[meter_rows])),
  " m tolerance"
)
vertex_rows <- disclosed & registry$parameter_type == "vertex_keep_fraction"
vertex_phrases <- paste0(
  sprintf("%g", 100 * as.numeric(registry$parameter_value[vertex_rows])),
  "% vertex-retention setting"
)
undisclosed_ids <- c("blm_ca_managed", "blm_held_managed_differences")
undisclosed_rows <- match(undisclosed_ids, registry$layer_id)
stopifnot(
  nrow(registry) == 29L,
  nrow(inventory) == 29L,
  identical(inventory$layer_id, registry$layer_id),
  identical(inventory$public_disclosure, registry$public_disclosure),
  all(endsWith(registry$public_disclosure[disclosed], warning_text)),
  all(!nzchar(registry$public_disclosure[!disclosed])),
  identical(registry$layer_id[undisclosed_rows], undisclosed_ids),
  identical(registry$disclosure_required[undisclosed_rows], c("no", "no")),
  identical(registry$public_disclosure[undisclosed_rows], c("", "")),
  identical(
    pt_polygon_generalization_public_note("rwqcb_regions"),
    paste(
      "Shared Regional Board boundaries generalized together at 100 m.",
      warning_text
    )
  ),
  all(mapply(grepl, meter_phrases, registry$public_disclosure[meter_rows],
    MoreArgs = list(fixed = TRUE)
  )),
  all(mapply(grepl, vertex_phrases, registry$public_disclosure[vertex_rows],
    MoreArgs = list(fixed = TRUE)
  )),
  identical(
    pt_polygon_generalization_public_note("acec"),
    paste("Display boundaries generalized with a 20 m tolerance.", warning_text)
  ),
  identical(
    pt_polygon_generalization_public_note("federal_wilderness"),
    paste("Display boundaries generalized with a 10 m tolerance.", warning_text)
  ),
  identical(
    pt_polygon_generalization_public_note("drecp"),
    paste("Display boundary generalized with a 100 m tolerance.", warning_text)
  ),
  all(vapply(c("huc8", "huc10", "huc12"), function(layer_id) {
    identical(
      pt_polygon_generalization_public_note(layer_id),
      paste0(
        "Shared ", toupper(layer_id), " boundaries generalized together at 200 m. ",
        warning_text
      )
    )
  }, logical(1))),
  all(!grepl(
    "retain detailed screening|optimized for responsive|retain useful local screening detail|reducing display complexity",
    registry$public_disclosure[disclosed],
    ignore.case = TRUE
  ))
)

ui_text <- paste(
  read_text("03_functions/leaflet_layer_local_reference_helpers.r"),
  read_text("03_functions/local_reference_interaction_helpers.r"),
  read_text("03_functions/leaflet_bulletin118_theme_helpers.r"),
  read_text("03_functions/leaflet_huc_theme_helpers.r"),
  read_text("03_functions/js/brim_local_reference_controller.js"),
  read_text("03_functions/js/brim_bulletin118_theme_control.js"),
  read_text("03_functions/js/brim_huc_theme_control.js")
)
stopifnot(
  !grepl("Generalized display geometry. Check authoritative", ui_text, fixed = TRUE),
  grepl("pt_polygon_generalization_public_note", ui_text, fixed = TRUE),
  grepl("pt_add_polygon_generalization_disclosure_control", ui_text, fixed = TRUE),
  grepl("generalization_disclosure", ui_text, fixed = TRUE),
  grepl("data-wsr-disclosure-source", ui_text, fixed = TRUE),
  grepl("activeState[key] ? 'block' : 'none'", ui_text, fixed = TRUE),
  !grepl("vertex retention|[0-9]+ m tolerance\\.", ui_text, perl = TRUE)
)

message(
  "Polygon disclosure tests passed: ", sum(disclosed),
  " layer-specific notes; ", sum(!disclosed), " intentionally undisclosed."
)
