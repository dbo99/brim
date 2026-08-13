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
stopifnot(
  nrow(registry) == 29L,
  nrow(inventory) == 29L,
  identical(inventory$layer_id, registry$layer_id),
  identical(inventory$public_disclosure, registry$public_disclosure),
  all(endsWith(registry$public_disclosure[disclosed], warning_text)),
  all(!nzchar(registry$public_disclosure[!disclosed])),
  identical(
    pt_polygon_generalization_public_note("rwqcb_regions"),
    paste(
      "Regional Board boundaries are generalized for broad statewide",
      "regulatory screening while keeping neighboring regions aligned.",
      warning_text
    )
  )
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
