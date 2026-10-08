# Prepared Dendra feed and accepted map-lab display core, injected by Ops Live.
pt_ops_live_dendra_js <- function() {
  paths <- file.path("03_functions", "js", c("dendra_core.js", "dendra_reader.js", "brim_local_reference_filter_engine.js", "soil_moisture_depths.js", "soil_moisture_charts.js", "soil_moisture_ui.js", "dendra_layer.js", "soil_moisture_engine.js", "soil_moisture_transport.js", "soil_moisture_controller.js"))
  if (!all(file.exists(paths))) stop("Missing Dendra Ops Live source module")
  paste(vapply(paths, function(p) paste(readLines(p, warn = FALSE), collapse = "\n"), character(1)), collapse = "\n")
}
