#!/usr/bin/env Rscript
# Source-only parity through the real Guide compiler and Ops onRender seam.
# Reuse the foundation suite's local fixture/bootstrap, stopping before its run.
bootstrap <- readLines("qa/test_guide_foundation.R", warn = FALSE)
cut <- match('bundle <- pt_build_guide_bundle(runtime_groups, MAP_DISPLAY, "default")', bootstrap)
stopifnot(!is.na(cut))
eval(parse(text = bootstrap[seq_len(cut - 1L)]))
source("03_functions/leaflet_ops_live_helpers.r")
# Final candidate QA supplies the already measured actual runtime-group order.
# Standalone execution retains the foundation suite's existing local fixture.
runtime_fixture <- Sys.getenv("BRIM_GUIDE_RUNTIME_GROUPS", "")
if (nzchar(runtime_fixture)) runtime_groups <- unlist(jsonlite::fromJSON(
  runtime_fixture, simplifyVector=FALSE)$runtime_groups, use.names=FALSE)
raw_registry <- jsonlite::fromJSON("00_config/guide_product_resource_relationships.json", simplifyVector = FALSE)
identities <- pt_ops_live_guide_identity_registry()
projection <- pt_ops_live_delivery_projection()
bundle <- pt_build_guide_bundle(runtime_groups, MAP_DISPLAY, "default")
ids <- vapply(raw_registry$products, `[[`, character(1), "product_id")
assert_identical(length(ids), 270L, "Full Product universe changed")
assert_true(!anyDuplicated(ids), "Duplicate authored Product")
assert_identical(nrow(identities), 48L, "Ops identity census changed")
assert_identical(length(projection), 47L, "Eligible projection changed")
assert_identical(identities$stable_id[!identities$included_by_default],
  "ops_nws_surface_wind_barbs", "Hard-disabled identity changed")
assert_true(!"ops_nws_surface_wind_barbs" %in% ids, "Disabled wind barbs acquired a Product")
assert_identical(sort(vapply(pt_guide_ops_products(MAP_DISPLAY, list()), `[[`, character(1), "id")),
  sort(vapply(projection, `[[`, character(1), "stable_id")), "Actual Guide/Ops eligible identity join differs")
classes <- setNames(vapply(raw_registry$products, `[[`, character(1), "delivery_class"), ids)
managed <- c("ops_delta_snapshot", "ops_streamflow_usgs_ca", "product-ops-usgs-groundwater",
  "ops_scan_soil_moisture", "ops_snow_pillow_swe", "ops_major_water_supply_forecasts",
  "ops_cdec_reservoir_storage", "product-ops-cocorahs-ca-daily", "ops_cocorahs_conus_daily",
  "ops_hrrr_surface_wind", "ops_gfs_surface_wind", "ops_nbm_wind_guidance", "ops_observed_metar_wind",
  "winter_storm_levels", "nbm_qpf", "product-ops-nbm-accumulated-qpf",
  "ops_nws_weather_stations", "ops_cnrfc_forecast_points")
assert_identical(sort(vapply(Filter(function(p) p$delivery_class == "brim_managed", projection),
  `[[`, character(1), "stable_id")), sort(managed), "Settled 18 Managed identities changed")
assert_identical(classes[["ops_radar_iem_nexrad"]], "provider_hosted", "Standard IEM WMS must not imply an enhancement")
provider_radar_qpe <- c("ops_radar_noaa_mrms", "ops_qpe_mrms_1hr",
  "ops_qpe_mrms_1day", "ops_qpe_mrms_3day", "ops_qpe_rfc_1day", "ops_qpe_rfc_7day")
for (id in provider_radar_qpe) {
  assert_identical(classes[[id]], "provider_hosted", paste("Conservative authored class changed", id))
  assert_identical(Filter(function(p) p$stable_id == id, projection)[[1L]]$delivery_class,
    "provider_hosted", paste("Actual Ops projection changed", id))
  assert_identical(raw_registry$products[[match(id, ids)]]$resource_links, list(),
    paste("Unlinked radar/QPE Product acquired a Resource", id))
}
occurrences <- function(value) {
  rows <- list()
  for (i in seq_along(value$products)) {
    p <- value$products[[i]]
    for (j in seq_along(p$relatedResources)) rows[[length(rows)+1L]] <- list(
      product_id=p$id, location=sprintf("products[%d].relatedResources[%d].deliveryClass",i-1L,j-1L),
      delivery_class=p$relatedResources[[j]]$deliveryClass)
  }
  for (i in seq_along(value$resources)) {
    r <- value$resources[[i]]
    for (j in seq_along(r$representedProducts)) rows[[length(rows)+1L]] <- list(
      product_id=r$representedProducts[[j]]$productId,
      location=sprintf("resources[%d].representedProducts[%d].deliveryClass",i-1L,j-1L),
      delivery_class=r$representedProducts[[j]]$deliveryClass)
  }
  rows
}
locations <- occurrences(bundle)
assert_true(!any(vapply(locations, function(row) row$product_id %in% provider_radar_qpe, logical(1))),
  "The six unlinked radar/QPE Products acquired compiled delivery occurrences")
assert_identical(length(locations), 242L, "Guide relationship projection occurrences changed")
for (row in locations) assert_identical(row$delivery_class, classes[[row$product_id]], row$location)
for (row in projection) assert_identical(row$delivery_class, classes[[row$stable_id]], row$stable_id)
nws <- Filter(function(p) p$id == "ops_nws_weather_stations", bundle$products)[[1L]]
assert_identical(length(nws$relatedResources), 0L, "NWS must not gain an invented Resource link")
assert_true(is.null(nws$deliveryClass), "Unlinked Product gained an invented deliveryClass field")
assert_identical(classes[[nws$id]], "brim_managed", "Unlinked curated NWS collection lost M")
# Actual wrapper capture: no rendering, network, geometry preparation or map build.
map <- leaflet::leaflet()
wrapped <- pt_add_ops_live_layers(map, MAP_DISPLAY)
hook <- wrapped$jsHooks$render[[1L]]
assert_identical(hook$data$opsDeliveryProjection, projection, "onRender uses a different projection")
syntax <- system2(Sys.getenv("NODE_BINARY", "node"), c("-e", shQuote(
  'new Function(require("fs").readFileSync(0,"utf8")); process.stdout.write("OPS_WRAPPER_JS_PARSE_PASS");'
)), input=paste0("return (", hook$code, ");"), stdout=TRUE, stderr=TRUE)
assert_true(is.null(attr(syntax,"status")) && identical(syntax,"OPS_WRAPPER_JS_PARSE_PASS"),
  "Actual full onRender JavaScript does not parse")
# Guide has no map-display enable flag: compare its actual hook present/absent.
with_guide <- pt_add_ops_live_layers(pt_add_brim_guide(map, bundle), MAP_DISPLAY)
assert_identical(with_guide$jsHooks$render[[2L]], hook,
  "Guide hook present versus absent changes the Ops hook")
off <- MAP_DISPLAY; off$add_ops_live_layers <- FALSE
assert_identical(pt_add_ops_live_layers(map, off), map, "Ops disabled bypass changed")
flag_checks <- list()
for (flag in unique(na.omit(identities$map_display_flag))) {
  flags <- MAP_DISPLAY; flags[[flag]] <- FALSE
  expected <- identities$stable_id[identities$included_by_default &
    (is.na(identities$map_display_flag) | identities$map_display_flag != flag)]
  actual <- vapply(pt_guide_ops_products(flags, list()), `[[`, character(1), "id")
  assert_identical(sort(actual), sort(expected), paste("Availability changed for", flag))
  flag_checks[[flag]] <- length(actual)
}
# Synthetic one-ID change stays in memory and uses the same Guide projection path.
synthetic <- raw_registry; index <- match("ops_cdec_reservoir_storage", ids)
synthetic$products[[index]]$delivery_class <- "provider_hosted"
synthetic_projection <- pt_ops_live_delivery_projection(source=synthetic)
resource_registry <- pt_guide_read_resource_registry()
relationships <- pt_guide_validate_product_resource_relationship_registry(
  synthetic, ids, resource_registry)
synthetic_bundle <- bundle
synthetic_bundle$products <- pt_guide_apply_product_enrichment(bundle$products,
  pt_guide_read_product_enrichment(), relationships, resource_registry=resource_registry,
  product_universe_ids=ids)
synthetic_bundle$resources <- pt_guide_resource_browser_records(
  pt_guide_resource_published_records(resource_registry), synthetic_bundle$products, relationships)
pt_validate_guide_bundle(synthetic_bundle)
synthetic_locations <- occurrences(synthetic_bundle)
changed <- which(vapply(seq_along(locations), function(i)
  !identical(locations[[i]], synthetic_locations[[i]]), logical(1)))
assert_true(length(changed) > 0L, "Synthetic class change did not reach Guide")
for (i in changed) {
  assert_identical(synthetic_locations[[i]]$product_id, ids[[index]], "Synthetic change leaked to another Product")
  assert_identical(synthetic_locations[[i]]$delivery_class, "provider_hosted", "Guide did not project synthetic class")
}
assert_identical(Filter(function(p) p$stable_id == ids[[index]], synthetic_projection)[[1L]]$delivery_class,
  "provider_hosted", "Ops did not project synthetic class")
# Invert exactly the synthetic class occurrences; every other field remains equal.
for (i in seq_along(synthetic_bundle$products)) for (j in seq_along(synthetic_bundle$products[[i]]$relatedResources))
  synthetic_bundle$products[[i]]$relatedResources[[j]]$deliveryClass <- bundle$products[[i]]$relatedResources[[j]]$deliveryClass
for (i in seq_along(synthetic_bundle$resources)) for (j in seq_along(synthetic_bundle$resources[[i]]$representedProducts))
  synthetic_bundle$resources[[i]]$representedProducts[[j]]$deliveryClass <- bundle$resources[[i]]$representedProducts[[j]]$deliveryClass
assert_identical(synthetic_bundle, bundle, "Synthetic projection changed unrelated Guide content")
negative_checks <- character()
reject <- function(label, value=raw_registry, identity=identities, pattern) {
  assert_error(pt_ops_live_delivery_projection(source=value, identities=identity), pattern, label)
  negative_checks <<- c(negative_checks,label)
}
x <- raw_registry; x$products <- x$products[-match("ops_nws_weather_stations",ids)]
reject("missing eligible ID",x,pattern="missing eligible")
x <- raw_registry; x$products[[length(x$products)+1L]] <- x$products[[1L]]
reject("duplicate Product ID",x,pattern="duplicate Product")
x <- identities; x$source_token[[2L]] <- x$source_token[[1L]]
reject("duplicate source token",identity=x,pattern="duplicate identity")
x <- identities; x$stable_id[[2L]] <- x$stable_id[[1L]]
reject("identity ID collision",identity=x,pattern="duplicate identity")
x <- raw_registry; x$products[[index]]$delivery_class <- "enhanced_maybe"
reject("invalid class",x,pattern="invalid Product")
x <- raw_registry; x$products[[index]]$delivery_class <- list("brim_managed")
reject("malformed class",x,pattern="invalid Product")
reject("malformed products",list(products="invalid"),pattern="malformed products")
x <- raw_registry; names(x$products[[index]])[[2L]] <- "product_id"
reject("duplicate object field",x,pattern="invalid Product")
x <- identities; x$source_token[[1L]] <- "__proto__"
reject("prototype token",identity=x,pattern="invalid identity")
x <- raw_registry; x$products[[index]]$product_id <- "constructor"
reject("prototype Product ID",x,pattern="invalid Product")
assert_identical(pt_ops_live_delivery_projection(source=raw_registry),projection,
  "Disabled Product absent must be valid")
serialize <- function(value) charToRaw(enc2utf8(as.character(jsonlite::toJSON(value,
  auto_unbox=TRUE,null="null",na="null"))))
measured_bundle <- Sys.getenv("BRIM_PROVIDER_TEST_BUNDLE", "")
if (nzchar(measured_bundle)) assert_identical(serialize(bundle),
  readBin(measured_bundle,"raw",n=file.info(measured_bundle)$size),
  "Parity compiler must use the actual measured candidate runtime order and raw payload")
report <- list(status="PASS",authority="products[].delivery_class",products=270L,identities=48L,
  eligible=47L,managed=18L,prepared_consumers=16L,curated_collections=2L,
  projection=projection,projection_bytes=length(serialize(projection)),
  projection_sha256=digest::digest(serialize(projection),algo="sha256",serialize=FALSE),
  guide_bytes=length(serialize(bundle)),guide_sha256=digest::digest(serialize(bundle),algo="sha256",serialize=FALSE),
  all_guide_delivery_occurrences=locations,synthetic_changed_locations=synthetic_locations[changed],
  negatives=negative_checks,include_flag_results=flag_checks,actual_onRender_projection="PASS",
  browser="UNAVAILABLE_POLICY",rendered_fit="PENDING_HUMAN_REVIEW")
audit_path <- Sys.getenv("BRIM_OPS_DELIVERY_AUDIT", "")
badge_path <- Sys.getenv("BRIM_OPS_BADGE_RESULT", "")
if (nzchar(audit_path) && nzchar(badge_path)) {
  audit <- jsonlite::fromJSON(audit_path, simplifyVector=FALSE)
  badge_result <- jsonlite::fromJSON(badge_path, simplifyVector=FALSE)
  assert_identical(length(audit$rows),48L,"Finite audit census changed")
  assert_identical(length(badge_result$rows),48L,"Actual badge render census changed")
  for (i in seq_len(48L)) {
    row <- audit$rows[[i]]; rendered <- badge_result$rows[[i]]
    assert_identical(row$stable_id,identities$stable_id[[i]],"Audit identity mismatch")
    assert_identical(rendered$stable_id,row$stable_id,"Rendered identity mismatch")
    expected <- if(row$eligible) classes[[row$stable_id]] else NULL
    assert_identical(row$final_class,expected,"Audit does not match authored class")
    assert_identical(rendered$delivery_class,expected,"Actual badge renderer uses a different class")
    assert_identical(rendered$badge,row$badge,"Actual badge differs from reviewed disposition")
    assert_identical(rendered$prepared,row$prepared_feed_consumer,"Prepared technical subset changed")
    for (ref in row$current_source_evidence) assert_identical(
      digest::digest(file=ref$path,algo="sha256"),ref$sha256,"Audit source binding stale")
  }
  report$audit_binding <- list(sha256=digest::digest(file=audit_path,algo="sha256"),rows=48L,status="PASS")
  report$actual_badge_binding <- list(sha256=digest::digest(file=badge_path,algo="sha256"),rows=48L,status="PASS")
}
output <- Sys.getenv("BRIM_OPS_PARITY_RESULT", "")
if (nzchar(output)) writeLines(jsonlite::toJSON(report,auto_unbox=TRUE,pretty=TRUE),output)
cat("GUIDE_OPS_DELIVERY_PARITY=PASS; 270 authored / 48 identities / 47 eligible / 18 Managed;",
    length(locations),"Guide occurrences;",length(changed),"synthetic occurrences;",
    length(negative_checks),"negative validations; projection bytes",length(serialize(projection)),"\n")
