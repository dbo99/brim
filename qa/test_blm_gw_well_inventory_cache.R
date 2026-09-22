#!/usr/bin/env Rscript
# Synthetic points/sidecars only; actual writer is replaced by an in-memory spy.
run_tests <- function() {
  root <- normalizePath(file.path(dirname(sub('^--file=', '', grep('^--file=',commandArgs(),value=TRUE))), '..'))
  e <- new.env(parent=globalenv())
  sys.source(file.path(root,'03_functions/blm_gw_well_inventory_cache_helpers.r'),e)
  for (expr in parse(file.path(root,'03_functions/spatial_helpers.r'))) {
    if (is.call(expr) && identical(expr[[1]],as.name('<-')) && as.character(expr[[2]]) %in% c('clean_sf_for_leaflet','make_valid_if_needed')) eval(expr,e)
  }
  checks <- 0L
  test <- function(label,code) {force(code); checks <<- checks+1L;cat('PASS ',label,'\n',sep='')}
  fails <- function(code) stopifnot(inherits(tryCatch({force(code);NULL},error=identity),'error'))
  wells <- sf::st_as_sf(data.frame(record_uid=c('noc_1','site_b','noc_2'),source_key=c('noc_blm_drilled','mojave_2025_blm_field_check','noc_blm_drilled'),
    source_display=c('NOC','Mojave','NOC'),hover_line1=c('One','Site 902','Two'),hover_line2=c('2020',NA,'1990'),
    well_name_display=c('One',NA,'Two'),longitude=c(-115,-115,-116),latitude=c(34,34,35),popup_html=c('NOC one','Mojave','NOC two'),
    water_level_recorded=c(NA,TRUE,NA),lab_sample_documented=c(NA,FALSE,NA)),
    coords=c('longitude','latitude'),crs=4326,remove=FALSE)
  distance <- data.frame(record_uid=wells$record_uid,source_key=wells$source_key,longitude=wells$longitude,latitude=wells$latitude,
    on_blm_ca=c(TRUE,FALSE,FALSE),on_blm_ca_chr=c('true','false','false'),dist_to_blm_mi=c(0,1,2),dist_to_blm_ft=c(0,5280,10560),
    blm_distance_label=c('on BLM','1.00 mi','2.00 mi'),blm_distance_bin=c('on BLM','off BLM, <=1 mi','off BLM, >1 to <=5 mi'),blm_distance_run_time='synthetic')
  prepare <- e$pt_prepare_blm_gw_well_inventory_cache
  expected <- prepare(wells,distance)
  test('actual preparation preserves NOC attributes, geometry, order and popup', {
    n <- expected$blm_noc_drilled_wells_map
    orig <- wells[c(1,3),setdiff(names(wells),c('water_level_recorded','lab_sample_documented'))]
    stopifnot(identical(sf::st_geometry(n),sf::st_geometry(orig)),identical(as.list(sf::st_drop_geometry(n)[names(sf::st_drop_geometry(orig))]),as.list(sf::st_drop_geometry(orig))))
  })
  test('Albion logical observations survive the shared owner; NOC schema has no union fields', {
    albion<-expected$mojave_2025_gw_well_inventory_map
    stopifnot(identical(albion$water_level_recorded,TRUE),identical(albion$lab_sample_documented,FALSE),
      !any(c('water_level_recorded','lab_sample_documented') %in% names(expected[[1]])))
    for(field in c('water_level_recorded','lab_sample_documented')) {
      bad<-wells;bad[[field]]<-NULL;fails(prepare(bad,distance))
      bad<-wells;bad[[field]]<-as.character(bad[[field]]);fails(prepare(bad,distance))
      bad<-wells;bad[[field]][2]<-NA;fails(prepare(bad,distance))
    }
    no_noc_flags<-wells[c(1,3),setdiff(names(wells),c('water_level_recorded','lab_sample_documented'))]
    stopifnot(identical(prepare(no_noc_flags,distance[c(1,3),])[[1]],expected[[1]]))
  })
  test('sidecar reuse follows keys/sources/coordinates and preserves original distance provenance', {
    metadata<-distance;metadata$input_well_inventory_rds<-'historical/combined.rds'
    metadata$input_well_inventory_mtime<-'historical normalization time'
    metadata$input_blm_lands_rds<-'historical/geometry.rds';metadata$input_blm_lands_mtime<-'historical geometry time'
    stopifnot(identical(prepare(wells,metadata,TRUE),expected),all(expected[[1]]$blm_distance_run_time=='synthetic'),
      all(expected[[2]]$blm_distance_run_time=='synthetic'))
  })
  before_dir<-Sys.getenv('BRIM_WELL_QA_BEFORE_DIR',unset='')
  if(nzchar(before_dir)) test('UI1 actual shared helper preserves prior NOC schema, types, values and geometry', {
    baseline<-new.env(parent=e)
    sys.source(file.path(before_dir,'03_functions/blm_gw_well_inventory_cache_helpers.r'),baseline)
    old_wells<-wells[,setdiff(names(wells),c('water_level_recorded','lab_sample_documented'))]
    old<-baseline$pt_prepare_blm_gw_well_inventory_cache(old_wells,distance,TRUE)
    stopifnot(identical(old[[1]],expected[[1]]),identical(sf::st_geometry(old[[1]]),sf::st_geometry(expected[[1]])))
  })
  # Real normalized inputs are sf/tibbles. A plain data.frame fixture can move
  # geometry to the end during preparation and hide geometry/agr index drift.
  observation_fields<-c('water_level_recorded','lab_sample_documented')
  geometry_column<-attr(wells,'sf_column')
  ordinary<-setdiff(names(wells),c(geometry_column,observation_fields))
  layouts<-list(
    middle=c(ordinary[1:6],geometry_column,ordinary[-(1:6)],observation_fields),
    first=c(geometry_column,ordinary,observation_fields),
    last=c(ordinary,observation_fields,geometry_column))
  for(position in names(layouts)) test(paste('full NOC object/agr parity with geometry',position), {
    fixture<-sf::st_as_sf(tibble::as_tibble(wells))[,layouts[[position]],drop=FALSE]
    noc_only<-fixture[fixture$source_key=='noc_blm_drilled',setdiff(names(fixture),observation_fields),drop=FALSE]
    reference<-prepare(noc_only,distance[distance$source_key=='noc_blm_drilled',],TRUE)[[1]]
    actual<-prepare(fixture,distance,TRUE)
    n<-actual[[1]];agr<-attr(n,'agr')
    stopifnot(identical(n,reference),identical(attr(n,'agr'),attr(reference,'agr')),
      identical(names(n),c(setdiff(names(fixture),observation_fields),setdiff(names(reference),names(fixture)))),
      identical(names(agr),setdiff(names(n),geometry_column)),!anyNA(names(agr)),!anyDuplicated(names(agr)),
      !any(observation_fields %in% names(agr)),identical(n$on_blm_ca,c(TRUE,FALSE)),
      identical(sf::st_crs(n),sf::st_crs(fixture)),identical(sf::st_precision(n),sf::st_precision(fixture)),
      identical(sf::st_geometry(n),sf::st_geometry(noc_only)))
    if(nzchar(before_dir)) stopifnot(identical(actual[[2]],baseline$pt_prepare_blm_gw_well_inventory_cache(fixture,distance,TRUE)[[2]]))
  })
  test('actual NOC selector preserves meaningful agr values and levels by name', {
    # Exercise the owner's exact projection expression, without redefining the
    # upstream clean/join operations' existing metadata semantics.
    expressions<-as.list(body(prepare))[-1]
    selectors<-Filter(function(x) is.call(x)&&identical(x[[1]],as.name('<-'))&&
      identical(x[[2]],as.name('noc'))&&is.call(x[[3]])&&identical(x[[3]][[1]],as.name('[')),expressions)
    stopifnot(length(selectors)==1L)
    for(position in names(layouts)) {
      fixture<-sf::st_as_sf(tibble::as_tibble(wells))[,layouts[[position]],drop=FALSE]
      joined<-dplyr::left_join(fixture,distance[,setdiff(names(distance),c('source_key','longitude','latitude'))],by='record_uid')
      joined<-sf::st_set_precision(joined,1000)
      fields<-setdiff(names(joined),geometry_column)
      sf::st_agr(joined)<-setNames(rep(c('constant','aggregate','identity'),length.out=length(fields)),fields)
      selected<-new.env(parent=environment(prepare));selected$noc<-joined;selected$observation_fields<-observation_fields
      eval(selectors[[1]],selected);result<-selected$noc;keep<-setdiff(names(joined),observation_fields)
      stopifnot(identical(names(result),keep),identical(class(result),class(joined)),identical(row.names(result),row.names(joined)),
        all(vapply(keep,function(nm) identical(result[[nm]],joined[[nm]]),logical(1))),
        identical(attr(result,'agr'),attr(joined,'agr')[setdiff(fields,observation_fields)]),
        identical(levels(attr(result,'agr')),levels(attr(joined,'agr'))),
        identical(sf::st_crs(result),sf::st_crs(joined)),identical(sf::st_precision(result),sf::st_precision(joined)))
    }
  })
  test('real-layout empty and optional NOC retain full metadata without observation fields', {
    fixture<-sf::st_as_sf(tibble::as_tibble(wells))[,layouts$middle,drop=FALSE]
    noc<-fixture[fixture$source_key=='noc_blm_drilled',,drop=FALSE]
    plain<-noc[,setdiff(names(noc),observation_fields),drop=FALSE]
    stopifnot(identical(prepare(noc,NULL)[[1]],prepare(plain,NULL)[[1]]),
      identical(prepare(noc[FALSE,],NULL)[[1]],prepare(plain[FALSE,],NULL)[[1]]))
  })
  test('actual joins are keyed and deterministic under sidecar shuffle', {
    stopifnot(identical(expected,prepare(wells,distance[c(3,1,2),],TRUE)),identical(expected,prepare(wells,distance)))
  })
  test('actual preparation rejects missing, duplicate, stale and colliding prerequisites', {
    fails(prepare(wells,NULL,TRUE));fails(prepare(wells,distance[-1,]));fails(prepare(wells,rbind(distance,distance[1,])))
    bad <- distance;bad$record_uid[1]<-'stale';fails(prepare(wells,bad))
    bad <- distance;bad$longitude[1]<- -114;fails(prepare(wells,bad))
    bad <- distance;bad$source_key[1]<-'mojave_2025_blm_field_check';fails(prepare(wells,bad))
    bad <- wells;bad$record_uid[2]<-bad$record_uid[1];fails(prepare(bad,distance))
    bad <- wells;bad$dist_to_blm_mi<-1;fails(prepare(bad,distance))
  })
  tmp <- tempfile('brim-well-cache-');dir.create(tmp);tmp<-normalizePath(tmp)
  on.exit(unlink(tmp,recursive=TRUE),add=TRUE)
  dirs <- list(rds=file.path(tmp,'rds'),cache_last=file.path(tmp,'latest'),cache_enr=file.path(tmp,'enriched'))
  invisible(lapply(dirs,dir.create))
  input <- file.path(dirs$rds,'blm_gw_well_inventory_combined_wgs84.rds')
  sidecar <- file.path(dirs$cache_last,'blm_gw_well_inventory_blm_distance_fields.csv')
  writeLines('synthetic placeholder',input);writeLines('synthetic placeholder',sidecar)
  sibling <- file.path(dirs$cache_last,'unrelated.rds');writeLines('protected synthetic sibling',sibling)
  writes <- list()
  spy <- function(x,timestamped_path,latest_path) {writes[[length(writes)+1L]] <<- list(object=x,archive=timestamped_path,latest=latest_path)}
  read_inventory <- function(path) {stopifnot(identical(path,input));wells}
  read_distance <- function(path) {stopifnot(identical(path,sidecar));distance}
  refresh <- function() e$pt_refresh_blm_gw_well_inventory_cache(dirs,'20260101_010203',read_inventory,read_distance,spy)
  test('actual focused orchestration prepares both caches and confines all mocked writes', {
    out<-refresh();stopifnot(identical(out,expected),length(writes)==2)
    stopifnot(identical(vapply(writes,function(w) basename(w$latest),character(1)),paste0(names(expected),'.rds')))
    stopifnot(identical(vapply(writes,function(w) basename(w$archive),character(1)),paste0(names(expected),'_20260101_010203.rds')))
    stopifnot(all(vapply(writes,function(w) dirname(w$archive)==dirs$cache_enr && dirname(w$latest)==dirs$cache_last,logical(1))))
    stopifnot(identical(readLines(sibling),'protected synthetic sibling'),length(list.files(dirs$cache_enr))==0)
  })
  test('actual normal cache-block calls the same owner with equivalent results', {
    e$DIR<-dirs;e$blm_gw_well_inventory_combined<-wells;e$pt_read_blm_gw_well_distances<-read_distance
    code<-readLines(file.path(root,'05_map_build/02_cache_blocks/04_cache_admin_water_reference_layers.r'))
    start<-grep('^blm_gw_distance_path <-',code);end<-grep('^# ---- 8.10s Springs',code)
    stopifnot(length(start)==1,length(end)==1)
    eval(parse(text=code[start:(end-1)]),e)
    stopifnot(identical(e$blm_noc_drilled_wells_map,expected[[1]]),identical(e$mojave_2025_gw_well_inventory_map,expected[[2]]))
  })
  test('failed prerequisites and output collisions precede every mocked write', {
    writes<-list();bad<-distance;bad$record_uid[1]<-'stale'
    fails(e$pt_refresh_blm_gw_well_inventory_cache(dirs,'20260101_010203',read_inventory,function(p) bad,spy));stopifnot(length(writes)==0)
    archive<-file.path(dirs$cache_enr,'mojave_2025_gw_well_inventory_map_20260101_010203.rds');writeLines('collision',archive)
    fails(refresh());stopifnot(length(writes)==0);unlink(archive)
    link<-file.path(dirs$cache_last,'blm_noc_drilled_wells_map.rds');stopifnot(file.symlink(sibling,link))
    fails(refresh());stopifnot(length(writes)==0);unlink(link)
    fails(e$pt_refresh_blm_gw_well_inventory_cache(dirs,'../escape',read_inventory,read_distance,spy));stopifnot(length(writes)==0)
    unlink(sidecar);fails(refresh());stopifnot(length(writes)==0)
  })
  test('normal optional absence remains an empty layer; no regeneration', {
    empty<-prepare(wells[FALSE,],NULL);stopifnot(all(vapply(empty,nrow,integer(1))==0L))
    unavailable<-prepare(wells,NULL);stopifnot(all(is.na(unavailable[[1]]$dist_to_blm_mi)))
  })
  test('focused script and runner dispatch do not invoke a broad cache stage', {
    caller<-readLines(file.path(root,'05_map_build/14_refresh_blm_gw_well_inventory_cache.r'))
    stopifnot(!any(grepl('02_build_core_map_cache|02_preprocess|run_build_map',caller)))
    runner<-new.env(parent=baseenv());calls<-character()
    for(expr in parse(file.path(root,'run_build_map.r'))) if(is.call(expr) && identical(expr[[1]],as.name('<-')) &&
      as.character(expr[[2]]) %in% c('SCRIPT_PATHS','refresh_blm_gw_well_inventory_cache','refresh_blm_gw_well_inventory_and_map')) eval(expr,runner)
    runner$run_step_clean<-function(path,label) {calls<<-c(calls,path)}
    runner$refresh_blm_gw_well_inventory_cache();stopifnot(identical(calls,'05_map_build/14_refresh_blm_gw_well_inventory_cache.r'))
    calls<-character();runner$refresh_blm_gw_well_inventory_and_map()
    stopifnot(identical(calls,c('02_preprocess/18_blm_groundwater_well_inventory.r','02_preprocess/63_update_blm_gw_well_inventory_blm_distance_fields.R',
      '05_map_build/14_refresh_blm_gw_well_inventory_cache.r','05_map_build/04_build_portatreasure2_core_map.r')))
  })
  cat('RESULT passed=',checks,' failed=0; actual_cache_saves=0; real_distances=NOT_RUN\n',sep='')
}
run_tests()
