#!/usr/bin/env Rscript
# Structural checks of reviewed source tables, plus small synthetic pure-function
# tests only. Never source the normalizer or normalize the real source tables.
# Reusable read-only acceptance comparison for separately authorized regeneration.
# Only the exact authored presentation deltas are allowed; all attributes (including
# sf metadata), other columns, row order and blank/NA distinctions remain exact.
assert_well_presentation_delta <- function(before, after, family) {
  stopifnot(family %in% c('noc','albion'))
  before_attrs<-attributes(before);after_attrs<-attributes(after)
  stopifnot(setequal(names(before_attrs),names(after_attrs)))
  for(nm in names(before_attrs)) stopifnot(identical(before_attrs[[nm]],after_attrs[[nm]]))
  # UI5 is the accepted before-state. Optional source_full is checked when the
  # actual schema carries it; never invent it in a cache/browser projection.
  allowed<-c('source_display','layer_name','popup_html')
  if(family=='albion' && 'source_full' %in% names(before)) allowed<-c(allowed,'source_full')
  stopifnot(all(allowed %in% names(before)))
  for(nm in setdiff(names(before),allowed)) stopifnot(identical(before[[nm]],after[[nm]]))
  for(nm in allowed) stopifnot(identical(attributes(before[[nm]]),attributes(after[[nm]])))
  if(family=='noc') {
    stopifnot(all(before$source_display=='NOC well inventory'),all(after$source_display=='BLM National Operations Center'),
      all(before$layer_name=='GW wells | NOC'),all(after$layer_name=='GW wells | BLM NOC inventory'))
    old_source<-'NOC well inventory';new_source<-'BLM National Operations Center'
  } else {
    stopifnot(all(before$source_display=='2025 Mojave-BLM limited field check'),all(after$source_display=='Mojave limited field inventory (2025)'),
      all(before$layer_name=='GW wells | 2025 Mojave-BLM limited field check'),all(after$layer_name=='GW sites | 2025 Mojave limited field inventory'))
    old_source<-'2025 Mojave-BLM limited groundwater-well field investigation';new_source<-'Mojave limited field inventory (2025)'
    if('source_full' %in% allowed) stopifnot(all(before$source_full==old_source),all(after$source_full==new_source))
  }
  old_row<-paste0("<div class='pt2-popup-row'><b>Source:</b> ",old_source,"</div>")
  new_row<-paste0("<div class='pt2-popup-row'><b>Source:</b> ",new_source,"</div>")
  stopifnot(!anyNA(before$popup_html),!anyNA(after$popup_html))
  for(i in seq_along(before$popup_html)) {
    a<-before$popup_html[[i]];b<-after$popup_html[[i]]
    hit<-gregexpr(old_row,a,fixed=TRUE)[[1]];stopifnot(length(hit)==1L,hit[1]>0)
    expected<-sub(old_row,new_row,a,fixed=TRUE)
    if(family=='albion') {
      stopifnot(!grepl('<b>Review status:</b>',a,fixed=TRUE),!grepl('<b>Review status:</b>',b,fixed=TRUE))
      label<-'<b>Reported basin:</b>';hit<-gregexpr(label,a,fixed=TRUE)[[1]]
      stopifnot(length(hit)==1L) # zero/one optional authored basin row; no duplicate
      expected<-sub(label,'<b>Field-reported basin:</b>',expected,fixed=TRUE)
    }
    stopifnot(identical(expected,b))
  }
  invisible(TRUE)
}

run_tests <- function() {
  root <- normalizePath(file.path(dirname(sub('^--file=', '', grep('^--file=', commandArgs(), value = TRUE))), '..'))
  owner <- new.env(parent = globalenv())
  # Evaluate actual function definitions and only the four coordinate constants.
  for (expr in parse(file.path(root, '02_preprocess/18_blm_groundwater_well_inventory.r'))) {
    if (is.call(expr) && identical(expr[[1]], as.name('<-')) && is.symbol(expr[[2]])) {
      name <- as.character(expr[[2]])
      if ((is.call(expr[[3]]) && identical(expr[[3]][[1]], as.name('function'))) ||
          name %in% c('BROAD_CA_LON_MIN','BROAD_CA_LON_MAX','BROAD_CA_LAT_MIN','BROAD_CA_LAT_MAX')) eval(expr, owner)
    }
  }
  checks <- 0L
  test <- function(label, code) {force(code); checks <<- checks + 1L; cat('PASS ', label, '\n', sep='')}
  fails <- function(code) stopifnot(inherits(tryCatch({force(code); NULL}, error=identity), 'error'))
  tables <- owner$pt_read_mojave_authority_048a(file.path(root, '00_config/blm_well_inventory'))
  m <- tables$master; a <- tables$measurements; c <- tables$corrections
  contract <- jsonlite::fromJSON(file.path(root, 'qa/fixtures/blm_well_inventory/acceptance_contract.json'))
  test('reviewed source: 138 unique sites and exact four status counts', {
    stopifnot(nrow(m)==contract$target$unique_site_count, !anyDuplicated(m$final_site_uid))
    for (k in names(contract$target$status_counts)) stopifnot(sum(m$final_status==k)==contract$target$status_counts[[k]])
  })
  test('reviewed source: exact point coordinates and spring record 34', {
    stopifnot(all(is.finite(as.numeric(m$final_latitude))), all(is.finite(as.numeric(m$final_longitude))),
      identical(m$report_record_number[m$final_feature_type=='spring_or_spring_box'], '34'))
  })
  test('reviewed source: duplicates absent; additions and record 145 retained', {
    stopifnot(!any(as.character(contract$target$excluded_duplicate_records) %in% m$report_record_number),
      all(as.character(137:145) %in% m$report_record_number), m$field_maps_number[m$report_record_number=='145']=='160')
  })
  test('reviewed source: all ten intentional blanks and settled names', {
    stopifnot(setequal(m$report_record_number[m$final_site_name==''], as.character(c(8,13,87,93,121,124,125,126,127,145))))
    for (k in names(contract$settled_final_names)) stopifnot(m$final_site_name[m$report_record_number==k]==contract$settled_final_names[[k]])
  })
  test('reviewed source: review set and fixed 135/136 geometry', {
    stopifnot(setequal(as.integer(m$report_record_number[m$review_status=='HUMAN REVIEW']), contract$target$human_review_records))
    rows <- m$report_record_number %in% c('135','136')
    stopifnot(all(as.numeric(m$final_latitude[rows])==34.712737), all(as.numeric(m$final_longitude[rows])== -115.125172))
  })
  test('reviewed source: basin concepts, monitoring flags, dedicated measurements', {
    stopifnot(sum(m$reported_spatial_basin_match=='False')==10, sum(m$monitoring_performed=='True')==26,
      nrow(a)==17, !anyDuplicated(a$report_record_number), all(c('reported_basin','spatial_bulletin118_name','spatial_bulletin118_id') %in% names(m)),
      'production' %in% a$depth_to_water_ft, 'UNK' %in% a$total_well_depth_ft, 'NR' %in% a$salinity_ppt)
  })
  test('reviewed source: accepted 85–96 legacy rotation and baseline actions', {
    ii <- match(as.character(85:96), c$report_record_number)
    stopifnot(identical(c$current_brim_record_identifier[ii], paste0('mojave2025_', c(95,96,85:94))))
    for (k in names(contract$supplied_baseline$action_counts)) stopifnot(sum(c$action==k)==contract$supplied_baseline$action_counts[[k]])
  })
  test('reviewed observation membership is keyed: 138 / 10 / 9 / 7, independent of row order', {
    membership <- function(master, a2) {
      ii <- match(master$report_record_number, a2$report_record_number)
      data.frame(key=master$report_record_number,
        water=!is.na(ii) & is.finite(owner$pt_num_048a(a2$depth_to_water_ft[ii])),
        lab=master$laboratory_sample_collected=='True')
    }
    flags <- membership(m,a)
    stopifnot(nrow(flags)==138, setequal(flags$key[flags$water],c('21','63','64','84','93','126','138','139','144','145')),
      setequal(flags$key[flags$lab],c('63','64','84','93','118','126','143','144','145')),
      setequal(flags$key[flags$water & flags$lab],c('63','64','84','93','126','144','145')))
    shuffled <- membership(m[rev(seq_len(nrow(m))),],a[rev(seq_len(nrow(a))),])
    stopifnot(identical(as.list(flags),as.list(shuffled[match(flags$key,shuffled$key),])))
    cat('OBSERVATION_SOURCE_COUNTS All=',nrow(flags),' water_level=',sum(flags$water),
      ' lab=',sum(flags$lab),' both=',sum(flags$water & flags$lab),'\n',sep='')
    stopifnot(nrow(m)-0L==138L,sum(c$action=='REMOVE_DUPLICATE')==2L)
  })
  test('Wiley maintainer NO_CHANGE retains the two distinct not-found sites', {
    ii <- match(c('62','79'),m$report_record_number)
    stopifnot(identical(m$final_site_name[ii],c('Wileys Well Rip Rap','Wiley Well')),
      all(m$final_status[ii]=='not_found'),!anyDuplicated(m$final_site_uid[ii]))
  })
  # Independent synthetic authority, including a future key and shared coordinates.
  master <- data.frame(final_site_uid=c('site_a','site_b','future_site'), report_record_number=c('901','902','903'),
    field_maps_number=c('0','','160'), final_site_name=c('','<Well & one>','Spring'),
    final_latitude=c('34.1234567890123','34.1234567890123','35'), final_longitude=c('-115.1234567890123','-115.1234567890123','-116'),
    final_status=c('present','present','spring_present'), final_feature_type=c('groundwater_well_site','groundwater_well_site','spring_or_spring_box'),
    table2_name_original=c('Stale label','Raw name','Raw spring'), table2_field_notes=c('<script>x</script>','notes','spring box'),
    table2_report_basin_original='reported', reported_basin='reported', spatial_bulletin118_id='7-000', spatial_bulletin118_name='spatial',
    monitoring_performed=c('True','True','False'), laboratory_sample_collected=c('False','True','False'),
    review_status=c('HUMAN REVIEW','ACCEPTED','ACCEPTED'), source_authority='<authority>', identity_notes='unresolved <attribute>',
    stringsAsFactors=FALSE)
  measurement <- data.frame(report_record_number='901', final_site_name='', final_latitude=master$final_latitude[1], final_longitude=master$final_longitude[1],
    spatial_bulletin118_name='spatial', depth_to_water_ft='production', total_well_depth_ft='UNK', sample_date='2025-01-02', monitoring_basin_location_notes='NR', salinity_ppt='NR')
  correction <- data.frame(final_site_uid=master$final_site_uid, report_record_number=master$report_record_number,
    current_brim_record_identifier=c('old_b','old_a',''), action=c('UPDATE','KEEP','ADD'), old_name=c('stale','old',''),
    reason='reviewed', source_file='synthetic.csv', source_sheet='synthetic', source_record_reference=master$report_record_number)
  normalize <- owner$pt_normalize_mojave_attributes_048a
  out <- normalize(master, measurement, correction)
  test('synthetic: stable keys and crosswalks survive shuffled rows', {
    shuffled <- normalize(master[c(3,1,2),], measurement, correction[c(2,3,1),])
    other <- shuffled[match(out$record_uid, shuffled$record_uid),]
    stopifnot(identical(out$record_uid, master$final_site_uid),
      identical(out$legacy_record_uid, c('old_b','old_a','')),
      identical(as.list(out), as.list(other)))
  })
  test('synthetic: original fields, blank names, co-located precision, separate basins', {
    stopifnot(identical(as.data.frame(out[names(master)]), master), is.na(out$well_name_display[1]),
      out$hover_line1[1]=='Site 901', out$longitude[1]==as.numeric(master$final_longitude[1]),
      out$longitude[1]==out$longitude[2], out$reported_basin[1]=='reported', out$groundwater_basin[1]=='spatial')
  })
  test('synthetic: qualifiers are preserved, no invented measurements or monitoring equivalence', {
    stopifnot(out$depth_to_water_raw[1]=='production', out$total_depth_raw[1]=='UNK', out$measurement_salinity_ppt[1]=='NR',
      is.na(out$depth_to_water_sort_ft[1]), all(is.na(out$depth_to_water_raw[2:3])),
      identical(out$measurement_available,c(TRUE,FALSE,FALSE)), sum(out$well_monitored_key=='monitoring_reported')==2,
      grepl('Monitoring reported',out$popup_html[1]), grepl('production',out$popup_html[1]), grepl('UNK',out$popup_html[1]))
  })
  test('synthetic: escaped popup provenance and distinct basin labels', {
    stopifnot(!grepl('<script>',out$popup_html[1],fixed=TRUE), grepl('&lt;script&gt;',out$popup_html[1],fixed=TRUE),
      grepl('&lt;authority&gt;',out$popup_html[1],fixed=TRUE), grepl('&lt;Well &amp; one&gt;',out$popup_html[2],fixed=TRUE),
      grepl('Field-reported basin',out$popup_html[1]),grepl('Spatial Bulletin 118 basin',out$popup_html[1]))
  })
  test('synthetic popup cleanup preserves lineage and current uncertainty without row misalignment', {
    stopifnot(!any(grepl('Original Table 2 name|Historical BRIM key|Identity notes|Review status|HUMAN REVIEW',out$popup_html)),
      identical(out$review_status,master$review_status),
      !any(grepl('Stale label|old_b|old_a',out$popup_html)),
      grepl('Unresolved attributes',out$popup_html[1]),grepl('unresolved &lt;attribute&gt;',out$popup_html[1],fixed=TRUE),
      !grepl('Unresolved attributes|unresolved',out$popup_html[2]),
      out$table2_name_original[1]=='Stale label',out$legacy_record_uid[1]=='old_b',
      out$identity_notes[1]=='unresolved <attribute>',grepl('Site 901',out$popup_html[1]),
      grepl('Measurement date',out$popup_html[1]),grepl('2025-01-02',out$popup_html[1]),
      !any(grepl('Lab date|Sample date|Collection date',out$popup_html)),
      grepl('Lab sample documented:</b> Not documented',out$popup_html[1],fixed=TRUE),
      grepl('Lab sample documented:</b> Yes',out$popup_html[2],fixed=TRUE))
  })
  test('synthetic ingestion rejects absent, missing and malformed lab documentation', {
    for (value in list(NA_character_,'','false','0','yes',TRUE)) {
      bad <- master;bad$laboratory_sample_collected <- rep(value,nrow(bad));fails(normalize(bad,measurement,correction))
    }
    bad <- master;bad$laboratory_sample_collected <- NULL;fails(normalize(bad,measurement,correction))
  })
  test('synthetic observations distinguish numerical readings, source membership and laboratory documentation', {
    s <- master[rep(2L,12),];s$final_site_uid <- paste0('obs_',seq_len(nrow(s)))
    s$report_record_number <- as.character(1001:1012);s$monitoring_performed <- 'True'
    s$laboratory_sample_collected <- ifelse(seq_len(nrow(s)) %in% c(2,3),'True','False')
    s$table2_depth_to_water <- '99' # Old broad/master values do not create eligibility.
    cc <- correction[rep(2L,12),];cc$final_site_uid<-s$final_site_uid;cc$report_record_number<-s$report_record_number
    cc$current_brim_record_identifier<-paste0('old_obs_',seq_len(nrow(s)))
    inds <- c(1,3,6:12);aa<-measurement[rep(1L,length(inds)),];aa$report_record_number<-s$report_record_number[inds]
    aa$depth_to_water_ft<-c('12','10','production','','Inf','NaN','0','-2','UNK')
    observed <- normalize(s,aa,cc)
    stopifnot(identical(observed$water_level_recorded,seq_len(12) %in% c(1,3,10,11)),
      identical(observed$lab_sample_documented,seq_len(12) %in% c(2,3)),
      all(observed$well_monitored_key=='monitoring_reported'),observed$depth_to_water_sort_ft[10]==0,
      observed$depth_to_water_sort_ft[11]== -2,observed$laboratory_sample_collected[1]=='False')
    aa$depth_to_water_ft[9]<-'NR';stopifnot(!normalize(s,aa,cc)$water_level_recorded[12])
  })
  test('synthetic: duplicate, missing and contradictory authorities fail', {
    fails(normalize(rbind(master,master[1,]),measurement,correction))
    fails(normalize(master,rbind(measurement,measurement),correction))
    fails(normalize(master,measurement,correction[-1,]))
    bad <- correction; bad$current_brim_record_identifier[2] <- bad$current_brim_record_identifier[1]; fails(normalize(master,measurement,bad))
    bad <- correction; bad$report_record_number[1] <- '999'; fails(normalize(master,measurement,bad))
    bad <- master; bad$final_latitude[1] <- 'NR'; fails(normalize(bad,measurement,correction))
    bad <- measurement; bad$report_record_number <- '903'; fails(normalize(master,bad,correction))
  })
  test('synthetic: missing corrected input fails even beside legacy input', {
    tmp <- tempfile('brim-authority-'); dir.create(tmp); on.exit(unlink(tmp,recursive=TRUE),add=TRUE)
    writeLines('legacy',file.path(tmp,'albion_rev1.csv'))
    fails(owner$pt_read_mojave_authority_048a(tmp))
    stopifnot(identical(list.files(tmp),'albion_rev1.csv'))
  })
  noc <- data.frame(objectid=c('001','002','003'),global_id='',well_uuid='',well_name=c('One','','Outside'),facility_name=c('','Facility',''),
    longitude=c('-115','-116','-101'),latitude=c('34','35','39'),well_id=c('0','002','3'),
    initial_static_water_level=c('0','12.5','NR'),well_completion_date=c('2020-10-03 12:00','1999/02/01',''),
    attachments_59=c('a','b','c'),attachments_62=c('x','y','z'),well_comments=c('<note>','',''))
  n <- owner$pt_normalize_noc_attributes_048a(noc,'NOC_BLMdrilled.csv')
  test('synthetic NOC: ordinary identifiers, coordinate exclusion and both attachment columns', {
    stopifnot(identical(n$source_record_id,noc$objectid), identical(n$record_uid,paste0('noc_',1:3)),
      identical(n$attachments_59,noc$attachments_59),identical(n$attachments_62,noc$attachments_62),
      identical(n$coord_status,c('ok','ok','outside broad BRIM-CA lon/lat range')),all(n$source_key=='noc_blm_drilled'))
  })
  test('synthetic NOC: neutral attribution with preserved display semantics and escaping', {
    stopifnot(all(n$source_display=='BLM National Operations Center'),all(n$layer_name=='GW wells | BLM NOC inventory'),
      all(grepl('Source:</b> BLM National Operations Center',n$popup_html,fixed=TRUE)))
    stopifnot(is.na(n$well_id[1]), n$well_name_display[2]=='Facility', is.na(n$depth_to_water_raw[1]),
      n$depth_to_water_display[2]=='12.5 ft', n$hover_line2[1]=='2020-10-03', n$well_present_key[1]=='not_applicable',
      grepl('NOC QA IDs',n$popup_html[1]),grepl('&lt;note&gt;',n$popup_html[1],fixed=TRUE))
  })
  test('synthetic actual summary: current-input conservation and separate historical actions', {
    # Evaluate only these actual assignments, never the normalizer entrypoint.
    code <- parse(file.path(root,'02_preprocess/18_blm_groundwater_well_inventory.r'))
    assignment <- function(name) {
      found <- Filter(function(expr) is.call(expr) && identical(expr[[1]],as.name('<-')) &&
        identical(expr[[2]],as.name(name)), as.list(code))
      stopifnot(length(found)==1L)
      found[[1]]
    }
    state <- new.env(parent=baseenv())
    historical <- correction[rep(1L,2),]
    historical$final_site_uid <- c('removed_a','removed_b')
    historical$report_record_number <- c('990','991')
    historical$current_brim_record_identifier <- c('old_removed_a','old_removed_b')
    historical$action <- 'REMOVE_DUPLICATE'
    state$mojave_authority <- list(corrections=rbind(correction,historical))
    state$noc_attr_all <- n
    state$noc_out <- n[n$coord_status=='ok',]
    state$mojave_attr_all <- out
    state$mojave_out <- out
    authority_before <- state$mojave_authority
    eval(assignment('mojave_duplicate_exclusions'),state)
    audit_before <- state$mojave_duplicate_exclusions
    stopifnot(nrow(audit_before)==2L, identical(as.list(audit_before),as.list(historical)))
    eval(assignment('source_summary'),state)
    summary <- state$source_summary
    stopifnot(identical(names(summary),c('source_key','source_display','raw_rows',
      'blank_rows_dropped','duplicate_rows_excluded','coordinate_rows_excluded',
      'map_ready_rows','named_rows','depth_to_water_rows','elevation_rows')),
      nrow(summary)==2L, identical(state$mojave_duplicate_exclusions,audit_before),
      identical(state$mojave_authority,authority_before))
    albion <- summary[summary$source_key=='mojave_2025_blm_field_check',]
    noc_summary <- summary[summary$source_key=='noc_blm_drilled',]
    cat('SUMMARY synthetic Albion raw=',albion$raw_rows,' duplicate_exclusions=',
      albion$duplicate_rows_excluded,' ready=',albion$map_ready_rows,
      '; historical_removals=',nrow(audit_before),'; NOC raw=',noc_summary$raw_rows,
      ' coordinate_exclusions=',noc_summary$coordinate_rows_excluded,
      ' ready=',noc_summary$map_ready_rows,'\n',sep='')
    stopifnot(albion$raw_rows==3L, albion$duplicate_rows_excluded==0L,
      albion$coordinate_rows_excluded==0L, albion$map_ready_rows==3L,
      noc_summary$raw_rows==3L, noc_summary$duplicate_rows_excluded==0L,
      noc_summary$coordinate_rows_excluded==1L, noc_summary$map_ready_rows==2L,
      all(summary$raw_rows-summary$blank_rows_dropped-summary$duplicate_rows_excluded-
        summary$coordinate_rows_excluded==summary$map_ready_rows))
  })
  # Actual browser R projection: no application or onRender execution.
  for (expr in parse(file.path(root,'03_functions/leaflet_layer_local_well_spring_helpers.r'))) {
    if (is.call(expr) && identical(expr[[1]],as.name('<-')) && identical(expr[[2]],as.name('pt_prepare_blm_gw_well_inventory_records'))) eval(expr,owner)
  }
  test('synthetic actual browser projection suppresses blank names and preserves NOC title', {
    browser <- owner$pt_prepare_blm_gw_well_inventory_records(out)
    stopifnot(is.na(browser$well_name_display[1]),browser$hover_line1[1]=='Site 901',browser$well_present_key[3]=='spring_present')
    n$well_name_display[1] <- NA_character_; n$hover_line1[1] <- NA_character_
    browser_noc <- owner$pt_prepare_blm_gw_well_inventory_records(n)
    stopifnot(browser_noc$well_name_display[1]=='Unnamed well',browser_noc$hover_line1[1]=='Unnamed well')
  })
  test('actual source/combined/cache/browser path retains typed observation flags and drops them from NOC', {
    for (expr in parse(file.path(root,'03_functions/spatial_helpers.r'))) {
      if (is.call(expr) && identical(expr[[1]],as.name('<-')) && as.character(expr[[2]]) %in% c('clean_sf_for_leaflet','make_valid_if_needed')) eval(expr,owner)
    }
    sys.source(file.path(root,'03_functions/blm_gw_well_inventory_cache_helpers.r'),owner)
    state <- new.env(parent=owner)
    flow_measurement<-measurement;flow_measurement$depth_to_water_ft<-'0'
    flow<-normalize(master,flow_measurement,correction)
    state$noc_sf<-sf::st_as_sf(n[n$coord_status=='ok',],coords=c('longitude','latitude'),crs=4326,remove=FALSE)
    state$mojave_sf<-sf::st_as_sf(flow,coords=c('longitude','latitude'),crs=4326,remove=FALSE)
    for (expr in parse(file.path(root,'02_preprocess/18_blm_groundwater_well_inventory.r'))) {
      if(is.call(expr) && identical(expr[[1]],as.name('<-')) && as.character(expr[[2]]) %in% c('common_cols','noc_out','mojave_out','combined_out')) eval(expr,state)
    }
    joined<-state$combined_out
    ii<-match(flow$record_uid,joined$record_uid)
    stopifnot(identical(joined$water_level_recorded[ii],flow$water_level_recorded),
      identical(joined$lab_sample_documented[ii],flow$lab_sample_documented),
      all(is.na(joined$water_level_recorded[joined$source_key=='noc_blm_drilled'])))
    prepared<-owner$pt_prepare_blm_gw_well_inventory_cache(joined)
    browser<-owner$pt_prepare_blm_gw_well_inventory_records(prepared[[2]])
    bi<-match(flow$record_uid,browser$record_uid)
    stopifnot(identical(browser$water_level_recorded[bi],flow$water_level_recorded),
      identical(browser$lab_sample_documented[bi],flow$lab_sample_documented),
      any(browser$water_level_recorded),any(!browser$water_level_recorded))
    encoded<-jsonlite::toJSON(browser,dataframe='rows',na='null',auto_unbox=TRUE)
    decoded<-jsonlite::fromJSON(encoded)
    stopifnot(is.logical(decoded$water_level_recorded),is.logical(decoded$lab_sample_documented),
      identical(decoded$lab_sample_documented,browser$lab_sample_documented))
    flags<-c('water_level_recorded','lab_sample_documented')
    stopifnot(!any(flags %in% names(prepared[[1]])),!any(flags %in% names(owner$pt_prepare_blm_gw_well_inventory_records(prepared[[1]]))))
    for(field in flags) {
      bad<-joined;bad[[field]]<-NULL;fails(owner$pt_prepare_blm_gw_well_inventory_cache(bad))
      bad<-prepared[[2]];bad[[field]]<-NULL;fails(owner$pt_prepare_blm_gw_well_inventory_records(bad))
      bad<-prepared[[2]];bad[[field]]<-as.character(bad[[field]]);fails(owner$pt_prepare_blm_gw_well_inventory_records(bad))
    }
  })
  before_dir<-Sys.getenv('BRIM_WELL_QA_BEFORE_DIR',unset='')
  if(nzchar(before_dir)) test('actual pre-change/current pure functions: exact NOC and Albion presentation deltas', {
    baseline<-new.env(parent=globalenv())
    for(file in c('02_preprocess/18_blm_groundwater_well_inventory.r','03_functions/leaflet_layer_local_well_spring_helpers.r')) {
      for(expr in parse(file.path(before_dir,file))) if(is.call(expr) && identical(expr[[1]],as.name('<-')) && is.symbol(expr[[2]]) &&
        ((is.call(expr[[3]]) && identical(expr[[3]][[1]],as.name('function'))) || as.character(expr[[2]]) %in% c('BROAD_CA_LON_MIN','BROAD_CA_LON_MAX','BROAD_CA_LAT_MIN','BROAD_CA_LAT_MAX'))) eval(expr,baseline)
    }
    old_n<-baseline$pt_normalize_noc_attributes_048a(noc,'NOC_BLMdrilled.csv')
    new_n<-owner$pt_normalize_noc_attributes_048a(noc,'NOC_BLMdrilled.csv')
    assert_well_presentation_delta(old_n,new_n,'noc')
    old_a<-baseline$pt_normalize_mojave_attributes_048a(master,measurement,correction)
    assert_well_presentation_delta(old_a,out,'albion')
    # Exercise the same comparator on spatial objects with every metadata field.
    spatial<-function(x) sf::st_as_sf(x,coords=c('longitude','latitude'),crs=4326,remove=FALSE)
    assert_well_presentation_delta(spatial(old_n),spatial(new_n),'noc')
    assert_well_presentation_delta(spatial(old_a),spatial(out),'albion')
    for(family in c('noc','albion')) {
      b<-if(family=='noc') spatial(old_n) else spatial(old_a)
      a<-if(family=='noc') spatial(new_n) else spatial(out)
      bad<-a;bad$source_key[1]<-'wrong';fails(assert_well_presentation_delta(b,bad,family))
      bad<-a;bad$popup_html[1]<-paste0(bad$popup_html[1],' ');fails(assert_well_presentation_delta(b,bad,family))
      bad<-a;attr(bad,'agr')<-structure(attr(bad,'agr'),names=rev(names(attr(bad,'agr'))));fails(assert_well_presentation_delta(b,bad,family))
      bad<-a;sf::st_precision(bad)<-1;fails(assert_well_presentation_delta(b,bad,family))
      bad<-a;bad$longitude[1]<-bad$longitude[1]+1e-8;fails(assert_well_presentation_delta(b,bad,family))
      bad<-a;bad$well_name_raw[1]<-NA_character_;b$well_name_raw[1]<-'';fails(assert_well_presentation_delta(b,bad,family))
    }
    old_browser<-baseline$pt_prepare_blm_gw_well_inventory_records(old_n)
    new_browser<-owner$pt_prepare_blm_gw_well_inventory_records(new_n)
    assert_well_presentation_delta(old_browser,new_browser,'noc')
    assert_well_presentation_delta(baseline$pt_prepare_blm_gw_well_inventory_records(old_a),owner$pt_prepare_blm_gw_well_inventory_records(out),'albion')
    stopifnot(identical(body(baseline$pt_prepare_blm_gw_well_inventory_records),body(owner$pt_prepare_blm_gw_well_inventory_records)))
  })
  test('strict presentation comparator rejects unapproved text, metadata, basin value and blank/NA drift', {
    old_source<-"<div class='pt2-popup-row'><b>Source:</b> 2025 Mojave-BLM limited groundwater-well field investigation</div>"
    basin<-"<div class='pt2-popup-row'><b>Reported basin:</b> Basin &amp; A</div>"
    base<-data.frame(record_uid='synthetic',review_status='HUMAN REVIEW',identity_notes='',
      source_display='2025 Mojave-BLM limited field check',layer_name='GW wells | 2025 Mojave-BLM limited field check',popup_html=paste0('title',old_source,basin,'tail'))
    current<-base;current$source_display<-'Mojave limited field inventory (2025)';current$layer_name<-'GW sites | 2025 Mojave limited field inventory'
    current$popup_html<-paste0("title<div class='pt2-popup-row'><b>Source:</b> Mojave limited field inventory (2025)</div><div class='pt2-popup-row'><b>Field-reported basin:</b> Basin &amp; A</div>tail")
    assert_well_presentation_delta(base,current,'albion')
    bad<-current;bad$identity_notes<-NA_character_;fails(assert_well_presentation_delta(base,bad,'albion'))
    bad<-current;bad$popup_html<-sub('Basin &amp; A','Basin B',bad$popup_html,fixed=TRUE);fails(assert_well_presentation_delta(base,bad,'albion'))
    bad<-current;attr(bad,'extra')<-TRUE;fails(assert_well_presentation_delta(base,bad,'albion'))
    bad<-base;bad$popup_html<-paste0(bad$popup_html,old_source);fails(assert_well_presentation_delta(bad,current,'albion'))
    bad<-current;bad$popup_html<-paste0(bad$popup_html,"<b>Review status:</b> HUMAN REVIEW");fails(assert_well_presentation_delta(base,bad,'albion'))
    base<-data.frame(record_uid='noc_1',source_display='NOC well inventory',layer_name='GW wells | NOC',
      popup_html="<div class='pt2-popup-row'><b>Source:</b> NOC well inventory</div>",dist_to_blm_mi=0)
    current<-base;current$source_display<-'BLM National Operations Center';current$layer_name<-'GW wells | BLM NOC inventory'
    current$popup_html<-"<div class='pt2-popup-row'><b>Source:</b> BLM National Operations Center</div>"
    assert_well_presentation_delta(base,current,'noc')
    bad<-current;bad$dist_to_blm_mi<-1;fails(assert_well_presentation_delta(base,bad,'noc'))
    bad<-current;bad$source_display<-'NOC';fails(assert_well_presentation_delta(base,bad,'noc'))
  })
  local_owner<-function(directory) {
    env<-new.env(parent=globalenv());env$LOCAL_LAYER_FEATURE_COUNT_LABELS<-character()
    sys.source(file.path(directory,'00_config/config_local_layer_registry.r'),env)
    for(file in c(file.path(directory,'03_functions/leaflet_layer_local_core_helpers.r'),
                  file.path(root,'03_functions/leaflet_guide_helpers.r'))) {
      for(expr in parse(file)) if(is.call(expr) && identical(expr[[1]],as.name('<-')) && is.symbol(expr[[2]]) &&
        is.call(expr[[3]]) && identical(expr[[3]][[1]],as.name('function'))) eval(expr,env)
    }
    env
  }
  local<-local_owner(root)
  test('actual registry/group/count/Labels names and historical aliases converge for both inventories', {
    families<-list(
      list(id='blm_noc_drilled_wells',name='GW wells | BLM NOC inventory',count=287,
        aliases=c('GW wells | NOC','BLM-drilled wells | NOC','BLM-drilled wells | NOC database')),
      list(id='mojave_2025_gw_well_inventory',name='GW sites | 2025 Mojave limited field inventory',count=138,
        aliases=c('GW wells | 2025 Mojave-BLM limited field check','GW wells | 2025 Mojave-BLM field check')))
    builder<-readLines(file.path(root,'05_map_build/04_build_portatreasure2_core_map.r'),warn=FALSE)
    for(f in families) {
      row<-local$LOCAL_LAYER_REGISTRY[local$LOCAL_LAYER_REGISTRY$layer_id==f$id,]
      stopifnot(nrow(row)==1L,row$display_name==f$name,row$canonical_group==paste('Points –',f$name),!grepl('\\(',f$name))
      mains<-c(f$name,f$aliases);inputs<-c(mains,paste('Points –',mains),paste('Monitoring Sites / Records –',mains))
      label_inputs<-c(paste('Labels:',mains),paste('Labels –',mains),paste('Reference – Labels –',mains))
      local$LOCAL_LAYER_FEATURE_COUNT_LABELS<-character()
      stopifnot(all(local$pt_layer_group_name(inputs)==paste('Points –',f$name)),all(local$pt_layer_group_name(label_inputs)==paste('Labels –',f$name)))
      for(count in c(f$count,17)) { # Count remains supplied dynamically, never in the approved name.
        local$pt_register_local_layer_feature_counts(setNames(count,paste('Points –',f$name)))
        for(suffix in c('',' (287)',' (~1.2k)')) {
          groups<-local$pt_layer_group_name(paste0(inputs,suffix));labels<-local$pt_layer_group_name(paste0(label_inputs,suffix))
          stopifnot(all(groups==paste0('Points – ',f$name,' (',count,')')),all(labels==paste('Labels –',f$name)),
            identical(local$pt_layer_group_name(groups),groups),identical(local$pt_layer_group_name(labels),labels))
        }
      }
      stopifnot(sum(grepl(f$name,builder,fixed=TRUE))==4L,
        any(grepl(paste0('"Points – ',f$name,'" = pt_count_sf_rows(layers$',f$id,')'),builder,fixed=TRUE)),
        any(grepl(paste0('"Labels: ',f$name,'"'),builder,fixed=TRUE)))
    }
  })
  if(nzchar(before_dir)) test('actual pre-change mapping and Guide/catalog parity for every unaffected registry row', {
    old<-local_owner(before_dir)
    before<-old$LOCAL_LAYER_REGISTRY;after<-local$LOCAL_LAYER_REGISTRY
    renamed<-c('blm_noc_drilled_wells','mojave_2025_gw_well_inventory');keep<-!before$layer_id %in% renamed
    stopifnot(identical(before[keep,],after[keep,]),identical(before$layer_id,after$layer_id))
    for(nm in setdiff(names(before),c('display_name','canonical_group'))) stopifnot(identical(before[[nm]],after[[nm]]))
    plain<-c(before$display_name[keep],before$canonical_group[keep],paste('Labels:',before$display_name[keep]),paste('Labels –',before$display_name[keep]))
    local$LOCAL_LAYER_FEATURE_COUNT_LABELS<-character()
    for(suffix in c('',' (287)')) stopifnot(identical(old$pt_layer_group_name(paste0(plain,suffix)),local$pt_layer_group_name(paste0(plain,suffix))))
    old$pt_register_local_layer_feature_counts(setNames(rep(321,nrow(before)),before$canonical_group))
    local$pt_register_local_layer_feature_counts(setNames(rep(321,nrow(after)),after$canonical_group))
    stopifnot(identical(old$pt_layer_group_name(plain),local$pt_layer_group_name(plain)))
    markers<-local$pt_guide_descriptive_markers(file.path(root,'08_docs/catalog/BRIM_LAYER_CATALOG.csv'))
    stopifnot(length(markers)==26L,identical(markers,old$pt_guide_descriptive_markers(file.path(root,'08_docs/catalog/BRIM_LAYER_CATALOG.csv'))))
    old_products<-old$pt_guide_local_products(old$pt_layer_group_name(c(before$canonical_group,paste('Labels:',before$display_name))),markers)
    products<-local$pt_guide_local_products(local$pt_layer_group_name(c(after$canonical_group,paste('Labels:',after$display_name))),markers)
    ids<-vapply(products,`[[`,character(1),'id');old_ids<-vapply(old_products,`[[`,character(1),'id')
    stopifnot(identical(ids,old_ids),!anyDuplicated(ids))
    for(i in seq_along(ids)) {
      if(!ids[i] %in% renamed) stopifnot(identical(old_products[[i]],products[[i]])) else {
        a<-old_products[[i]];b<-products[[i]]
        expected<-if(ids[i]=='blm_noc_drilled_wells') 'GW wells | BLM NOC inventory' else 'GW sites | 2025 Mojave limited field inventory'
        provider<-if(ids[i]=='blm_noc_drilled_wells') 'Bureau of Land Management' else 'BRIM source data'
        stopifnot(b$title==expected,b$provider==provider,tail(b$path,1)==expected)
        for(nm in setdiff(names(a),c('title','provider','path','pathLabel'))) stopifnot(identical(a[[nm]],b[[nm]]))
      }
    }
    cat('PARITY registry_rows=',nrow(after),' unchanged_rows=',sum(keep),' Guide_products=',length(products),' catalog_markers=',length(markers),'\n',sep='')
  })
  cat('RESULT passed=',checks,' failed=0; real_normalization=NOT_RUN\n',sep='')
}
run_tests()
