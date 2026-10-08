#!/usr/bin/env Rscript
# SM1 review-only config; actual Ops + Tools + legend modules. No live providers.
a<-commandArgs(TRUE)
if(length(a)<2)stop('OUTPUT.html STATIC_DATA_BASE_URL [PUBLIC_OUTLINE.geojson] [legacy]')
source('03_functions/leaflet_core_helpers.r');source('03_functions/layer_capability_helpers.r');source('03_functions/leaflet_ops_live_helpers.r');source('03_functions/leaflet_tools_adddata_helpers.r')
base<-sub('/$','',a[2]);legacy<-'legacy'%in%a
flags<-list(add_ops_live_layers=TRUE,add_ops_conus_radar=FALSE,add_ops_dendra_daily=TRUE,ops_dendra_daily_index_url=paste0(base,'/dendra/index.json'),add_ops_scan_soil_moisture_latest=TRUE,ops_scan_soil_moisture_latest_url=paste0(base,'/legacy/scan_soil_moisture_latest.geojson'),ops_scan_soil_moisture_summary_url=paste0(base,'/legacy/scan_soil_moisture_latest_summary.json'),ops_scan_soil_moisture_trace_url=paste0(base,'/legacy/scan_soil_moisture_current_wy_trace.csv'),ops_scan_depth_style_url=paste0(base,'/legacy/scan_depth_style.csv'),ops_scan_waterday_percentiles_url=paste0(base,'/legacy/scan_sms_waterday_percentiles.csv'),ops_scan_monthly_context_url=paste0(base,'/legacy/scan_sms_monthly_context.csv'),ops_scan_prior_wy_fallback_traces_url=paste0(base,'/legacy/scan_sms_prior_wy_fallback_traces.csv'),ops_soil_moisture_shared=!legacy,ops_soil_moisture_indexes=list(scan=paste0(base,'/scan.json'),dendra=paste0(base,'/dendra.json'),snotel=paste0(base,'/snotel.json')),ops_soil_moisture_snotel_pilot=!legacy,add_tools_adddata_panel=TRUE,add_blm_sma_context_overlay=FALSE)
m<-leaflet::leaflet(options=leaflet::leafletOptions(preferCanvas=FALSE),width='100%',height='100vh')
m<-pt_add_panes(m) # Existing production pane_ops=560, below tooltip/popup panes.
m<-leaflet::setView(m,lng=-119,lat=37.5,zoom=6)
if(length(a)>=3&&file.exists(a[3])){
 m<-leaflet::addPolygons(m,data=sf::st_read(a[3],quiet=TRUE),group='Review public state outlines',color='#99aaab',weight=1,fillColor='#f8f7ee',fillOpacity=1,options=leaflet::pathOptions(interactive=FALSE))
 m<-leaflet::addLayersControl(m,overlayGroups='Review public state outlines',options=leaflet::layersControlOptions(collapsed=FALSE),position='topleft')
 m<-pt_add_layer_control_headers(m)
 m<-htmlwidgets::onRender(m,"function(){this.attributionControl.addAttribution('Natural Earth · public domain');}")
}

m<-leaflet::addControl(m,htmltools::HTML('<div style="background:#fff;padding:9px;border:1px solid #bbcaca;border-radius:5px;font:12px Arial"><strong>Soil moisture · SM1 review</strong><br>Frozen, mixed source vintages · SNOTEL: 3-site pilot<br>BLM geometry unavailable · full production build untested</div>'),position='topright')
m<-htmlwidgets::onRender(m,paste(readLines('03_functions/js/brim_legend_closeout_helpers.js',warn=FALSE),collapse='\n'))
m<-pt_add_ops_live_layers(m,flags);m<-pt_add_tools_adddata_panel(m,flags)
m<-htmlwidgets::onRender(m,'function(el){window.soilReviewMap=this;}')
htmlwidgets::saveWidget(m,file=a[1],selfcontained=FALSE,libdir=paste0(tools::file_path_sans_ext(basename(a[1])),'_files'))
cat('SM1 focused actual Ops/Tools review build: shared=',!legacy,'\n')
