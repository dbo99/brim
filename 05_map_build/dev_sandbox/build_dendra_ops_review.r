#!/usr/bin/env Rscript
# Focused public-data integration build, using actual Ops/Tools assembly.
# Run from the BRIM source root. No preprocessing, remote fetches, or private GIS.
a<-commandArgs(TRUE)
if(!length(a)%in%c(2,3))stop("Usage: Rscript build_dendra_ops_review.r OUTPUT.html STATIC_INDEX_URL [PUBLIC_OUTLINE.geojson]")
for(p in c("leaflet","htmlwidgets","htmltools","jsonlite"))if(!requireNamespace(p,quietly=TRUE))stop("Missing existing local dependency: ",p)
source("03_functions/layer_capability_helpers.r")
source("03_functions/leaflet_ops_live_helpers.r")
source("03_functions/leaflet_tools_adddata_helpers.r")
flags<-list(add_ops_live_layers=TRUE,add_ops_conus_radar=FALSE,add_ops_dendra_daily=TRUE,ops_dendra_daily_index_url=a[2],add_tools_adddata_panel=TRUE,add_blm_sma_context_overlay=FALSE)
m<-leaflet::leaflet(options=leaflet::leafletOptions(preferCanvas=FALSE),width="100%",height="100vh")
m<-leaflet::setView(m,lng=-116.6084,lat=34.93652,zoom=6)
if(length(a)==3){
  outline<-jsonlite::fromJSON(a[3],simplifyVector=FALSE)
  m<-htmlwidgets::onRender(m,"function(el,x,data){L.geoJSON(data,{style:{color:'#99aaab',weight:1,fillColor:'#f8f7ee',fillOpacity:1},interactive:false}).addTo(this);this.attributionControl.addAttribution('Natural Earth · public domain');}",data=outline)
}
m<-leaflet::addControl(m,htmltools::HTML('<div style="background:#fff;padding:8px 12px;border:1px solid #bbcaca;border-radius:5px;font:12px Arial"><strong>Dendra · BRIM Ops Live review</strong><br>Focused integration build · frozen public observations · full production map untested</div>'),position="topright")
m<-pt_add_ops_live_layers(m,flags)
m<-pt_add_tools_adddata_panel(m,flags)
# A reference for exercising the real Leaflet layer lifecycle in local browser QA.
m<-htmlwidgets::onRender(m,"function(el){window.dendraReviewMap=this;}")
htmlwidgets::saveWidget(m,file=a[1],selfcontained=FALSE,libdir=paste0(tools::file_path_sans_ext(basename(a[1])),"_files"))
cat("Focused real Ops/Tools integration written; no production map acceptance claimed.\n")
