# Pure extraction of the validated 48/49 USGS BLM distance method.
# Caller owns input/output paths; no implicit geometry lookup, fetch or write.
sm1_blm_join <- function(stations, geometry=NULL, geometry_hash=NULL, cache=NULL, calculated_at=format(Sys.time(),tz='UTC',usetz=TRUE)) {
 stopifnot(all(c('key','longitude','latitude')%in%names(stations)))
 coords<-digest::digest(stations[c('key','longitude','latitude')],algo='sha256')
 if(!is.null(cache)&&identical(cache$coordinate_sha256,coords)&&identical(cache$geometry_sha256,geometry_hash))return(cache)
 out<-data.frame(key=stations$key,on_blm_ca=NA,dist_to_blm_mi=NA_real_,dist_to_blm_ft=NA_real_,null_reason='public BLM-CA geometry unavailable')
 if(!is.null(geometry)) {
  if(is.null(geometry_hash)||is.na(sf::st_crs(geometry)))stop('geometry fingerprint and CRS required')
  g<-sf::st_geometry(sf::st_transform(sf::st_make_valid(geometry),3310));if(length(g)>1)g<-sf::st_union(g)
  ok<-is.finite(stations$longitude)&is.finite(stations$latitude)&abs(stations$longitude)<=180&abs(stations$latitude)<=90
  out$null_reason<-'public station coordinates unavailable'
  if(any(ok)) {
   pts<-sf::st_transform(sf::st_as_sf(stations[ok,],coords=c('longitude','latitude'),crs=4326),3310)
   on<-lengths(sf::st_intersects(pts,g))>0
   dist_m<-as.numeric(sf::st_distance(pts,g));dist_m[on]<-0
   out$on_blm_ca[ok]<-on;out$dist_to_blm_mi[ok]<-round(dist_m/1609.344,3);out$dist_to_blm_ft[ok]<-round(dist_m*3.280839895,0);out$null_reason[ok]<-NA_character_
  }
 }
 list(rows=out,blm_geometry_scope='BLM-California managed lands',geometry_sha256=geometry_hash,coordinate_sha256=coords,method='USGS 48/49 st_intersects + st_distance; EPSG:3310; miles 3 decimals, feet whole',calculated_at=calculated_at)
}
