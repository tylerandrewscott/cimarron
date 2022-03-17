
library(sf)
library(tidyverse)
library(tigris)
library(tidycensus)
library(data.table)
library(cowplot)
library(ggmap)
library(ggmap)

library(osmdata)

tig_tract <- tigris::tracts(state = 'CO',year = 2019,class = 'sf')

sd_geo <- st_read('scratch/All Special Districts in Colorado.geojson')
sd_geo <- st_make_valid(sd_geo)
sd_geo <- st_transform(sd_geo,crs = st_crs(tig_tract))
mun_geo <- st_read('scratch/Municipal Boundaries in Colorado.geojson')
mun_geo <- st_make_valid(mun_geo)
mun_geo <- st_transform(mun_geo,crs = st_crs(tig_tract))
library(rgeos)

muni_inters <- st_intersects(tig_tract,mun_geo)

library(pbapply)
# city_count = pbsapply(seq_along(muni_inters),function(i){
# 
#   if(length(muni_inters[[i]])==0){NA}
#   else if(length(muni_inters[[i]])==1){1}
#   else{
#     rm(temp_inter)
#     temp_inter = st_intersection(x = tig_tract[i,],
#                                  y = mun_geo[muni_inters[[i]],],)
#     temp_inter = st_make_valid(temp_inter)
#     large_inter = round(st_area(temp_inter) / st_area(tig_tract[i,]),2)
#     sum(as.numeric(large_inter)>0.00)
#   }
# },cl = 1)
# city_count[is.na(city_count)] <- 0

sd_geo = st_make_valid(sd_geo)
sf::sf_use_s2(FALSE)
sd_inters <- st_intersects(tig_tract,sd_geo)
tig_tract$local_gov_intersects <- sapply(sd_inters,length) + sapply(muni_inters,length)

# bus <- fread('scratch/All_CO_businesses_geocoded.csv')
# bus <- bus[entitystatus %in% c('Exists', 'Good Standing'),]
# bus_by_zip = bus[,.N,by=.(principalzipcode)][order(-N),]
# zct_to_tract = fread('https://www2.census.gov/geo/docs/maps-data/data/rel/zcta_tract_rel_10.txt')
# #match(bus_by_zip$principalzipcode,zct_to_tract$ZCTA5)

npfs = fread('scratch/Charity_registrations__filtered_to_CO.csv')
npfs = npfs[inBusiness==T,]
npfs = npfs[!duplicated(entityId),]
npfs$query <- paste(npfs$principalAddress,npfs$principalCity,npfs$principalState,npfs$principalZipCode,sep = ', ')
npfs$query <- str_replace_all(npfs$query,'\\#','STE ')
npfs2 <- data.table(query = npfs$query,x = NA,y = NA)

count = 0

#### first batch OSM
for(i in 1:nrow(npfs2)){
  if(is.na(npfs2$x[i])){
    print(i)
  temp = tmaptools::geocode_OSM(q = npfs2$query[i],keep.unfound = T)
  count= count + 1
  npfs2$x[i]<-temp$coords[1]
  npfs2$y[i]<-temp$coords[2]
  if(count==500){Sys.sleep(10);count=0}
  }
}

#### second batch google (costs but better)
library(ggmap)
source('../google_map_key')
register_google(google_map_key)



#### first batch OSM
for(i in 1:nrow(npfs2)){
  if(is.na(npfs2$x[i])){
    print(i)
    temp = ggmap::geocode(npfs2$query[i])
    count= count + 1
    npfs2$x[i]<-temp$lon
    npfs2$y[i]<-temp$lat
    if(count==500){Sys.sleep(10);count=0}
  }
}

npfs2 <- npfs2[!is.na(x),]
npfs = left_join(npfs,npfs2)
saveRDS(npfs,'scratch/geotagged_nonprofits.rds')




bus = fread('scratch/Business_Entities_in_Colorado.csv')
bus = bus[entitystatus %in% c('Exists','Good Standing'),]
bus = bus[!duplicated(entityid),]
bus <- bus[principalstate=='CO',]
bus$query <- paste(bus$principaladdress1,bus$principalcity,bus$principalstate,bus$principalzipcode,sep = ', ')
bus$query <- str_replace_all(bus$query,'\\#','STE ')
bus2 <- data.table(query = bus$query,x = NA,y = NA)
count = 0

nas = sample(which(is.na(bus2$x)))
nas_ntile = dplyr::ntile(nas,length(nas)/1000)
uq_ntile = unique(nas_ntile)
#### first batch OSM
for(i in rev(uq_ntile)){
    temp = tmaptools::geocode_OSM(q = bus2$query[nas[nas_ntile==i]],keep.unfound = T)
    #count= count + 1
    bus2$x[nas[nas_ntile==i]]<-temp$lon
    bus2$y[nas[nas_ntile==i]]<-temp$lat
    #bus2$x[i]<-temp$lon
    #bus2$y[i]<-temp$lat
    Sys.sleep(1)
  }

nas = sample(which(is.na(bus2$x)))
nas_ntile = dplyr::ntile(nas,length(nas)/1000)
uq_ntile = unique(nas_ntile)
#### second batch google (costs but better)
for(i in uq_ntile){
    print(i)
    temp = ggmap::geocode(location = bus2$query[nas[nas_ntile==i]])
    #count= count + 1
    bus2$x[nas[nas_ntile==i]]<-temp$lon
    bus2$y[nas[nas_ntile==i]]<-temp$lat
  }
}



npfs2 <- npfs2[!is.na(x),]
npfs = left_join(npfs,npfs2)
saveRDS(npfs,'scratch/geotagged_nonprofits.rds')







