
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
npfs$address <- paste(npfs$principalAddress,npfs$principalCity,npfs$principalState,npfs$principalZipCode,sep = ', ')

address_dt = data.table(address = unique(npfs$address))
address_dt$address <- paste(address_dt$address, ", USA")
source('../google_map_key')
library(ggmap)
register_google(google_map_key)
gcodes <- geocode(location = address_dt$address,source = 'google',output = 'latlon')

address_dt <- cbind(address_dt,gcodes)
address_dt[grepl('Gunnison',address),]
npfs$address <- paste(npfs$address,', USA')
npfs_with_codes <- left_join(npfs,address_dt)
#npfs_with_codes <- npfs_with_codes[grepl('Gunnison',address),]
npfs_with_codes <- npfs_with_codes[!is.na(npfs_with_codes$lon),]
df <- npfs_with_codes  %>% select(lon,lat) %>% st_as_sf( coords = c("lon", "lat"), crs = 4326)
df <- st_transform(df,st_crs(tig_tract))
np_counts = st_contains(tig_tract,df)
tig_tract$Registered_Nonprofit_Count <- sapply(np_counts,length)

infra_counts <-lapply(list.files('scratch/infra_gis/',full.names = T),function(x) {
  print(x)
  temp <- st_read(x)
  temp <- st_transform(temp,st_crs(tig_tract)) 
  sapply(st_contains(tig_tract,temp),length)
})

tig_tract$Service_Infrastructure_Count <- (rowSums(do.call(cbind,infra_counts)))

tig_org_ecology <- tig_tract %>% as.data.frame() %>% select(GEOID,Service_Infrastructure_Count,Registered_Nonprofit_Count,local_gov_intersects)
saveRDS(tig_org_ecology,'building_blocks/tract_org_ecology.RDS')



