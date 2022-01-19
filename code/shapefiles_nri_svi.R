filepath1<-"Documents/GitHub/cimarron"
filepath2<-"Documents/CARES_FUND/data_correlates"
govsall<-readRDS(paste0(filepath1,"/building_blocks/CARESGOVS.rds"))
sds<-sf::read_sf(paste0(filepath2,"/sd_shapes/dlall.shp"))
munis<-sf::read_sf(paste0(filepath2,"/muni_shapes/MuniBounds.shp"))
schools<-sf::read_sf("Documents/GitHub/cimarron/building_blocks/CDPHE_CDOE_School_District_Boundaries/CDPHE_CDOE_School_District_Boundaries.shp")
head(schools)
muni_data<-rvest::read_html("https://dola.colorado.gov/lgis/municipalities.jsf")
muni_data<-muni_data %>% rvest::html_table() %>% .[[2]] %>% rename(lgid=X4,fips=X5,LGTYPEID=X3)
muni_data$fips<-muni_data$fips %>% stringr::str_pad(.,5,"left",0)
counties<-sf::read_sf(paste0(filepath2,"/counties/Colorado_County_Boundaries.shp"))
sds<-sds %>% select(LGID,LGNAME,LGTYPEID,LGSTATUSID)
munis<-munis %>% rename(fips=city,LGNAME=first_city) %>% select(-id) %>% merge(muni_data %>% select(LGTYPEID,lgid,fips), all.x=T,all.y=T)
munis$LGSTATUSID<-1
munis<-munis %>% rename(LGID=lgid)
munis<-munis %>% select(LGID,LGNAME,LGTYPEID,LGSTATUSID)
munis$LGID<-munis$LGID %>% stringr::str_pad(.,5,"left",0)
sds<-rbind(munis,sds)

counties<-merge(govsall[govsall$type==1,] %>% mutate(LABEL=tolower(county)) %>% select(LABEL,lgid),counties %>% mutate(LABEL=tolower(LABEL)) %>% select(LABEL))
counties<-counties %>% rename(LGNAME=LABEL,LGID=lgid) %>% mutate(LGTYPEID=1,LGSTATUSID=1)
counties$LGID<-stringr::str_pad(counties$LGID,5,"left",0)
sds<-rbind(sds,counties %>% sf::st_as_sf() %>% sf::st_transform(.,sf::st_crs(sds)))

govsall$lgid<-govsall$lgid %>% stringr::str_pad(.,5,"left",0)
head(govsall)
head(sds)
govsall2<-left_join(govsall,sds %>% rename(lgid=LGID))
head(govsall2)

govsall2$name<-govsall$name %>% stringr::str_split("\n",2) %>% sapply(.,function(X) X[1])
govsall2<-govsall2 %>% select(-junk)
head(govsall2)
govsall2 %>% saveRDS("Documents/CARES_FUND/data_correlates/government_shapes.rds")
install.packages("tidycensus")
tig.tracts<-tigris::tracts("CO",year=2019,class="sf")
#svi
svi<-sf::read_sf(paste0(filepath2,"/svi/SVI2018_COLORADO_tract.shp"))
svi<-sf::st_transform(sf::st_make_valid(svi),sf::st_crs(govsall2))
svi<-sf::st_make_valid(svi)
govsall2<-sf::st_as_sf(govsall2)
govsall2<-sf::st_make_valid(govsall2)
sf::sf_use_s2(FALSE)
svi.i<-sf::st_intersects(govsall2,svi)
govsall2$tracts<-lapply(svi.i,function(X) svi$FIPS[X])

#nri
nri<-sf::read_sf(paste0(filepath2,"/nri/NRI_Table_CensusTracts_Colorado.csv"))
risk_score<-sapply(govsall2$tracts,function(X){
  filter(nri,TRACTFIPS%in%X)$RISK_SCORE %>% as.numeric() %>% mean(na.rm=T)
})
govsall2$nri_risk_score<-risk_score

svi_scores<-lapply(govsall2$tracts,function(X){
  temp<-filter(svi,FIPS%in%X)
  temp<-temp %>% as.data.frame %>% select(RPL_THEME1,RPL_THEME2,RPL_THEME3,RPL_THEME4) %>% mutate_all(function(X) ifelse(X==-999,NA,X))
  data.frame("RPL_THEME1"=mean(temp$RPL_THEME1,na.rm=T),
             "RPL_THEME2"=mean(temp$RPL_THEME2,na.rm=T),
             "RPL_THEME3"=mean(temp$RPL_THEME3,na.rm=T),
             "RPL_THEME4"=mean(temp$RPL_THEME4,na.rm=T))
})
names(svi_scores)<-govsall2$lgid
svi_scores<-bind_rows(svi_scores,.id="lgid")
govsall2<-merge(govsall2,svi_scores,all.x=T)
head(govsall2)


govsall2 %>% saveRDS("Documents/CARES_FUND/data_correlates/government_shapes.rds")
head(govsall2)
