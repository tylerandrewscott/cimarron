
library(sf)
library(tidyverse)
library(tigris)
library(tidycensus)
library(data.table)
library(cowplot)
library(ggmap)
library(ggmap)

library(osmdata)

grant_geos <- readRDS('scratch/complex_grant_values_by_geometry.rds')
grant_geos <- grant_geos[grant_geos$scope_recode!='STATE-WIDE',]

county_grants <- grant_geos[grant_geos$scope_recode=='COUNTY',]
cog_geos <- grant_geos[grant_geos$scope_recode=='COG',]
grant_geos <- grant_geos[!grant_geos$scope_recode%in%c('COG','COUNTY'),]
#cog_ids <- fread('https://raw.githubusercontent.com/tylerascott/cimarron/main/building_blocks/transplanid.csv')
#cog_ids$FIPS <- paste0('08',formatC(cog_ids$COUNTYFP,width = 3,flag = 0))
#cog_geos <- grant_geos[grant_geos$scope_recode=='COG',]
co_counties <- tigris::counties(state ='CO',cb=T,class = 'sf')
county_totals <- as.data.table(county_grants)[,sum(value),by=.(combined_geo)]

tract_tags <- readRDS('scratch/shapefiles_and_tracts_matching_combined_geos.rds')
key = source('../census_key')
census_api_key(key$value)
tract_pop <- get_acs(geography = 'tract',state = 'CO',variables ='B01001_001',year = 2019)
tract_tags$totpop <- NA
for(i in 1:nrow(tract_tags)){
  tract_tags$totpop[i] = sum(tract_pop$estimate[match(tract_tags$tracts[[i]],tract_pop$GEOID)])
}

grant_geos$total_pop <- tract_tags$totpop[match(grant_geos$combined_geo,tract_tags$combined_geo)]
grant_geos <- grant_geos[grant_geos$total_pop>0,]
grant_geos$amount_per_person <- grant_geos$value / grant_geos$total_pop

grant_geos$tract_count <- sapply(tract_tags$tracts[match(grant_geos$combined_geo,tract_tags$combined_geo)],length)

grant_geos$amount_per_tract <- grant_geos$value / grant_geos$tract_count
grant_geos <- data.table(grant_geos)
grant_geos$tracts <- tract_tags$tracts[match(grant_geos$combined_geo,tract_tags$combined_geo)]

tract_award_dt <- lapply(1:nrow(grant_geos),function(i) {
  data.table(award_geo = grant_geos$combined_geo[i],
             tract = grant_geos$tracts[[i]],
             tract_award = grant_geos$amount_per_person[i] * tract_pop$estimate[match(grant_geos$tracts[[i]],tract_pop$GEOID)],
              tract_award_population_independent = grant_geos$amount_per_tract[i]        
             )})

tract_dt <- rbindlist(tract_award_dt)

tract_totals_dt <- tract_dt[,list(sum(tract_award),sum(tract_award_population_independent)),by=.(tract)]


tract_totals_dt$total_pop <- tract_pop$estimate[match(tract_totals_dt$tract,tract_pop$GEOID)]
setnames(tract_totals_dt,c('V1','V2','tract'),c('total_award','tract_award_population_independent','GEOID'))


tig_tract <- tigris::tracts(state = 'CO',year = 2019,class = 'sf')

tig_tract <- merge(tig_tract,tract_totals_dt)
tig_tract$CFIPS <- paste0('08',tig_tract$COUNTYFP)
tig_tract$CFIPS_LEVEL_AWARD <- county_totals$V1[match(tig_tract$CFIPS,county_totals$combined_geo)]
svi <- fread('scratch/Colorado_SVI_2018.csv')

svi$GEOID <- paste0('0',as.character(svi$FIPS))

tig_tract <- merge(tig_tract,svi) 
tig_tract$y <- tig_tract$total_award/tig_tract$total_pop

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





geocode()

is.na(npfs2$result[2000])
npfs2$result[1:10]



test= tmaptools::geocode_OSM(npfs$address[1])

test
npfs$principalZipCode <- str_extract(npfs$principalZipCode,'^[0-9]{5}')
nfps_by_zip = npfs[,.N,by=.(principalZipCode)]

library(ggthemes)
library(viridis)
# k_var <- 'median income ($1k)'
# m_var <- 'total award ($1M)'

g1 <- ggplot() + theme_map() + 
  scale_fill_viridis_c(name = 'SVI percentile',
                       breaks = c(.1,.5,.9),labels=c('10th','50th','90th')) + 
  scale_color_viridis_c(name = 'SVI percentile',
                       breaks = c(.1,.5,.9),labels=c('10th','50th','90th')) + 
  ggtitle('SVI by census tract') + #theme(legend.background = element_rect(fill = alpha('white',.5)))+
geom_sf(data = tig_tract[tig_tract$RPL_THEMES!='-999',],aes(fill = RPL_THEMES,col = RPL_THEMES)) +
  NULL


tig_tract$y <- ifelse(tig_tract$y>2000,2000,tig_tract$y)
g2 <- ggplot() + theme_map() + 
  scale_fill_viridis_c(name = 'award/capita',breaks=c(500,1000,2000),labels=c('$500','$1000','$2000+')) + 
  scale_colour_viridis_c(name = 'award/capita',breaks=c(500,1000,2000),labels=c('$500','$1000','$2000+'))+
  ggtitle('award/capita by census tract') +
 # theme(legend.background = element_rect(fill = alpha('white',.5)))+
  geom_sf(data = tig_tract,aes(fill = y,col = y)) + 
  NULL
library(cowplot)
gg_co_cow = ggdraw() +
draw_plot(g1,width = 1,height = .5,y = 0) + draw_plot(g2,width = 1,height = .5,y = 0.5)
ggsave(gg_co_cow,filename = 'output/figures/colorado_tracts_svi_and_awards.png',dpi = 500)



summary(tig_tract$total_award[tig_tract$RPL_THEMES!='-999']/tig_tract$total_pop[tig_tract$RPL_THEMES!='-999'])
# 
# g2 <- g_base + 
#   ggtitle('total CARES award estimate by census tract')+
#   geom_sf(data = tig_tract,aes(fill = total_award/1e6,col = total_award/1e6)) +
#   labs(fill = m_var,col = m_var)
#   
# cbsa <- tigris::combined_statistical_areas(year = 2019, cb = T,class = 'sf')    
# denver_cbsa <- cbsa[grepl('Denver',cbsa$NAME),]
# denver <- tigris::places(state = 'CO',cb = T,year = 2019,class = 'sf')
# denver <- denver[grepl('Denver',denver$NAME),]
# 
# counties <- counties(state = 'CO',cb = T,year =2019,class = 'sf')
# denver_centroid <- st_centroid(denver)
# denver_buffer <- st_buffer(denver_centroid,30000)
# denver_tract <- st_intersects(tig_tract,denver_buffer)
# keep <- sapply(denver_tract,length)>0
# 
# g3 <- g_base + 
#   geom_sf(data = tig_tract[keep,],aes(fill = med_income,col = med_income))+
#   scale_x_continuous(limits=c(NA,-104.6)) + 
#   scale_y_continuous(limits = c(39.4,40.1)) +
#   ggtitle('median income, central Denver')+  labs(fill = k_var,col = k_var)
# g4 <- g_base + 
#   geom_sf(data = tig_tract[keep,],aes(fill = total_award/1e6,col = total_award/1e6))+
#   scale_x_continuous(limits=c(NA,-104.6)) + 
# scale_y_continuous(limits = c(39.4,40.1)) +
#  ggtitle('total CARES award, central Denver')+ 
#   labs(fill = m_var,col = m_var)
# library(gridExtra)
# grid.arrange(g3,g4,ncol = 2)

#grid.arrange(g1,g2,g3,g4,ncol = 2)

denver_inset <- ggplot() + geom_sf(data = tig_tract) + theme_map() + 
  geom_sf(data = denver_buffer,col = 'red',fill = NA)

source('../static_google_maps_key')
source('../census_key')



# get the map info
# map <- get_googlemap("Colorado, USA", zoom = 7, maptype = "terrain")
# 
# states <- tigris::states(cb = T,year = 2019,class = 'sf')
# co_state<-states[states$STUSPS=='CO',]
# co_bbox <- st_bbox(co_state)
# names(co_bbox) <- c('left','bottom','right','top')
# # use the bounding box to get a stamen map
# stamMap <- get_stamenmap(co_bbox,crop = T)
# ggmap(stamMap)
# ggmap(stamMap)+ theme_map()
#   geom_sf(data = denver_buffer,col = 'red')
# 
# gg_inset_map = ggdraw() +
#   draw_plot(g3,width = 0.5,height = 1) + draw_plot(g4,width = 0.5,height = 1)+
#   draw_plot(denver_inset, x = 0.05, y = 0.65, width = 0.3, height = 0.3)


#$tig_tract <- tig_tract[tig_tract$total_pop>0,]

# 
# tract_dp02<- get_acs(geography = 'tract',state = 'CO',year = 2019,table = 'DP02',survey = 'acs5')
# tract_dp03<- get_acs(geography = 'tract',state = 'CO',year = 2019,table = 'DP03',survey = 'acs5')
# tract_dp03 <- data.table(tract_dp03)
# #DP03_0045PE #% employed in public administration # Percent!!INDUSTRY!!Civilian employed population 16 years and over!!Public administration
# #DP03_0043PE # Percent!!INDUSTRY!!Civilian employed population 16 years and over!!Arts, entertainment, and recreation, and accommodation and food services
# #DP03_0024PE #Estimate!!COMMUTING TO WORK!!Workers 16 years and over!!Worked from home
# jvars <- c('DP03_0045P','DP03_0043P','DP03_0024P')
# tract_job_stats <- dcast(tract_dp03[variable %in% jvars,.(GEOID,variable,estimate)],GEOID ~ variable,value.var = 'estimate')
# setnames(tract_job_stats,jvars,c('PA_employment_percent','Service_employment_percent','Percent_work_from_home'))
# tig_tract <- left_join(tig_tract,tract_job_stats)

tig_tract$km_sq <- st_area(tig_tract)/1e6
tig_tract$popdensity_kmsq <- tig_tract$total_pop/tig_tract$km_sq

co_hospitals <- st_read('scratch/Hospitals.geojson')
co_hospitals <- co_hospitals[co_hospitals$STATE=='CO',]
co_hospitals <- st_transform(co_hospitals,st_crs(tig_tract))
in_tract <- st_covered_by(co_hospitals,tig_tract)
co_hospitals$GEOID <- sapply(1:length(in_tract),function(i) tig_tract[i,]$GEOID)

co_hospitals$TRAUMA_HOSPITAL <- (co_hospitals$TRAUMA!='NOT AVAILABLE') + 0
co_hospitals$BEDS[co_hospitals$BEDS==-999] <- 0

# hospitals_by_tract <- co_hospitals %>% group_by(GEOID) %>% summarize(hospital_beds = sum(BEDS),
#                                                                      trauma_hospitals = sum(TRAUMA_HOSPITAL))

hospital_by_tract <- as.data.table(table(co_hospitals$GEOID))
tig_tract$HOSPITALS <- hospital_by_tract$N[match(tig_tract$GEOID,hospital_by_tract$V1)]
tig_tract$HOSPITALS[is.na(tig_tract$HOSPITALS)]<-0

infra <- fread('https://raw.githubusercontent.com/tylerascott/cimarron/main/building_blocks/infrastructure.csv')
infra[,V1:=NULL]
infra$GEOID <- paste0('0',infra$GEOID)
infra$INFRA_COUNT <- rowSums(infra %>% select(-GEOID))

tig_tract <- left_join(tig_tract,infra)

tig_tract$INFRA_COUNT <- tig_tract$INFRA_COUNT + tig_tract$HOSPITALS
# 
# tig_tract$COLLEGES[is.na(tig_tract$COLLEGES)]<-0
# tig_tract$DODSITE[is.na(tig_tract$DODSITE)]<-0
# tig_tract$BANKS[is.na(tig_tract$BANKS)]<-0
# tig_tract$FORTUNE500[is.na(tig_tract$FORTUNE500)]<-0
# tig_tract$STATEGOV[is.na(tig_tract$STATEGOV)]<-0
# tig_tract$PRISON[is.na(tig_tract$PRISON)]<-0
# tig_tract$RUNWAYS[is.na(tig_tract$RUNWAYS)]<-0
# 
# tig_tract$COLLEGES <- (tig_tract$COLLEGES>0)
# tig_tract$DODSITE <- (tig_tract$DODSITE>0)
# tig_tract$BANKS <- (tig_tract$BANKS>0)
# tig_tract$FORTUNE500 <- (tig_tract$FORTUNE500>0)
# tig_tract$STATEGOV <- (tig_tract$STATEGOV>0)
# tig_tract$PRISON <- (tig_tract$PRISON>0)
# tig_tract$RUNWAYS <- (tig_tract$RUNWAYS>0)

# 
# tig_tract$trauma_units[is.na(tig_tract$trauma_units)] <- 0
# tig_tract$trauma_units <- tig_tract$trauma_units>0

# 
# library(readxl)
# ruca <- read_excel('scratch/ruca2010revised.xlsx',skip = 1)
# ruca <- ruca[ruca$`Select State`=='CO',]
# tig_tract$primary_ruca <- ruca$`Primary RUCA Code 2010`[match(tig_tract$GEOID,ruca$`State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)`)]
# tig_tract$secondary_ruca <- ruca$`Secondary RUCA Code, 2010 (see errata)`[match(tig_tract$GEOID,ruca$`State-County-Tract FIPS Code (lookup by address at http://www.ffiec.gov/Geocode/)`)]

library(forcats)
# tig_tract$RUCA <- tig_tract$primary_ruca
# 
# tig_tract$RUCA<-fct_collapse(as.factor(tig_tract$RUCA), 
#                              'Metro_Core' = as.character(1),
#                              'Metro_Area' = as.character(2:3),
#                              'Micropolitan' = as.character(4:6),
#                              'Small_Town' = as.character(7:9),
#                              'Rural' = c('10'))
# library(ggthemes)
# ggplot() + theme_map() + 
#   ggtitle('Colorado census tracts',
#           'Rural-Urban Commuting Area codes')+
#   geom_sf(data = tig_tract[tig_tract$RUCA!='99',],aes(fill = RUCA,col = RUCA)) + 
#   scale_color_colorblind() + scale_fill_colorblind()

library("spdep")
library("spatialreg")
library(INLA)
tract.adj.q <- poly2nb(tig_tract)
W.bin <- nb2mat(tract.adj.q, style = "B")
tig_tract$ID <- 1:nrow(tig_tract)
#county random intercept
seed = 24

# Set prior on precision

pc.prec.u = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))
bprior <- list(prior = 'gaussian', param = c(0,1))
pc.prec = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))
cres <- list(return.marginals.predictor = FALSE, 
             return.marginals.random = FALSE)
u.resp <- sd(scale(log(tig_tract[tig_tract$RPL_THEMES!='-999',]$total_award/
                         tig_tract[tig_tract$RPL_THEMES!='-999',]$total_pop)))
tig_tract[tig_tract$RPL_THEMES!='-999'&tig_tract$RUCA=='99',]
famcontrol = list(list(prior = "pcprec", param = c(u.resp,0.01)))
                  
#ggplot(data = tig_tract[tig_tract$RPL_THEMES!=-999,]) + geom_point(aes(x = log(total_pop),y = log(total_award)))

mod0a <- inla(scale(log(total_award/total_pop)) ~ 1 + 
                scale(log(CFIPS_LEVEL_AWARD+1)) +
                f(CFIPS,model = 'iid',hyper = pc.prec.u),
              control.compute = list(waic = T,dic = T,cpo = T),
              control.family = list(hyper = pc.prec),
              #control.results = cres,
              control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
              family = 'gaussian',data = tig_tract[tig_tract$RPL_THEMES!=-999,])

#besag tract term

mod0b <- inla(scale(log(total_award/total_pop)) ~ 1 + 
                scale(log(CFIPS_LEVEL_AWARD+1)) +
                scale(log(popdensity_kmsq)) +
                f(CFIPS,model = 'iid',hyper = pc.prec.u)+
                f(as.integer(ID),model = 'besag',graph = W.bin),
              control.family = list(hyper = pc.prec),
              control.compute = list(waic = T,dic = T,cpo = T),
              control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
              family = 'gaussian',data = tig_tract[tig_tract$RPL_THEMES!=-999,])

mod1 <- inla(scale(log(total_award/total_pop)) ~ 1 +
               scale(log(CFIPS_LEVEL_AWARD+1)) +
               f(CFIPS,model = 'iid',hyper = pc.prec.u)+
               f(as.integer(ID),model = 'besag',graph = W.bin) +
               scale(log(popdensity_kmsq)) + scale(INFRA_COUNT) +
               scale(RPL_THEMES),
             control.family = list(hyper = pc.prec),
             control.compute = list(waic = T,dic = T,cpo = T),
             control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
             family = 'gaussian',data = tig_tract[tig_tract$RPL_THEMES!=-999,])
mod1_check <- inla(scale(log(tract_award_population_independent/total_pop)) ~ 1 +
               scale(log(CFIPS_LEVEL_AWARD+1)) +
               f(CFIPS,model = 'iid',hyper = pc.prec.u)+
               f(as.integer(ID),model = 'besag',graph = W.bin) +
               scale(log(popdensity_kmsq)) + scale(INFRA_COUNT) +
               scale(RPL_THEMES),
               control.family = list(hyper = pc.prec),
               control.compute = list(waic = T,dic = T,cpo = T),
               control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
             family = 'gaussian',data = tig_tract[tig_tract$RPL_THEMES!=-999,])

getCoefs <- function(x) {
   temp = x$summary.fixed[,c(1,3,5)]
   temp$coef = rownames(temp)
   temp
}

cdt <- rbindlist(list(getCoefs(mod0b) %>% mutate(mod = '0'),
getCoefs(mod1) %>% mutate(mod = '1')))
cdt$coef <- fct_inorder(cdt$coef)
cdt$coef <- fct_rev(cdt$coef)

coef_plot <- ggplot(cdt[mod==1,]) + 
  geom_segment(aes(y = coef,yend = coef,x = `0.025quant`,xend = `0.975quant`,group = mod),
               position = position_dodge(width = 0.25)) +
  geom_point(aes(x = mean,y = coef,shape = `0.025quant`<0 & `0.975quant`>0)) + 
  theme_bw() + scale_shape_manual(values = c(19,21),name = '95% CI spans zero?')+
  geom_vline(xintercept = 0,lty = 2,col = 'grey50') + 
  ggtitle('Posterier parameter estimates','ln(award/person by tract) ~ .') +
  xlab('posterior mean and 95% credible interval') +
 scale_y_discrete(labels=rev(c('intercept','ln(county awards)','ln(pop. density (km^2))',
                            '# infra. features','SVI index'))) +
  theme(legend.position = c(0.3,0.2),axis.title.y = element_blank(),
        axis.text.y = element_text(angle = 45)) + 
  labs(caption = "all predictors are standardized [x/mean(x)/sd(x)]")+
  NULL

  ggsave(filename ='output/figures/base_model_coefs.png',plot = coef_plot,dpi = 500,height = 4.5,width = 5,units = 'in')
  
  
  
  mod2 <- inla(scale(log(total_award/total_pop)) ~ 1 +
                 scale(log(CFIPS_LEVEL_AWARD+1)) +
                 f(CFIPS,model = 'iid',hyper = pc.prec.u)+
                 f(as.integer(ID),model = 'besag',graph = W.bin) +
                 scale(log(popdensity_kmsq)) + scale(INFRA_COUNT) +
                 scale(RPL_THEMES) + 
                 scale(log(popdensity_kmsq)):scale(RPL_THEMES),
               control.family = list(hyper = pc.prec),
               control.compute = list(waic = T,dic = T,cpo = T),
               control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
               family = 'gaussian',data = tig_tract[tig_tract$RPL_THEMES!=-999,])

x1seq = seq(0.1,0.9,0.05)
x2seq = seq(0.1,0.9,0.05)
tdt = as.data.table(as.matrix(mod2$model.matrix))

cols <- grep('RPL|dens',names(tdt),value=T)
temp_vars = data.table(tdt[,cols,with = F])
quants_to_use = seq(0.1,0.9,0.05)
x1_quantile <- quantile(tdt[[cols[1]]],quants_to_use)
x2_quantile <- quantile(tdt[[cols[2]]],quants_to_use)
combos = expand.grid(x1_quantile,x2_quantile)
combos[,3]<-combos[,1]*combos[,2]
names(combos)<-cols

lcomb_data = combos[rowSums(combos)!=0,]
lcom_data = lcomb_data[!duplicated(lcomb_data),]
lc = inla.make.lincombs(lcomb_data)

mod2_lc = inla(formula = scale(log(total_award/total_pop)) ~ 1 +
                  scale(log(CFIPS_LEVEL_AWARD+1)) +
                  f(CFIPS,model = 'iid',hyper = pc.prec.u)+
                  f(as.integer(ID),model = 'besag',graph = W.bin) +
                  scale(log(popdensity_kmsq)) + scale(INFRA_COUNT) +
                  scale(RPL_THEMES) + 
                  scale(log(popdensity_kmsq)):scale(RPL_THEMES),
                
                control.family = list(hyper = pc.prec),
                control.compute = list(waic = T,dic = T,cpo = T),
                control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
                family = mod2$.args$family,
                data=mod2$.args$data,lincomb = lc,
                control.update = list(result = mod2),
                control.predictor=list(compute=TRUE),verbose=F)

lcd = mod2_lc$summary.lincomb.derived

lcd$id = rownames(lcd)
qvals = lcomb_data[,1:2]
names(qvals) <- c('scale_val1','scale_val2')
lcd$x1_quantile = unlist(replicate(1,rep(x1seq,nrow(lcd)/length(x1seq)),simplify = F))
lcd$x2_quantile = unlist(replicate(1,rep(x2seq,each = nrow(lcd)/length(x1seq)),simplify = F))

lcd = cbind(lcd,qvals)
library(ggthemes)
gg_svi_density_interaction <- ggplot(lcd[lcd$x1_quantile%in%c(0.1,0.5,0.9),],
       aes(group = x1_quantile,color = as.factor(x1_quantile),
                                               x = x2_quantile)) + 
  geom_errorbar(aes(ymin = `0.025quant`,ymax = `0.975quant`)) +
  geom_path(aes(y = mean)) + theme_bw() +
  scale_x_continuous(name = 'tract SVI quantile') + 
  scale_y_continuous(name = 'estimated linear combination',limits = c(-1,1)) +
  scale_color_colorblind(labels = c('10th percentile','median','90th percentile'),
                         name = 'Population density') + 
  theme(legend.position = c(0.8,0.17))  +
  ggtitle('ln(award per capita)',subtitle ='SVI x population density')

ggsave(gg_svi_density_interaction,filename = 'output/figures/SVI_x_popdensity_interaction.png',
       dpi =500,height = 4.5, width = 6,units = 'in')


mod3 <- inla(scale(log(total_award/total_pop)) ~ 1 +
               scale(log(CFIPS_LEVEL_AWARD+1)) +
               f(CFIPS,model = 'iid',hyper = pc.prec.u)+
               f(as.integer(ID),model = 'besag',graph = W.bin) +
               scale(log(popdensity_kmsq)) + scale(INFRA_COUNT) +
               scale(RPL_THEMES) + 
               scale(INFRA_COUNT):scale(RPL_THEMES),
             control.family = list(hyper = pc.prec),
             control.compute = list(waic = T,dic = T,cpo = T),
             control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
             family = 'gaussian',data = tig_tract[tig_tract$RPL_THEMES!=-999,])


x1seq = seq(0.1,0.9,0.05)
x2seq = seq(0.1,0.9,0.05)
tdt = as.data.table(as.matrix(mod3$model.matrix))
cols <- grep('RPL|INFRA',names(tdt),value = T)
tig_sub <- tig_tract[tig_tract$RPL_THEMES!='-999',]
tdt <- cbind(tdt,tig_sub)


x1_values <- sort(unique(tdt$`scale(INFRA_COUNT)`[tdt$INFRA_COUNT%in%c(0,4,10)]))
x2_values <- quantile(tdt$`scale(RPL_THEMES)`,x2seq)

combos = expand.grid(x1_values,x2_values)
combos[,3]<-combos[,1]*combos[,2]
names(combos)<-cols

lcomb_data = combos[rowSums(combos)!=0,]
lcomb_data = lcomb_data[!duplicated(lcomb_data),]
lc = inla.make.lincombs(lcomb_data)

mod3_lc = inla(formula = scale(log(total_award/total_pop)) ~ 1 +
                  scale(log(CFIPS_LEVEL_AWARD+1)) +
                  f(CFIPS,model = 'iid',hyper = pc.prec.u)+
                  f(as.integer(ID),model = 'besag',graph = W.bin) +
                  scale(log(popdensity_kmsq)) + scale(INFRA_COUNT) +
                  scale(RPL_THEMES) + 
                  scale(INFRA_COUNT):scale(RPL_THEMES),
                control.family = list(hyper = pc.prec),
                control.compute = list(waic = T,dic = T,cpo = T),
                control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
                family = mod3$.args$family,
                data=mod3$.args$data,lincomb = lc,
                control.update = list(result = mod3),
                control.predictor=list(compute=TRUE),verbose=F)

lcd = mod3_lc$summary.lincomb.derived
lcd$id = rownames(lcd)
qvals = lcomb_data[,1:2]
lcd <- cbind(lcd,qvals)
lcd$infra_count <- rep(x1_values,length(x2_values))
lcd$rpl_quantile <- rep(x2seq,each = length(x1_values))


gg_svi_infrastructure_interaction <- ggplot(lcd,#[lcd$infra_count==min(lcd$infra_count),],
       aes(group = as.factor(infra_count),
           color = as.factor(infra_count),
           x = rpl_quantile)) + 
  geom_errorbar(aes(ymin = `0.025quant`,ymax = `0.975quant`),position = position_dodge(0.02)) +
  geom_path(aes(y = mean)) + theme_bw() +
  scale_x_continuous(name = 'tract SVI quantile') + 
  scale_y_continuous(name = 'estimated linear combination',limits = c(-.4,.4)) +
  scale_color_colorblind(labels = c('0 (5th perc.)','4 (median)','10 (90th perc.)'),
                         name = '# infrastructure features')+
  theme(legend.position = c(0.8,0.17))  +
  ggtitle('ln(award per capita)',subtitle ='SVI x # infrastructure features')



ggsave(gg_svi_infrastructure_interaction,filename = 'output/figures/SVI_x_infrastructure_interaction.png',
       dpi =500,height = 4.5, width = 6,units = 'in')


cleanCoefs = function(x,mod = NULL) {x %>%
    mutate(`0.95 credible interval` = paste0(round(`0.025quant`,3),', ',round(`0.975quant`,3))) %>%
    mutate(`posterior mean (0.95 credible interval)` = paste0(round(mean,3),' (',`0.95 credible interval`,')')) %>% 
    select(`posterior mean (0.95 credible interval)`) %>% mutate(Variable = rownames(.),model = mod)}
library(htmlTable)
mlist <- list(mod1,mod2,mod3)

# For Full (Outer) Join
mods_tab <- rbindlist(lapply(seq_along(mlist),function(x) mlist[[x]] %>% getCoefs() %>% cleanCoefs(.,x)))
mods_tab$Variable <- fct_inorder(mods_tab$Variable)
    
mods_tab_cast <- dcast(mods_tab, Variable~model,value.var = 'posterior mean (0.95 credible interval)')

mods_tab_cast$Variable <- fct_recode(mods_tab_cast$Variable,
'intercept' = '(Intercept)','ln(county awards)' = 'scale(log(CFIPS_LEVEL_AWARD + 1))',
'ln(pop. density (km^2))' = 'scale(log(popdensity_kmsq))', '# infra. features' = 'scale(INFRA_COUNT)',
'SVI index' = 'scale(RPL_THEMES)',
'ln(pop. density)*SVI index' = 'scale(log(popdensity_kmsq)):scale(RPL_THEMES)', '# infra.*SVI index' = 'scale(INFRA_COUNT):scale(RPL_THEMES)')



hyper_dt <- rbindlist(lapply(seq_along(mlist),function(x) mlist[[x]]$summary.hyper[,c(1,3,5)] %>% cleanCoefs(.,x)))
hyper_cast_dt <- dcast(hyper_dt, Variable~model,value.var = 'posterior mean (0.95 credible interval)')        

full_reg_table <- rbindlist(list(mods_tab_cast,hyper_cast_dt,
               as.data.table(matrix(c('DIC = ',sapply(mlist,function(x) round(x$dic$dic,3))),nrow = 1)),
               as.data.table(matrix(c('WAIC = ',sapply(mlist,function(x) round(x$wai$waic,3))),nrow = 1))),use.names = F)

htmlTable(full_reg_table,header = c('','Model 1','Model 2','Model 3'),
          caption = 'Posterior estimates and model goodness of fit',
          rgroup = c('linear coefficients','hyperparameters','goodness-of-fit'),n.rgroup = c(7,3,2),
          rnames = F,file = 'output/tables/tract_regression_table.html')


co_counties <- left_join(co_counties,
data.table(mod1$summary.random$CFIPS)[,.(ID,mean)] %>% rename(GEOID = ID))

gg_county_iid <- ggplot() + geom_sf(data = co_counties,aes(fill = mean),lwd = 0.2) + 
  scale_fill_viridis_c(option = 'B',name = 'posterior mean') + 
  theme_map() + ggtitle('Posterior means for modeled county intercepts') + 
  labs(caption = '(positive term = predicted to have more funds)')
ggsave(filename = 'output/figures/county_iid_intercepts.png',plot = gg_county_iid,dpi = 500,units = 'in',width = 6,height = 5)


besag_terms <- data.table(mod1$summary.random$`as.integer(ID)`)[,.(ID,mean)] %>% rename(spatial_mean = mean)


tig_tract <- left_join(tig_tract,besag_terms)
denver = co_counties[co_counties$NAME=='Denver',]
denver_centroid <- st_centroid(denver)
denver_buffer <- st_buffer(denver_centroid,30000)
denver_tract <- st_intersects(tig_tract,denver_buffer)
keep <- sapply(denver_tract,length)>0



besag_state <- ggplot() + geom_sf(data = tig_tract,aes(fill = spatial_mean),lwd = 0.05) + 
  scale_fill_viridis_c(option = 'A',name = 'posterior mean') + 
  theme_map() + ggtitle('Posterior means for tract ICAR spatial effect') + 
  labs(caption = '(positive term means tract is predicted to have more funds)')

ggsave(plot = besag_state,
       filename = 'output/figures/tract_icar_effects_state.png',dpi = 500)

besag_denver <- ggplot() + geom_sf(data = tig_tract[keep,],aes(fill = spatial_mean),lwd = 0.05) + 
  scale_fill_viridis_c(option = 'A',name = 'posterior mean') + 
  theme_map() + 
  theme(legend.position = c(0.8,0.2))+
  ggtitle('Posterior means for tract ICAR spatial effect','Denver area') + 
  labs(caption = '(positive term means tract is predicted to have more funds)')

buffer_inset <- ggplot() + geom_sf(data  = co_counties) + theme_map() + 
  geom_sf(data = denver_buffer,col ='red',fill = NA,lwd = 0.4)

gg_inset_map = ggdraw() +
   draw_plot(besag_denver,width = 1,height = 1) +
  draw_plot(buffer_inset,width = 0.3,height = 0.3,x = 0.2,y = 0.6)

ggsave(plot = gg_inset_map,
       filename = 'output/figures/tract_icar_effects_denver_inset.png',dpi = 500)


(denver_svi <- ggplot() + theme_map() + 
  scale_fill_viridis_c(name = 'SVI percentile') + 
  scale_colour_viridis_c(name = 'SVI percentile')+
  ggtitle('SVI by census tract','Denver core') +
  geom_sf(data = tig_tract[keep,] %>% filter(RPL_THEMES!='-999'),
    aes(fill = RPL_THEMES,col = RPL_THEMES)) + 
  theme(legend.position = c(0.8,0.25)))

tig_tract$y <- ifelse(tig_tract$y>2000,2000,tig_tract$y)
(denver_award <- ggplot() + theme_map() + 
  scale_fill_viridis_c(name = 'award/capita',breaks=c(500,1000,2000),labels=c('$500','$1000','$2000+')) + 
  scale_colour_viridis_c(name = 'award/capita',breaks=c(500,1000,2000),labels=c('$500','$1000','$2000+'))+
  ggtitle('award/capita by census tract','Denver core') +
  geom_sf(data = tig_tract[keep,],aes(fill = y,col = y)) +
    theme(legend.position = c(0.8,0.25)))

denver_inset_cowplot <- ggdraw() +
  draw_plot(denver_svi,width = 1,height = 0.5,x = 0,y = .5) +
  draw_plot(denver_award,width = 1,height = 0.5,x = 0,y  = 0) +
  draw_plot(buffer_inset,width = 0.25,height = 0.25,x = .2,y = .7)
  
ggsave(denver_inset_cowplot,filename = 'output/figures/denver_tracts_svi_and_awards.png',dpi = 500)







