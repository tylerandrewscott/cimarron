
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

org_eco = readRDS('building_blocks/tract_org_ecology.RDS')

tig_tract <- left_join(tig_tract,org_eco)

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
  geom_sf(data = tig_tract,#[tig_tract$RPL_THEMES!='-999',],
          aes(fill = RPL_THEMES,col = RPL_THEMES)) +theme(text=element_text(size = 14),legend.position='right')+
  NULL
tig_tract$y <- ifelse(tig_tract$y>2000,2000,tig_tract$y)
g2 <- ggplot() + theme_map() + 
  scale_fill_viridis_c(name = 'award/capita',breaks=c(500,1000,2000),labels=c('$500','$1000','$2000+')) + 
  scale_colour_viridis_c(name = 'award/capita',breaks=c(500,1000,2000),labels=c('$500','$1000','$2000+'))+
  ggtitle('award/capita by census tract') +
  # theme(legend.background = element_rect(fill = alpha('white',.5)))+
  geom_sf(data = tig_tract,aes(fill = y,col = y)) +
  theme(text=element_text(size = 14),legend.position = 'right')+
  NULL
library(cowplot)
gg_co_cow = ggdraw() +
  draw_plot(g1,width = 1,height = .5,y = 0) + draw_plot(g2,width = 1,height = .5,y = 0.5)
ggsave(plot = gg_co_cow,width = 10,height = 10,units = 'in',filename = 'output/figures/colorado_tracts_svi_and_awards.png',dpi = 300)


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
tig_tract$CFIPS_LEVEL_AWARD[is.na(tig_tract$CFIPS_LEVEL_AWARD)]<-0
tig_tract$award_per_capita <- tig_tract$total_award/tig_tract$total_pop
tig_tract$award_per_capita[is.na(tig_tract$award_per_capita)]<-0
tig_tract$RPL_THEMES[tig_tract$RPL_THEMES==(-999)]<-NA
tig_tract$popdensity_kmsq <- as.numeric(tig_tract$popdensity_kmsq)

test = tig_tract %>% as.data.frame() %>%
  select(award_per_capita,CFIPS_LEVEL_AWARD,popdensity_kmsq,Service_Infrastructure_Count,Registered_Nonprofit_Count,local_gov_intersects,RPL_THEMES)
library(texreg)
library(stargazer)
library(corrplot)


stargazer(test[!is.na(test$RPL_THEMES),],summary = T,summary.stat = c('n','min','median','mean','max'),out = 'output/tables/summary_statistics.html')

cortab = round(cor(test,use = 'pairwise.complete.obs'),3)
cortab[lower.tri(cortab)] <- '--'
stargazer(cortab[,-1],summary = F,out= 'output/tables/correlation_table.html')

famcontrol = list(list(prior = "pcprec", param = c(u.resp,0.01)))

data_for_model <- tig_tract[!is.na(tig_tract$RPL_THEMES),]
mod0a <- inla(scale(log(award_per_capita+1)) ~ 1 + 
                scale(log(CFIPS_LEVEL_AWARD+1)) +
                f(CFIPS,model = 'iid',hyper = pc.prec.u),
              control.compute = list(waic = T,dic = T,cpo = T),
              control.family = list(hyper = pc.prec),
              #control.results = cres,
              control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
              family = 'gaussian',data = data_for_model)

#besag tract term

mod0b <- inla(scale(log(award_per_capita+1)) ~ 1 + 
                scale(log(CFIPS_LEVEL_AWARD+1)) +
                scale(log(popdensity_kmsq)) +
                f(CFIPS,model = 'iid',hyper = pc.prec.u)+
                f(as.integer(ID),model = 'besag',graph = W.bin),
              control.family = list(hyper = pc.prec),
              control.compute = list(waic = T,dic = T,cpo = T),
              control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
              family = 'gaussian',data = data_for_model)


mod1 <- inla(scale(log(award_per_capita+1)) ~ 1 +
               scale(log(CFIPS_LEVEL_AWARD+1)) +
               f(CFIPS,model = 'iid',hyper = pc.prec.u)+
               f(as.integer(ID),model = 'besag',graph = W.bin) +
               scale(log(popdensity_kmsq)) + scale(Service_Infrastructure_Count) +
               scale(local_gov_intersects)+ scale(Registered_Nonprofit_Count) +
               scale(RPL_THEMES),
             control.family = list(hyper = pc.prec),
             control.compute = list(waic = T,dic = T,cpo = T),
             control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
             family = 'gaussian',data = data_for_model)

getCoefs <- function(x) {
  temp = x$summary.fixed[,c(1,3,5)]
  temp$coef = rownames(temp)
  temp
}

cdt <- rbindlist(list(getCoefs(mod0b) %>% mutate(mod = '0'),
                      getCoefs(mod1) %>% mutate(mod = '1')))
cdt$coef <- fct_inorder(cdt$coef)

lab.key = data.table(coef = unique(cdt$coef),
      label = c('intercept','ln(county awards)','ln(pop. density (km^2))',
         '# infra. features','# local govs','# nonprofits','SVI index'))
#cdt$coef <- fct_rev(cdt$coef)
cdt$coef_label <- lab.key$label[match(cdt$coef,lab.key$coef)]
cdt$coef_label <- fct_inorder(cdt$coef_label)
#cdt$coef_label <- fct_rev(cdt$coef_label)

coef_plot <- ggplot(cdt[mod==1,]) + 
  geom_segment(aes(y = coef_label,yend = coef_label,x = `0.025quant`,xend = `0.975quant`,group = mod),
               position = position_dodge(width = 0.25)) +
  geom_point(aes(x = mean,y = coef_label,shape = `0.025quant`<0 & `0.975quant`>0)) + 
  theme_bw() + scale_shape_manual(values = c(19,21),name = '95% CI spans zero?')+
  geom_vline(xintercept = 0,lty = 2,col = 'grey50') + 
  ggtitle('Posterier parameter estimates','ln(award/person by tract) ~ .') +
  xlab('posterior mean and 95% credible interval') +
  theme(legend.position = c(0.3,0.2),axis.title.y = element_blank(),
        axis.text.y = element_text(angle = 45)) + 
  labs(caption = "all predictors are standardized [x-mean(x)]/sd(x)")+
  NULL

ggsave(filename ='output/figures/base_model_coefs.png',plot = coef_plot,dpi = 300,height = 4.5,width = 5,units = 'in')



mod2 <- inla(scale(log(total_award/total_pop)) ~ 1 +
               scale(log(CFIPS_LEVEL_AWARD+1)) +
               f(CFIPS,model = 'iid',hyper = pc.prec.u)+
               f(as.integer(ID),model = 'besag',graph = W.bin) +
               scale(log(popdensity_kmsq)) + scale(Service_Infrastructure_Count) +
               scale(local_gov_intersects)+ scale(Registered_Nonprofit_Count) +
               scale(RPL_THEMES) + 
               scale(local_gov_intersects):scale(RPL_THEMES),
             control.family = list(hyper = pc.prec),
             control.compute = list(waic = T,dic = T,cpo = T),
             control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
             family = 'gaussian',data = data_for_model)

x1seq = seq(0.1,0.9,0.05)
x2seq = seq(0.1,0.9,0.05)
tdt = as.data.table(as.matrix(mod2$model.matrix))
cols <- grep('RPL|local_gov',names(tdt),value=T)
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
                 scale(log(popdensity_kmsq)) + scale(Service_Infrastructure_Count) +
                 scale(local_gov_intersects)+ scale(Registered_Nonprofit_Count) +
                 scale(RPL_THEMES) + 
                 scale(local_gov_intersects):scale(RPL_THEMES),
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
gg_svi_localgovs_interaction <- ggplot(lcd[lcd$x1_quantile%in%c(0.1,0.5,0.9),],
                                        aes(group = x1_quantile,color = as.factor(x1_quantile),
                                            x = x2_quantile)) + 
  geom_errorbar(aes(ymin = `0.025quant`,ymax = `0.975quant`)) +
  geom_path(aes(y = mean)) + theme_bw() +
  scale_x_continuous(name = 'tract SVI quantile') + 
  scale_y_continuous(name = 'estimated linear combination',limits = c(-1,1)) +
  scale_color_colorblind(labels = c('10th percentile','median','90th percentile'),
                         name = '# local governments') + 
  theme(legend.position = c(0.8,0.17))  +
  ggtitle('A. ln(award per capita)',subtitle ='SVI x # local govs')

ggsave(gg_svi_localgovs_interaction,filename = 'output/figures/SVI_x_localgovs_interaction.png',
       dpi =500,height = 4.5, width = 6,units = 'in')

mod3 <- inla(scale(log(total_award/total_pop)) ~ 1 +
               scale(log(CFIPS_LEVEL_AWARD+1)) +
               f(CFIPS,model = 'iid',hyper = pc.prec.u)+
               f(as.integer(ID),model = 'besag',graph = W.bin) +
               scale(log(popdensity_kmsq)) + scale(Service_Infrastructure_Count) +
               scale(local_gov_intersects)+ scale(Registered_Nonprofit_Count) +
               scale(RPL_THEMES) + 
               scale(Service_Infrastructure_Count):scale(RPL_THEMES),
             control.family = list(hyper = pc.prec),
             control.compute = list(waic = T,dic = T,cpo = T),
             control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
             family = 'gaussian',data = data_for_model)


x1seq = seq(0.1,0.9,0.05)
x2seq = seq(0.1,0.9,0.05)
tdt = as.data.table(as.matrix(mod3$model.matrix))
cols <- grep('RPL|Infrastruct',names(tdt),value=T)
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
mod3_lc = inla(formula = scale(log(total_award/total_pop)) ~ 1 +
                 scale(log(CFIPS_LEVEL_AWARD+1)) +
                 f(CFIPS,model = 'iid',hyper = pc.prec.u)+
                 f(as.integer(ID),model = 'besag',graph = W.bin) +
                 scale(log(popdensity_kmsq)) + scale(Service_Infrastructure_Count) +
                 scale(local_gov_intersects)+ scale(Registered_Nonprofit_Count) +
                 scale(RPL_THEMES) + 
                 scale(Service_Infrastructure_Count):scale(RPL_THEMES),
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
names(qvals) <- c('scale_val1','scale_val2')
lcd$x1_quantile = unlist(replicate(1,rep(x1seq,nrow(lcd)/length(x1seq)),simplify = F))
lcd$x2_quantile = unlist(replicate(1,rep(x2seq,each = nrow(lcd)/length(x1seq)),simplify = F))

lcd = cbind(lcd,qvals)
library(ggthemes)
gg_svi_serviceinfra_interaction <- ggplot(lcd[lcd$x1_quantile%in%c(0.1,0.5,0.9),],
                                       aes(group = x1_quantile,color = as.factor(x1_quantile),
                                           x = x2_quantile)) + 
  geom_errorbar(aes(ymin = `0.025quant`,ymax = `0.975quant`)) +
  geom_path(aes(y = mean)) + theme_bw() +
  scale_x_continuous(name = 'tract SVI quantile') + 
  scale_y_continuous(name = 'estimated linear combination',limits = c(-1,1)) +
  scale_color_colorblind(labels = c('10th percentile','median','90th percentile'),
                         name = '# service infra. features') + 
  theme(legend.position = c(0.8,0.17))  +
  ggtitle('B. ln(award per capita)',subtitle ='SVI x service infrastructure features')


ggsave(gg_svi_serviceinfra_interaction,filename = 'output/figures/SVI_x_serviceinfra_interaction.png',
       dpi =500,height = 4.5, width = 6,units = 'in')



mod4 <- inla(scale(log(total_award/total_pop)) ~ 1 +
               scale(log(CFIPS_LEVEL_AWARD+1)) +
               f(CFIPS,model = 'iid',hyper = pc.prec.u)+
               f(as.integer(ID),model = 'besag',graph = W.bin) +
               scale(log(popdensity_kmsq)) + scale(Service_Infrastructure_Count) +
               scale(local_gov_intersects)+ scale(Registered_Nonprofit_Count) +
               scale(RPL_THEMES) + 
               scale(Registered_Nonprofit_Count):scale(RPL_THEMES),
             control.family = list(hyper = pc.prec),
             control.compute = list(waic = T,dic = T,cpo = T),
             control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
             family = 'gaussian',data = data_for_model)


x1seq = seq(0.1,0.9,0.05)
x2seq = seq(0.1,0.9,0.05)
tdt = as.data.table(as.matrix(mod4$model.matrix))
cols <- grep('RPL|Nonprofit',names(tdt),value=T)
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
mod4_lc = inla(formula = scale(log(total_award/total_pop)) ~ 1 +
                 scale(log(CFIPS_LEVEL_AWARD+1)) +
                 f(CFIPS,model = 'iid',hyper = pc.prec.u)+
                 f(as.integer(ID),model = 'besag',graph = W.bin) +
                 scale(log(popdensity_kmsq)) + scale(Service_Infrastructure_Count) +
                 scale(local_gov_intersects)+ scale(Registered_Nonprofit_Count) +
                 scale(RPL_THEMES) + 
                 scale(Registered_Nonprofit_Count):scale(RPL_THEMES),
               control.family = list(hyper = pc.prec),
               control.compute = list(waic = T,dic = T,cpo = T),
               control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
               family = mod4$.args$family,
               data=mod4$.args$data,lincomb = lc,
               control.update = list(result = mod4),
               control.predictor=list(compute=TRUE),verbose=F)
lcd = mod4_lc$summary.lincomb.derived
lcd$id = rownames(lcd)
qvals = lcomb_data[,1:2]
names(qvals) <- c('scale_val1','scale_val2')
lcd$x1_quantile = unlist(replicate(1,rep(x1seq,nrow(lcd)/length(x1seq)),simplify = F))
lcd$x2_quantile = unlist(replicate(1,rep(x2seq,each = nrow(lcd)/length(x1seq)),simplify = F))

lcd = cbind(lcd,qvals)
library(ggthemes)
gg_svi_nonprofits_interaction <- ggplot(lcd[lcd$x1_quantile%in%c(0.1,0.5,0.9),],
                                          aes(group = x1_quantile,color = as.factor(x1_quantile),
                                              x = x2_quantile)) + 
  geom_errorbar(aes(ymin = `0.025quant`,ymax = `0.975quant`)) +
  geom_path(aes(y = mean)) + theme_bw() +
  scale_x_continuous(name = 'tract SVI quantile') + 
  scale_y_continuous(name = 'estimated linear combination',limits = c(-1,1)) +
  scale_color_colorblind(labels = c('10th percentile','median','90th percentile'),
                         name = '# registered nonprofits') + 
  theme(legend.position = c(0.8,0.17))  +
  ggtitle('C. ln(award per capita)',subtitle ='SVI x registered nonprofits')


ggsave(gg_svi_nonprofits_interaction,filename = 'output/figures/SVI_x_nonprofits_interaction.png',
       dpi =500,height = 4.5, width = 6,units = 'in')


library(gridExtra)
interaction_grid <- grid.arrange(gg_svi_localgovs_interaction,gg_svi_serviceinfra_interaction ,
             gg_svi_nonprofits_interaction,ncol = 2 )

ggsave(filename = 'output/figures/interaction_grid.png',plot = interaction_grid,dpi = 300,units = 'in',width = 10,height = 10)



cleanCoefs = function(x,mod = NULL) {x %>%
    mutate(`0.95 credible interval` = paste0(round(`0.025quant`,3),', ',round(`0.975quant`,3))) %>%
    mutate(`posterior mean (0.95 credible interval)` = paste0(round(mean,3),' (',`0.95 credible interval`,')')) %>% 
    select(`posterior mean (0.95 credible interval)`) %>% mutate(Variable = rownames(.),model = mod)}
library(htmlTable)
mlist <- list(mod1,mod2,mod3,mod4)
# For Full (Outer) Join
mods_tab <- rbindlist(lapply(seq_along(mlist),function(x) mlist[[x]] %>% getCoefs() %>% cleanCoefs(.,x)))
mods_tab$Variable <- fct_inorder(mods_tab$Variable)
mods_tab_cast <- dcast(mods_tab, Variable~model,value.var = 'posterior mean (0.95 credible interval)')

mods_tab_cast$Variable <- lab.key$label[match(mods_tab_cast$Variable,lab.key$coef)]

mods_tab_cast$Variable[is.na(mods_tab_cast$Variable)] <- c('SVI index * local governments',
'SVI index * # service infra. features',
'SVI index * # registered nonprofits')

hyper_dt <- rbindlist(lapply(seq_along(mlist),function(x) mlist[[x]]$summary.hyper[,c(1,3,5)] %>% cleanCoefs(.,x)))
hyper_cast_dt <- dcast(hyper_dt, Variable~model,value.var = 'posterior mean (0.95 credible interval)')        

full_reg_table <- rbindlist(list(mods_tab_cast,hyper_cast_dt,
                                 as.data.table(matrix(c('DIC = ',sapply(mlist,function(x) round(x$dic$dic,3))),nrow = 1)),
                                 as.data.table(matrix(c('WAIC = ',sapply(mlist,function(x) round(x$wai$waic,3))),nrow = 1))),use.names = F)

htmlTable(full_reg_table,header = c('','Model 1','Model 2','Model 3','Model 4'),
          caption = 'Posterior estimates and model goodness of fit',
          rgroup = c('linear coefficients','hyperparameters','goodness-of-fit'),n.rgroup = c(10,3,2),
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
colorado_counties <- tigris::counties(state = 'CO',cb = T,class = 'sf')
denver = colorado_counties[colorado_counties$NAME=='Denver',]
albersNA <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-110 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
denver <- st_transform(denver,albersNA)
denver_centroid <- st_centroid(denver)

denver_buffer <- st_buffer(denver_centroid,dist = 30000)
tig_tract <- st_transform(tig_tract,albersNA)
denver_tract <- st_intersects(tig_tract,denver_buffer)
keep <- sapply(denver_tract,length)>0

besag_state <- ggplot() + geom_sf(data = tig_tract,aes(fill = spatial_mean),lwd = 0.05) + 
  scale_fill_viridis_c(option = 'A',name = 'posterior mean') + 
  theme_map() + ggtitle('Posterior means for tract ICAR spatial effect') + 
  labs(caption = '(positive term means tract is predicted to have more funds)')

ggsave(plot = besag_state,
       filename = 'output/figures/tract_icar_effects_state.png',dpi = 500,units = 'in',width = 6,height = 6)

besag_denver <- ggplot() + geom_sf(data = tig_tract[keep,],aes(fill = spatial_mean),lwd = 0.05) + 
  scale_fill_viridis_c(option = 'A',name = 'posterior mean') + 
  theme_map() + 
  theme(legend.position = c(0.8,0.2))+
  ggtitle('Posterior means for tract ICAR spatial effect','Denver area') + 
  labs(caption = '(positive term means tract is predicted to have more funds)')

buffer_inset <- ggplot() + geom_sf(data  = colorado_counties) + theme_map() + 
  geom_sf(data = denver_buffer,col ='red',fill = NA,lwd = 0.4)

gg_inset_map = ggdraw() +
  draw_plot(besag_denver,width = 1,height = 1) +
  draw_plot(buffer_inset,width = 0.3,height = 0.3,x = 0.2,y = 0.6)

ggsave(plot = gg_inset_map,
       filename = 'output/figures/tract_icar_effects_denver_inset.png',dpi = 500,units = 'in',height = 6,width = 6)


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







