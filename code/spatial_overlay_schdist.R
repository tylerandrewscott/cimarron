
library(sf)
library(tidyverse)
library(tigris)
library(tidycensus)
library(data.table)
library("spdep")
library("spatialreg")
library(INLA)
grant_geos <- readRDS('scratch/complex_grant_values_by_geometry.rds')
grant_geos <- grant_geos[grant_geos$scope_recode!='STATE-WIDE',]
grant_geos <- grant_geos[grant_geos$scope_recode=='SCHOOLDIST',]

tract_tags <- readRDS('scratch/shapefiles_and_tracts_matching_combined_geos.rds')
key = source('../census_key')
census_api_key(key$value)
tract_pop <- get_acs(geography = 'tract',state = 'CO',variables ='B01001_001',year = 2019)
tract_tags$totpop <- NA
for(i in 1:nrow(tract_tags)){
  tract_tags$totpop[i] = sum(tract_pop$estimate[match(tract_tags$tracts[[i]],tract_pop$GEOID)])
}
svi <- fread('scratch/Colorado_SVI_2018.csv')
svi$GEOID <- paste0('0',as.character(svi$FIPS))

grant_geos <- data.table(grant_geos)
sd_awards <- grant_geos[,sum(value),by=.(combined_geo)]

sds <- st_read('scratch/CDPHE_CDOE_School_District_Boundaries.geojson')
tracts <- tigris::tracts(state = 'CO',cb = T,class = 'sf')
sds <- st_transform(sds,st_crs(tracts))
overs <- st_overlaps(sds,tracts)

sds$tracts <- lapply(overs,function(x) tracts$GEOID[x])
sds$median_SVI <- sapply(sds$tracts,function(x) median(svi$RPL_THEMES[svi$GEOID %in% x]))
sds$total_pop <- sapply(sds$tracts,function(x) sum(tract_pop$estimate[tract_pop$GEOID %in% x]))
sds$total_award <- sd_awards$V1[match(sds$GEOID,sd_awards$combined_geo)]
sds$total_award[is.na(sds$total_award)]<-0
infra <- fread('https://raw.githubusercontent.com/tylerascott/cimarron/main/building_blocks/infrastructure.csv')
infra[,V1:=NULL]
infra$GEOID <- paste0('0',infra$GEOID)

hosps <- st_read('scratch/Hospitals.geojson')
contains_hosp <- st_contains(tracts,hosps)
tracts$HOSPITAL <- sapply(contains_hosp,length)
infra$HOSPITAL <- tracts$HOSPITAL[match(infra$GEOID,tracts$GEOID)]
infra$INFRA_COUNT <- rowSums(infra %>% select(-GEOID))
sds$INFRA_COUNT <- sapply(sds$tracts,function(x) sum(infra$INFRA_COUNT[infra$GEOID %in% x]))
sds$km_sq <- st_area(sds)/1e6
sds$popdensity_kmsq <- sds$total_pop/sds$km_sq

sds.adj.q <- poly2nb(sds)
W.bin <- nb2mat(sds.adj.q, style = "B")
sds$ID <- 1:nrow(sds)

pc.prec.u = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))
bprior <- list(prior = 'gaussian', param = c(0,1))
pc.prec = list(prec = list(prior = "pc.prec", param = c(1, 0.01)))
cres <- list(return.marginals.predictor = FALSE, 
             return.marginals.random = FALSE)
u.resp <- sd(scale(log(sds$total_award/sds$total_pop)))

famcontrol = list(list(prior = "pcprec", param = c(u.resp,0.01)))

#ggplot(data = tig_tract[tig_tract$RPL_THEMES!=-999,]) + geom_point(aes(x = log(total_pop),y = log(total_award)))
sds$y <- scale(log(1 + sds$total_award/sds$total_pop))

mod1 <- inla(y ~ 1 +
               f(as.integer(ID),model = 'besag',graph = W.bin) +
               scale(log(popdensity_kmsq)) + scale(INFRA_COUNT) +
               scale(median_SVI),
             control.family = list(hyper = pc.prec),
             control.compute = list(waic = T,dic = T,cpo = T),
             control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
             family = 'gaussian',data = sds %>% select(-tracts))


getCoefs <- function(x) {
  temp = x$summary.fixed[,c(1,3,5)]
  temp$coef = rownames(temp)
  temp
}

cdt <- rbindlist(list(
                      getCoefs(mod1) %>% mutate(mod = '1')))
cdt$coef <- fct_inorder(cdt$coef)
cdt$coef <- fct_rev(cdt$coef)

coef_plot <- ggplot(cdt[mod==1,]) + 
  geom_segment(aes(y = coef,yend = coef,x = `0.025quant`,xend = `0.975quant`,group = mod),
               position = position_dodge(width = 0.25)) +
  geom_point(aes(x = mean,y = coef,shape = `0.025quant`<0 & `0.975quant`>0)) + 
  theme_bw() + scale_shape_manual(values = c(19,21),name = '95% CI spans zero?')+
  geom_vline(xintercept = 0,lty = 2,col = 'grey50') + 
  ggtitle('Posterier parameter estimates','ln(school district awards/person by tract) ~ .') +
  xlab('posterior mean and 95% credible interval') +
  scale_y_discrete(labels=rev(c('intercept','ln(pop. density (km^2))',
                                '# infra. features','SVI index'))) +
  theme(legend.position = c(0.8,0.2),axis.title.y = element_blank(),
        axis.text.y = element_text(angle = 45)) + 
  labs(caption = "all predictors are standardized [x/mean(x)/sd(x)]")
NULL

ggsave(filename = 'output/figures/school_dist_model_coefs.png',plot = coef_plot,dpi = 500,width = 6,height = 4.5, units = 'in')
library(ggthemes)
g1 = ggplot() + geom_sf(data = sds,aes(fill = median_SVI)) + 
  scale_fill_viridis_c(name = 'median SVI') + theme_map() + 
  ggtitle('School district median SVI') 

sds$funds <- ifelse(sds$total_award/sds$total_pop>1000,1000,sds$total_award/sds$total_pop)
g2 = ggplot() + geom_sf(data = sds,aes(fill = funds)) + 
  scale_fill_viridis_c(name = '$/person',breaks = c(250,500,750,1000),labels = c('250','500','750','1000+')) + theme_map() + 
  ggtitle('School district awards per capita') 
library(gridExtra)

ggsave(grid.arrange(g1,g2,ncol = 1),filename = 'output/figures/school_district_SVI_and_awards.png',dpi = 500)


cleanCoefs = function(x,mod = NULL) {x %>%
    mutate(`0.95 credible interval` = paste0(round(`0.025quant`,3),', ',round(`0.975quant`,3))) %>%
    mutate(`posterior mean (0.95 credible interval)` = paste0(round(mean,3),' (',`0.95 credible interval`,')')) %>% 
    select(`posterior mean (0.95 credible interval)`) %>% mutate(Variable = rownames(.),model = mod)}

mlist = list(mod1)
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
library(htmlTable)
htmlTable(full_reg_table,header = c('','Model 1'),
          caption = 'School district sample: Posterior estimates and model goodness of fit',
          rgroup = c('linear coefficients','hyperparameters','goodness-of-fit'),n.rgroup = c(4,2,2),
          rnames = F,file = 'output/tables/school_district_regression_table.html')

summary(mod1)











