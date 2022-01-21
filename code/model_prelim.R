library(data.table)
library(sf)
library(statnet)
library(lwgeom)
library(smoothr)
### thsi drops counties from dataset and then generates from there
gov_shapes <- readRDS('scratch/government_shapes.rds')
gov_shapes <- gov_shapes[gov_shapes$type!=1,]
area_thresh <- units::set_units(10, m^2)
gov_shapes <- fill_holes(gov_shapes,threshold = area_thresh)

st_multipolygon(gov_shapes)
gov_shapes <- lwgeom::st_snap_to_grid(gov_shapes,10)
gov_shapes <- st_make_valid(gov_shapes)

sf::rem

class(gov_shapes[979,])
gov_overs <- st_overlaps(gov_shapes)

#Snap to grid at 10m, as Landsat is only a 30m resolution anyway
test_set_snap <- test_set %>%
  mutate(geometry = map(geometry, ~lwgeom::st_snap_to_grid(.x, 10)))%>%
  mutate(geometry = map(geometry, lwgeom::st_make_valid))

overlap_snap <- st_intersection(test_set_snap)

as.data.table(gov_shapes)[,.(lgid,name,type)]



finance <- data.table(readRDS('building_blocks/finance_data_out.rds'))
finance$lgid[nchar(finance$lgid)==4] <- paste0('0',finance$lgid[nchar(finance$lgid)==4])
finance <- finance[lgid %in% gov_shapes$lgid,]

cov <- data.table(readRDS('scratch/cares_grant_dataset.rds'))
cov$lgid[nchar(cov$lgid)==4] <- paste0('0',cov$lgid[nchar(cov$lgid)==4])
cov <- cov[lgid %in% gov_shapes$lgid,]
total_amount <- cov[,sum(amount),by=.(lgid)][order(-V1)]
total_amount <- total_amount[order(lgid)]
library(INLA)
n = length(unique(gov_shapes$lgid))
Y<-matrix(NA,ncol = 2,nrow = n*2)

shapes_index <- match(total_amount$lgid,gov_shapes$lgid)
finance_index <- match(total_amount$lgid,finance$lgid)
cov_index <- match(total_amount$lgid,cov$lgid)

u <- (total_amount$V1>0)+0
y <- ifelse(total_amount$V1>0,log(total_amount$V1),NA)

Y[1:n,1] <- u
Y[n+(1:n),2] <- y
idat = list(Y = Y)
idat$mu.u <- rep(1:0, each=n)
idat$mu.y <- rep(0:1, each=n)
idat$u_type <-c(paste0('type_',cov$simpletypes[cov_index]),rep(NA,n))
idat$y_type <-c(rep(NA,n),paste0('type_',cov$simpletypes[cov_index]))
idat$y_type_county <- ifelse(idat$y_type=='type_county',1,0)
idat$u_type_county <- ifelse(idat$u_type=='type_county',1,0)
idat$y_type_metro <- ifelse(idat$y_type=='type_metro district',1,0)
idat$u_type_metro <- ifelse(idat$u_type=='type_metro district',1,0)
idat$y_type_muni <- ifelse(idat$y_type=='type_municipality',1,0)
idat$u_type_muni <- ifelse(idat$u_type=='type_municipality',1,0)
idat$y_type_school <- ifelse(idat$y_type=='type_school district',1,0)
idat$u_type_school <- ifelse(idat$u_type=='type_school district',1,0)
idat$y_type_othersd<- ifelse(idat$y_type=='type_other special district',1,0)
idat$u_type_othersd <- ifelse(idat$u_type=='type_other special district',1,0)
idat$u_debt_rev <- c(log(finance$TOTAL_DEBT[finance_index]/finance$REV_TOTAL[finance_index]+1),rep(NA,n))
idat$y_debt_rev <- c(rep(NA,n),log(finance$TOTAL_DEBT[finance_index]/finance$REV_TOTAL[finance_index]+1))
idat$u_exp_rev <- c(log(finance$EXP_TOTAL[finance_index]/finance$REV_TOTAL[finance_index]+1),rep(NA,n))
idat$y_exp_rev <- c(rep(NA,n),log(finance$EXP_TOTAL[finance_index]/finance$REV_TOTAL[finance_index]+1))
idat$u_intergov_rev <- c(log(finance$REV_INTGOVT[finance_index]/finance$REV_TOTAL[finance_index]+1),rep(NA,n))
idat$y_intergov_rev <- c(rep(NA,n),log(finance$REV_INTGOVT[finance_index]/finance$REV_TOTAL[finance_index]+1))
idat$u_nri_risk_score <- c(log(gov_shapes$nri_risk_score[shapes_index]+1),rep(NA,n))
idat$y_nri_risk_score <- c(rep(NA,n),log(gov_shapes$nri_risk_score[shapes_index]+1))

idat$u_rpl1 <-c(cov$RPL_THEME1[cov_index],rep(NA,n))
idat$y_rpl1 <-c(rep(NA,n),cov$RPL_THEME1[cov_index])
idat$u_rpl2 <-c(cov$RPL_THEME2[cov_index],rep(NA,n))
idat$y_rpl2 <-c(rep(NA,n),cov$RPL_THEME2[cov_index])
idat$u_rpl3 <-c(cov$RPL_THEME3[cov_index],rep(NA,n))
idat$y_rpl3 <-c(rep(NA,n),cov$RPL_THEME3[cov_index])
idat$u_rpl4 <-c(cov$RPL_THEME4[cov_index],rep(NA,n))
idat$y_rpl4 <-c(rep(NA,n),cov$RPL_THEME4[cov_index])

idat$u_health <-c(cov$`10_Health`[cov_index],rep(NA,n))
idat$y_health <-c(rep(NA,n),cov$`10_Health`[cov_index])

scale_vars <- !names(idat) %in% c('Y','mu.u','mu.y','u_type','y_type')
idat[scale_vars] <- lapply(idat[scale_vars],function(x) (x/(sd(x,na.rm= T)*2))-mean(x,na.rm=T))

bprior <- list(prior = 'gaussian', param = c(0,1))
cres <- list(return.marginals.predictor = FALSE, 
             return.marginals.random = FALSE)
u.resp <- sd(u,na.rm = T)
y.resp <- sd(y,na.rm=T)
famcontrol = list(list(prior = "pcprec", param = c(u.resp,0.01)),
                  list(prior = "pcprec", param = c(y.resp,0.01)))
#idat$Y_solo <- ifelse(idat$Y[1:n,1]==1,idat$Y[n+1:n,2],0)

# Making the model matrix #### 
vars0 <- names(idat)[grepl('mu|type_',names(idat))&!grepl('county|muni',names(idat))]
form0 <- reformulate(paste(vars0,collapse = '+'))
form0 <- update.formula(form0,Y~0+.)
mod0 <- inla(form0,
     family = c('zeroinflatedbinomial1','gaussian') , 
     data = idat,Ntrials=rep(1,n),
     control.predictor=list(compute=TRUE),
    # control.family = famcontrol,
     control.fixed = list(expand.factor.strategy = "inla",prec = bprior),
     control.compute = list(waic=TRUE,dic=TRUE,config = F),verbose = T)

vars1 <- names(idat)[!grepl('county|muni|type$|^Y',names(idat))]
form1 <- reformulate(vars1)
form1 <- update.formula(form1,Y~0+.)
mod1 <- inla(form1,      family = c('zeroinflatedbinomial1','gaussian') ,  
             data = idat,Ntrials=rep(1,n),
             control.predictor=list(compute=TRUE),
            # control.family = famcontrol,
             control.fixed = list(expand.factor.strategy = "inla",prec = bprior),
             control.compute = list(waic=TRUE,dic=TRUE,config = F),verbose = T)
mod0$waic$waic
mod1$waic$waic

coefs <- mod1$summary.fixed[,c(1,3,5)]
coefs$coef <- rownames(coefs)
u_coefs <- coefs[grepl('^u',coefs$coef),]
y_coefs <- coefs[grepl('^y',coefs$coef),]
u_coefs$coef <- gsub('^u_','',u_coefs$coef)
y_coefs$coef <- gsub('^y_','',u_coefs$coef)
u_coefs$coef[u_coefs$coef=='mu.u']<-'(intercept)'
y_coefs$coef[y_coefs$coef=='mu.y']<-'(intercept)'
library(forcats)
u_coefs$coef <- fct_inorder(u_coefs$coef)
u_coefs$coef <- fct_rev(u_coefs$coef)
y_coefs$coef <- fct_inorder(y_coefs$coef)
y_coefs$coef <- fct_rev(y_coefs$coef)
library(tidyverse)

g1 <- ggplot(u_coefs,aes(y = coef)) + theme_bw() +
  ggtitle("p(grant receipt (i.e., >$0))")+
  theme(axis.title.y = element_blank(),legend.position = c(.8,.2)) +
  xlab('posterior mean (95% credible interval')+
  geom_errorbarh(aes(xmin = `0.025quant`,xmax = `0.975quant`),height = 0.25)+
  geom_point(aes(x = mean,fill = as.character(`0.025quant`<0&`0.975quant`>0)),pch = 21) + 
  scale_fill_manual(values = c('black','white'),labels=c('no','yes'),
                                name='95% credible interval\ncontains zero') + 
  guides(fill = 'none')

g2 <- ggplot(y_coefs,aes(y = coef)) + theme_bw() +
  ggtitle("grant total|grant receipt")+
  theme(axis.title.y = element_blank(),legend.position = c(.8,.3)) +
  xlab('posterior mean (95% credible interval')+
  geom_errorbarh(aes(xmin = `0.025quant`,xmax = `0.975quant`),height = 0.25)+
  geom_point(aes(x = mean,fill = as.character(`0.025quant`<0&`0.975quant`>0)),pch = 21) + 
  scale_fill_manual(values = c('black','white'),labels=c('no','yes'),
                                name='contains zero?')
library(gridExtra)
grid.arrange(g1,g2,ncol = 2)

table(idat$u_type[1:n],idat$Y[1:n,1])

ggplot() + geom_boxplot(aes(x = idat$y_type[n+1:n], y = idat$Y[n+1:n,2])) + 
  coord_flip() + theme_bw()+ ylab("ln(amount by entity)|received a grant")
table(idat$u_type[1:n],idat$Y[1:n,1])
summary(mod1)
mod1$waic$waic  

summary(u)

[1] 90607.12
mod0$waic$waic

which(is.na(idat$u_type))

summary(mod1)
n
summary(mod1)


summary(mod1)
funders <- unique(cov$department)
recipients <- unique(gov_shapes$lgid)

grant_net <- network.initialize(n = length(funders) + length(recipients),bipartite = length(funders),directed = F)
network.vertex.names(grant_net) <- c(funders,recipients)

edges <- cov[,sum(amount),by=.(department,lgid)][V1>0,]
edges$lgid <- as.character(edges$lgid)
edges$lgid[nchar(edges$lgid)==4]<- paste0('0',edges$lgid[nchar(edges$lgid)==4])
tail_index <- match(edges$department,network.vertex.names(grant_net))
head_index <- match(edges$lgid,network.vertex.names(grant_net))
add.edges(grant_net,tail = tail_index, head = head_index,
          vals.eval = c(edges$V1,log(edges$V1)),names.eval = c('total_amount','ln(total)'))
library(ergm.count)

library(mhurdle)
Y_grand <- ifelse(is.na(y),0,y)
dt_grand <- as.data.table(idat[grep('^u_',names(idat))])[1:n,]
dt_grand$Y <- Y_grand
install.packages('pscl')
library(pscl)

library(MASS)



hurdle(Y~u_type,data = dt_grand,
       zero.dist = 'binomial',dist = 'geometric')


mh <- mhurdle(Y ~ 0 | u_type,data = dt_grand,
        h2 = TRUE, dist = "n", method = "bhhh")
lnmh <- update(mh, dist = "ln")
mhurdle::margins(mh)

length(idat$u_type)
length(Y_grand)


ergm(grant_net~sum+nonzero,reference = ~Poisson,response = 'ln(total)',verbose = T,)

network.density(grant_net)
ergm()

grant_net
inla.list.models()


plot(grant_net,isolates = F)

table(is.na(head_index))


cov[,.N,by=.(vendor_legal_name_not_redacted)][order(-N),]

table(cov$department)
cov[is.na(cov$lgid),]
cov$


childgovs <- readRDS('building_blocks/child_governments.rds')




table(cov$department)
table(cov$name)


table(is.na(cov$vendor_legal_name_not_redacted))
cov[is.na(cov$lgid),][1:5,]
cares <- fread('building_blocks/cares_grants_full - just_local_gov.csv')

head(cov)
table(cares$Entity.Name)
cares$