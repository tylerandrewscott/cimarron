library(data.table)
library(sf)
library(statnet)

gov_shapes <- readRDS('scratch/government_shapes.rds')
finance <- readRDS('building_blocks/finance_data_out.rds')
cov <- data.table(readRDS('scratch/cares_grant_dataset.rds'))
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
idat$mu.u <- rep(0:1, each=n)
idat$mu.y <- rep(1:0, each=n)
idat$u_type <-c(paste0('type_',cov$simpletypes[cov_index]),rep(NA,n))
idat$y_type <-c(rep(NA,n),paste0('type_',cov$simpletypes[cov_index]))
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

scale_vars <- !names(idat) %in% c('Y','mu.y','mu.y','u_type','y_type')
idat[scale_vars] <- lapply(idat[scale_vars],scale)

table(idat$u_type)
bprior <- list(prior = 'gaussian', param = c(0,1))
cres <- list(return.marginals.predictor = FALSE, 
             return.marginals.random = FALSE)
u.resp <- sd(u,na.rm = T)
y.resp <- sd(y,na.rm=T)
#famcontrol = list(list(prior = "pcprec", param = c(u.resp,0.01)),
                  list(prior = "pcprec", param = c(y.resp,0.01)))
mod0 <- inla(Y~ 0 + mu.u + mu.y + u_type + y_type,
             family = c('zeroinflatedbinomial1','gaussian') , 
     data = idat,Ntrials=rep(1,n),
     control.predictor=list(compute=TRUE),
    # control.family = famcontrol,
     control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
     control.compute = list(waic=TRUE,dic=TRUE,config = F))
mod0$waic$waic
form1 <- reformulate(paste(names(idat)[names(idat)!='Y'],collapse = '+'))
form1 <- update.formula(form1,Y~0+.)
mod1 <- inla(form1,      family = c('zeroinflatedbinomial1','gaussian') ,  
             data = idat,Ntrials=rep(1,n),
             control.predictor=list(compute=TRUE),
            # control.family = famcontrol,
             control.fixed = list(expand.factor.strategy = "model.matrix",prec = bprior),
             control.compute = list(waic=TRUE,dic=TRUE,config = F),verbose = T)
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

ergm(grant_net~sum+nonzero,reference = ~Poisson,response = 'ln(total)',verbose = T,)

network.density(grant_net)
ergm()

grant_net


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