makeflatfile
govsflatfile<-merge(readRDS("Documents/CARES_FUND/data_correlates/government_shapes.rds"), readRDS("Documents/CARES_FUND/budget_topic_model_out.rds")$theta  %>% as.data.frame()  %>% .[,1:13] %>% mutate("lgid"=readRDS("Documents/CARES_FUND/budget_topic_variables_out.rds")$lgid),all.x=T)
lgcares<-read.csv(paste0(filepath1,"/building_blocks/cares_grants_full - just_local_gov.csv"))
occares<-read.csv(paste0(filepath1,"/building_blocks/cares_grants_full - other_counties_partial.csv"))

#alternative cares data (from state but DOES NOT have county passthroughs)
alt.cares<-read.csv(paste0(filepath1,"/building_blocks/grants_from_state.csv"))
alt.cares<-dplyr::filter(alt.cares, program=="CVRF")
alt.other<-read.csv(paste0(filepath1,"/building_blocks/grants_from_state.csv")) %>% dplyr::filter(., program!="CVRF",lubridate::year(lubridate::ymd(dateofaward))<=2019)
alt.other<-ddply(alt.other,.(lgid),summarize,totalawardspre2020=sum(award))
alt.cares<-alt.cares %>% ddply(.,.(lgid),summarize,alt.Total=sum(award))
alt<-join(alt.cares,alt.other)
alt$lgid<-alt$lgid %>% stringr::str_pad(.,5,"left",0)

lgout<-ddply(lgcares,.(lgid),summarize,Total=sum(Amount))
ocout<-ddply(occares %>% mutate(Value=Value %>% gsub("[$,]","",.) %>% as.numeric()),.(LGID_to),summarize,Total=sum(Value))
ocout<-ocout %>% rename(lgid=LGID_to)
totalout<-rbind(lgout,ocout)        
totalout$lgid<-totalout$lgid %>% stringr::str_pad(.,5,"left",0)
govsflatfile<-merge(govsflatfile,totalout,all.x=T)
govsflatfile$Total[is.na(govsflatfile$Total)]<-0

library(INLA)
model_data<-govsflatfile %>% as.data.frame() %>% select(-geometry,-tracts)
colnames(model_data)<-colnames(model_data) %>% gsub("^[0-9]+_","",.)
model_data$Received_grant<-as.numeric(model_data$Total>0)
model_data<-left_join(model_data,alt)
model_data$alt.Total[is.na(model_data$alt.Total)]<-0
model_data$type[stringr::str_detect(model_data$county,"Adams|Denver|Jefferson|El")]
model_data$alt.Total[stringr::str_detect(model_data$county,"Adams|Denver|Jefferson|El")]
model_data
basic_model<-inla(Received_grant~scale(RPL_THEME1)+scale(RPL_THEME2)+scale(RPL_THEME3)+scale(RPL_THEME4)+scale(nri_risk_score)+scale(Finance_words)+scale(Public_Safety)+scale(Culture_and_Recreation)+scale(Social_Services)+scale(Public_Works)+scale(Health)+f(type,model="iid"),data=model_data,family="binomial")

modeldata2<-filter(model_data,Total>0)

dollars_model<-inla(log(Total)~scale(RPL_THEME1)+scale(RPL_THEME2)+scale(RPL_THEME3)+scale(RPL_THEME4)+scale(nri_risk_score)+scale(Finance_words)+scale(Public_Safety)+scale(Culture_and_Recreation)+scale(Social_Services)+scale(Public_Works)+scale(Health)+f(type,model="iid"),data=modeldata2,family="gaussian")

summary(dollars_model)
