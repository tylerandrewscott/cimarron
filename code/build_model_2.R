library(plyr)
library(dplyr)
gc() 
filepath1<-"Documents/GitHub/cimarron"
filepath2<-"Documents/CARES_FUND/data_correlates"
govsflatfile<-merge(readRDS("Documents/CARES_FUND/data_correlates/government_shapes.rds"), readRDS("Documents/CARES_FUND/budget_topic_model_out.rds")$theta  %>% as.data.frame()  %>% .[,1:13] %>% mutate("lgid"=readRDS("Documents/CARES_FUND/budget_topic_variables_out.rds")$lgid),all.x=T)
children<-readRDS("Documents/GitHub/cimarron/building_blocks/child_governments.rds")
finance<-readRDS("Documents/GitHub/cimarron/building_blocks/finance_data_out.rds")

mtable<-readRDS("Documents/GitHub/Cimarron/building_blocks/matchtable.rds")
missingsback<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRMK-IRbwQVxa0AJ6U9bdu4rN5Ped3oAAW6OH6984eXaAb29gGzZWIu0ixyKOw09hjXs8wC3FDmPm9N/pub?gid=905427508&single=true&output=csv")
missingsback<-missingsback %>% rename(Var1=vendor_legal_name_not_redacted)
mtable<-rbind(mtable,missingsback)
mtable<-na.omit(mtable) 

lgcares<-read.csv(paste0(filepath1,"/building_blocks/cares_grants_full - just_local_gov.csv"))
occares<-read.csv(paste0(filepath1,"/building_blocks/cares_grants_full - other_counties_partial.csv"))
head(occares)
#grant data (from state but DOES NOT have county passthroughs), those are above

#find all fund names
#funds <- read.socrata(
#  "https://data.colorado.gov/resource/rifs-n6ib.json?$query=SELECT fund, fiscal_year WHERE fiscal_year > 2019",
  
#  app_token = "OyLIQK0xzoBvA1GmLPsHltGCJ"
#)
#j1<-jsonlite::read_json("https://data.colorado.gov/resource/rifs-n6ib.json?$query=SELECT%20distinct%20fund")
#j1<-sapply(j1, function(X) X$fund)
#j1 %>% .[stringr::str_which(.,"CSF")]
#j1<-j1[c(68,184:187)]
#J1 is fund names kept in next pull
#this includes some ARP funds as well
library("RSocrata")
fullcovid<-read.socrata("https://data.colorado.gov/resource/rifs-n6ib.json?$where=fund in('CARES Act Fund (CARE)','Coronavirus Emergency Supplemental Funding (CESF)','Coronavirus Local Fiscal Recovery Fund (CLFR)','Coronavirus State Fiscal Recovery Fund (CSFR)','Coronavirus State Fiscal Recovery Fund - Governor''s Allocat (CSFG)')")
fullcovid2<-read.socrata("https://data.colorado.gov/resource/rifs-n6ib.json?$where=fund in('Address Confidentiality Program (ARPA-CSFR Funds) (21AF)','Affordable Housing and Home Ownership Cash Fund (CSFH)','Behavioral and Mental Health Cash Fund (CSFB)','Colorado Heritage Communities Fund (ARPA-CSFR Funds) (14CC)','Colorado Startup Loan Program Fund (ARPA-CSFR) (15CC)','Digital Grant Inclusion Grant Program Fund (ARPA-CSFR) (DGIC)','Economic Development (ARPA-CSFR Funds) (156C)','Economic Recovery and Relief Cash Fund (CSFE)','High-Risk Families Cash Fund - CSFRF (12HC)','Highway Users Tax Fund - CSFR (405C)','Housing Development Grant Fund (ARPA-CSFR Funds) (23VC)','Interconnectivity Grant Program Fund (ARPA-CSFR Funds) (IGPC)','Opiate Antagonist Bulk Purchase Fund (ARPA-CSFR) (15OC)','Victims Assistance and Law Enforcement (ARPA-CSFR) (207F)','Workers, Employers, and Workforce Centers Cash Fund (CSFW)','Revenue Loss Restoration Cash Fund (CSFL)','Victims Assistance Fund (ARPA-CSFR Funds) (714C)')")
fullcovid<-rbind(fullcovid,fullcovid2)


#try cares II
t2<-read.socrata("https://data.colorado.gov/resource/rifs-n6ib.json?$where=cfda_number in('84.184','84.425','93.084','93.107','93.145','93.147','93.243','93.317','93.318','93.323','93.354','93.359','93.391','93.421','93.665','93.817','93.829','93.860','93.889','93.914','93.042','93.044','93.045','93.047','93.048','93.052','93.110','93.142','93.153','93.211','93.253','93.286','93/286','93.301','93.310','93.394','93.397','93.398','93.432','93.498','93.550','93.557','93.568','93.569','93.575','93.623','93.645','93.837','93.839','93.840','93.855','93.879','93.914','93.917','93.918','93.933','96.969','97.024','97.036','97.042','97.044','14.871','14.157','14.191','14.195','14.218','14.225','14.228',',14.231','14.241','14.259','14.326','14.401','14.416','14.850','14.862','14.867','16.034','17.225','17.277','15.535','15.058','93.558','93.575','21.018','20.106','20.301','20.315','20.507','20.509','20.806','20.901','64.005','64.014','64.015','64.024','64.026','64.033','90.404','32.006','45.310','45.312','89.001','89.003','45.129','45.149','45.162','45.164','45.169','59.008','59.037','59.043','59.072','59.072','59.073','59.074','10.130','10.475','10.477','10.539','10.551','10.553','10.555','10.556','10.558','10.559','10.566','10.567','10.568','10.569','10.578','10.664','10.572','10.768','10.777','10.855','11.307','11.454','11.611','11.620','11.805','12.420','12.800')")
saveRDS(t2,'cares_in.rds')
t2<-readRDS('cares_in.rds')
t2<-filter(t2, as.numeric(fiscal_year)>=2020)
fullcovid<-rbind(fullcovid,t2)

fullcovid<-filter(fullcovid, vendor_legal_name_not_redacted!="NOT ENTERED")
fullcovid<-filter(fullcovid, lubridate::ymd(journal_date)>="2020-03-01")
fullcovid_cares<-fullcovid %>% filter(.,stringr::str_detect(paste0(major_program,program,appropriation_class,appropriation,fund),"CA20|CARES|20CA|CESF|COVID|Covid|CVRF"))


#fullcovid_cares<-filter(fullcovid_cares, account_category%in%c("FEDERAL GRANTS AND CONTRACTS (R081)","Grants (OE10)","TRANSFERS (R086)"))
fullcovid_arp<-fullcovid %>% filter(.,stringr::str_detect(paste0(major_program,appropriation_class,program,appropriation,fund),"ARP"))
fullcovid_cares<-filter(fullcovid_cares,record_id%in%fullcovid_arp$record_id==F)
fullcovid_cares$program_short<-fullcovid_cares$major_program %>% stringr::str_extract(.,"\\(\\w+\\)")
fullcovid_cares<-unique(fullcovid_cares)

govsall<-readRDS(paste0(filepath1,"/building_blocks/CARESGOVS.rds"))
govsall$name<-govsall$name %>% stringr::str_split("\\n",2) %>% sapply(.,function(x) x[1])
#fullcovid_cares<-fullcovid.1 %>% filter(.,stringr::str_detect(paste0(major_program,program,appropriation_class,appropriation,fund),"CA20|CARES|20CA|CESF"))

fullcovid_cares$vendorID<-fullcovid_cares$vendor_legal_name_not_redacted %>% stringr::str_extract(.,"\\(\\w+\\)") %>% gsub("\\(|\\)","",.)
fullcovid_cares<-join(fullcovid_cares,mtable %>% select(-Var1)%>% unique(),type="left",match="all")

missingnames<-filter(fullcovid_cares,is.na(lgid))
missingnames<-filter(missingnames, vendor_legal_name_not_redacted!="NOT ENTERED")

missingnames<-data.frame("vendor_legal_name_not_redacted"=missingnames$vendor_legal_name_not_redacted,"vendorID"=stringr::str_extract(missingnames$vendor_legal_name_not_redacted,"\\(\\w+\\)")) %>% unique()


missingnames %>% write.csv("missingnames23.csv")


#federal funds
#prf<-read.csv("Downloads/CRF details export (4).csv")
#head(prf)

#add back in direct distributions

cares_sums<-ddply(fullcovid_cares,.(lgid,name,vendorID,cabinet,department,fund,appropriation),summarize,amount=sum(as.numeric(amount),na.rm=T))
head(cares_sums)
cares_sums<-na.omit(cares_sums) 
colnames(cares_sums)
colnames(occares)
cares_sums<-rbind(cares_sums,occares %>% rename(lgid=LGID_to,name=To,amount=Value) %>% mutate(amount=as.numeric(gsub("\\,|\\$","",amount)),cabinet="county direct distributions",department="county direct distributions",fund="county direct distributions cvrf", appropriation="county direct distributions", vendorID="none") %>% select(lgid,name,cabinet,department, fund,appropriation,vendorID,amount))



# These are transfers that should show up but dont seem to
alt.other<-read.csv(paste0(filepath1,"/building_blocks/grants_from_state.csv")) %>% dplyr::filter(., program=="CVRF")

alt.other<-alt.other[which(as.numeric(alt.other$lgid)%in%as.numeric(cares_sums$lgid)==F),]


govsflatfile2<-data.frame("lgid"=unique(govsflatfile$lgid)) %>% left_join(govsflatfile)

govsflatfile2<-mutate(govsflatfile2,lgid=as.numeric(lgid))
cares_sums<-mutate(cares_sums,lgid=as.numeric(lgid))
govs_cares<-join(govsflatfile2,cares_sums %>% select(-name))
govs_cares$amount[is.na(govs_cares$amount)==T]<-0
govs_cares$firstcounty<-govs_cares$county %>% stringr::str_split(.,",",2) %>% sapply(.,function(X) X[1])


govs_cares<-join(govs_cares,finance,type="left",match="all")

govs_cares$simpletypes<-ifelse(govs_cares$type==1,"county",ifelse(govsflatfile2$type%in%c(2:5),"municipality",ifelse(govs_cares$type==6,"metro district",ifelse(govs_cares$type==99,"school district","other special district"))))

govs_cares %>% saveRDS("cares_grant_dataset.rds")

summary1<-govs_cares %>% ddply(.,.(simpletypes, cabinet),summarize, amount=sum(amount))
summary1<-filter(summary1,is.na(cabinet)==F)

ggplot(summary1)+geom_bar(aes(x=as.character(simpletypes),y=log(amount+1)),stat="identity")+facet_wrap(~cabinet,nrow=5)+coord_flip()+xlab("Government Type")+ylab("$ funded, ln+1")+theme_minimal()

table(summary1$program_short)


#do some modeling
library(INLA)
#program_specific
test<-inla(log(amount+1)~f(firstcounty, model="iid")+f(LGTYPEID,model="iid")+f(program_short,model="iid")+scale(RPL_THEME1)+scale(RPL_THEME2)+scale(RPL_THEME3)+scale(RPL_THEME4)+scale(nri_risk_score)+scale(TOTAL_DEBT+1)+scale(log(REV_TOTAL+1))+scale(log(EXP_TOTAL+1))+scale(log(REV_INTGOVT+1))+f(FINANCE_YEARS,model="iid")+f(lgid,model="iid"),data=govsflatfile2 %>% as.data.frame() %>% select(-geometry,-tracts))

govsff<-govsflatfile2 %>% group_by(lgid) %>% summarise(amount=sum(amount))

govsff<-join(govsff,govsflatfile2 %>% select(-amount,-name,-LGNAME,-program_short,-vendorID) %>% unique())


test<-inla(log(amount+1)~f(firstcounty, model="iid")+f(LGTYPEID,model="iid")+scale(RPL_THEME1)+scale(RPL_THEME2)+scale(RPL_THEME3)+scale(RPL_THEME4)+scale(nri_risk_score)+scale(TOTAL_DEBT+1)+scale(log(REV_TOTAL+1))+scale(log(EXP_TOTAL+1))+scale(log(REV_INTGOVT+1))+f(FINANCE_YEARS,model="iid")+scale(log(Two.Week..Incidence_2020_12_31))+scale(log(Two.Week..Incidence_2020_06_01)),data=govsff %>% as.data.frame() %>% select(-geometry,-tracts))
summary(test)
govsff$tw
??write.xlsx
library(ggplot2)
head(govsff)
govsff$simpletypes<-ifelse(govsff$type==1,"county",ifelse(govsff$type%in%c(2:5),"municipality",ifelse(govsff$type==6,"metro district",ifelse(govsff$type==99,"school district","other special district"))))

ggplot(govsff)+geom_histogram(aes(x=log(amount+1)))+facet_wrap(~simpletypes)
ggplot(govsff)+geom_point(aes(x=`12_Transfers`,y=log(amount+1)))+facet_wrap(~simpletypes)

covid<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vT5Xs4sxU5ZZCOYaW34DR2s4sg3wfWnYxftCqyvGdOL-FnQZsCqGqF2X-WwgEg6Aci2ZE9Km62kdNQW/pub?gid=1931132129&single=true&output=csv")
head(covid)
covid<-filter(covid,date%in%c("2020-12-31","2020-06-01"))
covid<-covid %>% select(county,date,Two.Week.Average.Positivity,Two.Week.Cumulative.Incidence) %>% tidyr::pivot_wider(id_cols=county,names_from = date,values_from = c(Two.Week.Average.Positivity,Two.Week.Cumulative.Incidence)) 
colnames(covid)<-gsub("Average|Cumulative","",colnames(covid))
covid<-covid %>% rename(county1=county)
colnames(covid)
govsff$county1<-toupper(govsff$firstcounty)
govsff<-plyr::join(govsff,covid,by="county1", type="left",match="all")
colnames(govsff)<-colnames(govsff) %>% gsub("-","_",.)


ggplot(govsff)+geom_point(aes(x=log(`Two.Week..Incidence_2020-06-01`),y=log(amount+1)))+facet_wrap(~simpletypes)
ggplot(govsff)+geom_point(aes(x=log(`Two.Week..Incidence_2020-12-31`),y=log(amount+1)))+facet_wrap(~simpletypes)

ggplot(govsff)+geom_point(aes(x=log(`Two.Week..Positivity_2020-12-31`),y=log(amount+1)))+facet_wrap(~simpletypes)+geom_smooth(aes(x=log(`Two.Week..Positivity_2020-12-31`),y=log(amount+1),colour=simpletypes))+facet_wrap(~simpletypes,scale="free")







govsff$`2_Finance_words`
https://drive.google.com/file/d/1MVkvXdNe4SGCUGdJfNdORBcs8ikgONQY/view?usp=sharing"

ggplot(govsff)+geom_point(aes(x=log(`12_Transfers`),y=log(REV_INTGOVT+1)))+geom_smooth(aes(x=log(`12_Transfers`),y=log(REV_INTGOVT+1)))+theme_minimal()

ggplot(govsff)+geom_point(aes(x=log(`6_Social_Services`),y=log(REV_INTGOVT+1)))+geom_smooth(aes(x=log(`12_Transfers`),y=log(REV_INTGOVT+1)))+theme_minimal()+facet_wrap(~simpletypes,scale="free")


+facet_wrap(~simpletypes,scale="free")


head(govsff)
#only keep types of governments that get grants
govsff<-filter(govsff,type%in%c(xtabs(~govsff$type+c(govsff$amount>0)) %>% reshape2::melt() %>% filter(`c(govsff$amount > 0)`==T,value==0) %>% .[,1])==F)
ggplot(govsff)+geom_point(aes(x=log(REV_TOTAL+1),y=log(amount+1)))+facet_wrap(~simpletypes)




govsflatfile2 %>% as.data.frame() %>% select(-geometry,-tracts) %>% colnames()

lgout<-ddply(lgcares,.(lgid),summarize,Total=sum(Amount))
ocout<-ddply(occares %>% mutate(Value=Value %>% gsub("[$,]","",.) %>% as.numeric()),.(LGID_to),summarize,Total=sum(Value))
ocout<-ocout %>% rename(lgid=LGID_to)
totalout<-rbind(lgout,ocout)        
totalout$lgid<-totalout$lgid %>% stringr::str_pad(.,5,"left",0)
govsflatfile<-merge(govsflatfile,totalout,all.x=T)
govsflatfile$Total[is.na(govsflatfile$Total)]<-0
govsflatfile<-govsflatfile %>% left_join(alt)
govsflatfile$alt.Total[is.na(govsflatfile$alt.Total)]<-0
govsflatfile$alt.Total[c(govsflatfile$type%in%c(1:5) & stringr::str_detect(govsflatfile$county,"Adams|Denver|Jefferson|Paso|Arapahoe"))]<-NA
govsflatfile$Total[c(govsflatfile$type%in%c(1:5) & stringr::str_detect(govsflatfile$county,"Adams|Denver|Jefferson|Paso|Arapahoe"))]

filter(govsflatfile, alt.Total>0)$alt.Total[1:10]
filter(govsflatfile, alt.Total>0)$Total[1:10]
library(INLA)
model_data<-govsflatfile %>% as.data.frame() %>% select(-geometry,-tracts)
colnames(model_data)<-colnames(model_data) %>% gsub("^[0-9]+_","",.)
model_data$Received_grant<-as.numeric(model_data$Total>0)
model_data<-left_join(model_data,alt)
model_data$alt.Total[stringr::str_detect(model_data$county,"Adams|Denver|Jefferson|El")]
model_data
basic_model<-inla(Received_grant~scale(RPL_THEME1)+scale(RPL_THEME2)+scale(RPL_THEME3)+scale(RPL_THEME4)+scale(nri_risk_score)+scale(Finance_words)+scale(Public_Safety)+scale(Culture_and_Recreation)+scale(Social_Services)+scale(Public_Works)+scale(Health)+f(type,model="iid"),data=model_data,family="binomial")
summary(basic_model)
modeldata2<-filter(model_data,Total>0)

dollars_model<-inla(log(Total)~scale(RPL_THEME1)+scale(RPL_THEME2)+scale(RPL_THEME3)+scale(RPL_THEME4)+scale(nri_risk_score)+scale(Finance_words)+scale(Public_Safety)+scale(Culture_and_Recreation)+scale(Social_Services)+scale(Public_Works)+scale(Health)+f(type,model="iid"),data=modeldata2,family="gaussian")

covid.county<-read.csv("https://github.com/nytimes/covid-19-data/raw/master/us-counties.csv")
covid.county<-filter(covid.county, state=="Colorado")
head(covid.county)
