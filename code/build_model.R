library(plyr)
library(dplyr)

filepath1<-"Documents/GitHub/cimarron"
filepath2<-"Documents/CARES_FUND/data_correlates"
govsflatfile<-merge(readRDS("Documents/CARES_FUND/data_correlates/government_shapes.rds"), readRDS("Documents/CARES_FUND/budget_topic_model_out.rds")$theta  %>% as.data.frame()  %>% .[,1:13] %>% mutate("lgid"=readRDS("Documents/CARES_FUND/budget_topic_variables_out.rds")$lgid),all.x=T)
lgcares<-read.csv(paste0(filepath1,"/building_blocks/cares_grants_full - just_local_gov.csv"))
occares<-read.csv(paste0(filepath1,"/building_blocks/cares_grants_full - other_counties_partial.csv"))
filter(lgcares,Short.Title=="SD-152")$Amount %>% sum()

#alternative cares data (from state but DOES NOT have county passthroughs)
alt.cares<-read.csv(paste0(filepath1,"/building_blocks/grants_from_state.csv"))
head(alt.cares)
alt.cares<-dplyr::filter(alt.cares, program=="CVRF")
filter(alt.cares, projectnmbr=="SD-152")$award
library("RSocrata")
codebook<-readLines("https://raw.githubusercontent.com/GoCodeColorado/GoCodeColorado-kbase-public/master/Data/CDPA%20Transparency%20Online%20Project%20(TOPS).md")
codebook<-codebook[stringr::str_detect(tolower(codebook),"grant")]
codebook
df <- read.socrata(
  "https://data.colorado.gov/resource/rifs-n6ib.json?fund=CARES Act Fund (CARE)",
  app_token = "OyLIQK0xzoBvA1GmLPsHltGCJ"
)
library(plyr)
library(dplyr)
df$department %>% table()
df$vendor_legal_name_not_redacted %>% table()
df$major_program %>% table()
?read.socrata
funds <- read.socrata(
  "https://data.colorado.gov/resource/rifs-n6ib.json?$query=SELECT fund, fiscal_year WHERE fiscal_year > 2019",
  
  app_token = "OyLIQK0xzoBvA1GmLPsHltGCJ"
)
j1<-jsonlite::read_json("https://data.colorado.gov/resource/rifs-n6ib.json?$query=SELECT%20distinct%20fund")
j1<-sapply(j1, function(X) X$fund)
j1<-j1[c(68,184:187)]
#J1 is fund names kept in next pull
#this includes some ARP funds as well

fullcovid<-read.socrata("https://data.colorado.gov/resource/rifs-n6ib.json?$where=fund in('CARES Act Fund (CARE)','Coronavirus Emergency Supplemental Funding (CESF)','Coronavirus Local Fiscal Recovery Fund (CLFR)','Coronavirus State Fiscal Recovery Fund (CSFR)','Coronavirus State Fiscal Recovery Fund - Governor''s Allocat (CSFG)')")
govsall<-readRDS(paste0(filepath1,"/building_blocks/CARESGOVS.rds"))
govsall$name<-govsall$name %>% stringr::str_split("\\n",2) %>% sapply(.,function(x) x[1])
fullcovid$vendor_legal_name_not_redacted %>% unique()
govsall$name[1:100]
fullcovid$vendor_legal_name_not_redacted[stringr::str_which(tolower(fullcovid$vendor_legal_name_not_redacted),"alamosa")]
vtable<-table(fullcovid$vendor_legal_name_not_redacted) %>% as.data.frame()
vtable<-filter(vtable, stringr::str_detect(Var1,"LLC")==F)
vtable<-filter(vtable, stringr::str_detect(Var1,"LLP")==F)
vtable<-filter(vtable, stringr::str_detect(tolower(Var1),"faith|church")==F)
vtable<-filter(vtable, stringr::str_detect(Var1," INC ")==F)
vtable<-filter(vtable, stringr::str_detect(Var1," Inc\\.")==F)
vtable<-filter(vtable, stringr::str_detect(Var1," LP ")==F)
vtable<-filter(vtable, stringr::str_detect(Var1," PC ")==F)
head(vtable,200)
vtable2<-lgcares %>% select(Vendor.Legal.Name...Not.Redacted,lgid) %>% unique() %>% na.omit()
colnames(vtable2)<-c("Var1","lgid")
vtable<-left_join(vtable,vtable2)
vtable<-filter(vtable, stringr::str_detect(Var1,"LLC")==F)
vtable<-filter(vtable, stringr::str_detect(Var1,"LLP")==F)
vtable<-filter(vtable, stringr::str_detect(tolower(Var1),"faith|church")==F)
vtable<-filter(vtable, stringr::str_detect(Var1," INC ")==F)
vtable<-filter(vtable, stringr::str_detect(Var1," Inc\\.")==F)
vtable<-filter(vtable, stringr::str_detect(Var1," LP ")==F)
vtable<-filter(vtable, stringr::str_detect(Var1," PC ")==F)
head(vtable,200)
install.packages("stringdist")
library(stringdist)
stringdist::amatch(tolower(vtable$Var1)[7] %>% gsub("\\(\\w+\\)","",.) %>% stringr::str_trim(),tolower(govsall$name),maxDist=.5)

stringdist::amatch(tolower(vtable$Var1)[7] %>% gsub("\\(\\w+\\)","",.) %>% stringr::str_trim(),tolower(schools$NAME),maxDist=.5)
schools %>% head()

xl1<-read.csv(file.path(filepath1,"building_blocks","school_vendor_crosswalk.csv"))
stringdist::stringdist(xl1$CDE.Org.Name[1],govsall$name[2321],"jw")
which.min(stringdist::stringdist(xl1$CDE.Org.Name[1],govsall$name,"jw"))
which.min(stringdist::stringdist(xl1$CDE.Org.Name[2],govsall$name,"jw"))
which.min(stringdist::stringdist(xl1$CDE.Org.Name[3],govsall$name,"jw"))
which.min(stringdist::stringdist(xl1$CDE.Org.Name[4],govsall$name,"jw"))
mdist<-stringdist::amatch(xl1$CDE.Org.Name,govsall$name,method="jw",maxDist=.5)
schools<-data.frame("vendorName"=NA,"vendorID"=xl1$Warrant.Vendor.ID,"orgName"=xl1$CDE.Org.Name,"lgname"=govsall$name[mdist],"lgid"=govsall$lgid[mdist])
write.csv(schools,"schoolsout.csv")
table(stringr::str_detect(vtable$Var1,schools$vendorID[-c(179:180)] %>% paste0("collapse"="|")))
vnonschool<-vtable[which(stringr::str_detect(vtable$Var1,schools$vendorID[-c(179:180)]  %>% paste0("collapse"="|"))==F),]
head(vnonschool)
vnonschool<-filter(vnonschool, is.na(lgid)==T)
mdist2<-stringdist::amatch(vnonschool$Var1 %>% gsub("\\(\\w+\\)","",.)  %>% stringr::str_trim() %>% tolower(),govsall$name %>% tolower(),method="jw",maxDist=.5)
vnonschool<-cbind(vnonschool,data.frame("name"=govsall$name[mdist2]),"lgid"=govsall$lgid[mdist2])
vnonschool %>% write.csv("vnonschool.csv")


nonschoolbackin<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRMK-IRbwQVxa0AJ6U9bdu4rN5Ped3oAAW6OH6984eXaAb29gGzZWIu0ixyKOw09hjXs8wC3FDmPm9N/pub?gid=389168823&single=true&output=csv")
head(nonschoolbackin)
nonschoolbackin_ready<-filter(nonschoolbackin,code=="t")
vtable1<-filter(vtable,is.na(lgid)==F) %>% left_join(.,govsall %>% select(name,lgid) %>% mutate(lgid=as.character(lgid)))
vtable1$vendorID<-vtable1$Var1 %>% stringr::str_extract(.,"\\(\\w+\\)")
colnames(nonschoolbackin_ready %>% rename(vendorID=X.1,name=lgname) %>% select(-code,-X))
vtable1<-rbind(vtable1,nonschoolbackin_ready %>% rename(vendorID=X.1,name=lgname) %>% select(-code,-X))
schoolsin<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRMK-IRbwQVxa0AJ6U9bdu4rN5Ped3oAAW6OH6984eXaAb29gGzZWIu0ixyKOw09hjXs8wC3FDmPm9N/pub?gid=187829948&single=true&output=csv")
head(schoolsin)
uvname<-fullcovid$vendor_legal_name_not_redacted %>% unique()
uvname<-sapply(schoolsin$vendorID,function(X) uvname[stringr:: str_which(uvname,X)]) %>% reshape2::melt()
colnames(uvname)<-c("Var1","vendorID")
schoolsin<-left_join(schoolsin,uvname)
vtable1<-rbind(vtable1 %>% select(-Freq),schoolsin %>% select(-orgName))
head(vtable1)
nonschoolbackin_notready<-filter(nonschoolbackin,code=="g")



amatch(,govsall$name,method="lcs")
govsall$name[760]
sapply(govsall$name,nchar) %>% max()
stringdist::stringdist(xl1$CDE.Org.Name[1],govsall$name[2321],method="lv")
?amatch
stringr::str_which(govsall$name,"Mapleton")
head(vtable)
inlist<-lapply(tolower(govsall$name),function(X) stringr::str_which(tolower(fullcovid$vendor_legal_name_not_redacted),X))

write.csv(vtable,"codeup_governments.csv")

           
df2<-filter(df, stringr::str_detect(department,"Local Government"))
df2<-filter(df2, program!="Administration - CARES (CVRF90APAD)", stringr::str_detect(vendor_legal_name_not_redacted,"COSTAC0108")==F)
df2$vendor_invoice %>% stringr::str_extract(.,"[A-Z]+\\-[A-Z]+\\-[0-9]+")
df2$vendor_legal_name_not_redacted

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
