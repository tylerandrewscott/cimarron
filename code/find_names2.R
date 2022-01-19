library(plyr)
library(dplyr)

filepath1<-"Documents/GitHub/cimarron"
filepath2<-"Documents/CARES_FUND/data_correlates"
govsflatfile<-merge(readRDS("Documents/CARES_FUND/data_correlates/government_shapes.rds"), readRDS("Documents/CARES_FUND/budget_topic_model_out.rds")$theta  %>% as.data.frame()  %>% .[,1:13] %>% mutate("lgid"=readRDS("Documents/CARES_FUND/budget_topic_variables_out.rds")$lgid),all.x=T)
head(govsflatfile)
mtable<-readRDS("Documents/GitHub/Cimarron/building_blocks/matchtable.rds")


lgcares<-read.csv(paste0(filepath1,"/building_blocks/cares_grants_full - just_local_gov.csv"))
occares<-read.csv(paste0(filepath1,"/building_blocks/cares_grants_full - other_counties_partial.csv"))

#grant data (from state but DOES NOT have county passthroughs), those are above

#find all fund names
#funds <- read.socrata(
#  "https://data.colorado.gov/resource/rifs-n6ib.json?$query=SELECT fund, fiscal_year WHERE fiscal_year > 2019",

#  app_token = "OyLIQK0xzoBvA1GmLPsHltGCJ"
)
j1<-jsonlite::read_json("https://data.colorado.gov/resource/rifs-n6ib.json?$query=SELECT%20distinct%20fund")
j1<-sapply(j1, function(X) X$fund)
j1 %>% .[stringr::str_which(.,"CSF")]
#j1<-j1[c(68,184:187)]
#J1 is fund names kept in next pull
#this includes some ARP funds as well

fullcovid<-read.socrata("https://data.colorado.gov/resource/rifs-n6ib.json?$where=fund in('CARES Act Fund (CARE)','Coronavirus Emergency Supplemental Funding (CESF)','Coronavirus Local Fiscal Recovery Fund (CLFR)','Coronavirus State Fiscal Recovery Fund (CSFR)','Coronavirus State Fiscal Recovery Fund - Governor''s Allocat (CSFG)')")
fullcovid2<-read.socrata("https://data.colorado.gov/resource/rifs-n6ib.json?$where=fund in('Address Confidentiality Program (ARPA-CSFR Funds) (21AF)','Affordable Housing and Home Ownership Cash Fund (CSFH)','Behavioral and Mental Health Cash Fund (CSFB)','Colorado Heritage Communities Fund (ARPA-CSFR Funds) (14CC)','Colorado Startup Loan Program Fund (ARPA-CSFR) (15CC)','Digital Grant Inclusion Grant Program Fund (ARPA-CSFR) (DGIC)','Economic Development (ARPA-CSFR Funds) (156C)','Economic Recovery and Relief Cash Fund (CSFE)','High-Risk Families Cash Fund - CSFRF (12HC)','Highway Users Tax Fund - CSFR (405C)','Housing Development Grant Fund (ARPA-CSFR Funds) (23VC)','Interconnectivity Grant Program Fund (ARPA-CSFR Funds) (IGPC)','Opiate Antagonist Bulk Purchase Fund (ARPA-CSFR) (15OC)','Victims Assistance and Law Enforcement (ARPA-CSFR) (207F)','Workers, Employers, and Workforce Centers Cash Fund (CSFW)','Revenue Loss Restoration Cash Fund (CSFL)','Victims Assistance Fund (ARPA-CSFR Funds) (714C)')")
fullcovid2$vendorID<-fullcovid2$vendor_legal_name_not_redacted %>% stringr::str_extract(.,"\\(\\w+\\)") %>% gsub("\\(|\\)","",.)
fullcovid2<-left_join(fullcovid2,mtable)
fullcovid$vendorID<-fullcovid$vendor_legal_name_not_redacted %>% stringr::str_extract(.,"\\(\\w+\\)") %>% gsub("\\(|\\)","",.)
fullcovid<-left_join(fullcovid,mtable)

missingIDs<-rbind(fullcovid[stringr::str_which(fullcovid$vendorID,"VC"),] %>% select(vendorID,name,vendor_legal_name_not_redacted) %>% filter(is.na(name)) %>% unique() %>% .[c(141,149,231,247,260,259,457,543),],fullcovid2[stringr::str_which(fullcovid2$vendorID,"VC"),] %>% select(vendorID,name,vendor_legal_name_not_redacted) %>% filter(is.na(name)) %>% .[7:17,])
write.csv(missingIDs,"codemissingids.csv")
missingsback<-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRMK-IRbwQVxa0AJ6U9bdu4rN5Ped3oAAW6OH6984eXaAb29gGzZWIu0ixyKOw09hjXs8wC3FDmPm9N/pub?gid=905427508&single=true&output=csv")
mtable<-rbind(mtable, missingsback %>% select(-X) %>% rename(Var1=vendor_legal_name_not_redacted))
head(mtable)