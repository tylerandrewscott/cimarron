govflatfile<-readRDS("Documents/CARES_FUND/data_correlates/government_shapes.rds")

citis_counties<-filter(govflatfile,type%in%1:5)

get_trend<-function(lgid){
read.csv(paste0("https://dola.colorado.gov/reports-prod/rwservlet?cm_trend&desformat=DELIMITEDDATA&p_lg_id=",lgid),sep="\t")}
citytrends<-lapply(citis_counties$lgid,get_trend)

citytrendstoo<-sapply(citytrends, nrow)
citytrendstoo<-citytrends[citytrendstoo>0]
citytrendstoo<-bind_rows(citytrendstoo)
citytrendstoo<-filter(citytrendstoo, AUDIT_YEAR%in%c(2017,2018,2019))
#small cities no audits since 2016
trytoo<-lapply(citis_counties[as.numeric(citis_counties$lgid)%in%as.numeric(citytrendstoo$LG_ID1)==F,]$lgid,get_trend)
trytoo<-bind_rows(trytoo[sapply(trytoo,nrow)>0])
colnames(citytrendstoo)
citytrendstoo %>% select(NAME,LG_ID1,AUDIT_YEAR,GEN_EXP_TOTAL,GEN_REV_TOTAL,GEN_REV_INTGOVT ,GEN_REV_FEDERAL,CM_MILL_LEVY,CM_NET_AV,GEN_TOT_DEBT,GEN_ASSETS,GEN_LIABILITIES) %>% saveRDS("Documents/GitHub/cimarron/building_blocks/cm_finance.rds")


sds_finance<-filter(govflatfile,type%in%1:5==F)

get_trend<-function(lgid){
  read.csv(paste0("https://dola.colorado.gov/reports-prod/rwservlet?sd_trend&desformat=DELIMITEDDATA&p_lg_id=",lgid),sep="\t")}
sdtrends<-lapply(sds_finance$lgid,get_trend)

head(sdtrends)

sdtrendstoo<-sapply(sdtrends, nrow)
sdtrendstoo<-sdtrends[sdtrendstoo>0]
sdtrendstoo<-bind_rows(sdtrendstoo)
sdtrendstoo<-filter(sdtrendstoo, AUDIT_YEAR%in%c(2017,2018,2019))
sdtrendstoo %>% select(NAME,LG_ID1,AUDIT_YEAR,EXP_TOTAL,REV_TOTAL,REV_INTGOVT,MILL_LEVY,NET_AV,TOTAL_DEBT,ASSETS,LIABILITIES) %>% saveRDS("Documents/GitHub/cimarron/building_blocks/sd_finance.rds")

#include governments that have audits on file

govflatfile[govflatfile$lgid%in%c(unique(sdtrendstoo$LG_ID1),stringr::str_pad(unique(citytrendstoo$LG_ID1),width=5,side="left",pad="0"))==F,] %>% select(name,lgid) 

#check child governments
childgovs<-lapply(citis_counties$lgid,function(X){try({
rvest::read_html(paste0("https://dola.colorado.gov/dlg_portal/filings.jsf?id=",X)) %>% html_nodes("#main_form\\:j_idt45\\:j_idt60\\:childGovernments a") %>% html_text() %>% stringr::str_extract(.,"\\([0-9]+\\)") %>% gsub("\\(|\\)","",.)})})
citis_counties$childgovs<-childgovs
citis_counties %>% as.data.frame() %>% select(name,lgid,childgovs) %>% saveRDS("Documents/GitHub/cimarron/building_blocks/child_governments.rds")
#
govflatfile[govflatfile$lgid%in%c(unique(sdtrendstoo$LG_ID1),stringr::str_pad(unique(citytrendstoo$LG_ID1),width=5,side="left",pad="0"))==F,] %>% select(name,lgid) %>% filter(lgid%in%unlist(childgovs)==F) 

#figure out missings

govflatfile<-readRDS("Documents/CARES_FUND/data_correlates/government_shapes.rds")
sdf<-readRDS("Documents/GitHub/cimarron/building_blocks/sd_finance.rds")
cmf<-readRDS("Documents/GitHub/cimarron/building_blocks/cm_finance.rds")
sdf<-filter(sdf,AUDIT_YEAR!=2020)
colnames(cmf)
cmf<-cmf %>% select(LG_ID1,AUDIT_YEAR,GEN_EXP_TOTAL,GEN_REV_TOTAL,GEN_REV_INTGOVT,GEN_TOT_DEBT,GEN_ASSETS)
colnames(cmf)<-gsub("GEN_","",colnames(cmf))
cmf<-cmf %>% rename(TOTAL_DEBT=TOT_DEBT)
sdf<-sdf %>% select(LG_ID1,AUDIT_YEAR,EXP_TOTAL,REV_TOTAL,REV_INTGOVT,TOTAL_DEBT,ASSETS)
finance_df<-rbind(cmf,sdf)
finance_df<-finance_df %>% rename(lgid=LG_ID1)
finance_df<-filter(finance_df,ASSETS>0)
library(dplyr)
dplyr::summ
finance_df2<-finance_df %>% mutate_all(as.numeric) %>% group_by(lgid) %>% summarise(EXP_TOTAL=mean(EXP_TOTAL),REV_TOTAL=mean(REV_TOTAL),REV_INTGOVT=mean(REV_INTGOVT),TOTAL_DEBT=mean(TOTAL_DEBT),ASSETS=mean(ASSETS),FINANCE_YEARS=length(AUDIT_YEAR))
finance_df2 %>% saveRDS("Documents/GitHub/cimarron/building_blocks/finance_data_out.rds")

govtemp<-govflatfile[govflatfile$lgid%in%c(stringr::str_pad(sdf$LG_ID1,width=5,side="left",pad="0"),stringr::str_pad(cmf$LG_ID1,width=5,side="left",pad="0"))==F,]
children<-readRDS("Documents/GitHub/cimarron/building_blocks/child_governments.rds")
govtemp<-govtemp[govtemp$lgid%in%c(children$childgovs %>% unlist() %>% unique())==F,]
head(govtemp)
readin<-pdftools::pdf_text("https://s3.us-east-2.amazonaws.com/lg.colorado.gov/published_exemptions/2019/1405.01%20Promontory%20Metropolitan%20District%20No.%201.pdf")
data.frame("total_revenue"=readin[[2]] %>% stringr::str_extract("2-24[\\w+\\W+]"),
           "total_expenditures"=readin[[2]] %>% stringr::str_extract("3-26[\\w+\\W+]"),
           "debt"=readin[[3]] %>% stringr::str_extract("4-4[\\w+\\W+]"),
           "assets"=readin[[4]] %>% stringr::str_extract("6-3[\\w+\\W+]"),
           "intergovernmental"=readin[[2]] %>% stringr::str_extract("2-6[\\w+\\W+]"))
           
           
           

https://dola.colorado.gov/reports-prod/rwservlet?cm_trend&desformat=DELIMITEDDATA&p_lg_id=01005
cm1<-read.csv("https://dola.colorado.gov/reports-prod/rwservlet?cm_compend&desformat=DELIMITEDDATA&p_end_year=2019",sep="\t")
cm2<-read.csv("https://dola.colorado.gov/reports-prod/rwservlet?cm_compend&desformat=DELIMITEDDATA&p_end_year=2018",sep="\t")
nrow(cm2)
head(cm1)
table(cm2$NAME)
b1<-httr::GET("https://dola.colorado.gov/reports-prod/rwservlet?cm_compend")
httr::content(b1, as="parsed")
  
