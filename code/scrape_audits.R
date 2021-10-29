

audit_table<-lapply(1:162,function(X){
grq<-rvest::read_html(paste0("https://apps.leg.co.gov/osa/lg/local_govs?page=",X))
Sys.sleep(1)
grq %>% rvest::html_table() %>% .[[1]]})
audit_table<-bind_rows(audit_table)
audit_table$cntrln<-sprintf("%.2f",audit_table$`Control Number`)
audit_table %>% saveRDS("Documents/CARES_FUND/audit_table.rds")
audit_table<-readRDS("Documents/CARES_FUND/audit_table.rds")

audit_table<-filter(audit_table, `Local Government Type`!="Inactive")

for(i in 5:nrow(audit_table)){
  try(
paste0("https://s3.us-east-2.amazonaws.com/lg.colorado.gov/published_audits/2019/",paste(audit_table$cntrln[i],audit_table$`Local Government`[i]),".pdf") %>% URLencode() %>% download.file(destfile=paste0("Documents/CARES_FUND/gov_financial_statements/",audit_table$cntrln[i],".pdf")))}

audit_table_missing<-filter(audit_table, cntrln%in%c(list.files("Documents/CARES_FUND/gov_financial_statements/") %>% gsub("\\.pdf","",.))==F)
for(i in 5:nrow(audit_table_missing)){
  try(
    paste0("https://s3.us-east-2.amazonaws.com/lg.colorado.gov/published_exemptions/2019/",paste(audit_table_missing$cntrln[i],audit_table_missing$`Local Government`[i]),".pdf") %>% URLencode() %>% download.file(destfile=paste0("Documents/CARES_FUND/gov_financial_statements/",audit_table_missing$cntrln[i],".pdf")))
  Sys.sleep(2)}

audit_table_missing<-filter(audit_table, cntrln%in%c(list.files("Documents/CARES_FUND/gov_financial_statements/") %>% gsub("\\.pdf","",.))==F)
audit_table_missing$lg_site<-NA


locgov<-rvest::session("https://apps.leg.co.gov/osa/lg/local_govs")
locgov2<-rvest::html_form(locgov)[[2]] %>% rvest::html_form_set(`q[control_number_cont]`=audit_table_missing$cntrln[i]) %>% rvest::html_form_submit()
audit_table_missing$lg_site[i]<-locgov2 %>% read_html() %>% html_nodes(".table-striped a") %>% html_attr("href")
locgov<-read_html(audit_table_missing$lg_site[i])
tpath<-paste0("tr:nth-child(",which(c(locgov %>% html_table() %>% .[[1]] %>% .$`Submission Year`==2019)==T),") a")
locgov %>% html_elements(tpath) %>% html_element("href")


for(i in 65:nrow(audit_table)){
  Sys.sleep(3)
  i=65
gpage<-read_html(paste0("https://apps.leg.co.gov/osa/lg/local_govs/",i))
if(gpage %>% html_nodes("p:nth-child(4) span") %>% html_text() %>% stringr::str_extract(.,"[0-9\\.]+") %in% c(list.files("Documents/CARES_FUND/gov_financial_statements/") %>% gsub("\\.pdf","",.))==T){}
else{
  tpath<-paste0("tr:nth-child(",which(c(gpage %>% html_table() %>% .[[1]] %>% .$`Submission Year`==2019)==T),") a")
  read_html(gpage %>% html_elements(tpath) %>% html_attr("href")) %>% html_elements(".button") %>% html_attr("href") %>% URLencode()  %>% download.file(destfile=paste0("Documents/CARES_FUND/gov_financial_statements/",gpage %>% html_nodes("p:nth-child(4) span") %>% html_text() %>% stringr::str_extract(.,"[0-9\\.]+"),".pdf"))
}
cat(paste0(" ",i," "))}
