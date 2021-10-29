#budget topic models
library(keyATM)
library(plyr)
library(dplyr)
library(tidytext)
govsall<-readRDS("CARESGOVS.rds")
govsall$lgid<-govsall$lgid %>% as.character() %>% stringr::str_pad(5,"left","0")
head(govsall)
flist<-list.files("Downloads/gov_budgets",full.names=T) %>% .[stringr::str_detect(.,"pdf")]
l_ply(flist,function(X) pdftools::pdf_text(X) %>% saveRDS(paste0("Documents/CARES_FUND/budgets/",X %>% basename(),".rds")))

f1<-lapply(paste0("Documents/CARES_FUND/budgets/",flist%>% basename(),".rds"),readRDS)
fsummary<-sapply(f1,function(X) sum(nchar(X)))

file.copy(flist[fsummary==0], paste0("Documents/CARES_FUND/budgets_nonocrd/",basename(flist[fsummary==0])))

names(f1)<-list.files("Downloads/gov_budgets") %>% .[stringr::str_detect(.,"pdf")] %>% .[1:100]
f1<-reshape2::melt(f1)
colnames(f1)<-c("text","lgid")
f1$lgid<-gsub(".pdf","",f1$lgid,fixed=T)
f1$pageid<-1:nrow(f1)
f1<-f1 %>% mutate(text=tolower(text))
f1<-f1 %>% unnest_tokens(word,text,token="words",drop=F) %>% select(-text) %>% anti_join(stop_words)
f1 <- f1 %>% filter(stringr::str_detect(word, "^[0-9]")==F)
f2<-f1 %>% group_by(lgid,pageid) %>% count(word,sort=F)
f2$spell<-hunspell::hunspell_check(f2$word)
f2<-filter(f2,spell==T) %>% select(-spell)
f2 %>% head(100) %>% View()
f2$uniid<-paste(f2$lgid,f2$pageid,sep="_")
f3<-cast_dfm(f2,uniid,word,n)


vars <- govsall %>% filter(lgid%in%f2$lgid) %>% select(lgid,type,county) %>% mutate(type=as.factor(type))
vars<-left_join(f2 %>% ungroup %>%select(uniid,lgid) %>% unique(),vars)
head(vars)

#https://leg.colorado.gov/sites/default/files/2018_local_government_handbook_with_cover_0.pdf
#table 12
keywords <- list(id_words=c("county","city","district","board","department","office"),
                 Finance_words=c("fund","budget","funds","tax","revenue","expenditures","expenditure"),
                   Public_Safety=c("police","sheriff","fire","ambulance","emergency"),
                 General_Government=c("administration","management","admin"),
                 Culture_and_Recreation=c("parks","park","natural","open","sports","activities","culture","recreation","golf","music","concert","art","arts"),
                 Social_Services=c("office","permit","assistance"),
                 Public_Works=c("works","sewer","water","electricity","waste","garbage","recycling","utilities","maintenance","roads","road","infrastructure"),
                 Judicial=c("court","judge","trial","fine","justice"),
                 Health=c("hospital","clinic","food"),
                 Debt_Service=c("loan","bond","debt","credit","obligation"),
                 Transfers=c("intergovernmental","transfers"))
head(f3)
vars<-data.frame("uniid"=quanteda::docid(f3)) %>% left_join(vars %>% mutate(uniid=as.factor(uniid)))
head(vars)
out <- keyATM(docs              = keyATM_read(f3),    # text input
              no_keyword_topics = 11,              # number of topics without keywords
              keywords          = keywords,       # keywords       
              model             = "covariates",  # select the model
              model_settings    = list(covariates_data    = vars, 
                                       covariates_formula = ~ type),
              options           = list(seed = 250,iterations=1500))
hrm<-by_strata_DocTopic(out, by_var = "type2", labels = c("no","yes"))
metdit<-by_strata_DocTopic(out, by_var = "type6", labels = c("no","yes")) 
library(ggplot2)
attab <- lapply(list(hrm,metdit),function(X) keyATM:::summary.strata_doctopic(X, .95, c("hdi"), point="mean") %>% bind_rows())
names(attab)<-c("home rule munic.","metro district")
attab<-bind_rows(attab,.id="govtype")
ggplot(attab %>% filter(TopicId%in%c(2:12)))+geom_pointrange(aes(x=Topic,y=Point,ymin=Lower,ymax=Upper,colour=label))+coord_flip()+facet_grid(~govtype)+theme_bw()
keyATM::top_words(out)
