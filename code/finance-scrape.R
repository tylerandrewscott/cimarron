https://dola.colorado.gov/reports-prod/rwservlet?sd_trend&desformat=DELIMITEDDATA&p_lg_id=64021&p_end_year=2014
library(sp)
library(rgdal)
govmap<-readOGR("inputs/govmap","dlall")
head(govmap)
head(govmap@data)
county<-readOGR("inputs/govmap","lm_cnty")
head(county@data)
county<-subset(county, cname=="Larimer")
spplot(county)
proj4string(county)
govmap<-spTransform(govmap, proj4string(county))
govmap$inLarimer<-is.na(over(govmap,county)[,1])==FALSE

govmap<-subset(govmap, inLarimer==TRUE)
which(as.numeric(as.character(govmap$LGTYPEID))%in%c(2,4,5))
for(X in 1:nrow(govmap)){
download.file(paste("https://dola.colorado.gov/reports-prod/rwservlet?sd_trend&desformat=DELIMITEDDATA&p_lg_id=",as.character(govmap$LGID)[X],"&p_end_year=2015",sep=""),destfile=file.path("inputs","allfiles",paste(as.character(govmap$LGID)[X],".txt",sep="")),method="auto")
  Sys.sleep(2)
}

m1<-largovs[largovs$Type.%in%c(2:5)==TRUE,]
m1$LG.ID.
for(X in 1:nrow(m1)){
  download.file(paste("https://dola.colorado.gov/reports-prod/rwservlet?sd_trend&desformat=DELIMITEDDATA&p_lg_id=",as.character(m1$LG.ID.)[X],"&p_end_year=2015",sep=""),destfile=file.path("inputs","allfiles",paste(as.character(m1$LG.ID.)[X],".txt",sep="")),method="auto")
  Sys.sleep(2)
}




munis<-rgdal::readOGR("inputs/govmap","Munibounds")
sort(munis@data$city)
sp1<-read.csv(textConnection(RCurl::getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRObXo6NMIQRcUtmtCKU-kqNPep8ZAlQJ4zfOAwlQyq5DLVB-jphXuCSk44m3tDpYQnZY6jSkvGckef/pub?gid=0&single=true&output=csv")))
nrow(sp1)

for(X in 1:nrow(sp1)){
  download.file(paste("https://dola.colorado.gov/reports-prod/rwservlet?cm_trend&desformat=DELIMITEDDATA&p_lg_id=",as.character(sp1$LG.ID.)[X],"&p_end_year=2015",sep=""),destfile=file.path("inputs","allfiles",paste(as.character(sp1$LG.ID.)[X],".txt",sep="")),method="auto")
  Sys.sleep(2)
}

nrow(govmap)



munis<-readOGR("inputs/govmap","Munibounds")
munis<-spTransform(munis, proj4string(county))
munis$inLarimer<-is.na(over(munis,county)[,1])==FALSE
munis<-subset(munis, inLarimer==TRUE)

t1<-read.table(textConnection(RCurl::getURL("https://dola.colorado.gov/reports-prod/rwservlet?sd_trend&desformat=DELIMITEDDATA&p_lg_id=35009&p_end_year=2015")),sep="\t")
?list.files
library(plyr)
library(dplyr)
s1<-list.files("inputs/allfiles",full.names=T)
s2<-lapply(s1,function(X) colnames(read.delim(X))) %>% unlist() %>% unique()

temp<-data.frame(matrix(ncol=length(s2)))[-1,]
colnames(temp)<-s2

which(s1$FUND_NAME=="AUDIT_YEAR1")
s1[6,]
s1<-lapply(s1,function(X) { rbind.fill(temp,lapply(stringr::str_which(readLines(X),"NAM.+[FULL_NAM.+|LG_I.+]"),function(Y) read.delim(X,skip=c(Y-1),colClasses="character",header=T)) %>% bind_rows())})

s3<-bind_rows(s1)
which(sapply(s1, function(X) TRUE%in%stringr::str_detect(X$AUDIT_YEAR,"1018330")))

table(s3$AUDIT_YEAR)
s1[[586]]$LG_ID1
s1[[586]]$AUDIT_YEAR

X<-"inputs/allfiles/30033.txt"
Y=18
lapply(stringr::str_which(readLines(X),"NAME.+[FULL_NAME|LG_ID]"),function(Y) read.delim(X,skip=c(Y-1),colClasses="character")) %>% bind_rows() 

Y<-stringr::str_which(readLines(X),"NAME.+[FULL_NAME|LG_ID]")
i=1
Y

s1<-lapply(s1,function(X) {lapply(stringr::str_which(readLines(X),"NAME.+[FULL_NAME|LG_ID]"),function(Y) {lapply(1:length(Y), function(i) {read.delim(X,skip=c(Y[i]-1),nrows=if(i<length(Y)){c(Y[i+1]-c(Y[i]+1))} else {-1},colClasses="character")}) %>% bind_rows()})})

X<-s1[1]
Y<-rwN
i<-2

lapply(rwN,function(Y) {lapply(1:length(rwN), function(i) {read.delim(X,skip=c(Y[i]-1),nrows=if(i<length(Y)){c(Y[i+1]-c(Y[i]+1))} else {-1},colClasses="character")}) %>% bind_rows()})

s2<-lapply(s1,function(X) {
Y<-stringr::str_which(readLines(X),"NAME.+[FULL_NAME|LG_ID]")
  lapply(1:length(Y), function(i) {
    read.delim(X,skip=c(Y[i]-1),nrows=if(i<length(Y)){c(Y[i+1]-c(Y[i]+1))
      } else {-1},colClasses="character")
    }) %>% bind_rows()})

s3<-bind_rows(s2)

s3<-s3[unique(c(which(is.na(s3$FUND_NAME)),which(s3$FUND_NAME==""),which(s3$FUND_NAME=="Governmental"))),]

filter(s3, NAME=="City of Fort Collins")
temp<-sapply(s3[,c("GEN_ASSETS","ASSETS","TOT_ASSETS")],as.numeric) %>% data.frame() %>%  rowSums(.,na.rm=T)
s3$MANUAL_ASSETS<-temp
s3$MANUAL_LGID<-s3$LG_ID1

s3$MANUAL_LGID<-ifelse(is.na(s3$MANUAL_LGID),s3$LG_ID2,s3$MANUAL_LGID)

temp<-sapply(s3[,c("GEN_LIABILITIES","LIABILITIES","TOT_LIABILITIES")],as.numeric) %>% data.frame() %>%  rowSums(.,na.rm=T)
s3$MANUAL_LIABILITIES<-temp
s3$MANUAL_YEAR<-ifelse(is.na(s3$AUDIT_YEAR),s3$AUDIT_YEAR1,s3$AUDIT_YEAR)
subdat<-ddply(s3, .(MANUAL_LGID,MANUAL_YEAR),summarize,"ASSETS"=max(MANUAL_ASSETS),"LIABILITIES"=max(MANUAL_LIABILITIES))
summary(subdat$LIABILITIES)
colnames(subdat)[1]<-"LGID"
m2013<-merge(govmap,filter(subdat,MANUAL_YEAR==2013))
m2013f<-fortify(m2013)
m2013f<-join(m2013f,m2013@data,by="id")

ggplot(m2013f[-which(is.na(m2013f$ASSETS)),])+geom_polygon(aes(x=long,y=lat,group=group,alpha=log(ASSETS+1)))+scale_alpha_continuous(range=c(0,1))+ggthemes::theme_map()
ggplot(m2013f[-which(is.na(m2013f$LIABILITIES)),])+geom_polygon(aes(x=long,y=lat,group=group,alpha=log(LIABILITIES+1)))+scale_alpha_continuous(range=c(0,1))+ggthemes::theme_map()
ggplot(m2013f[-which(is.na(m2013f$ASSETS/c(m2013f$LIABILITIES+1))),])+geom_polygon(aes(x=long,y=lat,group=group,alpha=log(c(ASSETS+1)/c(LIABILITIES+1))))+scale_alpha_continuous(range=c(0,1))+ggthemes::theme_map()+facet_wrap(~LGTYPEID)
govmap$



ggplot(s3)+geom_smooth(aes(x=MANUAL_YEAR,y=log(MANUAL_ASSETS)))
   
sum1<-rowsum(data.frame(sapply(s3[,c("GEN_ASSETS","ASSETS","TOT_ASSETS")],as.numeric)),group=1:nrow(s3),na.rm=T)
sum1
s3$GEN_REV_TOTAL
summary(as.numeric(s3$TOT_REV))
s3$
summary(as.numeric(s3$GEN_REV_TOTAL))
s3<-s3[which(s3$FUND_NAME!="AUDIT_YEAR1"),]
table(s3$EXP_OPERATING)
ggplot()+geom_histogram(aes(x=as.numeric(s3$ASSETS)))
table(s3$CASH)
s2<-bind_rows(s2)
which(s2$AUDIT_YEAR=="TOT_PROP_TAX")
table(s2$AUDIT_YEAR)
s2[188,]
?read.delim

s1<-lapply(s1,function(X) rbind.fill(data.frame(matrix(ncol=length(s2)))[-1,],lapply(stringr::str_which(readLines(X),"NAME.+[FULL_NAME|LG_ID]") %>% .[.!=length(readLines(X))],function(Y) read.delim(X,skip=c(Y-1),colClasses="character")) %>% bind_rows()))



s1[[2]]

s1<-bind_rows(s1)
which(s1$LG_ID1==35009)
names(table(s1$FULL_NAME))
colnames(s1)
s1<-s1[,-c(1:88)]
s1$FUND_NAME
unique(which(is.na(s1$FUND_NAME)),which(is.na(s1$FUND_NAME1))) %>% length()
s1<-s1[-which(is.na(s1$FUND_COUNT)),]
nrow(s1)/
table(s1$FULL_NAME1)

) %>% .[-nrow(.),]) %>% bind_rows()

s1[which(s1$AUDIT_YEAR=="67591"),]
head(s1)
hist(log(as.numeric(s1$EXP_OPERATING)))
proj4string(munis)
proj4string(govmap)
munis<-spTransform(munis, CRS("+proj=utm +zone=13 +datum=WGS84"))
govmap<-spTransform(govmap, CRS("+proj=utm +zone=13 +datum=WGS84"))
govmap@data<-govmap@data[,c("LGID","LGTYPEID","LGNAME")]
colnames(munis@data)[1:2]<-c("LGID","LGNAME")
munis@data$LGTYPEID<-"MUNICIPALITY"
govfull<-rbind(munis,govmap)
munis2<-munis
new_IDs = paste0("M", sapply(slot(munis2, "polygons"), function(x) slot(x, "ID")))
for (i in 1:length(slot(munis2, "polygons"))){
  slot(slot(munis2, "polygons")[[i]], "ID") = new_IDs[i]
}
sapply(slot(munis2, "polygons"), function(x) slot(x, "ID"))
munis2$id<-sapply(slot(munis2, "polygons"), function(x) slot(x, "ID"))
govmap$id<-sapply(slot(govmap, "polygons"), function(x) slot(x, "ID"))
row.names(munis2)<-munis2$id
govfull<-maptools::spRbind(munis2,govmap)

table(s1$LGID)
s1$LGID<-s1$LG_ID1
d1s<-join(s1,govfull@data)
head(d1s)
d1s[,7:88] <- sapply(d1s[,7:88],as.numeric)
warnings()

d1s<-lapply(unique(d1s$LGTYPEID),function(X) filter(d1s, LGTYPEID==X) %>% filter(.,AUDIT_YEAR%in%c(2012,2013,2014,2015)))


ops<-lapply(d1s,function(X) scale(as.numeric(as.character(X$EXP_OPERATING))))
colnames(d1s[[1]])
d1s<-lapply(d1s,function(DF){
DF[,7:88] <- sapply(DF[,7:88],as.numeric)
DF})
head(d1s)
?scale
d1break<-d1s

d12<-lapply(d1s,function(X){for(i in 7:35){
  X[,i]<-scale(X[,i])[,1]}
  X})

d1break<-bind_rows(d12)
d1break<-bind_rows(d1break)
d1break<-d1s
d1s$FULL_NAME
d1s$LGTYPEID

largovs<-read.csv(textConnection(RCurl::getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vRObXo6NMIQRcUtmtCKU-kqNPep8ZAlQJ4zfOAwlQyq5DLVB-jphXuCSk44m3tDpYQnZY6jSkvGckef/pub?gid=0&single=true&output=csv")))
d1break$FULL_NAME
which(as.character(d1break$LGID)%in%as.character(largovs$LG.ID.))
d1break<-d1break[,]
d1break$NAME

d1break$LGTYPEID<-as.character(d1break$LGTYPEID)
table(d1break$LGTYPEID)
$
ggplot(filter(d1break,LGTYPEID%in%c(10,11,16,6,20,8,9)))+geom_histogram(aes(x=EXP_OPERATING,fill=LGTYPEID))+facet_wrap(~LGTYPEID,scale="free")
ggplot(filter(d1break,LGTYPEID%in%c(10,11,16,6,20,8,9)))+geom_histogram(aes(x=REV_TOTAL,fill=LGTYPEID))+facet_wrap(~LGTYPEID,scale="free")
ggplot(filter(d1break,LGTYPEID%in%c(10,11,16,6,20,8,9)))+geom_histogram(aes(x=EXP_PRINCIPAL,fill=LGTYPEID))+facet_wrap(~LGTYPEID,scale="free")
d1break$rev
ggplot(filter(d1break,LGTYPEID%in%c(10,11,16,6,20,8,9)))+geom_histogram(aes(x=REV_INTGOVT,fill=LGTYPEID))+facet_wrap(~LGTYPEID,scale="free")
d1break$ASSETS
ggplot(filter(d1break,LGTYPEID%in%c(10,11,16,6,20,8,9)))+geom_histogram(aes(x=ASSETS,fill=LGTYPEID))+facet_wrap(~LGTYPEID,scale="free")
ggplot(filter(d1break,LGTYPEID%in%c(10,11,6,8,12)))+geom_histogram(aes(x=LIABILITIES,fill=LGTYPEID))+facet_wrap(~LGTYPEID,scale="free")
ggplot(filter(d1break,LGTYPEID%in%c(10,11,16,6,20,8,9)))+geom_point(aes(x=AUDIT_YEAR,y=ASSETS,colour=LGTYPEID))+facet_wrap(~LGTYPEID,scale="free")
rm(d12)
rm(d1s)
library(INLA)
inla(ASSETS~LGTYPEID+f(AUDIT_YEAR,model="rw1")+f(LG_ID1,model="iid"),data=d1break) %>% summary()
inla(ASSETS/LIABILITIES~LGTYPEID+f(AUDIT_YEAR,model="rw1")+f(LG_ID1,model="iid"),data=d1break) %>% summary()
head(d1break)
t15<-filter(d1break, AUDIT_YEAR==2015)
govlar<-merge(govmap,t15)
govlar<-subset(govlar, is.na(LGID)==FALSE)
govlar<-spTransform(govlar, CRS("+proj=utm +zone=13 +datum=WGS84"))

rgeos::
coordinates(govlar)[1,]
govlar<-rgeos::gSimplify(govlar, tol = 0.00001)
govlar<-rgeos::gBuffer(govlar, width=0,byid=TRUE)
proj4string(govlar)
govlar@data$id = rownames(govlar@data)
govlar.points = fortify(govlar, region="id")
govlar.df = join(govlar.points, govlar@data, by="id")
govlar.df<-govlar.df[is.na(govlar.df$AUDIT_YEAR)==FALSE,]

ggplot(govlar.df)+geom_polygon(aes(x=long,y=lat,group=group,alpha=as.numeric(ASSETS)))+ggthemes::theme_map()+scale_alpha_continuous(range=c(.1,.2))
ggplot(govlar.df)+geom_polygon(aes(x=long,y=lat,group=group,alpha=as.numeric(LIABILITIES)))+ggthemes::theme_map()+scale_alpha_continuous(range=c(.1,.2))

ggplot(govlar.df)+geom_polygon(aes(x=long,y=lat,group=group,alpha=as.numeric(ASSETS/c(LIABILITIES+.001))))+ggthemes::theme_map()+scale_alpha_continuous(range=c(.1,.3))

ggplot(govlar.df)+geom_polygon(aes(x=long,y=lat,group=group,alpha=as.numeric(ASSETS/c(LIABILITIES+.001))))+ggthemes::theme_map()+scale_alpha_continuous(range=c(.1,.3))



?scale_alpha_continuous
head(govlar.df)
govlar.df$lgty
uout<-rgeos::union(govlar,byid=TRUE)

?gIsValid
?rgeos::gIsValid
rgeos::gIsValid(govlar, byid=TRUE,reason=T)
getSpPPolygonsIDSlots(govlar)
head(govlar@data)
head(t15)
d1bre
ggplot(filter(d1break,LGTYPEID%in%c(10,11,6,8,12)))+geom_smooth(aes(x=EXP_OPERATING,y=AUDIT_YEAR,colour=LGTYPEID))

govmap$LG_ID1<-as.character(govmap$LGID)
gov.mult<-merge(govmap, s1,by=c("LG_ID1"),duplicateGeoms=T)
gov.mult$EXP_OPERATING<-as.numeric(gov.mult$EXP_OPERATING)
gov.mult$EXP_OPERATING_LOG<-log(gov.mult$EXP_OPERATING)
library(ggplot2)
gov.mult<-subset(gov.mult,as.numeric(AUDIT_YEAR)<=2016)
ggplot(gov.mult@data)+geom_point(aes(x=as.numeric(AUDIT_YEAR),y=EXP_OPERATING_LOG))+geom_smooth(aes(x=as.numeric(AUDIT_YEAR),y=EXP_OPERATING_LOG))+facet_wrap(~LGTYPEID,scale="free")


gov.mult$EXP_TOTAL_LOG<-log(as.numeric(gov.mult$EXP_TOTAL))
ggplot(filter(gov.mult@data,as.numeric(AUDIT_YEAR)<=2014))+geom_point(aes(x=as.numeric(AUDIT_YEAR),y=EXP_TOTAL_LOG),position="jitter")+geom_smooth(aes(x=as.numeric(AUDIT_YEAR),y=EXP_TOTAL_LOG))+facet_wrap(~LGTYPEID,scale="free")

head(gov.mult@data$EXP_CAPITAL_OUTLAY)

ggplot(filter(gov.mult@data,as.numeric(AUDIT_YEAR)<=2014))+geom_point(aes(x=as.numeric(AUDIT_YEAR),y=log(as.numeric(as.character(TOTAL_DEBT))+1)),position="jitter")+geom_smooth(aes(x=as.numeric(as.character(AUDIT_YEAR)),y=log(as.numeric(as.character(TOTAL_DEBT))+1)))+facet_wrap(~LGTYPEID,scale="free")


ggplot(filter(gov.mult@data,as.numeric(AUDIT_YEAR)<=2014))+geom_point(aes(x=as.numeric(AUDIT_YEAR),y=log(as.numeric(as.character(EXP_CAPITAL_OUTLAY))+1)),position="jitter")+geom_smooth(aes(x=as.numeric(as.character(AUDIT_YEAR)),y=log(as.numeric(as.character(EXP_CAPITAL_OUTLAY))+1)))+facet_wrap(~LGTYPEID,scale="free")

spplot(subset(gov.mult, as.numeric(AUDIT_YEAR)<=2014), "EXP_TOTAL_LOG")

gov.mult@data$ASSETS_LOG<-log(as.numeric(gov.mult@data$ASSETS))
gov.mult@data$NET_AV<-log(as.numeric(gov.mult@data$NET_AV))
fire<-readOGR("inputs/govmap/GIS_EnvironmentalSHP","WildFire")
geohaz<-readOGR("inputs/govmap/GIS_EnvironmentalSHP","GeologicHazard")
flood<-readOGR("inputs/govmap","FloodDB_WSPoly")
flood<-subset(flood, as.numeric(as.character(FYEAR))>=2011)
flood2<-readOGR("inputs/govmap","Floodplain_Colorado_100yr_Effective_1_28_2016")
head(flood2@data)

head(flood2@data)
proj4string(flood2)

ggplot(gov.mult@data)+geom_point(aes(x=as.numeric(AUDIT_YEAR),y=ASSETS_LOG),position="jitter")+geom_smooth(aes(x=as.numeric(AUDIT_YEAR),y=ASSETS_LOG))+facet_wrap(~LGTYPEID,scale="free")
ggplot(gov.mult@data)+geom_point(aes(x=as.numeric(AUDIT_YEAR),y=NET_AV),position="jitter")+geom_smooth(aes(x=as.numeric(AUDIT_YEAR),y=NET_AV))+facet_wrap(~LGTYPEID,scale="free")

nrow(govfull@data)
gpartd<-merge(govfull@data,filter(s1, AUDIT_YEAR==2015),all.x=T)

govfull@data<-gpartd
rm(partd)
gpartd1<-subset(govfull@data, LGID%in%as.character(largovs$LG.ID.))


gpartd<-rgeos::gSimplify(govfull, tol = 0.00001)
gpartd<-SpatialPolygonsDataFrame(gpartd,data=data.frame("id"=getSpPPolygonsIDSlots(gpartd),row.names=getSpPPolygonsIDSlots(gpartd)))

gpartd@data<-join(gpartd@data,gpartd1)

gpartd<-rgeos::gBuffer(gpartd, width=0,byid=TRUE)
?SpatialPolygonsDataFrame
proj4string(govlar)
gpartd@data$id = rownames(gpartd@data)
govfull$id
govlar.points = fortify(gpartd, region="id")
govlar.df = join(govlar.points, gpartd@data, by="id")
summary(as.numeric(govlar.df$ASSETS))
govlar.df<-govlar.df[is.na(govlar.df$AUDIT_YEAR)==FALSE,]

