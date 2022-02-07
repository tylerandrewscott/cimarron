
infra<-lapply(list.files("Downloads/infrafun",full.names=T),read_sf)
names(infra)<-list.files("Downloads/infrafun")
infralist<-lapply(infra,function(X) st_intersects(tracts,st_make_valid(st_transform(X,st_crs(tracts)))))
infralist2<-lapply(infralist,function(X) sapply(X,length))
infrastructure<-data.frame("GEOID"=tracts$GEOID,"COLLEGES"=infralist2[[1]],"DODSITE"=infralist2[[2]],"BANKS"=infralist2[[3]],"FIRESTATION"=infralist2[[4]],"FORTUNE500"=infralist2[[5]],"STATEGOV"=infralist2[[6]],"NURSING"=infralist2[[7]],"PRISON"=infralist2[[8]],"PUBSCHOOLS"=infralist2[[9]],"RUNWAYS"=infralist2[[10]])
write.csv(infrastructure,"Documents/GitHub/Cimarron/building_blocks/infrastructure.csv")
