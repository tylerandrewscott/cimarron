library(zipcodeR)

#load data and filter for citations of public law 116-136
cf<-list.files("Documents/CARES_FUND/federal_data/",full.names=T)
carescv<-lapply(cf,read.csv,stringsAsFactors=F)
cares_prime_asst<-rbind(carescv[[1]],carescv[[2]])
cares_sub_asst<-rbind(carescv[[3]],carescv[[4]])
library(plyr)
library(dplyr)

filter(cares_prime_asst,cfda_title=="CORONAVIRUS RELIEF FUND") %>% select(cfda_number,assistance_transaction_unique_key,assistance_award_unique_key,recipient_name) %>% unique() %>% write.csv('cares_recode_crvf_index.csv')


filter(cares_prime_asst,cfda_title=="CORONAVIRUS RELIEF FUND") %>% select(cfda_number,assistance_transaction_unique_key,assistance_award_unique_key,recipient_name) %>% unique() %>% write.csv('cares_recode_crvf_index.csv')
#contracts
#cares_prime_cont<-rbind(carescv[[5]],carescv[[6]])
#cares_sub_cont<-rbind(carescv[[7]],carescv[[8]])

#this is crf subgrants
cares_prime_asst$assistance_award_unique_key %>% stringr::str_which(stringr::fixed("ASST_NON_SLT1302_2001"))
cvrf<-read.csv("Documents/CARES_FUND/CRF_details.csv")
cvrfmup<-read.csv("Documents/GitHub/cimarron/building_blocks/cares_recode_crvf_index.csv") %>% select(-assistance_transaction_unique_key)
cvrf<-filter(cvrf, Sub.recipient!="MULTIPLE RECIPIENTS")
cvrf<-cvrf[cvrf$County %>% stringr::str_detect(.,"^CO"),]
cvrf<-left_join(cvrf,cvrfmup)
#note above CVRF subs not in subawards so this code adds labels for them


#filter to include cares cat
cares_sub_asst<-cares_sub_asst %>% filter(stringr::str_detect(prime_award_disaster_emergency_fund_codes,"116\\-136"))
cares_prime_asst<-cares_prime_asst %>% filter(stringr::str_detect(disaster_emergency_fund_codes_for_overall_award,"116\\-136"))

#cares_prime_cont<-cares_prime_cont %>% filter(stringr::str_detect(disaster_emergency_fund_codes_for_overall_award,"116\\-136"))
#cares_sub_cont<-cares_sub_cont %>% filter(stringr::str_detect(disaster_emergency_fund_codes_for_overall_award,"116\\-136"))

#filter to include only grants to governments/states
cares_prime_asst<-filter(cares_prime_asst, business_types_description%in%c("STATE GOVERNMENT","SPECIAL DISTRICT GOVERNMENT","PUBLIC/STATE CONTROLLED INSTITUTION OF HIGHER EDUCATION;STATE GOVERNMENT;OTHER","REGIONAL ORGANIZATION","PUBLIC/STATE CONTROLLED INSTITUTION OF HIGHER EDUCATION;STATE GOVERNMENT","PUBLIC/STATE CONTROLLED INSTITUTION OF HIGHER EDUCATION;NONPROFIT WITHOUT 501C3 IRS STATUS (OTHER THAN AN INSTITUTION OF HIGHER EDUCATION)","PUBLIC/STATE CONTROLLED INSTITUTION OF HIGHER EDUCATION","INDEPENDENT SCHOOL DISTRICT","COUNTY GOVERNMENT","CITY OR TOWNSHIP GOVERNMENT"))

cares_sub_asst<-filter(cares_sub_asst, prime_award_unique_key%in%c(cares_prime_asst$assistance_award_unique_key))

cvrf<-cvrf %>% mutate(granting_source=Prime.recipient,value=Sub.award.amount, name=Sub.recipient,combined_geo=Zip,County=County,City=City,class="sub",prime_award_unique_key=prime_assistance_award_unique_key) %>% select(granting_source,combined_geo,value,County,City,class,name,Award.number,prime_award_unique_key) %>% unique()

#process grants to summarize by where/whom/single value
t1<-cares_prime_asst %>% mutate_if(is.character,toupper) %>% select(assistance_award_unique_key,recipient_name,primary_place_of_performance_county_code,primary_place_of_performance_city_name,primary_place_of_performance_scope,primary_place_of_performance_zip_4,business_types_description,awarding_agency_name,federal_action_obligation,recipient_duns) %>% ddply(.,.(assistance_award_unique_key,primary_place_of_performance_county_code,primary_place_of_performance_city_name,primary_place_of_performance_scope,primary_place_of_performance_zip_4,business_types_description,awarding_agency_name,recipient_duns),summarize, obligation=sum(federal_action_obligation,na.rm=T))

t2<-cares_sub_asst %>% mutate_if(is.character,toupper) %>% select(prime_award_unique_key,subaward_number,subaward_primary_place_of_performance_city_name,subaward_primary_place_of_performance_address_zip_code,subawardee_business_types,prime_award_awarding_agency_name,subaward_amount,prime_award_primary_place_of_performance_scope,prime_awardee_duns)
t2<-rbind(ddply(t2,.(prime_award_unique_key),summarize,sub.award.amount=sum(subaward_amount)),ddply(cvrf,.(prime_award_unique_key),summarize,sub.award.amount=sum(value)) %>% na.omit()) %>% rename(assistance_award_unique_key=prime_award_unique_key)
head(t2)
prime_asst<-left_join(t1,t2)
prime_asst$sub.award.amount<-ifelse(is.na(prime_asst$sub.award.amount),0,prime_asst$sub.award.amount)
prime_asst$minus_sub_award<-prime_asst$obligation-prime_asst$sub.award.amount

#utilize given scope
prime_asst$combined_geo<-ifelse(prime_asst$primary_place_of_performance_scope=="STATE-WIDE","08000",ifelse(prime_asst$primary_place_of_performance_scope=="CITY-WIDE",prime_asst$primary_place_of_performance_city_name,ifelse(prime_asst$primary_place_of_performance_scope=="COUNTY-WIDE", prime_asst$primary_place_of_performance_county_code,prime_asst$primary_place_of_performance_zip_4)))

#this should in practice delete prime double counts but since cvrf isnt in there it doesnt
#prime_asst<-ddply(prime_asst,.(combined_geo,business_types_description,primary_place_of_performance_scope,awarding_agency_name,recipient_duns),summarize,value=sum(minus_sub_award))

#sum by descriptions
prime_asst<-ddply(prime_asst,.(combined_geo,business_types_description,primary_place_of_performance_scope,awarding_agency_name,recipient_duns
),summarize,value=sum(minus_sub_award))
prime_asst<-prime_asst %>% rename(awardee_type=business_types_description,granting_source=awarding_agency_name,scope=primary_place_of_performance_scope)
t2<-cares_sub_asst %>% mutate_if(is.character,toupper) %>% select(prime_awardee_name,prime_award_unique_key,subaward_number,subaward_primary_place_of_performance_city_name,subaward_primary_place_of_performance_address_zip_code,subawardee_business_types,subaward_amount,prime_awardee_duns)
sub_asst<-ddply(t2,.(subaward_primary_place_of_performance_address_zip_code,subawardee_business_types,prime_awardee_name,prime_awardee_duns),summarize,value=sum(subaward_amount))
sub_asst<-sub_asst %>% rename(awardee_type=subawardee_business_types,granting_source=prime_awardee_name,combined_geo=subaward_primary_place_of_performance_address_zip_code)
sub_asst$scope<-"ZIPCODE"
sub_asst$class<-"sub"
prime_asst$class<-"prime"



#incorpoarte CVRF data
cvrf<-cvrf %>% select(-prime_award_unique_key)
cvrf<-cvrf[cvrf$County %>% stringr::str_detect(.,"^CO"),]
cvrf$scope<-NA
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"dist")]<-"LOCAL"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"[0-9]+ [0-9]+")]<-"SCHOOLDIST"
cvrf$scope[tolower(cvrf$name) %>% stringr::str_detect(.,"county")]<-"COUNTY"
cvrf$scope[tolower(cvrf$name) %>% stringr::str_detect(.,"cnty")]<-"COUNTY"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"school")]<-"SCHOOLDIST"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"sd")]<-"SCHOOLDIST"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"re[0-9]+")]<-"SCHOOLDIST"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"re[0-9]")]<-"SCHOOLDIST"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"re.[0-9]")]<-"SCHOOLDIST"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"r[0-9]+")]<-"SCHOOLDIST"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"rj.[0-9]+")]<-"SCHOOLDIST"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"r.[0-9]")]<-"SCHOOLDIST"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"j[0-9]")]<-"SCHOOLDIST"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"[0-9]j")]<-"SCHOOLDIST"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"jt")]<-"SCHOOLDIST"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.," j ")]<-"SCHOOLDIST"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"rd[0-9]+")]<-"SCHOOLDIST"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"boces")]<-"edservicedistrict"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"elementary")]<-"SCHOOLDIST"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"[0-9]+ \\- ")]<-"SCHOOLDIST"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"authority")]<-"LOCAL"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"rescue")]<-"LOCAL"
cvrf$scope[cvrf$name %>% tolower %>% stringr::str_detect(.,"metropolitan district")]<-"LOCAL"
cvrf$scope[tolower(cvrf$name) %>% stringr::str_detect(.,"city")]<-"CITY"
cvrf$scope[tolower(cvrf$name) %>% stringr::str_detect(.,"town")]<-"CITY"
cvrf$scope[tolower(cvrf$name) %>% stringr::str_detect(.,"park \\&")]<-"LOCAL"
cvrf$scope[tolower(cvrf$name) %>% stringr::str_detect(.,"parks \\&")]<-"LOCAL"
cvrf$scope[tolower(cvrf$name) %>% stringr::str_detect(.,"public health")]<-"COUNTY"
cvrf$scope[tolower(cvrf$name) %>% stringr::str_detect(.,"health department")]<-"COUNTY"
cvrf$scope[tolower(cvrf$name) %>% stringr::str_detect(.,"inc|llc|ltd|lp")]<-"COMPANY"
cvrf$scope[is.na(cvrf$scope)]<-"CVRF OTHER"
cvrf$scope_recode<-NA
cvrf$scope_recode[cvrf$scope=="COUNTY"]<-"COUNTY"
cvrf$scope_recode[cvrf$scope=="CITY"]<-"CITY"
cvrf$scope_recode[cvrf$scope=="LOCAL"]<-"ZIPCODE"
cvrf$scope_recode[cvrf$scope=="SCHOOLDIST"]<-"SCHOOLDIST"
cvrf$scope_recode[cvrf$scope=="edservicedistrict"]<-"COUNTY"
cvrf$scope_recode[cvrf$scope%in%c("COMPANY","CVRF OTHER")]<-"ZIPCODE"
cvrf$scope_recode[cvrf$name%in%c("COLORADO HOUSING AND FINANCE AUTHORITY")]<-"STATE-WIDE"
cvrf$scope_recode[cvrf$name%in%c("MILE HIGH UNITED WAY, INC.")]<-"COUNTY"



cvrf<-cvrf %>% select(combined_geo, granting_source,value,scope,class,scope_recode) %>% mutate(awardee_type_limit=scope)


#merge all assistance rows together, recode scopes
assistance<-rbind.fill(sub_asst,prime_asst)

assistance$scope<- assistance$scope %>% plyr::revalue(.,c("SINGLE ZIP CODE"="ZIPCODE","COUNTY-WIDE"="COUNTY","CITY-WIDE"="CITY"))
assistance$scope_recode<- assistance$scope
assistance$scope_recode[stringr::str_detect(assistance$awardee_type,'MUNICIPALITY')]<-"CITY"
assistance$awardee_type %>% table()
assistance$scope_recode[stringr::str_detect(assistance$awardee_type,'CITY')]<-"CITY"
assistance$scope_recode[stringr::str_detect(assistance$awardee_type,'TOWN')]<-"CITY"
assistance$scope_recode[stringr::str_detect(assistance$awardee_type,'COUNTY')]<-"COUNTY"
assistance$scope_recode[stringr::str_detect(assistance$awardee_type,'COUNCIL')]<-"COG"
assistance$scope_recode[stringr::str_detect(assistance$awardee_type,'SCHOOL DISTRICT')]<-"SCHOOLDIST"
assistance$awardee_type_limit<-ifelse(stringr::str_detect(assistance$awardee_type,"ORGANIZATION"),"ORGANIZATION",ifelse(stringr::str_detect(assistance$awardee_type, "GOVERNMENT"),"GOVERNMENT","OTHER"))
#merge cvrf and assistance
assistance<-rbind.fill(assistance,cvrf)

# summarize
assistance2<-assistance %>% select(combined_geo,granting_source,prime_awardee_duns,recipient_duns,value,scope_recode,class,awardee_type_limit) %>% ddply(.,.(combined_geo,granting_source,value,scope_recode,class,awardee_type_limit),value=sum(value))

#this section identifies relevant geographies based on grant descriptions

#first, identify county grants as counties
counties<-tigris::counties(state="CO")
counties1<-assistance2 %>% filter(scope_recode=="COUNTY")
county_zip<-counties1[nchar(counties1$combined_geo)>=5,]
county_zip$combined_geo<-substr(county_zip$combined_geo,1,5)
countymatch<-readxl::read_excel("Documents/CARES_FUND/ZIP_COUNTY_062020.xlsx")
county_zip$combined_geo<-countymatch$COUNTY[match(county_zip$combined_geo,countymatch$ZIP)]
county_fp<-counties1[nchar(counties1$combined_geo)<5,]
county_fp$combined_geo<-stringr::str_pad(county_fp$combined_geo,3,"left","0")
county_fp$combined_geo<-paste0("08",county_fp$combined_geo)
counties1<-rbind(county_fp,county_zip)

#next, identify cities as cities. Note, denver and jeffco have some wrong observations so those are fixed "31", "59". Some cities have names some have zipcodes so this puts both sets to colorado city names from usps crosswalk files and colorado published city shapefiles.

cities1<-assistance2 %>% filter(scope_recode=="CITY")
cities1$combined_geo[cities1$combined_geo=="31"]<-"08031"
cities1$combined_geo[cities1$combined_geo=="59"]<-"08059"

#cities1<-join(cities1 %>% mutate(zipcode=substr(combined_geo,1,5)),zipcodeR::geocode_zip(substr(cities1$combined_geo,1,5)))

cities1$city1<-zip_code_db$major_city[match(substr(stringr::str_trim(cities1$combined_geo),1,5),zip_code_db$zipcode)]
munishape<-read_sf("Documents/CARES_FUND/data_correlates/muni_shapes")
cities1$city<-munishape$city[match(cities1$city1,munishape$first_city)]
cities1$combined_geo[is.na(cities1$city)==F]<-cities1$city[is.na(cities1$city)==F]
cities2<-filter(cities1,is.na(city))
cities2$city<-munishape$city[match(tolower(cities2$combined_geo),tolower(munishape$first_city))]
cities1<-rbind(filter(cities1,is.na(city)==F),filter(cities2, is.na(city)==F) %>% mutate(combined_geo=city))
cities2<-filter(cities2, is.na(city))

#where cities a) do not exist in data, b) are identified as county, or c) identify as a statewide grant, we save those for those relevant files

CITYSAVEFORZIP<-cities2 %>% filter(combined_geo %>% stringr::str_detect("^80|^81")) %>% mutate(scope_recode="ZIPCODE") %>% select(-city1,-city)
CITYSAVEFORCOUNTY <-cities2 %>% filter(combined_geo %>% stringr::str_detect("^80|^81|08000")==F)
CITYSAVEFORstate <-cities2 %>% filter(combined_geo %>% stringr::str_detect("08000")) %>% mutate(scope_recode="STATE-WIDE")

#add cities with county ids to county file, removed from city file above.
counties1<-rbind(counties1,CITYSAVEFORCOUNTY %>% select(-city,-city1))
counties_shp<-tigris::counties(state="CO")
counties1$geometry<-counties_shp$geometry[match(counties1$combined_geo,counties_shp$GEOID)]
cities1$geometry<-munishape$geometry[match(cities1$combined_geo,munishape$city)]

#school districts
schoold<-filter(assistance2, scope_recode=="SCHOOLDIST")
sdzip<-readxl::read_excel("Documents/CARES_FUND/grf21_lea_zcta5ce20.xlsx")
schoold$combined_geo<-sdzip$LEAID[match(schoold$combined_geo %>% substr(.,1,5),sdzip$ZCTA5CE20)]
sdshapes<-tigris::school_districts(state="CO")
schoold$geometry<-sdshapes$geometry[match(schoold$combined_geo, sdshapes$GEOID)]

#metro govs
mpo<-read_sf("Documents/CARES_FUND/data_correlates/mpo/Transportation_Planning_Regions.shp")
mpo_grants<-filter(assistance2,scope_recode=="COG")
zipcodes<-tigris::zctas(cb = F, starts_with = c("80","81"),class="sf")
zi<-st_intersection(st_transform(zipcodes[match(substr(mpo_grants$combined_geo,1,5),zipcodes$ZCTA5CE10),],st_crs(mpo)),mpo)
zi$area<-zi %>% st_area()
zi<-ddply(zi, .(ZCTA5CE10),summarize,NAME=NAME[which.max(area)])    
mpo_grants$combined_geo<-zi$NAME[match(substr(mpo_grants$combined_geo,1,5),zi$ZCTA5CE10)]
mpo_grants$geometry<-mpo$geometry[match(mpo_grants$combined_geo,mpo$NAME)]

#Zipcodes
zi1<-filter(assistance2,scope_recode=="ZIPCODE") %>% rbind(CITYSAVEFORZIP)
zi1$combined_geo<-zipcodes$ZCTA5CE10[match(substr(zi1$combined_geo,1,5),zipcodes$ZCTA5CE10)]
zi1$geometry<-zipcodes$geometry[match(zi1$combined_geo,zipcodes$ZCTA5CE10)]
zi1$combined_geo<-zipcodes$GEOID10[match(substr(zi1$combined_geo,1,5),zipcodes$ZCTA5CE10)]

#State
stateg<-filter(assistance2,scope_recode=="STATE-WIDE") %>% rbind(CITYSAVEFORstate %>% select(-city,-city1))
state<-tigris::states()
stateg$geometry<-state %>% filter(NAME=="Colorado") %>% .$geometry


#merge and simplify geometries to a Nad83
grantfiles<-list("state"=stateg,"counties"=counties1,"zipcode"=zi1,"schooldist"=schoold,"city"=cities1 %>% select(-city,-city1),"cog"=mpo_grants)
grantfiles[[1]]<-st_sf(grantfiles[[1]])
grantfiles[[2]]<-st_sf(grantfiles[[2]])
grantfiles[[3]]<-st_sf(grantfiles[[3]])
grantfiles[[2]]<-st_transform(grantfiles[[2]],st_crs(grantfiles[[3]]))
grantfiles[[4]]<-st_sf(grantfiles[[4]])
grantfiles[[4]]<-st_transform(grantfiles[[4]],st_crs(grantfiles[[3]]))
grantfiles[[5]]<-st_sf(grantfiles[[5]])
grantfiles[[5]]<-st_transform(grantfiles[[5]],st_crs(grantfiles[[3]]))
grantfiles[[6]]<-st_sf(grantfiles[[6]])
grantfiles[[6]]<-st_transform(grantfiles[[6]],st_crs(grantfiles[[3]]))

#for each geometry, summarize files
onefile<-do.call(rbind,grantfiles)
onefile.simple<-ddply(onefile %>% as.data.frame(),.(combined_geo, granting_source,scope_recode,class),summarize,value=sum(value))
onefile.shapes<-unique(onefile %>% select(combined_geo))

#now, find relevant tracts for the grant
tracts<-tigris::tracts("CO")
tractic<-st_intersects(st_make_valid(onefile.shapes),st_make_valid(tracts))
onefile.shapes$tracts<-lapply(tractic,function(X) tracts$GEOID[X])

#send grant files to results
onefile.simple %>% saveRDS("simple_grant_values_by_geometry.rds")
onefile.shapes %>% saveRDS("shapefiles_and_tracts_matching_combined_geos.rds")
onefile %>% saveRDS("complex_grant_values_by_geometry.rds")



#make descriptive figures
prime<-filter(onefile.simple %>% select(scope_recode,class,value,granting_source),class=="prime")
prime<-ddply(prime,  .(granting_source,scope_recode),summarize,prime=sum(value))
sub<-ddply(filter(onefile.simple %>% select(scope_recode,class,value,granting_source),class=="sub"),  .(granting_source,scope_recode),summarize,sub=sum(value))
subgrantsummary<-sub %>% group_by(granting_source,scope_recode) %>% summarize(subgrants=sum(sub))
subgrantsummary$granting_source[subgrantsummary$subgrants<1000000]<-"OTHER"
subgrantsummary<-subgrantsummary %>% arrange(subgrants)


subgrantsummary %>% ggplot(.)+geom_bar(aes(x=reorder(granting_source,-subgrants),y=subgrants,fill=scope_recode),stat="identity")+coord_flip()+theme_minimal()+ylab("subgrant values $")+xlab('Grantee giving more than $10m')+ggthemes::scale_fill_tableau(name="target scope (prime award)")+xlab("prime award recipient")+ylab("$ to target scope (sub award)")+theme(legend.position = c(.5,.7),legend.background = element_rect(fill="white"))

prime %>% ggplot(.)+geom_bar(aes(x=reorder(granting_source,-prime),y=prime,fill=scope_recode),stat="identity")+coord_flip()+theme_minimal()+ylab("prime values $")+ggthemes::scale_fill_tableau(name="target scope (prime award)")+xlab("prime granting agency")+ylab("$ to target scope (prime award)")+theme(legend.position = c(.5,.7),legend.background = element_rect(fill="white"))


