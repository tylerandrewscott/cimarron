test = readRDS('~/Downloads/send_to_tyler.rds')

library(tigris)
library(tidycensus)

counties <- tigris::counties(state = 'CO',class = 'sf',cb = T)
places <- tigris::places(state = 'CO',class = 'sf',cb = T)

key = source('../census_key')
census_api_key(key$value)


tidycensus::get_acs(state = 'CO',geography = 'county',variables = "B19013_001")

county_pop <- get_estimates(geography = "county", 
                             product = "population", 
                             breakdown_labels = TRUE, 
                             state = "CO")%>% filter(variable=='POP')

state_pop <- get_acs(geography = 'state',state = 'CO',variables ='B01001_001',year = 2019)
county_pop <- get_acs(geography = 'county',state = 'CO',variables ='B01001_001',year = 2019)
zcta_pop <- get_acs(geography = 'zcta',state = 'CO',variables ='B01001_001',year = 2019)
pop_list <- list('state' = state_pop,'county' = county_pop,'zcta' = zcta_pop)

co_zcta <- tigris::zctas(class = 'sf',state = 'CO',year = 2010)




test[1:2,1:6]




ggplot() + geom_sf(data = co_zcta)

test$scope_recode

v19 <- load_variables(2019, "acs5", cache = TRUE)
View(v19)

get_acs(geography = "zcta", 
              product = "population", 
              breakdown_labels = TRUE, 
              state = "CO")%>% filter(variable=='POP')



state_pop 


county_pop

get_estimates()
?places
counties$NAME
install.packages('tigris')


ggplot() + geom_sf(data = test)




head(test)
str(test)
test$class
head(test)
test[,1:6]
names(test)
library(tidyverse)
library(sf)
ggplot() + geom_sf(data = test[148,])

head(test$granting_source)
plot(test[148,])
test[100:102,]
