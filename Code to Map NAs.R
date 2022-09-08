library(tidycensus)
library(tidyverse)

setwd('~/Desktop/NA mappings')


#reading and fixing up my data
mydata <- read_csv('na.binary.csv')
mydata$CensusTract <- as.character(mydata$CensusTract)


#this is my api key. No idea why this didnt matter
census_api_key('7301d35d01744887fb3295152a3b3d821af9fdbf', install = T)


#getting the mapping data from census 
got_poly <- get_decennial(geography = "tract", year = 2020, state = "12", variables = "P1_001N", 
                          geometry = TRUE, key = '7301d35d01744887fb3295152a3b3d821af9fdbf') %>% 
  select(GEOID, geometry)


#graph for HPI 
left_join(got_poly, mydata, by=c("GEOID"="CensusTract")) %>%
  ggplot2::ggplot(aes(fill = factor(HPI))) + 
  geom_sf()+
  coord_sf(xlim=c(-80.5, -80), ylim=c(26, 27))


