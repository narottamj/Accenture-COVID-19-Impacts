setwd('~/Desktop/Accenture/Mapping Variables')
library(tidycensus)
library(tidyverse)

mydata <- read_csv('02.17.2022.model.df.csv')

cn <- as.data.frame(colnames(mydata))

#getting the mapping data from census 
got_poly <- get_decennial(geography = "tract", year = 2020, state = "12", variables = "P1_001N", 
                          geometry = TRUE, key = '7301d35d01744887fb3295152a3b3d821af9fdbf') %>% 
  select(GEOID, geometry)

#graph for HPI 
left_join(got_poly, mydata, by=c("GEOID"="CensusTract")) %>%
  ggplot2::ggplot(aes(fill = (TotalPopulation))) + 
  geom_sf(size = 0) + 
  scale_fill_gradient()

