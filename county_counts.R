library(tidyverse)
library(magrittr)
library(feather)

## Read in NYTimes Case/Death count data
county_counts <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')

county_counts %<>% 
  rename(cum_deaths = deaths, 
         cum_cases = cases)

## Assign fips to New York City 

#county_counts[county_counts$state == "New York", ] %>%  View
county_counts$fips[which(county_counts$state == "New York" & 
                           county_counts$county == "New York City")] <- "36xxx"

## Remove rows with fips NA

#county_counts[is.na(county_counts$fips), ] %>%  View
county_counts <- county_counts[!is.na(county_counts$fips), ]

## Create daily case and death count columns

county_counts %<>% 
  arrange(fips, state, county, date) %>% 
  group_by(fips) %>% 
  mutate(cases = diff(c(0,cum_cases))) %>%
  mutate(deaths = diff(c(0,cum_deaths))) %>%
  ungroup()

write_feather(county_counts, "./county_counts.feather")
