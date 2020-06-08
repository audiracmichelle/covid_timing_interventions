library(tidyverse)
library(magrittr)
library(feather)
library(lubridate)

## Read county_counts and county_desc
county_counts <- read_feather("./county_counts.feather")
county_desc <- read_feather("./county_desc.feather")

## Join jhu with NYTimes
#length(unique(county_counts$fips))
#length(unique(county_desc$fips))

#idx <- !county_desc$fips %in% county_counts$fips
#county_desc[idx, c("state_code", "county_name", "fips")] %>%  view()

#dim(county_counts)
#dim(county_desc)
county_data <- county_counts %>% 
  filter(fips != "36xxx") %>%
  full_join(county_desc, by = "fips")
#dim(county_data)

## New York City counts
diff_fips <- setdiff(county_desc$fips, county_counts$fips)

ny_data <- county_desc %>% 
  filter(fips %in% diff_fips, state_code == "NY")

ny_counts <- county_counts %>% 
  filter(fips == "36xxx")

for(r in 1:nrow(ny_data)) {
  xx_counts <- ny_counts
  xx_counts$county <- ny_data$county_name[r]
  xx_counts$fips <- ny_data$fips[r]
  xx_counts %<>% 
    mutate(cum_cases = 
             round(cum_cases * ny_data$popdensity[r] / sum(ny_data$popdensity)), 
           cum_deaths = 
             round(cum_deaths * ny_data$popdensity[r] / sum(ny_data$popdensity)), 
           cases = round(cases * ny_data$popdensity[r] / sum(ny_data$popdensity)),
           deaths = round(deaths * ny_data$popdensity[r] / sum(ny_data$popdensity))) %>% 
    left_join(ny_data)
  county_data <- rbind(county_data, xx_counts)
}
#dim(county_data)

# county_data %>%
#   filter(state == "New York",
#          grepl("county", county)) %>% view

## Create per capita and per sq deaths and cases
county_data <- county_data  %>% 
  arrange(fips, date) %>%
  mutate(deaths_per_cap = deaths / pop,
         cum_deaths_per_cap = cum_deaths / pop,
         deaths_per_sq_mi = deaths / sq_mi,
         cum_deaths_per_sq_mi = cum_deaths / sq_mi, 
         cases_per_cap = cases / pop,
         cum_cases_per_cap = cum_cases / pop,
         cases_per_sq_mi = cases / sq_mi,
         cum_cases_per_sq_mi = cum_cases / sq_mi)

#### ####
## Deaths threshold

# when did deaths exceed 3 per 10 million?
county_data$threshold_day <- county_data$date
county_data$threshold_day[county_data$cum_deaths_per_cap < 3/1e7] <- today()

county_data %<>%
  group_by(fips) %>%
  mutate(threshold_day = min(threshold_day, na.rm = TRUE)) %>% 
  ungroup()

# county_data %>%
#   filter(state == "New York",
#          grepl("county", county)) %>% 
#   select("date","county","state","fips","cum_cases","cum_deaths", "threshold_day") %>% view
 
# use the threshold_day variable to get the time counter 
county_data %<>%
  mutate(days_since_thresh = date - threshold_day)

#### ####
## Cases threshold

# when did cases exceed 30 per 10 million?
county_data$thresh_cases <- county_data$date
county_data$thresh_cases[county_data$cum_cases_per_cap < 30/1e7] <- today()

county_data %<>%
  group_by(fips) %>%
  mutate(thresh_cases = min(thresh_cases)) %>% 
  ungroup()

# use the thresh_cases variable to get the time counter 
county_data %<>%
  mutate(days_since_thresh_cases = date - thresh_cases)

## Intervention occurrence
county_data %<>% 
  mutate(days_after_gt500 = date - gt500, 
         days_after_gt50 = date - gt50, 
         days_after_stayhome = date - stayhome, 
         days_after_schools = date - schools, 
         days_after_restaurants = date - restaurants, 
         days_after_entertainment = date - entertainment)

## Include weekday

county_data %<>% 
  mutate(wday = wday(date, week_start = 1))

#length(unique(county_data$fips))

# county_data %>%
#   filter(state == "Texas",
#          county == "Travis") %>%
#   View()

write_feather(county_data, "./county_data.feather")
