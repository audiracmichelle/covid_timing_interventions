library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)

## Read county_train
county_train <- read_feather("../county_train.feather")

## Train model
options(mc.cores=4)
county_model = stan_glmer.nb(
  deaths ~
    poly(days_since_thresh, 2) * (log(popdensity) + log(percent_poverty)) +
    poly(days_since_thresh,2):(intervention + intervention:(log(popdensity) + log(percent_poverty))) +
    (poly(days_since_thresh, 2) | fips) + wday,
  offset = log(pop),
  county_train=county_train,
  algorithm="meanfield",
  iter = 50000,
  adapt_iter = 2000,
  QR=TRUE)
# county_model = stan_glmer.nb(
#   deaths ~
#     poly(days_since_thresh, 2) * (log(popdensity) + log(household_income)) +
#     poly(days_since_thresh,2):(intervention + intervention:(log(popdensity) + log(household_income))) +
#     (poly(days_since_thresh, 2) | fips) + wday,
#   offset = log(pop),
#   county_train=county_train,
#   iter = 2000,
#   chains=4,
#   warmup=1900)

saveRDS(county_model, paste("./_model.rds", sep = ""))