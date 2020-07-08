library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)

## Read county_train
county_train <- read_feather("../../county_train_cases.feather")

## define y
county_train %<>% 
  mutate(y = roll_cases) %>% 
  filter(!is.na(y))

## Train model
options(mc.cores=2)
model = stan_glmer.nb(
  y ~
    # random effects
    (poly(days_since_thresh, 2) | fips) +
    # 2 interaction
    t:intervention + t2:intervention
  ,
  offset = log(pop),
  data=county_train,
  algorithm="meanfield",
  iter = 50000,
  adapt_iter = 2500,
  QR=TRUE
)

saveRDS(model, paste("./model.rds", sep = ""))

#### #### 
## county_fit

county_fit <- model %>%
  posterior_predict(county_train, draws = 500)

saveRDS(county_fit, "./county_fit.rds")
