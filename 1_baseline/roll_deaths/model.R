library(tidyverse)
library(magrittr)
library(feather)
library(rstanarm)

#### #### 
## model

## Read county_train
county_train <- read_feather("../../county_train.feather") %>%
  mutate(t=days_since_thresh)

## define y
county_train$y <- county_train$roll_deaths
#dim(county_train)
county_train %<>% 
  filter(!is.na(roll_deaths))
#dim(county_train)

## Train model
options(mc.cores=2)
model = stan_glmer.nb(
  y ~
    # 1 baseline
    poly(t, 2) +
    (poly(t, 2) | fips)
  ,
  offset = log(pop),
  data=county_train,
  algorithm="mean",
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
