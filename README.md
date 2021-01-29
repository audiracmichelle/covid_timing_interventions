# covid_timing_interventions

Data extraction, preparation and model code for Audirac M., Tec M., Meyers L.A., Fox S. and
Zigler C. ["How Timing of Stay-at-home Orders and Mobility Reductions
Impacted First-Wave COVID-19 Deaths in US Counties"](https://www.medrxiv.org/content/10.1101/2020.11.24.20238055v1#:~:text=We%20find%20that%20population%20mobility,would%20have%20added%2016%2C149%20(95%25)

## Data extraction and preparation

Code for data extraction and preparation is in the main folder. Run `./county_train_.R` to obtain the `*.feather` files that contain the inputs of the model.

Processed mobility data is stored in safegr_raw.feather

## Docker container to run rstanarm

```
cd $(pwd)
docker build -t covid_timing_interventions .
docker run --rm -e PASSWORD=rstan -p 8787:8787 -v $(pwd):/home/rstudio/kitematic/ covid_timing_interventions
```

## Model code

To train the model use:

+ For stay-home orders `./4b_mcmc/roll_deaths/model.R`
+ For mobility decrease `./4b_mcmc/roll_deaths_decrease/model.R`

To generate posterior predictions use:

+ For stay-home orders `./4b_mcmc/roll_deaths/days_btwn_summary.R`
+ For mobility decrease `./4b_mcmc/roll_deaths_decrease/days_btwn_summary.R`

