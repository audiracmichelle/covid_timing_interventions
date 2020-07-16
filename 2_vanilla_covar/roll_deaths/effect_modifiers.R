rmarkdown::render(
  "_summary.Rmd", 
  params = list(model_file = "./_model.rds", covar = "popdensity"),
  output_file = "popdensity_summary.html"
)

rmarkdown::render(
  "_summary.Rmd", 
  params = list(model_file = "./_model.rds", covar = "percent_poverty"),
  output_file = "pct_pov_summary.html"
)

rmarkdown::render(
  "_intervention.Rmd", 
  params = list(model_file = "./_model.rds", covar = "intervention"),
  output_file = "intervention_summary.html"
)
