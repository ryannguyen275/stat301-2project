

# load packages
library(tidyverse)
library(tidymodels)
library(kableExtra)
library(xgboost)
library(vip)
library(doMC)

registerDoMC(cores = 4)

set.seed(13)

load("results/rf_info.rda")


# tuning
rf_tuned <- rf_workflows %>% 
  workflow_map(
    resamples = articles_folds,
    verbose = TRUE, 
    control = control_grid(
      save_pred = TRUE,
      verbose = TRUE,
      save_workflow = TRUE
    )
  )

save(rf_tuned, file = "results/rf_tuned.rda")

