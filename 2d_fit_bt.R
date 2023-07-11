
# load packages
library(tidyverse)
library(tidymodels)
library(kableExtra)
library(xgboost)
library(vip)
library(doMC)

registerDoMC(cores = 4)

set.seed(13)

load("results/bt_info.rda")


# tuning
bt_tuned <- bt_workflows %>% 
  workflow_map(
    resamples = articles_folds,
    verbose = TRUE, 
    control = control_grid(
      save_pred = TRUE,
      verbose = TRUE,
      save_workflow = TRUE
    )
  )

save(bt_tuned, file = "results/bt_tuned.rda")
