

# load packages
library(tidyverse)
library(tidymodels)
library(kableExtra)
library(xgboost)
library(vip)
library(doMC)

registerDoMC(cores = 4)

set.seed(13)

load("results/knn_info.rda")

# tuning
knn_tuned <- knn_workflows %>% 
  workflow_map(
    resamples = articles_folds,
    verbose = TRUE, 
    control = control_grid(
      save_pred = TRUE,
      verbose = TRUE,
      save_workflow = TRUE
    )
  )

save(knn_tuned, file = "results/knn_tuned.rda")
