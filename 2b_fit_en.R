

# load packages
library(tidyverse)
library(tidymodels)
library(kableExtra)
library(xgboost)
library(vip)
library(doMC)

registerDoMC(cores = 4)

set.seed(13)

load("results/en_info.rda")


# tuning
en_tuned <- en_workflows %>% 
  workflow_map(
    resamples = articles_folds,
    verbose = TRUE, 
    control = control_grid(
      save_pred = TRUE,
      parallel_over = "everything",
      save_workflow = TRUE
    )
  )

save(en_tuned, file = "results/en_tuned.rda")

