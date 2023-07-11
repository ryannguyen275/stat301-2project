
# load packages
library(tidyverse)
library(tidymodels)
library(kableExtra)
library(xgboost)
library(vip)
library(doMC)

registerDoMC(cores = 4)

set.seed(13)

load("results/lm_info.rda")

# tuning
lm_tuned <- lm_workflows %>% 
  workflow_map(
    fn = "fit_resamples",
    resamples = articles_folds,
    verbose = TRUE, 
    control = control_grid(
      save_pred = TRUE,
      save_workflow = TRUE
    )
  )

save(lm_tuned, file = "results/lm_tuned.rda")