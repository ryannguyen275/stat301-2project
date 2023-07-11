
# load packages
library(tidyverse)
library(tidymodels)
library(janitor)
library(kableExtra)
library(xgboost)
library(vip)
library(doMC)

registerDoMC(cores = 4)

set.seed(13)

################ LOAD DATA ############################

# LOAD-IN DATA
articles <- read_csv("data/articles.csv") %>% 
  janitor::clean_names()

load("data/articles_split.rda")

################# CREATING RECIPES ############################

## RECIPE 1: Kitchen Sink
articles_recipe1 <- recipe(log_shares ~., data = articles_train) %>% 
  # removing shares since it is linear to log_shares
  # removing data_channel & day_of_week since they are already accounted for
  step_rm(shares, day_of_week, data_channel) %>% 
  step_normalize(all_predictors()) %>% 
  step_nzv(all_predictors())

prep(articles_recipe1) %>% 
  bake(new_data = NULL)

# RECIPE 2: Only Lead Predictors
articles_recipe2 <- recipe(log_shares ~ rate_negative_words + rate_positive_words, 
                           data = articles_train) %>%
  step_normalize(all_predictors()) %>% 
  step_nzv(all_predictors())

prep(articles_recipe2) %>% 
  bake(new_data = NULL)

# RECIPE 3: Adding Interaction Terms
articles_recipe3 <- recipe(log_shares ~., data = articles_train) %>% 
  step_rm(shares, data_channel, day_of_week) %>%
  step_interact(~ num_keywords: starts_with("data_channel")) %>% 
  step_normalize(all_predictors()) %>% 
  step_nzv(all_predictors())

prep(articles_recipe3) %>% 
  bake(new_data = NULL)

# RECIPE 4: Other Lead Predictors + Interaction Terms
articles_recipe4 <- recipe(log_shares ~ avg_positive_polarity + avg_negative_polarity 
                           + global_subjectivity + title_subjectivity, 
                           data = articles_train) %>%
  step_interact(~ global_subjectivity: title_subjectivity) %>% 
  step_normalize(all_predictors()) %>% 
  step_nzv(all_predictors())

prep(articles_recipe4) %>% 
  bake(new_data = NULL)


############ SET ENGINES ###################################

### Null Model
null_mod <- null_model() %>% 
  set_engine("parsnip") %>% 
  set_mode("regression")

null_workflow <- workflow() %>% 
  add_model(null_mod) %>% 
  add_recipe(articles_recipe1)

null_fit <- fit_resamples(null_workflow, 
                          resamples = articles_folds,
                          control = control_resamples(save_pred = TRUE))

save(null_fit, file = "results/null_fit.rda")


### Linear Regression
lm_spec <- linear_reg() %>% 
  set_engine("glm")

### Elastic Net
en_spec <- linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

en_param <- extract_parameter_set_dials(en_spec)

en_grid <- grid_regular(en_param, levels = 5)

### Random Forest
rf_spec <- rand_forest(min_n = tune(),
                       mtry = tune()) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")

rf_param <- extract_parameter_set_dials(rf_spec) %>% 
  update(mtry = mtry(range = c(1,10)))

rf_grid <- grid_regular(rf_param, levels = 5)

### Boosted Tree
bt_spec <- bt_model <- boost_tree(mode = "regression",
                                  min_n = tune(),
                                  mtry = tune(), 
                                  learn_rate = tune()) %>% 
  set_engine("xgboost", importance = "impurity")

bt_param <- extract_parameter_set_dials(bt_spec) %>% 
  update(mtry = mtry(range = c(1,10)))

bt_grid <- grid_regular(bt_param, levels = 5)

### K-Nearest Neighbors
knn_spec <- nearest_neighbor(mode = "regression",
                             neighbors = tune()) %>% 
  set_engine("kknn")

knn_param <- extract_parameter_set_dials(knn_spec) %>% 
  update(neighbors = neighbors(range = c(1,50)))


knn_grid <- grid_regular(knn_param, levels = 5)

##################### WORKFLOWS ###############################

# Linear Regression
lm_workflows <- workflow_set(
  preproc = list(recipe1 = articles_recipe1, recipe2 = articles_recipe2, recipe3 = articles_recipe3, recipe4 = articles_recipe4), 
  models = list(lm = lm_spec))

save(lm_workflows, articles_folds, file = "results/lm_info.rda")

# Elastic Net
en_workflows <- workflow_set(
  preproc = list(recipe1 = articles_recipe1, recipe2 = articles_recipe2, recipe3 = articles_recipe3, recipe4 = articles_recipe4), 
  models = list(en = en_spec)) %>% 
  option_add(grid = en_grid, id = c("recipe1_en", "recipe2_en", "recipe3_en", "recipe4_en"))

save(en_workflows, articles_folds, file = "results/en_info.rda")

# Random Forests
rf_workflows <- workflow_set(
  preproc = list(recipe1 = articles_recipe1, recipe2 = articles_recipe2, recipe3 = articles_recipe3, recipe4 = articles_recipe4), 
  models = list(rf = rf_spec)) %>% 
  option_add(grid = rf_grid, id = c("recipe1_rf", "recipe2_rf", "recipe3_rf", "recipe4_rf"))

save(rf_workflows, articles_folds, file = "results/rf_info.rda")

# Boosted Tree
bt_workflows <- workflow_set(
  preproc = list(recipe1 = articles_recipe1, recipe2 = articles_recipe2, recipe3 = articles_recipe3, recipe4 = articles_recipe4), 
  models = list(bt = bt_spec)) %>% 
  option_add(grid = bt_grid, id = c("recipe1_bt", "recipe2_bt", "recipe3_bt", "recipe4_bt"))

save(bt_workflows, articles_folds, file = "results/bt_info.rda")

# K-Nearest Neighbors
knn_workflows <- workflow_set(
  preproc = list(recipe1 = articles_recipe1, recipe2 = articles_recipe2, recipe3 = articles_recipe3, recipe4 = articles_recipe4), 
  models = list(knn = knn_spec)) %>% 
  option_add(grid = knn_grid, id = c("recipe1_knn", "recipe2_knn", "recipe3_knn", "recipe4_knn"))

save(knn_workflows, articles_folds, file = "results/knn_info.rda")


