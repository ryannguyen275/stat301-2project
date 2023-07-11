
library(tidyverse)
library(tidymodels)
library(kableExtra)
library(vip)
library(doMC)
library(parallel)

registerDoMC(cores = 4)

set.seed(13)

# Loading in Data & Finding Best

load("data/articles_split.rda")

# Null 
load("results/null_fit.rda")

baseline <- null_fit %>% 
  collect_metrics() %>% 
  filter(.metric == "rmse") %>% 
  mutate(wflow_id = "null")

# Linear Regression

load("results/lm_tuned.rda")

best_lm <- lm_tuned %>% 
  rank_results() %>% 
  filter(.metric == "rmse") %>% 
  slice_head()

# Elastic Net Tuned
load("results/en_tuned.rda")

best_en <- en_tuned %>% 
  rank_results() %>% 
  filter(.metric == "rmse") %>% 
  slice_head()

best_en

en_metrics <- en_tuned %>% 
  extract_workflow_set_result("recipe3_en") %>% 
  select_best(metric = "rmse")

en_metrics

# Random Forest Tuned
load("results/rf_tuned.rda")

best_rf <- rf_tuned %>% 
  rank_results() %>% 
  filter(.metric == "rmse") %>% 
  slice_head()

rf_metrics<- rf_tuned %>% 
  extract_workflow_set_result("recipe3_rf") %>% 
  select_best(metric = "rmse")

rf_metrics

# Boosted Tree Tuned
load("results/bt_tuned.rda")

best_bt <- bt_tuned %>% 
  rank_results() %>% 
  filter(.metric == "rmse") %>% 
  slice_head()

best_bt

bt_metrics <- bt_tuned %>% 
  extract_workflow_set_result("recipe3_bt") %>% 
  select_best(metric = "rmse")

bt_metrics

# K-Nearest Neighbor Tuned
load("results/knn_tuned.rda")

best_knn <- knn_tuned %>% 
  rank_results() %>% 
  filter(.metric == "rmse") %>% 
  slice_head()

best_knn

knn_metrics<- knn_tuned %>% 
  extract_workflow_set_result("recipe1_knn") %>% 
  select_best(metric = "rmse")

knn_metrics

best_rmse <- tibble(model = c("Null", "Linear", "Elastic Net", "Random Forest", "Boosted Tree", "K-Nearest Neighbor"),
                    rmse = c(baseline$mean, best_lm$mean, best_en$mean, best_rf$mean, best_bt$mean, best_knn$mean),
                    se = c(baseline$std_err, best_lm$std_err, best_en$std_err, best_rf$std_err, best_bt$std_err, best_knn$std_err),
                    wflow = c(baseline$wflow_id, best_lm$wflow_id, best_en$wflow_id, best_rf$wflow_id, best_bt$wflow_id, best_knn$wflow_id))
best_rmse

best_parameters <- tibble(model = c("Elastic Net", "Random Forest", "Boosted Tree", "K-Nearest Neighbor"),
                          penalty = c(en_metrics$penalty, NA, NA, NA),
                          mixture = c(en_metrics$mixture, NA, NA, NA),
                          mtry = c(NA, rf_metrics$mtry, bt_metrics$mtry, NA),
                          min_n = c(NA, rf_metrics$min_n, bt_metrics$min_n, NA),
                          learn_rate = c(NA, NA, bt_metrics$learn_rate, NA),
                          neighbors = c(NA, NA, NA, knn_metrics$neighbors))

best_parameters

save(best_rmse, best_parameters, rf_tuned, file = "results/best_results.rda")

load("results/best_results.rda")

ggplot(best_rmse, aes(x = model, y = rmse, color = model)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = rmse - 1.96*se,
                    ymax = rmse + 1.96*se), width = 0.2) +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = "RMSEs of Different Models") +
  theme_minimal()

# Finding the best workflow

best_workflow <- rf_tuned %>% 
  extract_workflow_set_result("recipe3_rf") %>% 
  select_best(metric = "rmse")

final_workflow <- rf_tuned %>% 
  extract_workflow("recipe3_rf") %>% 
  finalize_workflow(best_workflow)

# Fitting best workflow to training set of data

fit_final <- fit(final_workflow, articles_test)

vip_plot <- fit_final %>% 
  extract_fit_parsnip() %>% 
  vip()

articles_metric <- metric_set(rmse, rsq)

articles_pred <- predict(fit_final, articles_test) %>% 
  bind_cols(articles_test) %>% 
  select(.pred, log_shares, shares) %>% 
  mutate(.pred_shares = 10^.pred) %>% 
  # removing outliers
  filter(shares < 800000)

articles_pred

final_results <- full_join((articles_pred %>% articles_metric(truth = log_shares, estimate = .pred)),
          (articles_pred %>% articles_metric(truth = shares, estimate = .pred_shares)))

final_results

ggplot(articles_pred, aes(x = log_shares, y = .pred)) + 
  geom_point(alpha = 0.2) + 
  geom_abline(intercept = 0, slope = 1) 

ggplot(articles_pred, aes(x = shares, y = .pred_shares)) + 
  geom_point(alpha = 0.2) + 
  geom_abline(intercept = 0, slope = 1) 

# visualize residuals 
articles_pred <- articles_pred %>% 
  mutate(log_residuals = log_shares - .pred,
         residuals = shares - .pred_shares)

articles_pred

save(vip_plot, final_results, articles_pred, file = "results/final_results.rda")

load(final_results)

ggplot(articles_pred, aes(x = shares, y = residuals)) + 
  geom_point(alpha = 0.2) + 
  geom_abline(intercept = 0, slope = 0)

ggplot(articles_pred, aes(x = log_shares, y = log_residuals)) + 
  geom_point(alpha = 0.2)

accuracy()

