library(tidyverse)

telco_customer <- read_csv("data/telco_customer_churn.csv")
telco_customer

telco_customer %>% 
  glimpse()

telco_customer %>% 
  count(churn_label) %>% 
  mutate(pct = n/sum(n))

library(tidymodels)

# Proporsi data training
p <- 0.8

number(c("training" = p, "testing" = 1-p)*nrow(telco_customer), 
       big.mark = ",")

set.seed(1)
telco_split <- telco_customer %>% 
  mutate(churn_label = factor(churn_label, levels = c("Yes", "No"))) %>% 
  initial_split(prop = p, strata = churn_label)

telco_training <- training(telco_split)
telco_testing <- testing(telco_split)

telco_training %>% 
  count(churn_label) %>% 
  mutate(pct = n/sum(n))

telco_testing %>% 
  count(churn_label) %>% 
  mutate(pct = n/sum(n))

# Cross-validation data ----
telco_cv <- telco_training %>% 
  vfold_cv(v = 5, strata = churn_label)

# Recipe ----
basic_recipe <- telco_training %>% 
  select(-customer_id) %>% 
  recipe(churn_label ~ .) %>% 
  step_mutate(across(.cols = c(married, referred_a_friend, 
                               phone_service, multiple_lines, 
                               internet_service, 
                               online_security:unlimited_data, 
                               paperless_billing), 
                     .fns = function(x)factor(x, levels = c("Yes", "No"))), 
              skip = FALSE) %>% 
  step_mutate(across(.cols = c(gender, offer, internet_type, 
                               contract, payment_method), 
                     .fns = as.factor), 
              skip = FALSE)

basic_recipe

normalized_dummy <- basic_recipe %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_factor_predictors())
normalized_dummy

normalized_dummy %>% 
  prep() %>% 
  juice() %>% 
  summarise(avg = mean(age), 
            std = sd(age))

lr_spec <- logistic_reg(penalty = tune(), 
                        mixture = tune()) %>% 
  set_engine("glmnet") %>% 
  set_mode("classification")

dtree_spec <- decision_tree(cost_complexity = tune(), 
                            tree_depth = tune(), 
                            min_n = tune()) %>% 
  set_engine("rpart", model = TRUE) %>% 
  set_mode("classification")


rf_spec <- rand_forest(mtry = tune(), 
                       min_n = tune(), trees = 500) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

wfset <- workflow_set(preproc = list(norm = normalized_dummy, 
                                     basic = basic_recipe, 
                                     basic = basic_recipe, 
                                     norm = normalized_dummy), 
                      models = list(logreg = lr_spec, 
                                    dtree = dtree_spec, 
                                    rf = rf_spec, 
                                    rf = rf_spec), 
                      cross = FALSE)

library(finetune)

race_ctrl <- control_race(
  save_pred = TRUE,
  parallel_over = "everything",
  save_workflow = TRUE
)

wfset_result <- wfset %>% 
  workflow_map(fn = "tune_race_anova", 
               resamples = telco_cv, 
               grid = 25, 
               control = race_ctrl, 
               metrics = metric_set(roc_auc), 
               verbose = TRUE, 
               seed = 123)
wfset_result

wfset_result %>% 
  filter(wflow_id == "norm_logreg") %>% 
  pull(result) %>% 
  pluck(1) %>% 
  pull(.metrics) 


wfset_result %>% 
  autoplot(
    rank_metric = "roc_auc", 
    metric = "roc_auc", 
    select_best = TRUE) +
  geom_text(aes(y = mean, label = wflow_id), 
            angle = 90, hjust = 0.5, vjust = -0.7) +
  theme(legend.position = "none")

## Logistic Regression ----

wfset_result %>% 
  extract_workflow_set_result("norm_logreg") %>% 
  collect_metrics()

lr_param <- wfset_result %>% 
  extract_workflow_set_result("norm_logreg") %>% 
  select_best(metric = "roc_auc")
lr_param

telco_best_lr <- wfset_result %>% 
  extract_workflow("norm_logreg") %>% 
  finalize_workflow(lr_param) %>% 
  last_fit(split = telco_split)

collect_metrics(telco_best_lr)


## Decision tree ----

wfset_result %>% 
  extract_workflow_set_result("basic_dtree") %>% 
  collect_metrics()

dtree_param <- wfset_result %>% 
  extract_workflow_set_result("basic_dtree") %>% 
  select_best(metric = "roc_auc")
dtree_param

telco_best_dtree <- wfset_result %>% 
  extract_workflow("basic_dtree") %>% 
  finalize_workflow(dtree_param) %>% 
  last_fit(split = telco_split)

collect_metrics(telco_best_dtree)

telco_best_dtree %>% 
  extract_workflow() %>% 
  extract_fit_engine() %>% 
  rpart.plot::rpart.plot(tweak = 1.5)


## Random Forest ----

wfset_result %>% 
  extract_workflow_set_result("basic_rf") %>% 
  collect_metrics()

rf_param <- wfset_result %>% 
  extract_workflow_set_result("basic_rf") %>% 
  select_best(metric = "roc_auc")
rf_param

telco_best_rf <- wfset_result %>% 
  extract_workflow("basic_rf") %>% 
  finalize_workflow(rf_param) %>% 
  last_fit(split = telco_split)

collect_metrics(telco_best_rf)
collect_predictions(telco_best_rf)



# Compare ------------------------------------------------------------

lr_pred <- telco_best_lr %>% 
  extract_workflow() %>% 
  augment(new_data = telco_testing) %>% 
  select(churn_label, .pred_class, .pred_Yes)

dtree_pred <- telco_best_dtree %>% 
  extract_workflow() %>% 
  augment(new_data = telco_testing) %>% 
  select(churn_label, .pred_class, .pred_Yes)

rf_pred <- telco_best_rf %>% 
  extract_workflow() %>% 
  augment(new_data = telco_testing) %>% 
  select(churn_label, .pred_class, .pred_Yes)

compare_roc <- bind_rows(
  lr_pred %>% 
    roc_curve(truth = churn_label, .pred_Yes) %>% 
    mutate(Algorithm = "Logreg"),
  dtree_pred %>% 
    roc_curve(truth = churn_label, .pred_Yes) %>% 
    mutate(Algorithm = "DecTree"),
  rf_pred %>% 
    roc_curve(truth = churn_label, .pred_Yes) %>% 
    mutate(Algorithm = "RF")
)

compare_roc %>% 
  ggplot(aes(x = 1 - specificity, 
             y = sensitivity, 
             color = Algorithm)) + 
  geom_line() + 
  coord_fixed()

compare_auc <- bind_rows(
  lr_pred %>% 
    roc_auc(truth = churn_label, .pred_Yes) %>% 
    mutate(Algorithm = "Logreg"),
  dtree_pred %>% 
    roc_auc(truth = churn_label, .pred_Yes) %>% 
    mutate(Algorithm = "DecTree"),
  rf_pred %>% 
    roc_auc(truth = churn_label, .pred_Yes) %>% 
    mutate(Algorithm = "RF")
) %>% 
  arrange(desc(.estimate))

compare_auc

# Extract Final Model & Worflow ----

telco_final_wf <- telco_best_rf %>% 
  extract_workflow()

telco_final_wf %>% 
  predict(new_data = telco_testing)

telco_final_wf %>% 
  predict(new_data = telco_testing, type = "prob")

telco_test_pred <- telco_final_wf %>% 
  augment(new_data = telco_testing)
telco_test_pred

telco_final_wf %>% 
  write_rds("telco_final_wf.rds")

telco_test_pred %>% 
  select(churn_label, .pred_class, .pred_Yes) %>% 
  write_csv("data/telco_test_pred.csv")


