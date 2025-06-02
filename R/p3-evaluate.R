library(readr)
library(tidymodels)

telco_customer <- read_csv("data/telco_customer_churn.csv")
telco_customer

set.seed(1)
telco_split <- telco_customer %>% 
  mutate(churn_label = factor(churn_label, levels = c("Yes", "No"))) %>% 
  initial_split(prop = 0.8, strata = churn_label)

telco_training <- training(telco_split)
telco_testing <- testing(telco_split)

telco_final_wf <- read_rds("telco_final_wf.rds")

telco_training_aug <- telco_final_wf %>% 
  augment(new_data = telco_training) %>% 
  select(churn_label, .pred_class, .pred_Yes)
telco_testing_aug <- telco_final_wf %>% 
  augment(new_data = telco_testing) %>% 
  select(churn_label, .pred_class, .pred_Yes)

# Evaluate Model ----
## Fungsi untuk evaluasi model ----
eval_model_class <- metric_set(accuracy, bal_accuracy, 
                               sensitivity, specificity, 
                               recall, precision, f_meas)

## Evaluate metric ----

### Bandingkan antara hasil prediksi di data training dan 
### testing untuk melihat ada overfitting atau tidak 

telco_training_aug %>% 
  conf_mat(truth = churn_label, estimate = .pred_class)
telco_testing_aug %>% 
  conf_mat(truth = churn_label, estimate = .pred_class)

telco_training_aug %>% 
  accuracy(truth = churn_label, estimate = .pred_class)
telco_testing_aug %>% 
  accuracy(truth = churn_label, estimate = .pred_class)

telco_training_aug %>% 
  bal_accuracy(truth = churn_label, estimate = .pred_class)
telco_testing_aug %>% 
  bal_accuracy(truth = churn_label, estimate = .pred_class)

telco_training_aug %>% 
  sensitivity(truth = churn_label, estimate = .pred_class)
telco_testing_aug %>% 
  sensitivity(truth = churn_label, estimate = .pred_class)

telco_training_aug %>% 
  recall(truth = churn_label, estimate = .pred_class)
telco_testing_aug %>% 
  recall(truth = churn_label, estimate = .pred_class)

telco_training_aug %>% 
  specificity(truth = churn_label, estimate = .pred_class)
telco_testing_aug %>% 
  specificity(truth = churn_label, estimate = .pred_class)

telco_training_aug %>% 
  precision(truth = churn_label, estimate = .pred_class)
telco_testing_aug %>% 
  precision(truth = churn_label, estimate = .pred_class)

telco_training_aug %>% 
  ppv(truth = churn_label, estimate = .pred_class)
telco_testing_aug %>% 
  ppv(truth = churn_label, estimate = .pred_class)

telco_training_aug %>% 
  f_meas(truth = churn_label, estimate = .pred_class)
telco_testing_aug %>% 
  f_meas(truth = churn_label, estimate = .pred_class)

## Evaluate multiple metrics at once ----
telco_training_aug %>% 
  eval_model_class(truth = churn_label, estimate = .pred_class)
telco_testing_aug %>% 
  eval_model_class(truth = churn_label, estimate = .pred_class)

## ROC and AUC ----
telco_training_aug %>% 
  roc_curve(truth = churn_label, .pred_Yes) %>% 
  autoplot()
telco_training_aug %>% 
  roc_auc(truth = churn_label, .pred_Yes) 

telco_testing_aug %>% 
  roc_curve(truth = churn_label, .pred_Yes) %>% 
  autoplot()
telco_testing_aug %>% 
  roc_auc(truth = churn_label, .pred_Yes) 

## Lift curve ----
telco_testing_aug %>% 
  lift_curve(truth = churn_label, .pred_Yes) %>% 
  autoplot()

telco_testing_aug %>% 
  arrange(desc(.pred_Yes)) %>% 
  select(churn_label, .pred_Yes) %>% 
  slice_head(n = ceiling(0.25*nrow(telco_testing_aug))) %>% 
  count(churn_label) %>% 
  mutate(pct = n/sum(n))

telco_testing_aug %>% 
  count(churn_label) %>% 
  mutate(pct = n/sum(n))

0.705/0.265
