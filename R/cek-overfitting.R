lr_pred_train <- telco_best_lr %>% 
  extract_workflow() %>% 
  augment(new_data = telco_training) %>% 
  select(churn_label, .pred_class, .pred_Yes)

dtree_pred_train <- telco_best_dtree %>% 
  extract_workflow() %>% 
  augment(new_data = telco_training) %>% 
  select(churn_label, .pred_class, .pred_Yes)

rf_pred_train <- telco_best_rf %>% 
  extract_workflow() %>% 
  augment(new_data = telco_training) %>% 
  select(churn_label, .pred_class, .pred_Yes)

compare_class_train <- bind_rows(
  lr_pred_train %>% 
    eval_model_class(truth = churn_label, estimate = .pred_class) %>% 
    mutate(Algorithm = "Logreg"),
  dtree_pred_train %>% 
    eval_model_class(truth = churn_label, estimate = .pred_class) %>% 
    mutate(Algorithm = "DecTree"),
  rf_pred_train %>% 
    eval_model_class(truth = churn_label, estimate = .pred_class) %>% 
    mutate(Algorithm = "RF")
)

compare_auc_tain <- bind_rows(
  lr_pred_train %>% 
    roc_auc(truth = churn_label, .pred_Yes) %>% 
    mutate(Algorithm = "Logreg"),
  dtree_pred_train %>% 
    roc_auc(truth = churn_label, .pred_Yes) %>% 
    mutate(Algorithm = "DecTree"),
  rf_pred_train %>% 
    roc_auc(truth = churn_label, .pred_Yes) %>% 
    mutate(Algorithm = "RF")
) 

compare_train <- compare_class_train %>% 
  bind_rows(compare_auc_tain) 


compare_class <- bind_rows(
  lr_pred %>% 
    eval_model_class(truth = churn_label, estimate = .pred_class) %>% 
    mutate(Algorithm = "Logreg"),
  dtree_pred %>% 
    eval_model_class(truth = churn_label, estimate = .pred_class) %>% 
    mutate(Algorithm = "DecTree"),
  rf_pred %>% 
    eval_model_class(truth = churn_label, estimate = .pred_class) %>% 
    mutate(Algorithm = "RF")
)

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
) 

compare_test <- compare_class %>% 
  bind_rows(compare_auc) 

compare_class_train %>% 
  rename(Training = .estimate) %>% 
  inner_join(compare_test%>% 
               rename(Testing = .estimate), by = join_by(.metric, .estimator, Algorithm)) %>% 
  transmute(Algorithm, .metric, Training, Testing, 
            seleisih = (Training - Testing)*100) %>% 
  print(n = Inf)
