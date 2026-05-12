library(dplyr)
library(tidyr)
library(mirt)
library(nnet)
library(caret)

source("01_data_generation/sim23_design.R")

datasets <- readRDS(
  "Data/sim23_datasets.rds"
)

# --- XGBoost Rasch calibration ---
start.Time <- Sys.time()
xgb_summary_rasch_2_3 <- data.frame()

for(i in 1:length(datasets)){
  
  data_long      <- datasets[[i]]$data %>%
    mutate(group = as.factor(paste(country, cycle, sep = "_")))
  true_dif_items <- datasets[[i]]$dif_items
  
  data_wide <- data_long %>%
    pivot_longer(starts_with("I"), names_to = "item", values_to = "response") %>%
    pivot_wider(names_from = item, values_from = response)
  
  resp_matrix <- data_wide %>%
    dplyr::select(any_of(all_items)) %>%
    as.matrix()
  storage.mode(resp_matrix) <- "numeric"
  
  # Rasch calibration — misspecified for discrimination and multidimensionality
  # NOTE: group means and variances constrained to 0 and 1 for all 6 groups
  mod_rasch <- multipleGroup(
    resp_matrix,
    model    = 1,
    group    = data_wide$group,
    itemtype = "Rasch",
    SE       = FALSE
  )
  data_wide$theta_hat <- as.numeric(fscores(mod_rasch, method = "EAP"))
  
  # Long-format, group_bin now encodes 6 groups as integer
  df_ml <- data_wide %>%
    pivot_longer(cols = starts_with("I"), names_to = "item_id", values_to = "response") %>%
    mutate(group_bin = as.integer(factor(group)) - 1L) %>%
    filter(!is.na(response))
  # group_bin: 0 to 5 — one integer per country x cycle combination
  # XGBoost treats this as a numeric feature and will learn
  # to split on group membership just as it does for theta_hat
  
  K           <- 5
  eps         <- 1e-6
  xgb_results <- data.frame()
  
  xgb_params <- list(
    objective  = "binary:logistic",
    eta        = 0.05,
    max_depth  = 3,
    nthread    = 1
  )
  
  for(it in unique(df_ml$item_id)){
    
    df_item <- df_ml %>% filter(item_id == it)
    
    folds      <- createFolds(df_item$response, k = K)
    cv_loss_m0 <- c()
    cv_loss_m1 <- c()
    
    for(k in 1:K){
      
      train_data <- df_item[-folds[[k]], ]
      test_data  <- df_item[ folds[[k]], ]
      
      # Model 0: ability only, no group
      feat_train0 <- matrix(train_data$theta_hat, ncol = 1)
      feat_test0  <- matrix(test_data$theta_hat,  ncol = 1)
      
      dtrain0 <- xgb.DMatrix(feat_train0, label = train_data$response)
      dtest0  <- xgb.DMatrix(feat_test0)
      fit0    <- xgb.train(xgb_params, dtrain0, nrounds = 100, verbose = 0)
      pred0   <- predict(fit0, dtest0)
      loss0   <- -mean(test_data$response * log(pred0 + eps) +
                         (1 - test_data$response) * log(1 - pred0 + eps))
      cv_loss_m0 <- c(cv_loss_m0, loss0)
      
      # Model 1: ability + group
      feat_train1 <- cbind(train_data$theta_hat, train_data$group_bin)
      feat_test1  <- cbind(test_data$theta_hat,  test_data$group_bin)
      
      dtrain1 <- xgb.DMatrix(feat_train1, label = train_data$response)
      dtest1  <- xgb.DMatrix(feat_test1)
      fit1    <- xgb.train(xgb_params, dtrain1, nrounds = 100, verbose = 0)
      pred1   <- predict(fit1, dtest1)
      loss1   <- -mean(test_data$response * log(pred1 + eps) +
                         (1 - test_data$response) * log(1 - pred1 + eps))
      cv_loss_m1 <- c(cv_loss_m1, loss1)
    }
    
    delta_loss <- mean(cv_loss_m0) - mean(cv_loss_m1)
    
    xgb_results <- rbind(xgb_results, data.frame(
      item_id    = it,
      Delta_loss = delta_loss
    ))
  }
  
  non_anchor_delta <- xgb_results %>%
    filter(!item_id %in% anchor_items) %>%
    pull(Delta_loss)
  threshold_xgb <- quantile(non_anchor_delta, 0.95)
  
  xgb_results$detected <- xgb_results$Delta_loss > threshold_xgb
  xgb_results$true_DIF <- xgb_results$item_id %in% true_dif_items
  
  TP_xgb <- sum( xgb_results$true_DIF &  xgb_results$detected)
  FP_xgb <- sum(!xgb_results$true_DIF &  xgb_results$detected)
  FN_xgb <- sum( xgb_results$true_DIF & !xgb_results$detected)
  TN_xgb <- sum(!xgb_results$true_DIF & !xgb_results$detected)
  
  xgb_summary_rasch_2_3 <- rbind(xgb_summary_rasch_2_3, data.frame(
    drift_type = datasets[[i]]$drift_type,
    magnitude  = datasets[[i]]$magnitude,
    proportion = datasets[[i]]$proportion,
    TP = TP_xgb, FP = FP_xgb, FN = FN_xgb, TN = TN_xgb
  ))
}

xgb_summary_rasch_2_3 <- xgb_summary_rasch_2_3 %>%
  mutate(
    sensitivity = TP / (TP + FN),
    specificity = TN / (TN + FP),
    accuracy    = (TP + TN) / (TP + TN + FP + FN)
  )

xgb_summary_rasch_2_3
end.Time <- Sys.time()
print(end.Time - start.Time)

