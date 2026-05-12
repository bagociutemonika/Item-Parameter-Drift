

library(tidyverse)
library(mirt)
library(caret)
library(xgboost)

source("../01_data_generation/sim11_rasch/01_design.R")

datasets <- readRDS("../Data/sim11_rasch_datasets.rds")

start.Time <- Sys.time()
xgb_summary <- data.frame()

for(i in 1:length(datasets)){
  
  data_long      <- datasets[[i]]$data   # the simulated response data in long format
  true_dif_items <- datasets[[i]]$dif_items # the character vector of item ID's that truly have drift
  
  ###### Data reshaping 
  
  # 1. Wide 
  
  data_wide <- data_long %>%
    pivot_longer(starts_with("I"), names_to = "item", values_to = "response") %>%
    pivot_wider(names_from = item, values_from = response)
  # 2. Response 
  
  resp_matrix <- data_wide %>%
    select(starts_with("I")) %>%
    as.matrix()
  storage.mode(resp_matrix) <- "numeric"
  
  
  ###### Calibrate the Rasch model  
  
  mod <- multipleGroup(
    resp_matrix,
    model    = 1,
    group    = data_wide$group,
    itemtype = "Rasch"
  )
  
  # extract theta's 
  data_wide$theta_hat <- as.numeric(fscores(mod, method = "EAP"))
  
  # Prepare the data 
  
  df_ml <- data_wide %>%
    pivot_longer(cols = starts_with("I"), names_to = "item_id", values_to = "response") %>%
    mutate(group_bin = as.integer(factor(group)) - 1L) %>%  # group as 0/1 for xgboost
    filter(!is.na(response))
  
  
  
  ############ XGBoost DIF 
  
  # CV  
  K           <- 5
  eps         <- 1e-6
  xgb_results <- data.frame()
  
  # XGBoost settings  
  xgb_params <- list(
    objective  = "binary:logistic", # I indicate that we are predicting a binary outcome 
    eta        = 0.05, # learning rate, so how much each new tree corrects the previous mistakes, small value = more careful, also more stable 
    max_depth  = 3,  # Shallow trees prevent overfitting; maximum depth of each decision tree
    nthread    = 1 # one CPU thread, reproducibility
  )
  
  
  ##### Inner Loop 
  
  for(it in unique(df_ml$item_id)){
    
    df_item <- df_ml %>% filter(item_id == it)
    
    folds      <- createFolds(df_item$response, k = K)
    cv_loss_m0 <- c()
    cv_loss_m1 <- c()
    
    
    #### Cross validation loops  
    for(k in 1:K){
      
      train_data <- df_item[-folds[[k]], ]
      test_data  <- df_item[ folds[[k]], ]
      
      
      
      # Model 0: ability only 
      # for model0, theta_hat is a single column matrix.
      dtrain0 <- xgb.DMatrix(matrix(train_data$theta_hat, ncol = 1), # internal data format
                             label = train_data$response) # The outcome we wan to predict (0/1)
      
      
      
      dtest0  <- xgb.DMatrix(matrix(test_data$theta_hat,  ncol = 1))  # we predict for the test set
      
      
      
      fit0    <- xgb.train(xgb_params, dtrain0, nrounds = 100, verbose = 0) 
      
      pred0   <- predict(fit0, dtest0)
      
      loss0   <- -mean(test_data$response * log(pred0 + eps) +
                         (1 - test_data$response) * log(1 - pred0 + eps))
      
      cv_loss_m0 <- c(cv_loss_m0, loss0)
      
      
      # Model 1: ability + group
      
      dtrain1 <- xgb.DMatrix(cbind(train_data$theta_hat, train_data$group_bin), # we 
                             label = train_data$response)
      
      dtest1  <- xgb.DMatrix(cbind(test_data$theta_hat, test_data$group_bin))
      
      fit1    <- xgb.train(xgb_params, dtrain1, nrounds = 100, verbose = 0) # here we train the model, it builds 100 trees sequentially, verbose silences the output so it does not print 100 lines 
      
      pred1   <- predict(fit1, dtest1) # here we apply the trained 100 trees to the held-out test data and returns predicted probabilities 
      
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
  
  # Same threshold approach: 95th percentile of non-anchor items
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
  
  xgb_summary <- rbind(xgb_summary, data.frame(
    magnitude  = datasets[[i]]$magnitude,
    proportion = datasets[[i]]$proportion,
    TP = TP_xgb, FP = FP_xgb, FN = FN_xgb, TN = TN_xgb
  ))
}

xgb_summary <- xgb_summary %>%
  mutate(
    sensitivity = TP / (TP + FN),
    specificity = TN / (TN + FP),
    accuracy    = (TP + TN) / (TP + TN + FP + FN)
  )

xgb_summary

end.Time <- Sys.time()
print(end.Time - start.Time)