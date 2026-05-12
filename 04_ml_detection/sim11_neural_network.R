

library(tidyverse)
library(mirt)
library(nnet)
library(caret)

source("../01_data_generation/sim11_rasch/01_design.R")

datasets <- readRDS("../Data/sim11_rasch_datasets.rds")


start.Time <- Sys.time()
nn_summary <- data.frame() 

for(i in 1:length(datasets)){  # I am looping over 9 data sets with the different drifts
  
  data_long <- datasets[[i]]$data  # The simulated response data for this condition 
  true_dif_items <- datasets[[i]]$dif_items # the character vector of item IDs that truly have drift
  
  ########  DATA PREPARATION  
  
  # Reshape and get theta
  data_wide <- data_long %>%
    pivot_longer(starts_with("I"), names_to = "item", values_to = "response") %>%
    pivot_wider(names_from = item, values_from = response)
  
  resp_matrix <- data_wide %>%
    select(starts_with("I")) %>%
    as.matrix()
  storage.mode(resp_matrix) <- "numeric"
  
  
  ####### Calibrate the RASCH   
  
  mod <- multipleGroup(
    resp_matrix,
    model    = 1,
    group    = data_wide$group, 
    itemtype = "Rasch", 
  )
  
  
  data_wide$theta_hat <- as.numeric(fscores(mod, method = "EAP")) # extract person ability estimates
  
  
  # reshapes back to long format, now one row per
  #person-item combination, with columns: person_id, group, theta_hat,
  #item_id, response.
  
  df_ml <- data_wide %>% 
    pivot_longer(cols = starts_with("I"), names_to = "item_id", values_to = "response") %>%
    filter(!is.na(response))
  
  ######Set constants
  
  K          <- 5 # 5 folds
  eps        <- 1e-6  # to prevent log(0), a tiny number is added
  nn_results <- data.frame() # empty table to collect one row per item for this datset
  
  
  ###### INNER LOOP 
  
  for(it in unique(df_ml$item_id)){  # we loop over every item ID (1-75), analysing each item independently. # So each item gets two models and its own delta_loss
    
    df_item <- df_ml %>%
      filter(item_id == it) %>%
      mutate(administration = as.factor(group))
    
    ##### Cross Validation 
    
    folds      <- createFolds(df_item$response, k = K) # function from the caret package; splits the data into # roughly equal parts
    cv_loss_m0 <- c() # vector that will store the loss value from each 5 folds
    cv_loss_m1 <- c()
    
    
    ##### Cross validation loop 
    for(k in 1:K){
      
      train_data <- df_item[-folds[[k]], ] 
      test_data  <- df_item[ folds[[k]], ]
      
      # Model 0: ability only 
      
      nn0   <- nnet(response ~ theta_hat,
                    data = train_data, size = 3, decay = 0.01,
                    maxit = 300, trace = FALSE)
      
      # predicting the values 
      
      pred0 <- predict(nn0, test_data, type = "raw")
      
      
      # Cross Entropy 
      loss0 <- -mean(test_data$response * log(pred0 + eps) +
                       (1 - test_data$response) * log(1 - pred0 + eps))
      
      
      # After 5 folds, we have 5 numbers
      cv_loss_m0 <- c(cv_loss_m0, loss0)
      
      
      
      
      
      # Model 1: ability + group 
      nn1   <- nnet(response ~ theta_hat + administration,
                    data = train_data, size = 3, decay = 0.01,
                    maxit = 300, trace = FALSE)
      
      # predicting the responses   
      pred1 <- predict(nn1, test_data, type = "raw")
      
      
      # Cross entropy
      loss1 <- -mean(test_data$response * log(pred1 + eps) +
                       (1 - test_data$response) * log(1 - pred1 + eps))
      
      
      # We store 5 values
      cv_loss_m1 <- c(cv_loss_m1, loss1)
    }
    
    
    
    ######## Comaparing both models; DELTA LOSS  
    
    # Delta_loss: how much does adding group improve predictions?
    # Positive and large = item likely has DIF
    
    
    delta_loss <- mean(cv_loss_m0) - mean(cv_loss_m1) # computing delta loss
    
    nn_results <- rbind(nn_results, data.frame(
      item_id    = it,
      Delta_loss = delta_loss
    ))
  }
  
  ########## Threshold
  
  # Threshold: 95th percentile of Delta_loss across all non-anchor items
  # Non-anchor items cannot drift by design, so their Delta_loss is noise
  
  
  non_anchor_delta <- nn_results %>%
    filter(!item_id %in% anchor_items) %>%
    pull(Delta_loss)  # We filter the 60 non-anchor items
  
  
  threshold_nn <- quantile(non_anchor_delta, 0.95)  # the 95% percentile of these 60 chance values 
  
  # Flag items as DIF
  
  nn_results$detected <- nn_results$Delta_loss > threshold_nn # TRUE if the item's delta_loss exceeds the threshold
  
  nn_results$true_DIF <- nn_results$item_id %in% true_dif_items # TRUE if this item in the ground-truth list of drifting items
  
  
  #######  Count correct and incorrect detections 
  
  TP_nn <- sum( nn_results$true_DIF &  nn_results$detected)
  
  FP_nn <- sum(!nn_results$true_DIF &  nn_results$detected)
  
  FN_nn <- sum( nn_results$true_DIF & !nn_results$detected)
  
  TN_nn <- sum(!nn_results$true_DIF & !nn_results$detected)
  
  
  
# Make a table 
  
  nn_summary <- rbind(nn_summary, data.frame(
    magnitude  = datasets[[i]]$magnitude,
    proportion = datasets[[i]]$proportion,
    TP = TP_nn, FP = FP_nn, FN = FN_nn, TN = TN_nn
  ))
}

# Compute performance metrics
nn_summary <- nn_summary %>%
  mutate(
    sensitivity = TP / (TP + FN),
    specificity = TN / (TN + FP),
    accuracy    = (TP + TN) / (TP + TN + FP + FN)
  )

nn_summary

end.Time <- Sys.time()
print(end.Time - start.Time)
