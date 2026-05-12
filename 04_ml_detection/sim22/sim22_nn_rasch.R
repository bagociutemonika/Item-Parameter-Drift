
# Simulation 2.2 — Neural Network DIF Detection
# Rasch Calibration



library(nnet)

library(caret)

select <- dplyr::select


# Load design


source("../01_data_generation/sim22_2pl_multidim/01_design_2pl_md.R")


# Load datasets


datasets_2PL_MD <- readRDS(
  "../Data/sim22_2pl_md_datasets.rds"
)

start.Time <- Sys.time()

select <- dplyr::select

nn_summary_rasch_2_2 <- data.frame()

for(i in 1:length(datasets_2PL_MD)){
  
  data_long      <- datasets_2PL_MD[[i]]$data
  true_dif_items <- datasets_2PL_MD[[i]]$dif_items
  
  data_wide <- data_long %>%
    pivot_longer(starts_with("I"), names_to = "item", values_to = "response") %>%
    pivot_wider(names_from = item, values_from = response)
  
  resp_matrix <- data_wide %>%
    dplyr::select(all_of(all_items)) %>%
    as.matrix()
  storage.mode(resp_matrix) <- "numeric"
  
  mod_rasch <- multipleGroup(
    resp_matrix,
    model    = 1,
    group    = data_wide$group,
    itemtype = "Rasch",
    SE       = FALSE
  )
  data_wide$theta_hat <- as.numeric(fscores(mod_rasch, method = "EAP"))
  
  # Long-format, no domain join
  df_ml <- data_wide %>%
    pivot_longer(cols = starts_with("I"), names_to = "item_id", values_to = "response") %>%
    mutate(administration = as.factor(group)) %>%
    filter(!is.na(response))
  
  K          <- 5
  eps        <- 1e-6
  nn_results <- data.frame()
  
  for(it in unique(df_ml$item_id)){
    
    df_item <- df_ml %>% filter(item_id == it)
    
    folds      <- createFolds(df_item$response, k = K)
    cv_loss_m0 <- c()
    cv_loss_m1 <- c()
    
    for(k in 1:K){
      
      train_data <- df_item[-folds[[k]], ]
      test_data  <- df_item[ folds[[k]], ]
      
      # Model 0: ability only, no group, no domain
      nn0   <- nnet(response ~ theta_hat,
                    data = train_data, size = 3, decay = 0.01,
                    maxit = 300, trace = FALSE)
      pred0 <- predict(nn0, test_data, type = "raw")
      loss0 <- -mean(test_data$response * log(pred0 + eps) +
                       (1 - test_data$response) * log(1 - pred0 + eps))
      cv_loss_m0 <- c(cv_loss_m0, loss0)
      
      # Model 1: ability + group, no domain
      nn1   <- nnet(response ~ theta_hat + administration,
                    data = train_data, size = 3, decay = 0.01,
                    maxit = 300, trace = FALSE)
      pred1 <- predict(nn1, test_data, type = "raw")
      loss1 <- -mean(test_data$response * log(pred1 + eps) +
                       (1 - test_data$response) * log(1 - pred1 + eps))
      cv_loss_m1 <- c(cv_loss_m1, loss1)
    }
    
    delta_loss <- mean(cv_loss_m0) - mean(cv_loss_m1)
    
    nn_results <- rbind(nn_results, data.frame(
      item_id    = it,
      Delta_loss = delta_loss
    ))
  }
  
  non_anchor_delta <- nn_results %>%
    filter(!item_id %in% anchor_items) %>%
    pull(Delta_loss)
  threshold_nn <- quantile(non_anchor_delta, 0.95)
  
  nn_results$detected <- nn_results$Delta_loss > threshold_nn
  nn_results$true_DIF <- nn_results$item_id %in% true_dif_items
  
  TP_nn <- sum( nn_results$true_DIF &  nn_results$detected)
  FP_nn <- sum(!nn_results$true_DIF &  nn_results$detected)
  FN_nn <- sum( nn_results$true_DIF & !nn_results$detected)
  TN_nn <- sum(!nn_results$true_DIF & !nn_results$detected)
  
  nn_summary_rasch_2_2 <- rbind(nn_summary_rasch_2_2, data.frame(
    drift_type = datasets_2PL_MD[[i]]$drift_type,
    magnitude  = datasets_2PL_MD[[i]]$magnitude,
    proportion = datasets_2PL_MD[[i]]$proportion,
    TP = TP_nn, FP = FP_nn, FN = FN_nn, TN = TN_nn
  ))
}

nn_summary_rasch_2_2 <- nn_summary_rasch_2_2 %>%
  mutate(
    sensitivity = TP / (TP + FN),
    specificity = TN / (TN + FP),
    accuracy    = (TP + TN) / (TP + TN + FP + FN)
  )

nn_summary_rasch_2_2

end.Time <- Sys.time()
print(end.Time - start.Time)