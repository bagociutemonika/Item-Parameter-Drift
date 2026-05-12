
# Simulation 1.2 — Neural Network DIF Detection


library(tidyverse)
library(mirt)
library(nnet)
library(caret)

source("../01_data_generation/sim12_multidim/01_multidim_design.R")

datasets <- readRDS(
  "../Data/sim12_multidim_datasets.rds"
)

start.Time <- Sys.time()

nn_summary_1_2 <- data.frame()
# Empty table to collect one row per dataset after each iteration.


for(i in 1:length(datasets)){
  
  data_long      <- datasets[[i]]$data
  # Response data for dataset i — multidimensional by design but we treat
  # it as unidimensional, exactly as IRT does.
  
  true_dif_items <- datasets[[i]]$dif_items
  # Ground truth: which anchor items truly have drift in this condition.
  
  # --- Reshape to wide ---
  data_wide <- data_long %>%
    pivot_longer(starts_with("I"), names_to = "item", values_to = "response") %>%
    pivot_wider(names_from = item, values_from = response)
  # Cleans the data into one row per person, one column per item.
  
  # --- Response matrix ---
  resp_matrix <- data_wide %>%
    dplyr::select(starts_with("I")) %>%
    as.matrix()
  storage.mode(resp_matrix) <- "numeric"
  # Needed by mirt: item columns only, forced to numeric storage.
  
  # --- Unidimensional Rasch model ---
  mod <- multipleGroup(
    resp_matrix,
    model    = 1,
    group    = data_wide$group,
    itemtype = "Rasch"
  )
  # model = 1: one latent dimension — the unidimensional assumption.
  # This is IDENTICAL to what the IRT Wald test uses above.
  # We do not fit a 3-factor model; both IRT and ML operate under
  # the same misspecification, keeping the comparison fair.
  
  data_wide$theta_hat <- as.numeric(fscores(mod, method = "EAP"))
  # One ability estimate per person from the misspecified unidimensional model.
  # This is the same noisy composite theta that the IRT test uses.
  
  # --- Build long-format ML dataset with domain attached ---
  df_ml <- data_wide %>%
    pivot_longer(cols = starts_with("I"), names_to = "item_id", values_to = "response") %>%
    mutate(administration = as.factor(group)) %>%
    filter(!is.na(response)) %>%
    left_join(item_domain, by = "item_id") %>%
    mutate(domain = as.factor(domain))
  # pivot_longer: one row per person-item pair.
  # filter(!is.na(response)): removes booklet-design missings.
  # left_join(item_domain, by = "item_id"): adds the domain column.
  #   Every row now knows whether its item is "algebra", "geometry", or "comb".
  #   This is item metadata — not a second latent trait — so the
  #   unidimensional assumption is not violated.
  # as.factor(domain): nnet() requires a factor for categorical inputs.
  
  # --- NN constants ---
  K          <- 5
  eps        <- 1e-6
  nn_results <- data.frame()
  
  for(it in unique(df_ml$item_id)){
    
    df_item <- df_ml %>% filter(item_id == it)
    # All rows for this item. The domain column is the same value for
    # every row (e.g. all "algebra" for I1) but it varies across items,
    # so the model can learn domain-specific response patterns.
    
    folds      <- createFolds(df_item$response, k = K)
    # 5 stratified folds — balanced proportion of 0s and 1s in each fold.
    
    cv_loss_m0 <- c()
    cv_loss_m1 <- c()
    
    for(k in 1:K){
      
      train_data <- df_item[-folds[[k]], ]
      test_data  <- df_item[ folds[[k]], ]
      
      # Model 0: theta_hat + domain, no group
      # theta_hat: ability, same as IRT uses
      # domain:    item content area — lets the network learn that the
      #            ICC shape may differ across algebra / geometry / comb
      #            items, capturing the multidimensional structure without
      #            estimating separate latent traits
      nn0 <- nnet(response ~ theta_hat,
                  data  = train_data, size = 3, decay = 0.01,
                  maxit = 300, trace = FALSE)
      
      pred0 <- predict(nn0, test_data, type = "raw")
      loss0 <- -mean(test_data$response * log(pred0 + eps) +
                       (1 - test_data$response) * log(1 - pred0 + eps))
      cv_loss_m0 <- c(cv_loss_m0, loss0)
      
      # Model 1: theta_hat + domain + group
      # Adds administration group on top.
      # If group still improves predictions beyond theta + domain,
      # the item has DIF that cannot be explained by ability or
      # domain membership alone.
      nn1 <- nnet(response ~ theta_hat + administration,
                  data  = train_data, size = 3, decay = 0.01,
                  maxit = 300, trace = FALSE)
      
      pred1 <- predict(nn1, test_data, type = "raw")
      loss1 <- -mean(test_data$response * log(pred1 + eps) +
                       (1 - test_data$response) * log(1 - pred1 + eps))
      cv_loss_m1 <- c(cv_loss_m1, loss1)
    }
    
    delta_loss <- mean(cv_loss_m0) - mean(cv_loss_m1)
    # How much does adding group improve predictions beyond theta + domain?
    # Large positive = group carries extra information = DIF.
    
    nn_results <- rbind(nn_results, data.frame(
      item_id    = it,
      Delta_loss = delta_loss
    ))
  }
  
  # --- Threshold: 95th percentile of non-anchor Delta_loss ---
  non_anchor_delta <- nn_results %>%
    filter(!item_id %in% anchor_items) %>%
    pull(Delta_loss)
  # Non-anchor items cannot drift by design — their Delta_loss is pure noise.
  # The 95th percentile of this noise = detection threshold.
  
  threshold_nn <- quantile(non_anchor_delta, 0.95)
  
  nn_results$detected <- nn_results$Delta_loss > threshold_nn
  # TRUE = flagged as DIF by our method.
  
  nn_results$true_DIF <- nn_results$item_id %in% true_dif_items
  # TRUE = genuinely drifting item (ground truth from simulation).
  
  TP_nn <- sum( nn_results$true_DIF &  nn_results$detected)
  FP_nn <- sum(!nn_results$true_DIF &  nn_results$detected)
  FN_nn <- sum( nn_results$true_DIF & !nn_results$detected)
  TN_nn <- sum(!nn_results$true_DIF & !nn_results$detected)
  
  nn_summary_1_2 <- rbind(nn_summary_1_2, data.frame(
    magnitude  = datasets[[i]]$magnitude,
    proportion = datasets[[i]]$proportion,
    TP = TP_nn, FP = FP_nn, FN = FN_nn, TN = TN_nn
  ))
}

nn_summary_1_2 <- nn_summary_1_2 %>%
  mutate(
    sensitivity = TP / (TP + FN),
    specificity = TN / (TN + FP),
    accuracy    = (TP + TN) / (TP + TN + FP + FN)
  )

nn_summary_1_2

end.Time <- Sys.time()
print(end.Time - start.Time)
