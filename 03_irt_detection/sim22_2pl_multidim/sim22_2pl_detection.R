
# Simulation 2.2 — 2PL DIF Detection


# Load Rasch detection script
# (loads datasets + prepare_data function)


source("sim22_rasch_detection.R")


# 2PL calibration and Wald DIF


start.Time <- Sys.time()

results_2pl <- list()

for(i in 1:length(datasets_2PL_MD)){
  
  prepared <- prepare_data(
    datasets_2PL_MD[[i]]
  )
  
  resp_matrix <- prepared$resp_matrix
  
  group <- prepared$group
  
  true_dif_items <- prepared$true_dif_items
  
  # 2PL calibration
  mod_2pl <- multipleGroup(
    resp_matrix,
    model = 1,
    group = group,
    itemtype = "2PL",
    SE = TRUE
  )
  
  # Wald DIF test
  dif_irt <- mirt::DIF(
    mod_2pl,
    which.par = c("a1","d"),
    Wald = TRUE
  )
  
  # DIF table
  dif_table <- as.data.frame(dif_irt)
  
  dif_table$item_id <- rownames(dif_table)
  
  dif_table <- dif_table %>%
    mutate(
      true_DIF = item_id %in% true_dif_items,
      detected = p < .05
    )
  
  # Confusion matrix
  conf_matrix <- table(
    True = dif_table$true_DIF,
    Detected = dif_table$detected
  )
  
  # Save results
  results_2pl[[i]] <- list(
    magnitude = datasets_2PL_MD[[i]]$magnitude,
    proportion = datasets_2PL_MD[[i]]$proportion,
    drift_type = datasets_2PL_MD[[i]]$drift_type,
    confusion = conf_matrix
  )
}

end.Time <- Sys.time()

print(end.Time - start.Time)