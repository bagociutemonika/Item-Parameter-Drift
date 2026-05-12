

# Simulation 2.1 — 2PL Calibration and Wald DIF


source("sim21_rasch_detection.R")


# 2PL DIF detection


start.Time <- Sys.time()

results_2pl <- list()

for(i in 1:length(datasets_2PL)){
  
  prepared <- prepare_data(datasets_2PL[[i]])
  
  resp_matrix <- prepared$resp_matrix
  
  group <- prepared$group
  
  true_dif_items <- prepared$true_dif_items
  
  mod_2pl_2_1 <- multipleGroup(
    resp_matrix,
    model = 1,
    group = group,
    itemtype = "2PL",
    SE = TRUE
  )
  
  dif_irt <- mirt::DIF(
    mod_2pl_2_1,
    which.par = c("a1", "d"),
    Wald = TRUE
  )
  
  dif_table <- as.data.frame(dif_irt)
  
  dif_table$item_id <- rownames(dif_table)
  
  dif_table <- dif_table %>%
    mutate(
      true_DIF = item_id %in% true_dif_items,
      detected = p < .05
    )
  
  conf_matrix <- table(
    True = dif_table$true_DIF,
    Detected = dif_table$detected
  )
  
  results_2pl[[i]] <- list(
    magnitude = datasets_2PL[[i]]$magnitude,
    proportion = datasets_2PL[[i]]$proportion,
    drift_type = datasets_2PL[[i]]$drift_type,
    confusion = conf_matrix
  )
}

end.Time <- Sys.time()

print(end.Time - start.Time)