# Simulation 2.1 — Rasch Calibration and Wald DIF


library(tidyverse)

library(mirt)

source("../../01_data_generation/sim21_2pl/01_design_2pl.R")

datasets_2PL <- readRDS(
  "../../Data/sim21_2pl_datasets.rds"
)



# Prepare dataset


prepare_data <- function(dataset_obj){
  
  data_long <- dataset_obj$data
  
  true_dif_items <- dataset_obj$dif_items
  
  data_wide <- data_long %>%
    pivot_longer(
      starts_with("I"),
      names_to = "item",
      values_to = "response"
    ) %>%
    pivot_wider(
      names_from = item,
      values_from = response
    )
  
  resp_matrix <- data_wide %>%
    dplyr::select(all_of(all_items)) %>%
    as.matrix()
  
  storage.mode(resp_matrix) <- "numeric"
  
  list(
    resp_matrix = resp_matrix,
    group = data_wide$group,
    true_dif_items = true_dif_items
  )
}



# Rasch DIF detection



start.Time <- Sys.time()

results_rasch <- list()

for(i in 1:length(datasets_2PL)){
  
  prepared <- prepare_data(datasets_2PL[[i]])
  
  resp_matrix <- prepared$resp_matrix
  
  group <- prepared$group
  
  true_dif_items <- prepared$true_dif_items
  
  mod_rasch_2_1 <- multipleGroup(
    resp_matrix,
    model = 1,
    group = group,
    itemtype = "Rasch",
    SE = TRUE
  )
  
  dif_irt <- mirt::DIF(
    mod_rasch_2_1,
    which.par = "d",
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
  
  results_rasch[[i]] <- list(
    magnitude = datasets_2PL[[i]]$magnitude,
    proportion = datasets_2PL[[i]]$proportion,
    drift_type = datasets_2PL[[i]]$drift_type,
    confusion = conf_matrix
  )
}

end.Time <- Sys.time()

print(end.Time - start.Time)