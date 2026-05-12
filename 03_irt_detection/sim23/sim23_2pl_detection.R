library(mirt)
library(dplyr)
library(tidyr)

source("01_data_generation/sim23_design.R")

datasets <- readRDS("Data/sim23_datasets.rds")

start.Time <- Sys.time()

results_2pl <- list()

for(i in 1:length(datasets)){
  
  data_long      <- datasets[[i]]$data
  true_dif_items <- datasets[[i]]$dif_items
  
  data_wide <- data_long %>%
    mutate(cycle_group = as.factor(as.character(cycle))) %>%
    pivot_longer(
      cols      = any_of(all_items),
      names_to  = "item",
      values_to = "response"
    ) %>%
    pivot_wider(
      names_from  = item,
      values_from = response
    )
  
  resp_matrix <- data_wide %>%
    dplyr::select(any_of(all_items)) %>%
    as.matrix()
  
  storage.mode(resp_matrix) <- "numeric"
  
  cycle_group <- data_wide$cycle_group
  
  mod_2pl <- multipleGroup(
    resp_matrix,
    model    = 1,
    group    = cycle_group,
    itemtype = "2PL",
    SE       = TRUE
  )
  
  dif_irt <- DIF(
    mod_2pl,
    which.par = c("a1","d"),
    Wald      = TRUE
  )
  
  dif_table <- as.data.frame(dif_irt)
  dif_table$item_id <- rownames(dif_table)
  
  dif_table <- dif_table %>%
    mutate(
      true_DIF = item_id %in% true_dif_items,
      detected = p < .05
    )
  
  conf_matrix <- table(
    True     = dif_table$true_DIF,
    Detected = dif_table$detected
  )
  
  results_2pl[[i]] <- list(
    magnitude  = datasets[[i]]$magnitude,
    proportion = datasets[[i]]$proportion,
    drift_type = datasets[[i]]$drift_type,
    confusion  = conf_matrix
  )
  
  cat("Dataset", i, "complete\n")
}

saveRDS(
  results_2pl,
  "Outputs/results_2pl_2_3.rds"
)

end.Time <- Sys.time()

print(end.Time - start.Time)