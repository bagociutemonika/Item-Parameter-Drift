



library(tidyverse)
library(mirt)


# Load simulation design


source("../../01_data_generation/sim11_rasch/01_design.R")


# Load datasets


datasets <- readRDS(
  "../../Data/sim11_rasch_datasets.rds"
)


# Calibration + Wald DIF function


analyze_dataset <- function(dataset_obj){
  
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
    select(all_of(all_items)) %>%
    as.matrix()
  
  storage.mode(resp_matrix) <- "numeric"
  
  # Rasch calibration
  mod <- multipleGroup(
    resp_matrix,
    model = 1,
    group = data_wide$group,
    itemtype = "Rasch",
    SE = TRUE
  )
  
  # Wald DIF detection
  dif_irt <- mirt::DIF(
    mod,
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
  
  # Confusion matrix
  conf_matrix <- table(
    True = dif_table$true_DIF,
    Detected = dif_table$detected
  )
  
  return(list(
    dif_table = dif_table,
    conf_matrix = conf_matrix
  ))
}



# Run DIF detection across all datasets



start.Time <- Sys.time()

results <- list()

for(i in 1:9){
  
  res <- analyze_dataset(datasets[[i]])
  
  results[[i]] <- list(
    magnitude = datasets[[i]]$magnitude,
    proportion = datasets[[i]]$proportion,
    confusion = res$conf_matrix,
    dif_table = res$dif_table
  )
}

end.Time <- Sys.time()

print(end.Time - start.Time)



# Quick checks



results[[1]]$confusion

head(results[[1]]$dif_table)