

source("01_data_generation/sim23_design.R")

datasets <- readRDS("Data/sim23_datasets.rds")

# Prepare data function


prepare_data <- function(dataset_obj) {
  
  data_long <- dataset_obj$data %>%
    mutate(group = as.factor(paste(country, cycle, sep = "_")))
  
  true_dif_items <- dataset_obj$dif_items
  
  data_wide <- data_long %>%
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
  
  group <- data_wide$group
  
  list(
    resp_matrix    = resp_matrix,
    group          = group,
    true_dif_items = true_dif_items,
    data_wide      = data_wide
  )
}


# Rasch DIF detection


start.Time <- Sys.time()

results_rasch <- list()

for(i in 1:length(datasets)){
  
  prepared <- prepare_data(datasets[[i]])
  
  resp_matrix    <- prepared$resp_matrix
  true_dif_items <- prepared$true_dif_items
  data_wide      <- prepared$data_wide
  
  cycle_group <- as.factor(as.character(data_wide$cycle))
  
  mod_rasch <- multipleGroup(
    resp_matrix,
    model    = 1,
    group    = cycle_group,
    itemtype = "Rasch",
    SE       = TRUE
  )
  
  dif_irt <- DIF(
    mod_rasch,
    which.par = "d",
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
  
  results_rasch[[i]] <- list(
    magnitude  = datasets[[i]]$magnitude,
    proportion = datasets[[i]]$proportion,
    drift_type = datasets[[i]]$drift_type,
    confusion  = conf_matrix
  )
  
  cat("Dataset", i, "complete\n")
}

saveRDS(
  results_rasch,
  "Outputs/results_rasch_2_3.rds"
)

end.Time <- Sys.time()

print(end.Time - start.Time)