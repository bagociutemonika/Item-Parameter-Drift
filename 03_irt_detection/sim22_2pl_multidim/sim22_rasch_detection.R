
# Simulation 2.2 — Rasch DIF Detection

# Load design objects


source("../../01_data_generation/sim22_2pl_multidim/01_design_2pl_md.R")


# Load datasets


datasets_2PL_MD <- readRDS(
  "../../Data/sim22_2pl_md_datasets.rds"
)


# Prepare datasets


prepare_data <- function(dataset_obj){
  
  data_long <- dataset_obj$data
  
  true_dif_items <- dataset_obj$dif_items
  
  # Convert to wide format
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
  
  # Response matrix
  resp_matrix <- data_wide %>%
    dplyr::select(any_of(all_items)) %>%
    as.matrix()
  
  storage.mode(resp_matrix) <- "numeric"
  
  list(
    resp_matrix = resp_matrix,
    group = data_wide$group,
    true_dif_items = true_dif_items
  )
}


# Rasch calibration and Wald DIF


start.Time <- Sys.time()

results_rasch <- list()

for(i in 1:length(datasets_2PL_MD)){
  
  prepared <- prepare_data(
    datasets_2PL_MD[[i]]
  )
  
  resp_matrix <- prepared$resp_matrix
  
  group <- prepared$group
  
  true_dif_items <- prepared$true_dif_items
  
  # Rasch calibration
  mod_rasch_2_2 <- multipleGroup(
    resp_matrix,
    model = 1,
    group = group,
    itemtype = "Rasch",
    SE = TRUE
  )
  
  # Wald DIF test
  dif_irt <- mirt::DIF(
    mod_rasch_2_2,
    which.par = "d",
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
  results_rasch[[i]] <- list(
    magnitude = datasets_2PL_MD[[i]]$magnitude,
    proportion = datasets_2PL_MD[[i]]$proportion,
    drift_type = datasets_2PL_MD[[i]]$drift_type,
    confusion = conf_matrix
  )
}

end.Time <- Sys.time()

print(end.Time - start.Time)