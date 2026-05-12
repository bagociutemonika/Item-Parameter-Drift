


# Simulation 1.1 — Dataset Generation


# Load settings and functions

source("01_design.R")

source("02_drift_function.R")

source("../shared_functions/generate_rasch.R")

source("03_simulate_dataset.R")


# Generate datasets

datasets <- list()

set.seed(2026)

counter <- 1

for(mag in drift_magnitudes){
  
  for(prop in drift_proportions){
    
    drift_obj <- generate_drift(
      b_true = b_true,
      anchor_items = anchor_items,
      drift_magnitude = mag,
      drift_proportion = prop
    )
    
    data_sim <- simulate_dataset(
      b_true = b_true,
      b_2018 = drift_obj$b_2018,
      N_per_group = N_per_group,
      groups = groups,
      booklets = booklets
    )
    
    datasets[[counter]] <- list(
      data = data_sim,
      magnitude = mag,
      proportion = prop,
      dif_items = drift_obj$dif_items
    )
    
    counter <- counter + 1
  }
}