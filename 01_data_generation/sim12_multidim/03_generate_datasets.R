

source("01_multidim_design.R")

source("02_generate_multidim.R")


# Drift conditions


drift_magnitudes <- c(0.2, 0.5, 1.0)

drift_proportions <- c(0.1, 0.2, 0.3)


# Generate datasets


start.Time <- Sys.time()

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
      booklets = booklets,
      item_domain = item_domain
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

end.Time <- Sys.time()

print(end.Time - start.Time)
