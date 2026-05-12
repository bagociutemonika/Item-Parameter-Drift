
# Simulation 2.1 — Generate Datasets


source("02_generate_2pl.R")

# Experimental conditions


drift_magnitudes <- c(0.2, 0.5, 1.0)

drift_proportions <- c(0.1, 0.2, 0.3)

drift_types <- c("a", "b", "ab")



# Generate datasets



start.Time <- Sys.time()

datasets_2PL <- list()

counter <- 1

for(type in drift_types){
  
  for(mag in drift_magnitudes){
    
    for(prop in drift_proportions){
      
      drift_obj <- generate_drift_2PL(
        a_true = a_true,
        b_true = b_true,
        anchor_items = anchor_items,
        drift_magnitude = mag,
        drift_proportion = prop,
        drift_type = type
      )
      
      data_sim <- simulate_dataset_2PL(
        a_true = a_true,
        b_true = b_true,
        a_2018 = drift_obj$a_2018,
        b_2018 = drift_obj$b_2018,
        N_per_group = N_per_group,
        groups = groups,
        booklets = booklets
      )
      
      datasets_2PL[[counter]] <- list(
        data = data_sim,
        magnitude = mag,
        proportion = prop,
        drift_type = type,
        dif_items = drift_obj$dif_items
      )
      
      counter <- counter + 1
    }
  }
}

end.Time <- Sys.time()

print(end.Time - start.Time)