
# Simulation 2.3 — Generate Datasets
# 2PL + Multidimensionality + Multiple Countries




# Load design and functions


source("01_design_2pl_multicountry.R")

source("02_generate_theta_md.R")

source("03_generate_2PL_MD.R")

source("04_generate_drift_2PL.R")

source("05_simulate_dataset.R")


# Generate datasets


datasets <- list()

counter <- 1

for(type in drift_types){
  
  for(mag in drift_magnitudes){
    
    for(prop in drift_proportions){
      
      drift_obj <- generate_drift_2PL(
        a_true            = a_true,
        b_true            = b_true,
        anchor_items      = anchor_items,
        drift_magnitude   = mag,
        drift_proportion  = prop,
        drift_type        = type
      )
      
      data_sim <- simulate_dataset(
        a_true  = a_true,
        b_true  = b_true,
        a_2018  = drift_obj$a_2018,
        b_2018  = drift_obj$b_2018
      )
      
      datasets[[counter]] <- list(
        data        = data_sim,
        magnitude   = mag,
        proportion  = prop,
        drift_type  = type,
        dif_items   = drift_obj$dif_items
      )
      
      cat(
        "Dataset",
        counter,
        "complete\n"
      )
      
      counter <- counter + 1
    }
  }
}


# Final check


length(datasets)