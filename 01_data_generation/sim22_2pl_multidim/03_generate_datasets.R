
# Simulation 2.2 — Generate Datasets

source("02_generate_2pl_md.R")

drift_magnitudes  <- c(0.2,0.5,1.0)

drift_proportions <- c(0.1,0.2,0.3)

drift_types <- c("a","b","ab")

datasets_2PL_MD <- list()

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
      
      a_2018 <- drift_obj$a_2018
      b_2018 <- drift_obj$b_2018
      
      sim_data <- list()
      
      for(g in groups){
        
        theta_matrix <- MASS::mvrnorm(
          N_per_group,
          mu = c(0,0,0),
          Sigma = Sigma
        )
        
        a_group <- if(g=="2015") a_true else a_2018
        b_group <- if(g=="2015") b_true else b_2018
        
        booklet_assignment <- sample(
          names(booklets),
          size = N_per_group,
          replace = TRUE
        )
        
        for(p in 1:N_per_group){
          
          bk <- booklet_assignment[p]
          
          items_bk <- booklets[[bk]]
          
          a_subset <- a_group[items_bk]
          b_subset <- b_group[items_bk]
          
          dim_subset <- dim_assignment[items_bk]
          
          resp <- generate_2PL_MD(
            theta_matrix[p,],
            a_subset,
            b_subset,
            dim_subset
          )
          
          df_person <- as.data.frame(resp) %>%
            mutate(
              group = g,
              booklet = bk,
              person_id = paste0(g,"_",p)
            )
          
          sim_data[[paste(g,p,sep="_")]] <- df_person
        }
      }
      
      data_long <- bind_rows(sim_data)
      
      datasets_2PL_MD[[counter]] <- list(
        data = data_long,
        magnitude = mag,
        proportion = prop,
        drift_type = type,
        dif_items = drift_obj$dif_items
      )
      
      counter <- counter + 1
    }
  }
}