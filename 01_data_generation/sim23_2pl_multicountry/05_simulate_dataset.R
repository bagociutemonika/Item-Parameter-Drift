
# Simulation 2.3 — Dataset Generator


simulate_dataset <- function(a_true,
                             b_true,
                             a_2018,
                             b_2018){
  
  sim_data <- list()
  
  for(cycle in cycles){
    
    for(country in countries){
      
      theta_matrix <- generate_theta_md(
        country,
        cycle,
        N_per_group
      )
      
      a_group <- if(cycle == 2015){
        
        a_true
        
      } else {
        
        a_2018
      }
      
      b_group <- if(cycle == 2015){
        
        b_true
        
      } else {
        
        b_2018
      }
      
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
            country  = country,
            cycle    = cycle,
            booklet  = bk,
            person_id = paste(
              country,
              cycle,
              p,
              sep = "_"
            )
          )
        
        sim_data[[paste(
          country,
          cycle,
          p,
          sep = "_"
        )]] <- df_person
      }
    }
  }
  
  bind_rows(sim_data)
}