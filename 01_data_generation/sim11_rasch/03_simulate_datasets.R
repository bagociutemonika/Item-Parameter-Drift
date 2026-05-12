

# dataset simulation (sim 1.1)

simulate_dataset <- function(
    b_true,
    b_2018,
    N_per_group,
    groups,
    booklets
){
  
  sim_data <- list()
  
  for(g in groups){
    
    theta <- rnorm(N_per_group)
    
    b_group <- if(g == "2015") b_true else b_2018
    
    booklet_assignment <- sample(
      names(booklets),
      size = N_per_group,
      replace = TRUE
    )
    
    for(p in 1:N_per_group){
      
      bk <- booklet_assignment[p]
      
      items_bk <- booklets[[bk]]
      
      b_subset <- b_group[items_bk]
      
      resp <- generate_rasch(theta[p], b_subset)
      
      df_person <- as.data.frame(resp) %>%
        mutate(
          group = g,
          booklet = bk,
          person_id = paste0(g, "_", p)
        )
      
      sim_data[[paste(g, p, sep = "_")]] <- df_person
    }
  }
  
  bind_rows(sim_data)
}