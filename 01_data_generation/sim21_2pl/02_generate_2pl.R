# Simulation 2.1 — 2PL Generator


library(tidyverse)

source("01_design_2pl.R")



# 2PL response generator



generate_2PL <- function(theta, a_vec, b_vec){
  
  N <- length(theta)
  
  J <- length(a_vec)
  
  resp <- matrix(NA, nrow = N, ncol = J)
  
  for(i in 1:J){
    
    p <- 1 / (1 + exp(-a_vec[i] * (theta - b_vec[i])))
    
    resp[,i] <- rbinom(N, 1, p)
  }
  
  colnames(resp) <- names(a_vec)
  
  resp
}



# Drift generator



generate_drift_2PL <- function(
    a_true,
    b_true,
    anchor_items,
    drift_magnitude,
    drift_proportion,
    drift_type
){
  
  n_drift <- round(length(anchor_items) * drift_proportion)
  
  dif_items <- sample(anchor_items, n_drift)
  
  a_2018 <- a_true
  
  b_2018 <- b_true
  
  if(drift_type == "a"){
    
    a_2018[dif_items] <- a_true[dif_items] + drift_magnitude
  }
  
  if(drift_type == "b"){
    
    b_2018[dif_items] <- b_true[dif_items] + drift_magnitude
  }
  
  if(drift_type == "ab"){
    
    a_2018[dif_items] <- a_true[dif_items] + drift_magnitude
    
    b_2018[dif_items] <- b_true[dif_items] + drift_magnitude
  }
  
  list(
    a_2018 = a_2018,
    b_2018 = b_2018,
    dif_items = dif_items
  )
}



# Dataset simulation



simulate_dataset_2PL <- function(
    a_true,
    b_true,
    a_2018,
    b_2018,
    N_per_group,
    groups,
    booklets
){
  
  sim_data <- list()
  
  for(g in groups){
    
    theta <- rnorm(N_per_group)
    
    a_group <- if(g == "2015") a_true else a_2018
    
    b_group <- if(g == "2015") b_true else b_2018
    
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
      
      resp <- generate_2PL(
        theta[p],
        a_subset,
        b_subset
      )
      
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