
library(tidyverse)
library(MASS)

source("01_multidim_design.R")

source("../sim11_rasch/02_drift_function.R")


# Multidimensional Rasch generator


generate_multidim <- function(
    theta_vec,
    items,
    b_vec,
    item_domain
){
  
  resp <- rep(NA, length(b_vec))
  
  names(resp) <- names(b_vec)
  
  for(item in items){
    
    domain_i <- item_domain$domain[
      item_domain$item_id == item
    ]
    
    theta <- theta_vec[domain_i]
    
    p <- 1 / (1 + exp(-(theta - b_vec[item])))
    
    resp[item] <- rbinom(1, 1, p)
  }
  
  resp <- as.data.frame(t(resp))
  
  return(resp)
}

# Dataset simulation


simulate_dataset <- function(
    b_true,
    b_2018,
    N_per_group,
    groups,
    booklets,
    item_domain
){
  
  sim_data <- list()
  
  domains <- c("algebra", "geometry", "comb")
  
  for(g in groups){
    
    theta_mat <- MASS::mvrnorm(
      N_per_group,
      mu = c(0,0,0),
      Sigma = rho
    )
    
    colnames(theta_mat) <- domains
    
    b_group <- if(g == "2015") b_true else b_2018
    
    booklet_assignment <- sample(
      names(booklets),
      size = N_per_group,
      replace = TRUE
    )
    
    for(p in 1:N_per_group){
      
      bk <- booklet_assignment[p]
      
      items_bk <- booklets[[bk]]
      
      theta_person <- theta_mat[p,]
      
      resp <- generate_multidim(
        theta_person,
        items_bk,
        b_group,
        item_domain
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
