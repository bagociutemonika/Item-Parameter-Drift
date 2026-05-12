
# Simulation 2.2 — Multidimensional 2PL Generator


source("01_design_2pl_md.R")

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

# Multidimensional 2PL generator


generate_2PL_MD <- function(
    theta_vec,
    a_vec,
    b_vec,
    dims
){
  
  J <- length(a_vec)
  
  resp <- matrix(NA,1,J)
  
  for(i in 1:J){
    
    theta <- theta_vec[dims[i]]
    
    p <- 1/(1+exp(-a_vec[i]*(theta-b_vec[i])))
    
    resp[,i] <- rbinom(1,1,p)
  }
  
  colnames(resp) <- names(a_vec)
  
  resp
}