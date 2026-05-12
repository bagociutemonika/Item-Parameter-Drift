
# Simulation 2.3 — Drift Generator


generate_drift_2PL <- function(a_true,
                               b_true,
                               anchor_items,
                               drift_magnitude,
                               drift_proportion,
                               drift_type){
  
  n_drift <- round(
    length(anchor_items) * drift_proportion
  )
  
  dif_items <- sample(
    anchor_items,
    n_drift
  )
  
  a_2018 <- a_true
  
  b_2018 <- b_true
  

  # a-shift

  
  if(drift_type == "a"){
    
    a_2018[dif_items] <-
      a_true[dif_items] + drift_magnitude
  }
  

  # b-shift

  
  if(drift_type == "b"){
    
    b_2018[dif_items] <-
      b_true[dif_items] + drift_magnitude
  }
  

  # ab-shift

  
  if(drift_type == "ab"){
    
    a_2018[dif_items] <-
      a_true[dif_items] + drift_magnitude
    
    b_2018[dif_items] <-
      b_true[dif_items] + drift_magnitude
  }
  
  list(
    a_2018   = a_2018,
    b_2018   = b_2018,
    dif_items = dif_items
  )
}