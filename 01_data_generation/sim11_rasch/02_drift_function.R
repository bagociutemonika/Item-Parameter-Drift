# Drift Function (sim 1.1)

generate_drift <- function(b_true,anchor_items,
                           drift_magnitude,
                           drift_proportion){
  
n_drift <- round(length(anchor_items) * drift_proportion)
  
dif_items <- sample(anchor_items, n_drift)
  
b_2018 <- b_true
b_2018[dif_items] <- b_true[dif_items] + drift_magnitude
  
list(
    b_2018 = b_2018,
    dif_items = dif_items
  )
}