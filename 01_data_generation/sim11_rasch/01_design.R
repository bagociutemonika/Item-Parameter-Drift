#1. Desing (sim 1.1)

#libraries
library(tidyverse)
library(dplyr)


set.seed(2026)


# Several booklets, several administrations and 
# anchor and non-anchor items


J_total  <- 75  #items 
J_anchor <-15
J_non    <- J_total - J_anchor # non-anchor items

N_per_group <- 2000

groups <- c("2015", "2018") # administrations

# item labeling 
all_items    <- paste0("I", 1:J_total) # I1 - I75
anchor_items <- paste0("I", 1:J_anchor) # I1 - I15
non_anchor   <- paste0("I", (J_anchor+1):J_total) # I16-I75

# Booklet structure (4 booklets; non-anchor items)

block1 <- non_anchor[1:15]     
block2 <- non_anchor[16:30]    
block3 <- non_anchor[31:45]   
block4 <- non_anchor[46:60]   



# each booklet has 15 non-anchor items
booklets <- list(
  B1 = c(anchor_items, block1),
  B2 = c(anchor_items, block2),
  B3 = c(anchor_items, block3),
  B4 = c(anchor_items, block4)
  
)

# True item parameters 

b_true <- rnorm(J_total, 0, 1) # difficulty drawn from n(0,1),
# scale is centered around 0
# We define the true measurement model
names(b_true) <- all_items


# Experimental conditions

drift_magnitudes  <- c(0.2, 0.5, 1.0)

drift_proportions <- c(0.1, 0.2, 0.3)


