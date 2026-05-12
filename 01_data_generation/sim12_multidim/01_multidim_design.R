
library(tidyverse)
library(MASS)

# Load Simulation 1.1 design
source("../sim11_rasch/01_design.R")


# Domains


domains <- c("algebra", "geometry", "comb")

# Item-domain assignment


item_domain <- data.frame(
  item_id = all_items,
  domain = rep(domains, length.out = J_total)
)


# Correlation matrix


rho <- matrix(
  c(
    1.0, 0.8, 0.8,
    0.8, 1.0, 0.8,
    0.8, 0.8, 1.0
  ),
  nrow = 3
)