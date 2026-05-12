

# Simulation 2.2 — Design
# 2PL Multidimensional Data Generation


library(tidyverse)

library(MASS)

set.seed(2026)

J_total  <- 75
J_anchor <- 15
J_non    <- J_total - J_anchor

N_per_group <- 2000

groups <- c("2015","2018")

all_items    <- paste0("I",1:J_total)
anchor_items <- paste0("I",1:J_anchor)
non_anchor   <- paste0("I",(J_anchor+1):J_total)

# Booklets

block1 <- non_anchor[1:15]
block2 <- non_anchor[16:30]
block3 <- non_anchor[31:45]
block4 <- non_anchor[46:60]

booklets <- list(
  B1 = c(anchor_items, block1),
  B2 = c(anchor_items, block2),
  B3 = c(anchor_items, block3),
  B4 = c(anchor_items, block4)
)


# Dimension assignment

dim_assignment <- rep(1:3, length.out = J_total)

names(dim_assignment) <- all_items

# Item parameters

a_true <- rlnorm(J_total,0,.2)
names(a_true) <- all_items

b_true <- rnorm(J_total,0,1)
names(b_true) <- all_items

# Correlation matrix

Sigma <- matrix(
  c(1.0,0.8,0.8,
    0.8,1.0,0.8,
    0.8,0.8,1.0),
  nrow=3
)