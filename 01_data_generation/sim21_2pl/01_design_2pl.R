
# Simulation 2.1 — 2PL Design


library(tidyverse)

set.seed(2026)


# Design

J_total  <- 75

J_anchor <- 15

J_non <- J_total - J_anchor

N_per_group <- 2000

groups <- c("2015", "2018")



# Item labels



all_items <- paste0("I", 1:J_total)

anchor_items <- paste0("I", 1:J_anchor)

non_anchor <- paste0("I", (J_anchor + 1):J_total)



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



# True item parameters



a_true <- rlnorm(J_total, 0, .2)

names(a_true) <- all_items

b_true <- rnorm(J_total, 0, 1)

names(b_true) <- all_items