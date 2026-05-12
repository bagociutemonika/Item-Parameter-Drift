

# Simulation 2.1 — Save Datasets




source("03_generate_datasets.R")

saveRDS(
  datasets_2PL,
  file = "../../Data/sim21_2pl_datasets.rds"
)