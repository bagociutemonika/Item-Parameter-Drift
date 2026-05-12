
# Simulation 2.2 — Save datasets


source("03_generate_datasets.R")

saveRDS(
  datasets_2PL_MD,
  file = "../../Data/sim22_2pl_md_datasets.rds"
)