
# Simulation 1.1 — Save Datasets


# Load generated datasets
source("04_generate_datasets.R")

# Save datasets
saveRDS(
  datasets,
  file = "../../data/sim11_rasch_datasets.rds"
)