
# Simulation 2.3 — Save Datasets



# Load generated datasets


source("06_generate_datasets.R")


# Save datasets


saveRDS(
  datasets,
  file = "../../Data/sim23_2pl_multicountry.rds"
)

