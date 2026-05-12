
# Simulation 1.1 — Dataset Checks


# Load generated datasets
source("04_generate_datasets.R")


# Check drifted items


datasets[[1]]$dif_items
datasets[[2]]$dif_items
datasets[[3]]$dif_items
datasets[[4]]$dif_items
datasets[[5]]$dif_items
datasets[[6]]$dif_items
datasets[[7]]$dif_items
datasets[[8]]$dif_items
datasets[[9]]$dif_items


head(datasets[[1]]$data)

# Dataset structure
str(datasets[[1]]$data)

# Number of drifting items
length(datasets[[1]]$dif_items)