

source("03_generate_datasets.R")


# Check drifting items


datasets[[1]]$dif_items

datasets[[2]]$dif_items

datasets[[3]]$dif_items


# Inspect dataset


head(datasets[[1]]$data)

str(datasets[[1]]$data)