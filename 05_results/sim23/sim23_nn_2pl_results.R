library(dplyr)

nn_summary_2pl_2_3 <- readRDS(
  "Outputs/nn_summary_2pl_2_3.rds"
)

print(nn_summary_2pl_2_3)

write.csv(
  nn_summary_2pl_2_3,
  "Outputs/nn_summary_2pl_2_3.csv",
  row.names = FALSE
)