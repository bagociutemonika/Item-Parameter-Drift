library(dplyr)

xgb_summary_rasch_2_3 <- readRDS(
  "Outputs/xgb_summary_rasch_2_3.rds"
)

print(xgb_summary_rasch_2_3)

write.csv(
  xgb_summary_rasch_2_3,
  "Outputs/xgb_summary_rasch_2_3.csv",
  row.names = FALSE
)