

source("../03_irt_detection/sim11_rasch/sim11_rasch_detection.R")


# Create summary table


summary_table <- data.frame()

for(i in 1:9){
  
  cm <- results[[i]]$confusion
  
  TN <- cm["FALSE","FALSE"]
  FP <- cm["FALSE","TRUE"]
  FN <- cm["TRUE","FALSE"]
  TP <- cm["TRUE","TRUE"]
  
  summary_table <- rbind(
    summary_table,
    data.frame(
      magnitude = results[[i]]$magnitude,
      proportion = results[[i]]$proportion,
      TP = TP,
      FP = FP,
      FN = FN,
      TN = TN
    )
  )
}


# Performance metrics


summary_table_1_1 <- summary_table %>%
  mutate(
    sensitivity = TP / (TP + FN),
    specificity = TN / (TN + FP),
    accuracy = (TP + TN) / (TP + TN + FP + FN)
  )

summary_table_1_1