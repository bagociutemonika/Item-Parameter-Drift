
# Simulation 1.2 — Wald Results


library(tidyverse)

source("../03_irt_detection/sim12_multidim/sim12_multidim_detection.R")


# Summary Table


summary_rasch_1_2 <- data.frame()

for(i in 1:9){
  
  cm <- results_rasch_1_2[[i]]$confusion
  
  TN <- cm["FALSE", "FALSE"]
  
  FP <- cm["FALSE", "TRUE"]
  
  FN <- cm["TRUE", "FALSE"]
  
  TP <- cm["TRUE", "TRUE"]
  
  summary_rasch_1_2 <- rbind(
    summary_rasch_1_2,
    data.frame(
      magnitude = results_rasch_1_2[[i]]$magnitude,
      proportion = results_rasch_1_2[[i]]$proportion,
      TP = TP,
      FP = FP,
      FN = FN,
      TN = TN
    )
  )
}


# Performance Metrics


summary_rasch_1_2 <- summary_rasch_1_2 %>%
  mutate(
    sensitivity = TP / (TP + FN),
    specificity = TN / (TN + FP),
    accuracy = (TP + TN) / (TP + TN + FP + FN)
  )

summary_rasch_1_2