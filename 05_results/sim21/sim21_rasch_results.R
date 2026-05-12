
# Simulation 2.1 — Rasch Results


source("../03_irt_detection/sim21_2pl/sim21_rasch_detection.R")

summary_rasch_2_1 <- data.frame()

for(i in 1:length(results_rasch)){
  
  cm <- results_rasch[[i]]$confusion
  
  TN <- cm["FALSE","FALSE"]
  
  FP <- cm["FALSE","TRUE"]
  
  FN <- cm["TRUE","FALSE"]
  
  TP <- cm["TRUE","TRUE"]
  
  summary_rasch_2_1 <- rbind(
    summary_rasch_2_1,
    data.frame(
      drift_type = results_rasch[[i]]$drift_type,
      magnitude = results_rasch[[i]]$magnitude,
      proportion = results_rasch[[i]]$proportion,
      TP = TP,
      FP = FP,
      FN = FN,
      TN = TN
    )
  )
}

summary_rasch_2_1 <- summary_rasch_2_1 %>%
  mutate(
    sensitivity = TP/(TP+FN),
    specificity = TN/(TN+FP),
    accuracy = (TP+TN)/(TP+TN+FP+FN)
  )

summary_rasch_2_1