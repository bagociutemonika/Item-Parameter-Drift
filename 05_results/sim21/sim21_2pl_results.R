

# Simulation 2.1 — 2PL Results


source("../03_irt_detection/sim21_2pl/sim21_2pl_detection.R")

summary_2pl_1 <- data.frame()

for(i in 1:length(results_2pl)){
  
  cm <- results_2pl[[i]]$confusion
  
  TN <- cm["FALSE","FALSE"]
  
  FP <- cm["FALSE","TRUE"]
  
  FN <- cm["TRUE","FALSE"]
  
  TP <- cm["TRUE","TRUE"]
  
  summary_2pl_1 <- rbind(
    summary_2pl_1,
    data.frame(
      drift_type = results_2pl[[i]]$drift_type,
      magnitude = results_2pl[[i]]$magnitude,
      proportion = results_2pl[[i]]$proportion,
      TP = TP,
      FP = FP,
      FN = FN,
      TN = TN
    )
  )
}

summary_2pl_1 <- summary_2pl_1 %>%
  mutate(
    sensitivity = TP/(TP+FN),
    specificity = TN/(TN+FP),
    accuracy = (TP+TN)/(TP+TN+FP+FN)
  )

summary_2pl_1