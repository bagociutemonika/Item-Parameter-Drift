
# Simulation 2.2 — 2PL Results


# Load 2PL DIF results


source("../03_irt_detection/sim22_2pl_multidim/sim22_2pl_detection.R")


# Build summary table


summary_2pl_2_2 <- data.frame()

for(i in 1:length(results_2pl)){
  
  cm <- results_2pl[[i]]$confusion
  
  # Safe extraction
  TN <- if(!is.null(cm["FALSE", "FALSE"])) cm["FALSE", "FALSE"] else 0
  
  FP <- if(!is.null(cm["FALSE", "TRUE"])) cm["FALSE", "TRUE"] else 0
  
  FN <- if(!is.null(cm["TRUE", "FALSE"])) cm["TRUE", "FALSE"] else 0
  
  TP <- if(!is.null(cm["TRUE", "TRUE"])) cm["TRUE", "TRUE"] else 0
  
  summary_2pl_2_2 <- rbind(
    summary_2pl_2_2,
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


# Performance metrics


summary_2pl_2_2 <- summary_2pl_2_2 %>%
  mutate(
    sensitivity = TP / (TP + FN),
    specificity = TN / (TN + FP),
    accuracy = (TP + TN) / (TP + TN + FP + FN)
  )

summary_2pl_2_2