library(dplyr)

results_2pl <- readRDS(
  "Outputs/results_2pl_2_3.rds"
)


# Safe extraction function


safe_extract <- function(cm, row, col){
  
  if(row %in% rownames(cm) &&
     col %in% colnames(cm)){
    
    return(as.integer(cm[row, col]))
    
  } else {
    
    return(0L)
    
  }
}


# Build summary table


summary_2pl_2_3 <- data.frame()

for(i in 1:length(results_2pl)){
  
  cm <- results_2pl[[i]]$confusion
  
  summary_2pl_2_3 <- rbind(
    summary_2pl_2_3,
    data.frame(
      drift_type = results_2pl[[i]]$drift_type,
      magnitude  = results_2pl[[i]]$magnitude,
      proportion = results_2pl[[i]]$proportion,
      
      TP = safe_extract(cm, "TRUE",  "TRUE"),
      FP = safe_extract(cm, "FALSE", "TRUE"),
      FN = safe_extract(cm, "TRUE",  "FALSE"),
      TN = safe_extract(cm, "FALSE", "FALSE")
    )
  )
}


# Performance metrics


summary_2pl_2_3 <- summary_2pl_2_3 %>%
  mutate(
    sensitivity = TP / (TP + FN),
    specificity = TN / (TN + FP),
    accuracy    = (TP + TN) / (TP + TN + FP + FN)
  )

print(summary_2pl_2_3)


# Save outputs


write.csv(
  summary_2pl_2_3,
  "Outputs/summary_2pl_2_3.csv",
  row.names = FALSE
)

saveRDS(
  summary_2pl_2_3,
  "Outputs/summary_2pl_2_3.rds"
)