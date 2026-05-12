library(dplyr)

results_rasch <- readRDS(
  "Outputs/results_rasch_2_3.rds"
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


summary_rasch_2_3 <- data.frame()

for(i in 1:length(results_rasch)){
  
  cm <- results_rasch[[i]]$confusion
  
  summary_rasch_2_3 <- rbind(
    summary_rasch_2_3,
    data.frame(
      drift_type = results_rasch[[i]]$drift_type,
      magnitude  = results_rasch[[i]]$magnitude,
      proportion = results_rasch[[i]]$proportion,
      
      TP = safe_extract(cm, "TRUE",  "TRUE"),
      FP = safe_extract(cm, "FALSE", "TRUE"),
      FN = safe_extract(cm, "TRUE",  "FALSE"),
      TN = safe_extract(cm, "FALSE", "FALSE")
    )
  )
}


# Performance metrics


summary_rasch_2_3 <- summary_rasch_2_3 %>%
  mutate(
    sensitivity = TP / (TP + FN),
    specificity = TN / (TN + FP),
    accuracy    = (TP + TN) / (TP + TN + FP + FN)
  )

print(summary_rasch_2_3)


# Save outputs


write.csv(
  summary_rasch_2_3,
  "Outputs/summary_rasch_2_3.csv",
  row.names = FALSE
)

saveRDS(
  summary_rasch_2_3,
  "Outputs/summary_rasch_2_3.rds"
)