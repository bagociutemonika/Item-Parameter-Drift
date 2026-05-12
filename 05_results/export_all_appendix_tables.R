

library(dplyr)
library(tidyr)
library(xtable)
library(knitr)
library(kableExtra)



# HELPER FUNCTION

format_table <- function(df, caption, label){
  
  out <- df %>%
    dplyr::select(
      all_of(c(
        "drift_type",
        "magnitude",
        "proportion",
        "sensitivity",
        "specificity",
        "accuracy"
      ))
    ) %>%
    mutate(
      drift_type  = as.character(drift_type),
      magnitude   = sprintf("%.1f", magnitude),
      proportion  = sprintf("%.0f%%", proportion * 100),
      sensitivity = sprintf("%.2f", sensitivity),
      specificity = sprintf("%.2f", specificity),
      accuracy    = sprintf("%.2f", accuracy)
    ) %>%
    rename(
      `Drift Type`  = drift_type,
      `Magnitude`   = magnitude,
      `Proportion`  = proportion,
      `Sensitivity` = sensitivity,
      `Specificity` = specificity,
      `Accuracy`    = accuracy
    )
  
  xtable(
    out,
    caption = caption,
    label   = label,
    digits  = 2
  )
}

# START EXPORT

# ============================================================
# SIMULATION 1.1
# ============================================================

print(
  format_table(
    summary_table,
    "Simulation 1.1: IRT Rasch detection results.",
    "tab:sim11_irt"
  ),
  include.rownames = FALSE,
  booktabs = TRUE,
  comment = FALSE
)

print(
  format_table(
    nn_summary,
    "Simulation 1.1: Neural Network detection results.",
    "tab:sim11_nn"
  ),
  include.rownames = FALSE,
  booktabs = TRUE,
  comment = FALSE
)

print(
  format_table(
    xgb_summary,
    "Simulation 1.1: XGBoost detection results.",
    "tab:sim11_xgb"
  ),
  include.rownames = FALSE,
  booktabs = TRUE,
  comment = FALSE
)

# ============================================================
# SIMULATION 1.2
# ============================================================

print(
  format_table(
    summary_rasch_1_2,
    "Simulation 1.2: IRT Rasch detection results.",
    "tab:sim12_irt"
  ),
  include.rownames = FALSE,
  booktabs = TRUE,
  comment = FALSE
)

print(
  format_table(
    nn_summary_1_2,
    "Simulation 1.2: Neural Network detection results.",
    "tab:sim12_nn"
  ),
  include.rownames = FALSE,
  booktabs = TRUE,
  comment = FALSE
)

print(
  format_table(
    xgb_summary_1_2,
    "Simulation 1.2: XGBoost detection results.",
    "tab:sim12_xgb"
  ),
  include.rownames = FALSE,
  booktabs = TRUE,
  comment = FALSE
)

# ============================================================
# SIMULATION 2.1
# ============================================================

tables_21 <- list(
  
  list(
    summary_rasch,
    "Simulation 2.1: IRT Rasch calibration.",
    "tab:sim21_irt_rasch"
  ),
  
  list(
    summary_2pl,
    "Simulation 2.1: IRT 2PL calibration.",
    "tab:sim21_irt_2pl"
  ),
  
  list(
    nn_summary_rasch_2_1,
    "Simulation 2.1: Neural Network Rasch theta.",
    "tab:sim21_nn_rasch"
  ),
  
  list(
    nn_summary_2pl_2_1,
    "Simulation 2.1: Neural Network 2PL theta.",
    "tab:sim21_nn_2pl"
  ),
  
  list(
    xgb_summary_rasch_2_1,
    "Simulation 2.1: XGBoost Rasch theta.",
    "tab:sim21_xgb_rasch"
  ),
  
  list(
    xgb_summary_2pl_2_1,
    "Simulation 2.1: XGBoost 2PL theta.",
    "tab:sim21_xgb_2pl"
  )
)

for(tbl in tables_21){
  
  print(
    format_table(
      tbl[[1]],
      tbl[[2]],
      tbl[[3]]
    ),
    include.rownames = FALSE,
    booktabs = TRUE,
    comment = FALSE
  )
  
  cat("\n\n")
}

# ============================================================
# SIMULATION 2.2
# ============================================================


tables_22 <- list(
  
  list(
    summary_rasch_2_2,
    "Simulation 2.2: IRT Rasch calibration.",
    "tab:sim22_irt_rasch"
  ),
  
  list(
    summary_2pl_2_2,
    "Simulation 2.2: IRT 2PL calibration.",
    "tab:sim22_irt_2pl"
  ),
  
  list(
    nn_summary_rasch_2_2,
    "Simulation 2.2: Neural Network Rasch theta.",
    "tab:sim22_nn_rasch"
  ),
  
  list(
    nn_summary_2pl_2_2,
    "Simulation 2.2: Neural Network 2PL theta.",
    "tab:sim22_nn_2pl"
  ),
  
  list(
    xgb_summary_rasch_2_2,
    "Simulation 2.2: XGBoost Rasch theta.",
    "tab:sim22_xgb_rasch"
  ),
  
  list(
    xgb_summary_2pl_2_2,
    "Simulation 2.2: XGBoost 2PL theta.",
    "tab:sim22_xgb_2pl"
  )
)

for(tbl in tables_22){
  
  print(
    format_table(
      tbl[[1]],
      tbl[[2]],
      tbl[[3]]
    ),
    include.rownames = FALSE,
    booktabs = TRUE,
    comment = FALSE
  )
  
  cat("\n\n")
}

# ============================================================
# SIMULATION 2.3
# ============================================================

tables_23 <- list(
  
  list(
    summary_rasch_2_3,
    "Simulation 2.3: IRT Rasch calibration.",
    "tab:sim23_irt_rasch"
  ),
  
  list(
    summary_2pl_2_3,
    "Simulation 2.3: IRT 2PL calibration.",
    "tab:sim23_irt_2pl"
  ),
  
  list(
    nn_summary_rasch_2_3,
    "Simulation 2.3: Neural Network Rasch theta.",
    "tab:sim23_nn_rasch"
  ),
  
  list(
    nn_summary_2pl_2_3,
    "Simulation 2.3: Neural Network 2PL theta.",
    "tab:sim23_nn_2pl"
  ),
  
  list(
    xgb_summary_rasch_2_3,
    "Simulation 2.3: XGBoost Rasch theta.",
    "tab:sim23_xgb_rasch"
  ),
  
  list(
    xgb_summary_2pl_2_3,
    "Simulation 2.3: XGBoost 2PL theta.",
    "tab:sim23_xgb_2pl"
  )
)

for(tbl in tables_23){
  
  print(
    format_table(
      tbl[[1]],
      tbl[[2]],
      tbl[[3]]
    ),
    include.rownames = FALSE,
    booktabs = TRUE,
    comment = FALSE
  )
  
  cat("\n\n")
}


sink()

cat("\n")
cat("Appendix tables exported successfully.\n")
cat("Saved to:\n")
cat("Outputs/appendix_tables/all_appendix_tables.tex\n")