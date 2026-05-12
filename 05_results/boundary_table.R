
library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

# ============================================================
# STEP 1 — LABEL ALL SUMMARY TABLES
# ============================================================

tag <- function(df, method){
  df %>%
    mutate(method = method)
}

all_summaries <- bind_rows(
  
  # =========================
  # Simulation 1.1
  # =========================
  tag(summary_table_1_1,  "Wald test"),
  tag(nn_summary,     "Neural Network"),
  tag(xgb_summary,    "XGBoost"),
  
  # =========================
  # Simulation 1.2
  # =========================
  tag(summary_rasch_1_2, "Wald test"),
  tag(nn_summary_1_2,    "Neural Network"),
  tag(xgb_summary_1_2,   "XGBoost"),
  
  # =========================
  # Simulation 2.1
  # =========================
  tag(summary_rasch_2_1,         "Wald test"),
  tag(summary_2pl_1,           "Wald test"),
  
  tag(nn_summary_rasch_2_1,  "Neural Network"),
  tag(nn_summary_2pl_2_1,    "Neural Network"),
  
  tag(xgb_summary_rasch_2_1, "XGBoost"),
  tag(xgb_summary_2pl_2_1,   "XGBoost"),
  
  # =========================
  # Simulation 2.2
  # =========================
  tag(summary_rasch_2_2,     "Wald test"),
  tag(summary_2pl_2_2,       "Wald test"),
  
  tag(nn_summary_rasch_2_2,  "Neural Network"),
  tag(nn_summary_2pl_2_2,    "Neural Network"),
  
  tag(xgb_summary_rasch_2_2, "XGBoost"),
  tag(xgb_summary_2pl_2_2,   "XGBoost"),
  
  # =========================
  # Simulation 2.3
  # =========================
  tag(summary_rasch_2_3,     "Wald test"),
  tag(summary_2pl_2_3,       "Wald test"),
  
  tag(nn_summary_rasch_2_3,  "Neural Network"),
  tag(nn_summary_2pl_2_3,    "Neural Network"),
  
  tag(xgb_summary_rasch_2_3, "XGBoost"),
  tag(xgb_summary_2pl_2_3,   "XGBoost")
)

# ============================================================
# STEP 2 — CREATE RANGE TABLE
# ============================================================

boundary_table <- all_summaries %>%
  group_by(method, magnitude) %>%
  summarise(
    
    sensitivity = paste0(
      sprintf("%.2f", min(sensitivity, na.rm = TRUE)),
      "--",
      sprintf("%.2f", max(sensitivity, na.rm = TRUE))
    ),
    
    specificity = paste0(
      sprintf("%.2f", min(specificity, na.rm = TRUE)),
      "--",
      sprintf("%.2f", max(specificity, na.rm = TRUE))
    ),
    
    accuracy = paste0(
      sprintf("%.2f", min(accuracy, na.rm = TRUE)),
      "--",
      sprintf("%.2f", max(accuracy, na.rm = TRUE))
    ),
    
    .groups = "drop"
    
  ) %>%
  arrange(method, magnitude)

# View table in R
boundary_table

# ============================================================
# STEP 3 — EXPORT LATEX TABLE
# ============================================================

boundary_table %>%
  rename(
    Method      = method,
    Magnitude   = magnitude,
    Sensitivity = sensitivity,
    Specificity = specificity,
    Accuracy    = accuracy
  ) %>%
  kable(
    format    = "latex",
    booktabs  = TRUE,
    caption   = paste(
      "Ranges of sensitivity, specificity, and accuracy",
      "across drift magnitudes, averaged over all",
      "simulation conditions and drift proportions."
    ),
    label     = "tab:boundary",
    align     = c("l", "c", "c", "c", "c"),
    col.names = c(
      "Method",
      "Magnitude",
      "Sensitivity",
      "Specificity",
      "Accuracy"
    ),
    escape = FALSE
  ) %>%
  kable_styling(
    latex_options = c("hold_position")
  ) %>%
  collapse_rows(
    columns = 1,
    latex_hline = "major"
  ) %>%
  cat()