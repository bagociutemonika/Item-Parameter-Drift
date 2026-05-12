
library(dplyr)
library(knitr)
library(kableExtra)

# ============================================================
# CREATE OUTPUT FOLDER
# ============================================================

if (!dir.exists("Outputs/latex_tables")) {
  dir.create("Outputs/latex_tables", recursive = TRUE)
}

# ============================================================
# FUNCTION TO EXPORT LATEX TABLE
# ============================================================

export_latex_table <- function(df,
                               caption,
                               label,
                               file_name) {
  
  # ----------------------------------------------------------
  # SIM 1.1 and 1.2 tables (9 rows)
  # ----------------------------------------------------------
  
  if (!"drift_type" %in% names(df)) {
    
    df_export <- df %>%
      mutate(
        Condition = paste0(
          magnitude,
          " / ",
          proportion * 100,
          "\\\\%"
        )
      ) %>%
      select(
        Condition,
        sensitivity,
        specificity,
        accuracy
      )
    
    col_names <- c(
      "Condition",
      "Sensitivity",
      "Specificity",
      "Accuracy"
    )
    
    align_vals <- "lccc"
    
  } else {
    
    # --------------------------------------------------------
    # SIM 2.1 / 2.2 / 2.3 tables (27 rows)
    # --------------------------------------------------------
    
    df_export <- df %>%
      mutate(
        Condition = paste0(
          magnitude,
          " / ",
          proportion * 100,
          "\\\\%"
        )
      ) %>%
      select(
        drift_type,
        Condition,
        sensitivity,
        specificity,
        accuracy
      )
    
    col_names <- c(
      "Drift Type",
      "Condition",
      "Sensitivity",
      "Specificity",
      "Accuracy"
    )
    
    align_vals <- "llccc"
  }
  
  # ----------------------------------------------------------
  # ROUND VALUES
  # ----------------------------------------------------------
  
  df_export <- df_export %>%
    mutate(
      sensitivity = round(sensitivity, 3),
      specificity = round(specificity, 3),
      accuracy    = round(accuracy, 3)
    )
  
  # ----------------------------------------------------------
  # BUILD TABLE
  # ----------------------------------------------------------
  
  latex_table <-
    kbl(
      df_export,
      format = "latex",
      booktabs = TRUE,
      longtable = FALSE,
      caption = caption,
      label = label,
      align = align_vals,
      col.names = col_names
    ) %>%
    kable_styling(
      latex_options = c("hold_position")
    )
  
  # ----------------------------------------------------------
  # SAVE
  # ----------------------------------------------------------
  
  writeLines(
    latex_table,
    paste0(
      "Outputs/latex_tables/",
      file_name,
      ".tex"
    )
  )
  
  cat("Saved:", file_name, "\n")
}

# ============================================================
# SIMULATION 1.1
# ============================================================

export_latex_table(
  summary_table_1_1,
  "Simulation 1.1 Wald results.",
  "tab:sim11_wald",
  "sim11_wald"
)

export_latex_table(
  nn_summary,
  "Simulation 1.1 neural network results.",
  "tab:sim11_nn",
  "sim11_nn"
)

export_latex_table(
  xgb_summary,
  "Simulation 1.1 XGBoost results.",
  "tab:sim11_xgb",
  "sim11_xgb"
)

# ============================================================
# SIMULATION 1.2
# ============================================================

export_latex_table(
  summary_rasch_1_2,
  "Simulation 1.2 Wald results.",
  "tab:sim12_wald",
  "sim12_wald"
)

export_latex_table(
  nn_summary_1_2,
  "Simulation 1.2 neural network results.",
  "tab:sim12_nn",
  "sim12_nn"
)

export_latex_table(
  xgb_summary_1_2,
  "Simulation 1.2 XGBoost results.",
  "tab:sim12_xgb",
  "sim12_xgb"
)

# ============================================================
# SIMULATION 2.1
# ============================================================

export_latex_table(
  summary_rasch_2_1,
  "Simulation 2.1 Wald Rasch results.",
  "tab:sim21_rasch",
  "sim21_rasch"
)

export_latex_table(
  summary_2pl_1,
  "Simulation 2.1 Wald 2PL results.",
  "tab:sim21_2pl",
  "sim21_2pl"
)

export_latex_table(
  nn_summary_rasch_2_1,
  "Simulation 2.1 neural network Rasch results.",
  "tab:sim21_nn_rasch",
  "sim21_nn_rasch"
)

export_latex_table(
  nn_summary_2pl_2_1,
  "Simulation 2.1 neural network 2PL results.",
  "tab:sim21_nn_2pl",
  "sim21_nn_2pl"
)

export_latex_table(
  xgb_summary_rasch_2_1,
  "Simulation 2.1 XGBoost Rasch results.",
  "tab:sim21_xgb_rasch",
  "sim21_xgb_rasch"
)

export_latex_table(
  xgb_summary_2pl_2_1,
  "Simulation 2.1 XGBoost 2PL results.",
  "tab:sim21_xgb_2pl",
  "sim21_xgb_2pl"
)

# ============================================================
# SIMULATION 2.2
# ============================================================

export_latex_table(
  summary_rasch_2_2,
  "Simulation 2.2 Wald Rasch results.",
  "tab:sim22_rasch",
  "sim22_rasch"
)

export_latex_table(
  summary_2pl_2_2,
  "Simulation 2.2 Wald 2PL results.",
  "tab:sim22_2pl",
  "sim22_2pl"
)

export_latex_table(
  nn_summary_rasch_2_2,
  "Simulation 2.2 neural network Rasch results.",
  "tab:sim22_nn_rasch",
  "sim22_nn_rasch"
)

export_latex_table(
  nn_summary_2pl_2_2,
  "Simulation 2.2 neural network 2PL results.",
  "tab:sim22_nn_2pl",
  "sim22_nn_2pl"
)

export_latex_table(
  xgb_summary_rasch_2_2,
  "Simulation 2.2 XGBoost Rasch results.",
  "tab:sim22_xgb_rasch",
  "sim22_xgb_rasch"
)

export_latex_table(
  xgb_summary_2pl_2_2,
  "Simulation 2.2 XGBoost 2PL results.",
  "tab:sim22_xgb_2pl",
  "sim22_xgb_2pl"
)

# ============================================================
# SIMULATION 2.3
# ============================================================

export_latex_table(
  summary_rasch_2_3,
  "Simulation 2.3 Wald Rasch results.",
  "tab:sim23_rasch",
  "sim23_rasch"
)

export_latex_table(
  summary_2pl_2_3,
  "Simulation 2.3 Wald 2PL results.",
  "tab:sim23_2pl",
  "sim23_2pl"
)

export_latex_table(
  nn_summary_rasch_2_3,
  "Simulation 2.3 neural network Rasch results.",
  "tab:sim23_nn_rasch",
  "sim23_nn_rasch"
)

export_latex_table(
  nn_summary_2pl_2_3,
  "Simulation 2.3 neural network 2PL results.",
  "tab:sim23_nn_2pl",
  "sim23_nn_2pl"
)

export_latex_table(
  xgb_summary_rasch_2_3,
  "Simulation 2.3 XGBoost Rasch results.",
  "tab:sim23_xgb_rasch",
  "sim23_xgb_rasch"
)

export_latex_table(
  xgb_summary_2pl_2_3,
  "Simulation 2.3 XGBoost 2PL results.",
  "tab:sim23_xgb_2pl",
  "sim23_xgb_2pl"
)

