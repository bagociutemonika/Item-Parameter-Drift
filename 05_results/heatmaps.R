

library(tidyverse)
library(viridis)
library(forcats)


# CREATE OUTPUT FOLDER


dir.create(
  "Outputs/figures",
  recursive = TRUE,
  showWarnings = FALSE
)


# COLOUR PALETTE


cb_palette <- c(
  "IRT (Rasch)" = "#E69F00",
  "IRT (2PL)"   = "#D55E00",
  "NN (Rasch)"  = "#0072B2",
  "NN (2PL)"    = "#56B4E9",
  "XGB (Rasch)" = "#009E73",
  "XGB (2PL)"   = "#CC79A7"
)

method_order <- names(cb_palette)


# SHARED THEME


theme_thesis <- function() {
  
  theme_bw(base_size = 15) +
    
    theme(
      
      # grids
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      
      # facet strips
      strip.background = element_rect(
        fill   = "grey92",
        colour = "grey70"
      ),
      
      strip.text = element_text(
        face = "bold",
        size = 14
      ),
      
      # legend
      legend.position = "bottom",
      
      legend.title = element_text(
        face = "bold",
        size = 12
      ),
      
      legend.text = element_text(
        size = 11
      ),
      
      legend.key.width = unit(1.6, "cm"),
      
      # axes
      axis.title = element_text(
        face = "bold",
        size = 13
      ),
      
      axis.text = element_text(
        size = 12
      ),
      
      # titles
      plot.title = element_text(
        face = "bold",
        size = 16
      ),
      
      plot.subtitle = element_text(
        size = 12,
        colour = "grey40"
      ),
      
      plot.caption = element_text(
        size = 10,
        colour = "grey50"
      ),
      
      panel.spacing = unit(1, "lines")
    )
}

# ============================================================
# HELPER FUNCTION
# ============================================================

tag <- function(df, method, simulation) {
  
  df %>%
    mutate(
      method     = method,
      simulation = simulation
    )
}


# BUILD MASTER DATASET


sim11 <- bind_rows(
  
  tag(summary_table_1_1, "IRT (Rasch)", "1.1"),
  tag(nn_summary,        "NN (Rasch)",  "1.1"),
  tag(xgb_summary,       "XGB (Rasch)", "1.1")
  
) %>%
  mutate(drift_type = "b")

sim12 <- bind_rows(
  
  tag(summary_rasch_1_2, "IRT (Rasch)", "1.2"),
  tag(nn_summary_1_2,    "NN (Rasch)",  "1.2"),
  tag(xgb_summary_1_2,   "XGB (Rasch)", "1.2")
  
) %>%
  mutate(drift_type = "b")

sim21 <- bind_rows(
  
  tag(summary_rasch_2_1,     "IRT (Rasch)", "2.1"),
  tag(summary_2pl_1,         "IRT (2PL)",   "2.1"),
  
  tag(nn_summary_rasch_2_1,  "NN (Rasch)",  "2.1"),
  tag(nn_summary_2pl_2_1,    "NN (2PL)",    "2.1"),
  
  tag(xgb_summary_rasch_2_1, "XGB (Rasch)", "2.1"),
  tag(xgb_summary_2pl_2_1,   "XGB (2PL)",   "2.1")
  
)

sim22 <- bind_rows(
  
  tag(summary_rasch_2_2,     "IRT (Rasch)", "2.2"),
  tag(summary_2pl_2_2,       "IRT (2PL)",   "2.2"),
  
  tag(nn_summary_rasch_2_2,  "NN (Rasch)",  "2.2"),
  tag(nn_summary_2pl_2_2,    "NN (2PL)",    "2.2"),
  
  tag(xgb_summary_rasch_2_2, "XGB (Rasch)", "2.2"),
  tag(xgb_summary_2pl_2_2,   "XGB (2PL)",   "2.2")
  
)

sim23 <- bind_rows(
  
  tag(summary_rasch_2_3,     "IRT (Rasch)", "2.3"),
  tag(summary_2pl_2_3,       "IRT (2PL)",   "2.3"),
  
  tag(nn_summary_rasch_2_3,  "NN (Rasch)",  "2.3"),
  tag(nn_summary_2pl_2_3,    "NN (2PL)",    "2.3"),
  
  tag(xgb_summary_rasch_2_3, "XGB (Rasch)", "2.3"),
  tag(xgb_summary_2pl_2_3,   "XGB (2PL)",   "2.3")
  
)

master <- bind_rows(
  sim11,
  sim12,
  sim21,
  sim22,
  sim23
) %>%
  
  mutate(
    
    magnitude = factor(
      magnitude,
      levels = c(0.2, 0.5, 1.0),
      labels = c("0.2", "0.5", "1.0")
    ),
    
    drift_type = factor(
      drift_type,
      levels = c("b", "a", "ab"),
      labels = c(
        "b-shift",
        "a-shift",
        "ab-shift"
      )
    ),
    
    simulation = factor(
      simulation,
      levels = c(
        "1.1",
        "1.2",
        "2.1",
        "2.2",
        "2.3"
      )
    ),
    
    method = factor(
      method,
      levels = method_order
    )
  )

# ============================================================
# HEATMAP — SIM 1.1 & 1.2
# ============================================================

heatmap_12 <- master %>%
  
  filter(
    simulation %in% c("1.1", "1.2")
  ) %>%
  
  group_by(
    simulation,
    magnitude,
    method
  ) %>%
  
  summarise(
    mean_sensitivity =
      mean(sensitivity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  mutate(
    
    sim_label = case_when(
      
      simulation == "1.1" ~
        "Simulation 1.1\nRasch DGM",
      
      simulation == "1.2" ~
        "Simulation 1.2\nRasch + MD"
    )
  )

fig1a <- ggplot(
  
  heatmap_12,
  
  aes(
    x    = magnitude,
    y    = fct_rev(method),
    fill = mean_sensitivity
  )
) +
  
  geom_tile(
    colour   = "white",
    linewidth = 1.3
  ) +
  
  geom_text(
    
    aes(
      label  = sprintf("%.2f", mean_sensitivity),
      colour = mean_sensitivity > 0.6
    ),
    
    size     = 6,
    fontface = "bold"
  ) +
  
  scale_colour_manual(
    values = c(
      "TRUE"  = "grey20",
      "FALSE" = "white"
    ),
    guide = "none"
  ) +
  
  scale_fill_viridis_c(
    option = "D",
    limits = c(0, 1),
    name   = "Mean Sensitivity"
  ) +
  
  facet_wrap(
    ~sim_label,
    nrow = 1
  ) +
  
  labs(
    
    title =
      "Simulations 1.1 and 1.2",
    
    subtitle =
      "Mean sensitivity averaged across drift proportions.",
    
    x = "Drift Magnitude (logits)",
    y = NULL
  ) +
  
  theme_thesis()

# ============================================================
# HEATMAP — SIM 2.1 TO 2.3
# ============================================================

heatmap_23 <- master %>%
  
  filter(
    simulation %in% c(
      "2.1",
      "2.2",
      "2.3"
    )
  ) %>%
  
  group_by(
    simulation,
    drift_type,
    magnitude,
    method
  ) %>%
  
  summarise(
    mean_sensitivity =
      mean(sensitivity, na.rm = TRUE),
    .groups = "drop"
  )

fig1b <- ggplot(
  
  heatmap_23,
  
  aes(
    x    = magnitude,
    y    = fct_rev(method),
    fill = mean_sensitivity
  )
) +
  
  geom_tile(
    colour   = "white",
    linewidth = 1.3
  ) +
  
  geom_text(
    
    aes(
      label  = sprintf("%.2f", mean_sensitivity),
      colour = mean_sensitivity > 0.6
    ),
    
    size     = 5,
    fontface = "bold"
  ) +
  
  scale_colour_manual(
    values = c(
      "TRUE"  = "grey20",
      "FALSE" = "white"
    ),
    guide = "none"
  ) +
  
  scale_fill_viridis_c(
    option = "D",
    limits = c(0, 1),
    name   = "Mean Sensitivity"
  ) +
  
  facet_grid(
    drift_type ~ simulation
  ) +
  
  labs(
    
    title =
      "Simulations 2.1–2.3",
    
    subtitle =
      paste(
        "Mean sensitivity averaged across",
        "drift proportions."
      ),
    
    x = "Drift Magnitude (logits)",
    y = NULL
  ) +
  
  theme_thesis()


# DISPLAY FIGURES


fig1a
fig1b


# SAVE FIGURE 1A


ggsave(
  
  filename =
    "Outputs/figures/heatmap_sim11_12.pdf",
  
  plot   = fig1a,
  
  width  = 14,
  height = 6,
  
  dpi = 600
)


# SAVE FIGURE 1B


ggsave(
  
  filename =
    "Outputs/figures/heatmap_sim21_23.pdf",
  
  plot   = fig1b,
  
  width  = 18,
  height = 10,
  
  dpi = 600
)

cat("\nFigures saved to Outputs/figures/\n")