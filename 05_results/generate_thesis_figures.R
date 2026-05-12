

set.seed(1234)

library(tidyverse)
library(patchwork)
library(viridis)

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
  theme_bw(base_size = 11) +
    theme(
      panel.grid.minor    = element_blank(),
      panel.grid.major.x  = element_blank(),
      strip.background    = element_rect(fill = "grey92", colour = "grey70"),
      strip.text          = element_text(face = "bold", size = 9),
      legend.position     = "bottom",
      legend.title        = element_text(face = "bold", size = 9),
      legend.text         = element_text(size = 8),
      legend.key.width    = unit(1.2, "cm"),
      axis.title          = element_text(face = "bold", size = 9),
      axis.text           = element_text(size = 8),
      plot.title          = element_text(face = "bold", size = 11),
      plot.subtitle       = element_text(size = 9, colour = "grey40"),
      plot.caption        = element_text(size = 7, colour = "grey50"),
      panel.spacing       = unit(0.5, "lines")
    )
}



# STEP 1 — ASSEMBLE MASTER DATA FRAME



tag <- function(df, method, simulation) {
  df %>% mutate(method = method, simulation = simulation)
}

sim11 <- bind_rows(
  tag(summary_table,  "IRT (Rasch)", "1.1"),
  tag(nn_summary,     "NN (Rasch)",  "1.1"),
  tag(xgb_summary,    "XGB (Rasch)", "1.1")
) %>% mutate(drift_type = "b")

sim12 <- bind_rows(
  tag(summary_rasch_1_2, "IRT (Rasch)", "1.2"),
  tag(nn_summary_1_2,    "NN (Rasch)",  "1.2"),
  tag(xgb_summary_1_2,   "XGB (Rasch)", "1.2")
) %>% mutate(drift_type = "b")

sim21 <- bind_rows(
  tag(summary_rasch,         "IRT (Rasch)", "2.1"),
  tag(summary_2pl,           "IRT (2PL)",   "2.1"),
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

master <- bind_rows(sim11, sim12, sim21, sim22, sim23) %>%
  mutate(
    magnitude  = factor(
      magnitude,
      levels = c(0.2, 0.5, 1.0),
      labels = c("0.2", "0.5", "1.0")
    ),
    drift_type = factor(
      drift_type,
      levels = c("b", "a", "ab"),
      labels = c("b-shift", "a-shift", "ab-shift")
    ),
    simulation = factor(
      simulation,
      levels = c("1.1", "1.2", "2.1", "2.2", "2.3")
    ),
    method = factor(method, levels = method_order)
  )



# FIGURE 1A — Heatmap: Simulations 1.1 and 1.2



heatmap_12 <- master %>%
  filter(simulation %in% c("1.1", "1.2")) %>%
  group_by(simulation, magnitude, method) %>%
  summarise(
    mean_sensitivity = mean(sensitivity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sim_label = case_when(
      simulation == "1.1" ~ "Sim 1.1\nRasch DGM",
      simulation == "1.2" ~ "Sim 1.2\nRasch DGM\n+ Multidimensional"
    )
  )

fig1a <- ggplot(
  heatmap_12,
  aes(x = magnitude, y = fct_rev(method), fill = mean_sensitivity)
) +
  geom_tile(colour = "white", linewidth = 1.0) +
  # FIX: adaptive text colour — dark text on bright tiles, white on dark
  geom_text(
    aes(
      label  = sprintf("%.2f", mean_sensitivity),
      colour = mean_sensitivity > 0.6
    ),
    size = 3.5, fontface = "bold"
  ) +
  scale_colour_manual(
    values = c("TRUE" = "grey20", "FALSE" = "white"),
    guide  = "none"
  ) +
  scale_fill_viridis_c(
    option  = "D",
    limits  = c(0, 1),
    breaks  = c(0, 0.25, 0.5, 0.75, 1),
    labels  = c("0", "0.25", "0.50", "0.75", "1"),
    name    = "Mean Sensitivity",
    guide   = guide_colorbar(
      barwidth       = 10,
      barheight      = 0.6,
      title.position = "top",
      title.hjust    = 0.5
    )
  ) +
  facet_wrap(~ sim_label, nrow = 1) +
  labs(
    title    = "A. Simulations 1.1 and 1.2 — Rasch Data Generating Model",
    subtitle = "b-shift (difficulty drift) only. Values averaged across proportions.",
    x        = "Drift Magnitude (logits)",
    y        = NULL
  ) +
  theme_thesis() +
  theme(
    axis.text.y     = element_text(size = 9),
    legend.position = "bottom"
  )



# FIGURE 1B — Heatmap: Simulations 2.1, 2.2, 2.3



heatmap_23 <- master %>%
  filter(simulation %in% c("2.1", "2.2", "2.3")) %>%
  group_by(simulation, drift_type, magnitude, method) %>%
  summarise(
    mean_sensitivity = mean(sensitivity, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sim_label = case_when(
      simulation == "2.1" ~ "Sim 2.1\n2PL DGM",
      simulation == "2.2" ~ "Sim 2.2\n2PL DGM\n+ MD",
      simulation == "2.3" ~ "Sim 2.3\n2PL DGM\n+ MD + Multi-country"
    ),
    sim_label = factor(
      sim_label,
      levels = c(
        "Sim 2.1\n2PL DGM",
        "Sim 2.2\n2PL DGM\n+ MD",
        "Sim 2.3\n2PL DGM\n+ MD + Multi-country"
      )
    )
  )

fig1b <- ggplot(
  heatmap_23,
  aes(x = magnitude, y = fct_rev(method), fill = mean_sensitivity)
) +
  geom_tile(colour = "white", linewidth = 0.8) +
  # FIX: adaptive text colour
  geom_text(
    aes(
      label  = sprintf("%.2f", mean_sensitivity),
      colour = mean_sensitivity > 0.6
    ),
    size = 2.8, fontface = "bold"
  ) +
  scale_colour_manual(
    values = c("TRUE" = "grey20", "FALSE" = "white"),
    guide  = "none"
  ) +
  scale_fill_viridis_c(
    option  = "D",
    limits  = c(0, 1),
    breaks  = c(0, 0.25, 0.5, 0.75, 1),
    labels  = c("0", "0.25", "0.50", "0.75", "1"),
    name    = "Mean Sensitivity",
    guide   = guide_colorbar(
      barwidth       = 10,
      barheight      = 0.6,
      title.position = "top",
      title.hjust    = 0.5
    )
  ) +
  facet_grid(drift_type ~ sim_label) +
  labs(
    title    = "B. Simulations 2.1, 2.2, and 2.3 — 2PL Data Generating Model",
    subtitle = paste0(
      "All three drift types. Values averaged across proportions.\n",
      "MD = multidimensional violation."
    ),
    x        = "Drift Magnitude (logits)",
    y        = NULL
  ) +
  theme_thesis() +
  theme(
    axis.text.y     = element_text(size = 8),
    legend.position = "bottom"
  )

# FIX: removed plot.title from annotation since there is no title
fig1 <- fig1a / fig1b +
  plot_annotation(
    caption = paste0(
      "Colour scale uses the viridis palette (colourblind and greyscale safe). ",
      "Dark = low sensitivity; bright yellow = near-perfect detection."
    ),
    theme = theme(
      plot.caption = element_text(size = 7, colour = "grey50")
    )
  ) +
  plot_layout(heights = c(1, 2.2))



# FIGURE 2 — Sensitivity and specificity line plots



fig2_data <- master %>%
  filter(simulation %in% c("2.1", "2.2", "2.3")) %>%
  group_by(method, drift_type, magnitude) %>%
  summarise(
    mean_sens = mean(sensitivity, na.rm = TRUE),
    mean_spec = mean(specificity, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(
    calibration = if_else(
      str_detect(as.character(method), "Rasch"), "Rasch", "2PL"
    )
  )

p_sens <- ggplot(
  fig2_data,
  aes(
    x        = magnitude,
    y        = mean_sens,
    colour   = method,
    linetype = calibration,
    group    = method
  )
) +
  geom_hline(
    yintercept = 0.80,
    linetype   = "dotted",
    colour     = "grey55",
    linewidth  = 0.5
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5, shape = 19) +
  scale_colour_manual(values = cb_palette, name = "Method") +
  scale_linetype_manual(
    values = c("Rasch" = "solid", "2PL" = "dashed"),
    name   = "Calibration"
  ) +
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = seq(0, 1, 0.25)
  ) +
  # FIX: x is a factor so use label string not numeric position
  annotate(
    "text", x = "0.2", y = 0.84,
    label  = "0.80 reference",
    size   = 2.5,
    colour = "grey45",
    hjust  = 0
  ) +
  facet_wrap(~ drift_type, nrow = 1) +
  labs(
    title = "A. Sensitivity",
    x     = NULL,
    y     = "Mean Sensitivity"
  ) +
  theme_thesis()

p_spec <- ggplot(
  fig2_data,
  aes(
    x        = magnitude,
    y        = mean_spec,
    colour   = method,
    linetype = calibration,
    group    = method
  )
) +
  geom_hline(
    yintercept = 0.95,
    linetype   = "dotted",
    colour     = "grey55",
    linewidth  = 0.5
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5, shape = 19) +
  scale_colour_manual(values = cb_palette, name = "Method") +
  scale_linetype_manual(
    values = c("Rasch" = "solid", "2PL" = "dashed"),
    name   = "Calibration"
  ) +
  scale_y_continuous(
    limits = c(0.55, 1.05),
    breaks = seq(0.6, 1, 0.1)
  ) +
  # FIX: x is a factor so use label string not numeric position
  annotate(
    "text", x = "0.2", y = 0.965,
    label  = "0.95 reference",
    size   = 2.5,
    colour = "grey45",
    hjust  = 0
  ) +
  facet_wrap(~ drift_type, nrow = 1) +
  labs(
    title = "B. Specificity",
    x     = "Drift Magnitude (logits)",
    y     = "Mean Specificity"
  ) +
  theme_thesis()

# FIX: removed plot.title from annotation since there is no title
fig2 <- p_sens / p_spec +
  plot_annotation(
    subtitle = paste0(
      "Averaged across Simulations 2.1, 2.2, and 2.3 and all proportions.\n",
      "Solid lines = Rasch calibration; dashed lines = 2PL calibration."
    ),
    theme = theme(
      plot.subtitle = element_text(size = 9, colour = "grey40")
    )
  ) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")



# FIGURE 3 — Robustness across simulation conditions



fig3_data <- master %>%
  filter(
    !(simulation %in% c("1.1", "1.2") & drift_type != "b-shift")
  ) %>%
  group_by(method, simulation, drift_type, magnitude) %>%
  summarise(
    mean_sens = mean(sensitivity, na.rm = TRUE),
    .groups   = "drop"
  )

fig3 <- ggplot(
  fig3_data,
  aes(
    x      = simulation,
    y      = mean_sens,
    colour = method,
    group  = method
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.0, shape = 19) +
  scale_colour_manual(values = cb_palette, name = "Method") +
  scale_y_continuous(
    limits = c(0, 1.05),
    breaks = seq(0, 1, 0.25)
  ) +
  facet_grid(
    magnitude ~ drift_type,
    labeller = labeller(
      magnitude = function(x) paste0(x, " logits")
    ),
    switch = "y"
  ) +
  labs(
    # FIX: added title
    title    = "Robustness of Detection Across Simulation Conditions",
    subtitle = paste0(
      "Each point is mean sensitivity averaged over proportions.\n",
      "Left to right: increasing complexity. ",
      "Flat lines indicate robustness to assumption violations."
    ),
    x        = "Simulation",
    y        = "Mean Sensitivity",
    caption  = paste0(
      "b-shift results only available for Simulations 1.1 and 1.2. ",
      "a-shift and ab-shift begin at Simulation 2.1."
    )
  ) +
  theme_thesis() +
  theme(
    strip.placement  = "outside",
    panel.spacing.x  = unit(0.8, "lines"),
    panel.spacing.y  = unit(0.5, "lines")
  )



# FIGURE 4 — Specificity in Simulation 2.3



fig4_data <- master %>%
  filter(simulation == "2.3") %>%
  group_by(method, drift_type, magnitude) %>%
  summarise(
    mean_spec = mean(specificity, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(
    calibration = if_else(
      str_detect(as.character(method), "Rasch"), "Rasch", "2PL"
    )
  )

fig4 <- ggplot(
  fig4_data,
  aes(
    x        = magnitude,
    y        = mean_spec,
    colour   = method,
    linetype = calibration,
    group    = method
  )
) +
  geom_hline(
    yintercept = 0.95,
    linetype   = "dotted",
    colour     = "grey55",
    linewidth  = 0.5
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5, shape = 19) +
  scale_colour_manual(values = cb_palette, name = "Method") +
  scale_linetype_manual(
    values = c("Rasch" = "solid", "2PL" = "dashed"),
    name   = "Calibration"
  ) +
  scale_y_continuous(
    limits = c(0.55, 1.05),
    breaks = seq(0.6, 1, 0.1)
  ) +
  facet_wrap(~ drift_type, nrow = 1) +
  labs(
    # FIX: added title
    title    = "Specificity in Simulation 2.3 (Multi-Country Setting)",
    subtitle = paste0(
      "Averaged across proportions of drifted items.\n",
      "ML methods maintain stable specificity; ",
      "IRT Rasch specificity drops under ab-shift drift."
    ),
    x        = "Drift Magnitude (logits)",
    y        = "Mean Specificity",
    caption  = "Dotted reference line at specificity = 0.95."
  ) +
  theme_thesis()

# ============================================================
# EXPORT FIGURES
# ============================================================

dir.create("Figures", showWarnings = FALSE)

ggsave(
  "Figures/fig1_heatmaps.png",
  fig1,
  width = 12,
  height = 10,
  dpi = 300
)

ggsave(
  "Figures/fig2_sensitivity_specificity.png",
  fig2,
  width = 11,
  height = 9,
  dpi = 300
)

ggsave(
  "Figures/fig3_robustness.png",
  fig3,
  width = 12,
  height = 8,
  dpi = 300
)

ggsave(
  "Figures/fig4_specificity_sim23.png",
  fig4,
  width = 10,
  height = 5,
  dpi = 300
)

cat("\nAll thesis figures exported successfully.\n")