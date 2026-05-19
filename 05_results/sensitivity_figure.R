
##Sensitivity by drift type


sim21_combined <- bind_rows(
  summary_rasch_2_1        %>% mutate(method = "IRT Rasch"),
  summary_2pl_1          %>% mutate(method = "IRT 2PL"),
  nn_summary_rasch_2_1 %>% mutate(method = "NN (Rasch θ)"),
  nn_summary_2pl_2_1   %>% mutate(method = "NN (2PL θ)"),
  xgb_summary_rasch_2_1 %>% mutate(method = "XGBoost (Rasch θ)"),
  xgb_summary_2pl_2_1   %>% mutate(method = "XGBoost (2PL θ)")
) %>%
  mutate(
    magnitude  = factor(magnitude,  levels = c(0.2, 0.5, 1.0)),
    proportion = factor(proportion, levels = sort(unique(proportion))),
    drift_type = factor(drift_type, levels = c("a", "b", "ab"),
                        labels = c("a-shift", "b-shift", "ab-shift")),
    method     = factor(method, levels = c("IRT Rasch", "IRT 2PL",
                                           "NN (Rasch θ)", "NN (2PL θ)",
                                           "XGBoost (Rasch θ)", "XGBoost (2PL θ)"))
  ) %>%
  # Average sensitivity across proportions for cleaner visualisation
  group_by(drift_type, magnitude, method) %>%
  summarise(mean_sensitivity = mean(sensitivity), .groups = "drop")

fig3 <- ggplot(sim21_combined,
               aes(x = magnitude, y = mean_sensitivity,
                   color = method, group = method, linetype = method)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c(
    "IRT Rasch"        = "#1b7837",
    "IRT 2PL"          = "#a6dba0",
    "NN (Rasch θ)"     = "#2166ac",
    "NN (2PL θ)"       = "#74add1",
    "XGBoost (Rasch θ)"= "#d73027",
    "XGBoost (2PL θ)"  = "#f46d43"
  ), name = "Method") +
  scale_linetype_manual(values = c(
    "IRT Rasch"        = "solid",
    "IRT 2PL"          = "dashed",
    "NN (Rasch θ)"     = "solid",
    "NN (2PL θ)"       = "dashed",
    "XGBoost (Rasch θ)"= "solid",
    "XGBoost (2PL θ)"  = "dashed"
  ), name = "Method") +
  facet_wrap(~drift_type, ncol = 3) +
  scale_y_continuous(limits = c(0, 1.05), breaks = c(0, 0.5, 1)) +
  labs(
    title    = "Figure 3. Mean Sensitivity by Drift Type, Magnitude, and Method",
    subtitle = "Simulation 2.1 (2PL data) — averaged across proportions",
    x        = "Drift Magnitude (logits)",
    y        = "Mean Sensitivity"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    strip.text    = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    legend.text     = element_text(size = 9)
  ) +
  guides(color    = guide_legend(nrow = 2),
         linetype = guide_legend(nrow = 2))


fig3




# ============================================================
# SAVE FIGURE
# ============================================================

ggsave(
  filename = "outputs/figures/Sensitivity_By_Drift_Type.pdf",
  plot     = fig3,
  width    = 12,
  height   = 5,
  dpi      = 300
)

ggsave(
  filename = "outputs/figures/Sensitivity_By_Drift_Type.png",
  plot     = fig3,
  width    = 12,
  height   = 5,
  dpi      = 300
)