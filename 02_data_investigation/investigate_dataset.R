
# GENERIC DATA INVESTIGATION FUNCTION


library(tidyverse)
library(ggplot2)

investigate_dataset <- function(
    data_long,
    dif_items,
    J_total,
    sim_name
){
  

  # OUTPUT FOLDERS

  
  fig_dir <- paste0(
    "02_data_investigation/outputs/",
    sim_name,
    "/figures"
  )
  
  tab_dir <- paste0(
    "02_data_investigation/outputs/",
    sim_name,
    "/tables"
  )
  
  dir.create(fig_dir,
             recursive = TRUE,
             showWarnings = FALSE)
  
  dir.create(tab_dir,
             recursive = TRUE,
             showWarnings = FALSE)
  

  # 1. BOOKLET DESIGN

  
  design_df <- data_long %>%
    dplyr::select(person_id,
                  booklet,
                  starts_with("I")) %>%
    pivot_longer(
      starts_with("I"),
      names_to = "item_id",
      values_to = "response"
    ) %>%
    mutate(administered = !is.na(response)) %>%
    group_by(booklet, item_id) %>%
    summarise(
      administered = any(administered),
      .groups = "drop"
    )
  
  design_df$item_id <- factor(
    design_df$item_id,
    levels = paste0("I", 1:J_total)
  )
  
  p_booklet <- ggplot(
    design_df,
    aes(x = item_id,
        y = booklet,
        fill = administered)
  ) +
    geom_tile(color = "grey80",
              linewidth = 0.2) +
    coord_flip() +
    scale_fill_manual(
      values = c(
        "TRUE"  = "#2C7BB6",
        "FALSE" = "#F0F0F0"
      )
    ) +
    theme_minimal(base_size = 12)
  
  ggsave(
    paste0(fig_dir,
           "/booklet_design_",
           sim_name,
           ".png"),
    p_booklet,
    width = 10,
    height = 6,
    dpi = 300
  )
  

  # 2. BOOKLET STATISTICS

  
  booklet_stats <- data_long %>%
    pivot_longer(
      starts_with("I"),
      names_to = "item_id",
      values_to = "response"
    ) %>%
    filter(!is.na(response)) %>%
    group_by(booklet, person_id) %>%
    summarise(
      total_score = sum(response),
      .groups = "drop"
    ) %>%
    group_by(booklet) %>%
    summarise(
      n_persons  = n(),
      mean_score = mean(total_score),
      sd_score   = sd(total_score),
      max_score  = max(total_score),
      .groups = "drop"
    )
  
  write.csv(
    booklet_stats,
    paste0(tab_dir,
           "/booklet_stats_",
           sim_name,
           ".csv"),
    row.names = FALSE
  )
  

  # 3. ITEM STATISTICS

  
  item_stats <- data_long %>%
    pivot_longer(
      starts_with("I"),
      names_to = "item_id",
      values_to = "response"
    ) %>%
    filter(!is.na(response)) %>%
    group_by(booklet, item_id) %>%
    summarise(
      pvalue = mean(response),
      sd     = sd(response),
      n      = n(),
      .groups = "drop"
    )
  
  write.csv(
    item_stats,
    paste0(tab_dir,
           "/item_stats_",
           sim_name,
           ".csv"),
    row.names = FALSE
  )
  

  # 4. DIF ITEM STATISTICS

  
  df <- data_long %>%
    pivot_longer(
      cols = starts_with("I"),
      names_to = "item_id",
      values_to = "response"
    )
  
  item_stats_group <- df %>%
    group_by(group, item_id) %>%
    summarise(
      pvalue = mean(response,
                    na.rm = TRUE),
      .groups = "drop"
    )
  
  dif_results <- item_stats_group %>%
    filter(item_id %in% dif_items)
  
  write.csv(
    dif_results,
    paste0(tab_dir,
           "/dif_item_stats_",
           sim_name,
           ".csv"),
    row.names = FALSE
  )
  

  # 5. P-VALUE PLOT

  
  p_pvalue <- ggplot(
    item_stats,
    aes(
      x = reorder(item_id, pvalue),
      y = pvalue,
      colour = booklet
    )
  ) +
    geom_point(size = 2) +
    coord_flip() +
    theme_bw()
  
  ggsave(
    paste0(fig_dir,
           "/pvalue_plot_",
           sim_name,
           ".png"),
    p_pvalue,
    width = 9,
    height = 7,
    dpi = 300
  )
  
  cat("Completed:", sim_name, "\n")
}