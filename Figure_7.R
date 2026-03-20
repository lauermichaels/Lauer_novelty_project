# Figure 7 -- Proportions of novel, conventional, or both according to citation quantile

library(arrow) # read parquet files
library(tidyverse) # data manipulation and ggplots

df_opa_sciscinet_2001_2022 <- read_parquet("df_opa_sciscinet_2001_2022_3_1_26.parquet")

df <- df_opa_sciscinet_2001_2022 

df_plot <- df %>%
  filter(!is.na(relative_citation_ratio)) %>%
  filter(!is.na(Atyp_10pct_Z)) %>%
  mutate(
    Novel = Novelty_Type %in% c("Darwin's Tower", "Avant-garde"),
    Conventional = Novelty_Type %in% c("Accepted Wisdom", "Darwin's Tower"),
    Darwin = Novelty_Type == "Darwin's Tower",
  ) %>%
  # Create 20 RCR quantiles: 1 = bottom 5%, 20 = top 5%
  mutate(
    rcr_ventile = ntile(relative_citation_ratio, 20)
  )

# Compute mean (proportion) Novel and Conventional by ventile and Science_Type
df_summary <- df_plot %>%
  group_by(Science_Type, rcr_ventile) %>%
  summarise(
    prop_novel = mean(Novel, na.rm = TRUE),
    prop_conventional = mean(Conventional, na.rm = TRUE),
    prop_darwin = mean(Darwin, na.rm = TRUE),
    .groups = "drop"
  )

# Overall (all Science_Types combined)
df_all <- df_plot %>%
  group_by(rcr_ventile) %>%
  summarise(
    prop_novel = mean(Novel, na.rm = TRUE),
    prop_conventional = mean(Conventional, na.rm = TRUE),
    prop_darwin = mean(Darwin, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Science_Type = "All")

# Combine All + Science-Type-specific
df_combined <- bind_rows(
  df_all,
  df_summary
)

# Reshape to long format: one row per (Science_Type, rcr_ventile, measure)
df_long <- df_combined %>%
  pivot_longer(
    cols = c(prop_novel, prop_conventional, prop_darwin),
    names_to = "Measure",
    values_to = "Proportion"
  ) %>%
  mutate(
    Measure = recode(
      Measure,
      prop_novel = "Novel",
      prop_conventional = "Conventional",
      prop_darwin = "Darwin's Tower",
    ),
    Percent = Proportion * 100
  ) %>%
  mutate(Measure = factor(Measure, levels = c("Novel","Conventional","Darwin's Tower")))

# Ensure ordering of panels
df_long$Science_Type <- factor(
  df_long$Science_Type,
  levels = c(
    "All",
    "Fundamental",
    "Mixed",
    "Human-focused"
  )
)

# 2x2 facet line plot
ggplot(df_long, aes(x = rcr_ventile, y = Percent, color = Measure)) +
  geom_line(size = 2) +
  geom_point(size = 3) +
  scale_x_continuous(
    breaks = 1:20,
    minor_breaks = NULL
  ) +
  labs(
    x = "Relative Citation Ratio quantile (1 = bottom 5%, 20 = top 5%)",
    y = "Percent of papers",
    color = "",
    title = ""
  ) +
  facet_wrap(~ Science_Type, nrow = 2, ncol = 2) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 14),
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(face = "bold")
  )

setwd("~/Library/CloudStorage/OneDrive-Personal/Novelty Paper")

ggsave("Figure 7.jpg", width = 10, height = 7.5, units = c("in"), dpi=600)


