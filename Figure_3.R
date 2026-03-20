# Figure 3 -- bar plots of novelty-conventionality profiles by funding and type of science

library(arrow) # read parquet files
library(tidyverse) # data manipulation and ggplots
library(scales) # For percent values on ggplots
library(patchwork) # multi-panel figures

setwd("~/Library/CloudStorage/OneDrive-Personal/Sciscinet v2")

df_opa_sciscinet_2001_2022 <- read_parquet("df_opa_sciscinet_2001_2022_3_1_26.parquet")

#################################################################################

## Bar plots -- nonmissing only

#################################################################################

# Plot of percent Platypus papers by Science Type and Funding

# 1. Keep only rows with non-missing Novelty_Type, Science_Type, NIH_funding
df_plot <- df_opa_sciscinet_2001_2022 %>%
  filter(
    !is.na(Atyp_10pct_Z),
    !is.na(Science_Type),
    !is.na(NIH_funding)
  ) %>%
  mutate(
    platypus = ifelse(Novelty_Type == "Platypus", 1, 0),
    NIH_funding_label = ifelse(NIH_funding == 1, "NIH-funded", "Not NIH-funded")
  )

# 2. Compute proportion Darwin's Tower by Science_Type and NIH_funding
df_summary <- df_plot %>%
  group_by(Science_Type, NIH_funding_label) %>%
  summarise(
    prop_platypus = mean(platypus),
    .groups = "drop"
  )

# 3. Plot: y-axis in percent
plot_platypus<-ggplot(df_summary,
                      aes(x = Science_Type,
                          y = prop_platypus,
                          fill = NIH_funding_label)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  scale_y_continuous(
    name = "",
    limits = c(0, 0.55),
    breaks = seq(0, 0.55, by = 0.1),
    labels = function(x) paste0(x * 100, "%")
  ) +
  scale_fill_discrete(name = "") +
  xlab("") +
  ggtitle("Platypus") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0, "pt")
  )

# Plot of percent Avant-garde papers by Science Type and Funding

# 1. Keep only rows with non-missing Novelty_Type, Science_Type, NIH_funding
df_plot <- df_opa_sciscinet_2001_2022 %>%
  filter(
    !is.na(Atyp_10pct_Z),
    !is.na(Science_Type),
    !is.na(NIH_funding)
  ) %>%
  mutate(
    avant = ifelse(Novelty_Type == "Avant-garde", 1, 0),
    NIH_funding_label = ifelse(NIH_funding == 1, "NIH-funded", "Not NIH-funded")
  )

# 2. Compute proportion Darwin's Tower by Science_Type and NIH_funding
df_summary <- df_plot %>%
  group_by(Science_Type, NIH_funding_label) %>%
  summarise(
    prop_avant = mean(avant),
    .groups = "drop"
  )

# 3. Plot: y-axis in percent
plot_avant<-ggplot(df_summary,
                   aes(x = Science_Type,
                       y = prop_avant,
                       fill = NIH_funding_label)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  scale_y_continuous(
    name = "",
    limits = c(0, 0.55),
    breaks = seq(0, 0.55, by = 0.1),
    labels = function(x) paste0(x * 100, "%")
  ) +
  scale_fill_discrete(name = "") +
  xlab("") +
  ggtitle("Avant-garde") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )

# Plot of percent Accepted Wisdom papers by Science Type and Funding

# 1. Keep only rows with non-missing Novelty_Type, Science_Type, NIH_funding
df_plot <- df_opa_sciscinet_2001_2022 %>%
  filter(
    !is.na(Atyp_10pct_Z),
    !is.na(Science_Type),
    !is.na(NIH_funding)
  ) %>%
  mutate(
    aw = ifelse(Novelty_Type == "Accepted Wisdom", 1, 0),
    NIH_funding_label = ifelse(NIH_funding == 1, "NIH-funded", "Not NIH-funded")
  )

# 2. Compute proportion Darwin's Tower by Science_Type and NIH_funding
df_summary <- df_plot %>%
  group_by(Science_Type, NIH_funding_label) %>%
  summarise(
    prop_aw = mean(aw),
    .groups = "drop"
  )

# 3. Plot: y-axis in percent
plot_aw<-ggplot(df_summary,
                aes(x = Science_Type,
                    y = prop_aw,
                    fill = NIH_funding_label)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  scale_y_continuous(
    name = "",
    limits = c(0, 0.55),
    breaks = seq(0, 0.55, by = 0.1),
    labels = function(x) paste0(x * 100, "%")
  ) +
  scale_fill_discrete(name = "") +
  xlab("") +
  ggtitle("Accepted Wisdom") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )



# Plot of percent Darwin's Tower papers by Science Type and Funding

# 1. Keep only rows with non-missing Novelty_Type, Science_Type, NIH_funding
df_plot <- df_opa_sciscinet_2001_2022 %>%
  filter(
    !is.na(Novelty_Type),
    !is.na(Science_Type),
    !is.na(NIH_funding)
  ) %>%
  mutate(
    darwin_tower = ifelse(Novelty_Type == "Darwin's Tower", 1, 0),
    NIH_funding_label = ifelse(NIH_funding == 1, "NIH-funded", "Not NIH-funded")
  )

# 2. Compute proportion Darwin's Tower by Science_Type and NIH_funding
df_summary <- df_plot %>%
  group_by(Science_Type, NIH_funding_label) %>%
  summarise(
    prop_darwin = mean(darwin_tower),
    .groups = "drop"
  )

# 3. Plot: y-axis in percent
plot_dt<-ggplot(df_summary,
                aes(x = Science_Type,
                    y = prop_darwin,
                    fill = NIH_funding_label)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  scale_y_continuous(
    name = "",
    limits = c(0, 0.55),
    breaks = seq(0, 0.55, by = 0.1),
    labels = function(x) paste0(x * 100, "%")
  ) +
  scale_fill_discrete(name = "") +
  xlab("") +
  ggtitle("Darwin's Tower") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "none"
  )

plot_platypus + plot_avant + plot_aw + plot_dt + plot_layout(ncol=2)

setwd("~/Library/CloudStorage/OneDrive-Personal/Novelty Paper")

ggsave("Figure 3.jpg", width = 6.665, height = 7.5, units = c("in"), dpi=600)
