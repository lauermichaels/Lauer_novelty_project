# Figure 6 -- likelihood of being highly cited bar plot

library(arrow) # read parquet files
library(tidyverse) # data manipulation and ggplots

df_opa_sciscinet_2001_2022 <- read_parquet("df_opa_sciscinet_2001_2022_3_1_26.parquet")

df <- df_opa_sciscinet_2001_2022 

# Filter: non-missing Atyp_10pct_Z, Novelty_Type, Science_Type, Highly_Cited
df_plot <- df %>%
  filter(
    !is.na(Atyp_10pct_Z),
    !is.na(Novelty_Type),
    !is.na(Science_Type),
    !is.na(Highly_Cited),
    !is.na(relative_citation_ratio)
  ) %>%
  mutate(
    # Make sure Highly_Cited is numeric 0/1
    Highly_Cited = as.integer(Highly_Cited),
    # Label funding status for facetting
    funding_group = ifelse(NIH_funding == 1, "NIH-funded", "Not NIH-funded")
  )

# Summarise: proportion Highly_Cited by Novelty_Type, Science_Type, funding_group
df_summary <- df_plot %>%
  group_by(funding_group, Novelty_Type, Science_Type) %>%
  summarise(
    prop_hc = mean(Highly_Cited),
    .groups = "drop"
  )

# Ensure facet order: non-NIH on left, NIH on right
df_summary$funding_group <- factor(
  df_summary$funding_group,
  levels = c("Not NIH-funded", "NIH-funded")
)

# Plot
ggplot(
  df_summary,
  aes(x = Novelty_Type, y = prop_hc, fill = Science_Type)) +
  theme_bw() +
  geom_col(position = position_dodge(width = 0.7)) +
  facet_wrap(~ funding_group, nrow = 1) +
  scale_y_continuous(
    name   = "Percent Highly Cited",
    limits = c(0, 0.20),
    breaks = seq(0, 0.20, by = 0.05),
    labels = function(x) paste0(x * 100, "%")
  ) +
  xlab("") +
  scale_fill_discrete(name = "") +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )

ggsave("Figure 6.jpg", width = 6.665, height = 3.75, units = c("in"), dpi=600)

