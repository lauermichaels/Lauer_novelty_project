# Supplemental Figure 3 -- Changes over time, mixed papers

library(arrow) # read parquet files
library(tidyverse) # data manipulation and ggplots
library(scales) # For percent values on ggplots
library(patchwork) # multi-panel figures

setwd("~/Library/CloudStorage/OneDrive-Personal/Sciscinet v2")

df_opa_sciscinet_2001_2022 <- read_parquet("df_opa_sciscinet_2001_2022_3_1_26.parquet")

# Our working data frame will be a 5% random sample

set.seed(123)
df <- df_opa_sciscinet_2001_2022 %>%
  sample_frac(0.05) %>%
  filter(Science_Type=="Mixed")

# Novelty NIH over time

df_plot <- df %>%
  filter(NIH_funding==1) %>%
  filter(!is.na(Atyp_10pct_Z)) %>%
  mutate(
    year_group = case_when(
      year >= 2001 & year <= 2009 ~ "2001-2009",
      year >= 2010 & year <= 2019 ~ "2010-2019",
      year >= 2020 & year <= 2022 ~ "2020-2022",
      TRUE                        ~ "Other"
    )
  )

# 1. Construct signed log2-magnitude coordinate
epsilon <- 1e-6  # to avoid log2(0)
df_plot <- df_plot %>%
  mutate(
    abs_val = abs(Atyp_10pct_Z),
    abs_val = ifelse(abs_val < epsilon, epsilon, abs_val),
    log2_mag = log2(abs_val),
    sign_val = sign(Atyp_10pct_Z),
    signed_log2 = log2_mag * sign_val
  )

# 2. Compute ECDF in this transformed coordinate (one curve per year_group)
ecdf_df <- df_plot %>%
  filter(!is.na(signed_log2), !is.na(year_group)) %>%
  arrange(year_group, signed_log2) %>%
  group_by(year_group) %>%
  mutate(
    ecdf_y = row_number() / n()
  ) %>%
  ungroup()

# 3. Define tick positions (in exponent units) and labels
neg_pows <- 8:1          # 8,7, 6,...,1
pos_pows <- 1:12         # 1..12

xticks_vals <- c(-neg_pows, 0, pos_pows)  # -8..-1, 0, 1..12

xticks_labels <- c(
  paste0("-2^", neg_pows),  # these we will parse as math
  "0",
  paste0("2^", pos_pows)
)

# 4. Plot with ggplot
plot_ecdf_novelty_nih<-ggplot(ecdf_df, aes(x = signed_log2, y = ecdf_y, color = year_group)) +
  geom_step(direction = "hv") +
  scale_x_continuous(
    breaks = xticks_vals,
    labels = c(
      parse(text = paste0("-2^", neg_pows)),
      "0",
      parse(text = paste0("2^", pos_pows))
    ),
    limits = c(-8, 12),       # corresponds to -2^8 .. 2^12
    expand = expansion(mult = 0.01)
  ) +
  scale_y_continuous(
    name = "Cumulative Distribution",
    limits = c(0, 1),
    expand = expansion(mult = 0),
    labels = label_percent(accuracy = 1)
  ) +
  labs(
    x = expression(10^th ~ "Percentile Z-score"),
    color = "",
    title = "NIH Novelty (Mixed)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.80, 0.55)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  annotate("text", x = -5, y = 0.85, label = "Novel\nCombinations", size = 3) +
  annotate("text", x = 7.5, y = 0.15, label = "Conventional\nCombinations", size = 3)

# Novelty Non-NIH over time

df_plot <- df %>%
  filter(NIH_funding==0) %>%
  filter(!is.na(Atyp_10pct_Z)) %>%
  mutate(
    year_group = case_when(
      year >= 2001 & year <= 2009 ~ "2001-2009",
      year >= 2010 & year <= 2019 ~ "2010-2019",
      year >= 2020 & year <= 2022 ~ "2020-2022",
      TRUE                        ~ "Other"
    )
  )

# 1. Construct signed log2-magnitude coordinate
epsilon <- 1e-6  # to avoid log2(0)
df_plot <- df_plot %>%
  mutate(
    abs_val = abs(Atyp_10pct_Z),
    abs_val = ifelse(abs_val < epsilon, epsilon, abs_val),
    log2_mag = log2(abs_val),
    sign_val = sign(Atyp_10pct_Z),
    signed_log2 = log2_mag * sign_val
  )

# 2. Compute ECDF in this transformed coordinate (one curve per year_group)
ecdf_df <- df_plot %>%
  filter(!is.na(signed_log2), !is.na(year_group)) %>%
  arrange(year_group, signed_log2) %>%
  group_by(year_group) %>%
  mutate(
    ecdf_y = row_number() / n()
  ) %>%
  ungroup()

# 3. Define tick positions (in exponent units) and labels
neg_pows <- 8:1          # 8,7,...,1
pos_pows <- 1:12         # 1..12

xticks_vals <- c(-neg_pows, 0, pos_pows)  # -8..-1, 0, 1..12

xticks_labels <- c(
  paste0("-2^", neg_pows),  # these we will parse as math
  "0",
  paste0("2^", pos_pows)
)

# 4. Plot with ggplot
plot_ecdf_novelty_non_nih<-ggplot(ecdf_df, aes(x = signed_log2, y = ecdf_y, color = year_group)) +
  geom_step(direction = "hv") +
  scale_x_continuous(
    breaks = xticks_vals,
    labels = c(
      parse(text = paste0("-2^", neg_pows)),
      "0",
      parse(text = paste0("2^", pos_pows))
    ),
    limits = c(-8, 12),       # corresponds to -2^8 .. 2^12
    expand = expansion(mult = 0.01)
  ) +
  scale_y_continuous(
    name = "Cumulative Distribution",
    limits = c(0, 1),
    expand = expansion(mult = 0),
    labels = label_percent(accuracy = 1)
  ) +
  labs(
    x = expression(10^th ~ "Percentile Z-score"),
    color = "",
    title = "Non-NIH Novelty (Mixed)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.80, 0.55)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  annotate("text", x = -5, y = 0.85, label = "Novel\nCombinations", size = 3) +
  annotate("text", x = 7.5, y = 0.15, label = "Conventional\nCombinations", size = 3)

### Conventionality plots

# Conventionality NIH over time

df_plot <- df %>%
  filter(NIH_funding==1) %>%
  filter(!is.na(Atyp_Median_Z)) %>%
  mutate(
    year_group = case_when(
      year >= 2001 & year <= 2009 ~ "2001-2009",
      year >= 2010 & year <= 2019 ~ "2010-2019",
      year >= 2020 & year <= 2022 ~ "2020-2022",
      TRUE                        ~ "Other"
    )
  )

# 1. Construct signed log2-magnitude coordinate
epsilon <- 1e-6  # to avoid log2(0)
df_plot <- df_plot %>%
  mutate(
    abs_val = abs(Atyp_Median_Z),
    abs_val = ifelse(abs_val < epsilon, epsilon, abs_val),
    log2_mag = log2(abs_val),
    sign_val = sign(Atyp_Median_Z),
    signed_log2 = log2_mag * sign_val
  )

# 2. Compute ECDF in this transformed coordinate (one curve per year_group)
ecdf_df <- df_plot %>%
  filter(!is.na(signed_log2), !is.na(year_group)) %>%
  arrange(year_group, signed_log2) %>%
  group_by(year_group) %>%
  mutate(
    ecdf_y = row_number() / n()
  ) %>%
  ungroup()

# 3. Define tick positions (in exponent units) and labels
neg_pows <- 8:1          # 8,7, 6,...,1
pos_pows <- 1:12         # 1..12

xticks_vals <- c(-neg_pows, 0, pos_pows)  # -8..-1, 0, 1..12

xticks_labels <- c(
  paste0("-2^", neg_pows),  # these we will parse as math
  "0",
  paste0("2^", pos_pows)
)

# 4. Plot with ggplot
plot_ecdf_conventionality_nih<-ggplot(ecdf_df, aes(x = signed_log2, y = ecdf_y, color = year_group)) +
  geom_step(direction = "hv") +
  scale_x_continuous(
    breaks = xticks_vals,
    labels = c(
      parse(text = paste0("-2^", neg_pows)),
      "0",
      parse(text = paste0("2^", pos_pows))
    ),
    limits = c(-8, 12),       # corresponds to -2^8 .. 2^12
    expand = expansion(mult = 0.01)
  ) +
  scale_y_continuous(
    name = "Cumulative Distribution",
    limits = c(0, 1),
    expand = expansion(mult = 0),
    labels = label_percent(accuracy = 1)
  ) +
  labs(
    x = "Median Z-score",
    color = "",
    title = "NIH Conventionality (Mixed)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.20, 0.55)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  annotate("text", x = -5, y = 0.85, label = "Novel\nCombinations", size = 3) +
  annotate("text", x = 8.5, y = 0.15, label = "Conventional\nCombinations", size = 3)

# Conventionality Non-NIH over time

df_plot <- df %>%
  filter(NIH_funding==0) %>%
  filter(!is.na(Atyp_Median_Z)) %>%
  mutate(
    year_group = case_when(
      year >= 2001 & year <= 2009 ~ "2001-2009",
      year >= 2010 & year <= 2019 ~ "2010-2019",
      year >= 2020 & year <= 2022 ~ "2020-2022",
      TRUE                        ~ "Other"
    )
  )

# 1. Construct signed log2-magnitude coordinate
epsilon <- 1e-6  # to avoid log2(0)
df_plot <- df_plot %>%
  mutate(
    abs_val = abs(Atyp_Median_Z),
    abs_val = ifelse(abs_val < epsilon, epsilon, abs_val),
    log2_mag = log2(abs_val),
    sign_val = sign(Atyp_Median_Z),
    signed_log2 = log2_mag * sign_val
  )

# 2. Compute ECDF in this transformed coordinate (one curve per year_group)
ecdf_df <- df_plot %>%
  filter(!is.na(signed_log2), !is.na(year_group)) %>%
  arrange(year_group, signed_log2) %>%
  group_by(year_group) %>%
  mutate(
    ecdf_y = row_number() / n()
  ) %>%
  ungroup()

# 3. Define tick positions (in exponent units) and labels
neg_pows <- 8:1          # 8,7,...,1
pos_pows <- 1:12         # 1..12

xticks_vals <- c(-neg_pows, 0, pos_pows)  # -8..-1, 0, 1..12

xticks_labels <- c(
  paste0("-2^", neg_pows),  # these we will parse as math
  "0",
  paste0("2^", pos_pows)
)

# 4. Plot with ggplot
plot_ecdf_conventionality_non_nih<-ggplot(ecdf_df, aes(x = signed_log2, y = ecdf_y, color = year_group)) +
  geom_step(direction = "hv") +
  scale_x_continuous(
    breaks = xticks_vals,
    labels = c(
      parse(text = paste0("-2^", neg_pows)),
      "0",
      parse(text = paste0("2^", pos_pows))
    ),
    limits = c(-8, 12),       # corresponds to -2^8 .. 2^12
    expand = expansion(mult = 0.01)
  ) +
  scale_y_continuous(
    name = "Cumulative Distribution",
    limits = c(0, 1),
    expand = expansion(mult = 0),
    labels = label_percent(accuracy = 1)
  ) +
  labs(
    x = "Median Z-score",
    color = "",
    title = "Non-NIH Conventionality (Mixed)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.20, 0.55)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  annotate("text", x = -5, y = 0.85, label = "Novel\nCombinations", size = 3) +
  annotate("text", x = 8.5, y = 0.15, label = "Conventional\nCombinations", size = 3)


plot_ecdf_novelty_nih + plot_ecdf_novelty_non_nih +
  plot_ecdf_conventionality_nih + plot_ecdf_conventionality_non_nih + plot_layout(ncol=2)

setwd("~/Library/CloudStorage/OneDrive-Personal/Novelty Paper")

ggsave("Supplementary Figure 3.jpg", width = 10, height = 7.5, units = c("in"), dpi=600)