# Figure 2 -- ECDF curves for NIH vs non-NIH broken down by Science Type

library(arrow) # read parquet files
library(tidyverse) # data manipulation and ggplots
library(scales) # For percent values on ggplots
library(patchwork) # multi-panel figures

setwd("~/Library/CloudStorage/OneDrive-Personal/Sciscinet v2")

df_opa_sciscinet_2001_2022 <- read_parquet("df_opa_sciscinet_2001_2022_3_1_26.parquet")

# Our working data frame will be a 5% random sample

set.seed(123)
df <- df_opa_sciscinet_2001_2022 %>%
  sample_frac(0.05)


#################################################################################

## NIH vs non-NIH Fundamental only

################################################################################

# Novelty Plot

df_plot <- df %>%
  filter(Science_Type=="Fundamental") %>%
  filter(!is.na(Atyp_10pct_Z)) %>%
  mutate(
    Funding = factor(
      Funding,
      levels = c("NIH", "Not NIH"),
      labels = c("NIH", "Not NIH")
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
  filter(!is.na(signed_log2), !is.na(Funding)) %>%
  arrange(Funding, signed_log2) %>%
  group_by(Funding) %>%
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
plot_ecdf_novelty_funding_fundamental<-ggplot(ecdf_df, aes(x = signed_log2, y = ecdf_y, color = Funding)) +
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
    title = "Novelty by Funding (Fundamental)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.75, 0.55)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  annotate("text", x = -5, y = 0.85, label = "Novel\nCombinations", size = 3) +
  annotate("text", x = 7, y = 0.15, label = "Conventional\nCombinations", size = 3)

# Conventionality Plot

df_plot <- df %>%
  filter(Science_Type=="Fundamental") %>%
  filter(!is.na(Atyp_Median_Z)) %>%
  mutate(
    Funding = factor(
      Funding,
      levels = c("NIH", "Not NIH"),
      labels = c("NIH", "Not NIH")
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
  filter(!is.na(signed_log2), !is.na(Funding)) %>%
  arrange(Funding, signed_log2) %>%
  group_by(Funding) %>%
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
plot_ecdf_conventionality_funding_fundamental<-ggplot(ecdf_df, aes(x = signed_log2, y = ecdf_y, color = Funding)) +
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
    title = "Conventionality by Funding (Fundamental)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.15, 0.55)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  annotate("text", x = -5, y = 0.85, label = "Novel\nCombinations", size = 3) +
  annotate("text", x = 8, y = 0.15, label = "Conventional\nCombinations", size = 3)

################################################################################

# NIH vs non-NIH Mixed only

################################################################################

# Novelty Plot

df_plot <- df %>%
  filter(Science_Type=="Mixed") %>%
  filter(!is.na(Atyp_10pct_Z)) %>%
  mutate(
    Funding = factor(
      Funding,
      levels = c("NIH", "Not NIH"),
      labels = c("NIH", "Not NIH")
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
  filter(!is.na(signed_log2), !is.na(Funding)) %>%
  arrange(Funding, signed_log2) %>%
  group_by(Funding) %>%
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
plot_ecdf_novelty_funding_mixed<-ggplot(ecdf_df, aes(x = signed_log2, y = ecdf_y, color = Funding)) +
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
    title = "Novelty by Funding (Mixed)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.75, 0.55)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  annotate("text", x = -5, y = 0.85, label = "Novel\nCombinations", size = 3) +
  annotate("text", x = 7, y = 0.15, label = "Conventional\nCombinations", size = 3)

# Conventionality Plot

df_plot <- df %>%
  filter(Science_Type=="Mixed") %>%
  filter(!is.na(Atyp_Median_Z)) %>%
  mutate(
    Funding = factor(
      Funding,
      levels = c("NIH", "Not NIH"),
      labels = c("NIH", "Not NIH")
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
  filter(!is.na(signed_log2), !is.na(Funding)) %>%
  arrange(Funding, signed_log2) %>%
  group_by(Funding) %>%
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
plot_ecdf_conventionality_funding_mixed<-ggplot(ecdf_df, aes(x = signed_log2, y = ecdf_y, color = Funding)) +
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
    title = "Conventionality by Funding (Mixed)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.15, 0.55)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  annotate("text", x = -5, y = 0.85, label = "Novel\nCombinations", size = 3) +
  annotate("text", x = 8, y = 0.15, label = "Conventional\nCombinations", size = 3)

#################################################################################

## NIH vs non-NIH human only

################################################################################

# Novelty Plot

df_plot <- df %>%
  filter(Science_Type=="Human-focused") %>%
  filter(!is.na(Atyp_10pct_Z)) %>%
  mutate(
    Funding = factor(
      Funding,
      levels = c("NIH", "Not NIH"),
      labels = c("NIH", "Not NIH")
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
  filter(!is.na(signed_log2), !is.na(Funding)) %>%
  arrange(Funding, signed_log2) %>%
  group_by(Funding) %>%
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
plot_ecdf_novelty_funding_human<-ggplot(ecdf_df, aes(x = signed_log2, y = ecdf_y, color = Funding)) +
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
    title = "Novelty by Funding (Human-focused)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.75, 0.55)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  annotate("text", x = -5, y = 0.85, label = "Novel\nCombinations", size = 3) +
  annotate("text", x = 7, y = 0.15, label = "Conventional\nCombinations", size = 3)

# Conventionality Plot

df_plot <- df %>%
  filter(Science_Type=="Human-focused") %>%
  filter(!is.na(Atyp_Median_Z)) %>%
  mutate(
    Funding = factor(
      Funding,
      levels = c("NIH", "Not NIH"),
      labels = c("NIH", "Not NIH")
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
  filter(!is.na(signed_log2), !is.na(Funding)) %>%
  arrange(Funding, signed_log2) %>%
  group_by(Funding) %>%
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
plot_ecdf_conventionality_funding_human<-ggplot(ecdf_df, aes(x = signed_log2, y = ecdf_y, color = Funding)) +
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
    title = "Conventionality by Funding (Human-focused)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.15, 0.55)
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  annotate("text", x = -5, y = 0.85, label = "Novel\nCombinations", size = 3) +
  annotate("text", x = 8, y = 0.15, label = "Conventional\nCombinations", size = 3)


plot_ecdf_novelty_funding_fundamental + plot_ecdf_conventionality_funding_fundamental +
  plot_ecdf_novelty_funding_mixed + plot_ecdf_conventionality_funding_mixed +
  plot_ecdf_novelty_funding_human + plot_ecdf_conventionality_funding_human +
  plot_layout(ncol=2)

setwd("~/Library/CloudStorage/OneDrive-Personal/Novelty Paper")

ggsave("Figure 2 3 2 26.jpg", width = 10, height = 11.25, units = c("in"), dpi=600)


