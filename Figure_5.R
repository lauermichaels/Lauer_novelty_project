# Figure 5 -- multinomial models showing changes over time

library(arrow) # read parquet files
library(tidyverse) # data manipulation and ggplots
library(splines) # splines
library(scales) # percent axes
library(nnet)   # for multinomial models

setwd("~")

df_opa_sciscinet_2001_2022 <- read_parquet("df_opa_sciscinet_2001_2022_3_1_26.parquet")

# Our working data frame will be a 5% random sample

set.seed(123)
df <- df_opa_sciscinet_2001_2022 %>%
  sample_frac(0.05)

#################################################################################

## Multinomial model for Novelty_Type over Time NIH papers

#################################################################################

df_nih<-df %>% filter(NIH_funding==1 & Novelty_Type != "Missing") %>%
  mutate(Novelty_Type = factor(Novelty_Type, levels = c("Platypus", "Avant-garde", "Accepted Wisdom", "Darwin's Tower"))) %>%
  mutate(year_c=year-2012)

# Multinomial model with non-linear time trend + controls

mod_multi_ctrl <- multinom(
  Novelty_Type ~ ns(year_c, df = 3) +
    Science_Type +
    institution_count +
    quartile_author_count +
    ns(reference_count, df = 3) +
    is_clinical,
  data  = df_nih,
  trace = FALSE
)

summary(mod_multi_ctrl)

# Compare to null model

mod_multi_ctrl_null <- multinom(
  Novelty_Type ~
    Science_Type +
    institution_count +
    quartile_author_count +
    ns(reference_count, df = 3) +
    is_clinical,
  data  = df_nih,
  trace = FALSE
)

# Likelihood ratio test
ll_full  <- logLik(mod_multi_ctrl)
ll_null  <- logLik(mod_multi_ctrl_null)
lr_stat  <- 2 * (ll_full - ll_null)
df_diff  <- attr(ll_full, "df") - attr(ll_null, "df")
p_value  <- pchisq(lr_stat, df = df_diff, lower.tail = FALSE)

lr_stat
df_diff
p_value

# Choose a representative / reference combination of controls

controls_ref <- df_nih %>%
  summarize(
    institution_count = median(institution_count, na.rm = TRUE),
    reference_count   = median(reference_count, na.rm = TRUE)
  )

inst_ref <- controls_ref$institution_count[1]
ref_ref  <- controls_ref$reference_count[1]

# Grid of years for prediction
year_grid <- data.frame(
  year_c = seq(min(df_nih$year_c, na.rm = TRUE),
               max(df_nih$year_c, na.rm = TRUE),
               length.out = 100)
)
year_grid$year <- year_grid$year_c + 2012

# Add control values (you can change these as desired)
newdata_grid <- year_grid %>%
  mutate(
    Science_Type          = factor("Fundamental",
                                   levels = levels(df_nih$Science_Type)),
    institution_count     = inst_ref,
    quartile_author_count = factor("Q2",
                                   levels = levels(df_nih$quartile_author_count)),
    reference_count       = ref_ref,
    is_clinical           = FALSE
  )

# Predicted probabilities for each Novelty_Type
pred_probs <- predict(mod_multi_ctrl, newdata = newdata_grid, type = "probs")

# Build long data frame for plotting
pred_df <- cbind(newdata_grid, as.data.frame(pred_probs))

pred_long <- pred_df %>%
  pivot_longer(
    cols = all_of(levels(df_nih$Novelty_Type)),
    names_to  = "Novelty_Type",
    values_to = "prob"
  )

pred_long = pred_long %>%
  mutate(Novelty_Type = factor(Novelty_Type, levels = c("Platypus","Avant-garde","Accepted Wisdom", "Darwin's Tower")))

plot_novelty_type_time_nih<-ggplot(pred_long, aes(x = year, y = prob, color = Novelty_Type)) +
  geom_line(size = 2) +
  scale_y_continuous(
    name = "",
    limits = c(0, 0.55),
    breaks = seq(0, 0.55, by = 0.1),
    labels = label_percent(accuracy = 1)
  )+
  labs(
    x = "",
    y = "",
    color = "",
    title = "NIH-funded"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "top",
    legend.margin = margin(0, 0, 0, 0, "pt")
  )

#################################################################################

## Multinomial model for Novelty_Type over Time non-NIH papers

#################################################################################

df_non_nih<-df %>% filter(NIH_funding==0 & Novelty_Type != "Missing") %>%
  mutate(Novelty_Type = factor(Novelty_Type, levels = c("Platypus", "Avant-garde", "Accepted Wisdom", "Darwin's Tower"))) %>%
  mutate(year_c=year-2012)

# Multinomial model with non-linear time trend + controls
mod_multi_ctrl <- multinom(
  Novelty_Type ~ ns(year_c, df = 3) +
    Science_Type +
    institution_count +
    quartile_author_count +
    ns(reference_count, df = 3) +
    is_clinical,
  data  = df_non_nih,
  trace = FALSE
)

summary(mod_multi_ctrl)

# Compare to null model

mod_multi_ctrl_null <- multinom(
  Novelty_Type ~
    Science_Type +
    institution_count +
    quartile_author_count +
    ns(reference_count, df = 3) +
    is_clinical,
  data  = df_non_nih,
  trace = FALSE
)

# Likelihood ratio test
ll_full  <- logLik(mod_multi_ctrl)
ll_null  <- logLik(mod_multi_ctrl_null)
lr_stat  <- 2 * (ll_full - ll_null)
df_diff  <- attr(ll_full, "df") - attr(ll_null, "df")
p_value  <- pchisq(lr_stat, df = df_diff, lower.tail = FALSE)

lr_stat
df_diff
p_value

# Choose a representative / reference combination of controls

controls_ref <- df_non_nih %>%
  summarize(
    institution_count = median(institution_count, na.rm = TRUE),
    reference_count   = median(reference_count, na.rm = TRUE)
  )

inst_ref <- controls_ref$institution_count[1]
ref_ref  <- controls_ref$reference_count[1]

# Grid of years for prediction
year_grid <- data.frame(
  year_c = seq(min(df_non_nih$year_c, na.rm = TRUE),
               max(df_non_nih$year_c, na.rm = TRUE),
               length.out = 100)
)
year_grid$year <- year_grid$year_c + 2012

# Add control values (you can change these as desired)
newdata_grid <- year_grid %>%
  mutate(
    Science_Type          = factor("Fundamental",
                                   levels = levels(df_non_nih$Science_Type)),
    institution_count     = inst_ref,
    quartile_author_count = factor("Q2",
                                   levels = levels(df_non_nih$quartile_author_count)),
    reference_count       = ref_ref,
    is_clinical           = FALSE
  )

# Predicted probabilities for each Novelty_Type
pred_probs <- predict(mod_multi_ctrl, newdata = newdata_grid, type = "probs")

# Build long data frame for plotting
pred_df <- cbind(newdata_grid, as.data.frame(pred_probs))

pred_long <- pred_df %>%
  pivot_longer(
    cols = all_of(levels(df_non_nih$Novelty_Type)),
    names_to  = "Novelty_Type",
    values_to = "prob"
  )

pred_long = pred_long %>%
  mutate(Novelty_Type = factor(Novelty_Type, levels = c("Platypus","Avant-garde","Accepted Wisdom", "Darwin's Tower")))

plot_novelty_type_time_non_nih<-ggplot(pred_long, aes(x = year, y = prob, color = Novelty_Type)) +
  geom_line(size = 2) +
  scale_y_continuous(
    name = "",
    limits = c(0, 0.55),
    breaks = seq(0, 0.55, by = 0.1),
    labels = label_percent(accuracy = 1)
  )+
  labs(
    x = "Publication year",
    y = "",
    color = "",
    title = "Non-NIH-funded"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position = "none"
  )

plot_novelty_type_time_nih + plot_novelty_type_time_non_nih + plot_layout(ncol=1)

ggsave("Figure 5.jpg", width = 6.665, height = 7.5, units = c("in"), dpi=600)


