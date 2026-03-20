# Figure 4 -- predicted outcomes illustrating interactions in logistic model for Darwin's Tower papers

library(arrow) # read parquet files
library(tidyverse) # data manipulation and ggplots
library(texreg) # regression results
library(splines) # splines
library(sandwich) # robust standard errors
library(lmtest) # testing robust standard errors

setwd("~/Library/CloudStorage/OneDrive-Personal/Sciscinet v2")

df_opa_sciscinet_2001_2022 <- read_parquet("df_opa_sciscinet_2001_2022_3_1_26.parquet")

# Our working data frame will be a 5% random sample

set.seed(123)
df <- df_opa_sciscinet_2001_2022 %>%
  sample_frac(0.05)

#----------------------------------------------------------
# 0. Create journal_volume, year_c, and missingness flag
#----------------------------------------------------------

df <- df %>%
  # journal_volume = total number of papers in each journal
  add_count(journal, name = "journal_volume") %>%
  mutate(
    # Center year at 2012
    year_c = year - 2012,
    # Make sure NIH_funding is numeric 0/1 (if it isn't already)
    NIH_funding = as.numeric(NIH_funding),
    # Indicator for Novelty_Type being observed
    novelty_obs = ifelse(is.na(Atyp_10pct_Z), 0, 1)
  )

#----------------------------------------------------------
# 1. Fit missingness model for Novelty_Type
#    P(Novelty_Type observed | covariates)
#----------------------------------------------------------

fit_miss_lpm <- lm(
  novelty_obs ~
    journal_volume +
    Science_Type +
    year_c +
    quartile_author_count +
    is_clinical,
  data = df
)

summary(fit_miss_lpm)

# Predicted probability that Novelty_Type is observed
df$pi_obs <- pmin(pmax(predict(fit_miss_lpm, type = "response"), 0.01), 0.99)

# Truncate to [0.01, 0.99] to avoid extreme weights

# Stabilized inverse probability weights
p_overall_obs <- mean(df$novelty_obs == 1, na.rm = TRUE)

df <- df %>%
  mutate(
    ipw = ifelse(
      novelty_obs == 1,
      p_overall_obs / pi_obs,
      NA_real_
    )
  )

# Optional: extra truncation of extremely large weights
q99 <- quantile(df$ipw, 0.99, na.rm = TRUE)
df$ipw <- pmin(df$ipw, q99)

# Limit regression analyses to those with nonmissing novelty conventionality values

df_analysis <- df %>%
  filter(novelty_obs == 1) 

df_analysis <- df_analysis %>%
  mutate(novel=ifelse(Novelty_Type=="Avant-garde"|Novelty_Type=="Darwin's Tower",1,0)) %>%
  mutate(conventional=ifelse(Novelty_Type=="Accedpted Wisdom"|Novelty_Type=="Darwin's Tower",1,0)) %>%
  mutate(darwin=ifelse(Novelty_Type=="Darwin's Tower",1,0))

# Regression model

fit_logit_weights_full_darwin <- glm(
  darwin ~
    NIH_funding +
    Science_Type +
    ns(year_c, 3) +
    quartile_author_count +
    institution_count +
    ns(reference_count, 3) +
    is_clinical + NIH_funding:Science_Type,
  data    = df_analysis,
  family  = binomial(),
  weights = ipw
)

## Plot showing results of logistic regression

## 1. Check factor levels
levels(df_analysis$Science_Type)

# Make sure NIH_funding is 0/1 numeric and is_clinical is factor(False, True)

## 2. Choose reference / typical values for other covariates
# Here:
# - year_c at 0 (i.e., year = 2012)
# - quartile_author_count at Q1
# - institution_count at its median
# - reference_count at its median
# - is_clinical = "False"
inst_med  <- median(df_analysis$institution_count, na.rm = TRUE)
ref_med   <- median(df_analysis$reference_count, na.rm = TRUE)

## 3. Build prediction grid
pred_grid <- expand.grid(
  NIH_funding          = c(0, 1),
  Science_Type         = levels(df_analysis$Science_Type),
  year_c               = 0,
  quartile_author_count = factor("Q1", levels = levels(df_analysis$quartile_author_count)),
  institution_count    = inst_med,
  reference_count      = ref_med,
  is_clinical          = FALSE)


## 4. Get predicted probabilities and (approx) CIs
pred_link <- predict(fit_logit_weights_full_darwin, newdata = pred_grid, type = "link", se.fit = TRUE)

pred_grid$linpred  <- pred_link$fit
pred_grid$se_link  <- pred_link$se.fit
pred_grid$prob     <- plogis(pred_grid$linpred)
pred_grid$prob_low <- plogis(pred_grid$linpred - 1.96 * pred_grid$se_link)
pred_grid$prob_high<- plogis(pred_grid$linpred + 1.96 * pred_grid$se_link)

## 5. Nice labels for plotting
pred_grid$NIH_funding_lab <- ifelse(pred_grid$NIH_funding == 1, "NIH-funded", "Not NIH-funded")

## 6. Plot
ggplot(pred_grid, aes(x = Science_Type, y = prob, color = NIH_funding_lab, group = NIH_funding_lab)) +
  geom_point(position = position_dodge(width = 0.3), size = 4) +
  geom_errorbar(
    aes(ymin = prob_low, ymax = prob_high),
    position = position_dodge(width = 0.3),
    width = 0.4
  ) +
  scale_y_continuous(
    name = "Predicted Probability",
    labels = label_percent(accuracy = 1)
  ) +
  labs(
    x = "Type of Science",
    color = "",
    title = ""
  ) +
  theme_bw()

ggsave("Figure 4.jpg", width = 6.665, height = 3.75, units = c("in"), dpi=600)




