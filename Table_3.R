# Table 3 -- regressions on novelty conventionality

library(arrow) # read parquet files
library(tidyverse) # data manipulation and ggplots
library(texreg) # regression results
library(splines) # splines
library(sandwich) # robust standard errors
library(lmtest) # testing robust standard errors

setwd("~")

df_opa_sciscinet_2001_2022 <- read_parquet("df_opa_sciscinet_2001_2022_3_1_26.parquet")

# Our working data frame will be a 5% random sample

set.seed(123)
df <- df_opa_sciscinet_2001_2022 %>%
  sample_frac(0.05)

# Create journal_volume, year_c, and missingness flag

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


# Fit missingness model for Novelty_Type
#    P(Novelty_Type observed | covariates)

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

# extra truncation of extremely large weights
q99 <- quantile(df$ipw, 0.99, na.rm = TRUE)
df$ipw <- pmin(df$ipw, q99)

# Limit regression analyses to those with nonmissing novelty conventionality values

df_analysis <- df %>%
  filter(novelty_obs == 1) 

####### Regressions for Novelty, for Conventionality, and for Darwin's Towers

df_analysis <- df_analysis %>%
  mutate(novel=ifelse(Novelty_Type=="Avant-garde"|Novelty_Type=="Darwin's Tower",1,0)) %>%
  mutate(conventional=ifelse(Novelty_Type=="Accedpted Wisdom"|Novelty_Type=="Darwin's Tower",1,0)) %>%
  mutate(darwin=ifelse(Novelty_Type=="Darwin's Tower",1,0))


fit_logit_weights_full_novel <- glm(
  novel ~
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

summary(fit_logit_weights_full_novel)  

# Robust SEs -- virtually identical
vcov_robust <- sandwich::vcovHC(fit_logit_weights_full_novel, type = "HC0")
coeftest(fit_logit_weights_full_novel, vcov. = vcov_robust)


fit_logit_weights_full_conventional <- glm(
  conventional ~
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

summary(fit_logit_weights_full_conventional)  

# Robust SEs -- virtually identical
vcov_robust <- sandwich::vcovHC(fit_logit_weights_full_conventional, type = "HC0")
coeftest(fit_logit_weights_full_conventional, vcov. = vcov_robust)

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

summary(fit_logit_weights_full_darwin)  

# Robust SEs -- virtually identical
vcov_robust <- sandwich::vcovHC(fit_logit_weights_full_darwin, type = "HC0")
coeftest(fit_logit_weights_full_darwin, vcov. = vcov_robust)

# Texreg display

coef_labels <- list(
  "(Intercept)"               = "Intercept",
  "NIH_funding"               = "NIH Funding",
  "Science_TypeMixed"                 = "Mixed topic",
  "Science_TypeHuman-focused"         = "Human-focused topic",
  "ns(year_c, 3)1"       = "Year (spline 1)",
  "ns(year_c, 3)2"       = "Year (spline 2)",
  "ns(year_c, 3)3"       = "Year (spline 3)",
  "quartile_author_countQ2"                = "Author count (Quartile 2)",
  "quartile_author_countQ3"                = "Author count (Quartile 3)",
  "quartile_author_countQ4"                = "Author count (Quartile 4)",
  "institution_count"         = "Institution count",
  "ns(reference_count, 3)1" = "Reference count (spline 1)",
  "ns(reference_count, 3)2" = "Reference count (spline 2)",
  "ns(reference_count, 3)3" = "Reference count (spline 3)",
  "is_clinicalTRUE"           = "Clinical Trial",
  "NIH_funding:Science_TypeMixed" = "NIH X Mixed",
  "NIH_funding:Science_TypeHuman-focused" = "NIH X Human-focused"
)

screenreg(
  list(fit_logit_weights_full_novel, fit_logit_weights_full_conventional, fit_logit_weights_full_darwin),
  custom.model.names = c("Novel", "Conventional","Darwin"),
  custom.coef.map = coef_labels,
  digits = 3
)

# Save table

htmlreg(
  list(fit_logit_weights_full_novel, fit_logit_weights_full_conventional, fit_logit_weights_full_darwin),
  custom.model.names = c("Novel", "Conventional","Darwin"),
  custom.coef.map = coef_labels,
  caption = "",
  file="Table 3 3 1 26.html",
  caption.above = TRUE,
  digits = 3
)
