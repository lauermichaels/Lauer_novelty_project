# Table 4 -- regressions on citation rates

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
  ) %>%
  
  # Limit to papers with known RCR
  
  filter(!is.na(relative_citation_ratio))

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

#----------------------------------------------------------
# 2. Restrict to observed Novelty_Type and define outcome
#----------------------------------------------------------

df_analysis <- df %>%
  filter(novelty_obs == 1 & !is.na(relative_citation_ratio)) %>%
  mutate(
    Highly_Cited_num = ifelse(Highly_Cited==TRUE, 1, 0)
  )

# Check variation in outcome

table(df_analysis$Highly_Cited)

#----------------------------------------------------------
# 3. Weighted logistic regression for Highly_Cited
#----------------------------------------------------------

fit_logit_weights_cited_simple <- glm(
  Highly_Cited_num ~
    NIH_funding +
    ns(year_c, 3),
  data    = df_analysis,
  family  = binomial(),
  weights = ipw
)

summary(fit_logit_weights_cited_simple)  

fit_logit_weights_cited_novelty <- glm(
  Highly_Cited_num ~
    NIH_funding +
    ns(year_c, 3) + Novelty_Type,
  data    = df_analysis,
  family  = binomial(),
  weights = ipw
)

summary(fit_logit_weights_cited_novelty)  

fit_logit_weights_cited_novelty_science <- glm(
  Highly_Cited_num ~
    NIH_funding +
    ns(year_c, 3) + Novelty_Type + Science_Type,
  data    = df_analysis,
  family  = binomial(),
  weights = ipw
)

summary(fit_logit_weights_cited_novelty_science)  

fit_logit_weights_cited_full <- glm(
  Highly_Cited_num ~
    NIH_funding +
    Novelty_Type +
    Science_Type +
    ns(year_c, 3) +
    quartile_author_count +
    institution_count +
    ns(reference_count, 3) +
    is_clinical + Novelty_Type:Science_Type +
    NIH_funding:Novelty_Type + NIH_funding:Science_Type,
  data    = df_analysis,
  family  = binomial(),
  weights = ipw
)

summary(fit_logit_weights_cited_full)  

# Robust SEs
vcov_robust <- sandwich::vcovHC(fit_logit_weights, type = "HC0")
coeftest(fit_logit_weights, vcov. = vcov_robust)

# Virtually identical

summary(df$ipw)
quantile(df$ipw, c(0.01, 0.1, 0.5, 0.9, 0.99), na.rm = TRUE)

# Texreg display

coef_labels <- list(
  "(Intercept)"               = "Intercept",
  "NIH_funding"               = "NIH Funding",
  "Novelty_TypeAvant-garde"   = "Avant-garde",
  "Novelty_TypeAccepted Wisdom"  = "Accepted Wisdom",
  "Novelty_TypeDarwin's Tower"   = "Darwin's Tower",
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
  "is_clinicalTrue"           = "Clinical Trial",
  "Novelty_TypeAvant-garde:Science_TypeHuman-focused" = "Avant-garde X Human-focused",
  "Novelty_TypeAccepted Wisdom:Science_TypeHuman-focused" = "Accepted Wisdom X Human-focused",
  "Novelty_TypeDarwin's Tower:Science_TypeHuman-focused" = "Darwin's Tower X Human-focused",
  "NIH_funding:Science_TypeMixed" = "NIH X Mixed",
  "NIH_funding:Science_TypeHuman-focused" = "NIH X Human-focused"
)

screenreg(
  list(fit_logit_weights_cited_simple, fit_logit_weights_cited_novelty, fit_logit_weights_cited_novelty_science, fit_logit_weights_cited_full),
  custom.model.names = c("(1)", "(2)","(3)", "(4)"),
  custom.coef.map = coef_labels,
  digits = 3
)

# Save table

setwd("~/Library/CloudStorage/OneDrive-Personal/Novelty Paper")

htmlreg(
  list(fit_logit_weights_cited_simple, fit_logit_weights_cited_novelty, fit_logit_weights_cited_novelty_science, fit_logit_weights_cited_full),
  custom.model.names = c("(1)", "(2)","(3)", "(4)"),
  custom.coef.map = coef_labels,
  caption = "",
  file="Table 4 3 1 26.html",
  caption.above = TRUE,
  digits = 3
)