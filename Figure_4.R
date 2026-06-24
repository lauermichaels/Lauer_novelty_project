# Figure 4 -- predicted outcomes illustrating interactions in logistic model
# DuckDB-efficient version

library(DBI)
library(duckdb)
library(tidyverse)
library(scales)
library(splines)
library(sandwich)
library(lmtest)

con <- dbConnect(duckdb::duckdb(), dbdir = "opa_sciscinet.duckdb")

dbExecute(con, "PRAGMA threads = 8")
dbExecute(con, "PRAGMA memory_limit = '32GB'")
dbExecute(con, "PRAGMA temp_directory = 'duckdb_temp'")

input_file <- "df_opa_sciscinet_2001_2022_6_18_26.parquet"
sample_n <- 500000

# Pull a model-ready sample from DuckDB

df <- dbGetQuery(con, sprintf("
    WITH base AS (
        SELECT
            pmid,
            journal,
            year,
            year - 2012 AS year_c,
            Science_Type,
            quartile_author_count,
            is_clinical,
            NIH_funding,
            institution_count,
            reference_count,
            Atyp_10pct_Z,
            Novelty_Type
        FROM read_parquet('%s')
    ),

    journal_counts AS (
        SELECT
            journal,
            COUNT(*) AS journal_volume
        FROM base
        WHERE journal IS NOT NULL
        GROUP BY journal
    ),

    model_ready AS (
        SELECT
            b.pmid,
            b.journal,
            COALESCE(j.journal_volume, 0) AS journal_volume,
            b.year,
            b.year_c,
            b.Science_Type,
            b.quartile_author_count,
            b.is_clinical,
            CAST(b.NIH_funding AS DOUBLE) AS NIH_funding,
            b.institution_count,
            b.reference_count,
            b.Atyp_10pct_Z,
            b.Novelty_Type,
            CASE
                WHEN b.Atyp_10pct_Z IS NULL THEN 0
                ELSE 1
            END AS novelty_obs
        FROM base AS b
        LEFT JOIN journal_counts AS j
            ON b.journal = j.journal
    )

    SELECT
        *
    FROM model_ready
    USING SAMPLE reservoir(%s ROWS) REPEATABLE (101)
", input_file, sample_n))

# Prepare factors and logical variables in R

df <- df %>%
  mutate(
    Science_Type = factor(
      Science_Type,
      levels = c("Fundamental", "Mixed", "Human-focused")
    ),
    quartile_author_count = factor(
      quartile_author_count,
      levels = c("Q1", "Q2", "Q3", "Q4")
    ),
    NIH_funding = as.numeric(NIH_funding),
    novelty_obs = as.numeric(novelty_obs),
    is_clinical = case_when(
      is_clinical %in% c(TRUE, "TRUE", "True", "true", 1, "1") ~ TRUE,
      is_clinical %in% c(FALSE, "FALSE", "False", "false", 0, "0") ~ FALSE,
      TRUE ~ NA
    )
  )

# Fit missingness model for observed novelty

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

# Create stabilized inverse probability weights

df$pi_obs <- pmin(
  pmax(
    predict(fit_miss_lpm, type = "response"),
    0.01
  ),
  0.99
)

p_overall_obs <- mean(df$novelty_obs == 1, na.rm = TRUE)

df <- df %>%
  mutate(
    ipw = ifelse(
      novelty_obs == 1,
      p_overall_obs / pi_obs,
      NA_real_
    )
  )

q99 <- quantile(df$ipw, 0.99, na.rm = TRUE)

df <- df %>%
  mutate(
    ipw = pmin(ipw, q99)
  )

# Limit to observed novelty data and create Darwin outcome

df_analysis <- df %>%
  filter(novelty_obs == 1) %>%
  mutate(
    darwin = ifelse(Novelty_Type == "Darwin's Tower", 1, 0)
  ) %>%
  filter(
    !is.na(darwin),
    !is.na(NIH_funding),
    !is.na(Science_Type),
    !is.na(year_c),
    !is.na(quartile_author_count),
    !is.na(institution_count),
    !is.na(reference_count),
    !is.na(is_clinical),
    !is.na(ipw)
  )

# Fit weighted logistic regression

fit_logit_weights_full_darwin <- glm(
  darwin ~
    NIH_funding +
    Science_Type +
    ns(year_c, 3) +
    quartile_author_count +
    institution_count +
    ns(reference_count, 3) +
    is_clinical +
    NIH_funding:Science_Type,
  data = df_analysis,
  family = binomial(),
  weights = ipw
)

summary(fit_logit_weights_full_darwin)

# Robust standard errors

coeftest(
  fit_logit_weights_full_darwin,
  vcov = vcovHC(fit_logit_weights_full_darwin, type = "HC1")
)

# Build prediction grid

inst_med <- median(df_analysis$institution_count, na.rm = TRUE)
ref_med <- median(df_analysis$reference_count, na.rm = TRUE)

pred_grid <- expand.grid(
  NIH_funding = c(0, 1),
  Science_Type = levels(df_analysis$Science_Type),
  year_c = 0,
  quartile_author_count = factor(
    "Q1",
    levels = levels(df_analysis$quartile_author_count)
  ),
  institution_count = inst_med,
  reference_count = ref_med,
  is_clinical = FALSE
)

# Generate predicted probabilities

# Generate predicted probabilities with HC1 robust standard errors

pred_link <- predict(
  fit_logit_weights_full_darwin,
  newdata = pred_grid,
  type = "link",
  se.fit = FALSE
)

# HC1 robust covariance matrix of coefficients
V_HC1 <- vcovHC(
  fit_logit_weights_full_darwin,
  type = "HC1"
)

# Build model matrix for the prediction grid
Terms <- delete.response(terms(fit_logit_weights_full_darwin))

mf_new <- model.frame(
  Terms,
  data = pred_grid,
  na.action = na.pass,
  xlev = fit_logit_weights_full_darwin$xlevels
)

X_new <- model.matrix(
  Terms,
  data = mf_new,
  contrasts.arg = fit_logit_weights_full_darwin$contrasts
)

# Robust SE of the linear predictor: sqrt(diag(X V X'))
se_link_HC1 <- sqrt(diag(X_new %*% V_HC1 %*% t(X_new)))

# Store results
pred_grid$linpred <- as.numeric(pred_link)
pred_grid$se_link_HC1 <- se_link_HC1

pred_grid$prob <- plogis(pred_grid$linpred)
pred_grid$prob_low <- plogis(pred_grid$linpred - 1.96 * pred_grid$se_link_HC1)
pred_grid$prob_high <- plogis(pred_grid$linpred + 1.96 * pred_grid$se_link_HC1)

pred_grid <- pred_grid %>%
  mutate(
    NIH_funding_lab = ifelse(
      NIH_funding == 1,
      "NIH-funded",
      "Not NIH-funded"
    ),
    NIH_funding_lab = factor(
      NIH_funding_lab,
      levels = c("NIH-funded", "Not NIH-funded")
    )
  )

pred_grid

# Plot figure 4

figure_4 <- ggplot(
  pred_grid,
  aes(
    x = Science_Type,
    y = prob,
    color = NIH_funding_lab,
    group = NIH_funding_lab
  )
) +
  geom_point(
    position = position_dodge(width = 0.3),
    size = 4
  ) +
  geom_errorbar(
    aes(
      ymin = prob_low,
      ymax = prob_high
    ),
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

figure_4

ggsave(
  "Figure 4.jpg",
  plot = figure_4,
  width = 6.665,
  height = 3.75,
  units = "in",
  dpi = 600
)

# Validate use of Sandwich robust SEs

all(colnames(X_new) == colnames(V_HC1)) # Came back as TRUE

dbDisconnect(con, shutdown = TRUE)

