# Table 3 of main paper; Figure 4 of main paper; supplemental Tables 4 and 5
# Logistic regression analyses for Darwin's Tower papers and highly cited papers

setwd("~/Library/CloudStorage/OneDrive-Personal/Sciscinet v2")

library(duckdb) # rapid read-in and sampling of large parquet files
library(DBI) # database interface
library(tidyverse) # data manipulation and ggplots
library(splines) # spline functions
library(sandwich) # generate Sandwich-robust standard errors
library(texreg) # make nice regression tables
library(pROC) # calculate area under ROC curves, measure of discrimination
library(scales) # aesthetics for ggplot

# Set up read-in and sampling of file
# We will generate 5 samples of 500,000 each

file_path <- "df_opa_sciscinet_2001_2022_3_1_26.parquet"
sample_n <- 500000
seed_vec <- c(101, 202, 303, 404, 505)

### Helper functions

# Correctly pull out first valid measure as a numeric

safe_num <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return(NA_real_)
  as.numeric(x[1])
}

# HC1 Sandwich robust standard errors

get_robust_se <- function(model_obj) {
  sqrt(diag(vcovHC(model_obj, type = "HC1")))
}

# Goodness of fit values for Texreg tables

make_texreg_obj <- function(model_obj,
                            coef_map_fun,
                            gof_names,
                            gof_vals,
                            gof_decimals) {
  cf <- coef(model_obj)
  se_vals <- get_robust_se(model_obj)
  p_vals <- 2 * pnorm(abs(cf / se_vals), lower.tail = FALSE)

  createTexreg(
    coef.names = coef_map_fun(names(cf)),
    coef = unname(cf),
    se = unname(se_vals),
    pvalues = unname(p_vals),
    gof.names = gof_names,
    gof = gof_vals,
    gof.decimal = gof_decimals
  )
}

# Labels for variables in Darwin's Tower model

pretty_term_darwin <- function(term_vec) {
  out_vals <- term_vec
  out_vals[out_vals == "(Intercept)"] <- "Intercept"
  out_vals[out_vals == "NIH_funding"] <- "NIH Funding"
  out_vals[out_vals == "Clinical"] <- "Clinical Trial"
  out_vals[out_vals == "is_clinical"] <- "Clinical Trial"
  out_vals[out_vals == "Science_TypeMixed"] <- "Science Type Mixed"
  out_vals[out_vals == "Science_TypeHuman-focused"] <- "Science Type Human-focused"
  out_vals[out_vals == "ns(year_c, 3)1"] <- "Year spline 1"
  out_vals[out_vals == "ns(year_c, 3)2"] <- "Year spline 2"
  out_vals[out_vals == "ns(year_c, 3)3"] <- "Year spline 3"
  out_vals[out_vals == "NIH_funding:Science_TypeMixed"] <- "NIH Funding × Science Type Mixed"
  out_vals[out_vals == "NIH_funding:Science_TypeHuman-focused"] <- "NIH Funding × Science Type Human-focused"
  out_vals
}

# Labels for variables in highly cited model

pretty_term_highly_cited <- function(term_vec) {
  out_vals <- term_vec
  out_vals[out_vals == "(Intercept)"] <- "Intercept"
  out_vals[out_vals == "NIH_funding"] <- "NIH Funding"
  out_vals[out_vals == "Clinical"] <- "Clinical Trial"
  out_vals[out_vals == "is_clinical"] <- "Clinical Trial"
  out_vals[out_vals == "Science_TypeMixed"] <- "Science Type Mixed"
  out_vals[out_vals == "Science_TypeHuman-focused"] <- "Science Type Human-focused"
  out_vals[out_vals == "Novelty_TypeAvant-garde"] <- "Avant-garde"
  out_vals[out_vals == "Novelty_TypeAccepted Wisdom"] <- "Accepted Wisdom"
  out_vals[out_vals == "Novelty_TypeDarwin's Tower"] <- "Darwin's Tower"
  out_vals[out_vals == "ns(year_c, 3)1"] <- "Year spline 1"
  out_vals[out_vals == "ns(year_c, 3)2"] <- "Year spline 2"
  out_vals[out_vals == "ns(year_c, 3)3"] <- "Year spline 3"
  out_vals[out_vals == "NIH_funding:Science_TypeMixed"] <- "NIH Funding × Science Type Mixed"
  out_vals[out_vals == "NIH_funding:Science_TypeHuman-focused"] <- "NIH Funding × Science Type Human-focused"
  out_vals
}

## Goodness of fit rows and labels 

gof_names_exact <- c(
  "N",
  "Nonmissing Atypical-Pair Measures",
  "Mean IPW",
  "Weighted AUC",
  "Unweighted AUC",
  "Unweighted AIC",
  "Unweighted BIC"
)

gof_decimals_exact <- c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)

## One-seed run function

run_one_seed <- function(one_seed, file_path, sample_n) {
  
  con <- dbConnect(duckdb::duckdb())
  
# Pull out needed variables
  
  sample_query <- paste0(
    "
    WITH base AS (
      SELECT
        ROW_NUMBER() OVER () AS rid,
        Highly_Cited,
        Atyp_10pct_Z,
        NIH_funding,
        Novelty_Type,
        Science_Type,
        year,
        is_clinical
      FROM read_parquet('", file_path, "')
    )
    SELECT
      Highly_Cited,
      Atyp_10pct_Z,
      NIH_funding,
      Novelty_Type,
      Science_Type,
      year,
      is_clinical
    FROM base
    ORDER BY hash(rid, ", one_seed, ")
    LIMIT ", sample_n, "
    "
  )
  
  df <- dbGetQuery(con, sample_query)
  dbDisconnect(con, shutdown = TRUE)
 
# Define terms
# Observed atyp refers to whether measures of atypical pairs are nonmissing
   
  df <- df %>%
    mutate(
      Clinical = ifelse(is_clinical %in% c(TRUE, 1), 1L, 0L),
      year_c = year - min(year, na.rm = TRUE),
      Darwin_Tower_num = ifelse(Novelty_Type == "Darwin's Tower", 1L, 0L),
      Highly_Cited_num = ifelse(Highly_Cited %in% c(TRUE, 1), 1L, 0L),
      observed_atyp = ifelse(!is.na(Atyp_10pct_Z), 1L, 0L)
    )
  
  df$Science_Type <- factor(df$Science_Type, levels = c("Fundamental", "Mixed", "Human-focused"))
  df$Novelty_Type <- factor(df$Novelty_Type, levels = c("Platypus", "Avant-garde", "Accepted Wisdom", "Darwin's Tower", "Missing"))

# Regression formula for whether measures of atypical pairs are nonmissing
# Model generates IPWs for missingness
    
  missing_formula <- observed_atyp ~ NIH_funding + Science_Type + ns(year_c, 3) + Clinical
  
  miss_fit <- glm(
    formula = missing_formula,
    family = binomial(),
    data = df
  )
  
  df$pr_observed <- predict(miss_fit, type = "response")
  df$ipw <- ifelse(df$observed_atyp == 1, 1 / df$pr_observed, NA_real_)
  
# Regression models for Darwin's Tower and Highly Cited papers will be limited
#   to papers within nonmissing measures of atypical pairs 
  
  analytic_df <- df %>%
    filter(observed_atyp == 1)
  
# Weighted and unweighted regression formulas for Darwin's Tower and Highly Cited papers
  
  darwin_formula <- Darwin_Tower_num ~ NIH_funding * Science_Type + ns(year_c, 3) + Clinical
  
  high_formula <- Highly_Cited_num ~ NIH_funding * Science_Type + Novelty_Type + ns(year_c, 3) + Clinical
  
  darwin_model <- glm(
    formula = darwin_formula,
    family = quasibinomial(),
    data = analytic_df,
    weights = ipw
  )
  
  high_model <- glm(
    formula = high_formula,
    family = quasibinomial(),
    data = analytic_df,
    weights = ipw
  )
  
  darwin_model_unweighted <- glm(
    formula = darwin_formula,
    family = binomial(),
    data = analytic_df
  )
  
  high_model_unweighted <- glm(
    formula = high_formula,
    family = binomial(),
    data = analytic_df
  )
  
  darwin_pred_weighted <- predict(darwin_model, type = "response")
  darwin_pred_unweighted <- predict(darwin_model_unweighted, type = "response")
  high_pred_weighted <- predict(high_model, type = "response")
  high_pred_unweighted <- predict(high_model_unweighted, type = "response")
  
# Generate AUC values
  
  darwin_auc_weighted <- as.numeric(
    pROC::auc(
      response = analytic_df$Darwin_Tower_num,
      predictor = darwin_pred_weighted
    )
  )
  
  darwin_auc_unweighted <- as.numeric(
    pROC::auc(
      response = analytic_df$Darwin_Tower_num,
      predictor = darwin_pred_unweighted
    )
  )
  
  high_auc_weighted <- as.numeric(
    pROC::auc(
      response = analytic_df$Highly_Cited_num,
      predictor = high_pred_weighted
    )
  )
  
  high_auc_unweighted <- as.numeric(
    pROC::auc(
      response = analytic_df$Highly_Cited_num,
      predictor = high_pred_unweighted
    )
  )
  
  sampled_n <- nrow(df)
  observed_atyp_n <- sum(df$observed_atyp, na.rm = TRUE)
  mean_ipw_val <- mean(analytic_df$ipw, na.rm = TRUE)
  
  return(list(
    seed = one_seed,
    df = df,
    analytic_df = analytic_df,
    miss_fit = miss_fit,
    darwin_model = darwin_model,
    high_model = high_model,
    darwin_model_unweighted = darwin_model_unweighted,
    high_model_unweighted = high_model_unweighted,
    darwin_n = nrow(df),
    high_n = nrow(df),
    darwin_observed_atyp = observed_atyp_n,
    high_observed_atyp = observed_atyp_n,
    darwin_mean_ipw = mean_ipw_val,
    high_mean_ipw = mean_ipw_val,
    darwin_auc_weighted = darwin_auc_weighted,
    darwin_auc_unweighted = darwin_auc_unweighted,
    high_auc_weighted = high_auc_weighted,
    high_auc_unweighted = high_auc_unweighted,
    darwin_aic_unweighted = AIC(darwin_model_unweighted),
    darwin_bic_unweighted = BIC(darwin_model_unweighted),
    high_aic_unweighted = AIC(high_model_unweighted),
    high_bic_unweighted = BIC(high_model_unweighted)
  ))
}

## Run all five samples

results_list <- lapply(
  seed_vec,
  function(one_seed) {
    run_one_seed(
      one_seed = one_seed,
      file_path = file_path,
      sample_n = sample_n
    )
  }
)

names(results_list) <- paste0("seed_", seed_vec)

## Supplement Table 4 -- Darwin's Tower across 5 samples

darwin_texregs <- lapply(
  results_list,
  function(x) {
    make_texreg_obj(
      model_obj = x$darwin_model,
      coef_map_fun = pretty_term_darwin,
      gof_names = gof_names_exact,
      gof_vals = c(
        safe_num(x$darwin_n),
        safe_num(x$darwin_observed_atyp),
        safe_num(x$darwin_mean_ipw),
        safe_num(x$darwin_auc_weighted),
        safe_num(x$darwin_auc_unweighted),
        safe_num(x$darwin_aic_unweighted),
        safe_num(x$darwin_bic_unweighted)
      ),
      gof_decimals = gof_decimals_exact
    )
  }
)

screenreg(
  darwin_texregs,
  digits = 3,
  stars = c(0.001, 0.01, 0.05),
  custom.model.names = paste0("Sample ", 1:5),
  single.row = FALSE
)

htmlreg(
  darwin_texregs,
  file = "Supplement Table 4.html",
  digits = 3,
  stars = c(0.001, 0.01, 0.05),
  custom.model.names = paste0("Sample ", 1:5),
  caption = "Inverse-probability-weighted logistic regression models predicting Darwin's Tower",
  caption.above = TRUE,
  custom.note = "Entries are log-odds coefficients with Sandwich-robust standard errors in parentheses. Weights are inverse probabilities of observing data on atypical pairs. AIC and BIC are from the corresponding unweighted binomial models fit on the same analytic sample.",
  inline.css = TRUE,
  doctype = TRUE,
  single.row = FALSE
)

## Supplement Table 5 -- Highly cited across 5 samples

high_texregs <- lapply(
  results_list,
  function(x) {
    make_texreg_obj(
      model_obj = x$high_model,
      coef_map_fun = pretty_term_highly_cited,
      gof_names = gof_names_exact,
      gof_vals = c(
        safe_num(x$darwin_n),
        safe_num(x$darwin_observed_atyp),
        safe_num(x$darwin_mean_ipw),
        safe_num(x$darwin_auc_weighted),
        safe_num(x$darwin_auc_unweighted),
        safe_num(x$darwin_aic_unweighted),
        safe_num(x$darwin_bic_unweighted)
      ),
      gof_decimals = gof_decimals_exact
    )
  }
)

screenreg(
  high_texregs,
  digits = 3,
  stars = c(0.001, 0.01, 0.05),
  custom.model.names = paste0("Sample ", 1:5),
  single.row = FALSE
)

htmlreg(
  high_texregs,
  file = "Supplement Table 5.html",
  digits = 3,
  stars = c(0.001, 0.01, 0.05),
  custom.model.names = paste0("Sample ", 1:5),
  caption = "Inverse-probability-weighted logistic regression models predicting highly cited papers.",
  caption.above = TRUE,
  custom.note = "Entries are log-odds coefficients with Sandwich-robust standard errors in parentheses. Weights are inverse probabilities of observing data on atypical pairs. AIC and BIC are from the corresponding unweighted binomial models fit on the same analytic sample.",
  inline.css = TRUE,
  doctype = TRUE,
  single.row = FALSE
)

## Main manuscript Table 3 based on first sample
## Same sampled rows for both outcomes

main_texregs <- list(
  make_texreg_obj(
    model_obj = results_list[[1]]$darwin_model,
    coef_map_fun = pretty_term_darwin,
    gof_names = gof_names_exact,
    gof_vals = c(
      safe_num(results_list[[1]]$darwin_n),
      safe_num(results_list[[1]]$darwin_observed_atyp),
      safe_num(results_list[[1]]$darwin_mean_ipw),
      safe_num(results_list[[1]]$darwin_auc_weighted),
      safe_num(results_list[[1]]$darwin_auc_unweighted),
      safe_num(results_list[[1]]$darwin_aic_unweighted),
      safe_num(results_list[[1]]$darwin_bic_unweighted)
    ),
    gof_decimals = gof_decimals_exact
  ),
  make_texreg_obj(
    model_obj = results_list[[1]]$high_model,
    coef_map_fun = pretty_term_highly_cited,
    gof_names = gof_names_exact,
    gof_vals = c(
      safe_num(results_list[[1]]$high_n),
      safe_num(results_list[[1]]$high_observed_atyp),
      safe_num(results_list[[1]]$high_mean_ipw),
      safe_num(results_list[[1]]$high_auc_weighted),
      safe_num(results_list[[1]]$high_auc_unweighted),
      safe_num(results_list[[1]]$high_aic_unweighted),
      safe_num(results_list[[1]]$high_bic_unweighted)
    ),
    gof_decimals = gof_decimals_exact
  )
)

screenreg(
  main_texregs,
  digits = 3,
  stars = c(0.001, 0.01, 0.05),
  custom.model.names = c("Darwin's Tower", "Highly cited"),
  single.row = FALSE
)

htmlreg(
  main_texregs,
  file = "Table 3.html",
  digits = 3,
  stars = c(0.001, 0.01, 0.05),
  custom.model.names = c("Darwin's Tower", "Highly cited"),
  caption = "Inverse-probability-weighted logistic regression models",
  caption.above = TRUE,
  custom.note = "Entries are log-odds coefficients with Sandwich-robust standard errors in parentheses. Both columns use the same random sample. Weights are inverse probabilities of observing data on atypical pairs. AIC and BIC are from the corresponding unweighted binomial models fit on the same analytic sample.",
  inline.css = TRUE,
  doctype = TRUE,
  single.row = FALSE
)

# Diagnostics on IPW

ipw_vals <- results_list[[1]]$analytic_df$ipw
ipw_vals <- ipw_vals[is.finite(ipw_vals) & !is.na(ipw_vals)]

ipw_q <- quantile(
  ipw_vals,
  probs = c(0, 0.01, 0.05, 0.50, 0.95, 0.99, 1.00),
  na.rm = TRUE
)

ess <- (sum(ipw_vals, na.rm = TRUE)^2) / sum(ipw_vals^2, na.rm = TRUE)

print(ipw_q)
print(ess)
print(mean(ipw_vals, na.rm = TRUE))
print(sd(ipw_vals, na.rm = TRUE))

## Figure 4 -- illustration of Darwin's Tower model
## First sample only, use Sandwich-robust SEs (Type HC1)

## Build prediction grid

pred_grid <- expand.grid(
  NIH_funding = c(0, 1),
  Science_Type = levels(analytic_df$Science_Type),
  year_c = 0,
  Clinical = 0
)

## Build model matrix for new data

X_new <- model.matrix(
  delete.response(terms(results_list[[1]]$darwin_model)),
  pred_grid
)

## Robust covariance matrix

V_rob <- sandwich::vcovHC(results_list[[1]]$darwin_model, type = "HC1")

## Predicted link-scale values and robust SEs

beta_hat <- coef(results_list[[1]]$darwin_model)
pred_grid$linpred <- as.vector(X_new %*% beta_hat)
pred_grid$se_link <- sqrt(diag(X_new %*% V_rob %*% t(X_new)))

## Convert to probabilities and 95% CIs

pred_grid$prob <- plogis(pred_grid$linpred)
pred_grid$prob_low <- plogis(pred_grid$linpred - 1.96 * pred_grid$se_link)
pred_grid$prob_high <- plogis(pred_grid$linpred + 1.96 * pred_grid$se_link)

## Nice labels for plotting

pred_grid$NIH_funding_lab <- ifelse(
  pred_grid$NIH_funding == 1,
  "NIH-funded",
  "Not NIH-funded"
)

## Plot

ggplot(pred_grid, aes(x = Science_Type, y = prob, color = NIH_funding_lab, group = NIH_funding_lab)) +
  geom_point(position = position_dodge(width = 0.3), size = 4) +
  geom_errorbar(
    aes(ymin = prob_low, ymax = prob_high),
    position = position_dodge(width = 0.3),
    width = 0.4
  ) +
  scale_y_continuous(
    name = "Predicted Probability",
    labels = scales::label_percent(accuracy = 1)
  ) +
  labs(
    x = "Type of Science",
    color = "",
    title = ""
  ) +
  theme_bw()

ggsave("Figure 4.jpg", width = 6.665, height = 3.75, units = c("in"), dpi=600)
