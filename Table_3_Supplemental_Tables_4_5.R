# Table 3 of main paper; supplemental Tables 4 and 5
# Logistic regression analyses for Darwin's Tower papers and highly cited papers
# DuckDB-efficient version

library(DBI)
library(duckdb)
library(tidyverse)
library(splines)
library(sandwich)
library(texreg)
library(pROC)
library(scales)

# Reuse an existing DuckDB connection if available
if (!exists("con") || !DBI::dbIsValid(con)) {
  con <- dbConnect(duckdb::duckdb(), dbdir = "opa_sciscinet.duckdb")
  
  dbExecute(con, "PRAGMA threads = 8")
  dbExecute(con, "PRAGMA memory_limit = '32GB'")
  dbExecute(con, "PRAGMA temp_directory = 'duckdb_temp'")
}

file_path <- "df_opa_sciscinet_2001_2022_6_18_26.parquet"
sample_n <- 500000
seed_vec <- c(101, 202, 303, 404, 505)

# Helper functions

safe_num <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  as.numeric(x[1])
}

get_robust_se <- function(model_obj) {
  sqrt(diag(vcovHC(model_obj, type = "HC1")))
}

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

# Nice labels

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

# GOF row labels

gof_names_exact <- c(
  "N",
  "Nonmissing Atypical-Pair Measures",
  "Mean IPW",
  "Weighted AUC",
  "Unweighted AUC",
  "Unweighted AIC",
  "Unweighted BIC"
)

gof_decimals_exact <- c(
  FALSE,
  FALSE,
  TRUE,
  TRUE,
  TRUE,
  TRUE,
  TRUE
)

# One seed model run

run_one_seed <- function(one_seed, file_path, sample_n, con) {
  
  sample_query <- sprintf("
    SELECT
        Highly_Cited,
        Atyp_10pct_Z,
        NIH_funding,
        Novelty_Type,
        Science_Type,
        year,
        is_clinical
    FROM read_parquet('%s')
    USING SAMPLE reservoir(%s ROWS) REPEATABLE (%s)
  ", file_path, sample_n, one_seed)
  
  df <- dbGetQuery(con, sample_query)
  
  df <- df %>%
    mutate(
      Clinical = ifelse(is_clinical %in% c(TRUE, 1, "TRUE", "True", "true", "1"), 1L, 0L),
      year_c = year - min(year, na.rm = TRUE),
      Darwin_Tower_num = ifelse(Novelty_Type == "Darwin's Tower", 1L, 0L),
      Highly_Cited_num = ifelse(Highly_Cited %in% c(TRUE, 1, "TRUE", "True", "true", "1"), 1L, 0L),
      observed_atyp = ifelse(!is.na(Atyp_10pct_Z), 1L, 0L),
      NIH_funding = as.numeric(NIH_funding)
    )
  
  df$Science_Type <- factor(
    df$Science_Type,
    levels = c("Fundamental", "Mixed", "Human-focused")
  )
  
  df$Novelty_Type <- factor(
    df$Novelty_Type,
    levels = c(
      "Platypus",
      "Avant-garde",
      "Accepted Wisdom",
      "Darwin's Tower",
      "Missing"
    )
  )
  
  missing_formula <- observed_atyp ~
    NIH_funding +
    Science_Type +
    ns(year_c, 3) +
    Clinical
  
  miss_fit <- glm(
    formula = missing_formula,
    family = binomial(),
    data = df
  )
  
  df$pr_observed <- predict(miss_fit, type = "response")
  
  df <- df %>%
    mutate(
      pr_observed = pmin(pmax(pr_observed, 0.01), 0.99),
      ipw = ifelse(observed_atyp == 1, 1 / pr_observed, NA_real_)
    )
  
  analytic_df <- df %>%
    filter(observed_atyp == 1) %>%
    filter(
      !is.na(Darwin_Tower_num),
      !is.na(Highly_Cited_num),
      !is.na(NIH_funding),
      !is.na(Science_Type),
      !is.na(Novelty_Type),
      !is.na(year_c),
      !is.na(Clinical),
      !is.na(ipw)
    )
  
  darwin_formula <- Darwin_Tower_num ~
    NIH_funding * Science_Type +
    ns(year_c, 3) +
    Clinical
  
  high_formula <- Highly_Cited_num ~
    NIH_funding * Science_Type +
    Novelty_Type +
    ns(year_c, 3) +
    Clinical
  
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
  
  darwin_auc_weighted <- as.numeric(
    pROC::auc(
      response = analytic_df$Darwin_Tower_num,
      predictor = darwin_pred_weighted,
      quiet = TRUE
    )
  )
  
  darwin_auc_unweighted <- as.numeric(
    pROC::auc(
      response = analytic_df$Darwin_Tower_num,
      predictor = darwin_pred_unweighted,
      quiet = TRUE
    )
  )
  
  high_auc_weighted <- as.numeric(
    pROC::auc(
      response = analytic_df$Highly_Cited_num,
      predictor = high_pred_weighted,
      quiet = TRUE
    )
  )
  
  high_auc_unweighted <- as.numeric(
    pROC::auc(
      response = analytic_df$Highly_Cited_num,
      predictor = high_pred_unweighted,
      quiet = TRUE
    )
  )
  
  sampled_n <- nrow(df)
  observed_atyp_n <- sum(df$observed_atyp, na.rm = TRUE)
  mean_ipw_val <- mean(analytic_df$ipw, na.rm = TRUE)
  
  ipw_vals <- analytic_df$ipw
  ipw_vals <- ipw_vals[is.finite(ipw_vals) & !is.na(ipw_vals)]
  
  list(
    seed = one_seed,
    
    darwin_model = darwin_model,
    high_model = high_model,
    
    darwin_n = sampled_n,
    high_n = sampled_n,
    
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
    high_bic_unweighted = BIC(high_model_unweighted),
    
    ipw_vals = ipw_vals
  )
}

# Run all 5 samples

results_list <- lapply(
  seed_vec,
  function(one_seed) {
    run_one_seed(
      one_seed = one_seed,
      file_path = file_path,
      sample_n = sample_n,
      con = con
    )
  }
)

names(results_list) <- paste0("seed_", seed_vec)

# Supplemental Table 4

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

# Supplementary Table 5

high_texregs <- lapply(
  results_list,
  function(x) {
    make_texreg_obj(
      model_obj = x$high_model,
      coef_map_fun = pretty_term_highly_cited,
      gof_names = gof_names_exact,
      gof_vals = c(
        safe_num(x$high_n),
        safe_num(x$high_observed_atyp),
        safe_num(x$high_mean_ipw),
        safe_num(x$high_auc_weighted),
        safe_num(x$high_auc_unweighted),
        safe_num(x$high_aic_unweighted),
        safe_num(x$high_bic_unweighted)
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

# Main manuscript Table 3 first sample only

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


# Diagnostics on IPW first sample

ipw_vals <- results_list[[1]]$ipw_vals
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

dbDisconnect(con, shutdown = TRUE)