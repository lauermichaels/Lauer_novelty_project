# Supplemental Tables 3a and 3b -- characteristics over time,
# broken down by funding, nonmissing atypical-pair values only
# DuckDB-efficient version

library(DBI)
library(duckdb)
library(tidyverse)
library(gt)

# Reuse existing DuckDB connection if one is already open
if (!exists("con") || !DBI::dbIsValid(con)) {
  con <- dbConnect(duckdb::duckdb(), dbdir = "opa_sciscinet.duckdb")

  dbExecute(con, "PRAGMA threads = 8")
  dbExecute(con, "PRAGMA memory_limit = '32GB'")
  dbExecute(con, "PRAGMA temp_directory = 'duckdb_temp'")
}

input_file <- "df_opa_sciscinet_2001_2022_6_18_26.parquet"

# Create a lean DuckDB base table

dbExecute(con, sprintf("
CREATE OR REPLACE TEMP TABLE supp_table3_base AS
SELECT
CASE
WHEN year >= 2001 AND year <= 2004 THEN '2001-2004'
WHEN year >= 2005 AND year <= 2009 THEN '2005-2009'
WHEN year >= 2010 AND year <= 2014 THEN '2010-2014'
WHEN year >= 2015 AND year <= 2019 THEN '2015-2019'
WHEN year >= 2020 AND year <= 2022 THEN '2020-2022'
ELSE 'Other'
END AS year_group,

CASE
WHEN NIH_funding = 1 THEN 'NIH-funded'
WHEN NIH_funding = 0 THEN 'Not NIH-funded'
ELSE NULL
END AS funding_group,

author_count_opa,
institution_count,
reference_count,
Atyp_10pct_Z,
Atyp_Median_Z,
Novelty_Type,
Science_Type,
is_clinical
FROM read_parquet('%s')
WHERE Atyp_10pct_Z IS NOT NULL
AND NIH_funding IS NOT NULL
", input_file))
# Continuous summaries in DuckDB

continuous_summary_supp3 <- dbGetQuery(con, "
WITH long_continuous AS (
  SELECT
  funding_group,
  year_group,
  'author_count_opa' AS variable,
  'Number of Authors' AS label,
  CAST(author_count_opa AS DOUBLE) AS value,
  1 AS var_order
  FROM supp_table3_base
  WHERE author_count_opa IS NOT NULL
  AND funding_group IS NOT NULL
  AND year_group != 'Other'
  
  UNION ALL
  
  SELECT
  funding_group,
  year_group,
  'institution_count' AS variable,
  'Number of Institutions' AS label,
  CAST(institution_count AS DOUBLE) AS value,
  2 AS var_order
  FROM supp_table3_base
  WHERE institution_count IS NOT NULL
  AND funding_group IS NOT NULL
  AND year_group != 'Other'
  
  UNION ALL
  
  SELECT
  funding_group,
  year_group,
  'reference_count' AS variable,
  'Number of References' AS label,
  CAST(reference_count AS DOUBLE) AS value,
  3 AS var_order
  FROM supp_table3_base
  WHERE reference_count IS NOT NULL
  AND funding_group IS NOT NULL
  AND year_group != 'Other'
  
  UNION ALL
  
  SELECT
  funding_group,
  year_group,
  'Atyp_10pct_Z' AS variable,
  '10th Percentile Z-score' AS label,
  CAST(Atyp_10pct_Z AS DOUBLE) AS value,
  4 AS var_order
  FROM supp_table3_base
  WHERE Atyp_10pct_Z IS NOT NULL
  AND funding_group IS NOT NULL
  AND year_group != 'Other'
  
  UNION ALL
  
  SELECT
  funding_group,
  year_group,
  'Atyp_Median_Z' AS variable,
  'Median Z-score' AS label,
  CAST(Atyp_Median_Z AS DOUBLE) AS value,
  5 AS var_order
  FROM supp_table3_base
  WHERE Atyp_Median_Z IS NOT NULL
  AND funding_group IS NOT NULL
  AND year_group != 'Other'
),

ranked AS (
  SELECT
  *,
  ROW_NUMBER() OVER (
    PARTITION BY funding_group, year_group, variable
    ORDER BY value
  ) AS rn,
  COUNT(*) OVER (
    PARTITION BY funding_group, year_group, variable
  ) AS n_total
  FROM long_continuous
),

trimmed AS (
  SELECT
  *
    FROM ranked
  WHERE rn >= FLOOR(n_total * 0.01) + 1
  AND rn <= n_total - FLOOR(n_total * 0.01)
)

SELECT
funding_group,
year_group,
variable,
label,
var_order,
AVG(value) AS mean_trimmed,
STDDEV_SAMP(value) AS sd_trimmed,
COUNT(*) AS n_trimmed
FROM trimmed
GROUP BY
funding_group,
year_group,
variable,
label,
var_order
")

# Categorical and dichotomous summaries in DuckDB


categorical_summary_supp3 <- dbGetQuery(con, "
WITH categorical_long AS (
  SELECT
  funding_group,
  year_group,
  'Novelty_Type' AS variable,
  'Article Type' AS label,
  Novelty_Type AS level,
  6 AS var_order,
  CASE Novelty_Type
  WHEN 'Platypus' THEN 1
  WHEN 'Avant-garde' THEN 2
  WHEN 'Accepted Wisdom' THEN 3
  WHEN 'Darwin''s Tower' THEN 4
  ELSE 5
  END AS level_order
  FROM supp_table3_base
  WHERE Novelty_Type IN (
    'Platypus',
    'Avant-garde',
    'Accepted Wisdom',
    'Darwin''s Tower'
  )
  AND funding_group IS NOT NULL
  AND year_group != 'Other'
  
  UNION ALL
  
  SELECT
  funding_group,
  year_group,
  'Science_Type' AS variable,
  'Science Type' AS label,
  Science_Type AS level,
  7 AS var_order,
  CASE Science_Type
  WHEN 'Fundamental' THEN 1
  WHEN 'Mixed' THEN 2
  WHEN 'Human-focused' THEN 3
  ELSE 4
  END AS level_order
  FROM supp_table3_base
  WHERE Science_Type IS NOT NULL
  AND funding_group IS NOT NULL
  AND year_group != 'Other'
),

categorical_counts AS (
  SELECT
  funding_group,
  year_group,
  variable,
  label,
  level,
  var_order,
  level_order,
  COUNT(*) AS n,
  SUM(COUNT(*)) OVER (
    PARTITION BY funding_group, year_group, variable
  ) AS denom
  FROM categorical_long
  GROUP BY
  funding_group,
  year_group,
  variable,
  label,
  level,
  var_order,
  level_order
),

clinical_counts AS (
  SELECT
  funding_group,
  year_group,
  'is_clinical' AS variable,
  'Clinical Trial' AS label,
  'TRUE' AS level,
  8 AS var_order,
  1 AS level_order,
  SUM(
    CASE
    WHEN is_clinical = TRUE THEN 1
    ELSE 0
    END
  ) AS n,
  COUNT(*) AS denom
  FROM supp_table3_base
  WHERE is_clinical IS NOT NULL
  AND funding_group IS NOT NULL
  AND year_group != 'Other'
  GROUP BY
  funding_group,
  year_group
)

SELECT
funding_group,
year_group,
variable,
label,
level,
var_order,
level_order,
n,
denom,
n::DOUBLE / denom::DOUBLE AS prop
FROM categorical_counts

UNION ALL

SELECT
funding_group,
year_group,
variable,
label,
level,
var_order,
level_order,
n,
denom,
n::DOUBLE / denom::DOUBLE AS prop
FROM clinical_counts
")

# Format summaries into a reusable table body

continuous_table_supp3 <- continuous_summary_supp3 %>%
  mutate(
    stat = paste0(
      sprintf("%.1f", mean_trimmed),
      " (",
      sprintf("%.1f", sd_trimmed),
      ")"
    ),
    row_label = label,
    row_type = "continuous",
    level_order = 0
  ) %>%
  select(
    funding_group,
    year_group,
    variable,
    label,
    row_label,
    row_type,
    var_order,
    level_order,
    stat
  )

categorical_table_supp3 <- categorical_summary_supp3 %>%
  mutate(
    stat = paste0(
      scales::comma(n, accuracy = 1),
      " (",
      sprintf("%.0f", prop * 100),
      "%)"
    ),
    row_label = ifelse(
      variable == "is_clinical",
      label,
      paste0("  ", level)
    ),
    row_type = "categorical"
  ) %>%
  select(
    funding_group,
    year_group,
    variable,
    label,
    row_label,
    row_type,
    var_order,
    level_order,
    stat
  )

supp_table3_body_long <- bind_rows(
  continuous_table_supp3,
  categorical_table_supp3
)


# Helper function to build each gt table


make_supp_table3_gt <- function(table_data, funding_value, table_title) {

  table_wide <- table_data %>%
    filter(funding_group == funding_value) %>%
    mutate(
      year_group = factor(
        year_group,
        levels = c(
          "2001-2004",
          "2005-2009",
          "2010-2014",
          "2015-2019",
          "2020-2022"
        )
      )
    ) %>%
    arrange(var_order, level_order, row_type) %>%
    select(row_label, year_group, stat, var_order, level_order) %>%
    pivot_wider(
      names_from = year_group,
      values_from = stat
    ) %>%
    arrange(var_order, level_order) %>%
    select(
      Variable = row_label,
      `2001-2004`,
      `2005-2009`,
      `2010-2014`,
      `2015-2019`,
      `2020-2022`
    )

  table_wide %>%
    gt() %>%
    tab_header(
      title = md(paste0("**", table_title, "**"))
    ) %>%
    tab_spanner(
      label = md("**Publication Year**"),
      columns = c(
        `2001-2004`,
        `2005-2009`,
        `2010-2014`,
        `2015-2019`,
        `2020-2022`
      )
    ) %>%
    cols_label(
      Variable = md("**Variable**"),
      `2001-2004` = md("**2001-2004**"),
      `2005-2009` = md("**2005-2009**"),
      `2010-2014` = md("**2010-2014**"),
      `2015-2019` = md("**2015-2019**"),
      `2020-2022` = md("**2020-2022**")
    ) %>%
    tab_footnote(
      footnote = "Mean (SD); n (%)",
      locations = cells_column_labels(
        columns = c(
          `2001-2004`,
          `2005-2009`,
          `2010-2014`,
          `2015-2019`,
          `2020-2022`
        )
      )
    ) %>%
    tab_options(
      table.font.names = "Times New Roman",
      table.font.size = px(12),
      data_row.padding = px(2)
    )
}


# Create Supplementary Tables 3a and 3b


tbl_3a_gt <- make_supp_table3_gt(
  table_data = supp_table3_body_long,
  funding_value = "NIH-funded",
  table_title = "Table 3a: Characteristics of NIH-funded Papers According to Year Among Papers with Nonmissing Atypical-Pair Measures"
)

tbl_3b_gt <- make_supp_table3_gt(
  table_data = supp_table3_body_long,
  funding_value = "Not NIH-funded",
  table_title = "Table 3b: Characteristics of Non-NIH-funded Papers According to Year Among Papers with Nonmissing Atypical-Pair Measures"
)

tbl_3a_gt
tbl_3b_gt

# Combine and save Supplementary Table 3


supplementary_table_3 <- gt_group(
  tbl_3a_gt,
  tbl_3b_gt
)

gtsave(
  supplementary_table_3,
  "Supplementary Table 3.html"
)

# Optional save of the summary data

arrow::write_parquet(
  supp_table3_body_long,
  "supplementary_table_3_summary_long.parquet"
)

dbDisconnect(con, shutdown = TRUE)
































































































# Supplemental Tables 3a and 3b -- characteristics over time, broken down by funding, nonmissing values only

library(arrow) # read parquet files
library(tidyverse) # data manipulation and ggplots
library(gtsummary) # summary tables
library(gt) # summary tables -- export

setwd("~/Library/CloudStorage/OneDrive-Personal/Sciscinet v2")

df_opa_sciscinet_2001_2022 <- read_parquet("df_opa_sciscinet_2001_2022_6_18_26.parquet")

theme_gtsummary_compact()

df_tbl_3 <-
  df_opa_sciscinet_2001_2022 %>%
  
  filter(!is.na(Atyp_10pct_Z)) %>%
  
  select(year,
         NIH_funding,
         author_count_opa, institution_count, reference_count,
         Atyp_10pct_Z, Atyp_Median_Z,
         Novelty_Type, Science_Type, is_clinical
  ) %>%
  
  mutate(
    Novelty_Type = factor(Novelty_Type, levels = c("Platypus", "Avant-garde", "Accepted Wisdom", "Darwin's Tower")                       )
  ) %>%
  
  mutate(
    Science_Type = factor(Science_Type, levels = c("Fundamental","Mixed","Human-focused"))
  ) %>%
  
  mutate(
    year_group = case_when(
      year >= 2001 & year <= 2004 ~ "2001-2004",
      year >= 2005 & year <= 2009 ~ "2005-2009",
      year >= 2010 & year <= 2014 ~ "2010-2014",
      year >= 2015 & year <= 2019 ~ "2015-2019",
      year >= 2020 & year <= 2022 ~ "2020-2022",
      TRUE                        ~ "Other"
    )
  )

###

# Table 3a NIH only

###

tbl_3a <-
  df_tbl_3 %>%
  filter(NIH_funding==1) %>%
  select(year_group,
         author_count_opa,
         institution_count,
         reference_count,
         Atyp_10pct_Z, Atyp_Median_Z,
         Novelty_Type, Science_Type, is_clinical
  ) %>%
  tbl_summary(
    by = year_group,
    type = list(
      author_count_opa ~ "continuous",
      institution_count ~ "continuous",
      reference_count ~ "continuous",
      Atyp_10pct_Z ~ "continuous",
      Atyp_Median_Z ~ "continuous",
      Novelty_Type ~ "categorical",
      Science_Type ~ "categorical",
      is_clinical ~ "dichotomous"
    ),
    label = list(
      author_count_opa ~ "Number of Authors",
      institution_count ~ "Number of Institutions",
      reference_count ~ "Number of References",
      Atyp_10pct_Z ~ "10th Percentile Z-score",
      Atyp_Median_Z ~ "Median Z-score",
      Novelty_Type ~ "Article Type",
      Science_Type ~ "Science Type",
      is_clinical ~ "Clinical Trial"
    ),
    statistic = list(
      statistic = all_continuous() ~ "{mean_trimmed} ({sd_trimmed})",
      all_dichotomous() ~ "{n} ({p}%)",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ c(1,1),
      all_dichotomous() ~ c(0, 0),
      all_categorical() ~ c(0, 0)
    ),
    
    missing = "no",
    sort=NULL
  ) %>%
  modify_table_body(
    ~ .x %>%
      mutate(
        .var_rank = match(
          variable,
          c(
            "author_count_opa","institution_count","reference_count",
            "Atyp_10pct_Z", "Atyp_Median_Z",
            "Novelty_Type", "Science_Type", "is_clinical"
          )
        )
      ) %>%
      arrange(.var_rank, row_type) %>%
      select(-.var_rank)
  )%>%
  modify_caption("<div style='text-align: left;'>Table 3a: Characteristics of NIH-funded Papers According to Year</div>") %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Publication Year**") %>% 
  modify_footnote(
    all_stat_cols() ~ "Mean (SD); n (%)") %>%
  bold_labels()

tbl_3a_gt <- as_gt(tbl_3a) %>%
  gt::tab_options(table.font.names = "Times New Roman")
tbl_3a_gt

###

# Table 3b Non-NIH only

###

tbl_3b <-
  df_tbl_3 %>%
  filter(NIH_funding==0) %>%
  select(year_group,
         author_count_opa,
         institution_count,
         reference_count,
         Atyp_10pct_Z, Atyp_Median_Z,
         Novelty_Type, Science_Type, is_clinical
  ) %>%
  tbl_summary(
    by = year_group,
    type = list(
      author_count_opa ~ "continuous",
      institution_count ~ "continuous",
      reference_count ~ "continuous",
      Atyp_10pct_Z ~ "continuous",
      Atyp_Median_Z ~ "continuous",
      Novelty_Type ~ "categorical",
      Science_Type ~ "categorical",
      is_clinical ~ "dichotomous"
    ),
    label = list(
      author_count_opa ~ "Number of Authors",
      institution_count ~ "Number of Institutions",
      reference_count ~ "Number of References",
      Atyp_10pct_Z ~ "10th Percentile Z-score",
      Atyp_Median_Z ~ "Median Z-score",
      Novelty_Type ~ "Article Type",
      Science_Type ~ "Science Type",
      is_clinical ~ "Clinical Trial"
    ),
    statistic = list(
      statistic = all_continuous() ~ "{mean_trimmed} ({sd_trimmed})",
      all_dichotomous() ~ "{n} ({p}%)",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ c(1,1),
      all_dichotomous() ~ c(0, 0),
      all_categorical() ~ c(0, 0)
    ),
    
    missing = "no",
    sort=NULL
  ) %>%
  modify_table_body(
    ~ .x %>%
      mutate(
        .var_rank = match(
          variable,
          c(
            "author_count_opa","institution_count","reference_count",
            "Atyp_10pct_Z", "Atyp_Median_Z",
            "Novelty_Type", "Science_Type", "is_clinical"
          )
        )
      ) %>%
      arrange(.var_rank, row_type) %>%
      select(-.var_rank)
  )%>%
  modify_caption("<div style='text-align: left;'>Table 3b: Characteristics of Non-NIH-funded Papers According to Year</div>") %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(all_stat_cols() ~ "**Publication Year**") %>% 
  modify_footnote(
    all_stat_cols() ~ "Mean (SD); n (%)") %>%
  bold_labels()

# Optional: convert to gt for extra styling / export
tbl_3b_gt <- as_gt(tbl_3b) %>%
  gt::tab_options(table.font.names = "Times New Roman")
tbl_3b_gt

supplementary_table_3 <- gt_group(tbl_3a_gt, tbl_3b_gt)

setwd("~/Library/CloudStorage/OneDrive-Personal/Novelty Paper")

gtsave(supplementary_table_3, "Supplementary Table 3.html")
