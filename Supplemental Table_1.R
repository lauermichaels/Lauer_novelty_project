# Supplemental Table 1 -- characteristics by NIH funding
# Limited to papers with nonmissing atypical-pair values
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

# Create a lean DuckDB table for the nonmissing atypical-pair sample

dbExecute(con, sprintf("
CREATE OR REPLACE TEMP TABLE supp_table1_base AS
SELECT
    Funding,
    author_count_opa,
    institution_count,
    reference_count,
    Atyp_10pct_Z,
    Atyp_Median_Z,
    Novelty_Type,
    Science_Type,
    relative_citation_ratio,
    Highly_Cited,
    is_clinical
FROM read_parquet('%s')
WHERE Atyp_10pct_Z IS NOT NULL
", input_file))

# Continuous summaries in duckDB

continuous_summary_supp1 <- dbGetQuery(con, "
    WITH long_continuous AS (
        SELECT
            Funding,
            'author_count_opa' AS variable,
            'Number of Authors' AS label,
            CAST(author_count_opa AS DOUBLE) AS value,
            1 AS var_order
        FROM supp_table1_base
        WHERE author_count_opa IS NOT NULL
          AND Funding IS NOT NULL

        UNION ALL

        SELECT
            Funding,
            'institution_count' AS variable,
            'Number of Institutions' AS label,
            CAST(institution_count AS DOUBLE) AS value,
            2 AS var_order
        FROM supp_table1_base
        WHERE institution_count IS NOT NULL
          AND Funding IS NOT NULL

        UNION ALL

        SELECT
            Funding,
            'reference_count' AS variable,
            'Number of References' AS label,
            CAST(reference_count AS DOUBLE) AS value,
            3 AS var_order
        FROM supp_table1_base
        WHERE reference_count IS NOT NULL
          AND Funding IS NOT NULL

        UNION ALL

        SELECT
            Funding,
            'Atyp_10pct_Z' AS variable,
            '10th Percentile Z-score' AS label,
            CAST(Atyp_10pct_Z AS DOUBLE) AS value,
            4 AS var_order
        FROM supp_table1_base
        WHERE Atyp_10pct_Z IS NOT NULL
          AND Funding IS NOT NULL

        UNION ALL

        SELECT
            Funding,
            'Atyp_Median_Z' AS variable,
            'Median Z-score' AS label,
            CAST(Atyp_Median_Z AS DOUBLE) AS value,
            5 AS var_order
        FROM supp_table1_base
        WHERE Atyp_Median_Z IS NOT NULL
          AND Funding IS NOT NULL
    ),

    ranked AS (
        SELECT
            *,
            ROW_NUMBER() OVER (
                PARTITION BY Funding, variable
                ORDER BY value
            ) AS rn,
            COUNT(*) OVER (
                PARTITION BY Funding, variable
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
        Funding,
        variable,
        label,
        var_order,
        AVG(value) AS mean_trimmed,
        STDDEV_SAMP(value) AS sd_trimmed,
        COUNT(*) AS n_trimmed
    FROM trimmed
    GROUP BY
        Funding,
        variable,
        label,
        var_order
    ORDER BY
        var_order,
        Funding
")

# Categorical summaries in duckDB

categorical_summary_supp1 <- dbGetQuery(con, "
    WITH novelty_long AS (
        SELECT
            Funding,
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
        FROM supp_table1_base
        WHERE Novelty_Type IN (
            'Platypus',
            'Avant-garde',
            'Accepted Wisdom',
            'Darwin''s Tower'
        )
          AND Funding IS NOT NULL
    ),

    science_long AS (
        SELECT
            Funding,
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
        FROM supp_table1_base
        WHERE Science_Type IS NOT NULL
          AND Funding IS NOT NULL
    ),

    categorical_long AS (
        SELECT * FROM novelty_long
        UNION ALL
        SELECT * FROM science_long
    ),

    categorical_counts AS (
        SELECT
            Funding,
            variable,
            label,
            level,
            var_order,
            level_order,
            COUNT(*) AS n,
            SUM(COUNT(*)) OVER (
                PARTITION BY Funding, variable
            ) AS denom
        FROM categorical_long
        GROUP BY
            Funding,
            variable,
            label,
            level,
            var_order,
            level_order
    ),

    clinical_counts AS (
        SELECT
            Funding,
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
        FROM supp_table1_base
        WHERE is_clinical IS NOT NULL
          AND Funding IS NOT NULL
        GROUP BY
            Funding
    )

    SELECT
        Funding,
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
        Funding,
        variable,
        label,
        level,
        var_order,
        level_order,
        n,
        denom,
        n::DOUBLE / denom::DOUBLE AS prop
    FROM clinical_counts

    ORDER BY
        var_order,
        level_order,
        Funding
")

# Format duckDB summaries into a table body

continuous_table_supp1 <- continuous_summary_supp1 %>%
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
    variable,
    label,
    row_label,
    row_type,
    var_order,
    level_order,
    Funding,
    stat
  )

categorical_table_supp1 <- categorical_summary_supp1 %>%
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
    variable,
    label,
    row_label,
    row_type,
    var_order,
    level_order,
    Funding,
    stat
  )

supp_table1_body_long <- bind_rows(
  continuous_table_supp1,
  categorical_table_supp1
)

# Pivot to NIH versus non-NIH columns

supp_table1_body_wide <- supp_table1_body_long %>%
  mutate(
    Funding = factor(
      Funding,
      levels = c("Not NIH", "NIH")
    )
  ) %>%
  arrange(var_order, level_order, row_type) %>%
  select(row_label, Funding, stat, var_order, level_order) %>%
  pivot_wider(
    names_from = Funding,
    values_from = stat
  ) %>%
  arrange(var_order, level_order) %>%
  select(
    Variable = row_label,
    `Not NIH`,
    NIH
  )

supp_table1_body_wide

# Create and save supplement Table 1

tbl_gt_supp_Table_1 <- supp_table1_body_wide %>%
  gt() %>%
  tab_header(
    title = md("**Supplemental Table 1: Characteristics of Papers According to Funding Among Papers with Nonmissing Atypical-Pair Measures**")
  ) %>%
  tab_spanner(
    label = md("**Funding**"),
    columns = c(`Not NIH`, NIH)
  ) %>%
  cols_label(
    Variable = md("**Variable**"),
    `Not NIH` = md("**Not NIH**"),
    NIH = md("**NIH**")
  ) %>%
  tab_footnote(
    footnote = "Mean (SD); n (%)",
    locations = cells_column_labels(columns = c(`Not NIH`, NIH))
  ) %>%
  tab_options(
    table.font.names = "Times New Roman"
  )

tbl_gt_supp_Table_1

gtsave(
  tbl_gt_supp_Table_1,
  "Supplemental Table 1.html"
)

# Save summary data

arrow::write_parquet(
  supp_table1_body_wide,
  "supplemental_table_1_summary.parquet"
)

dbDisconnect(con, shutdown = TRUE)
